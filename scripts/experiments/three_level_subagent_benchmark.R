#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = FALSE)
file_argument <- sub("^--file=", "", arguments[grepl("^--file=", arguments)])
if (!length(file_argument)) stop("无法确定实验脚本位置。", call. = FALSE)
project_root <- normalizePath(file.path(dirname(file_argument[[1]]), "..", ".."), winslash = "/", mustWork = TRUE)
Sys.setenv(TRACE_SDTM_ROOT = project_root)
project_library <- file.path(project_root, ".Rlib")
if (dir.exists(project_library)) .libPaths(c(project_library, .libPaths()))
for (file in c(
  "utils.R", "config.R", "registry.R", "profile.R", "recommend.R", "review.R",
  "transforms.R", "build.R", "validate_local.R", "p21.R", "evaluate.R", "report.R"
)) source(file.path(project_root, "R", file), encoding = "UTF-8")

args <- commandArgs(trailingOnly = TRUE)
action <- args[[1]] %||% "help"

argument_value <- function(name, required = TRUE) {
  position <- match(name, args)
  if (is.na(position) || position == length(args)) {
    if (required) stop(sprintf("缺少参数 %s。", name), call. = FALSE)
    return("")
  }
  args[[position + 1L]]
}

benchmark_config <- function(scenario, experiment_id) {
  apply_experiment_paths(load_project_config(scenario), experiment_id)
}

benchmark_groups <- function(config) {
  dictionary_path <- trace_path(config$paths$profile_dir, "source_dictionary.csv")
  if (!file.exists(dictionary_path)) profile_sources(config)
  # 画像生成时空字符串是契约的一部分。默认 CSV 读取会把它们改成 NA，
  # 进而令冻结请求在重建时由 "" 变成 null，造成虚假的校验失败。
  dictionary <- readr::read_csv(dictionary_path, show_col_types = FALSE) |>
    dplyr::mutate(dplyr::across(
      dplyr::any_of(c(
        "source_domain", "source_dataset", "source_variable", "label", "data_type",
        "example_values", "form_name", "grain", "keys", "concept_roles",
        "format_candidates", "partial_tokens"
      )),
      ~ tidyr::replace_na(as.character(.x), "")
    ))
  recommendation_groups(load_mapping_template(config), dictionary, config)
}

group_directory <- function(config, group_id) {
  ensure_dir(file.path(trace_path(config$paths$recommendation_dir), "groups", group_id))
}

git_commit <- function() {
  value <- system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE)
  as.character(value[[1]])
}

read_response <- function(path) {
  content <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  parsed <- jsonlite::fromJSON(extract_json_content(content), simplifyVector = FALSE)
  list(content = content, parsed = parsed, sha256 = digest::digest(content, algo = "sha256"))
}

prepare_benchmark <- function(experiment_id) {
  for (scenario in c("basic", "intermediate", "advanced")) {
    config <- benchmark_config(scenario, experiment_id)
    dictionary <- profile_sources(config)
    specification <- load_mapping_template(config)
    metadata <- load_metadata(config)
    registry <- load_transform_registry(config)
    groups <- recommendation_groups(specification, dictionary, config)
    group_rows <- list()
    for (group in groups) {
      path <- group_directory(config, group$group_id)
      request <- classification_prompt_v02(group, metadata, registry, config)
      request_path <- file.path(path, "stage1_request.txt")
      writeLines(enc2utf8(request), request_path, useBytes = TRUE)
      write_json(group$context, file.path(path, "source_context.json"))
      metadata_row <- list(
        scenario = scenario,
        group_id = group$group_id,
        concept_ids = vapply(group$concepts, `[[`, character(1), "concept_id"),
        stage1_request_sha256 = file_sha256(request_path),
        registry_sha256 = file_sha256(trace_path(config$paths$transform_registry)),
        profile_sha256 = file_sha256(trace_path(config$paths$profile_dir, "source_dictionary.csv")),
        concept_specification_sha256 = file_sha256(trace_path(config$paths$specification_template)),
        policy_sha256 = file_sha256(trace_path(config$paths$mapping_policies)),
        gold_sha256 = file_sha256(trace_path(config$paths$gold_specification)),
        code_commit = git_commit(),
        prepared_at = utc_now()
      )
      write_json(metadata_row, file.path(path, "request_metadata.json"))
      group_rows[[length(group_rows) + 1L]] <- metadata_row
    }
    manifest <- list(
      benchmark_id = experiment_id, scenario = scenario, provider = "codex_subagent",
      protocol = "fresh_domain_agent_two_stage_single_run",
      blind_limit = "fork_turns=none and task restrictions are procedural, not operating-system isolation",
      concept_count = length(specification$concepts), group_count = length(groups),
      groups = group_rows, api_key_used = FALSE
    )
    write_json(manifest, trace_path(config$paths$manifest_dir, "experiment_manifest.json"))
  }
}

validate_stage1 <- function(scenario, experiment_id, group_id, response_file, task_id) {
  config <- benchmark_config(scenario, experiment_id)
  groups <- benchmark_groups(config)
  group <- groups[[group_id]]
  if (is.null(group)) stop(sprintf("未知组：%s", group_id), call. = FALSE)
  path <- group_directory(config, group_id)
  request_path <- file.path(path, "stage1_request.txt")
  expected_request <- classification_prompt_v02(group, load_metadata(config), load_transform_registry(config), config)
  frozen_request <- paste(readLines(request_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  if (!identical(frozen_request, expected_request)) {
    stop("第一阶段请求与当前代码重建结果不一致。", call. = FALSE)
  }
  response <- read_response(response_file)
  records <- response$parsed$classifications %||% response$parsed
  parsed <- parse_classifications_v02(records, group, load_transform_registry(config))
  repeated <- parse_classifications_v02(records, group, load_transform_registry(config))
  normalized_hash <- digest::digest(registry_json(parsed), algo = "sha256")
  if (!identical(normalized_hash, digest::digest(registry_json(repeated), algo = "sha256"))) {
    stop("第一阶段冻结响应重复解析不一致。", call. = FALSE)
  }
  write_json(parsed, file.path(path, "stage1_normalized.json"))
  write_json(list(
    validation_status = "passed", request_sha256 = file_sha256(request_path),
    response_sha256 = response$sha256, normalized_sha256 = normalized_hash,
    subagent_task = task_id, validated_at = utc_now(), retry_performed = FALSE
  ), file.path(path, "stage1_validation.json"))
}

prepare_stage2 <- function(scenario, experiment_id, group_id) {
  config <- benchmark_config(scenario, experiment_id)
  group <- benchmark_groups(config)[[group_id]]
  path <- group_directory(config, group_id)
  classifications <- jsonlite::read_json(file.path(path, "stage1_normalized.json"), simplifyVector = FALSE)
  request <- selection_prompt_v02(group, classifications, load_metadata(config), load_transform_registry(config), config)
  writeLines(enc2utf8(request), file.path(path, "stage2_request.txt"), useBytes = TRUE)
}

validate_stage2 <- function(scenario, experiment_id, group_id, response_file, task_id) {
  config <- benchmark_config(scenario, experiment_id)
  group <- benchmark_groups(config)[[group_id]]
  path <- group_directory(config, group_id)
  classifications <- jsonlite::read_json(file.path(path, "stage1_normalized.json"), simplifyVector = FALSE)
  response <- read_response(response_file)
  records <- response$parsed$recommendations %||% response$parsed
  diagnostic <- diagnose_candidate_plans_v03(
    records, classifications, group, load_mapping_template(config), load_metadata(config),
    load_transform_registry(config), provenance = "codex_subagent", config = config
  )
  write_json(diagnostic, file.path(path, "stage2_concept_diagnostic.json"))
  parsed <- tryCatch(parse_candidate_plans_v02(
    records, classifications, group, load_mapping_template(config), load_metadata(config),
    load_transform_registry(config), provenance = "codex_subagent", config = config
  ), error = identity)
  if (inherits(parsed, "error")) {
    write_json(list(
      validation_status = "failed", failure_reason = sanitize_for_log(conditionMessage(parsed)),
      request_sha256 = file_sha256(file.path(path, "stage2_request.txt")), response_sha256 = response$sha256,
      subagent_task = task_id, validated_at = utc_now(), retry_performed = FALSE
    ), file.path(path, "stage2_validation_failure.json"))
    stop(parsed)
  }
  repeated <- parse_candidate_plans_v02(
    records, classifications, group, load_mapping_template(config), load_metadata(config),
    load_transform_registry(config), provenance = "codex_subagent", config = config
  )
  normalized_hash <- digest::digest(registry_json(parsed), algo = "sha256")
  if (!identical(normalized_hash, digest::digest(registry_json(repeated), algo = "sha256"))) {
    stop("第二阶段冻结响应重复解析不一致。", call. = FALSE)
  }
  write_json(parsed, file.path(path, "stage2_normalized.json"))
  write_json(list(
    validation_status = "passed", request_sha256 = file_sha256(file.path(path, "stage2_request.txt")),
    response_sha256 = response$sha256, normalized_sha256 = normalized_hash,
    subagent_task = task_id, validated_at = utc_now(), retry_performed = FALSE
  ), file.path(path, "stage2_validation.json"))
}

finalize_scenario <- function(scenario, experiment_id) {
  config <- benchmark_config(scenario, experiment_id)
  groups <- benchmark_groups(config)
  classifications <- list()
  candidates <- list()
  group_runs <- list()
  failures <- list()
  for (group in groups) {
    path <- group_directory(config, group$group_id)
    stage1_path <- file.path(path, "stage1_normalized.json")
    stage2_path <- file.path(path, "stage2_normalized.json")
    if (file.exists(stage1_path)) classifications <- c(classifications, jsonlite::read_json(stage1_path, simplifyVector = FALSE))
    if (file.exists(stage2_path)) {
      candidates <- c(candidates, jsonlite::read_json(stage2_path, simplifyVector = FALSE))
      group_runs[[length(group_runs) + 1L]] <- list(
        group_id = group$group_id,
        concept_ids = vapply(group$concepts, `[[`, character(1), "concept_id"),
        stage1 = jsonlite::read_json(file.path(path, "stage1_validation.json"), simplifyVector = FALSE),
        stage2 = jsonlite::read_json(file.path(path, "stage2_validation.json"), simplifyVector = FALSE)
      )
    } else {
      failure_path <- file.path(path, "stage2_validation_failure.json")
      failure <- if (file.exists(failure_path)) jsonlite::read_json(failure_path, simplifyVector = FALSE) else list(failure_reason = "缺少第二阶段结果。")
      failure$group_id <- group$group_id
      failure$concept_ids <- vapply(group$concepts, `[[`, character(1), "concept_id")
      failures[[length(failures) + 1L]] <- failure
    }
  }
  save_recommendations_v02(
    classifications, candidates, config, "codex_subagent", "same-codex-subagent-configuration",
    group_runs = group_runs,
    run_status = if (length(failures)) "completed_with_rejections" else "completed",
    group_failures = failures
  )
  review_against_gold(config)
  evaluate_recommendations(config)
}

if (action == "prepare") {
  prepare_benchmark(argument_value("--experiment-id"))
} else if (action == "validate-stage1") {
  validate_stage1(
    argument_value("--scenario"), argument_value("--experiment-id"), argument_value("--group"),
    argument_value("--response-file"), argument_value("--task-id")
  )
} else if (action == "prepare-stage2") {
  prepare_stage2(argument_value("--scenario"), argument_value("--experiment-id"), argument_value("--group"))
} else if (action == "validate-stage2") {
  validate_stage2(
    argument_value("--scenario"), argument_value("--experiment-id"), argument_value("--group"),
    argument_value("--response-file"), argument_value("--task-id")
  )
} else if (action == "finalize") {
  finalize_scenario(argument_value("--scenario"), argument_value("--experiment-id"))
} else {
  cat("Actions: prepare, validate-stage1, prepare-stage2, validate-stage2, finalize\n")
}
