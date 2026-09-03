# TraceSDTM 0.4 one-shot Codex subagent benchmark orchestration -------------

v04_benchmark_scenarios <- c("basic", "intermediate", "advanced")
v04_benchmark_domains <- c("DM", "AE", "VS")

v04_absolute_or_project_path <- function(path) {
  path <- as.character(path)
  if (grepl("^[A-Za-z]:[/\\\\]", path) || startsWith(path, "/")) path else trace_path(path)
}

v04_validate_experiment_id <- function(experiment_id) {
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", experiment_id)) {
    trace_abort("实验编号只能包含字母、数字、点、下划线和连字符。")
  }
  as.character(experiment_id)
}

v04_benchmark_config <- function(scenario, experiment_id) {
  if (!scenario %in% v04_benchmark_scenarios) {
    trace_abort(sprintf("未知场景：%s。", scenario))
  }
  experiment_id <- v04_validate_experiment_id(experiment_id)
  config <- load_project_config(scenario)
  base <- file.path("output", "benchmark", "v2", "experiments", experiment_id, scenario)
  generated <- c(
    profile_dir = "profile", recommendation_dir = "recommendations",
    review_dir = "review", csv_dir = file.path("sdtm", "csv"),
    xpt_dir = file.path("sdtm", "xpt"), lineage_dir = "lineage",
    local_validation_dir = file.path("validation", "local"),
    p21_validation_dir = file.path("validation", "p21"), report_dir = "report",
    manifest_dir = "manifests", log_dir = "logs"
  )
  config$paths$output_base <- base
  for (key in names(generated)) config$paths[[key]] <- file.path(base, generated[[key]])
  config$paths$approved_specification <- file.path(base, "specs", "approved_mapping.yml")
  config$project$experiment_id <- experiment_id
  config
}

v04_experiment_group <- function(config, domain) {
  domain <- toupper(as.character(domain))
  if (!domain %in% v04_benchmark_domains) trace_abort(sprintf("未知域：%s。", domain))
  groups <- recommendation_groups_v04(load_mapping_template(config))
  matches <- Filter(function(x) identical(as.character(x$target_domain), domain), groups)
  if (length(matches) != 1L) trace_abort(sprintf("%s 应恰好对应一个原子任务组。", domain))
  matches[[1]]
}

v04_group_evidence_dir <- function(config, group) {
  ensure_dir(file.path(
    v04_absolute_or_project_path(config$paths$recommendation_dir), "groups", as.character(group$group_id)
  ))
}

v04_stage_evidence_dir <- function(config, group, stage) {
  ensure_dir(file.path(v04_group_evidence_dir(config, group), stage))
}

v04_read_text_bytes <- function(path) {
  if (!file.exists(path)) trace_abort(sprintf("文件不存在：%s", path))
  size <- file.info(path)$size
  raw <- readBin(path, what = "raw", n = size)
  list(raw = raw, text = enc2utf8(rawToChar(raw)), sha256 = digest::digest(raw, algo = "sha256"))
}

v04_assert_no_secret <- function(text) {
  key <- Sys.getenv("TRACE_SDTM_API_KEY", unset = "")
  if (nzchar(key) && grepl(key, text, fixed = TRUE)) {
    trace_abort("待冻结内容包含接口密钥，已拒绝写入实验产物。")
  }
  invisible(TRUE)
}

v04_freeze_text <- function(text, path) {
  text <- enc2utf8(as.character(text))
  v04_assert_no_secret(text)
  bytes <- charToRaw(text)
  ensure_parent(path)
  if (file.exists(path)) {
    existing <- v04_read_text_bytes(path)
    incoming <- digest::digest(bytes, algo = "sha256")
    if (!identical(existing$sha256, incoming)) {
      trace_abort(sprintf("冻结文件已存在且内容不同，拒绝覆盖：%s", path))
    }
    return(existing$sha256)
  }
  connection <- file(path, open = "wb")
  on.exit(close(connection), add = TRUE)
  writeBin(bytes, connection)
  digest::digest(bytes, algo = "sha256")
}

v04_freeze_file <- function(source, destination) {
  content <- v04_read_text_bytes(source)
  v04_assert_no_secret(content$text)
  ensure_parent(destination)
  if (file.exists(destination)) {
    existing <- v04_read_text_bytes(destination)
    if (!identical(existing$sha256, content$sha256)) {
      trace_abort(sprintf("首次响应已冻结，拒绝用不同内容覆盖：%s", destination))
    }
    return(existing)
  }
  connection <- file(destination, open = "wb")
  on.exit(close(connection), add = TRUE)
  writeBin(content$raw, connection)
  content
}

v04_git_state <- function() {
  commit <- tryCatch(
    system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE),
    error = function(error) character()
  )
  dirty <- tryCatch(
    system2("git", c("status", "--porcelain"), stdout = TRUE, stderr = FALSE),
    error = function(error) character()
  )
  list(commit = as.character(commit[[1]] %||% "unavailable"), dirty = length(dirty) > 0L)
}

v04_benchmark_checksums <- function(config) {
  paths <- c(
    task_specification = config$paths$specification_template,
    mapping_policy = config$paths$mapping_policies,
    target_metadata = config$paths$metadata,
    transform_registry = config$paths$transform_registry,
    controlled_terminology = config$paths$controlled_terminology,
    unit_conversions = config$paths$unit_conversions,
    gold_specification = config$paths$gold_specification,
    source_profile = file.path(config$paths$profile_dir, "source_dictionary.csv")
  )
  configuration <- stats::setNames(
    lapply(paths, function(path) file_sha256(trace_path(path))), names(paths)
  )
  code_paths <- c(
    recommend = "R/recommend_v04.R", parameter_resolvers = "R/parameter_resolvers.R",
    evaluator = "R/evaluate_v04.R", orchestrator = "R/experiment_v04.R"
  )
  code <- stats::setNames(
    lapply(code_paths, function(path) file_sha256(trace_path(path))), names(code_paths)
  )
  list(configuration = configuration, code = code, git = v04_git_state())
}

v04_experiment_dictionary <- function(config) {
  path <- file.path(v04_absolute_or_project_path(config$paths$profile_dir), "source_dictionary.csv")
  if (!file.exists(path)) profile_sources(config)
  character_columns <- c(
    "source_domain", "source_dataset", "source_variable", "label", "data_type",
    "example_values", "form_name", "grain", "keys", "concept_roles",
    "format_candidates", "partial_tokens"
  )
  readr::read_csv(path, show_col_types = FALSE, na = c(""), progress = FALSE) |>
    dplyr::mutate(dplyr::across(
      dplyr::any_of(character_columns), ~ tidyr::replace_na(as.character(.x), "")
    ))
}

v04_blind_request <- function(prompt, conditional = FALSE) {
  heading <- if (isTRUE(conditional)) {
    "这是条件评价请求。端到端首次响应已经冻结；本请求给定正确目标，仅评价函数选择。"
  } else {
    "这是单次独立盲评请求。"
  }
  paste(
    heading,
    "不得调用任何工具、读取文件、访问网络或联系其他代理。",
    "只能依据下面的冻结请求作答；不得请求提示、修正或重试。",
    "只返回请求规定的 JSON 对象，不要使用 Markdown 代码块。",
    prompt,
    sep = "\n"
  )
}

v04_write_request_manifest <- function(config, group, stage, stage_dir, request_sha256,
                                       parent_hashes = list(), conditional = FALSE) {
  manifest <- list(
    schema_version = "0.4", protocol = "fresh_codex_subagent_first_response",
    scenario = config$project$scenario, domain = group$target_domain,
    group_id = group$group_id,
    task_ids = as.list(vapply(v04_group_tasks(group), task_id_v04, character(1))),
    task_count = length(v04_group_tasks(group)), stage = stage,
    conditional = isTRUE(conditional), request_sha256 = request_sha256,
    parent_hashes = parent_hashes, prepared_at = utc_now(),
    checksums = v04_benchmark_checksums(config), api_key_used = FALSE,
    isolation_limit = "过程约束，不属于操作系统级隔离"
  )
  write_json(manifest, file.path(stage_dir, "request_manifest.json"))
  invisible(manifest)
}

v04_prepare_target_request <- function(scenario, domain, experiment_id) {
  config <- v04_benchmark_config(scenario, experiment_id)
  dictionary <- v04_experiment_dictionary(config)
  specification <- load_mapping_template(config)
  group <- v04_experiment_group(config, domain)
  prompt <- target_prompt_v04(
    group, specification, load_metadata(config), load_mapping_policies(config), dictionary
  )
  prompt <- v04_blind_request(prompt)
  stage_dir <- v04_stage_evidence_dir(config, group, "targets")
  hash <- v04_freeze_text(prompt, file.path(stage_dir, "request.txt"))
  write_json(
    lapply(v04_group_tasks(group), v04_task_context,
           specification = specification, dictionary = dictionary),
    file.path(stage_dir, "request_context.json")
  )
  v04_write_request_manifest(config, group, "targets", stage_dir, hash)
  invisible(file.path(stage_dir, "request.txt"))
}

v04_read_stage_result <- function(stage_dir, filename = "normalized.json") {
  path <- file.path(stage_dir, filename)
  if (!file.exists(path)) trace_abort(sprintf("缺少已校验阶段结果：%s", path))
  jsonlite::read_json(path, simplifyVector = FALSE)
}

v04_write_response_evidence <- function(stage_dir, config, group, stage, response,
                                        normalized = NULL, error = NULL, subagent_task) {
  normalized_sha <- if (is.null(normalized)) NA_character_ else
    digest::digest(registry_json(normalized), algo = "sha256", serialize = FALSE)
  evidence <- list(
    schema_version = "0.4", scenario = config$project$scenario,
    domain = group$target_domain, group_id = group$group_id, stage = stage,
    validation_status = if (!is.null(error)) "failed" else if (length(normalized$failures %||% list()))
      "passed_with_task_failures" else "passed",
    failure_reason = if (is.null(error)) NULL else sanitize_for_log(conditionMessage(error)),
    request_sha256 = file_sha256(file.path(stage_dir, "request.txt")),
    response_sha256 = response$sha256, normalized_sha256 = normalized_sha,
    valid_count = length(normalized$valid %||% list()),
    failure_count = length(normalized$failures %||% list()),
    subagent_task = as.character(subagent_task), received_at = utc_now(),
    attempt_number = 1L, retry_performed = FALSE,
    checksums = v04_benchmark_checksums(config), api_key_logged = FALSE
  )
  write_json(evidence, file.path(stage_dir, "response_evidence.json"))
  invisible(evidence)
}

v04_import_first_response <- function(config, group, stage, response_file,
                                      subagent_task, parser) {
  stage_dir <- v04_stage_evidence_dir(config, group, stage)
  if (!file.exists(file.path(stage_dir, "request.txt"))) {
    trace_abort(sprintf("%s 请求尚未冻结。", stage))
  }
  response <- v04_freeze_file(response_file, file.path(stage_dir, "response_raw.txt"))
  parsed_json <- tryCatch(
    jsonlite::fromJSON(extract_json_content(response$text), simplifyVector = FALSE),
    error = identity
  )
  if (inherits(parsed_json, "error")) {
    normalized <- list(
      valid = list(),
      failures = lapply(v04_group_tasks(group), function(task) list(
        task_id = task_id_v04(task), stage = stage,
        error = sanitize_for_log(conditionMessage(parsed_json)), record_index = NA_integer_
      )),
      stage = stage, fatal_structure_error = TRUE
    )
    write_json(normalized, file.path(stage_dir, "normalized.json"))
    v04_write_response_evidence(
      stage_dir, config, group, stage, response,
      normalized = normalized, error = parsed_json,
      subagent_task = subagent_task
    )
    return(invisible(normalized))
  }
  normalized <- tryCatch(parser(parsed_json), error = identity)
  if (inherits(normalized, "error")) {
    parse_error <- normalized
    normalized <- list(
      valid = list(),
      failures = lapply(v04_group_tasks(group), function(task) list(
        task_id = task_id_v04(task), stage = stage,
        error = sanitize_for_log(conditionMessage(parse_error)), record_index = NA_integer_
      )),
      stage = stage, fatal_structure_error = TRUE
    )
    write_json(normalized, file.path(stage_dir, "normalized.json"))
    v04_write_response_evidence(
      stage_dir, config, group, stage, response,
      normalized = normalized, error = parse_error,
      subagent_task = subagent_task
    )
    return(invisible(normalized))
  }
  repeated <- parser(parsed_json)
  first_hash <- digest::digest(registry_json(normalized), algo = "sha256", serialize = FALSE)
  repeated_hash <- digest::digest(registry_json(repeated), algo = "sha256", serialize = FALSE)
  if (!identical(first_hash, repeated_hash)) trace_abort(sprintf("%s 重复解析结果不一致。", stage))
  write_json(normalized, file.path(stage_dir, "normalized.json"))
  v04_write_response_evidence(
    stage_dir, config, group, stage, response, normalized = normalized,
    subagent_task = subagent_task
  )
  invisible(normalized)
}

v04_expected_subagent_task <- function(config, group) {
  path <- file.path(v04_stage_evidence_dir(config, group, "targets"), "response_evidence.json")
  if (!file.exists(path)) trace_abort("目标阶段首次响应尚未冻结和校验。")
  as.character(jsonlite::read_json(path, simplifyVector = FALSE)$subagent_task)
}

v04_enforce_same_subagent <- function(config, group, subagent_task) {
  expected <- v04_expected_subagent_task(config, group)
  if (!identical(as.character(subagent_task), expected)) {
    trace_abort(sprintf("同一域必须由同一子代理完成；期望 %s，实际 %s。", expected, subagent_task))
  }
  invisible(TRUE)
}

v04_import_targets <- function(scenario, domain, experiment_id, response_file, subagent_task) {
  config <- v04_benchmark_config(scenario, experiment_id)
  group <- v04_experiment_group(config, domain)
  metadata <- load_metadata(config)
  v04_import_first_response(
    config, group, "targets", response_file, subagent_task,
    function(value) parse_target_decisions_v04(value, group, metadata)
  )
}

v04_prepare_function_request <- function(scenario, domain, experiment_id,
                                         conditional = FALSE) {
  config <- v04_benchmark_config(scenario, experiment_id)
  group <- v04_experiment_group(config, domain)
  specification <- load_mapping_template(config)
  metadata <- load_metadata(config)
  registry <- load_transform_registry(config)
  dictionary <- v04_experiment_dictionary(config)
  target_dir <- v04_stage_evidence_dir(config, group, "targets")
  targets <- if (isTRUE(conditional)) {
    cascade_dir <- v04_stage_evidence_dir(config, group, "functions_cascade")
    if (!file.exists(file.path(cascade_dir, "response_raw.txt")) ||
        !file.exists(file.path(cascade_dir, "response_evidence.json"))) {
      trace_abort("只有端到端函数响应冻结后，才能生成条件函数请求。")
    }
    records <- v04_seed_target_records(group, load_gold_specification(config), registry)
    records <- lapply(records, function(x) {
      x$evidence <- list("条件评价给定正确目标。")
      x$uncertainties <- list()
      x
    })
    result <- parse_target_decisions_v04(list(target_identifications = records), group, metadata)
    write_json(result, file.path(v04_group_evidence_dir(config, group), "conditional_target_decisions.json"))
    result
  } else {
    v04_read_stage_result(target_dir)
  }
  if (!isTRUE(conditional) && !length(targets$valid %||% list())) {
    stage <- "functions_cascade"
    stage_dir <- v04_stage_evidence_dir(config, group, stage)
    result <- list(
      valid = list(), failures = targets$failures %||% list(), stage = stage,
      skipped = TRUE, reason = "目标阶段没有通过严格校验的任务。"
    )
    write_json(result, file.path(stage_dir, "normalized.json"))
    write_json(list(
      schema_version = "0.4", scenario = config$project$scenario,
      domain = group$target_domain, stage = stage, request_required = FALSE,
      reason = result$reason, prepared_at = utc_now(),
      checksums = v04_benchmark_checksums(config)
    ), file.path(stage_dir, "request_manifest.json"))
    return(invisible(NULL))
  }
  prompt <- function_prompt_v04(
    group, targets, registry, specification, load_mapping_policies(config), dictionary
  )
  prompt <- v04_blind_request(prompt, conditional = conditional)
  stage <- if (isTRUE(conditional)) "functions_conditional" else "functions_cascade"
  stage_dir <- v04_stage_evidence_dir(config, group, stage)
  hash <- v04_freeze_text(prompt, file.path(stage_dir, "request.txt"))
  parent_hashes <- list(
    target_response = file_sha256(file.path(target_dir, "response_raw.txt")),
    cascade_function_response = if (isTRUE(conditional))
      file_sha256(file.path(v04_stage_evidence_dir(config, group, "functions_cascade"), "response_raw.txt")) else NULL
  )
  v04_write_request_manifest(
    config, group, stage, stage_dir, hash, parent_hashes,
    conditional = conditional
  )
  invisible(file.path(stage_dir, "request.txt"))
}

v04_import_functions <- function(scenario, domain, experiment_id, response_file,
                                 subagent_task, conditional = FALSE) {
  config <- v04_benchmark_config(scenario, experiment_id)
  group <- v04_experiment_group(config, domain)
  v04_enforce_same_subagent(config, group, subagent_task)
  metadata <- load_metadata(config)
  registry <- load_transform_registry(config)
  targets <- if (isTRUE(conditional)) {
    v04_read_stage_result(v04_group_evidence_dir(config, group), "conditional_target_decisions.json")
  } else {
    v04_read_stage_result(v04_stage_evidence_dir(config, group, "targets"))
  }
  stage <- if (isTRUE(conditional)) "functions_conditional" else "functions_cascade"
  v04_import_first_response(
    config, group, stage, response_file, subagent_task,
    function(value) parse_function_selections_v04(value, group, targets, registry)
  )
}

v04_parameter_stage_name <- function(conditional) {
  if (isTRUE(conditional)) "parameters_conditional" else "parameters_cascade"
}

v04_prepare_parameter_request <- function(scenario, domain, experiment_id,
                                          conditional = FALSE) {
  config <- v04_benchmark_config(scenario, experiment_id)
  group <- v04_experiment_group(config, domain)
  specification <- load_mapping_template(config)
  registry <- load_transform_registry(config)
  targets <- if (isTRUE(conditional)) {
    v04_read_stage_result(v04_group_evidence_dir(config, group), "conditional_target_decisions.json")
  } else {
    v04_read_stage_result(v04_stage_evidence_dir(config, group, "targets"))
  }
  function_stage <- if (isTRUE(conditional)) "functions_conditional" else "functions_cascade"
  functions <- v04_read_stage_result(v04_stage_evidence_dir(config, group, function_stage))
  resources <- list(
    controlled_terminology = load_controlled_terminology(config),
    unit_conversions = load_unit_conversions(config)
  )
  resolutions <- resolve_known_parameters_v04(
    specification, targets, functions, registry, load_mapping_policies(config), resources
  )
  stage <- v04_parameter_stage_name(conditional)
  stage_dir <- v04_stage_evidence_dir(config, group, stage)
  write_json(resolutions, file.path(stage_dir, "resolutions.json"))
  prompt <- parameter_prompt_v04(resolutions)
  if (is.null(prompt)) {
    result <- list(
      valid = list(), failures = list(), stage = "parameter_completion",
      skipped = TRUE, provider = "automatic_injection", resolutions = resolutions
    )
    write_json(result, file.path(stage_dir, "normalized.json"))
    write_json(list(
      schema_version = "0.4", scenario = scenario, domain = group$target_domain,
      stage = stage, request_required = FALSE,
      resolutions_sha256 = file_sha256(file.path(stage_dir, "resolutions.json")),
      prepared_at = utc_now(), checksums = v04_benchmark_checksums(config)
    ), file.path(stage_dir, "request_manifest.json"))
    return(invisible(NULL))
  }
  prompt <- v04_blind_request(prompt)
  hash <- v04_freeze_text(prompt, file.path(stage_dir, "request.txt"))
  v04_write_request_manifest(
    config, group, stage, stage_dir, hash,
    list(function_response = file_sha256(file.path(
      v04_stage_evidence_dir(config, group, function_stage), "response_raw.txt"
    )), resolutions = file_sha256(file.path(stage_dir, "resolutions.json"))),
    conditional = conditional
  )
  invisible(file.path(stage_dir, "request.txt"))
}

v04_import_parameters <- function(scenario, domain, experiment_id, response_file,
                                  subagent_task, conditional = FALSE) {
  config <- v04_benchmark_config(scenario, experiment_id)
  group <- v04_experiment_group(config, domain)
  v04_enforce_same_subagent(config, group, subagent_task)
  stage <- v04_parameter_stage_name(conditional)
  stage_dir <- v04_stage_evidence_dir(config, group, stage)
  resolutions_path <- file.path(stage_dir, "resolutions.json")
  if (!file.exists(resolutions_path)) trace_abort("请先执行参数请求准备。")
  resolutions <- jsonlite::read_json(resolutions_path, simplifyVector = FALSE)
  normalized <- v04_import_first_response(
    config, group, stage, response_file, subagent_task,
    function(value) parse_parameter_selections_v04(value, resolutions)
  )
  normalized$provider <- "codex_subagent"
  normalized$resolutions <- resolutions
  write_json(normalized, file.path(stage_dir, "normalized.json"))
  invisible(normalized)
}

v04_read_or_prepare_parameters <- function(scenario, domain, experiment_id,
                                           conditional = FALSE) {
  config <- v04_benchmark_config(scenario, experiment_id)
  group <- v04_experiment_group(config, domain)
  stage_dir <- v04_stage_evidence_dir(config, group, v04_parameter_stage_name(conditional))
  result_path <- file.path(stage_dir, "normalized.json")
  if (!file.exists(result_path)) {
    v04_prepare_parameter_request(scenario, domain, experiment_id, conditional)
  }
  if (!file.exists(result_path)) {
    trace_abort(sprintf("%s 的参数阶段需要子代理响应，尚不能汇总。", domain))
  }
  jsonlite::read_json(result_path, simplifyVector = FALSE)
}

v04_merge_parameter_results <- function(results) {
  valid <- do.call(base::c, unname(lapply(results, function(x) x$valid %||% list())))
  failures <- unname(do.call(base::c, unname(lapply(results, function(x) x$failures %||% list()))))
  resolutions <- list(
    valid = do.call(base::c, unname(lapply(results, function(x) x$resolutions$valid %||% list()))),
    failures = unname(do.call(base::c, unname(lapply(results, function(x) x$resolutions$failures %||% list())))),
    stage = "known_parameter_resolution"
  )
  list(
    valid = valid, failures = failures, stage = "parameter_completion",
    skipped = all(vapply(results, function(x) isTRUE(x$skipped), logical(1))),
    provider = "codex_subagent", resolutions = resolutions
  )
}

v04_finalize_scenario <- function(scenario, experiment_id) {
  config <- v04_benchmark_config(scenario, experiment_id)
  groups <- lapply(v04_benchmark_domains, function(domain) v04_experiment_group(config, domain))
  targets <- list(); conditional_targets <- list()
  functions <- list(); conditional_functions <- list()
  parameters <- list(); conditional_parameters <- list(); group_runs <- list()
  for (index in seq_along(groups)) {
    group <- groups[[index]]
    domain <- group$target_domain
    group_dir <- v04_group_evidence_dir(config, group)
    targets[[domain]] <- v04_read_stage_result(file.path(group_dir, "targets"))
    conditional_targets[[domain]] <- v04_read_stage_result(
      group_dir, "conditional_target_decisions.json"
    )
    functions[[domain]] <- v04_read_stage_result(file.path(group_dir, "functions_cascade"))
    conditional_functions[[domain]] <- v04_read_stage_result(file.path(group_dir, "functions_conditional"))
    parameters[[domain]] <- v04_read_or_prepare_parameters(scenario, domain, experiment_id, FALSE)
    conditional_parameters[[domain]] <- v04_read_or_prepare_parameters(scenario, domain, experiment_id, TRUE)
    group_runs[[domain]] <- list(
      domain = domain, group_id = group$group_id,
      subagent_task = v04_expected_subagent_task(config, group),
      target = jsonlite::read_json(file.path(group_dir, "targets", "response_evidence.json"), simplifyVector = FALSE),
      function_cascade = jsonlite::read_json(file.path(group_dir, "functions_cascade", "response_evidence.json"), simplifyVector = FALSE),
      function_conditional = jsonlite::read_json(file.path(group_dir, "functions_conditional", "response_evidence.json"), simplifyVector = FALSE)
    )
  }
  targets <- v04_merge_stage_results(targets, "target_identification")
  conditional_targets <- v04_merge_stage_results(
    conditional_targets, "conditional_target_identification"
  )
  functions <- v04_merge_stage_results(functions, "function_selection")
  conditional_functions <- v04_merge_stage_results(conditional_functions, "conditional_function_selection")
  parameters <- v04_merge_parameter_results(parameters)
  conditional_parameters <- v04_merge_parameter_results(conditional_parameters)
  recommendation_dir <- trace_path(config$paths$recommendation_dir)
  v04_write_stage(targets, recommendation_dir, "target_decisions.json")
  v04_write_stage(functions, recommendation_dir, "function_candidates.json")
  v04_write_stage(conditional_functions, recommendation_dir, "conditional_function_candidates.json")
  v04_write_stage(parameters, recommendation_dir, "parameter_completions.json")
  v04_write_stage(conditional_parameters, recommendation_dir, "conditional_parameter_completions.json")
  assembled <- assemble_recommendations_v04(
    targets, functions, parameters, config = config,
    recommendation_dir = recommendation_dir
  )
  conditional_assembled <- assemble_recommendations_v04(
    conditional_targets, conditional_functions, conditional_parameters,
    config = config,
    recommendation_dir = recommendation_dir
  )
  v04_write_stage(
    conditional_assembled, recommendation_dir,
    "conditional_assembled_recommendations.json"
  )
  # Re-write the cascading artifact because the conditional assembly uses the
  # same stable assembly helper and therefore the same default filename.
  v04_write_stage(assembled, recommendation_dir, "assembled_recommendations.json")
  evaluation_dir <- ensure_dir(file.path(trace_path(config$paths$output_base), "evaluation"))
  evaluation <- evaluate_three_stage_v04(
    config, targets, functions, parameters, assembled,
    conditional_functions, conditional_parameters, output_dir = evaluation_dir
  )
  conditional_evaluation <- evaluate_three_stage_v04(
    config, conditional_targets, conditional_functions,
    conditional_parameters, conditional_assembled,
    output_dir = ensure_dir(file.path(evaluation_dir, "conditional"))
  )
  manifest <- list(
    schema_version = "0.4", experiment_id = experiment_id, scenario = scenario,
    provider = "codex_subagent", protocol = "first_response_no_retry",
    task_count = length(prepare_tasks_v04(load_mapping_template(config))),
    group_runs = group_runs,
    cascading_result = evaluation$summary,
    conditional_result = conditional_evaluation$summary,
    checksums = v04_benchmark_checksums(config), finalized_at = utc_now(),
    api_key_used = FALSE,
    isolation_limit = "fork_turns=none 与任务约束是过程盲法，不属于操作系统级隔离"
  )
  write_json(manifest, file.path(trace_path(config$paths$manifest_dir), "experiment_manifest.json"))
  invisible(list(
    targets = targets, functions = functions, parameters = parameters,
    assembled = assembled, conditional_functions = conditional_functions,
    conditional_parameters = conditional_parameters, evaluation = evaluation,
    conditional_targets = conditional_targets,
    conditional_assembled = conditional_assembled,
    conditional_evaluation = conditional_evaluation,
    manifest = manifest
  ))
}
