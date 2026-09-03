#!/usr/bin/env Rscript

# Re-score the frozen v0.3 benchmark with the schema-driven parameter
# equivalence rules introduced in v0.4.  Historical v1 artifacts are read-only;
# every generated file is written below output/benchmark/v2/reports.

arguments <- commandArgs(trailingOnly = FALSE)
file_argument <- sub("^--file=", "", arguments[grepl("^--file=", arguments)])
if (!length(file_argument)) stop("无法确定脚本位置。", call. = FALSE)
project_root <- normalizePath(file.path(dirname(file_argument[[1]]), "..", ".."), winslash = "/", mustWork = TRUE)
Sys.setenv(TRACE_SDTM_ROOT = project_root, TRACE_SDTM_EXPERIMENT_ID = "")
project_library <- file.path(project_root, ".Rlib")
if (dir.exists(project_library)) .libPaths(c(project_library, .libPaths()))
for (file in c(
  "utils.R", "config.R", "registry.R", "profile.R", "recommend.R", "review.R",
  "transforms.R", "build.R", "validate_local.R", "p21.R", "evaluate.R"
)) source(file.path(project_root, "R", file), encoding = "UTF-8")

args <- commandArgs(trailingOnly = TRUE)
argument_value <- function(name, default = NULL) {
  position <- match(name, args)
  if (is.na(position)) return(default)
  if (position == length(args)) stop(sprintf("%s 后必须提供值。", name), call. = FALSE)
  args[[position + 1L]]
}

source_experiment <- argument_value("--source-experiment", "codex-three-level-20260902")
report_id <- argument_value("--report-id", source_experiment)
if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", source_experiment) ||
    !grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", report_id)) {
  stop("实验编号只能包含字母、数字、点、下划线和连字符。", call. = FALSE)
}

scenarios <- c("basic", "intermediate", "advanced")
level_labels <- c(basic = "基础", intermediate = "中等", advanced = "高级")
historical_paths <- list(
  basic = list(
    specification_template = "specs/benchmark/v1/basic_concepts.yml",
    gold_specification = "specs/benchmark/v1/basic_gold.yml",
    mapping_policies = "specs/benchmark/v1/basic_policies.yml",
    output_base = "output/benchmark/v1/basic"
  ),
  intermediate = list(
    specification_template = "specs/benchmark/v1/intermediate_concepts.yml",
    gold_specification = "specs/benchmark/v1/intermediate_gold.yml",
    mapping_policies = "specs/benchmark/v1/intermediate_policies.yml",
    output_base = "output/benchmark/v1/intermediate"
  ),
  advanced = list(
    specification_template = "specs/v0.2/advanced_concepts.yml",
    gold_specification = "specs/v0.2/advanced_gold.yml",
    mapping_policies = "specs/benchmark/v1/advanced_policies.yml",
    output_base = "output/benchmark/v1/advanced"
  )
)

historical_config <- function(scenario) {
  config <- load_project_config(scenario)
  paths <- historical_paths[[scenario]]
  config$paths$specification_template <- paths$specification_template
  config$paths$gold_specification <- paths$gold_specification
  config$paths$mapping_policies <- paths$mapping_policies
  config$paths$output_base <- paths$output_base
  config$paths$recommendation_dir <- file.path(
    paths$output_base, "experiments", source_experiment, "recommendations"
  )
  config$project$experiment_id <- source_experiment
  config
}

missing_candidate_row <- function(concept, expected, registry) {
  signature <- plan_signature_v02(expected, registry)
  tibble::tibble(
    concept_id = concept$concept_id, candidate_rank = 1L,
    target_domain = concept$target_domain, form_name = concept$form_name,
    complexity = as.character(concept$difficulty %||% concept_complexity_v02(expected, registry)),
    gold_categories = paste(signature$categories, collapse = " | "), proposed_categories = "",
    category_chain_correct = FALSE, function_chain_correct = FALSE,
    source_chain_correct = FALSE, target_set_complete = FALSE,
    missing_targets = paste(signature$targets, collapse = " | "), extra_targets = "",
    parameter_values_correct = FALSE, complete_plan_correct = FALSE,
    modification_grade = "major", strictly_validated = FALSE,
    parameter_schema_valid = FALSE, recommendation_score = NA_real_, reason = "",
    uncertainties = "", status = "group_rejected_or_missing", provenance = "",
    group_id = ""
  )
}

rescore_scenario <- function(scenario) {
  config <- historical_config(scenario)
  recommendation_dir <- trace_path(config$paths$recommendation_dir)
  candidate_path <- file.path(recommendation_dir, "candidate_plans.csv")
  classification_path <- file.path(recommendation_dir, "classification_evaluation.csv")
  summary_path <- file.path(recommendation_dir, "mapping_evaluation_summary.json")
  required <- c(candidate_path, classification_path, summary_path)
  if (any(!file.exists(required))) {
    stop(sprintf("%s 历史实验不完整：%s", scenario, paste(required[!file.exists(required)], collapse = "、")), call. = FALSE)
  }
  specification <- load_mapping_template(config)
  gold <- load_gold_specification(config)
  registry <- load_transform_registry(config)
  candidates <- readr::read_csv(candidate_path, show_col_types = FALSE)
  classification <- readr::read_csv(classification_path, show_col_types = FALSE)
  original_summary <- jsonlite::read_json(summary_path, simplifyVector = TRUE)
  evaluated <- suppressWarnings(candidate_evaluation_rows_v02(candidates, specification, gold, registry))
  concepts <- concept_lookup(specification)
  concept_ids <- names(concepts)
  top_one <- dplyr::filter(evaluated, .data$candidate_rank == 1L)
  absent <- setdiff(concept_ids, top_one$concept_id)
  if (length(absent)) {
    missing <- purrr::map_dfr(absent, function(id) {
      missing_candidate_row(concepts[[id]], gold_plan_steps(gold$plans[[id]]), registry)
    })
    top_one <- dplyr::bind_rows(top_one, missing)
  }
  top_one <- top_one |>
    dplyr::mutate(
      scenario = .env$scenario,
      level = unname(.env$level_labels[[.env$scenario]]),
      concept_id = factor(.data$concept_id, levels = .env$concept_ids),
      .before = 1L
    ) |>
    dplyr::arrange(.data$concept_id) |>
    dplyr::mutate(concept_id = as.character(.data$concept_id))
  top_three <- evaluated |>
    dplyr::filter(.data$candidate_rank <= 3L) |>
    dplyr::group_by(.data$concept_id) |>
    dplyr::summarise(
      complete_plan_top3 = any(.data$complete_plan_correct),
      .groups = "drop"
    ) |>
    dplyr::right_join(tibble::tibble(concept_id = concept_ids), by = "concept_id") |>
    dplyr::mutate(complete_plan_top3 = tidyr::replace_na(.data$complete_plan_top3, FALSE))
  top_one <- dplyr::left_join(top_one, top_three, by = "concept_id")
  corrected_summary <- list(
    scenario = scenario,
    level = unname(level_labels[[scenario]]),
    denominator = length(concept_ids),
    valid_classification = sum(!is.na(classification$categories)),
    category_chain_correct = sum(classification$classification_correct, na.rm = TRUE),
    candidate_coverage = sum(top_one$strictly_validated),
    semantic_target_complete = sum(top_one$target_set_complete),
    function_chain_correct = sum(top_one$function_chain_correct),
    source_chain_correct = sum(top_one$source_chain_correct),
    parameter_values_correct = sum(top_one$parameter_values_correct),
    complete_plan_top1_correct = sum(top_one$complete_plan_correct),
    complete_plan_top3_correct = sum(top_one$complete_plan_top3),
    original_complete_plan_top1_correct = as.integer(original_summary$complete_plan_top1_correct %||% NA_integer_)
  )
  input_paths <- c(
    candidate_path, classification_path, summary_path,
    trace_path(config$paths$specification_template), trace_path(config$paths$gold_specification),
    trace_path(config$paths$transform_registry)
  )
  manifest <- tibble::tibble(
    scenario = scenario,
    role = c("frozen_candidates", "frozen_classification_evaluation", "original_summary", "frozen_specification", "frozen_gold", "current_registry"),
    path = normalizePath(input_paths, winslash = "/", mustWork = TRUE),
    sha256 = vapply(input_paths, file_sha256, character(1)),
    access = "read_only"
  )
  list(detail = top_one, classification = dplyr::mutate(
    classification,
    scenario = .env$scenario,
    level = unname(.env$level_labels[[.env$scenario]]),
    .before = 1L
  ),
       summary = corrected_summary, manifest = manifest)
}

results <- stats::setNames(lapply(scenarios, rescore_scenario), scenarios)
detail <- dplyr::bind_rows(lapply(results, `[[`, "detail"))
classification <- dplyr::bind_rows(lapply(results, `[[`, "classification"))
input_manifest <- dplyr::bind_rows(lapply(results, `[[`, "manifest"))

metric_columns <- c(
  valid_classification = "有效分类", category_chain_correct = "类别链完全一致",
  candidate_coverage = "候选方案覆盖", semantic_target_complete = "目标变量集合完整",
  function_chain_correct = "函数链正确", source_chain_correct = "来源链正确",
  parameter_values_correct = "参数等价且正确", complete_plan_top1_correct = "完整方案首选正确",
  complete_plan_top3_correct = "完整方案前三项命中"
)
metrics <- purrr::imap_dfr(results, function(result, scenario) {
  summary <- result$summary
  purrr::imap_dfr(metric_columns, function(label, field) tibble::tibble(
    scenario = scenario, level = summary$level, metric = label,
    numerator = as.integer(summary[[field]]), denominator = as.integer(summary$denominator),
    proportion = as.integer(summary[[field]]) / as.integer(summary$denominator)
  ))
})
pooled <- metrics |>
  dplyr::group_by(.data$metric) |>
  dplyr::summarise(
    scenario = "all", level = "全部", numerator = sum(.data$numerator),
    denominator = sum(.data$denominator), proportion = .data$numerator / .data$denominator,
    level_equal_weight_mean = mean(.data$proportion), .groups = "drop"
  )
metrics <- dplyr::bind_rows(metrics, pooled)

original_vs_corrected <- purrr::imap_dfr(results, function(result, scenario) {
  summary <- result$summary
  tibble::tibble(
    scenario = scenario, level = summary$level, denominator = summary$denominator,
    original_complete_plan_top1 = summary$original_complete_plan_top1_correct,
    corrected_complete_plan_top1 = summary$complete_plan_top1_correct,
    correction = summary$complete_plan_top1_correct - summary$original_complete_plan_top1_correct,
    interpretation = "冻结响应和金标准未变；仅使用注册表参数模式重新判定 JSON/YAML 等价性。"
  )
})
original_vs_corrected <- dplyr::bind_rows(
  original_vs_corrected,
  original_vs_corrected |>
    dplyr::summarise(
      scenario = "all", level = "全部", denominator = sum(.data$denominator),
      original_complete_plan_top1 = sum(.data$original_complete_plan_top1),
      corrected_complete_plan_top1 = sum(.data$corrected_complete_plan_top1),
      correction = sum(.data$correction), interpretation = dplyr::first(.data$interpretation)
    )
)

report_root <- ensure_dir(trace_path("output", "benchmark", "v2", "reports", report_id, "v03_corrected_baseline"))
write_csv(detail, file.path(report_root, "v03_corrected_detail.csv"))
write_csv(classification, file.path(report_root, "v03_classification_detail.csv"))
write_csv(metrics, file.path(report_root, "v03_corrected_metrics.csv"))
write_csv(original_vs_corrected, file.path(report_root, "v03_original_vs_corrected.csv"))
write_csv(input_manifest, file.path(report_root, "v03_frozen_input_manifest.csv"))
write_json(list(
  schema_version = "0.4-report",
  historical_protocol = "v0.3-three-level-eval",
  source_experiment = source_experiment,
  denominator = nrow(detail),
  levels = lapply(results, `[[`, "summary"),
  pooled_original_complete_plan_top1 = sum(original_vs_corrected$original_complete_plan_top1[original_vs_corrected$scenario != "all"]),
  pooled_corrected_complete_plan_top1 = sum(original_vs_corrected$corrected_complete_plan_top1[original_vs_corrected$scenario != "all"]),
  generated_at = utc_now(),
  method = "冻结 v0.3 响应、任务规格和金标准；用 v0.4 模式驱动规范化比较重算参数和完整方案正确性。",
  source_v1_modified = FALSE
), file.path(report_root, "v03_corrected_summary.json"))
writeLines(c(
  "# v0.3 修正基线", "",
  sprintf("历史实验：`%s`。", source_experiment), "",
  "本目录仅是重新评价结果；`output/benchmark/v1` 内的冻结请求、响应和旧报告均未改写。",
  "修正点是根据函数参数模式比较 JSON 与 YAML：对象键顺序不影响结果，数值按声明类型规范化，数组默认保持顺序，仅注册表声明为集合的参数忽略元素顺序。",
  "该重算不是新的模型试验，不反映长期稳定性。"
), file.path(report_root, "README.md"), useBytes = TRUE)

message("已写入 v0.3 修正基线：", report_root)
