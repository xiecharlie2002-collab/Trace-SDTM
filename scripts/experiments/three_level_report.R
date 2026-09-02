#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = FALSE)
file_argument <- sub("^--file=", "", arguments[grepl("^--file=", arguments)])
if (!length(file_argument)) stop("无法确定报告脚本位置。", call. = FALSE)
project_root <- normalizePath(file.path(dirname(file_argument[[1]]), "..", ".."), winslash = "/", mustWork = TRUE)
Sys.setenv(TRACE_SDTM_ROOT = project_root)
project_library <- file.path(project_root, ".Rlib")
if (dir.exists(project_library)) .libPaths(c(project_library, .libPaths()))
for (file in c(
  "utils.R", "config.R", "registry.R", "profile.R", "recommend.R", "review.R",
  "transforms.R", "build.R", "validate_local.R", "p21.R", "evaluate.R", "report.R"
)) source(file.path(project_root, "R", file), encoding = "UTF-8")

args <- commandArgs(trailingOnly = TRUE)
position <- match("--experiment-id", args)
if (is.na(position) || position == length(args)) stop("缺少 --experiment-id。", call. = FALSE)
experiment_id <- args[[position + 1L]]
scenarios <- c("basic", "intermediate", "advanced")
level_labels <- c(basic = "基础", intermediate = "中等", advanced = "高级")

optional_csv <- function(path) {
  if (!file.exists(path)) return(tibble::tibble())
  readr::read_csv(path, show_col_types = FALSE)
}

optional_json <- function(path, default = list()) {
  if (!file.exists(path)) return(default)
  jsonlite::read_json(path, simplifyVector = TRUE)
}

scenario_result <- function(scenario) {
  config <- apply_experiment_paths(load_project_config(scenario), experiment_id)
  recommendation_dir <- trace_path(config$paths$recommendation_dir)
  summary <- optional_json(file.path(recommendation_dir, "mapping_evaluation_summary.json"))
  formal <- optional_csv(file.path(recommendation_dir, "mapping_evaluation_formal.csv"))
  classifications <- optional_csv(file.path(recommendation_dir, "classification_evaluation.csv"))
  specification <- load_mapping_template(config)
  total <- length(specification$concepts)
  metrics <- tibble::tribble(
    ~scenario, ~level, ~metric, ~numerator, ~denominator, ~denominator_type,
    scenario, level_labels[[scenario]], "有效分类", summary$stage1_valid_classification %||% 0L, total, "全部概念",
    scenario, level_labels[[scenario]], "类别链完全一致", summary$stage1_category_correct %||% 0L, total, "全部概念",
    scenario, level_labels[[scenario]], "包含全部金标准类别", summary$stage1_all_gold_categories_included %||% 0L, total, "全部概念",
    scenario, level_labels[[scenario]], "严格候选覆盖", summary$concepts_with_candidate %||% 0L, total, "全部概念",
    scenario, level_labels[[scenario]], "目标变量集合完整", summary$target_set_complete %||% 0L, total, "全部概念",
    scenario, level_labels[[scenario]], "完整方案首选正确", summary$complete_plan_top1_correct %||% 0L, total, "全部概念",
    scenario, level_labels[[scenario]], "完整方案前三项命中", summary$complete_plan_top3_hit %||% 0L, total, "全部概念",
    scenario, level_labels[[scenario]], "模型信息不足", summary$needs_information %||% 0L, total, "全部概念"
  ) |>
    dplyr::mutate(proportion = ifelse(denominator > 0, numerator / denominator, NA_real_))
  list(config = config, summary = summary, formal = formal, classifications = classifications, metrics = metrics)
}

results <- stats::setNames(lapply(scenarios, scenario_result), scenarios)
level_metrics <- dplyr::bind_rows(lapply(results, `[[`, "metrics"))

pooled <- level_metrics |>
  dplyr::group_by(metric) |>
  dplyr::summarise(
    scenario = "all", level = "全部", numerator = sum(numerator), denominator = sum(denominator),
    denominator_type = "三个层级合并", proportion = numerator / denominator,
    macro_level_mean = mean(proportion), .groups = "drop"
  )
metrics_all <- dplyr::bind_rows(level_metrics, pooled)

formal_all <- purrr::imap_dfr(results, function(result, scenario) {
  if (!nrow(result$formal)) return(tibble::tibble())
  scenario_name <- scenario
  dplyr::mutate(result$formal, scenario = .env$scenario_name, level = level_labels[[.env$scenario_name]], .before = 1L)
})
classification_all <- purrr::imap_dfr(results, function(result, scenario) {
  if (!nrow(result$classifications)) return(tibble::tibble())
  scenario_name <- scenario
  dplyr::mutate(result$classifications, scenario = .env$scenario_name, level = level_labels[[.env$scenario_name]], .before = 1L)
})

by_domain <- formal_all |>
  dplyr::group_by(scenario, level, target_domain) |>
  dplyr::summarise(
    complete_correct = sum(complete_plan_correct), target_complete = sum(target_set_complete),
    total = dplyr::n(), .groups = "drop"
  )
by_difficulty <- formal_all |>
  dplyr::group_by(complexity) |>
  dplyr::summarise(complete_correct = sum(complete_plan_correct), total = dplyr::n(), .groups = "drop")
modification <- formal_all |>
  dplyr::count(scenario, level, modification_grade, name = "concepts")

diagnostic_rows <- list()
failure_rows <- list()
for (scenario in scenarios) {
  result <- results[[scenario]]
  config <- result$config
  specification <- load_mapping_template(config)
  metadata <- load_metadata(config)
  registry <- load_transform_registry(config)
  gold <- load_gold_specification(config)
  group_root <- file.path(trace_path(config$paths$recommendation_dir), "groups")
  if (!dir.exists(group_root)) next
  for (group_id in list.dirs(group_root, recursive = FALSE, full.names = FALSE)) {
    group_path <- file.path(group_root, group_id)
    diagnostic <- optional_json(file.path(group_path, "stage2_concept_diagnostic.json"), list())
    valid <- diagnostic$valid_candidates %||% list()
    if (length(valid)) {
      table <- candidate_table_v02(valid)
      evaluated <- candidate_evaluation_rows_v02(table, specification, gold, registry)
      diagnostic_rows[[length(diagnostic_rows) + 1L]] <- dplyr::mutate(
        evaluated, scenario = scenario, level = level_labels[[scenario]], group_id = group_id, .before = 1L
      )
    }
    failures <- diagnostic$failures %||% list()
    if (length(failures)) {
      failure_rows[[length(failure_rows) + 1L]] <- purrr::map_dfr(failures, function(x) tibble::tibble(
        scenario = scenario, level = level_labels[[scenario]], group_id = group_id,
        concept_id = as.character(x$concept_id %||% ""),
        error_type = "structure_error", description = as.character(x$error %||% "")
      ))
    }
  }
}
diagnostic_all <- dplyr::bind_rows(diagnostic_rows)
validation_failures <- dplyr::bind_rows(failure_rows)
diagnostic_summary <- if (nrow(diagnostic_all)) diagnostic_all |>
  dplyr::group_by(scenario, level) |>
  dplyr::summarise(
    diagnostic_valid_top1 = sum(candidate_rank == 1L),
    target_complete = sum(candidate_rank == 1L & target_set_complete),
    complete_correct = sum(candidate_rank == 1L & complete_plan_correct), .groups = "drop"
  ) else tibble::tibble()

error_taxonomy <- formal_all |>
  dplyr::filter(modification_grade != "none") |>
  dplyr::mutate(
    error_type = dplyr::case_when(
      !strictly_validated ~ "structure_error",
      status == "needs_information" & grepl("格式|类型|函数|契约", paste(reason, uncertainties), ignore.case = TRUE) ~ "function_contract_error",
      status == "needs_information" ~ "uncertainty_or_context_gap",
      !target_set_complete ~ "professional_target_judgment_error",
      TRUE ~ "professional_function_or_parameter_error"
    )
  ) |>
  dplyr::select(scenario, level, target_domain, concept_id, modification_grade, error_type, status, reason, uncertainties, missing_targets, extra_targets)

old_root <- trace_path("output", "v0.2", "advanced", "experiments", "codex-subagent-contextfix-20260902", "recommendations")
old_summary <- optional_json(file.path(old_root, "mapping_evaluation_summary.json"), list(status = "not_available"))
advanced_comparison <- tibble::tibble(
  phase = c("修复前原始评价", "修复后三级基准高级层"),
  category_exact = c(old_summary$stage1_category_correct %||% NA_integer_, results$advanced$summary$stage1_category_correct %||% NA_integer_),
  target_complete = c(old_summary$target_set_complete %||% NA_integer_, results$advanced$summary$target_set_complete %||% NA_integer_),
  complete_top1 = c(old_summary$complete_plan_top1_correct %||% NA_integer_, results$advanced$summary$complete_plan_top1_correct %||% NA_integer_),
  denominator = 21L,
  note = c("按旧提示词、旧政策和旧评价器保留的历史结果。", "按修复后的提示词、政策和评价器得到；两者不是长期稳定性估计。")
)

report_root <- ensure_dir(trace_path("output", "benchmark", "v1", "reports", experiment_id))
write_csv(metrics_all, file.path(report_root, "metrics_by_level.csv"))
write_csv(formal_all, file.path(report_root, "formal_concept_results.csv"))
write_csv(classification_all, file.path(report_root, "classification_results.csv"))
write_csv(by_domain, file.path(report_root, "results_by_domain.csv"))
write_csv(by_difficulty, file.path(report_root, "results_by_difficulty.csv"))
write_csv(modification, file.path(report_root, "modification_grades.csv"))
write_csv(diagnostic_all, file.path(report_root, "diagnostic_concept_results.csv"))
write_csv(diagnostic_summary, file.path(report_root, "diagnostic_summary.csv"))
write_csv(validation_failures, file.path(report_root, "validation_failures.csv"))
write_csv(error_taxonomy, file.path(report_root, "error_taxonomy.csv"))
write_csv(advanced_comparison, file.path(report_root, "advanced_before_after.csv"))

table_tag <- function(data) {
  if (!nrow(data)) return(htmltools::tags$p("无可用结果。"))
  htmltools::tags$table(
    class = "result-table",
    htmltools::tags$thead(htmltools::tags$tr(lapply(names(data), htmltools::tags$th))),
    htmltools::tags$tbody(lapply(seq_len(nrow(data)), function(row) htmltools::tags$tr(
      lapply(data[row, , drop = FALSE], function(value) htmltools::tags$td(as.character(value)))
    )))
  )
}

display_metrics <- metrics_all |>
  dplyr::mutate(result = sprintf("%s/%s（%.1f%%）", numerator, denominator, 100 * proportion)) |>
  dplyr::select(level, metric, result, denominator_type)
html <- htmltools::tags$html(
  htmltools::tags$head(
    htmltools::tags$meta(charset = "UTF-8"),
    htmltools::tags$title("TraceSDTM 三级映射能力基准"),
    htmltools::tags$style(htmltools::HTML(
      "body{font-family:Arial,'Microsoft YaHei',sans-serif;margin:36px;color:#1f2937;line-height:1.55}h1,h2{color:#12304a}.note{background:#eef5fb;padding:14px;border-left:4px solid #3976a8}.result-table{border-collapse:collapse;width:100%;margin:12px 0 28px}.result-table th,.result-table td{border:1px solid #cbd5e1;padding:7px;text-align:left}.result-table th{background:#e8eef5}"
    ))
  ),
  htmltools::tags$body(
    htmltools::tags$h1("TraceSDTM 三级映射能力基准"),
    htmltools::tags$p(sprintf("实验编号：%s；基础18、中等20、高级21，共59个概念。", experiment_id)),
    htmltools::tags$div(class = "note", "这是三个按域聚集的单次盲评。概念不是相互独立的统计单位，不计算二项分布置信区间，也不能据此估计长期稳定性。正式结果保持整组原子拒绝；逐概念结果只用于诊断。"),
    htmltools::tags$h2("分层主要结果"), table_tag(display_metrics),
    htmltools::tags$h2("按域"), table_tag(by_domain),
    htmltools::tags$h2("按难度"), table_tag(by_difficulty),
    htmltools::tags$h2("修改程度"), table_tag(modification),
    htmltools::tags$h2("逐概念诊断口径"), table_tag(diagnostic_summary),
    htmltools::tags$h2("高级层修复前后"), table_tag(advanced_comparison),
    htmltools::tags$h2("错误分类"), table_tag(error_taxonomy),
    htmltools::tags$p("完整明细、校验失败和原始冻结响应与本报告一同保存。")
  )
)
htmltools::save_html(html, file.path(report_root, "three_level_benchmark_report.html"), background = "white")
writeLines(c(
  "# TraceSDTM 三级映射能力基准",
  "",
  sprintf("实验编号：`%s`。", experiment_id),
  "",
  "本目录包含分层、按域、按难度、修改程度、正式口径、逐概念诊断口径及高级层修复前后对照。",
  "这是单次按域聚集实验，不用于估计长期稳定性。"
), file.path(report_root, "README.md"), useBytes = TRUE)
