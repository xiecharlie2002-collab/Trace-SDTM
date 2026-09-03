#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = FALSE)
file_argument <- sub("^--file=", "", arguments[grepl("^--file=", arguments)])
if (!length(file_argument)) stop("无法确定报告脚本位置。", call. = FALSE)
project_root <- normalizePath(file.path(dirname(file_argument[[1]]), "..", ".."), winslash = "/", mustWork = TRUE)
Sys.setenv(TRACE_SDTM_ROOT = project_root)
project_library <- file.path(project_root, ".Rlib")
if (dir.exists(project_library)) .libPaths(c(project_library, .libPaths()))
source(file.path(project_root, "R", "utils.R"), encoding = "UTF-8")

args <- commandArgs(trailingOnly = TRUE)
argument_value <- function(name, default = NULL) {
  position <- match(name, args)
  if (is.na(position)) return(default)
  if (position == length(args)) stop(sprintf("%s 后必须提供值。", name), call. = FALSE)
  as.character(args[[position + 1L]])
}
experiment_id <- argument_value("--experiment-id")
if (is.null(experiment_id) || !grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", experiment_id)) {
  stop("需要合法的 --experiment-id。", call. = FALSE)
}

scenarios <- c("basic", "intermediate", "advanced")
level_labels <- c(basic = "基础", intermediate = "中等", advanced = "高级")
experiment_root <- trace_path("output", "benchmark", "v2", "experiments", experiment_id)
report_root <- ensure_dir(trace_path("output", "benchmark", "v2", "reports", experiment_id))

read_required <- function(path) {
  if (!file.exists(path)) stop(sprintf("盲评产物不完整：%s", path), call. = FALSE)
  readr::read_csv(path, show_col_types = FALSE)
}

read_mode <- function(scenario, mode) {
  scenario_value <- scenario
  level_value <- unname(level_labels[[scenario_value]])
  directory <- file.path(
    experiment_root, scenario_value, "evaluation",
    if (identical(mode, "conditional")) "conditional" else ""
  )
  detail <- read_required(file.path(directory, "v04_atomic_evaluation.csv"))
  assembly <- read_required(file.path(directory, "v04_assembly_evaluation.csv"))
  parameters <- read_required(file.path(directory, "v04_parameter_accounting.csv"))
  list(
    detail = dplyr::mutate(
      detail, evaluation_mode = mode, level = level_value, .before = 1L
    ),
    assembly = dplyr::mutate(
      assembly, evaluation_mode = mode, level = level_value, .before = 1L
    ),
    parameters = dplyr::mutate(
      parameters, scenario = scenario_value, evaluation_mode = mode,
      level = level_value, .before = 1L
    )
  )
}

results <- unlist(lapply(scenarios, function(scenario) {
  stats::setNames(
    list(read_mode(scenario, "cascade"), read_mode(scenario, "conditional")),
    c(paste0(scenario, "_cascade"), paste0(scenario, "_conditional"))
  )
}), recursive = FALSE)

detail <- dplyr::bind_rows(lapply(results, `[[`, "detail"))
assembly <- dplyr::bind_rows(lapply(results, `[[`, "assembly"))
parameters <- dplyr::bind_rows(lapply(results, `[[`, "parameters"))

metric_columns <- c(
  semantic_correct = "语义正确",
  target_structural_correct = "目标阶段结构正确",
  function_structural_correct = "函数阶段结构正确",
  parameter_structural_correct = "参数阶段结构正确",
  end_to_end_structural_correct = "端到端结构正确",
  function_top1_correct = "函数首选正确",
  function_top3_correct = "函数前三项命中",
  parameter_correct = "参数正确",
  complete_plan_top1_correct = "完整计划首选正确",
  complete_plan_top3_correct = "完整计划前三项命中"
)

summarise_metrics <- function(data, groups) {
  grouped <- dplyr::group_by(data, dplyr::across(dplyr::all_of(groups)))
  purrr::imap_dfr(metric_columns, function(label, column) {
    grouped |>
      dplyr::summarise(
        numerator = sum(.data[[column]], na.rm = TRUE),
        denominator = dplyr::n(), .groups = "drop"
      ) |>
      dplyr::mutate(
        metric = label,
        proportion = .data$numerator / .data$denominator,
        .after = dplyr::last_col()
      )
  })
}

metrics_by_level <- summarise_metrics(detail, c("evaluation_mode", "scenario", "level"))
metrics_by_domain <- summarise_metrics(detail, c("evaluation_mode", "scenario", "level", "target_domain"))
metrics_all <- detail |>
  dplyr::group_by(.data$evaluation_mode) |>
  dplyr::group_split() |>
  purrr::map_dfr(function(part) {
    mode <- dplyr::first(part$evaluation_mode)
    pooled <- summarise_metrics(part, "evaluation_mode") |>
      dplyr::mutate(scenario = "all", level = "全部", level_equal_weight_mean = NA_real_)
    equal_weight <- metrics_by_level |>
      dplyr::filter(.data$evaluation_mode == mode) |>
      dplyr::group_by(.data$metric) |>
      dplyr::summarise(level_equal_weight_mean = mean(.data$proportion), .groups = "drop")
    dplyr::left_join(
      dplyr::select(
        pooled,
        dplyr::all_of(c(
          "evaluation_mode", "scenario", "level", "metric",
          "numerator", "denominator", "proportion"
        ))
      ),
      equal_weight, by = "metric"
    )
  })

modification <- detail |>
  dplyr::count(.data$evaluation_mode, .data$scenario, .data$level,
               .data$target_domain, .data$modification_grade, name = "count")

assembly_summary <- assembly |>
  dplyr::group_by(.data$evaluation_mode, .data$scenario, .data$level) |>
  dplyr::summarise(
    assembly_groups = dplyr::n(),
    semantic_correct = sum(.data$semantic_correct),
    structural_correct = sum(.data$structural_correct),
    complete_plan_correct = sum(.data$complete_plan_correct),
    complete_plan_top3 = sum(.data$complete_plan_top3),
    .groups = "drop"
  )

parameter_summary <- parameters |>
  dplyr::group_by(.data$evaluation_mode, .data$scenario, .data$level) |>
  dplyr::summarise(
    parameter_total = sum(.data$parameter_total),
    automatically_injected = sum(.data$automatically_injected),
    requested_from_model = sum(.data$requested_from_model),
    model_completion_received = sum(.data$model_completion_received),
    model_once_correct = sum(.data$model_once_correct, na.rm = TRUE),
    reviewer_corrected = sum(.data$reviewer_corrected),
    parameter_conflicts = sum(.data$parameter_conflicts),
    unavailable = sum(.data$unavailable),
    fully_resolved_candidates = sum(.data$fully_resolved),
    candidate_count = dplyr::n(), .groups = "drop"
  )

write_csv(detail, file.path(report_root, "v04_atomic_results.csv"))
write_csv(assembly, file.path(report_root, "v04_assembly_results.csv"))
write_csv(parameters, file.path(report_root, "v04_parameter_results.csv"))
write_csv(metrics_by_level, file.path(report_root, "v04_metrics_by_level.csv"))
write_csv(metrics_by_domain, file.path(report_root, "v04_metrics_by_domain.csv"))
write_csv(metrics_all, file.path(report_root, "v04_metrics_all.csv"))
write_csv(modification, file.path(report_root, "v04_modification_grades.csv"))
write_csv(assembly_summary, file.path(report_root, "v04_assembly_summary.csv"))
write_csv(parameter_summary, file.path(report_root, "v04_parameter_summary.csv"))

v03_comparison_path <- file.path(
  report_root, "v03_corrected_baseline", "v03_original_vs_corrected.csv"
)
v03_comparison <- if (file.exists(v03_comparison_path)) {
  readr::read_csv(v03_comparison_path, show_col_types = FALSE)
} else tibble::tibble()

delivery_audit_path <- file.path(experiment_root, "delivery_audit.json")
delivery_audit <- if (file.exists(delivery_audit_path)) {
  jsonlite::read_json(delivery_audit_path, simplifyVector = TRUE)
} else NULL
delivery_summary <- if (!is.null(delivery_audit)) {
  values <- delivery_audit$verification_summary
  tibble::tibble(
    实际模型请求 = values$model_request_stage_count,
    请求校验值一致 = values$transmitted_text_hash_matches_frozen_file_count,
    响应证据一致 = values$frozen_file_hash_matches_evidence_count,
    严格校验通过 = values$strict_validation_passed_count,
    重试次数 = values$retry_count,
    密钥记录数 = values$api_key_logged_count
  )
} else tibble::tibble()
delivery_records <- if (!is.null(delivery_audit)) {
  tibble::as_tibble(delivery_audit$delivery_records)
} else tibble::tibble()

if (nrow(delivery_summary)) write_csv(delivery_summary, file.path(report_root, "v04_delivery_summary.csv"))
if (nrow(delivery_records)) write_csv(delivery_records, file.path(report_root, "v04_delivery_records.csv"))

cascade_level <- metrics_by_level |>
  dplyr::filter(
    .data$evaluation_mode == "cascade",
    .data$metric %in% c("语义正确", "端到端结构正确", "完整计划首选正确")
  )

summary <- list(
  schema_version = "0.4-report",
  experiment_id = experiment_id,
  protocol = "nine_fresh_domain_subagents_first_response_no_retry",
  atomic_task_count = sum(detail$evaluation_mode == "cascade"),
  assembly_group_count = sum(assembly$evaluation_mode == "cascade"),
  level_task_counts = as.list(table(detail$scenario[detail$evaluation_mode == "cascade"])),
  cascade = split(cascade_level, cascade_level$scenario),
  parameter_summary = parameter_summary,
  delivery_verification = delivery_audit$verification_summary %||% NULL,
  generated_at = utc_now(),
  interpretation_limits = list(
    "单次按域聚集实验，任务并非相互独立。",
    "不计算二项分布置信区间，不能估计长期稳定性。",
    "v0.3 与 v0.4 的协议和任务粒度不同，前后对照仅作描述。",
    "子代理盲法是过程约束，不属于操作系统级隔离。"
  )
)
write_json(summary, file.path(report_root, "v04_report_summary.json"))

table_tag <- function(data, digits = 3L) {
  if (!nrow(data)) return(htmltools::tags$p("无可用数据。"))
  display <- data
  display[] <- lapply(display, function(column) {
    if (is.numeric(column)) format(round(column, digits), trim = TRUE, scientific = FALSE) else as.character(column)
  })
  htmltools::tags$table(
    class = "data-table",
    htmltools::tags$thead(htmltools::tags$tr(lapply(names(display), htmltools::tags$th))),
    htmltools::tags$tbody(lapply(seq_len(nrow(display)), function(index) {
      htmltools::tags$tr(lapply(display[index, , drop = TRUE], htmltools::tags$td))
    }))
  )
}

overview <- cascade_level |>
  dplyr::transmute(
    层级 = .data$level, 指标 = .data$metric,
    结果 = paste0(.data$numerator, "/", .data$denominator),
    比例 = sprintf("%.1f%%", 100 * .data$proportion)
  )
conditional_overview <- metrics_by_level |>
  dplyr::filter(
    .data$evaluation_mode == "conditional",
    .data$metric %in% c("函数首选正确", "参数正确", "完整计划首选正确")
  ) |>
  dplyr::transmute(
    层级 = .data$level, 指标 = .data$metric,
    结果 = paste0(.data$numerator, "/", .data$denominator),
    比例 = sprintf("%.1f%%", 100 * .data$proportion)
  )

document <- htmltools::tags$html(
  htmltools::tags$head(
    htmltools::tags$meta(charset = "utf-8"),
    htmltools::tags$title("TraceSDTM v0.4 三级盲评报告"),
    htmltools::tags$style(htmltools::HTML(paste(
      "body{font-family:'Segoe UI','Microsoft YaHei',sans-serif;margin:36px;color:#1e293b;line-height:1.65;}",
      "h1,h2{color:#0f3d56;} .note{background:#f1f5f9;border-left:4px solid #0ea5e9;padding:12px 16px;}",
      ".data-table{border-collapse:collapse;width:100%;margin:12px 0 28px;font-size:14px;}",
      ".data-table th,.data-table td{border:1px solid #cbd5e1;padding:7px 9px;text-align:left;}",
      ".data-table th{background:#e2e8f0;} code{background:#f1f5f9;padding:2px 5px;}",
      sep = ""
    )))
  ),
  htmltools::tags$body(
    htmltools::tags$h1("TraceSDTM v0.4 三级映射能力盲评"),
    htmltools::tags$p(sprintf("实验编号：%s；生成时间：%s。", experiment_id, utc_now())),
    htmltools::tags$div(class = "note",
      "模型分三步完成目标变量识别、函数选择和有限参数补全；已知参数由程序从政策、注册表和资源目录确定性注入。"
    ),
    htmltools::tags$h2("请求传输与盲评协议"), table_tag(delivery_summary),
    htmltools::tags$p("校验值一致表示实际发送给子代理的文本与冻结请求文件逐字一致；盲法仍属于过程约束，不是操作系统级隔离。"),
    htmltools::tags$h2("真实串联评价"), table_tag(overview),
    htmltools::tags$h2("条件评价"),
    htmltools::tags$p("给定正确目标后评价函数；参数阶段给定正确上游决定，用于隔离各阶段能力。"),
    table_tag(conditional_overview),
    htmltools::tags$h2("按域分层"), table_tag(metrics_by_domain),
    htmltools::tags$h2("原有概念聚合口径"), table_tag(assembly_summary),
    htmltools::tags$h2("修改程度"), table_tag(modification),
    htmltools::tags$h2("参数来源"), table_tag(parameter_summary),
    htmltools::tags$h2("v0.3 等价比较修正"), table_tag(v03_comparison),
    htmltools::tags$h2("解释边界"),
    htmltools::tags$ul(lapply(summary$interpretation_limits, htmltools::tags$li)),
    htmltools::tags$p("所有分子和分母均保留在随附 CSV；本报告不把一次实验结果解释为模型长期正确率，也不声称达到申报级自动化。")
  )
)
htmltools::save_html(document, file.path(report_root, "trace_sdtm_v04_benchmark.html"), background = "white")
message("已生成 v0.4 离线报告：", file.path(report_root, "trace_sdtm_v04_benchmark.html"))
