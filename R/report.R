read_optional_csv <- function(path) {
  if (!file.exists(path)) return(tibble::tibble())
  readr::read_csv(path, show_col_types = FALSE)
}

read_optional_json <- function(path, default = list()) {
  if (!file.exists(path)) return(default)
  jsonlite::read_json(path, simplifyVector = FALSE)
}

html_table <- function(data, max_rows = 60L) {
  if (is.null(data) || !nrow(data)) return(htmltools::tags$p(class = "muted", "无记录"))
  shown <- utils::head(as.data.frame(data), max_rows)
  header <- htmltools::tags$tr(lapply(names(shown), htmltools::tags$th))
  rows <- lapply(seq_len(nrow(shown)), function(index) {
    htmltools::tags$tr(lapply(shown[index, , drop = FALSE], function(value) {
      text <- if (!length(value) || is.na(value)) "" else as.character(value)
      htmltools::tags$td(text)
    }))
  })
  note <- if (nrow(data) > max_rows) {
    htmltools::tags$p(class = "muted", sprintf("仅显示前 %s 行，共 %s 行。", max_rows, nrow(data)))
  }
  htmltools::tagList(htmltools::tags$div(class = "table-wrap", htmltools::tags$table(header, rows)), note)
}

metric_card <- function(label, value, tone = "normal") {
  htmltools::tags$div(
    class = paste("metric", tone),
    htmltools::tags$span(class = "metric-value", value),
    htmltools::tags$span(class = "metric-label", label)
  )
}

generate_report <- function(config) {
  if (!isTRUE(config$project$studio)) trace_abort("离线报告只适用于工作台项目运行。")
  ensure_output_directories(config)
  specification <- load_task_specification(config)
  dictionary <- read_optional_csv(file.path(trace_path(config$paths$profile_dir), "source_dictionary.csv"))
  if (nrow(dictionary)) dictionary <- dplyr::select(dictionary, -dplyr::any_of("example_values"))
  manifest <- read_optional_csv(file.path(trace_path(config$paths$manifest_dir), "dataset_manifest.csv"))
  adam_manifest <- read_optional_csv(file.path(trace_path(config$paths$manifest_dir), "adam_manifest.csv"))
  tlf_manifest <- read_optional_csv(file.path(trace_path(config$paths$manifest_dir), "tlf_manifest.csv"))
  lineage <- read_optional_csv(file.path(trace_path(config$paths$lineage_dir), "field_lineage.csv"))
  adam_lineage <- read_optional_csv(file.path(trace_path(config$paths$lineage_dir), "adam_lineage.csv"))
  tlf_lineage <- read_optional_csv(file.path(trace_path(config$paths$lineage_dir), "tlf_lineage.csv"))
  local_issues <- read_optional_csv(file.path(trace_path(config$paths$local_validation_dir), "local_issues.csv"))
  adam_issues <- read_optional_csv(file.path(trace_path(config$paths$adam_validation_dir), "adam_issues.csv"))
  tlf_issues <- read_optional_csv(file.path(trace_path(config$paths$tlf_validation_dir), "tlf_issues.csv"))
  p21_issues <- read_optional_csv(file.path(trace_path(config$paths$p21_validation_dir), "p21_issues.csv"))
  p21_run <- read_optional_json(file.path(trace_path(config$paths$p21_validation_dir), "p21_run.json"))
  demographics <- read_optional_csv(file.path(trace_path(config$paths$tlf_csv_dir), "t14_1_1_demographics.csv"))
  teae <- read_optional_csv(file.path(trace_path(config$paths$tlf_csv_dir), "t14_3_1_teae.csv"))
  assembled <- read_optional_json(file.path(trace_path(config$paths$recommendation_dir), "assembled_recommendations.json"))
  ai_review <- read_optional_json(v06_ai_review_path(config))
  approval <- if (file.exists(trace_path(config$paths$approved_specification))) {
    yaml::read_yaml(trace_path(config$paths$approved_specification))$specification$approval %||% list()
  } else list()

  plans <- assembled$plans %||% list()
  planned_tasks <- unique(vapply(plans, function(plan) as.character(plan$task_id %||% ""), character(1)))
  review_statuses <- vapply(ai_review$task_reviews %||% list(), function(item) as.character(item$status %||% ""), character(1))
  local_errors <- if (nrow(local_issues) && "severity" %in% names(local_issues)) sum(local_issues$severity == "ERROR") else 0L
  adam_errors <- if (nrow(adam_issues) && "severity" %in% names(adam_issues)) sum(adam_issues$severity == "ERROR") else 0L
  tlf_errors <- if (nrow(tlf_issues) && "severity" %in% names(tlf_issues)) sum(tlf_issues$severity == "ERROR") else 0L
  review_errors <- sum(review_statuses == "error")
  review_warnings <- sum(review_statuses == "warning")
  p21_status <- if (length(p21_run)) {
    data.frame(
      item = c("执行状态", "Community 版本", "规则引擎", "受控术语版本", "明细问题", "生成域 Reject"),
      value = c(
        as.character(p21_run$execution_status %||% "未知"),
        as.character(p21_run$community_version %||% ""),
        as.character(p21_run$engine_name %||% ""),
        as.character(p21_run$controlled_terminology_version %||% ""),
        as.character(nrow(p21_issues)),
        as.character(sum(p21_issues$severity == "Reject" & p21_issues$domain %in% c("DM", "AE", "VS"), na.rm = TRUE))
      ), stringsAsFactors = FALSE
    )
  } else {
    data.frame(item = "执行状态", value = "未运行或未配置；不阻止分析演示。", stringsAsFactors = FALSE)
  }

  css <- "
    body{font-family:Segoe UI,Arial,sans-serif;margin:0;color:#172033;background:#f4f7fb}
    main{max-width:1180px;margin:0 auto;padding:28px}h1{margin-bottom:4px}
    h2{margin-top:34px;border-bottom:2px solid #dce4ef;padding-bottom:8px}.muted{color:#637083}
    .grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(170px,1fr));gap:12px}
    .metric{background:white;border:1px solid #dce4ef;border-radius:10px;padding:16px;display:flex;flex-direction:column}
    .metric-value{font-size:26px;font-weight:700}.metric-label{color:#637083;margin-top:4px}
    .warning{border-left:5px solid #e7a33e}.good{border-left:5px solid #2c9b68}
    .flow{display:flex;flex-wrap:wrap;gap:8px;align-items:center}.flow span{background:#173b63;color:white;padding:10px 12px;border-radius:7px}
    .flow b{color:#6f7f94}.table-wrap{overflow:auto;background:white;border:1px solid #dce4ef;border-radius:8px}
    table{border-collapse:collapse;width:100%;font-size:13px}th,td{border-bottom:1px solid #e8edf4;text-align:left;padding:8px;vertical-align:top}
    th{background:#eaf0f7;position:sticky;top:0}
  "
  info <- data.frame(
    item = c("项目", "研究编号", "标准", "运行编号", "批准者", "批准时间", "报告时间"),
    value = c(
      config$project$name, config$project$study_id,
      paste(config$project$standard, config$project$standard_version),
      config$project$run_id, approval$reviewer %||% "尚未批准",
      approval$approved_at %||% "", utc_now()
    ), stringsAsFactors = FALSE
  )
  document <- htmltools::tags$html(
    htmltools::tags$head(
      htmltools::tags$meta(charset = "utf-8"),
      htmltools::tags$title("TraceSDTM Studio 项目报告"),
      htmltools::tags$style(htmltools::HTML(css))
    ),
    htmltools::tags$body(htmltools::tags$main(
      htmltools::tags$h1("TraceSDTM Studio 0.7"),
      htmltools::tags$p(class = "muted", "模型辅助 SDTM 映射与确定性 ADaM、汇总表构建的运行报告"),
      htmltools::tags$div(class = "grid",
        metric_card("来源字段", nrow(dictionary)),
        metric_card("冻结任务", length(specification$tasks %||% list())),
        metric_card("已规划任务", length(planned_tasks)),
        metric_card("审查错误", review_errors, if (review_errors) "warning" else "good"),
        metric_card("审查警告", review_warnings, if (review_warnings) "warning" else "good"),
        metric_card("生成域", nrow(manifest)),
        metric_card("SDTM 本地错误", local_errors, if (local_errors) "warning" else "good"),
        metric_card("Pinnacle 21 明细问题", nrow(p21_issues), if (nrow(p21_issues)) "warning" else "good"),
        metric_card("ADaM 数据集", nrow(adam_manifest)),
        metric_card("ADaM 本地错误", adam_errors, if (adam_errors) "warning" else "good"),
        metric_card("汇总表", nrow(tlf_manifest)),
        metric_card("表格检查错误", tlf_errors, if (tlf_errors) "warning" else "good")
      ),
      htmltools::tags$h2("执行流程"),
      htmltools::tags$div(class = "flow",
        htmltools::tags$span("数据冻结与画像"), htmltools::tags$b("→"),
        htmltools::tags$span("任务确认"), htmltools::tags$b("→"),
        htmltools::tags$span("映射生成"), htmltools::tags$b("→"),
        htmltools::tags$span("独立审查与人工批准"), htmltools::tags$b("→"),
        htmltools::tags$span("SDTM 构建与检查"), htmltools::tags$b("→"),
        htmltools::tags$span("确定性 ADaM 与汇总表")
      ),
      htmltools::tags$h2("运行信息"), html_table(info),
      htmltools::tags$h2("来源画像"), html_table(dictionary, 40L),
      htmltools::tags$h2("SDTM 数据集"), html_table(manifest),
      htmltools::tags$h2("SDTM 本地检查"), html_table(local_issues),
      htmltools::tags$h2("Pinnacle 21"),
      html_table(p21_status),
      if (nrow(p21_issues)) html_table(p21_issues) else NULL,
      htmltools::tags$h2("ADaM 数据集"), html_table(adam_manifest),
      htmltools::tags$h2("ADaM 本地检查"), html_table(adam_issues),
      htmltools::tags$h2("汇总表清单与检查"), html_table(tlf_manifest), html_table(tlf_issues),
      htmltools::tags$h2("T14.1.1 预览"), html_table(demographics),
      htmltools::tags$h2("T14.3.1 预览"), html_table(teae),
      htmltools::tags$h2("SDTM 字段级追溯"), html_table(lineage, 80L),
      htmltools::tags$h2("ADaM 变量级追溯"), html_table(adam_lineage, 80L),
      htmltools::tags$h2("汇总表追溯"), html_table(tlf_lineage, 80L),
      htmltools::tags$h2("使用边界"),
      htmltools::tags$p("人工智能只参与 SDTM 候选映射与独立审查；ADaM 派生和统计计算仅执行冻结规则。所有数据均为模拟数据；本地检查不等同于正式递交级 CDISC 合规验证。")
    ))
  )
  output <- file.path(trace_path(config$paths$report_dir), "trace_sdtm_report.html")
  ensure_parent(output)
  htmltools::save_html(document, output, background = "white")
  trace_info("已生成离线报告：%s", output)
  invisible(output)
}
