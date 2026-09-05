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
  lineage <- read_optional_csv(file.path(trace_path(config$paths$lineage_dir), "field_lineage.csv"))
  local_issues <- read_optional_csv(file.path(trace_path(config$paths$local_validation_dir), "local_issues.csv"))
  p21_issues <- read_optional_csv(file.path(trace_path(config$paths$p21_validation_dir), "p21_issues.csv"))
  assembled <- read_optional_json(file.path(trace_path(config$paths$recommendation_dir), "assembled_recommendations.json"))
  ai_review <- read_optional_json(v06_ai_review_path(config))
  approval <- if (file.exists(trace_path(config$paths$approved_specification))) {
    yaml::read_yaml(trace_path(config$paths$approved_specification))$specification$approval %||% list()
  } else list()

  plans <- assembled$plans %||% list()
  planned_tasks <- unique(vapply(plans, function(plan) as.character(plan$task_id %||% ""), character(1)))
  review_statuses <- vapply(ai_review$task_reviews %||% list(), function(item) as.character(item$status %||% ""), character(1))
  local_errors <- if (nrow(local_issues) && "severity" %in% names(local_issues)) sum(local_issues$severity == "ERROR") else 0L
  review_errors <- sum(review_statuses == "error")
  review_warnings <- sum(review_statuses == "warning")

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
      htmltools::tags$h1("TraceSDTM Studio 0.6"),
      htmltools::tags$p(class = "muted", "人工监督、程序约束、确定性构建的运行报告"),
      htmltools::tags$div(class = "grid",
        metric_card("来源字段", nrow(dictionary)),
        metric_card("冻结任务", length(specification$tasks %||% list())),
        metric_card("已规划任务", length(planned_tasks)),
        metric_card("审查错误", review_errors, if (review_errors) "warning" else "good"),
        metric_card("审查警告", review_warnings, if (review_warnings) "warning" else "good"),
        metric_card("生成域", nrow(manifest)),
        metric_card("本地错误", local_errors, if (local_errors) "warning" else "good")
      ),
      htmltools::tags$h2("执行流程"),
      htmltools::tags$div(class = "flow",
        htmltools::tags$span("数据冻结与画像"), htmltools::tags$b("→"),
        htmltools::tags$span("任务确认"), htmltools::tags$b("→"),
        htmltools::tags$span("映射生成"), htmltools::tags$b("→"),
        htmltools::tags$span("独立审查与人工批准"), htmltools::tags$b("→"),
        htmltools::tags$span("确定性构建与检查")
      ),
      htmltools::tags$h2("运行信息"), html_table(info),
      htmltools::tags$h2("来源画像"), html_table(dictionary, 40L),
      htmltools::tags$h2("生成数据集"), html_table(manifest),
      htmltools::tags$h2("本地检查"), html_table(local_issues),
      htmltools::tags$h2("Pinnacle 21"), html_table(p21_issues),
      htmltools::tags$h2("字段级追溯"), html_table(lineage, 80L),
      htmltools::tags$h2("使用边界"),
      htmltools::tags$p("人工智能只提出建议和报告问题；最终结果仍需由具备相应资质的临床数据标准专家审核。")
    ))
  )
  output <- file.path(trace_path(config$paths$report_dir), "trace_sdtm_report.html")
  ensure_parent(output)
  htmltools::save_html(document, output, background = "white")
  trace_info("已生成离线报告：%s", output)
  invisible(output)
}
