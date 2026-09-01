read_optional_csv <- function(path) {
  if (!file.exists(path)) return(tibble::tibble())
  readr::read_csv(path, show_col_types = FALSE)
}

read_optional_json <- function(path, default = list()) {
  if (!file.exists(path)) return(default)
  jsonlite::read_json(path, simplifyVector = TRUE)
}

html_table <- function(data, max_rows = 50L) {
  if (is.null(data) || !nrow(data)) return(htmltools::tags$p(class = "muted", "无记录"))
  shown <- utils::head(as.data.frame(data), max_rows)
  header <- htmltools::tags$tr(lapply(names(shown), function(name) htmltools::tags$th(name)))
  rows <- lapply(seq_len(nrow(shown)), function(index) {
    htmltools::tags$tr(lapply(shown[index, , drop = FALSE], function(value) {
      text <- if (length(value) == 0L || is.na(value)) "" else as.character(value)
      htmltools::tags$td(text)
    }))
  })
  note <- if (nrow(data) > max_rows) htmltools::tags$p(class = "muted", sprintf("仅显示前 %s 行，共 %s 行。", max_rows, nrow(data))) else NULL
  htmltools::tagList(htmltools::tags$div(class = "table-wrap", htmltools::tags$table(header, rows)), note)
}

metric_card <- function(label, value, tone = "normal") {
  htmltools::tags$div(class = paste("metric", tone), htmltools::tags$span(class = "metric-value", value), htmltools::tags$span(class = "metric-label", label))
}

generate_report <- function(config = load_project_config()) {
  ensure_output_directories(config)
  evaluation <- evaluate_recommendations(config)$summary
  dictionary <- read_optional_csv(trace_path(config$paths$profile_dir, "source_dictionary.csv"))
  manifest <- read_optional_csv(trace_path(config$paths$manifest_dir, "dataset_manifest.csv"))
  lineage <- read_optional_csv(trace_path(config$paths$lineage_dir, "field_lineage.csv"))
  local_issues <- read_optional_csv(trace_path(config$paths$local_validation_dir, "local_issues.csv"))
  p21_issues <- read_optional_csv(trace_path(config$paths$p21_validation_dir, "p21_issues.csv"))
  model_run <- read_optional_json(trace_path(config$paths$recommendation_dir, "model_run.json"), list(status = "not_run", provenance = "not_run"))
  p21_run <- read_optional_json(trace_path(config$paths$p21_validation_dir, "p21_run.json"), list(exit_status = NA, engine_name = "not_run"))

  generated_defects <- if (nrow(p21_issues)) sum(p21_issues$issue_class == "generated_domain_defect") else 0L
  expected_scope <- if (nrow(p21_issues)) sum(p21_issues$issue_class == "expected_mvp_scope") else 0L
  local_errors <- if (nrow(local_issues)) sum(local_issues$severity == "ERROR") else 0L

  css <- "
    body {font-family: Segoe UI, Arial, sans-serif; margin: 0; color: #172033; background: #f4f7fb;}
    main {max-width: 1180px; margin: 0 auto; padding: 28px;}
    h1 {margin-bottom: 4px;} h2 {margin-top: 34px; border-bottom: 2px solid #dce4ef; padding-bottom: 8px;}
    .subtitle,.muted {color:#637083;} .grid {display:grid; grid-template-columns:repeat(auto-fit,minmax(170px,1fr)); gap:12px;}
    .metric {background:white; border:1px solid #dce4ef; border-radius:10px; padding:16px; display:flex; flex-direction:column;}
    .metric-value {font-size:26px; font-weight:700;} .metric-label {color:#637083; margin-top:4px;}
    .warning {border-left:5px solid #e7a33e;} .good {border-left:5px solid #2c9b68;}
    .flow {display:flex; flex-wrap:wrap; gap:8px; align-items:center;} .flow span {background:#173b63;color:white;padding:10px 12px;border-radius:7px;}
    .flow b {color:#6f7f94;} .callout {background:#fff7df;border-left:5px solid #e7a33e;padding:14px 18px;border-radius:6px;}
    .table-wrap {overflow:auto; background:white; border:1px solid #dce4ef; border-radius:8px;} table {border-collapse:collapse;width:100%;font-size:13px;}
    th,td {border-bottom:1px solid #e8edf4;text-align:left;padding:8px;vertical-align:top;} th {background:#eaf0f7;position:sticky;top:0;}
    code {background:#eaf0f7;padding:2px 5px;border-radius:4px;} pre {background:#172033;color:#edf4ff;padding:14px;border-radius:8px;overflow:auto;}
  "

  document <- htmltools::tags$html(
    htmltools::tags$head(
      htmltools::tags$meta(charset = "utf-8"),
      htmltools::tags$title("TraceSDTM 项目报告"),
      htmltools::tags$style(htmltools::HTML(css))
    ),
    htmltools::tags$body(htmltools::tags$main(
      htmltools::tags$h1("TraceSDTM"),
      htmltools::tags$p(class = "subtitle", "人工监督、规格驱动、可追溯的 SDTM 自动化生成演示"),
      htmltools::tags$div(class = "grid",
        metric_card("原始字段", nrow(dictionary)),
        metric_card("生成域", if (nrow(manifest)) nrow(manifest) else 0L),
        metric_card("本地错误", local_errors, if (local_errors == 0L) "good" else "warning"),
        metric_card("Pinnacle 21 域内问题", generated_defects, if (generated_defects == 0L) "good" else "warning"),
        metric_card("范围导致问题", expected_scope),
        metric_card("推荐来源", model_run$provenance %||% "not_run")
      ),
      htmltools::tags$h2("流程架构"),
      htmltools::tags$div(class = "flow",
        htmltools::tags$span("原始数据"), htmltools::tags$b("→"),
        htmltools::tags$span("字段画像"), htmltools::tags$b("→"),
        htmltools::tags$span("候选映射"), htmltools::tags$b("→"),
        htmltools::tags$span("人工审核"), htmltools::tags$b("→"),
        htmltools::tags$span("确定性构建"), htmltools::tags$b("→"),
        htmltools::tags$span("双层验证"), htmltools::tags$b("→"),
        htmltools::tags$span("追溯报告")
      ),
      htmltools::tags$h2("运行信息"),
      html_table(data.frame(
        item = c("标准", "模型运行状态", "模型来源", "Pinnacle 21 引擎", "Pinnacle 21 退出状态", "报告生成时间"),
        value = c(
          paste(config$project$standard, config$project$standard_version),
          model_run$status %||% "not_run",
          model_run$provenance %||% "not_run",
          p21_run$engine_name %||% "not_run",
          as.character(p21_run$exit_status %||% NA),
          utc_now()
        )
      )),
      if (identical(model_run$provenance, "reference_seed")) htmltools::tags$div(class = "callout", "当前候选映射来自参考种子，不是实际模型输出。评价指标仅用于验证评价程序，不能写成模型准确率。") else NULL,
      htmltools::tags$h2("数据画像"),
      html_table(dictionary, 30L),
      htmltools::tags$h2("映射评价"),
      html_table(data.frame(metric = names(evaluation), value = vapply(evaluation, function(x) paste(x, collapse = ", "), character(1)))),
      htmltools::tags$h2("生成数据集"),
      html_table(manifest),
      htmltools::tags$h2("本地检查"),
      html_table(local_issues),
      htmltools::tags$h2("Pinnacle 21 Community"),
      htmltools::tags$p("原始问题完整保留；缺少 Define-XML 和未覆盖域的问题标记为最小项目范围导致，不计作合规通过。"),
      html_table(p21_issues, 80L),
      htmltools::tags$h2("字段追溯示例"),
      html_table(lineage, 50L),
      htmltools::tags$h2("已知限制"),
      htmltools::tags$ul(
        htmltools::tags$li("仅覆盖 DM、AE、VS，不代表完整研究提交包。"),
        htmltools::tags$li("不生成 Define-XML、aCRF、ADaM 或 TLF。"),
        htmltools::tags$li("未部署 CDISC CORE。"),
        htmltools::tags$li("没有电子签名、权限、审计追踪验证和法规申报级系统认证。"),
        htmltools::tags$li("所有结果仍需合格的临床数据标准专家审核。")
      )
    ))
  )
  output <- trace_path(config$paths$report_dir, "trace_sdtm_report.html")
  ensure_parent(output)
  htmltools::save_html(document, output, background = "white")
  trace_info("已生成离线报告：%s", output)
  invisible(output)
}

