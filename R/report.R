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

# -----------------------------------------------------------------------------
# 0.4 离线报告：展示三阶段推荐、原子任务审核和高级场景证据。

comparison_manifest_v02 <- function() {
  paths <- c(
    basic = trace_path("output", "benchmark", "v2", "basic", "manifests", "dataset_manifest.csv"),
    intermediate = trace_path("output", "benchmark", "v2", "intermediate", "manifests", "dataset_manifest.csv"),
    advanced = trace_path("output", "benchmark", "v2", "advanced", "manifests", "dataset_manifest.csv")
  )
  purrr::imap_dfr(paths, function(path, scenario) {
    if (!file.exists(path)) return(tibble::tibble())
    dplyr::mutate(readr::read_csv(path, show_col_types = FALSE), scenario = scenario, .before = 1L)
  })
}

generate_report <- function(config = load_project_config()) {
  ensure_output_directories(config)
  is_v04 <- identical(as.character(config$project$version), "0.4.0")
  if (is_v04) {
    stage_paths <- file.path(trace_path(config$paths$recommendation_dir), c(
      "target_decisions.json", "function_candidates.json",
      "parameter_completions.json", "assembled_recommendations.json"
    ))
    if (all(file.exists(stage_paths))) {
      stages <- lapply(stage_paths, jsonlite::read_json, simplifyVector = FALSE)
      evaluation_result <- evaluate_three_stage_v04(
        config, stages[[1]], stages[[2]], stages[[3]], stages[[4]]
      )
      evaluation <- c(evaluation_result$summary, list(
        evaluated_concepts = evaluation_result$summary$task_count,
        concepts_with_candidate = sum(evaluation_result$detail$end_to_end_structural_correct),
        rejected_groups = sum(!evaluation_result$detail$end_to_end_structural_correct)
      ))
    } else {
      evaluation_result <- list(detail = tibble::tibble())
      evaluation <- list(evaluated_concepts = 0L, concepts_with_candidate = 0L, rejected_groups = 0L)
    }
  } else {
    evaluation_result <- evaluate_recommendations(config)
    evaluation <- evaluation_result$summary
  }
  dictionary <- read_optional_csv(trace_path(config$paths$profile_dir, "source_dictionary.csv"))
  classifications <- if (is_v04) tibble::tibble() else read_optional_csv(trace_path(config$paths$recommendation_dir, "classification_evaluation.csv"))
  mapping_detail <- if (is_v04) evaluation_result$detail else read_optional_csv(trace_path(config$paths$recommendation_dir, "mapping_evaluation.csv"))
  manifest <- read_optional_csv(trace_path(config$paths$manifest_dir, "dataset_manifest.csv"))
  comparison <- comparison_manifest_v02()
  lineage <- read_optional_csv(trace_path(config$paths$lineage_dir, "field_lineage.csv"))
  partial_dates <- read_optional_csv(trace_path(config$paths$lineage_dir, "partial_date_notes.csv"))
  final_steps <- read_optional_csv(trace_path(config$paths$review_dir, "final_steps_audit.csv"))
  review <- read_optional_csv(trace_path(config$paths$review_dir, if (is_v04) "task_review_audit.csv" else "concept_review_audit.csv"))
  review_summary <- read_optional_json(trace_path(config$paths$review_dir, "expert_review_summary.json"), list())
  local_issues <- read_optional_csv(trace_path(config$paths$local_validation_dir, "local_issues.csv"))
  p21_issues <- read_optional_csv(trace_path(config$paths$p21_validation_dir, "p21_issues.csv"))
  model_run <- read_optional_json(trace_path(config$paths$recommendation_dir, "model_run.json"), list(status = "not_run", provenance = "not_run"))
  group_failures <- model_run$group_failures %||% data.frame()
  if (is.list(group_failures) && !is.data.frame(group_failures) && length(group_failures)) {
    group_failures <- purrr::map_dfr(group_failures, function(x) tibble::tibble(
      group_id = x$group_id %||% "", stage = x$stage %||% "", error = x$error %||% "",
      concept_ids = paste(unlist(x$concept_ids %||% character()), collapse = " | "), recorded_at = x$recorded_at %||% ""
    ))
  }
  build_run <- read_optional_json(trace_path(config$paths$manifest_dir, "build_manifest.json"), list())
  p21_run <- read_optional_json(trace_path(config$paths$p21_validation_dir, "p21_run.json"), list(exit_status = NA, engine_name = "not_run"))
  registry <- load_transform_registry(config)
  catalog <- registry_catalog_table(registry)
  unit_lineage <- if (nrow(lineage)) dplyr::filter(lineage, transform_id == "standardize_unit") else tibble::tibble()
  generated_defects <- if (nrow(p21_issues)) sum(p21_issues$issue_class == "generated_domain_defect") else 0L
  expected_scope <- if (nrow(p21_issues)) sum(p21_issues$issue_class == "expected_mvp_scope") else 0L
  local_errors <- if (nrow(local_issues)) sum(local_issues$severity == "ERROR") else 0L

  css <- "
    body {font-family: Segoe UI, Arial, sans-serif; margin:0; color:#172033; background:#f4f7fb;}
    main {max-width:1220px; margin:0 auto; padding:28px;} h1 {margin-bottom:4px;}
    h2 {margin-top:34px; border-bottom:2px solid #dce4ef; padding-bottom:8px;}
    h3 {margin-top:24px;} .subtitle,.muted {color:#637083;}
    .grid {display:grid; grid-template-columns:repeat(auto-fit,minmax(170px,1fr)); gap:12px;}
    .metric {background:white; border:1px solid #dce4ef; border-radius:10px; padding:16px; display:flex; flex-direction:column;}
    .metric-value {font-size:26px; font-weight:700;} .metric-label {color:#637083; margin-top:4px;}
    .warning {border-left:5px solid #e7a33e;} .good {border-left:5px solid #2c9b68;}
    .flow {display:flex; flex-wrap:wrap; gap:8px; align-items:center;} .flow span {background:#173b63;color:white;padding:10px 12px;border-radius:7px;}
    .flow b {color:#6f7f94;} .callout {background:#fff7df;border-left:5px solid #e7a33e;padding:14px 18px;border-radius:6px;}
    .table-wrap {overflow:auto; background:white; border:1px solid #dce4ef; border-radius:8px;} table {border-collapse:collapse;width:100%;font-size:13px;}
    th,td {border-bottom:1px solid #e8edf4;text-align:left;padding:8px;vertical-align:top;} th {background:#eaf0f7;position:sticky;top:0;}
    code {background:#eaf0f7;padding:2px 5px;border-radius:4px;} li {margin:6px 0;}
  "
  summary_table <- data.frame(
    metric = names(evaluation),
    value = vapply(evaluation, function(value) paste(value, collapse = ", "), character(1)),
    stringsAsFactors = FALSE
  )
  document <- htmltools::tags$html(
    htmltools::tags$head(
      htmltools::tags$meta(charset = "utf-8"),
      htmltools::tags$title("TraceSDTM 0.4 项目报告"),
      htmltools::tags$style(htmltools::HTML(css))
    ),
    htmltools::tags$body(htmltools::tags$main(
      htmltools::tags$h1("TraceSDTM 0.4"),
      htmltools::tags$p(class = "subtitle", sprintf("%s 场景：人工监督、注册表约束、规格驱动、可追溯的 SDTM 自动化生成", config$project$scenario)),
      htmltools::tags$div(class = "grid",
        metric_card("原始字段", nrow(dictionary)),
        metric_card("原子任务", evaluation$evaluated_concepts %||% 0L),
        metric_card("结构有效任务", evaluation$concepts_with_candidate %||% 0L,
                    if (identical(evaluation$concepts_with_candidate, evaluation$evaluated_concepts)) "good" else "warning"),
        metric_card("拒绝的推荐组", evaluation$rejected_groups %||% 0L,
                    if ((evaluation$rejected_groups %||% 0L) == 0L) "good" else "warning"),
        metric_card("登记函数", length(registry$transforms)),
        metric_card("本地错误", local_errors, if (local_errors == 0L) "good" else "warning"),
        metric_card("Pinnacle 21 域内问题", generated_defects, if (generated_defects == 0L) "good" else "warning"),
        metric_card("范围导致问题", expected_scope)
      ),
      htmltools::tags$h2("职责边界和执行架构"),
      htmltools::tags$div(class = "flow",
        htmltools::tags$span("来源登记与画像"), htmltools::tags$b("→"),
        htmltools::tags$span("原子任务与依赖"), htmltools::tags$b("→"),
        htmltools::tags$span("第一阶段：目标"), htmltools::tags$b("→"),
        htmltools::tags$span("第二阶段：函数"), htmltools::tags$b("→"),
        htmltools::tags$span("确定性参数注入"), htmltools::tags$b("→"),
        htmltools::tags$span("第三阶段：有限参数"), htmltools::tags$b("→"),
        htmltools::tags$span("人工审核"), htmltools::tags$b("→"),
        htmltools::tags$span("确定性 R 构建"), htmltools::tags$b("→"),
        htmltools::tags$span("本地 + Pinnacle 21")
      ),
      htmltools::tags$p("模型只提出候选；注册表限制来源、目标、参数和函数。审核后的 YAML 是唯一构建输入，构建期间不调用模型。"),
      if (identical(model_run$provenance, "reference_seed") || identical(model_run$provider, "seed")) htmltools::tags$div(class = "callout", "当前推荐来自专家金标准种子，不是实际模型输出；评价数字只证明评价程序和流程可复现。") else NULL,
      htmltools::tags$h2("运行信息"),
      html_table(data.frame(
        item = c("场景", "标准", "注册表版本", "注册表校验值", "推荐来源", "模型", "sdtm.oak 版本", "单位换算版本", "Pinnacle 21 引擎", "受控术语版本", "报告时间"),
        value = c(
          config$project$scenario, paste(config$project$standard, config$project$standard_version),
          registry$registry_version, file_sha256(trace_path(config$paths$transform_registry)),
          model_run$provenance %||% model_run$provider %||% "not_run", model_run$model %||% "not_run",
          build_run$sdtm_oak_version %||% "not_run", build_run$unit_conversion_version %||% "not_run",
          p21_run$engine_name %||% "not_run", p21_run$controlled_terminology_version %||% "not_run", utc_now()
        ), stringsAsFactors = FALSE
      )),
      htmltools::tags$h2("三阶段推荐评价"),
      html_table(summary_table),
      htmltools::tags$h3("严格校验拒绝"), html_table(group_failures, 20L),
      htmltools::tags$h3("类别判断"), html_table(classifications, 60L),
      htmltools::tags$h3("候选函数链"), html_table(mapping_detail, 80L),
      htmltools::tags$h2("审核决定和锁定函数链"),
      html_table(if (length(review_summary)) data.frame(
        metric = names(review_summary), value = vapply(review_summary, function(x) paste(x, collapse = ", "), character(1)),
        stringsAsFactors = FALSE
      ) else data.frame(), 20L),
      html_table(review, 60L), html_table(final_steps, 100L),
      htmltools::tags$h2("转换函数注册表"), html_table(catalog, 80L),
      htmltools::tags$h2("高级数据处理证据"),
      htmltools::tags$h3("不完整日期"),
      htmltools::tags$p("不进行日期填补；保持最大已知精度。不能安全计算研究日的记录保持缺失。"),
      html_table(partial_dates, 50L),
      htmltools::tags$h3("单位换算追溯"), html_table(unit_lineage, 50L),
      htmltools::tags$h3("基础场景与高级场景"), html_table(comparison),
      htmltools::tags$h2("生成数据集"), html_table(manifest),
      htmltools::tags$h2("本地检查"), html_table(local_issues),
      htmltools::tags$h2("Pinnacle 21 Community"),
      htmltools::tags$p("原始问题完整保留；缺少 Define-XML、TS 和未覆盖域的问题按最小项目范围分类，不宣称完整提交包通过。"),
      html_table(p21_issues, 100L),
      htmltools::tags$h2("字段级追溯"), html_table(lineage, 100L),
      htmltools::tags$h2("已知边界"),
      htmltools::tags$ul(
        htmltools::tags$li("只生成 DM、AE、VS，不生成 Define-XML、aCRF、ADaM 或 TLF。"),
        htmltools::tags$li("没有部署 CDISC CORE，也没有申报级系统验证、电子签名或多用户权限。"),
        htmltools::tags$li("来源粒度、连接键和临床概念由场景元数据预先登记，模型不能自行发明。"),
        htmltools::tags$li("所有方案仍需合格的临床数据标准专家审核。")
      )
    ))
  )
  output <- trace_path(config$paths$report_dir, "trace_sdtm_report.html")
  ensure_parent(output)
  htmltools::save_html(document, output, background = "white")
  trace_info("已生成 0.4 离线报告：%s", output)
  invisible(output)
}
