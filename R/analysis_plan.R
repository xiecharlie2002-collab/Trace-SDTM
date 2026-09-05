# Frozen ADaM and table specification ---------------------------------------

registered_analysis_rules <- function() {
  c(
    "COPY_DM", "COPY_AE", "COPY_ADSL", "PARSE_ISO_DATE",
    "TRT_PLANNED_NAME", "TRT_PLANNED_NUMBER", "TRT_ACTUAL_NAME", "TRT_ACTUAL_NUMBER",
    "TRT_DURATION_INCLUSIVE", "POP_ITT_PLANNED_TREATMENT_PRESENT",
    "POP_SAF_FIRST_DOSE_PRESENT", "TEAE_TRT_START_TO_END_PLUS_WINDOW",
    "FIRST_TEAE_SUBJECT", "FIRST_TEAE_SUBJECT_SOC", "FIRST_TEAE_SUBJECT_SOC_PT",
    "T_DEMOGRAPHICS_STANDARD", "T_TEAE_SOC_PT"
  )
}

analysis_plan_candidate_paths <- function(config = load_project_config()) {
  unique(Filter(nzchar, c(
    as.character(config$paths$analysis_plan_frozen %||% ""),
    as.character(config$paths$analysis_plan %||% "")
  )))
}

analysis_plan_path <- function(config = load_project_config(), must_exist = TRUE) {
  candidates <- analysis_plan_candidate_paths(config)
  existing <- candidates[file.exists(vapply(candidates, trace_path, character(1)))]
  if (length(existing)) return(trace_path(existing[[1L]]))
  if (isTRUE(must_exist)) trace_abort("当前项目未配置冻结分析规格；可继续使用 SDTM-only 流程。")
  ""
}

analysis_plan_configured <- function(config = load_project_config()) {
  nzchar(analysis_plan_path(config, must_exist = FALSE))
}

analysis_schema_path <- function(config = load_project_config()) {
  candidates <- unique(Filter(nzchar, c(
    as.character(config$paths$analysis_plan_schema_frozen %||% ""),
    as.character(config$paths$analysis_plan_schema %||% ""),
    "specs/analysis_plan.schema.json"
  )))
  existing <- candidates[file.exists(vapply(candidates, trace_path, character(1)))]
  if (!length(existing)) trace_abort("缺少分析规格 JSON Schema。")
  trace_path(existing[[1L]])
}

validate_analysis_plan <- function(plan, config = load_project_config()) {
  if (!is.list(plan)) trace_abort("分析规格必须是 YAML 对象。")
  json <- jsonlite::toJSON(plan, auto_unbox = TRUE, null = "null", na = "null")
  result <- jsonvalidate::json_validate(
    json, analysis_schema_path(config), engine = "ajv", verbose = TRUE
  )
  if (!isTRUE(result)) {
    details <- attr(result, "errors")
    message <- if (is.null(details)) "未知结构错误" else compact_value(capture.output(print(details)), 500L)
    trace_abort(sprintf("分析规格未通过 JSON Schema 校验：%s", message))
  }

  variables <- unlist(lapply(plan$datasets, function(dataset) {
    vapply(dataset$variables, function(variable) as.character(variable$rule_id), character(1))
  }), use.names = FALSE)
  table_rules <- vapply(plan$tables, function(table) as.character(table$shell_rule_id), character(1))
  unknown <- setdiff(unique(c(variables, table_rules)), registered_analysis_rules())
  if (length(unknown)) trace_abort(sprintf("分析规格引用未登记规则：%s。", paste(unknown, collapse = "、")))

  for (dataset_name in names(plan$datasets)) {
    dataset <- plan$datasets[[dataset_name]]
    names_in_plan <- vapply(dataset$variables, function(variable) as.character(variable$name), character(1))
    if (anyDuplicated(names_in_plan)) trace_abort(sprintf("%s 分析元数据包含重复变量。", dataset_name))
    if (!all(unlist(dataset$keys, use.names = FALSE) %in% names_in_plan)) {
      trace_abort(sprintf("%s 的主键未全部包含在分析元数据中。", dataset_name))
    }
  }
  treatment_names <- vapply(plan$treatments, function(item) as.character(item$name), character(1))
  treatment_codes <- vapply(plan$treatments, function(item) as.character(item$code), character(1))
  treatment_numbers <- vapply(plan$treatments, function(item) as.integer(item$number), integer(1))
  if (anyDuplicated(treatment_names) || anyDuplicated(treatment_codes) || anyDuplicated(treatment_numbers)) {
    trace_abort("分析规格中的治疗名称、代码和编号必须分别唯一。")
  }
  invisible(plan)
}

load_analysis_plan <- function(config = load_project_config()) {
  path <- analysis_plan_path(config)
  plan <- yaml::read_yaml(path)
  validate_analysis_plan(plan, config)
  attr(plan, "path") <- path
  attr(plan, "sha256") <- file_sha256(path)
  plan
}

assert_analysis_package_versions <- function(plan) {
  expected <- c(admiral = as.character(plan$standards$admiral), r2rtf = as.character(plan$standards$r2rtf))
  missing <- names(expected)[!vapply(names(expected), requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) trace_abort(sprintf("缺少分析依赖：%s。", paste(missing, collapse = "、")))
  observed <- vapply(names(expected), function(package) as.character(utils::packageVersion(package)), character(1))
  mismatch <- names(expected)[observed != expected]
  if (length(mismatch)) {
    values <- paste(sprintf("%s 应为 %s，实际为 %s", mismatch, expected[mismatch], observed[mismatch]), collapse = "；")
    trace_abort(sprintf("分析依赖版本与冻结规格不一致：%s。", values))
  }
  invisible(observed)
}

analysis_input_checksums <- function(paths) {
  stats::setNames(as.list(vapply(paths, file_sha256, character(1))), names(paths))
}

freeze_analysis_plan <- function(source, target, config = load_project_config()) {
  source <- normalizePath(source, winslash = "/", mustWork = TRUE)
  plan <- yaml::read_yaml(source)
  validate_analysis_plan(plan, config)
  ensure_parent(target)
  if (!isTRUE(file.copy(source, target, overwrite = TRUE, copy.date = TRUE))) {
    trace_abort("无法冻结分析规格。")
  }
  list(path = target, sha256 = file_sha256(target))
}

read_validation_summary <- function(path, stage_name) {
  if (!file.exists(path)) trace_abort(sprintf("缺少%s检查结果。", stage_name))
  summary <- jsonlite::read_json(path, simplifyVector = TRUE)
  if (!identical(as.character(summary$result), "passed") || as.integer(summary$errors %||% 0L) > 0L) {
    trace_abort(sprintf("%s检查存在错误，已阻止下游构建。", stage_name))
  }
  summary
}
