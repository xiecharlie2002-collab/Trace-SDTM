# Local ADaM checks; these do not claim full CDISC conformance ---------------

analysis_issue_table <- function() {
  tibble::tibble(
    rule_id = character(), severity = character(), dataset = character(), variable = character(),
    record_key = character(), message = character(), actual_value = character()
  )
}

analysis_issue <- function(rule_id, dataset, variable, message, actual = "", record_key = "") {
  tibble::tibble(
    rule_id = rule_id, severity = "ERROR", dataset = dataset, variable = variable,
    record_key = as.character(record_key), message = message, actual_value = as.character(actual)
  )
}

check_analysis_structure <- function(data, dataset, plan) {
  issues <- list()
  expected <- analysis_variable_names(plan, dataset)
  if (!identical(names(data), expected)) {
    issues[[length(issues) + 1L]] <- analysis_issue(
      "ADAM001", dataset, "", "变量集合或顺序与冻结分析元数据不一致。", paste(names(data), collapse = ",")
    )
  }
  keys <- unlist(plan$datasets[[dataset]]$keys, use.names = FALSE)
  if (all(keys %in% names(data))) {
    duplicate <- duplicated(data[keys]) | duplicated(data[keys], fromLast = TRUE)
    if (any(duplicate)) {
      values <- apply(as.data.frame(lapply(data[duplicate, keys, drop = FALSE], as.character)), 1L, paste, collapse = " | ")
      issues[[length(issues) + 1L]] <- analysis_issue(
        "ADAM002", dataset, paste(keys, collapse = ","), "主键不唯一。", paste(unique(values), collapse = "; ")
      )
    }
  }
  definitions <- plan$datasets[[dataset]]$variables
  for (definition in definitions) {
    variable <- as.character(definition$name)
    if (!variable %in% names(data)) next
    expected_type <- as.character(definition$type)
    valid <- switch(
      expected_type,
      character = is.character(data[[variable]]),
      numeric = is.numeric(data[[variable]]),
      date = inherits(data[[variable]], "Date"),
      FALSE
    )
    if (!valid) issues[[length(issues) + 1L]] <- analysis_issue(
      "ADAM003", dataset, variable, sprintf("变量类型应为 %s。", expected_type), class(data[[variable]])[[1L]]
    )
  }
  dplyr::bind_rows(issues) %||% analysis_issue_table()
}

validate_adsl_content <- function(adsl, dm, plan) {
  issues <- list()
  add <- function(value) issues[[length(issues) + 1L]] <<- value
  if (nrow(adsl) != nrow(dm) || dplyr::n_distinct(adsl$USUBJID) != nrow(adsl)) {
    add(analysis_issue("ADSL001", "ADSL", "USUBJID", "ADSL 必须每名 DM 受试者一条记录。", nrow(adsl)))
  }
  lookup <- treatment_lookup(plan)
  planned <- lookup$number[match(adsl$TRT01P, lookup$name)]
  actual <- lookup$number[match(adsl$TRT01A, lookup$name)]
  bad_planned <- which(is.na(planned) | adsl$TRT01PN != planned)
  bad_actual <- which(is.na(actual) | adsl$TRT01AN != actual)
  if (length(bad_planned)) add(analysis_issue(
    "ADSL002", "ADSL", "TRT01PN", "计划治疗名称与编号不一致。",
    paste(adsl$USUBJID[bad_planned], collapse = "; ")
  ))
  if (length(bad_actual)) add(analysis_issue(
    "ADSL003", "ADSL", "TRT01AN", "实际治疗名称与编号不一致。",
    paste(adsl$USUBJID[bad_actual], collapse = "; ")
  ))
  bad_order <- which(!is.na(adsl$TRTSDT) & !is.na(adsl$TRTEDT) & adsl$TRTSDT > adsl$TRTEDT)
  if (length(bad_order)) add(analysis_issue(
    "ADSL004", "ADSL", "TRTEDT", "末次给药日期早于首次给药日期。", paste(adsl$USUBJID[bad_order], collapse = "; ")
  ))
  expected_duration <- as.numeric(adsl$TRTEDT - adsl$TRTSDT) + 1
  bad_duration <- which(!(is.na(expected_duration) & is.na(adsl$TRTDURD)) & (is.na(expected_duration) | is.na(adsl$TRTDURD) | expected_duration != adsl$TRTDURD))
  if (length(bad_duration)) add(analysis_issue(
    "ADSL005", "ADSL", "TRTDURD", "治疗持续时间必须按起止日期含首尾计算。", paste(adsl$USUBJID[bad_duration], collapse = "; ")
  ))
  expected_itt <- ifelse(!is.na(adsl$TRT01P) & nzchar(adsl$TRT01P), "Y", "N")
  expected_saf <- ifelse(!is.na(adsl$TRTSDT), "Y", "N")
  if (any(adsl$ITTFL != expected_itt, na.rm = TRUE) || any(is.na(adsl$ITTFL))) add(analysis_issue(
    "ADSL006", "ADSL", "ITTFL", "意向治疗人群标志与登记规则不一致。"
  ))
  if (any(adsl$SAFFL != expected_saf, na.rm = TRUE) || any(is.na(adsl$SAFFL))) add(analysis_issue(
    "ADSL007", "ADSL", "SAFFL", "安全性人群标志与登记规则不一致。"
  ))
  missing_source <- setdiff(dm$USUBJID, adsl$USUBJID)
  if (length(missing_source)) add(analysis_issue(
    "ADSL008", "ADSL", "USUBJID", "存在未进入 ADSL 的 DM 受试者。", paste(missing_source, collapse = "; ")
  ))
  dplyr::bind_rows(issues) %||% analysis_issue_table()
}

validate_occurrence_flag <- function(adae, grouping, variable, rule_id) {
  marked <- adae[adae[[variable]] == "Y" & !is.na(adae[[variable]]), , drop = FALSE]
  eligible <- adae[adae$TRTEMFL == "Y" & !is.na(adae$TRTEMFL), , drop = FALSE]
  expected_groups <- if (nrow(eligible)) unique(eligible[grouping]) else eligible[grouping]
  marked_groups <- if (nrow(marked)) unique(marked[grouping]) else marked[grouping]
  key <- function(value) if (!nrow(value)) character() else do.call(paste, c(lapply(value, as.character), sep = "\r"))
  invalid_values <- any(!is.na(adae[[variable]]) & adae[[variable]] != "Y")
  duplicates <- if (nrow(marked)) anyDuplicated(key(marked[grouping])) > 0L else FALSE
  missing <- setdiff(key(expected_groups), key(marked_groups))
  extra <- setdiff(key(marked_groups), key(expected_groups))
  if (invalid_values || duplicates || length(missing) || length(extra)) {
    return(analysis_issue(rule_id, "ADAE", variable, "首次发生标志必须在每个治疗中出现事件层级恰好标记一条记录。"))
  }
  analysis_issue_table()
}

validate_adae_content <- function(adae, ae, adsl, plan) {
  issues <- list()
  add <- function(value) issues[[length(issues) + 1L]] <<- value
  if (nrow(adae) != nrow(ae)) add(analysis_issue(
    "ADAE001", "ADAE", "", "ADAE 记录数必须与来源 AE 一致。", sprintf("ADAE=%d, AE=%d", nrow(adae), nrow(ae))
  ))
  missing_subjects <- setdiff(unique(adae$USUBJID), adsl$USUBJID)
  if (length(missing_subjects)) add(analysis_issue(
    "ADAE002", "ADAE", "USUBJID", "ADAE 受试者无法关联 ADSL。", paste(missing_subjects, collapse = "; ")
  ))
  reversed <- which(!is.na(adae$ASTDT) & !is.na(adae$AENDT) & adae$ASTDT > adae$AENDT)
  if (length(reversed)) add(analysis_issue(
    "ADAE003", "ADAE", "AENDT", "分析结束日期早于分析开始日期。",
    paste(paste(adae$USUBJID[reversed], adae$AESEQ[reversed], sep = "/"), collapse = "; ")
  ))
  if (any(!is.na(adae$TRTEMFL) & adae$TRTEMFL != "Y")) add(analysis_issue(
    "ADAE004", "ADAE", "TRTEMFL", "治疗中出现标志只能取 Y 或缺失。"
  ))
  recomputed <- admiral::derive_var_trtemfl(
    dplyr::select(adae, dplyr::all_of(c("USUBJID", "ASTDT", "AENDT", "TRTSDT", "TRTEDT"))),
    start_date = ASTDT, end_date = AENDT, trt_start_date = TRTSDT, trt_end_date = TRTEDT,
    end_window = as.integer(plan$teae$end_window_days)
  )$TRTEMFL
  mismatch <- which(ifelse(is.na(adae$TRTEMFL), "", adae$TRTEMFL) != ifelse(is.na(recomputed), "", recomputed))
  if (length(mismatch)) add(analysis_issue(
    "ADAE005", "ADAE", "TRTEMFL", "治疗中出现标志与冻结窗口规则不一致。",
    paste(paste(adae$USUBJID[mismatch], adae$AESEQ[mismatch], sep = "/"), collapse = "; ")
  ))
  subject_match <- match(adae$USUBJID, adsl$USUBJID)
  treatment_mismatch <- is.na(subject_match) |
    ifelse(is.na(adae$TRTA), "", adae$TRTA) != ifelse(is.na(adsl$TRT01A[subject_match]), "", adsl$TRT01A[subject_match]) |
    ifelse(is.na(adae$TRTAN), -999, adae$TRTAN) != ifelse(is.na(adsl$TRT01AN[subject_match]), -999, adsl$TRT01AN[subject_match])
  if (any(treatment_mismatch)) add(analysis_issue(
    "ADAE006", "ADAE", "TRTA", "ADAE 治疗变量与 ADSL 不一致。",
    paste(unique(adae$USUBJID[treatment_mismatch]), collapse = "; ")
  ))
  add(validate_occurrence_flag(adae, c("USUBJID"), "AOCCFL", "ADAE007"))
  add(validate_occurrence_flag(adae, c("USUBJID", "AEBODSYS"), "AOCCSFL", "ADAE008"))
  add(validate_occurrence_flag(adae, c("USUBJID", "AEBODSYS", "AEDECOD"), "AOCCPFL", "ADAE009"))
  dplyr::bind_rows(issues) %||% analysis_issue_table()
}

validate_adam_datasets <- function(datasets, sdtm, plan) {
  required <- c("ADSL", "ADAE")
  missing <- setdiff(required, names(datasets))
  if (length(missing)) return(analysis_issue("ADAM000", paste(missing, collapse = ","), "", "缺少分析数据集。"))
  dplyr::bind_rows(
    check_analysis_structure(datasets$ADSL, "ADSL", plan),
    check_analysis_structure(datasets$ADAE, "ADAE", plan),
    validate_adsl_content(datasets$ADSL, sdtm$DM, plan),
    validate_adae_content(datasets$ADAE, sdtm$AE, datasets$ADSL, plan)
  )
}

validate_adam <- function(config = load_project_config()) {
  ensure_output_directories(config)
  plan <- load_analysis_plan(config)
  assert_analysis_package_versions(plan)
  datasets <- load_adam_datasets(config)
  sdtm <- load_built_datasets(config)
  issues <- validate_adam_datasets(datasets, sdtm, plan)
  if (!nrow(issues)) issues <- analysis_issue_table()
  path <- trace_path(config$paths$adam_validation_dir, "adam_issues.csv")
  write_csv(issues, path)
  write_json(list(
    generated_at = utc_now(), validator = "TraceSDTM local ADaM checks",
    scope = "local rules; not full ADaM/CDISC conformance validation",
    issue_count = nrow(issues), errors = sum(issues$severity == "ERROR"), warnings = 0L,
    result = if (nrow(issues)) "failed" else "passed", report_sha256 = file_sha256(path)
  ), trace_path(config$paths$adam_validation_dir, "adam_validation_summary.json"))
  trace_info("ADaM 本地检查完成：%d 个问题。", nrow(issues))
  invisible(issues)
}
