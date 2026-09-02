lineage_mapping_id <- function(lineage, domain, variable, value = NULL) {
  if (is.null(lineage) || !nrow(lineage)) return("")
  domain <- as.character(domain)[1]
  variable <- as.character(variable)[1]
  target_column <- if ("target_variables" %in% names(lineage)) "target_variables" else "target_variable"
  id_column <- if ("concept_id" %in% names(lineage)) "concept_id" else "mapping_id"
  target_values <- as.character(lineage[[target_column]])
  hit <- lineage$target_domain == domain & vapply(
    strsplit(target_values, " | ", fixed = TRUE), function(items) variable %in% items, logical(1)
  )
  paste(unique(lineage[[id_column]][hit]), collapse = " | ")
}

record_keys <- function(data, domain, metadata) {
  keys <- intersect(unlist(metadata$domains[[domain]]$keys, use.names = FALSE), names(data))
  if (!length(keys)) return(rep("", nrow(data)))
  apply(as.data.frame(lapply(data[keys], as.character), stringsAsFactors = FALSE), 1L, paste, collapse = " | ")
}

issue_rows <- function(data, indices, rule_id, severity, domain, variable, message,
                       run_id, metadata, lineage, actual = NULL, mapping_value = NULL) {
  indices <- which(indices)
  if (!length(indices)) return(empty_issue_table())
  keys <- record_keys(data, domain, metadata)
  actual_value <- if (is.null(actual)) {
    if (variable %in% names(data)) as.character(data[[variable]][indices]) else rep("", length(indices))
  } else if (length(actual) == nrow(data)) {
    as.character(actual[indices])
  } else {
    rep(as.character(actual), length(indices))
  }
  tibble::tibble(
    rule_id = rule_id,
    severity = severity,
    domain = domain,
    variable = variable,
    record_key = keys[indices],
    message = message,
    actual_value = actual_value,
    mapping_id = lineage_mapping_id(lineage, domain, variable, mapping_value),
    run_id = run_id,
    validator = "local",
    issue_class = "generated_domain_defect"
  )
}

dataset_issue <- function(rule_id, severity, domain, variable, message, run_id, lineage, actual = "") {
  tibble::tibble(
    rule_id = rule_id,
    severity = severity,
    domain = domain,
    variable = variable,
    record_key = "",
    message = message,
    actual_value = as.character(actual),
    mapping_id = lineage_mapping_id(lineage, domain, variable),
    run_id = run_id,
    validator = "local",
    issue_class = "generated_domain_defect"
  )
}

validate_one_domain <- function(data, domain, metadata, lineage, run_id) {
  domain_metadata <- metadata$domains[[domain]]
  variables <- domain_metadata$variables
  issues <- list()
  add <- function(value) issues[[length(issues) + 1L]] <<- value

  expected <- unlist(domain_metadata$expected_order, use.names = FALSE)
  required <- names(Filter(function(item) isTRUE(item$required), variables))
  missing_variables <- setdiff(required, names(data))
  for (variable in missing_variables) {
    add(dataset_issue("LOCAL001", "ERROR", domain, variable, "缺少必需变量。", run_id, lineage))
  }

  if ("DOMAIN" %in% names(data)) {
    add(issue_rows(data, is.na(data$DOMAIN) | data$DOMAIN != domain, "LOCAL002", "ERROR", domain, "DOMAIN", "DOMAIN 与数据集不一致。", run_id, metadata, lineage))
  }

  keys <- unlist(domain_metadata$keys, use.names = FALSE)
  if (all(keys %in% names(data))) {
    duplicate <- duplicated(data[keys]) | duplicated(data[keys], fromLast = TRUE)
    add(issue_rows(data, duplicate, "LOCAL003", "ERROR", domain, paste(keys, collapse = ","), "主键不唯一。", run_id, metadata, lineage, actual = record_keys(data, domain, metadata)))
  }

  for (variable in intersect(required, names(data))) {
    missing_value <- is.na(data[[variable]]) | (is.character(data[[variable]]) & !nzchar(trimws(data[[variable]])))
    add(issue_rows(data, missing_value, "LOCAL004", "ERROR", domain, variable, "必需变量存在缺失值。", run_id, metadata, lineage))
  }

  for (variable in intersect(names(variables), names(data))) {
    definition <- variables[[variable]]
    expected_type <- definition$type %||% ""
    type_ok <- switch(
      expected_type,
      character = is.character(data[[variable]]),
      numeric = is.numeric(data[[variable]]),
      TRUE
    )
    if (!type_ok) add(dataset_issue("LOCAL005", "ERROR", domain, variable, sprintf("字段类型应为 %s。", expected_type), run_id, lineage, class(data[[variable]])[1]))

    controlled <- unlist(definition$controlled %||% character(), use.names = FALSE)
    if (length(controlled)) {
      value <- as.character(data[[variable]])
      bad <- !is.na(value) & nzchar(value) & !value %in% controlled
      add(issue_rows(data, bad, "LOCAL006", "ERROR", domain, variable, "值不在项目允许的受控术语中。", run_id, metadata, lineage))
    }

    if (isTRUE(definition$iso8601)) {
      value <- as.character(data[[variable]])
      missing_value <- is.na(value) | !nzchar(trimws(value))
      syntactic <- missing_value | grepl("^\\d{4}(-\\d{2}(-\\d{2}(T.*)?)?)?$", value)
      complete <- !missing_value & grepl("^\\d{4}-\\d{2}-\\d{2}", value)
      valid_date <- rep(TRUE, length(value))
      valid_date[complete] <- !is.na(as.Date(substr(value[complete], 1L, 10L), format = "%Y-%m-%d"))
      add(issue_rows(data, !syntactic | !valid_date, "LOCAL007", "ERROR", domain, variable, "日期不符合 ISO 8601 或不是有效日期。", run_id, metadata, lineage))
    }

    if (is.character(data[[variable]])) {
      too_long <- !is.na(data[[variable]]) & nchar(data[[variable]], type = "bytes") > 200L
      add(issue_rows(data, too_long, "LOCAL008", "ERROR", domain, variable, "字符长度超过 XPT V5 的 200 字节限制。", run_id, metadata, lineage))
    }
  }

  if (!identical(names(data), expected)) {
    add(dataset_issue("LOCAL009", "ERROR", domain, "", "字段顺序或字段集合与项目元数据不一致。", run_id, lineage, paste(names(data), collapse = ",")))
  }
  dplyr::bind_rows(issues)
}

validate_subject_links <- function(datasets, metadata, lineage, run_id) {
  dm_ids <- unique(datasets$DM$USUBJID)
  purrr::map_dfr(c("AE", "VS"), function(domain) {
    data <- datasets[[domain]]
    issue_rows(
      data,
      !data$USUBJID %in% dm_ids,
      "LOCAL010",
      "ERROR",
      domain,
      "USUBJID",
      "USUBJID 在 DM 中不存在。",
      run_id,
      metadata,
      lineage
    )
  })
}

validate_sequences <- function(datasets, metadata, lineage, run_id) {
  purrr::imap_dfr(c(AE = "AESEQ", VS = "VSSEQ"), function(sequence_variable, domain) {
    data <- datasets[[domain]]
    grouped <- split(data[[sequence_variable]], data$USUBJID)
    invalid_subjects <- names(Filter(function(value) {
      any(is.na(value)) || anyDuplicated(value) || !setequal(value, seq_len(length(value)))
    }, grouped))
    issue_rows(
      data,
      data$USUBJID %in% invalid_subjects,
      "LOCAL011",
      "ERROR",
      domain,
      sequence_variable,
      "受试者内序号必须从 1 开始、连续且唯一。",
      run_id,
      metadata,
      lineage
    )
  })
}

validate_ae_dates <- function(data, metadata, lineage, run_id) {
  start <- suppressWarnings(as.Date(data$AESTDTC))
  end <- suppressWarnings(as.Date(data$AEENDTC))
  issue_rows(
    data,
    !is.na(start) & !is.na(end) & start > end,
    "LOCAL012",
    "ERROR",
    "AE",
    "AEENDTC",
    "AEENDTC 早于 AESTDTC。",
    run_id,
    metadata,
    lineage,
    actual = paste(data$AESTDTC, data$AEENDTC, sep = " -> ")
  )
}

validate_vs_pairs <- function(data, metadata, lineage, run_id) {
  pairs <- metadata$domains$VS$test_pairs
  expected_test <- vapply(pairs, function(item) item$test, character(1))
  expected_unit <- vapply(pairs, function(item) item$unit, character(1))
  test <- unname(expected_test[data$VSTESTCD])
  unit <- unname(expected_unit[data$VSTESTCD])
  conversions <- load_unit_conversions(load_project_config())$sets$vs_standard_v1
  conversion_table <- purrr::map_dfr(conversions, tibble::as_tibble)
  original_unit_valid <- vapply(seq_len(nrow(data)), function(index) {
    any(
      conversion_table$test_code == data$VSTESTCD[[index]] &
        conversion_table$from_unit == data$VSORRESU[[index]] &
        conversion_table$to_unit == unit[[index]]
    )
  }, logical(1))
  issues <- list(
    issue_rows(data, is.na(test) | data$VSTEST != test, "LOCAL013", "ERROR", "VS", "VSTEST", "VSTESTCD 与 VSTEST 不匹配。", run_id, metadata, lineage),
    issue_rows(data, is.na(unit) | data$VSSTRESU != unit, "LOCAL014", "ERROR", "VS", "VSSTRESU", "标准单位与检查项目不匹配。", run_id, metadata, lineage),
    issue_rows(data, !original_unit_valid, "LOCAL016", "ERROR", "VS", "VSORRESU", "原始单位没有登记到受控换算表。", run_id, metadata, lineage),
    issue_rows(data, is.na(data$VSSTRESN), "LOCAL015", "ERROR", "VS", "VSSTRESN", "数值型生命体征无法转换为标准数值。", run_id, metadata, lineage)
  )
  dplyr::bind_rows(issues)
}

validate_partial_date_derivations <- function(datasets, metadata, lineage, run_id) {
  checks <- list(
    list(domain = "AE", date = "AESTDTC", derived = "AESTDY"),
    list(domain = "AE", date = "AEENDTC", derived = "AEENDY"),
    list(domain = "VS", date = "VSDTC", derived = "VSDY")
  )
  purrr::map_dfr(checks, function(check) {
    data <- datasets[[check$domain]]
    if (!all(c(check$date, check$derived) %in% names(data))) return(empty_issue_table())
    value <- as.character(data[[check$date]])
    incomplete <- !is.na(value) & nzchar(value) & !grepl("^\\d{4}-\\d{2}-\\d{2}", value)
    issue_rows(
      data, incomplete & !is.na(data[[check$derived]]), "LOCAL017", "ERROR", check$domain,
      check$derived, "不完整日期不能派生研究日。", run_id, metadata, lineage
    )
  })
}

validate_local <- function(config = load_project_config()) {
  datasets <- load_built_datasets(config)
  metadata <- load_metadata(config)
  lineage_path <- trace_path(config$paths$lineage_dir, "field_lineage.csv")
  lineage <- if (file.exists(lineage_path)) readr::read_csv(lineage_path, show_col_types = FALSE) else NULL
  run_id <- new_run_id("local_validation")

  issues <- purrr::imap_dfr(datasets, ~ validate_one_domain(.x, .y, metadata, lineage, run_id))
  issues <- dplyr::bind_rows(
    issues,
    validate_subject_links(datasets, metadata, lineage, run_id),
    validate_sequences(datasets, metadata, lineage, run_id),
    validate_ae_dates(datasets$AE, metadata, lineage, run_id),
    validate_vs_pairs(datasets$VS, metadata, lineage, run_id),
    validate_partial_date_derivations(datasets, metadata, lineage, run_id)
  )
  if (!nrow(issues)) issues <- empty_issue_table()
  output <- trace_path(config$paths$local_validation_dir, "local_issues.csv")
  write_csv(issues, output)
  write_json(list(
    run_id = run_id,
    generated_at = utc_now(),
    validator = "local",
    issue_count = nrow(issues),
    errors = sum(issues$severity == "ERROR"),
    warnings = sum(issues$severity == "WARNING"),
    result = if (any(issues$severity == "ERROR")) "failed" else "passed",
    report_sha256 = file_sha256(output)
  ), trace_path(config$paths$local_validation_dir, "local_validation_summary.json"))
  trace_info("本地检查完成：%s 个问题。", nrow(issues))
  invisible(issues)
}
