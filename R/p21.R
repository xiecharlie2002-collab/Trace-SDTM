detect_windows_file_version <- function(path) {
  if (.Platform$OS.type != "windows" || !file.exists(path)) return(NA_character_)
  powershell <- file.path(Sys.getenv("SystemRoot"), "System32", "WindowsPowerShell", "v1.0", "powershell.exe")
  if (!file.exists(powershell)) return(NA_character_)
  escaped <- gsub("'", "''", path, fixed = TRUE)
  command <- sprintf("(Get-Item -LiteralPath '%s').VersionInfo.ProductVersion", escaped)
  output <- suppressWarnings(system2(
    powershell,
    c("-NoProfile", "-Command", shQuote(command, type = "cmd")),
    stdout = TRUE,
    stderr = FALSE
  ))
  if (!length(output)) NA_character_ else trimws(output[[length(output)]])
}

p21_expected_paths <- function(config = load_project_config(), p21 = load_p21_config()) {
  engine <- as.character(p21$validation$engine_version)
  standard_version <- as.character(p21$validation$standard_version)
  filter <- as.character(p21$validation$filter)
  ct <- as.character(p21$validation$controlled_terminology_version)
  list(
    executable = p21$community$executable,
    java = p21$community$java,
    client_jar = p21$community$client_jar,
    engine_config = file.path(p21$community$config_root, engine, sprintf("SDTM-IG %s (%s).xml", standard_version, filter)),
    terminology = file.path(p21$community$config_root, "data", "CDISC", "SDTM", ct, "SDTM Terminology.odm.xml"),
    xpt_dir = trace_path(config$paths$xpt_dir),
    output_dir = trace_path(config$paths$p21_validation_dir)
  )
}

doctor_p21 <- function(config = load_project_config(), stop_on_failure = FALSE) {
  ensure_output_directories(config)
  p21 <- load_p21_config()
  paths <- p21_expected_paths(config, p21)
  detected_version <- detect_windows_file_version(paths$executable)
  checks <- tibble::tibble(
    check = c("community_executable", "community_version", "java", "client_jar", "engine_config", "controlled_terminology", "xpt_directory", "output_directory_writable"),
    passed = c(
      file.exists(paths$executable),
      identical(detected_version, as.character(p21$community$expected_version)),
      file.exists(paths$java),
      file.exists(paths$client_jar),
      file.exists(paths$engine_config),
      file.exists(paths$terminology),
      dir.exists(paths$xpt_dir),
      file.access(paths$output_dir, 2L) == 0L
    ),
    observed = c(
      paths$executable,
      detected_version,
      paths$java,
      paths$client_jar,
      paths$engine_config,
      paths$terminology,
      paths$xpt_dir,
      paths$output_dir
    ),
    expected = c(
      "file exists",
      as.character(p21$community$expected_version),
      "file exists",
      as.character(p21$community$client_version),
      sprintf("FDA %s / SDTMIG %s", p21$validation$engine_version, p21$validation$standard_version),
      as.character(p21$validation$controlled_terminology_version),
      "DM, AE and VS XPT files",
      "writable"
    )
  )
  write_csv(checks, trace_path(config$paths$p21_validation_dir, "p21_doctor.csv"))
  write_json(list(
    checked_at = utc_now(),
    all_passed = all(checks$passed),
    community_version = detected_version,
    checks = as.data.frame(checks)
  ), trace_path(config$paths$p21_validation_dir, "p21_doctor.json"))
  trace_info("Pinnacle 21 环境检查：%s。", if (all(checks$passed)) "通过" else "未通过")
  if (stop_on_failure && !all(checks$passed)) {
    failed <- checks$check[!checks$passed]
    trace_abort(sprintf("Pinnacle 21 环境检查失败：%s", paste(failed, collapse = ", ")))
  }
  invisible(checks)
}

quote_cli_args <- function(args) {
  vapply(args, function(arg) shQuote(as.character(arg), type = "cmd"), character(1))
}

p21_command_args <- function(paths, p21, report_path) {
  c(
    paste0("-Xms", p21$validation$java_initial_memory),
    paste0("-Xmx", p21$validation$java_max_memory),
    "-jar",
    paths$client_jar,
    paste0("--standard=", p21$validation$standard),
    paste0("--standard.version=", p21$validation$standard_version),
    paste0("--engine.version=", p21$validation$engine_version),
    paste0("--filter=", p21$validation$filter),
    paste0("--config=", p21$community$config_root),
    paste0("--source.sdtm=", paths$xpt_dir),
    paste0("--cdisc.ct.sdtm.version=", p21$validation$controlled_terminology_version),
    paste0("--report=", report_path),
    "--report.type=Excel",
    paste0("--report.cutoff=", p21$validation$report_cutoff)
  )
}

read_p21_summary <- function(report_path) {
  raw <- openxlsx::read.xlsx(report_path, sheet = "Validation Summary", colNames = FALSE)
  values <- trimws(as.character(raw[[1]]))
  values <- values[nzchar(values) & grepl(":", values, fixed = TRUE)]
  result <- list()
  for (value in values) {
    position <- regexpr(":", value, fixed = TRUE)[1]
    key <- trimws(substr(value, 1L, position - 1L))
    item <- trimws(substr(value, position + 1L, nchar(value)))
    result[[key]] <- item
  }
  result
}

classify_p21_issue <- function(domain, rule_id, message, variable, value, p21) {
  generated <- unlist(p21$scope$generated_domains, use.names = FALSE)
  omitted <- unlist(p21$scope$omitted_domains, use.names = FALSE)
  text <- paste(rule_id, message, variable, value)
  if (grepl("not configured|dictionary.*missing|Missing.*dictionary", text, ignore.case = TRUE)) {
    return("external_dictionary_unavailable")
  }
  if (rule_id == "SD0057" && grepl("Expected variable", message, fixed = TRUE)) {
    return("expected_mvp_scope")
  }
  if (rule_id %in% c("SD1077", "SD1321")) return("expected_mvp_scope")
  if (identical(rule_id, "DD0101") || grepl("define\\.xml", message, ignore.case = TRUE)) {
    return("expected_mvp_scope")
  }
  if (grepl("^Missing .* dataset", message, ignore.case = TRUE) && any(omitted %in% c(domain, value))) {
    return("expected_mvp_scope")
  }
  if (domain == "GLOBAL" && grepl("Missing .* dataset", message, ignore.case = TRUE)) {
    return("expected_mvp_scope")
  }
  if (domain %in% generated) return("generated_domain_defect")
  "needs_review"
}

parse_p21_report <- function(report_path, config = load_project_config()) {
  if (!file.exists(report_path)) trace_abort(sprintf("Pinnacle 21 报告不存在：%s", report_path))
  required_sheets <- c("Validation Summary", "Dataset Summary", "Issue Summary", "Details", "Rules")
  sheets <- openxlsx::getSheetNames(report_path)
  missing <- setdiff(required_sheets, sheets)
  if (length(missing)) trace_abort(sprintf("Pinnacle 21 报告缺少工作表：%s", paste(missing, collapse = ", ")))

  p21 <- load_p21_config()
  details <- openxlsx::read.xlsx(report_path, sheet = "Details", check.names = FALSE)
  names(details) <- gsub("\\.", " ", names(details))
  required_columns <- c("Domain", "Record", "Count", "Variables", "Values", "Pinnacle 21 ID", "Message", "Severity")
  missing_columns <- setdiff(required_columns, names(details))
  if (length(missing_columns)) trace_abort(sprintf("Pinnacle 21 Details 缺少字段：%s", paste(missing_columns, collapse = ", ")))

  lineage_path <- trace_path(config$paths$lineage_dir, "field_lineage.csv")
  lineage <- if (file.exists(lineage_path)) readr::read_csv(lineage_path, show_col_types = FALSE) else NULL
  run_id <- new_run_id("p21_import")
  normalized <- purrr::map_dfr(seq_len(nrow(details)), function(index) {
    row <- details[index, , drop = FALSE]
    domain <- cell_text(row$Domain, "GLOBAL")
    variable <- cell_text(row$Variables)
    value <- cell_text(row$Values)
    severity_raw <- cell_text(row$Severity)
    rule_id <- cell_text(row[["Pinnacle 21 ID"]])
    message <- cell_text(row$Message)
    tibble::tibble(
      rule_id = rule_id,
      severity = if (nzchar(severity_raw)) severity_raw else "UNSPECIFIED",
      domain = domain,
      variable = variable,
      record_key = paste(Filter(nzchar, c(domain, cell_text(row$Record))), collapse = " | "),
      message = message,
      actual_value = value,
      mapping_id = lineage_mapping_id(lineage, domain, variable, if (variable == "VSTESTCD") value else NULL),
      run_id = run_id,
      validator = "pinnacle21_community",
      issue_class = classify_p21_issue(domain, rule_id, message, variable, value, p21),
      severity_raw = severity_raw,
      p21_record = cell_text(row$Record),
      p21_count = suppressWarnings(as.numeric(row$Count))
    )
  })

  output <- trace_path(config$paths$p21_validation_dir, "p21_issues.csv")
  write_csv(normalized, output)
  summary <- read_p21_summary(report_path)
  write_json(list(
    imported_at = utc_now(),
    report_sha256 = file_sha256(report_path),
    sheet_names = sheets,
    validation_summary = summary,
    issue_count = nrow(normalized),
    issues_by_class = as.list(table(normalized$issue_class)),
    generated_domain_rejects = sum(normalized$issue_class == "generated_domain_defect" & toupper(normalized$severity) == "REJECT")
  ), trace_path(config$paths$p21_validation_dir, "p21_import_summary.json"))
  trace_info("已解析 Pinnacle 21 报告：%s 个明细问题。", nrow(normalized))
  invisible(normalized)
}

import_p21_report <- function(file, config = load_project_config()) {
  source <- normalizePath(file, winslash = "/", mustWork = TRUE)
  destination <- trace_path(config$paths$p21_validation_dir, "p21_report.xlsx")
  ensure_parent(destination)
  if (!identical(tolower(source), tolower(normalizePath(destination, winslash = "/", mustWork = FALSE)))) {
    copied <- file.copy(source, destination, overwrite = TRUE)
    if (!copied) trace_abort("无法复制 Pinnacle 21 报告。")
  }
  parse_p21_report(destination, config)
}

validate_p21 <- function(config = load_project_config()) {
  ensure_output_directories(config)
  doctor_p21(config, stop_on_failure = TRUE)
  p21 <- load_p21_config()
  paths <- p21_expected_paths(config, p21)
  required_xpt <- file.path(paths$xpt_dir, c("dm.xpt", "ae.xpt", "vs.xpt"))
  if (any(!file.exists(required_xpt))) trace_abort("Pinnacle 21 验证前缺少 DM、AE 或 VS XPT。请先执行 build。")

  report_path <- trace_path(config$paths$p21_validation_dir, "p21_report.xlsx")
  log_path <- trace_path(config$paths$p21_validation_dir, "p21_cli.log")
  args <- p21_command_args(paths, p21, report_path)
  started <- utc_now()
  status <- with_working_directory(trace_root(), {
    suppressWarnings(system2(
      paths$java,
      args = quote_cli_args(args),
      stdout = log_path,
      stderr = log_path,
      wait = TRUE
    ))
  })
  status <- as.integer(status %||% 0L)
  process_completed <- file.exists(log_path) && any(grepl("Process completed", readLines(log_path, warn = FALSE), fixed = TRUE))
  manifest <- list(
    started_at = started,
    finished_at = utc_now(),
    exit_status = status,
    execution_status = if (file.exists(report_path) && process_completed) "completed_with_findings" else if (status == 0L) "completed" else "failed",
    process_completed = process_completed,
    report_exists = file.exists(report_path),
    community_version = detect_windows_file_version(paths$executable),
    client_version = as.character(p21$community$client_version),
    engine_name = as.character(p21$validation$engine_name),
    standard = paste0(p21$validation$standard, "IG ", p21$validation$standard_version),
    controlled_terminology_version = as.character(p21$validation$controlled_terminology_version),
    input_sha256 = stats::setNames(vapply(required_xpt, file_sha256, character(1)), basename(required_xpt)),
    report_sha256 = if (file.exists(report_path)) file_sha256(report_path) else NA_character_,
    command_arguments = args,
    secrets_logged = FALSE
  )
  write_json(manifest, trace_path(config$paths$p21_validation_dir, "p21_run.json"))
  if (!file.exists(report_path) || !process_completed) {
    trace_abort(sprintf("Pinnacle 21 命令行验证失败，退出状态 %s。请查看 %s。", status, log_path))
  }
  parse_p21_report(report_path, config)
}
