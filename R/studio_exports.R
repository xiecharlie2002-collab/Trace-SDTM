# TraceSDTM Studio 0.6 reports and evidence exports --------------------------

studio_evidence_files <- function(config) {
  run_path <- config$studio$run_path %||% trace_path(config$paths$output_base)
  allowed <- c("run.yml", "config", "profile", "tasks", "recommendations", "review", "specs",
               "sdtm", "lineage", "validation", "report", "manifests", "logs")
  files <- unlist(lapply(allowed, function(relative) {
    path <- file.path(run_path, relative)
    if (file.exists(path) && !dir.exists(path)) return(path)
    if (!dir.exists(path)) return(character())
    list.files(path, recursive = TRUE, full.names = TRUE, all.files = FALSE, no.. = TRUE)
  }), use.names = FALSE)
  files <- files[file.exists(files) & !dir.exists(files)]
  files <- files[!grepl("(^|[/\\\\])(inputs|raw)([/\\\\]|$)", files, ignore.case = TRUE)]
  files <- files[basename(files) != "source_dictionary.csv"]
  # The manifest is generated from the other evidence files. Including its own
  # previous version would create a permanently stale, self-referential hash.
  files <- files[basename(files) != "evidence_manifest.json"]
  files <- files[!grepl("api[_-]?key|secret|password", basename(files), ignore.case = TRUE)]
  unique(normalizePath(files, winslash = "/", mustWork = TRUE))
}

studio_secret_scan <- function(paths, secrets = character()) {
  secrets <- unique(as.character(secrets))
  secrets <- secrets[nzchar(secrets)]
  text_extensions <- c("yml", "yaml", "json", "jsonl", "csv", "txt", "md", "html", "log", "r", "ps1")
  findings <- list()
  for (path in paths) {
    if (!tolower(tools::file_ext(path)) %in% text_extensions) next
    size <- file.info(path)$size
    if (is.na(size) || size > 20 * 1024^2) next
    content <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    common_pattern <- "(sk-[A-Za-z0-9_-]{16,}|api[_-]?key\\s*[:=]\\s*['\"]?[A-Za-z0-9_-]{16,})"
    if (grepl(common_pattern, content, ignore.case = TRUE, perl = TRUE) ||
        any(vapply(secrets, function(secret) grepl(secret, content, fixed = TRUE), logical(1)))) {
      findings[[length(findings) + 1L]] <- path
    }
  }
  unlist(findings, use.names = FALSE)
}

studio_export_run_audit <- function(config) {
  project_audit <- file.path(config$studio$project_path, "audit", "events.jsonl")
  output <- file.path(trace_path(config$paths$manifest_dir), "audit_events.jsonl")
  if (!file.exists(project_audit)) {
    writeLines(character(), output, useBytes = TRUE)
    return(output)
  }
  lines <- readLines(project_audit, warn = FALSE, encoding = "UTF-8")
  selected <- Filter(function(line) {
    value <- tryCatch(jsonlite::fromJSON(line, simplifyVector = FALSE), error = function(error) NULL)
    !is.null(value) && (identical(as.character(value$run_id %||% ""), config$studio$run_id) ||
                         identical(as.character(value$event %||% ""), "project_created"))
  }, lines)
  ensure_parent(output)
  writeLines(enc2utf8(unlist(selected)), output, useBytes = TRUE)
  output
}

studio_export_evidence <- function(config, secrets = character()) {
  if (is.null(config$studio$project_id)) trace_abort("证据包导出只适用于工作台项目。")
  project <- studio_read_project(config$studio$project_id)
  read_only <- !identical(as.character(project$active_run_id %||% ""), as.character(config$studio$run_id)) ||
    isTRUE(studio_read_run(config$studio$project_id, config$studio$run_id)$stale) || isTRUE(project$archived)
  if (read_only) {
    pattern <- sprintf("^%s_%s_evidence_[0-9]{8}_[0-9]{6}\\.zip$", config$studio$project_id, config$studio$run_id)
    existing <- list.files(file.path(config$studio$project_path, "exports"), pattern = pattern, full.names = TRUE)
    if (!length(existing)) trace_abort("历史或归档运行为只读状态，且没有可下载的既有证据包。")
    return(existing[[which.max(file.info(existing)$mtime)]])
  }
  studio_assert_run_writable(config)
  run_path <- config$studio$run_path
  approved <- trace_path(config$paths$approved_specification)
  if (!file.exists(approved)) trace_abort("尚未批准映射规格，不能导出正式证据包。")
  studio_export_run_audit(config)
  dictionary_path <- file.path(trace_path(config$paths$profile_dir), "source_dictionary.csv")
  if (file.exists(dictionary_path)) {
    dictionary <- readr::read_csv(dictionary_path, show_col_types = FALSE)
    dictionary <- dplyr::select(dictionary, -dplyr::any_of("example_values"))
    write_csv(dictionary, file.path(trace_path(config$paths$manifest_dir), "source_dictionary_metadata.csv"))
  }
  files <- studio_evidence_files(config)
  findings <- studio_secret_scan(files, secrets)
  if (length(findings)) trace_abort(sprintf("证据包密钥扫描未通过：%s。", paste(basename(findings), collapse = "、")))
  relative <- vapply(files, studio_relative_path, character(1), root = run_path)
  manifest <- list(
    schema_version = "0.6", project_id = config$studio$project_id, run_id = config$studio$run_id,
    generated_at = utc_now(), raw_data_included = FALSE,
    files = lapply(seq_along(files), function(index) list(path = relative[[index]], sha256 = file_sha256(files[[index]])))
  )
  manifest_path <- file.path(trace_path(config$paths$manifest_dir), "evidence_manifest.json")
  write_json(manifest, manifest_path)
  files <- unique(c(files, normalizePath(manifest_path, winslash = "/", mustWork = TRUE)))
  relative <- vapply(files, studio_relative_path, character(1), root = run_path)
  export_dir <- file.path(config$studio$project_path, "exports")
  ensure_dir(export_dir)
  target <- file.path(export_dir, sprintf(
    "%s_%s_evidence_%s.zip", config$studio$project_id, config$studio$run_id,
    format(Sys.time(), "%Y%m%d_%H%M%S")
  ))
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(run_path)
  zip::zip(target, files = relative, recurse = FALSE, include_directories = FALSE,
           root = run_path, mode = "mirror")
  if (!file.exists(target)) trace_abort("证据包生成失败。")
  studio_append_audit(config$studio$project_id, "evidence_exported", list(
    run_id = config$studio$run_id, archive_name = basename(target), archive_sha256 = file_sha256(target),
    file_count = length(files), raw_data_included = FALSE
  ), config$studio$run_id)
  target
}

studio_run_summary <- function(config) {
  run <- studio_read_run(config$studio$project_id, config$studio$run_id)
  manifest_path <- file.path(trace_path(config$paths$manifest_dir), "dataset_manifest.csv")
  local_path <- file.path(trace_path(config$paths$local_validation_dir), "local_issues.csv")
  p21_path <- file.path(trace_path(config$paths$p21_validation_dir), "p21_issues.csv")
  list(
    run = run,
    datasets = if (file.exists(manifest_path)) readr::read_csv(manifest_path, show_col_types = FALSE) else tibble::tibble(),
    local_issues = if (file.exists(local_path)) readr::read_csv(local_path, show_col_types = FALSE) else empty_issue_table(),
    p21_issues = if (file.exists(p21_path)) readr::read_csv(p21_path, show_col_types = FALSE) else empty_issue_table(),
    lineage = {
      path <- file.path(trace_path(config$paths$lineage_dir), "field_lineage.csv")
      if (file.exists(path)) readr::read_csv(path, show_col_types = FALSE) else tibble::tibble()
    },
    report = file.path(trace_path(config$paths$report_dir), "trace_sdtm_report.html")
  )
}
