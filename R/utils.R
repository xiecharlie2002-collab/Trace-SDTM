`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

trace_root <- function() {
  root <- Sys.getenv("TRACE_SDTM_ROOT", unset = "")
  if (!nzchar(root)) {
    root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  }
  normalizePath(root, winslash = "/", mustWork = TRUE)
}

trace_path <- function(...) {
  file.path(trace_root(), ...)
}

trace_abort <- function(message, status = 1L) {
  condition <- structure(
    list(message = message, call = NULL, status = status),
    class = c("trace_sdtm_error", "error", "condition")
  )
  stop(condition)
}

trace_info <- function(...) {
  message(sprintf(...))
}

ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

ensure_parent <- function(path) {
  ensure_dir(dirname(path))
  invisible(path)
}

utc_now <- function() {
  format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

new_run_id <- function(prefix = "run") {
  paste0(prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", Sys.getpid())
}

write_json <- function(value, path, pretty = TRUE) {
  ensure_parent(path)
  jsonlite::write_json(
    value,
    path,
    pretty = pretty,
    auto_unbox = TRUE,
    null = "null",
    na = "null"
  )
  invisible(path)
}

write_csv <- function(value, path) {
  ensure_parent(path)
  readr::write_csv(value, path, na = "")
  invisible(path)
}

file_sha256 <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  digest::digest(file = path, algo = "sha256")
}

data_sha256 <- function(data) {
  normalized <- data
  for (name in names(normalized)) {
    if (is.factor(normalized[[name]])) normalized[[name]] <- as.character(normalized[[name]])
  }
  digest::digest(normalized, algo = "sha256", serialize = TRUE)
}

read_raw_csv <- function(path) {
  readr::read_csv(
    path,
    na = c("", "NA", "N/A"),
    show_col_types = FALSE,
    progress = FALSE,
    name_repair = "minimal"
  )
}

compact_value <- function(x, max_chars = 120L) {
  value <- paste(x, collapse = " | ")
  if (nchar(value) > max_chars) paste0(substr(value, 1L, max_chars - 3L), "...") else value
}

as_json_text <- function(x) {
  as.character(jsonlite::toJSON(x %||% list(), auto_unbox = TRUE, null = "null", na = "null"))
}

from_json_text <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x) || !nzchar(trimws(x))) return(list())
  jsonlite::fromJSON(x, simplifyVector = FALSE)
}

sanitize_for_log <- function(x) {
  key <- Sys.getenv("TRACE_SDTM_API_KEY", unset = "")
  if (nzchar(key)) gsub(key, "[REDACTED]", x, fixed = TRUE) else x
}

with_working_directory <- function(path, code) {
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(path)
  force(code)
}

empty_issue_table <- function() {
  tibble::tibble(
    rule_id = character(),
    severity = character(),
    domain = character(),
    variable = character(),
    record_key = character(),
    message = character(),
    actual_value = character(),
    mapping_id = character(),
    run_id = character(),
    validator = character(),
    issue_class = character()
  )
}
