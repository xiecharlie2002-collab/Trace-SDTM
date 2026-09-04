# TraceSDTM Studio 0.6 background jobs and diagnostics -----------------------

.trace_studio_jobs <- new.env(parent = emptyenv())
.trace_studio_jobs$active <- NULL

studio_recorded_stage <- function(config, stage, code) {
  if (is.null(config$studio$project_id)) return(force(code))
  project_id <- config$studio$project_id
  run_id <- config$studio$run_id
  studio_update_run_stage(project_id, run_id, stage, "running")
  result <- tryCatch(force(code), error = identity)
  if (inherits(result, "error")) {
    studio_update_run_stage(project_id, run_id, stage, "failed", sanitize_for_log(conditionMessage(result)))
    stop(result)
  }
  outcome <- "completed"
  if (identical(stage, "task_discovery") && identical(as.character(result$status %||% ""), "needs_manual_edit")) {
    outcome <- "needs_information"
  } else if (stage %in% c("recommend_targets", "recommend_functions", "recommend_parameters", "assemble") &&
             length(result$failures %||% list())) {
    outcome <- "needs_information"
  } else if (identical(stage, "recommend_parameters")) {
    resolutions <- result$resolutions$valid %||% list()
    if (any(vapply(resolutions, function(item) length(item$unavailable_parameters %||% list()) > 0L, logical(1)))) outcome <- "needs_information"
  } else if (identical(stage, "ai_review") &&
             (as.integer(result$summary$warning %||% 0L) > 0L || as.integer(result$summary$error %||% 0L) > 0L ||
              length(result$deterministic_validation$issues %||% list()))) {
    outcome <- "completed_with_warnings"
  }
  studio_update_run_stage(project_id, run_id, stage, outcome)
  result
}

studio_job_state_path <- function(config) file.path(trace_path(config$paths$log_dir), "studio_job.json")

studio_job_write_state <- function(config, state) {
  state$updated_at <- utc_now()
  write_json(state, studio_job_state_path(config))
  invisible(state)
}

studio_job_append_log <- function(path, lines, secrets = character()) {
  if (!length(lines)) return(invisible(FALSE))
  values <- unique(c(secrets, Sys.getenv("TRACE_SDTM_API_KEY", unset = "")))
  values <- values[nzchar(values)]
  clean <- as.character(lines)
  for (value in values) clean <- gsub(value, "[REDACTED]", clean, fixed = TRUE)
  ensure_parent(path)
  connection <- file(path, open = "a", encoding = "UTF-8")
  on.exit(close(connection), add = TRUE)
  writeLines(enc2utf8(clean), connection, useBytes = TRUE)
  invisible(TRUE)
}

studio_active_job <- function() .trace_studio_jobs$active

studio_start_job <- function(project_id, run_id, command, credentials = list(),
                             privacy = list(include_examples = FALSE, source_keys = character())) {
  current <- studio_active_job()
  if (!is.null(current) && current$process$is_alive()) trace_abort("工作台已有一个后台任务正在运行。")
  allowed <- c("profile", "discover-tasks", "recommend-targets", "recommend-functions", "recommend-parameters",
               "assemble-recommendations", "recommend", "ai-review", "build", "validate-local", "doctor-p21",
               "validate-p21", "report", "run")
  if (!command %in% allowed) trace_abort("工作台后台命令不在允许列表中。")
  config <- studio_load_run_config(project_id, run_id)
  studio_assert_run_writable(config)
  key <- as.character(credentials$api_key %||% "")
  environment <- c(
    TRACE_SDTM_STUDIO_HOME = studio_home(),
    TRACE_SDTM_PROMPT_PRIVACY = if (isTRUE(privacy$include_examples)) "selected_examples" else "metadata_only",
    TRACE_SDTM_INCLUDE_EXAMPLES = if (isTRUE(privacy$include_examples)) "1" else "0",
    TRACE_SDTM_EXAMPLE_SOURCE_KEYS = paste(unlist(privacy$source_keys %||% character(), use.names = FALSE), collapse = ","),
    TRACE_SDTM_API_KEY = key,
    TRACE_SDTM_BASE_URL = as.character(credentials$base_url %||% Sys.getenv("TRACE_SDTM_BASE_URL", unset = "")),
    TRACE_SDTM_MODEL = as.character(credentials$model %||% Sys.getenv("TRACE_SDTM_MODEL", unset = "")),
    TRACE_SDTM_REVIEW_BASE_URL = as.character(credentials$review_base_url %||% Sys.getenv("TRACE_SDTM_REVIEW_BASE_URL", unset = "")),
    TRACE_SDTM_REVIEW_MODEL = as.character(credentials$review_model %||% Sys.getenv("TRACE_SDTM_REVIEW_MODEL", unset = "")),
    TRACE_SDTM_TIMEOUT_SECONDS = Sys.getenv("TRACE_SDTM_TIMEOUT_SECONDS", unset = ""),
    TRACE_SDTM_MAX_COMPLETION_TOKENS = Sys.getenv("TRACE_SDTM_MAX_COMPLETION_TOKENS", unset = ""),
    TRACE_SDTM_THINKING_MODE = Sys.getenv("TRACE_SDTM_THINKING_MODE", unset = ""),
    TRACE_SDTM_REVIEWER = as.character(credentials$reviewer %||% "")
  )
  environment <- environment[nzchar(environment) | names(environment) %in% c(
    "TRACE_SDTM_PROMPT_PRIVACY", "TRACE_SDTM_INCLUDE_EXAMPLES", "TRACE_SDTM_EXAMPLE_SOURCE_KEYS"
  )]
  args <- c(command, "--project", project_id, "--run", run_id)
  root <- trace_root()
  script <- file.path(root, "scripts", "trace_sdtm.R")
  process <- callr::r_bg(
    function(script, args) {
      executable <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
      status <- system2(executable, c(shQuote(script), args), stdout = "", stderr = "")
      if (is.null(status)) status <- 0L
      if (!identical(as.integer(status), 0L)) stop(sprintf("TraceSDTM 子进程退出状态：%s", status), call. = FALSE)
      TRUE
    },
    args = list(script = script, args = args),
    env = c(callr::rcmd_safe_env(), environment), stdout = "|", stderr = "|", supervise = TRUE
  )
  log_path <- file.path(trace_path(config$paths$log_dir), "studio_job.log")
  state <- list(
    schema_version = "0.6", project_id = project_id, run_id = run_id,
    command = command, status = "running", pid = process$get_pid(),
    started_at = utc_now(), updated_at = utc_now(), exit_status = NULL,
    log_file = basename(log_path), secrets_logged = FALSE
  )
  studio_job_write_state(config, state)
  .trace_studio_jobs$active <- list(
    process = process, config = config, state = state, log_path = log_path,
    secrets = key, privacy = privacy
  )
  if (isTRUE(privacy$include_examples)) {
    studio_append_audit(project_id, "prompt_examples_authorized", list(
      authorized_at = utc_now(), selected_source_keys = as.list(privacy$source_keys %||% character()),
      identifiers_excluded = TRUE
    ), run_id)
  }
  invisible(state)
}

studio_poll_job <- function() {
  job <- studio_active_job()
  if (is.null(job)) return(NULL)
  output <- c(job$process$read_output_lines(), job$process$read_error_lines())
  studio_job_append_log(job$log_path, output, job$secrets)
  if (job$process$is_alive()) {
    job$state$status <- "running"
    studio_job_write_state(job$config, job$state)
    .trace_studio_jobs$active <- job
    return(job$state)
  }
  output <- c(job$process$read_output_lines(), job$process$read_error_lines())
  studio_job_append_log(job$log_path, output, job$secrets)
  exit_status <- job$process$get_exit_status()
  job$state$status <- if (identical(exit_status, 0L)) "completed" else "failed"
  job$state$exit_status <- exit_status
  job$state$finished_at <- utc_now()
  studio_job_write_state(job$config, job$state)
  .trace_studio_jobs$active <- NULL
  job$state
}

studio_cancel_job <- function() {
  job <- studio_active_job()
  if (is.null(job) || !job$process$is_alive()) return(invisible(FALSE))
  job$process$kill_tree()
  job$process$wait(timeout = 5000L)
  job$state$status <- "cancelled"
  job$state$finished_at <- utc_now()
  job$state$exit_status <- job$process$get_exit_status()
  studio_job_write_state(job$config, job$state)
  run <- studio_read_run(job$config$studio$project_id, job$config$studio$run_id)
  running <- names(Filter(function(status) identical(status, "running"), run$stages))
  for (stage in running) studio_update_run_stage(job$config$studio$project_id, job$config$studio$run_id, stage, "cancelled")
  lock <- file.path(job$config$studio$project_path, ".trace-project.lock")
  if (file.exists(lock) && !job$process$is_alive()) unlink(lock, force = TRUE)
  studio_append_audit(job$config$studio$project_id, "background_job_cancelled", list(command = job$state$command), job$config$studio$run_id)
  .trace_studio_jobs$active <- NULL
  invisible(TRUE)
}

studio_recover_interrupted_jobs <- function() {
  projects <- studio_list_projects(include_archived = TRUE)
  projects <- dplyr::filter(projects, .data$compatible)
  recovered <- list()
  for (project_id in projects$project_id) {
    runs <- studio_list_runs(project_id)
    for (run_id in runs$run_id) {
      config <- studio_load_run_config(project_id, run_id)
      state_path <- studio_job_state_path(config)
      if (!file.exists(state_path)) next
      state <- jsonlite::read_json(state_path, simplifyVector = FALSE)
      if (!identical(state$status, "running")) next
      pid <- suppressWarnings(as.integer(state$pid %||% NA_integer_))
      if (!is.na(pid) && pid %in% ps::ps_pids()) next
      state$status <- "interrupted"
      state$finished_at <- utc_now()
      studio_job_write_state(config, state)
      run <- studio_read_run(project_id, run_id)
      running <- names(Filter(function(status) identical(status, "running"), run$stages))
      for (stage in running) studio_update_run_stage(project_id, run_id, stage, "interrupted")
      lock <- file.path(config$studio$project_path, ".trace-project.lock")
      if (file.exists(lock)) unlink(lock, force = TRUE)
      studio_append_audit(project_id, "orphan_job_interrupted", list(command = state$command %||% ""), run_id)
      recovered[[length(recovered) + 1L]] <- paste(project_id, run_id, sep = "/")
    }
  }
  unlist(recovered, use.names = FALSE)
}

studio_doctor <- function() {
  settings <- studio_settings()
  packages <- c("shiny", "bslib", "DT", "callr", "processx", "ps", "zip")
  package_checks <- tibble::tibble(
    category = "R依赖", check = packages,
    passed = vapply(packages, requireNamespace, logical(1), quietly = TRUE),
    observed = vapply(packages, function(package) if (requireNamespace(package, quietly = TRUE)) as.character(utils::packageVersion(package)) else "未安装", character(1))
  )
  root <- studio_home(create = TRUE)
  general <- tibble::tibble(
    category = "工作台", check = c("仅本机监听", "项目目录可写", "R版本", "模型配置"),
    passed = c(identical(settings$host, "127.0.0.1"), file.access(root, 2L) == 0L,
               getRversion() >= "4.5.0",
               all(nzchar(c(Sys.getenv("TRACE_SDTM_BASE_URL", unset = ""), Sys.getenv("TRACE_SDTM_MODEL", unset = ""))))),
    observed = c(settings$host, root, as.character(getRversion()),
                 if (nzchar(Sys.getenv("TRACE_SDTM_MODEL", unset = ""))) "接口地址与模型已配置；密钥状态不落盘" else "可在会话中临时配置")
  )
  p21_checks <- tryCatch({
    p21 <- load_p21_config()
    paths <- p21_expected_paths(load_project_config("advanced"), p21)
    detected <- detect_windows_file_version(paths$executable)
    tibble::tibble(
      category = "Pinnacle 21",
      check = c("Community程序", "Community版本", "Java", "命令行组件", "FDA规则", "受控术语"),
      passed = c(
        file.exists(paths$executable), identical(detected, as.character(p21$community$expected_version)),
        file.exists(paths$java), file.exists(paths$client_jar), file.exists(paths$engine_config),
        file.exists(paths$terminology)
      ),
      observed = c(paths$executable, detected, paths$java, paths$client_jar, paths$engine_config, paths$terminology)
    )
  }, error = function(error) tibble::tibble(
    category = "Pinnacle 21", check = "本机配置", passed = FALSE,
    observed = conditionMessage(error)
  ))
  dplyr::bind_rows(general, package_checks, p21_checks)
}

studio_find_port <- function(requested = NULL) {
  candidates <- if (is.null(requested)) unlist(studio_settings()$port_range, use.names = FALSE) else as.integer(requested)
  for (port in candidates) {
    socket <- tryCatch(serverSocket(port), error = function(error) NULL)
    if (is.null(socket)) next
    close(socket)
    return(as.integer(port))
  }
  if (!is.null(requested)) trace_abort(sprintf("指定端口 %s 已被占用。", requested))
  trace_abort("3838至3848端口均被占用。")
}
