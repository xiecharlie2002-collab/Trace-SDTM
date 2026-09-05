argument_value <- function(args, name, default = NULL) {
  position <- match(name, args)
  if (is.na(position)) return(default)
  if (position == length(args)) trace_abort(sprintf("%s 后必须提供值。", name))
  as.character(args[[position + 1L]])
}

configure_output_paths <- function(config, output_base) {
  config$paths$output_base <- output_base
  generated_paths <- c(
    profile_dir = "profile", task_dir = "tasks", recommendation_dir = "recommendations",
    review_dir = "review", csv_dir = file.path("sdtm", "csv"),
    xpt_dir = file.path("sdtm", "xpt"), lineage_dir = "lineage",
    local_validation_dir = file.path("validation", "local"),
    p21_validation_dir = file.path("validation", "p21"), report_dir = "report",
    manifest_dir = "manifests", log_dir = "logs"
  )
  for (key in names(generated_paths)) {
    config$paths[[key]] <- file.path(output_base, generated_paths[[key]])
  }
  config$paths$approved_specification <- file.path(output_base, "specs", "approved_mapping.yml")
  config
}

load_project_config <- function() {
  config <- yaml::read_yaml(trace_path("config", "project.yml"))
  config$project$scenario <- "generic"
  config$project$studio <- FALSE
  configure_output_paths(config, config$paths$output_base %||% "output/local")
}

load_p21_config <- function(local_path = Sys.getenv(
  "TRACE_SDTM_P21_CONFIG", unset = trace_path("config", "p21.local.yml")
)) {
  config <- yaml::read_yaml(trace_path("config", "p21.yml"))
  if (nzchar(local_path)) {
    is_absolute <- grepl("^([A-Za-z]:[\\\\/]|/|\\\\\\\\)", local_path)
    resolved_local_path <- if (is_absolute) local_path else trace_path(local_path)
    if (file.exists(resolved_local_path)) {
      local_config <- yaml::read_yaml(resolved_local_path)
      if (!is.list(local_config)) trace_abort("Pinnacle 21 本地配置必须是 YAML 对象。")
      config <- utils::modifyList(config, local_config, keep.null = TRUE)
    }
  }
  environment_overrides <- list(
    TRACE_SDTM_P21_EXECUTABLE = c("community", "executable"),
    TRACE_SDTM_P21_JAVA = c("community", "java"),
    TRACE_SDTM_P21_CLIENT_JAR = c("community", "client_jar"),
    TRACE_SDTM_P21_CONFIG_ROOT = c("community", "config_root")
  )
  for (variable in names(environment_overrides)) {
    value <- Sys.getenv(variable, unset = "")
    if (!nzchar(value)) next
    path <- environment_overrides[[variable]]
    config[[path[[1L]]]][[path[[2L]]]] <- value
  }
  config
}

load_metadata <- function(config = load_project_config()) {
  yaml::read_yaml(trace_path(config$paths$metadata))
}

load_task_specification <- function(config = load_project_config()) {
  path <- as.character(config$paths$task_specification %||% "")
  if (!nzchar(path) || !file.exists(trace_path(path))) {
    trace_abort("缺少当前运行的冻结任务规格；请从工作台创建项目和运行。")
  }
  yaml::read_yaml(trace_path(path))
}

load_mapping_policies <- function(config = load_project_config()) {
  path <- as.character(config$paths$mapping_policies %||% "")
  if (!nzchar(path) || !file.exists(trace_path(path))) trace_abort("缺少当前运行的映射规则。")
  policies <- yaml::read_yaml(trace_path(path))
  required <- c(
    "policy_version", "scenario", "identifiers", "date_time_formats",
    "upstream_outputs", "sequence_rules", "baseline_rules",
    "unit_standardization", "dataset_output_contract"
  )
  missing <- setdiff(required, names(policies))
  if (length(missing)) trace_abort(sprintf("映射规则缺少字段：%s。", paste(missing, collapse = "、")))
  policies
}

load_approved_mapping <- function(config = load_project_config()) {
  path <- trace_path(config$paths$approved_specification)
  if (!file.exists(path)) trace_abort("尚未生成人工批准的映射规格，已阻止构建。")
  specification <- yaml::read_yaml(path)
  if (!identical(as.character(specification$schema_version), "0.6")) {
    trace_abort("当前版本只接受 schema_version 0.6 的批准规格。")
  }
  if (!identical(specification$specification$status, "approved")) {
    trace_abort("映射规格状态不是 approved，已阻止构建。")
  }
  specification
}

resolve_config_path <- function(relative_path) trace_path(relative_path)

ensure_output_directories <- function(config = load_project_config()) {
  keys <- c(
    "profile_dir", "task_dir", "recommendation_dir", "review_dir", "csv_dir", "xpt_dir",
    "lineage_dir", "local_validation_dir", "p21_validation_dir", "report_dir",
    "manifest_dir", "log_dir"
  )
  invisible(lapply(keys, function(key) {
    path <- config$paths[[key]] %||% ""
    if (nzchar(as.character(path))) ensure_dir(trace_path(path)) else NULL
  }))
}
