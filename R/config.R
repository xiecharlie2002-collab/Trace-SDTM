load_project_config <- function() {
  config <- yaml::read_yaml(trace_path("config", "project.yml"))
  experiment_id <- Sys.getenv("TRACE_SDTM_EXPERIMENT_ID", unset = "")
  if (nzchar(experiment_id)) config <- apply_experiment_paths(config, experiment_id)
  config
}

apply_experiment_paths <- function(config, experiment_id) {
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", experiment_id)) {
    trace_abort("TRACE_SDTM_EXPERIMENT_ID 只能包含字母、数字、点、下划线和连字符。")
  }
  base <- file.path("output", "experiments", experiment_id)
  generated_paths <- c(
    profile_dir = "profile",
    recommendation_dir = "recommendations",
    review_dir = "review",
    csv_dir = file.path("sdtm", "csv"),
    xpt_dir = file.path("sdtm", "xpt"),
    lineage_dir = "lineage",
    local_validation_dir = file.path("validation", "local"),
    p21_validation_dir = file.path("validation", "p21"),
    report_dir = "report",
    manifest_dir = "manifests",
    log_dir = "logs"
  )
  for (key in names(generated_paths)) config$paths[[key]] <- file.path(base, generated_paths[[key]])
  config$paths$approved_specification <- file.path(base, "specs", "approved_mapping.yml")
  config$project$experiment_id <- experiment_id
  config
}

load_p21_config <- function() {
  yaml::read_yaml(trace_path("config", "p21.yml"))
}

load_metadata <- function(config = load_project_config()) {
  yaml::read_yaml(trace_path(config$paths$metadata))
}

load_mapping_template <- function(config = load_project_config()) {
  yaml::read_yaml(trace_path(config$paths$specification_template))
}

load_approved_mapping <- function(config = load_project_config()) {
  path <- trace_path(config$paths$approved_specification)
  if (!file.exists(path)) {
    trace_abort("尚未生成已审核映射规格。请先执行 recommend --seed（或真实 recommend），完成人工审核后执行 approve。")
  }
  specification <- yaml::read_yaml(path)
  if (!identical(specification$specification$status, "approved")) {
    trace_abort("映射规格状态不是 approved，已阻止构建。")
  }
  specification
}

resolve_config_path <- function(relative_path) {
  trace_path(relative_path)
}

ensure_output_directories <- function(config = load_project_config()) {
  keys <- c(
    "profile_dir", "recommendation_dir", "review_dir", "csv_dir", "xpt_dir",
    "lineage_dir", "local_validation_dir", "p21_validation_dir", "report_dir",
    "manifest_dir", "log_dir"
  )
  invisible(lapply(keys, function(key) ensure_dir(trace_path(config$paths[[key]]))))
}
