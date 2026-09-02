scenario_from_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  position <- match("--scenario", args)
  if (!is.na(position)) {
    if (position == length(args)) trace_abort("--scenario 后必须提供场景名称。")
    return(as.character(args[[position + 1L]]))
  }
  Sys.getenv("TRACE_SDTM_SCENARIO", unset = "basic")
}

load_project_config <- function(scenario = Sys.getenv("TRACE_SDTM_SCENARIO", unset = "basic")) {
  config <- yaml::read_yaml(trace_path("config", "project.yml"))
  if (!scenario %in% names(config$scenarios)) {
    trace_abort(sprintf("未知场景：%s。允许值为 %s。", scenario, paste(names(config$scenarios), collapse = "、")))
  }
  scenario_config <- config$scenarios[[scenario]]
  config$project$scenario <- scenario
  config$paths <- c(config$paths, scenario_config)
  output_base <- scenario_config$output_base
  generated_paths <- c(
    profile_dir = "profile", recommendation_dir = "recommendations",
    review_dir = "review", csv_dir = file.path("sdtm", "csv"),
    xpt_dir = file.path("sdtm", "xpt"), lineage_dir = "lineage",
    local_validation_dir = file.path("validation", "local"),
    p21_validation_dir = file.path("validation", "p21"), report_dir = "report",
    manifest_dir = "manifests", log_dir = "logs"
  )
  for (key in names(generated_paths)) config$paths[[key]] <- file.path(output_base, generated_paths[[key]])
  config$paths$approved_specification <- file.path(output_base, "specs", "approved_mapping.yml")
  experiment_id <- Sys.getenv("TRACE_SDTM_EXPERIMENT_ID", unset = "")
  if (nzchar(experiment_id)) config <- apply_experiment_paths(config, experiment_id)
  config
}

apply_experiment_paths <- function(config, experiment_id) {
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", experiment_id)) {
    trace_abort("TRACE_SDTM_EXPERIMENT_ID 只能包含字母、数字、点、下划线和连字符。")
  }
  base <- file.path(config$paths$output_base, "experiments", experiment_id)
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

load_gold_specification <- function(config = load_project_config()) {
  yaml::read_yaml(trace_path(config$paths$gold_specification))
}

load_mapping_policies <- function(config = load_project_config()) {
  path <- config$paths$mapping_policies %||% ""
  if (!nzchar(path) || !file.exists(trace_path(path))) {
    trace_abort(sprintf("%s 场景缺少映射政策文件。", config$project$scenario))
  }
  policies <- yaml::read_yaml(trace_path(path))
  required <- c(
    "policy_version", "scenario", "identifiers", "date_time_formats",
    "upstream_outputs", "sequence_rules", "baseline_rules",
    "unit_standardization", "dataset_output_contract"
  )
  missing <- setdiff(required, names(policies))
  if (length(missing)) trace_abort(sprintf("映射政策缺少字段：%s。", paste(missing, collapse = "、")))
  if (!identical(as.character(policies$scenario), config$project$scenario)) {
    trace_abort(sprintf("映射政策场景 %s 与当前场景 %s 不一致。", policies$scenario, config$project$scenario))
  }
  policies
}

load_approved_mapping <- function(config = load_project_config()) {
  path <- trace_path(config$paths$approved_specification)
  if (!file.exists(path)) {
    trace_abort("尚未生成已审核映射规格。请先执行 recommend --seed（或真实 recommend），完成人工审核后执行 approve。")
  }
  specification <- yaml::read_yaml(path)
  if (!identical(as.character(specification$schema_version), "0.2")) {
    trace_abort("只支持 schema_version 0.2 的批准规格。旧版成果请通过 Git 标签查看。")
  }
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
