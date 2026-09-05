# TraceSDTM 0.6 structured human parameter completion ----------------------

v06_parameter_results_path <- function(config) file.path(trace_path(config$paths$recommendation_dir), "parameter_completions.json")

studio_manual_parameter_options_v06 <- function(config) {
  path <- v06_parameter_results_path(config)
  if (!file.exists(path)) return(list())
  result <- jsonlite::read_json(path, simplifyVector = FALSE)
  resolutions <- result$resolutions$valid %||% list()
  registry <- load_transform_registry(config)
  options <- list()
  for (key in names(resolutions)) {
    resolution <- resolutions[[key]]
    missing <- unlist(resolution$unavailable_parameters %||% character(), use.names = FALSE)
    if (!length(missing)) next
    entry <- registry_entry(resolution$transform_id, registry)
    properties <- entry$parameter_schema$properties[intersect(missing, names(entry$parameter_schema$properties %||% list()))]
    options[[key]] <- list(
      key = key, task_id = resolution$task_id, candidate_rank = resolution$candidate_rank,
      transform_id = resolution$transform_id, parameter_names = as.list(missing),
      schema = list(type = "object", additionalProperties = FALSE, required = as.list(missing), properties = properties),
      injected_parameters = resolution$injected_parameters %||% list()
    )
  }
  options
}

studio_save_manual_parameters_v06 <- function(config, key, values, reviewer) {
  studio_assert_mapping_editable_v06(config)
  reviewer <- trimws(as.character(reviewer %||% ""))
  if (!nzchar(reviewer)) trace_abort("人工填写参数时必须提供审核者标识。")
  path <- v06_parameter_results_path(config)
  if (!file.exists(path)) trace_abort("尚未执行参数解析。")
  result <- jsonlite::read_json(path, simplifyVector = FALSE)
  resolution <- result$resolutions$valid[[key]]
  if (is.null(resolution)) trace_abort(sprintf("未知参数候选：%s。", key))
  missing <- unlist(resolution$unavailable_parameters %||% character(), use.names = FALSE)
  values <- named_list(values)
  if (!setequal(names(values), missing)) trace_abort("人工填写值必须完整覆盖且只能覆盖标记为信息不足的参数。")
  existing <- result$valid[[key]] %||% list(
    parameters = resolution$injected_parameters %||% list(),
    parameter_sources = resolution$parameter_sources %||% list()
  )
  overwritten <- intersect(names(values), names(existing$parameters %||% list()))
  if (length(overwritten)) trace_abort(sprintf("不能覆盖已有参数：%s。", paste(overwritten, collapse = "、")))
  parameters <- existing$parameters %||% list()
  parameters[names(values)] <- values
  sources <- existing$parameter_sources %||% list()
  for (name in names(values)) sources[[name]] <- list(source = "reviewer", reference = reviewer)
  entry <- registry_entry(resolution$transform_id, load_transform_registry(config))
  errors <- json_schema_errors(parameters, entry$parameter_schema)
  if (length(errors)) trace_abort(paste("人工填写后参数仍不符合模式：", paste(errors, collapse = "；")))
  result$valid[[key]] <- list(
    task_id = resolution$task_id, candidate_rank = resolution$candidate_rank,
    status = "proposed", uncertainties = list(), parameters = parameters,
    parameter_sources = sources, completed_by = reviewer, completed_at = utc_now()
  )
  resolution$status <- "ready"
  resolution$unavailable_parameters <- list()
  result$resolutions$valid[[key]] <- resolution
  result$failures <- Filter(function(item) {
    !(identical(as.character(item$task_id %||% ""), as.character(resolution$task_id)) &&
        identical(as.integer(item$candidate_rank %||% NA_integer_), as.integer(resolution$candidate_rank)))
  }, result$failures %||% list())
  write_json(result, path)
  resolutions_path <- file.path(trace_path(config$paths$recommendation_dir), "parameter_resolutions.json")
  if (file.exists(resolutions_path)) {
    resolutions <- jsonlite::read_json(resolutions_path, simplifyVector = FALSE)
    resolutions$valid[[key]] <- resolution
    write_json(resolutions, resolutions_path)
  }
  if (!is.null(config$studio$project_id)) studio_append_audit(
    config$studio$project_id, "manual_parameters_completed",
    list(task_id = resolution$task_id, candidate_rank = resolution$candidate_rank, parameter_names = as.list(names(values))),
    config$studio$run_id, reviewer
  )
  remaining <- studio_manual_parameter_options_v06(config)
  if (!is.null(config$studio$project_id) && !length(remaining) && !length(result$failures %||% list())) {
    studio_update_run_stage(config$studio$project_id, config$studio$run_id, "recommend_parameters", "completed", actor = reviewer)
  }
  invisible(result$valid[[key]])
}
