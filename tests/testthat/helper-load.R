if (!exists("trace_root", mode = "function")) {
  configured_root <- Sys.getenv("TRACE_SDTM_ROOT", unset = "")
  root_candidates <- unique(c(
    configured_root, getwd(), file.path(getwd(), ".."), file.path(getwd(), "..", "..")
  ))
  root_candidates <- root_candidates[nzchar(root_candidates)]
  root_candidates <- root_candidates[vapply(
    root_candidates,
    function(path) file.exists(file.path(path, "DESCRIPTION")) && dir.exists(file.path(path, "R")),
    logical(1)
  )]
  if (!length(root_candidates)) stop("Unable to locate the TraceSDTM project root for tests.", call. = FALSE)
  root <- normalizePath(root_candidates[[1]], winslash = "/", mustWork = TRUE)
  Sys.setenv(TRACE_SDTM_ROOT = root)
  project_library <- file.path(root, ".Rlib")
  if (dir.exists(project_library)) .libPaths(c(project_library, .libPaths()))
  files <- c(
    "utils.R", "config.R", "analysis_plan.R", "studio_projects.R", "registry.R", "profile.R",
    "model_gateway.R", "parameter_resolvers.R", "mapping_stages.R",
    "task_discovery.R", "manual_parameters.R", "mapping_review.R",
    "studio_review.R", "ai_review.R", "transforms.R", "demo_mapping.R", "build.R",
    "validate_local.R", "adam_build.R", "adam_validate.R", "tlf.R",
    "p21.R", "report.R", "studio_exports.R",
    "studio_jobs.R", "studio_ui_helpers.R", "studio_app.R", "pipeline.R"
  )
  for (file in files) source(file.path(root, "R", file), encoding = "UTF-8")
}
