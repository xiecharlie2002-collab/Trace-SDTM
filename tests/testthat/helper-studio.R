studio_test_home <- function() {
  path <- file.path(tempdir(), paste0("trace-studio-", substr(digest::digest(paste(Sys.time(), runif(1)), serialize = FALSE), 1L, 10L)))
  ensure_dir(path)
}
studio_test_project_id <- function(prefix = "test") {
  paste0(prefix, "-", substr(digest::digest(paste(Sys.time(), runif(1)), serialize = FALSE), 1L, 10L))
}

studio_with_test_home <- function(code) {
  old <- Sys.getenv("TRACE_SDTM_STUDIO_HOME", unset = NA_character_)
  Sys.setenv(TRACE_SDTM_STUDIO_HOME = studio_test_home())
  on.exit(if (is.na(old)) Sys.unsetenv("TRACE_SDTM_STUDIO_HOME") else Sys.setenv(TRACE_SDTM_STUDIO_HOME = old), add = TRUE)
  force(code)
}

studio_prepare_project <- function(template = "basic", project_id = studio_test_project_id(template), study_id = NULL) {
  if (is.null(study_id)) {
    study_id <- c(basic = "TRACEB01", intermediate = "TRACE-MID", advanced = "TRACE002")[[template]]
  }
  studio_create_project(project_id, paste("Test", template), template, study_id)
  config <- load_project_config(template)
  sources <- studio_non_derived_sources(project_id)
  for (dataset in names(sources)) {
    source_path <- trace_path(config$paths$raw_dir, sources[[dataset]]$file)
    suggestions <- studio_import_source(project_id, dataset, source_path, basename(source_path), actor = "test")
    mapping <- stats::setNames(as.character(suggestions$suggested_source), suggestions$logical_field)
    studio_confirm_bindings(project_id, dataset, mapping, actor = "test")
  }
  project_id
}

studio_mock_assembled_from_gold <- function(config, template) {
  gold <- yaml::read_yaml(trace_path("specs", "benchmark", "v2", paste0(template, "_gold.yml")))$plans
  specification <- load_mapping_template(config)
  registry <- load_transform_registry(config)
  tasks <- stats::setNames(specification$tasks, vapply(specification$tasks, task_id_v04, character(1)))
  plans <- purrr::imap(gold, function(steps, id) {
    step <- steps[[1L]]
    step$step_id <- paste0(id, "_STEP_01")
    step$parameter_sources <- studio_parameter_provenance(step$parameters %||% list(), "reviewer", "fixed_test_response")
    entry <- registry_entry(step$transform_id, registry)
    output_mode <- entry$target_contract$output_mode
    list(
      task_id = id, assembly_group_id = tasks[[id]]$assembly_group_id,
      target_domain = tasks[[id]]$target_domain, candidate_rank = 1L,
      source_refs = tasks[[id]]$source_refs, depends_on = tasks[[id]]$depends_on,
      required = tasks[[id]]$required,
      semantic_decision = list(
        output_kind = if (output_mode == "dataset") "dataset" else if (output_mode == "none") "none" else "variables",
        target_variables = step$target_variables
      ),
      steps = list(step), recommendation_score = 1,
      reason = "固定测试响应", uncertainties = "", status = "proposed", review_required = TRUE
    )
  })
  list(schema_version = "0.4", stage = "assembly", plans = plans, failures = list())
}
