if (!exists("trace_root", mode = "function")) {
  configured_root <- Sys.getenv("TRACE_SDTM_ROOT", unset = "")
  root_candidates <- unique(c(
    configured_root,
    getwd(),
    file.path(getwd(), ".."),
    file.path(getwd(), "..", "..")
  ))
  root_candidates <- root_candidates[nzchar(root_candidates)]
  root_candidates <- root_candidates[
    vapply(
      root_candidates,
      function(path) file.exists(file.path(path, "DESCRIPTION")) && dir.exists(file.path(path, "R")),
      logical(1)
    )
  ]
  if (length(root_candidates) == 0L) {
    stop("Unable to locate the TraceSDTM project root for tests.", call. = FALSE)
  }
  root <- normalizePath(root_candidates[[1]], winslash = "/", mustWork = TRUE)
  Sys.setenv(TRACE_SDTM_ROOT = root)
  files <- c(
    "utils.R", "config.R", "studio_projects.R", "registry.R", "profile.R", "recommend.R", "task_discovery.R", "manual_parameters.R", "review.R", "review_v04.R", "studio_review.R", "ai_review.R",
    "transforms.R", "build.R", "validate_local.R", "p21.R", "evaluate.R", "evaluate_v04.R",
    "report.R", "studio_exports.R", "studio_jobs.R", "studio_app.R", "studio_app_v06.R", "pipeline.R"
  )
  for (file in files) source(file.path(root, "R", file), encoding = "UTF-8")
}

load_v03_config_for_tests <- function(scenario = "advanced") {
  config <- load_project_config(scenario)
  historical <- list(
    basic = list(
      specification_template = "specs/benchmark/v1/basic_concepts.yml",
      gold_specification = "specs/benchmark/v1/basic_gold.yml",
      mapping_policies = "specs/benchmark/v1/basic_policies.yml",
      output_base = "output/benchmark/v1/basic"
    ),
    intermediate = list(
      specification_template = "specs/benchmark/v1/intermediate_concepts.yml",
      gold_specification = "specs/benchmark/v1/intermediate_gold.yml",
      mapping_policies = "specs/benchmark/v1/intermediate_policies.yml",
      output_base = "output/benchmark/v1/intermediate"
    ),
    advanced = list(
      specification_template = "specs/v0.2/advanced_concepts.yml",
      gold_specification = "specs/v0.2/advanced_gold.yml",
      mapping_policies = "specs/benchmark/v1/advanced_policies.yml",
      output_base = "output/benchmark/v1/advanced"
    )
  )[[scenario]]
  config$paths$specification_template <- historical$specification_template
  config$paths$gold_specification <- historical$gold_specification
  config$paths$mapping_policies <- historical$mapping_policies
  generated <- c(
    profile_dir = "profile", recommendation_dir = "recommendations", review_dir = "review",
    csv_dir = file.path("sdtm", "csv"), xpt_dir = file.path("sdtm", "xpt"),
    lineage_dir = "lineage", local_validation_dir = file.path("validation", "local"),
    p21_validation_dir = file.path("validation", "p21"), report_dir = "report",
    manifest_dir = "manifests", log_dir = "logs"
  )
  config$paths$output_base <- historical$output_base
  for (key in names(generated)) config$paths[[key]] <- file.path(historical$output_base, generated[[key]])
  config$paths$approved_specification <- file.path(historical$output_base, "specs", "approved_mapping.yml")
  config
}
