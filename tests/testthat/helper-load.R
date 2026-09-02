if (!exists("trace_root", mode = "function")) {
  root <- normalizePath(file.path(getwd()), winslash = "/", mustWork = TRUE)
  Sys.setenv(TRACE_SDTM_ROOT = root)
  files <- c(
    "utils.R", "config.R", "registry.R", "profile.R", "recommend.R", "review.R",
    "transforms.R", "build.R", "validate_local.R", "p21.R", "evaluate.R",
    "report.R", "pipeline.R"
  )
  for (file in files) source(file.path(root, "R", file), encoding = "UTF-8")
}
