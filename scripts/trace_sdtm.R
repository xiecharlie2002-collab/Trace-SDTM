#!/usr/bin/env Rscript

invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", "Chinese (Simplified)_China.utf8")))
invisible(suppressWarnings(Sys.setlocale("LC_COLLATE", "Chinese (Simplified)_China.utf8")))

arguments <- commandArgs(trailingOnly = FALSE)
file_argument <- sub("^--file=", "", arguments[grepl("^--file=", arguments)])
if (!length(file_argument)) stop("无法确定 trace_sdtm.R 的位置。", call. = FALSE)
project_root <- normalizePath(file.path(dirname(file_argument[[1]]), ".."), winslash = "/", mustWork = TRUE)
Sys.setenv(TRACE_SDTM_ROOT = project_root)
project_library <- file.path(project_root, ".Rlib")
renv_active <- nzchar(Sys.getenv("RENV_PROJECT", unset = "")) || any(grepl("/renv/library/", normalizePath(.libPaths(), winslash = "/", mustWork = FALSE), fixed = TRUE))
if (!renv_active && dir.exists(project_library)) .libPaths(c(project_library, .libPaths()))

source_order <- c(
  "utils.R",
  "config.R",
  "studio_projects.R",
  "registry.R",
  "profile.R",
  "recommend.R",
  "review.R",
  "review_v04.R",
  "studio_review.R",
  "transforms.R",
  "build.R",
  "validate_local.R",
  "p21.R",
  "evaluate.R",
  "evaluate_v04.R",
  "report.R",
  "studio_exports.R",
  "studio_jobs.R",
  "studio_app.R",
  "pipeline.R"
)
for (file in source_order) source(file.path(project_root, "R", file), encoding = "UTF-8")

tryCatch(
  trace_main(commandArgs(trailingOnly = TRUE)),
  trace_sdtm_error = function(error) {
    message("错误：", conditionMessage(error))
    quit(save = "no", status = error$status %||% 1L)
  },
  error = function(error) {
    message("未预期错误：", conditionMessage(error))
    quit(save = "no", status = 1L)
  }
)
