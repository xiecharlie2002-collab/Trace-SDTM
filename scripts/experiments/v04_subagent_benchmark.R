#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = FALSE)
file_argument <- sub("^--file=", "", arguments[grepl("^--file=", arguments)])
if (!length(file_argument)) stop("无法确定实验脚本位置。", call. = FALSE)
project_root <- normalizePath(file.path(dirname(file_argument[[1]]), "..", ".."), winslash = "/", mustWork = TRUE)
Sys.setenv(TRACE_SDTM_ROOT = project_root)
project_library <- file.path(project_root, ".Rlib")
if (dir.exists(project_library)) .libPaths(c(project_library, .libPaths()))
for (file in c(
  "utils.R", "config.R", "registry.R", "profile.R", "recommend.R", "review.R",
  "review_v04.R", "transforms.R", "build.R", "validate_local.R", "p21.R",
  "evaluate.R", "evaluate_v04.R", "experiment_v04.R"
)) source(file.path(project_root, "R", file), encoding = "UTF-8")

args <- commandArgs(trailingOnly = TRUE)
action <- args[[1]] %||% "help"

argument_value <- function(name, required = TRUE) {
  position <- match(name, args)
  if (is.na(position) || position == length(args)) {
    if (required) trace_abort(sprintf("缺少参数 %s。", name))
    return("")
  }
  as.character(args[[position + 1L]])
}

common <- function() list(
  scenario = argument_value("--scenario"),
  domain = argument_value("--domain"),
  experiment_id = argument_value("--experiment-id")
)

run_action <- function() {
  values <- if (identical(action, "finalize")) list(
    scenario = argument_value("--scenario"),
    experiment_id = argument_value("--experiment-id")
  ) else common()
  if (identical(action, "prepare-targets")) {
    do.call(v04_prepare_target_request, values)
  } else if (identical(action, "import-targets")) {
    do.call(v04_import_targets, c(values, list(
      response_file = argument_value("--response-file"),
      subagent_task = argument_value("--task-id")
    )))
  } else if (identical(action, "prepare-functions")) {
    do.call(v04_prepare_function_request, c(values, list(conditional = FALSE)))
  } else if (identical(action, "import-functions")) {
    do.call(v04_import_functions, c(values, list(
      response_file = argument_value("--response-file"),
      subagent_task = argument_value("--task-id"), conditional = FALSE
    )))
  } else if (identical(action, "prepare-conditional-functions")) {
    do.call(v04_prepare_function_request, c(values, list(conditional = TRUE)))
  } else if (identical(action, "import-conditional-functions")) {
    do.call(v04_import_functions, c(values, list(
      response_file = argument_value("--response-file"),
      subagent_task = argument_value("--task-id"), conditional = TRUE
    )))
  } else if (identical(action, "prepare-parameters")) {
    do.call(v04_prepare_parameter_request, c(values, list(conditional = FALSE)))
  } else if (identical(action, "import-parameters")) {
    do.call(v04_import_parameters, c(values, list(
      response_file = argument_value("--response-file"),
      subagent_task = argument_value("--task-id"), conditional = FALSE
    )))
  } else if (identical(action, "prepare-conditional-parameters")) {
    do.call(v04_prepare_parameter_request, c(values, list(conditional = TRUE)))
  } else if (identical(action, "import-conditional-parameters")) {
    do.call(v04_import_parameters, c(values, list(
      response_file = argument_value("--response-file"),
      subagent_task = argument_value("--task-id"), conditional = TRUE
    )))
  } else if (identical(action, "finalize")) {
    do.call(v04_finalize_scenario, values)
  } else {
    cat(paste(
      "Actions:",
      "  prepare-targets | import-targets",
      "  prepare-functions | import-functions",
      "  prepare-conditional-functions | import-conditional-functions",
      "  prepare-parameters | import-parameters",
      "  prepare-conditional-parameters | import-conditional-parameters",
      "  finalize",
      sep = "\n"
    ), "\n")
  }
}

tryCatch(
  run_action(),
  trace_sdtm_error = function(error) {
    message("错误：", conditionMessage(error))
    quit(save = "no", status = error$status %||% 1L)
  },
  error = function(error) {
    message("未预期错误：", conditionMessage(error))
    quit(save = "no", status = 1L)
  }
)
