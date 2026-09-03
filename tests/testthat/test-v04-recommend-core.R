test_that("v0.4 target parsing isolates invalid tasks", {
  specification <- list(tasks = list(
    list(
      task_id = "DM_STUDY", assembly_group_id = "DM_CORE", target_domain = "DM",
      source_refs = list(list(ref_id = "dm__study", dataset = "dm", variable = "STUDY", role = "study")),
      depends_on = list(), required = TRUE, intent = "Map study identifier"
    ),
    list(
      task_id = "DM_DOMAIN", assembly_group_id = "DM_CORE", target_domain = "DM",
      source_refs = list(), depends_on = list(), required = TRUE, intent = "Set domain"
    )
  ))
  group <- recommendation_groups_v04(specification)[[1]]
  metadata <- list(domains = list(DM = list(variables = list(
    STUDYID = list(label = "Study Identifier"), DOMAIN = list(label = "Domain")
  ))))
  response <- list(target_identifications = list(
    list(
      task_id = "DM_STUDY", output_kind = "variables", target_variables = list("STUDYID"),
      score = 0.9, evidence = list("source name"), uncertainties = list(), status = "proposed"
    ),
    list(
      task_id = "DM_DOMAIN", output_kind = "variables", target_variables = list("NOT_A_VARIABLE"),
      score = 0.9, evidence = list(), uncertainties = list(), status = "proposed"
    )
  ))

  parsed <- parse_target_decisions_v04(response, group, metadata)
  expect_named(parsed$valid, "DM_STUDY")
  expect_length(parsed$failures, 1L)
  expect_match(parsed$failures[[1]]$error, "未知目标变量")

  prompt <- target_prompt_v04(group, specification, metadata, list(), dictionary = NULL)
  expect_match(prompt, "只识别")
  expect_match(prompt, "禁止返回 transform_id")
  expect_false(grepl("allowed_functions", prompt, fixed = TRUE))
})

test_that("v0.4 function selection rejects parameters without discarding siblings", {
  specification <- list(tasks = list(
    list(
      task_id = "DM_STUDY", assembly_group_id = "DM_CORE", target_domain = "DM",
      source_refs = list(list(ref_id = "dm__study", dataset = "dm", variable = "STUDY", role = "study")),
      depends_on = list(), required = TRUE
    ),
    list(
      task_id = "DM_DOMAIN", assembly_group_id = "DM_CORE", target_domain = "DM",
      source_refs = list(), depends_on = list(), required = TRUE
    )
  ))
  group <- recommendation_groups_v04(specification)[[1]]
  targets <- list(valid = list(
    DM_STUDY = list(task_id = "DM_STUDY", target_domain = "DM", output_kind = "variables", target_variables = list("STUDYID"), status = "proposed"),
    DM_DOMAIN = list(task_id = "DM_DOMAIN", target_domain = "DM", output_kind = "variables", target_variables = list("DOMAIN"), status = "proposed")
  ))
  registry <- list(transforms = list(
    list(
      transform_id = "assign_no_ct", model_selectable = TRUE,
      source_contract = list(minimum = 1L, maximum = 1L, minimum_datasets = 1L, maximum_datasets = 1L),
      target_contract = list(domains = list("DM"), patterns = list("^[A-Z][A-Z0-9]*$"), output_mode = "single"),
      parameter_schema = list(type = "object", additionalProperties = FALSE, properties = setNames(list(), character())),
      parameter_resolution = list()
    ),
    list(
      transform_id = "hardcode_no_ct", model_selectable = TRUE,
      source_contract = list(minimum = 0L, maximum = 1L, minimum_datasets = 0L, maximum_datasets = 1L),
      target_contract = list(domains = list("DM"), patterns = list("^[A-Z][A-Z0-9]*$"), output_mode = "single"),
      parameter_schema = list(type = "object", additionalProperties = FALSE, required = list("value"), properties = list(value = list(type = "string"))),
      parameter_resolution = list(value = list(resolver_id = "target_constant", source = "policy", allow_model = FALSE, override = FALSE))
    )
  ))
  response <- list(function_candidates = list(
    list(
      task_id = "DM_STUDY", candidate_rank = 1L, transform_id = "assign_no_ct",
      source_ref_ids = list("dm__study"), score = 0.9, reason = "direct",
      uncertainties = list(), status = "proposed", review_required = TRUE
    ),
    list(
      task_id = "DM_DOMAIN", candidate_rank = 1L, transform_id = "hardcode_no_ct",
      source_ref_ids = list(), parameters = list(value = "DM"), score = 0.9,
      reason = "constant", uncertainties = list(), status = "proposed", review_required = TRUE
    )
  ))

  parsed <- parse_function_selections_v04(response, group, targets, registry)
  expect_true("DM_STUDY#1" %in% names(parsed$valid))
  expect_false("DM_DOMAIN#1" %in% names(parsed$valid))
  expect_true(any(grepl("禁止字段", vapply(parsed$failures, `[[`, character(1), "error"))))

  prompt <- function_prompt_v04(group, targets, registry, specification, list())
  expect_match(prompt, "review_required 必须为 true", fixed = TRUE)
})

test_that("known parameters are injected and only finite model parameters are requested", {
  specification <- list(tasks = list(list(
    task_id = "DM_DOMAIN", assembly_group_id = "DM_CORE", target_domain = "DM",
    source_refs = list(), depends_on = list(), required = TRUE
  )))
  targets <- list(valid = list(DM_DOMAIN = list(
    task_id = "DM_DOMAIN", target_domain = "DM", output_kind = "variables",
    target_variables = list("DOMAIN"), status = "proposed"
  )))
  functions <- list(valid = list(`DM_DOMAIN#1` = list(
    task_id = "DM_DOMAIN", candidate_rank = 1L, transform_id = "hardcode_no_ct",
    source_ref_ids = list(), score = 1, reason = "constant", uncertainties = "",
    status = "proposed", review_required = TRUE
  )))
  registry <- list(transforms = list(list(
    transform_id = "hardcode_no_ct", model_selectable = TRUE,
    source_contract = list(minimum = 0L, maximum = 1L, minimum_datasets = 0L, maximum_datasets = 1L),
    target_contract = list(domains = list("DM"), patterns = list("^DOMAIN$"), output_mode = "single"),
    parameter_schema = list(type = "object", additionalProperties = FALSE, required = list("value"), properties = list(value = list(type = "string"))),
    parameter_resolution = list(value = list(resolver_id = "target_constant", source = "policy", allow_model = FALSE, override = FALSE))
  )))
  policies <- list(parameter_bindings = list(DM_DOMAIN = list(value = "DM")))

  resolved <- resolve_known_parameters_v04(
    specification, targets, functions, registry, policies, resources = list()
  )
  item <- resolved$valid[["DM_DOMAIN#1"]]
  expect_identical(item$injected_parameters$value, "DM")
  expect_identical(item$parameter_sources$value$source, "policy")
  expect_true(item$fully_resolved)
  expect_null(parameter_prompt_v04(resolved))
})

test_that("assembly blocks only downstream tasks", {
  specification <- list(tasks = list(
    list(task_id = "A", assembly_group_id = "G", target_domain = "DM", source_refs = list(), depends_on = list(), required = TRUE),
    list(task_id = "B", assembly_group_id = "G", target_domain = "DM", source_refs = list(), depends_on = list("A"), required = TRUE),
    list(task_id = "C", assembly_group_id = "G", target_domain = "DM", source_refs = list(), depends_on = list("B"), required = TRUE),
    list(task_id = "D", assembly_group_id = "G", target_domain = "DM", source_refs = list(), depends_on = list(), required = TRUE)
  ))
  decision <- function(id, target) list(task_id = id, target_domain = "DM", output_kind = "variables", target_variables = list(target), status = "proposed")
  candidate <- function(id) list(task_id = id, candidate_rank = 1L, transform_id = "hardcode_no_ct", source_ref_ids = list(), score = 1, reason = "constant", uncertainties = "", status = "proposed", review_required = TRUE)
  resolution <- function(id, value) list(task_id = id, candidate_rank = 1L, transform_id = "hardcode_no_ct", injected_parameters = list(value = value), parameter_sources = list(value = list(source = "policy", reference = "test")), unresolved_parameters = list(), parameter_options = list(), unavailable_parameters = list(), fully_resolved = TRUE, status = "ready")
  targets <- list(valid = list(A = decision("A", "STUDYID"), C = decision("C", "COUNTRY"), D = decision("D", "DOMAIN")), failures = list())
  functions <- list(valid = list(`A#1` = candidate("A"), `C#1` = candidate("C"), `D#1` = candidate("D")), failures = list())
  resolutions <- list(valid = list(`A#1` = resolution("A", "TRACE"), `C#1` = resolution("C", "US"), `D#1` = resolution("D", "DM")), failures = list())
  registry <- list(transforms = list(list(
    transform_id = "hardcode_no_ct", model_selectable = TRUE,
    parameter_schema = list(type = "object", additionalProperties = FALSE, required = list("value"), properties = list(value = list(type = "string")))
  )))

  assembled <- assemble_candidates_v04(
    specification, targets, functions, resolutions, list(valid = list(), failures = list()), registry
  )
  expect_true(all(c("A#1", "D#1") %in% names(assembled$plans)))
  expect_false("C#1" %in% names(assembled$plans))
  expect_identical(assembled$dependency_blocks$C$blocked_by, list("B"))
})

test_that("seed provider completes all three stages and persists evidence", {
  specification <- list(
    schema_version = "0.4",
    tasks = list(list(
      task_id = "DM_STUDY", assembly_group_id = "DM_CORE", target_domain = "DM",
      source_refs = list(list(ref_id = "dm__study", dataset = "dm", variable = "STUDY", role = "study")),
      depends_on = list(), required = TRUE, intent = "Map study identifier"
    ))
  )
  metadata <- list(domains = list(DM = list(variables = list(STUDYID = list(label = "Study Identifier")))))
  registry <- list(transforms = list(list(
    transform_id = "assign_no_ct", title = "Direct", description = "Direct assignment",
    execution_stage = "field_mapping", model_selectable = TRUE,
    source_contract = list(minimum = 1L, maximum = 1L, minimum_datasets = 1L, maximum_datasets = 1L, types = list("character")),
    target_contract = list(domains = list("DM"), patterns = list("^STUDYID$"), output_mode = "single"),
    parameter_schema = list(type = "object", additionalProperties = FALSE, properties = setNames(list(), character())),
    parameter_resolution = list(), preconditions = list(), not_allowed_when = list("semantic change")
  )))
  gold <- list(plans = list(DM_STUDY = list(list(
    transform_id = "assign_no_ct", source_ref_ids = list("dm__study"),
    target_variables = list("STUDYID"), parameters = list()
  ))))
  output <- withr::local_tempdir()
  config <- list(paths = list(recommendation_dir = output), model = list(endpoint_suffix = "/chat/completions"))

  run <- run_recommendation_v04(
    config = config, provider = "seed", specification = specification,
    metadata = metadata, registry = registry, policies = list(sentinel = TRUE),
    resources = list(sentinel = TRUE),
    gold = gold, dictionary = NULL, recommendation_dir = output
  )
  expect_identical(run$run$status, "completed")
  expect_named(run$assembled$plans, "DM_STUDY#1")
  expect_true(all(file.exists(file.path(output, c(
    "target_decisions.json", "function_candidates.json", "parameter_resolutions.json",
    "parameter_completions.json", "assembled_recommendations.json", "model_run.json"
  )))))
})

test_that("blind execution never retries while production repairs once", {
  calls <- 0L
  request <- function(prompt) {
    calls <<- calls + 1L
    if (calls == 1L) "not-json" else '{"ok":true}'
  }
  parse <- function(value) {
    if (!isTRUE(value$ok)) stop("bad structure")
    value
  }
  blind <- execute_model_stage_v04("prompt", request, parse, blind = TRUE)
  expect_equal(calls, 1L)
  expect_false(is.null(blind$error))

  calls <- 0L
  production <- execute_model_stage_v04("prompt", request, parse, blind = FALSE)
  expect_equal(calls, 2L)
  expect_true(production$repaired)
})
