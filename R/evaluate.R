normalize_optional_text <- function(x) {
  value <- as.character(x)
  value[is.na(value)] <- ""
  value
}

# -----------------------------------------------------------------------------
# Schema-driven canonical comparison shared by the 0.2 evaluator and the 0.4
# atomic evaluator.  The canonical representation is deliberately an internal
# R value rather than JSON text: this preserves the distinction between an
# absent object member and an explicitly supplied JSON null.

canonical_null <- function() structure(list(), class = "trace_sdtm_canonical_null")

schema_type_names <- function(schema) {
  unique(as.character(unlist(schema$type %||% character(), recursive = TRUE, use.names = FALSE)))
}

canonical_sort_key <- function(value) {
  paste(as.character(serialize(value, connection = NULL, ascii = TRUE)), collapse = "")
}

canonicalize_by_schema <- function(value, schema = list()) {
  types <- schema_type_names(schema)
  if (is.null(value)) return(canonical_null())

  is_object <- "object" %in% types || (!length(types) && is.list(value) && !is.null(names(value)))
  is_array <- "array" %in% types && !is_object

  if (is_object) {
    if (!is.list(value)) value <- as.list(value)
    if (is.null(names(value))) names(value) <- rep("", length(value))
    properties <- schema$properties %||% list()
    additional <- schema$additionalProperties %||% list()
    ordered_names <- sort(names(value), method = "radix")
    result <- value[ordered_names]
    for (name in ordered_names) {
      property_schema <- properties[[name]]
      if (is.null(property_schema)) {
        property_schema <- if (is.list(additional)) additional else list()
      }
      result[[name]] <- canonicalize_by_schema(value[[name]], property_schema)
    }
    if (!length(result)) return(setNames(list(), character()))
    return(result)
  }

  if (is_array) {
    values <- if (is.list(value) && is.null(names(value))) value else as.list(unname(value))
    canonical <- lapply(values, canonicalize_by_schema, schema = schema$items %||% list())
    if (identical(schema[["x-comparison"]] %||% "ordered", "set")) {
      keys <- vapply(canonical, canonical_sort_key, character(1))
      keep <- !duplicated(keys)
      canonical <- canonical[keep]
      keys <- keys[keep]
      canonical <- canonical[order(keys, method = "radix")]
    }
    return(unname(canonical))
  }

  # For union schemas the concrete R scalar determines the selected branch.
  # Pure array schemas were handled above, so a scalar allowed alongside an
  # array remains a scalar rather than being silently promoted.
  if ("integer" %in% types && !("number" %in% types)) return(suppressWarnings(as.integer(value[[1L]])))
  if ("number" %in% types) return(suppressWarnings(as.numeric(value[[1L]])))
  if ("boolean" %in% types) return(as.logical(value[[1L]]))
  if ("string" %in% types) return(as.character(value[[1L]]))

  if (is.list(value)) {
    if (!is.null(names(value))) {
      ordered_names <- sort(names(value), method = "radix")
      result <- value[ordered_names]
      for (name in ordered_names) result[[name]] <- canonicalize_by_schema(value[[name]], list())
      return(result)
    }
    return(unname(lapply(value, canonicalize_by_schema, schema = list())))
  }
  unname(value)
}

schema_comparison_details <- function(left, right, schema = list(), validate = TRUE) {
  left_errors <- if (isTRUE(validate) && exists("json_schema_errors", mode = "function")) {
    json_schema_errors(left, schema)
  } else character()
  right_errors <- if (isTRUE(validate) && exists("json_schema_errors", mode = "function")) {
    json_schema_errors(right, schema)
  } else character()
  left_valid <- !length(left_errors)
  right_valid <- !length(right_errors)
  equal <- left_valid && right_valid && identical(
    canonicalize_by_schema(left, schema),
    canonicalize_by_schema(right, schema)
  )
  list(
    equal = isTRUE(equal), left_valid = left_valid, right_valid = right_valid,
    left_errors = left_errors, right_errors = right_errors
  )
}

compare_by_schema <- function(left, right, schema = list(), validate = TRUE) {
  schema_comparison_details(left, right, schema, validate)$equal
}

step_parameter_comparisons_v04 <- function(proposed, expected, registry) {
  if (length(proposed) != length(expected)) return(rep(FALSE, max(length(proposed), length(expected))))
  index <- registry_index(registry)
  vapply(seq_along(expected), function(step_index) {
    proposed_step <- proposed[[step_index]]
    expected_step <- expected[[step_index]]
    transform_id <- as.character(expected_step$transform_id %||% "")
    if (!identical(as.character(proposed_step$transform_id %||% ""), transform_id)) return(FALSE)
    entry <- index[[transform_id]]
    if (is.null(entry)) return(FALSE)
    compare_by_schema(
      proposed_step$parameters %||% list(),
      expected_step$parameters %||% list(),
      entry$parameter_schema %||% list(type = "object"),
      validate = TRUE
    )
  }, logical(1))
}

parameters_equal_by_registry_v04 <- function(proposed, expected, registry) {
  comparisons <- step_parameter_comparisons_v04(proposed, expected, registry)
  length(comparisons) == length(expected) && all(comparisons)
}

canonical_step_v04 <- function(step, registry = NULL) {
  transform_id <- as.character(step$transform_id %||% "")
  parameter_schema <- list(type = "object")
  if (!is.null(registry) && nzchar(transform_id)) {
    entry <- registry_index(registry)[[transform_id]]
    if (!is.null(entry)) parameter_schema <- entry$parameter_schema %||% parameter_schema
  }
  source_ids <- step$source_ref_ids %||% step$source_keys %||% character()
  list(
    transform_id = transform_id,
    source_ref_ids = unname(as.character(unlist(source_ids, use.names = FALSE))),
    target_variables = unname(as.character(unlist(step$target_variables %||% character(), use.names = FALSE))),
    parameters = canonicalize_by_schema(step$parameters %||% list(), parameter_schema)
  )
}

canonical_plan_v04 <- function(steps, registry = NULL) {
  lapply(steps %||% list(), canonical_step_v04, registry = registry)
}

compare_plan_components_v04 <- function(proposed, expected, registry) {
  proposed_canonical <- canonical_plan_v04(proposed, registry)
  expected_canonical <- canonical_plan_v04(expected, registry)
  proposed_functions <- vapply(proposed_canonical, `[[`, character(1), "transform_id")
  expected_functions <- vapply(expected_canonical, `[[`, character(1), "transform_id")
  function_correct <- identical(proposed_functions, expected_functions)
  source_correct <- function_correct && identical(
    lapply(proposed_canonical, `[[`, "source_ref_ids"),
    lapply(expected_canonical, `[[`, "source_ref_ids")
  )
  step_target_correct <- function_correct && identical(
    lapply(proposed_canonical, `[[`, "target_variables"),
    lapply(expected_canonical, `[[`, "target_variables")
  )
  parameter_correct <- function_correct && parameters_equal_by_registry_v04(proposed, expected, registry)
  list(
    function_correct = function_correct,
    source_correct = source_correct,
    step_target_correct = step_target_correct,
    parameter_correct = parameter_correct
  )
}

evaluate_recommendations <- function(config = load_project_config()) {
  recommendation_path <- trace_path(config$paths$recommendation_dir, "mapping_recommendations.csv")
  gold_path <- trace_path(config$paths$gold_mapping)
  if (!file.exists(recommendation_path)) {
    summary <- list(status = "not_available", reason = "尚未生成候选映射。")
    write_json(summary, trace_path(config$paths$recommendation_dir, "mapping_evaluation_summary.json"))
    return(invisible(list(detail = tibble::tibble(), summary = summary)))
  }
  recommendations <- readr::read_csv(recommendation_path, show_col_types = FALSE)
  gold <- readr::read_csv(gold_path, show_col_types = FALSE)
  expected_details <- flatten_mapping_tasks(load_mapping_template(config)) |>
    dplyr::filter(include_in_recommendation) |>
    dplyr::select(
      mapping_id,
      mapping_type_gold = mapping_type,
      transform_parameters_gold = transform_parameters
    )
  recommendations$target_value <- normalize_optional_text(recommendations$target_value)
  gold$target_value <- normalize_optional_text(gold$target_value)

  compared <- dplyr::left_join(
    recommendations,
    gold,
    by = "mapping_id",
    suffix = c("_recommended", "_gold")
  ) |>
    dplyr::left_join(expected_details, by = "mapping_id") |>
    dplyr::mutate(
      correct = target_domain_recommended == target_domain_gold &
        target_variable_recommended == target_variable_gold &
        target_value_recommended == target_value_gold &
        transform_id_recommended == transform_id_gold,
      mapping_type_correct = mapping_type == mapping_type_gold,
      transform_parameters_correct = purrr::map2_lgl(
        transform_parameters,
        transform_parameters_gold,
        ~ canonical_json_text(.x) == canonical_json_text(.y)
      ),
      exact_correct = correct & mapping_type_correct & transform_parameters_correct
    )

  mapping_ids <- unique(gold$mapping_id)
  top1 <- dplyr::filter(compared, candidate_rank == 1L)
  covered_ids <- unique(top1$mapping_id[!is.na(top1$target_domain_gold)])
  top3_hit <- compared |>
    dplyr::filter(candidate_rank <= 3L) |>
    dplyr::group_by(mapping_id) |>
    dplyr::summarise(hit = any(correct), exact_hit = any(exact_correct), .groups = "drop") |>
    dplyr::right_join(tibble::tibble(mapping_id = mapping_ids), by = "mapping_id") |>
    dplyr::mutate(
      hit = tidyr::replace_na(hit, FALSE),
      exact_hit = tidyr::replace_na(exact_hit, FALSE)
    )

  model_run_path <- trace_path(config$paths$recommendation_dir, "model_run.json")
  model_run <- if (file.exists(model_run_path)) jsonlite::read_json(model_run_path, simplifyVector = TRUE) else list(status = "unknown")
  review_path <- trace_path(config$paths$review_dir, "mapping_review_audit.csv")
  review <- if (file.exists(review_path)) readr::read_csv(review_path, show_col_types = FALSE) else NULL

  summary <- list(
    status = model_run$status %||% "unknown",
    provenance = model_run$provenance %||% "unknown",
    evaluated_fields = length(mapping_ids),
    covered_fields = length(intersect(mapping_ids, covered_ids)),
    candidate_count = nrow(recommendations),
    top1_correct = sum(top1$correct, na.rm = TRUE),
    top1_denominator = length(mapping_ids),
    top3_hit = sum(top3_hit$hit, na.rm = TRUE),
    top3_denominator = length(mapping_ids),
    exact_top1_correct = sum(top1$exact_correct, na.rm = TRUE),
    exact_top3_hit = sum(top3_hit$exact_hit, na.rm = TRUE),
    score_out_of_unit_interval = sum(
      is.na(recommendations$recommendation_score) |
        recommendations$recommendation_score < 0 |
        recommendations$recommendation_score > 1
    ),
    accepted = if (is.null(review)) NA_integer_ else sum(review$decision == "accept"),
    modified = if (is.null(review)) NA_integer_ else sum(review$decision == "modify"),
    rejected = if (is.null(review)) NA_integer_ else sum(review$decision == "reject"),
    needs_information = if (is.null(review)) NA_integer_ else sum(review$decision == "needs_information"),
    disclaimer = if (identical(model_run$provenance, "reference_seed")) "参考种子不是实际模型结果，指标仅验证评价程序。" else "指标来自实际模型运行。"
  )
  write_csv(compared, trace_path(config$paths$recommendation_dir, "mapping_evaluation.csv"))
  write_csv(top1, trace_path(config$paths$recommendation_dir, "mapping_evaluation_top1.csv"))
  grouped <- top1 |>
    dplyr::group_by(target_domain_gold, difficulty) |>
    dplyr::summarise(correct = sum(correct, na.rm = TRUE), total = dplyr::n(), .groups = "drop")
  write_csv(grouped, trace_path(config$paths$recommendation_dir, "mapping_evaluation_by_group.csv"))
  by_domain <- top1 |>
    dplyr::group_by(target_domain_gold) |>
    dplyr::summarise(
      correct = sum(correct, na.rm = TRUE),
      exact_correct = sum(exact_correct, na.rm = TRUE),
      total = dplyr::n(),
      .groups = "drop"
    )
  write_csv(by_domain, trace_path(config$paths$recommendation_dir, "mapping_evaluation_by_domain.csv"))
  by_difficulty <- top1 |>
    dplyr::group_by(difficulty) |>
    dplyr::summarise(
      correct = sum(correct, na.rm = TRUE),
      exact_correct = sum(exact_correct, na.rm = TRUE),
      total = dplyr::n(),
      .groups = "drop"
    )
  write_csv(by_difficulty, trace_path(config$paths$recommendation_dir, "mapping_evaluation_by_difficulty.csv"))

  error_components <- tibble::tibble(
    component = c(
      "target_domain", "target_variable", "target_value", "mapping_type",
      "transform_id", "transform_parameters"
    ),
    mismatch_count = c(
      sum(top1$target_domain_recommended != top1$target_domain_gold, na.rm = TRUE),
      sum(top1$target_variable_recommended != top1$target_variable_gold, na.rm = TRUE),
      sum(top1$target_value_recommended != top1$target_value_gold, na.rm = TRUE),
      sum(!top1$mapping_type_correct, na.rm = TRUE),
      sum(top1$transform_id_recommended != top1$transform_id_gold, na.rm = TRUE),
      sum(!top1$transform_parameters_correct, na.rm = TRUE)
    ),
    denominator = nrow(top1)
  ) |>
    dplyr::arrange(dplyr::desc(mismatch_count), component)
  write_csv(error_components, trace_path(config$paths$recommendation_dir, "mapping_error_components.csv"))
  write_json(summary, trace_path(config$paths$recommendation_dir, "mapping_evaluation_summary.json"))
  invisible(list(
    detail = compared,
    grouped = grouped,
    by_domain = by_domain,
    by_difficulty = by_difficulty,
    error_components = error_components,
    summary = summary
  ))
}

# -----------------------------------------------------------------------------
# 0.2：分别评价类别、函数、目标变量集合、参数和完整函数链。

plan_signature_v02 <- function(steps, registry) {
  index <- registry_index(registry)
  list(
    categories = unique(vapply(steps, function(step) index[[step$transform_id]]$category, character(1))),
    functions = vapply(steps, `[[`, character(1), "transform_id"),
    sources = lapply(steps, function(step) unname(unlist(step$source_keys %||% character(), use.names = FALSE))),
    targets = unique(unlist(lapply(steps, step_target_variables), use.names = FALSE)),
    parameters = lapply(steps, function(step) step$parameters %||% list()),
    exact = canonical_steps_v02(steps)
  )
}

concept_complexity_v02 <- function(steps, registry) {
  categories <- plan_signature_v02(steps, registry)$categories
  advanced <- c("source_integration", "record_transposition", "unit_conversion")
  if (any(categories %in% advanced) || length(steps) >= 4L) return("complex")
  if (length(steps) >= 2L || any(categories %in% c("date_time_conversion", "temporal_derivation"))) return("moderate")
  "simple"
}

step_sequence_edit_distance <- function(left, right) {
  left <- as.character(left)
  right <- as.character(right)
  distances <- matrix(0L, nrow = length(left) + 1L, ncol = length(right) + 1L)
  distances[, 1L] <- seq.int(0L, length(left))
  distances[1L, ] <- seq.int(0L, length(right))
  if (!length(left) || !length(right)) return(distances[nrow(distances), ncol(distances)])
  for (i in seq_along(left)) {
    for (j in seq_along(right)) {
      distances[i + 1L, j + 1L] <- min(
        distances[i, j + 1L] + 1L,
        distances[i + 1L, j] + 1L,
        distances[i, j] + as.integer(left[[i]] != right[[j]])
      )
    }
  }
  distances[nrow(distances), ncol(distances)]
}

step_difference_count <- function(proposed, expected) {
  common <- min(length(proposed), length(expected))
  differences <- abs(length(proposed) - length(expected))
  if (common) {
    differences <- differences + sum(vapply(seq_len(common), function(index) {
      !identical(canonical_steps_v02(list(proposed[[index]])), canonical_steps_v02(list(expected[[index]])))
    }, logical(1)))
  }
  as.integer(differences)
}

modification_grade_v03 <- function(status, proposed, expected, target_correct, complete_correct) {
  if (isTRUE(complete_correct)) return("none")
  if (!identical(status, "proposed") || !length(proposed) || !isTRUE(target_correct)) return("major")
  proposed_functions <- vapply(proposed, `[[`, character(1), "transform_id")
  expected_functions <- vapply(expected, `[[`, character(1), "transform_id")
  if (identical(proposed_functions, expected_functions) && step_difference_count(proposed, expected) <= 1L) return("minor")
  if (step_sequence_edit_distance(proposed_functions, expected_functions) <= 1L) return("moderate")
  "major"
}

candidate_evaluation_rows_v02 <- function(candidates, specification, gold, registry) {
  concepts <- concept_lookup(specification)
  purrr::pmap_dfr(candidates, function(concept_id, candidate_rank, target_domain, categories,
                                      plan_json, recommendation_score, reason, uncertainties,
                                      status, review_required, provenance, group_id, ...) {
    concept <- concepts[[concept_id]]
    proposed <- from_json_text(plan_json)$steps %||% list()
    expected <- gold_plan_steps(gold$plans[[concept_id]])
    proposed_signature <- plan_signature_v02(proposed, registry)
    gold_signature <- plan_signature_v02(expected, registry)
    missing_targets <- setdiff(gold_signature$targets, proposed_signature$targets)
    extra_targets <- setdiff(proposed_signature$targets, gold_signature$targets)
    target_correct <- length(missing_targets) == 0L && length(extra_targets) == 0L
    function_correct <- identical(proposed_signature$functions, gold_signature$functions)
    source_correct <- identical(proposed_signature$sources, gold_signature$sources)
    parameter_correct <- function_correct && parameters_equal_by_registry_v04(
      proposed, expected, registry
    )
    tibble::tibble(
      concept_id = concept_id,
      candidate_rank = as.integer(candidate_rank),
      target_domain = target_domain,
      form_name = concept$form_name,
      complexity = as.character(concept$difficulty %||% concept_complexity_v02(expected, registry)),
      gold_categories = paste(gold_signature$categories, collapse = " | "),
      proposed_categories = paste(proposed_signature$categories, collapse = " | "),
      category_chain_correct = identical(proposed_signature$categories, gold_signature$categories),
      function_chain_correct = function_correct,
      source_chain_correct = source_correct,
      target_set_complete = target_correct,
      missing_targets = paste(missing_targets, collapse = " | "),
      extra_targets = paste(extra_targets, collapse = " | "),
      parameter_values_correct = parameter_correct,
      complete_plan_correct = all(c(function_correct, source_correct, target_correct, parameter_correct)),
      modification_grade = modification_grade_v03(
        status, proposed, expected, target_correct,
        all(c(function_correct, source_correct, target_correct, parameter_correct))
      ),
      strictly_validated = TRUE,
      parameter_schema_valid = TRUE,
      recommendation_score = as.numeric(recommendation_score),
      reason = as.character(reason),
      uncertainties = as.character(uncertainties),
      status = status,
      provenance = provenance,
      group_id = group_id
    )
  })
}

evaluate_recommendations <- function(config = load_project_config()) {
  candidate_path <- trace_path(config$paths$recommendation_dir, "candidate_plans.csv")
  classification_path <- trace_path(config$paths$recommendation_dir, "category_classifications.csv")
  if (!file.exists(candidate_path) || !file.exists(classification_path)) {
    summary <- list(status = "not_available", reason = "尚未生成 0.2 候选方案。")
    write_json(summary, trace_path(config$paths$recommendation_dir, "mapping_evaluation_summary.json"))
    return(invisible(list(detail = tibble::tibble(), summary = summary)))
  }
  candidates <- readr::read_csv(candidate_path, show_col_types = FALSE)
  classifications <- readr::read_csv(classification_path, show_col_types = FALSE)
  specification <- load_mapping_template(config)
  gold <- load_gold_specification(config)
  registry <- load_transform_registry(config)
  detail <- candidate_evaluation_rows_v02(candidates, specification, gold, registry)
  concept_ids <- vapply(specification$concepts, `[[`, character(1), "concept_id")
  concepts <- concept_lookup(specification)
  top1_available <- dplyr::filter(detail, candidate_rank == 1L)
  missing_ids <- setdiff(concept_ids, top1_available$concept_id)
  missing_rows <- purrr::map_dfr(missing_ids, function(concept_id) {
    concept <- concepts[[concept_id]]
    expected <- gold_plan_steps(gold$plans[[concept_id]])
    signature <- plan_signature_v02(expected, registry)
    tibble::tibble(
      concept_id = concept_id, candidate_rank = 1L, target_domain = concept$target_domain,
      form_name = concept$form_name,
      complexity = as.character(concept$difficulty %||% concept_complexity_v02(expected, registry)),
      gold_categories = paste(signature$categories, collapse = " | "), proposed_categories = "",
      category_chain_correct = FALSE, function_chain_correct = FALSE, source_chain_correct = FALSE,
      target_set_complete = FALSE, missing_targets = paste(signature$targets, collapse = " | "),
      extra_targets = "", parameter_values_correct = FALSE, complete_plan_correct = FALSE,
      modification_grade = "major", strictly_validated = FALSE,
      parameter_schema_valid = FALSE, recommendation_score = NA_real_,
      reason = "", uncertainties = "",
      status = "group_rejected_or_missing", provenance = "", group_id = ""
    )
  })
  top1 <- dplyr::bind_rows(top1_available, missing_rows) |>
    dplyr::mutate(concept_id = factor(concept_id, levels = concept_ids)) |>
    dplyr::arrange(concept_id) |>
    dplyr::mutate(concept_id = as.character(concept_id))
  top3 <- detail |>
    dplyr::filter(candidate_rank <= 3L) |>
    dplyr::group_by(concept_id) |>
    dplyr::summarise(
      category_top3_hit = any(category_chain_correct),
      function_top3_hit = any(function_chain_correct),
      complete_plan_top3_hit = any(complete_plan_correct),
      .groups = "drop"
    ) |>
    dplyr::right_join(tibble::tibble(concept_id = concept_ids), by = "concept_id") |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("_hit"), ~ tidyr::replace_na(.x, FALSE)))

  gold_categories <- purrr::map_dfr(specification$concepts, function(concept) tibble::tibble(
    concept_id = concept$concept_id,
    gold_categories = paste(plan_signature_v02(gold_plan_steps(gold$plans[[concept$concept_id]]), registry)$categories, collapse = " | ")
  ))
  class_detail <- dplyr::left_join(gold_categories, classifications, by = "concept_id") |>
    dplyr::rowwise() |>
    dplyr::mutate(
      classification_correct = !is.na(categories) & categories == gold_categories,
      category_all_gold_included = !is.na(categories) && all(
        strsplit(gold_categories, " \\| ")[[1L]] %in% strsplit(categories, " \\| ")[[1L]]
      ),
      missing_categories = if (is.na(categories)) gold_categories else paste(
        setdiff(strsplit(gold_categories, " \\| ")[[1L]], strsplit(categories, " \\| ")[[1L]]), collapse = " | "
      ),
      extra_categories = if (is.na(categories)) "" else paste(
        setdiff(strsplit(categories, " \\| ")[[1L]], strsplit(gold_categories, " \\| ")[[1L]]), collapse = " | "
      )
    ) |>
    dplyr::ungroup()
  review_path <- trace_path(config$paths$review_dir, "concept_review_audit.csv")
  review <- if (file.exists(review_path)) readr::read_csv(review_path, show_col_types = FALSE) else NULL
  review_summary <- read_optional_json(trace_path(config$paths$review_dir, "expert_review_summary.json"), list())
  model_run <- read_optional_json(trace_path(config$paths$recommendation_dir, "model_run.json"), list(status = "unknown", provenance = "unknown"))
  valid_scores <- !is.na(candidates$recommendation_score) & candidates$recommendation_score >= 0 & candidates$recommendation_score <= 1
  group_failures <- model_run$group_failures %||% list()
  rejected_groups <- if (is.data.frame(group_failures)) nrow(group_failures) else length(group_failures)
  rejected_group_concepts <- if (is.data.frame(group_failures)) {
    sum(lengths(group_failures$concept_ids %||% list()))
  } else {
    sum(vapply(group_failures, function(x) length(x$concept_ids %||% character()), integer(1)))
  }

  summary <- list(
    status = model_run$status %||% "unknown",
    provenance = model_run$provenance %||% "unknown",
    evaluated_concepts = length(concept_ids),
    stage1_valid_classification = sum(!is.na(class_detail$categories)),
    concepts_with_candidate = length(unique(candidates$concept_id[candidates$candidate_rank == 1L])),
    candidate_count = nrow(candidates),
    stage1_category_correct = sum(class_detail$classification_correct, na.rm = TRUE),
    stage1_all_gold_categories_included = sum(class_detail$category_all_gold_included, na.rm = TRUE),
    stage1_category_denominator = length(concept_ids),
    category_top1_correct = sum(top1$category_chain_correct, na.rm = TRUE),
    category_top3_hit = sum(top3$category_top3_hit, na.rm = TRUE),
    function_correct_given_category_correct = sum(top1$function_chain_correct & top1$category_chain_correct, na.rm = TRUE),
    function_given_category_denominator = sum(top1$category_chain_correct, na.rm = TRUE),
    target_set_complete = sum(top1$target_set_complete, na.rm = TRUE),
    target_set_denominator = length(concept_ids),
    target_omission_concepts = sum(nzchar(top1$missing_targets)),
    target_overreport_concepts = sum(nzchar(top1$extra_targets)),
    complete_plan_top1_correct = sum(top1$complete_plan_correct, na.rm = TRUE),
    complete_plan_top3_hit = sum(top3$complete_plan_top3_hit, na.rm = TRUE),
    parameter_schema_first_pass = sum(detail$parameter_schema_valid),
    parameter_schema_denominator = nrow(detail),
    score_in_unit_interval = sum(valid_scores),
    score_denominator = length(valid_scores),
    rejected_groups = rejected_groups,
    rejected_group_concepts = rejected_group_concepts,
    accepted = if (is.null(review)) review_summary$accepted %||% NA_integer_ else sum(review$decision == "accept"),
    modified = if (is.null(review)) review_summary$modified %||% NA_integer_ else sum(review$decision == "modify"),
    rejected = if (is.null(review)) review_summary$rejected %||% NA_integer_ else sum(review$decision == "reject"),
    needs_information = sum(top1$status == "needs_information", na.rm = TRUE),
    modification_none = sum(top1$modification_grade == "none"),
    modification_minor = sum(top1$modification_grade == "minor"),
    modification_moderate = sum(top1$modification_grade == "moderate"),
    modification_major = sum(top1$modification_grade == "major"),
    disclaimer = if (identical(model_run$provenance, "reference_seed")) "参考种子不是模型运行，指标仅验证评价程序。" else "指标来自实际模型运行及当前金标准。"
  )
  by_domain <- top1 |>
    dplyr::group_by(target_domain) |>
    dplyr::summarise(
      category_correct = sum(category_chain_correct), function_correct = sum(function_chain_correct),
      target_complete = sum(target_set_complete), complete_plan_correct = sum(complete_plan_correct),
      minor = sum(modification_grade == "minor"), moderate = sum(modification_grade == "moderate"),
      major = sum(modification_grade == "major"),
      total = dplyr::n(), .groups = "drop"
    )
  by_form <- top1 |>
    dplyr::group_by(target_domain, form_name) |>
    dplyr::summarise(complete_plan_correct = sum(complete_plan_correct), total = dplyr::n(), .groups = "drop")
  by_complexity <- top1 |>
    dplyr::group_by(complexity) |>
    dplyr::summarise(
      complete_plan_correct = sum(complete_plan_correct),
      minor = sum(modification_grade == "minor"), moderate = sum(modification_grade == "moderate"),
      major = sum(modification_grade == "major"), total = dplyr::n(), .groups = "drop"
    )
  by_category <- top1 |>
    tidyr::separate_rows(gold_categories, sep = " \\| ") |>
    dplyr::group_by(gold_categories) |>
    dplyr::summarise(function_correct = sum(function_chain_correct), complete_plan_correct = sum(complete_plan_correct), total = dplyr::n(), .groups = "drop")
  advanced_features <- top1 |>
    dplyr::filter(grepl("source_integration|date_time_conversion|unit_conversion|record_transposition", gold_categories)) |>
    dplyr::select(concept_id, target_domain, gold_categories, function_chain_correct, target_set_complete, complete_plan_correct)

  write_csv(detail, trace_path(config$paths$recommendation_dir, "mapping_evaluation.csv"))
  write_csv(top1, trace_path(config$paths$recommendation_dir, "mapping_evaluation_formal.csv"))
  write_csv(class_detail, trace_path(config$paths$recommendation_dir, "classification_evaluation.csv"))
  write_csv(by_domain, trace_path(config$paths$recommendation_dir, "mapping_evaluation_by_domain.csv"))
  write_csv(by_form, trace_path(config$paths$recommendation_dir, "mapping_evaluation_by_form.csv"))
  write_csv(by_complexity, trace_path(config$paths$recommendation_dir, "mapping_evaluation_by_complexity.csv"))
  write_csv(by_category, trace_path(config$paths$recommendation_dir, "mapping_evaluation_by_category.csv"))
  write_csv(advanced_features, trace_path(config$paths$recommendation_dir, "mapping_evaluation_advanced_features.csv"))
  write_json(summary, trace_path(config$paths$recommendation_dir, "mapping_evaluation_summary.json"))
  invisible(list(
    detail = detail, classifications = class_detail, by_domain = by_domain, by_form = by_form,
    by_complexity = by_complexity, by_category = by_category, advanced_features = advanced_features,
    summary = summary
  ))
}

# -----------------------------------------------------------------------------
# 0.4 reusable atomic/assembly evaluation.  Inputs may be tibbles, lists of
# records, or named lists keyed by task_id.  The evaluator intentionally knows
# nothing about repository paths or the on-disk experiment layout.

v04_record_list <- function(value) {
  if (is.data.frame(value)) {
    return(lapply(seq_len(nrow(value)), function(row) {
      stats::setNames(lapply(names(value), function(name) value[[name]][[row]]), names(value))
    }))
  }
  if (!is.list(value)) trace_abort("v0.4 评价输入必须是 data.frame、tibble 或 list。")
  record_fields <- c(
    "task_id", "concept_id", "target_domain", "output_kind", "semantic_decision",
    "steps", "plan", "plan_json", "candidate_plan", "approved_plan"
  )
  if (any(names(value) %in% record_fields)) return(list(value))
  records <- unname(value)
  record_ids <- names(value)
  if (!all(vapply(records, is.list, logical(1)))) trace_abort("v0.4 评价列表必须由记录组成。")
  if (!is.null(record_ids)) {
    records <- Map(function(record, id) {
      if (is.null(record$task_id) && is.null(record$concept_id)) record$task_id <- id
      record
    }, records, record_ids)
  }
  records
}

v04_first_field <- function(record, fields, default = NULL) {
  for (field in fields) if (!is.null(record[[field]])) return(record[[field]])
  default
}

normalize_string_array_v04 <- function(value) {
  if (is.null(value)) return(character())
  unname(as.character(unlist(value, recursive = TRUE, use.names = FALSE)))
}

normalize_record_v04 <- function(record, expected = FALSE) {
  parse_valid <- TRUE
  decode <- function(value) {
    if (!is.character(value) || length(value) != 1L || is.na(value)) return(value)
    text <- trimws(value)
    if (!grepl("^[\\[{]", text)) return(value)
    tryCatch(from_json_text(text), error = function(error) {
      parse_valid <<- FALSE
      NULL
    })
  }
  semantic <- decode(record$semantic_decision %||% list()) %||% list()
  plan <- decode(v04_first_field(record, c("approved_plan", "candidate_plan", "plan", "plan_json"), list())) %||% list()
  steps <- decode(record$steps)
  if (is.null(steps)) steps <- plan$steps %||% if (is.list(plan) && is.null(names(plan))) plan else list()
  steps <- decode(steps) %||% list()
  target_variables <- decode(v04_first_field(
    semantic, "target_variables",
    v04_first_field(record, "target_variables", plan$target_variables %||% character())
  ))
  explicit_structural <- v04_first_field(record, c("structural_correct", "strictly_validated"), NULL)
  list(
    task_id = as.character(v04_first_field(record, c("task_id", "concept_id"), "")),
    assembly_group_id = as.character(v04_first_field(record, c("assembly_group_id", "group_id"), "")),
    target_domain = as.character(v04_first_field(semantic, "target_domain", v04_first_field(record, "target_domain", ""))),
    output_kind = as.character(v04_first_field(semantic, "output_kind", v04_first_field(record, "output_kind", "variables"))),
    target_variables = normalize_string_array_v04(target_variables),
    steps = steps,
    candidate_rank = as.integer(v04_first_field(record, c("candidate_rank", "rank"), 1L)),
    status = as.character(v04_first_field(record, "status", if (expected) "gold" else "proposed")),
    explicit_structural = explicit_structural,
    parse_valid = parse_valid
  )
}

record_structure_valid_v04 <- function(record, registry) {
  if (!isTRUE(record$parse_valid)) return(FALSE)
  if (!is.null(record$explicit_structural) && !isTRUE(record$explicit_structural)) return(FALSE)
  if (!nzchar(record$task_id) || !nzchar(record$target_domain)) return(FALSE)
  if (!record$output_kind %in% c("variables", "dataset", "none")) return(FALSE)
  if (is.na(record$candidate_rank) || record$candidate_rank < 1L) return(FALSE)
  if (record$output_kind %in% c("dataset", "none") && length(record$target_variables)) return(FALSE)
  if (!is.list(record$steps)) return(FALSE)
  if (identical(record$status, "needs_information")) return(!length(record$steps))
  index <- registry_index(registry)
  for (step in record$steps) {
    if (!is.list(step)) return(FALSE)
    transform_id <- as.character(step$transform_id %||% "")
    if (!nzchar(transform_id) || is.null(index[[transform_id]])) return(FALSE)
    if (!is.null(step$source_ref_ids) && !is.null(step$source_keys)) return(FALSE)
    parameters <- step$parameters %||% list()
    if (length(json_schema_errors(parameters, index[[transform_id]]$parameter_schema))) return(FALSE)
  }
  TRUE
}

target_set_equal_v04 <- function(left, right) {
  schema <- list(
    type = "array", items = list(type = "string"),
    `x-comparison` = "set"
  )
  compare_by_schema(left, right, schema, validate = TRUE)
}

empty_evaluation_detail_v04 <- function() {
  tibble::tibble(
    task_id = character(), assembly_group_id = character(), target_domain = character(),
    candidate_rank = integer(), semantic_correct = logical(), structural_correct = logical(),
    function_correct = logical(), source_correct = logical(), step_target_correct = logical(),
    parameter_correct = logical(), complete_plan_correct = logical(), top3 = logical(),
    missing_targets = character(), extra_targets = character(), status = character()
  )
}

evaluation_rows_v04 <- function(candidates, expected, registry) {
  candidate_records <- lapply(v04_record_list(candidates), normalize_record_v04)
  expected_records <- lapply(v04_record_list(expected), normalize_record_v04, expected = TRUE)
  expected_ids <- vapply(expected_records, `[[`, character(1), "task_id")
  if (any(!nzchar(expected_ids)) || anyDuplicated(expected_ids)) {
    trace_abort("v0.4 金标准 task_id 必须非空且唯一。")
  }
  expected_index <- stats::setNames(expected_records, expected_ids)
  if (!length(candidate_records)) return(empty_evaluation_detail_v04())

  purrr::map_dfr(candidate_records, function(candidate) {
    gold <- expected_index[[candidate$task_id]]
    if (is.null(gold)) {
      return(tibble::tibble(
        task_id = candidate$task_id, assembly_group_id = candidate$assembly_group_id,
        target_domain = candidate$target_domain, candidate_rank = candidate$candidate_rank,
        semantic_correct = FALSE, structural_correct = FALSE, function_correct = FALSE,
        source_correct = FALSE, step_target_correct = FALSE, parameter_correct = FALSE,
        complete_plan_correct = FALSE, top3 = FALSE, missing_targets = "",
        extra_targets = paste(candidate$target_variables, collapse = " | "), status = candidate$status
      ))
    }
    structural_correct <- record_structure_valid_v04(candidate, registry)
    missing_targets <- setdiff(gold$target_variables, candidate$target_variables)
    extra_targets <- setdiff(candidate$target_variables, gold$target_variables)
    semantic_correct <- identical(candidate$output_kind, gold$output_kind) &&
      identical(candidate$target_domain, gold$target_domain) &&
      target_set_equal_v04(candidate$target_variables, gold$target_variables)
    components <- compare_plan_components_v04(candidate$steps, gold$steps, registry)
    complete <- all(c(
      structural_correct, semantic_correct, components$function_correct,
      components$source_correct, components$step_target_correct, components$parameter_correct
    ))
    tibble::tibble(
      task_id = candidate$task_id,
      assembly_group_id = if (nzchar(gold$assembly_group_id)) gold$assembly_group_id else candidate$assembly_group_id,
      target_domain = gold$target_domain,
      candidate_rank = candidate$candidate_rank,
      semantic_correct = semantic_correct,
      structural_correct = structural_correct,
      function_correct = components$function_correct,
      source_correct = components$source_correct,
      step_target_correct = components$step_target_correct,
      parameter_correct = components$parameter_correct,
      complete_plan_correct = complete,
      top3 = candidate$candidate_rank <= 3L && complete,
      missing_targets = paste(missing_targets, collapse = " | "),
      extra_targets = paste(extra_targets, collapse = " | "),
      status = candidate$status
    )
  })
}

evaluate_atomic_plans_v04 <- function(candidates, expected, registry) {
  expected_records <- lapply(v04_record_list(expected), normalize_record_v04, expected = TRUE)
  expected_ids <- vapply(expected_records, `[[`, character(1), "task_id")
  expected_index <- stats::setNames(expected_records, expected_ids)
  detail <- evaluation_rows_v04(candidates, expected, registry)

  atomic <- purrr::map_dfr(expected_ids, function(task_id) {
    gold <- expected_index[[task_id]]
    rows <- dplyr::filter(detail, .data$task_id == .env$task_id)
    ranked <- dplyr::arrange(rows, .data$candidate_rank)
    top1 <- dplyr::filter(ranked, .data$candidate_rank == 1L)
    if (!nrow(top1) && nrow(ranked)) top1 <- ranked[1L, , drop = FALSE]
    top3_hit <- nrow(rows) > 0L && any(rows$top3)
    if (!nrow(top1)) {
      return(tibble::tibble(
        task_id = task_id, assembly_group_id = gold$assembly_group_id,
        target_domain = gold$target_domain, semantic_correct = FALSE,
        structural_correct = FALSE, function_correct = FALSE, source_correct = FALSE,
        step_target_correct = FALSE, parameter_correct = FALSE,
        complete_plan_correct = FALSE, top3 = FALSE, candidate_available = FALSE
      ))
    }
    dplyr::transmute(
      top1[1L, , drop = FALSE], task_id, assembly_group_id, target_domain,
      semantic_correct, structural_correct, function_correct, source_correct,
      step_target_correct, parameter_correct, complete_plan_correct,
      top3 = top3_hit, candidate_available = TRUE
    )
  })

  assembly <- atomic |>
    dplyr::mutate(
      assembly_group_id = dplyr::if_else(
        is.na(.data$assembly_group_id) | !nzchar(.data$assembly_group_id),
        .data$task_id, .data$assembly_group_id
      )
    ) |>
    dplyr::group_by(.data$assembly_group_id) |>
    dplyr::summarise(
      task_count = dplyr::n(),
      semantic_correct = all(.data$semantic_correct),
      structural_correct = all(.data$structural_correct),
      function_correct = all(.data$function_correct),
      source_correct = all(.data$source_correct),
      parameter_correct = all(.data$parameter_correct),
      complete_plan_correct = all(.data$complete_plan_correct),
      top3 = all(.data$top3),
      .groups = "drop"
    )

  list(detail = detail, atomic = atomic, assembly = assembly)
}
