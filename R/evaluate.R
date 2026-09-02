normalize_optional_text <- function(x) {
  value <- as.character(x)
  value[is.na(value)] <- ""
  value
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
  if (length(steps) >= 2L || any(categories %in% c("datetime_conversion", "temporal_derivation"))) return("moderate")
  "simple"
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
    parameter_correct <- function_correct && identical(
      registry_json(lapply(proposed_signature$parameters, sort_json_object)),
      registry_json(lapply(gold_signature$parameters, sort_json_object))
    )
    tibble::tibble(
      concept_id = concept_id,
      candidate_rank = as.integer(candidate_rank),
      target_domain = target_domain,
      form_name = concept$form_name,
      complexity = concept_complexity_v02(expected, registry),
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
      parameter_schema_valid = TRUE,
      recommendation_score = as.numeric(recommendation_score),
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
  top1 <- dplyr::filter(detail, candidate_rank == 1L)
  concept_ids <- vapply(specification$concepts, `[[`, character(1), "concept_id")
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
    dplyr::mutate(classification_correct = !is.na(categories) & categories == gold_categories)
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
    concepts_with_candidate = length(unique(candidates$concept_id[candidates$candidate_rank == 1L])),
    candidate_count = nrow(candidates),
    stage1_category_correct = sum(class_detail$classification_correct, na.rm = TRUE),
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
    needs_information = if (is.null(review)) review_summary$needs_information %||% NA_integer_ else sum(review$decision == "needs_information"),
    disclaimer = if (identical(model_run$provenance, "reference_seed")) "参考种子不是模型运行，指标仅验证评价程序。" else "指标来自实际模型运行及当前金标准。"
  )
  by_domain <- top1 |>
    dplyr::group_by(target_domain) |>
    dplyr::summarise(
      category_correct = sum(category_chain_correct), function_correct = sum(function_chain_correct),
      target_complete = sum(target_set_complete), complete_plan_correct = sum(complete_plan_correct),
      total = dplyr::n(), .groups = "drop"
    )
  by_form <- top1 |>
    dplyr::group_by(target_domain, form_name) |>
    dplyr::summarise(complete_plan_correct = sum(complete_plan_correct), total = dplyr::n(), .groups = "drop")
  by_complexity <- top1 |>
    dplyr::group_by(complexity) |>
    dplyr::summarise(complete_plan_correct = sum(complete_plan_correct), total = dplyr::n(), .groups = "drop")
  by_category <- top1 |>
    tidyr::separate_rows(gold_categories, sep = " \\| ") |>
    dplyr::group_by(gold_categories) |>
    dplyr::summarise(function_correct = sum(function_chain_correct), complete_plan_correct = sum(complete_plan_correct), total = dplyr::n(), .groups = "drop")
  advanced_features <- top1 |>
    dplyr::filter(grepl("source_integration|datetime_conversion|unit_conversion|record_transposition", gold_categories)) |>
    dplyr::select(concept_id, target_domain, gold_categories, function_chain_correct, target_set_complete, complete_plan_correct)

  write_csv(detail, trace_path(config$paths$recommendation_dir, "mapping_evaluation.csv"))
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
