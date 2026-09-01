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
