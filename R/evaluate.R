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
  recommendations$target_value <- normalize_optional_text(recommendations$target_value)
  gold$target_value <- normalize_optional_text(gold$target_value)

  compared <- dplyr::left_join(
    recommendations,
    gold,
    by = "mapping_id",
    suffix = c("_recommended", "_gold")
  ) |>
    dplyr::mutate(
      correct = target_domain_recommended == target_domain_gold &
        target_variable_recommended == target_variable_gold &
        target_value_recommended == target_value_gold &
        transform_id_recommended == transform_id_gold
    )

  mapping_ids <- unique(gold$mapping_id)
  covered_ids <- unique(compared$mapping_id[!is.na(compared$target_domain_gold)])
  top1 <- dplyr::filter(compared, candidate_rank == 1L)
  top3_hit <- compared |>
    dplyr::filter(candidate_rank <= 3L) |>
    dplyr::group_by(mapping_id) |>
    dplyr::summarise(hit = any(correct), .groups = "drop")

  model_run_path <- trace_path(config$paths$recommendation_dir, "model_run.json")
  model_run <- if (file.exists(model_run_path)) jsonlite::read_json(model_run_path, simplifyVector = TRUE) else list(status = "unknown")
  review_path <- trace_path(config$paths$review_dir, "mapping_review_audit.csv")
  review <- if (file.exists(review_path)) readr::read_csv(review_path, show_col_types = FALSE) else NULL

  summary <- list(
    status = model_run$status %||% "unknown",
    provenance = model_run$provenance %||% "unknown",
    evaluated_fields = length(mapping_ids),
    covered_fields = length(intersect(mapping_ids, covered_ids)),
    top1_correct = sum(top1$correct, na.rm = TRUE),
    top1_denominator = nrow(top1),
    top3_hit = sum(top3_hit$hit, na.rm = TRUE),
    top3_denominator = nrow(top3_hit),
    accepted = if (is.null(review)) NA_integer_ else sum(review$decision == "accept"),
    modified = if (is.null(review)) NA_integer_ else sum(review$decision == "modify"),
    rejected = if (is.null(review)) NA_integer_ else sum(review$decision == "reject"),
    needs_information = if (is.null(review)) NA_integer_ else sum(review$decision == "needs_information"),
    disclaimer = if (identical(model_run$provenance, "reference_seed")) "参考种子不是实际模型结果，指标仅验证评价程序。" else "指标来自实际模型运行。"
  )
  write_csv(compared, trace_path(config$paths$recommendation_dir, "mapping_evaluation.csv"))
  grouped <- top1 |>
    dplyr::group_by(target_domain_gold, difficulty) |>
    dplyr::summarise(correct = sum(correct, na.rm = TRUE), total = dplyr::n(), .groups = "drop")
  write_csv(grouped, trace_path(config$paths$recommendation_dir, "mapping_evaluation_by_group.csv"))
  write_json(summary, trace_path(config$paths$recommendation_dir, "mapping_evaluation_summary.json"))
  invisible(list(detail = compared, grouped = grouped, summary = summary))
}

