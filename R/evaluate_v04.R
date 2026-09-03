# TraceSDTM 0.4 three-stage evaluation ---------------------------------------

gold_atomic_records_v04 <- function(specification, gold, registry) {
  task_index <- stats::setNames(specification$tasks, vapply(specification$tasks, task_id_v04, character(1)))
  lapply(names(task_index), function(id) {
    task <- task_index[[id]]
    steps <- gold$plans[[id]] %||% list()
    if (length(steps) != 1L) trace_abort(sprintf("%s 的原子金标准必须恰好包含一个步骤。", id))
    mode <- registry_entry(steps[[1]]$transform_id, registry)$target_contract$output_mode
    list(
      task_id = id, assembly_group_id = task$assembly_group_id, target_domain = task$target_domain,
      output_kind = if (mode == "dataset") "dataset" else if (mode == "none") "none" else "variables",
      target_variables = steps[[1]]$target_variables %||% list(), steps = steps,
      candidate_rank = 1L, status = "gold"
    )
  })
}

v04_valid_by_task <- function(stage, rank_one = FALSE) {
  valid <- stage$valid %||% list()
  if (isTRUE(rank_one)) valid <- Filter(function(x) identical(as.integer(x$candidate_rank), 1L), valid)
  if (!length(valid)) return(list())
  stats::setNames(valid, vapply(valid, function(x) as.character(x$task_id), character(1)))
}

v04_function_match <- function(candidate, gold_step) {
  identical(as.character(candidate$transform_id %||% ""), as.character(gold_step$transform_id)) &&
    identical(
      unname(unlist(candidate$source_ref_ids %||% character(), use.names = FALSE)),
      unname(unlist(gold_step$source_ref_ids %||% character(), use.names = FALSE))
    )
}

three_stage_detail_v04 <- function(specification, gold, registry, targets, functions,
                                   parameters, assembled, conditional_functions = NULL,
                                   conditional_parameters = NULL) {
  expected <- gold_atomic_records_v04(specification, gold, registry)
  expected_index <- stats::setNames(expected, vapply(expected, `[[`, character(1), "task_id"))
  target_index <- v04_valid_by_task(targets)
  function_top <- v04_valid_by_task(functions, rank_one = TRUE)
  function_all <- functions$valid %||% list()
  resolution_index <- parameters$resolutions$valid %||% list()
  completed_index <- parameters$valid %||% list()
  assembled_eval <- evaluate_atomic_plans_v04(assembled$plans %||% list(), expected, registry)$atomic

  conditional_top <- if (is.null(conditional_functions)) list() else v04_valid_by_task(conditional_functions, rank_one = TRUE)
  conditional_all <- conditional_functions$valid %||% list()

  purrr::map_dfr(names(expected_index), function(id) {
    gold_record <- expected_index[[id]]
    gold_step <- gold_record$steps[[1]]
    target <- target_index[[id]]
    target_structural <- !is.null(target)
    semantic <- target_structural && identical(as.character(target$output_kind), gold_record$output_kind) &&
      target_set_equal_v04(target$target_variables, gold_record$target_variables)
    function_candidate <- function_top[[id]]
    function_structural <- !is.null(function_candidate)
    function_correct <- function_structural && v04_function_match(function_candidate, gold_step)
    top3_candidates <- Filter(function(x) identical(as.character(x$task_id), id) && as.integer(x$candidate_rank) <= 3L, function_all)
    function_top3 <- any(vapply(top3_candidates, v04_function_match, logical(1), gold_step = gold_step))

    key <- paste(id, 1L, sep = "#")
    resolution <- resolution_index[[key]]
    parameter_structural <- !is.null(resolution) && identical(resolution$status, "ready") &&
      (isTRUE(resolution$fully_resolved) || !is.null(completed_index[[key]]))
    assembled_row <- dplyr::filter(assembled_eval, .data$task_id == .env$id)
    if (!nrow(assembled_row)) assembled_row <- tibble::tibble(
      parameter_correct = FALSE, complete_plan_correct = FALSE, top3 = FALSE,
      source_correct = FALSE, step_target_correct = FALSE
    )

    conditional_candidate <- conditional_top[[id]]
    conditional_function_correct <- !is.null(conditional_candidate) && v04_function_match(conditional_candidate, gold_step)
    conditional_candidates <- Filter(function(x) identical(as.character(x$task_id), id) && as.integer(x$candidate_rank) <= 3L, conditional_all)
    conditional_function_top3 <- any(vapply(conditional_candidates, v04_function_match, logical(1), gold_step = gold_step))

    assembled_available <- "candidate_available" %in% names(assembled_row) && isTRUE(assembled_row$candidate_available[[1]])
    end_structure <- target_structural && function_structural && parameter_structural && assembled_available
    modification <- if (isTRUE(assembled_row$complete_plan_correct[[1]])) {
      "none"
    } else if (semantic && function_correct && isTRUE(assembled_row$source_correct[[1]]) && isTRUE(assembled_row$step_target_correct[[1]])) {
      "minor"
    } else if (semantic && function_structural) {
      "moderate"
    } else "major"

    task <- specification$tasks[[match(id, vapply(specification$tasks, task_id_v04, character(1)))]]
    tibble::tibble(
      task_id = id, assembly_group_id = gold_record$assembly_group_id,
      scenario = specification$specification$scenario, target_domain = gold_record$target_domain,
      difficulty = task$difficulty %||% specification$specification$benchmark_level %||% specification$specification$scenario,
      target_structural_correct = target_structural,
      semantic_correct = semantic,
      function_structural_correct = function_structural,
      function_top1_correct = function_correct,
      function_top3_correct = function_top3,
      parameter_structural_correct = parameter_structural,
      parameter_correct = isTRUE(assembled_row$parameter_correct[[1]]),
      end_to_end_structural_correct = end_structure,
      complete_plan_top1_correct = isTRUE(assembled_row$complete_plan_correct[[1]]),
      complete_plan_top3_correct = isTRUE(assembled_row$top3[[1]]),
      conditional_function_top1_correct = conditional_function_correct,
      conditional_function_top3_correct = conditional_function_top3,
      modification_grade = modification,
      missing_targets = paste(setdiff(gold_record$target_variables, unlist(target$target_variables %||% character())), collapse = " | "),
      extra_targets = paste(setdiff(unlist(target$target_variables %||% character()), gold_record$target_variables), collapse = " | ")
    )
  })
}

v04_metric_rows <- function(detail, grouping = character()) {
  metrics <- c(
    semantic_correct = "语义正确",
    target_structural_correct = "目标阶段结构正确",
    function_structural_correct = "函数阶段结构正确",
    parameter_structural_correct = "参数阶段结构正确",
    end_to_end_structural_correct = "端到端结构正确",
    function_top1_correct = "函数首选正确",
    function_top3_correct = "函数前三项命中",
    parameter_correct = "最终参数正确",
    complete_plan_top1_correct = "完整计划首选正确",
    complete_plan_top3_correct = "完整计划前三项命中",
    conditional_function_top1_correct = "给定正确目标时函数首选正确",
    conditional_function_top3_correct = "给定正确目标时函数前三项命中"
  )
  groups <- if (length(grouping)) dplyr::group_by(detail, dplyr::across(dplyr::all_of(grouping))) else detail
  purrr::imap_dfr(metrics, function(label, column) groups |>
    dplyr::summarise(numerator = sum(.data[[column]], na.rm = TRUE), denominator = dplyr::n(), .groups = "drop") |>
    dplyr::mutate(metric = label, proportion = .data$numerator / .data$denominator, .after = dplyr::last_col()))
}

assembly_rollup_v04 <- function(detail) {
  detail |>
    dplyr::group_by(.data$scenario, .data$assembly_group_id) |>
    dplyr::summarise(
      target_domain = dplyr::first(.data$target_domain), task_count = dplyr::n(),
      semantic_correct = all(.data$semantic_correct),
      structural_correct = all(.data$end_to_end_structural_correct),
      complete_plan_correct = all(.data$complete_plan_top1_correct),
      complete_plan_top3 = all(.data$complete_plan_top3_correct), .groups = "drop"
    )
}

parameter_accounting_v04 <- function(parameters) {
  resolutions <- parameters$resolutions$valid %||% list()
  purrr::map_dfr(resolutions, function(x) tibble::tibble(
    task_id = x$task_id, candidate_rank = as.integer(x$candidate_rank),
    parameter_total = length(x$injected_parameters) + length(x$unresolved_parameters) + length(x$unavailable_parameters),
    automatically_injected = length(x$injected_parameters),
    requested_from_model = length(x$unresolved_parameters),
    unavailable = length(x$unavailable_parameters),
    fully_resolved = isTRUE(x$fully_resolved)
  ))
}

evaluate_three_stage_v04 <- function(config, targets, functions, parameters, assembled,
                                     conditional_functions = NULL, conditional_parameters = NULL,
                                     output_dir = trace_path(config$paths$recommendation_dir)) {
  specification <- load_mapping_template(config)
  gold <- load_gold_specification(config)
  registry <- load_transform_registry(config)
  detail <- three_stage_detail_v04(
    specification, gold, registry, targets, functions, parameters, assembled,
    conditional_functions, conditional_parameters
  )
  metrics <- v04_metric_rows(detail)
  by_domain <- v04_metric_rows(detail, "target_domain")
  assembly <- assembly_rollup_v04(detail)
  parameter_counts <- parameter_accounting_v04(parameters)
  ensure_dir(output_dir)
  write_csv(detail, file.path(output_dir, "v04_atomic_evaluation.csv"))
  write_csv(metrics, file.path(output_dir, "v04_metrics.csv"))
  write_csv(by_domain, file.path(output_dir, "v04_metrics_by_domain.csv"))
  write_csv(assembly, file.path(output_dir, "v04_assembly_evaluation.csv"))
  write_csv(parameter_counts, file.path(output_dir, "v04_parameter_accounting.csv"))
  summary <- list(
    schema_version = "0.4", scenario = config$project$scenario,
    task_count = nrow(detail), assembly_group_count = nrow(assembly),
    semantic_correct = sum(detail$semantic_correct),
    structural_correct = sum(detail$end_to_end_structural_correct),
    complete_plan_top1_correct = sum(detail$complete_plan_top1_correct),
    complete_plan_top3_correct = sum(detail$complete_plan_top3_correct),
    parameters_total = sum(parameter_counts$parameter_total),
    parameters_injected = sum(parameter_counts$automatically_injected),
    parameters_requested_from_model = sum(parameter_counts$requested_from_model),
    generated_at = utc_now()
  )
  write_json(summary, file.path(output_dir, "v04_evaluation_summary.json"))
  list(detail = detail, metrics = metrics, by_domain = by_domain, assembly = assembly,
       parameter_accounting = parameter_counts, summary = summary)
}
