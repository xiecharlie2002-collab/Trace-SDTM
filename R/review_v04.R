# TraceSDTM 0.4 atomic-task review and approval --------------------------------

v04_plan_rows <- function(plans) {
  if (is.null(plans)) plans <- list()
  purrr::map_dfr(plans, function(plan) tibble::tibble(
    task_id = as.character(plan$task_id),
    assembly_group_id = as.character(plan$assembly_group_id),
    target_domain = as.character(plan$target_domain),
    candidate_rank = as.integer(plan$candidate_rank),
    output_kind = as.character(plan$semantic_decision$output_kind),
    target_variables = as.character(registry_json(plan$semantic_decision$target_variables %||% list())),
    plan_json = as.character(registry_json(list(steps = plan$steps))),
    recommendation_score = as.numeric(plan$recommendation_score),
    reason = as.character(plan$reason %||% ""),
    uncertainties = as.character(plan$uncertainties %||% ""),
    status = as.character(plan$status %||% "proposed")
  ))
}

v04_step_rows <- function(plans, top_only = FALSE) {
  selected <- if (isTRUE(top_only)) Filter(function(x) identical(as.integer(x$candidate_rank), 1L), plans) else plans
  purrr::map_dfr(selected, function(plan) purrr::imap_dfr(plan$steps %||% list(), function(step, order) tibble::tibble(
    task_id = as.character(plan$task_id),
    assembly_group_id = as.character(plan$assembly_group_id),
    candidate_rank = as.integer(plan$candidate_rank),
    step_order = as.integer(order),
    transform_id = as.character(step$transform_id),
    source_ref_ids = as.character(registry_json(step$source_ref_ids %||% list())),
    target_variables = as.character(registry_json(step$target_variables %||% list())),
    parameters = as.character(registry_json(step$parameters %||% list())),
    parameter_sources = as.character(registry_json(step$parameter_sources %||% list()))
  )))
}

v04_parameter_rows <- function(plans) {
  purrr::map_dfr(plans, function(plan) purrr::imap_dfr(plan$steps %||% list(), function(step, order) {
    parameters <- step$parameters %||% list()
    if (!length(parameters)) return(tibble::tibble(
      task_id = character(), candidate_rank = integer(), step_order = integer(),
      parameter = character(), value = character(), source = character(), reference = character()
    ))
    purrr::imap_dfr(parameters, function(value, name) {
      provenance <- step$parameter_sources[[name]] %||% list(source = "未记录", reference = "")
      tibble::tibble(
        task_id = as.character(plan$task_id), candidate_rank = as.integer(plan$candidate_rank),
        step_order = as.integer(order), parameter = name,
        value = as.character(registry_json(value)), source = as.character(provenance$source %||% "未记录"),
        reference = as.character(provenance$reference %||% "")
      )
    })
  }))
}

v04_source_context_rows <- function(specification, config) {
  dictionary_path <- trace_path(config$paths$profile_dir, "source_dictionary.csv")
  dictionary <- if (file.exists(dictionary_path)) readr::read_csv(dictionary_path, show_col_types = FALSE) else tibble::tibble()
  purrr::map_dfr(specification$tasks, function(task) {
    refs <- task_source_refs_v04(task)
    if (!length(refs)) return(tibble::tibble(
      task_id = task$task_id, ref_id = "", source_dataset = "", source_variable = "",
      role = "", example_values = "", missing_rate = NA_real_
    ))
    purrr::map_dfr(refs, function(ref) {
      hit <- if (nrow(dictionary)) dplyr::filter(
        dictionary, .data$source_dataset == .env$ref$dataset, .data$source_variable == .env$ref$variable
      ) else tibble::tibble()
      tibble::tibble(
        task_id = task$task_id, ref_id = ref$ref_id, source_dataset = ref$dataset,
        source_variable = ref$variable, role = ref$role,
        example_values = if (isTRUE(config$project$studio)) "" else if (nrow(hit)) as.character(hit$example_values[[1]] %||% "") else "",
        missing_rate = if (nrow(hit)) as.numeric(hit$missing_rate[[1]] %||% NA_real_) else NA_real_
      )
    })
  })
}

create_review_workbook_v04 <- function(assembled, config = load_project_config(), preapprove = FALSE) {
  plans <- assembled$plans %||% assembled
  specification <- load_mapping_template(config)
  tasks <- stats::setNames(specification$tasks, vapply(specification$tasks, task_id_v04, character(1)))
  candidates <- v04_plan_rows(plans)
  top <- candidates |>
    dplyr::filter(.data$candidate_rank == 1L) |>
    dplyr::arrange(.data$target_domain, .data$task_id)
  review <- purrr::map_dfr(names(tasks), function(id) {
    hit <- dplyr::filter(top, .data$task_id == .env$id)
    task <- tasks[[id]]
    tibble::tibble(
      task_id = id, assembly_group_id = task$assembly_group_id,
      target_domain = task$target_domain, form_name = task$form_name %||% "",
      required = isTRUE(task$required), selected_rank = if (nrow(hit)) hit$candidate_rank[[1]] else NA_integer_,
      recommendation_score = if (nrow(hit)) hit$recommendation_score[[1]] else NA_real_,
      model_status = if (nrow(hit)) hit$status[[1]] else "missing",
      decision = if (preapprove && nrow(hit)) "accept" else "",
      review_comment = if (preapprove && nrow(hit)) "专家金标准种子，仅用于离线演示。" else ""
    )
  })
  steps <- v04_step_rows(plans)
  final_steps <- v04_step_rows(plans, top_only = TRUE) |> dplyr::select(-"candidate_rank")

  workbook <- openxlsx::createWorkbook()
  write_review_sheet(workbook, "Instructions", data.frame(
    item = c("审核单位", "accept", "modify", "reject", "needs_information", "构建边界", "参数来源"),
    description = c(
      "每行是一个原子临床动作；旧概念由 assembly_group_id 聚合。",
      "接受 selected_rank 指定的完整原子方案。",
      "在 Final Steps 中填写完整且通过注册表验证的单步方案。",
      "仅非必需任务可以拒绝。",
      "信息不足未解决时不能批准或构建。",
      "正式构建只读取批准后的0.4 YAML，构建过程不调用模型。",
      "policy、registry、resource、derived、model、reviewer 分别记录每个参数的来源。"
    ), stringsAsFactors = FALSE
  ), filter = FALSE)
  write_review_sheet(workbook, "Task Review", review)
  if (nrow(review)) openxlsx::dataValidation(
    workbook, "Task Review", cols = match("decision", names(review)), rows = 2:(nrow(review) + 1L),
    type = "list", value = '"accept,modify,reject,needs_information"'
  )
  write_review_sheet(workbook, "Target Decisions", dplyr::select(candidates, dplyr::all_of(c("task_id", "candidate_rank", "output_kind", "target_variables"))))
  write_review_sheet(workbook, "Function Skeletons", dplyr::select(steps, -dplyr::all_of(c("parameters", "parameter_sources"))))
  write_review_sheet(workbook, "Parameter Resolution", v04_parameter_rows(plans))
  write_review_sheet(workbook, "Candidate Plans", candidates)
  write_review_sheet(workbook, "Final Steps", final_steps)
  write_review_sheet(workbook, "Source Context", v04_source_context_rows(specification, config))
  write_review_sheet(workbook, "Transform Catalog", registry_catalog_table(load_transform_registry(config)))
  path <- trace_path(config$paths$review_dir, "mapping_review.xlsx")
  ensure_parent(path)
  openxlsx::saveWorkbook(workbook, path, overwrite = TRUE)
  invisible(path)
}

v04_steps_from_review <- function(data, task_id) {
  rows <- dplyr::filter(data, .data$task_id == .env$task_id) |> dplyr::arrange(.data$step_order)
  if (!nrow(rows)) return(list())
  purrr::pmap(rows, function(task_id, assembly_group_id, step_order, transform_id,
                             source_ref_ids, target_variables, parameters, parameter_sources, ...) list(
    step_id = paste0(task_id, "_STEP_", sprintf("%02d", as.integer(step_order))),
    transform_id = as.character(transform_id),
    source_ref_ids = as.list(unname(unlist(from_json_text(as.character(source_ref_ids)), use.names = FALSE))),
    target_variables = as.list(unname(unlist(from_json_text(as.character(target_variables)), use.names = FALSE))),
    parameters = from_json_text(as.character(parameters)),
    parameter_sources = from_json_text(as.character(parameter_sources))
  ))
}

v04_candidate_steps <- function(candidates, task_id, rank) {
  hit <- dplyr::filter(candidates, .data$task_id == .env$task_id, .data$candidate_rank == .env$rank)
  if (nrow(hit) != 1L) trace_abort(sprintf("%s 无法唯一定位候选序号 %s。", task_id, rank))
  from_json_text(hit$plan_json[[1]])$steps %||% list()
}

review_against_gold_v04 <- function(config = load_project_config()) {
  path <- trace_path(config$paths$review_dir, "mapping_review.xlsx")
  if (!file.exists(path)) trace_abort("缺少审核工作簿，请先执行 recommend。")
  review <- openxlsx::read.xlsx(path, sheet = "Task Review", check.names = FALSE)
  candidates <- openxlsx::read.xlsx(path, sheet = "Candidate Plans", check.names = FALSE)
  gold <- load_gold_specification(config)$plans
  registry <- load_transform_registry(config)
  replacement <- list()
  for (index in seq_len(nrow(review))) {
    id <- as.character(review$task_id[[index]])
    expected <- gold[[id]] %||% list()
    proposed <- tryCatch(v04_candidate_steps(candidates, id, as.integer(review$selected_rank[[index]])), error = function(e) list())
    components <- compare_plan_components_v04(proposed, expected, registry)
    exact <- length(proposed) == length(expected) && all(unlist(components, use.names = FALSE))
    review$decision[[index]] <- if (exact) "accept" else "modify"
    review$review_comment[[index]] <- if (exact) "评价用金标准复核：完整原子方案一致。" else "评价用金标准复核：已替换为专家原子方案；不等同于法规签字。"
    replacement[[id]] <- expected
  }
  tasks <- stats::setNames(load_mapping_template(config)$tasks, vapply(load_mapping_template(config)$tasks, task_id_v04, character(1)))
  final <- purrr::imap_dfr(replacement, function(steps, id) purrr::imap_dfr(steps, function(step, order) tibble::tibble(
    task_id = id, assembly_group_id = tasks[[id]]$assembly_group_id, step_order = as.integer(order),
    transform_id = step$transform_id,
    source_ref_ids = as.character(registry_json(step$source_ref_ids %||% list())),
    target_variables = as.character(registry_json(step$target_variables %||% list())),
    parameters = as.character(registry_json(step$parameters %||% list())),
    parameter_sources = as.character(registry_json(stats::setNames(lapply(names(step$parameters %||% list()), function(x) list(source = "reviewer", reference = "gold_review_v04")), names(step$parameters %||% list()))))
  )))
  workbook <- openxlsx::loadWorkbook(path)
  openxlsx::writeData(workbook, "Task Review", review, startRow = 1L, startCol = 1L, colNames = TRUE)
  openxlsx::writeData(workbook, "Final Steps", final, startRow = 1L, startCol = 1L, colNames = TRUE)
  openxlsx::saveWorkbook(workbook, path, overwrite = TRUE)
  summary <- list(
    method = "gold_standard_atomic_plan_v04", reviewed_at = utc_now(), total = nrow(review),
    accepted = sum(review$decision == "accept"), modified = sum(review$decision == "modify"),
    disclaimer = "用于作品集实验评价，不等同于法规流程中的独立临床标准专家签字。"
  )
  write_json(summary, trace_path(config$paths$review_dir, "expert_review_summary.json"))
  invisible(review)
}

approved_specification_from_review_tables_v04 <- function(review, candidates, final_steps,
                                                           config = load_project_config(),
                                                           reviewer,
                                                           review_artifact_sha256 = NA_character_) {
  reviewer <- trimws(as.character(reviewer %||% ""))
  if (!nzchar(reviewer)) trace_abort("审核者标识不能为空。")
  review$decision <- tolower(trimws(as.character(review$decision)))
  if (any(!review$decision %in% c("accept", "modify", "reject", "needs_information"))) trace_abort("每个原子任务都必须选择有效审核决定。")
  if (any(review$decision == "needs_information")) trace_abort("仍有 needs_information，不能锁定规格。")

  specification <- load_mapping_template(config)
  tasks <- stats::setNames(specification$tasks, vapply(specification$tasks, task_id_v04, character(1)))
  registry <- load_transform_registry(config)
  approved <- list()
  for (index in seq_len(nrow(review))) {
    id <- as.character(review$task_id[[index]])
    task <- tasks[[id]]
    if (is.null(task)) trace_abort(sprintf("审核表包含未知任务：%s。", id))
    decision <- review$decision[[index]]
    if (decision == "reject") {
      if (isTRUE(task$required)) trace_abort(sprintf("必需任务 %s 不能拒绝。", id))
      next
    }
    steps <- if (decision == "accept") {
      v04_candidate_steps(candidates, id, as.integer(review$selected_rank[[index]]))
    } else v04_steps_from_review(final_steps, id)
    if (length(steps) != 1L) trace_abort(sprintf("原子任务 %s 必须且只能包含一个最终步骤。", id))
    entry <- registry_entry(steps[[1]]$transform_id, registry)
    output_mode <- as.character(entry$target_contract$output_mode)
    output_kind <- if (output_mode == "dataset") "dataset" else if (output_mode == "none") "none" else "variables"
    task$semantic_decision <- list(
      output_kind = output_kind,
      target_variables = as.list(unname(unlist(steps[[1]]$target_variables %||% character(), use.names = FALSE)))
    )
    task$approved_plan <- list(steps = steps)
    task$review <- list(
      decision = decision, reviewer = reviewer, reviewed_at = utc_now(),
      comment = as.character(review$review_comment[[index]] %||% "")
    )
    approved[[id]] <- task
  }
  task_order <- vapply(specification$tasks, task_id_v04, character(1))
  specification$tasks <- unname(approved[intersect(task_order, names(approved))])
  specification$specification$status <- "approved"
  specification$specification$approval <- list(
    reviewer = reviewer, approved_at = utc_now(), review_artifact_sha256 = review_artifact_sha256,
    review_workbook_sha256 = review_artifact_sha256,
    registry_version = registry$registry_version,
    registry_sha256 = file_sha256(trace_path(config$paths$transform_registry))
  )
  validate_specification_v04(specification, config, require_approved = TRUE)
  specification
}

approve_mapping_v04 <- function(config = load_project_config()) {
  path <- trace_path(config$paths$review_dir, "mapping_review.xlsx")
  if (!file.exists(path)) trace_abort("缺少 mapping_review.xlsx。请先执行 recommend。")
  review <- openxlsx::read.xlsx(path, sheet = "Task Review", check.names = FALSE)
  candidates <- openxlsx::read.xlsx(path, sheet = "Candidate Plans", check.names = FALSE)
  final_steps <- openxlsx::read.xlsx(path, sheet = "Final Steps", check.names = FALSE)
  reviewer <- Sys.getenv("TRACE_SDTM_REVIEWER", unset = if (isTRUE(config$project$studio)) "" else "portfolio_demo_reviewer")
  if (!nzchar(trimws(reviewer))) trace_abort("请通过 TRACE_SDTM_REVIEWER 提供审核者标识。")
  specification <- approved_specification_from_review_tables_v04(
    review, candidates, final_steps, config, reviewer, file_sha256(path)
  )
  output <- trace_path(config$paths$approved_specification)
  ensure_parent(output)
  yaml::write_yaml(specification, output)
  write_csv(review, trace_path(config$paths$review_dir, "task_review_audit.csv"))
  write_csv(final_steps, trace_path(config$paths$review_dir, "final_steps_audit.csv"))
  trace_info("已锁定 0.4 审核规格：%s", output)
  invisible(specification)
}
