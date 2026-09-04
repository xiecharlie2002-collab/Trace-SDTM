# TraceSDTM Studio 0.6 structured review service -----------------------------

studio_review_state_path <- function(config) file.path(trace_path(config$paths$review_dir), "studio_review_state.json")

studio_load_assembled <- function(config) {
  path <- file.path(trace_path(config$paths$recommendation_dir), "assembled_recommendations.json")
  if (!file.exists(path)) trace_abort("尚未生成完整候选计划。")
  jsonlite::read_json(path, simplifyVector = FALSE)
}

studio_initialize_review <- function(config, overwrite = FALSE) {
  path <- studio_review_state_path(config)
  if (file.exists(path) && !isTRUE(overwrite)) return(jsonlite::read_json(path, simplifyVector = FALSE))
  studio_assert_run_writable(config)
  assembled <- studio_load_assembled(config)
  specification <- load_mapping_template(config)
  plans <- assembled$plans %||% list()
  by_task <- split(plans, vapply(plans, function(plan) as.character(plan$task_id), character(1)))
  task_state <- stats::setNames(lapply(specification$tasks, function(task) {
    candidates <- by_task[[task_id_v04(task)]] %||% list()
    ranks <- sort(vapply(candidates, function(plan) as.integer(plan$candidate_rank), integer(1)))
    list(
      task_id = task_id_v04(task), required = isTRUE(task$required),
      decision = "pending", selected_rank = if (length(ranks)) ranks[[1L]] else NULL,
      review_comment = "", reviewer = "", reviewed_at = NULL,
      modified_step = NULL, validation = list(valid = FALSE, messages = list("尚未审核"))
    )
  }), vapply(specification$tasks, task_id_v04, character(1)))
  state <- list(
    schema_version = "0.6", status = "in_review", created_at = utc_now(), updated_at = utc_now(),
    project_id = config$studio$project_id %||% NULL, run_id = config$studio$run_id %||% NULL,
    assembled_sha256 = file_sha256(file.path(trace_path(config$paths$recommendation_dir), "assembled_recommendations.json")),
    tasks = task_state
  )
  write_json(state, path)
  state
}

studio_read_review <- function(config) studio_initialize_review(config)

studio_candidate_for_task <- function(assembled, task_id, rank) {
  plans <- assembled$plans %||% list()
  hits <- Filter(function(plan) identical(as.character(plan$task_id), as.character(task_id)) &&
                   identical(as.integer(plan$candidate_rank), as.integer(rank)), plans)
  if (length(hits) != 1L) trace_abort(sprintf("%s 无法唯一定位候选序号 %s。", task_id, rank))
  hits[[1L]]
}

studio_parameter_provenance <- function(parameters, source = "reviewer", reference = "studio_structured_review") {
  stats::setNames(lapply(names(parameters %||% list()), function(name) list(
    source = source, reference = reference
  )), names(parameters %||% list()))
}

studio_validate_modified_step <- function(config, task_id, step) {
  specification <- load_mapping_template(config)
  tasks <- stats::setNames(specification$tasks, vapply(specification$tasks, task_id_v04, character(1)))
  task <- tasks[[task_id]]
  if (is.null(task)) trace_abort(sprintf("未知审核任务：%s。", task_id))
  if (!is.list(step) || !nzchar(as.character(step$transform_id %||% ""))) trace_abort("修改方案必须选择转换函数。")
  step$step_id <- paste0(task_id, "_STEP_01")
  step$source_ref_ids <- as.list(unname(unlist(step$source_ref_ids %||% character(), use.names = FALSE)))
  step$target_variables <- as.list(unname(unlist(step$target_variables %||% character(), use.names = FALSE)))
  step$parameters <- step$parameters %||% list()
  if (is.null(step$parameter_sources)) step$parameter_sources <- studio_parameter_provenance(step$parameters)
  allowed_provenance <- c("policy", "registry", "resource", "derived", "model", "reviewer")
  if (length(step$parameter_sources)) {
    invalid <- names(Filter(function(item) !as.character(item$source %||% "") %in% allowed_provenance, step$parameter_sources))
    if (length(invalid)) trace_abort(sprintf("参数来源无效：%s。", paste(invalid, collapse = "、")))
  }
  entry <- registry_entry(step$transform_id, load_transform_registry(config))
  output_mode <- as.character(entry$target_contract$output_mode)
  output_kind <- if (output_mode == "dataset") "dataset" else if (output_mode == "none") "none" else "variables"
  task$semantic_decision <- list(output_kind = output_kind, target_variables = step$target_variables)
  task$approved_plan <- list(steps = list(step))
  validate_task_v04(task, specification, load_metadata(config), load_transform_registry(config), config, require_approved = TRUE)
  step
}

studio_save_review_decision <- function(config, task_id, decision, reviewer,
                                        selected_rank = NULL, review_comment = "",
                                        modified_step = NULL) {
  studio_assert_mapping_editable_v06(config)
  reviewer <- trimws(as.character(reviewer %||% ""))
  if (!nzchar(reviewer) || tolower(reviewer) %in% c("portfolio_demo_reviewer", "demo", "default")) {
    trace_abort("请填写可识别的审核者标识，不能使用默认演示名称。")
  }
  decision <- tolower(trimws(as.character(decision %||% "")))
  if (!decision %in% c("accept", "modify", "reject", "needs_information")) trace_abort("审核决定无效。")
  state <- studio_read_review(config)
  item <- state$tasks[[task_id]]
  if (is.null(item)) trace_abort(sprintf("未知审核任务：%s。", task_id))
  assembled <- studio_load_assembled(config)
  validation <- list(valid = TRUE, messages = list())
  if (identical(decision, "accept")) {
    rank <- suppressWarnings(as.integer(selected_rank))
    if (is.na(rank)) trace_abort("接受候选时必须选择候选序号。")
    studio_candidate_for_task(assembled, task_id, rank)
    item$selected_rank <- rank
    item$modified_step <- NULL
  } else if (identical(decision, "modify")) {
    item$modified_step <- studio_validate_modified_step(config, task_id, modified_step)
  } else if (identical(decision, "reject")) {
    if (isTRUE(item$required)) trace_abort(sprintf("必需任务 %s 不能拒绝。", task_id))
    item$modified_step <- NULL
  } else {
    validation <- list(valid = FALSE, messages = list("信息不足尚未解决"))
    item$modified_step <- NULL
  }
  item$decision <- decision
  item$reviewer <- reviewer
  item$review_comment <- as.character(review_comment %||% "")
  item$reviewed_at <- utc_now()
  item$validation <- validation
  state$tasks[[task_id]] <- item
  state$updated_at <- utc_now()
  state$status <- if (all(vapply(state$tasks, function(x) x$decision %in% c("accept", "modify", "reject"), logical(1)))) "ready_for_approval" else "in_review"
  write_json(state, studio_review_state_path(config))
  if (!is.null(config$studio$project_id)) {
    studio_append_audit(config$studio$project_id, "review_decision_saved", list(
      task_id = task_id, decision = decision,
      selected_rank = if (identical(decision, "accept")) item$selected_rank else NULL
    ), config$studio$run_id, reviewer)
    studio_update_run_stage(config$studio$project_id, config$studio$run_id, "human_approval",
                            if (identical(state$status, "ready_for_approval")) "running" else "pending",
                            actor = reviewer)
  }
  invisible(state)
}

studio_review_tables <- function(config, state = studio_read_review(config)) {
  assembled <- studio_load_assembled(config)
  plans <- assembled$plans %||% list()
  specification <- load_mapping_template(config)
  task_index <- stats::setNames(specification$tasks, vapply(specification$tasks, task_id_v04, character(1)))
  candidates <- v04_plan_rows(plans)
  review <- purrr::map_dfr(names(state$tasks), function(id) {
    item <- state$tasks[[id]]
    task <- task_index[[id]]
    tibble::tibble(
      task_id = id, assembly_group_id = task$assembly_group_id,
      target_domain = task$target_domain, form_name = task$form_name %||% "",
      required = isTRUE(task$required), selected_rank = as.integer(item$selected_rank %||% NA_integer_),
      recommendation_score = {
        hit <- tryCatch(studio_candidate_for_task(assembled, id, item$selected_rank), error = function(error) NULL)
        if (is.null(hit)) NA_real_ else as.numeric(hit$recommendation_score)
      },
      model_status = if (length(Filter(function(plan) identical(plan$task_id, id), plans))) "proposed" else "missing",
      decision = if (identical(item$decision, "pending")) "needs_information" else item$decision,
      review_comment = item$review_comment %||% ""
    )
  })
  final_steps <- purrr::map_dfr(names(state$tasks), function(id) {
    item <- state$tasks[[id]]
    if (!identical(item$decision, "modify") || is.null(item$modified_step)) return(tibble::tibble())
    step <- item$modified_step
    tibble::tibble(
      task_id = id, assembly_group_id = task_index[[id]]$assembly_group_id, step_order = 1L,
      transform_id = as.character(step$transform_id),
      source_ref_ids = as.character(registry_json(step$source_ref_ids %||% list())),
      target_variables = as.character(registry_json(step$target_variables %||% list())),
      parameters = as.character(registry_json(step$parameters %||% list())),
      parameter_sources = as.character(registry_json(step$parameter_sources %||% list()))
    )
  })
  if (!nrow(final_steps)) final_steps <- v04_step_rows(plans, top_only = TRUE) |> dplyr::select(-"candidate_rank")
  list(review = review, candidates = candidates, final_steps = final_steps, plans = plans)
}

studio_write_review_workbook <- function(config, state = studio_read_review(config), filename = "mapping_review.xlsx") {
  path <- file.path(trace_path(config$paths$review_dir), filename)
  approved_path <- trace_path(config$paths$approved_specification %||% "")
  if (nzchar(as.character(config$paths$approved_specification %||% "")) && file.exists(approved_path)) {
    if (file.exists(path)) return(path)
    trace_abort("批准后的审核记录为只读状态，且没有既有审核工作簿。")
  }
  if (!is.null(config$studio$project_id)) {
    project <- studio_read_project(config$studio$project_id)
    run <- studio_read_run(config$studio$project_id, config$studio$run_id)
    read_only <- !identical(as.character(project$active_run_id %||% ""), as.character(config$studio$run_id)) ||
      isTRUE(run$stale) || isTRUE(project$archived)
    if (read_only) {
      if (file.exists(path)) return(path)
      trace_abort("历史或归档运行为只读状态，且没有既有审核工作簿。")
    }
    studio_assert_run_writable(config)
  }
  tables <- studio_review_tables(config, state)
  specification <- load_mapping_template(config)
  workbook <- openxlsx::createWorkbook()
  write_review_sheet(workbook, "Instructions", data.frame(
    item = c("用途", "审核边界", "批准边界"),
    description = c("TraceSDTM Studio 0.6 只读审核快照。", "只允许登记函数、来源编号、目标变量和受控参数。", "构建只读取批准规格，不调用模型。"),
    stringsAsFactors = FALSE
  ), filter = FALSE)
  write_review_sheet(workbook, "Task Review", tables$review)
  write_review_sheet(workbook, "Candidate Plans", tables$candidates)
  write_review_sheet(workbook, "Final Steps", tables$final_steps)
  write_review_sheet(workbook, "Function Skeletons", v04_step_rows(tables$plans) |> dplyr::select(-dplyr::all_of(c("parameters", "parameter_sources"))))
  write_review_sheet(workbook, "Parameter Resolution", v04_parameter_rows(tables$plans))
  write_review_sheet(workbook, "Source Context", v04_source_context_rows(specification, config))
  write_review_sheet(workbook, "Transform Catalog", registry_catalog_table(load_transform_registry(config)))
  ensure_parent(path)
  openxlsx::saveWorkbook(workbook, path, overwrite = TRUE)
  invisible(path)
}

studio_approve_review <- function(config, reviewer = NULL) {
  studio_assert_mapping_editable_v06(config)
  state <- studio_read_review(config)
  if (!identical(state$status, "ready_for_approval")) trace_abort("仍有未完成或信息不足的审核任务，不能批准。")
  ai_review <- studio_assert_ai_review_approvable_v06(config)
  reviewers <- unique(vapply(state$tasks, function(x) trimws(as.character(x$reviewer %||% "")), character(1)))
  reviewers <- reviewers[nzchar(reviewers)]
  reviewer <- trimws(as.character(reviewer %||% if (length(reviewers) == 1L) reviewers else ""))
  if (!nzchar(reviewer)) trace_abort("批准时必须提供唯一审核者标识。")
  workbook <- studio_write_review_workbook(config, state, "mapping_review.xlsx")
  tables <- studio_review_tables(config, state)
  specification <- approved_specification_from_review_tables_v04(
    tables$review, tables$candidates, tables$final_steps, config, reviewer, file_sha256(workbook)
  )
  output <- trace_path(config$paths$approved_specification)
  write_yaml(specification, output)
  approval <- list(
    schema_version = "0.6", status = "approved", reviewer = reviewer,
    approved_at = utc_now(), compatible_mapping_schema = "0.4",
    approved_specification = studio_relative_path(output, config$studio$run_path %||% dirname(output)),
    approved_specification_sha256 = file_sha256(output), review_workbook_sha256 = file_sha256(workbook),
    ai_review_sha256 = file_sha256(v06_ai_review_path(config)),
    ai_review_summary = ai_review$summary
  )
  write_yaml(approval, file.path(trace_path(config$paths$review_dir), "studio_approval.yml"))
  write_csv(tables$review, file.path(trace_path(config$paths$review_dir), "task_review_audit.csv"))
  write_csv(tables$final_steps, file.path(trace_path(config$paths$review_dir), "final_steps_audit.csv"))
  state$status <- "approved"
  state$approved_at <- approval$approved_at
  state$approved_specification_sha256 <- approval$approved_specification_sha256
  write_json(state, studio_review_state_path(config))
  if (!is.null(config$studio$project_id)) {
    studio_update_run_stage(config$studio$project_id, config$studio$run_id, "human_approval", "completed", actor = reviewer)
    studio_append_audit(config$studio$project_id, "review_approved", list(
      specification_sha256 = approval$approved_specification_sha256,
      workbook_sha256 = approval$review_workbook_sha256
    ), config$studio$run_id, reviewer)
  }
  invisible(specification)
}

studio_import_review_workbook <- function(config, uploaded_file, reviewer) {
  trace_abort("TraceSDTM 0.6 的审核工作簿是只读审计导出，不再作为批准或导入入口。请在工作台中完成结构化审核。")
}

studio_review_task_details <- function(config, task_id) {
  state <- studio_read_review(config)
  assembled <- studio_load_assembled(config)
  specification <- load_mapping_template(config)
  task <- Filter(function(x) identical(task_id_v04(x), task_id), specification$tasks)
  if (length(task) != 1L) trace_abort("未知审核任务。")
  plans <- Filter(function(x) identical(as.character(x$task_id), task_id), assembled$plans %||% list())
  list(task = task[[1L]], state = state$tasks[[task_id]], candidates = plans)
}

studio_review_editor_options <- function(config, task_id, target_variables = NULL) {
  details <- studio_review_task_details(config, task_id)
  task <- details$task
  metadata <- load_metadata(config)
  registry <- load_transform_registry(config)
  decision <- list(output_kind = "variables", target_variables = as.list(target_variables %||% character()))
  entries <- Filter(function(entry) {
    if (!isTRUE(entry$model_selectable)) return(FALSE)
    if (!task$target_domain %in% unlist(entry$target_contract$domains, use.names = FALSE)) return(FALSE)
    TRUE
  }, registry$transforms)
  list(
    source_refs = {
      refs <- task_source_refs_v04(task)
      ids <- vapply(refs, `[[`, character(1), "ref_id")
      labels <- vapply(refs, function(ref) paste0(ref$ref_id, " — ", ref$dataset, ".", ref$variable), character(1))
      stats::setNames(ids, labels)
    },
    target_variables = names(metadata$domains[[task$target_domain]]$variables),
    transforms = {
      ids <- vapply(entries, `[[`, character(1), "transform_id")
      labels <- vapply(entries, function(entry) paste0(entry$transform_id, " — ", entry$description), character(1))
      stats::setNames(ids, labels)
    },
    parameter_schemas = stats::setNames(lapply(entries, `[[`, "parameter_schema"), vapply(entries, `[[`, character(1), "transform_id")),
    current = details
  )
}
