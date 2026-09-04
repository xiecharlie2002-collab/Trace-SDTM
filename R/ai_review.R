# TraceSDTM 0.6 independent artificial-intelligence review -----------------

v06_ai_review_path <- function(config) file.path(trace_path(config$paths$review_dir), "ai_review.json")
v06_ai_review_ack_path <- function(config) file.path(trace_path(config$paths$review_dir), "ai_review_acknowledgements.json")

v06_effective_review_plans <- function(config, state = studio_read_review(config)) {
  assembled <- studio_load_assembled(config)
  specification <- load_mapping_template(config)
  task_index <- stats::setNames(specification$tasks, vapply(specification$tasks, task_id_v04, character(1)))
  lapply(names(task_index), function(id) {
    item <- state$tasks[[id]]
    if (is.null(item)) trace_abort(sprintf("人工审核状态缺少任务 %s。", id))
    decision <- as.character(item$decision %||% "pending")
    if (identical(decision, "reject")) {
      return(list(task_id = id, mapping_status = "excluded", target_domain = task_index[[id]]$target_domain, steps = list()))
    }
    rank <- suppressWarnings(as.integer(item$selected_rank %||% 1L))
    candidate <- studio_candidate_for_task(assembled, id, rank)
    modified <- identical(decision, "modify") && !is.null(item$modified_step)
    steps <- if (modified) list(item$modified_step) else candidate$steps
    semantic <- candidate$semantic_decision %||% list()
    if (modified) {
      mode <- as.character(registry_entry(steps[[1L]]$transform_id, load_transform_registry(config))$target_contract$output_mode)
      semantic <- list(
        output_kind = if (identical(mode, "dataset")) "dataset" else if (identical(mode, "none")) "none" else "variables",
        target_variables = steps[[1L]]$target_variables %||% list()
      )
    }
    list(
      task_id = id,
      mapping_status = if (identical(decision, "needs_information")) "needs_information" else "selected",
      target_domain = task_index[[id]]$target_domain,
      semantic_decision = list(
        output_kind = semantic$output_kind %||% "variables",
        target_variables = semantic$target_variables %||% steps[[1L]]$target_variables %||% list()
      ),
      steps = lapply(steps, function(step) list(
        step_id = step$step_id,
        transform_id = step$transform_id,
        source_ref_ids = step$source_ref_ids %||% list(),
        target_variables = step$target_variables %||% list(),
        parameters = step$parameters %||% list(),
        parameter_sources = step$parameter_sources %||% list()
      ))
    )
  })
}

v06_reviewable_plan_sha256 <- function(config, state = studio_read_review(config)) {
  digest::digest(v06_effective_review_plans(config, state), algo = "sha256", serialize = TRUE)
}

v06_deterministic_review_checks <- function(config, plans, state = studio_read_review(config)) {
  specification <- load_mapping_template(config)
  task_index <- stats::setNames(specification$tasks, vapply(specification$tasks, task_id_v04, character(1)))
  issues <- list()
  for (plan in plans) {
    id <- as.character(plan$task_id)
    task <- task_index[[id]]
    if (identical(plan$mapping_status, "excluded")) {
      if (isTRUE(task$required)) issues[[length(issues) + 1L]] <- list(
        task_id = id, code = "required_task_excluded", location = "mapping_status",
        message = "必需任务不能排除。"
      )
      next
    }
    if (identical(plan$mapping_status, "needs_information")) {
      issues[[length(issues) + 1L]] <- list(
        task_id = id, code = "mapping_needs_information", location = "mapping_status",
        message = "映射仍处于信息不足状态。"
      )
      next
    }
    if (length(plan$steps) != 1L) {
      issues[[length(issues) + 1L]] <- list(
        task_id = id, code = "invalid_atomic_step_count", location = "steps",
        message = "原子任务必须恰好包含一个映射步骤。"
      )
      next
    }
    checked <- tryCatch(studio_validate_modified_step(config, id, plan$steps[[1L]]), error = identity)
    if (inherits(checked, "error")) issues[[length(issues) + 1L]] <- list(
      task_id = id, code = "mapping_validation_failed", location = "steps[1]",
      message = sanitize_for_log(conditionMessage(checked))
    )
  }
  list(valid = !length(issues), checked_at = utc_now(), issues = issues)
}

v06_sanitized_frozen_tasks <- function(specification) {
  lapply(specification$tasks %||% list(), function(task) list(
    task_id = task_id_v04(task), assembly_group_id = task$assembly_group_id,
    clinical_action = task$clinical_action %||% task$action %||% "",
    candidate_target_domains = task$candidate_target_domains %||% list(task$target_domain),
    target_domain = task$target_domain,
    source_refs = lapply(task_source_refs_v04(task), function(ref) list(
      ref_id = ref$ref_id, dataset = ref$dataset, variable = ref$variable, role = ref$role %||% ""
    )),
    depends_on = task$depends_on %||% list(), expected_cardinality = task$expected_cardinality %||% "",
    required = isTRUE(task$required)
  ))
}

v06_review_profile_context <- function(config, tasks) {
  path <- trace_path(config$paths$project_context)
  context <- if (file.exists(path)) jsonlite::read_json(path, simplifyVector = FALSE) else list()
  source_keys <- unique(unlist(lapply(tasks, function(task) vapply(
    task_source_refs_v04(task), function(ref) paste(ref$dataset, ref$variable, sep = "."), character(1)
  )), use.names = FALSE))
  datasets <- unique(sub("\\..*$", "", source_keys))
  profiles <- Filter(function(row) {
    key <- paste(row$source_dataset %||% "", row$source_variable %||% "", sep = ".")
    key %in% source_keys
  }, context$field_profiles %||% list())
  relationships <- Filter(function(row) {
    as.character(row$left_dataset %||% "") %in% datasets && as.character(row$right_dataset %||% "") %in% datasets
  }, context$relationships %||% list())
  list(field_profiles = profiles, relationships = relationships)
}

v06_review_metadata <- function(config, plans) {
  metadata <- load_metadata(config)
  domains <- unique(vapply(plans, function(plan) as.character(plan$target_domain), character(1)))
  stats::setNames(lapply(domains, function(domain) {
    variables <- unique(unlist(lapply(plans, function(plan) {
      if (!identical(as.character(plan$target_domain), domain)) return(character())
      unlist(lapply(plan$steps %||% list(), function(step) step$target_variables %||% character()), use.names = FALSE)
    }), use.names = FALSE))
    entry <- metadata$domains[[domain]]
    list(domain = domain, label = entry$label, variables = entry$variables[intersect(variables, names(entry$variables))])
  }), domains)
}

v06_review_function_contracts <- function(config, plans) {
  registry <- load_transform_registry(config)
  ids <- unique(unlist(lapply(plans, function(plan) vapply(plan$steps %||% list(), function(step) as.character(step$transform_id), character(1))), use.names = FALSE))
  lapply(Filter(function(entry) entry$transform_id %in% ids, registry$transforms), function(entry) list(
    transform_id = entry$transform_id, description = entry$description,
    source_contract = entry$source_contract, target_contract = entry$target_contract,
    parameter_schema = entry$parameter_schema, parameter_resolution = entry$parameter_resolution %||% list(),
    preconditions = entry$preconditions %||% list(), not_allowed_when = entry$not_allowed_when %||% list()
  ))
}

ai_review_prompt_v06 <- function(config, plans, deterministic) {
  specification <- load_mapping_template(config)
  tasks <- v06_sanitized_frozen_tasks(specification)
  payload <- list(
    frozen_tasks = tasks,
    final_mapping_plans = plans,
    related_profiles = v06_review_profile_context(config, specification$tasks),
    standard_metadata = v06_review_metadata(config, plans),
    function_contracts = v06_review_function_contracts(config, plans),
    deterministic_validation = deterministic
  )
  paste(
    "你是独立的临床数据标准映射审查者。这是全新审查上下文，不得假设或复用前序模型的解释。",
    "只审查并报告问题，禁止修改映射、补写参数、选择替代函数或输出代码。",
    "每个冻结任务必须恰好返回一条审查记录。status 只能为 pass、warning 或 error。",
    "每条记录严格包含 task_id、status、issues。每个问题严格包含 issue_id、severity、location、message、suggested_resolution；issue_id 只用大写字母、数字、下划线或连字符，severity 只能为 warning 或 error。",
    "pass 的 issues 必须为空；warning 至少包含一个 warning 且不能包含 error；error 至少包含一个 error。问题编号在整个响应中不可重复。",
    "程序校验错误必须作为 error 报告。不得返回推理过程、推荐分值或修改后的计划。",
    "返回 JSON 对象，顶层键 task_reviews。",
    "审查输入：", registry_json(payload), sep = "\n"
  )
}

parse_ai_review_v06 <- function(response, task_ids) {
  if (is.character(response)) response <- jsonlite::fromJSON(extract_json_content(response), simplifyVector = FALSE)
  records <- response$task_reviews %||% NULL
  if (!is.list(records)) trace_abort("人工智能审查响应必须包含 task_reviews 数组。")
  required <- c("task_id", "status", "issues")
  issue_fields <- c("issue_id", "severity", "location", "message", "suggested_resolution")
  seen_tasks <- character(); seen_issues <- character(); parsed <- list()
  for (record in records) {
    if (!is.list(record) || !setequal(names(record), required)) trace_abort("人工智能审查记录字段不完整或包含额外字段。")
    id <- as.character(record$task_id %||% "")
    if (!id %in% task_ids || id %in% seen_tasks) trace_abort(sprintf("人工智能审查任务编号未知或重复：%s。", id))
    status <- as.character(record$status %||% "")
    if (!status %in% c("pass", "warning", "error")) trace_abort(sprintf("任务 %s 的审查状态无效。", id))
    issues <- record$issues %||% list()
    if (!is.list(issues)) trace_abort(sprintf("任务 %s 的 issues 必须为数组。", id))
    normalized <- lapply(issues, function(issue) {
      if (!is.list(issue) || !setequal(names(issue), issue_fields)) trace_abort(sprintf("任务 %s 的问题字段不完整或包含额外字段。", id))
      issue_id <- as.character(issue$issue_id %||% "")
      severity <- as.character(issue$severity %||% "")
      if (!grepl("^[A-Z0-9][A-Z0-9_-]{2,80}$", issue_id) || issue_id %in% seen_issues) trace_abort(sprintf("问题编号无效或重复：%s。", issue_id))
      if (!severity %in% c("warning", "error")) trace_abort(sprintf("问题 %s 的严重程度无效。", issue_id))
      seen_issues <<- c(seen_issues, issue_id)
      list(
        issue_id = issue_id, severity = severity,
        location = as.character(issue$location %||% ""), message = as.character(issue$message %||% ""),
        suggested_resolution = as.character(issue$suggested_resolution %||% "")
      )
    })
    severities <- vapply(normalized, `[[`, character(1), "severity")
    if (identical(status, "pass") && length(normalized)) trace_abort(sprintf("任务 %s 通过时不能包含问题。", id))
    if (identical(status, "warning") && (!length(normalized) || any(severities == "error"))) trace_abort(sprintf("任务 %s 的警告状态与问题不一致。", id))
    if (identical(status, "error") && !any(severities == "error")) trace_abort(sprintf("任务 %s 的错误状态缺少错误问题。", id))
    seen_tasks <- c(seen_tasks, id)
    parsed[[id]] <- list(task_id = id, status = status, issues = normalized)
  }
  missing <- setdiff(task_ids, seen_tasks)
  if (length(missing)) trace_abort(sprintf("人工智能审查遗漏任务：%s。", paste(missing, collapse = "、")))
  unname(parsed[task_ids])
}

v06_review_model_settings <- function(config) {
  api_key <- Sys.getenv("TRACE_SDTM_API_KEY", unset = "")
  base_url <- Sys.getenv("TRACE_SDTM_REVIEW_BASE_URL", unset = Sys.getenv("TRACE_SDTM_BASE_URL", unset = ""))
  model <- Sys.getenv("TRACE_SDTM_REVIEW_MODEL", unset = Sys.getenv("TRACE_SDTM_MODEL", unset = ""))
  if (!nzchar(api_key) || !nzchar(base_url) || !nzchar(model)) trace_abort("独立人工智能审查需要接口密钥、接口地址和审查模型；审查模型未配置时使用映射模型。")
  list(api_key = api_key, base_url = base_url, model = model, endpoint = paste0(sub("/$", "", base_url), config$model$endpoint_suffix))
}

run_ai_review_v06 <- function(config, request_fn = NULL) {
  studio_assert_mapping_editable_v06(config)
  config <- apply_model_runtime_overrides(config)
  state <- studio_read_review(config)
  plans <- v06_effective_review_plans(config, state)
  deterministic <- v06_deterministic_review_checks(config, plans, state)
  prompt <- ai_review_prompt_v06(config, plans, deterministic)
  directory <- ensure_dir(trace_path(config$paths$review_dir))
  prompt_path <- file.path(directory, "ai_review_prompt.txt")
  writeLines(enc2utf8(prompt), prompt_path, useBytes = TRUE)
  settings <- if (is.null(request_fn)) v06_review_model_settings(config) else list(model = "injected_request")
  started <- utc_now()
  result <- tryCatch({
    raw <- if (is.null(request_fn)) {
      request_json_v02(
        prompt, settings$endpoint, settings$api_key, settings$model, config, "independent_ai_review",
        file.path(directory, "ai_review_raw_response.json")
      )$parsed
    } else {
      v04_call_request(request_fn, prompt, "ai_review", "all_tasks")
    }
    task_ids <- vapply(load_mapping_template(config)$tasks, task_id_v04, character(1))
    reviews <- parse_ai_review_v06(raw, task_ids)
    counts <- table(factor(vapply(reviews, `[[`, character(1), "status"), levels = c("pass", "warning", "error")))
    output <- list(
      schema_version = "0.6", prompt_version = "ai_review_v06_1", reviewed_at = utc_now(),
      model = settings$model, reviewable_plan_sha256 = v06_reviewable_plan_sha256(config, state),
      deterministic_validation = deterministic, task_reviews = reviews,
      summary = list(pass = unname(counts[["pass"]]), warning = unname(counts[["warning"]]), error = unname(counts[["error"]]))
    )
    write_json(output, v06_ai_review_path(config))
    write_json(list(
      schema_version = "0.6", prompt_version = "ai_review_v06_1", model = settings$model,
      started_at = started, completed_at = utc_now(), call_count = 1L,
      prompt_sha256 = digest::digest(prompt, algo = "sha256", serialize = FALSE),
      input_sha256 = output$reviewable_plan_sha256,
      output_sha256 = digest::digest(reviews, algo = "sha256", serialize = TRUE), failure_reason = NULL
    ), file.path(directory, "ai_review_call.json"))
    output
  }, error = function(error) {
    write_json(list(
      schema_version = "0.6", prompt_version = "ai_review_v06_1", model = settings$model,
      started_at = started, completed_at = utc_now(), call_count = 1L,
      prompt_sha256 = digest::digest(prompt, algo = "sha256", serialize = FALSE),
      input_sha256 = v06_reviewable_plan_sha256(config, state), output_sha256 = NULL,
      failure_reason = sanitize_for_log(conditionMessage(error))
    ), file.path(directory, "ai_review_call.json"))
    stop(error)
  })
  if (!is.null(config$studio$project_id)) studio_append_audit(
    config$studio$project_id, "ai_review_completed",
    list(summary = result$summary, reviewable_plan_sha256 = result$reviewable_plan_sha256),
    config$studio$run_id, "ai_reviewer"
  )
  invisible(result)
}

read_ai_review_v06 <- function(config) {
  path <- v06_ai_review_path(config)
  if (!file.exists(path)) trace_abort("尚未完成人工智能独立审查。")
  jsonlite::read_json(path, simplifyVector = FALSE)
}

studio_acknowledge_ai_warnings_v06 <- function(config, reviewer, comment) {
  studio_assert_run_writable(config)
  review <- read_ai_review_v06(config)
  if (!identical(review$reviewable_plan_sha256, v06_reviewable_plan_sha256(config))) trace_abort("映射已改变，请重新执行人工智能独立审查。")
  warning_ids <- unlist(lapply(review$task_reviews, function(item) vapply(
    Filter(function(issue) identical(issue$severity, "warning"), item$issues %||% list()),
    function(issue) as.character(issue$issue_id), character(1)
  )), use.names = FALSE)
  if (!length(warning_ids)) return(invisible(NULL))
  reviewer <- trimws(as.character(reviewer %||% ""))
  comment <- trimws(as.character(comment %||% ""))
  if (!nzchar(reviewer) || !nzchar(comment)) trace_abort("确认人工智能警告时必须填写审核者和说明。")
  acknowledgement <- list(
    schema_version = "0.6", ai_review_sha256 = file_sha256(v06_ai_review_path(config)),
    reviewable_plan_sha256 = review$reviewable_plan_sha256, warning_ids = as.list(warning_ids),
    reviewer = reviewer, comment = comment, acknowledged_at = utc_now()
  )
  write_json(acknowledgement, v06_ai_review_ack_path(config))
  studio_append_audit(config$studio$project_id, "ai_review_warnings_acknowledged", list(
    warning_ids = as.list(warning_ids), comment = comment
  ), config$studio$run_id, reviewer)
  invisible(acknowledgement)
}

studio_assert_ai_review_approvable_v06 <- function(config) {
  review <- read_ai_review_v06(config)
  current_hash <- v06_reviewable_plan_sha256(config)
  if (!identical(review$reviewable_plan_sha256, current_hash)) trace_abort("人工审核选择或修改后映射已改变，请重新执行人工智能独立审查。")
  deterministic_errors <- review$deterministic_validation$issues %||% list()
  ai_errors <- unlist(lapply(review$task_reviews, function(item) vapply(
    Filter(function(issue) identical(issue$severity, "error"), item$issues %||% list()),
    function(issue) as.character(issue$issue_id), character(1)
  )), use.names = FALSE)
  if (length(deterministic_errors) || length(ai_errors)) trace_abort("人工智能审查仍有错误，禁止最终批准。")
  warning_ids <- unlist(lapply(review$task_reviews, function(item) vapply(
    Filter(function(issue) identical(issue$severity, "warning"), item$issues %||% list()),
    function(issue) as.character(issue$issue_id), character(1)
  )), use.names = FALSE)
  if (length(warning_ids)) {
    acknowledgement <- read_json_file(v06_ai_review_ack_path(config), list())
    if (!identical(acknowledgement$ai_review_sha256 %||% "", file_sha256(v06_ai_review_path(config))) ||
        !setequal(unlist(acknowledgement$warning_ids %||% character(), use.names = FALSE), warning_ids) ||
        !nzchar(trimws(as.character(acknowledgement$comment %||% "")))) {
      trace_abort("人工智能审查警告尚未填写确认说明。")
    }
  }
  invisible(review)
}
