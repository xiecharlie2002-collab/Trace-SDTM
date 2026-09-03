# TraceSDTM 0.4 three-stage recommendation -----------------------------------

task_id_v04 <- function(task) as.character(task$task_id %||% task$concept_id %||% "")

task_source_refs_v04 <- function(task) {
  refs <- task$source_refs %||% list()
  if (!length(refs)) return(list())
  task_id <- task_id_v04(task)
  result <- lapply(seq_along(refs), function(index) {
    ref <- refs[[index]]
    if (!nzchar(as.character(ref$ref_id %||% ""))) ref$ref_id <- sprintf("%s_SRC_%02d", task_id, index)
    ref$ref_id <- as.character(ref$ref_id)
    ref$dataset <- as.character(ref$dataset)
    ref$variable <- as.character(ref$variable)
    ref$role <- as.character(ref$role %||% "")
    ref
  })
  ids <- vapply(result, `[[`, character(1), "ref_id")
  if (anyDuplicated(ids)) trace_abort(sprintf("%s 存在重复 ref_id：%s", task_id, paste(unique(ids[duplicated(ids)]), collapse = ", ")))
  result
}

prepare_tasks_v04 <- function(specification) {
  tasks <- specification$tasks %||% specification$concepts %||% list()
  result <- lapply(tasks, function(task) {
    id <- task_id_v04(task)
    if (!nzchar(id)) trace_abort("v0.4 任务缺少 task_id。")
    task$task_id <- id
    task$assembly_group_id <- as.character(task$assembly_group_id %||% task$concept_id %||% id)
    task$source_refs <- task_source_refs_v04(task)
    task$depends_on <- as.list(unname(unlist(task$depends_on %||% character(), use.names = FALSE)))
    task
  })
  ids <- vapply(result, task_id_v04, character(1))
  if (anyDuplicated(ids)) trace_abort(sprintf("v0.4 规格存在重复任务：%s", paste(unique(ids[duplicated(ids)]), collapse = ", ")))
  known_dependencies <- unique(unlist(lapply(result, function(x) x$depends_on), use.names = FALSE))
  unknown <- setdiff(known_dependencies, ids)
  if (length(unknown)) trace_abort(sprintf("v0.4 规格引用未知依赖：%s", paste(unknown, collapse = ", ")))
  result
}

v04_task_context <- function(task, specification, dictionary = NULL) {
  refs <- task_source_refs_v04(task)
  profiles <- if (is.null(dictionary)) list() else lapply(refs, source_profile_context, specification = specification, dictionary = dictionary)
  list(
    task_id = task_id_v04(task), assembly_group_id = task$assembly_group_id,
    target_domain = task$target_domain,
    clinical_action = task$clinical_action %||% task$intent %||% task$description %||% task$form_name,
    form_name = task$form_name, expected_cardinality = task$expected_cardinality,
    required = isTRUE(task$required), depends_on = task$depends_on,
    source_refs = refs, field_profiles = profiles
  )
}

v04_group_tasks <- function(group) {
  tasks <- group$tasks %||% group$concepts %||% list()
  lapply(tasks, function(x) { x$source_refs <- task_source_refs_v04(x); x })
}

v04_strict_fields <- function(record, required, allowed, label) {
  if (!is.list(record) || is.null(names(record))) trace_abort(sprintf("%s 必须是 JSON 对象。", label))
  missing <- setdiff(required, names(record))
  extra <- setdiff(names(record), allowed)
  if (length(missing)) trace_abort(sprintf("%s 缺少字段：%s", label, paste(missing, collapse = ", ")))
  if (length(extra)) trace_abort(sprintf("%s 包含本阶段禁止字段：%s", label, paste(extra, collapse = ", ")))
  invisible(TRUE)
}

target_identification_prompt_v04 <- function(group, specification, metadata, policies, dictionary = NULL) {
  tasks <- v04_group_tasks(group)
  domain <- as.character(group$target_domain %||% tasks[[1]]$target_domain)
  variables <- metadata$domains[[domain]]$variables
  context <- lapply(tasks, v04_task_context, specification = specification, dictionary = dictionary)
  prompt <- paste(
    "你是临床数据标准映射助手。当前只识别每项临床动作生成的 SDTM 目标，不选择函数，不填写参数。",
    "每项任务恰好返回一条记录。只能引用提供的 task_id 和目标域变量目录，不得发明变量。",
    "output_kind 只能是 variables、dataset 或 none。variables 必须有目标变量；dataset 和 none 的 target_variables 必须为空数组。",
    "status 只能是 proposed 或 needs_information。信息不足时不得猜测，target_variables 必须为空。",
    "禁止返回 transform_id、source_ref_ids、parameters、代码、公式、连接键或正则表达式。",
    "返回 JSON 对象，顶层键 target_identifications。每条严格包含 task_id、output_kind、target_variables、score、evidence、uncertainties、status。",
    "目标域变量目录：", registry_json(list(domain = domain, variables = variables)),
    "项目映射政策：", registry_json(policies),
    "任务及观察证据：", registry_json(context),
    sep = "\n"
  )
  assert_blind_prompt(prompt)
}

validate_target_identification_v04 <- function(record, task, metadata) {
  required <- c("task_id", "output_kind", "target_variables", "score", "evidence", "uncertainties", "status")
  v04_strict_fields(record, required, required, paste0(task_id_v04(task), " 目标识别"))
  id <- as.character(record$task_id)
  if (!identical(id, task_id_v04(task))) trace_abort(sprintf("目标识别试图改写任务编号：%s", id))
  output_kind <- as.character(record$output_kind)
  if (!output_kind %in% c("variables", "dataset", "none")) trace_abort(sprintf("%s 的 output_kind 无效。", id))
  status <- as.character(record$status)
  if (!status %in% c("proposed", "needs_information")) trace_abort(sprintf("%s 的目标状态无效。", id))
  score <- as.numeric(record$score)
  if (length(score) != 1L || is.na(score) || score < 0 || score > 1) trace_abort(sprintf("%s 的目标分值超出 0 到 1。", id))
  targets <- unname(unlist(record$target_variables %||% character(), use.names = FALSE))
  if (anyDuplicated(targets)) trace_abort(sprintf("%s 返回重复目标变量。", id))
  if (identical(status, "needs_information") && length(targets)) trace_abort(sprintf("%s 信息不足时不能返回目标变量。", id))
  if (identical(status, "proposed") && identical(output_kind, "variables") && !length(targets)) trace_abort(sprintf("%s 的 variables 输出缺少目标变量。", id))
  if (output_kind %in% c("dataset", "none") && length(targets)) trace_abort(sprintf("%s 的 %s 输出必须使用空目标变量数组。", id, output_kind))
  allowed <- names(metadata$domains[[task$target_domain]]$variables)
  unknown <- setdiff(targets, allowed)
  if (length(unknown)) trace_abort(sprintf("%s 返回未知目标变量：%s", id, paste(unknown, collapse = ", ")))
  list(
    task_id = id, target_domain = task$target_domain, output_kind = output_kind,
    target_variables = as.list(targets), score = score,
    evidence = recommendation_text(record, "evidence"), uncertainties = recommendation_text(record, "uncertainties"),
    status = status, structure_valid = TRUE
  )
}

v04_isolated_parse <- function(records, tasks, validator, stage) {
  lookup <- stats::setNames(tasks, vapply(tasks, task_id_v04, character(1)))
  valid <- list(); failures <- list(); seen <- character()
  if (!is.list(records)) records <- list()
  for (index in seq_along(records)) {
    record <- records[[index]]
    id <- as.character(record$task_id %||% "")
    if (!nzchar(id) || is.null(lookup[[id]])) {
      failures[[length(failures) + 1L]] <- list(task_id = id, stage = stage, error = sprintf("%s 返回未知或空 task_id。", stage), record_index = index)
      next
    }
    if (id %in% seen) {
      valid[[id]] <- NULL
      failures[[length(failures) + 1L]] <- list(task_id = id, stage = stage, error = sprintf("%s 返回重复任务。", stage), record_index = index)
      next
    }
    seen <- c(seen, id)
    value <- tryCatch(validator(record, lookup[[id]]), error = identity)
    if (inherits(value, "error")) failures[[length(failures) + 1L]] <- list(task_id = id, stage = stage, error = sanitize_for_log(conditionMessage(value)), record_index = index)
    else valid[[id]] <- value
  }
  missing <- setdiff(names(lookup), seen)
  for (id in missing) failures[[length(failures) + 1L]] <- list(task_id = id, stage = stage, error = sprintf("%s 遗漏任务。", stage), record_index = NA_integer_)
  list(valid = valid, failures = failures, missing_task_ids = missing, stage = stage)
}

parse_target_identifications_v04 <- function(records, group, metadata) {
  tasks <- v04_group_tasks(group)
  v04_isolated_parse(records, tasks, function(record, task) validate_target_identification_v04(record, task, metadata), "target_identification")
}

v04_transform_compatible <- function(entry, task, decision) {
  if (!isTRUE(entry$model_selectable)) return(FALSE)
  if (!task$target_domain %in% unname(unlist(entry$target_contract$domains, use.names = FALSE))) return(FALSE)
  mode <- as.character(entry$target_contract$output_mode)
  kind <- decision$output_kind
  if (identical(kind, "dataset") && !identical(mode, "dataset")) return(FALSE)
  if (identical(kind, "none") && !identical(mode, "none")) return(FALSE)
  if (identical(kind, "variables") && mode %in% c("dataset", "none")) return(FALSE)
  targets <- unname(unlist(decision$target_variables, use.names = FALSE))
  patterns <- unname(unlist(entry$target_contract$patterns %||% character(), use.names = FALSE))
  if (length(targets) && length(patterns) && any(!vapply(targets, function(x) any(vapply(patterns, grepl, logical(1), x = x)), logical(1)))) return(FALSE)
  if (identical(mode, "single") && length(targets) != 1L) return(FALSE)
  TRUE
}

v04_function_card <- function(entry) list(
  transform_id = entry$transform_id,
  category = entry$category,
  description = entry$description,
  execution_stage = entry$execution_stage,
  accepted_source_count = list(
    minimum = entry$source_contract$minimum,
    maximum = entry$source_contract$maximum,
    minimum_datasets = entry$source_contract$minimum_datasets,
    maximum_datasets = entry$source_contract$maximum_datasets,
    types = entry$source_contract$types
  ),
  output_mode = entry$target_contract$output_mode,
  not_allowed_when = as.list(unname(unlist(entry$not_allowed_when %||% character(), use.names = FALSE)))
)

v04_function_task_context <- function(task, specification, dictionary = NULL) {
  context <- v04_task_context(task, specification, dictionary)
  context$field_profiles <- lapply(context$field_profiles, function(profile) list(
    declared_source_key = profile$declared_source_key,
    role = profile$role,
    resolution = profile$resolution,
    evidence = lapply(if (is.data.frame(profile$evidence)) {
      lapply(seq_len(nrow(profile$evidence)), function(index) as.list(profile$evidence[index, , drop = FALSE]))
    } else {
      profile$evidence
    }, function(item) list(
      source_dataset = item$source_dataset,
      source_variable = item$source_variable,
      data_type = item$data_type,
      example_values = item$example_values,
      format_candidates = item$format_candidates,
      partial_tokens = item$partial_tokens
    ))
  ))
  context
}

function_selection_prompt_v04 <- function(group, target_results, registry, specification, policies, dictionary = NULL) {
  tasks <- v04_group_tasks(group)
  task_index <- stats::setNames(tasks, vapply(tasks, task_id_v04, character(1)))
  decisions <- target_results$valid %||% target_results
  payload <- lapply(decisions, function(decision) {
    task <- task_index[[decision$task_id]]
    entries <- Filter(function(entry) v04_transform_compatible(entry, task, decision), registry$transforms)
    list(
      target_decision = decision,
      task = v04_function_task_context(task, specification, dictionary),
      allowed_transform_ids = as.list(vapply(entries, `[[`, character(1), "transform_id"))
    )
  })
  used_ids <- unique(unlist(lapply(payload, `[[`, "allowed_transform_ids"), use.names = FALSE))
  catalog <- lapply(Filter(function(entry) entry$transform_id %in% used_ids, registry$transforms), v04_function_card)
  prompt <- paste(
    "你是临床数据标准映射助手。当前只选择转换函数和来源引用，不填写或推测任何参数。",
    "每项任务可返回一到三个候选，candidate_rank 为 1 到 3且不可重复。",
    "只能使用该任务 allowed_functions 中的 transform_id，以及该任务 source_refs 中的 ref_id。不得返回 dataset.variable。",
    "一个候选只代表当前原子临床动作；不得添加相邻动作的函数。",
    "status 只能为 proposed 或 needs_information。proposed 必须有 transform_id；needs_information 的 transform_id 必须为空且 source_ref_ids 必须为空。",
    "review_required 必须为 true；所有模型建议均需人工审核。",
    "禁止返回 target_variables、parameters、函数参数、代码、公式、连接键或正则表达式。",
    "返回 JSON 对象，顶层键 function_candidates。每条严格包含 task_id、candidate_rank、transform_id、source_ref_ids、score、reason、uncertainties、status、review_required。",
    "每项任务只可从 allowed_transform_ids 选择；完整函数卡在共享函数目录中按 transform_id 查找。",
    "目标决定、任务上下文和允许函数编号：", registry_json(payload),
    "共享函数目录：", registry_json(catalog),
    "项目政策：", registry_json(policies),
    sep = "\n"
  )
  assert_blind_prompt(prompt)
}

validate_function_candidate_v04 <- function(record, task, decision, registry) {
  required <- c("task_id", "candidate_rank", "transform_id", "source_ref_ids", "score", "reason", "uncertainties", "status", "review_required")
  v04_strict_fields(record, required, required, paste0(task_id_v04(task), " 函数选择"))
  id <- task_id_v04(task)
  rank <- as.integer(record$candidate_rank)
  if (length(rank) != 1L || is.na(rank) || rank < 1L || rank > 3L) trace_abort(sprintf("%s 的候选序号必须为 1 到 3。", id))
  score <- as.numeric(record$score)
  if (length(score) != 1L || is.na(score) || score < 0 || score > 1) trace_abort(sprintf("%s 的函数分值超出 0 到 1。", id))
  status <- as.character(record$status)
  if (!status %in% c("proposed", "needs_information")) trace_abort(sprintf("%s 的函数状态无效。", id))
  transform_id <- as.character(record$transform_id %||% "")
  source_ids <- unname(unlist(record$source_ref_ids %||% character(), use.names = FALSE))
  if (anyDuplicated(source_ids)) trace_abort(sprintf("%s 返回重复来源编号。", id))
  if (identical(status, "needs_information")) {
    if (nzchar(transform_id) || length(source_ids)) trace_abort(sprintf("%s 信息不足时不能附带函数或来源。", id))
  } else {
    if (!nzchar(transform_id)) trace_abort(sprintf("%s 的 proposed 候选缺少函数。", id))
    entry <- registry_entry(transform_id, registry)
    if (!v04_transform_compatible(entry, task, decision)) trace_abort(sprintf("%s 选择的函数 %s 与目标决定不兼容。", id, transform_id))
    refs <- task_source_refs_v04(task)
    index <- stats::setNames(refs, vapply(refs, `[[`, character(1), "ref_id"))
    unknown <- setdiff(source_ids, names(index))
    if (length(unknown)) trace_abort(sprintf("%s 引用了未知来源编号：%s", id, paste(unknown, collapse = ", ")))
    selected <- unname(index[source_ids])
    source_count <- length(selected)
    dataset_count <- length(unique(vapply(selected, function(x) x$dataset, character(1))))
    contract <- entry$source_contract
    if (source_count < contract$minimum || source_count > contract$maximum) trace_abort(sprintf("%s/%s 的来源字段数不符合注册表。", id, transform_id))
    if (dataset_count < contract$minimum_datasets || dataset_count > contract$maximum_datasets) trace_abort(sprintf("%s/%s 的来源数据集数不符合注册表。", id, transform_id))
  }
  if (!isTRUE(record$review_required)) trace_abort(sprintf("%s 必须要求人工审核。", id))
  list(
    task_id = id, candidate_rank = rank, transform_id = transform_id,
    source_ref_ids = as.list(source_ids), score = score,
    reason = recommendation_text(record, "reason"), uncertainties = recommendation_text(record, "uncertainties"),
    status = status, review_required = TRUE, structure_valid = TRUE
  )
}

parse_function_candidates_v04 <- function(records, group, target_results, registry) {
  tasks <- v04_group_tasks(group)
  decisions <- target_results$valid %||% target_results
  lookup <- stats::setNames(tasks, vapply(tasks, task_id_v04, character(1)))
  valid <- list(); failures <- list(); keys <- character()
  if (!is.list(records)) records <- list()
  for (index in seq_along(records)) {
    record <- records[[index]]; id <- as.character(record$task_id %||% "")
    rank <- suppressWarnings(as.integer(record$candidate_rank %||% NA_integer_)); key <- paste(id, rank, sep = "#")
    if (is.null(lookup[[id]]) || is.null(decisions[[id]])) {
      failures[[length(failures) + 1L]] <- list(task_id = id, candidate_rank = rank, stage = "function_selection", error = "函数阶段引用未知或未通过目标阶段的任务。", record_index = index); next
    }
    if (key %in% keys) {
      valid[[key]] <- NULL; failures[[length(failures) + 1L]] <- list(task_id = id, candidate_rank = rank, stage = "function_selection", error = "函数阶段候选序号重复。", record_index = index); next
    }
    keys <- c(keys, key)
    value <- tryCatch(validate_function_candidate_v04(record, lookup[[id]], decisions[[id]], registry), error = identity)
    if (inherits(value, "error")) failures[[length(failures) + 1L]] <- list(task_id = id, candidate_rank = rank, stage = "function_selection", error = sanitize_for_log(conditionMessage(value)), record_index = index)
    else valid[[key]] <- value
  }
  proposed_ids <- unique(vapply(Filter(function(x) x$candidate_rank == 1L, valid), function(x) x$task_id, character(1)))
  expected <- names(Filter(function(x) identical(x$status, "proposed"), decisions))
  missing <- setdiff(expected, proposed_ids)
  for (id in missing) failures[[length(failures) + 1L]] <- list(task_id = id, candidate_rank = 1L, stage = "function_selection", error = "函数阶段缺少首选候选。", record_index = NA_integer_)
  list(valid = valid, failures = failures, missing_task_ids = missing, stage = "function_selection")
}

parameter_completion_prompt_v04 <- function(resolutions) {
  requested <- Filter(function(x) identical(x$status, "ready") && !isTRUE(x$fully_resolved) && length(x$unresolved_parameters), resolutions)
  if (!length(requested)) return(NULL)
  payload <- lapply(requested, function(x) list(
    task_id = x$task_id, candidate_rank = x$candidate_rank, transform_id = x$transform_id,
    requested_parameters = x$unresolved_parameters, allowed_options = x$parameter_options,
    injected_parameter_names = as.list(names(x$injected_parameters))
  ))
  paste(
    "你是临床数据标准映射助手。当前只补全程序尚不能确定、且已列出有限选项的参数。",
    "只能填写 requested_parameters，并且值只能来自 allowed_options。不得返回或覆盖 injected_parameter_names。",
    "禁止增加参数、函数、来源、目标、代码、公式、连接键、正则表达式或自由条件。",
    "返回 JSON 对象，顶层键 parameter_completions。每条严格包含 task_id、candidate_rank、parameters、uncertainties、status。",
    "status 只能为 proposed 或 needs_information；needs_information 时 parameters 必须为空对象。",
    "待补全参数：", registry_json(payload), sep = "\n"
  )
}

parse_parameter_completions_v04 <- function(records, resolutions) {
  lookup <- stats::setNames(resolutions, vapply(resolutions, function(x) paste(x$task_id, x$candidate_rank, sep = "#"), character(1)))
  valid <- list(); failures <- list(); seen <- character()
  if (!is.list(records)) records <- list()
  for (index in seq_along(records)) {
    record <- records[[index]]
    required <- c("task_id", "candidate_rank", "parameters", "uncertainties", "status")
    id <- as.character(record$task_id %||% ""); rank <- suppressWarnings(as.integer(record$candidate_rank %||% NA_integer_)); key <- paste(id, rank, sep = "#")
    value <- tryCatch({
      v04_strict_fields(record, required, required, paste0(key, " 参数补全"))
      if (is.null(lookup[[key]])) trace_abort(sprintf("参数补全引用未知候选：%s", key))
      if (key %in% seen) trace_abort(sprintf("参数补全出现重复候选：%s", key))
      status <- as.character(record$status)
      parameters <- v04_named_list(record$parameters)
      if (!status %in% c("proposed", "needs_information")) trace_abort(sprintf("%s 参数状态无效。", key))
      if (identical(status, "needs_information") && length(parameters)) trace_abort(sprintf("%s 信息不足时参数必须为空。", key))
      if (identical(status, "proposed")) merge_parameter_completion_v04(lookup[[key]], parameters) else list(parameters = list(), parameter_sources = list())
    }, error = identity)
    seen <- c(seen, key)
    if (inherits(value, "error")) failures[[length(failures) + 1L]] <- list(task_id = id, candidate_rank = rank, stage = "parameter_completion", error = sanitize_for_log(conditionMessage(value)), record_index = index)
    else valid[[key]] <- c(list(task_id = id, candidate_rank = rank, status = as.character(record$status), uncertainties = recommendation_text(record, "uncertainties")), value)
  }
  expected <- names(Filter(function(x) identical(x$status, "ready") && !isTRUE(x$fully_resolved) && length(x$unresolved_parameters), resolutions))
  missing <- setdiff(expected, seen)
  for (key in missing) failures[[length(failures) + 1L]] <- list(task_id = strsplit(key, "#", fixed = TRUE)[[1]][[1]], candidate_rank = NA_integer_, stage = "parameter_completion", error = "参数阶段遗漏候选。", record_index = NA_integer_)
  list(valid = valid, failures = failures, missing_candidate_keys = missing, stage = "parameter_completion")
}

apply_dependency_blocking_v04 <- function(tasks, available_task_ids) {
  tasks <- stats::setNames(tasks, vapply(tasks, task_id_v04, character(1)))
  available <- intersect(as.character(available_task_ids), names(tasks)); blocked <- list()
  repeat {
    changed <- FALSE
    for (id in intersect(available, names(tasks))) {
      dependencies <- unname(unlist(tasks[[id]]$depends_on %||% character(), use.names = FALSE))
      missing <- setdiff(dependencies, available)
      if (length(missing)) {
        available <- setdiff(available, id); blocked[[id]] <- list(task_id = id, blocked_by = as.list(missing), reason = "dependency_failed"); changed <- TRUE
      }
    }
    if (!changed) break
  }
  list(available_task_ids = available, blocked = blocked)
}

assemble_candidate_plans_v04 <- function(group, target_results, function_results, resolutions,
                                         parameter_results = list(valid = list()), registry) {
  tasks <- v04_group_tasks(group); task_index <- stats::setNames(tasks, vapply(tasks, task_id_v04, character(1)))
  decisions <- target_results$valid %||% target_results
  function_candidates <- function_results$valid %||% function_results
  resolution_index <- stats::setNames(resolutions, vapply(resolutions, function(x) paste(x$task_id, x$candidate_rank, sep = "#"), character(1)))
  completions <- parameter_results$valid %||% list()
  plans <- list(); failures <- list()
  for (key in names(function_candidates)) {
    candidate <- function_candidates[[key]]; resolution <- resolution_index[[key]]
    if (is.null(resolution) || !identical(resolution$status, "ready")) { failures[[length(failures) + 1L]] <- list(task_id = candidate$task_id, candidate_rank = candidate$candidate_rank, stage = "assembly", error = "参数无法确定。"); next }
    if (isTRUE(resolution$fully_resolved)) merged <- list(parameters = resolution$injected_parameters, parameter_sources = resolution$parameter_sources)
    else {
      completion <- completions[[key]]
      if (is.null(completion) || !identical(completion$status, "proposed")) { failures[[length(failures) + 1L]] <- list(task_id = candidate$task_id, candidate_rank = candidate$candidate_rank, stage = "assembly", error = "缺少有效参数补全。"); next }
      merged <- list(parameters = completion$parameters, parameter_sources = completion$parameter_sources)
    }
    task <- task_index[[candidate$task_id]]; decision <- decisions[[candidate$task_id]]; entry <- registry_entry(candidate$transform_id, registry)
    errors <- json_schema_errors(merged$parameters, entry$parameter_schema)
    if (length(errors)) { failures[[length(failures) + 1L]] <- list(task_id = candidate$task_id, candidate_rank = candidate$candidate_rank, stage = "assembly", error = paste("最终参数不符合模式：", paste(errors, collapse = "；"))); next }
    plans[[key]] <- list(
      task_id = candidate$task_id, assembly_group_id = task$assembly_group_id,
      target_domain = task$target_domain, candidate_rank = candidate$candidate_rank,
      semantic_decision = decision,
      steps = list(list(
        step_id = paste0(candidate$task_id, "_STEP_01"), transform_id = candidate$transform_id,
        source_ref_ids = candidate$source_ref_ids, target_variables = decision$target_variables,
        parameters = merged$parameters, parameter_sources = merged$parameter_sources
      )),
      recommendation_score = candidate$score, reason = candidate$reason,
      uncertainties = candidate$uncertainties, status = "proposed", review_required = TRUE
    )
  }
  top_ids <- unique(vapply(Filter(function(x) identical(x$candidate_rank, 1L), plans), function(x) x$task_id, character(1)))
  dependency <- apply_dependency_blocking_v04(tasks, top_ids)
  blocked_ids <- names(dependency$blocked)
  if (length(blocked_ids)) plans <- plans[!vapply(plans, function(x) x$task_id %in% blocked_ids, logical(1))]
  list(plans = plans, failures = failures, dependency_blocks = dependency$blocked, available_task_ids = dependency$available_task_ids)
}

execute_model_stage_v04 <- function(prompt, request_fn, parse_fn, blind = FALSE, max_structure_repairs = 1L) {
  if (!is.function(request_fn) || !is.function(parse_fn)) trace_abort("阶段执行器需要 request_fn 和 parse_fn。")
  max_repairs <- if (isTRUE(blind)) 0L else min(1L, max(0L, as.integer(max_structure_repairs)))
  attempts <- list(); current_prompt <- prompt
  for (attempt in seq_len(max_repairs + 1L)) {
    raw <- request_fn(current_prompt)
    result <- tryCatch({
      parsed <- if (is.character(raw)) jsonlite::fromJSON(extract_json_content(raw), simplifyVector = FALSE) else raw
      parse_fn(parsed)
    }, error = identity)
    attempts[[attempt]] <- list(prompt_sha256 = digest::digest(current_prompt, algo = "sha256"), response_sha256 = digest::digest(raw, algo = "sha256"), valid = !inherits(result, "error"), repair = attempt > 1L)
    if (!inherits(result, "error")) return(list(result = result, attempts = attempts, repaired = attempt > 1L))
    if (attempt > max_repairs) return(list(error = sanitize_for_log(conditionMessage(result)), attempts = attempts, repaired = FALSE))
    current_prompt <- paste(
      "只修复下列响应的 JSON 结构，不改变已经表达的任务、目标、函数、来源或参数选择。",
      "若原响应无法解析，不得借机重新进行专业判断。返回当前阶段要求的 JSON 对象。",
      "结构错误：", sanitize_for_log(conditionMessage(result)),
      "原始请求：", prompt, "原始响应：", as.character(raw), sep = "\n"
    )
  }
}

# Public v0.4 stage interfaces -----------------------------------------------

target_prompt_v04 <- function(group, specification, metadata, policies, dictionary = NULL) {
  target_identification_prompt_v04(group, specification, metadata, policies, dictionary)
}

v04_stage_records <- function(response, key) {
  if (!is.list(response)) trace_abort(sprintf("%s 阶段响应必须是 JSON 对象。", key))
  if (key %in% names(response)) {
    records <- response[[key]]
  } else if (length(response) && all(vapply(response, is.list, logical(1)))) {
    records <- response
  } else {
    trace_abort(sprintf("阶段响应缺少顶层字段 %s。", key))
  }
  if (!is.list(records)) trace_abort(sprintf("%s 必须是 JSON 数组。", key))
  records
}

parse_target_decisions_v04 <- function(response, group, metadata) {
  records <- v04_stage_records(response, "target_identifications")
  parse_target_identifications_v04(records, group, metadata)
}

function_prompt_v04 <- function(group, target_results, registry, specification,
                                policies, dictionary = NULL) {
  function_selection_prompt_v04(
    group, target_results, registry, specification, policies, dictionary
  )
}

parse_function_selections_v04 <- function(response, group, target_results, registry) {
  records <- v04_stage_records(response, "function_candidates")
  parse_function_candidates_v04(records, group, target_results, registry)
}

v04_registry_resolution_metadata <- function(entry) {
  resolution <- entry$parameter_resolution %||% list()
  if (!length(resolution)) return(list())
  if (!is.list(resolution)) trace_abort(sprintf(
    "%s 的 parameter_resolution 必须是对象。", entry$transform_id
  ))
  resolution$parameters %||% resolution
}

v04_apply_registry_resolution <- function(resolution, task, decision, candidate,
                                          entry, policies, resources) {
  declarations <- v04_registry_resolution_metadata(entry)
  if (!length(declarations)) return(resolution)
  required <- unname(unlist(entry$parameter_schema$required %||% character(), use.names = FALSE))
  allowed_sources <- c("policy", "registry", "resource", "derived", "model")
  for (name in names(declarations)) {
    declaration <- declarations[[name]]
    if (!is.list(declaration)) declaration <- list(value = declaration)
    declared_source <- as.character(declaration$source %||% "")
    if (!declared_source %in% allowed_sources) trace_abort(sprintf(
      "%s/%s 的参数 %s 声明了未知来源。", task_id_v04(task), entry$transform_id, name
    ))
    if (!identical(declaration$override, FALSE)) trace_abort(sprintf(
      "%s/%s 的参数 %s 必须声明 override=false。", task_id_v04(task), entry$transform_id, name
    ))
    resolver_id <- as.character(declaration$resolver_id %||% "")
    if (name %in% names(resolution$injected_parameters)) {
      resolution$parameter_sources[[name]] <- list(
        source = declared_source,
        reference = paste0("resolver:", resolver_id)
      )
      next
    }
    value <- declaration$value
    source <- declared_source
    reference <- as.character(declaration$reference %||% paste0("registry:", entry$transform_id, ":", name))
    if (is.null(value) && resolver_id %in% c("target_domain", "target_constant")) {
      value <- task$target_domain
      source <- "derived"
    }
    if (is.null(value) && identical(resolver_id, "target_variable")) {
      targets <- unname(unlist(decision$target_variables %||% character(), use.names = FALSE))
      if (length(targets) == 1L) value <- targets[[1]]
      source <- "derived"
    }
    if (is.null(value)) next
    resolution$injected_parameters[[name]] <- value
    resolution$parameter_sources[[name]] <- list(source = source, reference = reference)
  }
  unresolved <- setdiff(required, names(resolution$injected_parameters))
  options <- v04_parameter_options(entry$parameter_schema)
  finite <- unresolved[vapply(unresolved, function(x) {
    declaration <- declarations[[x]] %||% list()
    isTRUE(declaration$allow_model) &&
      length(options[[x]] %||% list()) > 0L
  }, logical(1))]
  resolution$unresolved_parameters <- finite
  resolution$parameter_options <- options[finite]
  resolution$unavailable_parameters <- setdiff(unresolved, finite)
  resolution$fully_resolved <- !length(unresolved)
  resolution$status <- if (length(resolution$unavailable_parameters)) "needs_information" else "ready"
  resolution
}

resolve_known_parameters_v04 <- function(specification, target_results,
                                         function_results, registry, policies,
                                         resources = list()) {
  tasks <- prepare_tasks_v04(specification)
  task_index <- stats::setNames(tasks, vapply(tasks, task_id_v04, character(1)))
  decisions <- target_results$valid %||% target_results
  candidates <- function_results$valid %||% function_results
  resolutions <- list()
  failures <- list()
  for (key in names(candidates)) {
    candidate <- candidates[[key]]
    task <- task_index[[candidate$task_id]]
    decision <- decisions[[candidate$task_id]]
    value <- tryCatch({
      if (is.null(task) || is.null(decision)) {
        trace_abort(sprintf("%s 缺少通过校验的任务或目标决定。", key))
      }
      resolved <- resolve_parameters_v04(
        task, decision, candidate, specification, registry, policies, resources
      )
      v04_apply_registry_resolution(
        resolved, task, decision, candidate,
        registry_entry(candidate$transform_id, registry), policies, resources
      )
    }, error = identity)
    if (inherits(value, "error")) {
      failures[[length(failures) + 1L]] <- list(
        task_id = candidate$task_id,
        candidate_rank = candidate$candidate_rank,
        stage = "known_parameter_resolution",
        error = sanitize_for_log(conditionMessage(value))
      )
    } else {
      resolutions[[key]] <- value
    }
  }
  list(
    valid = resolutions,
    failures = failures,
    stage = "known_parameter_resolution"
  )
}

parameter_prompt_v04 <- function(resolutions) {
  parameter_completion_prompt_v04(resolutions$valid %||% resolutions)
}

parse_parameter_selections_v04 <- function(response, resolutions) {
  records <- v04_stage_records(response, "parameter_completions")
  parse_parameter_completions_v04(records, resolutions$valid %||% resolutions)
}

assemble_candidates_v04 <- function(specification, target_results,
                                    function_results, resolutions,
                                    parameter_results = list(valid = list()),
                                    registry) {
  tasks <- prepare_tasks_v04(specification)
  task_index <- stats::setNames(tasks, vapply(tasks, task_id_v04, character(1)))
  decisions <- target_results$valid %||% target_results
  candidates <- function_results$valid %||% function_results
  resolution_index <- resolutions$valid %||% resolutions
  completions <- parameter_results$valid %||% list()
  plans <- list()
  failures <- c(
    target_results$failures %||% list(),
    function_results$failures %||% list(),
    resolutions$failures %||% list(),
    parameter_results$failures %||% list()
  )

  for (key in names(candidates)) {
    candidate <- candidates[[key]]
    task <- task_index[[candidate$task_id]]
    decision <- decisions[[candidate$task_id]]
    resolution <- resolution_index[[key]]
    assembled <- tryCatch({
      if (is.null(task) || is.null(decision)) trace_abort("缺少通过校验的任务或目标决定。")
      if (is.null(resolution) || !identical(resolution$status, "ready")) {
        trace_abort("参数无法确定。")
      }
      if (isTRUE(resolution$fully_resolved)) {
        merged <- list(
          parameters = resolution$injected_parameters,
          parameter_sources = resolution$parameter_sources
        )
      } else {
        completion <- completions[[key]]
        if (is.null(completion) || !identical(completion$status, "proposed")) {
          trace_abort("缺少有效参数补全。")
        }
        merged <- list(
          parameters = completion$parameters,
          parameter_sources = completion$parameter_sources
        )
      }
      entry <- registry_entry(candidate$transform_id, registry)
      errors <- json_schema_errors(merged$parameters, entry$parameter_schema)
      if (length(errors)) trace_abort(paste("最终参数不符合模式：", paste(errors, collapse = "；")))
      list(
        task_id = candidate$task_id,
        assembly_group_id = task$assembly_group_id,
        target_domain = task$target_domain,
        candidate_rank = candidate$candidate_rank,
        source_refs = task$source_refs,
        depends_on = task$depends_on,
        required = isTRUE(task$required),
        semantic_decision = decision,
        steps = list(list(
          step_id = paste0(candidate$task_id, "_STEP_01"),
          transform_id = candidate$transform_id,
          source_ref_ids = candidate$source_ref_ids,
          target_variables = decision$target_variables,
          parameters = merged$parameters,
          parameter_sources = merged$parameter_sources
        )),
        recommendation_score = candidate$score,
        reason = candidate$reason,
        uncertainties = candidate$uncertainties,
        status = "proposed",
        review_required = TRUE
      )
    }, error = identity)
    if (inherits(assembled, "error")) {
      failures[[length(failures) + 1L]] <- list(
        task_id = candidate$task_id,
        candidate_rank = candidate$candidate_rank,
        stage = "assembly",
        error = sanitize_for_log(conditionMessage(assembled))
      )
    } else {
      plans[[key]] <- assembled
    }
  }

  top_ids <- unique(vapply(
    Filter(function(x) identical(x$candidate_rank, 1L), plans),
    function(x) x$task_id, character(1)
  ))
  dependency <- apply_dependency_blocking_v04(tasks, top_ids)
  blocked_ids <- names(dependency$blocked)
  if (length(blocked_ids)) {
    plans <- plans[!vapply(plans, function(x) x$task_id %in% blocked_ids, logical(1))]
    failures <- c(failures, unname(dependency$blocked))
  }
  list(
    plans = plans,
    failures = failures,
    dependency_blocks = dependency$blocked,
    available_task_ids = dependency$available_task_ids,
    stage = "assembly"
  )
}

# v0.4 orchestration and persistence -----------------------------------------

load_source_dictionary_v04 <- function(config) {
  profile_dir <- config$paths$profile_dir %||% ""
  if (!nzchar(as.character(profile_dir))) return(NULL)
  path <- trace_path(profile_dir, "source_dictionary.csv")
  if (!file.exists(path)) profile_sources(config)
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
}

recommendation_groups_v04 <- function(specification) {
  tasks <- prepare_tasks_v04(specification)
  domains <- unique(vapply(tasks, function(x) as.character(x$target_domain), character(1)))
  stats::setNames(lapply(domains, function(domain) {
    members <- Filter(function(x) identical(as.character(x$target_domain), domain), tasks)
    list(
      group_id = paste0(tolower(domain), "_atomic_v04"),
      target_domain = domain,
      tasks = members
    )
  }), paste0(tolower(domains), "_atomic_v04"))
}

v04_merge_stage_results <- function(results, stage) {
  valid_parts <- unname(lapply(results, `[[`, "valid"))
  failure_parts <- unname(lapply(results, `[[`, "failures"))
  list(
    valid = do.call(base::c, valid_parts),
    failures = unname(do.call(base::c, failure_parts)),
    stage = stage
  )
}

v04_recommendation_dir <- function(config = NULL, recommendation_dir = NULL) {
  path <- recommendation_dir %||% if (!is.null(config)) trace_path(config$paths$recommendation_dir) else NULL
  if (is.null(path) || !nzchar(as.character(path))) trace_abort("缺少 recommendation_dir。")
  ensure_dir(path)
}

v04_write_stage <- function(value, directory, filename) {
  write_json(value, file.path(directory, filename))
  invisible(value)
}

v04_call_request <- function(request_fn, prompt, stage, group_id) {
  formal_names <- names(formals(request_fn))
  args <- list(prompt)
  if ("stage" %in% formal_names || "..." %in% formal_names) args$stage <- stage
  if ("group_id" %in% formal_names || "..." %in% formal_names) args$group_id <- group_id
  do.call(request_fn, args)
}

v04_default_request_fn <- function(config, directory) {
  api_key <- Sys.getenv("TRACE_SDTM_API_KEY", unset = "")
  base_url <- Sys.getenv("TRACE_SDTM_BASE_URL", unset = "")
  model <- Sys.getenv("TRACE_SDTM_MODEL", unset = "")
  if (!nzchar(api_key) || !nzchar(base_url) || !nzchar(model)) {
    trace_abort("模型路径需要 TRACE_SDTM_API_KEY、TRACE_SDTM_BASE_URL 和 TRACE_SDTM_MODEL。")
  }
  endpoint <- paste0(sub("/$", "", base_url), config$model$endpoint_suffix)
  function(prompt, stage = "stage", group_id = "group") {
    safe <- gsub("[^A-Za-z0-9._-]", "_", paste(group_id, stage, sep = "_"))
    request_json_v02(
      prompt, endpoint, api_key, model, config, paste(group_id, stage),
      file.path(directory, paste0(safe, "_raw_response.json"))
    )$parsed
  }
}

v04_seed_target_records <- function(group, gold, registry) {
  lapply(v04_group_tasks(group), function(task) {
    steps <- gold$plans[[task_id_v04(task)]] %||% list()
    if (length(steps) != 1L) trace_abort(sprintf(
      "%s 的原子金标准必须恰好包含一个步骤。", task_id_v04(task)
    ))
    entry <- registry_entry(steps[[1]]$transform_id, registry)
    mode <- as.character(entry$target_contract$output_mode)
    kind <- if (identical(mode, "dataset")) "dataset" else if (identical(mode, "none")) "none" else "variables"
    list(
      task_id = task_id_v04(task), output_kind = kind,
      target_variables = as.list(unname(unlist(steps[[1]]$target_variables %||% character(), use.names = FALSE))),
      score = 1, evidence = list("专家金标准种子。"),
      uncertainties = list("不是真实模型结果。"), status = "proposed"
    )
  })
}

recommend_targets_v04 <- function(config = NULL, provider = c("model", "seed"),
                                  specification = NULL, metadata = NULL,
                                  policies = NULL, gold = NULL, dictionary = NULL,
                                  request_fn = NULL, blind = FALSE,
                                  recommendation_dir = NULL, registry = NULL) {
  provider <- match.arg(provider)
  if (is.null(config)) config <- load_project_config()
  specification <- specification %||% load_mapping_template(config)
  metadata <- metadata %||% load_metadata(config)
  policies <- policies %||% load_mapping_policies(config)
  registry <- registry %||% load_transform_registry(config)
  dictionary <- dictionary %||% load_source_dictionary_v04(config)
  directory <- v04_recommendation_dir(config, recommendation_dir)
  groups <- recommendation_groups_v04(specification)
  if (identical(provider, "seed")) gold <- gold %||% load_gold_specification(config)
  if (identical(provider, "model") && is.null(request_fn)) request_fn <- v04_default_request_fn(config, directory)
  results <- list()
  attempts <- list()
  prompt_dir <- ensure_dir(file.path(directory, "prompts"))
  for (group in groups) {
    prompt <- target_prompt_v04(group, specification, metadata, policies, dictionary)
    writeLines(enc2utf8(prompt), file.path(prompt_dir, paste0(group$group_id, "_targets.txt")), useBytes = TRUE)
    if (identical(provider, "seed")) {
      records <- v04_seed_target_records(group, gold, registry)
      result <- parse_target_decisions_v04(list(target_identifications = records), group, metadata)
      attempts[[group$group_id]] <- list(provider = "seed", attempt_count = 0L)
    } else {
      execution <- execute_model_stage_v04(
        prompt,
        function(value) v04_call_request(request_fn, value, "targets", group$group_id),
        function(value) parse_target_decisions_v04(value, group, metadata),
        blind = blind
      )
      if (!is.null(execution$error)) {
        result <- list(valid = list(), failures = list(list(
          task_id = "", stage = "target_identification", error = execution$error
        )))
      } else result <- execution$result
      attempts[[group$group_id]] <- execution$attempts
    }
    results[[group$group_id]] <- result
  }
  combined <- v04_merge_stage_results(results, "target_identification")
  combined$provider <- provider
  combined$attempts <- attempts
  v04_write_stage(combined, directory, "target_decisions.json")
  combined
}

v04_group_stage_subset <- function(stage_result, group, candidate_level = FALSE) {
  ids <- vapply(v04_group_tasks(group), task_id_v04, character(1))
  valid <- stage_result$valid %||% stage_result
  keep <- vapply(valid, function(x) as.character(x$task_id) %in% ids, logical(1))
  selected <- valid[keep]
  selected_names <- vapply(selected, function(x) {
    if (isTRUE(candidate_level)) paste(x$task_id, x$candidate_rank, sep = "#") else as.character(x$task_id)
  }, character(1))
  names(selected) <- selected_names
  list(valid = selected, failures = list(), stage = stage_result$stage %||% "")
}

v04_seed_function_records <- function(group, gold) {
  lapply(v04_group_tasks(group), function(task) {
    step <- gold$plans[[task_id_v04(task)]][[1]]
    list(
      task_id = task_id_v04(task), candidate_rank = 1L,
      transform_id = step$transform_id,
      source_ref_ids = as.list(unname(unlist(step$source_ref_ids %||% character(), use.names = FALSE))),
      score = 1, reason = "专家金标准种子。", uncertainties = list("不是真实模型结果。"),
      status = "proposed", review_required = TRUE
    )
  })
}

recommend_functions_v04 <- function(target_results, config = NULL,
                                    provider = c("model", "seed"),
                                    specification = NULL, registry = NULL,
                                    policies = NULL, gold = NULL,
                                    dictionary = NULL, request_fn = NULL,
                                    blind = FALSE, recommendation_dir = NULL) {
  provider <- match.arg(provider)
  if (is.null(config)) config <- load_project_config()
  specification <- specification %||% load_mapping_template(config)
  registry <- registry %||% load_transform_registry(config)
  policies <- policies %||% load_mapping_policies(config)
  dictionary <- dictionary %||% load_source_dictionary_v04(config)
  directory <- v04_recommendation_dir(config, recommendation_dir)
  groups <- recommendation_groups_v04(specification)
  if (identical(provider, "seed")) gold <- gold %||% load_gold_specification(config)
  if (identical(provider, "model") && is.null(request_fn)) request_fn <- v04_default_request_fn(config, directory)
  results <- list(); attempts <- list(); prompt_dir <- ensure_dir(file.path(directory, "prompts"))
  for (group in groups) {
    target_subset <- v04_group_stage_subset(target_results, group)
    if (!length(target_subset$valid)) next
    prompt <- function_prompt_v04(group, target_subset, registry, specification, policies, dictionary)
    writeLines(enc2utf8(prompt), file.path(prompt_dir, paste0(group$group_id, "_functions.txt")), useBytes = TRUE)
    if (identical(provider, "seed")) {
      records <- v04_seed_function_records(group, gold)
      result <- parse_function_selections_v04(list(function_candidates = records), group, target_subset, registry)
      attempts[[group$group_id]] <- list(provider = "seed", attempt_count = 0L)
    } else {
      execution <- execute_model_stage_v04(
        prompt,
        function(value) v04_call_request(request_fn, value, "functions", group$group_id),
        function(value) parse_function_selections_v04(value, group, target_subset, registry),
        blind = blind
      )
      if (!is.null(execution$error)) result <- list(valid = list(), failures = list(list(task_id = "", stage = "function_selection", error = execution$error)))
      else result <- execution$result
      attempts[[group$group_id]] <- execution$attempts
    }
    results[[group$group_id]] <- result
  }
  combined <- v04_merge_stage_results(results, "function_selection")
  combined$provider <- provider; combined$attempts <- attempts
  v04_write_stage(combined, directory, "function_candidates.json")
  combined
}

v04_seed_parameter_records <- function(resolutions, gold) {
  records <- list()
  for (key in names(resolutions$valid)) {
    resolution <- resolutions$valid[[key]]
    if (isTRUE(resolution$fully_resolved)) next
    step <- gold$plans[[resolution$task_id]][[1]]
    missing <- resolution$unresolved_parameters
    parameters <- step$parameters[missing]
    records[[length(records) + 1L]] <- list(
      task_id = resolution$task_id,
      candidate_rank = resolution$candidate_rank,
      parameters = parameters,
      uncertainties = list("不是真实模型结果。"),
      status = "proposed"
    )
  }
  records
}

recommend_parameters_v04 <- function(target_results, function_results,
                                     config = NULL, provider = c("model", "seed"),
                                     specification = NULL, registry = NULL,
                                     policies = NULL, resources = NULL, gold = NULL,
                                     request_fn = NULL, blind = FALSE,
                                     recommendation_dir = NULL) {
  provider <- match.arg(provider)
  if (is.null(config)) config <- load_project_config()
  specification <- specification %||% load_mapping_template(config)
  registry <- registry %||% load_transform_registry(config)
  policies <- policies %||% load_mapping_policies(config)
  resources <- resources %||% list(
    controlled_terminology = load_controlled_terminology(config),
    unit_conversions = load_unit_conversions(config)
  )
  directory <- v04_recommendation_dir(config, recommendation_dir)
  resolutions <- resolve_known_parameters_v04(
    specification, target_results, function_results, registry, policies, resources
  )
  v04_write_stage(resolutions, directory, "parameter_resolutions.json")
  prompt <- parameter_prompt_v04(resolutions)
  if (is.null(prompt)) {
    result <- list(valid = list(), failures = list(), stage = "parameter_completion", skipped = TRUE)
  } else if (identical(provider, "seed")) {
    gold <- gold %||% load_gold_specification(config)
    records <- v04_seed_parameter_records(resolutions, gold)
    result <- parse_parameter_selections_v04(list(parameter_completions = records), resolutions)
    result$valid <- lapply(result$valid, function(item) {
      for (name in names(item$parameter_sources %||% list())) {
        if (identical(item$parameter_sources[[name]]$source, "model")) {
          item$parameter_sources[[name]] <- list(source = "reviewer", reference = "gold_seed")
        }
      }
      item
    })
  } else {
    if (is.null(request_fn)) request_fn <- v04_default_request_fn(config, directory)
    writeLines(enc2utf8(prompt), file.path(ensure_dir(file.path(directory, "prompts")), "parameters.txt"), useBytes = TRUE)
    execution <- execute_model_stage_v04(
      prompt,
      function(value) v04_call_request(request_fn, value, "parameters", "all_tasks"),
      function(value) parse_parameter_selections_v04(value, resolutions),
      blind = blind
    )
    if (!is.null(execution$error)) result <- list(valid = list(), failures = list(list(task_id = "", stage = "parameter_completion", error = execution$error)))
    else result <- execution$result
    result$attempts <- execution$attempts
  }
  result$provider <- provider
  result$resolutions <- resolutions
  v04_write_stage(result, directory, "parameter_completions.json")
  result
}

assemble_recommendations_v04 <- function(target_results, function_results,
                                         parameter_results, config = NULL,
                                         specification = NULL, registry = NULL,
                                         recommendation_dir = NULL) {
  if (is.null(config)) config <- load_project_config()
  specification <- specification %||% load_mapping_template(config)
  registry <- registry %||% load_transform_registry(config)
  resolutions <- parameter_results$resolutions %||% list(valid = list())
  result <- assemble_candidates_v04(
    specification, target_results, function_results, resolutions,
    parameter_results, registry
  )
  directory <- v04_recommendation_dir(config, recommendation_dir)
  v04_write_stage(result, directory, "assembled_recommendations.json")
  result
}

run_recommendation_v04 <- function(config = NULL, provider = c("model", "seed"),
                                   specification = NULL, metadata = NULL,
                                   registry = NULL, policies = NULL,
                                   resources = NULL, gold = NULL,
                                   dictionary = NULL, request_fn = NULL,
                                   blind = FALSE, recommendation_dir = NULL) {
  provider <- match.arg(provider)
  if (is.null(config)) config <- load_project_config()
  specification <- specification %||% load_mapping_template(config)
  metadata <- metadata %||% load_metadata(config)
  registry <- registry %||% load_transform_registry(config)
  policies <- policies %||% load_mapping_policies(config)
  dictionary <- dictionary %||% load_source_dictionary_v04(config)
  if (identical(provider, "seed")) gold <- gold %||% load_gold_specification(config)
  targets <- recommend_targets_v04(
    config = config, provider = provider, specification = specification,
    metadata = metadata, policies = policies, gold = gold,
    dictionary = dictionary, request_fn = request_fn, blind = blind,
    recommendation_dir = recommendation_dir, registry = registry
  )
  functions <- recommend_functions_v04(
    target_results = targets, config = config, provider = provider,
    specification = specification, registry = registry, policies = policies,
    gold = gold, dictionary = dictionary, request_fn = request_fn,
    blind = blind, recommendation_dir = recommendation_dir
  )
  parameters <- recommend_parameters_v04(
    target_results = targets, function_results = functions, config = config,
    provider = provider, specification = specification, registry = registry,
    policies = policies, resources = resources, gold = gold,
    request_fn = request_fn, blind = blind,
    recommendation_dir = recommendation_dir
  )
  assembled <- assemble_recommendations_v04(
    target_results = targets, function_results = functions,
    parameter_results = parameters, config = config,
    specification = specification, registry = registry,
    recommendation_dir = recommendation_dir
  )
  directory <- v04_recommendation_dir(config, recommendation_dir)
  run <- list(
    schema_version = "0.4", provider = provider,
    status = if (length(assembled$failures)) "completed_with_task_failures" else "completed",
    generated_at = utc_now(),
    task_count = length(prepare_tasks_v04(specification)),
    target_valid_count = length(targets$valid),
    function_candidate_count = length(functions$valid),
    assembled_plan_count = length(assembled$plans),
    blind = isTRUE(blind), api_key_logged = FALSE
  )
  v04_write_stage(run, directory, "model_run.json")
  list(
    targets = targets, functions = functions, parameters = parameters,
    assembled = assembled, run = run
  )
}
