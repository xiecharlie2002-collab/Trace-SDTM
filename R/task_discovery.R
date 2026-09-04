# TraceSDTM 0.6 task discovery, validation and confirmation -----------------

v06_task_dir <- function(config) ensure_dir(trace_path(config$paths$task_dir))
v06_task_draft_path <- function(config) file.path(v06_task_dir(config), "task_draft.json")
v06_task_validation_path <- function(config) file.path(v06_task_dir(config), "task_validation.json")
v06_task_confirmation_path <- function(config) file.path(v06_task_dir(config), "task_confirmation.json")

v06_allowed_domains <- function(config, metadata = load_metadata(config)) {
  requested <- unlist(config$project$target_domains %||% character(), use.names = FALSE)
  if (length(requested)) intersect(requested, names(metadata$domains)) else names(metadata$domains)
}

task_discovery_prompt_v06 <- function(config) {
  context_path <- trace_path(config$paths$project_context)
  if (!file.exists(context_path)) trace_abort("请先生成数据画像和项目上下文。")
  context <- jsonlite::read_json(context_path, simplifyVector = FALSE)
  paste(
    "你是临床数据标准化任务规划助手。当前阶段只提出原子任务和候选目标域。",
    "禁止选择目标变量、转换函数或函数参数，禁止返回代码、公式、正则表达式或连接规则。",
    "一个来源数据集可以支持多个目标域；一个来源字段可以参与多个原子任务；每项任务只表达一个临床映射动作。",
    "只能使用 context.project.target_domain_scope 中的目标域，以及 context.source_catalog 中真实存在的数据集和字段。",
    "返回 JSON 对象，顶层键 task_drafts。每条任务严格包含 task_id、assembly_group_id、clinical_action、candidate_target_domains、target_domain、source_refs、depends_on、expected_cardinality、required、evidence、uncertainties、status。",
    "candidate_target_domains 可以包含多个候选；target_domain 是供人工确认的首选，并且必须位于候选列表中。",
    "source_refs 中每条严格包含 ref_id、dataset、variable、role。status 只能为 proposed 或 needs_information。",
    "task_id、assembly_group_id 和 ref_id 使用大写字母、数字与下划线；depends_on 只能引用同一响应中的 task_id。",
    "expected_cardinality 只能为 one_record_to_one_record、many_records_to_one_record、one_record_to_many_records、many_records_to_many_records、dataset_level 或 no_output。",
    "需要跨数据集关联但关系画像未确认可执行时，保留来源证据并把 status 设为 needs_information，不得自行创造连接键。",
    "项目上下文：", registry_json(context), sep = "\n"
  )
}

v06_task_records <- function(response) {
  if (is.character(response)) response <- jsonlite::fromJSON(extract_json_content(response), simplifyVector = FALSE)
  records <- response$task_drafts %||% response$tasks %||% response
  if (!is.list(records) || (length(records) && !is.null(names(records)) && !all(grepl("^[0-9]+$", names(records))))) {
    if (!is.list(records) || !all(vapply(records, is.list, logical(1)))) trace_abort("任务发现响应必须包含 task_drafts 数组。")
  }
  if (!length(records) || !all(vapply(records, is.list, logical(1)))) trace_abort("任务发现响应没有任务记录。")
  records
}

v06_normalize_task <- function(task) {
  task$task_id <- as.character(task$task_id %||% "")
  task$assembly_group_id <- as.character(task$assembly_group_id %||% "")
  task$clinical_action <- as.character(task$clinical_action %||% "")
  task$target_domain <- as.character(task$target_domain %||% "")
  task$candidate_target_domains <- as.list(unique(as.character(unlist(
    task$candidate_target_domains %||% task$target_domain, use.names = FALSE
  ))))
  task$source_refs <- lapply(task$source_refs %||% list(), function(ref) list(
    ref_id = as.character(ref$ref_id %||% ""), dataset = as.character(ref$dataset %||% ""),
    variable = as.character(ref$variable %||% ""), role = as.character(ref$role %||% "")
  ))
  task$depends_on <- as.list(unique(as.character(unlist(task$depends_on %||% character(), use.names = FALSE))))
  task$expected_cardinality <- as.character(task$expected_cardinality %||% "")
  task$required <- isTRUE(task$required)
  task$evidence <- as.list(as.character(unlist(task$evidence %||% character(), use.names = FALSE)))
  task$uncertainties <- as.list(as.character(unlist(task$uncertainties %||% character(), use.names = FALSE)))
  task$status <- as.character(task$status %||% "")
  task
}

v06_validation_error <- function(code, message, task_id = "", path = "") {
  list(code = as.character(code), task_id = as.character(task_id), path = as.character(path), message = as.character(message))
}

validate_task_drafts_v06 <- function(tasks, specification, metadata, allowed_domains) {
  tasks <- lapply(tasks %||% list(), v06_normalize_task)
  errors <- list()
  add <- function(code, message, task_id = "", path = "") {
    errors[[length(errors) + 1L]] <<- v06_validation_error(code, message, task_id, path)
  }
  allowed_fields <- c("task_id", "assembly_group_id", "clinical_action", "candidate_target_domains", "target_domain", "source_refs",
                      "depends_on", "expected_cardinality", "required", "evidence", "uncertainties", "status")
  cardinalities <- c("one_record_to_one_record", "many_records_to_one_record", "one_record_to_many_records",
                     "many_records_to_many_records", "dataset_level", "no_output")
  ids <- character()
  for (index in seq_along(tasks)) {
    task <- tasks[[index]]
    id <- task$task_id
    extra <- setdiff(names(task), allowed_fields)
    if (length(extra)) add("extra_task_fields", paste("任务包含禁止字段：", paste(extra, collapse = "、")), id, paste0("tasks[", index, "]"))
    if (!grepl("^[A-Z0-9][A-Z0-9_]{2,80}$", id)) add("invalid_task_id", "任务编号必须为3至81位大写字母、数字或下划线。", id, "task_id")
    if (!grepl("^[A-Z0-9][A-Z0-9_]{2,80}$", task$assembly_group_id)) add("invalid_group_id", "组合组编号格式无效。", id, "assembly_group_id")
    if (!nzchar(trimws(task$clinical_action)) || nchar(task$clinical_action) > 1000L) add("invalid_clinical_action", "临床动作必须为1至1000个字符。", id, "clinical_action")
    candidate_domains <- unlist(task$candidate_target_domains %||% character(), use.names = FALSE)
    if (!length(candidate_domains) || any(!candidate_domains %in% allowed_domains)) add("invalid_candidate_domains", "候选目标域不能为空且必须全部位于允许范围内。", id, "candidate_target_domains")
    if (!task$target_domain %in% allowed_domains) add("unknown_target_domain", sprintf("目标域 %s 不在允许范围内。", task$target_domain), id, "target_domain")
    if (length(candidate_domains) && !task$target_domain %in% candidate_domains) add("target_not_candidate", "首选目标域必须包含在候选目标域中。", id, "target_domain")
    if (!task$expected_cardinality %in% cardinalities) add("invalid_cardinality", "预期记录粒度不在允许列表中。", id, "expected_cardinality")
    if (!task$status %in% c("proposed", "needs_information")) add("invalid_status", "任务状态必须为 proposed 或 needs_information。", id, "status")
    ref_ids <- character()
    for (ref_index in seq_along(task$source_refs)) {
      ref <- task$source_refs[[ref_index]]
      if (!grepl("^[A-Z0-9][A-Z0-9_]{2,100}$", ref$ref_id)) add("invalid_ref_id", "来源编号格式无效。", id, paste0("source_refs[", ref_index, "].ref_id"))
      source <- specification$source_catalog[[ref$dataset]]
      if (is.null(source)) {
        add("unknown_dataset", sprintf("来源数据集 %s 不存在。", ref$dataset), id, paste0("source_refs[", ref_index, "].dataset"))
      } else {
        columns <- unlist(source$columns %||% character(), use.names = FALSE)
        if (!ref$variable %in% columns) add("unknown_variable", sprintf("来源字段 %s.%s 不存在。", ref$dataset, ref$variable), id, paste0("source_refs[", ref_index, "].variable"))
      }
      ref_ids <- c(ref_ids, ref$ref_id)
    }
    if (anyDuplicated(ref_ids)) add("duplicate_ref_id", "同一任务中来源编号重复。", id, "source_refs")
    ids <- c(ids, id)
  }
  duplicated_ids <- unique(ids[duplicated(ids)])
  for (id in duplicated_ids) add("duplicate_task_id", sprintf("任务编号 %s 重复。", id), id, "task_id")
  known <- unique(ids[nzchar(ids)])
  for (task in tasks) {
    unknown <- setdiff(unlist(task$depends_on, use.names = FALSE), known)
    if (length(unknown)) add("unknown_dependency", paste("引用未知依赖：", paste(unknown, collapse = "、")), task$task_id, "depends_on")
    if (task$task_id %in% unlist(task$depends_on, use.names = FALSE)) add("self_dependency", "任务不能依赖自身。", task$task_id, "depends_on")
  }
  if (!length(duplicated_ids)) {
    dependencies <- stats::setNames(lapply(tasks, function(task) intersect(unlist(task$depends_on, use.names = FALSE), known)), ids)
    remaining <- known
    ordered <- character()
    while (length(remaining)) {
      ready <- remaining[vapply(remaining, function(id) all(dependencies[[id]] %in% ordered), logical(1))]
      if (!length(ready)) {
        add("dependency_cycle", paste("任务依赖图存在环：", paste(remaining, collapse = "、")), "", "depends_on")
        break
      }
      ordered <- c(ordered, ready)
      remaining <- setdiff(remaining, ready)
    }
  }
  groups <- split(tasks, vapply(tasks, function(task) task$assembly_group_id, character(1)))
  for (group in groups) {
    domains <- unique(vapply(group, function(task) task$target_domain, character(1)))
    if (length(domains) > 1L) add("mixed_group_domains", "同一组合组中的任务必须属于同一目标域。", group[[1]]$task_id, "assembly_group_id")
  }
  for (domain in unique(vapply(tasks, function(task) task$target_domain, character(1)))) {
    members <- Filter(function(task) identical(task$target_domain, domain), tasks)
    datasets <- unique(unlist(lapply(members, function(task) vapply(task$source_refs, function(ref) ref$dataset, character(1))), use.names = FALSE))
    if (domain %in% allowed_domains && !length(datasets)) add("domain_without_source", sprintf("目标域 %s 没有任何来源数据集。", domain), "", "source_refs")
  }
  ref_map <- list()
  for (task in tasks) for (ref in task$source_refs) {
    key <- paste(ref$dataset, ref$variable, sep = ".")
    if (!is.null(ref_map[[ref$ref_id]]) && !identical(ref_map[[ref$ref_id]], key)) add("inconsistent_ref_id", sprintf("来源编号 %s 指向多个字段。", ref$ref_id), task$task_id, "source_refs")
    ref_map[[ref$ref_id]] <- key
  }
  list(valid = !length(errors), tasks = tasks, errors = errors, checked_at = utc_now())
}

v06_task_repair_prompt <- function(tasks, validation, repair_number) {
  paste(
    "根据程序错误修订任务草案。只修改错误涉及的结构，不增加目标变量、函数、参数、代码、公式、正则表达式或连接键。",
    "必须返回完整 task_drafts 数组，而不是仅返回修改项。",
    sprintf("这是第 %s 次且最多第2次专业修订。", repair_number),
    "程序错误：", registry_json(validation$errors),
    "当前任务草案：", registry_json(tasks),
    sep = "\n"
  )
}

run_task_discovery_v06 <- function(config, request_fn = NULL, max_repairs = 2L) {
  studio_assert_mapping_editable_v06(config)
  specification <- load_mapping_template(config)
  metadata <- load_metadata(config)
  allowed_domains <- v06_allowed_domains(config, metadata)
  if (!length(allowed_domains)) trace_abort("当前标准没有可用目标域。")
  if (is.null(request_fn)) request_fn <- v04_default_request_fn(config, v06_task_dir(config))
  original_prompt <- task_discovery_prompt_v06(config)
  writeLines(enc2utf8(original_prompt), file.path(v06_task_dir(config), "task_discovery_prompt.txt"), useBytes = TRUE)
  prompt <- original_prompt
  attempts <- list()
  tasks <- list()
  validation <- list(valid = FALSE, tasks = list(), errors = list(v06_validation_error("not_run", "尚未执行任务发现。")))
  for (attempt in seq_len(as.integer(max_repairs) + 1L)) {
    raw <- tryCatch(v04_call_request(request_fn, prompt, paste0("task_discovery_attempt_", attempt), "all_sources"), error = identity)
    if (inherits(raw, "error")) stop(raw)
    parsed <- tryCatch(lapply(v06_task_records(raw), v06_normalize_task), error = identity)
    if (inherits(parsed, "error")) {
      tasks <- list()
      validation <- list(valid = FALSE, tasks = list(), errors = list(v06_validation_error("invalid_response", sanitize_for_log(conditionMessage(parsed)))))
    } else {
      tasks <- parsed
      validation <- validate_task_drafts_v06(tasks, specification, metadata, allowed_domains)
    }
    attempts[[attempt]] <- list(
      attempt = attempt, repair = attempt > 1L, model = Sys.getenv("TRACE_SDTM_MODEL", unset = ""),
      prompt_sha256 = digest::digest(prompt, algo = "sha256", serialize = FALSE),
      response_sha256 = digest::digest(registry_json(raw), algo = "sha256", serialize = FALSE),
      valid = isTRUE(validation$valid), error_count = length(validation$errors)
    )
    if (isTRUE(validation$valid) || attempt > as.integer(max_repairs)) break
    prompt <- v06_task_repair_prompt(tasks, validation, attempt)
  }
  status <- if (isTRUE(validation$valid)) "ready_for_confirmation" else "needs_manual_edit"
  draft <- list(
    schema_version = "0.6", status = status, generated_at = utc_now(),
    tasks = validation$tasks, validation = validation, attempts = attempts
  )
  write_json(draft, v06_task_draft_path(config))
  write_json(validation, v06_task_validation_path(config))
  write_json(list(
    schema_version = "0.6", prompt_version = "task_discovery_v06_1",
    model = Sys.getenv("TRACE_SDTM_MODEL", unset = ""), call_count = length(attempts),
    attempts = attempts, failure_reason = if (isTRUE(validation$valid)) NULL else paste(
      vapply(validation$errors, function(error) error$message, character(1)), collapse = "；"
    )
  ), file.path(v06_task_dir(config), "task_discovery_call.json"))
  confirmation <- list(
    schema_version = "0.6", status = "in_confirmation", updated_at = utc_now(),
    tasks = stats::setNames(lapply(validation$tasks, function(task) list(
      task_id = task$task_id, decision = "pending", reviewer = "", confirmed_at = NULL
    )), vapply(validation$tasks, function(task) task$task_id, character(1)))
  )
  write_json(confirmation, v06_task_confirmation_path(config))
  draft
}

read_task_draft_v06 <- function(config) {
  path <- v06_task_draft_path(config)
  if (!file.exists(path)) trace_abort("尚未生成人工智能任务草案。")
  jsonlite::read_json(path, simplifyVector = FALSE)
}

read_task_confirmation_v06 <- function(config) {
  path <- v06_task_confirmation_path(config)
  if (!file.exists(path)) trace_abort("尚未初始化任务确认状态。")
  jsonlite::read_json(path, simplifyVector = FALSE)
}

studio_task_rows_v06 <- function(config) {
  draft <- read_task_draft_v06(config)
  confirmation <- read_task_confirmation_v06(config)
  error_counts <- table(vapply(draft$validation$errors %||% list(), function(error) as.character(error$task_id %||% ""), character(1)))
  purrr::map_dfr(draft$tasks, function(task) tibble::tibble(
    task_id = task$task_id, target_domain = task$target_domain,
    candidate_target_domains = paste(unlist(task$candidate_target_domains), collapse = " | "),
    clinical_action = task$clinical_action,
    sources = paste(vapply(task$source_refs, function(ref) paste(ref$dataset, ref$variable, sep = "."), character(1)), collapse = " | "),
    depends_on = paste(unlist(task$depends_on), collapse = " | "),
    cardinality = task$expected_cardinality, required = isTRUE(task$required),
    model_status = task$status,
    validation_errors = if (task$task_id %in% names(error_counts)) as.integer(error_counts[[task$task_id]]) else 0L,
    decision = confirmation$tasks[[task$task_id]]$decision %||% "pending"
  ))
}

studio_update_task_draft_v06 <- function(config, task_id, clinical_action, target_domain,
                                         source_keys, depends_on, expected_cardinality,
                                         required = TRUE) {
  studio_assert_mapping_editable_v06(config)
  draft <- read_task_draft_v06(config)
  index <- which(vapply(draft$tasks, function(task) identical(task$task_id, task_id), logical(1)))
  if (length(index) != 1L) trace_abort(sprintf("无法唯一定位任务：%s。", task_id))
  source_keys <- unique(as.character(unlist(source_keys %||% character(), use.names = FALSE)))
  refs <- lapply(source_keys, function(key) {
    parts <- strsplit(key, ".", fixed = TRUE)[[1L]]
    if (length(parts) < 2L) trace_abort(sprintf("来源键格式无效：%s。", key))
    dataset <- parts[[1L]]
    variable <- paste(parts[-1L], collapse = ".")
    list(
      ref_id = toupper(gsub("[^A-Za-z0-9]+", "_", paste(dataset, variable, sep = "__"))),
      dataset = dataset, variable = variable, role = v06_role_for_field(variable)
    )
  })
  task <- draft$tasks[[index]]
  task$clinical_action <- trimws(as.character(clinical_action))
  task$target_domain <- as.character(target_domain)
  task$candidate_target_domains <- as.list(unique(c(
    unlist(task$candidate_target_domains %||% character(), use.names = FALSE), task$target_domain
  )))
  task$source_refs <- refs
  task$depends_on <- as.list(unique(as.character(unlist(depends_on %||% character(), use.names = FALSE))))
  task$expected_cardinality <- as.character(expected_cardinality)
  task$required <- isTRUE(required)
  task$status <- "proposed"
  draft$tasks[[index]] <- task
  specification <- load_mapping_template(config)
  validation <- validate_task_drafts_v06(draft$tasks, specification, load_metadata(config), v06_allowed_domains(config))
  draft$tasks <- validation$tasks
  draft$validation <- validation
  draft$status <- if (isTRUE(validation$valid)) "ready_for_confirmation" else "needs_manual_edit"
  draft$updated_at <- utc_now()
  write_json(draft, v06_task_draft_path(config))
  write_json(validation, v06_task_validation_path(config))
  confirmation <- read_task_confirmation_v06(config)
  confirmation$tasks[[task_id]]$decision <- "pending"
  confirmation$tasks[[task_id]]$reviewer <- ""
  confirmation$tasks[[task_id]]$confirmed_at <- NULL
  confirmation$status <- "in_confirmation"
  confirmation$updated_at <- utc_now()
  write_json(confirmation, v06_task_confirmation_path(config))
  validation
}

studio_set_task_decisions_v06 <- function(config, task_ids, decision = c("accept", "exclude"), reviewer) {
  studio_assert_mapping_editable_v06(config)
  decision <- match.arg(decision)
  reviewer <- trimws(as.character(reviewer %||% ""))
  if (!nzchar(reviewer)) trace_abort("确认任务时必须填写审核者标识。")
  draft <- read_task_draft_v06(config)
  state <- read_task_confirmation_v06(config)
  task_ids <- unique(as.character(unlist(task_ids %||% character(), use.names = FALSE)))
  unknown <- setdiff(task_ids, names(state$tasks))
  if (length(unknown)) trace_abort(sprintf("包含未知任务：%s。", paste(unknown, collapse = "、")))
  if (identical(decision, "accept")) {
    invalid <- unique(vapply(draft$validation$errors %||% list(), function(error) as.character(error$task_id %||% ""), character(1)))
    invalid <- invalid[nzchar(invalid)]
    blocked <- intersect(task_ids, invalid)
    if (length(blocked)) trace_abort(sprintf("以下任务仍有结构错误，不能确认：%s。", paste(blocked, collapse = "、")))
    global_errors <- Filter(function(error) !nzchar(as.character(error$task_id %||% "")), draft$validation$errors %||% list())
    if (length(global_errors)) trace_abort("任务草案仍存在全局结构错误，不能确认。")
  }
  for (id in task_ids) {
    state$tasks[[id]]$decision <- decision
    state$tasks[[id]]$reviewer <- reviewer
    state$tasks[[id]]$confirmed_at <- utc_now()
  }
  decisions <- vapply(state$tasks, function(item) item$decision, character(1))
  state$status <- if (all(decisions %in% c("accept", "exclude"))) "ready_to_freeze" else "in_confirmation"
  state$updated_at <- utc_now()
  write_json(state, v06_task_confirmation_path(config))
  if (!is.null(config$studio$project_id)) studio_update_run_stage(
    config$studio$project_id, config$studio$run_id, "task_confirmation", "running", actor = reviewer
  )
  state
}

v06_domain_sources <- function(tasks, specification) {
  domains <- unique(vapply(tasks, function(task) task$target_domain, character(1)))
  stats::setNames(lapply(domains, function(domain) {
    members <- Filter(function(task) identical(task$target_domain, domain), tasks)
    datasets <- unlist(lapply(members, function(task) vapply(task$source_refs, function(ref) ref$dataset, character(1))), use.names = FALSE)
    if (!length(datasets)) trace_abort(sprintf("目标域 %s 没有可作为基础记录的数据集。", domain))
    counts <- table(datasets)
    candidates <- names(counts)[counts == max(counts)]
    rows <- vapply(candidates, function(id) as.integer(specification$source_catalog[[id]]$row_count %||% 0L), integer(1))
    candidates[order(-rows, candidates)][[1L]]
  }), domains)
}

studio_freeze_tasks_v06 <- function(config, reviewer) {
  studio_assert_mapping_editable_v06(config)
  reviewer <- trimws(as.character(reviewer %||% ""))
  if (!nzchar(reviewer)) trace_abort("冻结任务时必须填写审核者标识。")
  draft <- read_task_draft_v06(config)
  confirmation <- read_task_confirmation_v06(config)
  decisions <- vapply(confirmation$tasks, function(item) item$decision, character(1))
  if (any(!decisions %in% c("accept", "exclude"))) trace_abort("仍有未确认任务，不能冻结。")
  accepted_ids <- names(decisions)[decisions == "accept"]
  if (!length(accepted_ids)) trace_abort("至少需要确认一个任务。")
  tasks <- Filter(function(task) task$task_id %in% accepted_ids, draft$tasks)
  specification <- load_mapping_template(config)
  validation <- validate_task_drafts_v06(tasks, specification, load_metadata(config), v06_allowed_domains(config))
  if (!isTRUE(validation$valid)) trace_abort(paste(
    "确认后的任务仍未通过结构校验：",
    paste(vapply(validation$errors, function(error) error$message, character(1)), collapse = "；")
  ))
  specification$tasks <- validation$tasks
  specification$domain_sources <- v06_domain_sources(validation$tasks, specification)
  specification$specification$status <- "frozen"
  specification$specification$expected_task_count <- length(validation$tasks)
  specification$specification$task_confirmation <- list(
    reviewer = reviewer, confirmed_at = utc_now(),
    draft_sha256 = file_sha256(v06_task_draft_path(config)),
    confirmation_sha256 = file_sha256(v06_task_confirmation_path(config))
  )
  tasks_path <- trace_path(config$paths$specification_template)
  write_yaml(specification, tasks_path)
  frozen_path <- file.path(v06_task_dir(config), "frozen_tasks.yml")
  write_yaml(specification, frozen_path)
  confirmation$status <- "frozen"
  confirmation$frozen_at <- utc_now()
  confirmation$frozen_sha256 <- file_sha256(frozen_path)
  write_json(confirmation, v06_task_confirmation_path(config))
  run <- studio_read_run(config$studio$project_id, config$studio$run_id)
  run$configuration_sha256[["tasks.yml"]] <- file_sha256(tasks_path)
  run$stages$task_confirmation <- "completed"
  run$current_stage <- "task_confirmation"
  studio_write_run(config$studio$project_id, config$studio$run_id, run)
  studio_append_audit(config$studio$project_id, "tasks_frozen", list(
    task_count = length(validation$tasks), tasks_sha256 = file_sha256(frozen_path)
  ), config$studio$run_id, reviewer)
  invisible(specification)
}

v06_assert_tasks_frozen <- function(config) {
  specification <- load_mapping_template(config)
  if (!identical(as.character(specification$specification$status %||% ""), "frozen") || !length(specification$tasks %||% list())) {
    trace_abort("请先确认并冻结结构有效的任务。")
  }
  invisible(specification)
}
