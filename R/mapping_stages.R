# Three-stage mapping recommendation -----------------------------------------

task_identifier <- function(task) as.character(task$task_id %||% task$concept_id %||% "")

task_source_refs <- function(task) {
  refs <- task$source_refs %||% list()
  if (!length(refs)) return(list())
  task_id <- task_identifier(task)
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

prepare_tasks <- function(specification) {
  tasks <- specification$tasks %||% specification$concepts %||% list()
  result <- lapply(tasks, function(task) {
    id <- task_identifier(task)
    if (!nzchar(id)) trace_abort("任务缺少 task_id。")
    task$task_id <- id
    task$assembly_group_id <- as.character(task$assembly_group_id %||% task$concept_id %||% id)
    task$source_refs <- task_source_refs(task)
    task$depends_on <- as.list(unname(unlist(task$depends_on %||% character(), use.names = FALSE)))
    task
  })
  ids <- vapply(result, task_identifier, character(1))
  if (anyDuplicated(ids)) trace_abort(sprintf("任务规格存在重复任务：%s", paste(unique(ids[duplicated(ids)]), collapse = ", ")))
  known_dependencies <- unique(unlist(lapply(result, function(x) x$depends_on), use.names = FALSE))
  unknown <- setdiff(known_dependencies, ids)
  if (length(unknown)) trace_abort(sprintf("任务规格引用未知依赖：%s", paste(unknown, collapse = ", ")))
  result
}

profile_evidence_columns <- function(dictionary) {
  intersect(
    c(
      "source_dataset", "source_variable", "label", "data_type", "example_values",
      "missing_rate", "unique_count", "form_name", "grain", "format_candidates",
      "partial_tokens", "record_count"
    ),
    names(dictionary)
  )
}

source_profile_context <- function(ref, specification, dictionary) {
  declared_key <- source_ref_key(ref)
  ref_variable <- as.character(ref$variable)
  evidence <- dplyr::filter(
    dictionary,
    paste0(.data$source_dataset, ".", .data$source_variable) == .env$declared_key
  )
  resolution <- "direct"
  if (!nrow(evidence)) {
    catalog <- specification$source_catalog[[ref$dataset]] %||% list()
    parents <- unname(unlist(catalog$profile_parents %||% character(), use.names = FALSE))
    if (isTRUE(catalog$derived) && length(parents)) {
      evidence <- dplyr::filter(
        dictionary, .data$source_dataset %in% .env$parents,
        .data$source_variable == .env$ref_variable
      )
      resolution <- if (nrow(evidence)) "derived_candidates" else "unavailable"
    } else {
      resolution <- "unavailable"
    }
  }
  evidence <- dplyr::select(evidence, dplyr::all_of(profile_evidence_columns(dictionary)))
  list(
    declared_source_key = declared_key,
    role = as.character(ref$role %||% ""),
    is_key = ref_variable %in% unlist(
      specification$source_catalog[[ref$dataset]]$keys %||% character(), use.names = FALSE
    ),
    resolution = resolution,
    evidence = as.data.frame(evidence)
  )
}

task_context <- function(task, specification, dictionary = NULL) {
  refs <- task_source_refs(task)
  profiles <- if (is.null(dictionary)) list() else lapply(refs, source_profile_context, specification = specification, dictionary = dictionary)
  profiles <- lapply(profiles, apply_prompt_privacy)
  list(
    task_id = task_identifier(task), assembly_group_id = task$assembly_group_id,
    target_domain = task$target_domain,
    clinical_action = task$clinical_action %||% task$intent %||% task$description %||% task$form_name,
    form_name = task$form_name, expected_cardinality = task$expected_cardinality,
    required = isTRUE(task$required), depends_on = task$depends_on,
    source_refs = refs, field_profiles = profiles
  )
}

prompt_privacy_mode <- function() {
  tolower(Sys.getenv("TRACE_SDTM_PROMPT_PRIVACY", unset = "metadata_only"))
}

sensitive_profile <- function(profile) {
  if (isTRUE(profile$is_key)) return(TRUE)
  role <- tolower(as.character(profile$role %||% ""))
  key <- tolower(as.character(profile$declared_source_key %||% ""))
  grepl("identifier|subject|site|center|(^|_)key($|_)", role) ||
    grepl("(^|[._])(study|patnum|subjid|usubjid|siteid)([._]|$)", key)
}

profile_selected_for_examples <- function(profile) {
  selected <- trimws(unlist(strsplit(Sys.getenv("TRACE_SDTM_EXAMPLE_SOURCE_KEYS", unset = ""), ",", fixed = TRUE)))
  selected <- selected[nzchar(selected)]
  key <- as.character(profile$declared_source_key %||% "")
  alternate <- gsub("\\.", "__", key)
  length(selected) && (key %in% selected || alternate %in% selected)
}

apply_prompt_privacy <- function(profile) {
  mode <- prompt_privacy_mode()
  if (!mode %in% c("metadata_only", "selected_examples")) return(profile)
  allow_examples <- identical(mode, "selected_examples") &&
    identical(Sys.getenv("TRACE_SDTM_INCLUDE_EXAMPLES", unset = "0"), "1") &&
    profile_selected_for_examples(profile) && !sensitive_profile(profile)
  evidence <- profile$evidence
  if (is.data.frame(evidence)) {
    if ("example_values" %in% names(evidence)) {
      if (allow_examples) evidence$example_values <- as.character(evidence$example_values)
      else evidence$example_values <- NULL
    }
  } else if (is.list(evidence)) {
    evidence <- lapply(evidence, function(item) {
      if (!allow_examples && is.list(item)) item$example_values <- NULL
      item
    })
  }
  profile$evidence <- evidence
  profile$example_values_included <- isTRUE(allow_examples)
  profile
}

group_tasks <- function(group) {
  tasks <- group$tasks %||% group$concepts %||% list()
  lapply(tasks, function(x) { x$source_refs <- task_source_refs(x); x })
}

strict_fields <- function(record, required, allowed, label) {
  if (!is.list(record) || is.null(names(record))) trace_abort(sprintf("%s 必须是 JSON 对象。", label))
  missing <- setdiff(required, names(record))
  extra <- setdiff(names(record), allowed)
  if (length(missing)) trace_abort(sprintf("%s 缺少字段：%s", label, paste(missing, collapse = ", ")))
  if (length(extra)) trace_abort(sprintf("%s 包含本阶段禁止字段：%s", label, paste(extra, collapse = ", ")))
  invisible(TRUE)
}

target_identification_prompt <- function(group, specification, metadata, policies, dictionary = NULL) {
  tasks <- group_tasks(group)
  domain <- as.character(group$target_domain %||% tasks[[1]]$target_domain)
  variables <- metadata$domains[[domain]]$variables
  context <- lapply(tasks, task_context, specification = specification, dictionary = dictionary)
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
  prompt
}

validate_target_identification <- function(record, task, metadata) {
  required <- c("task_id", "output_kind", "target_variables", "score", "evidence", "uncertainties", "status")
  strict_fields(record, required, required, paste0(task_identifier(task), " 目标识别"))
  id <- as.character(record$task_id)
  if (!identical(id, task_identifier(task))) trace_abort(sprintf("目标识别试图改写任务编号：%s", id))
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
    evidence = model_text(record, "evidence"), uncertainties = model_text(record, "uncertainties"),
    status = status, structure_valid = TRUE
  )
}

isolated_parse <- function(records, tasks, validator, stage) {
  lookup <- stats::setNames(tasks, vapply(tasks, task_identifier, character(1)))
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

parse_target_identifications <- function(records, group, metadata) {
  tasks <- group_tasks(group)
  isolated_parse(records, tasks, function(record, task) validate_target_identification(record, task, metadata), "target_identification")
}

normalize_source_type <- function(value) {
  value <- as.character(value %||% "")
  if (value %in% c("integer", "double")) return("numeric")
  if (value %in% c("POSIXt", "POSIXlt")) return("POSIXct")
  value
}

source_ref_type <- function(ref, dictionary = NULL) {
  if (is.null(dictionary) || !is.data.frame(dictionary) || !nrow(dictionary)) return(NA_character_)
  hit <- dictionary[
    as.character(dictionary$source_dataset) == as.character(ref$dataset) &
      as.character(dictionary$source_variable) == as.character(ref$variable),
    , drop = FALSE
  ]
  if (nrow(hit) != 1L) return(NA_character_)
  normalize_source_type(hit$data_type[[1L]])
}

source_profile_row <- function(ref, dictionary = NULL) {
  if (is.null(dictionary) || !is.data.frame(dictionary) || !nrow(dictionary)) return(NULL)
  hit <- dictionary[
    as.character(dictionary$source_dataset) == as.character(ref$dataset) &
      as.character(dictionary$source_variable) == as.character(ref$variable),
    , drop = FALSE
  ]
  if (nrow(hit) == 1L) hit else NULL
}

ref_meets_known_preconditions <- function(entry, ref, dictionary = NULL, policies = NULL) {
  row <- source_profile_row(ref, dictionary)
  if (is.null(row)) return(TRUE)
  preconditions <- unlist(entry$preconditions %||% character(), use.names = FALSE)
  data_type <- normalize_source_type(row$data_type[[1L]])
  if ("numeric_result" %in% preconditions && !identical(data_type, "numeric")) return(FALSE)
  if (any(c("complete_dates_only", "no_missing_subject_keys") %in% preconditions) &&
      as.numeric(row$missing_rate[[1L]] %||% 0) > 0) return(FALSE)
  formats <- trimws(unlist(strsplit(as.character(row$format_candidates[[1L]] %||% ""), "|", fixed = TRUE)))
  formats <- formats[nzchar(formats)]
  if ("contains_date_component" %in% preconditions &&
      !length(formats) && !data_type %in% c("Date", "POSIXct")) return(FALSE)
  if ("unambiguous_date_format" %in% preconditions) {
    approved <- if (is.null(policies)) list() else policies$date_time_formats$approved_sources %||% list()
    records <- Filter(function(item) {
      identical(as.character(item$source$dataset %||% ""), as.character(ref$dataset)) &&
        identical(as.character(item$source$variable %||% ""), as.character(ref$variable))
    }, approved)
    approved_formats <- if (length(records) == 1L) unlist(records[[1L]]$formats %||% character(), use.names = FALSE) else character()
    if (length(formats) != 1L || length(approved_formats) != 1L || !identical(formats[[1L]], approved_formats[[1L]])) return(FALSE)
  }
  TRUE
}

known_resources_compatible <- function(entry, decision, resources = NULL) {
  if (is.null(resources)) return(TRUE)
  preconditions <- unlist(entry$preconditions %||% character(), use.names = FALSE)
  targets <- unlist(decision$target_variables %||% character(), use.names = FALSE)
  if ("codelist_exists" %in% preconditions) {
    codelists <- resources$controlled_terminology$codelists %||% resources$codelists %||% list()
    ids <- vapply(targets, target_codelist, character(1))
    if (!length(ids) || any(!nzchar(ids)) || any(!ids %in% names(codelists))) return(FALSE)
  }
  if ("conversion_set_exists" %in% preconditions) {
    sets <- resources$unit_conversions$sets %||% resources$unit_conversion_sets %||% list()
    if (!length(sets)) return(FALSE)
  }
  if ("visit_map_exists" %in% preconditions) {
    maps <- resources$controlled_terminology$visit_maps %||% resources$visit_maps %||% list()
    if (!length(maps)) return(FALSE)
  }
  TRUE
}

allowed_source_ref_ids <- function(entry, task, dictionary = NULL, policies = NULL) {
  refs <- task_source_refs(task)
  allowed_types <- vapply(unlist(entry$source_contract$types %||% character(), use.names = FALSE), normalize_source_type, character(1))
  if (!length(refs)) return(character())
  keep <- vapply(refs, function(ref) {
    observed <- source_ref_type(ref, dictionary)
    (is.na(observed) || !length(allowed_types) || observed %in% allowed_types) &&
      ref_meets_known_preconditions(entry, ref, dictionary, policies)
  }, logical(1))
  vapply(refs[keep], `[[`, character(1), "ref_id")
}

transform_compatible <- function(entry, task, decision, dictionary = NULL, policies = NULL, resources = NULL) {
  if (!isTRUE(entry$model_selectable)) return(FALSE)
  if (!task$target_domain %in% unname(unlist(entry$target_contract$domains, use.names = FALSE))) return(FALSE)
  mode <- as.character(entry$target_contract$output_mode)
  kind <- decision$output_kind
  if (identical(kind, "dataset") && !identical(mode, "dataset")) return(FALSE)
  if (identical(kind, "none") && !identical(mode, "none")) return(FALSE)
  if (identical(kind, "variables") && mode %in% c("dataset", "none")) return(FALSE)
  targets <- unname(unlist(decision$target_variables, use.names = FALSE))
  target_codelists <- vapply(targets, target_codelist, character(1))
  has_controlled_target <- length(target_codelists) > 0L && any(nzchar(target_codelists))
  if (has_controlled_target && entry$transform_id %in% c("assign_no_ct", "hardcode_no_ct", "normalize_case")) return(FALSE)
  if (!has_controlled_target && entry$transform_id %in% c("assign_ct", "hardcode_ct")) return(FALSE)
  patterns <- unname(unlist(entry$target_contract$patterns %||% character(), use.names = FALSE))
  if (length(targets) && length(patterns) && any(!vapply(targets, function(x) any(vapply(patterns, grepl, logical(1), x = x)), logical(1)))) return(FALSE)
  if (identical(mode, "single") && length(targets) != 1L) return(FALSE)
  refs <- task_source_refs(task)
  if (!known_resources_compatible(entry, decision, resources)) return(FALSE)
  allowed_ids <- allowed_source_ref_ids(entry, task, dictionary, policies)
  eligible <- refs[vapply(refs, function(ref) ref$ref_id %in% allowed_ids, logical(1))]
  contract <- entry$source_contract
  if (length(eligible) < as.integer(contract$minimum)) return(FALSE)
  eligible_datasets <- unique(vapply(eligible, function(ref) as.character(ref$dataset), character(1)))
  if (length(eligible_datasets) < as.integer(contract$minimum_datasets)) return(FALSE)
  TRUE
}

function_card <- function(entry) list(
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

function_task_context <- function(task, specification, dictionary = NULL) {
  context <- task_context(task, specification, dictionary)
  context$field_profiles <- lapply(context$field_profiles, function(profile) list(
    declared_source_key = profile$declared_source_key,
    role = profile$role,
    is_key = isTRUE(profile$is_key),
    resolution = profile$resolution,
    example_values_included = isTRUE(profile$example_values_included),
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
  if (prompt_privacy_mode() %in% c("metadata_only", "selected_examples")) {
    context$field_profiles <- lapply(context$field_profiles, function(profile) {
      if (!isTRUE(profile$example_values_included)) {
        profile$evidence <- lapply(profile$evidence, function(item) {
          item$example_values <- NULL
          item
        })
      }
      profile
    })
  }
  context
}

function_selection_prompt <- function(group, target_results, registry, specification, policies, dictionary = NULL, resources = NULL) {
  tasks <- group_tasks(group)
  task_index <- stats::setNames(tasks, vapply(tasks, task_identifier, character(1)))
  decisions <- target_results$valid %||% target_results
  payload <- lapply(decisions, function(decision) {
    task <- task_index[[decision$task_id]]
    entries <- Filter(function(entry) transform_compatible(entry, task, decision, dictionary, policies, resources), registry$transforms)
    allowed_sources <- stats::setNames(lapply(entries, function(entry) {
      as.list(allowed_source_ref_ids(entry, task, dictionary, policies))
    }), vapply(entries, `[[`, character(1), "transform_id"))
    list(
      target_decision = decision,
      task = function_task_context(task, specification, dictionary),
      allowed_transform_ids = as.list(vapply(entries, `[[`, character(1), "transform_id")),
      allowed_source_ref_ids_by_transform = allowed_sources
    )
  })
  used_ids <- unique(unlist(lapply(payload, `[[`, "allowed_transform_ids"), use.names = FALSE))
  catalog <- lapply(Filter(function(entry) entry$transform_id %in% used_ids, registry$transforms), function_card)
  prompt <- paste(
    "你是临床数据标准映射助手。当前只选择转换函数和来源引用，不填写或推测任何参数。",
    "每项任务可返回一到三个候选，candidate_rank 为 1 到 3且不可重复。",
    "只能使用该任务 allowed_transform_ids 中的 transform_id；来源编号还必须位于 allowed_source_ref_ids_by_transform 对应函数的列表中。不得返回 dataset.variable。",
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
  prompt
}

validate_function_candidate <- function(record, task, decision, registry, dictionary = NULL, policies = NULL, resources = NULL) {
  required <- c("task_id", "candidate_rank", "transform_id", "source_ref_ids", "score", "reason", "uncertainties", "status", "review_required")
  strict_fields(record, required, required, paste0(task_identifier(task), " 函数选择"))
  id <- task_identifier(task)
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
    if (!transform_compatible(entry, task, decision, dictionary, policies, resources)) trace_abort(sprintf("%s 选择的函数 %s 与目标决定、来源画像或已知前置条件不兼容。", id, transform_id))
    refs <- task_source_refs(task)
    index <- stats::setNames(refs, vapply(refs, `[[`, character(1), "ref_id"))
    unknown <- setdiff(source_ids, names(index))
    if (length(unknown)) trace_abort(sprintf("%s 引用了未知来源编号：%s", id, paste(unknown, collapse = ", ")))
    selected <- unname(index[source_ids])
    allowed_source_ids <- allowed_source_ref_ids(entry, task, dictionary, policies)
    incompatible_sources <- setdiff(source_ids, allowed_source_ids)
    if (length(incompatible_sources)) trace_abort(sprintf(
      "%s/%s 的来源字段类型不符合注册表：%s", id, transform_id,
      paste(incompatible_sources, collapse = ", ")
    ))
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
    reason = model_text(record, "reason"), uncertainties = model_text(record, "uncertainties"),
    status = status, review_required = TRUE, structure_valid = TRUE
  )
}

parse_function_candidates <- function(records, group, target_results, registry, dictionary = NULL, policies = NULL, resources = NULL) {
  tasks <- group_tasks(group)
  decisions <- target_results$valid %||% target_results
  lookup <- stats::setNames(tasks, vapply(tasks, task_identifier, character(1)))
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
    value <- tryCatch(validate_function_candidate(record, lookup[[id]], decisions[[id]], registry, dictionary, policies, resources), error = identity)
    if (inherits(value, "error")) failures[[length(failures) + 1L]] <- list(task_id = id, candidate_rank = rank, stage = "function_selection", error = sanitize_for_log(conditionMessage(value)), record_index = index)
    else valid[[key]] <- value
  }
  proposed_ids <- unique(vapply(Filter(function(x) x$candidate_rank == 1L, valid), function(x) x$task_id, character(1)))
  expected <- names(Filter(function(x) identical(x$status, "proposed"), decisions))
  missing <- setdiff(expected, proposed_ids)
  for (id in missing) failures[[length(failures) + 1L]] <- list(task_id = id, candidate_rank = 1L, stage = "function_selection", error = "函数阶段缺少首选候选。", record_index = NA_integer_)
  list(valid = valid, failures = failures, missing_task_ids = missing, stage = "function_selection")
}

parameter_completion_prompt <- function(resolutions) {
  requested <- Filter(function(x) !isTRUE(x$fully_resolved) && length(x$unresolved_parameters), resolutions)
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

parse_parameter_completions <- function(records, resolutions) {
  lookup <- stats::setNames(resolutions, vapply(resolutions, function(x) paste(x$task_id, x$candidate_rank, sep = "#"), character(1)))
  valid <- list(); failures <- list(); seen <- character()
  if (!is.list(records)) records <- list()
  for (index in seq_along(records)) {
    record <- records[[index]]
    required <- c("task_id", "candidate_rank", "parameters", "uncertainties", "status")
    id <- as.character(record$task_id %||% ""); rank <- suppressWarnings(as.integer(record$candidate_rank %||% NA_integer_)); key <- paste(id, rank, sep = "#")
    value <- tryCatch({
      strict_fields(record, required, required, paste0(key, " 参数补全"))
      if (is.null(lookup[[key]])) trace_abort(sprintf("参数补全引用未知候选：%s", key))
      if (key %in% seen) trace_abort(sprintf("参数补全出现重复候选：%s", key))
      status <- as.character(record$status)
      parameters <- named_list(record$parameters)
      if (!status %in% c("proposed", "needs_information")) trace_abort(sprintf("%s 参数状态无效。", key))
      if (identical(status, "needs_information") && length(parameters)) trace_abort(sprintf("%s 信息不足时参数必须为空。", key))
      if (identical(status, "proposed")) merge_parameter_completion(lookup[[key]], parameters) else list(parameters = list(), parameter_sources = list())
    }, error = identity)
    seen <- c(seen, key)
    if (inherits(value, "error")) failures[[length(failures) + 1L]] <- list(task_id = id, candidate_rank = rank, stage = "parameter_completion", error = sanitize_for_log(conditionMessage(value)), record_index = index)
    else valid[[key]] <- c(list(task_id = id, candidate_rank = rank, status = as.character(record$status), uncertainties = model_text(record, "uncertainties")), value)
  }
  expected <- names(Filter(function(x) !isTRUE(x$fully_resolved) && length(x$unresolved_parameters), resolutions))
  missing <- setdiff(expected, seen)
  for (key in missing) failures[[length(failures) + 1L]] <- list(task_id = strsplit(key, "#", fixed = TRUE)[[1]][[1]], candidate_rank = NA_integer_, stage = "parameter_completion", error = "参数阶段遗漏候选。", record_index = NA_integer_)
  list(valid = valid, failures = failures, missing_candidate_keys = missing, stage = "parameter_completion")
}

apply_dependency_blocking <- function(tasks, available_task_ids) {
  tasks <- stats::setNames(tasks, vapply(tasks, task_identifier, character(1)))
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

assemble_candidate_plans <- function(group, target_results, function_results, resolutions,
                                         parameter_results = list(valid = list()), registry) {
  tasks <- group_tasks(group); task_index <- stats::setNames(tasks, vapply(tasks, task_identifier, character(1)))
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
  dependency <- apply_dependency_blocking(tasks, top_ids)
  blocked_ids <- names(dependency$blocked)
  if (length(blocked_ids)) plans <- plans[!vapply(plans, function(x) x$task_id %in% blocked_ids, logical(1))]
  list(plans = plans, failures = failures, dependency_blocks = dependency$blocked, available_task_ids = dependency$available_task_ids)
}

execute_model_stage <- function(prompt, request_fn, parse_fn, max_structure_repairs = 1L) {
  if (!is.function(request_fn) || !is.function(parse_fn)) trace_abort("阶段执行器需要 request_fn 和 parse_fn。")
  max_repairs <- min(1L, max(0L, as.integer(max_structure_repairs)))
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

# Public stage interfaces ----------------------------------------------------

target_prompt <- function(group, specification, metadata, policies, dictionary = NULL) {
  target_identification_prompt(group, specification, metadata, policies, dictionary)
}

stage_records <- function(response, key) {
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

parse_target_decisions <- function(response, group, metadata) {
  records <- stage_records(response, "target_identifications")
  parse_target_identifications(records, group, metadata)
}

function_prompt <- function(group, target_results, registry, specification,
                                policies, dictionary = NULL, resources = NULL) {
  function_selection_prompt(
    group, target_results, registry, specification, policies, dictionary, resources
  )
}

parse_function_selections <- function(response, group, target_results, registry, dictionary = NULL, policies = NULL, resources = NULL) {
  records <- stage_records(response, "function_candidates")
  parse_function_candidates(records, group, target_results, registry, dictionary, policies, resources)
}

registry_resolution_metadata <- function(entry) {
  resolution <- entry$parameter_resolution %||% list()
  if (!length(resolution)) return(list())
  if (!is.list(resolution)) trace_abort(sprintf(
    "%s 的 parameter_resolution 必须是对象。", entry$transform_id
  ))
  resolution$parameters %||% resolution
}

apply_registry_resolution <- function(resolution, task, decision, candidate,
                                          entry, policies, resources) {
  declarations <- registry_resolution_metadata(entry)
  if (!length(declarations)) return(resolution)
  required <- unname(unlist(entry$parameter_schema$required %||% character(), use.names = FALSE))
  allowed_sources <- c("policy", "registry", "resource", "derived", "model")
  for (name in names(declarations)) {
    declaration <- declarations[[name]]
    if (!is.list(declaration)) declaration <- list(value = declaration)
    declared_source <- as.character(declaration$source %||% "")
    if (!declared_source %in% allowed_sources) trace_abort(sprintf(
      "%s/%s 的参数 %s 声明了未知来源。", task_identifier(task), entry$transform_id, name
    ))
    if (!identical(declaration$override, FALSE)) trace_abort(sprintf(
      "%s/%s 的参数 %s 必须声明 override=false。", task_identifier(task), entry$transform_id, name
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
    targets <- unname(unlist(decision$target_variables %||% character(), use.names = FALSE))
    if (is.null(value) && identical(resolver_id, "target_domain")) {
      value <- task$target_domain
      source <- "derived"
    }
    if (is.null(value) && identical(resolver_id, "target_constant") &&
        length(targets) == 1L && identical(targets[[1L]], "DOMAIN")) {
      value <- task$target_domain
      source <- "derived"
    }
    if (is.null(value) && identical(resolver_id, "target_variable")) {
      if (length(targets) == 1L) value <- targets[[1]]
      source <- "derived"
    }
    if (is.null(value)) next
    resolution$injected_parameters[[name]] <- value
    resolution$parameter_sources[[name]] <- list(source = source, reference = reference)
  }
  unresolved <- setdiff(required, names(resolution$injected_parameters))
  options <- parameter_options(entry$parameter_schema)
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

resolve_known_parameters <- function(specification, target_results,
                                         function_results, registry, policies,
                                         resources = list()) {
  tasks <- prepare_tasks(specification)
  task_index <- stats::setNames(tasks, vapply(tasks, task_identifier, character(1)))
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
      resolved <- resolve_parameters(
        task, decision, candidate, specification, registry, policies, resources
      )
      apply_registry_resolution(
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

parameter_prompt <- function(resolutions) {
  parameter_completion_prompt(resolutions$valid %||% resolutions)
}

parse_parameter_selections <- function(response, resolutions) {
  records <- stage_records(response, "parameter_completions")
  parse_parameter_completions(records, resolutions$valid %||% resolutions)
}

assemble_candidates <- function(specification, target_results,
                                    function_results, resolutions,
                                    parameter_results = list(valid = list()),
                                    registry) {
  tasks <- prepare_tasks(specification)
  task_index <- stats::setNames(tasks, vapply(tasks, task_identifier, character(1)))
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
  dependency <- apply_dependency_blocking(tasks, top_ids)
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

# Orchestration and persistence ----------------------------------------------

load_source_dictionary <- function(config) {
  profile_dir <- config$paths$profile_dir %||% ""
  if (!nzchar(as.character(profile_dir))) return(NULL)
  path <- trace_path(profile_dir, "source_dictionary.csv")
  if (!file.exists(path)) profile_sources(config)
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
}

load_recommendation_resources <- function(config) list(
  controlled_terminology = load_controlled_terminology(config),
  unit_conversions = load_unit_conversions(config)
)

recommendation_groups <- function(specification) {
  tasks <- prepare_tasks(specification)
  domains <- unique(vapply(tasks, function(x) as.character(x$target_domain), character(1)))
  stats::setNames(lapply(domains, function(domain) {
    members <- Filter(function(x) identical(as.character(x$target_domain), domain), tasks)
    list(
      group_id = paste0(tolower(domain), "_atomic"),
      target_domain = domain,
      tasks = members
    )
  }), paste0(tolower(domains), "_atomic"))
}

merge_stage_results <- function(results, stage) {
  valid_parts <- unname(lapply(results, `[[`, "valid"))
  failure_parts <- unname(lapply(results, `[[`, "failures"))
  list(
    valid = do.call(base::c, valid_parts),
    failures = unname(do.call(base::c, failure_parts)),
    stage = stage
  )
}

resolve_recommendation_dir <- function(config = NULL, recommendation_dir = NULL) {
  path <- recommendation_dir %||% if (!is.null(config)) trace_path(config$paths$recommendation_dir) else NULL
  if (is.null(path) || !nzchar(as.character(path))) trace_abort("缺少 recommendation_dir。")
  ensure_dir(path)
}

write_stage <- function(value, directory, filename) {
  write_json(value, file.path(directory, filename))
  invisible(value)
}

call_request <- function(request_fn, prompt, stage, group_id) {
  formal_names <- names(formals(request_fn))
  args <- list(prompt)
  if ("stage" %in% formal_names || "..." %in% formal_names) args$stage <- stage
  if ("group_id" %in% formal_names || "..." %in% formal_names) args$group_id <- group_id
  do.call(request_fn, args)
}

default_request_fn <- function(config, directory) {
  config <- apply_model_runtime_overrides(config)
  api_key <- Sys.getenv("TRACE_SDTM_API_KEY", unset = "")
  base_url <- Sys.getenv("TRACE_SDTM_BASE_URL", unset = "")
  model <- Sys.getenv("TRACE_SDTM_MODEL", unset = "")
  if (!nzchar(api_key) || !nzchar(base_url) || !nzchar(model)) {
    trace_abort("模型路径需要 TRACE_SDTM_API_KEY、TRACE_SDTM_BASE_URL 和 TRACE_SDTM_MODEL。")
  }
  endpoint <- paste0(sub("/$", "", base_url), config$model$endpoint_suffix)
  function(prompt, stage = "stage", group_id = "group") {
    safe <- gsub("[^A-Za-z0-9._-]", "_", paste(group_id, stage, sep = "_"))
    request_model_json(
      prompt, endpoint, api_key, model, config, paste(group_id, stage),
      file.path(directory, paste0(safe, "_raw_response.json"))
    )$parsed
  }
}

recommend_targets <- function(config = NULL, specification = NULL, metadata = NULL,
                              policies = NULL, dictionary = NULL, request_fn = NULL,
                              recommendation_dir = NULL, registry = NULL) {
  if (is.null(config)) config <- load_project_config()
  specification <- specification %||% load_task_specification(config)
  metadata <- metadata %||% load_metadata(config)
  policies <- policies %||% load_mapping_policies(config)
  registry <- registry %||% load_transform_registry(config)
  dictionary <- dictionary %||% load_source_dictionary(config)
  directory <- resolve_recommendation_dir(config, recommendation_dir)
  groups <- recommendation_groups(specification)
  if (is.null(request_fn)) request_fn <- default_request_fn(config, directory)
  results <- list()
  attempts <- list()
  prompt_dir <- ensure_dir(file.path(directory, "prompts"))
  for (group in groups) {
    prompt <- target_prompt(group, specification, metadata, policies, dictionary)
    writeLines(enc2utf8(prompt), file.path(prompt_dir, paste0(group$group_id, "_targets.txt")), useBytes = TRUE)
    execution <- execute_model_stage(
      prompt,
      function(value) call_request(request_fn, value, "targets", group$group_id),
      function(value) parse_target_decisions(value, group, metadata)
    )
    if (!is.null(execution$error)) {
      result <- list(valid = list(), failures = list(list(
        task_id = "", stage = "target_identification", error = execution$error
      )))
    } else result <- execution$result
    attempts[[group$group_id]] <- execution$attempts
    results[[group$group_id]] <- result
  }
  combined <- merge_stage_results(results, "target_identification")
  combined$provider <- "model"
  combined$attempts <- attempts
  combined$prompt_version <- "target_selection_v06_1"
  combined$model <- Sys.getenv("TRACE_SDTM_MODEL", unset = "")
  combined$call_count <- sum(vapply(attempts, length, integer(1)))
  combined$failure_reason <- if (length(combined$failures)) paste(vapply(combined$failures, function(item) item$error, character(1)), collapse = "；") else NULL
  write_stage(combined, directory, "target_decisions.json")
  combined
}

group_stage_subset <- function(stage_result, group, candidate_level = FALSE) {
  ids <- vapply(group_tasks(group), task_identifier, character(1))
  valid <- stage_result$valid %||% stage_result
  keep <- vapply(valid, function(x) as.character(x$task_id) %in% ids, logical(1))
  selected <- valid[keep]
  selected_names <- vapply(selected, function(x) {
    if (isTRUE(candidate_level)) paste(x$task_id, x$candidate_rank, sep = "#") else as.character(x$task_id)
  }, character(1))
  names(selected) <- selected_names
  list(valid = selected, failures = list(), stage = stage_result$stage %||% "")
}

recommend_functions <- function(target_results, config = NULL,
                                specification = NULL, registry = NULL,
                                policies = NULL, dictionary = NULL,
                                request_fn = NULL, recommendation_dir = NULL) {
  if (is.null(config)) config <- load_project_config()
  specification <- specification %||% load_task_specification(config)
  registry <- registry %||% load_transform_registry(config)
  policies <- policies %||% load_mapping_policies(config)
  dictionary <- dictionary %||% load_source_dictionary(config)
  resources <- load_recommendation_resources(config)
  directory <- resolve_recommendation_dir(config, recommendation_dir)
  groups <- recommendation_groups(specification)
  if (is.null(request_fn)) request_fn <- default_request_fn(config, directory)
  results <- list(); attempts <- list(); prompt_dir <- ensure_dir(file.path(directory, "prompts"))
  for (group in groups) {
    target_subset <- group_stage_subset(target_results, group)
    if (!length(target_subset$valid)) next
    prompt <- function_prompt(group, target_subset, registry, specification, policies, dictionary, resources)
    writeLines(enc2utf8(prompt), file.path(prompt_dir, paste0(group$group_id, "_functions.txt")), useBytes = TRUE)
    execution <- execute_model_stage(
      prompt,
      function(value) call_request(request_fn, value, "functions", group$group_id),
      function(value) parse_function_selections(value, group, target_subset, registry, dictionary, policies, resources)
    )
    if (!is.null(execution$error)) {
      result <- list(valid = list(), failures = list(list(task_id = "", stage = "function_selection", error = execution$error)))
    } else result <- execution$result
    attempts[[group$group_id]] <- execution$attempts
    results[[group$group_id]] <- result
  }
  combined <- merge_stage_results(results, "function_selection")
  combined$provider <- "model"; combined$attempts <- attempts
  combined$prompt_version <- "function_selection_v06_1"
  combined$model <- Sys.getenv("TRACE_SDTM_MODEL", unset = "")
  combined$call_count <- sum(vapply(attempts, length, integer(1)))
  combined$failure_reason <- if (length(combined$failures)) paste(vapply(combined$failures, function(item) item$error, character(1)), collapse = "；") else NULL
  write_stage(combined, directory, "function_candidates.json")
  combined
}

recommend_parameters <- function(target_results, function_results,
                                 config = NULL, specification = NULL, registry = NULL,
                                 policies = NULL, resources = NULL,
                                 request_fn = NULL, recommendation_dir = NULL) {
  if (is.null(config)) config <- load_project_config()
  specification <- specification %||% load_task_specification(config)
  registry <- registry %||% load_transform_registry(config)
  policies <- policies %||% load_mapping_policies(config)
  resources <- resources %||% list(
    controlled_terminology = load_controlled_terminology(config),
    unit_conversions = load_unit_conversions(config)
  )
  directory <- resolve_recommendation_dir(config, recommendation_dir)
  resolutions <- resolve_known_parameters(
    specification, target_results, function_results, registry, policies, resources
  )
  write_stage(resolutions, directory, "parameter_resolutions.json")
  prompt <- parameter_prompt(resolutions)
  if (is.null(prompt)) {
    result <- list(valid = list(), failures = list(), stage = "parameter_completion", skipped = TRUE)
  } else {
    if (is.null(request_fn)) request_fn <- default_request_fn(config, directory)
    writeLines(enc2utf8(prompt), file.path(ensure_dir(file.path(directory, "prompts")), "parameters.txt"), useBytes = TRUE)
    execution <- execute_model_stage(
      prompt,
      function(value) call_request(request_fn, value, "parameters", "all_tasks"),
      function(value) parse_parameter_selections(value, resolutions)
    )
    if (!is.null(execution$error)) result <- list(valid = list(), failures = list(list(task_id = "", stage = "parameter_completion", error = execution$error)))
    else result <- execution$result
    result$attempts <- execution$attempts
  }
  result$provider <- "model"
  result$resolutions <- resolutions
  result$prompt_version <- "finite_parameter_selection_v06_1"
  result$model <- if (!isTRUE(result$skipped)) Sys.getenv("TRACE_SDTM_MODEL", unset = "") else ""
  result$call_count <- if (isTRUE(result$skipped)) 0L else length(result$attempts %||% list())
  result$failure_reason <- if (length(result$failures %||% list())) paste(vapply(result$failures, function(item) item$error, character(1)), collapse = "；") else NULL
  write_stage(result, directory, "parameter_completions.json")
  result
}

assemble_recommendations <- function(target_results, function_results,
                                         parameter_results, config = NULL,
                                         specification = NULL, registry = NULL,
                                         recommendation_dir = NULL) {
  if (is.null(config)) config <- load_project_config()
  specification <- specification %||% load_task_specification(config)
  registry <- registry %||% load_transform_registry(config)
  resolutions <- parameter_results$resolutions %||% list(valid = list())
  result <- assemble_candidates(
    specification, target_results, function_results, resolutions,
    parameter_results, registry
  )
  directory <- resolve_recommendation_dir(config, recommendation_dir)
  write_stage(result, directory, "assembled_recommendations.json")
  result
}

run_recommendation <- function(config = NULL, specification = NULL, metadata = NULL,
                               registry = NULL, policies = NULL, resources = NULL,
                               dictionary = NULL, request_fn = NULL,
                               recommendation_dir = NULL) {
  if (is.null(config)) config <- load_project_config()
  specification <- specification %||% load_task_specification(config)
  metadata <- metadata %||% load_metadata(config)
  registry <- registry %||% load_transform_registry(config)
  policies <- policies %||% load_mapping_policies(config)
  dictionary <- dictionary %||% load_source_dictionary(config)
  targets <- recommend_targets(
    config = config, specification = specification,
    metadata = metadata, policies = policies,
    dictionary = dictionary, request_fn = request_fn,
    recommendation_dir = recommendation_dir, registry = registry
  )
  functions <- recommend_functions(
    target_results = targets, config = config,
    specification = specification, registry = registry, policies = policies,
    dictionary = dictionary, request_fn = request_fn,
    recommendation_dir = recommendation_dir
  )
  parameters <- recommend_parameters(
    target_results = targets, function_results = functions, config = config,
    specification = specification, registry = registry,
    policies = policies, resources = resources,
    request_fn = request_fn,
    recommendation_dir = recommendation_dir
  )
  assembled <- assemble_recommendations(
    target_results = targets, function_results = functions,
    parameter_results = parameters, config = config,
    specification = specification, registry = registry,
    recommendation_dir = recommendation_dir
  )
  directory <- resolve_recommendation_dir(config, recommendation_dir)
  run <- list(
    schema_version = "0.6", provider = "model",
    status = if (length(assembled$failures)) "completed_with_task_failures" else "completed",
    generated_at = utc_now(),
    task_count = length(prepare_tasks(specification)),
    target_valid_count = length(targets$valid),
    function_candidate_count = length(functions$valid),
    assembled_plan_count = length(assembled$plans), api_key_logged = FALSE
  )
  write_stage(run, directory, "model_run.json")
  list(
    targets = targets, functions = functions, parameters = parameters,
    assembled = assembled, run = run
  )
}
