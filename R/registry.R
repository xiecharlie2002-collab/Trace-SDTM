load_transform_registry <- function(config = load_project_config(), validate = TRUE) {
  path <- trace_path(config$paths$transform_registry)
  if (!file.exists(path)) trace_abort(sprintf("转换注册表不存在：%s", path))
  registry <- yaml::read_yaml(path)
  if (validate) validate_transform_registry(registry, config)
  registry
}

registry_json <- function(value) {
  jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", na = "null", pretty = FALSE)
}

.trace_registry_cache <- new.env(parent = emptyenv())

json_schema_type <- function(schema) {
  type <- unlist(schema$type %||% character(), use.names = FALSE)
  if (length(type) == 1L) type else character()
}

prepare_value_for_schema <- function(value, schema) {
  type <- json_schema_type(schema)
  if (identical(type, "object") && is.list(value)) {
    if (!length(value) && is.null(names(value))) value <- setNames(list(), character())
    properties <- schema$properties %||% list()
    for (name in intersect(names(value), names(properties))) {
      value[[name]] <- prepare_value_for_schema(value[[name]], properties[[name]])
    }
    return(value)
  }
  if (identical(type, "array")) {
    values <- if (is.null(value)) list() else if (is.list(value) && is.null(names(value))) value else as.list(value)
    return(lapply(values, prepare_value_for_schema, schema = schema$items %||% list()))
  }
  value
}

prepare_json_schema <- function(schema) {
  if (!is.list(schema)) return(schema)
  for (name in names(schema)) {
    value <- schema[[name]]
    if (name %in% c("required", "enum", "allOf", "anyOf", "oneOf")) {
      schema[[name]] <- if (is.list(value) && is.null(names(value))) value else as.list(value)
    } else if (name == "type" && length(value) > 1L) {
      schema[[name]] <- as.list(value)
    } else if (name == "properties" && is.list(value)) {
      schema[[name]] <- lapply(value, prepare_json_schema)
    } else if (name == "items" && is.list(value)) {
      schema[[name]] <- prepare_json_schema(value)
    } else if (is.list(value)) {
      schema[[name]] <- prepare_json_schema(value)
    }
  }
  schema
}

json_schema_errors <- function(value, schema) {
  prepared_schema <- prepare_json_schema(schema)
  prepared_value <- prepare_value_for_schema(value, schema)
  schema_json <- registry_json(prepared_schema)
  cache_key <- paste0("schema_", digest::digest(schema_json, algo = "sha256"))
  if (!exists(cache_key, envir = .trace_registry_cache, inherits = FALSE)) {
    assign(cache_key, jsonvalidate::json_validator(schema_json, engine = "ajv"), envir = .trace_registry_cache)
  }
  validator <- get(cache_key, envir = .trace_registry_cache, inherits = FALSE)
  result <- validator(registry_json(prepared_value), verbose = TRUE, greedy = TRUE)
  if (isTRUE(result)) return(character())
  errors <- attr(result, "errors")
  if (is.null(errors) || !nrow(errors)) return("未提供详细错误。")
  vapply(seq_len(nrow(errors)), function(index) {
    values <- lapply(errors[index, , drop = FALSE], function(value) {
      flattened <- unlist(value, recursive = TRUE, use.names = FALSE)
      paste(flattened[!is.na(flattened)], collapse = ",")
    })
    values <- unlist(values, use.names = FALSE)
    paste(values[nzchar(values)], collapse = "：")
  }, character(1))
}

validate_transform_registry <- function(registry, config = load_project_config()) {
  schema_path <- trace_path(config$paths$transform_registry_schema)
  schema <- jsonlite::read_json(schema_path, simplifyVector = FALSE)
  top_errors <- json_schema_errors(registry, schema)
  if (length(top_errors)) trace_abort(sprintf("转换注册表结构无效：%s", paste(top_errors, collapse = "；")))

  ids <- vapply(registry$transforms, `[[`, character(1), "transform_id")
  if (anyDuplicated(ids)) trace_abort(sprintf("转换注册表存在重复编号：%s", paste(unique(ids[duplicated(ids)]), collapse = ", ")))
  categories <- names(registry$categories)
  used_categories <- vapply(registry$transforms, `[[`, character(1), "category")
  unknown_categories <- setdiff(used_categories, categories)
  if (length(unknown_categories)) trace_abort(sprintf("转换函数使用未知类别：%s", paste(unknown_categories, collapse = ", ")))

  implementations <- names(transform_implementation_bindings())
  implementation_ids <- vapply(registry$transforms, `[[`, character(1), "implementation_id")
  unresolved <- setdiff(implementation_ids, implementations)
  if (length(unresolved)) trace_abort(sprintf("转换函数实现未绑定：%s", paste(unresolved, collapse = ", ")))
  declared_preconditions <- unique(unlist(lapply(registry$transforms, `[[`, "preconditions"), use.names = FALSE))
  unresolved_preconditions <- setdiff(declared_preconditions, names(precondition_check_bindings()))
  if (length(unresolved_preconditions)) trace_abort(sprintf("前置条件检查器未绑定：%s", paste(unresolved_preconditions, collapse = ", ")))

  for (entry in registry$transforms) {
    probe <- tryCatch(
      jsonvalidate::json_validator(registry_json(prepare_json_schema(entry$parameter_schema)), engine = "ajv"),
      error = identity
    )
    if (inherits(probe, "error")) {
      trace_abort(sprintf("%s 的参数模式无效：%s", entry$transform_id, conditionMessage(probe)))
    }
    if (!length(entry$examples)) trace_abort(sprintf("%s 缺少示例。", entry$transform_id))
    parameter_names <- names(entry$parameter_schema$properties %||% list())
    resolution_names <- names(entry$parameter_resolution %||% list())
    if (!setequal(parameter_names, resolution_names)) {
      trace_abort(sprintf(
        "%s 的参数解析元数据与参数模式不一致；缺少=%s，多余=%s。",
        entry$transform_id,
        paste(setdiff(parameter_names, resolution_names), collapse = ", "),
        paste(setdiff(resolution_names, parameter_names), collapse = ", ")
      ))
    }
    if (length(resolution_names)) {
      invalid_override <- vapply(entry$parameter_resolution, function(x) !identical(x$override, FALSE), logical(1))
      if (any(invalid_override)) trace_abort(sprintf("%s 不允许模型覆盖自动注入参数。", entry$transform_id))
      if (exists("parameter_resolver_bindings", mode = "function")) {
        resolver_ids <- vapply(entry$parameter_resolution, function(x) as.character(x$resolver_id %||% ""), character(1))
        unknown_resolvers <- setdiff(resolver_ids, names(parameter_resolver_bindings()))
        if (length(unknown_resolvers)) trace_abort(sprintf(
          "%s 使用未绑定的参数解析器：%s。", entry$transform_id, paste(unknown_resolvers, collapse = ", ")
        ))
      }
    }
    if (isTRUE(entry$model_selectable)) {
      example_parameters <- entry$examples[[1]]$parameters %||% setNames(list(), character())
      positive_errors <- json_schema_errors(example_parameters, entry$parameter_schema)
      if (length(positive_errors)) trace_abort(sprintf("%s 的正例参数不能通过自身模式：%s", entry$transform_id, paste(positive_errors, collapse = "；")))
      if (isFALSE(entry$parameter_schema$additionalProperties %||% TRUE)) {
        negative_parameters <- example_parameters
        negative_parameters$arbitrary_code <- "stop('unsafe')"
        if (!length(json_schema_errors(negative_parameters, entry$parameter_schema))) {
          trace_abort(sprintf("%s 的反例没有被参数模式拒绝。", entry$transform_id))
        }
      }
      if (!length(entry$not_allowed_when)) trace_abort(sprintf("%s 缺少禁用条件。", entry$transform_id))
    }
  }
  invisible(TRUE)
}

precondition_source_exists <- function(context) {
  specification <- context$specification
  for (ref in step_source_refs(context$concept, context$step)) {
    catalog <- specification$source_catalog[[ref$dataset]]
    if (is.null(catalog)) trace_abort(sprintf("%s 引用了未登记数据集 %s。", context$concept$concept_id, ref$dataset))
    if (!isTRUE(catalog$derived)) {
      path <- trace_path(context$config$paths$raw_dir, catalog$file)
      if (!file.exists(path)) trace_abort(sprintf("来源文件不存在：%s", path))
      cache_key <- paste0("header_", digest::digest(normalizePath(path, winslash = "/", mustWork = TRUE), algo = "sha256"))
      if (!exists(cache_key, envir = .trace_registry_cache, inherits = FALSE)) {
        assign(
          cache_key,
          names(readr::read_csv(path, n_max = 0L, show_col_types = FALSE, name_repair = "minimal")),
          envir = .trace_registry_cache
        )
      }
      header <- get(cache_key, envir = .trace_registry_cache, inherits = FALSE)
      if (!ref$variable %in% header) trace_abort(sprintf("来源字段不存在：%s.%s", ref$dataset, ref$variable))
    }
  }
  invisible(TRUE)
}

precondition_codelist_exists <- function(context) {
  id <- context$step$parameters$codelist_id %||% ""
  if (nzchar(id) && is.null(load_controlled_terminology(context$config)$codelists[[id]])) trace_abort(sprintf("未登记术语表：%s", id))
  invisible(TRUE)
}

precondition_hardcoded_term_allowed <- function(context) {
  precondition_codelist_exists(context)
  id <- context$step$parameters$codelist_id
  value <- as.character(context$step$parameters$value)
  allowed <- unlist(load_controlled_terminology(context$config)$codelists[[id]], use.names = FALSE)
  if (!value %in% allowed) trace_abort(sprintf("固定值 %s 不在术语表 %s 中。", value, id))
  invisible(TRUE)
}

precondition_conversion_set_exists <- function(context) {
  id <- context$step$parameters$conversion_set_id %||% ""
  if (nzchar(id) && is.null(load_unit_conversions(context$config)$sets[[id]])) trace_abort(sprintf("未登记单位换算集合：%s", id))
  invisible(TRUE)
}

precondition_visit_map_exists <- function(context) {
  id <- context$step$parameters$visit_map_id %||% ""
  if (nzchar(id) && is.null(load_controlled_terminology(context$config)$visit_maps[[id]])) trace_abort(sprintf("未登记访视表：%s", id))
  invisible(TRUE)
}

precondition_join_metadata <- function(context) {
  parameters <- context$step$parameters
  for (dataset in c(parameters$left_dataset, parameters$right_dataset)) {
    if (is.null(context$specification$source_catalog[[dataset]])) trace_abort(sprintf("连接数据集未登记：%s", dataset))
  }
  invisible(TRUE)
}

precondition_deferred_runtime <- function(context) invisible(TRUE)

precondition_check_bindings <- function() {
  deferred <- c(
    "all_terms_mappable", "all_visits_mappable", "complete_dates_only", "complete_reference_dates",
    "condition_variables_exist", "contains_date_component", "delimiter_present", "dm_reference_exists",
    "findings_dates_complete", "findings_test_registered", "join_cardinality_valid", "no_missing_subject_keys",
    "numeric_result", "record_variables_exist", "source_unit_registered", "source_vectors_same_length",
    "sources_aligned", "target_and_reference_exist", "unambiguous_date_format"
  )
  bindings <- stats::setNames(rep(list(precondition_deferred_runtime), length(deferred)), deferred)
  c(bindings, list(
    source_exists = precondition_source_exists,
    datasets_exist = precondition_join_metadata,
    join_keys_exist = precondition_join_metadata,
    subject_keys_exist = precondition_source_exists,
    codelist_exists = precondition_codelist_exists,
    hardcoded_term_allowed = precondition_hardcoded_term_allowed,
    conversion_set_exists = precondition_conversion_set_exists,
    visit_map_exists = precondition_visit_map_exists
  ))
}

run_step_preconditions <- function(entry, step, concept, specification, metadata, config) {
  context <- list(entry = entry, step = step, concept = concept, specification = specification, metadata = metadata, config = config)
  bindings <- precondition_check_bindings()
  for (id in unlist(entry$preconditions %||% character(), use.names = FALSE)) bindings[[id]](context)
  invisible(TRUE)
}

registry_index <- function(registry = load_transform_registry()) {
  stats::setNames(registry$transforms, vapply(registry$transforms, `[[`, character(1), "transform_id"))
}

registry_entry <- function(transform_id, registry = load_transform_registry()) {
  entry <- registry_index(registry)[[transform_id]]
  if (is.null(entry)) trace_abort(sprintf("未登记的转换函数：%s", transform_id))
  entry
}

registry_model_entries <- function(registry = load_transform_registry(), categories = NULL) {
  entries <- Filter(function(x) isTRUE(x$model_selectable), registry$transforms)
  if (!is.null(categories)) entries <- Filter(function(x) x$category %in% categories, entries)
  entries
}

source_ref_key <- function(ref) paste0(ref$dataset, ".", ref$variable)

concept_source_refs <- function(concept) concept$source_refs %||% list()

step_source_refs <- function(concept, step) {
  refs <- concept_source_refs(concept)
  if (!is.null(step$source_ref_ids)) {
    ids <- unlist(step$source_ref_ids, use.names = FALSE)
    if (!length(ids)) return(list())
    available <- vapply(refs, function(ref) as.character(ref$ref_id %||% ""), character(1))
    missing <- setdiff(ids, available)
    concept_id <- as.character(concept$task_id %||% concept$concept_id %||% "未知任务")
    if (length(missing)) trace_abort(sprintf("%s/%s 引用了任务外来源编号：%s", concept_id, step$transform_id, paste(missing, collapse = ", ")))
    return(refs[match(ids, available)])
  }
  if (is.null(step$source_keys)) return(refs)
  keys <- unlist(step$source_keys, use.names = FALSE)
  if (!length(keys)) return(list())
  available <- vapply(refs, source_ref_key, character(1))
  missing <- setdiff(keys, available)
  concept_id <- as.character(concept$task_id %||% concept$concept_id %||% "未知任务")
  if (length(missing)) trace_abort(sprintf("%s/%s 引用了概念外来源：%s", concept_id, step$transform_id, paste(missing, collapse = ", ")))
  refs[match(keys, available)]
}

step_target_variables <- function(step) unlist(step$target_variables %||% character(), use.names = FALSE)

validate_step_contract <- function(step, concept, specification, metadata, registry, config = load_project_config()) {
  entry <- registry_entry(step$transform_id, registry)
  sources <- step_source_refs(concept, step)
  source_count <- length(sources)
  dataset_count <- length(unique(vapply(sources, function(x) as.character(x$dataset), character(1))))
  contract <- entry$source_contract
  if (source_count < contract$minimum || source_count > contract$maximum) {
    trace_abort(sprintf("%s/%s 的来源字段数 %s 不符合 %s 到 %s。", concept$concept_id, step$transform_id, source_count, contract$minimum, contract$maximum))
  }
  if (dataset_count < contract$minimum_datasets || dataset_count > contract$maximum_datasets) {
    trace_abort(sprintf("%s/%s 的来源数据集数 %s 不符合 %s 到 %s。", concept$concept_id, step$transform_id, dataset_count, contract$minimum_datasets, contract$maximum_datasets))
  }
  if (!concept$target_domain %in% unlist(entry$target_contract$domains, use.names = FALSE)) {
    trace_abort(sprintf("%s 不允许在 %s 域使用 %s。", concept$concept_id, concept$target_domain, step$transform_id))
  }

  targets <- step_target_variables(step)
  output_mode <- entry$target_contract$output_mode
  if (output_mode %in% c("single", "multiple", "records") && !length(targets)) {
    trace_abort(sprintf("%s/%s 缺少目标变量。", concept$concept_id, step$transform_id))
  }
  if (output_mode == "single" && length(targets) != 1L) {
    trace_abort(sprintf("%s/%s 必须且只能生成一个目标变量。", concept$concept_id, step$transform_id))
  }
  if (output_mode %in% c("dataset", "none") && length(targets)) {
    trace_abort(sprintf("%s/%s 输出数据集或无字段输出时 target_variables 必须为空。", concept$concept_id, step$transform_id))
  }
  allowed_variables <- names(metadata$domains[[concept$target_domain]]$variables)
  unknown_targets <- setdiff(targets, allowed_variables)
  if (length(unknown_targets)) trace_abort(sprintf("%s 包含未知目标变量：%s", concept$concept_id, paste(unknown_targets, collapse = ", ")))
  patterns <- unlist(entry$target_contract$patterns, use.names = FALSE)
  if (length(patterns) && length(targets)) {
    valid_target <- vapply(targets, function(target) any(vapply(patterns, grepl, logical(1), x = target)), logical(1))
    if (any(!valid_target)) trace_abort(sprintf("%s/%s 的目标变量不符合注册模式：%s", concept$concept_id, step$transform_id, paste(targets[!valid_target], collapse = ", ")))
  }

  parameters <- step$parameters %||% list()
  parameter_errors <- json_schema_errors(parameters, entry$parameter_schema)
  if (length(parameter_errors)) {
    trace_abort(sprintf("%s/%s 的参数不符合模式：%s", concept$concept_id, step$transform_id, paste(parameter_errors, collapse = "；")))
  }
  run_step_preconditions(entry, step, concept, specification, metadata, config)
  invisible(entry)
}

validate_concept_plan <- function(concept, specification, metadata, registry, config = load_project_config()) {
  if (!nzchar(as.character(concept$concept_id %||% ""))) trace_abort("概念缺少 concept_id。")
  if (!concept$target_domain %in% names(metadata$domains)) trace_abort(sprintf("%s 使用未知域。", concept$concept_id))
  source_catalog <- specification$source_catalog
  for (ref in concept_source_refs(concept)) {
    if (is.null(source_catalog[[ref$dataset]])) trace_abort(sprintf("%s 引用了未知数据集 %s。", concept$concept_id, ref$dataset))
  }
  steps <- concept$steps %||% list()
  if (isTRUE(concept$required) && !length(steps)) trace_abort(sprintf("必需概念 %s 没有转换步骤。", concept$concept_id))
  entries <- lapply(steps, validate_step_contract, concept = concept, specification = specification, metadata = metadata, registry = registry, config = config)
  if (length(entries) > 1L) {
    stage_orders <- vapply(entries, function(entry) registry$categories[[entry$category]]$stage_order, integer(1))
    if (is.unsorted(stage_orders, strictly = FALSE)) trace_abort(sprintf("%s 的转换步骤执行阶段顺序无效。", concept$concept_id))
  }
  invisible(TRUE)
}

validate_specification_compiled <- function(specification, config = load_project_config(), require_approved = FALSE) {
  if (!identical(as.character(specification$schema_version), "compiled")) {
    trace_abort("内部编译规格的 schema_version 必须为 compiled。")
  }
  if (require_approved && !identical(specification$specification$status, "approved")) trace_abort("规格尚未批准。")
  metadata <- load_metadata(config)
  registry <- load_transform_registry(config)
  concepts <- specification$concepts %||% list()
  ids <- vapply(concepts, function(x) as.character(x$concept_id), character(1))
  if (anyDuplicated(ids)) trace_abort(sprintf("规格存在重复概念编号：%s", paste(unique(ids[duplicated(ids)]), collapse = ", ")))
  for (concept in concepts) {
    if (!length(concept$steps %||% list()) && !require_approved) {
      if (!concept$target_domain %in% names(metadata$domains)) trace_abort(sprintf("%s 使用未知域。", concept$concept_id))
      for (ref in concept_source_refs(concept)) {
        if (is.null(specification$source_catalog[[ref$dataset]])) trace_abort(sprintf("%s 引用了未知数据集 %s。", concept$concept_id, ref$dataset))
      }
    } else {
      validate_concept_plan(concept, specification, metadata, registry, config)
    }
  }
  invisible(TRUE)
}

# -----------------------------------------------------------------------------
# 当前任务规格只保存 task/source_ref_id；构建前编译为确定性执行器能够消费的
# 临床概念组。编译结果只存在于内存中，不作为对外兼容格式。

specification_tasks <- function(specification) specification$tasks %||% list()

task_steps <- function(task) {
  task$approved_plan$steps %||% task$steps %||% list()
}

task_ref_index <- function(task) {
  refs <- concept_source_refs(task)
  ids <- vapply(refs, function(ref) as.character(ref$ref_id %||% ""), character(1))
  stats::setNames(refs, ids)
}

resolve_step_sources <- function(step, task) {
  ids <- unlist(step$source_ref_ids %||% character(), use.names = FALSE)
  refs <- task_ref_index(task)
  missing <- setdiff(ids, names(refs))
  if (length(missing)) {
    trace_abort(sprintf("%s/%s 引用了任务外来源编号：%s。", task_identifier(task), step$transform_id, paste(missing, collapse = ", ")))
  }
  step$source_ref_ids <- as.list(ids)
  step$source_keys <- as.list(vapply(refs[ids], source_ref_key, character(1)))
  step$.task_id <- task_identifier(task)
  step
}

topological_task_order <- function(tasks) {
  ids <- vapply(tasks, task_identifier, character(1))
  dependencies <- lapply(tasks, function(task) unlist(task$depends_on %||% character(), use.names = FALSE))
  names(dependencies) <- ids
  unknown <- unique(setdiff(unlist(dependencies, use.names = FALSE), ids))
  if (length(unknown)) trace_abort(sprintf("任务依赖引用未知编号：%s。", paste(unknown, collapse = ", ")))
  remaining <- ids
  ordered <- character()
  while (length(remaining)) {
    ready <- remaining[vapply(remaining, function(id) all(dependencies[[id]] %in% ordered), logical(1))]
    if (!length(ready)) trace_abort(sprintf("任务依赖图存在环：%s。", paste(remaining, collapse = ", ")))
    ordered <- c(ordered, ready)
    remaining <- setdiff(remaining, ready)
  }
  ordered
}

validate_task <- function(task, specification, metadata, registry, config, require_approved = FALSE) {
  id <- task_identifier(task)
  if (!nzchar(id)) trace_abort("任务缺少 task_id。")
  if (!nzchar(as.character(task$assembly_group_id %||% ""))) trace_abort(sprintf("%s 缺少 assembly_group_id。", id))
  if (!task$target_domain %in% names(metadata$domains)) trace_abort(sprintf("%s 使用未知域。", id))
  if (!is.logical(task$required) || length(task$required) != 1L) trace_abort(sprintf("%s 的 required 必须为单个逻辑值。", id))

  refs <- concept_source_refs(task)
  ref_ids <- vapply(refs, function(ref) as.character(ref$ref_id %||% ""), character(1))
  if (any(!nzchar(ref_ids))) trace_abort(sprintf("%s 的每个来源引用都必须有 ref_id。", id))
  if (anyDuplicated(ref_ids)) trace_abort(sprintf("%s 存在重复 ref_id。", id))
  for (ref in refs) {
    if (is.null(specification$source_catalog[[ref$dataset]])) trace_abort(sprintf("%s 引用了未知数据集 %s。", id, ref$dataset))
    if (!nzchar(as.character(ref$variable %||% ""))) trace_abort(sprintf("%s 存在缺少变量名的来源引用。", id))
  }

  steps <- task_steps(task)
  if (require_approved && isTRUE(task$required) && !length(steps)) trace_abort(sprintf("必需任务 %s 没有批准计划。", id))
  if (length(steps)) {
    concept <- task
    concept$concept_id <- id
    concept$steps <- lapply(steps, resolve_step_sources, task = task)
    validate_concept_plan(concept, specification, metadata, registry, config)
    decision <- task$semantic_decision %||% list()
    if (length(decision)) {
      expected_targets <- unlist(decision$target_variables %||% character(), use.names = FALSE)
      actual_targets <- unique(unlist(lapply(concept$steps, step_target_variables), use.names = FALSE))
      if (!setequal(expected_targets, actual_targets)) {
        trace_abort(sprintf("%s 的语义目标集合与批准步骤不一致。", id))
      }
      output_kind <- as.character(decision$output_kind %||% "variables")
      if (output_kind %in% c("dataset", "none") && length(expected_targets)) {
        trace_abort(sprintf("%s 的 %s 输出必须使用空目标变量集合。", id, output_kind))
      }
    }
  }
  invisible(TRUE)
}

validate_task_specification <- function(specification, config = load_project_config(), require_approved = FALSE) {
  if (!identical(as.character(specification$schema_version), "0.6")) {
    trace_abort("任务规格的 schema_version 必须为 0.6。")
  }
  if (require_approved && !identical(specification$specification$status, "approved")) trace_abort("规格尚未批准。")
  tasks <- specification_tasks(specification)
  if (!length(tasks)) trace_abort("任务规格没有原子任务。")
  ids <- vapply(tasks, task_identifier, character(1))
  if (anyDuplicated(ids)) trace_abort(sprintf("规格存在重复任务编号：%s。", paste(unique(ids[duplicated(ids)]), collapse = ", ")))
  topological_task_order(tasks)

  # 同一个 ref_id 在不同任务出现时必须始终指向同一来源字段。
  ref_rows <- purrr::map_dfr(tasks, function(task) purrr::map_dfr(concept_source_refs(task), function(ref) tibble::tibble(
    ref_id = as.character(ref$ref_id), source_key = source_ref_key(ref)
  )))
  if (nrow(ref_rows)) {
    inconsistent <- ref_rows |>
      dplyr::distinct() |>
      dplyr::count(ref_id) |>
      dplyr::filter(.data$n > 1L)
    if (nrow(inconsistent)) trace_abort(sprintf("以下 ref_id 指向多个来源字段：%s。", paste(inconsistent$ref_id, collapse = ", ")))
  }
  metadata <- load_metadata(config)
  registry <- load_transform_registry(config)
  invisible(lapply(tasks, validate_task, specification = specification, metadata = metadata,
                   registry = registry, config = config, require_approved = require_approved))
}

compile_specification <- function(specification, config = load_project_config()) {
  validate_specification(specification, config, require_approved = TRUE)
  tasks <- specification_tasks(specification)
  order <- topological_task_order(tasks)
  task_map <- stats::setNames(tasks, vapply(tasks, task_identifier, character(1)))
  tasks <- unname(task_map[order])
  task_to_group <- stats::setNames(vapply(tasks, function(task) as.character(task$assembly_group_id), character(1)), order)
  group_order <- unique(unname(task_to_group[order]))

  concepts <- lapply(group_order, function(group_id) {
    members <- Filter(function(task) identical(as.character(task$assembly_group_id), group_id), tasks)
    first <- members[[1]]
    refs <- unlist(lapply(members, concept_source_refs), recursive = FALSE)
    if (length(refs)) refs <- refs[!duplicated(vapply(refs, source_ref_key, character(1)))]
    canonical_ref_ids <- if (length(refs)) stats::setNames(
      vapply(refs, function(ref) as.character(ref$ref_id), character(1)),
      vapply(refs, source_ref_key, character(1))
    ) else stats::setNames(character(), character())
    dependencies <- unique(unlist(lapply(members, function(task) {
      ids <- unlist(task$depends_on %||% character(), use.names = FALSE)
      unname(task_to_group[ids])
    }), use.names = FALSE))
    dependencies <- setdiff(dependencies, group_id)
    steps <- unlist(lapply(members, function(task) {
      lapply(task_steps(task), function(step) {
        resolved <- resolve_step_sources(step, task)
        source_keys <- unlist(resolved$source_keys %||% character(), use.names = FALSE)
        if (length(source_keys)) {
          canonical <- unname(canonical_ref_ids[source_keys])
          if (any(is.na(canonical))) trace_abort(sprintf(
            "%s 的步骤无法映射到组合组 %s 的规范来源编号。", task_identifier(task), group_id
          ))
          resolved$source_ref_ids <- as.list(canonical)
        }
        resolved
      })
    }), recursive = FALSE)
    list(
      concept_id = group_id,
      assembly_group_id = group_id,
      task_ids = vapply(members, task_identifier, character(1)),
      target_domain = first$target_domain,
      form_name = first$form_name %||% "",
      source_refs = refs,
      depends_on = dependencies,
      expected_cardinality = first$expected_cardinality %||% "one_record_to_one_record",
      required = any(vapply(members, function(task) isTRUE(task$required), logical(1))),
      review = first$review %||% list(),
      steps = steps
    )
  })

  compiled <- specification
  compiled$schema_version <- "compiled"
  compiled$concepts <- concepts
  compiled$tasks <- NULL
  compiled$specification$compiled_from_schema <- "0.6"
  validate_specification_compiled(compiled, config, require_approved = TRUE)
  compiled
}

validate_specification <- function(specification, config = load_project_config(), require_approved = FALSE) {
  version <- as.character(specification$schema_version %||% "")
  if (identical(version, "0.6")) {
    return(validate_task_specification(specification, config, require_approved))
  }
  trace_abort("当前版本只接受 schema_version 0.6 的任务规格。")
}

registry_catalog_table <- function(registry = load_transform_registry()) {
  purrr::map_dfr(registry$transforms, function(entry) tibble::tibble(
    transform_id = entry$transform_id,
    category = entry$category,
    stage = entry$execution_stage,
    model_selectable = isTRUE(entry$model_selectable),
    provider = paste0(entry$provider$name, "::", entry$provider[["function"]]),
    description = entry$description,
    source_count = sprintf("%s-%s", entry$source_contract$minimum, entry$source_contract$maximum),
    target_patterns = paste(unlist(entry$target_contract$patterns), collapse = " | "),
    required_parameters = paste(unlist(entry$parameter_schema$required %||% character()), collapse = " | "),
    not_allowed_when = paste(unlist(entry$not_allowed_when), collapse = "；")
  ))
}

write_transform_catalog <- function(config = load_project_config()) {
  registry <- load_transform_registry(config)
  table <- registry_catalog_table(registry)
  header <- c(
    "# TraceSDTM 转换函数目录",
    "",
    sprintf("注册表版本：`%s`", registry$registry_version),
    "",
    "本文件由 `registry-docs` 根据转换注册表生成，请勿手工维护。",
    "",
    "| 函数 | 类别 | 阶段 | 模型可选 | 实现 | 说明 | 参数 | 禁用条件 |",
    "|---|---|---|---|---|---|---|---|"
  )
  rows <- apply(table, 1L, function(row) sprintf(
    "| `%s` | %s | %s | %s | `%s` | %s | %s | %s |",
    row[["transform_id"]], row[["category"]], row[["stage"]], row[["model_selectable"]],
    row[["provider"]], gsub("\\|", "\\\\|", row[["description"]]),
    gsub("\\|", "\\\\|", row[["required_parameters"]]), gsub("\\|", "\\\\|", row[["not_allowed_when"]])
  ))
  output <- trace_path(config$paths$generated_transform_docs)
  ensure_parent(output)
  writeLines(c(header, rows), output, useBytes = TRUE)
  trace_info("已生成转换函数目录：%s", output)
  invisible(output)
}
