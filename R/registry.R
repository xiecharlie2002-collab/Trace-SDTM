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
  result <- jsonvalidate::json_validate(
    registry_json(prepared_value),
    registry_json(prepared_schema),
    engine = "ajv",
    verbose = TRUE,
    greedy = TRUE
  )
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
      header <- names(readr::read_csv(path, n_max = 0L, show_col_types = FALSE, name_repair = "minimal"))
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
  if (is.null(step$source_keys)) return(refs)
  keys <- unlist(step$source_keys, use.names = FALSE)
  if (!length(keys)) return(list())
  available <- vapply(refs, source_ref_key, character(1))
  missing <- setdiff(keys, available)
  if (length(missing)) trace_abort(sprintf("%s/%s 引用了概念外来源：%s", concept$concept_id, step$transform_id, paste(missing, collapse = ", ")))
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

validate_specification_v02 <- function(specification, config = load_project_config(), require_approved = FALSE) {
  if (!identical(as.character(specification$schema_version), "0.2")) trace_abort("规格 schema_version 必须为 0.2。")
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
