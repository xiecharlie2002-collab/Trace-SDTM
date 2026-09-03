# TraceSDTM 0.4 parameter resolution -----------------------------------------
#
# Parameters are resolved from the frozen task, project policy, registry and
# controlled resources.  Gold plans are deliberately not accepted by this API.

v04_named_list <- function(x) {
  if (is.null(x) || !length(x)) return(setNames(list(), character()))
  if (!is.list(x)) x <- as.list(x)
  x
}

v04_parameter_value <- function(value, source, reference = "") {
  list(value = value, source = source, reference = as.character(reference))
}

# 注册表中的 resolver_id 只能从此受控绑定表解析。每个解析器接收已经由
# 项目政策、资源目录和任务上下文计算出的候选值；不存在的值保持未解析，
# 绝不把 resolver_id 当作函数名动态执行。
v04_resolver_from_evidence <- function(parameter_name, evidence, ...) evidence[[parameter_name]]

parameter_resolver_bindings_v04 <- function() {
  ids <- c(
    "target_constant", "target_codelist", "approved_formats", "partial_date_tokens",
    "source_relationship", "source_priority", "conflict_policy", "reference_datetime_policy",
    "field_combination_policy", "identifier_policy", "site_identifier_policy",
    "conditional_policy", "ongoing_policy", "case_policy", "findings_policy",
    "unit_policy", "sequence_policy", "study_day_policy", "visit_policy",
    "baseline_policy", "task_policy"
  )
  stats::setNames(rep(list(v04_resolver_from_evidence), length(ids)), ids)
}

v04_parameter_bindings <- function(task, transform_id, policies) {
  task_id <- task_id_v04(task)
  candidates <- list(
    task$known_parameters,
    task$parameter_policy,
    (policies$parameter_bindings %||% list())[[task_id]],
    (policies$transform_parameter_bindings %||% list())[[transform_id]]
  )
  result <- setNames(list(), character())
  for (candidate in candidates) {
    if (is.null(candidate)) next
    if (!is.list(candidate)) trace_abort(sprintf("%s 的已知参数必须是对象。", task_id))
    result[names(candidate)] <- candidate
  }
  result
}

v04_source_key <- function(ref) paste0(as.character(ref$dataset), ".", as.character(ref$variable))

v04_policy_date_record <- function(ref, policies) {
  key <- v04_source_key(ref)
  approved <- policies$date_time_formats$approved_sources %||% list()
  for (item in approved) {
    source <- item$source %||% list()
    if (identical(paste0(source$dataset, ".", source$variable), key)) return(item)
  }
  legacy <- policies$date_time_formats[[key]]
  if (!is.null(legacy)) return(list(source = ref, formats = legacy))
  NULL
}

v04_resolve_date_parameters <- function(task, selected_refs, transform_id, policies) {
  records <- lapply(selected_refs, v04_policy_date_record, policies = policies)
  if (!length(records) || any(vapply(records, is.null, logical(1)))) return(list())
  formats <- unname(unlist(lapply(records, function(x) x$formats %||% character()), use.names = FALSE))
  resolved <- list(formats = v04_parameter_value(as.list(formats), "policy", "date_time_formats"))
  if (transform_id %in% c("to_iso8601_datetime", "to_iso8601_partial_datetime")) {
    tokens <- unique(c(
      unname(unlist(lapply(records, function(x) x$unknown_tokens %||% character()), use.names = FALSE)),
      unname(unlist(policies$date_time_formats$partial_date_tokens %||% character(), use.names = FALSE))
    ))
    if (length(tokens)) resolved$unknown_tokens <- v04_parameter_value(as.list(tokens), "policy", "date_time_formats.partial_date_tokens")
  }
  resolved
}

v04_identifier_policy <- function(policies, kind) {
  identifiers <- policies$identifiers %||% list()
  if (identical(kind, "usubjid")) identifiers$usubjid %||% identifiers$subject_identifier %||% list()
  else identifiers$site_id %||% identifiers$site_identifier %||% list()
}

v04_target_codelist <- function(target) {
  map <- c(
    AGEU = "AGEU", SEX = "SEX", ETHNIC = "ETHNIC", RACE = "RACE",
    AESEV = "AESEV", AESER = "NY", AESHOSP = "NY", AEREL = "AEREL",
    AEOUT = "AEOUT", AEENRF = "AEENRF", VSPOS = "VSPOS", VSBLFL = "VSBLFL"
  )
  unname(map[[target]] %||% "")
}

v04_findings_policy <- function(task, selected_refs, policies) {
  findings <- policies$unit_standardization$findings %||% list()
  variables <- vapply(selected_refs, function(x) as.character(x$variable), character(1))
  for (item in findings) {
    if (as.character(item$source_variable %||% "") %in% variables) return(item)
  }
  NULL
}

v04_find_conversion <- function(task, selected_refs, policies, resources) {
  conversion_set_id <- as.character(policies$unit_standardization$conversion_set_id %||% "")
  sets <- resources$unit_conversions$sets %||% resources$unit_conversion_sets %||% list()
  if (!nzchar(conversion_set_id) && length(sets) == 1L) conversion_set_id <- names(sets)[[1]]
  if (!nzchar(conversion_set_id) || is.null(sets[[conversion_set_id]])) return(NULL)
  token <- toupper(paste(c(task_id_v04(task), task$assembly_group_id %||% "", vapply(selected_refs, function(x) x$variable, character(1))), collapse = "_"))
  entries <- sets[[conversion_set_id]]
  matches <- Filter(function(x) nzchar(as.character(x$test_code %||% "")) && grepl(as.character(x$test_code), token, fixed = TRUE), entries)
  if (!length(matches)) return(NULL)
  target_units <- unique(vapply(matches, function(x) as.character(x$to_unit), character(1)))
  if (length(target_units) != 1L) return(NULL)
  list(conversion_set_id = conversion_set_id, target_unit = target_units[[1]])
}

v04_merge_parameters <- function(task, selected_refs, specification) {
  datasets <- unique(vapply(selected_refs, function(x) as.character(x$dataset), character(1)))
  if (length(datasets) != 2L) return(list())
  left <- datasets[[1]]
  right <- datasets[[2]]
  left_keys <- unname(unlist(specification$source_catalog[[left]]$keys %||% character(), use.names = FALSE))
  right_keys <- unname(unlist(specification$source_catalog[[right]]$keys %||% character(), use.names = FALSE))
  keys <- intersect(left_keys, right_keys)
  if (!length(keys)) return(list())
  derived <- names(Filter(function(x) {
    parents <- unname(unlist(x$profile_parents %||% character(), use.names = FALSE))
    isTRUE(x$derived) && setequal(parents, datasets)
  }, specification$source_catalog))
  if (length(derived) != 1L) return(list())
  right_vars <- vapply(Filter(function(x) identical(x$dataset, right), selected_refs), function(x) as.character(x$variable), character(1))
  select <- setdiff(right_vars, keys)
  if (!length(select)) return(list())
  list(
    left_dataset = left, right_dataset = right, output_dataset = derived[[1]],
    by = stats::setNames(as.list(keys), keys), relationship = "one-to-one",
    join_type = "left", select = as.list(select), conflict_policy = "error"
  )
}

v04_reference_datetime_parameters <- function(policies) {
  rule <- policies$reference_datetime_rules %||% list()
  if (!length(rule$sources %||% list())) return(list())
  approved <- policies$date_time_formats$approved_sources %||% list()
  formats_for <- function(dataset, variable) {
    matches <- Filter(function(x) identical(as.character(x$source$dataset), dataset) && identical(as.character(x$source$variable), variable), approved)
    if (length(matches)) as.list(unname(unlist(matches[[1]]$formats, use.names = FALSE))) else list()
  }
  sources <- lapply(rule$sources, function(x) list(
    dataset = x$dataset,
    date_variable = x$date_variable,
    time_variable = x$time_variable %||% NULL,
    date_formats = formats_for(x$dataset, x$date_variable),
    time_formats = formats_for(x$dataset, x$time_variable %||% "")
  ))
  selection <- if (grepl("earliest|min", as.character(rule$selection %||% ""), ignore.case = TRUE)) "min" else if (grepl("latest|max", as.character(rule$selection %||% ""), ignore.case = TRUE)) "max" else ""
  subject_keys <- unname(unlist(rule$aggregation$group_by %||% rule$subject_key %||% character(), use.names = FALSE))
  subject_keys <- setdiff(subject_keys, as.character(rule$study_key %||% ""))
  if (!nzchar(selection) || !length(subject_keys) || any(!vapply(sources, function(x) length(x$date_formats) > 0L, logical(1)))) return(list())
  list(selection = selection, subject_keys = as.list(subject_keys), sources = sources)
}

v04_fallback_resolved_parameters <- function(task, decision, candidate, selected_refs, specification, policies, resources) {
  id <- candidate$transform_id
  targets <- unname(unlist(decision$target_variables %||% character(), use.names = FALSE))
  target <- if (length(targets)) targets[[1]] else ""
  value <- switch(id,
    assign_no_ct = list(),
    hardcode_no_ct = if (identical(target, "DOMAIN")) list(value = task$target_domain) else list(),
    assign_ct = { codelist <- v04_target_codelist(target); if (nzchar(codelist)) list(codelist_id = codelist) else list() },
    hardcode_ct = if (identical(target, "AGEU")) list(value = "YEARS", codelist_id = "AGEU") else list(),
    to_iso8601_date = lapply(v04_resolve_date_parameters(task, selected_refs, id, policies), `[[`, "value"),
    to_iso8601_datetime = lapply(v04_resolve_date_parameters(task, selected_refs, id, policies), `[[`, "value"),
    to_iso8601_partial_datetime = lapply(v04_resolve_date_parameters(task, selected_refs, id, policies), `[[`, "value"),
    derive_usubjid = { p <- v04_identifier_policy(policies, "usubjid"); if (nzchar(as.character(p$separator %||% ""))) list(separator = p$separator) else list() },
    extract_delimited_part = {
      p <- v04_identifier_policy(policies, "siteid")
      separator <- p$separator %||% p$delimiter
      position <- p$position %||% p$part_position
      if (!is.null(separator) && !is.null(position)) list(separator = separator, position = as.integer(position)) else list()
    },
    derive_reference_datetime = v04_reference_datetime_parameters(policies),
    merge_sources = v04_merge_parameters(task, selected_refs, specification),
    transpose_findings = {
      p <- v04_findings_policy(task, selected_refs, policies)
      if (is.null(p)) list() else list(test_code = p$test_code, test_name = p$test_name, original_unit = p$original_unit %||% NULL, unit_source = p$unit_source %||% NULL)
    },
    standardize_unit = v04_find_conversion(task, selected_refs, policies, resources) %||% list(),
    derive_sequence = {
      p <- policies$sequence_rules[[task$target_domain]] %||% list()
      if (length(p$record_variables %||% character())) list(record_variables = as.list(unname(unlist(p$record_variables, use.names = FALSE))), start_at = as.integer(p$start_at %||% 1L)) else list()
    },
    derive_study_day = {
      target_dates <- c(AESTDY = "AESTDTC", AEENDY = "AEENDTC", VSDY = "VSDTC")
      date <- unname(target_dates[[target]] %||% "")
      if (nzchar(date)) list(target_date = date, reference_date = "RFSTDTC") else list()
    },
    derive_visitnum = {
      maps <- resources$controlled_terminology$visit_maps %||% resources$visit_maps %||% list()
      if (length(maps) == 1L) list(visit_map_id = names(maps)[[1]]) else list()
    },
    derive_baseline_flag = {
      p <- policies$baseline_rules %||% list()
      if (!is.null(p$reference_field)) list(reference_date = p$reference_field, baseline_visits = as.list(unname(unlist(p$eligible_visits %||% character(), use.names = FALSE))), baseline_timepoints = as.list(unname(unlist(p$baseline_timepoints %||% character(), use.names = FALSE)))) else list()
    },
    derive_ongoing_flag = list(ongoing_value = "ONGOING"),
    list()
  )
  value
}

v04_parameter_options <- function(schema) {
  properties <- schema$properties %||% list()
  lapply(properties, function(property) {
    direct <- unname(unlist(property$enum %||% character(), use.names = FALSE))
    item <- unname(unlist(property$items$enum %||% character(), use.names = FALSE))
    values <- if (length(direct)) direct else item
    if (length(values)) as.list(values) else list()
  })
}

resolve_parameters_v04 <- function(task, decision, candidate, specification, registry,
                                    policies, resources = list()) {
  refs <- task_source_refs_v04(task)
  ref_index <- stats::setNames(refs, vapply(refs, function(x) x$ref_id, character(1)))
  selected_ids <- unname(unlist(candidate$source_ref_ids %||% character(), use.names = FALSE))
  unknown_refs <- setdiff(selected_ids, names(ref_index))
  if (length(unknown_refs)) trace_abort(sprintf("%s 引用了未知来源编号：%s", task_id_v04(task), paste(unknown_refs, collapse = ", ")))
  selected_refs <- unname(ref_index[selected_ids])
  entry <- registry_entry(candidate$transform_id, registry)
  explicit <- v04_parameter_bindings(task, candidate$transform_id, policies)
  fallback_evidence <- v04_fallback_resolved_parameters(task, decision, candidate, selected_refs, specification, policies, resources)
  declarations <- entry$parameter_resolution %||% list()
  bindings <- parameter_resolver_bindings_v04()
  resolver_ids <- vapply(declarations, function(x) as.character(x$resolver_id %||% ""), character(1))
  unknown_resolvers <- setdiff(resolver_ids, names(bindings))
  if (length(unknown_resolvers)) trace_abort(sprintf(
    "%s/%s 使用未绑定的参数解析器：%s", task_id_v04(task), candidate$transform_id,
    paste(unknown_resolvers, collapse = ", ")
  ))
  fallback <- setNames(list(), character())
  for (name in names(declarations)) {
    value <- bindings[[resolver_ids[[name]]]](name, fallback_evidence, task = task, decision = decision,
                                               candidate = candidate, policies = policies, resources = resources)
    if (name %in% names(fallback_evidence)) fallback[name] <- list(value)
  }
  known <- fallback
  known[names(explicit)] <- explicit
  properties <- entry$parameter_schema$properties %||% list()
  unknown_names <- setdiff(names(known), names(properties))
  if (length(unknown_names)) trace_abort(sprintf("%s/%s 的政策注入了未登记参数：%s", task_id_v04(task), candidate$transform_id, paste(unknown_names, collapse = ", ")))

  resolved <- setNames(list(), character())
  provenance <- setNames(list(), character())
  fallback_names <- names(fallback)
  for (name in names(known)) {
    item <- known[[name]]
    if (is.list(item) && all(c("value", "source") %in% names(item))) {
      resolved[name] <- list(item$value)
      provenance[[name]] <- list(source = item$source, reference = item$reference %||% "")
    } else {
      resolved[name] <- list(item)
      source <- if (name %in% names(explicit)) "policy" else if (name %in% fallback_names) "derived" else "registry"
      provenance[[name]] <- list(source = source, reference = paste0("resolver:", candidate$transform_id, ":", name))
    }
  }
  required <- unname(unlist(entry$parameter_schema$required %||% character(), use.names = FALSE))
  unresolved <- setdiff(required, names(resolved))
  options <- v04_parameter_options(entry$parameter_schema)
  finite <- unresolved[vapply(unresolved, function(x) length(options[[x]] %||% list()) > 0L, logical(1))]
  unavailable <- setdiff(unresolved, finite)
  list(
    task_id = task_id_v04(task), candidate_rank = candidate$candidate_rank,
    transform_id = candidate$transform_id, injected_parameters = resolved,
    parameter_sources = provenance, unresolved_parameters = finite,
    parameter_options = options[finite], unavailable_parameters = unavailable,
    fully_resolved = !length(unresolved), status = if (length(unavailable)) "needs_information" else "ready"
  )
}

merge_parameter_completion_v04 <- function(resolution, model_parameters = list()) {
  model_parameters <- v04_named_list(model_parameters)
  overwritten <- intersect(names(model_parameters), names(resolution$injected_parameters))
  if (length(overwritten)) trace_abort(sprintf("%s 的模型参数试图覆盖自动注入值：%s", resolution$task_id, paste(overwritten, collapse = ", ")))
  illegal <- setdiff(names(model_parameters), resolution$unresolved_parameters)
  if (length(illegal)) trace_abort(sprintf("%s 返回了未请求参数：%s", resolution$task_id, paste(illegal, collapse = ", ")))
  missing <- setdiff(resolution$unresolved_parameters, names(model_parameters))
  if (length(missing)) trace_abort(sprintf("%s 仍缺少参数：%s", resolution$task_id, paste(missing, collapse = ", ")))
  for (name in names(model_parameters)) {
    allowed <- unname(unlist(resolution$parameter_options[[name]], use.names = FALSE))
    supplied <- unname(unlist(model_parameters[[name]], use.names = FALSE))
    if (length(allowed) && any(!supplied %in% allowed)) trace_abort(sprintf("%s 的参数 %s 超出有限选项。", resolution$task_id, name))
  }
  parameters <- resolution$injected_parameters
  parameters[names(model_parameters)] <- model_parameters
  sources <- resolution$parameter_sources
  for (name in names(model_parameters)) sources[[name]] <- list(source = "model", reference = "parameter_completion_v04")
  list(parameters = parameters, parameter_sources = sources)
}
