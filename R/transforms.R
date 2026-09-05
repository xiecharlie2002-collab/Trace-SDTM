mapping_sources <- function(mapping) {
  unlist(mapping$source_variables %||% character(), use.names = FALSE)
}

direct_map <- function(raw, mapping) {
  sources <- mapping_sources(mapping)
  if (length(sources) != 1L || !sources %in% names(raw)) {
    trace_abort(sprintf("%s 的 direct_map 必须引用一个有效来源字段。", mapping$mapping_id))
  }
  raw[[sources]]
}

assign_constant <- function(raw, mapping) {
  rep(mapping$parameters$value %||% NA_character_, nrow(raw))
}

map_controlled_term <- function(raw, mapping) {
  source <- mapping_sources(mapping)
  if (length(source) != 1L || !source %in% names(raw)) {
    trace_abort(sprintf("%s 的受控术语映射来源无效。", mapping$mapping_id))
  }
  value <- as.character(raw[[source]])
  terms <- unlist(mapping$parameters$terms %||% list(), use.names = TRUE)
  mapped <- unname(terms[value])
  mapped[is.na(value)] <- NA_character_
  unmatched <- unique(value[!is.na(value) & is.na(mapped)])
  if (length(unmatched)) {
    trace_abort(sprintf(
      "%s 存在未定义的术语：%s",
      mapping$mapping_id,
      paste(unmatched, collapse = ", ")
    ))
  }
  mapped
}

to_iso8601_date <- function(raw, mapping) {
  source <- mapping_sources(mapping)
  if (length(source) != 1L || !source %in% names(raw)) {
    trace_abort(sprintf("%s 的日期映射来源无效。", mapping$mapping_id))
  }
  value <- as.character(raw[[source]])
  formats <- unlist(mapping$parameters$formats %||% c("y-m-d"), use.names = FALSE)
  converted <- as.character(sdtm.oak::create_iso8601(
    value,
    .format = list(formats),
    .warn = FALSE
  ))
  invalid <- !is.na(value) & is.na(converted)
  if (any(invalid)) {
    trace_abort(sprintf(
      "%s 无法转换以下日期：%s",
      mapping$mapping_id,
      paste(unique(value[invalid]), collapse = ", ")
    ))
  }
  converted
}

combine_fields <- function(raw, mapping) {
  sources <- mapping_sources(mapping)
  if (!length(sources) || any(!sources %in% names(raw))) {
    trace_abort(sprintf("%s 的组合字段来源无效。", mapping$mapping_id))
  }
  separator <- mapping$parameters$separator %||% "-"
  source_data <- lapply(sources, function(source) as.character(raw[[source]]))
  missing <- Reduce(`|`, lapply(source_data, is.na))
  result <- do.call(paste, c(source_data, sep = separator))
  result[missing] <- NA_character_
  result
}

derive_usubjid <- function(raw, mapping) {
  combine_fields(raw, mapping)
}

conditional_map <- function(raw, mapping) {
  parameters <- mapping$parameters
  condition_variable <- parameters$condition_variable %||% ""
  if (!condition_variable %in% names(raw)) trace_abort(sprintf("%s 的条件字段不存在。", mapping$mapping_id))
  result <- rep(NA_character_, nrow(raw))
  result[as.character(raw[[condition_variable]]) == as.character(parameters$equals)] <- as.character(parameters$value)
  result
}

derive_visitnum <- function(raw, mapping) {
  source <- mapping_sources(mapping)
  value <- as.character(raw[[source]])
  terms <- unlist(mapping$parameters$terms %||% list(), use.names = TRUE)
  mapped <- suppressWarnings(as.numeric(unname(terms[value])))
  unmatched <- unique(value[!is.na(value) & is.na(mapped)])
  if (length(unmatched)) trace_abort(sprintf("%s 存在未定义访视：%s", mapping$mapping_id, paste(unmatched, collapse = ", ")))
  mapped
}

custom_transform <- function(raw, mapping) {
  source <- mapping_sources(mapping)
  if (length(source) != 1L || !source %in% names(raw)) trace_abort(sprintf("%s 的自定义转换来源无效。", mapping$mapping_id))
  name <- mapping$parameters$name %||% ""
  value <- raw[[source]]
  if (identical(name, "uppercase")) return(toupper(as.character(value)))
  if (identical(name, "extract_siteid")) return(sub("-.*$", "", as.character(value)))
  if (identical(name, "ongoing_if_missing")) {
    missing <- is.na(value) | !nzchar(trimws(as.character(value)))
    return(ifelse(missing, "ONGOING", NA_character_))
  }
  trace_abort(sprintf("%s 请求了未登记的自定义转换：%s", mapping$mapping_id, name))
}

execute_mapping <- function(raw, mapping) {
  transform <- mapping$transform_id
  functions <- list(
    direct_map = direct_map,
    assign_constant = assign_constant,
    map_controlled_term = map_controlled_term,
    to_iso8601_date = to_iso8601_date,
    combine_fields = combine_fields,
    derive_usubjid = derive_usubjid,
    conditional_map = conditional_map,
    derive_visitnum = derive_visitnum,
    custom_transform = custom_transform
  )
  function_to_call <- functions[[transform]]
  if (is.null(function_to_call)) trace_abort(sprintf("%s 使用了未登记的转换函数：%s", mapping$mapping_id, transform))
  function_to_call(raw, mapping)
}

derive_sequence <- function(data, mapping) {
  record_variables <- unlist(mapping$parameters$record_variables, use.names = FALSE)
  missing <- setdiff(record_variables, names(data))
  if (length(missing)) trace_abort(sprintf("%s 派生序号时缺少字段：%s", mapping$mapping_id, paste(missing, collapse = ", ")))
  sdtm.oak::derive_seq(
    tgt_dat = data,
    tgt_var = mapping$target_variable,
    rec_vars = record_variables,
    sbj_vars = c("STUDYID", "USUBJID")
  )
}

# 登记转换函数执行器 ----------------------------------------------------------

load_controlled_terminology <- function(config = load_project_config()) {
  yaml::read_yaml(trace_path(config$paths$controlled_terminology))
}

load_unit_conversions <- function(config = load_project_config()) {
  yaml::read_yaml(trace_path(config$paths$unit_conversions))
}

step_sources <- function(state, concept, step = list()) {
  refs <- step_source_refs(concept, step)
  lapply(refs, function(ref) {
    data <- state$sources[[ref$dataset]]
    if (is.null(data)) trace_abort(sprintf("%s 缺少来源数据集 %s。", concept$concept_id, ref$dataset))
    if (!ref$variable %in% names(data)) trace_abort(sprintf("%s 缺少来源字段 %s.%s。", concept$concept_id, ref$dataset, ref$variable))
    data[[ref$variable]]
  })
}

step_source_names <- function(concept, step = list()) vapply(step_source_refs(concept, step), function(ref) as.character(ref$variable), character(1))

set_step_target <- function(state, step, value) {
  targets <- step_target_variables(step)
  if (length(targets) != 1L) trace_abort(sprintf("%s 必须指定一个目标变量。", step$transform_id))
  if (length(value) == 1L && nrow(state$target) != 1L) value <- rep(value, nrow(state$target))
  if (length(value) != nrow(state$target)) trace_abort(sprintf("%s 输出长度与目标数据不一致。", step$transform_id))
  state$target[[targets]] <- value
  state
}

oak_scaffold <- function(value, name = "RAW") {
  tibble::tibble(
    oak_id = seq_along(value),
    raw_source = "TraceSDTM",
    patient_number = as.character(seq_along(value)),
    !!name := value
  )
}

ct_spec_from_config <- function(codelist_id, config = load_project_config()) {
  ct <- load_controlled_terminology(config)$codelists[[codelist_id]]
  if (is.null(ct)) trace_abort(sprintf("未登记受控术语表：%s", codelist_id))
  collected <- names(ct)
  values <- unlist(ct, use.names = FALSE)
  tibble::tibble(
    codelist_code = codelist_id,
    term_code = paste0(codelist_id, "_", seq_along(values)),
    term_value = values,
    collected_value = collected,
    term_preferred_term = values,
    term_synonyms = ""
  )
}

step_assign_no_ct <- function(state, concept, step, config) {
  value <- step_sources(state, concept, step)[[1]]
  raw <- oak_scaffold(value)
  target <- step_target_variables(step)[[1]]
  result <- sdtm.oak::assign_no_ct(raw_dat = raw, raw_var = "RAW", tgt_var = target, id_vars = sdtm.oak::oak_id_vars())
  set_step_target(state, step, result[[target]])
}

step_hardcode_no_ct <- function(state, concept, step, config) {
  anchor <- if (length(step_source_refs(concept, step))) step_sources(state, concept, step)[[1]] else seq_len(nrow(state$target))
  raw <- oak_scaffold(anchor)
  target <- step_target_variables(step)[[1]]
  result <- sdtm.oak::hardcode_no_ct(
    raw_dat = raw, raw_var = "RAW", tgt_var = target,
    tgt_val = step$parameters$value, id_vars = sdtm.oak::oak_id_vars()
  )
  set_step_target(state, step, result[[target]])
}

step_assign_ct <- function(state, concept, step, config) {
  value <- as.character(step_sources(state, concept, step)[[1]])
  target <- step_target_variables(step)[[1]]
  codelist <- step$parameters$codelist_id
  raw <- oak_scaffold(value)
  spec <- ct_spec_from_config(codelist, config)
  result <- suppressMessages(sdtm.oak::assign_ct(
    raw_dat = raw, raw_var = "RAW", tgt_var = target,
    ct_spec = spec, ct_clst = codelist, id_vars = sdtm.oak::oak_id_vars()
  ))
  mapped <- as.character(result[[target]])
  collected <- names(load_controlled_terminology(config)$codelists[[codelist]])
  unmatched <- unique(value[!is.na(value) & nzchar(value) & !value %in% collected])
  if (length(unmatched)) trace_abort(sprintf("%s 存在未登记术语：%s", concept$concept_id, paste(unmatched, collapse = ", ")))
  set_step_target(state, step, mapped)
}

step_hardcode_ct <- function(state, concept, step, config) {
  codelist <- step$parameters$codelist_id
  allowed <- unlist(load_controlled_terminology(config)$codelists[[codelist]], use.names = FALSE)
  value <- as.character(step$parameters$value)
  if (!value %in% allowed) trace_abort(sprintf("%s 不在术语表 %s 中。", value, codelist))
  anchor <- if (length(step_source_refs(concept, step))) step_sources(state, concept, step)[[1]] else seq_len(nrow(state$target))
  raw <- oak_scaffold(anchor)
  target <- step_target_variables(step)[[1]]
  result <- suppressMessages(sdtm.oak::hardcode_ct(
    raw_dat = raw, raw_var = "RAW", tgt_var = target, tgt_val = value,
    ct_spec = ct_spec_from_config(codelist, config), ct_clst = codelist,
    id_vars = sdtm.oak::oak_id_vars()
  ))
  set_step_target(state, step, result[[target]])
}

normalize_format_vector <- function(formats, source_count) {
  formats <- unlist(formats, use.names = FALSE)
  if (source_count == 1L) return(list(formats))
  if (length(formats) != source_count) trace_abort("日期时间格式数量必须与来源字段数量一致。")
  as.list(formats)
}

step_to_iso8601_date <- function(state, concept, step, config) {
  value <- as.character(step_sources(state, concept, step)[[1]])
  if (any(grepl("UNK|\\bUN\\b", value, ignore.case = TRUE), na.rm = TRUE)) trace_abort(sprintf("%s 包含不完整日期，不能使用完整日期转换。", concept$concept_id))
  formats <- unlist(step$parameters$formats, use.names = FALSE)
  result <- as.character(sdtm.oak::create_iso8601(value, .format = list(formats), .warn = FALSE))
  invalid <- !is.na(value) & is.na(result)
  if (any(invalid)) trace_abort(sprintf("%s 无法转换日期：%s", concept$concept_id, paste(unique(value[invalid]), collapse = ", ")))
  set_step_target(state, step, result)
}

step_to_iso8601_datetime <- function(state, concept, step, config) {
  values <- lapply(step_sources(state, concept, step), as.character)
  if (length(values) == 1L && all(grepl("^\\d{1,2}:\\d{2}", values[[1]][!is.na(values[[1]])]))) trace_abort("只有时间没有日期，不能生成当前 DTC 变量。")
  raw <- oak_scaffold(values[[1]], "RAW1")
  if (length(values) == 2L) raw$RAW2 <- values[[2]]
  formats <- unlist(step$parameters$formats, use.names = FALSE)
  target <- step_target_variables(step)[[1]]
  result <- sdtm.oak::assign_datetime(
    raw_dat = raw, raw_var = paste0("RAW", seq_along(values)), tgt_var = target,
    raw_fmt = formats, raw_unk = unlist(step$parameters$unknown_tokens %||% c("UN", "UNK")),
    id_vars = sdtm.oak::oak_id_vars(), .warn = FALSE
  )
  converted <- as.character(result[[target]])
  invalid <- !is.na(values[[1]]) & is.na(converted)
  if (any(invalid)) trace_abort(sprintf("%s 存在无法转换的日期时间。", concept$concept_id))
  set_step_target(state, step, converted)
}

canonicalize_partial_iso <- function(value) {
  result <- value
  result <- sub("----$", "", result)
  result <- sub("--$", "", result)
  result <- sub("T-:-$", "", result)
  result
}

step_to_iso8601_partial_datetime <- function(state, concept, step, config) {
  values <- lapply(step_sources(state, concept, step), as.character)
  if (length(values) == 1L && all(grepl("^\\d{1,2}:\\d{2}", values[[1]][!is.na(values[[1]])]))) trace_abort("只有时间没有日期，不能生成当前 DTC 变量。")
  formats <- unlist(step$parameters$formats, use.names = FALSE)
  if (length(formats) != length(values)) trace_abort("不完整日期时间格式数量必须与来源字段数量一致。")
  args <- c(values, list(.format = formats, .na = unlist(step$parameters$unknown_tokens), .warn = FALSE))
  converted <- as.character(do.call(sdtm.oak::create_iso8601, args))
  converted <- canonicalize_partial_iso(converted)
  invalid <- !is.na(values[[1]]) & (is.na(converted) | !nzchar(converted))
  if (any(invalid)) trace_abort(sprintf("%s 存在无法解释的不完整日期。", concept$concept_id))
  set_step_target(state, step, converted)
}

step_merge_sources <- function(state, concept, step, config) {
  parameters <- step$parameters
  left <- state$sources[[parameters$left_dataset]]
  right <- state$sources[[parameters$right_dataset]]
  if (is.null(left) || is.null(right)) trace_abort(sprintf("%s 连接的数据集不存在。", concept$concept_id))
  by <- unlist(parameters$by, use.names = TRUE)
  if (any(!names(by) %in% names(left)) || any(!unname(by) %in% names(right))) trace_abort(sprintf("%s 连接键不存在。", concept$concept_id))
  right_keys <- unname(by)
  if (parameters$relationship %in% c("one-to-one", "many-to-one") && anyDuplicated(right[right_keys])) {
    trace_abort(sprintf("%s 的右侧数据不满足 %s。", concept$concept_id, parameters$relationship))
  }
  if (parameters$relationship == "one-to-one" && anyDuplicated(left[names(by)])) trace_abort(sprintf("%s 的左侧数据不满足 one-to-one。", concept$concept_id))
  select <- unique(c(right_keys, unlist(parameters$select, use.names = FALSE)))
  if (any(!select %in% names(right))) trace_abort(sprintf("%s 连接选择了不存在的字段。", concept$concept_id))
  right <- right[, select, drop = FALSE]
  joined <- dplyr::left_join(left, right, by = by, relationship = parameters$relationship)
  if (nrow(joined) != nrow(left)) trace_abort(sprintf("%s 连接改变了基础记录数。", concept$concept_id))
  state$sources[[parameters$output_dataset]] <- joined
  if (identical(state$base_dataset, parameters$left_dataset)) {
    state$base_dataset <- parameters$output_dataset
    state$target <- tibble::tibble(.SOURCE_ROW = seq_len(nrow(joined)))
  }
  state
}

step_coalesce_fields <- function(state, concept, step, config) {
  values <- step_sources(state, concept, step)
  lengths <- vapply(values, length, integer(1))
  if (length(unique(lengths)) != 1L) trace_abort(sprintf("%s 的候选来源没有按记录对齐。", concept$concept_id))
  result <- values[[1]]
  for (value in values[-1]) {
    if (identical(step$parameters$conflict_policy, "error_if_disagree")) {
      conflict <- !is.na(result) & !is.na(value) & as.character(result) != as.character(value)
      if (any(conflict)) trace_abort(sprintf("%s 的候选来源存在冲突。", concept$concept_id))
    }
    missing <- is.na(result) | (is.character(result) & !nzchar(trimws(result)))
    result[missing] <- value[missing]
  }
  set_step_target(state, step, result)
}

step_derive_reference_datetime <- function(state, concept, step, config) {
  parameters <- step$parameters
  target <- step_target_variables(step)[[1]]
  subject_key <- unlist(parameters$subject_keys, use.names = FALSE)[[1]]
  dm_key <- if (subject_key %in% names(state$sources[[state$base_dataset]])) state$sources[[state$base_dataset]][[subject_key]] else state$target$SUBJID
  dm_temp <- state$target
  dm_temp$patient_number <- as.character(dm_key)
  source_list <- list()
  rows <- list()
  for (source in parameters$sources) {
    raw <- state$sources[[source$dataset]]
    if (is.null(raw) || !subject_key %in% names(raw)) trace_abort(sprintf("%s 缺少参考日期受试者键。", concept$concept_id))
    raw$patient_number <- as.character(raw[[subject_key]])
    source_list[[source$dataset]] <- raw
    rows[[length(rows) + 1L]] <- tibble::tibble(
      raw_dataset_name = source$dataset,
      date_var = source$date_variable,
      time_var = source$time_variable %||% NA_character_,
      dformat = unlist(source$date_formats, use.names = FALSE)[[1]],
      tformat = if (length(source$time_formats %||% list())) unlist(source$time_formats, use.names = FALSE)[[1]] else NA_character_,
      sdtm_var_name = target
    )
  }
  config_df <- dplyr::bind_rows(rows)
  derived <- sdtm.oak::oak_cal_ref_dates(
    ds_in = dm_temp, der_var = target, min_max = parameters$selection,
    ref_date_config_df = config_df, raw_source = source_list
  )
  set_step_target(state, step, as.character(derived[[target]]))
}

step_combine_fields <- function(state, concept, step, config) {
  values <- lapply(step_sources(state, concept, step), as.character)
  missing <- Reduce(`|`, lapply(values, is.na))
  result <- do.call(paste, c(values, sep = step$parameters$separator))
  if (step$parameters$missing_policy == "return_missing") result[missing] <- NA_character_
  set_step_target(state, step, result)
}

step_derive_usubjid <- function(state, concept, step, config) {
  step$parameters$missing_policy <- "return_missing"
  step_combine_fields(state, concept, step, config)
}

step_extract_delimited_part <- function(state, concept, step, config) {
  value <- as.character(step_sources(state, concept, step)[[1]])
  parts <- strsplit(value, step$parameters$separator, fixed = TRUE)
  position <- as.integer(step$parameters$position)
  result <- vapply(parts, function(x) if (length(x) >= position) x[[position]] else NA_character_, character(1))
  if (any(is.na(result) & !is.na(value))) trace_abort(sprintf("%s 的分隔字段位置不存在。", concept$concept_id))
  set_step_target(state, step, result)
}

evaluate_safe_condition <- function(data, condition) {
  variable <- condition$variable
  if (!variable %in% names(data)) trace_abort(sprintf("条件字段不存在：%s", variable))
  value <- data[[variable]]
  operator <- condition$operator
  compare_to <- condition$compare_to
  switch(
    operator,
    equals = as.character(value) == as.character(compare_to),
    not_equals = as.character(value) != as.character(compare_to),
    `in` = as.character(value) %in% as.character(unlist(compare_to)),
    is_missing = is.na(value) | (is.character(value) & !nzchar(trimws(value))),
    not_missing = !(is.na(value) | (is.character(value) & !nzchar(trimws(value)))),
    trace_abort(sprintf("不允许的条件运算符：%s", operator))
  )
}

step_conditional_assign <- function(state, concept, step, config) {
  dataset <- unique(vapply(concept_source_refs(concept), function(x) x$dataset, character(1)))[[1]]
  raw <- state$sources[[dataset]]
  condition <- evaluate_safe_condition(raw, step$parameters$condition)
  result <- rep(step$parameters$else_value %||% NA, nrow(raw))
  result[condition %in% TRUE] <- step$parameters$value
  set_step_target(state, step, result)
}

step_derive_ongoing_flag <- function(state, concept, step, config) {
  values <- lapply(step_sources(state, concept, step), as.character)
  date_missing <- is.na(values[[1]]) | !nzchar(trimws(values[[1]]))
  ongoing <- date_missing
  if (length(values) > 1L) ongoing <- ongoing | values[[2]] %in% unlist(step$parameters$affirmative_values %||% c("Y", "Yes"))
  result <- ifelse(ongoing, step$parameters$ongoing_value, NA_character_)
  set_step_target(state, step, result)
}

step_normalize_case <- function(state, concept, step, config) {
  value <- as.character(step_sources(state, concept, step)[[1]])
  result <- if (step$parameters$case == "upper") toupper(value) else tolower(value)
  set_step_target(state, step, result)
}

step_transpose_findings <- function(state, concept, step, config) {
  refs <- step_source_refs(concept, step)
  result_ref <- refs[[1]]
  raw <- state$sources[[result_ref$dataset]]
  value <- raw[[result_ref$variable]]
  observed <- !is.na(value) & nzchar(trimws(as.character(value)))
  block <- state$target[observed, , drop = FALSE]
  block$VSTESTCD <- step$parameters$test_code
  block$VSTEST <- step$parameters$test_name
  block$VSORRES <- as.character(value[observed])
  unit <- step$parameters$original_unit
  if (!is.null(step$parameters$unit_source) && nzchar(step$parameters$unit_source)) {
    if (!step$parameters$unit_source %in% names(raw)) trace_abort(sprintf("%s 的单位字段不存在。", concept$concept_id))
    unit <- as.character(raw[[step$parameters$unit_source]][observed])
  }
  aliases <- unlist(load_unit_conversions(config)$unit_aliases %||% list(), use.names = TRUE)
  alias_hit <- !is.na(unit) & as.character(unit) %in% names(aliases)
  unit[alias_hit] <- unname(aliases[as.character(unit[alias_hit])])
  block$VSORRESU <- if (length(unit) == 1L) rep(unit, nrow(block)) else unit
  block$.CONCEPT_ID <- concept$concept_id
  state$current_records <- block
  state
}

step_standardize_unit <- function(state, concept, step, config) {
  block <- state$current_records
  if (is.null(block)) trace_abort(sprintf("%s 必须在纵向转换之后执行单位换算。", concept$concept_id))
  conversion_config <- load_unit_conversions(config)
  set <- conversion_config$sets[[step$parameters$conversion_set_id]]
  if (is.null(set)) trace_abort(sprintf("未登记单位换算集合：%s", step$parameters$conversion_set_id))
  table <- purrr::map_dfr(set, tibble::as_tibble)
  aliases <- unlist(conversion_config$unit_aliases %||% list(), use.names = TRUE)
  original_unit <- as.character(block$VSORRESU)
  normalized_unit <- original_unit
  alias_hit <- normalized_unit %in% names(aliases)
  normalized_unit[alias_hit] <- unname(aliases[normalized_unit[alias_hit]])
  numeric_value <- suppressWarnings(as.numeric(block$VSORRES))
  if (any(is.na(numeric_value) & !is.na(block$VSORRES))) trace_abort(sprintf("%s 包含无法换算的非数值结果。", concept$concept_id))
  standardized <- numeric(length(numeric_value))
  for (index in seq_along(numeric_value)) {
    row <- table[table$test_code == block$VSTESTCD[[index]] & table$from_unit == normalized_unit[[index]] & table$to_unit == step$parameters$target_unit, , drop = FALSE]
    if (nrow(row) != 1L) trace_abort(sprintf("%s 未登记 %s/%s 到 %s 的换算。", concept$concept_id, block$VSTESTCD[[index]], normalized_unit[[index]], step$parameters$target_unit))
    standardized[[index]] <- round(numeric_value[[index]] * row$multiplier[[1]] + row$offset[[1]], row$round_digits[[1]])
  }
  block$VSSTRESN <- standardized
  block$VSSTRESC <- format(standardized, trim = TRUE, scientific = FALSE)
  block$VSSTRESU <- step$parameters$target_unit
  state$current_records <- block
  state
}

step_derive_sequence <- function(state, concept, step, config) {
  target <- step_target_variables(step)[[1]]
  state$target <- sdtm.oak::derive_seq(
    tgt_dat = state$target, tgt_var = target,
    rec_vars = unlist(step$parameters$record_variables),
    sbj_vars = c("STUDYID", "USUBJID"), start_at = step$parameters$start_at %||% 1L
  )
  state
}

complete_iso_date <- function(value) !is.na(value) & grepl("^\\d{4}-\\d{2}-\\d{2}", value)

step_derive_study_day <- function(state, concept, step, config) {
  target_date <- step$parameters$target_date
  reference_date <- step$parameters$reference_date
  target <- step_target_variables(step)[[1]]
  original <- as.character(state$target[[target_date]])
  derived <- sdtm.oak::derive_study_day(
    sdtm_in = state$target, dm_domain = state$dm,
    tgdt = target_date, refdt = reference_date,
    study_day_var = target, merge_key = "USUBJID"
  )
  derived[[target]][!complete_iso_date(original)] <- NA_real_
  derived[[target_date]] <- original
  state$target <- derived
  state
}

step_derive_visitnum <- function(state, concept, step, config) {
  value <- as.character(step_sources(state, concept, step)[[1]])
  visit_map <- load_controlled_terminology(config)$visit_maps[[step$parameters$visit_map_id]]
  if (is.null(visit_map)) trace_abort(sprintf("未登记访视表：%s", step$parameters$visit_map_id))
  mapped <- suppressWarnings(as.numeric(unname(unlist(visit_map)[value])))
  if (any(is.na(mapped) & !is.na(value))) trace_abort(sprintf("%s 包含未登记访视。", concept$concept_id))
  set_step_target(state, step, mapped)
}

step_derive_baseline_flag <- function(state, concept, step, config) {
  data <- state$target
  added_vsstat <- !"VSSTAT" %in% names(data)
  if (added_vsstat) data$VSSTAT <- NA_character_
  data$oak_id <- seq_len(nrow(data))
  data$raw_source <- "TraceSDTM"
  data$patient_number <- data$USUBJID
  derived <- sdtm.oak::derive_blfl(
    sdtm_in = data, dm_domain = state$dm,
    tgt_var = step_target_variables(step)[[1]], ref_var = step$parameters$reference_date,
    baseline_visits = unlist(step$parameters$baseline_visits %||% character()),
    baseline_timepoints = unlist(step$parameters$baseline_timepoints %||% character())
  )
  transient <- c("oak_id", "raw_source", "patient_number", if (added_vsstat) "VSSTAT")
  state$target <- dplyr::select(derived, -dplyr::any_of(transient))
  state
}

step_do_not_map <- function(state, concept, step, config) state

transform_implementation_bindings <- function() {
  list(
    assign_no_ct = step_assign_no_ct,
    hardcode_no_ct = step_hardcode_no_ct,
    assign_ct = step_assign_ct,
    hardcode_ct = step_hardcode_ct,
    to_iso8601_date = step_to_iso8601_date,
    to_iso8601_datetime = step_to_iso8601_datetime,
    to_iso8601_partial_datetime = step_to_iso8601_partial_datetime,
    merge_sources = step_merge_sources,
    coalesce_fields = step_coalesce_fields,
    derive_reference_datetime = step_derive_reference_datetime,
    combine_fields = step_combine_fields,
    derive_usubjid = step_derive_usubjid,
    extract_delimited_part = step_extract_delimited_part,
    conditional_assign = step_conditional_assign,
    derive_ongoing_flag = step_derive_ongoing_flag,
    normalize_case = step_normalize_case,
    transpose_findings = step_transpose_findings,
    standardize_unit = step_standardize_unit,
    derive_sequence = step_derive_sequence,
    derive_study_day = step_derive_study_day,
    derive_visitnum = step_derive_visitnum,
    derive_baseline_flag = step_derive_baseline_flag,
    do_not_map = step_do_not_map
  )
}

execute_registered_step <- function(state, concept, step, config = load_project_config(), registry = load_transform_registry(config)) {
  entry <- registry_entry(step$transform_id, registry)
  implementation <- transform_implementation_bindings()[[entry$implementation_id]]
  if (is.null(implementation)) trace_abort(sprintf("%s 的实现未绑定。", step$transform_id))
  implementation(state, concept, step, config)
}
