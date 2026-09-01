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
