source_label_lookup <- c(
  STUDY = "Study identifier",
  PATNUM = "Subject identifier",
  FIRST_DOSE_DT = "First dose date",
  IC_DT = "Informed consent date",
  `IT.AGE` = "Age",
  `IT.SEX` = "Sex",
  `IT.ETHNIC` = "Ethnicity",
  `IT.RACE` = "Race",
  COUNTRY = "Country",
  PLANNED_ARM = "Planned treatment arm",
  PLANNED_ARMCD = "Planned treatment arm code",
  ACTUAL_ARM = "Actual treatment arm",
  ACTUAL_ARMCD = "Actual treatment arm code",
  `IT.AETERM` = "Reported adverse event term",
  AEOUTCOME = "Adverse event outcome",
  AEDECOD = "Coded adverse event term",
  AEBODSYS = "Adverse event body system",
  `IT.AESEV` = "Adverse event severity",
  `IT.AESER` = "Serious event indicator",
  `IT.AESHOSP` = "Hospitalization seriousness criterion",
  `IT.AEREL` = "Adverse event relationship",
  `IT.AESTDAT` = "Adverse event start date",
  `IT.AEENDAT` = "Adverse event end date",
  INSTANCE = "Visit name",
  VTLD = "Vital signs date",
  `IT.HEIGHT_VSORRES` = "Height result",
  `IT.WEIGHT` = "Weight result",
  `IT.TEMP` = "Temperature result",
  SYS_BP = "Systolic blood pressure",
  DIA_BP = "Diastolic blood pressure",
  PULSE = "Pulse rate",
  SUBPOS = "Body position"
)

infer_source_label <- function(variable) {
  value <- unname(source_label_lookup[variable])
  if (!length(value) || is.na(value)) gsub("[._]", " ", variable) else value
}

infer_slash_date_format <- function(values) {
  values <- values[grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", values)]
  if (!length(values)) return(character())
  parts <- do.call(rbind, strsplit(values, "/", fixed = TRUE))
  first <- suppressWarnings(as.integer(parts[, 1L]))
  second <- suppressWarnings(as.integer(parts[, 2L]))
  first_over_12 <- any(first > 12L, na.rm = TRUE)
  second_over_12 <- any(second > 12L, na.rm = TRUE)
  if (first_over_12 && second_over_12) return("mixed_m/d/y_and_d/m/y")
  if (first_over_12) return("d-m-y")
  if (second_over_12) return("m/d/y")
  "m/d/y_or_d/m/y"
}

infer_source_formats <- function(values) {
  values <- as.character(stats::na.omit(values))
  formats <- infer_slash_date_format(values)
  if (any(grepl("^(UNK|UN|\\d{1,2})-[A-Za-z]{3}-(UNK|UN|\\d{4})$", values, ignore.case = TRUE))) {
    formats <- c(formats, "dd-mmm-yyyy")
  }
  if (any(grepl("^\\d{4}-\\d{2}-\\d{2}", values))) formats <- c(formats, "y-m-d")
  if (any(grepl("^\\d{1,2}:\\d{2}:\\d{2}$", values))) {
    formats <- c(formats, "H:M:S")
  } else if (any(grepl("^\\d{1,2}:\\d{2}$", values))) {
    formats <- c(formats, "H:M")
  }
  unique(formats)
}

profile_one_dataset <- function(domain, domain_spec, config) {
  path <- trace_path(config$paths$raw_dir, domain_spec$source_file)
  if (!file.exists(path)) trace_abort(sprintf("缺少原始数据：%s", path))
  data <- read_raw_csv(path)
  form_name <- if ("FORM" %in% names(data)) {
    compact_value(unique(stats::na.omit(as.character(data$FORM))), 80L)
  } else {
    tools::file_path_sans_ext(domain_spec$source_file)
  }

  purrr::map_dfr(names(data), function(variable) {
    values <- data[[variable]]
    observed <- unique(as.character(stats::na.omit(values)))
    tibble::tibble(
      source_domain = domain,
      source_dataset = tools::file_path_sans_ext(domain_spec$source_file),
      source_variable = variable,
      label = infer_source_label(variable),
      data_type = class(values)[1],
      example_values = compact_value(utils::head(observed, 5L)),
      missing_rate = mean(is.na(values)),
      unique_count = length(observed),
      form_name = form_name,
      record_count = nrow(data)
    )
  })
}

v06_identifier_like <- function(names) {
  grepl("(^|_)(study|studyid|subject|subjid|usubjid|patnum|patient|site|siteid|id|seq)(_|$)",
        tolower(names), perl = TRUE)
}

v06_candidate_keys <- function(data) {
  fields <- names(data)
  complete_unique <- vapply(data, function(values) {
    text <- trimws(as.character(values))
    !any(is.na(text) | !nzchar(text)) && !anyDuplicated(text)
  }, logical(1))
  singles <- fields[complete_unique]
  preferred <- singles[v06_identifier_like(singles)]
  if (length(preferred)) return(as.list(preferred[[1L]]))
  if (length(singles)) return(as.list(singles[[1L]]))
  candidates <- fields[v06_identifier_like(fields)]
  candidates <- utils::head(candidates, 8L)
  if (length(candidates) >= 2L) {
    pairs <- utils::combn(candidates, 2L, simplify = FALSE)
    for (pair in pairs) {
      values <- lapply(data[pair], function(x) trimws(as.character(x)))
      if (any(vapply(values, function(x) any(is.na(x) | !nzchar(x)), logical(1)))) next
      key <- do.call(paste, c(values, sep = "\u001F"))
      if (!anyDuplicated(key)) return(as.list(pair))
    }
  }
  list()
}

v06_infer_grain <- function(keys, description = "") {
  keys <- unlist(keys %||% character(), use.names = FALSE)
  subject_key <- any(grepl("PATNUM|SUBJID|USUBJID|SUBJECT|PATIENT", keys, ignore.case = TRUE))
  description_support <- grepl("每名?受试者.{0,8}(一条|一行|唯一)|one record per subject", description, ignore.case = TRUE, perl = TRUE)
  if (!length(keys)) "unknown" else if (subject_key && description_support) "one_record_per_subject_description_supported" else if (length(keys) == 1L) "one_record_per_candidate_key" else "one_record_per_composite_candidate_key"
}

v06_role_for_field <- function(variable) {
  value <- toupper(as.character(variable))
  if (value %in% c("STUDY", "STUDYID")) return("study_identifier")
  if (value %in% c("PATNUM", "SUBJID", "USUBJID", "SUBJECT", "PATIENT")) return("subject_identifier")
  if (value %in% c("SITE", "SITEID", "CENTER", "CENTRE")) return("site_identifier")
  if (grepl("DATE|DAT$|DTC$", value)) return("date")
  if (grepl("TIME|TIM$", value)) return("time")
  ""
}

v06_relationship_rows <- function(data_by_dataset, catalog, description = "") {
  ids <- names(data_by_dataset)
  if (length(ids) < 2L) return(tibble::tibble(
    left_dataset = character(), right_dataset = character(), common_fields = character(),
    candidate_keys = character(), relationship = character(), confidence = character(),
    project_description_evidence = logical(), executable = logical()
  ))
  pairs <- utils::combn(ids, 2L, simplify = FALSE)
  purrr::map_dfr(pairs, function(pair) {
    left <- data_by_dataset[[pair[[1L]]]]
    right <- data_by_dataset[[pair[[2L]]]]
    common <- intersect(names(left), names(right))
    left_keys <- unlist(catalog[[pair[[1L]]]]$keys %||% character(), use.names = FALSE)
    right_keys <- unlist(catalog[[pair[[2L]]]]$keys %||% character(), use.names = FALSE)
    join <- intersect(intersect(left_keys, right_keys), common)
    if (!length(join)) join <- common[v06_identifier_like(common)]
    join <- utils::head(join, 4L)
    completeness <- function(data, fields) {
      if (!length(fields)) return(FALSE)
      all(vapply(data[fields], function(x) !any(is.na(x) | !nzchar(trimws(as.character(x)))), logical(1)))
    }
    unique_by <- function(data, fields) {
      if (!length(fields) || !completeness(data, fields)) return(FALSE)
      !anyDuplicated(do.call(paste, c(lapply(data[fields], as.character), sep = "\u001F")))
    }
    left_unique <- unique_by(left, join)
    right_unique <- unique_by(right, join)
    relationship <- if (!length(join)) "unknown" else if (left_unique && right_unique) "one-to-one" else if (right_unique) "many-to-one" else if (left_unique) "one-to-many" else "many-to-many"
    mentions_both <- all(vapply(pair, function(id) grepl(id, description, ignore.case = TRUE, fixed = TRUE), logical(1)))
    relation_word <- grepl("关联|连接|合并|join|merge|relat", description, ignore.case = TRUE)
    description_evidence <- mentions_both && relation_word
    confidence <- if (length(intersect(left_keys, right_keys)) && completeness(left, join) && completeness(right, join)) "high" else if (length(join) || description_evidence) "medium" else "low"
    tibble::tibble(
      left_dataset = pair[[1L]], right_dataset = pair[[2L]],
      common_fields = paste(common, collapse = " | "), candidate_keys = paste(join, collapse = " | "),
      relationship = relationship, confidence = confidence,
      project_description_evidence = description_evidence, executable = FALSE
    )
  })
}

v06_special_rule_drafts <- function(description) {
  text <- trimws(unlist(strsplit(as.character(description %||% ""), "[\\r\\n。；]+", perl = TRUE)))
  text <- text[nzchar(text)]
  matches <- grepl("必须|应当|采用|禁止|规则|格式|单位|标识|基线|访视|日期", text)
  as.list(unique(text[matches]))
}

v06_domain_definition <- function(domain, label, entry) {
  standard <- c(
    DM = "受试者级人口学特征、治疗组、研究标识和参考日期信息。",
    AE = "受试者发生的不良事件、严重性、结局、因果关系和事件日期信息。",
    VS = "生命体征检查项目、原始与标准结果、单位、访视和检查日期信息。"
  )
  unname(standard[[domain]] %||% sprintf("%s；标准主键为 %s。", label, paste(unlist(entry$keys %||% character()), collapse = "、")))
}

profile_sources_v06 <- function(config) {
  ensure_output_directories(config)
  specification <- load_task_specification(config)
  catalog <- specification$source_catalog %||% list()
  if (!length(catalog)) trace_abort("运行快照没有来源数据集。")
  data_by_dataset <- list()
  literal_by_dataset <- list()
  dictionary <- purrr::imap_dfr(catalog, function(source, dataset) {
    if (isTRUE(source$derived)) return(tibble::tibble())
    path <- trace_path(config$paths$raw_dir, source$file)
    if (!file.exists(path)) trace_abort(sprintf("缺少原始数据：%s", path))
    data <- read_raw_csv(path)
    literal <- readr::read_csv(
      path, na = c("", "NA", "N/A"), show_col_types = FALSE, progress = FALSE,
      name_repair = "minimal", col_types = readr::cols(.default = readr::col_character())
    )
    data_by_dataset[[dataset]] <<- data
    literal_by_dataset[[dataset]] <<- literal
    keys <- v06_candidate_keys(literal)
    catalog[[dataset]]$keys <<- keys
    catalog[[dataset]]$grain <<- v06_infer_grain(keys, config$project$description)
    catalog[[dataset]]$columns <<- as.list(names(literal))
    purrr::map_dfr(names(data), function(variable) {
      values <- data[[variable]]
      literal_values <- literal[[variable]]
      observed <- unique(as.character(stats::na.omit(literal_values)))
      character_values <- as.character(stats::na.omit(literal_values))
      partial_tokens <- unique(unlist(stringr::str_extract_all(character_values, stringr::regex("UNK|\\bUN\\b", ignore_case = TRUE))))
      tibble::tibble(
        source_domain = "", source_dataset = dataset, source_variable = variable,
        label = infer_source_label(variable), data_type = class(values)[1],
        example_values = compact_value(utils::head(observed, 5L)),
        example_authorized = FALSE,
        example_always_excluded = v06_role_for_field(variable) %in% c("study_identifier", "subject_identifier", "site_identifier") || variable %in% unlist(keys),
        missing_rate = mean(is.na(literal_values)), unique_count = length(observed),
        form_name = source$form_name %||% dataset, grain = catalog[[dataset]]$grain,
        keys = paste(unlist(keys), collapse = " | "), concept_roles = v06_role_for_field(variable),
        format_candidates = paste(infer_source_formats(character_values), collapse = " | "),
        partial_tokens = paste(partial_tokens, collapse = " | "), record_count = nrow(data)
      )
    })
  })
  dictionary <- dplyr::arrange(dictionary, source_dataset, source_variable)
  dictionary_path <- trace_path(config$paths$profile_dir, "source_dictionary.csv")
  write_csv(dictionary, dictionary_path)
  relationships <- v06_relationship_rows(data_by_dataset, catalog, config$project$description)
  write_csv(relationships, trace_path(config$paths$profile_dir, "source_relationships.csv"))

  specification$source_catalog <- catalog
  specification$specification$status <- "profiled"
  write_yaml(specification, trace_path(config$paths$task_specification))

  policies <- load_mapping_policies(config)
  allowed_formats <- c("m/d/y", "d-m-y", "dd-mmm-yyyy", "y-m-d", "H:M", "H:M:S")
  date_rows <- dplyr::filter(dictionary, nzchar(.data$format_candidates))
  policies$date_time_formats$approved_sources <- lapply(seq_len(nrow(date_rows)), function(index) {
    formats <- trimws(unlist(strsplit(date_rows$format_candidates[[index]], "|", fixed = TRUE)))
    formats <- intersect(formats, allowed_formats)
    if (!length(formats)) return(NULL)
    list(source = list(dataset = date_rows$source_dataset[[index]], variable = date_rows$source_variable[[index]]), formats = as.list(formats))
  })
  policies$date_time_formats$approved_sources <- Filter(Negate(is.null), policies$date_time_formats$approved_sources)
  write_yaml(policies, trace_path(config$paths$mapping_policies))

  metadata <- load_metadata(config)
  requested_domains <- unlist(config$project$target_domains %||% character(), use.names = FALSE)
  allowed_domains <- if (length(requested_domains)) intersect(requested_domains, names(metadata$domains)) else names(metadata$domains)
  domain_catalog <- stats::setNames(lapply(allowed_domains, function(domain) {
    entry <- metadata$domains[[domain]]
    # 任务发现阶段只需要知道允许的目标域，不应提前看到目标变量目录。
    list(domain = domain, label = entry$label, definition = v06_domain_definition(domain, entry$label, entry))
  }), allowed_domains)
  safe_dictionary <- dictionary
  safe_dictionary$example_values <- NULL
  context <- list(
    schema_version = "0.6", generated_at = utc_now(),
    project = list(
      project_id = config$project$project_id, name = config$project$name,
      study_id = config$project$study_id, description = config$project$description,
      standard = config$project$standard, standard_version = config$project$standard_version,
      target_domain_scope = as.list(allowed_domains)
    ),
    domain_catalog = domain_catalog,
    source_catalog = catalog,
    field_profiles = jsonlite::fromJSON(jsonlite::toJSON(safe_dictionary, dataframe = "rows", na = "null"), simplifyVector = FALSE),
    relationships = jsonlite::fromJSON(jsonlite::toJSON(relationships, dataframe = "rows", na = "null"), simplifyVector = FALSE),
    special_rule_drafts = v06_special_rule_drafts(config$project$description)
  )
  write_json(context, trace_path(config$paths$project_context))
  summary <- list(
    schema_version = "0.6", generated_at = utc_now(), datasets = length(data_by_dataset),
    fields = nrow(dictionary), candidate_relationships = nrow(relationships),
    dictionary_sha256 = file_sha256(dictionary_path), context_sha256 = file_sha256(trace_path(config$paths$project_context))
  )
  write_json(summary, trace_path(config$paths$profile_dir, "profile_summary.json"))
  if (!is.null(config$studio$project_id)) {
    run <- studio_read_run(config$studio$project_id, config$studio$run_id)
    run$configuration_sha256[["tasks.yml"]] <- file_sha256(trace_path(config$paths$task_specification))
    run$configuration_sha256[["mapping_policies.yml"]] <- file_sha256(trace_path(config$paths$mapping_policies))
    studio_write_run(config$studio$project_id, config$studio$run_id, run)
  }
  trace_info("已生成通用数据画像：%s 个数据集，%s 个字段。", length(data_by_dataset), nrow(dictionary))
  dictionary
}

profile_sources <- function(config = load_project_config()) {
  profile_sources_v06(config)
}
