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
  if (first_over_12) return("d/m/y")
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

profile_sources <- function(config = load_project_config()) {
  ensure_output_directories(config)
  specification <- load_mapping_template(config)
  concepts <- specification$concepts %||% list()
  dictionary <- purrr::imap_dfr(specification$source_catalog, function(source, dataset) {
    if (isTRUE(source$derived)) return(tibble::tibble())
    path <- trace_path(config$paths$raw_dir, source$file)
    if (!file.exists(path)) trace_abort(sprintf("缺少原始数据：%s", path))
    data <- read_raw_csv(path)
    literal_data <- readr::read_csv(
      path, na = c("", "NA", "N/A"), show_col_types = FALSE, progress = FALSE,
      name_repair = "minimal", col_types = readr::cols(.default = readr::col_character())
    )
    domains <- unique(vapply(Filter(function(concept) any(vapply(concept_source_refs(concept), function(ref) identical(ref$dataset, dataset), logical(1))), concepts), function(concept) concept$target_domain, character(1)))
    purrr::map_dfr(names(data), function(variable) {
      values <- data[[variable]]
      literal_values <- literal_data[[variable]]
      observed <- unique(as.character(stats::na.omit(literal_values)))
      roles <- unique(unlist(lapply(concepts, function(concept) {
        refs <- Filter(function(ref) identical(ref$dataset, dataset) && identical(ref$variable, variable), concept_source_refs(concept))
        vapply(refs, function(ref) as.character(ref$role %||% ""), character(1))
      })))
      character_values <- as.character(stats::na.omit(literal_values))
      partial_tokens <- unique(unlist(stringr::str_extract_all(character_values, stringr::regex("UNK|\\bUN\\b", ignore_case = TRUE))))
      formats <- infer_source_formats(character_values)
      tibble::tibble(
        source_domain = paste(domains, collapse = " | "),
        source_dataset = dataset,
        source_variable = variable,
        label = infer_source_label(variable),
        data_type = class(values)[1],
        example_values = compact_value(utils::head(observed, 5L)),
        missing_rate = mean(is.na(values)),
        unique_count = length(observed),
        form_name = source$form_name,
        grain = source$grain,
        keys = paste(unlist(source$keys), collapse = " | "),
        concept_roles = paste(roles[nzchar(roles)], collapse = " | "),
        format_candidates = paste(unique(formats), collapse = " | "),
        partial_tokens = paste(partial_tokens, collapse = " | "),
        record_count = nrow(data)
      )
    })
  })
  dictionary <- dplyr::arrange(dictionary, source_dataset, source_variable)
  output <- trace_path(config$paths$profile_dir, "source_dictionary.csv")
  write_csv(dictionary, output)

  relationships <- purrr::imap_dfr(specification$source_catalog, function(source, dataset) tibble::tibble(
    source_dataset = dataset,
    file = source$file %||% "",
    derived = isTRUE(source$derived),
    form_name = source$form_name,
    grain = source$grain,
    keys = paste(unlist(source$keys), collapse = " | ")
  ))
  write_csv(relationships, trace_path(config$paths$profile_dir, "source_relationships.csv"))

  summary <- list(
    generated_at = utc_now(),
    datasets = dplyr::n_distinct(dictionary$source_dataset),
    fields = nrow(dictionary),
    fields_by_domain = as.list(table(dictionary$source_domain)),
    dictionary_sha256 = file_sha256(output)
  )
  write_json(summary, trace_path(config$paths$profile_dir, "profile_summary.json"))
  trace_info("已生成数据画像：%s 个字段。", nrow(dictionary))
  dictionary
}
