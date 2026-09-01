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
  dictionary <- purrr::imap_dfr(
    specification$domains,
    ~ profile_one_dataset(.y, .x, config)
  )
  dictionary <- dplyr::arrange(dictionary, source_domain, source_variable)
  output <- trace_path(config$paths$profile_dir, "source_dictionary.csv")
  write_csv(dictionary, output)

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
