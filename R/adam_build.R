# Deterministic ADaM construction -------------------------------------------

analysis_variable_names <- function(plan, dataset) {
  vapply(plan$datasets[[dataset]]$variables, function(variable) as.character(variable$name), character(1))
}

apply_analysis_labels <- function(data, plan, dataset) {
  definitions <- plan$datasets[[dataset]]$variables
  for (definition in definitions) {
    variable <- as.character(definition$name)
    if (variable %in% names(data)) attr(data[[variable]], "label") <- as.character(definition$label)
  }
  attr(data, "label") <- as.character(plan$datasets[[dataset]]$label)
  data
}

treatment_lookup <- function(plan) {
  tibble::tibble(
    name = vapply(plan$treatments, function(item) as.character(item$name), character(1)),
    code = vapply(plan$treatments, function(item) as.character(item$code), character(1)),
    number = vapply(plan$treatments, function(item) as.numeric(item$number), numeric(1))
  )
}

map_treatment_number <- function(name, code, plan) {
  lookup <- treatment_lookup(plan)
  by_name <- lookup$number[match(as.character(name), lookup$name)]
  by_code <- lookup$number[match(as.character(code), lookup$code)]
  ifelse(!is.na(by_name), by_name, by_code)
}

derive_adsl <- function(dm, plan) {
  required <- c(
    "STUDYID", "USUBJID", "SUBJID", "SITEID", "AGE", "AGEU", "SEX", "RACE", "ETHNIC",
    "COUNTRY", "ARM", "ARMCD", "ACTARM", "ACTARMCD", "RFSTDTC", "RFENDTC"
  )
  missing <- setdiff(required, names(dm))
  if (length(missing)) trace_abort(sprintf("构建 ADSL 所需 DM 变量缺失：%s。", paste(missing, collapse = "、")))

  adsl <- dm |>
    dplyr::transmute(
      STUDYID = as.character(.data$STUDYID), USUBJID = as.character(.data$USUBJID),
      SUBJID = as.character(.data$SUBJID), SITEID = as.character(.data$SITEID),
      AGE = as.numeric(.data$AGE), AGEU = as.character(.data$AGEU), SEX = as.character(.data$SEX),
      RACE = as.character(.data$RACE), ETHNIC = as.character(.data$ETHNIC), COUNTRY = as.character(.data$COUNTRY),
      TRT01P = as.character(.data$ARM), TRT01PN = map_treatment_number(.data$ARM, .data$ARMCD, plan),
      TRT01A = as.character(.data$ACTARM), TRT01AN = map_treatment_number(.data$ACTARM, .data$ACTARMCD, plan),
      RFSTDTC = as.character(.data$RFSTDTC), RFENDTC = as.character(.data$RFENDTC)
    ) |>
    admiral::derive_vars_dt(new_vars_prefix = "TRTS", dtc = RFSTDTC, highest_imputation = "n") |>
    admiral::derive_vars_dt(new_vars_prefix = "TRTE", dtc = RFENDTC, highest_imputation = "n") |>
    dplyr::mutate(
      TRTDURD = as.numeric(.data$TRTEDT - .data$TRTSDT) + 1,
      ITTFL = dplyr::if_else(!is.na(.data$TRT01P) & nzchar(.data$TRT01P), "Y", "N"),
      SAFFL = dplyr::if_else(!is.na(.data$TRTSDT), "Y", "N")
    ) |>
    dplyr::select(dplyr::all_of(analysis_variable_names(plan, "ADSL"))) |>
    dplyr::arrange(.data$STUDYID, .data$USUBJID)
  apply_analysis_labels(adsl, plan, "ADSL")
}

set_first_occurrence_flag <- function(data, grouping, new_variable) {
  data[[new_variable]] <- NA_character_
  eligible <- which(data$TRTEMFL == "Y")
  if (!length(eligible)) return(data)
  order_values <- do.call(order, c(lapply(grouping, function(variable) as.character(data[[variable]][eligible])), list(
    data$ASTDT[eligible], data$AESEQ[eligible], na.last = TRUE
  )))
  ordered <- eligible[order_values]
  keys <- do.call(paste, c(lapply(grouping, function(variable) as.character(data[[variable]][ordered])), sep = "\r"))
  data[[new_variable]][ordered[!duplicated(keys)]] <- "Y"
  data
}

derive_adae <- function(ae, adsl, plan) {
  required <- c("STUDYID", "USUBJID", "AESEQ", "AESTDTC", "AEENDTC", "AEBODSYS", "AEDECOD")
  missing <- setdiff(required, names(ae))
  if (length(missing)) trace_abort(sprintf("构建 ADAE 所需 AE 变量缺失：%s。", paste(missing, collapse = "、")))
  subject_variables <- c(
    "STUDYID", "USUBJID", "TRT01P", "TRT01PN", "TRT01A", "TRT01AN",
    "TRTSDT", "TRTEDT", "SAFFL"
  )
  adae <- ae |>
    dplyr::mutate(dplyr::across(where(~ inherits(.x, "haven_labelled")), haven::zap_labels)) |>
    dplyr::left_join(dplyr::select(adsl, dplyr::all_of(subject_variables)), by = c("STUDYID", "USUBJID"))
  adae <- adae |>
    admiral::derive_vars_dt(new_vars_prefix = "AST", dtc = AESTDTC, highest_imputation = "n") |>
    admiral::derive_vars_dt(new_vars_prefix = "AEN", dtc = AEENDTC, highest_imputation = "n") |>
    dplyr::rename(
      TRTP = TRT01P, TRTPN = TRT01PN,
      TRTA = TRT01A, TRTAN = TRT01AN
    ) |>
    admiral::derive_var_trtemfl(
      start_date = ASTDT, end_date = AENDT, trt_start_date = TRTSDT,
      trt_end_date = TRTEDT, end_window = as.integer(plan$teae$end_window_days)
    )
  adae <- set_first_occurrence_flag(adae, c("USUBJID"), "AOCCFL")
  adae <- set_first_occurrence_flag(adae, c("USUBJID", "AEBODSYS"), "AOCCSFL")
  adae <- set_first_occurrence_flag(adae, c("USUBJID", "AEBODSYS", "AEDECOD"), "AOCCPFL")
  expected <- analysis_variable_names(plan, "ADAE")
  missing_output <- setdiff(expected, names(adae))
  if (length(missing_output)) trace_abort(sprintf("ADAE 派生后缺少变量：%s。", paste(missing_output, collapse = "、")))
  adae <- adae |>
    dplyr::select(dplyr::all_of(expected)) |>
    dplyr::arrange(.data$STUDYID, .data$USUBJID, .data$AESEQ)
  apply_analysis_labels(adae, plan, "ADAE")
}

write_adam_dataset <- function(data, dataset, plan, config) {
  csv_path <- trace_path(config$paths$adam_csv_dir, paste0(tolower(dataset), ".csv"))
  xpt_path <- trace_path(config$paths$adam_xpt_dir, paste0(tolower(dataset), ".xpt"))
  csv_data <- data
  date_variables <- names(csv_data)[vapply(csv_data, inherits, logical(1), "Date")]
  for (variable in date_variables) csv_data[[variable]] <- format(csv_data[[variable]], "%Y-%m-%d")
  write_csv(csv_data, csv_path)
  ensure_parent(xpt_path)
  haven::write_xpt(
    data, xpt_path, version = 5, name = tolower(dataset),
    label = as.character(plan$datasets[[dataset]]$label)
  )
  tibble::tibble(
    dataset = dataset, records = nrow(data), variables = ncol(data),
    data_sha256 = data_sha256(data), csv_sha256 = file_sha256(csv_path), xpt_sha256 = file_sha256(xpt_path),
    csv_path = csv_path, xpt_path = xpt_path
  )
}

adam_lineage_rows <- function(plan, dataset, input_checksums) {
  plan_sha <- as.character(attr(plan, "sha256") %||% file_sha256(attr(plan, "path")))
  purrr::map_dfr(plan$datasets[[dataset]]$variables, function(variable) {
    tibble::tibble(
      target_dataset = dataset,
      target_variable = as.character(variable$name),
      source_variables = paste(unlist(variable$sources, use.names = FALSE), collapse = " | "),
      rule_id = as.character(variable$rule_id),
      analysis_plan_sha256 = plan_sha,
      input_data_sha256 = as.character(jsonlite::toJSON(input_checksums, auto_unbox = TRUE)),
      implementation = if (as.character(variable$rule_id) %in% c("PARSE_ISO_DATE", "TEAE_TRT_START_TO_END_PLUS_WINDOW")) {
        paste0("admiral ", plan$standards$admiral)
      } else {
        "TraceSDTM deterministic rule registry"
      }
    )
  })
}

load_adam_datasets <- function(config = load_project_config()) {
  datasets <- c("ADSL", "ADAE")
  paths <- stats::setNames(file.path(trace_path(config$paths$adam_xpt_dir), paste0(tolower(datasets), ".xpt")), datasets)
  missing <- names(paths)[!file.exists(paths)]
  if (length(missing)) trace_abort(sprintf("缺少 ADaM 数据集：%s。请先执行 build-adam。", paste(missing, collapse = "、")))
  lapply(paths, function(path) {
    data <- tibble::as_tibble(haven::read_xpt(path))
    data[] <- lapply(data, function(value) {
      if (is.character(value)) value[!nzchar(trimws(value))] <- NA_character_
      value
    })
    data
  })
}

build_adam <- function(config = load_project_config()) {
  ensure_output_directories(config)
  load_approved_mapping(config)
  read_validation_summary(
    trace_path(config$paths$local_validation_dir, "local_validation_summary.json"), "SDTM 本地"
  )
  plan <- load_analysis_plan(config)
  assert_analysis_package_versions(plan)
  sdtm <- load_built_datasets(config)
  missing <- setdiff(c("DM", "AE"), names(sdtm))
  if (length(missing)) trace_abort(sprintf("ADaM 构建需要已批准并通过检查的 SDTM 数据集：%s。", paste(missing, collapse = "、")))
  input_paths <- c(
    DM = trace_path(config$paths$xpt_dir, "dm.xpt"),
    AE = trace_path(config$paths$xpt_dir, "ae.xpt")
  )
  input_checksums <- analysis_input_checksums(input_paths)
  adsl <- derive_adsl(sdtm$DM, plan)
  adae <- derive_adae(sdtm$AE, adsl, plan)
  datasets <- list(ADSL = adsl, ADAE = adae)
  manifest <- purrr::imap_dfr(datasets, ~ write_adam_dataset(.x, .y, plan, config))
  lineage <- purrr::map_dfr(names(datasets), ~ adam_lineage_rows(plan, .x, input_checksums))
  write_csv(lineage, trace_path(config$paths$lineage_dir, "adam_lineage.csv"))
  write_csv(manifest, trace_path(config$paths$manifest_dir, "adam_manifest.csv"))
  write_json(list(
    generated_at = utc_now(), standards = plan$standards,
    analysis_plan_sha256 = attr(plan, "sha256"), input_data_sha256 = input_checksums,
    datasets = as.data.frame(manifest)
  ), trace_path(config$paths$manifest_dir, "adam_build_manifest.json"))
  trace_info("已确定性生成 ADaM：ADSL=%d 行，ADAE=%d 行。", nrow(adsl), nrow(adae))
  invisible(datasets)
}
