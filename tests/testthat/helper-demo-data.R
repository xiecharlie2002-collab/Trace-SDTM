demo_iso_date <- function(value, format = "%m/%d/%Y") {
  result <- as.Date(as.character(value), format = format)
  ifelse(is.na(result), NA_character_, format(result, "%Y-%m-%d"))
}

demo_study_day <- function(date_value, reference_value) {
  date <- as.Date(date_value)
  reference <- as.Date(reference_value)
  difference <- as.numeric(date - reference)
  ifelse(is.na(difference), NA_real_, difference + ifelse(difference >= 0, 1, 0))
}

demo_sdtm_datasets <- function() {
  dm_raw <- read_raw_csv(trace_path("data", "raw", "dm_raw.csv"))
  ae_raw <- read_raw_csv(trace_path("data", "raw", "ae_raw.csv"))
  vs_raw <- read_raw_csv(trace_path("data", "raw", "vs_raw.csv"))
  ct <- load_controlled_terminology()
  ct_map <- function(values, id) {
    mapping <- unlist(ct$codelists[[id]], use.names = TRUE)
    unname(mapping[match(as.character(values), names(mapping))])
  }
  dm <- dm_raw |>
    dplyr::transmute(
      STUDYID = as.character(.data$STUDY), DOMAIN = "DM",
      USUBJID = paste(.data$STUDY, .data$PATNUM, sep = "-"), SUBJID = as.character(.data$PATNUM),
      RFSTDTC = demo_iso_date(.data$FIRST_DOSE_DT), RFENDTC = demo_iso_date(.data$LAST_DOSE_DT),
      RFICDTC = demo_iso_date(.data$IC_DT), SITEID = sub("-.*$", "", .data$PATNUM),
      AGE = as.numeric(.data[["IT.AGE"]]), AGEU = "YEARS", SEX = ct_map(.data[["IT.SEX"]], "SEX"),
      RACE = ct_map(.data[["IT.RACE"]], "RACE"), ETHNIC = ct_map(.data[["IT.ETHNIC"]], "ETHNIC"),
      ARMCD = as.character(.data$PLANNED_ARMCD), ARM = as.character(.data$PLANNED_ARM),
      ACTARMCD = as.character(.data$ACTUAL_ARMCD), ACTARM = as.character(.data$ACTUAL_ARM),
      COUNTRY = as.character(.data$COUNTRY)
    )
  reference <- stats::setNames(dm$RFSTDTC, dm$USUBJID)
  ae <- ae_raw |>
    dplyr::mutate(
      STUDYID = as.character(.data$STUDY), DOMAIN = "AE",
      USUBJID = paste(.data$STUDY, .data$PATNUM, sep = "-"),
      AETERM = as.character(.data[["IT.AETERM"]]), AEDECOD = as.character(.data$AEDECOD),
      AEBODSYS = as.character(.data$AEBODSYS), AESEV = ct_map(.data[["IT.AESEV"]], "AESEV"),
      AESER = ct_map(.data[["IT.AESER"]], "NY"), AEREL = ct_map(.data[["IT.AEREL"]], "AEREL"),
      AEOUT = ct_map(.data$AEOUTCOME, "AEOUT"), AESHOSP = ct_map(.data[["IT.AESHOSP"]], "NY"),
      AESTDTC = demo_iso_date(.data[["IT.AESTDAT"]]), AEENDTC = demo_iso_date(.data[["IT.AEENDAT"]]),
      AESTDY = demo_study_day(.data$AESTDTC, reference[.data$USUBJID]),
      AEENDY = demo_study_day(.data$AEENDTC, reference[.data$USUBJID]),
      AEENRF = ifelse(is.na(.data$AEENDTC), "ONGOING", NA_character_)
    ) |>
    dplyr::group_by(.data$STUDYID, .data$USUBJID) |>
    dplyr::mutate(AESEQ = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::select(STUDYID, DOMAIN, USUBJID, AESEQ, AETERM, AEDECOD, AEBODSYS, AESEV, AESER, AEREL, AEOUT, AESHOSP, AESTDTC, AEENDTC, AESTDY, AEENDY, AEENRF)
  test_map <- tibble::tribble(
    ~source, ~VSTESTCD, ~VSTEST, ~unit,
    "SYS_BP", "SYSBP", "Systolic Blood Pressure", "mmHg",
    "DIA_BP", "DIABP", "Diastolic Blood Pressure", "mmHg",
    "PULSE", "PULSE", "Pulse Rate", "beats/min",
    "IT.HEIGHT_VSORRES", "HEIGHT", "Height", "cm",
    "IT.WEIGHT", "WEIGHT", "Weight", "kg",
    "IT.TEMP", "TEMP", "Temperature", "C"
  )
  vs_rows <- lapply(seq_len(nrow(test_map)), function(index) {
    item <- test_map[index, ]
    vs_raw |>
      dplyr::transmute(
        STUDYID = as.character(.data$STUDY), DOMAIN = "VS",
        USUBJID = paste(.data$STUDY, .data$PATNUM, sep = "-"),
        VSTESTCD = item$VSTESTCD, VSTEST = item$VSTEST, VSPOS = as.character(.data$SUBPOS),
        VSORRES = as.character(.data[[item$source]]), VSORRESU = item$unit,
        VSSTRESC = as.character(.data[[item$source]]), VSSTRESN = as.numeric(.data[[item$source]]), VSSTRESU = item$unit,
        VISITNUM = ifelse(.data$INSTANCE == "Screening 1", -1, 2), VISIT = as.character(.data$INSTANCE),
        VSDTC = demo_iso_date(.data$VTLD, "%d-%b-%Y"), VSDY = demo_study_day(.data$VSDTC, reference[.data$USUBJID])
      ) |>
      dplyr::filter(!is.na(.data$VSORRES) & nzchar(.data$VSORRES))
  })
  vs <- dplyr::bind_rows(vs_rows) |>
    dplyr::arrange(.data$STUDYID, .data$USUBJID, .data$VSDTC, .data$VSTESTCD) |>
    dplyr::group_by(.data$STUDYID, .data$USUBJID) |>
    dplyr::mutate(VSSEQ = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::select(STUDYID, DOMAIN, USUBJID, VSSEQ, VSTESTCD, VSTEST, VSPOS, VSORRES, VSORRESU, VSSTRESC, VSSTRESN, VSSTRESU, VISITNUM, VISIT, VSDTC, VSDY)
  metadata <- load_metadata()
  list(
    DM = apply_sdtm_labels(dm, "DM", metadata),
    AE = apply_sdtm_labels(ae, "AE", metadata),
    VS = apply_sdtm_labels(vs, "VS", metadata)
  )
}

demo_analysis_config <- function(base) {
  config <- load_project_config()
  config$paths$analysis_plan_frozen <- ""
  config <- configure_output_paths(config, base)
  config$paths$analysis_plan_frozen <- ""
  config
}
