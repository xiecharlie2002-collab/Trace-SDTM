# Network-independent approved mapping fixture for the public demonstration ---

demo_source_ref <- function(dataset, variable) {
  list(
    ref_id = paste0("ref_", dataset, "_", gsub("[^A-Za-z0-9]", "_", variable)),
    dataset = dataset, variable = variable
  )
}

demo_mapping_task <- function(id, domain, transform_id, targets, sources = list(), parameters = list(),
                              assembly_group_id = id, depends_on = character(), output_kind = "variables") {
  list(
    task_id = id, assembly_group_id = assembly_group_id,
    clinical_action = paste("Map", paste(targets, collapse = ", "), "for", domain),
    candidate_target_domains = as.list(domain), target_domain = domain,
    source_refs = sources, depends_on = as.list(depends_on),
    expected_cardinality = if (identical(transform_id, "transpose_findings")) "one_record_to_many_records" else "one_record_to_one_record",
    required = TRUE, evidence = list("TraceSDTM 0.7 simulated-data fixture"), uncertainties = list(),
    status = "approved",
    semantic_decision = list(target_variables = as.list(targets), output_kind = output_kind),
    approved_plan = list(steps = list(list(
      transform_id = transform_id, target_variables = as.list(targets),
      source_ref_ids = as.list(vapply(sources, function(ref) as.character(ref$ref_id), character(1))),
      parameters = parameters
    ))),
    review = list(decision = "approve", reviewer = "TraceSDTM fixture", reviewed_at = "2026-09-05T00:00:00Z")
  )
}

demo_simple_task <- function(id, domain, target, transform, dataset = NULL, variable = NULL, parameters = list()) {
  sources <- if (is.null(dataset)) list() else list(demo_source_ref(dataset, variable))
  demo_mapping_task(id, domain, transform, target, sources, parameters)
}

demo_three_domain_approved_mapping <- function() {
  tasks <- list(
    demo_simple_task("DM_STUDYID", "DM", "STUDYID", "assign_no_ct", "dm_raw", "STUDY"),
    demo_simple_task("DM_DOMAIN", "DM", "DOMAIN", "hardcode_no_ct", parameters = list(value = "DM")),
    demo_mapping_task("DM_USUBJID", "DM", "derive_usubjid", "USUBJID", list(
      demo_source_ref("dm_raw", "STUDY"), demo_source_ref("dm_raw", "PATNUM")
    ), list(separator = "-")),
    demo_simple_task("DM_SUBJID", "DM", "SUBJID", "assign_no_ct", "dm_raw", "PATNUM"),
    demo_simple_task("DM_RFSTDTC", "DM", "RFSTDTC", "to_iso8601_date", "dm_raw", "FIRST_DOSE_DT", list(formats = list("m/d/y"))),
    demo_simple_task("DM_RFENDTC", "DM", "RFENDTC", "to_iso8601_date", "dm_raw", "LAST_DOSE_DT", list(formats = list("m/d/y"))),
    demo_simple_task("DM_RFICDTC", "DM", "RFICDTC", "to_iso8601_date", "dm_raw", "IC_DT", list(formats = list("m/d/y"))),
    demo_simple_task("DM_SITEID", "DM", "SITEID", "extract_delimited_part", "dm_raw", "PATNUM", list(separator = "-", position = 1L)),
    demo_simple_task("DM_AGE", "DM", "AGE", "assign_no_ct", "dm_raw", "IT.AGE"),
    demo_simple_task("DM_AGEU", "DM", "AGEU", "hardcode_ct", parameters = list(value = "YEARS", codelist_id = "AGEU")),
    demo_simple_task("DM_SEX", "DM", "SEX", "assign_ct", "dm_raw", "IT.SEX", list(codelist_id = "SEX")),
    demo_simple_task("DM_RACE", "DM", "RACE", "assign_ct", "dm_raw", "IT.RACE", list(codelist_id = "RACE")),
    demo_simple_task("DM_ETHNIC", "DM", "ETHNIC", "assign_ct", "dm_raw", "IT.ETHNIC", list(codelist_id = "ETHNIC")),
    demo_simple_task("DM_ARMCD", "DM", "ARMCD", "assign_no_ct", "dm_raw", "PLANNED_ARMCD"),
    demo_simple_task("DM_ARM", "DM", "ARM", "assign_no_ct", "dm_raw", "PLANNED_ARM"),
    demo_simple_task("DM_ACTARMCD", "DM", "ACTARMCD", "assign_no_ct", "dm_raw", "ACTUAL_ARMCD"),
    demo_simple_task("DM_ACTARM", "DM", "ACTARM", "assign_no_ct", "dm_raw", "ACTUAL_ARM"),
    demo_simple_task("DM_COUNTRY", "DM", "COUNTRY", "assign_no_ct", "dm_raw", "COUNTRY"),

    demo_simple_task("AE_STUDYID", "AE", "STUDYID", "assign_no_ct", "ae_raw", "STUDY"),
    demo_simple_task("AE_DOMAIN", "AE", "DOMAIN", "hardcode_no_ct", parameters = list(value = "AE")),
    demo_mapping_task("AE_USUBJID", "AE", "derive_usubjid", "USUBJID", list(
      demo_source_ref("ae_raw", "STUDY"), demo_source_ref("ae_raw", "PATNUM")
    ), list(separator = "-")),
    demo_simple_task("AE_AETERM", "AE", "AETERM", "assign_no_ct", "ae_raw", "IT.AETERM"),
    demo_simple_task("AE_AEDECOD", "AE", "AEDECOD", "assign_no_ct", "ae_raw", "AEDECOD"),
    demo_simple_task("AE_AEBODSYS", "AE", "AEBODSYS", "assign_no_ct", "ae_raw", "AEBODSYS"),
    demo_simple_task("AE_AESEV", "AE", "AESEV", "assign_ct", "ae_raw", "IT.AESEV", list(codelist_id = "AESEV")),
    demo_simple_task("AE_AESER", "AE", "AESER", "assign_ct", "ae_raw", "IT.AESER", list(codelist_id = "NY")),
    demo_simple_task("AE_AEREL", "AE", "AEREL", "assign_ct", "ae_raw", "IT.AEREL", list(codelist_id = "AEREL")),
    demo_simple_task("AE_AEOUT", "AE", "AEOUT", "assign_ct", "ae_raw", "AEOUTCOME", list(codelist_id = "AEOUT")),
    demo_simple_task("AE_AESHOSP", "AE", "AESHOSP", "assign_ct", "ae_raw", "IT.AESHOSP", list(codelist_id = "NY")),
    demo_simple_task("AE_AESTDTC", "AE", "AESTDTC", "to_iso8601_date", "ae_raw", "IT.AESTDAT", list(formats = list("m/d/y"))),
    demo_simple_task("AE_AEENDTC", "AE", "AEENDTC", "to_iso8601_date", "ae_raw", "IT.AEENDAT", list(formats = list("m/d/y"))),
    demo_simple_task("AE_AEENRF", "AE", "AEENRF", "derive_ongoing_flag", "ae_raw", "IT.AEENDAT", list(ongoing_value = "ONGOING", affirmative_values = list())),
    demo_mapping_task("AE_AESEQ", "AE", "derive_sequence", "AESEQ", parameters = list(
      record_variables = as.list(c("STUDYID", "USUBJID", "AESTDTC", "AETERM", ".SOURCE_ROW")), start_at = 1L
    )),
    demo_mapping_task("AE_AESTDY", "AE", "derive_study_day", "AESTDY", parameters = list(target_date = "AESTDTC", reference_date = "RFSTDTC")),
    demo_mapping_task("AE_AEENDY", "AE", "derive_study_day", "AEENDY", parameters = list(target_date = "AEENDTC", reference_date = "RFSTDTC")),

    demo_simple_task("VS_STUDYID", "VS", "STUDYID", "assign_no_ct", "vs_raw", "STUDY"),
    demo_simple_task("VS_DOMAIN", "VS", "DOMAIN", "hardcode_no_ct", parameters = list(value = "VS")),
    demo_mapping_task("VS_USUBJID", "VS", "derive_usubjid", "USUBJID", list(
      demo_source_ref("vs_raw", "STUDY"), demo_source_ref("vs_raw", "PATNUM")
    ), list(separator = "-")),
    demo_simple_task("VS_VSPOS", "VS", "VSPOS", "assign_ct", "vs_raw", "SUBPOS", list(codelist_id = "VSPOS")),
    demo_simple_task("VS_VISIT", "VS", "VISIT", "assign_no_ct", "vs_raw", "INSTANCE"),
    demo_simple_task("VS_VISITNUM", "VS", "VISITNUM", "derive_visitnum", "vs_raw", "INSTANCE", list(visit_map_id = "trace_visits_v1")),
    demo_simple_task("VS_VSDTC", "VS", "VSDTC", "to_iso8601_date", "vs_raw", "VTLD", list(formats = list("dd-mmm-yyyy")))
  )
  findings <- list(
    list(id = "SYSBP", variable = "SYS_BP", name = "Systolic Blood Pressure", unit = "mmHg"),
    list(id = "DIABP", variable = "DIA_BP", name = "Diastolic Blood Pressure", unit = "mmHg"),
    list(id = "PULSE", variable = "PULSE", name = "Pulse Rate", unit = "beats/min"),
    list(id = "HEIGHT", variable = "IT.HEIGHT_VSORRES", name = "Height", unit = "cm"),
    list(id = "WEIGHT", variable = "IT.WEIGHT", name = "Weight", unit = "kg"),
    list(id = "TEMP", variable = "IT.TEMP", name = "Temperature", unit = "C")
  )
  for (finding in findings) {
    id <- paste0("VS_", finding$id)
    source <- list(demo_source_ref("vs_raw", finding$variable))
    task <- demo_mapping_task(
      id, "VS", "transpose_findings", c("VSTESTCD", "VSTEST", "VSORRES", "VSORRESU"),
      source, list(test_code = finding$id, test_name = finding$name, original_unit = finding$unit, unit_source = NULL)
    )
    task$semantic_decision$target_variables <- as.list(c("VSTESTCD", "VSTEST", "VSORRES", "VSORRESU", "VSSTRESC", "VSSTRESN", "VSSTRESU"))
    task$approved_plan$steps[[2L]] <- list(
      transform_id = "standardize_unit",
      target_variables = as.list(c("VSSTRESC", "VSSTRESN", "VSSTRESU")),
      source_ref_ids = as.list(vapply(source, function(ref) as.character(ref$ref_id), character(1))),
      parameters = list(conversion_set_id = "vs_standard_v1", target_unit = finding$unit)
    )
    tasks[[length(tasks) + 1L]] <- task
  }
  tasks[[length(tasks) + 1L]] <- demo_mapping_task("VS_VSSEQ", "VS", "derive_sequence", "VSSEQ", parameters = list(
    record_variables = as.list(c("STUDYID", "USUBJID", "VSDTC", "VSTESTCD", ".SOURCE_ROW")), start_at = 1L
  ))
  tasks[[length(tasks) + 1L]] <- demo_mapping_task("VS_VSDY", "VS", "derive_study_day", "VSDY", parameters = list(
    target_date = "VSDTC", reference_date = "RFSTDTC"
  ))
  list(
    schema_version = "0.6",
    specification = list(
      name = "TraceSDTM 0.7 three-domain approved fixture", version = "0.7.0",
      status = "approved", standard = "SDTMIG 3.4",
      approval = list(reviewer = "TraceSDTM fixture", approved_at = "2026-09-05T00:00:00Z")
    ),
    source_catalog = list(
      dm_raw = list(file = "dm_raw.csv", derived = FALSE),
      ae_raw = list(file = "ae_raw.csv", derived = FALSE),
      vs_raw = list(file = "vs_raw.csv", derived = FALSE)
    ),
    domain_sources = list(DM = "dm_raw", AE = "ae_raw", VS = "vs_raw"),
    tasks = tasks
  )
}

write_demo_three_domain_approved_mapping <- function(config = load_project_config()) {
  specification <- demo_three_domain_approved_mapping()
  validate_specification(specification, config, require_approved = TRUE)
  path <- trace_path(config$paths$approved_specification)
  write_yaml(specification, path)
  trace_info("已写入不依赖外部模型的三域批准映射样例：%s", path)
  invisible(specification)
}
