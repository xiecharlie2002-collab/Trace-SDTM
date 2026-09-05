test_that("SDTM 异常场景被对应规则识别", {
  datasets <- demo_sdtm_datasets()
  metadata <- load_metadata()
  run_id <- "negative-test"

  bad_ct <- datasets$DM
  bad_ct$SEX[[1L]] <- "INVALID"
  expect_true("LOCAL006" %in% validate_one_domain(bad_ct, "DM", metadata, NULL, run_id)$rule_id)

  duplicate_sequence <- datasets
  duplicate_sequence$AE$AESEQ[[3L]] <- duplicate_sequence$AE$AESEQ[[2L]]
  expect_true("LOCAL011" %in% validate_sequences(duplicate_sequence, metadata, NULL, run_id)$rule_id)

  reversed <- datasets$AE
  reversed$AEENDTC[[1L]] <- "2024-12-01"
  expect_true("LOCAL012" %in% validate_ae_dates(reversed, metadata, NULL, run_id)$rule_id)

  unlinked <- datasets
  unlinked$AE$USUBJID[[1L]] <- "TRACE001-UNKNOWN"
  expect_true("LOCAL010" %in% validate_subject_links(unlinked, metadata, NULL, run_id)$rule_id)

  bad_unit <- datasets$VS
  bad_unit$VSORRESU[[1L]] <- "unknown-unit"
  expect_true("LOCAL016" %in% validate_vs_pairs(bad_unit, metadata, NULL, run_id)$rule_id)

  partial <- datasets
  partial$AE$AESTDTC[[1L]] <- "2025-01"
  partial$AE$AESTDY[[1L]] <- 1
  expect_true("LOCAL017" %in% validate_partial_date_derivations(partial, metadata, NULL, run_id)$rule_id)
})

test_that("ADaM 缺失关联、重复主键、日期和标志错误均被识别", {
  skip_if_not_installed("admiral", minimum_version = "1.5.0")
  plan <- load_analysis_plan()
  sdtm <- demo_sdtm_datasets()
  adsl <- derive_adsl(sdtm$DM, plan)
  adae <- derive_adae(sdtm$AE, adsl, plan)

  missing_subject <- adae
  missing_subject$USUBJID[[1L]] <- "TRACE001-UNKNOWN"
  expect_true("ADAE002" %in% validate_adae_content(missing_subject, sdtm$AE, adsl, plan)$rule_id)

  duplicate_key <- dplyr::bind_rows(adae, adae[1L, ])
  expect_true("ADAM002" %in% check_analysis_structure(duplicate_key, "ADAE", plan)$rule_id)

  wrong_date <- adae
  wrong_date$AENDT[[1L]] <- wrong_date$ASTDT[[1L]] - 1
  expect_true("ADAE003" %in% validate_adae_content(wrong_date, sdtm$AE, adsl, plan)$rule_id)

  wrong_flag <- adae
  wrong_flag$TRTEMFL[[1L]] <- "N"
  rules <- validate_adae_content(wrong_flag, sdtm$AE, adsl, plan)$rule_id
  expect_true(all(c("ADAE004", "ADAE005") %in% rules))

  wrong_treatment <- adae
  wrong_treatment$TRTAN[[1L]] <- 99
  expect_true("ADAE006" %in% validate_adae_content(wrong_treatment, sdtm$AE, adsl, plan)$rule_id)
})

test_that("批准样例可完成三域 SDTM 与确定性分析全流程", {
  skip_if_not_installed("admiral", minimum_version = "1.5.0")
  skip_if_not_installed("r2rtf", minimum_version = "1.3.1")
  studio_root <- file.path(tempdir(), paste0("trace-e2e-", sample.int(1e8, 1L)))
  previous_home <- Sys.getenv("TRACE_SDTM_STUDIO_HOME", unset = NA_character_)
  on.exit({
    if (is.na(previous_home)) Sys.unsetenv("TRACE_SDTM_STUDIO_HOME") else Sys.setenv(TRACE_SDTM_STUDIO_HOME = previous_home)
  }, add = TRUE)
  Sys.setenv(TRACE_SDTM_STUDIO_HOME = studio_root)
  studio_create_project(
    "analysis-e2e", "三域确定性分析", "TRACE001", "模拟三域数据的自动化验收运行。",
    target_domains = c("DM", "AE", "VS")
  )
  studio_import_sources("analysis-e2e", c(
    trace_path("data", "raw", "dm_raw.csv"), trace_path("data", "raw", "ae_raw.csv"),
    trace_path("data", "raw", "vs_raw.csv")
  ), c("dm_raw.csv", "ae_raw.csv", "vs_raw.csv"))
  studio_import_analysis_plan_v07("analysis-e2e", trace_path("specs", "analysis_plan.yml"))
  run_id <- studio_create_run("analysis-e2e")
  config <- studio_load_run_config("analysis-e2e", run_id)
  specification <- write_demo_three_domain_approved_mapping(config)
  expect_equal(length(specification$tasks), 50L)

  first <- build_sdtm(config)
  first_hash <- vapply(first, data_sha256, character(1))
  expect_equal(vapply(first, nrow, integer(1)), c(DM = 6L, AE = 9L, VS = 60L))
  expect_equal(vapply(first, ncol, integer(1)), c(DM = 18L, AE = 17L, VS = 16L))
  expect_equal(nrow(validate_local(config)), 0L)
  second <- build_sdtm(config)
  expect_identical(vapply(second, data_sha256, character(1)), first_hash)

  adam <- build_adam(config)
  expect_equal(c(nrow(adam$ADSL), nrow(adam$ADAE)), c(6L, 9L))
  expect_equal(nrow(validate_adam(config)), 0L)
  tables <- build_tlf(config)
  expect_equal(length(tables), 2L)
  expect_equal(nrow(validate_tlf(config)), 0L)
  report <- generate_report(config)
  evidence <- studio_export_evidence(config)
  expect_true(file.exists(report))
  expect_true(file.exists(evidence))
  archive_files <- utils::unzip(evidence, list = TRUE)$Name
  expect_true(any(grepl("adam/xpt/adsl.xpt$", archive_files)))
  expect_true(any(grepl("tlf/rtf/t14_3_1_teae.rtf$", archive_files)))
  expect_true(any(grepl("config/analysis_plan.yml$", archive_files)))
  expect_false(any(grepl("(^|/)inputs/", archive_files)))
  expect_identical(file_sha256(config$paths$analysis_plan_frozen), studio_read_run("analysis-e2e", run_id)$analysis_plan_sha256)

  manifest <- readr::read_csv(trace_path(config$paths$manifest_dir, "dataset_manifest.csv"), show_col_types = FALSE)
  expect_equal(manifest$variables, c(18, 17, 16))
  expect_true(all(file.exists(c(
    trace_path(config$paths$adam_xpt_dir, "adsl.xpt"), trace_path(config$paths$adam_xpt_dir, "adae.xpt"),
    trace_path(config$paths$tlf_rtf_dir, "t14_1_1_demographics.rtf"),
    trace_path(config$paths$tlf_rtf_dir, "t14_3_1_teae.rtf")
  ))))
  studio_import_analysis_plan_v07("analysis-e2e", trace_path("specs", "analysis_plan.yml"))
  expect_true(studio_read_run("analysis-e2e", run_id)$stale)
})
