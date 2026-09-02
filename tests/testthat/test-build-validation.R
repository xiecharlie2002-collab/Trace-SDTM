test_that("高级场景三个域可重复构建并从 XPT 回读", {
  config <- load_project_config("advanced")
  if (!file.exists(trace_path(config$paths$approved_specification))) {
    seed_recommendations(config)
    approve_mapping(config)
  }
  first <- build_sdtm(config)
  first_manifest <- readr::read_csv(trace_path(config$paths$manifest_dir, "dataset_manifest.csv"), show_col_types = FALSE)
  second <- build_sdtm(config)
  second_manifest <- readr::read_csv(trace_path(config$paths$manifest_dir, "dataset_manifest.csv"), show_col_types = FALSE)

  expect_setequal(names(first), c("DM", "AE", "VS"))
  expect_equal(nrow(first$DM), 4L)
  expect_equal(nrow(first$AE), 5L)
  expect_equal(nrow(first$VS), 44L)
  expect_equal(first_manifest$data_sha256, second_manifest$data_sha256)

  reread <- load_built_datasets(config)
  expect_equal(nrow(reread$VS), nrow(second$VS))
  expect_setequal(unique(reread$VS$VSTESTCD), c("SYSBP", "DIABP", "PULSE", "HEIGHT", "WEIGHT", "TEMP"))
})

test_that("高级参考日期、不完整日期、单位和基线标志符合预期", {
  config <- load_project_config("advanced")
  datasets <- load_built_datasets(config)
  expect_equal(as.character(datasets$DM$RFSTDTC), c("2025-01-05T08:00", "2025-01-05T09:00", "2025-01-06T08:30", "2025-01-06T10:30"))
  expect_true(all(is.na(datasets$AE$AESTDY[datasets$AE$AESTDTC %in% c("2025-01", "2025")])))
  converted <- dplyr::filter(datasets$VS, VSORRES %in% c("70", "180", "98.6"))
  expect_true(all(c("177.8", "81.6", "37.0") %in% converted$VSSTRESC))
  expect_equal(sum(datasets$VS$VSBLFL == "Y", na.rm = TRUE), 24L)
})

test_that("正常高级数据通过本地检查", {
  issues <- validate_local(load_project_config("advanced"))
  expect_equal(nrow(issues), 0L)
})

test_that("故意错误能够被对应本地规则发现", {
  config <- load_project_config("advanced")
  metadata <- load_metadata(config)
  datasets <- load_built_datasets(config)
  lineage <- readr::read_csv(trace_path(config$paths$lineage_dir, "field_lineage.csv"), show_col_types = FALSE)

  bad_dm <- datasets$DM
  bad_dm$SEX[1] <- "INVALID"
  sex_issues <- validate_one_domain(bad_dm, "DM", metadata, lineage, "negative_test")
  expect_true("LOCAL006" %in% sex_issues$rule_id)

  bad_ae <- dplyr::bind_rows(datasets$AE, datasets$AE[1, ])
  duplicate_issues <- validate_one_domain(bad_ae, "AE", metadata, lineage, "negative_test")
  expect_true("LOCAL003" %in% duplicate_issues$rule_id)
})

test_that("每条来源追溯可以解析到原始字段", {
  config <- load_project_config("advanced")
  lineage <- readr::read_csv(trace_path(config$paths$lineage_dir, "field_lineage.csv"), show_col_types = FALSE)
  expect_true(all(c("concept_id", "source_datasets", "source_fields", "join_rule", "transform_id", "reviewer", "records_created") %in% names(lineage)))
  expect_true(any(lineage$transform_id == "merge_sources" & nzchar(lineage$join_rule)))
  expect_true(any(lineage$transform_id == "standardize_unit" & lineage$unit_conversion_version == "1.0.0"))
})
