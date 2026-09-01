test_that("三个域可以确定性构建并从 XPT 回读", {
  config <- load_project_config()
  first <- build_sdtm(config)
  first_manifest <- readr::read_csv(trace_path(config$paths$manifest_dir, "dataset_manifest.csv"), show_col_types = FALSE)
  second <- build_sdtm(config)
  second_manifest <- readr::read_csv(trace_path(config$paths$manifest_dir, "dataset_manifest.csv"), show_col_types = FALSE)

  expect_setequal(names(first), c("DM", "AE", "VS"))
  expect_equal(nrow(first$DM), 6L)
  expect_equal(nrow(first$AE), 8L)
  expect_equal(nrow(first$VS), 60L)
  expect_equal(first_manifest$data_sha256, second_manifest$data_sha256)

  reread <- load_built_datasets(config)
  expect_equal(nrow(reread$VS), nrow(second$VS))
  expect_setequal(unique(reread$VS$VSTESTCD), c("SYSBP", "DIABP", "PULSE", "HEIGHT", "WEIGHT", "TEMP"))
})

test_that("正常数据通过本地检查", {
  issues <- validate_local()
  expect_equal(nrow(issues), 0L)
})

test_that("故意错误能够被对应本地规则发现", {
  config <- load_project_config()
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
  config <- load_project_config()
  specification <- load_approved_mapping(config)
  lineage <- readr::read_csv(trace_path(config$paths$lineage_dir, "field_lineage.csv"), show_col_types = FALSE)
  for (domain in names(specification$domains)) {
    raw <- read_raw_csv(trace_path(config$paths$raw_dir, specification$domains[[domain]]$source_file))
    rows <- lineage[lineage$target_domain == domain & !is.na(lineage$source_variables), ]
    sources <- unique(unlist(strsplit(rows$source_variables, " \\| ")))
    expect_true(all(sources %in% names(raw)), info = domain)
  }
})

