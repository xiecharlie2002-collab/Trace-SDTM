test_that("v0.4 种子生成原子审核工作簿和参数来源", {
  config <- load_project_config("basic")
  result <- run_recommendation_v04(config, provider = "seed")
  path <- create_review_workbook_v04(result$assembled, config, preapprove = TRUE)
  expect_true(file.exists(path))
  expect_setequal(
    openxlsx::getSheetNames(path),
    c("Instructions", "Task Review", "Target Decisions", "Function Skeletons",
      "Parameter Resolution", "Candidate Plans", "Final Steps", "Source Context", "Transform Catalog")
  )
  parameters <- openxlsx::read.xlsx(path, sheet = "Parameter Resolution", check.names = FALSE)
  expect_true(all(parameters$source %in% c("policy", "registry", "resource", "derived", "model", "reviewer")))
})

test_that("旧规格不会被v0.4静默批准或构建", {
  config <- load_project_config("advanced")
  old <- yaml::read_yaml(trace_path("specs", "v0.2", "advanced_concepts.yml"))
  expect_error(validate_specification(old, config), "只接受 schema_version 0.4")
})

test_that("高级v0.4批准规格可重复构建并通过本地检查", {
  config <- load_project_config("advanced")
  if (!file.exists(trace_path(config$paths$approved_specification))) {
    result <- run_recommendation_v04(config, provider = "seed")
    create_review_workbook_v04(result$assembled, config, preapprove = TRUE)
    approve_mapping_v04(config)
  }
  first <- build_sdtm(config)
  second <- build_sdtm(config)
  expect_equal(vapply(first, nrow, integer(1)), c(DM = 4L, AE = 5L, VS = 44L))
  expect_identical(lapply(first, data_sha256), lapply(second, data_sha256))
  expect_equal(nrow(validate_local(config)), 0L)
  lineage <- readr::read_csv(trace_path(config$paths$lineage_dir, "field_lineage.csv"), show_col_types = FALSE)
  expect_true(all(c("task_id", "assembly_group_id", "concept_id") %in% names(lineage)))
  expect_true(all(c("VS_HEIGHT_RECORD", "VS_HEIGHT_STANDARDIZATION") %in% lineage$task_id))
})

test_that("项目报告使用v0.4三阶段口径", {
  config <- load_project_config("advanced")
  result <- run_recommendation_v04(config, provider = "seed")
  path <- generate_report(config)
  html <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(html, "TraceSDTM 0.4", fixed = TRUE)
  expect_match(html, "三阶段推荐评价", fixed = TRUE)
  expect_match(html, "原子任务", fixed = TRUE)
  expect_false(grepl("TraceSDTM 0.2 项目报告", html, fixed = TRUE))
  expect_identical(result$run$schema_version, "0.4")
})
