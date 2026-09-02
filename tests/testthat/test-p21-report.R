test_that("Pinnacle 21 4.2.0 环境检查通过", {
  checks <- doctor_p21(load_project_config("advanced"))
  expect_true(all(checks$passed))
})

test_that("Pinnacle 21 报告的五类工作表可以解析", {
  config <- load_project_config("advanced")
  report <- trace_path(config$paths$p21_validation_dir, "p21_report.xlsx")
  skip_if_not(file.exists(report), "需要先执行 validate-p21")
  issues <- parse_p21_report(report, config)
  expect_gt(nrow(issues), 0L)
  expect_true(all(c("rule_id", "severity", "issue_class", "validator") %in% names(issues)))
  expect_true(all(issues$severity[nchar(issues$severity_raw) == 0L | is.na(issues$severity_raw)] == "UNSPECIFIED"))
  expect_false(any(issues$issue_class == "generated_domain_defect"))
})

test_that("中文文本和路径文本不会被审核辅助函数破坏", {
  expect_identical(cell_text("路径含中文"), "路径含中文")
})

test_that("可按需执行 Pinnacle 21 完整外部验证", {
  skip_if_not(identical(Sys.getenv("TRACE_SDTM_RUN_P21_TEST"), "true"), "设置 TRACE_SDTM_RUN_P21_TEST=true 后执行")
  issues <- validate_p21(load_project_config("advanced"))
  expect_false(any(issues$issue_class == "generated_domain_defect"))
})
