test_that("Pinnacle 21 4.2.0 环境检查通过", {
  p21 <- load_p21_config()
  required_paths <- unlist(p21$community[c("executable", "java", "client_jar", "config_root")], use.names = FALSE)
  skip_if_not(
    all(nzchar(required_paths)) && all(file.exists(required_paths)),
    "需要先创建 config/p21.local.yml 并安装 Pinnacle 21 Community"
  )
  checks <- doctor_p21(load_project_config("advanced"))
  expect_true(all(checks$passed))
})

test_that("Pinnacle 21 共享配置不包含本机路径", {
  shared <- yaml::read_yaml(trace_path("config", "p21.yml"))
  expect_identical(shared$community$executable, "")
  expect_identical(shared$community$java, "")
  expect_identical(shared$community$client_jar, "")
  expect_identical(shared$community$config_root, "")
})

test_that("Pinnacle 21 路径可以通过环境变量覆盖", {
  overrides <- c(
    TRACE_SDTM_P21_EXECUTABLE = "C:/portable/p21.exe",
    TRACE_SDTM_P21_JAVA = "C:/portable/java.exe",
    TRACE_SDTM_P21_CLIENT_JAR = "C:/portable/p21-client.jar",
    TRACE_SDTM_P21_CONFIG_ROOT = "C:/portable/configs"
  )
  old_values <- Sys.getenv(names(overrides), unset = NA_character_)
  on.exit({
    for (variable in names(overrides)) {
      if (is.na(old_values[[variable]])) {
        Sys.unsetenv(variable)
      } else {
        do.call(Sys.setenv, stats::setNames(list(old_values[[variable]]), variable))
      }
    }
  }, add = TRUE)
  do.call(Sys.setenv, as.list(overrides))
  p21 <- load_p21_config(local_path = "")
  expect_identical(p21$community$executable, "C:/portable/p21.exe")
  expect_identical(p21$community$java, "C:/portable/java.exe")
  expect_identical(p21$community$client_jar, "C:/portable/p21-client.jar")
  expect_identical(p21$community$config_root, "C:/portable/configs")
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
