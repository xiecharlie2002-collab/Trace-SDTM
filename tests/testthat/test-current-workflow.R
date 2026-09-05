test_that("公共资源和转换注册表可以加载", {
  config <- load_project_config()
  expect_true(file.exists(trace_path(config$paths$controlled_terminology)))
  expect_true(file.exists(trace_path(config$paths$unit_conversions)))
  registry <- load_transform_registry(config)
  expect_length(registry$transforms, 23L)
})

test_that("通用项目可导入、冻结和生成画像", {
  project_root <- file.path(tempdir(), paste0("trace-studio-", sample.int(1e8, 1L)))
  dir.create(project_root, recursive = TRUE)
  previous_home <- Sys.getenv("TRACE_SDTM_STUDIO_HOME", unset = NA_character_)
  on.exit({
    if (is.na(previous_home)) Sys.unsetenv("TRACE_SDTM_STUDIO_HOME") else Sys.setenv(TRACE_SDTM_STUDIO_HOME = previous_home)
  }, add = TRUE)
  Sys.setenv(TRACE_SDTM_STUDIO_HOME = project_root)

  studio_create_project(
    "smoke-project", "最小流程", "TRACE001",
    "模拟受试者数据，仅生成 DM。", target_domains = "DM"
  )
  imported <- studio_import_sources(
    "smoke-project", trace_path("data", "raw", "dm_raw.csv"), "dm_raw.csv"
  )
  expect_equal(imported$dataset, "dm_raw")
  expect_equal(imported$rows, 6L)

  run_id <- studio_create_run("smoke-project")
  config <- studio_load_run_config("smoke-project", run_id)
  dictionary <- profile_sources(config)
  expect_equal(nrow(dictionary), 14L)
  expect_identical(as.character(load_task_specification(config)$schema_version), "0.6")
})

test_that("工作台只显示五个主步骤", {
  page <- paste(htmltools::renderTags(trace_studio_ui())$html, collapse = "")
  labels <- c("1 项目与数据", "2 任务确认", "3 映射生成", "4 审查批准", "5 结果")
  expect_true(all(vapply(labels, grepl, logical(1), x = page, fixed = TRUE)))
})
