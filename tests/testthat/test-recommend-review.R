test_that("参考种子覆盖至少 25 个映射任务且有明确来源", {
  recommendations <- seed_recommendations()
  expect_gte(nrow(recommendations), 25L)
  expect_true(all(recommendations$provenance == "reference_seed"))
  expect_true(all(recommendations$review_required))
})

test_that("未知模型输出被严格拒绝", {
  config <- load_project_config()
  metadata <- load_metadata(config)
  tasks <- dplyr::filter(flatten_mapping_tasks(), include_in_recommendation)
  candidate <- data.frame(
    mapping_id = tasks$mapping_id[1],
    candidate_rank = 1L,
    source_dataset = tasks$source_dataset[1],
    source_variable = tasks$source_variable[1],
    target_domain = tasks$target_domain[1],
    target_variable = tasks$target_variable[1],
    target_value = "",
    mapping_type = tasks$mapping_type[1],
    transform_id = "execute_arbitrary_code",
    transform_parameters = "{}",
    recommendation_score = 0.5,
    reason = "test",
    uncertainties = "test",
    review_required = TRUE
  )
  expect_error(validate_recommendations(candidate, config, tasks, metadata), "不允许")
})

test_that("没有批准规格时构建入口被阻止", {
  config <- load_project_config()
  config$paths$approved_specification <- "specs/not_created_for_test.yml"
  expect_error(load_approved_mapping(config), "尚未生成")
})

test_that("密钥从日志文本中删除", {
  old <- Sys.getenv("TRACE_SDTM_API_KEY", unset = NA_character_)
  on.exit({
    if (is.na(old)) Sys.unsetenv("TRACE_SDTM_API_KEY") else Sys.setenv(TRACE_SDTM_API_KEY = old)
  }, add = TRUE)
  Sys.setenv(TRACE_SDTM_API_KEY = "test-secret-value")
  expect_false(grepl("test-secret-value", sanitize_for_log("Bearer test-secret-value"), fixed = TRUE))
})

