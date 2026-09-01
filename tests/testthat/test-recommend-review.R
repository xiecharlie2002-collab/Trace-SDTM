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

test_that("真实推荐任务按固定大小分批且不遗漏", {
  tasks <- dplyr::filter(flatten_mapping_tasks(), include_in_recommendation)
  batches <- split_recommendation_tasks(tasks, batch_size = 4L)
  expect_equal(length(batches), 10L)
  expect_equal(sum(vapply(batches, nrow, integer(1))), nrow(tasks))
  expect_true(all(vapply(batches, nrow, integer(1)) <= 4L))
  expect_equal(unlist(lapply(batches, `[[`, "mapping_id"), use.names = FALSE), tasks$mapping_id)
})

test_that("盲评提示不向模型泄露目标答案", {
  tasks <- dplyr::filter(flatten_mapping_tasks(), include_in_recommendation)
  blind <- blind_recommendation_tasks(tasks)
  expect_identical(names(blind), c("mapping_id", "source_dataset", "source_variable"))
  expect_false(any(c("target_domain", "target_variable", "mapping_type", "transform_id") %in% names(blind)))
  prompt <- recommendation_prompt(tibble::tibble(), tasks[1, ], load_metadata(), load_project_config())
  expect_match(prompt, "允许的映射类型", fixed = TRUE)
  expect_match(prompt, "允许的转换函数", fixed = TRUE)
})

test_that("实验编号将全部生成物隔离到独立目录", {
  config <- yaml::read_yaml(trace_path("config", "project.yml"))
  routed <- apply_experiment_paths(config, "deepseek-test")
  expect_match(routed$paths$recommendation_dir, "output/experiments/deepseek-test", fixed = TRUE)
  expect_match(routed$paths$approved_specification, "output/experiments/deepseek-test", fixed = TRUE)
  expect_error(apply_experiment_paths(config, "../outside"), "只能包含")
})

test_that("模型参数同时接受 JSON 对象和 JSON 字符串", {
  base <- list(
    mapping_id = "DM001", candidate_rank = 1L, source_dataset = "dm_raw",
    source_variable = "STUDYID", target_domain = "DM", target_variable = "STUDYID",
    target_value = "", mapping_type = "direct", transform_id = "direct_map",
    recommendation_score = 0.9, reason = "test", uncertainties = "none",
    review_required = TRUE
  )
  object_record <- c(base, list(transform_parameters = list(source = "STUDYID")))
  string_record <- c(base, list(candidate_rank = 2L, transform_parameters = '{"source":"STUDYID"}'))
  parsed <- parse_recommendation_records(list(object_record, string_record))
  expect_equal(nrow(parsed), 2L)
  expect_true(all(vapply(parsed$transform_parameters, function(x) is.list(from_json_text(x)), logical(1))))
})

test_that("金标准审核区分核心映射和函数参数契约", {
  expected <- dplyr::filter(flatten_mapping_tasks(), mapping_id == "DM005")
  candidate <- expected
  expect_length(mapping_mismatch_fields(candidate, expected), 0L)
  candidate$transform_parameters <- '{"source_format":"MM/DD/YYYY"}'
  expect_identical(mapping_mismatch_fields(candidate, expected), "transform_parameters")
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
