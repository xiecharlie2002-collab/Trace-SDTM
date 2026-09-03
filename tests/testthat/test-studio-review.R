test_that("结构化网页审核与Excel路径共享同一批准校验", {
  studio_with_test_home({
    project_id <- studio_prepare_project("basic")
    run_id <- studio_create_run(project_id, "test")
    config <- studio_load_run_config(project_id, run_id)
    assembled <- studio_mock_assembled_from_gold(config, "basic")
    write_json(assembled, file.path(config$paths$recommendation_dir, "assembled_recommendations.json"))
    studio_initialize_review(config, overwrite = TRUE)
    for (id in names(studio_read_review(config)$tasks)) {
      studio_save_review_decision(config, id, "accept", "reviewer-01", 1L, "测试审核")
    }
    web_spec <- studio_approve_review(config, "reviewer-01")
    workbook <- file.path(config$paths$review_dir, "mapping_review.xlsx")
    review <- openxlsx::read.xlsx(workbook, sheet = "Task Review", check.names = FALSE)
    candidates <- openxlsx::read.xlsx(workbook, sheet = "Candidate Plans", check.names = FALSE)
    final_steps <- openxlsx::read.xlsx(workbook, sheet = "Final Steps", check.names = FALSE)
    excel_spec <- approved_specification_from_review_tables_v04(review, candidates, final_steps, config, "reviewer-01", file_sha256(workbook))
    web_steps <- lapply(web_spec$tasks, function(task) task$approved_plan$steps)
    excel_steps <- lapply(excel_spec$tasks, function(task) task$approved_plan$steps)
    expect_equal(web_steps, excel_steps)
    expect_equal(web_spec$schema_version, "0.4")
    expect_true(file.exists(file.path(config$paths$review_dir, "studio_approval.yml")))
  })
})

test_that("Studio阻止默认审核者、必需任务拒绝和非法修改", {
  studio_with_test_home({
    project_id <- studio_prepare_project("basic")
    run_id <- studio_create_run(project_id, "test")
    config <- studio_load_run_config(project_id, run_id)
    assembled <- studio_mock_assembled_from_gold(config, "basic")
    write_json(assembled, file.path(config$paths$recommendation_dir, "assembled_recommendations.json"))
    studio_initialize_review(config, overwrite = TRUE)
    expect_error(studio_save_review_decision(config, "DM_STUDY_IDENTIFIER", "accept", "demo", 1L), "默认演示")
    expect_error(studio_save_review_decision(config, "DM_STUDY_IDENTIFIER", "reject", "reviewer-01"), "不能拒绝")
    expect_error(studio_save_review_decision(config, "DM_STUDY_IDENTIFIER", "modify", "reviewer-01", modified_step = list(
      transform_id = "unknown_transform", source_ref_ids = list("dm_basic__STUDY"), target_variables = list("STUDYID"), parameters = list()
    )), "未登记的转换函数")
  })
})

test_that("递归参数表单支持对象、数组、枚举、数值、逻辑和空值", {
  schema <- list(type = "object", required = list("mode", "items"), properties = list(
    mode = list(type = "string", enum = list("A", "B")),
    items = list(type = "array", minItems = 1, items = list(type = "integer")),
    flag = list(type = "boolean"),
    condition = list(type = "object", properties = list(operator = list(type = "string"), compare_to = list(type = list("string", "null"))))
  ))
  prefix <- "schema_test"
  mock <- list()
  mock[[studio_input_id(prefix, "parameters.mode")]] <- "B"
  mock[[paste0(studio_input_id(prefix, "parameters.items"), "__count")]] <- 2L
  mock[[studio_input_id(prefix, "parameters.items[1]")]] <- 3
  mock[[studio_input_id(prefix, "parameters.items[2]")]] <- 4
  mock[[studio_input_id(prefix, "parameters.flag")]] <- TRUE
  mock[[studio_input_id(prefix, "parameters.condition.operator")]] <- "equals"
  mock[[paste0(studio_input_id(prefix, "parameters.condition.compare_to"), "__null")]] <- TRUE
  result <- studio_schema_value(schema, prefix, mock)
  expect_equal(result$mode, "B")
  expect_equal(unlist(result$items), c(3L, 4L))
  expect_true(result$flag)
  expect_null(result$condition$compare_to)
})

test_that("证据包保留目录结构并排除原始输入与字段示例", {
  studio_with_test_home({
    project_id <- studio_prepare_project("basic")
    run_id <- studio_create_run(project_id, "test")
    config <- studio_load_run_config(project_id, run_id)
    profile_sources(config)
    assembled <- studio_mock_assembled_from_gold(config, "basic")
    write_json(assembled, file.path(config$paths$recommendation_dir, "assembled_recommendations.json"))
    studio_initialize_review(config, overwrite = TRUE)
    for (id in names(studio_read_review(config)$tasks)) studio_save_review_decision(config, id, "accept", "reviewer-01", 1L)
    studio_approve_review(config, "reviewer-01")
    archive <- studio_export_evidence(config, secrets = "unit-test-secret-never-write")
    listing <- zip::zip_list(archive)$filename
    expect_true("specs/approved_mapping.yml" %in% listing)
    expect_true("manifests/source_dictionary_metadata.csv" %in% listing)
    expect_false(any(grepl("(^|/)inputs/|source_dictionary\\.csv$", listing)))
    manifest <- read_json_file(file.path(config$paths$manifest_dir, "evidence_manifest.json"))
    manifest_paths <- vapply(manifest$files, function(item) item$path, character(1))
    expect_false("manifests/evidence_manifest.json" %in% manifest_paths)
    expect_true(all(vapply(manifest$files, function(item) {
      path <- file.path(config$studio$run_path, item$path)
      file.exists(path) && identical(file_sha256(path), as.character(item$sha256))
    }, logical(1))))
    metadata <- readr::read_csv(file.path(config$paths$manifest_dir, "source_dictionary_metadata.csv"), show_col_types = FALSE)
    expect_false("example_values" %in% names(metadata))
  })
})
