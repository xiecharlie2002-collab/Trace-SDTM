test_that("Studio创建项目时隔离模板资源且不复制金标准", {
  studio_with_test_home({
    project_id <- studio_create_project("study-demo", "Study Demo", "basic", "TRACE001")$project_id
    path <- studio_project_path(project_id)
    expect_true(all(file.exists(file.path(path, c("project.yml", "config/tasks.yml", "config/transform_registry.yml", "config/mapping_policies.yml")))))
    expect_false(any(grepl("gold", list.files(path, recursive = TRUE), ignore.case = TRUE)))
    expect_equal(studio_read_project(project_id)$config_version, "0.5.0")
    expect_error(studio_project_path("../escape", FALSE), "项目编号")
  })
})

test_that("Studio上传、字段绑定和运行快照不会相互覆盖", {
  studio_with_test_home({
    project_id <- studio_prepare_project("basic")
    project <- studio_read_project(project_id)
    expect_equal(project$stages$data_sources, "ready")
    run_one <- studio_create_run(project_id, "test")
    config_one <- studio_load_run_config(project_id, run_one)
    first_hash <- file_sha256(file.path(config_one$paths$raw_dir, "dm_raw.csv"))
    studio_save_policy(project_id, list(study_id = "TRACEB01", subject_separator = "_"), "test")
    expect_true(studio_read_run(project_id, run_one)$stale)
    run_two <- studio_create_run(project_id, "test")
    expect_false(identical(run_one, run_two))
    expect_identical(file_sha256(file.path(config_one$paths$raw_dir, "dm_raw.csv")), first_hash)
    expect_equal(studio_read_project(project_id)$active_run_id, run_two)
  })
})

test_that("CSV预检拒绝重复字段、非UTF-8和越界大小", {
  duplicate <- tempfile(fileext = ".csv")
  writeLines(c("A,A", "1,2"), duplicate, useBytes = TRUE)
  expect_error(studio_validate_utf8_csv(duplicate), "重复字段名")
  latin <- tempfile(fileext = ".csv")
  writeBin(charToRaw(iconv("NAME\ncafé", from = "UTF-8", to = "latin1")), latin)
  expect_error(studio_validate_utf8_csv(latin), "UTF-8")
  valid <- tempfile(fileext = ".csv")
  writeLines(c("A", "1"), valid, useBytes = TRUE)
  expect_error(studio_validate_utf8_csv(valid, max_mb = 0.000001), "上限")
})

test_that("高级模板允许多记录受试者键但检查一记录键唯一性", {
  studio_with_test_home({
    project_id <- studio_prepare_project("advanced")
    bindings <- studio_read_bindings(project_id)
    expect_gt(bindings$datasets$ex_a_raw$key_duplicate_rows, 0L)
    expect_false(bindings$datasets$ex_a_raw$key_uniqueness_required)
    expect_true(bindings$datasets$dm_subject$key_uniqueness_required)
    run_id <- studio_create_run(project_id, "test")
    dictionary <- profile_sources(studio_load_run_config(project_id, run_id))
    expect_equal(nrow(dictionary), 57L)
    expect_true(all(c("AETERM", "AEDECOD", "AEBODSYS") %in% dictionary$source_variable[dictionary$source_dataset == "ae_main"]))
  })
})

test_that("Studio模型请求默认不含示例值且敏感示例始终排除", {
  studio_with_test_home({
    project_id <- studio_prepare_project("basic")
    run_id <- studio_create_run(project_id, "test")
    config <- studio_load_run_config(project_id, run_id)
    profile_sources(config)
    default <- studio_prompt_preview(config, "targets", FALSE)
    expect_false(grepl("Headache|TRACEB01|101-001", default$text))
    allowed <- studio_prompt_preview(config, "targets", TRUE, "ae_basic.AETERM")
    expect_match(allowed$text, "Headache", fixed = TRUE)
    sensitive <- studio_prompt_preview(config, "targets", TRUE, "dm_basic.PATNUM")
    expect_false(grepl("101-001", sensitive$text, fixed = TRUE))
    expect_equal(nchar(default$sha256), 64L)
  })
})

test_that("两个项目的上传、政策和运行文件互不串扰", {
  studio_with_test_home({
    first <- studio_prepare_project("basic")
    second <- studio_prepare_project("basic")
    studio_save_policy(first, list(study_id = "TRACEB01", subject_separator = "_"), "test")
    studio_save_policy(second, list(study_id = "TRACEB01", subject_separator = "-"), "test")
    run_first <- studio_create_run(first, "test")
    run_second <- studio_create_run(second, "test")
    first_policy <- studio_read_policy(first)
    second_policy <- studio_read_policy(second)
    expect_equal(first_policy$identifiers$usubjid$separator, "_")
    expect_equal(second_policy$identifiers$usubjid$separator, "-")
    expect_false(identical(studio_run_path(first, run_first), studio_run_path(second, run_second)))
    expect_false(identical(
      file_sha256(file.path(studio_run_path(first, run_first), "config", "mapping_policies.yml")),
      file_sha256(file.path(studio_run_path(second, run_second), "config", "mapping_policies.yml"))
    ))
  })
})

test_that("项目研究编号与绑定后的研究标识保持一致", {
  studio_with_test_home({
    project_id <- studio_prepare_project("basic", project_id = "study-check")
    expect_error(
      studio_save_policy(project_id, list(study_id = "OTHER-STUDY")),
      "研究标识字段与项目研究编号"
    )
    expect_identical(studio_read_project(project_id)$study_id, "TRACEB01")
  })
})

test_that("历史运行和过期运行不能再次写入", {
  studio_with_test_home({
    project_id <- studio_prepare_project("basic")
    old_run <- studio_create_run(project_id, "test")
    current_run <- studio_create_run(project_id, "test")
    expect_error(studio_assert_run_writable(studio_load_run_config(project_id, old_run)), "历史运行")
    expect_silent(studio_assert_run_writable(studio_load_run_config(project_id, current_run)))
    studio_save_policy(project_id, list(study_id = "TRACEB01", subject_separator = "_"), "test")
    expect_error(studio_assert_run_writable(studio_load_run_config(project_id, current_run)), "运行已过期")
  })
})

test_that("受控术语与访视编辑只能采用登记值", {
  studio_with_test_home({
    project_id <- studio_prepare_project("advanced")
    expect_error(studio_add_terminology_mapping(project_id, "SEX", "Woman", "INVALID", "test"), "允许标准值")
    studio_add_terminology_mapping(project_id, "SEX", "Woman", "F", "test")
    studio_add_visit_mapping(project_id, "trace_visits_v1", "Week 4", 4, "test")
    options <- studio_policy_options(project_id)
    expect_equal(options$codelists$SEX$Woman, "F")
    expect_equal(options$visit_maps$trace_visits_v1[["Week 4"]], 4)
  })
})
