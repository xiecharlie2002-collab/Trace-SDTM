test_that("Studio界面包含十个工作页面并固定本机地址", {
  html <- as.character(trace_studio_ui())
  for (label in c("项目", "数据源", "项目政策", "数据画像", "映射推荐", "人工审核", "构建", "验证", "追溯与报告", "系统检查")) {
    expect_match(html, label, fixed = TRUE)
  }
  expect_equal(studio_settings()$host, "127.0.0.1")
  expect_equal(unlist(studio_settings()$port_range), 3838:3848)
  doctor <- studio_doctor()
  expect_true(all(c("工作台", "R依赖", "Pinnacle 21") %in% doctor$category))
})

test_that("Studio后台画像任务完成并写入脱敏状态", {
  studio_with_test_home({
    project_id <- studio_prepare_project("basic")
    run_id <- studio_create_run(project_id, "test")
    secret <- "unit-test-secret-never-write"
    studio_start_job(project_id, run_id, "profile", credentials = list(api_key = secret))
    deadline <- Sys.time() + 30
    state <- studio_poll_job()
    while (!is.null(studio_active_job()) && Sys.time() < deadline) {
      Sys.sleep(0.1)
      state <- studio_poll_job()
    }
    expect_equal(state$status, "completed")
    config <- studio_load_run_config(project_id, run_id)
    expect_true(file.exists(file.path(config$paths$profile_dir, "source_dictionary.csv")))
    saved <- jsonlite::read_json(studio_job_state_path(config), simplifyVector = FALSE)
    expect_true(identical(saved$secrets_logged, FALSE))
    expect_false(any(grepl("api_key", names(saved), ignore.case = TRUE)))
    files <- list.files(studio_project_path(project_id), recursive = TRUE, full.names = TRUE)
    expect_length(studio_secret_scan(files, secret), 0L)
  })
})

test_that("Studio取消任务并在重启时标记孤立任务", {
  studio_with_test_home({
    project_id <- studio_prepare_project("basic")
    run_id <- studio_create_run(project_id, "test")
    config <- studio_load_run_config(project_id, run_id)
    process <- callr::r_bg(function() Sys.sleep(20), stdout = "|", stderr = "|")
    state <- list(schema_version = "0.5", project_id = project_id, run_id = run_id,
                  command = "profile", status = "running", pid = process$get_pid(), started_at = utc_now())
    .trace_studio_jobs$active <- list(process = process, config = config, state = state,
                                      log_path = file.path(config$paths$log_dir, "studio_job.log"), secrets = character())
    studio_update_run_stage(project_id, run_id, "profile", "running")
    expect_true(studio_cancel_job())
    expect_equal(studio_read_run(project_id, run_id)$stages$profile, "cancelled")

    orphan <- state
    orphan$status <- "running"
    orphan$pid <- 99999999L
    write_json(orphan, studio_job_state_path(config))
    studio_update_run_stage(project_id, run_id, "profile", "running")
    recovered <- studio_recover_interrupted_jobs()
    expect_true(paste(project_id, run_id, sep = "/") %in% recovered)
    expect_equal(studio_read_run(project_id, run_id)$stages$profile, "interrupted")
  })
})
