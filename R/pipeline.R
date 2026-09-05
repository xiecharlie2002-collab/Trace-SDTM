workflow_commands <- function() {
  c(
    "profile", "discover-tasks", "recommend-targets", "recommend-functions",
    "recommend-parameters", "assemble-recommendations", "recommend", "ai-review",
    "approve", "build", "validate-local", "build-adam", "validate-adam",
    "build-tlf", "validate-tlf", "run-analysis", "doctor-p21", "validate-p21",
    "import-p21", "report", "export-evidence", "run"
  )
}

print_trace_help <- function() {
  cat(paste(
    "TraceSDTM 命令：",
    "  studio                    启动仅监听 127.0.0.1 的五步工作台",
    "  studio-doctor             检查工作台依赖和本机配置",
    "  registry-check            验证转换函数注册表",
    "  registry-docs             重新生成转换函数目录",
    "  profile                   生成数据集、字段和关系画像",
    "  discover-tasks            发现任务并执行结构校验",
    "  recommend-targets         选择目标变量",
    "  recommend-functions       从程序候选中选择函数",
    "  recommend-parameters      自动注入并解析参数",
    "  assemble-recommendations  组装映射计划并初始化审核",
    "  recommend                 顺序执行三个映射阶段",
    "  ai-review                 独立人工智能审查",
    "  approve                   人工批准并冻结规格",
    "  build                     确定性生成 SDTM",
    "  validate-local            执行本地检查",
    "  build-adam                从已批准且检查通过的 SDTM 生成 ADSL 和 ADAE",
    "  validate-adam             执行 ADaM 本地规则检查",
    "  build-tlf                 从检查通过的 ADaM 生成两张汇总表",
    "  validate-tlf              检查表格分母、去重、同源结果和文件",
    "  run-analysis              顺序执行 ADaM 与表格构建及检查",
    "  doctor-p21                检查 Pinnacle 21 配置",
    "  validate-p21              运行 Pinnacle 21",
    "  import-p21 --file <xlsx>  导入 Pinnacle 21 报告",
    "  report                    生成离线报告",
    "  export-evidence           导出不含原始数据的证据包",
    "  run                       构建、本地检查、Pinnacle 21 和报告",
    "  test                      运行当前版本检查",
    "工作流命令必须同时提供 --project <项目编号> --run <运行编号>。",
    sep = "\n"
  ), "\n")
}

read_recommendation_stage <- function(config, filename) {
  path <- trace_path(config$paths$recommendation_dir, filename)
  if (!file.exists(path)) trace_abort(sprintf("缺少阶段产物 %s。", path))
  jsonlite::read_json(path, simplifyVector = FALSE)
}

run_tests <- function() {
  testthat::test_dir(trace_path("tests", "testthat"), reporter = "summary", stop_on_failure = TRUE)
}

trace_execute_command <- function(command, args, config) {
  is_studio <- isTRUE(config$project$studio)
  if (command %in% workflow_commands() && !is_studio) {
    trace_abort("该命令必须指定工作台项目和运行：--project <项目编号> --run <运行编号>。")
  }
  if (is_studio && !identical(command, "export-evidence")) studio_assert_run_writable(config)
  if (is_studio && command %in% c(
    "profile", "discover-tasks", "recommend-targets", "recommend-functions",
    "recommend-parameters", "assemble-recommendations", "recommend", "ai-review", "approve"
  )) studio_assert_mapping_editable_v06(config)
  stage <- function(name, value) {
    if (is_studio) studio_recorded_stage(config, name, value) else force(value)
  }

  switch(
    command,
    `registry-check` = {
      registry <- load_transform_registry(config)
      trace_info("转换注册表检查通过：版本 %s，共 %d 个函数。", registry$registry_version, length(registry$transforms))
    },
    `registry-docs` = write_transform_catalog(config),
    profile = stage("profile", profile_sources(config)),
    `discover-tasks` = {
      if (!file.exists(trace_path(config$paths$project_context))) stage("profile", profile_sources(config))
      stage("task_discovery", run_task_discovery_v06(config))
    },
    `recommend-targets` = {
      v06_assert_tasks_frozen(config)
      if (!file.exists(file.path(trace_path(config$paths$profile_dir), "source_dictionary.csv"))) {
        stage("profile", profile_sources(config))
      }
      stage("recommend_targets", recommend_targets(config))
    },
    `recommend-functions` = {
      v06_assert_tasks_frozen(config)
      targets <- read_recommendation_stage(config, "target_decisions.json")
      stage("recommend_functions", recommend_functions(targets, config))
    },
    `recommend-parameters` = {
      v06_assert_tasks_frozen(config)
      targets <- read_recommendation_stage(config, "target_decisions.json")
      functions <- read_recommendation_stage(config, "function_candidates.json")
      stage("recommend_parameters", recommend_parameters(targets, functions, config))
    },
    `assemble-recommendations` = {
      v06_assert_tasks_frozen(config)
      targets <- read_recommendation_stage(config, "target_decisions.json")
      functions <- read_recommendation_stage(config, "function_candidates.json")
      parameters <- read_recommendation_stage(config, "parameter_completions.json")
      assembled <- stage("assemble", assemble_recommendations(targets, functions, parameters, config))
      create_review_workbook(assembled, config)
      studio_initialize_review(config, overwrite = TRUE)
    },
    recommend = {
      v06_assert_tasks_frozen(config)
      if (!file.exists(file.path(trace_path(config$paths$profile_dir), "source_dictionary.csv"))) {
        stage("profile", profile_sources(config))
      }
      targets <- stage("recommend_targets", recommend_targets(config))
      functions <- stage("recommend_functions", recommend_functions(targets, config))
      parameters <- stage("recommend_parameters", recommend_parameters(targets, functions, config))
      assembled <- stage("assemble", assemble_recommendations(targets, functions, parameters, config))
      create_review_workbook(assembled, config)
      studio_initialize_review(config, overwrite = TRUE)
    },
    `ai-review` = stage("ai_review", run_ai_review_v06(config)),
    approve = {
      reviewer <- Sys.getenv("TRACE_SDTM_REVIEWER", unset = "")
      stage("human_approval", studio_approve_review(config, reviewer))
    },
    build = stage("build", build_sdtm(config)),
    `validate-local` = stage("validate_local", validate_local(config)),
    `build-adam` = stage("build_adam", build_adam(config)),
    `validate-adam` = stage("validate_adam", validate_adam(config)),
    `build-tlf` = stage("build_tlf", build_tlf(config)),
    `validate-tlf` = stage("validate_tlf", validate_tlf(config)),
    `run-analysis` = {
      stage("build_adam", build_adam(config))
      stage("validate_adam", validate_adam(config))
      stage("build_tlf", build_tlf(config))
      stage("validate_tlf", validate_tlf(config))
    },
    `doctor-p21` = doctor_p21(config),
    `validate-p21` = stage("validate_p21", validate_p21(config)),
    `import-p21` = {
      report_file <- argument_value(args, "--file", default = NULL)
      if (is.null(report_file)) trace_abort("import-p21 需要 --file <报告路径>。")
      stage("validate_p21", import_p21_report(report_file, config))
    },
    report = stage("report", generate_report(config)),
    `export-evidence` = {
      path <- studio_export_evidence(config)
      trace_info("已生成项目证据包：%s", path)
    },
    run = {
      load_approved_mapping(config)
      stage("build", build_sdtm(config))
      stage("validate_local", validate_local(config))
      stage("validate_p21", validate_p21(config))
      stage("report", generate_report(config))
      trace_info("TraceSDTM 完整流程执行完成。")
    },
    test = run_tests(),
    help = print_trace_help(),
    trace_abort(sprintf("未知命令：%s", command))
  )
  invisible(TRUE)
}

trace_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  command <- args[[1]] %||% "help"
  if (identical(command, "help")) return(invisible(print_trace_help()))
  if (identical(command, "studio-doctor")) {
    print(studio_doctor(), row.names = FALSE)
    return(invisible(TRUE))
  }
  if (identical(command, "studio")) {
    requested <- argument_value(args, "--port", default = NULL)
    port <- studio_find_port(if (is.null(requested)) NULL else as.integer(requested))
    launch_trace_studio(port = port, launch_browser = !"--no-browser" %in% args)
    return(invisible(TRUE))
  }

  config <- studio_resolve_cli_config(args)
  execute <- function() trace_execute_command(command, args, config)
  if (isTRUE(config$project$studio)) {
    studio_with_project_lock(config$studio$project_id, execute())
  } else {
    execute()
  }
  invisible(TRUE)
}
