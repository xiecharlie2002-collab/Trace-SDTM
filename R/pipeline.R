run_pipeline <- function(config = load_project_config()) {
  load_approved_mapping(config)
  build_sdtm(config)
  validate_local(config)
  validate_p21(config)
  generate_report(config)
  trace_info("TraceSDTM 完整流程执行完成。")
  invisible(TRUE)
}

print_trace_help <- function() {
  cat(paste(
    "TraceSDTM 命令：",
    "  studio                 启动仅监听 127.0.0.1 的本地浏览器工作台",
    "  studio-doctor          检查工作台依赖、目录和本机配置",
    "  registry-check         验证注册表、参数模式和实现绑定",
    "  registry-docs          由注册表生成函数目录",
    "  profile                生成数据集、字段和关系画像",
    "  discover-tasks         人工智能发现原子任务并由程序校验",
    "  recommend-targets      仅执行目标变量识别",
    "  recommend-functions    读取目标结果并执行函数选择",
    "  recommend-parameters   读取目标和函数结果并解析参数",
    "  assemble-recommendations  组装完整候选并初始化结构化审核",
    "  recommend              顺序执行目标、函数和参数阶段",
    "  ai-review              使用全新上下文执行独立人工智能审查",
    "  recommend --seed       生成明确标记的离线参考种子",
    "  evaluate-v04          评价当前 v0.4 三阶段推荐产物",
    "  review-gold            仅用于实验评价：按金标准填写审核表",
    "  approve                人工最终批准并锁定映射规格",
    "  build",
    "  validate-local",
    "  doctor-p21",
    "  validate-p21",
    "  import-p21 --file <xlsx>",
    "  report",
    "  export-evidence        导出不含原始数据的项目证据包（工作台运行）",
    "  run",
    "  test",
    "内置数据命令可追加 --scenario basic|intermediate|advanced；默认 basic。",
    "工作台运行可追加 --project <项目编号> --run <运行编号>。",
    sep = "\n"
  ), "\n")
}

read_recommendation_stage_v04 <- function(config, filename) {
  path <- trace_path(config$paths$recommendation_dir, filename)
  if (!file.exists(path)) trace_abort(sprintf("缺少阶段产物 %s。", path))
  jsonlite::read_json(path, simplifyVector = FALSE)
}

run_tests <- function() {
  testthat::test_dir(trace_path("tests", "testthat"), reporter = "summary", stop_on_failure = TRUE)
}

trace_execute_command <- function(command, args, config) {
  is_studio <- isTRUE(config$project$studio)
  if (is_studio && !identical(command, "export-evidence")) studio_assert_run_writable(config)
  if (is_studio && command %in% c(
    "profile", "discover-tasks", "recommend-targets", "recommend-functions",
    "recommend-parameters", "assemble-recommendations", "recommend", "ai-review", "approve"
  )) studio_assert_mapping_editable_v06(config)
  stage <- function(name, value) studio_recorded_stage(config, name, value)
  if (is_studio && ("--seed" %in% args || command %in% c("evaluate-v04", "review-gold"))) {
    trace_abort("普通工作台项目不包含金标准，禁止使用 --seed、review-gold 或准确率评价。")
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
      if (is_studio) v06_assert_tasks_frozen(config)
      if (!file.exists(file.path(trace_path(config$paths$profile_dir), "source_dictionary.csv"))) stage("profile", profile_sources(config))
      stage("recommend_targets", recommend_targets_v04(
        config, provider = if ("--seed" %in% args) "seed" else "model",
        blind = "--blind" %in% args
      ))
    },
    `recommend-functions` = {
      if (is_studio) v06_assert_tasks_frozen(config)
      targets <- read_recommendation_stage_v04(config, "target_decisions.json")
      stage("recommend_functions", recommend_functions_v04(
        targets, config, provider = if ("--seed" %in% args) "seed" else "model",
        blind = "--blind" %in% args
      ))
    },
    `recommend-parameters` = {
      if (is_studio) v06_assert_tasks_frozen(config)
      targets <- read_recommendation_stage_v04(config, "target_decisions.json")
      functions <- read_recommendation_stage_v04(config, "function_candidates.json")
      stage("recommend_parameters", recommend_parameters_v04(
        targets, functions, config, provider = if ("--seed" %in% args) "seed" else "model",
        blind = "--blind" %in% args
      ))
    },
    `assemble-recommendations` = {
      if (is_studio) v06_assert_tasks_frozen(config)
      targets <- read_recommendation_stage_v04(config, "target_decisions.json")
      functions <- read_recommendation_stage_v04(config, "function_candidates.json")
      parameters <- read_recommendation_stage_v04(config, "parameter_completions.json")
      assembled <- stage("assemble", assemble_recommendations_v04(targets, functions, parameters, config))
      create_review_workbook_v04(assembled, config, preapprove = "--seed" %in% args)
      if (is_studio) studio_initialize_review(config, overwrite = TRUE)
    },
    recommend = {
      provider <- if ("--seed" %in% args) "seed" else "model"
      if (is_studio) {
        v06_assert_tasks_frozen(config)
        if (!file.exists(file.path(trace_path(config$paths$profile_dir), "source_dictionary.csv"))) stage("profile", profile_sources(config))
        targets <- stage("recommend_targets", recommend_targets_v04(config, provider = provider, blind = "--blind" %in% args))
        functions <- stage("recommend_functions", recommend_functions_v04(targets, config, provider = provider, blind = "--blind" %in% args))
        parameters <- stage("recommend_parameters", recommend_parameters_v04(targets, functions, config, provider = provider, blind = "--blind" %in% args))
        assembled <- stage("assemble", assemble_recommendations_v04(targets, functions, parameters, config))
        create_review_workbook_v04(assembled, config, preapprove = FALSE)
        studio_initialize_review(config, overwrite = TRUE)
      } else {
        profile_sources(config)
        recommendation <- run_recommendation_v04(config, provider = provider, blind = "--blind" %in% args)
        create_review_workbook_v04(recommendation$assembled, config, preapprove = identical(provider, "seed"))
      }
    },
    `ai-review` = {
      if (!is_studio) trace_abort("独立人工智能审查命令只适用于 0.6 工作台运行。")
      stage("ai_review", run_ai_review_v06(config))
    },
    `evaluate-v04` = {
      targets <- read_recommendation_stage_v04(config, "target_decisions.json")
      functions <- read_recommendation_stage_v04(config, "function_candidates.json")
      parameters <- read_recommendation_stage_v04(config, "parameter_completions.json")
      assembled <- read_recommendation_stage_v04(config, "assembled_recommendations.json")
      evaluation <- evaluate_three_stage_v04(config, targets, functions, parameters, assembled)
      trace_info(
        "v0.4 评价完成：语义 %d/%d，结构 %d/%d，完整计划 %d/%d。",
        evaluation$summary$semantic_correct, evaluation$summary$task_count,
        evaluation$summary$structural_correct, evaluation$summary$task_count,
        evaluation$summary$complete_plan_top1_correct, evaluation$summary$task_count
      )
    },
    `review-gold` = review_against_gold_v04(config),
    approve = if (is_studio) {
      reviewer <- Sys.getenv("TRACE_SDTM_REVIEWER", unset = "")
      stage("human_approval", studio_approve_review(config, reviewer))
    } else {
      approve_mapping_v04(config)
    },
    build = stage("build", build_sdtm(config)),
    `validate-local` = stage("validate_local", validate_local(config)),
    `doctor-p21` = doctor_p21(config),
    `validate-p21` = stage("validate_p21", validate_p21(config)),
    `import-p21` = {
      position <- match("--file", args)
      if (is.na(position) || position == length(args)) trace_abort("import-p21 需要 --file <报告路径>。")
      stage("validate_p21", import_p21_report(args[[position + 1L]], config))
    },
    report = stage("report", generate_report(config)),
    `export-evidence` = {
      if (!is_studio) trace_abort("export-evidence 只适用于工作台项目运行。")
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
  if (identical(command, "help")) {
    print_trace_help()
    return(invisible(TRUE))
  }
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
  scenario <- scenario_from_args(args)
  Sys.setenv(TRACE_SDTM_SCENARIO = scenario)
  config <- studio_resolve_cli_config(args, scenario)
  execute <- function() trace_execute_command(command, args, config)
  if (isTRUE(config$project$studio)) {
    studio_with_project_lock(config$studio$project_id, execute())
  } else {
    execute()
  }
  invisible(TRUE)
}
