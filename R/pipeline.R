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
    "  registry-check         验证注册表、参数模式和实现绑定",
    "  registry-docs          由注册表生成函数目录",
    "  profile",
    "  recommend-targets      仅执行目标变量识别",
    "  recommend-functions    读取目标结果并执行函数选择",
    "  recommend-parameters   读取目标和函数结果并解析参数",
    "  assemble-recommendations  组装完整候选并生成审核工作簿",
    "  recommend              顺序执行 v0.4 三阶段推荐",
    "  recommend --seed       生成明确标记的离线参考种子",
    "  evaluate-v04          评价当前 v0.4 三阶段推荐产物",
    "  review-gold            仅用于实验评价：按金标准填写审核表",
    "  approve                从审核工作簿锁定规格",
    "  build",
    "  validate-local",
    "  doctor-p21",
    "  validate-p21",
    "  import-p21 --file <xlsx>",
    "  report",
    "  run",
    "  test",
    "所有数据命令均可追加 --scenario basic|intermediate|advanced；默认 basic。",
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

trace_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  command <- args[[1]] %||% "help"
  scenario <- scenario_from_args(args)
  Sys.setenv(TRACE_SDTM_SCENARIO = scenario)
  config <- load_project_config(scenario)
  switch(
    command,
    `registry-check` = {
      registry <- load_transform_registry(config)
      trace_info("转换注册表检查通过：版本 %s，共 %d 个函数。", registry$registry_version, length(registry$transforms))
    },
    `registry-docs` = write_transform_catalog(config),
    profile = profile_sources(config),
    `recommend-targets` = {
      profile_sources(config)
      recommend_targets_v04(
        config, provider = if ("--seed" %in% args) "seed" else "model",
        blind = "--blind" %in% args
      )
    },
    `recommend-functions` = {
      targets <- read_recommendation_stage_v04(config, "target_decisions.json")
      recommend_functions_v04(
        targets, config, provider = if ("--seed" %in% args) "seed" else "model",
        blind = "--blind" %in% args
      )
    },
    `recommend-parameters` = {
      targets <- read_recommendation_stage_v04(config, "target_decisions.json")
      functions <- read_recommendation_stage_v04(config, "function_candidates.json")
      recommend_parameters_v04(
        targets, functions, config, provider = if ("--seed" %in% args) "seed" else "model",
        blind = "--blind" %in% args
      )
    },
    `assemble-recommendations` = {
      targets <- read_recommendation_stage_v04(config, "target_decisions.json")
      functions <- read_recommendation_stage_v04(config, "function_candidates.json")
      parameters <- read_recommendation_stage_v04(config, "parameter_completions.json")
      assembled <- assemble_recommendations_v04(targets, functions, parameters, config)
      create_review_workbook_v04(assembled, config, preapprove = "--seed" %in% args)
    },
    recommend = {
      profile_sources(config)
      provider <- if ("--seed" %in% args) "seed" else "model"
      recommendation <- run_recommendation_v04(config, provider = provider, blind = "--blind" %in% args)
      create_review_workbook_v04(recommendation$assembled, config, preapprove = identical(provider, "seed"))
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
    approve = approve_mapping_v04(config),
    build = build_sdtm(config),
    `validate-local` = validate_local(config),
    `doctor-p21` = doctor_p21(config),
    `validate-p21` = validate_p21(config),
    `import-p21` = {
      position <- match("--file", args)
      if (is.na(position) || position == length(args)) trace_abort("import-p21 需要 --file <报告路径>。")
      import_p21_report(args[[position + 1L]], config)
    },
    report = generate_report(config),
    run = run_pipeline(config),
    test = run_tests(),
    help = print_trace_help(),
    trace_abort(sprintf("未知命令：%s", command))
  )
  invisible(TRUE)
}
