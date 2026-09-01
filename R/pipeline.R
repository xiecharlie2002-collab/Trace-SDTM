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
    "  profile",
    "  recommend              调用真实 OpenAI 兼容接口",
    "  recommend --seed       生成明确标记的离线参考种子",
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
    sep = "\n"
  ), "\n")
}

run_tests <- function() {
  testthat::test_dir(trace_path("tests", "testthat"), reporter = "summary", stop_on_failure = TRUE)
}

trace_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  command <- args[[1]] %||% "help"
  config <- load_project_config()
  switch(
    command,
    profile = profile_sources(config),
    recommend = {
      profile_sources(config)
      if ("--seed" %in% args) seed_recommendations(config) else call_mapping_model(config)
    },
    `review-gold` = review_against_gold(config),
    approve = approve_mapping(config),
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
