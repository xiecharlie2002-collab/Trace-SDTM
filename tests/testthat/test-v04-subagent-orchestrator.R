source(trace_path("R", "experiment_v04.R"), encoding = "UTF-8")

test_that("v0.4 experiment paths are isolated below benchmark v2", {
  config <- v04_benchmark_config("basic", "orchestrator-test")
  expect_identical(
    config$paths$output_base,
    file.path("output", "benchmark", "v2", "experiments", "orchestrator-test", "basic")
  )
  expect_match(config$paths$recommendation_dir, "experiments[/\\\\]orchestrator-test")
  expect_error(v04_benchmark_config("unknown", "orchestrator-test"), "未知场景")
  expect_error(v04_benchmark_config("basic", "../escape"), "实验编号")
})

test_that("frozen text is byte stable and cannot be replaced", {
  directory <- withr::local_tempdir()
  path <- file.path(directory, "frozen.txt")
  first <- v04_freeze_text("第一条\nsecond", path)
  second <- v04_freeze_text("第一条\nsecond", path)
  expect_identical(first, second)
  expect_identical(v04_read_text_bytes(path)$text, "第一条\nsecond")
  expect_error(v04_freeze_text("different", path), "拒绝覆盖")
})

test_that("blind request states procedural isolation and contains no repository path", {
  request <- v04_blind_request("只返回 JSON。")
  expect_match(request, "不得调用任何工具")
  expect_match(request, "不得请求提示、修正或重试")
  expect_false(grepl("02_project", request, fixed = TRUE))
  conditional <- v04_blind_request("只返回 JSON。", conditional = TRUE)
  expect_match(conditional, "端到端首次响应已经冻结")
  expect_match(conditional, "给定正确目标")
})

test_that("first response import preserves raw bytes and records deterministic parsing", {
  directory <- withr::local_tempdir()
  group <- list(group_id = "dm_atomic_v04", target_domain = "DM")
  stage_dir <- file.path(directory, "groups", group$group_id, "test")
  dir.create(stage_dir, recursive = TRUE)
  v04_freeze_text("request", file.path(stage_dir, "request.txt"))
  response_file <- file.path(directory, "incoming.txt")
  v04_freeze_text('{"items":[{"task_id":"A"}]}', response_file)
  config <- list(
    project = list(scenario = "basic"),
    paths = list(
      specification_template = "specs/benchmark/v2/basic_tasks.yml",
      mapping_policies = "specs/benchmark/v2/basic_policies.yml",
      metadata = "specs/sdtm_metadata.yml",
      transform_registry = "config/transform_registry.yml",
      controlled_terminology = "specs/v0.2/controlled_terminology.yml",
      unit_conversions = "specs/v0.2/unit_conversions.yml",
      gold_specification = "specs/benchmark/v2/basic_gold.yml",
      profile_dir = "output/benchmark/v2/basic/profile",
      recommendation_dir = directory
    )
  )
  parser <- function(value) list(valid = value$items, failures = list(), stage = "test")
  result <- v04_import_first_response(
    config, group, "test", response_file, "agent-A", parser
  )
  expect_length(result$valid, 1L)
  expect_identical(
    v04_read_text_bytes(file.path(stage_dir, "response_raw.txt"))$raw,
    v04_read_text_bytes(response_file)$raw
  )
  evidence <- jsonlite::read_json(file.path(stage_dir, "response_evidence.json"), simplifyVector = FALSE)
  expect_identical(evidence$attempt_number, 1L)
  expect_false(evidence$retry_performed)
  expect_identical(evidence$subagent_task, "agent-A")
  expect_identical(evidence$validation_status, "passed")
})

test_that("致命结构错误被冻结并进入分母而不要求重试", {
  directory <- withr::local_tempdir()
  group <- list(
    group_id = "dm_atomic_v04", target_domain = "DM",
    tasks = list(list(task_id = "A", source_refs = list()))
  )
  stage_dir <- file.path(directory, "groups", group$group_id, "targets")
  dir.create(stage_dir, recursive = TRUE)
  v04_freeze_text("request", file.path(stage_dir, "request.txt"))
  incoming <- file.path(directory, "incoming.txt")
  v04_freeze_text("not json", incoming)
  config <- list(
    project = list(scenario = "basic"),
    paths = list(
      recommendation_dir = directory,
      specification_template = "specs/benchmark/v2/basic_tasks.yml",
      mapping_policies = "specs/benchmark/v2/basic_policies.yml",
      metadata = "specs/sdtm_metadata.yml",
      transform_registry = "config/transform_registry.yml",
      controlled_terminology = "specs/v0.2/controlled_terminology.yml",
      unit_conversions = "specs/v0.2/unit_conversions.yml",
      gold_specification = "specs/benchmark/v2/basic_gold.yml",
      profile_dir = "output/benchmark/v2/basic/profile"
    )
  )
  result <- v04_import_first_response(
    config, group, "targets", incoming, "agent-A", function(value) value
  )
  expect_true(result$fatal_structure_error)
  expect_length(result$valid, 0L)
  expect_length(result$failures, 1L)
  expect_true(file.exists(file.path(stage_dir, "normalized.json")))
  evidence <- jsonlite::read_json(file.path(stage_dir, "response_evidence.json"), simplifyVector = FALSE)
  expect_identical(evidence$validation_status, "failed")
  expect_false(evidence$retry_performed)
})

test_that("conditional request is gated by frozen cascading function response", {
  directory <- withr::local_tempdir()
  config <- list(paths = list(recommendation_dir = directory))
  group <- list(group_id = "dm_atomic_v04")
  cascade <- v04_stage_evidence_dir(config, group, "functions_cascade")
  gate <- function() {
    if (!file.exists(file.path(cascade, "response_raw.txt")) ||
        !file.exists(file.path(cascade, "response_evidence.json"))) {
      trace_abort("只有端到端函数响应冻结后，才能生成条件函数请求。")
    }
    TRUE
  }
  expect_error(gate(), "端到端函数响应冻结后")
  v04_freeze_text("{}", file.path(cascade, "response_raw.txt"))
  write_json(list(validation_status = "passed"), file.path(cascade, "response_evidence.json"))
  expect_true(gate())
})
