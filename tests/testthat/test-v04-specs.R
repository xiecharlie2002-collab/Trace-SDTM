test_that("v0.4 三级原子任务数量和旧概念聚合数量固定", {
  expected_tasks <- c(basic = 18L, intermediate = 49L, advanced = 57L)
  expected_groups <- c(basic = 18L, intermediate = 20L, advanced = 21L)
  for (scenario in names(expected_tasks)) {
    config <- load_project_config(scenario)
    specification <- load_mapping_template(config)
    gold <- load_gold_specification(config)
    expect_identical(as.character(specification$schema_version), "0.4")
    expect_length(specification$tasks, expected_tasks[[scenario]])
    expect_length(unique(vapply(specification$tasks, `[[`, character(1), "assembly_group_id")), expected_groups[[scenario]])
    ids <- vapply(specification$tasks, task_id_v04, character(1))
    expect_identical(anyDuplicated(ids), 0L)
    expect_setequal(ids, names(gold$plans))
    expect_silent(validate_specification_v04(specification, config))
  }
})

test_that("v0.4 来源编号稳定且依赖图无环", {
  for (scenario in c("basic", "intermediate", "advanced")) {
    specification <- load_mapping_template(load_project_config(scenario))
    ids <- vapply(specification$tasks, task_id_v04, character(1))
    expect_setequal(topological_task_order_v04(specification$tasks), ids)
    rows <- purrr::map_dfr(specification$tasks, function(task) purrr::map_dfr(task$source_refs %||% list(), function(ref) tibble::tibble(
      ref_id = ref$ref_id, source_key = source_ref_key(ref)
    )))
    expect_false(any(!nzchar(rows$ref_id)))
    expect_true(all(dplyr::count(dplyr::distinct(rows), .data$ref_id)$n == 1L))
  }
})

test_that("大概念按临床动作拆分", {
  intermediate <- load_mapping_template(load_project_config("intermediate"))$tasks
  advanced <- load_mapping_template(load_project_config("advanced"))$tasks
  ids_i <- vapply(intermediate, task_id_v04, character(1))
  ids_a <- vapply(advanced, task_id_v04, character(1))
  expect_true(all(c("AE_END_DATETIME", "AE_ONGOING_STATUS", "AE_END_STUDY_DAY") %in% ids_i))
  expect_true(all(c("VS_SEQUENCE", "VS_STUDY_DAY") %in% ids_i))
  expect_true(all(c("VS_SEQUENCE", "VS_STUDY_DAY", "VS_BASELINE_FLAG") %in% ids_a))
  expect_true(all(paste0("VS_", c("HEIGHT", "WEIGHT", "TEMP", "SYSBP", "DIABP", "PULSE"), "_STANDARDIZATION") %in% ids_a))
})

test_that("批准后的0.4规格可编译为确定性执行组", {
  config <- load_project_config("advanced")
  specification <- load_mapping_template(config)
  gold <- load_gold_specification(config)
  registry <- load_transform_registry(config)
  specification$specification$status <- "approved"
  specification$specification$approval <- list(reviewer = "test", approved_at = "test")
  specification$tasks <- lapply(specification$tasks, function(task) {
    step <- gold$plans[[task$task_id]][[1]]
    mode <- registry_entry(step$transform_id, registry)$target_contract$output_mode
    task$semantic_decision <- list(
      output_kind = if (mode == "dataset") "dataset" else if (mode == "none") "none" else "variables",
      target_variables = step$target_variables %||% list()
    )
    task$approved_plan <- list(steps = list(step))
    task$review <- list(reviewer = "test", reviewed_at = "test")
    task
  })
  compiled <- compile_specification_v04(specification, config)
  expect_length(compiled$concepts, 21L)
  vs_height <- concept_lookup(compiled)$VS_HEIGHT
  expect_identical(vapply(vs_height$steps, `[[`, character(1), "transform_id"), c("transpose_findings", "standardize_unit"))
  expect_identical(vapply(vs_height$steps, `[[`, character(1), ".task_id"), c("VS_HEIGHT_RECORD", "VS_HEIGHT_STANDARDIZATION"))
})
