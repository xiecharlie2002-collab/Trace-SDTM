test_that("三级场景具有锁定的概念数和难度", {
  expected <- c(basic = 18L, intermediate = 20L, advanced = 21L)
  expected_difficulty <- c(basic = "simple", intermediate = "moderate")
  for (scenario in names(expected)) {
    config <- load_project_config(scenario)
    specification <- load_mapping_template(config)
    gold <- load_gold_specification(config)
    ids <- vapply(specification$concepts, `[[`, character(1), "concept_id")
    expect_length(ids, expected[[scenario]])
    expect_identical(anyDuplicated(ids), 0L)
    expect_setequal(ids, names(gold$plans))
    expect_silent(load_mapping_policies(config))
    if (scenario %in% names(expected_difficulty)) {
      expect_true(all(vapply(specification$concepts, function(x) identical(x$difficulty, expected_difficulty[[scenario]]), logical(1))))
    }
  }
})

test_that("日期画像区分明确、冲突和真正歧义并识别秒", {
  expect_identical(infer_slash_date_format(c("12/20/2024", "12/21/2024")), "m/d/y")
  expect_identical(infer_slash_date_format(c("20/12/2024", "21/12/2024")), "d/m/y")
  expect_identical(infer_slash_date_format(c("01/05/2025", "01/12/2025")), "m/d/y_or_d/m/y")
  expect_identical(infer_slash_date_format(c("20/12/2024", "12/20/2024")), "mixed_m/d/y_and_d/m/y")
  expect_identical(infer_source_formats("09:30:00"), "H:M:S")
  expect_identical(infer_source_formats("09:30"), "H:M")
})

test_that("高级画像、政策和函数卡的时间契约一致", {
  config <- load_project_config("advanced")
  dictionary <- profile_sources(config)
  consent <- dplyr::filter(dictionary, source_dataset == "dm_subject", source_variable == "IC_DT")
  time <- dplyr::filter(dictionary, source_dataset == "ae_main", source_variable == "AESTTIM")
  expect_identical(consent$format_candidates[[1]], "m/d/y")
  expect_identical(time$data_type[[1]], "hms")
  expect_identical(time$format_candidates[[1]], "H:M")
  registry <- load_transform_registry(config)
  datetime <- registry_entry("to_iso8601_partial_datetime", registry)
  expect_true("hms" %in% unlist(datetime$source_contract$types))
  policy_text <- as.character(registry_json(load_mapping_policies(config)))
  expect_match(policy_text, "H:M:S", fixed = TRUE)
  expect_match(policy_text, "applies_to_identity_conversions", fixed = TRUE)
  intermediate <- profile_sources(load_project_config("intermediate"))
  intermediate_time <- dplyr::filter(intermediate, source_dataset == "ae_intermediate", source_variable == "AESTTIM")
  expect_identical(intermediate_time$format_candidates[[1]], "H:M:S")
})

test_that("提示词包含政策和最小类别规则但不包含金标准链", {
  config <- load_project_config("advanced")
  dictionary <- profile_sources(config)
  group <- recommendation_groups(load_mapping_template(config), dictionary, config)$vs_connected_01
  prompt <- classification_prompt_v02(group, load_metadata(config), load_transform_registry(config), config)
  expect_match(prompt, "最小类别链", fixed = TRUE)
  expect_match(prompt, "encapsulates", fixed = TRUE)
  expect_match(prompt, "身份单位标准化", fixed = TRUE)
  expect_false(grepl("advanced_gold.yml", prompt, fixed = TRUE))
  expect_false(grepl('"transform_id"', prompt, fixed = TRUE))
})

test_that("数据集输出拒绝非空目标变量", {
  config <- load_project_config("advanced")
  specification <- load_mapping_template(config)
  concept <- concept_lookup(specification)$AE_SOURCE_INTEGRATION
  step <- gold_plan_steps(load_gold_specification(config)$plans$AE_SOURCE_INTEGRATION)[[1]]
  step$target_variables <- "STUDYID"
  expect_error(
    validate_step_contract(step, concept, specification, load_metadata(config), load_transform_registry(config), config),
    "target_variables 必须为空"
  )
})

test_that("修改程度按预定规则分为四级", {
  expected <- list(list(transform_id = "assign_no_ct", source_keys = "x.A", target_variables = "A", parameters = list()))
  expect_identical(modification_grade_v03("proposed", expected, expected, TRUE, TRUE), "none")
  parameter_change <- expected
  parameter_change[[1]]$source_keys <- "x.B"
  expect_identical(modification_grade_v03("proposed", parameter_change, expected, TRUE, FALSE), "minor")
  function_change <- expected
  function_change[[1]]$transform_id <- "normalize_case"
  expect_identical(modification_grade_v03("proposed", function_change, expected, TRUE, FALSE), "moderate")
  expect_identical(modification_grade_v03("needs_information", list(), expected, FALSE, FALSE), "major")
})

test_that("逐概念诊断不会改变整组严格校验", {
  config <- load_project_config("advanced")
  specification <- load_mapping_template(config)
  metadata <- load_metadata(config)
  registry <- load_transform_registry(config)
  dictionary <- profile_sources(config)
  group <- recommendation_groups(specification, dictionary, config)$ae_connected_01
  classifications <- lapply(group$concepts, function(concept) list(
    concept_id = concept$concept_id,
    categories = unique(vapply(gold_plan_steps(load_gold_specification(config)$plans[[concept$concept_id]]), function(step) registry_entry(step$transform_id, registry)$category, character(1))),
    classification_score = 1, evidence = "test", uncertainties = "", status = "proposed", group_id = group$group_id
  ))
  record <- list(
    concept_id = "AE_SOURCE_INTEGRATION", candidate_rank = 1L, target_domain = "AE",
    steps = gold_plan_steps(load_gold_specification(config)$plans$AE_SOURCE_INTEGRATION),
    recommendation_score = 1, reason = "test", uncertainties = "", status = "proposed", review_required = TRUE
  )
  record$steps[[1]]$target_variables <- "ae_merged"
  diagnostic <- diagnose_candidate_plans_v03(list(record), classifications, group, specification, metadata, registry, config = config)
  expect_length(diagnostic$failures, 1L)
  expect_length(diagnostic$valid_candidates, 0L)
  expect_error(parse_candidate_plans_v02(list(record), classifications, group, specification, metadata, registry, config = config), "未知目标变量|target_variables")
})
