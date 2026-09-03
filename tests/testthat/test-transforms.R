test_that("转换注册表唯一、完整且所有实现均可解析", {
  config <- load_v03_config_for_tests("advanced")
  registry <- load_transform_registry(config)
  ids <- vapply(registry$transforms, `[[`, character(1), "transform_id")
  expect_length(ids, 23L)
  expect_identical(anyDuplicated(ids), 0L)
  expect_setequal(vapply(registry$transforms, `[[`, character(1), "implementation_id"), names(transform_implementation_bindings()))
  expect_true(all(vapply(registry$transforms, function(entry) length(entry$examples) > 0L, logical(1))))
  expect_true(all(vapply(Filter(function(entry) isTRUE(entry$model_selectable), registry$transforms), function(entry) length(entry$not_allowed_when) > 0L, logical(1))))
})

test_that("参数模式禁止未知参数和自由公式", {
  config <- load_v03_config_for_tests("advanced")
  specification <- load_mapping_template(config)
  metadata <- load_metadata(config)
  registry <- load_transform_registry(config)
  concept <- concept_lookup(specification)$VS_HEIGHT
  step <- gold_plan_steps(load_gold_specification(config)$plans$VS_HEIGHT)[[2]]
  step$parameters$formula <- "value * 2.54"
  expect_error(validate_step_contract(step, concept, specification, metadata, registry, config), "参数不符合模式")
})

test_that("完整、不完整和日期时间转换不进行日期填补", {
  config <- load_v03_config_for_tests("advanced")
  specification <- load_mapping_template(config)
  sources <- load_registered_sources_v02(specification, config)
  sources$ae_merged <- dplyr::left_join(sources$ae_main, sources$sae_detail, by = c("STUDY", "PATNUM", "AEID"))
  concept <- concept_lookup(specification)$AE_START
  step <- gold_plan_steps(load_gold_specification(config)$plans$AE_START)[[1]]
  state <- list(sources = sources, target = tibble::tibble(.SOURCE_ROW = seq_len(nrow(sources$ae_merged))))
  result <- step_to_iso8601_partial_datetime(state, concept, step, config)$target$AESTDTC
  expect_equal(result[c(1, 2, 3)], c("2025-01-06T10:30", "2025-01", "2025"))
  expect_false(any(grepl("01-01", result[2:3], fixed = TRUE)))
})

test_that("受控单位换算得到指定示例且未知单位被拒绝", {
  config <- load_v03_config_for_tests("advanced")
  state <- list(current_records = tibble::tibble(VSTESTCD = c("HEIGHT", "WEIGHT", "TEMP"), VSORRES = c("70", "180", "98.6"), VSORRESU = c("in", "LB", "F")))
  concept <- list(concept_id = "UNIT_TEST")
  steps <- list(
    list(transform_id = "standardize_unit", target_variables = c("VSSTRESC", "VSSTRESN", "VSSTRESU"), parameters = list(conversion_set_id = "vs_standard_v1", target_unit = "cm")),
    list(transform_id = "standardize_unit", target_variables = c("VSSTRESC", "VSSTRESN", "VSSTRESU"), parameters = list(conversion_set_id = "vs_standard_v1", target_unit = "kg")),
    list(transform_id = "standardize_unit", target_variables = c("VSSTRESC", "VSSTRESN", "VSSTRESU"), parameters = list(conversion_set_id = "vs_standard_v1", target_unit = "C"))
  )
  values <- vapply(seq_along(steps), function(index) {
    one <- state
    one$current_records <- state$current_records[index, ]
    step_standardize_unit(one, concept, steps[[index]], config)$current_records$VSSTRESN
  }, numeric(1))
  expect_equal(values, c(177.8, 81.6, 37.0))
  bad <- state
  bad$current_records <- tibble::tibble(VSTESTCD = "WEIGHT", VSORRES = "180", VSORRESU = "stone")
  expect_error(step_standardize_unit(bad, concept, steps[[2]], config), "未登记")
})

test_that("受控连接在重复键或缺失键时停止", {
  config <- load_v03_config_for_tests("advanced")
  specification <- load_mapping_template(config)
  concept <- concept_lookup(specification)$AE_SOURCE_INTEGRATION
  step <- gold_plan_steps(load_gold_specification(config)$plans$AE_SOURCE_INTEGRATION)[[1]]
  sources <- load_registered_sources_v02(specification, config)
  sources$sae_detail <- dplyr::bind_rows(sources$sae_detail, sources$sae_detail[1, ])
  state <- list(sources = sources, base_dataset = "ae_main", target = tibble::tibble(.SOURCE_ROW = sources$ae_main$.SOURCE_ROW))
  expect_error(step_merge_sources(state, concept, step, config), "不满足 one-to-one")
})
