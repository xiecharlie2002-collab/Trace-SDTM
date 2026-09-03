test_registry_v04 <- function() {
  list(transforms = list(
    list(
      transform_id = "date_transform",
      category = "date_time_conversion",
      parameter_schema = list(
        type = "object", additionalProperties = FALSE, required = "formats",
        properties = list(formats = list(type = "array", items = list(type = "string")))
      )
    ),
    list(
      transform_id = "plain_transform",
      category = "temporal_derivation",
      parameter_schema = list(
        type = "object", additionalProperties = FALSE,
        properties = setNames(list(), character())
      )
    )
  ))
}

test_that("模式驱动比较统一 JSON 数组和 YAML 原子向量", {
  schema <- test_registry_v04()$transforms[[1]]$parameter_schema
  json_value <- jsonlite::fromJSON('{"formats":["m/d/y"]}', simplifyVector = FALSE)
  yaml_value <- list(formats = "m/d/y")

  expect_true(compare_by_schema(json_value, yaml_value, schema))
  expect_identical(
    canonicalize_by_schema(json_value, schema),
    canonicalize_by_schema(yaml_value, schema)
  )
})

test_that("对象键、数组次序和集合扩展按约定比较", {
  object_schema <- list(
    type = "object", additionalProperties = FALSE,
    properties = list(a = list(type = "string"), b = list(type = "number"))
  )
  expect_true(compare_by_schema(list(a = "x", b = 1L), list(b = 1, a = "x"), object_schema))

  ordered <- list(type = "array", items = list(type = "string"))
  unordered <- c(ordered, list(`x-comparison` = "set"))
  expect_false(compare_by_schema(c("a", "b"), c("b", "a"), ordered))
  expect_true(compare_by_schema(c("a", "b"), c("b", "a"), unordered))
  expect_true(compare_by_schema(c("a", "a", "b"), c("b", "a"), unordered))
})

test_that("模式规范化数值但区分缺失成员和显式 null", {
  number_schema <- list(type = "number")
  expect_true(compare_by_schema(1L, 1, number_schema))

  nullable_schema <- list(
    type = "object", additionalProperties = FALSE,
    properties = list(value = list(type = c("string", "null")))
  )
  expect_false(compare_by_schema(list(), list(value = NULL), nullable_schema))
  expect_false(compare_by_schema(list(value = "x", unknown = TRUE), list(value = "x"), nullable_schema))
})

test_that("步骤比较使用各转换函数参数模式", {
  registry <- test_registry_v04()
  proposed <- list(list(
    transform_id = "date_transform", source_ref_ids = list("date_ref"),
    target_variables = list("AESTDTC"), parameters = list(formats = list("m/d/y"))
  ))
  expected <- list(list(
    transform_id = "date_transform", source_ref_ids = "date_ref",
    target_variables = "AESTDTC", parameters = list(formats = "m/d/y")
  ))
  result <- compare_plan_components_v04(proposed, expected, registry)
  expect_true(all(unlist(result, use.names = FALSE)))

  proposed[[1]]$parameters$formats <- c("m/d/y", "y-m-d")
  expect_false(compare_plan_components_v04(proposed, expected, registry)$parameter_correct)
})

test_that("0.2 候选评价复用模式比较且保持旧列接口", {
  registry <- test_registry_v04()
  expected_step <- list(
    transform_id = "date_transform", source_keys = "raw.AESTDT",
    target_variables = "AESTDTC", parameters = list(formats = "m/d/y")
  )
  proposed_step <- expected_step
  proposed_step$parameters$formats <- list("m/d/y")
  candidates <- tibble::tibble(
    concept_id = "AE_START", candidate_rank = 1L, target_domain = "AE",
    categories = "date_time_conversion",
    plan_json = as.character(registry_json(list(steps = list(proposed_step)))),
    recommendation_score = 1, reason = "test", uncertainties = "",
    status = "proposed", review_required = TRUE, provenance = "test", group_id = "ae"
  )
  specification <- list(concepts = list(list(
    concept_id = "AE_START", target_domain = "AE", form_name = "AE",
    difficulty = "simple", source_refs = list()
  )))
  gold <- list(plans = list(AE_START = list(steps = list(expected_step))))

  result <- candidate_evaluation_rows_v02(candidates, specification, gold, registry)
  expect_true(result$parameter_values_correct[[1]])
  expect_true(result$complete_plan_correct[[1]])
})

test_that("0.4 评价分别输出原子、前三项和组装组结果", {
  registry <- test_registry_v04()
  expected <- list(
    list(
      task_id = "T1", assembly_group_id = "A", target_domain = "AE",
      output_kind = "variables", target_variables = c("AESTDTC", "AESTDY"),
      steps = list(list(
        transform_id = "date_transform", source_ref_ids = "date_ref",
        target_variables = c("AESTDTC", "AESTDY"), parameters = list(formats = "m/d/y")
      ))
    ),
    list(
      task_id = "T2", assembly_group_id = "A", target_domain = "AE",
      output_kind = "variables", target_variables = "AESEQ",
      steps = list(list(
        transform_id = "plain_transform", source_ref_ids = "sort_ref",
        target_variables = "AESEQ", parameters = list()
      ))
    )
  )
  candidates <- list(
    list(
      task_id = "T1", candidate_rank = 1L, target_domain = "AE",
      output_kind = "variables", target_variables = c("AESTDY", "AESTDTC"),
      steps = list(list(
        transform_id = "date_transform", source_ref_ids = list("date_ref"),
        target_variables = c("AESTDTC", "AESTDY"), parameters = list(formats = list("m/d/y"))
      ))
    ),
    list(
      task_id = "T2", candidate_rank = 1L, target_domain = "AE",
      output_kind = "variables", target_variables = "AESEQ",
      steps = list(list(
        transform_id = "plain_transform", source_ref_ids = "wrong_ref",
        target_variables = "AESEQ", parameters = list()
      ))
    ),
    list(
      task_id = "T2", candidate_rank = 2L, target_domain = "AE",
      output_kind = "variables", target_variables = "AESEQ",
      steps = list(list(
        transform_id = "plain_transform", source_ref_ids = "sort_ref",
        target_variables = "AESEQ", parameters = list()
      ))
    )
  )

  result <- evaluate_atomic_plans_v04(candidates, expected, registry)
  expect_true(all(c(
    "semantic_correct", "structural_correct", "function_correct", "source_correct",
    "parameter_correct", "complete_plan_correct", "top3"
  ) %in% names(result$detail)))
  expect_true(dplyr::filter(result$atomic, task_id == "T1")$complete_plan_correct[[1]])
  expect_false(dplyr::filter(result$atomic, task_id == "T2")$complete_plan_correct[[1]])
  expect_true(dplyr::filter(result$atomic, task_id == "T2")$top3[[1]])
  expect_false(result$assembly$complete_plan_correct[[1]])
  expect_true(result$assembly$top3[[1]])
})
