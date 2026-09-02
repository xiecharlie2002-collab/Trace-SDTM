test_that("临床概念按域和依赖关系分组且不会产生重复任务", {
  config <- load_project_config("advanced")
  profile_sources(config)
  dictionary <- readr::read_csv(trace_path(config$paths$profile_dir, "source_dictionary.csv"), show_col_types = FALSE)
  specification <- load_mapping_template(config)
  groups <- recommendation_groups(specification, dictionary, config)
  ids <- unlist(lapply(groups, function(group) vapply(group$concepts, `[[`, character(1), "concept_id")))
  expect_identical(anyDuplicated(ids), 0L)
  expect_setequal(ids, vapply(specification$concepts, `[[`, character(1), "concept_id"))
  group_of <- stats::setNames(rep(names(groups), vapply(groups, function(group) length(group$concepts), integer(1))), ids)
  for (concept in specification$concepts) {
    dependencies <- intersect(unlist(concept$depends_on %||% character()), ids)
    same_domain <- dependencies[vapply(dependencies, function(id) concept_lookup(specification)[[id]]$target_domain == concept$target_domain, logical(1))]
    expect_true(all(group_of[same_domain] == group_of[[concept$concept_id]]))
  }
})

test_that("第二阶段只看到第一阶段类别内函数的完整函数卡", {
  config <- load_project_config("advanced")
  registry <- load_transform_registry(config)
  cards <- function_cards_for_model(registry, "unit_conversion")
  expect_setequal(vapply(cards, `[[`, character(1), "transform_id"), "standardize_unit")
  card <- cards[[1]]
  expect_true(all(c("parameter_schema", "not_allowed_when", "examples", "target_contract") %in% names(card)))
  expect_false("assign_no_ct" %in% vapply(cards, `[[`, character(1), "transform_id"))
})

test_that("参考种子生成七张审核工作表并覆盖高级来源字段", {
  config <- load_project_config("advanced")
  recommendations <- seed_recommendations(config)
  expect_equal(nrow(recommendations), length(load_mapping_template(config)$concepts))
  expect_true(all(recommendations$provenance == "reference_seed"))
  dictionary <- readr::read_csv(trace_path(config$paths$profile_dir, "source_dictionary.csv"), show_col_types = FALSE)
  expect_gte(nrow(dictionary), 25L)
  sheets <- openxlsx::getSheetNames(trace_path(config$paths$review_dir, "mapping_review.xlsx"))
  expect_setequal(sheets, c("Instructions", "Concept Review", "Candidate Plans", "Plan Steps", "Final Steps", "Source Context", "Transform Catalog"))
})

test_that("超范围分值、未知函数和信息不足方案被拒绝", {
  config <- load_project_config("advanced")
  specification <- load_mapping_template(config)
  metadata <- load_metadata(config)
  registry <- load_transform_registry(config)
  concept <- concept_lookup(specification)$DM_CONSENT
  classification <- list(concept_id = "DM_CONSENT", categories = "datetime_conversion")
  base <- list(
    concept_id = "DM_CONSENT", candidate_rank = 1L, target_domain = "DM",
    steps = gold_plan_steps(load_gold_specification(config)$plans$DM_CONSENT), recommendation_score = 1.2,
    reason = "test", uncertainties = "", status = "proposed", review_required = TRUE
  )
  expect_error(validate_candidate_plan_v02(base, classification, concept, specification, metadata, registry, "dm", "model"), "超出")
  base$recommendation_score <- 0.8
  base$steps[[1]]$transform_id <- "execute_arbitrary_code"
  expect_error(validate_candidate_plan_v02(base, classification, concept, specification, metadata, registry, "dm", "model"), "未登记")
  base$steps <- gold_plan_steps(load_gold_specification(config)$plans$DM_CONSENT)
  base$status <- "needs_information"
  expect_error(validate_candidate_plan_v02(base, classification, concept, specification, metadata, registry, "dm", "model"), "不能附带")
})

test_that("实验产物按场景隔离且密钥不会进入提示词", {
  config <- load_project_config("advanced")
  routed <- apply_experiment_paths(config, "deepseek-test")
  expect_match(routed$paths$recommendation_dir, "output/v0.2/advanced/experiments/deepseek-test", fixed = TRUE)
  expect_error(apply_experiment_paths(config, "../outside"), "只能包含")
  old <- Sys.getenv("TRACE_SDTM_API_KEY", unset = NA_character_)
  on.exit(if (is.na(old)) Sys.unsetenv("TRACE_SDTM_API_KEY") else Sys.setenv(TRACE_SDTM_API_KEY = old), add = TRUE)
  Sys.setenv(TRACE_SDTM_API_KEY = "test-secret-value")
  profile_sources(config)
  dictionary <- readr::read_csv(trace_path(config$paths$profile_dir, "source_dictionary.csv"), show_col_types = FALSE)
  group <- recommendation_groups(load_mapping_template(config), dictionary, config)[[1]]
  prompt <- classification_prompt_v02(group, load_metadata(config), load_transform_registry(config), config)
  expect_false(grepl("test-secret-value", prompt, fixed = TRUE))
  expect_false(grepl("test-secret-value", sanitize_for_log("Bearer test-secret-value"), fixed = TRUE))
})

test_that("没有批准规格时构建入口被阻止", {
  config <- load_project_config("advanced")
  config$paths$approved_specification <- "specs/not_created_for_test.yml"
  expect_error(load_approved_mapping(config), "尚未生成")
})
