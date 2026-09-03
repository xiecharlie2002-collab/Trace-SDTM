test_that("临床概念按域和依赖关系分组且不会产生重复任务", {
  config <- load_v03_config_for_tests("advanced")
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
  config <- load_v03_config_for_tests("advanced")
  registry <- load_transform_registry(config)
  cards <- function_cards_for_model(registry, "unit_conversion")
  expect_setequal(vapply(cards, `[[`, character(1), "transform_id"), "standardize_unit")
  card <- cards[[1]]
  expect_true(all(c("parameter_schema", "not_allowed_when", "examples", "target_contract") %in% names(card)))
  expect_false("assign_no_ct" %in% vapply(cards, `[[`, character(1), "transform_id"))
})

test_that("高级场景为直接来源和派生来源提供字段画像证据", {
  config <- load_v03_config_for_tests("advanced")
  profile_sources(config)
  dictionary <- readr::read_csv(trace_path(config$paths$profile_dir, "source_dictionary.csv"), show_col_types = FALSE)
  specification <- load_mapping_template(config)
  contexts <- lapply(specification$concepts, concept_context, specification = specification, dictionary = dictionary)
  names(contexts) <- vapply(specification$concepts, `[[`, character(1), "concept_id")

  for (context in contexts) {
    expect_length(context$field_profiles, length(context$source_refs))
    if (length(context$source_refs)) {
      expect_false(any(vapply(context$field_profiles, function(x) identical(x$resolution, "unavailable"), logical(1))))
      expect_true(all(vapply(context$field_profiles, function(x) nrow(x$evidence) >= 1L, logical(1))))
    }
  }

  ae_start <- as.character(registry_json(contexts$AE_START$field_profiles))
  expect_match(ae_start, "dd-mmm-yyyy", fixed = TRUE)
  expect_match(ae_start, "UNK-JAN-2025", fixed = TRUE)
  expect_match(ae_start, "UNK-UNK-2025", fixed = TRUE)
  expect_match(ae_start, "derived_candidates", fixed = TRUE)

  vs_profiles <- as.character(registry_json(c(
    contexts$VS_HEIGHT$field_profiles,
    contexts$VS_WEIGHT$field_profiles,
    contexts$VS_TEMP$field_profiles
  )))
  expect_match(vs_profiles, "in | cm", fixed = TRUE)
  expect_match(vs_profiles, "lb | kg", fixed = TRUE)
  expect_match(vs_profiles, "F | C", fixed = TRUE)
})

test_that("提示词使用字符串类别并只提供裁剪后的受控资源", {
  config <- load_v03_config_for_tests("advanced")
  profile_sources(config)
  dictionary <- readr::read_csv(trace_path(config$paths$profile_dir, "source_dictionary.csv"), show_col_types = FALSE)
  specification <- load_mapping_template(config)
  registry <- load_transform_registry(config)
  metadata <- load_metadata(config)
  groups <- recommendation_groups(specification, dictionary, config)
  group <- groups$vs_connected_01
  stage1 <- classification_prompt_v02(group, metadata, registry, config)
  expect_false(grepl("类别编号数组", stage1, fixed = TRUE))
  expect_match(stage1, "类别标识符字符串数组", fixed = TRUE)
  expect_match(stage1, "stage_order 必须非递减", fixed = TRUE)
  expect_match(stage1, "status 只能逐字使用 proposed 或 needs_information", fixed = TRUE)
  expect_match(stage1, "不得使用 ready、classified、success", fixed = TRUE)
  expect_match(stage1, "direct_assignment", fixed = TRUE)

  classifications <- lapply(group$concepts, function(concept) list(
    concept_id = concept$concept_id,
    categories = if (concept$concept_id == "VS_POST_DERIVATIONS") "temporal_derivation" else "direct_assignment",
    classification_score = 1,
    evidence = "test",
    uncertainties = "",
    status = "proposed",
    group_id = group$group_id
  ))
  stage2 <- selection_prompt_v02(group, classifications, metadata, registry, config)
  expect_match(stage2, "trace_visits_v1", fixed = TRUE)
  expect_match(stage2, "vs_standard_v1", fixed = TRUE)
  expect_match(stage2, ".SOURCE_ROW", fixed = TRUE)
  resources <- as.character(registry_json(model_resource_catalog(config)))
  expect_false(grepl("multiplier", resources, fixed = TRUE))
  expect_false(grepl("offset", resources, fixed = TRUE))
  expect_error(assert_blind_prompt("advanced_gold.yml"), "禁止内容")
})

test_that("参考种子生成七张审核工作表并覆盖高级来源字段", {
  config <- load_v03_config_for_tests("advanced")
  recommendations <- seed_recommendations(config)
  expect_equal(nrow(recommendations), length(load_mapping_template(config)$concepts))
  expect_true(all(recommendations$provenance == "reference_seed"))
  dictionary <- readr::read_csv(trace_path(config$paths$profile_dir, "source_dictionary.csv"), show_col_types = FALSE)
  expect_gte(nrow(dictionary), 25L)
  sheets <- openxlsx::getSheetNames(trace_path(config$paths$review_dir, "mapping_review.xlsx"))
  expect_setequal(sheets, c("Instructions", "Concept Review", "Candidate Plans", "Plan Steps", "Final Steps", "Source Context", "Transform Catalog"))
})

test_that("超范围分值、未知函数和信息不足方案被拒绝", {
  config <- load_v03_config_for_tests("advanced")
  specification <- load_mapping_template(config)
  metadata <- load_metadata(config)
  registry <- load_transform_registry(config)
  concept <- concept_lookup(specification)$DM_CONSENT
  classification <- list(concept_id = "DM_CONSENT", categories = "date_time_conversion")
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

test_that("审核比较忽略字符向量与 JSON 列表的表示差异", {
  vector_step <- list(list(
    transform_id = "assign_no_ct",
    source_keys = "ae_merged.AETERM",
    target_variables = "AETERM",
    parameters = list()
  ))
  list_step <- list(list(
    transform_id = "assign_no_ct",
    source_keys = list("ae_merged.AETERM"),
    target_variables = list("AETERM"),
    parameters = list()
  ))
  expect_identical(canonical_steps_v02(vector_step), canonical_steps_v02(list_step))
})

test_that("实验产物按场景隔离且密钥不会进入提示词", {
  config <- load_v03_config_for_tests("advanced")
  routed <- apply_experiment_paths(config, "deepseek-test")
  expect_match(routed$paths$recommendation_dir, "output/benchmark/v1/advanced/experiments/deepseek-test", fixed = TRUE)
  expect_error(apply_experiment_paths(config, "../outside"), "只能包含")
  old <- Sys.getenv("TRACE_SDTM_API_KEY", unset = NA_character_)
  on.exit(if (is.na(old)) Sys.unsetenv("TRACE_SDTM_API_KEY") else Sys.setenv(TRACE_SDTM_API_KEY = old), add = TRUE)
  Sys.setenv(TRACE_SDTM_API_KEY = "secret42")
  profile_sources(config)
  dictionary <- readr::read_csv(trace_path(config$paths$profile_dir, "source_dictionary.csv"), show_col_types = FALSE)
  group <- recommendation_groups(load_mapping_template(config), dictionary, config)[[1]]
  prompt <- classification_prompt_v02(group, load_metadata(config), load_transform_registry(config), config)
  expect_false(grepl("secret42", prompt, fixed = TRUE))
  expect_false(grepl("secret42", sanitize_for_log("Bearer secret42"), fixed = TRUE))
})

test_that("没有批准规格时构建入口被阻止", {
  config <- load_v03_config_for_tests("advanced")
  config$paths$approved_specification <- "specs/not_created_for_test.yml"
  expect_error(load_approved_mapping(config), "尚未生成")
})
