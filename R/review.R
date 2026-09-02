create_review_workbook <- function(recommendations, config = load_project_config(), preapprove = FALSE) {
  top <- recommendations |>
    dplyr::filter(candidate_rank == 1L) |>
    dplyr::arrange(target_domain, mapping_id)
  top$decision <- if (preapprove) "accept" else ""
  top$final_domain <- ""
  top$final_variable <- ""
  top$final_value <- ""
  top$final_mapping_type <- ""
  top$final_transform_id <- ""
  top$final_parameters <- ""
  top$review_comment <- if (preapprove) "离线参考种子，已由演示审核者确认。" else ""

  workbook <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(workbook, "Instructions")
  instructions <- data.frame(
    item = c("用途", "允许决策", "accept", "modify", "reject", "needs_information", "重要说明"),
    description = c(
      "逐条审核候选映射，只有完成审核的规格才能构建 SDTM。",
      "accept、modify、reject、needs_information",
      "接受候选方案。",
      "填写 final_* 字段后接受修改方案。",
      "拒绝候选；必需映射被拒绝时不能锁定规格。",
      "信息不足；存在此状态时不能锁定规格。",
      "reference_seed 不是实际模型结果，不能据此声称模型准确率。"
    ),
    stringsAsFactors = FALSE
  )
  openxlsx::writeData(workbook, "Instructions", instructions)
  openxlsx::setColWidths(workbook, "Instructions", cols = 1:2, widths = c(22, 88))

  openxlsx::addWorksheet(workbook, "Mapping Review")
  openxlsx::writeData(workbook, "Mapping Review", top, withFilter = TRUE)
  openxlsx::freezePane(workbook, "Mapping Review", firstRow = TRUE)
  decision_col <- match("decision", names(top))
  openxlsx::dataValidation(
    workbook,
    "Mapping Review",
    cols = decision_col,
    rows = 2:(nrow(top) + 1L),
    type = "list",
    value = '"accept,modify,reject,needs_information"'
  )
  openxlsx::setColWidths(workbook, "Mapping Review", cols = seq_along(top), widths = "auto")

  openxlsx::addWorksheet(workbook, "All Candidates")
  all_candidates <- dplyr::arrange(recommendations, target_domain, mapping_id, candidate_rank)
  openxlsx::writeData(workbook, "All Candidates", all_candidates, withFilter = TRUE)
  openxlsx::freezePane(workbook, "All Candidates", firstRow = TRUE)
  openxlsx::setColWidths(workbook, "All Candidates", cols = seq_along(all_candidates), widths = "auto")

  path <- trace_path(config$paths$review_dir, "mapping_review.xlsx")
  ensure_parent(path)
  openxlsx::saveWorkbook(workbook, path, overwrite = TRUE)
  invisible(path)
}

sort_json_object <- function(value) {
  if (!is.list(value)) return(value)
  if (!is.null(names(value))) value <- value[sort(names(value))]
  lapply(value, sort_json_object)
}

canonical_json_text <- function(value) {
  parsed <- if (is.character(value)) from_json_text(cell_text(value, "{}")) else value
  as_json_text(sort_json_object(parsed %||% list()))
}

mapping_mismatch_fields <- function(candidate, expected) {
  checks <- c(
    target_domain = cell_text(candidate$target_domain) == cell_text(expected$target_domain),
    target_variable = cell_text(candidate$target_variable) == cell_text(expected$target_variable),
    target_value = cell_text(candidate$target_value) == cell_text(expected$target_value),
    mapping_type = cell_text(candidate$mapping_type) == cell_text(expected$mapping_type),
    transform_id = cell_text(candidate$transform_id) == cell_text(expected$transform_id),
    transform_parameters = canonical_json_text(candidate$transform_parameters) == canonical_json_text(expected$transform_parameters)
  )
  names(checks)[!checks]
}

review_against_gold <- function(config = load_project_config()) {
  review_path <- trace_path(config$paths$review_dir, "mapping_review.xlsx")
  if (!file.exists(review_path)) trace_abort("缺少审核工作簿，请先执行真实 recommend。")
  review <- openxlsx::read.xlsx(review_path, sheet = "Mapping Review", check.names = FALSE)
  expected <- dplyr::filter(flatten_mapping_tasks(load_mapping_template(config)), include_in_recommendation)
  expected_rows <- split(expected, expected$mapping_id)

  for (row_index in seq_len(nrow(review))) {
    mapping_id <- as.character(review$mapping_id[[row_index]])
    expected_row <- expected_rows[[mapping_id]]
    if (is.null(expected_row) || nrow(expected_row) != 1L) trace_abort(sprintf("金标准中无法唯一定位 %s。", mapping_id))
    candidate_row <- review[row_index, , drop = FALSE]
    mismatches <- mapping_mismatch_fields(candidate_row, expected_row)
    if (!length(mismatches)) {
      review$decision[[row_index]] <- "accept"
      review$review_comment[[row_index]] <- "评价用金标准复核：候选与专家模板完全一致。"
    } else {
      review$decision[[row_index]] <- "modify"
      review$final_domain[[row_index]] <- expected_row$target_domain[[1]]
      review$final_variable[[row_index]] <- expected_row$target_variable[[1]]
      review$final_value[[row_index]] <- expected_row$target_value[[1]]
      review$final_mapping_type[[row_index]] <- expected_row$mapping_type[[1]]
      review$final_transform_id[[row_index]] <- expected_row$transform_id[[1]]
      review$final_parameters[[row_index]] <- expected_row$transform_parameters[[1]]
      review$review_comment[[row_index]] <- paste0(
        "评价用金标准复核：修改 ", paste(mismatches, collapse = "、"),
        "。这不是法规意义上的独立专家签字。"
      )
    }
  }

  workbook <- openxlsx::loadWorkbook(review_path)
  openxlsx::writeData(workbook, "Mapping Review", review, startRow = 1L, startCol = 1L, colNames = TRUE)
  openxlsx::saveWorkbook(workbook, review_path, overwrite = TRUE)
  summary <- list(
    reviewer_method = "gold_standard_comparison_v1",
    reviewed_at = utc_now(),
    total = nrow(review),
    accepted = sum(review$decision == "accept"),
    modified = sum(review$decision == "modify"),
    rejected = sum(review$decision == "reject"),
    needs_information = sum(review$decision == "needs_information"),
    disclaimer = "本步骤用于作品集实验评价，不等同于法规流程中的独立临床标准专家签字。"
  )
  write_json(summary, trace_path(config$paths$review_dir, "expert_review_summary.json"))
  trace_info("评价审核完成：接受 %d，修改 %d，拒绝 %d。", summary$accepted, summary$modified, summary$rejected)
  invisible(review)
}

cell_text <- function(x, default = "") {
  if (is.null(x) || length(x) == 0L || is.na(x)) default else as.character(x)
}

find_mapping_entry <- function(specification, mapping_id) {
  for (domain in names(specification$domains)) {
    for (section in c("mappings", "transpose_mappings")) {
      entries <- specification$domains[[domain]][[section]] %||% list()
      ids <- vapply(entries, function(entry) as.character(entry$mapping_id), character(1))
      hit <- match(mapping_id, ids)
      if (!is.na(hit)) return(list(domain = domain, section = section, index = hit, entry = entries[[hit]]))
    }
  }
  NULL
}

approve_mapping <- function(config = load_project_config()) {
  review_path <- trace_path(config$paths$review_dir, "mapping_review.xlsx")
  if (!file.exists(review_path)) trace_abort("缺少 mapping_review.xlsx。请先执行 recommend 或 recommend --seed。")
  review <- openxlsx::read.xlsx(review_path, sheet = "Mapping Review", check.names = FALSE)
  required_columns <- c("mapping_id", "decision", "target_domain", "target_variable", "transform_id")
  missing <- setdiff(required_columns, names(review))
  if (length(missing)) trace_abort(sprintf("审核工作簿缺少字段：%s", paste(missing, collapse = ", ")))

  review$decision <- tolower(trimws(as.character(review$decision)))
  allowed <- c("accept", "modify", "reject", "needs_information")
  if (any(!review$decision %in% allowed)) trace_abort("每条映射都必须选择有效 decision。")
  if (any(review$decision == "needs_information")) trace_abort("仍有 needs_information，不能锁定规格。")

  specification <- load_mapping_template(config)
  tasks <- flatten_mapping_tasks(specification)
  metadata <- load_metadata(config)
  allowed_types <- unlist(config$model$allowed_mapping_types)
  allowed_transforms <- unlist(config$model$allowed_transform_ids)
  required_ids <- tasks$mapping_id[tasks$mapping_required]
  rejected_required <- review$mapping_id[review$decision == "reject" & review$mapping_id %in% required_ids]
  if (length(rejected_required)) {
    trace_abort(sprintf("必需映射不能直接拒绝：%s。请改为 modify 并填写最终方案。", paste(rejected_required, collapse = ", ")))
  }

  for (row_index in seq_len(nrow(review))) {
    row <- review[row_index, , drop = FALSE]
    located <- find_mapping_entry(specification, as.character(row$mapping_id))
    if (is.null(located)) trace_abort(sprintf("审核表包含未知 mapping_id：%s", row$mapping_id))
    entries <- specification$domains[[located$domain]][[located$section]]

    if (row$decision == "reject") {
      entries[[located$index]] <- NULL
      specification$domains[[located$domain]][[located$section]] <- entries
      next
    }

    entry <- entries[[located$index]]
    if (row$decision == "modify") {
      final_fields <- c("final_domain", "final_variable", "final_mapping_type", "final_transform_id")
      if (any(vapply(final_fields, function(name) is.na(row[[name]]) || !nzchar(trimws(as.character(row[[name]]))), logical(1)))) {
        trace_abort(sprintf("%s 选择 modify 后必须填写 final_domain、final_variable、final_mapping_type 和 final_transform_id。", row$mapping_id))
      }
      if (!identical(as.character(row$final_domain), located$domain)) {
        trace_abort(sprintf("%s 的最小版本不允许跨域修改。", row$mapping_id))
      }
      entry$target_variable <- as.character(row$final_variable)
      entry$target_value <- cell_text(row$final_value)
      entry$mapping_type <- as.character(row$final_mapping_type)
      entry$transform_id <- as.character(row$final_transform_id)
      entry$parameters <- from_json_text(cell_text(row$final_parameters, "{}"))
    } else {
      entry$target_variable <- as.character(row$target_variable)
      entry$target_value <- cell_text(row$target_value)
      entry$mapping_type <- as.character(row$mapping_type)
      entry$transform_id <- as.character(row$transform_id)
      entry$parameters <- from_json_text(cell_text(row$transform_parameters, "{}"))
    }
    if (!entry$target_variable %in% names(metadata$domains[[located$domain]]$variables)) {
      trace_abort(sprintf("%s 的最终目标变量不在元数据中：%s。", row$mapping_id, entry$target_variable))
    }
    if (!entry$mapping_type %in% allowed_types) {
      trace_abort(sprintf("%s 的最终映射类型不受允许：%s。", row$mapping_id, entry$mapping_type))
    }
    if (!entry$transform_id %in% allowed_transforms) {
      trace_abort(sprintf("%s 的最终转换函数不受允许：%s。", row$mapping_id, entry$transform_id))
    }
    entries[[located$index]] <- entry
    specification$domains[[located$domain]][[located$section]] <- entries
  }

  reviewer <- Sys.getenv("TRACE_SDTM_REVIEWER", unset = "portfolio_demo_reviewer")
  specification$specification$status <- "approved"
  specification$specification$approval <- list(
    reviewer = reviewer,
    approved_at = utc_now(),
    review_workbook_sha256 = file_sha256(review_path)
  )
  output <- trace_path(config$paths$approved_specification)
  ensure_parent(output)
  yaml::write_yaml(specification, output)
  write_csv(review, trace_path(config$paths$review_dir, "mapping_review_audit.csv"))
  trace_info("已锁定审核规格：%s", output)
  invisible(specification)
}

# -----------------------------------------------------------------------------
# 0.2：以临床概念及完整函数链为审核单位。

plan_steps_table_v02 <- function(candidates) {
  purrr::pmap_dfr(candidates, function(concept_id, candidate_rank, target_domain, categories,
                                      plan_json, recommendation_score, reason, uncertainties,
                                      status, review_required, provenance, group_id, ...) {
    plan <- from_json_text(plan_json)
    steps <- plan$steps %||% list()
    if (!length(steps)) {
      return(tibble::tibble(
        concept_id = concept_id, candidate_rank = as.integer(candidate_rank), step_order = NA_integer_,
        transform_id = "", source_keys = "[]", target_variables = "[]", parameters = "{}"
      ))
    }
    purrr::imap_dfr(steps, function(step, index) tibble::tibble(
      concept_id = concept_id,
      candidate_rank = as.integer(candidate_rank),
      step_order = as.integer(index),
      transform_id = as.character(step$transform_id),
      source_keys = as.character(registry_json(as.list(unname(unlist(step$source_keys %||% character()))))),
      target_variables = as.character(registry_json(as.list(unname(unlist(step$target_variables %||% character()))))),
      parameters = as.character(registry_json(step$parameters %||% list()))
    ))
  })
}

source_context_table_v02 <- function(specification, config) {
  dictionary_path <- trace_path(config$paths$profile_dir, "source_dictionary.csv")
  dictionary <- if (file.exists(dictionary_path)) readr::read_csv(dictionary_path, show_col_types = FALSE) else tibble::tibble()
  purrr::map_dfr(specification$concepts, function(concept) {
    refs <- concept_source_refs(concept)
    if (!length(refs)) {
      return(tibble::tibble(
        concept_id = concept$concept_id, target_domain = concept$target_domain,
        form_name = concept$form_name, depends_on = paste(unlist(concept$depends_on %||% character()), collapse = " | "),
        source_dataset = "", source_variable = "", role = "", example_values = "",
        missing_rate = NA_real_, units = "", relationship = concept$expected_cardinality
      ))
    }
    purrr::map_dfr(refs, function(ref) {
      profile <- dplyr::filter(dictionary, source_dataset == ref$dataset, source_variable == ref$variable)
      tibble::tibble(
        concept_id = concept$concept_id,
        target_domain = concept$target_domain,
        form_name = concept$form_name,
        depends_on = paste(unlist(concept$depends_on %||% character()), collapse = " | "),
        source_dataset = ref$dataset,
        source_variable = ref$variable,
        role = ref$role,
        example_values = if (nrow(profile)) as.character(profile$example_values[[1]] %||% "") else "",
        missing_rate = if (nrow(profile)) as.numeric(profile$missing_rate[[1]] %||% NA_real_) else NA_real_,
        units = if (nrow(profile) && "unit_values" %in% names(profile)) as.character(profile$unit_values[[1]] %||% "") else "",
        relationship = concept$expected_cardinality
      )
    })
  })
}

write_review_sheet <- function(workbook, name, data, filter = TRUE) {
  openxlsx::addWorksheet(workbook, name)
  openxlsx::writeData(workbook, name, data, withFilter = filter)
  openxlsx::freezePane(workbook, name, firstRow = TRUE)
  if (ncol(data)) openxlsx::setColWidths(workbook, name, cols = seq_len(ncol(data)), widths = "auto")
}

create_review_workbook <- function(recommendations, config = load_project_config(), preapprove = FALSE) {
  specification <- load_mapping_template(config)
  registry <- load_transform_registry(config)
  candidates <- dplyr::arrange(recommendations, target_domain, concept_id, candidate_rank)
  top <- candidates |>
    dplyr::filter(candidate_rank == 1L) |>
    dplyr::arrange(target_domain, concept_id)
  concept_map <- concept_lookup(specification)
  review <- purrr::pmap_dfr(top, function(concept_id, candidate_rank, target_domain, categories,
                                         plan_json, recommendation_score, reason, uncertainties,
                                         status, review_required, provenance, group_id, ...) {
    concept <- concept_map[[concept_id]]
    tibble::tibble(
      concept_id = concept_id,
      target_domain = target_domain,
      form_name = concept$form_name,
      required = isTRUE(concept$required),
      selected_rank = as.integer(candidate_rank),
      recommendation_score = as.numeric(recommendation_score),
      model_status = status,
      decision = if (preapprove && status == "proposed") "accept" else "",
      review_comment = if (preapprove) "专家金标准种子，仅用于离线演示。" else ""
    )
  })
  steps <- plan_steps_table_v02(candidates)
  final_steps <- dplyr::filter(steps, candidate_rank == 1L) |>
    dplyr::select(-candidate_rank)
  source_context <- source_context_table_v02(specification, config)
  catalog <- registry_catalog_table(registry)

  workbook <- openxlsx::createWorkbook()
  write_review_sheet(workbook, "Instructions", data.frame(
    item = c("审核单位", "accept", "modify", "reject", "needs_information", "构建边界", "种子说明"),
    description = c(
      "每行是一个临床概念；接受或修改的是完整候选方案，不是孤立字段。",
      "接受 selected_rank 指定的整套候选方案。",
      "在 Final Steps 中写出完整函数链，并确保通过注册表检查。",
      "拒绝非必需概念；必需概念不能拒绝。",
      "信息不足未解决时不能批准或构建。",
      "正式构建只读取批准后的 YAML，构建时不会调用模型。",
      "reference_seed 来自专家金标准，不能作为模型准确率结果。"
    ), stringsAsFactors = FALSE
  ), filter = FALSE)
  write_review_sheet(workbook, "Concept Review", review)
  decision_col <- match("decision", names(review))
  if (nrow(review)) openxlsx::dataValidation(
    workbook, "Concept Review", cols = decision_col, rows = 2:(nrow(review) + 1L),
    type = "list", value = '"accept,modify,reject,needs_information"'
  )
  write_review_sheet(workbook, "Candidate Plans", candidates)
  write_review_sheet(workbook, "Plan Steps", steps)
  write_review_sheet(workbook, "Final Steps", final_steps)
  write_review_sheet(workbook, "Source Context", source_context)
  write_review_sheet(workbook, "Transform Catalog", catalog)
  path <- trace_path(config$paths$review_dir, "mapping_review.xlsx")
  ensure_parent(path)
  openxlsx::saveWorkbook(workbook, path, overwrite = TRUE)
  invisible(path)
}

steps_from_table_v02 <- function(data, concept_id) {
  rows <- dplyr::filter(data, .data$concept_id == .env$concept_id) |>
    dplyr::arrange(step_order)
  if (!nrow(rows) || all(!nzchar(trimws(as.character(rows$transform_id))))) return(list())
  purrr::pmap(rows, function(concept_id, step_order, transform_id, source_keys, target_variables, parameters, ...) list(
    transform_id = as.character(transform_id),
    source_keys = unname(unlist(from_json_text(as.character(source_keys)), use.names = FALSE)),
    target_variables = unname(unlist(from_json_text(as.character(target_variables)), use.names = FALSE)),
    parameters = from_json_text(as.character(parameters))
  ))
}

candidate_steps_v02 <- function(candidates, concept_id, rank) {
  hit <- dplyr::filter(candidates, .data$concept_id == .env$concept_id, .data$candidate_rank == .env$rank)
  if (nrow(hit) != 1L) trace_abort(sprintf("%s 无法唯一定位候选序号 %s。", concept_id, rank))
  from_json_text(hit$plan_json[[1]])$steps %||% list()
}

canonical_steps_v02 <- function(steps) registry_json(lapply(steps, function(step) list(
  transform_id = as.character(step$transform_id),
  source_keys = as.list(unname(as.character(unlist(step$source_keys %||% character(), use.names = FALSE)))),
  target_variables = as.list(unname(as.character(unlist(step$target_variables %||% character(), use.names = FALSE)))),
  parameters = sort_json_object(step$parameters %||% list())
)))

review_against_gold <- function(config = load_project_config()) {
  review_path <- trace_path(config$paths$review_dir, "mapping_review.xlsx")
  if (!file.exists(review_path)) trace_abort("缺少审核工作簿，请先执行 recommend。")
  review <- openxlsx::read.xlsx(review_path, sheet = "Concept Review", check.names = FALSE)
  candidates <- openxlsx::read.xlsx(review_path, sheet = "Candidate Plans", check.names = FALSE)
  final_steps <- openxlsx::read.xlsx(review_path, sheet = "Final Steps", check.names = FALSE)
  gold <- load_gold_specification(config)$plans
  replacement <- list()
  for (index in seq_len(nrow(review))) {
    id <- as.character(review$concept_id[[index]])
    expected <- gold_plan_steps(gold[[id]])
    proposed <- candidate_steps_v02(candidates, id, as.integer(review$selected_rank[[index]]))
    if (identical(canonical_steps_v02(proposed), canonical_steps_v02(expected))) {
      review$decision[[index]] <- "accept"
      review$review_comment[[index]] <- "评价用金标准复核：完整函数链一致。"
    } else {
      review$decision[[index]] <- "modify"
      review$review_comment[[index]] <- "评价用金标准复核：已在 Final Steps 中替换为专家函数链；不等同于法规签字。"
    }
    replacement[[id]] <- expected
  }
  final_steps <- purrr::imap_dfr(replacement, function(steps, id) {
    purrr::imap_dfr(steps, function(step, order) tibble::tibble(
      concept_id = id, step_order = as.integer(order), transform_id = step$transform_id,
      source_keys = as.character(registry_json(as.list(unname(unlist(step$source_keys %||% character()))))),
      target_variables = as.character(registry_json(as.list(unname(unlist(step$target_variables %||% character()))))),
      parameters = as.character(registry_json(step$parameters %||% list()))
    ))
  })
  workbook <- openxlsx::loadWorkbook(review_path)
  openxlsx::writeData(workbook, "Concept Review", review, startRow = 1L, startCol = 1L, colNames = TRUE)
  openxlsx::writeData(workbook, "Final Steps", final_steps, startRow = 1L, startCol = 1L, colNames = TRUE)
  openxlsx::saveWorkbook(workbook, review_path, overwrite = TRUE)
  summary <- list(
    reviewer_method = "gold_standard_concept_plan_v02", reviewed_at = utc_now(), total = nrow(review),
    accepted = sum(review$decision == "accept"), modified = sum(review$decision == "modify"),
    rejected = sum(review$decision == "reject"), needs_information = sum(review$decision == "needs_information"),
    disclaimer = "用于作品集实验评价，不等同于法规流程中的独立临床标准专家签字。"
  )
  write_json(summary, trace_path(config$paths$review_dir, "expert_review_summary.json"))
  trace_info("概念方案复核完成：接受 %d，修改 %d。", summary$accepted, summary$modified)
  invisible(review)
}

approve_mapping <- function(config = load_project_config()) {
  review_path <- trace_path(config$paths$review_dir, "mapping_review.xlsx")
  if (!file.exists(review_path)) trace_abort("缺少 mapping_review.xlsx。请先执行 recommend。")
  review <- openxlsx::read.xlsx(review_path, sheet = "Concept Review", check.names = FALSE)
  candidates <- openxlsx::read.xlsx(review_path, sheet = "Candidate Plans", check.names = FALSE)
  final_steps <- openxlsx::read.xlsx(review_path, sheet = "Final Steps", check.names = FALSE)
  required_columns <- c("concept_id", "selected_rank", "decision")
  missing <- setdiff(required_columns, names(review))
  if (length(missing)) trace_abort(sprintf("Concept Review 缺少字段：%s", paste(missing, collapse = ", ")))
  review$decision <- tolower(trimws(as.character(review$decision)))
  if (any(!review$decision %in% c("accept", "modify", "reject", "needs_information"))) {
    trace_abort("每个临床概念都必须选择有效审核决定。")
  }
  if (any(review$decision == "needs_information")) trace_abort("仍有 needs_information，不能锁定规格。")

  specification <- load_mapping_template(config)
  metadata <- load_metadata(config)
  registry <- load_transform_registry(config)
  concepts <- concept_lookup(specification)
  approved <- list()
  for (index in seq_len(nrow(review))) {
    id <- as.character(review$concept_id[[index]])
    concept <- concepts[[id]]
    if (is.null(concept)) trace_abort(sprintf("审核表包含未知概念：%s", id))
    decision <- review$decision[[index]]
    if (decision == "reject") {
      if (isTRUE(concept$required)) trace_abort(sprintf("必需概念 %s 不能拒绝。", id))
      next
    }
    steps <- if (decision == "accept") {
      candidate_steps_v02(candidates, id, as.integer(review$selected_rank[[index]]))
    } else {
      steps_from_table_v02(final_steps, id)
    }
    if (!length(steps)) trace_abort(sprintf("%s 没有完整的最终函数链。", id))
    concept$steps <- steps
    concept$review <- list(
      decision = decision,
      reviewer = Sys.getenv("TRACE_SDTM_REVIEWER", unset = "portfolio_demo_reviewer"),
      reviewed_at = utc_now(),
      comment = as.character(review$review_comment[[index]] %||% "")
    )
    validate_concept_plan(concept, specification, metadata, registry, config)
    approved[[id]] <- concept
  }
  template_order <- vapply(specification$concepts, `[[`, character(1), "concept_id")
  specification$concepts <- unname(approved[intersect(template_order, names(approved))])
  reviewer <- Sys.getenv("TRACE_SDTM_REVIEWER", unset = "portfolio_demo_reviewer")
  specification$specification$status <- "approved"
  specification$specification$approval <- list(
    reviewer = reviewer, approved_at = utc_now(), review_workbook_sha256 = file_sha256(review_path),
    registry_version = registry$registry_version,
    registry_sha256 = file_sha256(trace_path(config$paths$transform_registry))
  )
  validate_specification_v02(specification, config, require_approved = TRUE)
  output <- trace_path(config$paths$approved_specification)
  ensure_parent(output)
  yaml::write_yaml(specification, output)
  write_csv(review, trace_path(config$paths$review_dir, "concept_review_audit.csv"))
  write_csv(final_steps, trace_path(config$paths$review_dir, "final_steps_audit.csv"))
  trace_info("已锁定 0.2 审核规格：%s", output)
  invisible(specification)
}
