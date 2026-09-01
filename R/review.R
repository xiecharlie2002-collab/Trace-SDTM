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
