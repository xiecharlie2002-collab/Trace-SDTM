create_review_workbook <- function(recommendations, config = load_project_config(), preapprove = FALSE) {
  top <- recommendations |>
    dplyr::filter(candidate_rank == 1L) |>
    dplyr::arrange(target_domain, mapping_id)
  top$decision <- if (preapprove) "accept" else ""
  top$final_domain <- ""
  top$final_variable <- ""
  top$final_value <- ""
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

  path <- trace_path(config$paths$review_dir, "mapping_review.xlsx")
  ensure_parent(path)
  openxlsx::saveWorkbook(workbook, path, overwrite = TRUE)
  invisible(path)
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
      final_fields <- c("final_domain", "final_variable", "final_transform_id")
      if (any(vapply(final_fields, function(name) is.na(row[[name]]) || !nzchar(trimws(as.character(row[[name]]))), logical(1)))) {
        trace_abort(sprintf("%s 选择 modify 后必须填写 final_domain、final_variable 和 final_transform_id。", row$mapping_id))
      }
      if (!identical(as.character(row$final_domain), located$domain)) {
        trace_abort(sprintf("%s 的最小版本不允许跨域修改。", row$mapping_id))
      }
      entry$target_variable <- as.character(row$final_variable)
      entry$target_value <- cell_text(row$final_value)
      entry$transform_id <- as.character(row$final_transform_id)
      entry$parameters <- from_json_text(cell_text(row$final_parameters, "{}"))
    } else {
      entry$target_variable <- as.character(row$target_variable)
      entry$target_value <- cell_text(row$target_value)
      entry$transform_id <- as.character(row$transform_id)
      entry$parameters <- from_json_text(cell_text(row$transform_parameters, "{}"))
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
