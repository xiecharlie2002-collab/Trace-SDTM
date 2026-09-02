#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = FALSE)
file_argument <- sub("^--file=", "", arguments[grepl("^--file=", arguments)])
if (!length(file_argument)) stop("无法确定脚本位置。", call. = FALSE)
project_root <- normalizePath(file.path(dirname(file_argument[[1]]), "..", ".."), winslash = "/", mustWork = TRUE)
Sys.setenv(TRACE_SDTM_ROOT = project_root)
project_library <- file.path(project_root, ".Rlib")
if (dir.exists(project_library)) .libPaths(c(project_library, .libPaths()))

source_order <- c(
  "utils.R", "config.R", "registry.R", "profile.R", "recommend.R", "review.R",
  "transforms.R", "build.R", "validate_local.R", "p21.R", "evaluate.R",
  "report.R", "pipeline.R"
)
for (file in source_order) source(file.path(project_root, "R", file), encoding = "UTF-8")

experiment_root <- trace_path("output", "v0.2", "advanced", "experiments")
provider_ids <- c(
  codex_subagent = "codex-subagent-contextfix-20260902",
  deepseek_v4_flash = "deepseek-v4-flash-contextfix-20260902"
)
group_ids <- c("dm_connected_01", "ae_connected_01", "vs_connected_01")
paired_dir <- ensure_dir(file.path(experiment_root, "contextfix-paired-20260902"))

read_csv_if_exists <- function(path) {
  if (!file.exists(path)) return(tibble::tibble())
  readr::read_csv(path, show_col_types = FALSE)
}

read_json_if_exists <- function(path, default = list()) {
  if (!file.exists(path)) return(default)
  jsonlite::read_json(path, simplifyVector = TRUE)
}

provider_result <- function(provider, experiment_id) {
  base <- file.path(experiment_root, experiment_id)
  recommendation_dir <- file.path(base, "recommendations")
  model_run_path <- file.path(recommendation_dir, "model_run.json")
  if (!file.exists(model_run_path)) {
    return(list(
      provider = provider,
      experiment_id = experiment_id,
      status = "pending_missing_rotated_api_key",
      total = 21L
    ))
  }
  classifications <- read_csv_if_exists(file.path(recommendation_dir, "category_classifications.csv"))
  candidates <- read_csv_if_exists(file.path(recommendation_dir, "candidate_plans.csv"))
  evaluation <- read_json_if_exists(file.path(recommendation_dir, "mapping_evaluation_summary.json"))
  review <- read_json_if_exists(file.path(base, "review", "expert_review_summary.json"))
  candidate_concepts <- if (nrow(candidates)) length(unique(candidates$concept_id[candidates$candidate_rank == 1L])) else 0L
  list(
    provider = provider,
    experiment_id = experiment_id,
    status = as.character(evaluation$status %||% read_json_if_exists(model_run_path)$status %||% "completed"),
    total = 21L,
    stage1_valid = if (nrow(classifications)) length(unique(classifications$concept_id)) else 0L,
    stage1_correct = as.integer(evaluation$stage1_category_correct %||% 0L),
    stage2_valid = candidate_concepts,
    category_top1 = as.integer(evaluation$category_top1_correct %||% 0L),
    category_top3 = as.integer(evaluation$category_top3_hit %||% 0L),
    function_correct = as.integer(evaluation$function_correct_given_category_correct %||% 0L),
    function_denominator = as.integer(evaluation$function_given_category_denominator %||% 0L),
    target_complete = as.integer(evaluation$target_set_complete %||% 0L),
    complete_top1 = as.integer(evaluation$complete_plan_top1_correct %||% 0L),
    complete_top3 = as.integer(evaluation$complete_plan_top3_hit %||% 0L),
    parameter_valid = as.integer(evaluation$parameter_schema_first_pass %||% 0L),
    parameter_denominator = as.integer(evaluation$parameter_schema_denominator %||% 0L),
    accepted = as.integer(review$accepted %||% evaluation$accepted %||% 0L),
    modified = as.integer(review$modified %||% evaluation$modified %||% 0L),
    rejected = as.integer(review$rejected %||% evaluation$rejected %||% 0L),
    needs_information = as.integer(review$needs_information %||% evaluation$needs_information %||% 0L),
    rejected_group_concepts = as.integer(evaluation$rejected_group_concepts %||% 0L)
  )
}

results <- Map(provider_result, names(provider_ids), unname(provider_ids))
names(results) <- names(provider_ids)

metric_rows <- function(result) {
  if (identical(result$status, "pending_missing_rotated_api_key")) {
    return(tibble::tibble(
      provider = result$provider,
      metric = "experiment_status",
      numerator = NA_integer_, denominator = result$total,
      denominator_type = "all_21_concepts",
      note = "等待已轮换的新接口密钥，未运行。"
    ))
  }
  tibble::tribble(
    ~provider, ~metric, ~numerator, ~denominator, ~denominator_type, ~note,
    result$provider, "stage1_valid_classification", result$stage1_valid, result$total, "all_21_concepts", "严格结构校验通过。",
    result$provider, "stage1_category_chain_correct", result$stage1_correct, result$total, "all_21_concepts", "第一阶段类别链与金标准一致。",
    result$provider, "stage2_valid_candidate", result$stage2_valid, result$total, "all_21_concepts", "至少有一套通过严格校验的首选方案。",
    result$provider, "candidate_category_top1", result$category_top1, result$total, "all_21_concepts", "未通过结构校验或无候选者按未命中计。",
    result$provider, "candidate_category_top3", result$category_top3, result$total, "all_21_concepts", "本次每个概念实际只返回一个候选。",
    result$provider, "target_set_complete", result$target_complete, result$total, "all_21_concepts", "目标变量集合无遗漏、无多报。",
    result$provider, "complete_plan_top1", result$complete_top1, result$total, "all_21_concepts", "函数、来源、目标和参数全部一致。",
    result$provider, "complete_plan_top3", result$complete_top3, result$total, "all_21_concepts", "本次每个概念实际只返回一个候选。",
    result$provider, "function_correct_given_category", result$function_correct, result$function_denominator, "validated_candidates_with_correct_category", "通过结构校验且类别正确后的条件结果。",
    result$provider, "parameter_schema_first_pass", result$parameter_valid, result$parameter_denominator, "strictly_validated_candidates", "整组被拒绝的原始候选不进入该条件分母。",
    result$provider, "expert_accept", result$accepted, result$stage2_valid, "strictly_validated_candidates", "金标准事后复核直接接受。",
    result$provider, "expert_modify", result$modified, result$stage2_valid, "strictly_validated_candidates", "金标准事后复核修改后接受。"
  )
}

comparison <- purrr::map_dfr(results, metric_rows)
write_csv(comparison, file.path(paired_dir, "paired_metrics.csv"))

prompt_hashes <- purrr::map_dfr(group_ids, function(group_id) {
  codex_path <- file.path(experiment_root, provider_ids[["codex_subagent"]], "recommendations", "groups", group_id, "stage1_request.txt")
  deepseek_path <- file.path(experiment_root, provider_ids[["deepseek_v4_flash"]], "recommendations", "groups", group_id, "stage1_request.txt")
  codex_hash <- if (file.exists(codex_path)) file_sha256(codex_path) else NA_character_
  deepseek_hash <- if (file.exists(deepseek_path)) file_sha256(deepseek_path) else NA_character_
  tibble::tibble(
    group_id = group_id,
    codex_stage1_sha256 = codex_hash,
    deepseek_stage1_sha256 = deepseek_hash,
    identical = !is.na(codex_hash) && identical(codex_hash, deepseek_hash)
  )
})
write_csv(prompt_hashes, file.path(paired_dir, "stage1_prompt_hashes.csv"))

error_taxonomy <- tibble::tribble(
  ~provider, ~error_type, ~scope, ~affected_concepts, ~description, ~evidence,
  "codex_subagent", "structure_error", "AE", 8L, "把派生数据集 ae_merged 写入 target_variables，整组第二阶段被严格拒绝。", "recommendations/groups/ae_connected_01/stage2_validation_failure.json",
  "codex_subagent", "context_gap", "DM", 3L, "SITEID 来源、USUBJID 格式和部分日期格式没有明确到足以确定执行。", "DM_CORE、DM_REFERENCE_START、DM_CONSENT 原始回答",
  "codex_subagent", "context_contract_mismatch", "AE", 2L, "画像把时间字段标为 hms，但不完整日期函数卡只接受 character；推荐者因此停止 AE_START 和 AE_END。", "AE_START、AE_END 原始回答",
  "codex_subagent", "professional_judgment_error", "VS", 3L, "同单位检查未选择 standardize_unit，导致 SYSBP、DIABP、PULSE 的类别链和函数链偏离金标准。", "VS 第二阶段原始回答",
  "codex_subagent", "context_misread", "VS", 1L, "VS_CORE 额外选择 source_integration；来源目录只声明一个 vs_mixed 数据集，DM 是依赖而非待合并来源。", "VS 第一阶段原始回答",
  "codex_subagent", "uncertainty_handling", "multiple", 5L, "对缺少批准规则的核心、日期和后续派生概念选择 needs_information；这是保守处理，但降低候选覆盖率。", "DM 与 VS 的 needs_information 候选"
)
write_csv(error_taxonomy, file.path(paired_dir, "error_taxonomy.csv"))

codex <- results[["codex_subagent"]]
conditional <- list(
  experiment_id = codex$experiment_id,
  overall = list(
    denominator = codex$total,
    stage1_valid = codex$stage1_valid,
    stage1_category_correct = codex$stage1_correct,
    stage2_valid_candidate = codex$stage2_valid,
    target_set_complete = codex$target_complete,
    complete_plan_top1 = codex$complete_top1
  ),
  conditional_on_strict_stage2_validation = list(
    denominator = codex$stage2_valid,
    target_set_complete = codex$target_complete,
    complete_plan_top1 = codex$complete_top1,
    expert_accept = codex$accepted,
    expert_modify = codex$modified
  ),
  conditional_on_correct_category = list(
    denominator = codex$function_denominator,
    function_chain_correct = codex$function_correct
  ),
  interpretation = "总体结果以全部 21 个概念为分母；条件结果只描述通过相应结构门槛的概念，不能代替总体结果。"
)
write_json(conditional, file.path(paired_dir, "codex_conditional_results.json"))

format_fraction <- function(numerator, denominator) {
  if (is.null(numerator) || is.na(numerator) || is.null(denominator) || is.na(denominator) || denominator == 0L) return("—")
  sprintf("%d/%d", numerator, denominator)
}

html_metric_table <- comparison |>
  dplyr::mutate(result = purrr::map2_chr(numerator, denominator, format_fraction)) |>
  dplyr::select(provider, metric, result, denominator_type, note)

static_html_table <- function(data) {
  header <- htmltools::tags$tr(lapply(names(data), htmltools::tags$th))
  rows <- lapply(seq_len(nrow(data)), function(index) {
    htmltools::tags$tr(lapply(data[index, , drop = TRUE], function(value) {
      htmltools::tags$td(as.character(value))
    }))
  })
  htmltools::tags$table(htmltools::tags$thead(header), htmltools::tags$tbody(rows))
}

html <- htmltools::tags$html(
  htmltools::tags$head(
    htmltools::tags$meta(charset = "utf-8"),
    htmltools::tags$title("TraceSDTM 上下文修复与子代理对照实验"),
    htmltools::tags$style(htmltools::HTML("body{font-family:Segoe UI,Arial,sans-serif;max-width:1200px;margin:32px auto;line-height:1.55;color:#1f2937}table{border-collapse:collapse;width:100%;margin:16px 0}th,td{border:1px solid #d1d5db;padding:7px 9px;text-align:left;vertical-align:top}th{background:#f3f4f6}.note{background:#fff7ed;border-left:4px solid #f97316;padding:12px}.ok{background:#ecfdf5;border-left:4px solid #10b981;padding:12px}code{background:#f3f4f6;padding:2px 4px}"))
  ),
  htmltools::tags$body(
    htmltools::tags$h1("TraceSDTM 上下文修复与子代理对照实验"),
    htmltools::tags$p(class = "note", "这是单次配对实验，不能估计模型输出的长期稳定性。DeepSeek 结果在提供已轮换的新接口密钥并完成冻结运行前保持为空。"),
    htmltools::tags$h2("实验边界"),
    htmltools::tags$ul(
      htmltools::tags$li("固定 21 个临床概念；只评价推荐，不批准、不构建、不运行 Pinnacle 21。"),
      htmltools::tags$li("第一阶段不向推荐者提供金标准目标变量集合。"),
      htmltools::tags$li("子代理隔离依靠 fork_turns=none 和任务约束，不是操作系统级隔离。"),
      htmltools::tags$li("原始响应先冻结；结构或专业错误不进行挑选性重试。")
    ),
    htmltools::tags$h2("第一阶段提示词配对"),
    static_html_table(prompt_hashes),
    htmltools::tags$h2("评价结果"),
    static_html_table(html_metric_table),
    htmltools::tags$p(class = "ok", sprintf(
      "Codex 子代理：第一阶段结构有效 %s；第二阶段有效候选 %s；完整方案正确 %s；专家直接接受 %s，修改后接受 %s。",
      format_fraction(codex$stage1_valid, codex$total),
      format_fraction(codex$stage2_valid, codex$total),
      format_fraction(codex$complete_top1, codex$total),
      format_fraction(codex$accepted, codex$stage2_valid),
      format_fraction(codex$modified, codex$stage2_valid)
    )),
    htmltools::tags$h2("错误分类"),
    static_html_table(error_taxonomy),
    htmltools::tags$h2("解释限制"),
    htmltools::tags$p("条件结果只描述通过严格结构校验的候选，不能忽略被整组拒绝的 AE 八个概念。总体比较一律保留 21 个概念作为分母。"),
    htmltools::tags$p("第二阶段请求较长；Codex 的 AE 和 VS 请求按确定字符位置分段传输，完整规范请求及校验值保存在各组目录中。"),
    htmltools::tags$p(sprintf("报告生成时间：%s；代码提交：%s。", utc_now(), system2("git", c("rev-parse", "HEAD"), stdout = TRUE)[1]))
  )
)
htmltools::save_html(html, file.path(paired_dir, "contextfix_paired_report.html"), background = "white")

markdown <- c(
  "# TraceSDTM 上下文修复与子代理对照实验",
  "",
  "这是单次配对实验，不能据此估计模型结果的长期稳定性。",
  "",
  sprintf("- Codex 第一阶段有效分类：%s", format_fraction(codex$stage1_valid, codex$total)),
  sprintf("- Codex 第二阶段有效候选：%s", format_fraction(codex$stage2_valid, codex$total)),
  sprintf("- Codex 完整首选方案正确：%s", format_fraction(codex$complete_top1, codex$total)),
  sprintf("- 通过严格第二阶段校验后的专家直接接受：%s", format_fraction(codex$accepted, codex$stage2_valid)),
  sprintf("- 通过严格第二阶段校验后的修改后接受：%s", format_fraction(codex$modified, codex$stage2_valid)),
  "- DeepSeek：等待已轮换的新接口密钥，当前未运行。",
  "",
  "完整指标见 `paired_metrics.csv`，错误分类见 `error_taxonomy.csv`，第一阶段配对校验值见 `stage1_prompt_hashes.csv`。"
)
writeLines(enc2utf8(markdown), file.path(paired_dir, "README.md"), useBytes = TRUE)

message("已生成配对实验报告：", file.path(paired_dir, "contextfix_paired_report.html"))
