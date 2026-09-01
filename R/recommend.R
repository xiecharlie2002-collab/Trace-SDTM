flatten_mapping_tasks <- function(specification = load_mapping_template()) {
  purrr::imap_dfr(specification$domains, function(domain_spec, domain) {
    entries <- c(domain_spec$mappings %||% list(), domain_spec$transpose_mappings %||% list())
    purrr::map_dfr(entries, function(entry) {
      tibble::tibble(
        mapping_id = as.character(entry$mapping_id),
        source_dataset = tools::file_path_sans_ext(domain_spec$source_file),
        source_variable = paste(unlist(entry$source_variables %||% character()), collapse = " | "),
        target_domain = domain,
        target_variable = as.character(entry$target_variable %||% ""),
        target_value = as.character(entry$target_value %||% ""),
        mapping_type = as.character(entry$mapping_type),
        transform_id = as.character(entry$transform_id),
        transform_parameters = as_json_text(entry$parameters %||% list()),
        include_in_recommendation = isTRUE(entry$include_in_recommendation),
        mapping_required = isTRUE(entry$mapping_required)
      )
    })
  })
}

seed_recommendations <- function(config = load_project_config()) {
  tasks <- dplyr::filter(flatten_mapping_tasks(load_mapping_template(config)), include_in_recommendation)
  recommendations <- dplyr::transmute(
    tasks,
    mapping_id,
    candidate_rank = 1L,
    source_dataset,
    source_variable,
    target_domain,
    target_variable,
    target_value,
    mapping_type,
    transform_id,
    transform_parameters,
    recommendation_score = 1,
    reason = "参考种子：来自专家编写的映射模板，用于离线演示和测试。",
    uncertainties = "这不是真实模型运行结果，不能用于评价模型准确率。",
    review_required = TRUE,
    provenance = "reference_seed"
  )
  save_recommendations(recommendations, config, source = "reference_seed")
  recommendations
}

recommendation_prompt <- function(dictionary, tasks, metadata) {
  target_variables <- purrr::imap(metadata$domains, ~ names(.x$variables))
  paste(
    "你是临床数据标准映射助手。根据原始字段画像，为每个 mapping_id 选择候选 SDTM 映射。",
    "只能使用给出的域、目标变量、映射类型和 transform_id。不要生成代码。",
    "返回一个 JSON 对象，顶层键必须是 recommendations。每个元素包含：",
    "mapping_id, candidate_rank, source_dataset, source_variable, target_domain,",
    "target_variable, target_value, mapping_type, transform_id, transform_parameters,",
    "recommendation_score, reason, uncertainties, review_required。",
    "transform_parameters 必须是 JSON 对象。candidate_rank 从 1 开始，每个 mapping_id 最多 3 个候选。",
    "原始字段画像：", jsonlite::toJSON(dictionary, dataframe = "rows", auto_unbox = TRUE, na = "null"),
    "待判断任务：", jsonlite::toJSON(tasks, dataframe = "rows", auto_unbox = TRUE, na = "null"),
    "允许的目标变量：", jsonlite::toJSON(target_variables, auto_unbox = TRUE),
    sep = "\n"
  )
}

extract_json_content <- function(content) {
  content <- trimws(content)
  content <- sub("^```(?:json)?\\s*", "", content, ignore.case = TRUE)
  content <- sub("\\s*```$", "", content)
  content
}

validate_recommendations <- function(recommendations, config, tasks, metadata) {
  required <- c(
    "mapping_id", "candidate_rank", "source_dataset", "source_variable",
    "target_domain", "target_variable", "mapping_type", "transform_id",
    "recommendation_score", "reason", "uncertainties", "review_required"
  )
  missing <- setdiff(required, names(recommendations))
  if (length(missing)) trace_abort(sprintf("模型结果缺少字段：%s", paste(missing, collapse = ", ")))

  if (!"target_value" %in% names(recommendations)) recommendations$target_value <- ""
  if (!"transform_parameters" %in% names(recommendations)) recommendations$transform_parameters <- "{}"
  recommendations$mapping_id <- as.character(recommendations$mapping_id)
  recommendations$candidate_rank <- as.integer(recommendations$candidate_rank)
  recommendations$target_value <- as.character(recommendations$target_value %||% "")
  recommendations$provenance <- "model"

  unknown_ids <- setdiff(recommendations$mapping_id, tasks$mapping_id)
  if (length(unknown_ids)) trace_abort(sprintf("模型返回未知 mapping_id：%s", paste(unknown_ids, collapse = ", ")))
  if (anyDuplicated(paste(recommendations$mapping_id, recommendations$candidate_rank))) {
    trace_abort("同一 mapping_id 出现重复 candidate_rank。")
  }
  if (any(is.na(recommendations$candidate_rank) | recommendations$candidate_rank < 1L | recommendations$candidate_rank > 3L)) {
    trace_abort("candidate_rank 必须是 1 到 3。")
  }

  allowed_domains <- names(metadata$domains)
  bad_domains <- setdiff(unique(recommendations$target_domain), allowed_domains)
  if (length(bad_domains)) trace_abort(sprintf("模型返回未知目标域：%s", paste(bad_domains, collapse = ", ")))

  allowed_types <- unlist(config$model$allowed_mapping_types)
  allowed_transforms <- unlist(config$model$allowed_transform_ids)
  if (any(!recommendations$mapping_type %in% allowed_types)) trace_abort("模型返回了不允许的 mapping_type。")
  if (any(!recommendations$transform_id %in% allowed_transforms)) trace_abort("模型返回了不允许的 transform_id。")

  for (index in seq_len(nrow(recommendations))) {
    row <- recommendations[index, , drop = FALSE]
    allowed_variables <- names(metadata$domains[[row$target_domain]]$variables)
    if (row$mapping_type != "drop" && !row$target_variable %in% allowed_variables) {
      trace_abort(sprintf("%s 返回未知目标变量 %s.%s。", row$mapping_id, row$target_domain, row$target_variable))
    }
    parameter_value <- row$transform_parameters[[1]]
    if (is.list(parameter_value)) {
      recommendations$transform_parameters[index] <- as_json_text(parameter_value)
    } else {
      tryCatch(from_json_text(as.character(parameter_value)), error = function(e) {
        trace_abort(sprintf("%s 的 transform_parameters 不是有效 JSON。", row$mapping_id))
      })
    }
  }
  tibble::as_tibble(recommendations)
}

call_mapping_model <- function(config = load_project_config()) {
  api_key <- Sys.getenv("TRACE_SDTM_API_KEY", unset = "")
  base_url <- Sys.getenv("TRACE_SDTM_BASE_URL", unset = "")
  model <- Sys.getenv("TRACE_SDTM_MODEL", unset = "")
  if (!nzchar(api_key) || !nzchar(base_url) || !nzchar(model)) {
    trace_abort("未配置 TRACE_SDTM_API_KEY、TRACE_SDTM_BASE_URL 和 TRACE_SDTM_MODEL，无法执行真实推荐。可用 recommend --seed 生成明确标记的离线参考种子。")
  }

  dictionary_path <- trace_path(config$paths$profile_dir, "source_dictionary.csv")
  if (!file.exists(dictionary_path)) profile_sources(config)
  dictionary <- readr::read_csv(dictionary_path, show_col_types = FALSE)
  tasks <- dplyr::filter(flatten_mapping_tasks(load_mapping_template(config)), include_in_recommendation)
  metadata <- load_metadata(config)
  prompt <- recommendation_prompt(dictionary, tasks, metadata)
  endpoint <- paste0(sub("/$", "", base_url), config$model$endpoint_suffix)

  request <- httr2::request(endpoint) |>
    httr2::req_headers(Authorization = paste("Bearer", api_key)) |>
    httr2::req_body_json(list(
      model = model,
      temperature = config$model$temperature,
      messages = list(
        list(role = "system", content = "Return only valid JSON. Follow the supplied clinical mapping constraints."),
        list(role = "user", content = prompt)
      )
    )) |>
    httr2::req_timeout(config$model$timeout_seconds) |>
    httr2::req_retry(max_tries = config$model$max_attempts)

  response <- httr2::req_perform(request)
  body <- httr2::resp_body_json(response, simplifyVector = FALSE)
  content <- body$choices[[1]]$message$content %||% ""
  parsed <- jsonlite::fromJSON(extract_json_content(content), simplifyDataFrame = TRUE)
  recommendations <- parsed$recommendations %||% parsed
  recommendations <- validate_recommendations(as.data.frame(recommendations), config, tasks, metadata)
  save_recommendations(recommendations, config, source = "model", model = model, response_text = content)
  recommendations
}

save_recommendations <- function(recommendations, config, source, model = NA_character_, response_text = "") {
  ensure_output_directories(config)
  csv_path <- trace_path(config$paths$recommendation_dir, "mapping_recommendations.csv")
  json_path <- trace_path(config$paths$recommendation_dir, "mapping_recommendations.json")
  write_csv(recommendations, csv_path)
  write_json(as.data.frame(recommendations), json_path)
  write_json(list(
    status = if (source == "model") "completed" else "reference_seed",
    provenance = source,
    model = model,
    generated_at = utc_now(),
    recommendation_count = nrow(recommendations),
    response_sha256 = if (nzchar(response_text)) digest::digest(response_text, algo = "sha256") else NA_character_,
    api_key_logged = FALSE
  ), trace_path(config$paths$recommendation_dir, "model_run.json"))
  create_review_workbook(recommendations, config, preapprove = identical(source, "reference_seed"))
  trace_info("已生成 %s 条候选映射，来源：%s。", nrow(recommendations), source)
  invisible(recommendations)
}

