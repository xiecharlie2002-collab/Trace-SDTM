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

blind_recommendation_tasks <- function(tasks) {
  dplyr::select(tasks, mapping_id, source_dataset, source_variable)
}

recommendation_prompt <- function(dictionary, tasks, metadata, config = load_project_config()) {
  target_variables <- purrr::imap(metadata$domains, ~ names(.x$variables))
  blind_tasks <- blind_recommendation_tasks(tasks)
  paste(
    "你是临床数据标准映射助手。根据原始字段画像，为每个 mapping_id 选择候选 SDTM 映射。",
    "只能使用给出的域、目标变量、映射类型和 transform_id。不要生成代码。",
    "返回一个 JSON 对象，顶层键必须是 recommendations。每个元素包含：",
    "mapping_id, candidate_rank, source_dataset, source_variable, target_domain,",
    "target_variable, target_value, mapping_type, transform_id, transform_parameters,",
    "recommendation_score, reason, uncertainties, review_required。",
    "transform_parameters 必须是 JSON 对象。candidate_rank 从 1 开始，每个 mapping_id 最多 3 个候选。",
    "每个 mapping_id 至少返回 rank 1；存在合理歧义时返回 3 个按可信程度排序且互不重复的候选。",
    "待判断任务只提供来源信息，不包含标准答案。不要根据 mapping_id 的编号猜测目标。",
    "原始字段画像：", jsonlite::toJSON(dictionary, dataframe = "rows", auto_unbox = TRUE, na = "null"),
    "待判断任务：", jsonlite::toJSON(blind_tasks, dataframe = "rows", auto_unbox = TRUE, na = "null"),
    "允许的目标变量：", jsonlite::toJSON(target_variables, auto_unbox = TRUE),
    "允许的映射类型：", jsonlite::toJSON(unlist(config$model$allowed_mapping_types), auto_unbox = TRUE),
    "允许的转换函数：", jsonlite::toJSON(unlist(config$model$allowed_transform_ids), auto_unbox = TRUE),
    sep = "\n"
  )
}

split_recommendation_tasks <- function(tasks, batch_size = 8L) {
  batch_size <- as.integer(batch_size)
  if (is.na(batch_size) || batch_size < 1L) trace_abort("model.batch_size 必须是正整数。")
  split(tasks, ceiling(seq_len(nrow(tasks)) / batch_size))
}

extract_json_content <- function(content) {
  content <- trimws(content)
  content <- sub("^```(?:json)?\\s*", "", content, ignore.case = TRUE)
  content <- sub("\\s*```$", "", content)
  content
}

recommendation_scalar <- function(record, name, default = NA) {
  value <- record[[name]]
  if (is.null(value) || length(value) == 0L) return(default)
  if (is.list(value) || length(value) != 1L) {
    trace_abort(sprintf("模型字段 %s 必须是单个值。", name))
  }
  value
}

recommendation_text <- function(record, name, default = "") {
  value <- record[[name]]
  if (is.null(value) || length(value) == 0L) return(default)
  flattened <- unlist(value, recursive = TRUE, use.names = FALSE)
  if (!length(flattened)) return(default)
  paste(as.character(flattened), collapse = "; ")
}

normalize_model_parameters <- function(value) {
  if (is.null(value) || length(value) == 0L) return("{}")
  if (is.character(value)) return(as_json_text(from_json_text(value)))
  if (is.list(value)) return(as_json_text(value))
  trace_abort("模型字段 transform_parameters 必须是 JSON 对象或 JSON 字符串。")
}

parse_recommendation_records <- function(records) {
  if (!is.list(records) || !length(records)) trace_abort("模型没有返回候选映射记录。")
  required <- c(
    "mapping_id", "candidate_rank", "source_dataset", "source_variable",
    "target_domain", "target_variable", "mapping_type", "transform_id",
    "recommendation_score", "reason", "uncertainties", "review_required"
  )
  purrr::map_dfr(records, function(record) {
    if (!is.list(record)) trace_abort("每条模型候选必须是 JSON 对象。")
    missing <- setdiff(required, names(record))
    if (length(missing)) trace_abort(sprintf("模型候选缺少字段：%s", paste(missing, collapse = ", ")))
    tibble::tibble(
      mapping_id = as.character(recommendation_scalar(record, "mapping_id")),
      candidate_rank = as.integer(recommendation_scalar(record, "candidate_rank")),
      source_dataset = as.character(recommendation_scalar(record, "source_dataset")),
      source_variable = as.character(recommendation_scalar(record, "source_variable")),
      target_domain = as.character(recommendation_scalar(record, "target_domain")),
      target_variable = as.character(recommendation_scalar(record, "target_variable")),
      target_value = as.character(recommendation_scalar(record, "target_value", "")),
      mapping_type = as.character(recommendation_scalar(record, "mapping_type")),
      transform_id = as.character(recommendation_scalar(record, "transform_id")),
      transform_parameters = normalize_model_parameters(record$transform_parameters),
      recommendation_score = as.numeric(recommendation_scalar(record, "recommendation_score")),
      reason = recommendation_text(record, "reason"),
      uncertainties = recommendation_text(record, "uncertainties"),
      review_required = as.logical(recommendation_scalar(record, "review_required"))
    )
  })
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

  expected_sources <- dplyr::select(tasks, mapping_id, expected_dataset = source_dataset, expected_variable = source_variable)
  checked_sources <- dplyr::left_join(recommendations, expected_sources, by = "mapping_id")
  source_mismatch <- checked_sources$source_dataset != checked_sources$expected_dataset |
    checked_sources$source_variable != checked_sources$expected_variable
  if (any(source_mismatch, na.rm = TRUE)) {
    bad_ids <- unique(checked_sources$mapping_id[source_mismatch])
    trace_abort(sprintf("模型改写了来源字段：%s", paste(bad_ids, collapse = ", ")))
  }

  allowed_domains <- names(metadata$domains)
  bad_domains <- setdiff(unique(recommendations$target_domain), allowed_domains)
  if (length(bad_domains)) trace_abort(sprintf("模型返回未知目标域：%s", paste(bad_domains, collapse = ", ")))

  allowed_types <- unlist(config$model$allowed_mapping_types)
  allowed_transforms <- unlist(config$model$allowed_transform_ids)
  bad_types <- setdiff(unique(recommendations$mapping_type), allowed_types)
  bad_transforms <- setdiff(unique(recommendations$transform_id), allowed_transforms)
  if (length(bad_types)) trace_abort(sprintf("模型返回了不允许的 mapping_type：%s。", paste(bad_types, collapse = ", ")))
  if (length(bad_transforms)) trace_abort(sprintf("模型返回了不允许的 transform_id：%s。", paste(bad_transforms, collapse = ", ")))

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

perform_mapping_request <- function(prompt, endpoint, api_key, model, config) {
  request <- httr2::request(endpoint) |>
    httr2::req_headers(Authorization = paste("Bearer", api_key)) |>
    httr2::req_body_json(list(
      model = model,
      temperature = config$model$temperature,
      max_tokens = config$model$max_completion_tokens %||% 8192L,
      response_format = list(type = "json_object"),
      messages = list(
        list(role = "system", content = "Return only valid JSON. Follow the supplied clinical mapping constraints."),
        list(role = "user", content = prompt)
      )
    )) |>
    httr2::req_timeout(config$model$timeout_seconds)

  max_attempts <- as.integer(config$model$max_attempts %||% 1L)
  last_error <- NULL
  for (attempt in seq_len(max_attempts)) {
    response <- tryCatch(httr2::req_perform(request), error = identity)
    if (!inherits(response, "error")) return(response)
    last_error <- response
    if (attempt < max_attempts) Sys.sleep(min(attempt, 2L))
  }
  trace_abort(sprintf("模型请求连续失败 %d 次：%s", max_attempts, conditionMessage(last_error)))
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
  endpoint <- paste0(sub("/$", "", base_url), config$model$endpoint_suffix)
  batches <- split_recommendation_tasks(tasks, config$model$batch_size %||% 8L)
  batch_results <- vector("list", length(batches))
  response_hashes <- character(length(batches))
  batch_dir <- ensure_dir(trace_path(config$paths$recommendation_dir, "batches"))

  for (batch_index in seq_along(batches)) {
    batch_tasks <- batches[[batch_index]]
    batch_dictionary <- dplyr::filter(dictionary, source_dataset %in% unique(batch_tasks$source_dataset))
    prompt <- recommendation_prompt(batch_dictionary, batch_tasks, metadata, config)
    prompt_hash <- digest::digest(prompt, algo = "sha256")
    batch_stem <- sprintf("batch_%02d", batch_index)
    batch_csv <- file.path(batch_dir, paste0(batch_stem, ".csv"))
    batch_meta <- file.path(batch_dir, paste0(batch_stem, ".json"))
    if (file.exists(batch_csv) && file.exists(batch_meta)) {
      cached_meta <- jsonlite::read_json(batch_meta, simplifyVector = TRUE)
      cache_matches <- identical(as.character(cached_meta$prompt_sha256), prompt_hash) &&
        identical(as.character(cached_meta$model), model)
      if (cache_matches) {
        cached <- readr::read_csv(batch_csv, show_col_types = FALSE)
        batch_results[[batch_index]] <- validate_recommendations(cached, config, batch_tasks, metadata)
        response_hashes[[batch_index]] <- as.character(cached_meta$response_sha256)
        trace_info("已加载模型推荐检查点：第 %d/%d 批，%d 条候选。", batch_index, length(batches), nrow(cached))
        next
      }
    }
    trace_info("正在请求模型推荐：第 %d/%d 批，%d 个任务。", batch_index, length(batches), nrow(batch_tasks))
    response <- perform_mapping_request(prompt, endpoint, api_key, model, config)
    body <- httr2::resp_body_json(response, simplifyVector = FALSE)
    content <- body$choices[[1]]$message$content %||% ""
    if (!nzchar(trimws(content))) trace_abort(sprintf("第 %d 批模型响应为空。", batch_index))
    parsed <- tryCatch(
      jsonlite::fromJSON(extract_json_content(content), simplifyVector = FALSE),
      error = function(error) trace_abort(sprintf("第 %d 批不是有效 JSON：%s", batch_index, conditionMessage(error)))
    )
    batch_recommendations <- parse_recommendation_records(parsed$recommendations %||% parsed)
    batch_results[[batch_index]] <- validate_recommendations(
      batch_recommendations, config, batch_tasks, metadata
    )
    response_hashes[[batch_index]] <- digest::digest(content, algo = "sha256")
    write_csv(batch_results[[batch_index]], batch_csv)
    write_json(list(
      batch_index = batch_index,
      task_count = nrow(batch_tasks),
      candidate_count = nrow(batch_results[[batch_index]]),
      mapping_ids = batch_tasks$mapping_id,
      model = model,
      prompt_design = "blind_source_only_v1",
      prompt_sha256 = prompt_hash,
      response_sha256 = response_hashes[[batch_index]],
      completed_at = utc_now(),
      api_key_logged = FALSE
    ), batch_meta)
  }

  recommendations <- dplyr::bind_rows(batch_results)
  save_recommendations(
    recommendations,
    config,
    source = "model",
    model = model,
    response_hashes = response_hashes,
    batch_sizes = vapply(batches, nrow, integer(1)),
    prompt_design = "blind_source_only_v1"
  )
  recommendations
}

save_recommendations <- function(recommendations, config, source, model = NA_character_, response_text = "", response_hashes = character(), batch_sizes = integer(), prompt_design = NA_character_) {
  ensure_output_directories(config)
  csv_path <- trace_path(config$paths$recommendation_dir, "mapping_recommendations.csv")
  json_path <- trace_path(config$paths$recommendation_dir, "mapping_recommendations.json")
  write_csv(recommendations, csv_path)
  write_json(as.data.frame(recommendations), json_path)
  write_json(list(
    status = if (source == "model") "completed" else "reference_seed",
    provenance = source,
    model = model,
    prompt_design = if (source == "model") prompt_design else "reference_seed",
    generated_at = utc_now(),
    recommendation_count = nrow(recommendations),
    batch_count = length(batch_sizes),
    batch_sizes = as.integer(batch_sizes),
    response_sha256 = if (length(response_hashes)) {
      digest::digest(paste(response_hashes, collapse = "|"), algo = "sha256")
    } else if (nzchar(response_text)) {
      digest::digest(response_text, algo = "sha256")
    } else {
      NA_character_
    },
    batch_response_sha256 = unname(response_hashes),
    api_key_logged = FALSE
  ), trace_path(config$paths$recommendation_dir, "model_run.json"))
  create_review_workbook(recommendations, config, preapprove = identical(source, "reference_seed"))
  trace_info("已生成 %s 条候选映射，来源：%s。", nrow(recommendations), source)
  invisible(recommendations)
}
