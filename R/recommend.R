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

apply_model_runtime_overrides <- function(config) {
  timeout_override <- Sys.getenv("TRACE_SDTM_TIMEOUT_SECONDS", unset = "")
  if (nzchar(timeout_override)) {
    timeout_value <- suppressWarnings(as.integer(timeout_override))
    if (is.na(timeout_value) || timeout_value < 1L) trace_abort("TRACE_SDTM_TIMEOUT_SECONDS 必须是正整数。")
    config$model$timeout_seconds <- timeout_value
  }
  token_override <- Sys.getenv("TRACE_SDTM_MAX_COMPLETION_TOKENS", unset = "")
  if (nzchar(token_override)) {
    token_value <- suppressWarnings(as.integer(token_override))
    if (is.na(token_value) || token_value < 256L) trace_abort("TRACE_SDTM_MAX_COMPLETION_TOKENS 必须是不小于 256 的整数。")
    config$model$max_completion_tokens <- token_value
  }
  thinking_override <- tolower(Sys.getenv("TRACE_SDTM_THINKING_MODE", unset = ""))
  if (nzchar(thinking_override)) {
    if (!thinking_override %in% c("enabled", "disabled")) {
      trace_abort("TRACE_SDTM_THINKING_MODE 只能是 enabled 或 disabled。")
    }
    config$model$thinking_mode <- thinking_override
  }
  config
}

perform_mapping_request <- function(prompt, endpoint, api_key, model, config) {
  request_body <- list(
    model = model,
    temperature = config$model$temperature,
    max_tokens = config$model$max_completion_tokens %||% 8192L,
    response_format = list(type = "json_object"),
    messages = list(
      list(role = "system", content = "Return only valid JSON. Follow the supplied clinical mapping constraints."),
      list(role = "user", content = prompt)
    )
  )
  thinking_mode <- config$model$thinking_mode %||% ""
  if (nzchar(thinking_mode)) {
    if (!thinking_mode %in% c("enabled", "disabled")) {
      trace_abort("模型 thinking_mode 只能是 enabled 或 disabled。")
    }
    request_body$thinking <- list(type = thinking_mode)
  }
  request <- httr2::request(endpoint) |>
    httr2::req_headers(Authorization = paste("Bearer", api_key)) |>
    httr2::req_body_json(request_body) |>
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
  timeout_override <- Sys.getenv("TRACE_SDTM_TIMEOUT_SECONDS", unset = "")
  if (nzchar(timeout_override)) {
    timeout_value <- suppressWarnings(as.integer(timeout_override))
    if (is.na(timeout_value) || timeout_value < 1L) trace_abort("TRACE_SDTM_TIMEOUT_SECONDS 必须是正整数。")
    config$model$timeout_seconds <- timeout_value
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

# v0.4 is implemented in separate modules so the tagged v0.2/v0.3 interfaces
# remain reproducible.  The files are sourced here because this repository uses
# script-style modules rather than an installed-package namespace.
source(trace_path("R", "parameter_resolvers.R"), encoding = "UTF-8")
source(trace_path("R", "recommend_v04.R"), encoding = "UTF-8")

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

# -----------------------------------------------------------------------------
# 0.2：临床概念分组和两阶段推荐。下列同名函数覆盖上方保留的 0.1 实现；
# 旧实现仅用于通过 v0.1-mvp 标签复现历史结果。

concept_lookup <- function(specification) {
  stats::setNames(specification$concepts, vapply(specification$concepts, `[[`, character(1), "concept_id"))
}

gold_plan_steps <- function(plan) {
  if (is.null(plan)) return(list())
  plan$steps %||% plan
}

profile_evidence_columns <- function(dictionary) {
  intersect(
    c(
      "source_dataset", "source_variable", "label", "data_type", "example_values",
      "missing_rate", "unique_count", "form_name", "grain", "format_candidates",
      "partial_tokens", "record_count"
    ),
    names(dictionary)
  )
}

source_profile_context <- function(ref, specification, dictionary) {
  declared_key <- source_ref_key(ref)
  ref_variable <- as.character(ref$variable)
  direct <- dplyr::filter(
    dictionary,
    paste0(.data$source_dataset, ".", .data$source_variable) == .env$declared_key
  )
  resolution <- "direct"
  evidence <- direct
  if (!nrow(evidence)) {
    catalog <- specification$source_catalog[[ref$dataset]] %||% list()
    parents <- unname(unlist(catalog$profile_parents %||% character(), use.names = FALSE))
    if (isTRUE(catalog$derived) && length(parents)) {
      evidence <- dplyr::filter(
        dictionary,
        .data$source_dataset %in% .env$parents,
        .data$source_variable == .env$ref_variable
      )
      resolution <- if (nrow(evidence)) "derived_candidates" else "unavailable"
    } else {
      resolution <- "unavailable"
    }
  }
  evidence <- dplyr::select(evidence, dplyr::all_of(profile_evidence_columns(dictionary)))
  list(
    declared_source_key = declared_key,
    role = as.character(ref$role %||% ""),
    is_key = ref_variable %in% unlist(specification$source_catalog[[ref$dataset]]$keys %||% character(), use.names = FALSE),
    resolution = resolution,
    evidence = as.data.frame(evidence)
  )
}

concept_context <- function(concept, specification, dictionary) {
  source_refs <- concept_source_refs(concept)
  datasets <- unique(vapply(concept_source_refs(concept), function(ref) as.character(ref$dataset), character(1)))
  list(
    concept_id = concept$concept_id,
    target_domain = concept$target_domain,
    form_name = concept$form_name,
    expected_cardinality = concept$expected_cardinality,
    required = isTRUE(concept$required),
    depends_on = unname(unlist(concept$depends_on %||% character())),
    source_refs = concept$source_refs %||% list(),
    source_catalog = specification$source_catalog[intersect(datasets, names(specification$source_catalog))],
    field_profiles = lapply(source_refs, source_profile_context, specification = specification, dictionary = dictionary)
  )
}

recommendation_groups <- function(specification, dictionary, config = load_project_config()) {
  concepts <- specification$concepts
  domains <- unique(vapply(concepts, `[[`, character(1), "target_domain"))
  groups <- list()
  for (domain in domains) {
    members <- Filter(function(concept) identical(concept$target_domain, domain), concepts)
    group_id <- paste0(tolower(domain), "_connected_01")
    payload <- lapply(members, concept_context, specification = specification, dictionary = dictionary)
    size <- nchar(registry_json(payload), type = "bytes")
    limit <- as.integer(config$model$max_group_characters %||% 45000L)
    if (size > limit) {
      trace_abort(sprintf(
        "%s 域依赖连通组约 %s 个字符，超过当前上限 %s。为避免拆开依赖关系，已停止推荐。",
        domain, size, limit
      ))
    }
    groups[[group_id]] <- list(group_id = group_id, target_domain = domain, concepts = members, context = payload)
  }
  groups
}

category_catalog_for_model <- function(registry) {
  purrr::imap(registry$categories, function(value, category) list(
    category = category,
    name = value$name,
    description = value$description,
    stage_order = value$stage_order,
    encapsulates = unname(unlist(value$encapsulates %||% character(), use.names = FALSE)),
    do_not_add = unname(unlist(value$do_not_add %||% character(), use.names = FALSE))
  ))
}

mapping_policy_context <- function(config = load_project_config()) {
  policies <- load_mapping_policies(config)
  # 映射政策只包含研究级约定，不得序列化金标准步骤。
  forbidden <- c("plans", "steps", "transform_id", "gold_specification")
  present <- intersect(forbidden, names(policies))
  if (length(present)) trace_abort(sprintf("映射政策包含禁止顶层字段：%s。", paste(present, collapse = "、")))
  policies
}

classification_prompt_v02 <- function(group, metadata, registry, config = load_project_config()) {
  target_meta <- metadata$domains[[group$target_domain]]$variables
  prompt <- paste(
    "你是临床数据标准映射助手。第一阶段只判断每个临床概念需要的转换类别链，不选择具体函数。",
    "类别表示最终将选择的登记函数所属类别，不是业务语义关键词清单。类别目录的 encapsulates 已由该类别内部完成，不得为这些内部操作重复增加类别。",
    "必须返回覆盖概念所需函数的最小类别链：不能因为一个函数内部组合日期时间、生成检查代码或规范术语大小写而重复列出字段组合、直接赋值或字符规范化。",
    "不得发明来源字段、数据集或连接键，不得生成程序、公式、正则表达式或自由条件。",
    "若日期格式有歧义、连接键不足、基数关系不明确或单位不能识别，status 必须为 needs_information。",
    "返回 JSON 对象，顶层键 classifications。每个概念恰好一条记录，字段为 concept_id、categories、classification_score、evidence、uncertainties、status。",
    "status 只能逐字使用 proposed 或 needs_information：信息足够时使用 proposed，信息不足时使用 needs_information；不得使用 ready、classified、success 或其他近义词。",
    "categories 必须是按执行顺序排列的类别标识符字符串数组，只能逐字使用类别目录中的 category 值；不得使用数字、中文名称或自行缩写。",
    "categories 对应的 stage_order 必须非递减，并在输出前自行核对。正确示例：{\"concept_id\":\"EXAMPLE\",\"categories\":[\"direct_assignment\",\"temporal_derivation\"],\"classification_score\":0.9,\"evidence\":[\"有直接来源并需要研究日派生。\"],\"uncertainties\":[],\"status\":\"proposed\"}。",
    "转换类别目录：", registry_json(category_catalog_for_model(registry)),
    "经批准的项目映射政策：", registry_json(mapping_policy_context(config)),
    "目标域专业上下文：", registry_json(list(domain = group$target_domain, variables = target_meta)),
    "临床概念及来源上下文：", registry_json(group$context),
    sep = "\n"
  )
  assert_blind_prompt(prompt)
}

assert_blind_prompt <- function(prompt) {
  forbidden <- c(
    "advanced_gold.yml", "basic_gold.yml", "gold_specification", "mapping_evaluation",
    "expert_review_summary", "concept_review_audit", "final_steps_audit"
  )
  lowered_prompt <- tolower(prompt)
  present <- forbidden[vapply(forbidden, function(value) grepl(tolower(value), lowered_prompt, fixed = TRUE), logical(1))]
  if (length(present)) trace_abort(sprintf("盲评提示词包含禁止内容：%s", paste(present, collapse = ", ")))
  prompt
}

model_resource_catalog <- function(config = load_project_config()) {
  terminology <- load_controlled_terminology(config)
  conversions <- load_unit_conversions(config)
  conversion_sets <- lapply(conversions$sets %||% list(), function(entries) {
    lapply(entries, function(entry) list(
      test_code = entry$test_code,
      from_unit = entry$from_unit,
      to_unit = entry$to_unit,
      round_digits = entry$round_digits
    ))
  })
  list(
    codelists = terminology$codelists %||% list(),
    visit_maps = terminology$visit_maps %||% list(),
    unit_conversion_sets = conversion_sets,
    unit_aliases = conversions$unit_aliases %||% list(),
    internal_fields = list(
      list(
        field = ".SOURCE_ROW",
        available_from_stage = "source_preparation",
        purpose = "确定性记录排序和序号派生的最终稳定并列判定字段"
      )
    )
  )
}

function_cards_for_model <- function(registry, categories) {
  lapply(registry_model_entries(registry, categories), function(entry) list(
    transform_id = entry$transform_id,
    category = entry$category,
    name = entry$name,
    description = entry$description,
    execution_stage = entry$execution_stage,
    source_contract = list(
      minimum = entry$source_contract$minimum, maximum = entry$source_contract$maximum,
      minimum_datasets = entry$source_contract$minimum_datasets, maximum_datasets = entry$source_contract$maximum_datasets,
      types = as.list(unlist(entry$source_contract$types, use.names = FALSE))
    ),
    target_contract = list(
      domains = as.list(unlist(entry$target_contract$domains, use.names = FALSE)),
      patterns = as.list(unlist(entry$target_contract$patterns, use.names = FALSE)),
      output_mode = entry$target_contract$output_mode
    ),
    parameter_schema = prepare_json_schema(entry$parameter_schema),
    preconditions = as.list(unlist(entry$preconditions, use.names = FALSE)),
    not_allowed_when = as.list(unlist(entry$not_allowed_when, use.names = FALSE)),
    composability = lapply(entry$composability, function(value) as.list(unlist(value, use.names = FALSE))),
    examples = entry$examples
  ))
}

selection_prompt_v02 <- function(group, classifications, metadata, registry, config = load_project_config()) {
  categories <- unique(unlist(lapply(classifications, `[[`, "categories"), use.names = FALSE))
  prompt <- paste(
    "你是临床数据标准映射助手。第二阶段在第一阶段选定的类别内，选择登记过的函数和严格参数。",
    "每个概念返回一到三个完整候选方案；候选必须覆盖该临床概念所需的全部目标变量，步骤按执行顺序排列。",
    "只能使用下方函数卡中的 transform_id。参数名、类型和枚举必须严格符合 parameter_schema，additionalProperties 为 false。",
    "source_keys 只能引用该概念 source_refs 中的 dataset.variable；无需来源的后续派生可使用空数组。",
    "不得生成或嵌入 R 代码、单位公式、正则表达式、连接键或自由条件。单位换算只能选择 conversion_set_id 和 target_unit。",
    "若信息不足，返回 status=needs_information、steps=[] 并说明待确认信息，不得猜测。所有候选都必须 review_required=true。",
    "返回 JSON 对象，顶层键 recommendations。每条含 concept_id、candidate_rank、target_domain、steps、recommendation_score、reason、uncertainties、status、review_required。",
    "每个 step 必须含 transform_id、source_keys、target_variables、parameters。candidate_rank 为 1 到 3，分值为 0 到 1。",
    "函数卡 output_mode=dataset 或 none 时 target_variables 必须为 []；派生数据集名称只能写入登记参数 output_dataset，不能当作 SDTM 目标变量。",
    "项目政策要求身份单位标准化时，即使原始单位与目标单位相同，也必须选择 standardize_unit，不得用普通直接赋值替代。",
    "上游概念在项目政策 upstream_outputs 中声明的变量可用于后续派生参数；无需当前原始来源的后续派生仍使用 source_keys=[]。",
    "第一阶段分类：", registry_json(classifications),
    "本次唯一可见的函数卡：", registry_json(function_cards_for_model(registry, categories)),
    "可用受控资源目录：", registry_json(model_resource_catalog(config)),
    "经批准的项目映射政策：", registry_json(mapping_policy_context(config)),
    "目标域专业上下文：", registry_json(list(domain = group$target_domain, variables = metadata$domains[[group$target_domain]]$variables)),
    "临床概念及来源上下文：", registry_json(group$context),
    sep = "\n"
  )
  assert_blind_prompt(prompt)
}

parse_classifications_v02 <- function(records, group, registry) {
  if (!is.list(records) || !length(records)) trace_abort(sprintf("%s 第一阶段没有返回分类。", group$group_id))
  known_ids <- vapply(group$concepts, `[[`, character(1), "concept_id")
  known_categories <- names(registry$categories)
  parsed <- lapply(records, function(record) {
    required <- c("concept_id", "categories", "classification_score", "evidence", "uncertainties", "status")
    missing <- setdiff(required, names(record))
    if (length(missing)) trace_abort(sprintf("第一阶段分类缺少字段：%s", paste(missing, collapse = ", ")))
    categories <- unname(unlist(record$categories, use.names = FALSE))
    score <- as.numeric(record$classification_score)
    status <- as.character(record$status)
    if (!as.character(record$concept_id) %in% known_ids) trace_abort(sprintf("第一阶段返回未知概念：%s", record$concept_id))
    if (!length(categories) || any(!categories %in% known_categories)) trace_abort(sprintf("%s 返回未知或空类别链。", record$concept_id))
    if (anyDuplicated(categories)) trace_abort(sprintf("%s 的类别链存在重复类别。", record$concept_id))
    orders <- vapply(categories, function(category) registry$categories[[category]]$stage_order, integer(1))
    if (is.unsorted(orders, strictly = FALSE)) trace_abort(sprintf("%s 的类别链顺序无效。", record$concept_id))
    if (length(score) != 1L || is.na(score) || score < 0 || score > 1) trace_abort(sprintf("%s 的分类分值超出 0 到 1。", record$concept_id))
    if (!status %in% c("proposed", "needs_information")) trace_abort(sprintf("%s 的分类状态无效。", record$concept_id))
    list(
      concept_id = as.character(record$concept_id), categories = categories,
      classification_score = score, evidence = recommendation_text(record, "evidence"),
      uncertainties = recommendation_text(record, "uncertainties"), status = status,
      group_id = group$group_id
    )
  })
  ids <- vapply(parsed, `[[`, character(1), "concept_id")
  if (anyDuplicated(ids)) trace_abort(sprintf("%s 第一阶段出现重复概念。", group$group_id))
  missing_ids <- setdiff(known_ids, ids)
  if (length(missing_ids)) trace_abort(sprintf("%s 第一阶段遗漏概念：%s", group$group_id, paste(missing_ids, collapse = ", ")))
  parsed[match(known_ids, ids)]
}

validate_candidate_plan_v02 <- function(record, classification, concept, specification, metadata, registry, group_id, provenance,
                                        config = load_project_config()) {
  required <- c("concept_id", "candidate_rank", "target_domain", "steps", "recommendation_score", "reason", "uncertainties", "status", "review_required")
  missing <- setdiff(required, names(record))
  if (length(missing)) trace_abort(sprintf("第二阶段候选缺少字段：%s", paste(missing, collapse = ", ")))
  rank <- as.integer(record$candidate_rank)
  score <- as.numeric(record$recommendation_score)
  status <- as.character(record$status)
  if (is.na(rank) || rank < 1L || rank > 3L) trace_abort(sprintf("%s 的候选序号必须为 1 到 3。", concept$concept_id))
  if (is.na(score) || score < 0 || score > 1) trace_abort(sprintf("%s 的推荐分值超出 0 到 1。", concept$concept_id))
  if (!status %in% c("proposed", "needs_information")) trace_abort(sprintf("%s 的推荐状态无效。", concept$concept_id))
  if (!identical(as.character(record$target_domain), concept$target_domain)) trace_abort(sprintf("%s 试图改写目标域。", concept$concept_id))
  if (!isTRUE(record$review_required)) trace_abort(sprintf("%s 必须要求人工审核。", concept$concept_id))
  steps <- record$steps %||% list()
  if (status == "needs_information" && length(steps)) trace_abort(sprintf("%s 信息不足时不能附带可执行步骤。", concept$concept_id))
  if (status == "proposed" && !length(steps)) trace_abort(sprintf("%s proposed 候选缺少转换步骤。", concept$concept_id))
  if (length(steps)) {
    candidate_categories <- vapply(steps, function(step) registry_entry(step$transform_id, registry)$category, character(1))
    if (any(!candidate_categories %in% classification$categories)) trace_abort(sprintf("%s 使用了第一阶段未选类别中的函数。", concept$concept_id))
    proposal <- concept
    proposal$steps <- steps
    validate_concept_plan(proposal, specification, metadata, registry, config)
  }
  list(
    concept_id = concept$concept_id,
    candidate_rank = rank,
    target_domain = concept$target_domain,
    categories = classification$categories,
    steps = steps,
    recommendation_score = score,
    reason = recommendation_text(record, "reason"),
    uncertainties = recommendation_text(record, "uncertainties"),
    status = status,
    review_required = TRUE,
    provenance = provenance,
    group_id = group_id
  )
}

parse_candidate_plans_v02 <- function(records, classifications, group, specification, metadata, registry, provenance = "model",
                                      config = load_project_config()) {
  if (!is.list(records) || !length(records)) trace_abort(sprintf("%s 第二阶段没有返回候选方案。", group$group_id))
  concepts <- stats::setNames(group$concepts, vapply(group$concepts, `[[`, character(1), "concept_id"))
  classes <- stats::setNames(classifications, vapply(classifications, `[[`, character(1), "concept_id"))
  parsed <- lapply(records, function(record) {
    id <- as.character(record$concept_id %||% "")
    if (is.null(concepts[[id]])) trace_abort(sprintf("第二阶段返回未知概念：%s", id))
    validate_candidate_plan_v02(record, classes[[id]], concepts[[id]], specification, metadata, registry, group$group_id, provenance, config)
  })
  keys <- vapply(parsed, function(x) paste(x$concept_id, x$candidate_rank), character(1))
  if (anyDuplicated(keys)) trace_abort(sprintf("%s 第二阶段出现重复候选序号。", group$group_id))
  top_ids <- vapply(Filter(function(x) x$candidate_rank == 1L, parsed), `[[`, character(1), "concept_id")
  missing_top <- setdiff(names(concepts), top_ids)
  if (length(missing_top)) trace_abort(sprintf("%s 缺少首选方案：%s", group$group_id, paste(missing_top, collapse = ", ")))
  parsed
}

diagnose_candidate_plans_v03 <- function(records, classifications, group, specification, metadata, registry,
                                         provenance = "model", config = load_project_config()) {
  concepts <- stats::setNames(group$concepts, vapply(group$concepts, `[[`, character(1), "concept_id"))
  classes <- stats::setNames(classifications, vapply(classifications, `[[`, character(1), "concept_id"))
  valid <- list()
  failures <- list()
  if (!is.list(records) || !length(records)) {
    failures[[1L]] <- list(concept_id = "", candidate_rank = NA_integer_, error = "第二阶段没有返回候选方案。")
  } else {
    for (index in seq_along(records)) {
      record <- records[[index]]
      id <- as.character(record$concept_id %||% "")
      rank <- suppressWarnings(as.integer(record$candidate_rank %||% NA_integer_))
      result <- tryCatch({
        if (is.null(concepts[[id]])) trace_abort(sprintf("第二阶段返回未知概念：%s", id))
        validate_candidate_plan_v02(
          record, classes[[id]], concepts[[id]], specification, metadata, registry,
          group$group_id, provenance, config
        )
      }, error = identity)
      if (inherits(result, "error")) {
        failures[[length(failures) + 1L]] <- list(
          concept_id = id, candidate_rank = rank,
          error = sanitize_for_log(conditionMessage(result))
        )
      } else {
        valid[[length(valid) + 1L]] <- result
      }
    }
  }
  valid_top <- vapply(Filter(function(x) x$candidate_rank == 1L, valid), `[[`, character(1), "concept_id")
  missing_top <- setdiff(names(concepts), valid_top)
  list(
    valid_candidates = valid,
    failures = failures,
    valid_top_concepts = valid_top,
    missing_top_concepts = missing_top,
    diagnostic_only = TRUE
  )
}

classifications_table_v02 <- function(classifications) {
  purrr::map_dfr(classifications, function(x) tibble::tibble(
    concept_id = x$concept_id,
    categories = paste(x$categories, collapse = " | "),
    classification_score = x$classification_score,
    evidence = x$evidence,
    uncertainties = x$uncertainties,
    status = x$status,
    group_id = x$group_id
  ))
}

candidate_table_v02 <- function(candidates) {
  purrr::map_dfr(candidates, function(x) tibble::tibble(
    concept_id = x$concept_id,
    candidate_rank = x$candidate_rank,
    target_domain = x$target_domain,
    categories = paste(x$categories, collapse = " | "),
    plan_json = as.character(registry_json(list(steps = lapply(x$steps, function(step) {
      step$source_keys <- as.list(unlist(step$source_keys %||% character(), use.names = FALSE))
      step$target_variables <- as.list(unlist(step$target_variables %||% character(), use.names = FALSE))
      step
    })))),
    recommendation_score = x$recommendation_score,
    reason = x$reason,
    uncertainties = x$uncertainties,
    status = x$status,
    review_required = x$review_required,
    provenance = x$provenance,
    group_id = x$group_id
  ))
}

save_recommendations_v02 <- function(classifications, candidates, config, source, model = NA_character_, group_runs = list(),
                                     run_status = NULL, group_failures = list()) {
  ensure_output_directories(config)
  class_table <- classifications_table_v02(classifications)
  candidate_table <- candidate_table_v02(candidates)
  write_csv(class_table, trace_path(config$paths$recommendation_dir, "category_classifications.csv"))
  write_json(classifications, trace_path(config$paths$recommendation_dir, "category_classifications.json"))
  write_csv(candidate_table, trace_path(config$paths$recommendation_dir, "candidate_plans.csv"))
  write_json(candidates, trace_path(config$paths$recommendation_dir, "candidate_plans.json"))
  write_json(list(
    status = run_status %||% if (identical(source, "model")) "completed" else "reference_seed",
    schema_version = "0.2",
    scenario = config$project$scenario,
    provenance = source,
    model = model,
    prompt_design = "concept_grouped_two_stage_registry_v02",
    generated_at = utc_now(),
    concept_count = length(unique(candidate_table$concept_id)),
    candidate_count = nrow(candidate_table),
    group_runs = group_runs,
    group_failures = group_failures,
    api_key_logged = FALSE
  ), trace_path(config$paths$recommendation_dir, "model_run.json"))
  create_review_workbook(candidate_table, config, preapprove = identical(source, "reference_seed"))
  trace_info("已生成 %d 个概念的 %d 套候选方案，来源：%s。", length(unique(candidate_table$concept_id)), nrow(candidate_table), source)
  invisible(candidate_table)
}

seed_recommendations <- function(config = load_project_config()) {
  specification <- load_mapping_template(config)
  gold <- load_gold_specification(config)
  metadata <- load_metadata(config)
  registry <- load_transform_registry(config)
  validate_specification_v02(specification, config)
  dictionary_path <- trace_path(config$paths$profile_dir, "source_dictionary.csv")
  if (!file.exists(dictionary_path)) profile_sources(config)
  dictionary <- readr::read_csv(dictionary_path, show_col_types = FALSE)
  groups <- recommendation_groups(specification, dictionary, config)
  plans <- gold$plans
  classifications <- list()
  candidates <- list()
  for (group in groups) {
    for (concept in group$concepts) {
      steps <- gold_plan_steps(plans[[concept$concept_id]])
      proposal <- concept
      proposal$steps <- steps
      validate_concept_plan(proposal, specification, metadata, registry, config)
      categories <- unique(vapply(steps, function(step) registry_entry(step$transform_id, registry)$category, character(1)))
      classification <- list(
        concept_id = concept$concept_id, categories = categories, classification_score = 1,
        evidence = "专家金标准种子。", uncertainties = "不是真实模型结果。", status = "proposed", group_id = group$group_id
      )
      classifications[[length(classifications) + 1L]] <- classification
      candidates[[length(candidates) + 1L]] <- validate_candidate_plan_v02(
        list(
          concept_id = concept$concept_id, candidate_rank = 1L, target_domain = concept$target_domain,
          steps = steps, recommendation_score = 1, reason = "专家金标准种子。",
          uncertainties = "不是真实模型结果，不能用于评价模型准确率。", status = "proposed", review_required = TRUE
        ), classification, concept, specification, metadata, registry, group$group_id, "reference_seed", config
      )
    }
  }
  save_recommendations_v02(classifications, candidates, config, "reference_seed")
}

request_json_v02 <- function(prompt, endpoint, api_key, model, config, label, raw_path = NULL) {
  response <- perform_mapping_request(prompt, endpoint, api_key, model, config)
  body <- httr2::resp_body_json(response, simplifyVector = FALSE)
  content <- body$choices[[1]]$message$content %||% ""
  content_sha256 <- digest::digest(content, algo = "sha256")
  if (!is.null(raw_path)) {
    ensure_parent(raw_path)
    writeLines(enc2utf8(as.character(content)), raw_path, useBytes = TRUE)
    write_json(list(
      received_at = utc_now(),
      response_sha256 = content_sha256,
      finish_reason = body$choices[[1]]$finish_reason %||% NA_character_,
      reasoning_bytes = nchar(as.character(body$choices[[1]]$message$reasoning_content %||% ""), type = "bytes"),
      usage = body$usage %||% list()
    ), paste0(raw_path, ".metadata.json"))
  }
  if (!nzchar(trimws(content))) {
    finish_reason <- as.character(body$choices[[1]]$finish_reason %||% "未提供")
    reasoning_length <- nchar(as.character(body$choices[[1]]$message$reasoning_content %||% ""), type = "bytes")
    trace_abort(sprintf("%s 模型响应为空；结束原因=%s，推理字段=%d 字节。", label, finish_reason, reasoning_length))
  }
  parsed <- tryCatch(
    jsonlite::fromJSON(extract_json_content(content), simplifyVector = FALSE),
    error = function(error) trace_abort(sprintf("%s 不是有效 JSON：%s", label, conditionMessage(error)))
  )
  list(parsed = parsed, content_sha256 = content_sha256)
}

call_mapping_model <- function(config = load_project_config()) {
  api_key <- Sys.getenv("TRACE_SDTM_API_KEY", unset = "")
  base_url <- Sys.getenv("TRACE_SDTM_BASE_URL", unset = "")
  model <- Sys.getenv("TRACE_SDTM_MODEL", unset = "")
  if (!nzchar(api_key) || !nzchar(base_url) || !nzchar(model)) {
    trace_abort("未配置 TRACE_SDTM_API_KEY、TRACE_SDTM_BASE_URL 和 TRACE_SDTM_MODEL。可用 recommend --seed 生成明确标记的离线参考种子。")
  }
  config <- apply_model_runtime_overrides(config)
  specification <- load_mapping_template(config)
  validate_specification_v02(specification, config)
  metadata <- load_metadata(config)
  registry <- load_transform_registry(config)
  dictionary_path <- trace_path(config$paths$profile_dir, "source_dictionary.csv")
  if (!file.exists(dictionary_path)) profile_sources(config)
  dictionary <- readr::read_csv(dictionary_path, show_col_types = FALSE)
  groups <- recommendation_groups(specification, dictionary, config)
  endpoint <- paste0(sub("/$", "", base_url), config$model$endpoint_suffix)
  all_classifications <- list()
  all_candidates <- list()
  group_runs <- list()
  group_failures <- list()
  blind_evaluation <- identical(tolower(Sys.getenv("TRACE_SDTM_BLIND_EVAL", unset = "false")), "true")
  group_dir <- ensure_dir(trace_path(config$paths$recommendation_dir, "groups"))

  for (group in groups) {
    stage1_prompt <- classification_prompt_v02(group, metadata, registry, config)
    stage1_prompt_hash <- digest::digest(stage1_prompt, algo = "sha256")
    group_path <- ensure_dir(file.path(group_dir, group$group_id))
    cache_paths <- list(
      classifications = file.path(group_path, "classifications.json"),
      candidates = file.path(group_path, "candidate_plans.json"),
      run = file.path(group_path, "run_metadata.json")
    )
    write_json(group$context, file.path(group_path, "source_context.json"))
    writeLines(enc2utf8(stage1_prompt), file.path(group_path, "stage1_request.txt"), useBytes = TRUE)
    resume <- identical(tolower(Sys.getenv("TRACE_SDTM_RESUME", unset = "false")), "true")
    if (resume && all(vapply(cache_paths, file.exists, logical(1)))) {
      cached_run <- jsonlite::read_json(cache_paths$run, simplifyVector = TRUE)
      cached_class_raw <- jsonlite::read_json(cache_paths$classifications, simplifyVector = FALSE)
      cached_classifications <- parse_classifications_v02(cached_class_raw, group, registry)
      cached_stage2_prompt <- selection_prompt_v02(group, cached_classifications, metadata, registry, config)
      cache_matches <- identical(as.character(cached_run$stage1_prompt_sha256), stage1_prompt_hash) &&
        identical(as.character(cached_run$stage2_prompt_sha256), digest::digest(cached_stage2_prompt, algo = "sha256")) &&
        (is.null(cached_run$model) || identical(as.character(cached_run$model), model))
      if (cache_matches) {
        cached_candidate_raw <- jsonlite::read_json(cache_paths$candidates, simplifyVector = FALSE)
        cached_candidates <- parse_candidate_plans_v02(
          cached_candidate_raw, cached_classifications, group, specification, metadata, registry, provenance = "model", config = config
        )
        all_classifications <- c(all_classifications, cached_classifications)
        all_candidates <- c(all_candidates, cached_candidates)
        group_runs[[length(group_runs) + 1L]] <- cached_run
        trace_info("已复用并重新校验 %s 的两阶段检查点。", group$group_id)
        next
      }
    }
    trace_info("正在进行 %s 的第一阶段类别判断（%d 个临床概念）。", group$group_id, length(group$concepts))
    stage1_result <- tryCatch({
      stage1 <- request_json_v02(
        stage1_prompt, endpoint, api_key, model, config, paste0(group$group_id, " 第一阶段"),
        file.path(group_path, "stage1_raw_response.json")
      )
      classifications <- parse_classifications_v02(stage1$parsed$classifications %||% stage1$parsed, group, registry)
      write_json(classifications, file.path(group_path, "classifications.json"))
      list(stage = stage1, classifications = classifications)
    }, error = identity)
    if (inherits(stage1_result, "error")) {
      failure <- list(
        group_id = group$group_id, stage = "classification", error = sanitize_for_log(conditionMessage(stage1_result)),
        concept_ids = vapply(group$concepts, `[[`, character(1), "concept_id"), recorded_at = utc_now()
      )
      write_json(failure, file.path(group_path, "validation_failure.json"))
      group_failures[[length(group_failures) + 1L]] <- failure
      if (!blind_evaluation) stop(stage1_result)
      trace_info("盲评保留了 %s 第一阶段的失败结果，并继续下一组。", group$group_id)
      next
    }
    stage1 <- stage1_result$stage
    classifications <- stage1_result$classifications
    all_classifications <- c(all_classifications, classifications)
    trace_info("正在进行 %s 的第二阶段函数选择。", group$group_id)
    stage2_prompt <- selection_prompt_v02(group, classifications, metadata, registry, config)
    writeLines(enc2utf8(stage2_prompt), file.path(group_path, "stage2_request.txt"), useBytes = TRUE)
    stage2_result <- tryCatch({
      stage2 <- request_json_v02(
        stage2_prompt, endpoint, api_key, model, config, paste0(group$group_id, " 第二阶段"),
        file.path(group_path, "stage2_raw_response.json")
      )
      raw_records <- stage2$parsed$recommendations %||% stage2$parsed
      diagnostic <- diagnose_candidate_plans_v03(
        raw_records, classifications, group, specification, metadata, registry,
        provenance = "model", config = config
      )
      write_json(diagnostic, file.path(group_path, "stage2_concept_diagnostic.json"))
      candidates <- parse_candidate_plans_v02(
        raw_records, classifications, group,
        specification, metadata, registry, provenance = "model", config = config
      )
      list(stage = stage2, candidates = candidates)
    }, error = identity)
    if (inherits(stage2_result, "error")) {
      failure <- list(
        group_id = group$group_id, stage = "function_selection", error = sanitize_for_log(conditionMessage(stage2_result)),
        concept_ids = vapply(group$concepts, `[[`, character(1), "concept_id"), recorded_at = utc_now(),
        stage1_response_sha256 = stage1$content_sha256
      )
      write_json(failure, file.path(group_path, "validation_failure.json"))
      group_failures[[length(group_failures) + 1L]] <- failure
      if (!blind_evaluation) stop(stage2_result)
      trace_info("盲评保留了 %s 第二阶段的失败结果，并继续下一组。", group$group_id)
      next
    }
    stage2 <- stage2_result$stage
    candidates <- stage2_result$candidates
    write_json(candidates, file.path(group_path, "candidate_plans.json"))
    run <- list(
      group_id = group$group_id,
      model = model,
      thinking_mode = config$model$thinking_mode %||% "default",
      max_completion_tokens = config$model$max_completion_tokens %||% 8192L,
      concept_ids = vapply(group$concepts, `[[`, character(1), "concept_id"),
      stage1_prompt_sha256 = digest::digest(stage1_prompt, algo = "sha256"),
      stage1_response_sha256 = stage1$content_sha256,
      stage2_prompt_sha256 = digest::digest(stage2_prompt, algo = "sha256"),
      stage2_response_sha256 = stage2$content_sha256,
      visible_transform_ids = vapply(
        function_cards_for_model(registry, unique(unlist(lapply(classifications, `[[`, "categories"), use.names = FALSE))),
        `[[`, character(1), "transform_id"
      )
    )
    write_json(run, file.path(group_path, "run_metadata.json"))
    group_runs[[length(group_runs) + 1L]] <- run
    all_candidates <- c(all_candidates, candidates)
  }
  save_recommendations_v02(
    all_classifications, all_candidates, config, "model", model, group_runs,
    run_status = if (length(group_failures)) "completed_with_rejections" else "completed",
    group_failures = group_failures
  )
}
