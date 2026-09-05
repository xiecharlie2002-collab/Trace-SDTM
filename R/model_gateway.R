# Shared model gateway -------------------------------------------------------

extract_json_content <- function(content) {
  content <- trimws(content)
  content <- sub("^```(?:json)?\\s*", "", content, ignore.case = TRUE)
  sub("\\s*```$", "", content)
}

model_text <- function(record, name, default = "") {
  value <- record[[name]]
  if (is.null(value) || !length(value)) return(default)
  flattened <- unlist(value, recursive = TRUE, use.names = FALSE)
  if (!length(flattened)) return(default)
  paste(as.character(flattened), collapse = "; ")
}

apply_model_runtime_overrides <- function(config) {
  timeout_override <- Sys.getenv("TRACE_SDTM_TIMEOUT_SECONDS", unset = "")
  if (nzchar(timeout_override)) {
    timeout_value <- suppressWarnings(as.integer(timeout_override))
    if (is.na(timeout_value) || timeout_value < 1L) {
      trace_abort("TRACE_SDTM_TIMEOUT_SECONDS 必须是正整数。")
    }
    config$model$timeout_seconds <- timeout_value
  }

  token_override <- Sys.getenv("TRACE_SDTM_MAX_COMPLETION_TOKENS", unset = "")
  if (nzchar(token_override)) {
    token_value <- suppressWarnings(as.integer(token_override))
    if (is.na(token_value) || token_value < 256L) {
      trace_abort("TRACE_SDTM_MAX_COMPLETION_TOKENS 必须是不小于 256 的整数。")
    }
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

perform_model_request <- function(prompt, endpoint, api_key, model, config) {
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

request_model_json <- function(prompt, endpoint, api_key, model, config, label, raw_path = NULL) {
  response <- perform_model_request(prompt, endpoint, api_key, model, config)
  body <- httr2::resp_body_json(response, simplifyVector = FALSE)
  content <- body$choices[[1]]$message$content %||% ""
  content_sha256 <- digest::digest(content, algo = "sha256")
  if (!is.null(raw_path)) {
    ensure_parent(raw_path)
    writeLines(enc2utf8(as.character(content)), raw_path, useBytes = TRUE)
    write_json(list(
      received_at = utc_now(), response_sha256 = content_sha256,
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
