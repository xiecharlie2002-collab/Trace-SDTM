# Shared Studio user-interface helpers --------------------------------------

studio_notify_error <- function(session, expression) {
  tryCatch(
    expression,
    error = function(error) {
      shiny::showNotification(sanitize_for_log(conditionMessage(error)), type = "error", duration = 8, session = session)
      NULL
    }
  )
}

studio_input_id <- function(prefix, path) {
  paste(prefix, gsub("[^A-Za-z0-9]+", "_", path), substr(digest::digest(path, algo = "sha256", serialize = FALSE), 1L, 6L), sep = "__")
}

studio_schema_type <- function(schema) {
  types <- unname(unlist(schema$type %||% "string", use.names = FALSE))
  types <- setdiff(types, "null")
  if (length(types)) types[[1L]] else "string"
}

studio_schema_ui <- function(schema, prefix, value = NULL, input = NULL, path = "parameters") {
  type <- studio_schema_type(schema)
  id <- studio_input_id(prefix, path)
  label <- tail(strsplit(path, "\\.", perl = TRUE)[[1L]], 1L)
  nullable <- "null" %in% unname(unlist(schema$type %||% character(), use.names = FALSE))
  null_ui <- if (nullable) shiny::checkboxInput(paste0(id, "__null"), paste0(label, "：使用空值"), value = is.null(value)) else NULL
  if (identical(type, "object")) {
    properties <- schema$properties %||% list()
    if (length(properties)) {
      fields <- lapply(names(properties), function(name) studio_schema_ui(
        properties[[name]], prefix, value = value[[name]] %||% NULL, input = input,
        path = paste(path, name, sep = ".")
      ))
    } else {
      pairs <- value %||% list()
      default_count <- max(length(pairs), as.integer(schema$minProperties %||% 1L))
      count_id <- paste0(id, "__count")
      count <- if (!is.null(input) && !is.null(input[[count_id]])) as.integer(input[[count_id]]) else default_count
      count <- max(0L, min(20L, count))
      pair_names <- names(pairs) %||% character()
      fields <- c(list(shiny::numericInput(count_id, paste0(label, " 项数"), value = count, min = 0, max = 20, step = 1)),
                  lapply(seq_len(count), function(index) shiny::fluidRow(
                    shiny::column(5, shiny::textInput(paste0(id, "__key_", index), paste0("键 ", index), value = pair_names[[index]] %||% "")),
                    shiny::column(7, shiny::textInput(paste0(id, "__value_", index), paste0("值 ", index), value = as.character(pairs[[index]] %||% "")))
                  )))
    }
    return(shiny::tagList(null_ui, shiny::tags$fieldset(class = "trace-schema-fieldset", shiny::tags$legend(label), fields)))
  }
  if (identical(type, "array")) {
    values <- unname(unlist(value %||% list(), use.names = FALSE))
    item_schema <- schema$items %||% list(type = "string")
    if (length(item_schema$enum %||% list())) {
      control <- shiny::selectizeInput(id, label, choices = unname(unlist(item_schema$enum)), selected = values, multiple = TRUE)
      return(shiny::tagList(null_ui, control))
    }
    default_count <- max(length(values), as.integer(schema$minItems %||% if (length(values)) length(values) else 1L))
    count_id <- paste0(id, "__count")
    count <- if (!is.null(input) && !is.null(input[[count_id]])) as.integer(input[[count_id]]) else default_count
    max_count <- min(as.integer(schema$maxItems %||% 20L), 20L)
    count <- max(0L, min(max_count, count))
    fields <- lapply(seq_len(count), function(index) studio_schema_ui(
      item_schema, prefix, value = if (length(value) >= index) value[[index]] else NULL,
      input = input, path = paste0(path, "[", index, "]")
    ))
    return(shiny::tagList(null_ui, shiny::numericInput(count_id, paste0(label, " 项数"), value = count, min = schema$minItems %||% 0L, max = max_count, step = 1), fields))
  }
  enum <- unname(unlist(schema$enum %||% character(), use.names = FALSE))
  if (length(enum)) return(shiny::tagList(null_ui, shiny::selectInput(id, label, choices = enum, selected = value %||% enum[[1L]])))
  control <- switch(type,
    boolean = shiny::checkboxInput(id, label, value = isTRUE(value)),
    integer = shiny::numericInput(id, label, value = as.integer(value %||% schema$minimum %||% 0L), min = schema$minimum %||% NA, max = schema$maximum %||% NA, step = 1),
    number = shiny::numericInput(id, label, value = as.numeric(value %||% schema$minimum %||% 0), min = schema$minimum %||% NA, max = schema$maximum %||% NA),
    shiny::textInput(id, label, value = as.character(value %||% ""))
  )
  shiny::tagList(null_ui, control)
}

studio_schema_value <- function(schema, prefix, input, value = NULL, path = "parameters") {
  type <- studio_schema_type(schema)
  id <- studio_input_id(prefix, path)
  nullable <- "null" %in% unname(unlist(schema$type %||% character(), use.names = FALSE))
  if (nullable && isTRUE(input[[paste0(id, "__null")]])) return(NULL)
  if (identical(type, "object")) {
    properties <- schema$properties %||% list()
    if (length(properties)) {
      result <- lapply(names(properties), function(name) studio_schema_value(
        properties[[name]], prefix, input, value = value[[name]] %||% NULL,
        path = paste(path, name, sep = ".")
      ))
      names(result) <- names(properties)
      required <- unname(unlist(schema$required %||% character(), use.names = FALSE))
      keep <- names(result) %in% required | !vapply(result, function(item) is.null(item) || identical(item, ""), logical(1))
      return(result[keep])
    }
    count <- as.integer(input[[paste0(id, "__count")]] %||% 0L)
    result <- list()
    for (index in seq_len(count)) {
      key <- trimws(as.character(input[[paste0(id, "__key_", index)]] %||% ""))
      item <- input[[paste0(id, "__value_", index)]] %||% ""
      if (nzchar(key)) result[[key]] <- item
    }
    return(result)
  }
  if (identical(type, "array")) {
    item_schema <- schema$items %||% list(type = "string")
    if (length(item_schema$enum %||% list())) return(as.list(unname(input[[id]] %||% character())))
    count <- as.integer(input[[paste0(id, "__count")]] %||% 0L)
    return(lapply(seq_len(count), function(index) studio_schema_value(
      item_schema, prefix, input, value = if (length(value) >= index) value[[index]] else NULL,
      path = paste0(path, "[", index, "]")
    )))
  }
  raw <- input[[id]]
  switch(type, boolean = isTRUE(raw), integer = as.integer(raw), number = as.numeric(raw), as.character(raw %||% ""))
}

studio_with_prompt_privacy <- function(include_examples, source_keys, code) {
  variables <- c("TRACE_SDTM_PROMPT_PRIVACY", "TRACE_SDTM_INCLUDE_EXAMPLES", "TRACE_SDTM_EXAMPLE_SOURCE_KEYS")
  old <- Sys.getenv(variables, unset = NA_character_)
  on.exit({
    for (name in variables) {
      if (is.na(old[[name]])) Sys.unsetenv(name) else do.call(Sys.setenv, stats::setNames(list(old[[name]]), name))
    }
  }, add = TRUE)
  Sys.setenv(
    TRACE_SDTM_PROMPT_PRIVACY = if (isTRUE(include_examples)) "selected_examples" else "metadata_only",
    TRACE_SDTM_INCLUDE_EXAMPLES = if (isTRUE(include_examples)) "1" else "0",
    TRACE_SDTM_EXAMPLE_SOURCE_KEYS = paste(unlist(source_keys %||% character()), collapse = ",")
  )
  force(code)
}

studio_prompt_preview <- function(config, stage = "targets", include_examples = FALSE, source_keys = character()) {
  studio_with_prompt_privacy(include_examples, source_keys, {
    specification <- load_task_specification(config)
    metadata <- load_metadata(config)
    registry <- load_transform_registry(config)
    policies <- load_mapping_policies(config)
    dictionary <- load_source_dictionary(config)
    groups <- recommendation_groups(specification)
    prompts <- if (identical(stage, "targets")) {
      lapply(groups, target_prompt, specification = specification, metadata = metadata, policies = policies, dictionary = dictionary)
    } else if (identical(stage, "functions")) {
      targets <- read_recommendation_stage(config, "target_decisions.json")
      lapply(groups, function(group) {
        ids <- vapply(group$tasks, task_identifier, character(1))
        subset <- list(valid = targets$valid[intersect(ids, names(targets$valid))], failures = list())
        function_prompt(group, subset, registry, specification, policies, dictionary, load_recommendation_resources(config))
      })
    } else {
      targets <- read_recommendation_stage(config, "target_decisions.json")
      functions <- read_recommendation_stage(config, "function_candidates.json")
      resolutions <- resolve_known_parameters(
        specification, targets, functions, registry, policies, load_recommendation_resources(config)
      )
      list(parameter_prompt(resolutions) %||% "全部参数均由程序确定性注入，本阶段不发送模型请求。")
    }
    text <- paste(unlist(prompts), collapse = "\n\n===== 下一组 =====\n\n")
    list(text = text, sha256 = digest::digest(text, algo = "sha256", serialize = FALSE), stage = stage,
         examples_included = isTRUE(include_examples))
  })
}
