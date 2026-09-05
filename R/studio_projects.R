# TraceSDTM Studio 0.6 project service ---------------------------------------

studio_settings <- function() {
  path <- trace_path("config", "studio.yml")
  defaults <- list(
    schema_version = "0.6",
    host = "127.0.0.1",
    port_range = as.list(3838:3848),
    max_upload_mb = 100,
    projects_root = file.path("workspace", "projects"),
    poll_interval_ms = 500,
    evidence_include_raw = FALSE
  )
  if (!file.exists(path)) return(defaults)
  utils::modifyList(defaults, yaml::read_yaml(path), keep.null = TRUE)
}

studio_home <- function(create = TRUE) {
  configured <- Sys.getenv("TRACE_SDTM_STUDIO_HOME", unset = "")
  if (!nzchar(configured)) configured <- studio_settings()$projects_root
  path <- if (is_absolute_path(configured)) configured else trace_path(configured)
  if (isTRUE(create)) ensure_dir(path)
  normalizePath(path, winslash = "/", mustWork = isTRUE(create))
}

studio_validate_project_id <- function(project_id) {
  project_id <- trimws(as.character(project_id %||% ""))
  if (!grepl("^[a-z0-9][a-z0-9-]{2,47}$", project_id)) {
    trace_abort("项目编号必须为3至48位小写字母、数字或连字符，并以字母或数字开头。")
  }
  project_id
}

studio_validate_run_id <- function(run_id) {
  run_id <- trimws(as.character(run_id %||% ""))
  if (!grepl("^run_[0-9]{8}_[0-9]{6}_[0-9]+(?:_[0-9]+)?$", run_id)) {
    trace_abort("运行编号格式无效。")
  }
  run_id
}

studio_path_within <- function(path, root) {
  root_value <- tolower(normalizePath(root, winslash = "/", mustWork = TRUE))
  path_value <- tolower(normalizePath(path, winslash = "/", mustWork = FALSE))
  identical(path_value, root_value) || startsWith(path_value, paste0(root_value, "/"))
}

studio_project_path <- function(project_id, must_exist = TRUE) {
  project_id <- studio_validate_project_id(project_id)
  root <- studio_home(create = TRUE)
  path <- file.path(root, project_id)
  if (!studio_path_within(path, root)) trace_abort("项目路径超出工作台根目录。")
  if (isTRUE(must_exist) && !file.exists(file.path(path, "project.yml"))) {
    trace_abort(sprintf("项目不存在：%s。", project_id))
  }
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

studio_relative_path <- function(path, root) {
  normalized_path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  normalized_root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  prefix <- paste0(normalized_root, "/")
  if (!startsWith(tolower(normalized_path), tolower(prefix))) trace_abort("路径不属于指定项目。")
  substring(normalized_path, nchar(prefix) + 1L)
}

studio_read_project <- function(project_id) {
  path <- studio_project_path(project_id)
  value <- yaml::read_yaml(file.path(path, "project.yml"))
  if (!identical(as.character(value$schema_version), "0.6")) {
    trace_abort("该项目使用不兼容的旧版工作台结构，不能在 TraceSDTM 0.7 中运行。请新建项目并重新上传原始数据。")
  }
  value
}

studio_write_project <- function(project_id, value) {
  path <- studio_project_path(project_id)
  value$updated_at <- utc_now()
  write_yaml(value, file.path(path, "project.yml"))
}

studio_append_audit <- function(project_id, event, details = list(), run_id = NULL,
                                actor = "local_user") {
  path <- studio_project_path(project_id)
  forbidden <- c("api_key", "secret", "password", "raw_values", "example_values")
  if (length(intersect(tolower(names(details)), forbidden))) {
    trace_abort("审计事件包含禁止持久化的字段。")
  }
  record <- list(
    schema_version = "0.6", event_id = paste0("evt_", digest::digest(paste(Sys.time(), runif(1)), algo = "sha256", serialize = FALSE)),
    occurred_at = utc_now(), project_id = project_id, run_id = run_id,
    actor = as.character(actor %||% "local_user"), event = as.character(event), details = details
  )
  target <- file.path(path, "audit", "events.jsonl")
  ensure_parent(target)
  line <- jsonlite::toJSON(record, auto_unbox = TRUE, null = "null", na = "null")
  connection <- file(target, open = "a", encoding = "UTF-8")
  on.exit(close(connection), add = TRUE)
  writeLines(enc2utf8(line), connection, useBytes = TRUE)
  invisible(record)
}

studio_with_project_lock <- function(project_id, code) {
  path <- studio_project_path(project_id)
  lock_path <- file.path(path, ".trace-project.lock")
  connection <- tryCatch(file(lock_path, open = "wx", encoding = "UTF-8"), error = identity)
  if (inherits(connection, "error")) trace_abort("该项目正在被网页或命令行任务写入，请稍后重试。")
  writeLines(sprintf("pid=%s\nstarted=%s", Sys.getpid(), utc_now()), connection)
  close(connection)
  on.exit(if (file.exists(lock_path)) unlink(lock_path, force = TRUE), add = TRUE)
  force(code)
}

studio_supported_standards <- function() {
  metadata <- yaml::read_yaml(trace_path("specs", "sdtm_metadata.yml"))
  list(list(
    standard = as.character(metadata$standard %||% "SDTMIG"),
    version = as.character(metadata$version %||% "3.4"),
    metadata = trace_path("specs", "sdtm_metadata.yml")
  ))
}

studio_default_policy_v06 <- function(study_id) {
  list(
    schema_version = "0.6", policy_version = "3.0.0", scenario = "generic",
    identifiers = list(usubjid = list(
      components = list(), separator = "-", missing_component_policy = "reject"
    )),
    date_time_formats = list(
      preserve_partial_dates = TRUE, prohibit_date_imputation = TRUE,
      partial_date_tokens = list("UNK", "UN"), approved_sources = list()
    ),
    upstream_outputs = list(), sequence_rules = list(), baseline_rules = list(),
    unit_standardization = list(required = FALSE, identity_conversion_required = FALSE),
    dataset_output_contract = list(
      applies_when_output_mode = "dataset", target_variables = list(),
      dataset_name_parameter = "output_dataset"
    ),
    parameter_bindings = list(), project_defaults = list(study_id = study_id)
  )
}

studio_source_catalog_file <- function(project_id) {
  file.path(studio_project_path(project_id), "config", "source_catalog.yml")
}

studio_read_source_catalog <- function(project_id) {
  path <- studio_source_catalog_file(project_id)
  if (!file.exists(path)) return(list())
  value <- yaml::read_yaml(path)
  value$source_catalog %||% list()
}

studio_write_source_catalog <- function(project_id, value) {
  write_yaml(list(schema_version = "0.6", source_catalog = value), studio_source_catalog_file(project_id))
}

studio_create_project <- function(project_id, name, study_id = "TRACE001", description,
                                  standard = "SDTMIG", standard_version = "3.4",
                                  target_domains = character()) {
  project_id <- studio_validate_project_id(project_id)
  name <- trimws(as.character(name %||% ""))
  study_id <- trimws(as.character(study_id %||% ""))
  description <- trimws(as.character(description %||% ""))
  standard <- trimws(as.character(standard %||% "SDTMIG"))
  standard_version <- trimws(as.character(standard_version %||% "3.4"))
  target_domains <- unique(as.character(unname(unlist(target_domains %||% character(), use.names = FALSE))))
  if (!nzchar(name)) trace_abort("项目名称不能为空。")
  if (!nzchar(study_id) || nchar(study_id) > 40L) trace_abort("研究编号必须为1至40个字符。")
  if (!nzchar(description) || nchar(description) > 12000L) trace_abort("项目描述必须为1至12000个字符。")
  supported <- studio_supported_standards()
  matched <- Filter(function(item) identical(item$standard, standard) && identical(item$version, standard_version), supported)
  if (length(matched) != 1L) trace_abort(sprintf("当前未安装标准元数据：%s %s。", standard, standard_version))
  metadata <- yaml::read_yaml(matched[[1]]$metadata)
  unknown_domains <- setdiff(target_domains, names(metadata$domains))
  if (length(unknown_domains)) trace_abort(sprintf("目标域不在当前标准目录中：%s。", paste(unknown_domains, collapse = "、")))
  path <- studio_project_path(project_id, must_exist = FALSE)
  if (file.exists(file.path(path, "project.yml"))) trace_abort(sprintf("项目已存在：%s。", project_id))

  for (relative in c(
    "raw/original", "raw/normalized", "config", "runs", "audit", "exports"
  )) ensure_dir(file.path(path, relative))

  frozen <- c(
    metadata = matched[[1]]$metadata,
    transform_registry = trace_path("config", "transform_registry.yml"),
    transform_registry_schema = trace_path("config", "transform_registry.schema.json"),
    controlled_terminology = trace_path("specs", "resources", "controlled_terminology.yml"),
    unit_conversions = trace_path("specs", "resources", "unit_conversions.yml"),
    analysis_plan_schema = trace_path("specs", "analysis_plan.schema.json")
  )
  target_names <- c(metadata = "metadata.yml", transform_registry = "transform_registry.yml",
                    transform_registry_schema = "transform_registry.schema.json",
                    controlled_terminology = "controlled_terminology.yml", unit_conversions = "unit_conversions.yml",
                    analysis_plan_schema = "analysis_plan.schema.json")
  frozen_manifest <- list()
  for (key in names(frozen)) {
    source <- trace_path(frozen[[key]])
    target <- file.path(path, "config", target_names[[key]])
    if (!file.exists(source) || !isTRUE(file.copy(source, target, overwrite = FALSE, copy.date = TRUE))) {
      trace_abort(sprintf("无法冻结模板资源：%s。", key))
    }
    frozen_manifest[[key]] <- list(file = file.path("config", target_names[[key]]), sha256 = file_sha256(target))
  }
  write_yaml(studio_default_policy_v06(study_id), file.path(path, "config", "mapping_policies.yml"))

  project <- list(
    schema_version = "0.6", project_id = project_id, name = name,
    study_id = study_id, description = description, standard = standard,
    standard_version = standard_version, target_domains = as.list(target_domains),
    config_version = "0.7.0", analysis_configured = FALSE,
    created_at = utc_now(), updated_at = utc_now(), archived = FALSE,
    active_run_id = NULL, active_run_stale = FALSE,
    stages = list(data_sources = "not_started", profile = "not_started", task_discovery = "not_started",
                  task_confirmation = "not_started", mapping = "not_started", ai_review = "not_started",
                  approval = "not_started", build = "not_started"),
    frozen_resources = frozen_manifest
  )
  write_yaml(project, file.path(path, "project.yml"))
  studio_write_source_catalog(project_id, list())
  studio_append_audit(project_id, "project_created", list(
    study_id = study_id, standard = standard, standard_version = standard_version,
    target_domains = as.list(target_domains), description_sha256 = digest::digest(description, algo = "sha256", serialize = FALSE)
  ))
  project
}

studio_import_analysis_plan_v07 <- function(project_id, upload_path, actor = "local_user") {
  studio_with_project_lock(project_id, {
    project <- studio_read_project(project_id)
    if (isTRUE(project$archived)) trace_abort("归档项目不能修改。")
    upload_path <- normalizePath(upload_path, winslash = "/", mustWork = TRUE)
    config <- load_project_config()
    config$paths$analysis_plan_schema <- file.path(studio_project_path(project_id), "config", "analysis_plan.schema.json")
    plan <- yaml::read_yaml(upload_path)
    validate_analysis_plan(plan, config)
    target <- file.path(studio_project_path(project_id), "config", "analysis_plan.yml")
    if (!isTRUE(file.copy(upload_path, target, overwrite = TRUE, copy.date = TRUE))) trace_abort("无法导入分析规格。")
    project$analysis_configured <- TRUE
    project$analysis_plan_sha256 <- file_sha256(target)
    studio_write_project(project_id, project)
    studio_mark_active_run_stale(project_id, "项目分析规格已更新", actor)
    studio_append_audit(project_id, "analysis_plan_imported", list(
      analysis_plan_sha256 = project$analysis_plan_sha256, schema_version = as.character(plan$schema_version)
    ), actor = actor)
  })
  invisible(TRUE)
}

studio_update_project_v06 <- function(project_id, name, study_id, description,
                                      standard = "SDTMIG", standard_version = "3.4",
                                      target_domains = character(), actor = "local_user") {
  studio_with_project_lock(project_id, {
    project <- studio_read_project(project_id)
    if (isTRUE(project$archived)) trace_abort("归档项目不能修改。")
    name <- trimws(as.character(name %||% ""))
    study_id <- trimws(as.character(study_id %||% ""))
    description <- trimws(as.character(description %||% ""))
    target_domains <- unique(as.character(unlist(target_domains %||% character(), use.names = FALSE)))
    if (!nzchar(name)) trace_abort("项目名称不能为空。")
    if (!nzchar(study_id) || nchar(study_id) > 40L) trace_abort("研究编号必须为1至40个字符。")
    if (!nzchar(description) || nchar(description) > 12000L) trace_abort("项目描述必须为1至12000个字符。")
    supported <- studio_supported_standards()
    matched <- Filter(function(item) identical(item$standard, standard) && identical(item$version, standard_version), supported)
    if (length(matched) != 1L) trace_abort(sprintf("当前未安装标准元数据：%s %s。", standard, standard_version))
    metadata <- yaml::read_yaml(matched[[1L]]$metadata)
    unknown <- setdiff(target_domains, names(metadata$domains))
    if (length(unknown)) trace_abort(sprintf("目标域不在当前标准目录中：%s。", paste(unknown, collapse = "、")))
    before <- list(
      name = project$name, study_id = project$study_id, description = project$description,
      standard = project$standard, standard_version = project$standard_version,
      target_domains = unlist(project$target_domains %||% character(), use.names = FALSE)
    )
    project$name <- name
    project$study_id <- study_id
    project$description <- description
    project$standard <- standard
    project$standard_version <- standard_version
    project$target_domains <- as.list(target_domains)
    studio_write_project(project_id, project)
    changed <- !identical(before, list(
      name = name, study_id = study_id, description = description,
      standard = standard, standard_version = standard_version, target_domains = target_domains
    ))
    if (changed) {
      studio_mark_active_run_stale(project_id, "项目描述、研究编号、标准或目标域范围已更新", actor)
      studio_append_audit(project_id, "project_context_updated", list(
        study_id = study_id, standard = standard, standard_version = standard_version,
        target_domains = as.list(target_domains),
        description_sha256 = digest::digest(description, algo = "sha256", serialize = FALSE)
      ), actor = actor)
    }
    studio_read_project(project_id)
  })
}

studio_list_projects <- function(include_archived = FALSE) {
  root <- studio_home(create = TRUE)
  directories <- list.dirs(root, recursive = FALSE, full.names = TRUE)
  rows <- lapply(directories, function(path) {
    file <- file.path(path, "project.yml")
    if (!file.exists(file)) return(NULL)
    value <- tryCatch(yaml::read_yaml(file), error = function(error) NULL)
    if (is.null(value) || !as.character(value$schema_version %||% "") %in% c("0.5", "0.6")) return(NULL)
    compatible <- identical(as.character(value$schema_version), "0.6")
    data.frame(
      project_id = as.character(value$project_id), name = as.character(value$name),
      schema_version = as.character(value$schema_version), compatible = compatible,
      study_id = as.character(value$study_id),
      standard = if (compatible) paste(value$standard, value$standard_version) else "旧版模板项目",
      target_domains = if (compatible) paste(unlist(value$target_domains %||% character()), collapse = "、") else "",
      active_run_id = as.character(value$active_run_id %||% ""),
      stale = isTRUE(value$active_run_stale), archived = isTRUE(value$archived),
      updated_at = as.character(value$updated_at %||% value$created_at), stringsAsFactors = FALSE
    )
  })
  result <- dplyr::bind_rows(Filter(Negate(is.null), rows))
  if (!nrow(result)) return(tibble::tibble(
    project_id = character(), name = character(), schema_version = character(), compatible = logical(),
    study_id = character(), standard = character(), target_domains = character(),
    active_run_id = character(), stale = logical(), archived = logical(), updated_at = character()
  ))
  if (!isTRUE(include_archived)) result <- dplyr::filter(result, !.data$archived)
  dplyr::arrange(result, dplyr::desc(.data$updated_at))
}

studio_archive_project <- function(project_id, actor = "local_user") {
  studio_with_project_lock(project_id, {
    project <- studio_read_project(project_id)
    project$archived <- TRUE
    project$archived_at <- utc_now()
    studio_write_project(project_id, project)
    studio_append_audit(project_id, "project_archived", actor = actor)
  })
  invisible(TRUE)
}

studio_template_specification <- function(project_id) {
  trace_abort("TraceSDTM 0.7 不使用模板规格。任务只存在于运行快照中。")
}

studio_source_catalog <- function(project_id) studio_read_source_catalog(project_id)

studio_non_derived_sources <- function(project_id) {
  Filter(function(x) !isTRUE(x$derived), studio_source_catalog(project_id))
}

studio_binding_file <- function(project_id) file.path(studio_project_path(project_id), "config", "field_bindings.yml")

studio_read_bindings <- function(project_id) {
  path <- studio_binding_file(project_id)
  if (!file.exists(path)) return(list(schema_version = "0.5", datasets = list()))
  yaml::read_yaml(path)
}

studio_write_bindings <- function(project_id, value) write_yaml(value, studio_binding_file(project_id))

studio_required_fields <- function(project_id, dataset) {
  specification <- studio_template_specification(project_id)
  source <- specification$source_catalog[[dataset]]
  if (is.null(source) || isTRUE(source$derived)) trace_abort(sprintf("未知或派生来源槽位：%s。", dataset))
  refs <- unlist(lapply(specification$tasks, function(task) {
    Filter(function(ref) identical(as.character(ref$dataset), dataset), task_source_refs(task))
  }), recursive = FALSE)
  derived_catalog <- Filter(function(item) {
    parents <- unlist(item$profile_parents %||% character(), use.names = FALSE)
    isTRUE(item$derived) && length(parents) && identical(parents[[1L]], dataset)
  }, specification$source_catalog)
  if (length(derived_catalog)) {
    for (derived_name in names(derived_catalog)) {
      derived <- derived_catalog[[derived_name]]
      derived_refs <- unlist(lapply(specification$tasks, function(task) {
        Filter(function(ref) identical(as.character(ref$dataset), derived_name), task_source_refs(task))
      }), recursive = FALSE)
      other_parents <- setdiff(unlist(derived$profile_parents, use.names = FALSE), dataset)
      explicitly_owned_elsewhere <- unique(unlist(lapply(specification$tasks, function(task) {
        other <- Filter(function(ref) as.character(ref$dataset) %in% other_parents, task_source_refs(task))
        vapply(other, function(ref) as.character(ref$variable), character(1))
      }), use.names = FALSE))
      derived_keys <- unlist(derived$keys %||% character(), use.names = FALSE)
      derived_refs <- Filter(function(ref) !as.character(ref$variable) %in% setdiff(explicitly_owned_elsewhere, derived_keys), derived_refs)
      refs <- c(refs, derived_refs)
    }
  }
  if (length(refs)) refs <- refs[!duplicated(vapply(refs, function(ref) as.character(ref$variable), character(1)))]
  variables <- unique(c(unlist(source$keys, use.names = FALSE), vapply(refs, function(ref) as.character(ref$variable), character(1))))
  roles <- stats::setNames(rep("", length(variables)), variables)
  for (ref in refs) if (!nzchar(roles[[ref$variable]])) roles[[ref$variable]] <- as.character(ref$role %||% "")
  tibble::tibble(
    logical_field = variables,
    label = vapply(variables, infer_source_label, character(1)),
    role = unname(roles[variables]),
    is_key = variables %in% unlist(source$keys, use.names = FALSE),
    required = TRUE
  )
}

studio_validate_utf8_csv <- function(path, max_mb = studio_settings()$max_upload_mb) {
  if (!file.exists(path)) trace_abort("上传文件不存在。")
  if (tolower(tools::file_ext(path)) != "csv") trace_abort("只接受CSV文件。")
  size <- file.info(path)$size
  if (is.na(size) || size <= 0) trace_abort("CSV文件为空。")
  if (size > as.numeric(max_mb) * 1024^2) trace_abort(sprintf("CSV超过%s MB上限。", max_mb))
  raw <- readBin(path, what = "raw", n = size)
  if (any(raw == as.raw(0L))) trace_abort("CSV包含空字节，可能不是UTF-8文本。")
  if (length(raw) >= 3L && identical(as.integer(raw[1:3]), c(239L, 187L, 191L))) raw <- raw[-(1:3)]
  text <- tryCatch(rawToChar(raw), error = identity)
  if (inherits(text, "error") || is.na(iconv(text, from = "UTF-8", to = "UTF-8", sub = NA_character_))) {
    trace_abort("CSV不是有效的UTF-8或UTF-8 BOM编码。")
  }
  data <- tryCatch(readr::read_csv(
    I(text), col_types = readr::cols(.default = readr::col_character()),
    na = c("", "NA", "N/A"), name_repair = "minimal", show_col_types = FALSE,
    progress = FALSE, trim_ws = FALSE
  ), error = identity)
  if (inherits(data, "error")) trace_abort(sprintf("CSV解析失败：%s", conditionMessage(data)))
  if (!ncol(data)) trace_abort("CSV没有字段。")
  if (any(!nzchar(trimws(names(data))))) trace_abort("CSV包含空字段名。")
  if (anyDuplicated(names(data))) trace_abort(sprintf("CSV包含重复字段名：%s。", paste(unique(names(data)[duplicated(names(data))]), collapse = "、")))
  data
}

studio_normalize_name <- function(value) toupper(gsub("[^A-Za-z0-9]", "", iconv(as.character(value), to = "ASCII//TRANSLIT")))

studio_binding_score <- function(logical_field, label, source_field) {
  logical_normal <- studio_normalize_name(logical_field)
  source_normal <- studio_normalize_name(source_field)
  label_normal <- studio_normalize_name(label)
  if (identical(logical_field, source_field)) return(1)
  if (identical(toupper(logical_field), toupper(source_field))) return(0.99)
  if (nzchar(logical_normal) && identical(logical_normal, source_normal)) return(0.97)
  candidates <- c(logical_normal, label_normal)
  candidates <- candidates[nzchar(candidates)]
  if (!length(candidates) || !nzchar(source_normal)) return(0)
  max(vapply(candidates, function(candidate) {
    distance <- as.numeric(utils::adist(candidate, source_normal))
    max(0, 1 - distance / max(nchar(candidate), nchar(source_normal), 1L))
  }, numeric(1)))
}

studio_binding_suggestions <- function(project_id, dataset) {
  bindings <- studio_read_bindings(project_id)
  item <- bindings$datasets[[dataset]]
  if (is.null(item$original_file)) trace_abort(sprintf("来源槽位 %s 尚未上传文件。", dataset))
  path <- file.path(studio_project_path(project_id), item$original_file)
  data <- studio_validate_utf8_csv(path)
  required <- studio_required_fields(project_id, dataset)
  purrr::map_dfr(seq_len(nrow(required)), function(index) {
    scores <- vapply(names(data), function(source) studio_binding_score(
      required$logical_field[[index]], required$label[[index]], source
    ), numeric(1))
    order <- order(scores, decreasing = TRUE)
    tibble::tibble(
      logical_field = required$logical_field[[index]], label = required$label[[index]],
      role = required$role[[index]], is_key = required$is_key[[index]],
      suggested_source = names(data)[order[[1L]]], score = round(scores[order[[1L]]], 3),
      alternatives = paste(utils::head(names(data)[order], 3L), collapse = " | ")
    )
  })
}

studio_assert_study_id <- function(project_id, dataset, data, expected = NULL) {
  required <- studio_required_fields(project_id, dataset)
  fields <- required$logical_field[grepl("study_identifier", required$role, ignore.case = TRUE)]
  fields <- intersect(fields, names(data))
  if (!length(fields)) return(invisible(TRUE))
  values <- unique(trimws(unlist(lapply(data[fields], as.character), use.names = FALSE)))
  values <- values[!is.na(values) & nzchar(values)]
  expected <- trimws(as.character(expected %||% studio_read_project(project_id)$study_id))
  unexpected <- setdiff(values, expected)
  if (length(unexpected)) {
    trace_abort(sprintf(
      "%s 的研究标识字段与项目研究编号 %s 不一致：%s。",
      dataset, expected, paste(utils::head(unexpected, 5L), collapse = "、")
    ))
  }
  invisible(TRUE)
}

studio_assert_confirmed_study_ids <- function(project_id, expected) {
  bindings <- studio_read_bindings(project_id)
  for (dataset in names(studio_non_derived_sources(project_id))) {
    item <- bindings$datasets[[dataset]]
    if (!identical(item$status %||% "", "confirmed")) next
    path <- file.path(studio_project_path(project_id), item$normalized_file)
    data <- readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE)
    studio_assert_study_id(project_id, dataset, data, expected)
  }
  invisible(TRUE)
}

studio_mark_active_run_stale <- function(project_id, reason, actor = "local_user") {
  project <- studio_read_project(project_id)
  has_active_run <- nzchar(as.character(project$active_run_id %||% ""))
  project$active_run_stale <- has_active_run
  if (has_active_run) {
    manifest_path <- file.path(studio_project_path(project_id), "runs", project$active_run_id, "run.yml")
    if (file.exists(manifest_path)) {
      manifest <- yaml::read_yaml(manifest_path)
      manifest$stale <- TRUE
      manifest$stale_reason <- as.character(reason)
      manifest$stale_at <- utc_now()
      write_yaml(manifest, manifest_path)
    }
  }
  studio_write_project(project_id, project)
  if (has_active_run) studio_append_audit(project_id, "active_run_marked_stale", list(reason = as.character(reason)), project$active_run_id, actor)
  invisible(project)
}

studio_dataset_id_v06 <- function(filename) {
  stem <- tools::file_path_sans_ext(basename(as.character(filename %||% "")))
  ascii <- suppressWarnings(iconv(stem, from = "", to = "ASCII//TRANSLIT", sub = ""))
  id <- tolower(gsub("[^A-Za-z0-9]+", "_", ascii))
  id <- gsub("^_+|_+$", "", id)
  if (!nzchar(id)) id <- paste0("dataset_", substr(digest::digest(stem, algo = "sha256", serialize = FALSE), 1L, 8L))
  if (grepl("^[0-9]", id)) id <- paste0("d_", id)
  substr(id, 1L, 48L)
}

studio_import_sources <- function(project_id, uploaded_paths, original_names,
                                  actor = "local_user") {
  uploaded_paths <- as.character(uploaded_paths %||% character())
  original_names <- as.character(original_names %||% character())
  if (!length(uploaded_paths) || length(uploaded_paths) != length(original_names)) {
    trace_abort("请选择一个或多个CSV文件。")
  }
  studio_with_project_lock(project_id, {
    project <- studio_read_project(project_id)
    if (isTRUE(project$archived)) trace_abort("归档项目不能上传数据。")
    ids <- vapply(original_names, studio_dataset_id_v06, character(1))
    if (anyDuplicated(ids)) trace_abort(sprintf(
      "本次上传存在重复数据集编号：%s。请先修改文件名。", paste(unique(ids[duplicated(ids)]), collapse = "、")
    ))
    catalog <- studio_read_source_catalog(project_id)
    collisions <- intersect(ids, names(catalog))
    if (length(collisions)) trace_abort(sprintf(
      "数据集编号已存在：%s。为保留审计历史，请修改文件名后重新上传。", paste(collisions, collapse = "、")
    ))
    inspected <- lapply(uploaded_paths, studio_validate_utf8_csv)
    hashes <- vapply(uploaded_paths, file_sha256, character(1))
    stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    project_path <- studio_project_path(project_id)
    rows <- list()
    for (index in seq_along(uploaded_paths)) {
      id <- ids[[index]]
      original_target <- file.path(project_path, "raw", "original", sprintf(
        "%s_%s_%s.csv", id, stamp, substr(hashes[[index]], 1L, 10L)
      ))
      normalized_target <- file.path(project_path, "raw", "normalized", paste0(id, ".csv"))
      if (file.exists(original_target) || file.exists(normalized_target)) {
        trace_abort(sprintf("数据集 %s 的目标文件已经存在。", id))
      }
      if (!isTRUE(file.copy(uploaded_paths[[index]], original_target, overwrite = FALSE, copy.date = TRUE)) ||
          !isTRUE(file.copy(uploaded_paths[[index]], normalized_target, overwrite = FALSE, copy.date = TRUE))) {
        trace_abort(sprintf("无法保存数据集 %s。", id))
      }
      data <- inspected[[index]]
      catalog[[id]] <- list(
        file = basename(normalized_target), original_name = basename(original_names[[index]]),
        original_file = studio_relative_path(original_target, project_path),
        form_name = tools::file_path_sans_ext(basename(original_names[[index]])),
        grain = "unknown", keys = list(), derived = FALSE,
        row_count = nrow(data), column_count = ncol(data), columns = as.list(names(data)),
        sha256 = hashes[[index]], uploaded_at = utc_now()
      )
      rows[[length(rows) + 1L]] <- tibble::tibble(
        dataset = id, original_name = basename(original_names[[index]]),
        rows = nrow(data), columns = ncol(data), sha256 = hashes[[index]]
      )
      studio_append_audit(project_id, "source_uploaded", list(
        dataset = id, original_name = basename(original_names[[index]]), sha256 = hashes[[index]],
        row_count = nrow(data), column_count = ncol(data)
      ), actor = actor)
    }
    studio_write_source_catalog(project_id, catalog)
    project$stages$data_sources <- "ready"
    studio_write_project(project_id, project)
    studio_mark_active_run_stale(project_id, "通用来源数据已更新", actor)
    dplyr::bind_rows(rows)
  })
}

studio_source_manifest <- function(project_id) {
  catalog <- studio_read_source_catalog(project_id)
  purrr::imap_dfr(catalog, function(item, id) tibble::tibble(
    dataset = id, original_name = as.character(item$original_name %||% item$file),
    rows = as.integer(item$row_count %||% NA_integer_),
    columns = as.integer(item$column_count %||% length(item$columns %||% list())),
    grain = as.character(item$grain %||% "unknown"),
    keys = paste(unlist(item$keys %||% character()), collapse = "、"),
    sha256 = as.character(item$sha256 %||% "")
  ))
}

studio_source_preview_v06 <- function(project_id, dataset, rows = 20L) {
  source <- studio_read_source_catalog(project_id)[[dataset]]
  if (is.null(source)) trace_abort(sprintf("未知数据集：%s。", dataset))
  path <- file.path(studio_project_path(project_id), "raw", "normalized", source$file)
  utils::head(studio_validate_utf8_csv(path), as.integer(rows))
}

studio_import_source <- function(project_id, dataset, uploaded_path, original_name = basename(uploaded_path),
                                 actor = "local_user") {
  studio_with_project_lock(project_id, {
    source <- studio_source_catalog(project_id)[[dataset]]
    if (is.null(source) || isTRUE(source$derived)) trace_abort(sprintf("未知或派生来源槽位：%s。", dataset))
    data <- studio_validate_utf8_csv(uploaded_path)
    hash <- file_sha256(uploaded_path)
    stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    target_name <- sprintf("%s_%s_%s.csv", dataset, stamp, substr(hash, 1L, 10L))
    target <- file.path(studio_project_path(project_id), "raw", "original", target_name)
    if (!isTRUE(file.copy(uploaded_path, target, overwrite = FALSE, copy.date = TRUE))) trace_abort("无法保存上传文件。")
    bindings <- studio_read_bindings(project_id)
    bindings$datasets[[dataset]] <- list(
      original_file = studio_relative_path(target, studio_project_path(project_id)),
      original_name = basename(original_name), sha256 = hash,
      uploaded_at = utc_now(), row_count = nrow(data), column_count = ncol(data),
      status = "uploaded_unbound", bindings = list()
    )
    studio_write_bindings(project_id, bindings)
    project <- studio_read_project(project_id)
    project$stages$data_sources <- "binding_required"
    studio_write_project(project_id, project)
    studio_mark_active_run_stale(project_id, sprintf("来源 %s 已更新", dataset), actor)
    studio_append_audit(project_id, "source_uploaded", list(
      dataset = dataset, original_name = basename(original_name), sha256 = hash,
      row_count = nrow(data), column_count = ncol(data)
    ), actor = actor)
  })
  studio_binding_suggestions(project_id, dataset)
}

studio_confirm_bindings <- function(project_id, dataset, mapping, actor = "local_user") {
  studio_with_project_lock(project_id, {
    mapping <- unlist(mapping, use.names = TRUE)
    required <- studio_required_fields(project_id, dataset)$logical_field
    if (is.null(names(mapping)) || !setequal(names(mapping), required)) trace_abort("字段绑定必须完整覆盖模板逻辑字段。")
    mapping <- mapping[required]
    if (any(!nzchar(mapping)) || anyDuplicated(unname(mapping))) trace_abort("每个逻辑字段必须绑定到不同的上传字段。")
    bindings <- studio_read_bindings(project_id)
    item <- bindings$datasets[[dataset]]
    if (is.null(item$original_file)) trace_abort("尚未上传来源文件。")
    original_path <- file.path(studio_project_path(project_id), item$original_file)
    data <- studio_validate_utf8_csv(original_path)
    unknown <- setdiff(unname(mapping), names(data))
    if (length(unknown)) trace_abort(sprintf("绑定引用未知字段：%s。", paste(unknown, collapse = "、")))
    normalized <- data[, unname(mapping), drop = FALSE]
    names(normalized) <- names(mapping)
    studio_assert_study_id(project_id, dataset, normalized)
    source <- studio_source_catalog(project_id)[[dataset]]
    key_fields <- unlist(source$keys, use.names = FALSE)
    key_data <- normalized[, key_fields, drop = FALSE]
    key_missing <- apply(key_data, 1L, function(row) any(is.na(row) | !nzchar(trimws(as.character(row)))))
    if (any(key_missing)) trace_abort(sprintf("%s 的连接键有 %d 行缺失。", dataset, sum(key_missing)))
    key_text <- do.call(paste, c(lapply(key_data, as.character), sep = "\u001F"))
    require_unique <- grepl("^one_record", as.character(source$grain %||% ""))
    if (require_unique && anyDuplicated(key_text)) trace_abort(sprintf("%s 的模板键不能唯一标识记录，共有 %d 个重复键。", dataset, sum(duplicated(key_text))))
    target <- file.path(studio_project_path(project_id), "raw", "normalized", source$file)
    write_csv(normalized, target)
    item$bindings <- as.list(mapping)
    item$normalized_file <- studio_relative_path(target, studio_project_path(project_id))
    item$normalized_sha256 <- file_sha256(target)
    item$key_fields <- as.list(key_fields)
    item$key_missing_rows <- 0L
    item$key_duplicate_rows <- sum(duplicated(key_text))
    item$key_uniqueness_required <- require_unique
    item$confirmed_at <- utc_now()
    item$status <- "confirmed"
    bindings$datasets[[dataset]] <- item
    studio_write_bindings(project_id, bindings)
    all_sources <- names(studio_non_derived_sources(project_id))
    confirmed <- vapply(all_sources, function(id) identical(bindings$datasets[[id]]$status %||% "", "confirmed"), logical(1))
    project <- studio_read_project(project_id)
    project$stages$data_sources <- if (all(confirmed)) "ready" else "binding_required"
    studio_write_project(project_id, project)
    studio_mark_active_run_stale(project_id, sprintf("来源 %s 的字段绑定已更新", dataset), actor)
    studio_append_audit(project_id, "field_bindings_confirmed", list(
      dataset = dataset, logical_fields = as.list(names(mapping)),
      normalized_sha256 = item$normalized_sha256
    ), actor = actor)
  })
  invisible(TRUE)
}

studio_policy_file <- function(project_id) file.path(studio_project_path(project_id), "config", "mapping_policies.yml")

studio_read_policy <- function(project_id) yaml::read_yaml(studio_policy_file(project_id))

studio_policy_options <- function(project_id) {
  path <- studio_project_path(project_id)
  policy <- studio_read_policy(project_id)
  terminology <- yaml::read_yaml(file.path(path, "config", "controlled_terminology.yml"))
  units <- yaml::read_yaml(file.path(path, "config", "unit_conversions.yml"))
  metadata <- yaml::read_yaml(file.path(path, "config", "metadata.yml"))
  sequence_fields <- stats::setNames(lapply(c("AE", "VS"), function(domain) {
    unique(c(names(metadata$domains[[domain]]$variables %||% list()), ".SOURCE_ROW"))
  }), c("AE", "VS"))
  unit_targets <- list()
  for (set_id in names(units$sets %||% list())) {
    rows <- units$sets[[set_id]]
    unit_targets[[set_id]] <- split(
      vapply(rows, function(item) as.character(item$to_unit), character(1)),
      vapply(rows, function(item) as.character(item$test_code), character(1))
    )
    unit_targets[[set_id]] <- lapply(unit_targets[[set_id]], unique)
  }
  list(
    date_formats = c("m/d/y", "d/m/y", "dd-mmm-yyyy", "y-m-d", "H:M", "H:M:S"),
    subject_separators = c("-", "_", "/", ""),
    site_delimiters = c("-", "_", "/"),
    reference_selections = c("earliest_complete_datetime", "latest_complete_datetime"),
    codelists = terminology$codelists, visit_maps = terminology$visit_maps,
    unit_sets = names(units$sets), unit_targets = unit_targets,
    sequence_fields = sequence_fields, baseline_references = c("RFSTDTC", "RFXSTDTC"),
    current = policy
  )
}

studio_save_policy <- function(project_id, settings, actor = "local_user") {
  studio_with_project_lock(project_id, {
    policy <- studio_read_policy(project_id)
    options <- studio_policy_options(project_id)
    project <- studio_read_project(project_id)
    study_id <- trimws(as.character(settings$study_id %||% project$study_id))
    if (!nzchar(study_id) || nchar(study_id) > 40L) trace_abort("研究编号必须为1至40个字符。")
    studio_assert_confirmed_study_ids(project_id, study_id)
    separator <- as.character(settings$subject_separator %||%
      policy$identifiers$usubjid$separator %||% policy$identifiers$subject_identifier$separator %||% "-")
    if (!separator %in% options$subject_separators) trace_abort("受试者标识符分隔符不在允许列表中。")
    if (!is.null(policy$identifiers$usubjid)) policy$identifiers$usubjid$separator <- separator
    if (!is.null(policy$identifiers$subject_identifier)) policy$identifiers$subject_identifier$separator <- separator

    if (!is.null(settings$site_delimiter) && !is.null(policy$identifiers$site_identifier)) {
      if (!settings$site_delimiter %in% options$site_delimiters) trace_abort("中心编号分隔符不在允许列表中。")
      position <- suppressWarnings(as.integer(settings$site_position %||% 1L))
      if (is.na(position) || position < 1L || position > 10L) trace_abort("中心编号位置必须是1至10的整数。")
      policy$identifiers$site_identifier$delimiter <- as.character(settings$site_delimiter)
      policy$identifiers$site_identifier$part_position <- position
    }
    if (!is.null(settings$reference_selection) && !is.null(policy$reference_datetime_rules)) {
      if (!settings$reference_selection %in% options$reference_selections) trace_abort("参考日期选择规则无效。")
      policy$reference_datetime_rules$selection <- as.character(settings$reference_selection)
    }
    if (!is.null(settings$date_formats)) {
      requested <- settings$date_formats
      for (index in seq_along(policy$date_time_formats$approved_sources %||% list())) {
        entry <- policy$date_time_formats$approved_sources[[index]]
        key <- paste(entry$source$dataset, entry$source$variable, sep = "__")
        value <- unlist(requested[[key]] %||% entry$formats, use.names = FALSE)
        if (!length(value) || any(!value %in% options$date_formats)) trace_abort(sprintf("%s 的日期时间格式无效。", key))
        policy$date_time_formats$approved_sources[[index]]$formats <- as.list(value)
      }
    }
    if (!is.null(settings$baseline_visit) && length(policy$baseline_rules)) {
      known_visits <- unique(unlist(options$visit_maps, recursive = TRUE, use.names = TRUE))
      visit_names <- unique(unlist(lapply(options$visit_maps, names), use.names = FALSE))
      if (nzchar(settings$baseline_visit) && !settings$baseline_visit %in% visit_names) trace_abort("基线访视不在已登记访视表中。")
      policy$baseline_rules$eligible_visits <- if (nzchar(settings$baseline_visit)) list(settings$baseline_visit) else list()
    }
    if (!is.null(settings$baseline_reference) && length(policy$baseline_rules)) {
      if (!settings$baseline_reference %in% options$baseline_references) trace_abort("基线参考变量不在允许列表中。")
      policy$baseline_rules$reference_field <- as.character(settings$baseline_reference)
    }
    if (!is.null(settings$sequence_rules)) {
      for (domain in intersect(names(settings$sequence_rules), names(policy$sequence_rules %||% list()))) {
        fields <- unlist(settings$sequence_rules[[domain]], use.names = FALSE)
        if (!length(fields) || any(!fields %in% options$sequence_fields[[domain]])) trace_abort(sprintf("%s序号排序字段无效。", domain))
        policy$sequence_rules[[domain]]$record_variables <- as.list(fields)
      }
    }
    if (!is.null(settings$unit_set) && length(policy$unit_standardization)) {
      if (nzchar(settings$unit_set) && !settings$unit_set %in% options$unit_sets) trace_abort("单位换算集合未登记。")
      if (nzchar(settings$unit_set)) policy$unit_standardization$conversion_set_id <- settings$unit_set
    }
    if (!is.null(settings$unit_targets) && length(policy$unit_standardization$findings %||% list())) {
      set_id <- policy$unit_standardization$conversion_set_id %||% ""
      allowed_targets <- options$unit_targets[[set_id]] %||% list()
      for (index in seq_along(policy$unit_standardization$findings)) {
        test_code <- as.character(policy$unit_standardization$findings[[index]]$test_code)
        requested <- as.character(settings$unit_targets[[test_code]] %||% policy$unit_standardization$findings[[index]]$target_unit)
        if (!requested %in% unlist(allowed_targets[[test_code]] %||% character(), use.names = FALSE)) {
          trace_abort(sprintf("%s的目标单位未在换算集合中登记。", test_code))
        }
        policy$unit_standardization$findings[[index]]$target_unit <- requested
      }
    }
    project$study_id <- study_id
    write_yaml(policy, studio_policy_file(project_id))
    project$stages$policy <- "configured"
    studio_write_project(project_id, project)
    studio_mark_active_run_stale(project_id, "项目政策已更新", actor)
    studio_append_audit(project_id, "policy_saved", list(
      policy_sha256 = file_sha256(studio_policy_file(project_id)),
      study_id = study_id, subject_separator = separator
    ), actor = actor)
  })
  invisible(TRUE)
}

studio_add_terminology_mapping <- function(project_id, codelist_id, source_value, target_value,
                                           actor = "local_user") {
  studio_with_project_lock(project_id, {
    path <- file.path(studio_project_path(project_id), "config", "controlled_terminology.yml")
    terminology <- yaml::read_yaml(path)
    codelist <- terminology$codelists[[codelist_id]]
    if (is.null(codelist)) trace_abort("受控术语表编号未登记。")
    source_value <- trimws(as.character(source_value %||% ""))
    target_value <- as.character(target_value %||% "")
    if (!nzchar(source_value) || nchar(source_value) > 200L) trace_abort("来源术语必须为1至200个字符。")
    allowed <- unique(as.character(unname(unlist(codelist, use.names = FALSE))))
    if (!target_value %in% allowed) trace_abort("目标术语不在当前术语表的允许标准值中。")
    terminology$codelists[[codelist_id]][[source_value]] <- target_value
    write_yaml(terminology, path)
    studio_mark_active_run_stale(project_id, sprintf("受控术语表 %s 已更新", codelist_id), actor)
    studio_append_audit(project_id, "terminology_mapping_saved", list(
      codelist_id = codelist_id, source_value_sha256 = digest::digest(source_value, algo = "sha256", serialize = FALSE),
      target_value = target_value
    ), actor = actor)
  })
  invisible(TRUE)
}

studio_add_visit_mapping <- function(project_id, visit_map_id, visit_name, visit_number,
                                     actor = "local_user") {
  studio_with_project_lock(project_id, {
    path <- file.path(studio_project_path(project_id), "config", "controlled_terminology.yml")
    terminology <- yaml::read_yaml(path)
    if (is.null(terminology$visit_maps[[visit_map_id]])) trace_abort("访视表编号未登记。")
    visit_name <- trimws(as.character(visit_name %||% ""))
    visit_number <- suppressWarnings(as.numeric(visit_number))
    if (!nzchar(visit_name) || nchar(visit_name) > 100L) trace_abort("访视名称必须为1至100个字符。")
    if (is.na(visit_number) || !is.finite(visit_number)) trace_abort("访视编号必须是有限数值。")
    terminology$visit_maps[[visit_map_id]][[visit_name]] <- visit_number
    write_yaml(terminology, path)
    studio_mark_active_run_stale(project_id, sprintf("访视表 %s 已更新", visit_map_id), actor)
    studio_append_audit(project_id, "visit_mapping_saved", list(
      visit_map_id = visit_map_id, visit_name = visit_name, visit_number = visit_number
    ), actor = actor)
  })
  invisible(TRUE)
}

studio_assert_sources_ready <- function(project_id) {
  sources <- studio_read_source_catalog(project_id)
  if (!length(sources)) trace_abort("请先上传至少一个通用CSV来源文件。")
  root <- file.path(studio_project_path(project_id), "raw", "normalized")
  missing <- names(sources)[!vapply(sources, function(item) file.exists(file.path(root, item$file)), logical(1))]
  if (length(missing)) trace_abort(sprintf("以下规范化来源文件不存在：%s。", paste(missing, collapse = "、")))
  invisible(TRUE)
}

studio_run_path <- function(project_id, run_id, must_exist = TRUE) {
  run_id <- studio_validate_run_id(run_id)
  project_path <- studio_project_path(project_id)
  path <- file.path(project_path, "runs", run_id)
  if (!studio_path_within(path, project_path)) trace_abort("运行路径超出项目目录。")
  if (isTRUE(must_exist) && !file.exists(file.path(path, "run.yml"))) trace_abort(sprintf("运行不存在：%s。", run_id))
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

studio_read_run <- function(project_id, run_id) {
  value <- yaml::read_yaml(file.path(studio_run_path(project_id, run_id), "run.yml"))
  if (!identical(as.character(value$schema_version %||% ""), "0.6")) {
    trace_abort("该运行使用不兼容的旧版工作台结构，不能在 TraceSDTM 0.7 中继续。")
  }
  for (stage in c("build_adam", "validate_adam", "build_tlf", "validate_tlf")) {
    if (is.null(value$stages[[stage]])) value$stages[[stage]] <- "not_configured"
  }
  value
}

studio_assert_run_writable <- function(config) {
  if (is.null(config$studio$project_id) || is.null(config$studio$run_id)) return(invisible(TRUE))
  project <- studio_read_project(config$studio$project_id)
  run <- studio_read_run(config$studio$project_id, config$studio$run_id)
  if (isTRUE(project$archived)) trace_abort("归档项目为只读状态。")
  if (!identical(as.character(project$active_run_id %||% ""), as.character(config$studio$run_id))) {
    trace_abort("历史运行为只读状态；请切换到当前运行。")
  }
  if (isTRUE(run$stale) || isTRUE(project$active_run_stale)) {
    trace_abort("当前运行已过期；请从最新数据和政策创建新运行。")
  }
  invisible(TRUE)
}

studio_assert_mapping_editable_v06 <- function(config) {
  studio_assert_run_writable(config)
  approved <- trace_path(config$paths$approved_specification %||% "")
  if (nzchar(as.character(config$paths$approved_specification %||% "")) && file.exists(approved)) {
    trace_abort("当前运行的映射已经最终批准并冻结；如需修改，请从项目数据创建新运行。")
  }
  invisible(TRUE)
}

studio_write_run <- function(project_id, run_id, value) {
  value$updated_at <- utc_now()
  write_yaml(value, file.path(studio_run_path(project_id, run_id), "run.yml"))
}

studio_next_run_id <- function() {
  paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", Sys.getpid(), "_", sprintf("%03d", sample.int(999L, 1L)))
}

studio_create_run <- function(project_id, actor = "local_user") {
  run_id <- NULL
  studio_with_project_lock(project_id, {
    studio_assert_sources_ready(project_id)
    project <- studio_read_project(project_id)
    if (isTRUE(project$archived)) trace_abort("归档项目不能创建新运行。")
    run_id <- studio_next_run_id()
    run_path <- studio_run_path(project_id, run_id, must_exist = FALSE)
    for (relative in c(
      "inputs", "config", "profile", "tasks", "recommendations", "review", "specs", "sdtm/csv", "sdtm/xpt",
      "adam/csv", "adam/xpt", "tlf/csv", "tlf/html", "tlf/rtf", "lineage",
      "validation/local", "validation/adam", "validation/tlf", "validation/p21", "report", "manifests", "logs"
    )) ensure_dir(file.path(run_path, relative))
    sources <- studio_non_derived_sources(project_id)
    input_manifest <- list()
    for (dataset in names(sources)) {
      source <- file.path(studio_project_path(project_id), "raw", "normalized", sources[[dataset]]$file)
      target <- file.path(run_path, "inputs", sources[[dataset]]$file)
      if (!file.exists(source) || !isTRUE(file.copy(source, target, overwrite = FALSE, copy.date = TRUE))) {
        trace_abort(sprintf("无法冻结规范化来源：%s。", dataset))
      }
      input_manifest[[dataset]] <- list(file = file.path("inputs", sources[[dataset]]$file), sha256 = file_sha256(target))
    }
    config_names <- c("metadata.yml", "transform_registry.yml", "transform_registry.schema.json",
                      "controlled_terminology.yml", "unit_conversions.yml", "mapping_policies.yml",
                      "source_catalog.yml")
    project_analysis <- file.path(studio_project_path(project_id), "config", "analysis_plan.yml")
    if (file.exists(project_analysis)) config_names <- c(config_names, "analysis_plan.yml", "analysis_plan.schema.json")
    config_manifest <- list()
    for (name in config_names) {
      source <- file.path(studio_project_path(project_id), "config", name)
      target <- file.path(run_path, "config", name)
      if (!isTRUE(file.copy(source, target, overwrite = FALSE, copy.date = TRUE))) trace_abort(sprintf("无法冻结运行配置：%s。", name))
      config_manifest[[name]] <- file_sha256(target)
    }
    task_specification <- list(
      schema_version = "0.6",
      specification = list(
        name = paste0(project$name, " 通用原子任务"), version = "0.7.0",
        status = "profile_pending", standard = paste(project$standard, project$standard_version),
        project_id = project_id, run_id = run_id
      ),
      source_catalog = sources, domain_sources = list(), tasks = list()
    )
    tasks_path <- file.path(run_path, "config", "tasks.yml")
    write_yaml(task_specification, tasks_path)
    config_manifest[["tasks.yml"]] <- file_sha256(tasks_path)
    manifest <- list(
      schema_version = "0.6", run_id = run_id, project_id = project_id,
      study_id = project$study_id, project_description = project$description,
      standard = project$standard, standard_version = project$standard_version,
      target_domains = project$target_domains,
      created_at = utc_now(), updated_at = utc_now(), stale = FALSE,
      status = "created", current_stage = "created",
      stages = list(
        data_freeze = "completed", profile = "pending", task_discovery = "pending",
        task_confirmation = "pending", recommend_targets = "pending",
        recommend_functions = "pending", recommend_parameters = "pending", assemble = "pending",
        ai_review = "pending", human_approval = "pending", build = "pending",
        validate_local = "pending", validate_p21 = "pending",
        build_adam = if (file.exists(project_analysis)) "pending" else "not_configured",
        validate_adam = if (file.exists(project_analysis)) "pending" else "not_configured",
        build_tlf = if (file.exists(project_analysis)) "pending" else "not_configured",
        validate_tlf = if (file.exists(project_analysis)) "pending" else "not_configured",
        report = "pending"
      ),
      inputs = input_manifest, configuration_sha256 = config_manifest,
      analysis_plan_sha256 = if (file.exists(project_analysis)) file_sha256(project_analysis) else NULL,
      studio_version = "0.7.0"
    )
    write_yaml(manifest, file.path(run_path, "run.yml"))
    project$active_run_id <- run_id
    project$active_run_stale <- FALSE
    project$stages$profile <- "ready"
    studio_write_project(project_id, project)
    studio_append_audit(project_id, "run_created", list(
      input_count = length(input_manifest),
      configuration_count = length(config_manifest)
    ), run_id, actor)
  })
  run_id
}

studio_update_run_stage <- function(project_id, run_id, stage, status, message = NULL,
                                    actor = "system") {
  allowed_stages <- c(
    "data_freeze", "profile", "task_discovery", "task_confirmation",
    "recommend_targets", "recommend_functions", "recommend_parameters", "assemble",
    "ai_review", "human_approval", "build", "validate_local", "validate_p21",
    "build_adam", "validate_adam", "build_tlf", "validate_tlf", "report"
  )
  allowed_status <- c("pending", "not_configured", "running", "completed", "completed_with_warnings", "needs_information", "failed", "blocked", "cancelled", "interrupted")
  if (!stage %in% allowed_stages || !status %in% allowed_status) trace_abort("运行阶段或状态无效。")
  run <- studio_read_run(project_id, run_id)
  run$stages[[stage]] <- status
  run$current_stage <- stage
  terminal_failure <- status %in% c("failed", "cancelled", "interrupted")
  completed_values <- c("completed", "completed_with_warnings")
  pipeline_stages <- c("data_freeze", "profile", "task_discovery", "task_confirmation",
                       "recommend_targets", "recommend_functions", "recommend_parameters", "assemble",
                       "ai_review", "human_approval", "build")
  all_complete <- all(vapply(run$stages[pipeline_stages], function(value) as.character(value) %in% completed_values, logical(1)))
  run$status <- if (terminal_failure) status else if (all_complete) "completed" else "active"
  run$last_message <- as.character(message %||% "")
  studio_write_run(project_id, run_id, run)
  studio_append_audit(project_id, "run_stage_changed", list(stage = stage, status = status), run_id, actor)
  invisible(run)
}

studio_list_runs <- function(project_id) {
  root <- file.path(studio_project_path(project_id), "runs")
  directories <- list.dirs(root, recursive = FALSE, full.names = TRUE)
  values <- lapply(directories, function(path) {
    manifest <- file.path(path, "run.yml")
    if (!file.exists(manifest)) return(NULL)
    value <- tryCatch(yaml::read_yaml(manifest), error = function(error) NULL)
    if (is.null(value)) return(NULL)
    data.frame(
      run_id = as.character(value$run_id), status = as.character(value$status),
      current_stage = as.character(value$current_stage), stale = isTRUE(value$stale),
      created_at = as.character(value$created_at), updated_at = as.character(value$updated_at), stringsAsFactors = FALSE
    )
  })
  result <- dplyr::bind_rows(Filter(Negate(is.null), values))
  if (!nrow(result)) return(tibble::tibble(run_id = character(), status = character(), current_stage = character(), stale = logical(), created_at = character(), updated_at = character()))
  dplyr::arrange(result, dplyr::desc(.data$created_at))
}

studio_load_run_config <- function(project_id, run_id) {
  project <- studio_read_project(project_id)
  run_path <- studio_run_path(project_id, run_id)
  config <- yaml::read_yaml(trace_path("config", "project.yml"))
  config$project$name <- project$name
  config$project$version <- "0.7.0"
  config$project$study_id <- project$study_id
  config$project$description <- project$description
  config$project$standard <- project$standard
  config$project$standard_version <- project$standard_version
  config$project$target_domains <- project$target_domains
  config$project$scenario <- "generic"
  config$project$studio <- TRUE
  config$project$project_id <- project_id
  config$project$run_id <- run_id
  config$paths <- list(
    raw_dir = file.path(run_path, "inputs"),
    task_specification = file.path(run_path, "config", "tasks.yml"),
    mapping_policies = file.path(run_path, "config", "mapping_policies.yml"),
    metadata = file.path(run_path, "config", "metadata.yml"),
    controlled_terminology = file.path(run_path, "config", "controlled_terminology.yml"),
    unit_conversions = file.path(run_path, "config", "unit_conversions.yml"),
    transform_registry = file.path(run_path, "config", "transform_registry.yml"),
    transform_registry_schema = file.path(run_path, "config", "transform_registry.schema.json"),
    generated_transform_docs = file.path(run_path, "report", "transform_catalog.md")
  )
  config <- configure_output_paths(config, run_path)
  analysis_path <- file.path(run_path, "config", "analysis_plan.yml")
  schema_path <- file.path(run_path, "config", "analysis_plan.schema.json")
  config$paths$analysis_plan_frozen <- if (file.exists(analysis_path)) analysis_path else ""
  config$paths$analysis_plan_schema_frozen <- if (file.exists(schema_path)) schema_path else ""
  config$paths$task_dir <- file.path(run_path, "tasks")
  config$paths$project_context <- file.path(run_path, "profile", "project_context.json")
  task_specification <- yaml::read_yaml(config$paths$task_specification)
  task_domains <- unique(vapply(task_specification$tasks %||% list(), function(task) as.character(task$target_domain %||% ""), character(1)))
  task_domains <- task_domains[nzchar(task_domains)]
  if (!length(task_domains)) task_domains <- unlist(project$target_domains %||% character(), use.names = FALSE)
  if (!length(task_domains)) task_domains <- names(yaml::read_yaml(config$paths$metadata)$domains)
  config$project$generated_domains <- as.list(task_domains)
  config$studio <- list(
    project_id = project_id, run_id = run_id,
    project_path = studio_project_path(project_id), run_path = run_path
  )
  config
}

studio_resolve_cli_config <- function(args) {
  project_id <- argument_value(args, "--project", default = NULL)
  run_id <- argument_value(args, "--run", default = NULL)
  if (is.null(project_id) && is.null(run_id)) return(load_project_config())
  if (is.null(project_id) || is.null(run_id)) trace_abort("工作台命令必须同时提供 --project 和 --run。")
  studio_load_run_config(project_id, run_id)
}

studio_project_summary <- function(project_id) {
  project <- studio_read_project(project_id)
  sources <- studio_read_source_catalog(project_id)
  source_status <- stats::setNames(rep("ready", length(sources)), names(sources))
  list(project = project, source_status = as.list(source_status), runs = studio_list_runs(project_id))
}
