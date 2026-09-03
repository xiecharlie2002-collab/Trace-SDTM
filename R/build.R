mapping_lineage_row <- function(domain, source_file, mapping, records_created, approval) {
  tibble::tibble(
    mapping_id = as.character(mapping$mapping_id),
    target_domain = domain,
    target_variable = as.character(mapping$target_variable %||% ""),
    target_value = as.character(mapping$target_value %||% ""),
    source_dataset = tools::file_path_sans_ext(source_file),
    source_variables = paste(mapping_sources(mapping), collapse = " | "),
    transform_id = as.character(mapping$transform_id),
    transform_parameters = as_json_text(mapping$parameters %||% list()),
    records_created = as.integer(records_created),
    reviewer = as.character(approval$reviewer %||% ""),
    approved_at = as.character(approval$approved_at %||% "")
  )
}

apply_tabular_mappings <- function(raw, domain, domain_spec, approval) {
  raw$.SOURCE_ROW <- seq_len(nrow(raw))
  target <- tibble::tibble(.SOURCE_ROW = raw$.SOURCE_ROW)
  lineage <- list()
  sequence_mapping <- NULL
  deferred_mappings <- list()

  for (mapping in domain_spec$mappings) {
    if (identical(mapping$transform_id, "derive_sequence")) {
      sequence_mapping <- mapping
      next
    }
    if (identical(mapping$transform_id, "derive_study_day")) {
      deferred_mappings[[length(deferred_mappings) + 1L]] <- mapping
      next
    }
    target[[mapping$target_variable]] <- execute_mapping(raw, mapping)
    lineage[[length(lineage) + 1L]] <- mapping_lineage_row(
      domain, domain_spec$source_file, mapping, nrow(target), approval
    )
  }

  if (!is.null(sequence_mapping)) {
    target <- derive_sequence(target, sequence_mapping)
    lineage[[length(lineage) + 1L]] <- mapping_lineage_row(
      domain, domain_spec$source_file, sequence_mapping, nrow(target), approval
    )
  }
  list(data = target, lineage = dplyr::bind_rows(lineage), deferred_mappings = deferred_mappings)
}

apply_vs_mappings <- function(raw, domain_spec, approval) {
  raw$.SOURCE_ROW <- seq_len(nrow(raw))
  base <- tibble::tibble(.SOURCE_ROW = raw$.SOURCE_ROW)
  lineage <- list()
  sequence_mapping <- NULL
  deferred_mappings <- list()

  for (mapping in domain_spec$mappings) {
    if (identical(mapping$transform_id, "derive_sequence")) {
      sequence_mapping <- mapping
      next
    }
    if (identical(mapping$transform_id, "derive_study_day")) {
      deferred_mappings[[length(deferred_mappings) + 1L]] <- mapping
      next
    }
    base[[mapping$target_variable]] <- execute_mapping(raw, mapping)
    lineage[[length(lineage) + 1L]] <- mapping_lineage_row(
      "VS", domain_spec$source_file, mapping, nrow(base), approval
    )
  }

  blocks <- purrr::map(domain_spec$transpose_mappings, function(mapping) {
    source <- mapping_sources(mapping)
    if (length(source) != 1L || !source %in% names(raw)) {
      trace_abort(sprintf("%s 的纵向转换来源无效。", mapping$mapping_id))
    }
    observed <- !is.na(raw[[source]]) & nzchar(trimws(as.character(raw[[source]])))
    block <- base[observed, , drop = FALSE]
    value <- as.character(raw[[source]][observed])
    block$VSTESTCD <- as.character(mapping$target_value)
    block$VSTEST <- as.character(mapping$target_test)
    block$VSORRES <- value
    block$VSORRESU <- as.character(mapping$unit)
    block$VSSTRESC <- value
    block$VSSTRESN <- suppressWarnings(as.numeric(value))
    block$VSSTRESU <- as.character(mapping$unit)
    block$.MAPPING_ID <- as.character(mapping$mapping_id)
    lineage[[length(lineage) + 1L]] <<- mapping_lineage_row(
      "VS", domain_spec$source_file, mapping, nrow(block), approval
    )
    block
  })
  target <- dplyr::bind_rows(blocks)
  if (!is.null(sequence_mapping)) {
    target <- derive_sequence(target, sequence_mapping)
    lineage[[length(lineage) + 1L]] <- mapping_lineage_row(
      "VS", domain_spec$source_file, sequence_mapping, nrow(target), approval
    )
  }
  list(data = target, lineage = dplyr::bind_rows(lineage), deferred_mappings = deferred_mappings)
}

apply_deferred_study_days <- function(result, dm, domain, domain_spec, approval) {
  deferred <- result$deferred_mappings %||% list()
  if (!length(deferred)) return(result)
  for (mapping in deferred) {
    target_date <- mapping$parameters$target_date
    original_target_date <- as.character(result$data[[target_date]])
    result$data <- sdtm.oak::derive_study_day(
      sdtm_in = result$data,
      dm_domain = dm,
      tgdt = target_date,
      refdt = mapping$parameters$reference_date,
      study_day_var = mapping$target_variable,
      merge_key = "USUBJID"
    )
    result$data[[target_date]] <- original_target_date
    result$lineage <- dplyr::bind_rows(
      result$lineage,
      mapping_lineage_row(domain, domain_spec$source_file, mapping, nrow(result$data), approval)
    )
  }
  result
}

apply_sdtm_labels <- function(data, domain, metadata) {
  domain_metadata <- metadata$domains[[domain]]
  for (variable in intersect(names(data), names(domain_metadata$variables))) {
    attr(data[[variable]], "label") <- domain_metadata$variables[[variable]]$label
  }
  attr(data, "label") <- domain_metadata$label
  data
}

finalize_domain <- function(data, domain, metadata) {
  transient <- intersect(c(".SOURCE_ROW", ".MAPPING_ID"), names(data))
  if (length(transient)) data <- dplyr::select(data, -dplyr::all_of(transient))
  expected <- unlist(metadata$domains[[domain]]$expected_order, use.names = FALSE)
  missing <- setdiff(expected, names(data))
  if (length(missing)) trace_abort(sprintf("%s 构建后缺少预期字段：%s", domain, paste(missing, collapse = ", ")))
  definitions <- metadata$domains[[domain]]$variables
  for (variable in intersect(names(data), names(definitions))) {
    if (identical(definitions[[variable]]$type, "numeric") && !is.numeric(data[[variable]])) {
      original <- data[[variable]]
      converted <- suppressWarnings(as.numeric(original))
      invalid <- !is.na(original) & nzchar(trimws(as.character(original))) & is.na(converted)
      if (any(invalid)) trace_abort(sprintf("%s.%s 包含不能转换为数值的结果。", domain, variable))
      data[[variable]] <- converted
    }
    if (identical(definitions[[variable]]$type, "character") && !is.character(data[[variable]])) {
      data[[variable]] <- as.character(data[[variable]])
    }
  }
  data <- dplyr::select(data, dplyr::all_of(expected))
  apply_sdtm_labels(data, domain, metadata)
}

write_domain_outputs <- function(data, domain, config, metadata) {
  csv_path <- trace_path(config$paths$csv_dir, paste0(tolower(domain), ".csv"))
  xpt_path <- trace_path(config$paths$xpt_dir, paste0(tolower(domain), ".xpt"))
  write_csv(data, csv_path)
  ensure_parent(xpt_path)
  haven::write_xpt(
    data,
    xpt_path,
    version = 5,
    name = tolower(domain),
    label = metadata$domains[[domain]]$label
  )
  tibble::tibble(
    domain = domain,
    records = nrow(data),
    variables = ncol(data),
    data_sha256 = data_sha256(data),
    csv_sha256 = file_sha256(csv_path),
    xpt_sha256 = file_sha256(xpt_path),
    csv_path = csv_path,
    xpt_path = xpt_path
  )
}

build_sdtm <- function(config = load_project_config()) {
  ensure_output_directories(config)
  specification <- load_approved_mapping(config)
  metadata <- load_metadata(config)
  approval <- specification$specification$approval
  datasets <- list()
  lineage <- list()
  manifest <- list()

  for (domain in unlist(config$project$generated_domains, use.names = FALSE)) {
    domain_spec <- specification$domains[[domain]]
    raw_path <- trace_path(config$paths$raw_dir, domain_spec$source_file)
    raw <- read_raw_csv(raw_path)
    result <- if (domain == "VS") {
      apply_vs_mappings(raw, domain_spec, approval)
    } else {
      apply_tabular_mappings(raw, domain, domain_spec, approval)
    }
    if (domain != "DM") {
      result <- apply_deferred_study_days(result, datasets$DM, domain, domain_spec, approval)
    }
    data <- finalize_domain(result$data, domain, metadata)
    datasets[[domain]] <- data
    lineage[[domain]] <- result$lineage
    manifest[[domain]] <- write_domain_outputs(data, domain, config, metadata)
  }

  lineage_data <- dplyr::bind_rows(lineage) |>
    dplyr::arrange(target_domain, mapping_id)
  write_csv(lineage_data, trace_path(config$paths$lineage_dir, "field_lineage.csv"))

  manifest_data <- dplyr::bind_rows(manifest)
  write_csv(manifest_data, trace_path(config$paths$manifest_dir, "dataset_manifest.csv"))
  write_json(list(
    run_id = new_run_id("build"),
    generated_at = utc_now(),
    standard = config$project$standard,
    standard_version = config$project$standard_version,
    specification_sha256 = file_sha256(trace_path(config$paths$approved_specification)),
    sdtm_oak_version = as.character(utils::packageVersion("sdtm.oak")),
    datasets = as.data.frame(manifest_data)
  ), trace_path(config$paths$manifest_dir, "build_manifest.json"))
  trace_info("已生成 SDTM：%s。", paste(sprintf("%s=%s 行", manifest_data$domain, manifest_data$records), collapse = "，"))
  invisible(datasets)
}

load_built_datasets <- function(config = load_project_config()) {
  domains <- unlist(config$project$generated_domains, use.names = FALSE)
  paths <- setNames(lapply(domains, function(domain) trace_path(config$paths$xpt_dir, paste0(tolower(domain), ".xpt"))), domains)
  missing <- names(paths)[!vapply(paths, file.exists, logical(1))]
  if (length(missing)) trace_abort(sprintf("缺少已构建数据集：%s。请先执行 build。", paste(missing, collapse = ", ")))
  lapply(paths, function(path) tibble::as_tibble(haven::read_xpt(path)))
}

# -----------------------------------------------------------------------------
# 0.2：按批准的临床概念函数链执行，不解析模型文本。

load_registered_sources_v02 <- function(specification, config) {
  sources <- list()
  for (dataset in names(specification$source_catalog)) {
    entry <- specification$source_catalog[[dataset]]
    if (isTRUE(entry$derived)) next
    path <- trace_path(config$paths$raw_dir, entry$file)
    if (!file.exists(path)) trace_abort(sprintf("来源数据文件不存在：%s", path))
    # 构建阶段按字符读取，以保留临床原始值的精确字面形式；类型解释由注册函数负责。
    data <- readr::read_csv(
      path, na = c("", "NA", "N/A"), show_col_types = FALSE, progress = FALSE,
      name_repair = "minimal", col_types = readr::cols(.default = readr::col_character())
    )
    data$.SOURCE_ROW <- seq_len(nrow(data))
    sources[[dataset]] <- data
  }
  sources
}

lineage_step_v02 <- function(concept, step, entry, records_created, specification, approval, config) {
  refs <- step_source_refs(concept, step)
  source_datasets <- unique(vapply(refs, function(ref) as.character(ref$dataset), character(1)))
  source_fields <- vapply(refs, source_ref_key, character(1))
  parameters <- step$parameters %||% list()
  join_rule <- if (identical(step$transform_id, "merge_sources")) as.character(registry_json(parameters[c("left_dataset", "right_dataset", "by", "relationship", "join_type")])) else ""
  aggregation_rule <- if (identical(step$transform_id, "derive_reference_datetime")) as.character(registry_json(parameters[c("selection", "subject_keys", "sources")])) else ""
  unit_config <- load_unit_conversions(config)
  tibble::tibble(
    task_id = as.character(step$.task_id %||% concept$concept_id),
    assembly_group_id = as.character(concept$assembly_group_id %||% concept$concept_id),
    concept_id = concept$concept_id,
    target_domain = concept$target_domain,
    target_variables = paste(step_target_variables(step), collapse = " | "),
    source_datasets = paste(source_datasets, collapse = " | "),
    source_fields = paste(source_fields, collapse = " | "),
    join_rule = join_rule,
    aggregation_rule = aggregation_rule,
    transform_id = step$transform_id,
    transform_parameters = as.character(registry_json(parameters)),
    implementation = paste0(entry$provider$name, "::", entry$provider[["function"]]),
    sdtm_oak_version = if (identical(entry$provider$name, "sdtm.oak")) as.character(utils::packageVersion("sdtm.oak")) else "",
    unit_conversion_version = if (identical(step$transform_id, "standardize_unit")) as.character(unit_config$version) else "",
    reviewer = as.character(concept$review$reviewer %||% approval$reviewer %||% ""),
    reviewed_at = as.character(concept$review$reviewed_at %||% approval$approved_at %||% ""),
    records_created = as.integer(records_created)
  )
}

domain_concepts_v02 <- function(specification, domain) {
  Filter(function(concept) identical(concept$target_domain, domain), specification$concepts)
}

check_concept_dependencies_v02 <- function(specification) {
  positions <- stats::setNames(seq_along(specification$concepts), vapply(specification$concepts, `[[`, character(1), "concept_id"))
  for (concept in specification$concepts) {
    dependencies <- unlist(concept$depends_on %||% character(), use.names = FALSE)
    unknown <- setdiff(dependencies, names(positions))
    if (length(unknown)) trace_abort(sprintf("%s 依赖未批准或未知概念：%s", concept$concept_id, paste(unknown, collapse = ", ")))
    if (length(dependencies) && any(positions[dependencies] >= positions[[concept$concept_id]])) {
      trace_abort(sprintf("%s 的依赖必须在本概念之前执行。", concept$concept_id))
    }
  }
  invisible(TRUE)
}

prepare_domain_state_v02 <- function(domain, specification, sources, dm = NULL) {
  base_dataset <- specification$domain_sources[[domain]]
  base <- sources[[base_dataset]]
  if (is.null(base)) trace_abort(sprintf("%s 的基础来源数据集 %s 不存在。", domain, base_dataset))
  list(
    domain = domain,
    base_dataset = base_dataset,
    sources = sources,
    target = tibble::tibble(.SOURCE_ROW = base$.SOURCE_ROW),
    current_records = NULL,
    findings = list(),
    dm = dm
  )
}

execute_domain_v02 <- function(domain, specification, sources, dm, config, registry) {
  state <- prepare_domain_state_v02(domain, specification, sources, dm)
  lineage <- list()
  concepts <- domain_concepts_v02(specification, domain)
  approval <- specification$specification$approval
  for (concept in concepts) {
    has_transpose <- any(vapply(concept$steps, function(step) identical(step$transform_id, "transpose_findings"), logical(1)))
    if (domain == "VS" && !has_transpose && length(state$findings)) {
      state$target <- dplyr::bind_rows(state$findings)
      state$findings <- list()
    }
    state$current_records <- NULL
    for (step in concept$steps) {
      entry <- registry_entry(step$transform_id, registry)
      state <- execute_registered_step(state, concept, step, config, registry)
      records <- if (!is.null(state$current_records)) nrow(state$current_records) else nrow(state$target)
      lineage[[length(lineage) + 1L]] <- lineage_step_v02(
        concept, step, entry, records, specification, approval, config
      )
    }
    if (!is.null(state$current_records)) {
      state$findings[[length(state$findings) + 1L]] <- state$current_records
      state$current_records <- NULL
    }
    sources <- state$sources
  }
  if (domain == "VS" && length(state$findings)) state$target <- dplyr::bind_rows(state$findings)
  list(data = state$target, lineage = dplyr::bind_rows(lineage), sources = state$sources)
}

partial_date_notes_v02 <- function(datasets) {
  purrr::imap_dfr(datasets, function(data, domain) {
    variables <- grep("DTC$", names(data), value = TRUE)
    purrr::map_dfr(variables, function(variable) {
      values <- as.character(data[[variable]])
      partial <- !is.na(values) & nzchar(values) & !grepl("^\\d{4}-\\d{2}-\\d{2}(T.*)?$", values)
      if (!any(partial)) return(tibble::tibble())
      tibble::tibble(
        domain = domain, variable = variable, value = unique(values[partial]),
        explanation = "保留原始最大已知精度，未进行日期填补；依赖完整日期的派生保持缺失。"
      )
    })
  })
}

build_sdtm <- function(config = load_project_config()) {
  ensure_output_directories(config)
  approved_specification <- load_approved_mapping(config)
  validate_specification_v04(approved_specification, config, require_approved = TRUE)
  specification <- compile_specification_v04(approved_specification, config)
  check_concept_dependencies_v02(specification)
  metadata <- load_metadata(config)
  registry <- load_transform_registry(config)
  sources <- load_registered_sources_v02(specification, config)
  datasets <- list()
  lineage <- list()
  manifests <- list()
  for (domain in unlist(config$project$generated_domains, use.names = FALSE)) {
    result <- execute_domain_v02(domain, specification, sources, datasets$DM, config, registry)
    sources <- result$sources
    data <- finalize_domain(result$data, domain, metadata)
    datasets[[domain]] <- data
    lineage[[domain]] <- result$lineage
    manifests[[domain]] <- write_domain_outputs(data, domain, config, metadata)
  }
  lineage_data <- dplyr::bind_rows(lineage) |> dplyr::arrange(target_domain, concept_id)
  write_csv(lineage_data, trace_path(config$paths$lineage_dir, "field_lineage.csv"))
  partial_notes <- partial_date_notes_v02(datasets)
  write_csv(partial_notes, trace_path(config$paths$lineage_dir, "partial_date_notes.csv"))
  manifest_data <- dplyr::bind_rows(manifests)
  write_csv(manifest_data, trace_path(config$paths$manifest_dir, "dataset_manifest.csv"))
  write_json(list(
    run_id = new_run_id("build"), generated_at = utc_now(), scenario = config$project$scenario,
    standard = config$project$standard, standard_version = config$project$standard_version,
    specification_sha256 = file_sha256(trace_path(config$paths$approved_specification)),
    registry_version = registry$registry_version,
    registry_sha256 = file_sha256(trace_path(config$paths$transform_registry)),
    sdtm_oak_version = as.character(utils::packageVersion("sdtm.oak")),
    unit_conversion_version = load_unit_conversions(config)$version,
    datasets = as.data.frame(manifest_data)
  ), trace_path(config$paths$manifest_dir, "build_manifest.json"))
  trace_info("已按 0.4 原子任务批准规格生成 SDTM：%s。", paste(sprintf("%s=%s 行", manifest_data$domain, manifest_data$records), collapse = "，"))
  invisible(datasets)
}
