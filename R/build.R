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
