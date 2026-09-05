# Deterministic table construction and validation ----------------------------

format_count_percent <- function(count, denominator) {
  if (is.na(denominator) || denominator == 0L) return(sprintf("%d (NA)", count))
  sprintf("%d (%.1f%%)", count, 100 * count / denominator)
}

analysis_groups <- function(plan, adsl, treatment_variable, population_variable) {
  population <- adsl[adsl[[population_variable]] == "Y" & !is.na(adsl[[population_variable]]), , drop = FALSE]
  registered <- vapply(plan$treatments, function(item) as.character(item$name), character(1))
  unregistered <- setdiff(unique(stats::na.omit(as.character(population[[treatment_variable]]))), registered)
  if (length(unregistered)) trace_abort(sprintf("表格遇到未登记治疗组：%s。", paste(unregistered, collapse = "、")))
  denominators <- stats::setNames(
    vapply(registered, function(group) dplyr::n_distinct(population$USUBJID[population[[treatment_variable]] == group]), integer(1)),
    registered
  )
  denominators <- c(denominators, Total = dplyr::n_distinct(population$USUBJID))
  list(data = population, names = c(registered, "Total"), denominators = denominators)
}

table_headers <- function(groups) {
  stats::setNames(
    sprintf("%s (N=%d)", names(groups$denominators), as.integer(groups$denominators)),
    names(groups$denominators)
  )
}

make_table_rows <- function(parameter, statistic, values) {
  result <- tibble::tibble(Parameter = as.character(parameter), Statistic = as.character(statistic))
  for (name in names(values)) result[[name]] <- as.character(values[[name]])
  result
}

demographic_table_object <- function(adsl, plan) {
  groups <- analysis_groups(plan, adsl, "TRT01P", "ITTFL")
  headers <- table_headers(groups)
  rows <- list()
  add <- function(value) rows[[length(rows) + 1L]] <<- value
  age_statistics <- list(
    N = function(x) as.character(sum(!is.na(x))),
    Mean = function(x) if (all(is.na(x))) "NA" else sprintf("%.1f", mean(x, na.rm = TRUE)),
    `Standard deviation` = function(x) if (sum(!is.na(x)) < 2L) "NA" else sprintf("%.1f", stats::sd(x, na.rm = TRUE)),
    Median = function(x) if (all(is.na(x))) "NA" else sprintf("%.1f", stats::median(x, na.rm = TRUE)),
    Minimum = function(x) if (all(is.na(x))) "NA" else sprintf("%.1f", min(x, na.rm = TRUE)),
    Maximum = function(x) if (all(is.na(x))) "NA" else sprintf("%.1f", max(x, na.rm = TRUE))
  )
  for (statistic in names(age_statistics)) {
    values <- lapply(groups$names, function(group) {
      data <- if (identical(group, "Total")) groups$data else groups$data[groups$data$TRT01P == group, , drop = FALSE]
      age_statistics[[statistic]](as.numeric(data$AGE))
    })
    names(values) <- unname(headers)
    add(make_table_rows(if (identical(statistic, "N")) "Age (years)" else "", statistic, values))
  }
  categories <- c(SEX = "Sex", RACE = "Race", ETHNIC = "Ethnicity")
  for (variable in names(categories)) {
    values <- as.character(groups$data[[variable]])
    levels <- sort(unique(values[!is.na(values) & nzchar(values)]))
    if (any(is.na(values) | !nzchar(values))) levels <- c(levels, "Missing")
    for (index in seq_along(levels)) {
      level <- levels[[index]]
      displays <- lapply(groups$names, function(group) {
        data <- if (identical(group, "Total")) groups$data else groups$data[groups$data$TRT01P == group, , drop = FALSE]
        match_level <- if (identical(level, "Missing")) is.na(data[[variable]]) | !nzchar(as.character(data[[variable]])) else as.character(data[[variable]]) == level
        format_count_percent(sum(match_level), groups$denominators[[group]])
      })
      names(displays) <- unname(headers)
      add(make_table_rows(if (index == 1L) categories[[variable]] else "", paste0("  ", level), displays))
    }
  }
  data <- dplyr::bind_rows(rows)
  list(
    table_id = "T14.1.1", title = as.character(plan$tables[["T14.1.1"]]$title),
    population = "Intent-to-treat population", headers = headers, denominators = groups$denominators,
    data_cutoff_date = as.character(plan$data_cutoff_date),
    notes = c(
      "Percentages use the number of subjects in the column as denominator.",
      "Missing age is not imputed and is excluded from descriptive statistics.",
      "Mean, standard deviation, and percentages are displayed to one decimal place."
    ),
    source = "Program source: TraceSDTM deterministic table rule T_DEMOGRAPHICS_STANDARD",
    data = data
  )
}

teae_subject_count <- function(data, group, level, soc = NULL, pt = NULL) {
  subset <- data
  if (!identical(group, "Total")) subset <- subset[subset$TRTA == group, , drop = FALSE]
  flag <- switch(level, overall = "AOCCFL", soc = "AOCCSFL", pt = "AOCCPFL")
  subset <- subset[subset[[flag]] == "Y" & !is.na(subset[[flag]]), , drop = FALSE]
  if (!is.null(soc)) subset <- subset[as.character(subset$AEBODSYS) == soc, , drop = FALSE]
  if (!is.null(pt)) subset <- subset[as.character(subset$AEDECOD) == pt, , drop = FALSE]
  dplyr::n_distinct(subset$USUBJID)
}

teae_table_object <- function(adsl, adae, plan) {
  groups <- analysis_groups(plan, adsl, "TRT01A", "SAFFL")
  headers <- table_headers(groups)
  eligible <- adae[adae$SAFFL == "Y" & adae$TRTEMFL == "Y" & !is.na(adae$TRTEMFL), , drop = FALSE]
  rows <- list()
  add <- function(parameter, values) {
    names(values) <- unname(headers)
    rows[[length(rows) + 1L]] <<- make_table_rows(parameter, "", values)
  }
  overall <- lapply(groups$names, function(group) {
    format_count_percent(teae_subject_count(adae, group, "overall"), groups$denominators[[group]])
  })
  add("At least one treatment-emergent adverse event", overall)

  if (nrow(eligible)) {
    soc_counts <- eligible |>
      dplyr::filter(.data$AOCCSFL == "Y") |>
      dplyr::group_by(.data$AEBODSYS) |>
      dplyr::summarise(total_count = dplyr::n_distinct(.data$USUBJID), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(.data$total_count), .data$AEBODSYS)
    for (soc in as.character(soc_counts$AEBODSYS)) {
      soc_values <- lapply(groups$names, function(group) {
        format_count_percent(teae_subject_count(adae, group, "soc", soc = soc), groups$denominators[[group]])
      })
      add(soc, soc_values)
      pt_counts <- eligible |>
        dplyr::filter(.data$AEBODSYS == soc, .data$AOCCPFL == "Y") |>
        dplyr::group_by(.data$AEDECOD) |>
        dplyr::summarise(total_count = dplyr::n_distinct(.data$USUBJID), .groups = "drop") |>
        dplyr::arrange(dplyr::desc(.data$total_count), .data$AEDECOD)
      for (pt in as.character(pt_counts$AEDECOD)) {
        pt_values <- lapply(groups$names, function(group) {
          format_count_percent(teae_subject_count(adae, group, "pt", soc = soc, pt = pt), groups$denominators[[group]])
        })
        add(paste0("  ", pt), pt_values)
      }
    }
  }
  list(
    table_id = "T14.3.1", title = as.character(plan$tables[["T14.3.1"]]$title),
    population = "Safety population", headers = headers, denominators = groups$denominators,
    data_cutoff_date = as.character(plan$data_cutoff_date),
    notes = c(
      "Treatment-emergent period is first dose through 30 days after last dose.",
      "A subject is counted once within each displayed level.",
      "Percentages use the number of safety subjects in the column as denominator."
    ),
    source = "Program source: TraceSDTM deterministic table rule T_TEAE_SOC_PT",
    data = dplyr::bind_rows(rows)
  )
}

write_table_csv <- function(object, config) {
  filename <- switch(object$table_id, "T14.1.1" = "t14_1_1_demographics.csv", "T14.3.1" = "t14_3_1_teae.csv")
  path <- trace_path(config$paths$tlf_csv_dir, filename)
  write_csv(object$data, path)
  path
}

write_table_html <- function(object, config) {
  filename <- switch(object$table_id, "T14.1.1" = "t14_1_1_demographics.html", "T14.3.1" = "t14_3_1_teae.html")
  path <- trace_path(config$paths$tlf_html_dir, filename)
  header <- htmltools::tags$tr(lapply(names(object$data), htmltools::tags$th))
  body <- lapply(seq_len(nrow(object$data)), function(index) {
    htmltools::tags$tr(lapply(object$data[index, , drop = TRUE], function(value) htmltools::tags$td(as.character(value))))
  })
  page <- htmltools::tags$html(
    htmltools::tags$head(
      htmltools::tags$meta(charset = "utf-8"),
      htmltools::tags$title(paste(object$table_id, object$title)),
      htmltools::tags$style(htmltools::HTML(
        "body{font-family:Arial,sans-serif;margin:32px;color:#17212b}h1{font-size:18px;margin-bottom:4px}h2{font-size:15px;font-weight:normal;margin-top:0}table{border-collapse:collapse;width:100%;font-size:13px}th,td{padding:6px 8px;border-bottom:1px solid #ccd5df;text-align:center}th:first-child,td:first-child,th:nth-child(2),td:nth-child(2){text-align:left}td:first-child,td:nth-child(2){white-space:pre-wrap}th{border-top:2px solid #243b53;border-bottom:2px solid #243b53}.note{font-size:11px;margin:4px 0}.source{font-size:11px;margin-top:10px}"
      ))
    ),
    htmltools::tags$body(
      htmltools::tags$h1(object$table_id), htmltools::tags$h2(object$title),
      htmltools::tags$p(object$population),
      htmltools::tags$table(htmltools::tags$thead(header), htmltools::tags$tbody(body)),
      lapply(object$notes, function(note) htmltools::tags$p(class = "note", note)),
      htmltools::tags$p(class = "note", paste("Data cutoff date:", object$data_cutoff_date)),
      htmltools::tags$p(class = "source", object$source)
    )
  )
  ensure_parent(path)
  htmltools::save_html(page, path, background = "white")
  path
}

write_table_rtf <- function(object, config) {
  filename <- switch(object$table_id, "T14.1.1" = "t14_1_1_demographics.rtf", "T14.3.1" = "t14_3_1_teae.rtf")
  path <- trace_path(config$paths$tlf_rtf_dir, filename)
  table <- object$data
  encoded <- table |>
    r2rtf::rtf_page(orientation = "landscape", use_i18n = TRUE) |>
    r2rtf::rtf_title(title = object$table_id, subtitle = object$title, text_font_size = 10) |>
    r2rtf::rtf_subline(text = object$population, text_font_size = 9) |>
    r2rtf::rtf_body(
      col_rel_width = c(2.5, 1.4, rep(1.5, ncol(table) - 2L)),
      text_justification = c("l", "l", rep("c", ncol(table) - 2L)), text_font_size = 8
    ) |>
    r2rtf::rtf_footnote(
      footnote = c(object$notes, paste("Data cutoff date:", object$data_cutoff_date)), text_font_size = 8
    ) |>
    r2rtf::rtf_source(source = object$source, text_font_size = 8) |>
    r2rtf::rtf_encode()
  ensure_parent(path)
  r2rtf::write_rtf(encoded, file = path)
  path
}

write_table_object <- function(object, config) {
  result_sha <- data_sha256(object$data)
  paths <- c(
    csv = write_table_csv(object, config),
    html = write_table_html(object, config),
    rtf = write_table_rtf(object, config)
  )
  tibble::tibble(
    table_id = object$table_id, title = object$title, population = object$population,
    rows = nrow(object$data), result_sha256 = result_sha,
    csv_sha256 = file_sha256(paths[["csv"]]), html_sha256 = file_sha256(paths[["html"]]),
    rtf_sha256 = file_sha256(paths[["rtf"]]), csv_path = paths[["csv"]],
    html_path = paths[["html"]], rtf_path = paths[["rtf"]]
  )
}

build_tlf <- function(config = load_project_config()) {
  ensure_output_directories(config)
  read_validation_summary(
    trace_path(config$paths$adam_validation_dir, "adam_validation_summary.json"), "ADaM 本地"
  )
  plan <- load_analysis_plan(config)
  assert_analysis_package_versions(plan)
  adam <- load_adam_datasets(config)
  objects <- list(
    `T14.1.1` = demographic_table_object(adam$ADSL, plan),
    `T14.3.1` = teae_table_object(adam$ADSL, adam$ADAE, plan)
  )
  manifest <- purrr::map_dfr(objects, write_table_object, config = config)
  input_paths <- c(
    ADSL = trace_path(config$paths$adam_xpt_dir, "adsl.xpt"),
    ADAE = trace_path(config$paths$adam_xpt_dir, "adae.xpt")
  )
  input_checksums <- analysis_input_checksums(input_paths)
  lineage <- purrr::imap_dfr(objects, function(object, table_id) tibble::tibble(
    table_id = table_id,
    source_datasets = if (identical(table_id, "T14.1.1")) "ADSL" else "ADSL | ADAE",
    shell_rule_id = as.character(plan$tables[[table_id]]$shell_rule_id),
    population = object$population, treatment_basis = as.character(plan$tables[[table_id]]$treatment_basis),
    analysis_plan_sha256 = attr(plan, "sha256"),
    input_data_sha256 = as.character(jsonlite::toJSON(input_checksums, auto_unbox = TRUE)),
    result_sha256 = data_sha256(object$data)
  ))
  write_csv(lineage, trace_path(config$paths$lineage_dir, "tlf_lineage.csv"))
  write_csv(manifest, trace_path(config$paths$manifest_dir, "tlf_manifest.csv"))
  write_json(lapply(objects, function(object) {
    object$data <- as.data.frame(object$data)
    object
  }), trace_path(config$paths$manifest_dir, "tlf_normalized_results.json"))
  write_json(list(
    generated_at = utc_now(), analysis_plan_sha256 = attr(plan, "sha256"),
    input_data_sha256 = input_checksums, tables = as.data.frame(manifest)
  ), trace_path(config$paths$manifest_dir, "tlf_build_manifest.json"))
  trace_info("已由同一规范化结果生成表格：%s。", paste(names(objects), collapse = "、"))
  invisible(objects)
}

table_validation_issues <- function(objects, adam, config) {
  issues <- list()
  add <- function(value) issues[[length(issues) + 1L]] <<- value
  plan <- load_analysis_plan(config)
  treatment_names <- vapply(plan$treatments, function(item) as.character(item$name), character(1))
  independently_count_denominators <- function(treatment_variable, population_variable) {
    population <- adam$ADSL[
      !is.na(adam$ADSL[[population_variable]]) & adam$ADSL[[population_variable]] == "Y",
      , drop = FALSE
    ]
    counts <- stats::setNames(vapply(treatment_names, function(group) {
      dplyr::n_distinct(population$USUBJID[population[[treatment_variable]] == group])
    }, integer(1)), treatment_names)
    c(counts, Total = dplyr::n_distinct(population$USUBJID))
  }
  for (table_id in names(objects)) {
    object <- objects[[table_id]]
    expected_denominators <- if (identical(table_id, "T14.1.1")) {
      independently_count_denominators("TRT01P", "ITTFL")
    } else {
      independently_count_denominators("TRT01A", "SAFFL")
    }
    if (!identical(as.integer(object$denominators[names(expected_denominators)]), as.integer(expected_denominators))) {
      add(analysis_issue(
        "TLF001", table_id, "denominator", "治疗组分母与 ADSL 独立复算结果不一致。",
        paste(object$denominators, collapse = ",")
      ))
    }
    stem <- if (identical(table_id, "T14.1.1")) "t14_1_1_demographics" else "t14_3_1_teae"
    paths <- c(
      csv = trace_path(config$paths$tlf_csv_dir, paste0(stem, ".csv")),
      html = trace_path(config$paths$tlf_html_dir, paste0(stem, ".html")),
      rtf = trace_path(config$paths$tlf_rtf_dir, paste0(stem, ".rtf"))
    )
    missing <- names(paths)[!vapply(paths, function(path) file.exists(path) && file.info(path)$size > 0L, logical(1))]
    if (length(missing)) add(analysis_issue("TLF002", table_id, "", "表格格式文件缺失或为空。", paste(missing, collapse = ",")))
    if (file.exists(paths[["csv"]])) {
      observed <- readr::read_csv(paths[["csv"]], show_col_types = FALSE, name_repair = "minimal", trim_ws = FALSE)
      expected <- object$data
      for (name in names(observed)) observed[[name]] <- ifelse(is.na(observed[[name]]), "", as.character(observed[[name]]))
      for (name in names(expected)) expected[[name]] <- ifelse(is.na(expected[[name]]), "", as.character(expected[[name]]))
      if (!identical(as.matrix(observed), as.matrix(expected))) add(analysis_issue("TLF003", table_id, "", "CSV 与规范化结果对象不一致。"))
    }
  }
  teae <- objects[["T14.3.1"]]$data[1L, , drop = FALSE]
  teae_denominators <- independently_count_denominators("TRT01A", "SAFFL")
  eligible_teae <- adam$ADAE[
    !is.na(adam$ADAE$SAFFL) & adam$ADAE$SAFFL == "Y" &
      !is.na(adam$ADAE$TRTEMFL) & adam$ADAE$TRTEMFL == "Y",
    , drop = FALSE
  ]
  teae_counts <- stats::setNames(vapply(treatment_names, function(group) {
    dplyr::n_distinct(eligible_teae$USUBJID[eligible_teae$TRTA == group])
  }, integer(1)), treatment_names)
  teae_counts <- c(teae_counts, Total = dplyr::n_distinct(eligible_teae$USUBJID))
  expected_overall <- stats::setNames(
    vapply(names(teae_denominators), function(group) {
      format_count_percent(teae_counts[[group]], teae_denominators[[group]])
    }, character(1)),
    sprintf("%s (N=%d)", names(teae_denominators), as.integer(teae_denominators))
  )
  actual_overall <- unlist(teae[names(expected_overall)], use.names = TRUE)
  if (!identical(as.character(actual_overall), as.character(expected_overall))) add(analysis_issue(
    "TLF004", "T14.3.1", "overall", "治疗中出现不良事件总体行与独立复算结果不一致。",
    paste(actual_overall, collapse = ",")
  ))
  if (nrow(adam$ADSL) && nrow(adam$ADAE)) {
    flagged_counts <- adam$ADAE |>
      dplyr::filter(.data$AOCCFL == "Y") |>
      dplyr::summarise(subjects = dplyr::n_distinct(.data$USUBJID)) |>
      dplyr::pull(.data$subjects)
    if (!identical(as.integer(flagged_counts), as.integer(teae_counts[["Total"]]))) add(analysis_issue(
      "TLF005", "T14.3.1", "USUBJID", "总体首次发生标志与治疗中出现事件的受试者去重计数不一致。",
      flagged_counts
    ))
  }
  dplyr::bind_rows(issues) %||% analysis_issue_table()
}

validate_tlf <- function(config = load_project_config()) {
  ensure_output_directories(config)
  plan <- load_analysis_plan(config)
  adam <- load_adam_datasets(config)
  objects <- list(
    `T14.1.1` = demographic_table_object(adam$ADSL, plan),
    `T14.3.1` = teae_table_object(adam$ADSL, adam$ADAE, plan)
  )
  issues <- table_validation_issues(objects, adam, config)
  if (!nrow(issues)) issues <- analysis_issue_table()
  path <- trace_path(config$paths$tlf_validation_dir, "tlf_issues.csv")
  write_csv(issues, path)
  write_json(list(
    generated_at = utc_now(), validator = "TraceSDTM local table checks",
    issue_count = nrow(issues), errors = sum(issues$severity == "ERROR"), warnings = 0L,
    result = if (nrow(issues)) "failed" else "passed", report_sha256 = file_sha256(path)
  ), trace_path(config$paths$tlf_validation_dir, "tlf_validation_summary.json"))
  trace_info("表格本地检查完成：%d 个问题。", nrow(issues))
  invisible(issues)
}

run_analysis <- function(config = load_project_config()) {
  build_adam(config)
  validate_adam(config)
  build_tlf(config)
  validate_tlf(config)
  trace_info("确定性分析流程执行完成。")
  invisible(TRUE)
}
