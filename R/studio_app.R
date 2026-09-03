# TraceSDTM Studio 0.5 Shiny application ------------------------------------

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
      item_schema, prefix, input,
      value = if (length(value) >= index) value[[index]] else NULL,
      path = paste0(path, "[", index, "]")
    )))
  }
  raw <- input[[id]]
  switch(type,
    boolean = isTRUE(raw), integer = as.integer(raw), number = as.numeric(raw),
    as.character(raw %||% "")
  )
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
    specification <- load_mapping_template(config)
    metadata <- load_metadata(config)
    registry <- load_transform_registry(config)
    policies <- load_mapping_policies(config)
    dictionary <- load_source_dictionary_v04(config)
    groups <- recommendation_groups_v04(specification)
    prompts <- if (identical(stage, "targets")) {
      lapply(groups, target_prompt_v04, specification = specification, metadata = metadata, policies = policies, dictionary = dictionary)
    } else if (identical(stage, "functions")) {
      targets <- read_recommendation_stage_v04(config, "target_decisions.json")
      lapply(groups, function(group) {
        ids <- vapply(group$tasks, task_id_v04, character(1))
        subset <- list(valid = targets$valid[intersect(ids, names(targets$valid))], failures = list())
        function_prompt_v04(group, subset, registry, specification, policies, dictionary)
      })
    } else {
      targets <- read_recommendation_stage_v04(config, "target_decisions.json")
      functions <- read_recommendation_stage_v04(config, "function_candidates.json")
      resolutions <- resolve_known_parameters_v04(specification, targets, functions, registry, policies,
                                                   load_recommendation_resources(config))
      list(parameter_prompt_v04(resolutions) %||% "全部参数均由程序确定性注入，本阶段不发送模型请求。")
    }
    text <- paste(unlist(prompts), collapse = "\n\n===== 下一组 =====\n\n")
    list(text = text, sha256 = digest::digest(text, algo = "sha256", serialize = FALSE), stage = stage,
         examples_included = isTRUE(include_examples))
  })
}

trace_studio_ui <- function() {
  bslib::page_sidebar(
    title = shiny::div(class = "trace-title", "TraceSDTM Studio", shiny::tags$small("0.5")),
    fillable = TRUE,
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#176B87", base_font = bslib::font_collection("Segoe UI", "Microsoft YaHei", "Arial", "sans-serif")),
    sidebar = bslib::sidebar(
      width = 310,
      shiny::selectInput("active_project", "当前项目", choices = character()),
      shiny::selectInput("active_run", "当前运行", choices = character()),
      shiny::actionButton("refresh_all", "刷新状态", class = "btn-outline-primary w-100"),
      shiny::hr(),
      shiny::uiOutput("sidebar_project_status"),
      shiny::uiOutput("sidebar_job_status"),
      shiny::actionButton("cancel_job", "取消后台任务", class = "btn-outline-danger w-100")
    ),
    shiny::tags$head(shiny::tags$style(shiny::HTML("\
      .trace-title{font-weight:700;letter-spacing:.2px}.trace-title small{font-size:.55em;color:#7b8a98;margin-left:.4rem}\
      .trace-card{border:1px solid #dfe7ee;border-radius:12px;padding:18px;background:white;margin-bottom:16px}\
      .trace-muted{color:#64748b}.trace-ok{color:#16855b}.trace-warn{color:#b36b00}.trace-error{color:#b42318}\
      .trace-schema-fieldset{border:1px solid #dfe7ee;border-radius:8px;padding:10px 14px;margin-bottom:10px}\
      .trace-schema-fieldset legend{font-size:.9rem;font-weight:600;padding:0 6px;width:auto}\
      pre.trace-log{max-height:260px;overflow:auto;background:#0f172a;color:#dbeafe;padding:12px;border-radius:8px}\
      .navbar-brand{font-weight:700}\
    "))),
    bslib::navset_card_tab(
      id = "main_nav",
      bslib::nav_panel("项目",
        shiny::div(class = "trace-card",
          shiny::h3("创建本地项目"),
          shiny::fluidRow(
            shiny::column(3, shiny::textInput("new_project_id", "项目编号", placeholder = "例如 trace-demo")),
            shiny::column(3, shiny::textInput("new_project_name", "项目名称")),
            shiny::column(2, shiny::selectInput("new_project_template", "模板", c("基础" = "basic", "中等" = "intermediate", "高级" = "advanced"))),
            shiny::column(2, shiny::textInput("new_study_id", "研究编号", value = "TRACE001")),
            shiny::column(2, shiny::br(), shiny::actionButton("create_project", "创建项目", class = "btn-primary w-100"))
          )
        ),
        shiny::div(class = "trace-card",
          shiny::div(class = "d-flex justify-content-between", shiny::h3("本地项目"), shiny::checkboxInput("show_archived", "显示归档项目", FALSE)),
          DT::DTOutput("projects_table"),
          shiny::actionButton("archive_project", "归档当前项目", class = "btn-outline-warning")
        )
      ),
      bslib::nav_panel("数据源",
        shiny::div(class = "trace-card",
          shiny::h3("模板来源槽位"),
          shiny::selectInput("source_slot", "来源槽位", choices = character()),
          shiny::fileInput("source_upload", "上传UTF-8 CSV", accept = c(".csv", "text/csv")),
          shiny::actionButton("upload_source", "保存上传并生成绑定建议", class = "btn-primary"),
          shiny::p(class = "trace-muted", "原文件只读保留；系统另建规范化暂存CSV。模板之外的列不会进入转换或模型上下文。")
        ),
        shiny::div(class = "trace-card", shiny::h3("字段绑定"), shiny::uiOutput("binding_controls"), shiny::actionButton("confirm_bindings", "确认绑定并执行键质量检查", class = "btn-success")),
        shiny::div(class = "trace-card", shiny::h3("本地数据预览"), DT::DTOutput("source_preview"))
      ),
      bslib::nav_panel("项目政策",
        shiny::div(class = "trace-card",
          shiny::h3("受控政策"),
          shiny::fluidRow(
            shiny::column(3, shiny::textInput("policy_study_id", "研究编号")),
            shiny::column(3, shiny::selectInput("policy_subject_separator", "受试者标识符分隔符", choices = c("-", "_", "/", ""))),
            shiny::column(3, shiny::selectInput("policy_reference_selection", "参考日期选择", choices = c("最早完整日期时间" = "earliest_complete_datetime", "最晚完整日期时间" = "latest_complete_datetime"))),
            shiny::column(3, shiny::selectInput("policy_unit_set", "单位换算集合", choices = character()))
          ),
          shiny::fluidRow(
            shiny::column(3, shiny::selectInput("policy_site_delimiter", "中心编号分隔符", choices = c("-", "_", "/"))),
            shiny::column(3, shiny::numericInput("policy_site_position", "中心编号位置", value = 1, min = 1, max = 10, step = 1)),
            shiny::column(3, shiny::selectInput("policy_baseline_visit", "基线访视", choices = character())),
            shiny::column(3, shiny::selectInput("policy_baseline_reference", "基线参考变量", choices = c("RFSTDTC", "RFXSTDTC")))
          ),
          shiny::fluidRow(
            shiny::column(6, shiny::selectizeInput("policy_ae_sequence", "AE序号排序字段（按所选顺序）", choices = character(), multiple = TRUE)),
            shiny::column(6, shiny::selectizeInput("policy_vs_sequence", "VS序号排序字段（按所选顺序）", choices = character(), multiple = TRUE))
          ),
          shiny::h4("批准的日期与时间格式"), shiny::uiOutput("date_format_controls"),
          shiny::h4("已登记单位目标"), shiny::uiOutput("unit_target_controls"),
          shiny::actionButton("save_policy", "保存受控政策", class = "btn-primary"),
          shiny::p(class = "trace-muted", "不能输入R代码、正则表达式、自由公式、自由连接键或未登记函数。")
        ),
        shiny::div(class = "trace-card",
          shiny::h3("术语与访视资源"), DT::DTOutput("policy_resources"),
          shiny::fluidRow(
            shiny::column(3, shiny::selectInput("term_codelist", "术语表", choices = character())),
            shiny::column(3, shiny::textInput("term_source_value", "来源术语")),
            shiny::column(3, shiny::selectInput("term_target_value", "允许的标准值", choices = character())),
            shiny::column(3, shiny::br(), shiny::actionButton("save_term_mapping", "保存术语映射", class = "btn-outline-primary w-100"))
          ),
          shiny::fluidRow(
            shiny::column(3, shiny::selectInput("visit_map_id", "访视表", choices = character())),
            shiny::column(3, shiny::textInput("visit_source_name", "来源访视名称")),
            shiny::column(3, shiny::numericInput("visit_target_number", "访视编号", value = 0)),
            shiny::column(3, shiny::br(), shiny::actionButton("save_visit_mapping", "保存访视映射", class = "btn-outline-primary w-100"))
          )
        )
      ),
      bslib::nav_panel("数据画像",
        shiny::div(class = "trace-card",
          shiny::h3("建立新的运行快照"),
          shiny::actionButton("create_run_profile", "创建运行并开始画像", class = "btn-primary"),
          shiny::actionButton("profile_existing", "重新执行当前运行画像", class = "btn-outline-primary"),
          shiny::p(class = "trace-muted", "每次从画像重新开始都会冻结当前规范化数据和政策，生成新的运行编号。")
        ),
        shiny::div(class = "trace-card", shiny::h3("字段画像"), DT::DTOutput("profile_table")),
        shiny::div(class = "trace-card", shiny::h3("来源关系"), DT::DTOutput("relationship_table"))
      ),
      bslib::nav_panel("映射推荐",
        shiny::div(class = "trace-card",
          shiny::h3("会话内模型配置"),
          shiny::fluidRow(
            shiny::column(4, shiny::textInput("model_base_url", "接口地址", value = Sys.getenv("TRACE_SDTM_BASE_URL", unset = ""))),
            shiny::column(3, shiny::textInput("model_name", "模型", value = Sys.getenv("TRACE_SDTM_MODEL", unset = ""))),
            shiny::column(5, shiny::passwordInput("model_api_key", "接口密钥（仅当前会话内存）"))
          ),
          shiny::checkboxInput("include_examples", "明确授权发送所选去标识化示例值", FALSE),
          shiny::selectizeInput("example_source_keys", "可发送示例的来源字段", choices = character(), multiple = TRUE),
          shiny::p(class = "trace-muted", "默认只发送字段名称、标签、类型、缺失比例、唯一值数量、格式候选、角色、粒度和键。标识符、键及中心字段始终不发送示例值。")
        ),
        shiny::div(class = "trace-card",
          shiny::h3("三阶段运行"),
          shiny::div(class = "d-flex gap-2 flex-wrap",
            shiny::actionButton("run_targets", "1 识别目标", class = "btn-outline-primary"),
            shiny::actionButton("run_functions", "2 选择函数", class = "btn-outline-primary"),
            shiny::actionButton("run_parameters", "3 解析参数", class = "btn-outline-primary"),
            shiny::actionButton("run_assemble", "4 组装计划", class = "btn-outline-primary"),
            shiny::actionButton("run_recommend_all", "一键推荐", class = "btn-primary")
          ),
          shiny::hr(),
          shiny::selectInput("preview_stage", "预览阶段", c("目标识别" = "targets", "函数选择" = "functions", "参数补全" = "parameters")),
          shiny::actionButton("preview_prompt", "生成完整请求预览及校验值"),
          shiny::verbatimTextOutput("prompt_hash"),
          shiny::textAreaInput("prompt_preview_text", "请求正文", value = "", rows = 14, width = "100%")
        ),
        shiny::div(class = "trace-card", shiny::h3("后台日志"), shiny::uiOutput("job_log"))
      ),
      bslib::nav_panel("人工审核",
        shiny::div(class = "trace-card",
          shiny::fluidRow(
            shiny::column(4, shiny::selectInput("review_task", "原子任务", choices = character())),
            shiny::column(3, shiny::textInput("reviewer_id", "审核者标识")),
            shiny::column(2, shiny::selectInput("review_decision", "决定", c("接受" = "accept", "修改" = "modify", "拒绝" = "reject", "信息不足" = "needs_information"))),
            shiny::column(3, shiny::selectInput("review_rank", "候选序号", choices = 1:3))
          ),
          shiny::textInput("review_comment", "审核说明"),
          DT::DTOutput("review_candidates")
        ),
        shiny::conditionalPanel("input.review_decision == 'modify'",
          shiny::div(class = "trace-card",
            shiny::h3("结构化修改"),
            shiny::selectInput("modify_transform", "转换函数", choices = character()),
            shiny::selectizeInput("modify_sources", "来源编号", choices = character(), multiple = TRUE),
            shiny::selectizeInput("modify_targets", "目标变量", choices = character(), multiple = TRUE),
            shiny::uiOutput("modify_parameters")
          )
        ),
        shiny::div(class = "trace-card",
          shiny::actionButton("save_review", "保存当前任务审核", class = "btn-primary"),
          shiny::actionButton("approve_review", "批准全部审核", class = "btn-success"),
          shiny::downloadButton("download_review", "导出Excel"),
          shiny::fileInput("import_review_file", "导入已审核Excel", accept = ".xlsx"),
          shiny::actionButton("import_review", "严格校验并导入"),
          DT::DTOutput("review_status_table")
        )
      ),
      bslib::nav_panel("构建",
        shiny::div(class = "trace-card", shiny::h3("确定性生成"), shiny::actionButton("run_build", "生成DM、AE、VS", class = "btn-primary"), shiny::p(class = "trace-muted", "构建只读取批准的YAML规格，不会再次调用模型。")),
        shiny::div(class = "trace-card", shiny::h3("数据集摘要"), DT::DTOutput("dataset_manifest")),
        shiny::div(class = "trace-card", shiny::h3("输出文件"), DT::DTOutput("output_files"))
      ),
      bslib::nav_panel("验证",
        shiny::div(class = "trace-card",
          shiny::h3("独立验证"),
          shiny::actionButton("run_local_validation", "运行本地检查", class = "btn-primary"),
          shiny::actionButton("run_p21_doctor", "检查Pinnacle 21环境"),
          shiny::actionButton("run_p21_validation", "自动运行Pinnacle 21", class = "btn-outline-primary"),
          shiny::fileInput("p21_report_upload", "导入Community Excel报告", accept = ".xlsx"),
          shiny::actionButton("import_p21", "导入验证报告")
        ),
        shiny::div(class = "trace-card", shiny::h3("本地问题"), DT::DTOutput("local_issues")),
        shiny::div(class = "trace-card", shiny::h3("Pinnacle 21问题"), DT::DTOutput("p21_issues"))
      ),
      bslib::nav_panel("追溯与报告",
        shiny::div(class = "trace-card",
          shiny::h3("报告与证据"),
          shiny::actionButton("run_report", "生成离线报告", class = "btn-primary"),
          shiny::uiOutput("report_link"),
          shiny::downloadButton("download_evidence", "下载项目证据包")
        ),
        shiny::div(class = "trace-card", shiny::h3("字段级追溯"), shiny::textInput("lineage_filter", "筛选任务、域、变量或函数"), DT::DTOutput("lineage_table"))
      ),
      bslib::nav_panel("系统检查",
        shiny::div(class = "trace-card", shiny::h3("工作台检查"), shiny::actionButton("refresh_doctor", "重新检查"), DT::DTOutput("studio_doctor_table")),
        shiny::div(class = "trace-card", shiny::h3("安全边界"), shiny::tags$ul(
          shiny::tags$li("服务固定监听127.0.0.1。"),
          shiny::tags$li("密钥只保存在当前会话内存，并以环境变量传给单个后台子进程。"),
          shiny::tags$li("工作台不提供项目删除、自由代码、自由连接关系或未登记转换函数。"),
          shiny::tags$li("本工具不是法规申报级认证系统，所有映射仍需合格专家审核。")
        ))
      )
    )
  )
}

trace_studio_server <- function(input, output, session) {
  refresh <- shiny::reactiveVal(0L)
  binding_state <- shiny::reactiveVal(NULL)
  binding_input_ids <- shiny::reactiveVal(list())
  prompt_state <- shiny::reactiveVal(NULL)
  doctor_state <- shiny::reactiveVal(studio_doctor())
  session_key <- shiny::reactiveVal("")

  projects <- shiny::reactive({
    refresh()
    studio_list_projects(include_archived = isTRUE(input$show_archived))
  })

  shiny::observe({
    data <- projects()
    choices <- stats::setNames(data$project_id, paste0(data$name, " [", data$project_id, "]"))
    selected <- if (!is.null(input$active_project) && input$active_project %in% data$project_id) input$active_project else if (nrow(data)) data$project_id[[1L]] else character()
    shiny::updateSelectInput(session, "active_project", choices = choices, selected = selected)
  })

  active_project <- shiny::reactive({
    shiny::req(nzchar(input$active_project %||% ""))
    input$active_project
  })

  runs <- shiny::reactive({
    refresh(); shiny::req(active_project())
    studio_list_runs(active_project())
  })

  shiny::observe({
    data <- runs()
    project <- studio_read_project(active_project())
    choices <- stats::setNames(data$run_id, paste0(data$run_id, ifelse(data$stale, "（已过期）", "")))
    selected <- if (length(project$active_run_id) && project$active_run_id %in% data$run_id) project$active_run_id else if (nrow(data)) data$run_id[[1L]] else character()
    shiny::updateSelectInput(session, "active_run", choices = choices, selected = selected)
  })

  active_config <- shiny::reactive({
    shiny::req(active_project(), nzchar(input$active_run %||% ""))
    studio_load_run_config(active_project(), input$active_run)
  })

  output$projects_table <- DT::renderDT(DT::datatable(projects(), selection = "single", rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE)))

  shiny::observeEvent(input$projects_table_rows_selected, {
    row <- input$projects_table_rows_selected
    if (length(row)) shiny::updateSelectInput(session, "active_project", selected = projects()$project_id[[row]])
  })

  output$sidebar_project_status <- shiny::renderUI({
    if (!nzchar(input$active_project %||% "")) return(shiny::p(class = "trace-muted", "尚未创建项目"))
    project <- studio_read_project(input$active_project)
    shiny::tagList(
      shiny::strong(project$name), shiny::br(),
      shiny::span(class = "trace-muted", paste("模板：", project$template)), shiny::br(),
      shiny::span(class = if (isTRUE(project$active_run_stale)) "trace-warn" else "trace-ok",
                  if (isTRUE(project$active_run_stale)) "当前运行已过期" else "当前配置未标记过期")
    )
  })

  output$sidebar_job_status <- shiny::renderUI({
    refresh()
    job <- studio_active_job()
    if (is.null(job)) return(shiny::p(class = "trace-muted", "无后台任务"))
    shiny::tagList(shiny::strong(job$state$command), shiny::br(), shiny::span(class = "trace-warn", job$state$status))
  })

  shiny::observeEvent(input$refresh_all, refresh(refresh() + 1L))

  shiny::observeEvent(input$create_project, studio_notify_error(session, {
    project <- studio_create_project(input$new_project_id, input$new_project_name, input$new_project_template, input$new_study_id)
    refresh(refresh() + 1L)
    shiny::updateSelectInput(session, "active_project", selected = project$project_id)
    shiny::showNotification("项目已创建。", type = "message")
  }))

  shiny::observeEvent(input$archive_project, studio_notify_error(session, {
    studio_archive_project(active_project(), actor = input$reviewer_id %||% "local_user")
    refresh(refresh() + 1L)
    shiny::showNotification("项目已归档；目录和历史运行仍保留。", type = "message")
  }))

  shiny::observe({
    shiny::req(active_project())
    sources <- studio_non_derived_sources(active_project())
    choices <- stats::setNames(names(sources), vapply(sources, function(item) paste(item$form_name, "—", item$file), character(1)))
    shiny::updateSelectInput(session, "source_slot", choices = choices)
  })

  shiny::observeEvent(input$upload_source, studio_notify_error(session, {
    shiny::req(input$source_upload$datapath, input$source_slot)
    suggestions <- studio_import_source(active_project(), input$source_slot, input$source_upload$datapath, input$source_upload$name)
    binding_state(list(dataset = input$source_slot, suggestions = suggestions))
    refresh(refresh() + 1L)
    shiny::showNotification("上传已保存，请确认字段绑定。", type = "message")
  }))

  shiny::observeEvent(input$source_slot, {
    if (!nzchar(active_project() %||% "") || !nzchar(input$source_slot %||% "")) return()
    value <- tryCatch(studio_binding_suggestions(active_project(), input$source_slot), error = function(error) NULL)
    if (!is.null(value)) binding_state(list(dataset = input$source_slot, suggestions = value))
  }, ignoreInit = TRUE)

  output$binding_controls <- shiny::renderUI({
    state <- binding_state()
    if (is.null(state)) return(shiny::p(class = "trace-muted", "上传CSV后显示绑定建议。"))
    bindings <- studio_read_bindings(active_project())$datasets[[state$dataset]]
    data <- studio_validate_utf8_csv(file.path(studio_project_path(active_project()), bindings$original_file))
    ids <- list()
    controls <- lapply(seq_len(nrow(state$suggestions)), function(index) {
      row <- state$suggestions[index, ]
      id <- paste0("bind_", index, "_", substr(digest::digest(row$logical_field, serialize = FALSE), 1L, 5L))
      ids[[row$logical_field]] <<- id
      shiny::fluidRow(
        shiny::column(4, shiny::tags$label(paste0(row$logical_field, if (isTRUE(row$is_key)) "（键）" else "")), shiny::p(class = "trace-muted", paste(row$label, row$role))),
        shiny::column(5, shiny::selectInput(id, NULL, choices = names(data), selected = row$suggested_source)),
        shiny::column(3, shiny::span(sprintf("建议分值 %.3f", row$score)))
      )
    })
    binding_input_ids(ids)
    controls
  })

  shiny::observeEvent(input$confirm_bindings, studio_notify_error(session, {
    state <- binding_state(); shiny::req(state)
    ids <- binding_input_ids()
    mapping <- stats::setNames(vapply(ids, function(id) as.character(input[[id]]), character(1)), names(ids))
    studio_confirm_bindings(active_project(), state$dataset, mapping, actor = input$reviewer_id %||% "local_user")
    refresh(refresh() + 1L)
    shiny::showNotification("字段绑定和键质量检查已通过。", type = "message")
  }))

  output$source_preview <- DT::renderDT({
    state <- binding_state(); shiny::req(state)
    item <- studio_read_bindings(active_project())$datasets[[state$dataset]]
    path <- file.path(studio_project_path(active_project()), item$original_file)
    DT::datatable(utils::head(studio_validate_utf8_csv(path), 20L), rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
  })

  policy_values <- shiny::reactive({ refresh(); shiny::req(active_project()); studio_policy_options(active_project()) })
  shiny::observe({
    options <- policy_values(); policy <- options$current; project <- studio_read_project(active_project())
    separator <- policy$identifiers$usubjid$separator %||% policy$identifiers$subject_identifier$separator %||% "-"
    shiny::updateTextInput(session, "policy_study_id", value = project$study_id)
    shiny::updateSelectInput(session, "policy_subject_separator", choices = options$subject_separators, selected = separator)
    shiny::updateSelectInput(session, "policy_unit_set", choices = options$unit_sets, selected = policy$unit_standardization$conversion_set_id %||% "")
    visits <- unique(unlist(lapply(options$visit_maps, names), use.names = FALSE))
    selected_visit <- unlist(policy$baseline_rules$eligible_visits %||% character(), use.names = FALSE)
    shiny::updateSelectInput(session, "policy_baseline_visit", choices = c("不设置" = "", visits), selected = if (length(selected_visit)) selected_visit[[1L]] else "")
    shiny::updateSelectInput(session, "policy_baseline_reference", choices = options$baseline_references, selected = policy$baseline_rules$reference_field %||% "RFSTDTC")
    shiny::updateSelectizeInput(session, "policy_ae_sequence", choices = options$sequence_fields$AE,
                                selected = unlist(policy$sequence_rules$AE$record_variables %||% character()), server = TRUE)
    shiny::updateSelectizeInput(session, "policy_vs_sequence", choices = options$sequence_fields$VS,
                                selected = unlist(policy$sequence_rules$VS$record_variables %||% character()), server = TRUE)
    shiny::updateSelectInput(session, "term_codelist", choices = names(options$codelists))
    shiny::updateSelectInput(session, "visit_map_id", choices = names(options$visit_maps))
    if (!is.null(policy$identifiers$site_identifier)) {
      shiny::updateSelectInput(session, "policy_site_delimiter", selected = policy$identifiers$site_identifier$delimiter)
      shiny::updateNumericInput(session, "policy_site_position", value = policy$identifiers$site_identifier$part_position)
    }
    if (!is.null(policy$reference_datetime_rules)) shiny::updateSelectInput(session, "policy_reference_selection", selected = policy$reference_datetime_rules$selection)
  })

  output$date_format_controls <- shiny::renderUI({
    options <- policy_values(); entries <- options$current$date_time_formats$approved_sources %||% list()
    if (!length(entries)) return(shiny::p(class = "trace-muted", "本模板没有需配置的日期时间字段。"))
    lapply(seq_along(entries), function(index) {
      entry <- entries[[index]]; key <- paste(entry$source$dataset, entry$source$variable, sep = "__")
      shiny::selectizeInput(paste0("datefmt_", index), key, choices = options$date_formats,
                            selected = unlist(entry$formats), multiple = TRUE)
    })
  })

  output$unit_target_controls <- shiny::renderUI({
    options <- policy_values(); policy <- options$current
    findings <- policy$unit_standardization$findings %||% list()
    if (!length(findings)) return(shiny::p(class = "trace-muted", "本模板不需要单位标准化。"))
    set_id <- input$policy_unit_set %||% policy$unit_standardization$conversion_set_id
    allowed <- options$unit_targets[[set_id]] %||% list()
    shiny::fluidRow(lapply(seq_along(findings), function(index) {
      finding <- findings[[index]]; code <- as.character(finding$test_code)
      shiny::column(4, shiny::selectInput(paste0("unit_target_", index), paste0(code, " 目标单位"),
                                         choices = unique(unlist(allowed[[code]] %||% character())), selected = finding$target_unit))
    }))
  })

  shiny::observe({
    options <- policy_values(); id <- input$term_codelist %||% names(options$codelists)[[1L]]
    values <- unique(as.character(unname(unlist(options$codelists[[id]] %||% character()))))
    shiny::updateSelectInput(session, "term_target_value", choices = values)
  })

  output$policy_resources <- DT::renderDT({
    options <- policy_values()
    terminology <- purrr::imap_dfr(options$codelists, function(values, id) tibble::tibble(resource = "受控术语", id = id, source = names(values), target = unname(unlist(values))))
    visits <- purrr::imap_dfr(options$visit_maps, function(values, id) tibble::tibble(resource = "访视表", id = id, source = names(values), target = as.character(unname(unlist(values)))))
    DT::datatable(dplyr::bind_rows(terminology, visits), rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
  })

  shiny::observeEvent(input$save_policy, studio_notify_error(session, {
    entries <- policy_values()$current$date_time_formats$approved_sources %||% list()
    date_formats <- list()
    for (index in seq_along(entries)) {
      key <- paste(entries[[index]]$source$dataset, entries[[index]]$source$variable, sep = "__")
      date_formats[[key]] <- as.list(input[[paste0("datefmt_", index)]] %||% character())
    }
    findings <- policy_values()$current$unit_standardization$findings %||% list()
    unit_targets <- list()
    for (index in seq_along(findings)) unit_targets[[as.character(findings[[index]]$test_code)]] <- input[[paste0("unit_target_", index)]]
    studio_save_policy(active_project(), list(
      study_id = input$policy_study_id, subject_separator = input$policy_subject_separator,
      site_delimiter = input$policy_site_delimiter, site_position = input$policy_site_position,
      reference_selection = input$policy_reference_selection, baseline_visit = input$policy_baseline_visit,
      baseline_reference = input$policy_baseline_reference,
      sequence_rules = list(AE = input$policy_ae_sequence, VS = input$policy_vs_sequence),
      unit_set = input$policy_unit_set, unit_targets = unit_targets, date_formats = date_formats
    ), actor = input$reviewer_id %||% "local_user")
    refresh(refresh() + 1L)
    shiny::showNotification("项目政策已保存，现有运行（如有）已标记过期。", type = "message")
  }))

  shiny::observeEvent(input$save_term_mapping, studio_notify_error(session, {
    studio_add_terminology_mapping(active_project(), input$term_codelist, input$term_source_value, input$term_target_value,
                                   actor = input$reviewer_id %||% "local_user")
    refresh(refresh() + 1L)
    shiny::showNotification("受控术语映射已保存。", type = "message")
  }))

  shiny::observeEvent(input$save_visit_mapping, studio_notify_error(session, {
    studio_add_visit_mapping(active_project(), input$visit_map_id, input$visit_source_name, input$visit_target_number,
                             actor = input$reviewer_id %||% "local_user")
    refresh(refresh() + 1L)
    shiny::showNotification("访视映射已保存。", type = "message")
  }))

  start_job <- function(command) {
    config <- active_config()
    key <- input$model_api_key %||% ""
    if (nzchar(key)) session_key(key) else key <- session_key()
    if (!nzchar(key)) key <- Sys.getenv("TRACE_SDTM_API_KEY", unset = "")
    credentials <- list(api_key = key, base_url = input$model_base_url, model = input$model_name, reviewer = input$reviewer_id)
    studio_start_job(config$studio$project_id, config$studio$run_id, command, credentials,
                     list(include_examples = isTRUE(input$include_examples), source_keys = input$example_source_keys %||% character()))
    refresh(refresh() + 1L)
  }

  shiny::observeEvent(input$create_run_profile, studio_notify_error(session, {
    run_id <- studio_create_run(active_project(), actor = input$reviewer_id %||% "local_user")
    refresh(refresh() + 1L)
    shiny::updateSelectInput(session, "active_run", selected = run_id)
    shiny::showNotification(paste("已创建运行", run_id), type = "message")
    shiny::invalidateLater(250, session)
  }))

  shiny::observeEvent(input$profile_existing, studio_notify_error(session, start_job("profile")))
  shiny::observeEvent(input$run_targets, studio_notify_error(session, start_job("recommend-targets")))
  shiny::observeEvent(input$run_functions, studio_notify_error(session, start_job("recommend-functions")))
  shiny::observeEvent(input$run_parameters, studio_notify_error(session, start_job("recommend-parameters")))
  shiny::observeEvent(input$run_assemble, studio_notify_error(session, start_job("assemble-recommendations")))
  shiny::observeEvent(input$run_recommend_all, studio_notify_error(session, start_job("recommend")))
  shiny::observeEvent(input$run_build, studio_notify_error(session, start_job("build")))
  shiny::observeEvent(input$run_local_validation, studio_notify_error(session, start_job("validate-local")))
  shiny::observeEvent(input$run_p21_doctor, studio_notify_error(session, start_job("doctor-p21")))
  shiny::observeEvent(input$run_p21_validation, studio_notify_error(session, start_job("validate-p21")))
  shiny::observeEvent(input$run_report, studio_notify_error(session, start_job("report")))

  shiny::observeEvent(input$cancel_job, studio_notify_error(session, {
    studio_cancel_job(); refresh(refresh() + 1L); shiny::showNotification("已请求取消后台任务。", type = "warning")
  }))

  shiny::observe({
    shiny::invalidateLater(studio_settings()$poll_interval_ms, session)
    state <- studio_poll_job()
    if (!is.null(state)) refresh(refresh() + 1L)
  })

  output$job_log <- shiny::renderUI({
    refresh()
    if (!nzchar(input$active_run %||% "")) return(shiny::p(class = "trace-muted", "尚无运行。"))
    path <- file.path(studio_run_path(active_project(), input$active_run), "logs", "studio_job.log")
    lines <- if (file.exists(path)) utils::tail(readLines(path, warn = FALSE, encoding = "UTF-8"), 80L) else "尚无日志。"
    shiny::tags$pre(class = "trace-log", paste(lines, collapse = "\n"))
  })

  output$profile_table <- DT::renderDT({
    config <- active_config(); refresh()
    path <- file.path(trace_path(config$paths$profile_dir), "source_dictionary.csv")
    shiny::req(file.exists(path))
    DT::datatable(readr::read_csv(path, show_col_types = FALSE), rownames = FALSE, options = list(scrollX = TRUE, pageLength = 15))
  })
  output$relationship_table <- DT::renderDT({
    config <- active_config(); refresh(); path <- file.path(trace_path(config$paths$profile_dir), "source_relationships.csv"); shiny::req(file.exists(path))
    DT::datatable(readr::read_csv(path, show_col_types = FALSE), rownames = FALSE, options = list(scrollX = TRUE))
  })

  shiny::observe({
    config <- active_config(); refresh()
    path <- file.path(trace_path(config$paths$profile_dir), "source_dictionary.csv")
    if (!file.exists(path)) return()
    dictionary <- readr::read_csv(path, show_col_types = FALSE)
    sensitive <- grepl("identifier|subject|site|center|(^|_)key($|_)", tolower(dictionary$concept_roles %||% "")) |
      grepl("^(STUDY|PATNUM|SUBJID|USUBJID|SITEID)$", dictionary$source_variable, ignore.case = TRUE)
    keys <- paste(dictionary$source_dataset, dictionary$source_variable, sep = ".")
    choices <- stats::setNames(keys[!sensitive], paste(dictionary$label[!sensitive], "[", keys[!sensitive], "]"))
    shiny::updateSelectizeInput(session, "example_source_keys", choices = choices, server = TRUE)
  })

  shiny::observeEvent(input$preview_prompt, studio_notify_error(session, {
    value <- studio_prompt_preview(active_config(), input$preview_stage, isTRUE(input$include_examples), input$example_source_keys %||% character())
    prompt_state(value)
    shiny::updateTextAreaInput(session, "prompt_preview_text", value = value$text)
  }))
  output$prompt_hash <- shiny::renderText({ value <- prompt_state(); if (is.null(value)) "" else paste("SHA-256:", value$sha256, "| 示例值:", if (value$examples_included) "按授权筛选" else "未包含") })

  shiny::observe({
    config <- active_config(); refresh()
    assembled <- file.path(trace_path(config$paths$recommendation_dir), "assembled_recommendations.json")
    if (!file.exists(assembled)) return()
    state <- studio_initialize_review(config)
    tasks <- names(state$tasks)
    labels <- vapply(tasks, function(id) paste0(id, " — ", state$tasks[[id]]$decision), character(1))
    shiny::updateSelectInput(session, "review_task", choices = stats::setNames(tasks, labels))
  })

  review_details <- shiny::reactive({ shiny::req(input$review_task); studio_review_task_details(active_config(), input$review_task) })
  output$review_candidates <- DT::renderDT({
    details <- review_details()
    rows <- v04_plan_rows(details$candidates)
    DT::datatable(rows, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 5), selection = "single")
  })

  shiny::observe({
    details <- review_details(); item <- details$state
    shiny::updateSelectInput(session, "review_decision", selected = if (item$decision == "pending") "accept" else item$decision)
    shiny::updateSelectInput(session, "review_rank", selected = item$selected_rank %||% 1L)
    shiny::updateTextInput(session, "review_comment", value = item$review_comment %||% "")
    if (nzchar(item$reviewer %||% "")) shiny::updateTextInput(session, "reviewer_id", value = item$reviewer)
    options <- studio_review_editor_options(active_config(), input$review_task)
    shiny::updateSelectInput(session, "modify_transform", choices = options$transforms)
    shiny::updateSelectizeInput(session, "modify_sources", choices = options$source_refs, server = TRUE)
    shiny::updateSelectizeInput(session, "modify_targets", choices = options$target_variables, server = TRUE)
  })

  selected_modify_entry <- shiny::reactive({
    shiny::req(input$modify_transform)
    registry_entry(input$modify_transform, load_transform_registry(active_config()))
  })
  output$modify_parameters <- shiny::renderUI({
    entry <- selected_modify_entry()
    current <- review_details()$state$modified_step
    value <- if (!is.null(current) && identical(current$transform_id, entry$transform_id)) current$parameters else list()
    studio_schema_ui(entry$parameter_schema, paste0("param_", input$review_task), value, input)
  })

  shiny::observeEvent(input$review_candidates_rows_selected, {
    row <- input$review_candidates_rows_selected
    if (!length(row)) return()
    rows <- v04_plan_rows(review_details()$candidates)
    shiny::updateSelectInput(session, "review_rank", selected = rows$candidate_rank[[row]])
  })

  shiny::observeEvent(input$save_review, studio_notify_error(session, {
    modified <- NULL
    if (identical(input$review_decision, "modify")) {
      entry <- selected_modify_entry()
      modified <- list(
        transform_id = input$modify_transform,
        source_ref_ids = as.list(input$modify_sources %||% character()),
        target_variables = as.list(input$modify_targets %||% character()),
        parameters = studio_schema_value(entry$parameter_schema, paste0("param_", input$review_task), input),
        parameter_sources = NULL
      )
    }
    studio_with_project_lock(active_project(), studio_save_review_decision(
      active_config(), input$review_task, input$review_decision, input$reviewer_id,
      input$review_rank, input$review_comment, modified
    ))
    refresh(refresh() + 1L)
    shiny::showNotification("审核决定已通过校验并保存。", type = "message")
  }))

  shiny::observeEvent(input$approve_review, studio_notify_error(session, {
    studio_with_project_lock(active_project(), studio_approve_review(active_config(), input$reviewer_id))
    refresh(refresh() + 1L)
    shiny::showNotification("审核已批准，构建规格已锁定。", type = "message")
  }))

  output$download_review <- shiny::downloadHandler(
    filename = function() paste0(active_project(), "_", input$active_run, "_mapping_review.xlsx"),
    content = function(file) {
      source <- studio_write_review_workbook(active_config())
      file.copy(source, file, overwrite = TRUE)
    }
  )

  shiny::observeEvent(input$import_review, studio_notify_error(session, {
    shiny::req(input$import_review_file$datapath)
    studio_with_project_lock(active_project(), studio_import_review_workbook(active_config(), input$import_review_file$datapath, input$reviewer_id))
    refresh(refresh() + 1L)
    shiny::showNotification("Excel审核表已严格校验并批准。", type = "message")
  }))

  output$review_status_table <- DT::renderDT({
    config <- active_config(); refresh(); state <- studio_read_review(config)
    rows <- purrr::map_dfr(state$tasks, function(item) tibble::tibble(
      task_id = item$task_id, required = item$required, decision = item$decision,
      selected_rank = item$selected_rank %||% NA_integer_, reviewer = item$reviewer,
      reviewed_at = item$reviewed_at %||% "", valid = isTRUE(item$validation$valid)
    ))
    DT::datatable(rows, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 12))
  })

  run_summary <- shiny::reactive({ refresh(); studio_run_summary(active_config()) })
  output$dataset_manifest <- DT::renderDT(DT::datatable(run_summary()$datasets, rownames = FALSE, options = list(scrollX = TRUE)))
  output$local_issues <- DT::renderDT(DT::datatable(run_summary()$local_issues, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10)))
  output$p21_issues <- DT::renderDT(DT::datatable(run_summary()$p21_issues, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10)))
  output$output_files <- DT::renderDT({
    config <- active_config(); refresh(); root <- config$studio$run_path
    files <- unlist(lapply(c(config$paths$csv_dir, config$paths$xpt_dir), function(path) if (dir.exists(path)) list.files(path, full.names = TRUE) else character()))
    rows <- tibble::tibble(file = basename(files), relative_path = vapply(files, studio_relative_path, character(1), root = root), size = file.info(files)$size, sha256 = vapply(files, file_sha256, character(1)))
    DT::datatable(rows, rownames = FALSE, options = list(scrollX = TRUE))
  })

  shiny::observeEvent(input$import_p21, studio_notify_error(session, {
    shiny::req(input$p21_report_upload$datapath)
    studio_with_project_lock(active_project(), {
      studio_update_run_stage(active_project(), input$active_run, "validate_p21", "running")
      import_p21_report(input$p21_report_upload$datapath, active_config())
      studio_update_run_stage(active_project(), input$active_run, "validate_p21", "completed")
      studio_append_audit(active_project(), "p21_report_imported", list(report_sha256 = file_sha256(input$p21_report_upload$datapath)), input$active_run)
    })
    refresh(refresh() + 1L); shiny::showNotification("Pinnacle 21报告已导入。", type = "message")
  }))

  output$lineage_table <- DT::renderDT({
    data <- run_summary()$lineage; query <- tolower(trimws(input$lineage_filter %||% ""))
    if (nzchar(query) && nrow(data)) data <- data[apply(data, 1L, function(row) any(grepl(query, tolower(as.character(row)), fixed = TRUE))), , drop = FALSE]
    DT::datatable(data, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 15))
  })

  output$report_link <- shiny::renderUI({
    report <- run_summary()$report
    if (!file.exists(report)) return(shiny::p(class = "trace-muted", "尚未生成报告。"))
    prefix <- paste0("trace-report-", substr(digest::digest(dirname(report), serialize = FALSE), 1L, 10L))
    if (!prefix %in% names(shiny::resourcePaths())) {
      shiny::addResourcePath(prefix, dirname(report))
    }
    shiny::tags$a(href = paste0("/", prefix, "/", basename(report)), target = "_blank", class = "btn btn-outline-primary", "在新窗口打开离线报告")
  })

  output$download_evidence <- shiny::downloadHandler(
    filename = function() paste0(active_project(), "_", input$active_run, "_evidence.zip"),
    content = function(file) {
      source <- studio_with_project_lock(active_project(), studio_export_evidence(active_config(), session_key()))
      file.copy(source, file, overwrite = TRUE)
    }
  )

  output$studio_doctor_table <- DT::renderDT(DT::datatable(doctor_state(), rownames = FALSE, options = list(dom = "t", scrollX = TRUE)))
  shiny::observeEvent(input$refresh_doctor, doctor_state(studio_doctor()))

  session$onSessionEnded(function() {
    job <- studio_active_job()
    if (!is.null(job) && job$process$is_alive()) try(studio_cancel_job(), silent = TRUE)
    session_key("")
    invisible(TRUE)
  })
}

trace_studio_app <- function() shiny::shinyApp(ui = trace_studio_ui(), server = trace_studio_server)

launch_trace_studio <- function(port = NULL, launch_browser = TRUE) {
  settings <- studio_settings()
  if (!identical(as.character(settings$host), "127.0.0.1")) trace_abort("工作台配置必须固定监听127.0.0.1。")
  if (is.null(port)) port <- studio_find_port()
  options(shiny.maxRequestSize = as.numeric(settings$max_upload_mb) * 1024^2)
  recovered <- studio_recover_interrupted_jobs()
  if (length(recovered)) trace_info("已将 %d 个孤立任务标记为 interrupted。", length(recovered))
  trace_info("TraceSDTM Studio 正在运行：http://127.0.0.1:%s", port)
  shiny::runApp(trace_studio_app(), host = "127.0.0.1", port = as.integer(port), launch.browser = launch_browser)
}
