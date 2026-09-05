# TraceSDTM Studio 0.7 five-step application --------------------------------

trace_studio_ui <- function() {
  metadata <- yaml::read_yaml(trace_path("specs", "sdtm_metadata.yml"))
  domain_choices <- stats::setNames(names(metadata$domains), vapply(metadata$domains, function(item) item$label, character(1)))
  bslib::page_sidebar(
    title = shiny::div(class = "trace-title", "TraceSDTM Studio", shiny::tags$small("0.7")),
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
      details.trace-advanced{margin-top:14px}details.trace-advanced summary{cursor:pointer;font-weight:600}\
    "))),
    bslib::navset_card_tab(
      id = "main_nav",
      bslib::nav_panel("1 项目与数据",
        shiny::div(class = "trace-card",
          shiny::h3("创建通用项目"),
          shiny::fluidRow(
            shiny::column(3, shiny::textInput("new_project_id", "项目编号", placeholder = "例如 trace-demo")),
            shiny::column(3, shiny::textInput("new_project_name", "项目名称")),
            shiny::column(3, shiny::textInput("new_study_id", "研究编号", value = "TRACE001")),
            shiny::column(3, shiny::selectInput("new_standard", "标准及版本", choices = c("SDTMIG 3.4" = "SDTMIG|3.4")))
          ),
          shiny::textAreaInput("new_project_description", "项目描述", rows = 4, placeholder = "说明研究设计、来源数据和特殊映射要求"),
          shiny::selectizeInput("new_target_domains", "目标域（可多选；留空表示全部）", choices = domain_choices, multiple = TRUE),
          shiny::actionButton("create_project", "创建项目", class = "btn-primary")
        ),
        shiny::div(class = "trace-card",
          shiny::div(class = "d-flex justify-content-between", shiny::h3("项目"), shiny::checkboxInput("show_archived", "显示归档项目", FALSE)),
          DT::DTOutput("projects_table"),
          shiny::actionButton("archive_project", "归档当前项目", class = "btn-outline-warning")
        ),
        shiny::div(class = "trace-card",
          shiny::h3("当前项目设置"),
          shiny::fluidRow(
            shiny::column(4, shiny::textInput("edit_project_name", "项目名称")),
            shiny::column(4, shiny::textInput("edit_study_id", "研究编号")),
            shiny::column(4, shiny::selectInput("edit_standard", "标准及版本", choices = c("SDTMIG 3.4" = "SDTMIG|3.4")))
          ),
          shiny::textAreaInput("edit_project_description", "项目描述", rows = 3),
          shiny::selectizeInput("edit_target_domains", "目标域（可多选；留空表示全部）", choices = domain_choices, multiple = TRUE),
          shiny::actionButton("save_project_settings", "保存项目设置", class = "btn-outline-primary"),
          shiny::p(class = "trace-muted", "修改研究编号、描述、标准或目标域范围后，当前运行会立即标记为过期。")
        ),
        shiny::div(class = "trace-card",
          shiny::h3("原始数据与画像"),
          shiny::fileInput("source_upload", "上传一个或多个 UTF-8 CSV", accept = c(".csv", "text/csv"), multiple = TRUE),
          shiny::actionButton("upload_source", "导入文件", class = "btn-primary"),
          shiny::selectInput("source_dataset", "预览数据集", choices = character()),
          DT::DTOutput("source_preview"),
          shiny::hr(),
          shiny::actionButton("create_run_profile", "创建运行并生成画像", class = "btn-primary"),
          shiny::actionButton("profile_existing", "重新生成当前运行画像", class = "btn-outline-primary"),
          shiny::p(class = "trace-muted", "原文件只读保存；运行使用冻结副本。字段画像和关系候选由程序自动生成。")
        ),
        shiny::div(class = "trace-card",
          shiny::h3("可选分析规格"),
          shiny::fileInput("analysis_plan_upload", "导入 analysis_plan.yml", accept = c(".yml", ".yaml")),
          shiny::actionButton("import_analysis_plan", "校验并保存分析规格", class = "btn-outline-primary"),
          shiny::uiOutput("analysis_plan_status"),
          shiny::p(class = "trace-muted", "分析规格在创建运行时冻结；未配置时项目继续按 SDTM-only 模式使用。")
        ),
        shiny::div(class = "trace-card", shiny::h3("字段画像"), DT::DTOutput("profile_table")),
        shiny::div(class = "trace-card", shiny::h3("关系画像"), DT::DTOutput("relationship_table"))
      ),
      bslib::nav_panel("2 任务确认",
        shiny::div(class = "trace-card",
          shiny::h3("会话内模型配置"),
          shiny::fluidRow(
            shiny::column(4, shiny::textInput("model_base_url", "接口地址", value = Sys.getenv("TRACE_SDTM_BASE_URL", unset = ""))),
            shiny::column(3, shiny::textInput("model_name", "映射模型", value = Sys.getenv("TRACE_SDTM_MODEL", unset = ""))),
            shiny::column(5, shiny::passwordInput("model_api_key", "接口密钥（仅当前会话内存）"))
          ),
          shiny::fluidRow(
            shiny::column(4, shiny::textInput("review_base_url", "审查接口地址（可留空）", value = Sys.getenv("TRACE_SDTM_REVIEW_BASE_URL", unset = ""))),
            shiny::column(3, shiny::textInput("review_model_name", "审查模型（可留空）", value = Sys.getenv("TRACE_SDTM_REVIEW_MODEL", unset = ""))),
            shiny::column(5, shiny::textInput("reviewer_id", "审核者标识"))
          ),
          shiny::checkboxInput("include_examples", "明确授权发送所选去标识化示例值", FALSE),
          shiny::selectizeInput("example_source_keys", "可发送示例的来源字段", choices = character(), multiple = TRUE),
          shiny::p(class = "trace-muted", "默认不发送原始记录。标识符和键字段即使被选择也不会发送示例值。")
        ),
        shiny::div(class = "trace-card",
          shiny::h3("发现并确认原子任务"),
          shiny::actionButton("run_task_discovery", "人工智能生成任务草案", class = "btn-primary"),
          shiny::actionButton("accept_valid_tasks", "批量接受结构有效任务", class = "btn-outline-primary"),
          shiny::actionButton("exclude_selected_tasks", "排除表中所选任务", class = "btn-outline-warning"),
          shiny::actionButton("freeze_tasks", "冻结已确认任务", class = "btn-success"),
          DT::DTOutput("task_table"),
          DT::DTOutput("task_error_table")
        ),
        shiny::div(class = "trace-card",
          shiny::h3("逐项编辑"),
          shiny::selectInput("task_edit_id", "任务", choices = character()),
          shiny::textAreaInput("task_edit_action", "临床动作", rows = 3),
          shiny::fluidRow(
            shiny::column(4, shiny::selectInput("task_edit_domain", "目标域", choices = domain_choices)),
            shiny::column(4, shiny::selectInput("task_edit_cardinality", "记录粒度", choices = c(
              "一对一" = "one_record_to_one_record", "多对一" = "many_records_to_one_record",
              "一对多" = "one_record_to_many_records", "多对多" = "many_records_to_many_records",
              "数据集级" = "dataset_level", "无输出" = "no_output"
            ))),
            shiny::column(4, shiny::checkboxInput("task_edit_required", "必需任务", TRUE))
          ),
          shiny::selectizeInput("task_edit_sources", "来源字段", choices = character(), multiple = TRUE),
          shiny::selectizeInput("task_edit_dependencies", "依赖任务", choices = character(), multiple = TRUE),
          shiny::actionButton("save_task_edit", "保存并重新校验", class = "btn-primary")
        )
      ),
      bslib::nav_panel("3 映射生成",
        shiny::div(class = "trace-card",
          shiny::h3("目标、函数与参数"),
          shiny::actionButton("run_recommend_all", "一键生成映射", class = "btn-primary"),
          shiny::tags$details(class = "trace-advanced", shiny::tags$summary("高级阶段控制"),
            shiny::div(class = "d-flex gap-2 flex-wrap mt-3",
              shiny::actionButton("run_targets", "选择目标变量", class = "btn-outline-primary"),
              shiny::actionButton("run_functions", "选择函数", class = "btn-outline-primary"),
              shiny::actionButton("run_parameters", "解析参数", class = "btn-outline-primary"),
              shiny::actionButton("run_assemble", "组装计划", class = "btn-outline-primary")
            ),
            shiny::selectInput("preview_stage", "请求预览阶段", c("目标变量" = "targets", "函数" = "functions", "有限参数" = "parameters")),
            shiny::actionButton("preview_prompt", "生成请求预览及校验值"),
            shiny::verbatimTextOutput("prompt_hash"),
            shiny::textAreaInput("prompt_preview_text", "请求正文", value = "", rows = 12, width = "100%")
          )
        ),
        shiny::div(class = "trace-card",
          shiny::h3("需要人工填写的自由参数"),
          shiny::selectInput("manual_parameter_key", "候选任务", choices = character()),
          shiny::uiOutput("manual_parameter_form"),
          shiny::actionButton("save_manual_parameters", "保存人工参数", class = "btn-primary"),
          DT::DTOutput("parameter_status_table")
        ),
        shiny::div(class = "trace-card", shiny::h3("后台日志"), shiny::uiOutput("job_log"))
      ),
      bslib::nav_panel("4 审查批准",
        shiny::div(class = "trace-card",
          shiny::h3("独立人工智能审查"),
          shiny::actionButton("run_ai_review", "执行独立审查", class = "btn-primary"),
          DT::DTOutput("ai_review_table"),
          shiny::textAreaInput("ai_warning_comment", "警告确认说明", rows = 2),
          shiny::actionButton("ack_ai_warnings", "确认全部警告", class = "btn-outline-warning")
        ),
        shiny::div(class = "trace-card",
          shiny::h3("人工最终审核"),
          shiny::actionButton("accept_all_mappings", "批量接受首选映射", class = "btn-outline-primary"),
          shiny::fluidRow(
            shiny::column(4, shiny::selectInput("review_task", "原子任务", choices = character())),
            shiny::column(3, shiny::selectInput("review_decision", "决定", c("接受" = "accept", "修改" = "modify", "拒绝" = "reject", "信息不足" = "needs_information"))),
            shiny::column(2, shiny::selectInput("review_rank", "候选序号", choices = 1:3)),
            shiny::column(3, shiny::textInput("review_comment", "审核说明"))
          ),
          DT::DTOutput("review_candidates"),
          shiny::conditionalPanel("input.review_decision == 'modify'",
            shiny::selectInput("modify_transform", "转换函数", choices = character()),
            shiny::selectizeInput("modify_sources", "来源编号", choices = character(), multiple = TRUE),
            shiny::selectizeInput("modify_targets", "目标变量", choices = character(), multiple = TRUE),
            shiny::uiOutput("modify_parameters")
          ),
          shiny::actionButton("save_review", "保存当前任务审核", class = "btn-primary"),
          shiny::actionButton("approve_review", "最终批准", class = "btn-success"),
          shiny::downloadButton("download_review", "导出只读审核记录"),
          DT::DTOutput("review_status_table")
        )
      ),
      bslib::nav_panel("5 结果",
        bslib::accordion(
          open = c("SDTM"),
          bslib::accordion_panel("SDTM",
            shiny::actionButton("run_build", "生成批准的三个域", class = "btn-primary"),
            shiny::actionButton("run_local_validation", "运行 SDTM 本地检查", class = "btn-outline-primary"),
            shiny::p(class = "trace-muted", "构建只读取批准映射，不再调用模型。"),
            DT::DTOutput("dataset_manifest"), DT::DTOutput("output_files"), DT::DTOutput("local_issues")
          ),
          bslib::accordion_panel("ADaM",
            shiny::actionButton("run_build_adam", "生成 ADSL 与 ADAE", class = "btn-primary"),
            shiny::actionButton("run_validate_adam", "运行 ADaM 本地检查", class = "btn-outline-primary"),
            shiny::p(class = "trace-muted", "仅执行冻结分析规则；要求 SDTM 已批准、生成且本地检查无错误。"),
            DT::DTOutput("adam_manifest"), DT::DTOutput("adam_output_files"), DT::DTOutput("adam_issues")
          ),
          bslib::accordion_panel("汇总表",
            shiny::actionButton("run_build_tlf", "生成两张汇总表", class = "btn-primary"),
            shiny::actionButton("run_validate_tlf", "运行表格检查", class = "btn-outline-primary"),
            shiny::p(class = "trace-muted", "CSV、HTML 和 RTF 均来自同一个规范化结果对象。"),
            DT::DTOutput("tlf_manifest"), DT::DTOutput("tlf_output_files"), DT::DTOutput("tlf_issues"),
            shiny::uiOutput("tlf_preview_links")
          )
        ),
        shiny::div(class = "trace-card", shiny::h3("三级追溯"), shiny::textInput("lineage_filter", "筛选任务、域、变量或规则"),
                   DT::DTOutput("lineage_table"), DT::DTOutput("adam_lineage_table"), DT::DTOutput("tlf_lineage_table")),
        shiny::div(class = "trace-card", shiny::h3("报告与证据"),
          shiny::actionButton("run_report", "生成离线报告", class = "btn-outline-primary"),
          shiny::uiOutput("report_link"), shiny::downloadButton("download_evidence", "下载项目证据包")
        )
      )
    )
  )
}

studio_render_dt_v06 <- function(expr, env = parent.frame()) {
  expr <- substitute(expr)
  DT::renderDT(expr, env = env, quoted = TRUE, server = FALSE)
}

trace_studio_server <- function(input, output, session) {
  refresh <- shiny::reactiveVal(0L)
  prompt_state <- shiny::reactiveVal(NULL)
  session_key <- shiny::reactiveVal("")

  all_projects <- shiny::reactive({
    refresh()
    studio_list_projects(include_archived = isTRUE(input$show_archived))
  })
  projects <- shiny::reactive(dplyr::filter(all_projects(), .data$compatible))
  shiny::observe({
    data <- projects()
    choices <- stats::setNames(data$project_id, paste0(data$name, " [", data$project_id, "]"))
    selected <- if (nzchar(input$active_project %||% "") && input$active_project %in% data$project_id) input$active_project else if (nrow(data)) data$project_id[[1L]] else character()
    shiny::updateSelectInput(session, "active_project", choices = choices, selected = selected)
  })
  active_project <- shiny::reactive({ shiny::req(nzchar(input$active_project %||% "")); input$active_project })
  runs <- shiny::reactive({ refresh(); shiny::req(active_project()); studio_list_runs(active_project()) })
  shiny::observe({
    data <- runs(); project <- studio_read_project(active_project())
    choices <- stats::setNames(data$run_id, paste0(data$run_id, ifelse(data$stale, "（已过期）", "")))
    selected <- if (length(project$active_run_id) && project$active_run_id %in% data$run_id) project$active_run_id else if (nrow(data)) data$run_id[[1L]] else character()
    shiny::updateSelectInput(session, "active_run", choices = choices, selected = selected)
  })
  active_config <- shiny::reactive({
    shiny::req(active_project(), nzchar(input$active_run %||% ""))
    studio_load_run_config(active_project(), input$active_run)
  })

  output$projects_table <- studio_render_dt_v06(DT::datatable(all_projects(), selection = "single", rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE)))
  shiny::observeEvent(input$projects_table_rows_selected, {
    row <- input$projects_table_rows_selected
    if (!length(row)) return()
    selected <- all_projects()[row, , drop = FALSE]
    if (!isTRUE(selected$compatible[[1L]])) {
      shiny::showNotification("这是不兼容的旧版项目。请新建 0.7 项目并重新上传原始数据。", type = "warning", duration = 8)
    } else shiny::updateSelectInput(session, "active_project", selected = selected$project_id[[1L]])
  })
  output$sidebar_project_status <- shiny::renderUI({
    if (!nzchar(input$active_project %||% "")) return(shiny::p(class = "trace-muted", "尚未创建 0.7 项目"))
    project <- studio_read_project(input$active_project)
    shiny::tagList(
      shiny::strong(project$name), shiny::br(),
      shiny::span(class = "trace-muted", paste(project$standard, project$standard_version, "·", project$study_id)), shiny::br(),
      shiny::span(class = if (isTRUE(project$active_run_stale)) "trace-warn" else "trace-ok", if (isTRUE(project$active_run_stale)) "当前运行已过期" else "当前配置有效")
    )
  })
  output$sidebar_job_status <- shiny::renderUI({
    refresh(); job <- studio_active_job()
    if (is.null(job)) return(shiny::p(class = "trace-muted", "无后台任务"))
    shiny::tagList(shiny::strong(job$state$command), shiny::br(), shiny::span(class = "trace-warn", job$state$status))
  })
  shiny::observeEvent(input$refresh_all, refresh(refresh() + 1L))

  shiny::observeEvent(input$create_project, studio_notify_error(session, {
    standard_parts <- strsplit(input$new_standard %||% "SDTMIG|3.4", "|", fixed = TRUE)[[1L]]
    project <- studio_create_project(
      input$new_project_id, input$new_project_name, input$new_study_id,
      input$new_project_description, standard_parts[[1L]], standard_parts[[2L]], input$new_target_domains
    )
    refresh(refresh() + 1L)
    shiny::updateSelectInput(session, "active_project", selected = project$project_id)
    shiny::showNotification("0.7 通用项目已创建。", type = "message")
  }))
  shiny::observeEvent(input$archive_project, studio_notify_error(session, {
    studio_archive_project(active_project(), actor = input$reviewer_id %||% "local_user")
    refresh(refresh() + 1L)
    shiny::showNotification("项目已归档；文件和历史运行仍保留。", type = "message")
  }))
  shiny::observe({
    project <- studio_read_project(active_project())
    shiny::updateTextInput(session, "edit_project_name", value = project$name)
    shiny::updateTextInput(session, "edit_study_id", value = project$study_id)
    shiny::updateTextAreaInput(session, "edit_project_description", value = project$description)
    shiny::updateSelectInput(session, "edit_standard", selected = paste(project$standard, project$standard_version, sep = "|"))
    shiny::updateSelectizeInput(session, "edit_target_domains", selected = unlist(project$target_domains %||% character()), server = FALSE)
  })
  shiny::observeEvent(input$save_project_settings, studio_notify_error(session, {
    parts <- strsplit(input$edit_standard %||% "SDTMIG|3.4", "|", fixed = TRUE)[[1L]]
    studio_update_project_v06(
      active_project(), input$edit_project_name, input$edit_study_id, input$edit_project_description,
      parts[[1L]], parts[[2L]], input$edit_target_domains, input$reviewer_id %||% "local_user"
    )
    refresh(refresh() + 1L); shiny::showNotification("项目设置已保存；既有当前运行已标记为过期。", type = "message")
  }))

  sources <- shiny::reactive({ refresh(); shiny::req(active_project()); studio_read_source_catalog(active_project()) })
  shiny::observe({
    catalog <- sources()
    labels <- purrr::imap_chr(catalog, function(item, id) paste0(id, " — ", item$original_name))
    shiny::updateSelectInput(session, "source_dataset", choices = stats::setNames(names(catalog), labels))
  })
  shiny::observeEvent(input$upload_source, studio_notify_error(session, {
    shiny::req(input$source_upload$datapath)
    studio_import_sources(active_project(), input$source_upload$datapath, input$source_upload$name, input$reviewer_id %||% "local_user")
    refresh(refresh() + 1L)
    shiny::showNotification("CSV 已导入并生成数据集编号。", type = "message")
  }))
  shiny::observeEvent(input$import_analysis_plan, studio_notify_error(session, {
    shiny::req(input$analysis_plan_upload$datapath)
    studio_import_analysis_plan_v07(active_project(), input$analysis_plan_upload$datapath)
    refresh(refresh() + 1L)
    shiny::showNotification("分析规格已通过校验并保存；后续新运行会冻结该版本。", type = "message")
  }))
  output$analysis_plan_status <- shiny::renderUI({
    refresh(); shiny::req(active_project())
    project <- studio_read_project(active_project())
    if (!isTRUE(project$analysis_configured)) return(shiny::p(class = "trace-muted", "当前状态：未配置"))
    shiny::p(class = "trace-ok", paste0("当前状态：已配置；SHA-256 ", substr(project$analysis_plan_sha256 %||% "", 1L, 16L), "…"))
  })
  output$source_preview <- studio_render_dt_v06({
    shiny::req(active_project(), nzchar(input$source_dataset %||% "")); refresh()
    DT::datatable(studio_source_preview_v06(active_project(), input$source_dataset, 20L), rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
  })

  current_credentials <- function() {
    key <- input$model_api_key %||% ""
    if (nzchar(key)) session_key(key) else key <- session_key()
    if (!nzchar(key)) key <- Sys.getenv("TRACE_SDTM_API_KEY", unset = "")
    list(
      api_key = key, base_url = input$model_base_url, model = input$model_name,
      review_base_url = input$review_base_url, review_model = input$review_model_name,
      reviewer = input$reviewer_id
    )
  }
  start_job_for <- function(project_id, run_id, command) {
    studio_start_job(project_id, run_id, command, current_credentials(), list(
      include_examples = isTRUE(input$include_examples), source_keys = input$example_source_keys %||% character()
    ))
    refresh(refresh() + 1L)
  }
  start_job <- function(command) {
    config <- active_config()
    start_job_for(config$studio$project_id, config$studio$run_id, command)
  }
  shiny::observeEvent(input$create_run_profile, studio_notify_error(session, {
    project_id <- active_project()
    run_id <- studio_create_run(project_id, actor = input$reviewer_id %||% "local_user")
    refresh(refresh() + 1L)
    shiny::updateSelectInput(session, "active_run", selected = run_id)
    start_job_for(project_id, run_id, "profile")
    shiny::showNotification(paste("已创建运行并开始画像：", run_id), type = "message")
  }))
  shiny::observeEvent(input$profile_existing, studio_notify_error(session, start_job("profile")))
  shiny::observeEvent(input$run_task_discovery, studio_notify_error(session, start_job("discover-tasks")))
  shiny::observeEvent(input$run_targets, studio_notify_error(session, start_job("recommend-targets")))
  shiny::observeEvent(input$run_functions, studio_notify_error(session, start_job("recommend-functions")))
  shiny::observeEvent(input$run_parameters, studio_notify_error(session, start_job("recommend-parameters")))
  shiny::observeEvent(input$run_assemble, studio_notify_error(session, start_job("assemble-recommendations")))
  shiny::observeEvent(input$run_recommend_all, studio_notify_error(session, start_job("recommend")))
  shiny::observeEvent(input$run_ai_review, studio_notify_error(session, start_job("ai-review")))
  shiny::observeEvent(input$run_build, studio_notify_error(session, start_job("build")))
  shiny::observeEvent(input$run_local_validation, studio_notify_error(session, start_job("validate-local")))
  shiny::observeEvent(input$run_build_adam, studio_notify_error(session, start_job("build-adam")))
  shiny::observeEvent(input$run_validate_adam, studio_notify_error(session, start_job("validate-adam")))
  shiny::observeEvent(input$run_build_tlf, studio_notify_error(session, start_job("build-tlf")))
  shiny::observeEvent(input$run_validate_tlf, studio_notify_error(session, start_job("validate-tlf")))
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

  output$profile_table <- studio_render_dt_v06({
    config <- active_config(); refresh(); path <- file.path(trace_path(config$paths$profile_dir), "source_dictionary.csv"); shiny::req(file.exists(path))
    DT::datatable(readr::read_csv(path, show_col_types = FALSE), rownames = FALSE, options = list(scrollX = TRUE, pageLength = 15))
  })
  output$relationship_table <- studio_render_dt_v06({
    config <- active_config(); refresh(); path <- file.path(trace_path(config$paths$profile_dir), "source_relationships.csv"); shiny::req(file.exists(path))
    DT::datatable(readr::read_csv(path, show_col_types = FALSE), rownames = FALSE, options = list(scrollX = TRUE))
  })
  shiny::observe({
    config <- active_config(); refresh(); path <- file.path(trace_path(config$paths$profile_dir), "source_dictionary.csv")
    if (!file.exists(path)) return()
    dictionary <- readr::read_csv(path, show_col_types = FALSE)
    roles <- as.character(dictionary$concept_roles %||% dictionary$role %||% "")
    sensitive <- grepl("identifier|subject|site|center|(^|_)key($|_)", tolower(roles)) |
      grepl("^(STUDY|PATNUM|SUBJID|USUBJID|SITEID)$", dictionary$source_variable, ignore.case = TRUE)
    keys <- paste(dictionary$source_dataset, dictionary$source_variable, sep = ".")
    choices <- stats::setNames(keys[!sensitive], paste(dictionary$label[!sensitive], "[", keys[!sensitive], "]"))
    shiny::updateSelectizeInput(session, "example_source_keys", choices = choices, server = TRUE)
  })

  task_rows <- shiny::reactive({
    config <- active_config(); refresh()
    if (!file.exists(v06_task_draft_path(config))) return(tibble::tibble())
    studio_task_rows_v06(config)
  })
  output$task_table <- studio_render_dt_v06(DT::datatable(task_rows(), selection = "multiple", rownames = FALSE, options = list(scrollX = TRUE, pageLength = 12)))
  output$task_error_table <- studio_render_dt_v06({
    config <- active_config(); refresh()
    if (!file.exists(v06_task_validation_path(config))) return(DT::datatable(tibble::tibble(status = "尚未执行结构校验"), rownames = FALSE))
    errors <- read_json_file(v06_task_validation_path(config), list(errors = list()))$errors %||% list()
    if (!length(errors)) return(DT::datatable(tibble::tibble(status = "结构校验通过，无错误"), rownames = FALSE))
    rows <- purrr::map_dfr(errors, function(error) tibble::as_tibble(error))
    DT::datatable(rows, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 8))
  })
  shiny::observe({
    rows <- task_rows(); ids <- rows$task_id %||% character()
    shiny::updateSelectInput(session, "task_edit_id", choices = stats::setNames(ids, paste(ids, rows$target_domain %||% "", sep = " — ")))
  })
  task_edit_details <- shiny::reactive({
    config <- active_config(); shiny::req(nzchar(input$task_edit_id %||% ""))
    draft <- read_task_draft_v06(config)
    Filter(function(task) identical(task$task_id, input$task_edit_id), draft$tasks)[[1L]]
  })
  shiny::observe({
    task <- task_edit_details(); config <- active_config(); specification <- load_task_specification(config)
    source_keys <- unlist(purrr::imap(specification$source_catalog, function(source, id) paste(id, unlist(source$columns), sep = ".")), use.names = FALSE)
    all_ids <- vapply(read_task_draft_v06(config)$tasks, function(item) item$task_id, character(1))
    shiny::updateTextAreaInput(session, "task_edit_action", value = task$clinical_action)
    shiny::updateSelectInput(session, "task_edit_domain", choices = v06_allowed_domains(config), selected = task$target_domain)
    shiny::updateSelectInput(session, "task_edit_cardinality", selected = task$expected_cardinality)
    shiny::updateCheckboxInput(session, "task_edit_required", value = isTRUE(task$required))
    shiny::updateSelectizeInput(session, "task_edit_sources", choices = source_keys, selected = vapply(task$source_refs, function(ref) paste(ref$dataset, ref$variable, sep = "."), character(1)), server = TRUE)
    shiny::updateSelectizeInput(session, "task_edit_dependencies", choices = setdiff(all_ids, task$task_id), selected = unlist(task$depends_on), server = TRUE)
  })
  shiny::observeEvent(input$save_task_edit, studio_notify_error(session, {
    studio_with_project_lock(active_project(), studio_update_task_draft_v06(
      active_config(), input$task_edit_id, input$task_edit_action, input$task_edit_domain,
      input$task_edit_sources, input$task_edit_dependencies, input$task_edit_cardinality, input$task_edit_required
    ))
    refresh(refresh() + 1L); shiny::showNotification("任务已保存并重新校验。", type = "message")
  }))
  shiny::observeEvent(input$accept_valid_tasks, studio_notify_error(session, {
    config <- active_config(); rows <- task_rows(); draft <- read_task_draft_v06(config)
    invalid <- unique(vapply(draft$validation$errors %||% list(), function(error) as.character(error$task_id %||% ""), character(1)))
    ids <- setdiff(rows$task_id, invalid[nzchar(invalid)])
    studio_with_project_lock(active_project(), studio_set_task_decisions_v06(config, ids, "accept", input$reviewer_id))
    refresh(refresh() + 1L); shiny::showNotification("结构有效任务已批量接受。", type = "message")
  }))
  shiny::observeEvent(input$exclude_selected_tasks, studio_notify_error(session, {
    rows <- input$task_table_rows_selected; shiny::req(length(rows))
    studio_with_project_lock(active_project(), studio_set_task_decisions_v06(active_config(), task_rows()$task_id[rows], "exclude", input$reviewer_id))
    refresh(refresh() + 1L); shiny::showNotification("所选任务已排除。", type = "message")
  }))
  shiny::observeEvent(input$freeze_tasks, studio_notify_error(session, {
    studio_with_project_lock(active_project(), studio_freeze_tasks_v06(active_config(), input$reviewer_id))
    refresh(refresh() + 1L); shiny::showNotification("任务已冻结，可以生成映射。", type = "message")
  }))

  shiny::observeEvent(input$preview_prompt, studio_notify_error(session, {
    value <- studio_prompt_preview(active_config(), input$preview_stage, isTRUE(input$include_examples), input$example_source_keys %||% character())
    prompt_state(value); shiny::updateTextAreaInput(session, "prompt_preview_text", value = value$text)
  }))
  output$prompt_hash <- shiny::renderText({
    value <- prompt_state(); if (is.null(value)) "" else paste("SHA-256:", value$sha256, "| 示例值:", if (value$examples_included) "按授权筛选" else "未包含")
  })

  manual_options <- shiny::reactive({ refresh(); studio_manual_parameter_options_v06(active_config()) })
  shiny::observe({
    options <- manual_options(); keys <- names(options)
    if (!length(keys)) {
      shiny::updateSelectInput(session, "manual_parameter_key", choices = character())
      return()
    }
    labels <- vapply(options, function(item) paste0(item$task_id, " #", item$candidate_rank, " — ", paste(unlist(item$parameter_names), collapse = "、")), character(1))
    shiny::updateSelectInput(session, "manual_parameter_key", choices = stats::setNames(keys, labels))
  })
  manual_selected <- shiny::reactive({
    options <- manual_options(); shiny::req(nzchar(input$manual_parameter_key %||% ""), !is.null(options[[input$manual_parameter_key]])); options[[input$manual_parameter_key]]
  })
  output$manual_parameter_form <- shiny::renderUI({
    if (!length(manual_options())) return(shiny::p(class = "trace-ok", "没有需要人工填写的自由参数。"))
    item <- manual_selected()
    shiny::tagList(
      shiny::p(class = "trace-muted", paste("函数：", item$transform_id, "；程序已注入：", paste(names(item$injected_parameters), collapse = "、"))),
      studio_schema_ui(item$schema, paste0("manual_", item$key), list(), input)
    )
  })
  shiny::observeEvent(input$save_manual_parameters, studio_notify_error(session, {
    item <- manual_selected()
    values <- studio_schema_value(item$schema, paste0("manual_", item$key), input)
    studio_with_project_lock(active_project(), studio_save_manual_parameters_v06(active_config(), item$key, values, input$reviewer_id))
    refresh(refresh() + 1L); shiny::showNotification("人工参数已通过模式校验并保存。请重新组装计划。", type = "message")
  }))
  output$parameter_status_table <- studio_render_dt_v06({
    config <- active_config(); refresh(); path <- v06_parameter_results_path(config)
    if (!file.exists(path)) return(DT::datatable(tibble::tibble(status = "尚未执行参数解析"), rownames = FALSE))
    result <- read_json_file(path, list())
    rows <- purrr::imap_dfr(result$resolutions$valid %||% list(), function(item, key) tibble::tibble(
      candidate = key, task_id = item$task_id, transform_id = item$transform_id,
      auto_injected = paste(names(item$injected_parameters %||% list()), collapse = "、"),
      finite_model = paste(unlist(item$unresolved_parameters %||% character()), collapse = "、"),
      manual_required = paste(unlist(item$unavailable_parameters %||% character()), collapse = "、"), status = item$status
    ))
    DT::datatable(rows, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
  })

  shiny::observe({
    config <- active_config(); refresh(); assembled <- file.path(trace_path(config$paths$recommendation_dir), "assembled_recommendations.json")
    if (!file.exists(assembled)) return()
    state <- studio_initialize_review(config)
    tasks <- names(state$tasks); labels <- vapply(tasks, function(id) paste0(id, " — ", state$tasks[[id]]$decision), character(1))
    shiny::updateSelectInput(session, "review_task", choices = stats::setNames(tasks, labels))
  })
  review_details <- shiny::reactive({ shiny::req(input$review_task); studio_review_task_details(active_config(), input$review_task) })
  output$review_candidates <- studio_render_dt_v06({
    rows <- plan_rows(review_details()$candidates)
    DT::datatable(rows, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 5), selection = "single")
  })
  shiny::observe({
    details <- review_details(); item <- details$state
    shiny::updateSelectInput(session, "review_decision", selected = if (item$decision == "pending") "accept" else item$decision)
    shiny::updateSelectInput(session, "review_rank", selected = item$selected_rank %||% 1L)
    shiny::updateTextInput(session, "review_comment", value = item$review_comment %||% "")
    options <- studio_review_editor_options(active_config(), input$review_task)
    shiny::updateSelectInput(session, "modify_transform", choices = options$transforms)
    shiny::updateSelectizeInput(session, "modify_sources", choices = options$source_refs, server = TRUE)
    shiny::updateSelectizeInput(session, "modify_targets", choices = options$target_variables, server = TRUE)
  })
  selected_modify_entry <- shiny::reactive({ shiny::req(input$modify_transform); registry_entry(input$modify_transform, load_transform_registry(active_config())) })
  output$modify_parameters <- shiny::renderUI({
    entry <- selected_modify_entry(); current <- review_details()$state$modified_step
    value <- if (!is.null(current) && identical(current$transform_id, entry$transform_id)) current$parameters else list()
    studio_schema_ui(entry$parameter_schema, paste0("param_", input$review_task), value, input)
  })
  shiny::observeEvent(input$review_candidates_rows_selected, {
    row <- input$review_candidates_rows_selected
    if (length(row)) shiny::updateSelectInput(session, "review_rank", selected = plan_rows(review_details()$candidates)$candidate_rank[[row]])
  })
  save_one_review <- function(task_id, decision, rank, comment, modified = NULL) {
    studio_save_review_decision(active_config(), task_id, decision, input$reviewer_id, rank, comment, modified)
  }
  shiny::observeEvent(input$save_review, studio_notify_error(session, {
    modified <- NULL
    if (identical(input$review_decision, "modify")) {
      entry <- selected_modify_entry()
      modified <- list(
        transform_id = input$modify_transform, source_ref_ids = as.list(input$modify_sources %||% character()),
        target_variables = as.list(input$modify_targets %||% character()),
        parameters = studio_schema_value(entry$parameter_schema, paste0("param_", input$review_task), input), parameter_sources = NULL
      )
    }
    studio_with_project_lock(active_project(), save_one_review(input$review_task, input$review_decision, input$review_rank, input$review_comment, modified))
    refresh(refresh() + 1L); shiny::showNotification("人工审核决定已保存。映射变更后需重新执行独立审查。", type = "message")
  }))
  shiny::observeEvent(input$accept_all_mappings, studio_notify_error(session, {
    config <- active_config(); state <- studio_read_review(config)
    studio_with_project_lock(active_project(), {
      for (id in names(state$tasks)) studio_save_review_decision(config, id, "accept", input$reviewer_id, state$tasks[[id]]$selected_rank %||% 1L, "批量接受首选映射")
    })
    refresh(refresh() + 1L); shiny::showNotification("首选映射已批量接受。", type = "message")
  }))
  output$review_status_table <- studio_render_dt_v06({
    config <- active_config(); refresh()
    if (!file.exists(file.path(trace_path(config$paths$recommendation_dir), "assembled_recommendations.json"))) return(DT::datatable(tibble::tibble(status = "尚未组装映射计划"), rownames = FALSE))
    state <- studio_read_review(config)
    rows <- purrr::map_dfr(state$tasks, function(item) tibble::tibble(
      task_id = item$task_id, required = item$required, decision = item$decision,
      selected_rank = item$selected_rank %||% NA_integer_, reviewer = item$reviewer,
      reviewed_at = item$reviewed_at %||% "", valid = isTRUE(item$validation$valid)
    ))
    DT::datatable(rows, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 12))
  })
  output$ai_review_table <- studio_render_dt_v06({
    config <- active_config(); refresh(); path <- v06_ai_review_path(config)
    if (!file.exists(path)) return(DT::datatable(tibble::tibble(status = "尚未执行独立人工智能审查"), rownames = FALSE))
    review <- read_ai_review_v06(config)
    model_rows <- purrr::map_dfr(review$task_reviews, function(item) {
      if (!length(item$issues %||% list())) return(tibble::tibble(task_id = item$task_id, status = item$status, issue_id = "", severity = "", location = "", message = "", suggested_resolution = ""))
      purrr::map_dfr(item$issues, function(issue) tibble::tibble(task_id = item$task_id, status = item$status, !!!issue))
    })
    program_rows <- purrr::map_dfr(review$deterministic_validation$issues %||% list(), function(issue) tibble::tibble(
      task_id = issue$task_id, status = "error", issue_id = issue$code, severity = "error",
      location = issue$location, message = issue$message, suggested_resolution = "按程序错误修改映射后重新校验"
    ))
    DT::datatable(dplyr::bind_rows(program_rows, model_rows), rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
  })
  shiny::observeEvent(input$ack_ai_warnings, studio_notify_error(session, {
    studio_with_project_lock(active_project(), studio_acknowledge_ai_warnings_v06(active_config(), input$reviewer_id, input$ai_warning_comment))
    refresh(refresh() + 1L); shiny::showNotification("人工智能警告已记录确认说明。", type = "message")
  }))
  shiny::observeEvent(input$approve_review, studio_notify_error(session, {
    studio_with_project_lock(active_project(), studio_approve_review(active_config(), input$reviewer_id))
    refresh(refresh() + 1L); shiny::showNotification("映射已最终批准并冻结。", type = "message")
  }))
  output$download_review <- shiny::downloadHandler(
    filename = function() paste0(active_project(), "_", input$active_run, "_mapping_review.xlsx"),
    content = function(file) { source <- studio_write_review_workbook(active_config()); file.copy(source, file, overwrite = TRUE) }
  )

  run_summary <- shiny::reactive({ refresh(); studio_run_summary(active_config()) })
  output$dataset_manifest <- studio_render_dt_v06(DT::datatable(run_summary()$datasets, rownames = FALSE, options = list(scrollX = TRUE)))
  output$adam_manifest <- studio_render_dt_v06(DT::datatable(run_summary()$adam_datasets, rownames = FALSE, options = list(scrollX = TRUE)))
  output$tlf_manifest <- studio_render_dt_v06(DT::datatable(run_summary()$tables, rownames = FALSE, options = list(scrollX = TRUE)))
  output$local_issues <- studio_render_dt_v06(DT::datatable(run_summary()$local_issues, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10)))
  output$adam_issues <- studio_render_dt_v06(DT::datatable(run_summary()$adam_issues, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10)))
  output$tlf_issues <- studio_render_dt_v06(DT::datatable(run_summary()$tlf_issues, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10)))
  output$output_files <- studio_render_dt_v06({
    config <- active_config(); refresh(); root <- config$studio$run_path
    files <- unlist(lapply(c(config$paths$csv_dir, config$paths$xpt_dir), function(path) if (dir.exists(path)) list.files(path, full.names = TRUE) else character()))
    rows <- tibble::tibble(file = basename(files), relative_path = vapply(files, studio_relative_path, character(1), root = root), size = file.info(files)$size, sha256 = vapply(files, file_sha256, character(1)))
    DT::datatable(rows, rownames = FALSE, options = list(scrollX = TRUE))
  })
  output$adam_output_files <- studio_render_dt_v06({
    config <- active_config(); refresh(); root <- config$studio$run_path
    files <- unlist(lapply(c(config$paths$adam_csv_dir, config$paths$adam_xpt_dir), function(path) if (dir.exists(path)) list.files(path, full.names = TRUE) else character()))
    rows <- tibble::tibble(file = basename(files), relative_path = vapply(files, studio_relative_path, character(1), root = root), size = file.info(files)$size, sha256 = vapply(files, file_sha256, character(1)))
    DT::datatable(rows, rownames = FALSE, options = list(scrollX = TRUE))
  })
  output$tlf_output_files <- studio_render_dt_v06({
    config <- active_config(); refresh(); root <- config$studio$run_path
    files <- unlist(lapply(c(config$paths$tlf_csv_dir, config$paths$tlf_html_dir, config$paths$tlf_rtf_dir), function(path) if (dir.exists(path)) list.files(path, full.names = TRUE) else character()))
    rows <- tibble::tibble(file = basename(files), relative_path = vapply(files, studio_relative_path, character(1), root = root), size = file.info(files)$size, sha256 = vapply(files, file_sha256, character(1)))
    DT::datatable(rows, rownames = FALSE, options = list(scrollX = TRUE))
  })
  output$lineage_table <- studio_render_dt_v06({
    data <- run_summary()$lineage; query <- tolower(trimws(input$lineage_filter %||% ""))
    if (nzchar(query) && nrow(data)) data <- data[apply(data, 1L, function(row) any(grepl(query, tolower(as.character(row)), fixed = TRUE))), , drop = FALSE]
    DT::datatable(data, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 15))
  })
  output$adam_lineage_table <- studio_render_dt_v06({
    data <- run_summary()$adam_lineage; query <- tolower(trimws(input$lineage_filter %||% ""))
    if (nzchar(query) && nrow(data)) data <- data[apply(data, 1L, function(row) any(grepl(query, tolower(as.character(row)), fixed = TRUE))), , drop = FALSE]
    DT::datatable(data, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 15))
  })
  output$tlf_lineage_table <- studio_render_dt_v06({
    data <- run_summary()$tlf_lineage; query <- tolower(trimws(input$lineage_filter %||% ""))
    if (nzchar(query) && nrow(data)) data <- data[apply(data, 1L, function(row) any(grepl(query, tolower(as.character(row)), fixed = TRUE))), , drop = FALSE]
    DT::datatable(data, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 15))
  })
  output$tlf_preview_links <- shiny::renderUI({
    config <- active_config(); refresh()
    files <- list.files(trace_path(config$paths$tlf_html_dir), pattern = "[.]html$", full.names = TRUE)
    if (!length(files)) return(shiny::p(class = "trace-muted", "尚未生成 HTML 表格。"))
    prefix <- paste0("trace-tlf-", substr(digest::digest(dirname(files[[1L]]), serialize = FALSE), 1L, 10L))
    if (!prefix %in% names(shiny::resourcePaths())) shiny::addResourcePath(prefix, dirname(files[[1L]]))
    shiny::tagList(lapply(files, function(file) shiny::tags$a(
      href = paste0("/", prefix, "/", basename(file)), target = "_blank",
      class = "btn btn-outline-primary me-2", paste("预览", basename(file))
    )))
  })
  output$report_link <- shiny::renderUI({
    report <- run_summary()$report
    if (!file.exists(report)) return(shiny::p(class = "trace-muted", "尚未生成报告。"))
    prefix <- paste0("trace-report-", substr(digest::digest(dirname(report), serialize = FALSE), 1L, 10L))
    if (!prefix %in% names(shiny::resourcePaths())) shiny::addResourcePath(prefix, dirname(report))
    shiny::tags$a(href = paste0("/", prefix, "/", basename(report)), target = "_blank", class = "btn btn-outline-primary", "打开离线报告")
  })
  output$download_evidence <- shiny::downloadHandler(
    filename = function() paste0(active_project(), "_", input$active_run, "_evidence.zip"),
    content = function(file) { source <- studio_with_project_lock(active_project(), studio_export_evidence(active_config(), session_key())); file.copy(source, file, overwrite = TRUE) }
  )
  session$onSessionEnded(function() {
    job <- studio_active_job(); if (!is.null(job) && job$process$is_alive()) try(studio_cancel_job(), silent = TRUE)
    session_key(""); invisible(TRUE)
  })
}

trace_studio_app <- function() {
  shiny::shinyApp(ui = trace_studio_ui(), server = trace_studio_server)
}

launch_trace_studio <- function(port = NULL, launch_browser = TRUE) {
  settings <- studio_settings()
  if (!identical(as.character(settings$host), "127.0.0.1")) {
    trace_abort("工作台配置必须固定监听127.0.0.1。")
  }
  if (is.null(port)) port <- studio_find_port()
  options(shiny.maxRequestSize = as.numeric(settings$max_upload_mb) * 1024^2)
  recovered <- studio_recover_interrupted_jobs()
  if (length(recovered)) trace_info("已将 %d 个孤立任务标记为 interrupted。", length(recovered))
  trace_info("TraceSDTM Studio 正在运行：http://127.0.0.1:%s", port)
  shiny::runApp(
    trace_studio_app(), host = "127.0.0.1", port = as.integer(port),
    launch.browser = launch_browser
  )
}
