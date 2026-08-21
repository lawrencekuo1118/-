# Control Design Assistant — compact UI + named drafts
# Run: shiny::runApp("control-design-app")

library(shiny)
library(bslib)
library(DT)
write_json <- jsonlite::write_json
read_json <- jsonlite::read_json

.source_root <- local({
  cmd <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd, value = TRUE)
  if (length(file_arg)) {
    dirname(normalizePath(sub("^--file=", "", file_arg)))
  } else if (!is.null(sys.frames()[[1]]$ofile)) {
    dirname(normalizePath(sys.frames()[[1]]$ofile))
  } else {
    getwd()
  }
})
root <- .source_root
data_dir <- file.path(root, "data")
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

source(file.path(root, "R", "constants.R"), local = TRUE)
source(file.path(root, "R", "assemble.R"), local = TRUE)
source(file.path(root, "R", "pbc_registry.R"), local = TRUE)
source(file.path(root, "R", "rcm_csa.R"), local = TRUE)
source(file.path(root, "R", "library.R"), local = TRUE)
source(file.path(root, "R", "draft_store.R"), local = TRUE)

fill_inputs_from_ctrl <- function(session, ctrl) {
  if (is.null(ctrl)) return()
  updateSelectInput(session, "cycle", selected = ctrl$cycle %||% CYCLES_NINE[[1]])
  updateTextInput(session, "risk_name", value = ctrl$risk_name %||% "")
  updateTextAreaInput(session, "risk_description", value = ctrl$risk_description %||% "")
  updateTextAreaInput(session, "risk_attr_financial",
                      value = gsub("^\\[[^\\]]+\\]\\s*", "", ctrl$risk_attr_financial %||% ""))
  updateTextAreaInput(session, "risk_attr_operations",
                      value = gsub("^\\[[^\\]]+\\]\\s*", "", ctrl$risk_attr_operations %||% ""))
  updateTextAreaInput(session, "risk_attr_compliance",
                      value = gsub("^\\[[^\\]]+\\]\\s*", "", ctrl$risk_attr_compliance %||% ""))
  updateTextInput(session, "significant_account", value = ctrl$significant_account %||% "")
  updateTextInput(session, "control_id", value = ctrl$control_id %||% ctrl$library_id %||% "")
  updateTextInput(session, "responsible_unit", value = ctrl$responsible_unit %||% "")
  updateTextAreaInput(session, "control_objective", value = ctrl$control_objective %||% "")
  updateTextAreaInput(session, "control_activity", value = ctrl$control_activity %||% "")
  updateTextAreaInput(session, "iuc_or_system", value = ctrl$iuc_or_system %||% "")
  updateSelectizeInput(session, "nature", selected = ctrl$nature %||% NATURE_CHOICES[[1]])
  updateSelectizeInput(session, "approach", selected = ctrl$approach %||% APPROACH_CHOICES[[1]])
  updateSelectizeInput(session, "type", selected = ctrl$type %||% TYPE_CHOICES[[1]])
  updateTextAreaInput(session, "inputs", value = ctrl$inputs %||% "")
  updateTextAreaInput(session, "review_steps", value = ctrl$review_steps %||% "")
  updateTextAreaInput(session, "outputs", value = ctrl$outputs %||% "")
  updateTextAreaInput(session, "investigation_threshold", value = ctrl$investigation_threshold %||% "")
  if (!is.null(ctrl$frequency)) updateSelectInput(session, "frequency", selected = ctrl$frequency)
}

ui <- page_navbar(
  title = "控制設計",
  theme = bs_theme(
    version = 5, bootswatch = "flatly", primary = "#1B4F72",
    base_font = font_google("Noto Sans TC"),
    "font-size-base" = "0.9rem"
  ),
  fillable = TRUE,
  sidebar = sidebar(
    width = 280,
    open = "desktop",
    selectInput("cycle", NULL, choices = CYCLES_NINE),
    textInput("lib_query", NULL, placeholder = "搜尋完美範本…"),
    selectInput("lib_pick", NULL, choices = c("① 優先：從範本庫套用…" = "")),
    div(
      class = "d-flex gap-1 flex-wrap",
      actionButton("apply_lib", "套用", class = "btn-sm btn-primary"),
      actionButton("save_to_lib", "存入庫", class = "btn-sm btn-outline-success")
    ),
    tags$hr(class = "my-2"),
    textInput("risk_name", NULL, placeholder = "風險名稱"),
    textAreaInput("risk_description", NULL, rows = 2, placeholder = "風險描述（RoMM）"),
    textInput("company", NULL, placeholder = "公司名稱"),
    tags$hr(class = "my-2"),
    tags$strong("草稿"),
    textInput("draft_name", NULL, placeholder = "草稿名稱"),
    selectInput("draft_pick", NULL, choices = c("已存草稿…" = "")),
    div(
      class = "d-flex gap-1 flex-wrap",
      actionButton("save_draft_file", "儲存", class = "btn-sm btn-primary"),
      actionButton("load_draft_file", "載入", class = "btn-sm btn-outline-secondary"),
      actionButton("delete_draft_file", "刪除", class = "btn-sm btn-outline-danger")
    ),
    checkboxInput("autosave_draft", "自動儲存", TRUE),
    uiOutput("draft_status"),
    downloadButton("download_json", "匯出 JSON", class = "btn-sm w-100 mt-1")
  ),
  nav_panel(
    "設計",
    layout_columns(
      col_widths = c(7, 5),
      card(
        full_screen = TRUE,
        textInput("control_id", NULL, value = "CD-001", placeholder = "控制點 ID"),
        textAreaInput("control_objective", NULL, rows = 2, placeholder = "控制目標（Why）"),
        textAreaInput("control_activity", NULL, rows = 2, placeholder = "控制活動（How，勿重述目標）"),
        layout_columns(
          col_widths = c(6, 6),
          selectInput("frequency", NULL, choices = FREQUENCY_CHOICES, selected = FREQUENCY_CHOICES[[4]]),
          textInput("responsible_unit", NULL, placeholder = "負責單位")
        ),
        selectizeInput(
          "pbc_apply", NULL, choices = NULL, multiple = TRUE,
          options = list(placeholder = "套用 IUC／PBC（原名→新名）")
        ),
        textAreaInput("iuc_or_system", NULL, rows = 1, placeholder = "IUC／制度（不同則分拆）"),
        accordion(
          open = FALSE,
          accordion_panel(
            "進階",
            layout_columns(
              col_widths = c(4, 8),
              textInput("attr_label_fr", NULL, value = "財務報導"),
              textAreaInput("risk_attr_financial", NULL, rows = 1, placeholder = "屬性1")
            ),
            layout_columns(
              col_widths = c(4, 8),
              textInput("attr_label_op", NULL, value = "營運"),
              textAreaInput("risk_attr_operations", NULL, rows = 1, placeholder = "屬性2")
            ),
            layout_columns(
              col_widths = c(4, 8),
              textInput("attr_label_cp", NULL, value = "法令遵循"),
              textAreaInput("risk_attr_compliance", NULL, rows = 1, placeholder = "屬性3")
            ),
            textInput("significant_account", NULL, placeholder = "重大科目"),
            selectizeInput(
              "assertions", NULL, choices = ASSERTION_CHOICES, multiple = TRUE,
              selected = ASSERTION_CHOICES[1:2],
              options = list(create = TRUE, placeholder = "聲明")
            ),
            selectInput("romm_classification", NULL, choices = ROMM_CLASS_CHOICES),
            layout_columns(
              col_widths = c(4, 4, 4),
              selectizeInput("nature", NULL, choices = NATURE_CHOICES,
                             options = list(create = TRUE, placeholder = "Nature")),
              selectizeInput("approach", NULL, choices = APPROACH_CHOICES,
                             options = list(create = TRUE, placeholder = "Approach")),
              selectizeInput("type", NULL, choices = TYPE_CHOICES,
                             options = list(create = TRUE, placeholder = "Type"))
            ),
            textAreaInput("inputs", NULL, rows = 1, placeholder = "Inputs"),
            textAreaInput("review_steps", NULL, rows = 3, placeholder = "Steps（每行一步）"),
            textAreaInput("outputs", NULL, rows = 1, placeholder = "Outputs"),
            textAreaInput("investigation_threshold", NULL, rows = 1, placeholder = "調查門檻"),
            checkboxInput("pbc_also_inputs", "套用 PBC 時寫入 Inputs 對照", TRUE)
          )
        ),
        div(
          class = "d-flex gap-1 flex-wrap mt-2",
          actionButton("add_draft", "加入", class = "btn-primary btn-sm"),
          actionButton("update_draft", "更新", class = "btn-sm"),
          actionButton("remove_draft", "刪除列", class = "btn-sm btn-outline-danger"),
          actionButton("generate_controls", "產生控制點", class = "btn-success btn-sm")
        )
      ),
      card(
        uiOutput("live_validation"),
        verbatimTextOutput("live_preview"),
        navset_underline(
          nav_panel("佇列", DTOutput("draft_table")),
          nav_panel("控制點", DTOutput("control_table"), verbatimTextOutput("control_paragraph"))
        )
      )
    )
  ),
  nav_panel(
    "範本庫",
    layout_columns(
      col_widths = c(4, 8),
      card(
        p(class = "text-muted small mb-1",
          "大量完美控制點可 CSV／JSON 匯入；設計時優先從此庫套用。亦可將目前表單或已產生控制點存入（累積制）。"),
        fileInput("upload_lib", NULL, buttonLabel = "匯入 CSV／JSON",
                  accept = c(".csv", ".json")),
        checkboxInput("lib_overwrite", "同 ID 則覆蓋", TRUE),
        div(
          class = "d-flex gap-1 flex-wrap",
          actionButton("lib_add_current", "表單→庫", class = "btn-sm btn-primary"),
          actionButton("lib_add_selected_control", "控制點→庫", class = "btn-sm"),
          actionButton("lib_delete", "刪除選取", class = "btn-sm btn-outline-danger"),
          downloadButton("download_lib_csv", "匯出 CSV", class = "btn-sm"),
          downloadButton("download_lib_json", "匯出 JSON", class = "btn-sm")
        ),
        textInput("lib_title_override", NULL, placeholder = "存入時標題（可空）"),
        textInput("lib_tags", NULL, placeholder = "標籤（;分隔）")
      ),
      card(
        DTOutput("lib_table"),
        verbatimTextOutput("lib_preview")
      )
    )
  ),
  nav_panel(
    "PBC",
    layout_columns(
      col_widths = c(4, 8),
      card(
        textInput("pbc_client", NULL, placeholder = "客戶取得原名"),
        textInput("pbc_reviewed", NULL, placeholder = "檢視後新命名"),
        textInput("pbc_id", NULL, placeholder = "ID（可空）"),
        selectInput("pbc_cycle", NULL, choices = c("循環（共用）" = "", CYCLES_NINE)),
        textInput("pbc_notes", NULL, placeholder = "備註"),
        div(
          class = "d-flex gap-1 flex-wrap",
          actionButton("pbc_add", "登錄", class = "btn-primary btn-sm"),
          actionButton("pbc_delete", "刪除", class = "btn-outline-danger btn-sm"),
          downloadButton("download_pbc", "匯出", class = "btn-sm")
        ),
        fileInput("upload_pbc", NULL, buttonLabel = "匯入 CSV", accept = ".csv")
      ),
      card(DTOutput("pbc_table"), verbatimTextOutput("pbc_all_status"))
    )
  ),
  nav_panel(
    "RCM",
    card(
      p(class = "text-muted small mb-1", "目標＝Why；活動＝How。兩欄不得相同。"),
      DTOutput("rcm_table"),
      downloadButton("download_rcm", "下載 RCM", class = "btn-sm"),
      tags$hr(),
      tags$strong("缺漏"),
      DTOutput("gap_table")
    )
  ),
  nav_panel(
    "訪談／CSA",
    layout_columns(
      col_widths = c(3, 9),
      card(
        selectizeInput(
          "worksheet_controls", NULL, choices = NULL, multiple = TRUE,
          options = list(placeholder = "控制點（空＝全部）")
        ),
        checkboxGroupInput("interview_elements", "訪談元素",
                           choices = DESIGN_ELEMENTS, selected = DEFAULT_INTERVIEW_ELEMENTS),
        checkboxGroupInput("csa_elements", "CSA 元素",
                           choices = DESIGN_ELEMENTS, selected = DEFAULT_CSA_ELEMENTS),
        div(
          class = "d-flex gap-1 flex-wrap",
          actionButton("ws_select_core_iv", "訪談核心", class = "btn-sm"),
          actionButton("ws_select_core_csa", "CSA 核心", class = "btn-sm")
        )
      ),
      navset_card_underline(
        nav_panel(
          "訪談", DTOutput("interview_table"),
          downloadButton("download_interview", "下載", class = "btn-sm")
        ),
        nav_panel(
          "CSA", DTOutput("csa_table"),
          downloadButton("download_csa", "下載", class = "btn-sm")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  drafts <- reactiveVal(list())
  controls <- reactiveVal(list())
  pbc_path_csv <- file.path(data_dir, "pbc_registry.csv")
  pbc_path_json <- file.path(data_dir, "pbc_registry.json")
  pbc_reg <- reactiveVal(load_pbc_registry(pbc_path_csv, pbc_path_json))
  lib_path_json <- file.path(data_dir, "control_library.json")
  lib_path_csv <- file.path(data_dir, "control_library.csv")
  # Seed once if missing, then always load persisted (accumulative)
  if (!file.exists(lib_path_json)) {
    save_control_library(seed_control_library(), lib_path_json, lib_path_csv)
  }
  lib <- reactiveVal(load_control_library(lib_path_json, fallback_seed = TRUE))
  next_id <- reactiveVal(1L)
  last_saved_at <- reactiveVal(NULL)
  draft_tick <- reactiveVal(0L)

  persist_pbc <- function(reg) save_pbc_registry(reg, pbc_path_csv, pbc_path_json)
  persist_lib <- function(library) {
    save_control_library(library, lib_path_json, lib_path_csv)
    library
  }

  refresh_lib_choices <- function() {
    ch <- library_choices(lib(), cycle_filter = input$cycle, query = input$lib_query)
    updateSelectInput(
      session, "lib_pick",
      choices = c("① 優先：從範本庫套用…" = "", ch),
      selected = {
        cur <- input$lib_pick %||% ""
        if (nzchar(cur) && cur %in% ch) cur else ""
      }
    )
  }

  refresh_draft_list <- function(selected = NULL) {
    df <- list_saved_drafts(data_dir)
    draft_tick(draft_tick() + 1L)
    ch <- c("已存草稿…" = "")
    if (nrow(df)) {
      labels <- sprintf("%s（%s｜Q%d／C%d）", df$name, df$saved_at, df$n_drafts, df$n_controls)
      ch <- c(ch, stats::setNames(df$path, labels))
    }
    updateSelectInput(session, "draft_pick", choices = ch,
                      selected = if (!is.null(selected) && selected %in% ch) selected else "")
  }

  refresh_pbc_choices <- function() {
    ch <- pbc_choices(pbc_reg(), cycle_filter = input$cycle)
    updateSelectizeInput(
      session, "pbc_apply", choices = ch, server = TRUE,
      selected = intersect(input$pbc_apply %||% character(), unname(ch))
    )
  }

  observe({
    input$cycle
    input$lib_query
    refresh_lib_choices()
  })
  observe({
    input$cycle
    refresh_pbc_choices()
  })
  observe({
    refresh_draft_list()
  })

  # Auto-resume last session on start
  observeEvent(TRUE, {
    legacy <- file.path(data_dir, "session_draft.json")
    if (file.exists(legacy)) {
      try(apply_payload(load_draft_payload(legacy), notify = FALSE), silent = TRUE)
    }
  }, once = TRUE)

  observeEvent(input$pbc_apply, {
    ids <- input$pbc_apply
    if (!length(ids)) return()
    updateTextAreaInput(session, "iuc_or_system", value = apply_pbc_to_iuc(pbc_reg(), ids))
    if (isTRUE(input$pbc_also_inputs)) {
      mapped <- format_pbc_for_inputs(pbc_reg(), ids)
      cur <- trimws(input$inputs %||% "")
      if (grepl("【IUC／PBC 命名對照】", cur, fixed = TRUE)) {
        cur <- trimws(sub("【IUC／PBC 命名對照】[\\s\\S]*$", "", cur))
      }
      new_inputs <- if (nzchar(cur)) paste(cur, mapped, sep = "\n") else mapped
      updateTextAreaInput(session, "inputs", value = new_inputs)
    }
  }, ignoreInit = TRUE)

  output$pbc_all_status <- renderText({
    lines <- format_pbc_status_lines(pbc_reg())
    if (!length(lines)) "（命名庫尚無資料）" else paste(lines, collapse = "\n")
  })

  observeEvent(input$apply_lib, {
    id <- input$lib_pick
    if (!nzchar(id %||% "")) return(showNotification("請先從範本庫選擇", type = "warning"))
    item <- get_library_item(lib(), id)
    if (is.null(item)) return()
    fill_inputs_from_ctrl(session, item$control)
    # Prefer library detailed description into preview path via fields already filled
    showNotification(paste("已套用範本：", item$title), type = "message")
  })

  add_ctrl_to_library <- function(ctrl, title = NULL, tags = character()) {
    if (!is.null(title) && nzchar(trimws(title))) ctrl$title <- trimws(title)
    tag_vec <- unlist(strsplit(as.character(tags %||% ""), "[;；,，|/]+"))
    tag_vec <- trimws(tag_vec)
    tag_vec <- tag_vec[nzchar(tag_vec)]
    item <- library_item_from_control(ctrl, tags = tag_vec)
    new_lib <- upsert_library_item(lib(), item)
    lib(persist_lib(new_lib))
    refresh_lib_choices()
    item
  }

  observeEvent(input$save_to_lib, {
    d <- current_draft_from_inputs()
    item <- add_ctrl_to_library(d, title = input$lib_title_override, tags = input$lib_tags)
    showNotification(paste("已存入範本庫", item$library_id), type = "message")
  })
  observeEvent(input$lib_add_current, {
    d <- current_draft_from_inputs()
    item <- add_ctrl_to_library(d, title = input$lib_title_override, tags = input$lib_tags)
    showNotification(paste("已存入", item$library_id), type = "message")
  })
  observeEvent(input$lib_add_selected_control, {
    s <- input$control_table_rows_selected
    cs <- controls()
    if (is.null(s) || !length(cs)) return(showNotification("請先在設計頁選取控制點", type = "warning"))
    item <- add_ctrl_to_library(cs[[s]], title = input$lib_title_override, tags = input$lib_tags)
    showNotification(paste("控制點已存入", item$library_id), type = "message")
  })
  observeEvent(input$upload_lib, {
    f <- input$upload_lib
    if (is.null(f)) return()
    tryCatch({
      new_lib <- import_control_library_file(f$datapath, lib(), overwrite = isTRUE(input$lib_overwrite))
      lib(persist_lib(new_lib))
      refresh_lib_choices()
      showNotification(sprintf("範本庫共 %d 筆", length(new_lib)), type = "message")
    }, error = function(e) showNotification(conditionMessage(e), type = "error"))
  })
  observeEvent(input$lib_delete, {
    s <- input$lib_table_rows_selected
    if (is.null(s)) return(showNotification("請選取範本列", type = "warning"))
    df <- library_summary_df(lib())
    id <- df$library_id[s]
    lib(persist_lib(delete_library_item(lib(), id)))
    refresh_lib_choices()
  })
  output$lib_table <- renderDT({
    datatable(
      library_summary_df(filter_library(lib(), cycle_filter = input$cycle, query = input$lib_query)),
      selection = "single", rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE, dom = "ftip")
    )
  })
  output$lib_preview <- renderText({
    s <- input$lib_table_rows_selected
    items <- filter_library(lib(), cycle_filter = input$cycle, query = input$lib_query)
    if (is.null(s) || !length(items)) return("選取範本以預覽描述")
    item <- items[[s]]
    paste(
      c(
        paste0("【", item$library_id, "】", item$title),
        item$control$summary_description %||% "",
        "----",
        item$control$detailed_description %||% assemble_control_paragraph(item$control)
      ),
      collapse = "\n"
    )
  })
  output$download_lib_csv <- downloadHandler(
    filename = function() sprintf("control_library-%s.csv", format(Sys.time(), "%Y%m%d")),
    content = function(file) utils::write.csv(library_to_flat_df(lib()), file, row.names = FALSE, fileEncoding = "UTF-8")
  )
  output$download_lib_json <- downloadHandler(
    filename = function() sprintf("control_library-%s.json", format(Sys.time(), "%Y%m%d")),
    content = function(file) save_control_library(lib(), file)
  )

  labelize <- function(label, body) {
    body <- trimws(body %||% "")
    lab <- trimws(label %||% "")
    if (!nzchar(body)) return("")
    if (!nzchar(lab)) return(body)
    sprintf("[%s] %s", lab, body)
  }

  current_draft_from_inputs <- function() {
    list(
      draft_id = next_id(),
      control_id = input$control_id %||% "",
      company = input$company %||% "",
      cycle = input$cycle %||% "",
      risk_name = input$risk_name %||% "",
      risk_description = input$risk_description %||% "",
      risk_attr_financial = labelize(input$attr_label_fr, input$risk_attr_financial),
      risk_attr_operations = labelize(input$attr_label_op, input$risk_attr_operations),
      risk_attr_compliance = labelize(input$attr_label_cp, input$risk_attr_compliance),
      romm_classification = input$romm_classification %||% "",
      significant_account = input$significant_account %||% "",
      assertions = paste(input$assertions %||% character(), collapse = "；"),
      control_objective = input$control_objective %||% "",
      control_activity = input$control_activity %||% "",
      frequency = input$frequency %||% "",
      responsible_unit = input$responsible_unit %||% "",
      iuc_or_system = input$iuc_or_system %||% "",
      nature = input$nature %||% "",
      approach = input$approach %||% "",
      type = input$type %||% "",
      inputs = input$inputs %||% "",
      review_steps = input$review_steps %||% "",
      outputs = input$outputs %||% "",
      investigation_threshold = input$investigation_threshold %||% "",
      dependent_controls = ""
    )
  }

  form_snapshot <- function() {
    list(
      company = input$company, cycle = input$cycle, risk_name = input$risk_name,
      risk_description = input$risk_description,
      control_id = input$control_id, control_objective = input$control_objective,
      control_activity = input$control_activity, frequency = input$frequency,
      responsible_unit = input$responsible_unit, iuc_or_system = input$iuc_or_system
    )
  }

  make_payload <- function(name = NULL) {
    build_draft_payload(
      drafts = drafts(), controls = controls(), pbc = pbc_reg(),
      interview_elements = input$interview_elements,
      csa_elements = input$csa_elements,
      form_snapshot = form_snapshot(),
      name = name
    )
  }

  apply_payload <- function(payload, notify = TRUE) {
    if (!is.null(payload$drafts)) drafts(payload$drafts)
    if (!is.null(payload$controls)) controls(payload$controls)
    if (!is.null(payload$pbc) && length(payload$pbc)) {
      pbc_reg(normalize_pbc_df(as.data.frame(payload$pbc, stringsAsFactors = FALSE)))
      persist_pbc(pbc_reg())
      refresh_pbc_choices()
    }
    if (!is.null(payload$interview_elements)) {
      updateCheckboxGroupInput(session, "interview_elements",
                               selected = unlist(payload$interview_elements))
    }
    if (!is.null(payload$csa_elements)) {
      updateCheckboxGroupInput(session, "csa_elements",
                               selected = unlist(payload$csa_elements))
    }
    snap <- payload$form_snapshot
    if (!is.null(snap)) {
      if (!is.null(snap$company)) updateTextInput(session, "company", value = snap$company)
      if (!is.null(snap$cycle)) updateSelectInput(session, "cycle", selected = snap$cycle)
      if (!is.null(snap$risk_name)) updateTextInput(session, "risk_name", value = snap$risk_name)
      if (!is.null(snap$risk_description)) {
        updateTextAreaInput(session, "risk_description", value = snap$risk_description)
      }
      if (!is.null(snap$control_objective)) {
        updateTextAreaInput(session, "control_objective", value = snap$control_objective)
      }
      if (!is.null(snap$control_activity)) {
        updateTextAreaInput(session, "control_activity", value = snap$control_activity)
      }
      if (!is.null(snap$iuc_or_system)) {
        updateTextAreaInput(session, "iuc_or_system", value = snap$iuc_or_system)
      }
    }
    if (!is.null(payload$name) && nzchar(payload$name)) {
      updateTextInput(session, "draft_name", value = payload$name)
    }
    last_saved_at(payload$saved_at %||% format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    if (notify) showNotification("草稿已載入", type = "message")
  }

  do_save_draft <- function(name = NULL, quiet = FALSE) {
    nm <- name %||% input$draft_name
    if (!nzchar(trimws(nm %||% ""))) nm <- format(Sys.time(), "草稿_%Y%m%d_%H%M%S")
    path <- save_named_draft(data_dir, nm, make_payload(nm))
    updateTextInput(session, "draft_name", value = sanitize_draft_name(nm))
    last_saved_at(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    refresh_draft_list(selected = path)
    if (!quiet) showNotification(paste("已儲存", basename(path)), type = "message")
    path
  }

  output$draft_status <- renderUI({
    draft_tick()
    ts <- last_saved_at()
    nq <- length(drafts())
    nc <- length(controls())
    txt <- if (is.null(ts)) sprintf("未儲存｜佇列 %d／控制點 %d", nq, nc)
    else sprintf("已存 %s｜佇列 %d／控制點 %d", ts, nq, nc)
    tags$small(class = "text-muted", txt)
  })

  output$live_preview <- renderText(assemble_control_paragraph(current_draft_from_inputs()))
  output$live_validation <- renderUI({
    v <- validate_control_design(current_draft_from_inputs())
    gaps <- detect_design_gaps(current_draft_from_inputs())
    if (v$ok && !nrow(gaps)) {
      div(class = "alert alert-success py-1 mb-2", "元素齊備")
    } else {
      miss <- if (!v$ok) paste("缺：", paste(v$missing, collapse = "、")) else NULL
      extra <- if (nrow(gaps)) paste(gaps$gap_item, collapse = "；") else NULL
      div(class = "alert alert-warning py-1 mb-2", paste(na.omit(c(miss, extra)), collapse = "｜"))
    }
  })

  observeEvent(input$add_draft, {
    d <- current_draft_from_inputs()
    d$draft_id <- next_id()
    if (!nzchar(trimws(d$control_id))) d$control_id <- sprintf("CD-%03d", d$draft_id)
    drafts(c(drafts(), list(d)))
    next_id(next_id() + 1L)
    updateTextInput(session, "control_id", value = sprintf("CD-%03d", next_id()))
    if (isTRUE(input$autosave_draft)) do_save_draft(quiet = TRUE)
  })

  drafts_df <- reactive({
    ds <- drafts()
    if (!length(ds)) {
      return(data.frame(ID = character(), 風險 = character(), 目標 = character(),
                        活動 = character(), IUC = character(), stringsAsFactors = FALSE))
    }
    data.frame(
      ID = vapply(ds, function(x) x$control_id, ""),
      風險 = vapply(ds, function(x) x$risk_name, ""),
      目標 = vapply(ds, function(x) x$control_objective, ""),
      活動 = vapply(ds, function(x) x$control_activity, ""),
      IUC = vapply(ds, function(x) x$iuc_or_system, ""),
      stringsAsFactors = FALSE
    )
  })
  output$draft_table <- renderDT({
    datatable(drafts_df(), selection = "single", rownames = FALSE,
              options = list(dom = "t", pageLength = 6, scrollX = TRUE, ordering = FALSE))
  })

  selected_draft_index <- reactive({
    s <- input$draft_table_rows_selected
    if (is.null(s) || !length(drafts())) NULL else s
  })

  observeEvent(input$update_draft, {
    idx <- selected_draft_index()
    if (is.null(idx)) return(showNotification("請選佇列列", type = "warning"))
    ds <- drafts()
    new_d <- current_draft_from_inputs()
    new_d$draft_id <- ds[[idx]]$draft_id
    ds[[idx]] <- new_d
    drafts(ds)
    if (isTRUE(input$autosave_draft)) do_save_draft(quiet = TRUE)
  })
  observeEvent(input$remove_draft, {
    idx <- selected_draft_index()
    if (is.null(idx)) return()
    drafts(drafts()[-idx])
    if (isTRUE(input$autosave_draft)) do_save_draft(quiet = TRUE)
  })

  observeEvent(input$generate_controls, {
    ds <- drafts()
    if (!length(ds)) return(showNotification("無佇列", type = "warning"))
    risk_keys <- vapply(ds, function(d) paste(d$cycle, d$risk_name, sep = "||"), "")
    groups <- split(seq_along(ds), risk_keys)
    result <- list()
    seq_no <- 1L
    for (gk in names(groups)) {
      for (pt in split_controls_by_iuc(ds[groups[[gk]]])) {
        pt$control_id <- sprintf("CP-%03d", seq_no)
        pt$summary_description <- assemble_summary_description(pt)
        pt$detailed_description <- assemble_control_paragraph(pt)
        pt$validation <- validate_control_design(pt)
        result[[length(result) + 1]] <- pt
        seq_no <- seq_no + 1L
      }
    }
    controls(result)
    showNotification(sprintf("已產生 %d 控制點", length(result)), type = "message")
    if (isTRUE(input$autosave_draft)) do_save_draft(quiet = TRUE)
  })

  controls_df <- reactive({
    cs <- controls()
    if (!length(cs)) {
      return(data.frame(ControlID = character(), IUC = character(), 摘要 = character(),
                        stringsAsFactors = FALSE))
    }
    data.frame(
      ControlID = vapply(cs, function(x) x$control_id, ""),
      IUC = vapply(cs, function(x) x$iuc_or_system, ""),
      摘要 = vapply(cs, function(x) x$summary_description, ""),
      stringsAsFactors = FALSE
    )
  })
  output$control_table <- renderDT({
    datatable(controls_df(), selection = "single", rownames = FALSE,
              options = list(dom = "t", pageLength = 6, scrollX = TRUE, ordering = FALSE))
  })
  output$control_paragraph <- renderText({
    s <- input$control_table_rows_selected
    cs <- controls()
    if (is.null(s) || !length(cs)) return("選取控制點以檢視")
    cs[[s]]$detailed_description
  })

  # PBC
  observeEvent(input$pbc_add, {
    tryCatch({
      reg <- upsert_pbc(pbc_reg(), list(
        pbc_id = input$pbc_id, client_pbc_name = input$pbc_client,
        reviewed_name = input$pbc_reviewed, iuc_or_system = input$pbc_reviewed,
        cycle = input$pbc_cycle, notes = input$pbc_notes
      ))
      pbc_reg(reg)
      persist_pbc(reg)
      refresh_pbc_choices()
      updateTextInput(session, "pbc_id", value = "")
      updateTextInput(session, "pbc_client", value = "")
      updateTextInput(session, "pbc_reviewed", value = "")
      updateTextInput(session, "pbc_notes", value = "")
      showNotification("已登錄 PBC", type = "message")
    }, error = function(e) showNotification(conditionMessage(e), type = "error"))
  })
  output$pbc_table <- renderDT({
    df <- pbc_reg()
    show <- df[, c("pbc_id", "client_pbc_name", "reviewed_name", "cycle", "notes"), drop = FALSE]
    names(show) <- c("ID", "客戶原名", "檢視後", "循環", "備註")
    datatable(show, selection = "single", rownames = FALSE,
              options = list(pageLength = 8, scrollX = TRUE, dom = "tip"))
  })
  observeEvent(input$pbc_table_rows_selected, {
    s <- input$pbc_table_rows_selected
    if (is.null(s)) return()
    row <- pbc_reg()[s, , drop = FALSE]
    updateTextInput(session, "pbc_id", value = row$pbc_id[[1]])
    updateTextInput(session, "pbc_client", value = row$client_pbc_name[[1]])
    updateTextInput(session, "pbc_reviewed", value = row$reviewed_name[[1]])
    updateTextInput(session, "pbc_notes", value = row$notes[[1]])
    updateSelectInput(session, "pbc_cycle",
                      selected = if (nzchar(row$cycle[[1]])) row$cycle[[1]] else "")
  }, ignoreInit = TRUE)
  observeEvent(input$pbc_delete, {
    s <- input$pbc_table_rows_selected
    if (is.null(s)) return(showNotification("請先選取", type = "warning"))
    reg <- delete_pbc(pbc_reg(), pbc_reg()$pbc_id[s])
    pbc_reg(reg)
    persist_pbc(reg)
    refresh_pbc_choices()
  })
  observeEvent(input$upload_pbc, {
    f <- input$upload_pbc
    if (is.null(f)) return()
    tryCatch({
      reg <- import_pbc_csv(f$datapath, pbc_reg())
      pbc_reg(reg)
      persist_pbc(reg)
      refresh_pbc_choices()
      showNotification(sprintf("已匯入，共 %d 筆", nrow(reg)), type = "message")
    }, error = function(e) showNotification(conditionMessage(e), type = "error"))
  })
  output$download_pbc <- downloadHandler(
    filename = function() sprintf("pbc-%s.csv", format(Sys.time(), "%Y%m%d")),
    content = function(file) utils::write.csv(pbc_reg(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )

  # RCM / worksheets
  output$rcm_table <- renderDT({
    datatable(controls_to_rcm(controls()), rownames = FALSE,
              options = list(scrollX = TRUE, pageLength = 8, dom = "tip"))
  })
  selected_worksheet_controls <- reactive({
    cs <- controls()
    if (!length(cs)) return(list())
    ids <- input$worksheet_controls
    if (!length(ids) || all(!nzchar(ids))) return(cs)
    Filter(function(c) c$control_id %in% ids, cs)
  })
  observe({
    cs <- controls()
    if (!length(cs)) {
      updateSelectizeInput(session, "worksheet_controls", choices = character(), server = TRUE)
      return()
    }
    ch <- stats::setNames(
      vapply(cs, function(x) x$control_id, ""),
      vapply(cs, function(x) sprintf("%s｜%s", x$control_id, x$risk_name %||% ""), "")
    )
    updateSelectizeInput(session, "worksheet_controls", choices = ch, server = TRUE)
  })
  observeEvent(input$ws_select_core_iv, {
    updateCheckboxGroupInput(session, "interview_elements", selected = DEFAULT_INTERVIEW_ELEMENTS)
  })
  observeEvent(input$ws_select_core_csa, {
    updateCheckboxGroupInput(session, "csa_elements", selected = DEFAULT_CSA_ELEMENTS)
  })
  output$interview_table <- renderDT({
    datatable(controls_to_interview(selected_worksheet_controls(), input$interview_elements),
              rownames = FALSE, options = list(scrollX = TRUE, pageLength = 8, dom = "tip"))
  })
  output$csa_table <- renderDT({
    datatable(controls_to_csa(selected_worksheet_controls(), input$csa_elements),
              rownames = FALSE, options = list(scrollX = TRUE, pageLength = 8, dom = "tip"))
  })
  output$gap_table <- renderDT({
    datatable(detect_gaps_many(controls()), rownames = FALSE,
              options = list(scrollX = TRUE, pageLength = 8, dom = "tip"))
  })

  # Named drafts
  observeEvent(input$save_draft_file, do_save_draft())
  observeEvent(input$load_draft_file, {
    path <- input$draft_pick
    if (!nzchar(path %||% "")) {
      path <- file.path(data_dir, "session_draft.json")
    }
    if (!file.exists(path)) return(showNotification("尚無草稿", type = "warning"))
    apply_payload(load_draft_payload(path))
  })
  observeEvent(input$delete_draft_file, {
    nm <- input$draft_name
    path <- input$draft_pick
    if (nzchar(path %||% "") && basename(path) != "session_draft.json") {
      file.remove(path)
    } else if (nzchar(trimws(nm %||% ""))) {
      delete_named_draft(data_dir, nm)
    } else {
      return(showNotification("請選擇或輸入要刪除的草稿", type = "warning"))
    }
    refresh_draft_list()
    showNotification("草稿已刪除", type = "message")
  })
  observeEvent(input$draft_pick, {
    path <- input$draft_pick
    if (!nzchar(path %||% "")) return()
    nm <- if (basename(path) == "session_draft.json") "（自動／預設）" else sub("\\.json$", "", basename(path))
    updateTextInput(session, "draft_name", value = nm)
  }, ignoreInit = TRUE)

  output$download_json <- downloadHandler(
    filename = function() sprintf("control-pack-%s.json", format(Sys.time(), "%Y%m%d-%H%M%S")),
    content = function(file) write_json(make_payload(input$draft_name), file,
                                        auto_unbox = TRUE, pretty = TRUE, force = TRUE)
  )
  output$download_rcm <- downloadHandler(
    filename = function() "rcm.csv",
    content = function(file) write.csv(controls_to_rcm(controls()), file, row.names = FALSE, fileEncoding = "UTF-8")
  )
  output$download_interview <- downloadHandler(
    filename = function() "interview.csv",
    content = function(file) {
      write.csv(controls_to_interview(selected_worksheet_controls(), input$interview_elements),
                file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  output$download_csa <- downloadHandler(
    filename = function() "csa.csv",
    content = function(file) {
      write.csv(controls_to_csa(selected_worksheet_controls(), input$csa_elements),
                file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui, server)
