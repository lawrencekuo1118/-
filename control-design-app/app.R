# Control Design Assistant — Form 4120SR / RCM / CSA
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

fill_inputs_from_ctrl <- function(session, ctrl) {
  if (is.null(ctrl)) return()
  updateSelectInput(session, "cycle", selected = ctrl$cycle %||% CYCLES_NINE[[1]])
  updateTextInput(session, "risk_name", value = ctrl$risk_name %||% "")
  updateTextAreaInput(session, "risk_description", value = ctrl$risk_description %||% "")
  updateTextAreaInput(session, "risk_attr_financial", value = gsub("^\\[[^\\]]+\\]\\s*", "", ctrl$risk_attr_financial %||% ""))
  updateTextAreaInput(session, "risk_attr_operations", value = gsub("^\\[[^\\]]+\\]\\s*", "", ctrl$risk_attr_operations %||% ""))
  updateTextAreaInput(session, "risk_attr_compliance", value = gsub("^\\[[^\\]]+\\]\\s*", "", ctrl$risk_attr_compliance %||% ""))
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
  title = "控制點設計｜RCM／CSA",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#1B4F72",
                   base_font = font_google("Noto Sans TC")),
  fillable = TRUE,
  sidebar = sidebar(
    width = 300,
    open = "desktop",
    textInput("company", "公司", placeholder = "公司名稱"),
    selectInput("lib_pick", "範本庫（優先套用）", choices = c("（不使用）" = ""), selected = ""),
    actionButton("apply_lib", "套用範本", class = "btn-sm btn-outline-primary"),
    hr(),
    selectInput("cycle", "九大循環", choices = CYCLES_NINE),
    textInput("risk_name", "風險", placeholder = "風險名稱"),
    textAreaInput("risk_description", NULL, rows = 2, placeholder = "RoMM 完整描述"),
    textInput("attr_label_fr", NULL, value = "財務報導"),
    textAreaInput("risk_attr_financial", NULL, rows = 1, placeholder = "屬性1"),
    textInput("attr_label_op", NULL, value = "營運"),
    textAreaInput("risk_attr_operations", NULL, rows = 1, placeholder = "屬性2"),
    textInput("attr_label_cp", NULL, value = "法令遵循"),
    textAreaInput("risk_attr_compliance", NULL, rows = 1, placeholder = "屬性3"),
    textInput("significant_account", "重大科目", placeholder = "科目"),
    selectizeInput("assertions", "聲明", choices = ASSERTION_CHOICES, multiple = TRUE,
                   selected = ASSERTION_CHOICES[1:2], options = list(create = TRUE)),
    selectInput("romm_classification", "RoMM", choices = ROMM_CLASS_CHOICES),
    hr(),
    actionButton("save_draft_file", "儲存草稿", class = "btn-sm btn-secondary"),
    actionButton("load_draft_file", "載入草稿", class = "btn-sm btn-outline-secondary"),
    downloadButton("download_json", "匯出 JSON", class = "btn-sm")
  ),
  nav_panel(
    "設計",
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("控制元素（目標 ≠ 活動）"),
        layout_columns(
          col_widths = c(6, 6),
          textInput("control_id", "ID", value = "CD-001"),
          selectInput("frequency", "頻率", choices = FREQUENCY_CHOICES, selected = FREQUENCY_CHOICES[[4]])
        ),
        textInput("responsible_unit", "負責單位", placeholder = "Owner"),
        textAreaInput("control_objective", "控制目標（Why／對應風險與聲明）", rows = 2),
        textAreaInput("control_activity", "控制活動（How／執行作為，勿重述目標）", rows = 2),
        selectizeInput("pbc_apply", "套用已登錄 IUC／PBC", choices = NULL, multiple = TRUE,
                       options = list(placeholder = "從命名庫多選")),
        textAreaInput("iuc_or_system", "IUC／制度（不同則分拆控制點）", rows = 2),
        layout_columns(
          col_widths = c(4, 4, 4),
          selectizeInput("nature", "Nature", choices = NATURE_CHOICES, options = list(create = TRUE)),
          selectizeInput("approach", "Approach", choices = APPROACH_CHOICES, options = list(create = TRUE)),
          selectizeInput("type", "Type", choices = TYPE_CHOICES, options = list(create = TRUE))
        ),
        textAreaInput("inputs", "Inputs", rows = 1),
        textAreaInput("review_steps", "Steps（每行一步）", rows = 3),
        textAreaInput("outputs", "Outputs", rows = 1),
        textAreaInput("investigation_threshold", "調查門檻（選填）", rows = 1),
        div(
          actionButton("add_draft", "加入草稿", class = "btn-primary btn-sm"),
          actionButton("update_draft", "更新選取", class = "btn-sm"),
          actionButton("remove_draft", "刪除選取", class = "btn-sm btn-outline-danger"),
          actionButton("generate_controls", "產生控制點", class = "btn-success btn-sm")
        )
      ),
      card(
        card_header("預覽／缺漏"),
        verbatimTextOutput("live_preview"),
        uiOutput("live_validation"),
        hr(),
        h6("草稿"),
        DTOutput("draft_table"),
        h6("控制點"),
        DTOutput("control_table"),
        verbatimTextOutput("control_paragraph")
      )
    )
  ),
  nav_panel(
    "IUC／PBC",
    card(
      card_header("客戶 PBC 原名 ↔ 檢視後命名（可重複套用）"),
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        textInput("pbc_id", "ID", placeholder = "自動"),
        textInput("pbc_client", "客戶原名", placeholder = "PBC 原始檔名／標題"),
        textInput("pbc_reviewed", "檢視後命名", placeholder = "標準化 IUC 名稱"),
        textInput("pbc_notes", "備註", placeholder = "來源／版本")
      ),
      actionButton("pbc_add", "登錄／更新", class = "btn-primary btn-sm"),
      actionButton("pbc_delete", "刪除選取", class = "btn-outline-danger btn-sm"),
      DTOutput("pbc_table")
    )
  ),
  nav_panel(
    "RCM",
    card(
      card_header("RCM 底稿（控制目標與控制活動分欄，不混用）"),
      p(class = "text-muted small",
        "控制目標＝要達成什麼／對應何風險與聲明；控制活動＝如何執行。兩者文字不得相同。"),
      DTOutput("rcm_table"),
      downloadButton("download_rcm", "下載 RCM CSV", class = "btn-sm")
    )
  ),
  nav_panel(
    "訪談／CSA",
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("訪談問題（依設計元素拆分）"),
        DTOutput("interview_table"),
        downloadButton("download_interview", "下載訪談 CSV", class = "btn-sm")
      ),
      card(
        card_header("CSA 自我評估步驟"),
        DTOutput("csa_table"),
        downloadButton("download_csa", "下載 CSA CSV", class = "btn-sm")
      )
    )
  ),
  nav_panel(
    "缺漏",
    card(
      card_header("設計缺漏／待補文件／潛在控制缺失"),
      DTOutput("gap_table")
    )
  )
)

server <- function(input, output, session) {
  drafts <- reactiveVal(list())
  controls <- reactiveVal(list())
  pbc_reg <- reactiveVal(empty_pbc_registry())
  lib <- reactiveVal(seed_control_library())
  next_id <- reactiveVal(1L)

  observe({
    updateSelectInput(session, "lib_pick",
                      choices = c("（不使用）" = "", library_choices(lib())))
  })

  observe({
    ch <- pbc_choices(pbc_reg())
    updateSelectizeInput(session, "pbc_apply", choices = ch, server = TRUE)
  })

  observeEvent(input$pbc_apply, {
    if (!length(input$pbc_apply)) return()
    updateTextAreaInput(session, "iuc_or_system",
                        value = apply_pbc_to_iuc(pbc_reg(), input$pbc_apply))
  }, ignoreInit = TRUE)

  observeEvent(input$apply_lib, {
    id <- input$lib_pick
    if (!nzchar(id %||% "")) {
      showNotification("請先選擇範本", type = "warning")
      return()
    }
    item <- get_library_item(lib(), id)
    if (is.null(item)) return()
    fill_inputs_from_ctrl(session, item$control)
    showNotification(paste("已套用", item$title), type = "message")
  })

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

  output$live_preview <- renderText(assemble_control_paragraph(current_draft_from_inputs()))
  output$live_validation <- renderUI({
    v <- validate_control_design(current_draft_from_inputs())
    gaps <- detect_design_gaps(current_draft_from_inputs())
    if (v$ok && !nrow(gaps)) {
      div(class = "alert alert-success py-1", "元素齊備")
    } else {
      miss <- if (!v$ok) paste("缺：", paste(v$missing, collapse = "、")) else NULL
      extra <- if (nrow(gaps)) paste(gaps$gap_item, collapse = "；") else NULL
      div(class = "alert alert-warning py-1", paste(na.omit(c(miss, extra)), collapse = "｜"))
    }
  })

  observeEvent(input$add_draft, {
    d <- current_draft_from_inputs()
    d$draft_id <- next_id()
    if (!nzchar(trimws(d$control_id))) d$control_id <- sprintf("CD-%03d", d$draft_id)
    drafts(c(drafts(), list(d)))
    next_id(next_id() + 1L)
    updateTextInput(session, "control_id", value = sprintf("CD-%03d", next_id()))
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
  output$draft_table <- renderDT(datatable(drafts_df(), selection = "single", rownames = FALSE,
                                           options = list(dom = "t", pageLength = 5, scrollX = TRUE)))

  selected_draft_index <- reactive({
    s <- input$draft_table_rows_selected
    if (is.null(s) || !length(drafts())) NULL else s
  })

  observeEvent(input$update_draft, {
    idx <- selected_draft_index()
    if (is.null(idx)) return(showNotification("請選草稿", type = "warning"))
    ds <- drafts()
    new_d <- current_draft_from_inputs()
    new_d$draft_id <- ds[[idx]]$draft_id
    ds[[idx]] <- new_d
    drafts(ds)
  })
  observeEvent(input$remove_draft, {
    idx <- selected_draft_index()
    if (is.null(idx)) return()
    drafts(drafts()[-idx])
  })

  observeEvent(input$generate_controls, {
    ds <- drafts()
    if (!length(ds)) return(showNotification("無草稿", type = "warning"))
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
    showNotification(sprintf("已產生 %d 控制點（已依 IUC 分拆／合併）", length(result)), type = "message")
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
  output$control_table <- renderDT(datatable(controls_df(), selection = "single", rownames = FALSE,
                                             options = list(dom = "t", pageLength = 5, scrollX = TRUE)))
  output$control_paragraph <- renderText({
    s <- input$control_table_rows_selected
    cs <- controls()
    if (is.null(s) || !length(cs)) return("選取控制點以檢視段落")
    cs[[s]]$detailed_description
  })

  # PBC registry
  observeEvent(input$pbc_add, {
    tryCatch({
      pbc_reg(upsert_pbc(pbc_reg(), list(
        pbc_id = input$pbc_id,
        client_pbc_name = input$pbc_client,
        reviewed_name = input$pbc_reviewed,
        iuc_or_system = input$pbc_reviewed,
        cycle = input$cycle,
        notes = input$pbc_notes
      )))
      updateTextInput(session, "pbc_id", value = "")
      updateTextInput(session, "pbc_client", value = "")
      updateTextInput(session, "pbc_reviewed", value = "")
      updateTextInput(session, "pbc_notes", value = "")
    }, error = function(e) showNotification(conditionMessage(e), type = "error"))
  })
  output$pbc_table <- renderDT(datatable(pbc_reg(), selection = "single", rownames = FALSE,
                                         options = list(pageLength = 8, scrollX = TRUE)))
  observeEvent(input$pbc_delete, {
    s <- input$pbc_table_rows_selected
    if (is.null(s)) return()
    reg <- pbc_reg()
    pbc_reg(reg[-s, , drop = FALSE])
  })

  # RCM / interview / CSA / gaps
  output$rcm_table <- renderDT({
    datatable(controls_to_rcm(controls()), rownames = FALSE,
              options = list(scrollX = TRUE, pageLength = 10))
  })
  output$interview_table <- renderDT({
    cs <- controls()
    df <- if (!length(cs)) control_to_interview(list())[0, ] else do.call(rbind, lapply(cs, control_to_interview))
    datatable(df, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
  })
  output$csa_table <- renderDT({
    cs <- controls()
    df <- if (!length(cs)) control_to_csa(list())[0, ] else do.call(rbind, lapply(cs, control_to_csa))
    datatable(df, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
  })
  output$gap_table <- renderDT({
    datatable(detect_gaps_many(controls()), rownames = FALSE,
              options = list(scrollX = TRUE, pageLength = 12))
  })

  # Persistence
  draft_path <- file.path(data_dir, "session_draft.json")
  observeEvent(input$save_draft_file, {
    payload <- list(
      drafts = drafts(),
      controls = controls(),
      pbc = pbc_reg(),
      saved_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
    write_json(payload, draft_path, auto_unbox = TRUE, pretty = TRUE, force = TRUE)
    showNotification(paste("已儲存", draft_path), type = "message")
  })
  observeEvent(input$load_draft_file, {
    if (!file.exists(draft_path)) return(showNotification("尚無草稿檔", type = "warning"))
    payload <- read_json(draft_path, simplifyVector = FALSE)
    if (!is.null(payload$drafts)) drafts(payload$drafts)
    if (!is.null(payload$controls)) controls(payload$controls)
    if (!is.null(payload$pbc) && length(payload$pbc)) {
      # rebuild data.frame from json list-of-lists or columnar list
      pbc_reg(as.data.frame(payload$pbc, stringsAsFactors = FALSE))
    }
    showNotification("草稿已載入", type = "message")
  })

  output$download_json <- downloadHandler(
    filename = function() sprintf("control-pack-%s.json", format(Sys.time(), "%Y%m%d-%H%M%S")),
    content = function(file) {
      write_json(list(drafts = drafts(), controls = controls(), pbc = pbc_reg()),
                 file, auto_unbox = TRUE, pretty = TRUE, force = TRUE)
    }
  )
  output$download_rcm <- downloadHandler(
    filename = function() "rcm.csv",
    content = function(file) write.csv(controls_to_rcm(controls()), file, row.names = FALSE, fileEncoding = "UTF-8")
  )
  output$download_interview <- downloadHandler(
    filename = function() "interview.csv",
    content = function(file) {
      cs <- controls()
      df <- if (!length(cs)) data.frame() else do.call(rbind, lapply(cs, control_to_interview))
      write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  output$download_csa <- downloadHandler(
    filename = function() "csa.csv",
    content = function(file) {
      cs <- controls()
      df <- if (!length(cs)) data.frame() else do.call(rbind, lapply(cs, control_to_csa))
      write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui, server)
