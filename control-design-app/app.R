# Control Design Assistant — Form 4120SR Significant Risk aligned
# Run: shiny::runApp("control-design-app")

library(shiny)
library(bslib)
library(DT)
# Avoid jsonlite::validate masking shiny::validate
write_json <- jsonlite::write_json

app_dir <- if (exists("appDir", inherits = FALSE)) appDir else getwd()
# Support both runApp(dir) and source("app.R")
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
source(file.path(root, "R", "constants.R"), local = TRUE)
source(file.path(root, "R", "assemble.R"), local = TRUE)

empty_draft <- function(id = 1L) {
  list(
    draft_id = id,
    control_id = sprintf("CD-%03d", id),
    company = "",
    cycle = CYCLES_NINE[[1]],
    risk_name = "",
    risk_description = "",
    risk_attr_financial = "",
    risk_attr_operations = "",
    risk_attr_compliance = "",
    romm_classification = ROMM_CLASS_CHOICES[[1]],
    significant_account = "",
    assertions = ASSERTION_CHOICES[1:2],
    control_objective = "",
    control_activity = "",
    frequency = FREQUENCY_CHOICES[[4]],
    responsible_unit = "",
    iuc_or_system = "",
    nature = NATURE_CHOICES[[1]],
    approach = APPROACH_CHOICES[[1]],
    type = TYPE_CHOICES[[6]],
    inputs = "",
    review_steps = "",
    outputs = "",
    investigation_threshold = "",
    dependent_controls = ""
  )
}

ui <- page_navbar(
  title = "控制點設計輔助｜Form 4120SR",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1B4F72",
    base_font = font_google("Noto Sans TC"),
    heading_font = font_google("Noto Sans TC")
  ),
  sidebar = sidebar(
    width = 340,
    h5("公司現況"),
    textInput("company", "公司／受查者名稱", placeholder = "例：○○股份有限公司"),
    selectInput("cycle", "九大循環", choices = CYCLES_NINE, selected = CYCLES_NINE[[1]]),
    textInput("risk_name", "風險名稱（可自訂）", placeholder = "例：收入認列時點不正確"),
    textAreaInput("risk_description", "風險說明", rows = 2, placeholder = "完整 RoMM 文字，非僅編號"),
    hr(),
    h5("風險三大屬性（可自訂）"),
    textInput("attr_label_fr", "屬性1標籤", value = RISK_ATTR_DEFAULTS$financial_reporting$label),
    textAreaInput("risk_attr_financial", "屬性1內容", rows = 2,
                  placeholder = RISK_ATTR_DEFAULTS$financial_reporting$prompt),
    textInput("attr_label_op", "屬性2標籤", value = RISK_ATTR_DEFAULTS$operations$label),
    textAreaInput("risk_attr_operations", "屬性2內容", rows = 2,
                  placeholder = RISK_ATTR_DEFAULTS$operations$prompt),
    textInput("attr_label_cp", "屬性3標籤", value = RISK_ATTR_DEFAULTS$compliance$label),
    textAreaInput("risk_attr_compliance", "屬性3內容", rows = 2,
                  placeholder = RISK_ATTR_DEFAULTS$compliance$prompt),
    selectInput("romm_classification", "RoMM 分類", choices = ROMM_CLASS_CHOICES),
    textInput("significant_account", "重大科目", placeholder = "例：營業收入、應收帳款"),
    selectizeInput("assertions", "相關聲明（Assertions）", choices = ASSERTION_CHOICES,
                   multiple = TRUE, selected = ASSERTION_CHOICES[1:2],
                   options = list(create = TRUE, placeholder = "可多選或自訂輸入"))
  ),
  nav_panel(
    "設計控制草稿",
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header("控制設計元素（均為可編輯；對齊 Form 4120SR Control Summary / Note 1）"),
        layout_columns(
          col_widths = c(6, 6),
          textInput("control_id", "Control ID", value = "CD-001"),
          selectInput("frequency", "控制頻率", choices = FREQUENCY_CHOICES, selected = FREQUENCY_CHOICES[[4]])
        ),
        textInput("responsible_unit", "負責單位／Control Owner", placeholder = "例：財務部會計課／主管"),
        textAreaInput("control_objective", "控制目標", rows = 2,
                      placeholder = "此控制欲達成之目的（對應 Design Factor 1：與風險／聲明之關連）"),
        textAreaInput("control_activity", "控制活動（摘要）", rows = 2,
                      placeholder = "Control Activity — Summary Description"),
        textAreaInput("iuc_or_system", "所使用 IUC 或制度（不同 IUC 將分拆為不同控制點）", rows = 2,
                      placeholder = "例：銷貨日報表／ERP AR 模組／收入認列作業辦法"),
        layout_columns(
          col_widths = c(4, 4, 4),
          selectizeInput("nature", "Nature", choices = NATURE_CHOICES, options = list(create = TRUE)),
          selectizeInput("approach", "Approach", choices = APPROACH_CHOICES, options = list(create = TRUE)),
          selectizeInput("type", "Type", choices = TYPE_CHOICES, options = list(create = TRUE))
        ),
        textAreaInput("inputs", "Inputs Used by Reviewer（投入）", rows = 2,
                      placeholder = "執行控制所使用之資訊／報表及其產生方式"),
        textAreaInput("review_steps", "Specific Activities（步驟，每行一步）", rows = 4,
                      placeholder = "Step 1: …\nStep 2: …\nStep 3: …"),
        textAreaInput("outputs", "Outputs of the Control（產出／軌跡）", rows = 2,
                      placeholder = "核准簽核、調節表、例外追蹤清單等"),
        textAreaInput("investigation_threshold", "調查門檻與後續追蹤（Design Factor 5，選填）", rows = 2),
        textAreaInput("dependent_controls", "依賴之其他控制／資訊（選填）", rows = 2),
        layout_columns(
          col_widths = c(4, 4, 4),
          actionButton("add_draft", "加入草稿列", class = "btn-primary"),
          actionButton("update_draft", "更新選取草稿", class = "btn-secondary"),
          actionButton("clear_form", "清空表單", class = "btn-outline-secondary")
        )
      ),
      card(
        card_header("即時段落預覽（單一草稿）"),
        verbatimTextOutput("live_preview"),
        uiOutput("live_validation")
      )
    ),
    card(
      card_header("草稿佇列"),
      p(class = "text-muted small",
        "規則：同一風險下，若 IUC／制度不同，產生控制點時自動分拆（Form 4120SR Note 7）。",
        "相同 IUC 之草稿會合併為同一控制點。"),
      DTOutput("draft_table"),
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        actionButton("remove_draft", "刪除選取", class = "btn-outline-danger"),
        actionButton("generate_controls", "產生控制點（依 IUC 分拆）", class = "btn-success"),
        downloadButton("download_json", "匯出 JSON"),
        downloadButton("download_csv", "匯出 CSV")
      )
    )
  ),
  nav_panel(
    "控制點成果",
    card(
      card_header("已分拆之標準控制點（每列一個 Control Point）"),
      p(class = "text-muted",
        "內容自動拼湊自：九大循環、風險三大屬性、控制目標／活動／頻率／負責單位、IUC／制度，",
        "並補齊 Form 4120SR 要求之 Nature／Approach／Type、Inputs、Steps、Outputs。"),
      DTOutput("control_table")
    ),
    card(
      card_header("選取控制點 — 完整控制描述段落"),
      verbatimTextOutput("control_paragraph"),
      h6("Control Activity — Summary Description"),
      verbatimTextOutput("control_summary"),
      uiOutput("control_validation")
    )
  ),
  nav_panel(
    "範本對照",
    card(
      card_header("Form 4120SR 設計面必要元素"),
      markdown(paste(
        "本工具對齊 **Form 4120SR Control Testing Template — Significant Risk**（Word／Excel）之控制設計敘述結構：",
        "",
        "- **Control Summary**：Control ID、Summary Description、Detailed Description",
        "- **Note 1**：Inputs Used by Reviewer、Specific Activities（Steps）、Outputs",
        "- **Nature / Approach / Type**（Note 2）",
        "- **RoMM／Significant Account／Assertions**（Note 3）",
        "- **Design Factor 1–5**（目的與風險關連、Owner、頻率、彙總層級、調查門檻）",
        "- **IUC 依賴**（Note 7–9）：不同 IUC 分拆為不同控制點",
        "",
        "參考檔案位於 `control-design-app/templates/`。",
        sep = "\n"
      )),
      tags$ul(
        tags$li(tags$code("templates/Form_4120SR_Word.docx")),
        tags$li(tags$code("templates/Form_4120SR_Excel.xlsx"))
      )
    )
  )
)

server <- function(input, output, session) {
  drafts <- reactiveVal(list())
  controls <- reactiveVal(list())
  next_id <- reactiveVal(1L)

  current_draft_from_inputs <- function() {
    attrs_fr <- input$risk_attr_financial
    attrs_op <- input$risk_attr_operations
    attrs_cp <- input$risk_attr_compliance
    # Prefix with custom labels so assembled paragraph reflects user-defined attribute names
    labelize <- function(label, body) {
      body <- trimws(body %||% "")
      lab <- trimws(label %||% "")
      if (!nzchar(body)) return("")
      if (!nzchar(lab)) return(body)
      sprintf("[%s] %s", lab, body)
    }
    list(
      draft_id = next_id(),
      control_id = input$control_id %||% "",
      company = input$company %||% "",
      cycle = input$cycle %||% "",
      risk_name = input$risk_name %||% "",
      risk_description = input$risk_description %||% "",
      risk_attr_financial = labelize(input$attr_label_fr, attrs_fr),
      risk_attr_operations = labelize(input$attr_label_op, attrs_op),
      risk_attr_compliance = labelize(input$attr_label_cp, attrs_cp),
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
      dependent_controls = input$dependent_controls %||% ""
    )
  }

  output$live_preview <- renderText({
    d <- current_draft_from_inputs()
    assemble_control_paragraph(d)
  })

  output$live_validation <- renderUI({
    d <- current_draft_from_inputs()
    v <- validate_control_design(d)
    if (v$ok) {
      div(class = "alert alert-success", "設計元素齊備，可加入草稿。")
    } else {
      div(class = "alert alert-warning",
          strong("尚缺 Form 4120SR 設計元素："),
          paste(v$missing, collapse = "、"))
    }
  })

  observeEvent(input$add_draft, {
    d <- current_draft_from_inputs()
    d$draft_id <- next_id()
    if (!nzchar(trimws(d$control_id))) {
      d$control_id <- sprintf("CD-%03d", d$draft_id)
    }
    drafts(c(drafts(), list(d)))
    next_id(next_id() + 1L)
    updateTextInput(session, "control_id", value = sprintf("CD-%03d", next_id()))
    showNotification("已加入草稿列", type = "message")
  })

  observeEvent(input$clear_form, {
    updateTextInput(session, "control_objective", value = "")
    updateTextInput(session, "control_activity", value = "")
    updateTextAreaInput(session, "iuc_or_system", value = "")
    updateTextAreaInput(session, "inputs", value = "")
    updateTextAreaInput(session, "review_steps", value = "")
    updateTextAreaInput(session, "outputs", value = "")
    updateTextAreaInput(session, "investigation_threshold", value = "")
    updateTextAreaInput(session, "dependent_controls", value = "")
  })

  drafts_df <- reactive({
    ds <- drafts()
    if (!length(ds)) {
      return(data.frame(
        選 = integer(), ID = character(), 循環 = character(), 風險 = character(),
        控制目標 = character(), IUC = character(), 頻率 = character(), 負責單位 = character(),
        stringsAsFactors = FALSE
      ))
    }
    data.frame(
      選 = vapply(ds, `[[`, integer(1), "draft_id"),
      ID = vapply(ds, function(x) x$control_id, character(1)),
      循環 = vapply(ds, function(x) x$cycle, character(1)),
      風險 = vapply(ds, function(x) x$risk_name, character(1)),
      控制目標 = vapply(ds, function(x) x$control_objective, character(1)),
      IUC = vapply(ds, function(x) x$iuc_or_system, character(1)),
      頻率 = vapply(ds, function(x) x$frequency, character(1)),
      負責單位 = vapply(ds, function(x) x$responsible_unit, character(1)),
      stringsAsFactors = FALSE
    )
  })

  output$draft_table <- renderDT({
    datatable(
      drafts_df(),
      selection = "single",
      rownames = FALSE,
      options = list(pageLength = 8, scrollX = TRUE, dom = "tip")
    )
  })

  selected_draft_index <- reactive({
    s <- input$draft_table_rows_selected
    if (is.null(s) || !length(drafts())) return(NULL)
    s
  })

  observeEvent(input$update_draft, {
    idx <- selected_draft_index()
    if (is.null(idx)) {
      showNotification("請先選取草稿列", type = "warning")
      return()
    }
    ds <- drafts()
    new_d <- current_draft_from_inputs()
    new_d$draft_id <- ds[[idx]]$draft_id
    ds[[idx]] <- new_d
    drafts(ds)
    showNotification("草稿已更新", type = "message")
  })

  observeEvent(input$remove_draft, {
    idx <- selected_draft_index()
    if (is.null(idx)) {
      showNotification("請先選取草稿列", type = "warning")
      return()
    }
    ds <- drafts()
    ds <- ds[-idx]
    drafts(ds)
  })

  observeEvent(input$generate_controls, {
    ds <- drafts()
    if (!length(ds)) {
      showNotification("尚無草稿可產生", type = "warning")
      return()
    }
    # Group by risk first, then split each risk group by IUC
    risk_keys <- vapply(ds, function(d) {
      paste(d$cycle %||% "", d$risk_name %||% "", sep = "||")
    }, character(1))
    groups <- split(seq_along(ds), risk_keys)
    result <- list()
    seq_no <- 1L
    for (gk in names(groups)) {
      subset <- ds[groups[[gk]]]
      split_pts <- split_controls_by_iuc(subset)
      for (pt in split_pts) {
        pt$control_id <- sprintf("%s-%02d", nzchar_or(pt$control_id, "CD"), seq_no)
        # Prefer first draft id base; ensure uniqueness across splits
        if (length(split_pts) > 1 || length(groups) > 0) {
          pt$control_id <- sprintf("CP-%03d", seq_no)
        }
        pt$summary_description <- assemble_summary_description(pt)
        pt$detailed_description <- assemble_control_paragraph(pt)
        pt$validation <- validate_control_design(pt)
        result[[length(result) + 1]] <- pt
        seq_no <- seq_no + 1L
      }
    }
    controls(result)
    n_split <- length(result)
    n_draft <- length(ds)
    msg <- if (n_split > n_draft) {
      sprintf("已產生 %d 個控制點（草稿 %d；因 IUC 差異分拆）", n_split, n_draft)
    } else if (n_split < n_draft) {
      sprintf("已產生 %d 個控制點（草稿 %d；相同 IUC 已合併）", n_split, n_draft)
    } else {
      sprintf("已產生 %d 個控制點", n_split)
    }
    showNotification(msg, type = "message")
  })

  controls_df <- reactive({
    cs <- controls()
    if (!length(cs)) {
      return(data.frame(
        ControlID = character(), 循環 = character(), 風險 = character(),
        IUC = character(), 摘要 = character(), 缺漏 = character(),
        stringsAsFactors = FALSE
      ))
    }
    data.frame(
      ControlID = vapply(cs, function(x) x$control_id, character(1)),
      循環 = vapply(cs, function(x) x$cycle, character(1)),
      風險 = vapply(cs, function(x) x$risk_name, character(1)),
      IUC = vapply(cs, function(x) x$iuc_or_system, character(1)),
      摘要 = vapply(cs, function(x) x$summary_description, character(1)),
      缺漏 = vapply(cs, function(x) {
        if (isTRUE(x$validation$ok)) "完整" else paste(x$validation$missing, collapse = "、")
      }, character(1)),
      stringsAsFactors = FALSE
    )
  })

  output$control_table <- renderDT({
    datatable(
      controls_df(),
      selection = "single",
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })

  selected_control <- reactive({
    s <- input$control_table_rows_selected
    cs <- controls()
    if (is.null(s) || !length(cs)) return(NULL)
    cs[[s]]
  })

  output$control_paragraph <- renderText({
    c <- selected_control()
    if (is.null(c)) return("請於上方表格選取一筆控制點。")
    c$detailed_description
  })

  output$control_summary <- renderText({
    c <- selected_control()
    if (is.null(c)) return("")
    c$summary_description
  })

  output$control_validation <- renderUI({
    c <- selected_control()
    if (is.null(c)) return(NULL)
    v <- c$validation
    if (isTRUE(v$ok)) {
      div(class = "alert alert-success", "此控制點已涵蓋 Form 4120SR 設計敘述必要元素。")
    } else {
      div(class = "alert alert-warning",
          strong("缺漏："), paste(v$missing, collapse = "、"))
    }
  })

  output$download_json <- downloadHandler(
    filename = function() sprintf("control-points-%s.json", format(Sys.time(), "%Y%m%d-%H%M%S")),
    content = function(file) {
      payload <- list(drafts = drafts(), controls = controls())
      write_json(payload, file, auto_unbox = TRUE, pretty = TRUE, force = TRUE)
    }
  )

  output$download_csv <- downloadHandler(
    filename = function() sprintf("control-points-%s.csv", format(Sys.time(), "%Y%m%d-%H%M%S")),
    content = function(file) {
      cs <- controls()
      if (!length(cs)) {
        write.csv(data.frame(), file, row.names = FALSE, fileEncoding = "UTF-8")
        return()
      }
      df <- data.frame(
        control_id = vapply(cs, `[[`, "", "control_id"),
        cycle = vapply(cs, `[[`, "", "cycle"),
        risk_name = vapply(cs, `[[`, "", "risk_name"),
        iuc_or_system = vapply(cs, `[[`, "", "iuc_or_system"),
        summary_description = vapply(cs, `[[`, "", "summary_description"),
        detailed_description = vapply(cs, `[[`, "", "detailed_description"),
        stringsAsFactors = FALSE
      )
      write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui, server)
