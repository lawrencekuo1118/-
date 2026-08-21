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
source(file.path(root, "R", "objective_activity.R"), local = TRUE)
source(file.path(root, "R", "pbc_registry.R"), local = TRUE)
source(file.path(root, "R", "rcm_csa.R"), local = TRUE)
source(file.path(root, "R", "library.R"), local = TRUE)
source(file.path(root, "R", "draft_store.R"), local = TRUE)

fill_inputs_from_ctrl <- function(session, ctrl) {
  if (is.null(ctrl)) return()
  updateSelectInput(session, "cycle", selected = ctrl$cycle %||% CYCLES_NINE[[1]])
  # ① 流程資訊
  updateTextInput(session, "sub_process_id", value = ctrl$sub_process_id %||% "")
  updateTextInput(session, "sub_process", value = ctrl$sub_process %||% "")
  updateTextInput(session, "control_id", value = ctrl$control_id %||% ctrl$library_id %||% "")
  # ② 風險資訊
  updateTextInput(session, "risk_factor", value = ctrl$risk_factor %||% ctrl$risk_name %||% "")
  updateTextInput(session, "risk_name", value = ctrl$risk_name %||% ctrl$risk_factor %||% "")
  updateTextAreaInput(session, "risk_description", value = ctrl$risk_description %||% "")
  rc <- normalize_risk_category(ctrl)
  updateSelectInput(session, "risk_category", selected = if (nzchar(rc)) rc else "")
  updateTextInput(session, "significant_account",
                  value = {
                    ac <- ctrl$significant_account %||% ""
                    if (identical(ac, "NA")) "" else ac
                  })
  # ③ 控制資訊
  updateTextAreaInput(session, "control_objective", value = ctrl$control_objective %||% "")
  updateTextAreaInput(session, "control_activity", value = ctrl$control_activity %||% "")
  ct <- normalize_control_type_manual_auto(ctrl$nature %||% ctrl$control_type)
  at <- normalize_control_activity_type_pd(ctrl$approach %||% ctrl$control_activity_type)
  updateSelectInput(session, "nature", selected = if (nzchar(ct)) ct else "")
  updateSelectInput(session, "approach", selected = if (nzchar(at)) at else "")
  freq <- ctrl$frequency %||% ""
  if (nzchar(freq) && !(freq %in% FREQUENCY_CHOICES)) {
    updateSelectInput(session, "frequency", choices = unique(c(FREQUENCY_CHOICES, freq)), selected = freq)
  } else if (nzchar(freq)) {
    updateSelectInput(session, "frequency", selected = freq)
  }
  updateTextInput(session, "responsible_unit", value = ctrl$responsible_unit %||% "")
  updateTextAreaInput(session, "iuc_or_system",
                      value = ctrl$related_system %||% ctrl$iuc_or_system %||% "")
  updateTextAreaInput(session, "company_status",
                      value = ctrl$company_status %||% ctrl$detailed_description %||% "")
  updateTextAreaInput(session, "design_gap_note", value = ctrl$design_gap_note %||% "")
  updateTextInput(session, "related_policy", value = ctrl$related_policy %||% "")
  updateTextInput(session, "related_law", value = ctrl$related_law %||% "")
  updateTextInput(session, "related_document",
                  value = ctrl$related_document %||% ctrl$outputs %||% "")
  # ④ 控制分析與評估
  updateSelectInput(session, "effectiveness", selected = ctrl$effectiveness %||% "")
  updateTextAreaInput(session, "residual_risk", value = ctrl$residual_risk %||% "")
  updateTextAreaInput(session, "improvement", value = ctrl$improvement %||% "")
  # 進階 4120SR
  updateTextAreaInput(session, "risk_attr_financial",
                      value = gsub("^\\[[^\\]]+\\]\\s*", "", ctrl$risk_attr_financial %||% ""))
  updateTextAreaInput(session, "risk_attr_operations",
                      value = gsub("^\\[[^\\]]+\\]\\s*", "", ctrl$risk_attr_operations %||% ""))
  updateTextAreaInput(session, "risk_attr_compliance",
                      value = gsub("^\\[[^\\]]+\\]\\s*", "", ctrl$risk_attr_compliance %||% ""))
  updateSelectizeInput(session, "type", selected = ctrl$type %||% TYPE_CHOICES[[1]])
  updateTextAreaInput(session, "inputs", value = ctrl$inputs %||% "")
  updateTextAreaInput(session, "review_steps", value = ctrl$review_steps %||% "")
  updateTextAreaInput(session, "outputs", value = ctrl$outputs %||% ctrl$related_document %||% "")
  updateTextAreaInput(session, "investigation_threshold", value = ctrl$investigation_threshold %||% "")
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
    selectInput("cycle", NULL, choices = CYCLES_NINE, selected = "電腦化資訊系統循環"),
    textInput("lib_query", NULL, placeholder = "搜尋完美範本…"),
    selectInput("lib_pick", NULL, choices = c("① 優先：從範本庫套用…" = "")),
    div(
      class = "d-flex gap-1 flex-wrap",
      actionButton("apply_lib", "套用", class = "btn-sm btn-primary"),
      actionButton("save_to_lib", "存入庫", class = "btn-sm btn-outline-success")
    ),
    tags$hr(class = "my-2"),
    tags$strong(class = "small", "引導選取（候選來自範本庫）"),
    selectInput("cascade_sub", NULL, choices = c("② 子作業…" = "")),
    selectInput("cascade_risk", NULL, choices = c("③ 風險因素…" = "")),
    selectInput("cascade_objective", NULL, choices = c("④ 控制目標…" = "")),
    selectInput("cascade_activity", NULL, choices = c("⑤ 控制活動…" = "")),
    selectInput("cascade_iuc", NULL, choices = c("⑥ IUC／相關系統…" = "")),
    actionButton("apply_cascade", "套用引導選取", class = "btn-sm btn-primary w-100"),
    tags$hr(class = "my-2"),
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
        # ---- 依鯨鏈 RCM 標題列分組防呆 ----
        p(
          class = "small text-muted mb-2",
          "欄位依鯨鏈 RCM 標題列分組：",
          strong("流程｜風險｜控制｜分析"),
          "。", strong("控制目標 ≠ 控制活動"),
          "；", strong("控制類型（人工/自動）≠ 控制活動類型（預防/偵測）"), "。"
        ),
        accordion(
          id = "rcm_design_groups",
          open = c("① 流程資訊", "② 風險資訊", "③ 控制資訊（目標 ≠ 活動；類型欄勿對調）"),
          accordion_panel(
            "① 流程資訊",
            textInput("sub_process_id", NULL, placeholder = "子作業編號（例：EC-101）"),
            textInput("sub_process", NULL, placeholder = "子作業名稱"),
            textInput("control_id", NULL, value = "", placeholder = "控制編號（空則自動 EC-101-01）")
          ),
          accordion_panel(
            "② 風險資訊",
            textInput("risk_factor", NULL, placeholder = "風險因素（Risk Factor）"),
            textInput("risk_name", NULL, placeholder = "風險簡稱（可同因素；亦可自訂）"),
            textAreaInput("risk_description", NULL, rows = 2, placeholder = "風險描述（Risk Description）"),
            selectInput("risk_category", NULL, choices = c("風險類別（報導面／營運面／遵循面）…" = "", RISK_CATEGORY_CHOICES)),
            textInput("significant_account", NULL, placeholder = "會計科目（無則 NA）")
          ),
          accordion_panel(
            "③ 控制資訊（目標 ≠ 活動；類型欄勿對調）",
            textAreaInput("control_objective", NULL, rows = 2,
                          placeholder = "控制目標 Why：確保／防止…（結果，勿寫步驟）"),
            textAreaInput("control_activity", NULL, rows = 2,
                          placeholder = "控制活動 How：誰＋動作＋表單／系統（勿重述目標；單一活動僅對應一種預防/偵測）"),
            uiOutput("oa_live_check"),
            uiOutput("type_live_check"),
            div(
              class = "d-flex gap-1 flex-wrap mb-2",
              actionButton("oa_split_suggest", "拆分建議", class = "btn-sm btn-outline-secondary"),
              actionButton("oa_swap", "對調目標/活動", class = "btn-sm btn-outline-secondary")
            ),
            layout_columns(
              col_widths = c(6, 6),
              selectInput("nature", NULL, choices = c("控制類型（人工/自動）…" = "", CONTROL_TYPE_MANUAL_AUTO)),
              selectInput("approach", NULL, choices = c("控制活動類型（預防/偵測）…" = "", CONTROL_ACTIVITY_TYPE_PD))
            ),
            layout_columns(
              col_widths = c(6, 6),
              selectInput("frequency", NULL,
                          choices = unique(c(FREQUENCY_CHOICES, "持續")),
                          selected = FREQUENCY_CHOICES[[4]]),
              textInput("responsible_unit", NULL, placeholder = "流程負責單位")
            ),
            selectizeInput(
              "pbc_apply", NULL, choices = NULL, multiple = TRUE,
              options = list(placeholder = "套用 IUC／PBC（原名→新名）→相關系統／文件")
            ),
            textAreaInput("iuc_or_system", NULL, rows = 1, placeholder = "相關系統／IUC（不同則分拆；無則可自訂並存庫）"),
            textAreaInput("company_status", NULL, rows = 3,
                          placeholder = "控制現況描述（選定目標／活動／IUC 後再書寫）"),
            textAreaInput("design_gap_note", NULL, rows = 2, placeholder = "控制設計差異說明"),
            layout_columns(
              col_widths = c(6, 6),
              textInput("related_policy", NULL, placeholder = "相關政策或程序"),
              textInput("related_law", NULL, placeholder = "相關法令")
            ),
            textInput("related_document", NULL, placeholder = "相關文件")
          ),
          accordion_panel(
            "④ 控制分析與評估",
            selectInput("effectiveness", NULL, choices = c("控制有效性評估…" = "", "有效", "無效")),
            textAreaInput("residual_risk", NULL, rows = 1, placeholder = "可能潛在風險"),
            textAreaInput("improvement", NULL, rows = 1, placeholder = "建議改善方式")
          ),
          accordion_panel(
            "進階（4120SR／三大屬性細節）",
            layout_columns(
              col_widths = c(4, 8),
              textInput("attr_label_fr", NULL, value = "財務報導"),
              textAreaInput("risk_attr_financial", NULL, rows = 1, placeholder = "屬性1細節")
            ),
            layout_columns(
              col_widths = c(4, 8),
              textInput("attr_label_op", NULL, value = "營運"),
              textAreaInput("risk_attr_operations", NULL, rows = 1, placeholder = "屬性2細節")
            ),
            layout_columns(
              col_widths = c(4, 8),
              textInput("attr_label_cp", NULL, value = "法令遵循"),
              textAreaInput("risk_attr_compliance", NULL, rows = 1, placeholder = "屬性3細節")
            ),
            selectizeInput(
              "assertions", NULL, choices = ASSERTION_CHOICES, multiple = TRUE,
              selected = ASSERTION_CHOICES[1:2],
              options = list(create = TRUE, placeholder = "聲明（輔助）")
            ),
            selectInput("romm_classification", NULL, choices = ROMM_CLASS_CHOICES),
            selectizeInput("type", NULL, choices = TYPE_CHOICES,
                           options = list(create = TRUE, placeholder = "Form 4120SR Type")),
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
          actionButton("generate_controls", "產生控制點＝RCM列", class = "btn-success btn-sm")
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
        fileInput("upload_lib", NULL, buttonLabel = "匯入 CSV／JSON／RCM xlsx",
                  accept = c(".csv", ".json", ".xlsx", ".xls")),
        checkboxInput("lib_overwrite", "同 ID 則覆蓋", TRUE),
        actionButton("import_jinglian_seed", "載入鯨鏈資訊循環 RCM（首批）",
                     class = "btn-sm btn-outline-primary w-100 mb-2"),
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
      p(class = "text-muted small mb-1",
        "完成控制點設計＝完成 RCM 一列（鯨鏈標題列）。欄位群組：",
        paste(names(RCM_HEADER_GROUPS), collapse = " ｜ "),
        "。", strong("控制目標≠控制活動"), "；類型欄防呆見「設計檢核」。"),
      DTOutput("rcm_table"),
      downloadButton("download_rcm", "下載 RCM CSV", class = "btn-sm"),
      tags$hr(),
      tags$strong("缺漏／缺文件／控制缺失"),
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
    seed <- seed_control_library()
    jl_path <- file.path(root, "templates", "鯨鏈科技_資訊循環_RCM_v1_0820.xlsx")
    if (file.exists(jl_path)) {
      seed <- tryCatch(
        import_control_library_file(jl_path, seed, overwrite = TRUE),
        error = function(e) seed
      )
    }
    save_control_library(seed, lib_path_json, lib_path_csv)
  }
  lib <- reactiveVal(load_control_library(lib_path_json, fallback_seed = TRUE))
  # If persisted library lacks 鯨鏈首批，合併一次（不覆蓋既有同 ID 以外的新增）
  observeEvent(TRUE, {
    jl_path <- file.path(root, "templates", "鯨鏈科技_資訊循環_RCM_v1_0820.xlsx")
    cur <- lib()
    has_jl <- any(vapply(cur, function(x) grepl("^JL-", x$library_id %||% ""), logical(1)))
    if (!has_jl && file.exists(jl_path)) {
      merged <- tryCatch(
        import_control_library_file(jl_path, cur, overwrite = FALSE),
        error = function(e) cur
      )
      if (length(merged) > length(cur)) {
        lib(persist_lib(merged))
        refresh_lib_choices()
      }
    }
  }, once = TRUE)
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
      sub_process_id = input$sub_process_id %||% "",
      sub_process = input$sub_process %||% "",
      risk_factor = input$risk_factor %||% "",
      risk_name = {
        rn <- trimws(input$risk_name %||% "")
        if (nzchar(rn)) rn else trimws(input$risk_factor %||% "")
      },
      risk_description = input$risk_description %||% "",
      risk_category = input$risk_category %||% "",
      risk_attr_financial = labelize(input$attr_label_fr, input$risk_attr_financial),
      risk_attr_operations = labelize(input$attr_label_op, input$risk_attr_operations),
      risk_attr_compliance = labelize(input$attr_label_cp, input$risk_attr_compliance),
      romm_classification = input$romm_classification %||% "",
      significant_account = input$significant_account %||% "",
      assertions = paste(input$assertions %||% character(), collapse = "；"),
      control_objective = input$control_objective %||% "",
      control_activity = input$control_activity %||% "",
      company_status = input$company_status %||% "",
      design_gap_note = input$design_gap_note %||% "",
      frequency = input$frequency %||% "",
      responsible_unit = input$responsible_unit %||% "",
      iuc_or_system = input$iuc_or_system %||% "",
      related_system = input$iuc_or_system %||% "",
      related_policy = input$related_policy %||% "",
      related_law = input$related_law %||% "",
      related_document = input$related_document %||% "",
      effectiveness = input$effectiveness %||% "",
      residual_risk = input$residual_risk %||% "",
      improvement = input$improvement %||% "",
      nature = input$nature %||% "",
      approach = input$approach %||% "",
      control_type = input$nature %||% "",
      control_activity_type = input$approach %||% "",
      type = input$type %||% "",
      inputs = input$inputs %||% "",
      review_steps = input$review_steps %||% "",
      outputs = {
        out <- trimws(input$outputs %||% "")
        if (nzchar(out)) out else trimws(input$related_document %||% "")
      },
      investigation_threshold = input$investigation_threshold %||% "",
      dependent_controls = "",
      key_control = "Y"
    )
  }

  form_snapshot <- function() {
    list(
      company = input$company, cycle = input$cycle,
      sub_process_id = input$sub_process_id, sub_process = input$sub_process,
      risk_factor = input$risk_factor, risk_name = input$risk_name,
      risk_description = input$risk_description, risk_category = input$risk_category,
      control_id = input$control_id, control_objective = input$control_objective,
      control_activity = input$control_activity, frequency = input$frequency,
      responsible_unit = input$responsible_unit, iuc_or_system = input$iuc_or_system,
      nature = input$nature, approach = input$approach,
      company_status = input$company_status, design_gap_note = input$design_gap_note
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
  output$oa_live_check <- renderUI({
    chk <- rcm_objective_activity_check(input$control_objective, input$control_activity)
    cls <- if (isTRUE(chk$ok)) "alert alert-success py-1 mb-2" else "alert alert-danger py-1 mb-2"
    div(class = cls, format_oa_check_html(chk))
  })
  output$type_live_check <- renderUI({
    tchk <- rcm_type_fields_check(input$nature, input$approach)
    cls <- if (isTRUE(tchk$ok)) "alert alert-secondary py-1 mb-2" else "alert alert-warning py-1 mb-2"
    div(class = cls, tags$small(tags$strong("類型欄防呆："), tchk$msg))
  })

  # ---- Cascade candidates from library (cycle → 子作業 → 風險 → 目標 → 活動 → IUC) ----
  cascade_pool <- reactive({
    items <- filter_library(lib(), cycle_filter = input$cycle, query = NULL)
    lapply(items, function(it) it$control)
  })

  observe({
    pool <- cascade_pool()
    subs <- unique(vapply(pool, function(c) {
      paste(c$sub_process_id %||% "", c$sub_process %||% "", sep = "||")
    }, character(1)))
    subs <- subs[nzchar(gsub("\\|", "", subs))]
    labels <- vapply(subs, function(k) {
      parts <- strsplit(k, "\\|\\|", perl = TRUE)[[1]]
      sprintf("%s｜%s", parts[[1]], if (length(parts) > 1) parts[[2]] else "")
    }, character(1))
    ch <- c("② 子作業…" = "", stats::setNames(subs, labels))
    updateSelectInput(session, "cascade_sub", choices = ch,
                      selected = if ((input$cascade_sub %||% "") %in% subs) input$cascade_sub else "")
  })

  observe({
    pool <- cascade_pool()
    sub_key <- input$cascade_sub %||% ""
    if (nzchar(sub_key)) {
      parts <- strsplit(sub_key, "\\|\\|", perl = TRUE)[[1]]
      spid <- parts[[1]]
      spn <- if (length(parts) > 1) parts[[2]] else ""
      pool <- Filter(function(c) {
        identical(c$sub_process_id %||% "", spid) ||
          (nzchar(spn) && identical(c$sub_process %||% "", spn))
      }, pool)
    }
    risks <- unique(vapply(pool, function(c) c$risk_factor %||% c$risk_name %||% "", character(1)))
    risks <- risks[nzchar(risks)]
    ch <- c("③ 風險因素…" = "", stats::setNames(risks, risks), "＋自訂新增" = "__custom__")
    updateSelectInput(session, "cascade_risk", choices = ch,
                      selected = if ((input$cascade_risk %||% "") %in% c(risks, "__custom__")) input$cascade_risk else "")
  })

  observe({
    pool <- cascade_pool()
    rk <- input$cascade_risk %||% ""
    if (nzchar(rk) && !identical(rk, "__custom__")) {
      pool <- Filter(function(c) {
        identical(c$risk_factor %||% "", rk) || identical(c$risk_name %||% "", rk)
      }, pool)
    }
    objs <- unique(vapply(pool, function(c) c$control_objective %||% "", character(1)))
    objs <- objs[nzchar(objs)]
    labels <- vapply(objs, function(o) {
      if (nchar(o) > 48) paste0(substr(o, 1, 48), "…") else o
    }, character(1))
    ch <- c("④ 控制目標…" = "", stats::setNames(objs, labels), "＋自訂新增" = "__custom__")
    updateSelectInput(session, "cascade_objective", choices = ch,
                      selected = if ((input$cascade_objective %||% "") %in% c(objs, "__custom__")) input$cascade_objective else "")
  })

  observe({
    pool <- cascade_pool()
    obj <- input$cascade_objective %||% ""
    if (nzchar(obj) && !identical(obj, "__custom__")) {
      pool <- Filter(function(c) identical(c$control_objective %||% "", obj), pool)
    }
    acts <- unique(vapply(pool, function(c) {
      paste(c$control_activity %||% "", normalize_control_activity_type_pd(c$approach %||% c$control_activity_type), sep = "||")
    }, character(1)))
    acts <- acts[nzchar(gsub("\\|", "", acts))]
    labels <- vapply(acts, function(k) {
      parts <- strsplit(k, "\\|\\|", perl = TRUE)[[1]]
      act <- parts[[1]]
      pd <- if (length(parts) > 1) parts[[2]] else ""
      short <- if (nchar(act) > 40) paste0(substr(act, 1, 40), "…") else act
      sprintf("[%s] %s", if (nzchar(pd)) pd else "屬性未定", short)
    }, character(1))
    ch <- c("⑤ 控制活動…" = "", stats::setNames(acts, labels), "＋自訂新增" = "__custom__")
    updateSelectInput(session, "cascade_activity", choices = ch,
                      selected = if ((input$cascade_activity %||% "") %in% c(acts, "__custom__")) input$cascade_activity else "")
  })

  observe({
    pool <- cascade_pool()
    iucs <- unique(vapply(pool, function(c) c$related_system %||% c$iuc_or_system %||% "", character(1)))
    iucs <- iucs[nzchar(iucs)]
    # also from PBC registry
    pbc_names <- unique(c(pbc_reg()$reviewed_name, pbc_reg()$client_pbc_name))
    pbc_names <- pbc_names[nzchar(as.character(pbc_names))]
    all_iuc <- unique(c(iucs, as.character(pbc_names)))
    ch <- c("⑥ IUC／相關系統…" = "", stats::setNames(all_iuc, all_iuc), "＋自訂新增" = "__custom__")
    updateSelectInput(session, "cascade_iuc", choices = ch,
                      selected = if ((input$cascade_iuc %||% "") %in% c(all_iuc, "__custom__")) input$cascade_iuc else "")
  })

  observeEvent(input$apply_cascade, {
    pool <- cascade_pool()
    sub_key <- input$cascade_sub %||% ""
    if (nzchar(sub_key)) {
      parts <- strsplit(sub_key, "\\|\\|", perl = TRUE)[[1]]
      updateTextInput(session, "sub_process_id", value = parts[[1]])
      if (length(parts) > 1) updateTextInput(session, "sub_process", value = parts[[2]])
      pool <- Filter(function(c) {
        identical(c$sub_process_id %||% "", parts[[1]]) ||
          (length(parts) > 1 && identical(c$sub_process %||% "", parts[[2]]))
      }, pool)
    }
    rk <- input$cascade_risk %||% ""
    if (nzchar(rk) && !identical(rk, "__custom__")) {
      updateTextInput(session, "risk_factor", value = rk)
      updateTextInput(session, "risk_name", value = rk)
      hit <- Filter(function(c) identical(c$risk_factor %||% "", rk) || identical(c$risk_name %||% "", rk), pool)
      if (length(hit)) {
        fill_inputs_from_ctrl(session, hit[[1]])
        pool <- hit
      }
    }
    obj <- input$cascade_objective %||% ""
    if (nzchar(obj) && !identical(obj, "__custom__")) {
      updateTextAreaInput(session, "control_objective", value = obj)
      pool <- Filter(function(c) identical(c$control_objective %||% "", obj), pool)
      if (length(pool) == 1L) fill_inputs_from_ctrl(session, pool[[1]])
    }
    act_key <- input$cascade_activity %||% ""
    if (nzchar(act_key) && !identical(act_key, "__custom__")) {
      parts <- strsplit(act_key, "\\|\\|", perl = TRUE)[[1]]
      updateTextAreaInput(session, "control_activity", value = parts[[1]])
      if (length(parts) > 1 && nzchar(parts[[2]])) {
        updateSelectInput(session, "approach", selected = parts[[2]])
      }
      hit <- Filter(function(c) identical(c$control_activity %||% "", parts[[1]]), pool)
      if (length(hit)) {
        # preserve OA already set; fill remaining Jinglian fields
        fill_inputs_from_ctrl(session, hit[[1]])
        updateTextAreaInput(session, "control_objective", value = obj)
        updateTextAreaInput(session, "control_activity", value = parts[[1]])
        if (length(parts) > 1 && nzchar(parts[[2]])) {
          updateSelectInput(session, "approach", selected = parts[[2]])
        }
      }
    }
    iuc <- input$cascade_iuc %||% ""
    if (nzchar(iuc) && !identical(iuc, "__custom__")) {
      updateTextAreaInput(session, "iuc_or_system", value = iuc)
    }
    # auto control id from sub_process_id
    spid <- input$sub_process_id %||% {
      if (nzchar(sub_key)) strsplit(sub_key, "\\|\\|", perl = TRUE)[[1]][[1]] else ""
    }
    if (nzchar(spid) && !nzchar(trimws(input$control_id %||% ""))) {
      n <- length(controls()) + length(drafts()) + 1L
      updateTextInput(session, "control_id", value = sprintf("%s-%02d", spid, n))
    }
    showNotification("已套用引導選取；請補寫控制現況描述", type = "message")
  })

  observeEvent(input$import_jinglian_seed, {
    path <- file.path(root, "templates", "鯨鏈科技_資訊循環_RCM_v1_0820.xlsx")
    if (!file.exists(path)) {
      return(showNotification("找不到鯨鏈 RCM 範本檔", type = "error"))
    }
    tryCatch({
      new_lib <- import_control_library_file(path, lib(), overwrite = isTRUE(input$lib_overwrite))
      lib(persist_lib(new_lib))
      refresh_lib_choices()
      showNotification(sprintf("已載入鯨鏈 RCM，範本庫共 %d 筆", length(new_lib)), type = "message")
    }, error = function(e) showNotification(conditionMessage(e), type = "error"))
  })

  observeEvent(input$oa_swap, {
    o <- input$control_objective %||% ""
    a <- input$control_activity %||% ""
    updateTextAreaInput(session, "control_objective", value = a)
    updateTextAreaInput(session, "control_activity", value = o)
  })

  observeEvent(input$oa_split_suggest, {
    # Prefer splitting whichever field looks mixed; else join both
    blob <- paste(
      c(input$control_objective %||% "", input$control_activity %||% ""),
      collapse = "。"
    )
    sug <- suggest_objective_activity_split(blob)
    updateTextAreaInput(session, "control_objective", value = sug$objective)
    updateTextAreaInput(session, "control_activity", value = sug$activity)
    showNotification(sug$note, type = "message")
  })

  output$live_validation <- renderUI({
    d <- current_draft_from_inputs()
    gaps <- detect_design_gaps(d)
    chk <- rcm_objective_activity_check(d$control_objective, d$control_activity)
    ready <- is_rcm_row_ready(d)
    if (isTRUE(ready$ready) && isTRUE(chk$ok)) {
      div(class = "alert alert-success py-1 mb-2",
          "可寫入 RCM 一列｜", format_oa_check_html(chk))
    } else {
      high <- gaps[gaps$severity == "高", , drop = FALSE]
      summary <- if (!isTRUE(chk$ok)) chk$msg
      else if (nrow(high)) paste(sprintf("[%s] %s", high$category, high$gap_item), collapse = "；")
      else paste(gaps$gap_item, collapse = "；")
      div(class = "alert alert-warning py-1 mb-2", paste0("尚不可定稿 RCM：", summary))
    }
  })

  observeEvent(input$add_draft, {
    d <- current_draft_from_inputs()
    chk <- rcm_objective_activity_check(d$control_objective, d$control_activity)
    if (!isTRUE(chk$ok)) {
      return(showNotification(
        paste0("目標／活動未分開，無法加入：", chk$msg),
        type = "error", duration = 8
      ))
    }
    tchk <- rcm_type_fields_check(d$nature, d$approach)
    if (!isTRUE(tchk$ok)) {
      return(showNotification(paste0("類型欄位防呆：", tchk$msg), type = "error", duration = 8))
    }
    d$draft_id <- next_id()
    if (!nzchar(trimws(d$control_id))) {
      d$control_id <- derive_control_id(d, length(drafts()) + 1L)
    }
    drafts(c(drafts(), list(d)))
    next_id(next_id() + 1L)
    updateTextInput(session, "control_id", value = "")
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
    new_d <- current_draft_from_inputs()
    chk <- rcm_objective_activity_check(new_d$control_objective, new_d$control_activity)
    if (!isTRUE(chk$ok)) {
      return(showNotification(paste0("目標／活動未分開：", chk$msg), type = "error", duration = 8))
    }
    ds <- drafts()
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
        pt$control_id <- derive_control_id(pt, seq_no)
        pt$risk_id <- derive_risk_id(pt, seq_no)
        if (is_blank(pt$company_status) || !nzchar(trimws(pt$company_status %||% ""))) {
          pt$company_status <- assemble_control_paragraph(pt)
        }
        pt$summary_description <- assemble_summary_description(pt)
        pt$detailed_description <- pt$company_status
        pt$validation <- validate_control_design(pt)
        pt$rcm_ready <- is_rcm_row_ready(pt)
        result[[length(result) + 1]] <- pt
        seq_no <- seq_no + 1L
      }
    }
    controls(result)
    n_ready <- sum(vapply(result, function(x) isTRUE(x$rcm_ready$ready), logical(1)))
    showNotification(
      sprintf("已產生 %d 控制點＝%d 列 RCM（其中 %d 列設計檢核通過）",
              length(result), length(result), n_ready),
      type = "message"
    )
    if (isTRUE(input$autosave_draft)) do_save_draft(quiet = TRUE)
  })

  controls_df <- reactive({
    cs <- controls()
    if (!length(cs)) {
      return(data.frame(控制編號 = character(), IUC = character(), RCM = character(),
                        摘要 = character(), stringsAsFactors = FALSE))
    }
    data.frame(
      控制編號 = vapply(cs, function(x) x$control_id, ""),
      IUC = vapply(cs, function(x) x$iuc_or_system, ""),
      RCM = vapply(cs, function(x) if (isTRUE(x$rcm_ready$ready)) "可入列" else "待補", ""),
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
