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
source(file.path(root, "R", "cascade.R"), local = TRUE)
source(file.path(root, "R", "draft_store.R"), local = TRUE)

# UI label with required asterisk
lab_req <- function(txt) {
  tagList(txt, tags$span("*", class = "text-danger ms-1", title = "設計必填"))
}
lab_opt <- function(txt) {
  tagList(txt, tags$span(class = "text-muted small ms-1", "選填"))
}

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
                    ac <- trimws(as.character(ctrl$significant_account %||% ""))
                    if (is_reporting_risk_category(ctrl$risk_category %||% "")) {
                      if (!nzchar(ac) || identical(toupper(ac), "NA")) "" else ac
                    } else {
                      ""
                    }
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
  updateSelectizeInput(session, "related_law",
                       selected = {
                         raw <- trimws(as.character(ctrl$related_law %||% ""))
                         if (!nzchar(raw)) character(0) else trimws(unlist(strsplit(raw, "[;；|/]+")))
                       })
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
  header = tags$script(HTML("
    Shiny.addCustomMessageHandler('toggleAccount', function(msg) {
      var el = document.getElementById('significant_account');
      if (!el) return;
      el.disabled = !msg.enabled;
      el.readOnly = !msg.enabled;
      el.classList.toggle('bg-light', !msg.enabled);
    });
    Shiny.addCustomMessageHandler('toggleLaw', function(msg) {
      var el = document.getElementById('related_law');
      if (!el) return;
      // selectize
      var $el = $('#related_law');
      if ($el.length && $el[0].selectize) {
        if (msg.enabled) $el[0].selectize.enable();
        else { $el[0].selectize.disable(); $el[0].selectize.clear(); }
      } else {
        el.disabled = !msg.enabled;
      }
    });
  ")),
  fillable = TRUE,
  sidebar = sidebar(
    width = 280,
    open = "desktop",
    selectInput("cycle", "循環", choices = CYCLES_NINE_CHOICES,
                selected = "電腦化資訊系統循環"),
    textInput("lib_query", NULL, placeholder = "搜尋完美範本…"),
    selectInput("lib_pick", NULL, choices = c("① 優先：從範本庫套用…" = "")),
    div(
      class = "d-flex gap-1 flex-wrap",
      actionButton("apply_lib", "套用", class = "btn-sm btn-primary"),
      actionButton("save_to_lib", "存入庫", class = "btn-sm btn-outline-success")
    ),
    checkboxInput("auto_collect_lib", "設計完成自動收集入庫", TRUE),
    uiOutput("lib_count_badge"),
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
        card_header("引導設計（依序選取）"),
        p(
          class = "small text-muted mb-2",
          "流程：", strong("循環 → 子作業 → 風險 → 控制目標 → 控制活動（單一預防/偵測）→ IUC"),
          "；", tags$span(class = "text-danger", "*"), "為", strong("設計必填"),
          "；六大就緒後才可書寫", strong("公司現況"),
          "。控制編號自動順編（如 EC-101-01）。"
        ),
        uiOutput("cascade_step_status"),
        uiOutput("design_required_checklist"),
        # Step 2: 子作業
        selectInput("cascade_sub", NULL, choices = c("② 選擇子作業…" = "")),
        conditionalPanel(
          "input.cascade_sub == '__custom__'",
          layout_columns(
            col_widths = c(4, 8),
            textInput("custom_sub_id", NULL, placeholder = "子作業編號"),
            textInput("custom_sub_name", NULL, placeholder = "子作業名稱")
          )
        ),
        # Step 3: 風險
        selectInput("cascade_risk", NULL, choices = c("③ 選擇風險因素…" = "")),
        uiOutput("cascade_risk_detail"),
        conditionalPanel(
          "input.cascade_risk == '__custom__'",
          textInput("custom_risk_factor", NULL, placeholder = "自訂風險因素"),
          textAreaInput("custom_risk_desc", NULL, rows = 2, placeholder = "自訂風險描述"),
          selectInput("custom_risk_category", NULL,
                      choices = c("風險類別…" = "", RISK_CATEGORY_CHOICES))
        ),
        # Step 4: 目標
        selectInput("cascade_objective", NULL, choices = c("④ 選擇控制目標…" = "")),
        conditionalPanel(
          "input.cascade_objective == '__custom__'",
          textAreaInput("custom_objective", NULL, rows = 2, placeholder = "自訂控制目標（Why）")
        ),
        # Step 5: 活動（標示單一 PD）
        selectInput("cascade_activity", NULL, choices = c("⑤ 選擇控制活動…" = "")),
        conditionalPanel(
          "input.cascade_activity == '__custom__'",
          textAreaInput("custom_activity", NULL, rows = 2, placeholder = "自訂控制活動（How）"),
          selectInput("custom_approach", NULL,
                      choices = c("此活動之控制屬性（僅一種）…" = "", CONTROL_ACTIVITY_TYPE_PD)),
          selectInput("custom_nature", NULL,
                      choices = c("控制類型（人工/自動）…" = "", CONTROL_TYPE_MANUAL_AUTO)),
          selectInput("custom_frequency", NULL, choices = c("頻率…" = "", FREQUENCY_CHOICES)),
          textInput("custom_owner", NULL, placeholder = "流程負責單位")
        ),
        # Step 6: IUC
        selectInput("cascade_iuc", NULL, choices = c("⑥ 選擇 IUC／相關系統…" = "")),
        conditionalPanel(
          "input.cascade_iuc == '__custom__'",
          textInput("custom_iuc", NULL, placeholder = "自訂 IUC／相關系統名稱"),
          checkboxInput("custom_iuc_save", "一併新增至 APP 範本庫／PBC", TRUE)
        ),
        div(
          class = "d-flex gap-1 flex-wrap mb-2",
          actionButton("apply_cascade", "套用引導選取至表單", class = "btn-primary btn-sm"),
          actionButton("save_custom_cascade", "將自訂項存入範本庫", class = "btn-outline-success btn-sm"),
          actionButton("reset_cascade", "重設引導", class = "btn-outline-secondary btn-sm")
        ),
        uiOutput("auto_control_id_box"),
        tags$hr(),
        # ---- 依鯨鏈 RCM 標題列分組（由引導填入；現況欄受鎖）----
        p(
          class = "small text-muted mb-2",
          "下方為 RCM 欄位（引導填入後可微調）。",
          strong("控制目標 ≠ 控制活動"), "；",
          strong("控制類型 ≠ 控制活動類型"), "。"
        ),
        accordion(
          id = "rcm_design_groups",
          open = c("① 流程資訊", "③ 控制資訊（目標 ≠ 活動；類型欄勿對調）"),
          accordion_panel(
            "① 流程資訊",
            textInput("sub_process_id", lab_req("子作業編號"), placeholder = "例：EC-101"),
            textInput("sub_process", lab_req("子作業名稱"), placeholder = "子作業名稱"),
            textInput("control_id", "控制編號", value = "",
                      placeholder = "自動順編（可覆寫）")
          ),
          accordion_panel(
            "② 風險資訊",
            textInput("risk_factor", lab_req("風險因素"), placeholder = "Risk Factor"),
            textInput("risk_name", "風險簡稱", placeholder = "可同因素；亦可自訂"),
            textAreaInput("risk_description", lab_req("風險描述"), rows = 2,
                          placeholder = "Risk Description"),
            selectInput("risk_category", lab_req("風險類別"),
                        choices = c("請選擇…" = "", RISK_CATEGORY_CHOICES)),
            textInput("significant_account", "會計科目", value = "",
                      placeholder = "僅報導面可填且必填"),
            uiOutput("significant_account_hint")
          ),
          accordion_panel(
            "③ 控制資訊（目標 ≠ 活動；類型欄勿對調）",
            textAreaInput("control_objective", lab_req("控制目標"), rows = 2,
                          placeholder = "Why：確保／防止…（結果，勿寫步驟）"),
            textAreaInput("control_activity", lab_req("控制活動"), rows = 2,
                          placeholder = "How：誰＋動作＋表單／系統（單一活動僅一種預防/偵測）"),
            uiOutput("oa_live_check"),
            uiOutput("type_live_check"),
            div(
              class = "d-flex gap-1 flex-wrap mb-2",
              actionButton("oa_split_suggest", "拆分建議", class = "btn-sm btn-outline-secondary"),
              actionButton("oa_swap", "對調目標/活動", class = "btn-sm btn-outline-secondary")
            ),
            layout_columns(
              col_widths = c(6, 6),
              selectInput("nature", lab_req("控制類型"),
                          choices = c("請選擇…" = "", CONTROL_TYPE_MANUAL_AUTO)),
              selectInput("approach", lab_req("控制活動類型"),
                          choices = c("請選擇…" = "", CONTROL_ACTIVITY_TYPE_PD))
            ),
            layout_columns(
              col_widths = c(6, 6),
              selectInput("frequency", lab_req("控制頻率"),
                          choices = unique(c(FREQUENCY_CHOICES, "持續")),
                          selected = "每季"),
              textInput("responsible_unit", lab_req("流程負責單位"),
                        placeholder = "Control Owner")
            ),
            selectizeInput(
              "pbc_apply", "套用 IUC／PBC", choices = NULL, multiple = TRUE,
              options = list(placeholder = "原名→新名")
            ),
            textAreaInput("iuc_or_system", lab_req("相關系統／IUC"), rows = 1,
                          placeholder = "須由引導選擇或自訂入庫"),
            uiOutput("six_rules_box"),
            uiOutput("company_status_lock_msg"),
            # Hidden unlock flag driven by server
            div(style = "display:none;", checkboxInput("status_unlocked", NULL, FALSE)),
            conditionalPanel(
              condition = "input.status_unlocked == true",
              textAreaInput("company_status", lab_opt("控制現況描述"), rows = 5,
                            placeholder = "依六大控制項目書寫公司實際作法；定稿可自動帶入"),
              actionButton("fill_status_scaffold", "帶入六大規則草稿", class = "btn-sm btn-outline-primary mb-2")
            ),
            conditionalPanel(
              condition = "input.status_unlocked != true",
              helpText(class = "text-muted small", "（公司現況欄位將於引導＋六大就緒後顯示）")
            ),
            textAreaInput("design_gap_note", lab_opt("控制設計差異說明"), rows = 2),
            textInput("related_policy", lab_opt("相關政策或程序")),
            selectizeInput(
              "related_law", "相關法令",
              choices = c("請選擇或輸入…" = "", RELATED_LAW_CHOICES),
              multiple = TRUE,
              options = list(create = TRUE, placeholder = "僅遵循面可填；可多選／自訂")
            ),
            uiOutput("related_law_hint"),
            textInput("related_document", lab_opt("相關文件"))
          ),
          accordion_panel(
            "④ 控制分析與評估",
            selectInput("effectiveness", lab_opt("控制有效性評估"),
                        choices = c("…" = "", "有效", "無效")),
            textAreaInput("residual_risk", lab_opt("可能潛在風險"), rows = 1),
            textAreaInput("improvement", lab_opt("建議改善方式"), rows = 1)
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
          actionButton("finalize_rcm_row", "完成設計＝寫入 RCM 一列", class = "btn-success btn-sm"),
          actionButton("add_draft", "暫存佇列（未定稿）", class = "btn-outline-primary btn-sm"),
          actionButton("update_draft", "更新佇列", class = "btn-sm"),
          actionButton("remove_draft", "刪除佇列", class = "btn-sm btn-outline-danger"),
          actionButton("generate_controls", "佇列批次定稿→RCM", class = "btn-outline-success btn-sm"),
          actionButton("collect_ready_to_lib", "RCM列→累積範本庫", class = "btn-outline-success btn-sm")
        )
      ),
      card(
        uiOutput("live_validation"),
        uiOutput("rcm_parity_box"),
        verbatimTextOutput("live_preview"),
        navset_underline(
          nav_panel("RCM 列", DTOutput("control_table"), verbatimTextOutput("control_paragraph")),
          nav_panel("暫存佇列", DTOutput("draft_table"))
        )
      )
    )
  ),
  nav_panel(
    "範本庫",
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("累積制通用範本庫"),
        p(
          class = "text-muted small mb-1",
          "管道：", strong("設計／RCM 就緒列 → 入庫"), "｜",
          strong("CSV／JSON／RCM xlsx 匯入"), "｜",
          strong("自訂引導項"), "。入庫後設計時", strong("優先套用"), "，完善後可覆寫同 ID（累積）。"
        ),
        verbatimTextOutput("lib_stats_text"),
        fileInput("upload_lib", NULL, buttonLabel = "匯入 CSV／JSON／RCM xlsx",
                  accept = c(".csv", ".json", ".xlsx", ".xls")),
        checkboxInput("lib_overwrite", "同 ID 則覆蓋（累積更新）", TRUE),
        actionButton("import_jinglian_seed", "載入鯨鏈資訊循環 RCM（首批）",
                     class = "btn-sm btn-outline-primary w-100 mb-2"),
        tags$hr(class = "my-2"),
        tags$strong(class = "small", "收集入庫"),
        textInput("lib_title_override", NULL, placeholder = "存入時標題（可空）"),
        textInput("lib_tags", NULL, placeholder = "標籤（;分隔）"),
        div(
          class = "d-flex gap-1 flex-wrap",
          actionButton("lib_add_current", "表單→庫", class = "btn-sm btn-primary"),
          actionButton("lib_add_selected_control", "選取控制點→庫", class = "btn-sm"),
          actionButton("lib_add_all_ready", "全部就緒控制點→庫", class = "btn-sm btn-success"),
          actionButton("lib_add_all_drafts", "佇列→庫", class = "btn-sm btn-outline-success")
        ),
        div(
          class = "d-flex gap-1 flex-wrap mt-2",
          actionButton("lib_delete", "刪除選取", class = "btn-sm btn-outline-danger"),
          downloadButton("download_lib_csv", "匯出 CSV", class = "btn-sm"),
          downloadButton("download_lib_json", "匯出 JSON", class = "btn-sm")
        )
      ),
      card(
        DTOutput("lib_table"),
        verbatimTextOutput("lib_preview")
      )
    )
  ),
  nav_panel(
    "參數庫",
    layout_columns(
      col_widths = c(3, 9),
      card(
        card_header("後台參數查詢"),
        p(class = "small text-muted",
          "彙整範本庫／佇列／已定稿 RCM 與系統預設清單中目前已存的參數選項。"),
        selectInput("param_filter", "參數類型", choices = c("全部" = "")),
        textInput("param_query", NULL, placeholder = "搜尋選項值…"),
        actionButton("param_refresh", "重新整理", class = "btn-sm btn-primary"),
        downloadButton("download_params", "下載參數清單 CSV", class = "btn-sm mt-2")
      ),
      card(
        uiOutput("param_stats"),
        DTOutput("param_table")
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
        strong("設計控制點完成＝RCM 一列"),
        "（1 控制點 ↔ 1 RCM 列）。欄位群組：",
        paste(names(RCM_HEADER_GROUPS), collapse = " ｜ "),
        "。", strong("控制目標≠控制活動"), "；類型欄防呆見「設計檢核」。"),
      uiOutput("rcm_count_box"),
      DTOutput("rcm_table"),
      downloadButton("download_rcm", "下載 RCM CSV", class = "btn-sm"),
      tags$hr(),
      tags$strong("缺漏／缺文件／控制缺失"),
      DTOutput("gap_table")
    )
  ),
  nav_panel(
    "① 訪談",
    layout_columns(
      col_widths = c(3, 9),
      card(
        p(class = "small text-muted",
          strong("優先：訪談問題"), "（對齊已定稿 RCM 列）。勾選元素後下載題綱。"),
        selectizeInput(
          "worksheet_controls", NULL, choices = NULL, multiple = TRUE,
          options = list(placeholder = "RCM 控制點（空＝全部已定稿）")
        ),
        checkboxGroupInput("interview_elements", "訪談元素",
                           choices = DESIGN_ELEMENTS, selected = DEFAULT_INTERVIEW_ELEMENTS),
        actionButton("ws_select_core_iv", "訪談核心元素", class = "btn-sm btn-primary"),
        uiOutput("interview_status")
      ),
      card(
        DTOutput("interview_table"),
        downloadButton("download_interview", "下載訪談題綱 CSV", class = "btn-sm")
      )
    )
  ),
  nav_panel(
    "② CSA",
    layout_columns(
      col_widths = c(3, 9),
      card(
        p(class = "small text-muted",
          "在訪談＋RCM 定稿後設計 CSA 測試步驟（測試程序／PBC／預期結果）。"),
        checkboxGroupInput("csa_elements", "CSA 元素",
                           choices = DESIGN_ELEMENTS, selected = DEFAULT_CSA_ELEMENTS),
        actionButton("ws_select_core_csa", "CSA 核心元素", class = "btn-sm"),
        uiOutput("csa_status")
      ),
      card(
        DTOutput("csa_table"),
        downloadButton("download_csa", "下載 CSA 測試步驟 CSV", class = "btn-sm")
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
  # Ensure 鯨鏈／資訊循環首批一定在庫（空庫或缺 JL- 時強制合併）
  observeEvent(TRUE, {
    cur <- lib()
    jl_ids <- sum(vapply(cur, function(x) grepl("^JL-", x$library_id %||% ""), logical(1)))
    need <- length(cur) < 5 || jl_ids < 10
    if (!need) return()
    batch <- file.path(root, "data", "jinglian_it_rcm_batch.json")
    jl_path <- file.path(root, "templates", "鯨鏈科技_資訊循環_RCM_v1_0820.xlsx")
    merged <- cur
    if (file.exists(batch)) {
      merged <- tryCatch(
        merge_libraries(merged, load_control_library(batch, fallback_seed = FALSE), overwrite = FALSE),
        error = function(e) merged
      )
    } else if (file.exists(jl_path)) {
      merged <- tryCatch(
        import_control_library_file(jl_path, merged, overwrite = FALSE),
        error = function(e) merged
      )
    }
    if (!length(merged)) merged <- seed_control_library(TRUE)
    if (length(merged) > length(cur) || jl_ids < 10) {
      lib(persist_lib(merged))
      refresh_lib_choices()
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

  output$lib_count_badge <- renderUI({
    st <- library_stats(lib())
    tags$small(class = "text-muted", sprintf("範本庫累積 %d 筆／%d 循環", st$n, st$n_cycles))
  })
  output$lib_stats_text <- renderText({
    st <- library_stats(lib())
    src <- if (length(st$sources)) {
      paste(sprintf("%s=%s", names(st$sources), unlist(st$sources)), collapse = "；")
    } else "—"
    paste(
      sprintf("累積筆數：%d", st$n),
      sprintf("涵蓋循環：%d（%s）", st$n_cycles, paste(st$cycles, collapse = "、")),
      sprintf("來源：%s", src),
      sep = "\n"
    )
  })

  param_catalog_df <- reactive({
    input$param_refresh
    parameter_catalog(
      library = lib(), drafts = drafts(), controls = controls(),
      presets = list(
        "循環" = unname(CYCLES_NINE_CHOICES),
        "風險類別" = RISK_CATEGORY_CHOICES,
        "控制類型" = CONTROL_TYPE_MANUAL_AUTO,
        "控制活動類型" = CONTROL_ACTIVITY_TYPE_PD,
        "控制頻率" = FREQUENCY_CHOICES,
        "相關法令" = unname(RELATED_LAW_CHOICES)
      )
    )
  })

  observe({
    df <- param_catalog_df()
    params <- sort(unique(df$參數))
    updateSelectInput(session, "param_filter",
                      choices = c("全部" = "", stats::setNames(params, params)),
                      selected = input$param_filter %||% "")
  })

  output$param_stats <- renderUI({
    df <- param_catalog_df()
    tags$p(class = "small text-muted",
           sprintf("共 %d 筆參數選項（%d 類）", nrow(df), length(unique(df$參數))))
  })

  output$param_table <- renderDT({
    df <- param_catalog_df()
    f <- input$param_filter %||% ""
    q <- trimws(input$param_query %||% "")
    if (nzchar(f)) df <- df[df$參數 == f, , drop = FALSE]
    if (nzchar(q)) df <- df[grepl(q, df$選項值, fixed = TRUE) | grepl(q, df$參數, fixed = TRUE), , drop = FALSE]
    datatable(df, rownames = FALSE, filter = "top",
              options = list(pageLength = 25, scrollX = TRUE))
  })

  output$download_params <- downloadHandler(
    filename = function() sprintf("param_catalog_%s.csv", format(Sys.Date(), "%Y%m%d")),
    content = function(file) utils::write.csv(param_catalog_df(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )

  observeEvent(input$param_refresh, {
    # Invalidate catalog reactive via input$param_refresh dependency; confirm to user
    n <- nrow(param_catalog_df())
    showNotification(sprintf("參數庫已重新整理（%d 筆選項）", n), type = "message")
  })

  add_ctrl_to_library <- function(ctrl, title = NULL, tags = character(), source = "manual") {
    if (!is.null(title) && nzchar(trimws(title))) ctrl$title <- trimws(title)
    tag_vec <- unlist(strsplit(as.character(tags %||% ""), "[;；,，|/]+"))
    tag_vec <- trimws(tag_vec)
    tag_vec <- tag_vec[nzchar(tag_vec)]
    res <- collect_controls_to_library(
      lib(), list(ctrl),
      overwrite = TRUE,
      tags = tag_vec,
      source = source,
      quality_gate = FALSE
    )
    lib(persist_lib(res$library))
    refresh_lib_choices()
    if (length(res$items)) res$items[[1]] else NULL
  }

  collect_many_to_lib <- function(ctrls, source = "collect", quality_gate = TRUE) {
    tag_vec <- unlist(strsplit(as.character(input$lib_tags %||% ""), "[;；,，|/]+"))
    tag_vec <- trimws(tag_vec)
    tag_vec <- tag_vec[nzchar(tag_vec)]
    res <- collect_controls_to_library(
      lib(), ctrls,
      overwrite = isTRUE(input$lib_overwrite),
      tags = c(tag_vec, "累積收集"),
      source = source,
      quality_gate = quality_gate
    )
    lib(persist_lib(res$library))
    refresh_lib_choices()
    res
  }

  observeEvent(input$apply_lib, {
    id <- input$lib_pick
    if (!nzchar(id %||% "")) return(showNotification("請先從範本庫選擇", type = "warning"))
    item <- get_library_item(lib(), id)
    if (is.null(item)) return()
    fill_inputs_from_ctrl(session, item$control)
    showNotification(paste("已套用範本：", item$title), type = "message")
  })

  observeEvent(input$save_to_lib, {
    d <- current_draft_from_inputs()
    item <- add_ctrl_to_library(d, title = input$lib_title_override, tags = input$lib_tags, source = "form")
    showNotification(paste("已存入範本庫", item$library_id), type = "message")
  })
  observeEvent(input$lib_add_current, {
    d <- current_draft_from_inputs()
    item <- add_ctrl_to_library(d, title = input$lib_title_override, tags = input$lib_tags, source = "form")
    showNotification(paste("已存入", item$library_id), type = "message")
  })
  observeEvent(input$lib_add_selected_control, {
    s <- input$control_table_rows_selected
    cs <- controls()
    if (is.null(s) || !length(cs)) return(showNotification("請先在設計頁選取控制點", type = "warning"))
    item <- add_ctrl_to_library(cs[[s]], title = input$lib_title_override, tags = input$lib_tags, source = "control")
    showNotification(paste("控制點已存入", item$library_id), type = "message")
  })
  observeEvent(input$lib_add_all_ready, {
    cs <- controls()
    if (!length(cs)) return(showNotification("尚無已產生控制點", type = "warning"))
    ready <- Filter(function(c) isTRUE((c$rcm_ready$ready %||% is_rcm_row_ready(c)$ready)), cs)
    if (!length(ready)) return(showNotification("無 RCM 就緒控制點可入庫", type = "warning"))
    res <- collect_many_to_lib(ready, source = "rcm_ready", quality_gate = TRUE)
    showNotification(
      sprintf("就緒入庫：新增 %d／更新 %d／略過 %d；庫共 %d 筆",
              res$added, res$updated, res$skipped, length(res$library)),
      type = "message", duration = 8
    )
  })
  observeEvent(input$lib_add_all_drafts, {
    ds <- drafts()
    if (!length(ds)) return(showNotification("佇列為空", type = "warning"))
    res <- collect_many_to_lib(ds, source = "draft_queue", quality_gate = TRUE)
    showNotification(
      sprintf("佇列入庫：新增 %d／更新 %d／略過 %d", res$added, res$updated, res$skipped),
      type = "message", duration = 8
    )
  })
  observeEvent(input$collect_ready_to_lib, {
    cs <- controls()
    if (!length(cs)) {
      d <- current_draft_from_inputs()
      res <- collect_many_to_lib(list(d), source = "design_collect", quality_gate = TRUE)
    } else {
      ready <- Filter(function(c) isTRUE((c$rcm_ready$ready %||% is_rcm_row_ready(c)$ready)), cs)
      if (!length(ready)) ready <- cs
      res <- collect_many_to_lib(ready, source = "design_collect", quality_gate = TRUE)
    }
    showNotification(
      sprintf("已收集入累積庫：+%d／覆寫 %d／略過 %d", res$added, res$updated, res$skipped),
      type = "message"
    )
  })
  observeEvent(input$upload_lib, {
    f <- input$upload_lib
    if (is.null(f)) return()
    tryCatch({
      new_lib <- import_control_library_file(f$datapath, lib(), overwrite = isTRUE(input$lib_overwrite))
      new_lib <- lapply(new_lib, function(it) {
        if (is.null(it$source) || identical(it$source, "manual") || identical(it$source, "persisted")) {
          it$source <- "import"
        }
        it
      })
      lib(persist_lib(new_lib))
      refresh_lib_choices()
      showNotification(sprintf("範本庫累積共 %d 筆", length(new_lib)), type = "message")
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
        paste0("來源：", item$source %||% "—", "｜標籤：", paste(item$tags, collapse = ";")),
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
      related_law = {
        v <- input$related_law %||% character(0)
        paste(unique(trimws(as.character(v))), collapse = "；")
      },
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

  # ---- Forced cascade: cycle → 子作業 → 風險 → 目標 → 活動(單一PD) → IUC ----
  cascade_rows <- reactive({
    library_controls_flat(lib(), cycle = input$cycle)
  })

  resolve_cascade_selection <- function() {
    sub_key <- input$cascade_sub %||% ""
    sp_id <- ""; sp_name <- ""
    if (identical(sub_key, "__custom__")) {
      sp_id <- trimws(input$custom_sub_id %||% "")
      sp_name <- trimws(input$custom_sub_name %||% "")
    } else if (nzchar(sub_key)) {
      sp <- parse_sub_process_key(sub_key)
      sp_id <- sp$id; sp_name <- sp$name
    }

    rk <- input$cascade_risk %||% ""
    risk_factor <- ""; risk_desc <- ""; risk_cat <- ""
    if (identical(rk, "__custom__")) {
      risk_factor <- trimws(input$custom_risk_factor %||% "")
      risk_desc <- trimws(input$custom_risk_desc %||% "")
      risk_cat <- input$custom_risk_category %||% ""
    } else if (nzchar(rk)) {
      risk_factor <- rk
      det <- cascade_risk_detail(filter_cascade_rows(cascade_rows(), sub_key = if (!identical(sub_key, "__custom__")) sub_key else NULL), rk)
      risk_desc <- det$risk_description
      risk_cat <- det$risk_category
    }

    obj_sel <- input$cascade_objective %||% ""
    objective <- if (identical(obj_sel, "__custom__")) {
      trimws(input$custom_objective %||% "")
    } else obj_sel

    act_sel <- input$cascade_activity %||% ""
    activity <- ""; approach <- ""; nature <- ""; frequency <- ""; owner <- ""
    if (identical(act_sel, "__custom__")) {
      activity <- trimws(input$custom_activity %||% "")
      approach <- normalize_single_activity_type(input$custom_approach)
      nature <- normalize_control_type_manual_auto(input$custom_nature)
      frequency <- input$custom_frequency %||% ""
      owner <- trimws(input$custom_owner %||% "")
    } else if (nzchar(act_sel)) {
      ak <- parse_activity_key(act_sel)
      activity <- ak$activity
      approach <- ak$approach
    }

    iuc_sel <- input$cascade_iuc %||% ""
    iuc <- if (identical(iuc_sel, "__custom__")) {
      trimws(input$custom_iuc %||% "")
    } else iuc_sel

    # Enrich from matched library row when not custom
    rows <- filter_cascade_rows(
      cascade_rows(),
      sub_key = if (!identical(sub_key, "__custom__") && nzchar(sub_key)) sub_key else NULL,
      risk_factor = if (!identical(rk, "__custom__") && nzchar(rk)) rk else NULL,
      objective = if (!identical(obj_sel, "__custom__") && nzchar(obj_sel)) obj_sel else NULL,
      activity_key_sel = if (!identical(act_sel, "__custom__") && nzchar(act_sel)) act_sel else NULL
    )
    matched <- if (length(rows)) rows[[1]] else NULL
    if (!is.null(matched)) {
      if (!nzchar(nature)) nature <- matched$nature
      if (!nzchar(frequency)) frequency <- matched$frequency
      if (!nzchar(owner)) owner <- matched$responsible_unit
      if (!nzchar(approach)) approach <- matched$approach
    }

    list(
      cycle = input$cycle %||% "",
      sub_process_id = sp_id,
      sub_process = sp_name,
      risk_factor = risk_factor,
      risk_name = risk_factor,
      risk_description = risk_desc,
      risk_category = risk_cat,
      control_objective = objective,
      control_activity = activity,
      approach = approach,
      nature = nature,
      frequency = frequency,
      responsible_unit = owner,
      iuc_or_system = iuc,
      related_system = iuc
    )
  }

  # 會計科目：僅報導面可填且必填；其它類別鎖定並清空
  output$significant_account_hint <- renderUI({
    cat <- input$risk_category %||% ""
    if (is_reporting_risk_category(cat)) {
      div(class = "alert alert-info py-1 mb-2 small",
          lab_req("報導面"), " — 會計科目為必填，請填財務報表科目。")
    } else if (nzchar(cat)) {
      div(class = "alert alert-secondary py-1 mb-2 small",
          "非報導面：會計科目已鎖定不可填（將自動清空）。")
    } else {
      helpText(class = "text-muted small", "請先選擇風險類別；僅報導面可填會計科目。")
    }
  })

  output$related_law_hint <- renderUI({
    cat <- input$risk_category %||% ""
    if (is_compliance_risk_category(cat)) {
      div(class = "alert alert-info py-1 mb-2 small",
          lab_req("遵循面"), " — 相關法令為必填（可多選台灣／美國預設或自訂）。")
    } else if (nzchar(cat)) {
      div(class = "alert alert-secondary py-1 mb-2 small",
          "非遵循面：相關法令已鎖定不可填（將自動清空）。")
    } else {
      helpText(class = "text-muted small", "請先選擇風險類別；僅遵循面可填相關法令。")
    }
  })

  observe({
    cat <- input$risk_category %||% ""
    session$sendCustomMessage(
      "toggleAccount",
      list(enabled = is_reporting_risk_category(cat))
    )
    session$sendCustomMessage(
      "toggleLaw",
      list(enabled = is_compliance_risk_category(cat))
    )
    if (nzchar(cat) && !is_reporting_risk_category(cat)) {
      if (nzchar(trimws(input$significant_account %||% ""))) {
        updateTextInput(session, "significant_account", value = "")
      }
    }
    if (nzchar(cat) && !is_compliance_risk_category(cat)) {
      if (length(input$related_law)) {
        updateSelectizeInput(session, "related_law", selected = character(0))
      }
    }
  })

  observe({
    rows <- cascade_rows()
    ch_sub <- cascade_sub_process_choices(rows)
    n_lib <- length(lib())
    n_rows <- length(rows)
    label0 <- if (n_rows) {
      sprintf("② 選擇子作業…（本循環 %d 筆／庫 %d）", n_rows, n_lib)
    } else {
      sprintf("② 尚無子作業候選（庫 %d 筆 — 請確認循環或載入鯨鏈首批）", n_lib)
    }
    ch <- c(stats::setNames("", label0), ch_sub, "＋自訂新增子作業" = "__custom__")
    cur <- input$cascade_sub %||% ""
    updateSelectInput(session, "cascade_sub", choices = ch,
                      selected = if (cur %in% unname(ch)) cur else "")
  })

  observe({
    rows <- cascade_rows()
    sub_key <- input$cascade_sub %||% ""
    if (nzchar(sub_key) && !identical(sub_key, "__custom__")) {
      rows <- filter_cascade_rows(rows, sub_key = sub_key)
    } else if (!nzchar(sub_key)) {
      rows <- list()
    }
    ch_risk <- cascade_risk_choices(rows)
    ch <- c("③ 選擇風險因素…" = "", ch_risk, "＋自訂新增風險" = "__custom__")
    cur <- input$cascade_risk %||% ""
    updateSelectInput(session, "cascade_risk", choices = ch,
                      selected = if (cur %in% unname(ch)) cur else "")
  })

  output$cascade_risk_detail <- renderUI({
    rk <- input$cascade_risk %||% ""
    if (!nzchar(rk) || identical(rk, "__custom__")) return(NULL)
    rows <- cascade_rows()
    sub_key <- input$cascade_sub %||% ""
    if (nzchar(sub_key) && !identical(sub_key, "__custom__")) {
      rows <- filter_cascade_rows(rows, sub_key = sub_key)
    }
    det <- cascade_risk_detail(rows, rk)
    div(
      class = "alert alert-info py-2 mb-2 small",
      tags$strong("風險屬性／描述："),
      if (length(det$attrs)) tags$ul(lapply(det$attrs, tags$li)) else tags$span("（無屬性細節）"),
      tags$div(tags$em(det$risk_description %||% ""))
    )
  })

  observe({
    rows <- cascade_rows()
    sub_key <- input$cascade_sub %||% ""
    rk <- input$cascade_risk %||% ""
    if (!nzchar(rk)) {
      updateSelectInput(session, "cascade_objective",
                        choices = c("④ 選擇控制目標…" = ""), selected = "")
      return()
    }
    if (nzchar(sub_key) && !identical(sub_key, "__custom__")) {
      rows <- filter_cascade_rows(rows, sub_key = sub_key)
    }
    if (!identical(rk, "__custom__")) {
      rows <- filter_cascade_rows(rows, risk_factor = rk)
    }
    ch_obj <- cascade_objective_choices(rows)
    ch <- c("④ 選擇控制目標…" = "", ch_obj, "＋自訂新增目標" = "__custom__")
    cur <- input$cascade_objective %||% ""
    updateSelectInput(session, "cascade_objective", choices = ch,
                      selected = if (cur %in% unname(ch)) cur else "")
  })

  observe({
    rows <- cascade_rows()
    sub_key <- input$cascade_sub %||% ""
    rk <- input$cascade_risk %||% ""
    obj <- input$cascade_objective %||% ""
    if (!nzchar(obj)) {
      updateSelectInput(session, "cascade_activity",
                        choices = c("⑤ 選擇控制活動…" = ""), selected = "")
      return()
    }
    if (nzchar(sub_key) && !identical(sub_key, "__custom__")) {
      rows <- filter_cascade_rows(rows, sub_key = sub_key)
    }
    if (nzchar(rk) && !identical(rk, "__custom__")) {
      rows <- filter_cascade_rows(rows, risk_factor = rk)
    }
    if (!identical(obj, "__custom__")) {
      rows <- filter_cascade_rows(rows, objective = obj)
    }
    ch_act <- cascade_activity_choices(rows)
    ch <- c("⑤ 選擇控制活動（標示單一預防/偵測）…" = "", ch_act,
            "＋自訂新增活動" = "__custom__")
    cur <- input$cascade_activity %||% ""
    updateSelectInput(session, "cascade_activity", choices = ch,
                      selected = if (cur %in% unname(ch)) cur else "")
  })

  observe({
    rows <- cascade_rows()
    act <- input$cascade_activity %||% ""
    # IUC choices: filter by prior selection when possible
    sub_key <- input$cascade_sub %||% ""
    rk <- input$cascade_risk %||% ""
    obj <- input$cascade_objective %||% ""
    if (nzchar(sub_key) && !identical(sub_key, "__custom__")) {
      rows <- filter_cascade_rows(rows, sub_key = sub_key)
    }
    if (nzchar(rk) && !identical(rk, "__custom__")) {
      rows <- filter_cascade_rows(rows, risk_factor = rk)
    }
    if (nzchar(obj) && !identical(obj, "__custom__")) {
      rows <- filter_cascade_rows(rows, objective = obj)
    }
    if (nzchar(act) && !identical(act, "__custom__")) {
      rows <- filter_cascade_rows(rows, activity_key_sel = act)
    }
    ch_iuc <- cascade_iuc_choices(rows, pbc_df = pbc_reg())
    ch <- c("⑥ 選擇 IUC／相關系統…" = "", ch_iuc, "＋自訂新增 IUC" = "__custom__")
    cur <- input$cascade_iuc %||% ""
    updateSelectInput(session, "cascade_iuc", choices = ch,
                      selected = if (cur %in% unname(ch)) cur else "")
  })

  output$cascade_step_status <- renderUI({
    sel <- resolve_cascade_selection()
    ready <- cascade_selection_ready(sel)
    steps <- c(
      sprintf("①循環：%s", if (nzchar(sel$cycle)) "✓" else "○"),
      sprintf("②子作業：%s", if (nzchar(sel$sub_process_id) || nzchar(sel$sub_process)) "✓" else "○"),
      sprintf("③風險：%s", if (nzchar(sel$risk_factor)) "✓" else "○"),
      sprintf("④目標：%s", if (nzchar(sel$control_objective)) "✓" else "○"),
      sprintf("⑤活動：%s", if (nzchar(sel$control_activity) && activity_type_ok(sel$approach)) "✓" else "○"),
      sprintf("⑥IUC：%s", if (nzchar(sel$iuc_or_system)) "✓" else "○")
    )
    cls <- if (isTRUE(ready$ready)) "alert alert-success py-1 mb-2 small" else "alert alert-secondary py-1 mb-2 small"
    div(class = cls, paste(steps, collapse = " ｜ "),
        if (!ready$ready) tags$span(class = "text-muted", " — 完成後才可書寫公司現況"))
  })

  output$design_required_checklist <- renderUI({
    d <- current_draft_from_inputs()
    req <- design_required_check(d)
    items <- lapply(names(req$required), function(f) {
      ok <- isTRUE(req$filled[[f]])
      tags$li(
        class = if (ok) "text-success" else "text-danger",
        if (ok) "✓ " else "○ ",
        req$required[[f]]
      )
    })
    if (identical(req$account_mode, "required")) {
      ok_a <- isTRUE(req$filled$significant_account)
      items <- c(items, list(tags$li(
        class = if (ok_a) "text-success" else "text-danger",
        if (ok_a) "✓ " else "○ ", "會計科目（報導面必填）"
      )))
    } else if (identical(req$account_mode, "locked")) {
      ok_a <- isTRUE(req$filled$significant_account)
      items <- c(items, list(tags$li(
        class = if (ok_a) "text-success" else "text-danger",
        if (ok_a) "✓ " else "○ ", "會計科目已鎖定（非報導面不可填）"
      )))
    }
    if (identical(req$law_mode, "required")) {
      ok_l <- isTRUE(req$filled$related_law)
      items <- c(items, list(tags$li(
        class = if (ok_l) "text-success" else "text-danger",
        if (ok_l) "✓ " else "○ ", "相關法令（遵循面必填）"
      )))
    } else if (identical(req$law_mode, "locked")) {
      ok_l <- isTRUE(req$filled$related_law)
      items <- c(items, list(tags$li(
        class = if (ok_l) "text-success" else "text-danger",
        if (ok_l) "✓ " else "○ ", "相關法令已鎖定（非遵循面不可填）"
      )))
    }
    cls <- if (isTRUE(req$ok)) "alert alert-success py-2 mb-2 small" else "alert alert-warning py-2 mb-2 small"
    n_cascade <- length(cascade_rows())
    acct_needed <- identical(req$account_mode, "required") || identical(req$account_mode, "locked")
    law_needed <- identical(req$law_mode, "required") || identical(req$law_mode, "locked")
    n_all <- length(req$required) + as.integer(acct_needed) + as.integer(law_needed)
    n_ok <- sum(unlist(req$filled[names(req$required)])) +
      as.integer(acct_needed && isTRUE(req$filled$significant_account)) +
      as.integer(law_needed && isTRUE(req$filled$related_law))
    div(
      class = cls,
      tags$strong(sprintf("設計必填 %d／%d", n_ok, n_all)),
      tags$span(class = "text-muted ms-2", sprintf("｜引導候選 %d 筆", n_cascade)),
      tags$ul(class = "mb-0 ps-3", style = "columns: 2; -webkit-columns: 2;", items),
      if (!req$ok) tags$div(class = "mt-1", "未齊：", paste(req$missing, collapse = "、")),
      if (!n_cascade) tags$div(
        class = "mt-1 text-danger",
        "本循環尚無引導選項 — 請按「載入鯨鏈資訊循環 RCM（首批）」或確認循環為資訊循環。"
      )
    )
  })

  output$auto_control_id_box <- renderUI({
    sel <- resolve_cascade_selection()
    spid <- sel$sub_process_id
    if (!nzchar(spid)) return(NULL)
    ids <- collect_existing_control_ids(lists = list(lib(), drafts(), controls()))
    nid <- next_rcm_control_id(spid, ids)
    div(class = "small text-muted mb-2",
        "自動控制編號預覽：", tags$code(nid),
        "（套用引導或加入佇列時寫入）")
  })

  output$six_rules_box <- renderUI({
    d <- current_draft_from_inputs()
    chk <- six_status_rules_check(d)
    if (isTRUE(chk$ok)) {
      div(class = "alert alert-success py-1 mb-2 small", "六大控制項目就緒，可書寫公司現況。")
    } else {
      div(class = "alert alert-warning py-1 mb-2 small",
          "六大控制項目未齊：", paste(chk$missing, collapse = "、"))
    }
  })

  output$company_status_lock_msg <- renderUI({
    sel <- resolve_cascade_selection()
    d <- current_draft_from_inputs()
    cas <- cascade_selection_ready(sel)
    six <- six_status_rules_check(d)
    unlocked <- isTRUE(cas$ready) && isTRUE(six$ok)
    updateCheckboxInput(session, "status_unlocked", value = unlocked)
    if (unlocked) {
      div(class = "alert alert-success py-1 mb-2 small", "公司現況欄已解鎖，請依六大控制項目書寫。")
    } else {
      div(
        class = "alert alert-secondary py-2 mb-2 small",
        tags$strong("公司現況欄位尚未顯示"),
        tags$br(),
        "請先完成引導選取（含 IUC）並補齊六大控制項目。",
        if (!cas$ready) tags$div(sprintf("引導缺：%s", paste(cas$missing, collapse = ", "))),
        if (!six$ok) tags$div(sprintf("六大缺：%s", paste(six$missing, collapse = "、")))
      )
    }
  })

  observeEvent(input$fill_status_scaffold, {
    sel <- resolve_cascade_selection()
    d <- current_draft_from_inputs()
    cas <- cascade_selection_ready(sel)
    six <- six_status_rules_check(d)
    if (!isTRUE(cas$ready) || !isTRUE(six$ok)) {
      return(showNotification("請先完成引導與六大控制項目", type = "warning"))
    }
    updateTextAreaInput(session, "company_status", value = assemble_status_scaffold(d))
  })

  # Block empty status add when cascade not ready — soft gate on typing not needed;
  # hard gate on add_draft already requires six rules.

  observeEvent(input$reset_cascade, {
    updateSelectInput(session, "cascade_sub", selected = "")
    updateSelectInput(session, "cascade_risk", selected = "")
    updateSelectInput(session, "cascade_objective", selected = "")
    updateSelectInput(session, "cascade_activity", selected = "")
    updateSelectInput(session, "cascade_iuc", selected = "")
  })

  observeEvent(input$apply_cascade, {
    sel <- resolve_cascade_selection()
    ready <- cascade_selection_ready(sel)
    if (!isTRUE(ready$ready)) {
      return(showNotification(
        paste0("引導尚未完成：", paste(ready$missing, collapse = ", ")),
        type = "warning", duration = 6
      ))
    }
    if (!activity_type_ok(sel$approach)) {
      return(showNotification("控制活動必須只對應一種屬性（預防性或偵測性）", type = "error"))
    }

    # Match full library control when possible
    matched <- match_cascade_control(cascade_rows(), sel)
    if (!is.null(matched) && is.list(matched)) {
      # matched may be flat row or raw control
      ctrl <- if (!is.null(matched$raw)) matched$raw else matched
      if (!is.null(ctrl$control_objective) || !is.null(ctrl$risk_name) || !is.null(ctrl$cycle)) {
        fill_inputs_from_ctrl(session, ctrl)
      }
    }

    updateTextInput(session, "sub_process_id", value = sel$sub_process_id)
    updateTextInput(session, "sub_process", value = sel$sub_process)
    updateTextInput(session, "risk_factor", value = sel$risk_factor)
    updateTextInput(session, "risk_name", value = sel$risk_name)
    if (nzchar(sel$risk_description)) {
      updateTextAreaInput(session, "risk_description", value = sel$risk_description)
    }
    if (nzchar(sel$risk_category)) {
      updateSelectInput(session, "risk_category", selected = sel$risk_category)
    }
    updateTextAreaInput(session, "control_objective", value = sel$control_objective)
    updateTextAreaInput(session, "control_activity", value = sel$control_activity)
    if (nzchar(sel$approach)) updateSelectInput(session, "approach", selected = sel$approach)
    if (nzchar(sel$nature)) updateSelectInput(session, "nature", selected = sel$nature)
    if (nzchar(sel$frequency)) {
      updateSelectInput(session, "frequency",
                        choices = unique(c(FREQUENCY_CHOICES, sel$frequency)),
                        selected = sel$frequency)
    }
    if (nzchar(sel$responsible_unit)) {
      updateTextInput(session, "responsible_unit", value = sel$responsible_unit)
    }
    updateTextAreaInput(session, "iuc_or_system", value = sel$iuc_or_system)

    # Auto RCM control id
    ids <- collect_existing_control_ids(lists = list(lib(), drafts(), controls()))
    nid <- next_rcm_control_id(sel$sub_process_id, ids)
    updateTextInput(session, "control_id", value = nid)

    # Custom IUC → optional save to PBC / library
    if (identical(input$cascade_iuc, "__custom__") && isTRUE(input$custom_iuc_save) &&
        nzchar(sel$iuc_or_system)) {
      tryCatch({
        reg <- upsert_pbc(pbc_reg(), list(
          client_pbc_name = sel$iuc_or_system,
          reviewed_name = sel$iuc_or_system,
          iuc_or_system = sel$iuc_or_system,
          cycle = sel$cycle,
          notes = "引導自訂 IUC"
        ))
        pbc_reg(reg)
        persist_pbc(reg)
        refresh_pbc_choices()
      }, error = function(e) NULL)
    }

    showNotification(
      sprintf("已套用引導；控制編號 %s。六大就緒後可書寫公司現況。", nid),
      type = "message"
    )
  })

  observeEvent(input$save_custom_cascade, {
    sel <- resolve_cascade_selection()
    if (!nzchar(sel$control_objective) || !nzchar(sel$control_activity)) {
      return(showNotification("請至少具備控制目標與控制活動再存庫", type = "warning"))
    }
    if (!activity_type_ok(sel$approach)) {
      return(showNotification("自訂活動須指定單一預防/偵測屬性", type = "error"))
    }
    item <- custom_cascade_to_library_item(sel, tags = c("自訂新增", "引導"))
    lib(persist_lib(upsert_library_item(lib(), item)))
    refresh_lib_choices()
    showNotification(paste("已存入範本庫", item$library_id), type = "message")
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
    gaps <- tryCatch(detect_design_gaps(d), error = function(e) {
      data.frame(
        control_id = "", category = "系統", severity = "高",
        gap_item = paste0("檢核錯誤：", conditionMessage(e)),
        suggested_action = "請回報此錯誤",
        stringsAsFactors = FALSE
      )
    })
    chk <- rcm_objective_activity_check(d$control_objective, d$control_activity)
    ready <- is_rcm_row_ready(d)
    six <- six_status_rules_check(d)
    req <- design_required_check(d)
    if (isTRUE(ready$ready) && isTRUE(chk$ok) && isTRUE(req$ok)) {
      div(class = "alert alert-success py-1 mb-2",
          "設計必填齊全＝可寫入 RCM 一列｜", format_oa_check_html(chk))
    } else {
      high <- gaps[gaps$severity == "高", , drop = FALSE]
      summary <- if (!isTRUE(req$ok)) paste0("必填未齊：", paste(req$missing, collapse = "、"))
      else if (!isTRUE(chk$ok)) (chk$msg %||% paste(chk$issues, collapse = "；"))
      else if (!isTRUE(six$ok)) paste0("六大未齊：", paste(six$missing, collapse = "、"))
      else if (nrow(high)) paste(sprintf("[%s] %s", high$category, high$gap_item), collapse = "；")
      else paste(gaps$gap_item, collapse = "；")
      div(class = "alert alert-warning py-1 mb-2", paste0("尚不可定稿 RCM 一列：", summary))
    }
  })

  output$rcm_parity_box <- renderUI({
    parity <- assert_design_rcm_parity(controls())
    cls <- if (isTRUE(parity$ok)) "alert alert-secondary py-1 mb-2 small" else "alert alert-danger py-1 mb-2 small"
    div(class = cls,
        sprintf("不變條件：設計控制點 %d ＝ RCM 列 %d", parity$n_controls, parity$n_rcm_rows),
        if (!parity$ok) "（不一致，請重新定稿）")
  })
  output$rcm_count_box <- renderUI({
    parity <- assert_design_rcm_parity(controls())
    tags$p(class = "small mb-2",
           sprintf("目前已定稿 %d 個控制點＝%d 列 RCM", parity$n_controls, parity$n_rcm_rows))
  })

  # Primary path: 設計完成 → 直接寫入一筆控制點／RCM 列（1:1）
  observeEvent(input$finalize_rcm_row, {
    d <- current_draft_from_inputs()
    ids <- collect_existing_control_ids(lists = list(lib(), drafts(), controls()))
    fin <- finalize_control_as_rcm_row(d, existing_ids = ids, seq_hint = length(controls()) + 1L)
    if (!isTRUE(fin$ok)) {
      return(showNotification(
        paste0("尚未完成設計，不能寫入 RCM 列：", fin$msg),
        type = "error", duration = 10
      ))
    }
    pt <- fin$control
    # upsert by control_id into controls()
    cs <- controls()
    idx <- which(vapply(cs, function(x) identical(x$control_id, pt$control_id), logical(1)))
    if (length(idx)) cs[[idx[[1]]]] <- pt else cs[[length(cs) + 1]] <- pt
    controls(cs)
    # also keep a draft snapshot for edit history
    d$control_id <- pt$control_id
    d$draft_id <- next_id()
    drafts(c(drafts(), list(d)))
    next_id(next_id() + 1L)
    updateTextInput(session, "control_id",
                    value = next_rcm_control_id(
                      pt$sub_process_id,
                      collect_existing_control_ids(lists = list(lib(), drafts(), controls()))
                    ))
    if (isTRUE(input$auto_collect_lib)) {
      res <- collect_many_to_lib(list(pt), source = "finalize_rcm", quality_gate = TRUE)
      showNotification(
        sprintf("%s｜已累積入庫 +%d／覆寫 %d", fin$msg, res$added, res$updated),
        type = "message", duration = 8
      )
    } else {
      showNotification(fin$msg, type = "message", duration = 8)
    }
    if (isTRUE(input$autosave_draft)) do_save_draft(quiet = TRUE)
  })

  observeEvent(input$add_draft, {
    d <- current_draft_from_inputs()
    chk <- rcm_objective_activity_check(d$control_objective, d$control_activity)
    if (!isTRUE(chk$ok)) {
      return(showNotification(
        paste0("目標／活動未分開，無法暫存：", chk$msg),
        type = "error", duration = 8
      ))
    }
    d$draft_id <- next_id()
    if (!nzchar(trimws(d$control_id))) {
      ids <- collect_existing_control_ids(lists = list(lib(), drafts(), controls()))
      d$control_id <- next_rcm_control_id(d$sub_process_id %||% derive_sub_process_id(d, 1L), ids)
    }
    drafts(c(drafts(), list(d)))
    next_id(next_id() + 1L)
    showNotification("已暫存佇列（尚未定稿為 RCM 列；就緒後請按「完成設計＝寫入 RCM 一列」）",
                     type = "message")
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
    if (!length(ds)) return(showNotification("無暫存佇列可批次定稿", type = "warning"))
    # Also split by IUC within same risk
    risk_keys <- vapply(ds, function(d) paste(d$cycle, d$risk_name, sep = "||"), "")
    groups <- split(seq_along(ds), risk_keys)
    result <- controls()
    used_ids <- collect_existing_control_ids(lists = list(lib(), drafts(), controls()))
    n_ok <- 0L
    n_fail <- 0L
    for (gk in names(groups)) {
      for (pt0 in split_controls_by_iuc(ds[groups[[gk]]])) {
        fin <- finalize_control_as_rcm_row(pt0, existing_ids = used_ids,
                                          seq_hint = length(result) + 1L)
        if (!isTRUE(fin$ok)) {
          n_fail <- n_fail + 1L
          next
        }
        pt <- fin$control
        used_ids <- c(used_ids, pt$control_id)
        idx <- which(vapply(result, function(x) identical(x$control_id, pt$control_id), logical(1)))
        if (length(idx)) result[[idx[[1]]]] <- pt else result[[length(result) + 1]] <- pt
        n_ok <- n_ok + 1L
      }
    }
    controls(result)
    parity <- assert_design_rcm_parity(result)
    showNotification(
      sprintf("批次定稿：成功 %d 列 RCM／未就緒 %d｜不變條件 控制點%d＝RCM%d",
              n_ok, n_fail, parity$n_controls, parity$n_rcm_rows),
      type = if (n_ok) "message" else "warning", duration = 10
    )
    if (isTRUE(input$auto_collect_lib) && n_ok > 0) {
      ready <- Filter(function(x) isTRUE(x$rcm_ready$ready), result)
      res <- collect_many_to_lib(ready, source = "auto_rcm", quality_gate = TRUE)
      showNotification(
        sprintf("自動累積入庫：+%d／覆寫 %d／略過 %d", res$added, res$updated, res$skipped),
        type = "message"
      )
    }
    if (isTRUE(input$autosave_draft)) do_save_draft(quiet = TRUE)
  })

  controls_df <- reactive({
    cs <- controls()
    if (!length(cs)) {
      return(data.frame(控制編號 = character(), IUC = character(), RCM列 = character(),
                        摘要 = character(), stringsAsFactors = FALSE))
    }
    rcm <- controls_to_rcm(cs)
    data.frame(
      控制編號 = vapply(cs, function(x) x$control_id, ""),
      IUC = vapply(cs, function(x) x$iuc_or_system, ""),
      RCM列 = vapply(seq_along(cs), function(i) {
        if (isTRUE(cs[[i]]$rcm_ready$ready)) "已定稿＝1列" else "待補"
      }, ""),
      目標 = if (nrow(rcm)) as.character(rcm[["控制目標"]]) else character(length(cs)),
      活動 = if (nrow(rcm)) as.character(rcm[["控制活動"]]) else character(length(cs)),
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
  output$interview_status <- renderUI({
    cs <- selected_worksheet_controls()
    n <- length(Filter(function(c) isTRUE(c$rcm_ready$ready) || isTRUE(is_rcm_row_ready(c)$ready), cs))
    iv <- controls_to_interview(cs, input$interview_elements, finalized_only = TRUE)
    tags$small(class = "text-muted",
               sprintf("已定稿 RCM %d 列 → 訪談題 %d 則", n, nrow(iv)))
  })
  output$csa_status <- renderUI({
    cs <- selected_worksheet_controls()
    csa <- controls_to_csa(cs, input$csa_elements, finalized_only = TRUE)
    tags$small(class = "text-muted",
               sprintf("CSA 測試步驟 %d 列（僅已定稿 RCM）", nrow(csa)))
  })
  output$interview_table <- renderDT({
    datatable(controls_to_interview(selected_worksheet_controls(), input$interview_elements,
                                    finalized_only = TRUE),
              rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10, dom = "tip"))
  })
  output$csa_table <- renderDT({
    datatable(controls_to_csa(selected_worksheet_controls(), input$csa_elements,
                              finalized_only = TRUE),
              rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10, dom = "tip"))
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
    filename = function() sprintf("interview-%s.csv", format(Sys.time(), "%Y%m%d")),
    content = function(file) {
      write.csv(controls_to_interview(selected_worksheet_controls(), input$interview_elements,
                                      finalized_only = TRUE),
                file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  output$download_csa <- downloadHandler(
    filename = function() sprintf("csa-teststeps-%s.csv", format(Sys.time(), "%Y%m%d")),
    content = function(file) {
      write.csv(controls_to_csa(selected_worksheet_controls(), input$csa_elements,
                                finalized_only = TRUE),
                file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui, server)
