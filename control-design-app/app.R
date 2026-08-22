# 尬電SOX — compact UI
# Run: shiny::runApp("control-design-app")

library(shiny)
library(bslib)
library(DT)

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
source(file.path(root, "R", "parameter_store.R"), local = TRUE)

# UI label with required asterisk
lab_req <- function(txt) {
  tagList(txt, tags$span("*", class = "text-danger ms-1", title = "設計必填"))
}
lab_opt <- function(txt) {
  tagList(txt, tags$span(class = "text-muted small ms-1", "選填"))
}

fill_inputs_from_ctrl <- function(session, ctrl, lib_items = NULL) {
  if (is.null(ctrl)) return()
  apply_ctrl_to_cascade(session, ctrl)
  apply_supplement_from_ctrl(session, ctrl)
}

ui <- page_navbar(
  title = "尬電SOX",
  window_title = "尬電SOX",
  theme = bs_theme(
    version = 5,
    primary = BRAND_BLUE,
    success = BRAND_GREEN,
    secondary = "#4A4A4A",
    dark = BRAND_BLACK,
    light = BRAND_GRAY,
    "body-bg" = BRAND_WHITE,
    "body-color" = BRAND_BLACK,
    "navbar-bg" = BRAND_BLACK,
    "navbar-dark-color" = "rgba(255,255,255,0.85)",
    "navbar-dark-hover-color" = BRAND_GREEN,
    "navbar-dark-active-color" = BRAND_GREEN,
    "navbar-dark-brand-color" = BRAND_WHITE,
    "navbar-dark-brand-hover-color" = BRAND_GREEN,
    "link-color" = BRAND_BLUE,
    "link-hover-color" = BRAND_GREEN,
    "border-color" = "#D9D9D9",
    "input-border-color" = "#C8C8C8",
    "focus-ring-color" = "rgba(134, 188, 37, 0.35)",
    # Do not use font_google() — it stalls shinyapps cold start (shiny-busy / disconnect)
    base_font = '"Noto Sans TC", "Microsoft JhengHei", "PingFang TC", "Segoe UI", sans-serif',
    "font-size-base" = "0.9rem"
  ),
  header = tags$script(HTML(sprintf("
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
    Shiny.addCustomMessageHandler('toggleFrequency', function(msg) {
      var el = document.getElementById('frequency');
      if (!el) return;
      el.disabled = !msg.enabled;
      el.classList.toggle('bg-light', !msg.enabled);
    });
    document.addEventListener('DOMContentLoaded', function() {
      var style = document.createElement('style');
      style.textContent = [
        ':root { --brand-blue: %s; --brand-green: %s; --brand-black: %s; --brand-white: %s; }',
        '.navbar { background-color: var(--brand-black) !important; border-bottom: 3px solid var(--brand-green); }',
        '.navbar .navbar-brand { color: var(--brand-white) !important; font-weight: 700; letter-spacing: 0.02em; }',
        '.navbar .navbar-brand::after { content: \"\"; display: inline-block; width: 0.45em; height: 0.45em; margin-left: 0.15em; margin-bottom: 0.05em; border-radius: 50%%; background: var(--brand-green); vertical-align: middle; }',
        '.navbar .nav-link { color: rgba(255,255,255,0.82) !important; }',
        '.navbar .nav-link:hover, .navbar .nav-link.active { color: var(--brand-green) !important; }',
        '.bslib-sidebar-layout > .sidebar { background: var(--brand-white); border-right: 1px solid #E5E5E5; }',
        '.card { border-color: #E5E5E5; }',
        '.card-header { background: var(--brand-white); border-bottom: 2px solid var(--brand-green); color: var(--brand-blue); font-weight: 600; }',
        '.btn-primary { background-color: var(--brand-blue); border-color: var(--brand-blue); }',
        '.btn-primary:hover, .btn-primary:focus { background-color: #00205B; border-color: #00205B; }',
        '.btn-success { background-color: var(--brand-green); border-color: var(--brand-green); color: var(--brand-black); font-weight: 600; }',
        '.btn-success:hover, .btn-success:focus { background-color: #6FA01E; border-color: #6FA01E; color: var(--brand-black); }',
        '.btn-outline-success { color: var(--brand-green); border-color: var(--brand-green); }',
        '.btn-outline-success:hover { background-color: var(--brand-green); border-color: var(--brand-green); color: var(--brand-black); }',
        '.btn-outline-primary { color: var(--brand-blue); border-color: var(--brand-blue); }',
        '.btn-outline-primary:hover { background-color: var(--brand-blue); color: var(--brand-white); }',
        '.accordion-button:not(.collapsed) { background-color: rgba(134,188,37,0.12); color: var(--brand-blue); box-shadow: inset 0 -1px 0 var(--brand-green); }',
        '.accordion-button:focus { box-shadow: 0 0 0 0.2rem rgba(134,188,37,0.25); }',
        '.form-control:focus, .form-select:focus { border-color: var(--brand-green); box-shadow: 0 0 0 0.2rem rgba(134,188,37,0.2); }',
        '.alert-success { background-color: rgba(134,188,37,0.15); border-color: var(--brand-green); color: #1A2E00; }',
        '.alert-info { background-color: rgba(0,46,130,0.08); border-color: var(--brand-blue); color: var(--brand-blue); }',
        '.text-danger { color: #C41E3A !important; }',
        'a { color: var(--brand-blue); }',
        'a:hover { color: var(--brand-green); }',
        '.lib-options-section .shiny-input-container { margin-bottom: 0.75rem; }',
        '.lib-options-section .form-check { margin-bottom: 0.75rem; }',
        '.lib-options-actions { clear: both; width: 100%%; margin-top: 1rem; padding-top: 1rem; border-top: 1px solid #dee2e6; }',
        '.sidebar-lib-block .shiny-input-container { margin-bottom: 0.5rem; }',
        '.sidebar-lib-block .form-check { margin-top: 0.5rem; margin-bottom: 0.25rem; }',
        '.home-hero { background: linear-gradient(135deg, #000000 0%%, #002E82 70%%); color: #fff; padding: 1.5rem 1.75rem; border-radius: 0.5rem; margin-bottom: 1rem; border-bottom: 4px solid var(--brand-green); }',
        '.home-hero h2 { color: #fff; font-weight: 700; margin: 0 0 0.5rem 0; }',
        '.home-hero p { color: rgba(255,255,255,0.88); margin: 0; }',
        '.home-section h5 { color: var(--brand-blue); font-weight: 700; border-left: 4px solid var(--brand-green); padding-left: 0.6rem; margin-bottom: 0.75rem; }',
        '.home-steps { list-style: none; padding-left: 0; counter-reset: step; }',
        '.home-steps li { counter-increment: step; position: relative; padding: 0.55rem 0.75rem 0.55rem 2.6rem; margin-bottom: 0.4rem; background: #F7F9FC; border-radius: 0.35rem; border: 1px solid #E5E5E5; }',
        '.home-steps li::before { content: counter(step); position: absolute; left: 0.55rem; top: 0.5rem; width: 1.5rem; height: 1.5rem; border-radius: 50%%; background: var(--brand-green); color: #000; font-weight: 700; font-size: 0.8rem; display: flex; align-items: center; justify-content: center; }',
        '.home-tabs-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(220px, 1fr)); gap: 0.75rem; }',
        '.home-tab-card { border: 1px solid #E5E5E5; border-top: 3px solid var(--brand-green); border-radius: 0.4rem; padding: 0.85rem 1rem; background: #fff; }',
        '.home-tab-card strong { color: var(--brand-blue); display: block; margin-bottom: 0.35rem; }'
      ].join('\\n');
      document.head.appendChild(style);
    });
  ", BRAND_BLUE, BRAND_GREEN, BRAND_BLACK, BRAND_WHITE))),
  fillable = TRUE,
  sidebar = sidebar(
    width = 280,
    open = "desktop",
    div(
      class = "d-flex flex-column h-100",
      div(
        textInput("company", NULL, placeholder = "公司名稱"),
        selectInput("cycle", "循環",
                    choices = c("請選擇循環…" = "", CYCLES_NINE_CHOICES),
                    selected = "")
      ),
      div(
        class = "mt-auto pt-2 sidebar-lib-block",
        tags$hr(class = "my-2"),
        textInput("lib_query", NULL, placeholder = "搜尋範本庫…"),
        selectInput("lib_pick", NULL, choices = c("① 優先：從範本庫套用…" = "")),
        div(
          class = "d-flex gap-1 flex-wrap mb-2",
          actionButton("apply_lib", "套用", class = "btn-sm btn-primary"),
          actionButton("save_to_lib", "存入庫", class = "btn-sm btn-outline-success")
        ),
        checkboxInput("auto_collect_lib", "設計完成自動收集入庫", TRUE),
        uiOutput("lib_count_badge")
      )
    )
  ),
  nav_panel(
    "首頁",
    div(
      class = "home-hero",
      tags$h2("尬電SOX"),
      p("輔助快速且精準設計標準內部控制點，產出 RCM、訪談題綱與自我評估（CSA）測試步驟。",
        " RCM 標題列對齊鯨鏈資訊循環格式；設計採強制引導流程。")
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(
        class = "home-section",
        card_header("整體設計流程"),
        tags$ol(
          class = "home-steps mb-0",
          tags$li(tags$strong("左側選循環"), "（與公司名稱）→ 範本庫可搜尋／套用既有控制點。"),
          tags$li(tags$strong("控制點設計"), "：依序選取 ",
                  strong("循環 → 子作業 → 風險 → 控制目標 → 控制活動（單一預防／偵測）→ IUC"),
                  "（", tags$span(class = "text-danger", "須依序選取"),
                  "：未選上一層時，下一層沒有候選）。"),
          tags$li("補齊 ", strong("基本資料 → 風險辨識 → 控制設計"),
                  "；", tags$span(class = "text-danger", "*"), " 為設計必填。"),
          tags$li(strong("完成設計＝寫入 RCM 一列"),
                  "（1 控制點 ↔ 1 RCM 列；控制編號自動順編如 EC-101-01）。"),
          tags$li(tags$strong("訪談問項設計"), "：對齊已定稿 RCM，勾選元素後預覽／下載題綱。"),
          tags$li(tags$strong("自我評估測試步驟設計"),
                  "：填寫 Form 4120SR Inputs／Steps／Outputs，並產製 CSA 測試程序／PBC／預期結果。")
        )
      ),
      card(
        class = "home-section",
        card_header("設計必填與防呆"),
        tags$ul(
          class = "mb-2 ps-3",
          tags$li(strong("六大控制項目"), "：控制類型、控制活動類型、頻率、負責單位、IUC、控制活動。"),
          tags$li(strong("控制目標 ≠ 控制活動"), "（Why／How 分欄；可拆分建議或對調）。"),
          tags$li(strong("控制類型"), "僅人工／自動；", strong("自動"), "時頻率強制「持續」。"),
          tags$li(strong("控制活動類型"), "僅單一預防性或偵測性。"),
          tags$li(strong("會計科目"), "僅報導面可填且必填；", strong("相關法令"), "僅遵循面可填且必填。"),
          tags$li(strong("不變條件"), "：已定稿控制點數＝RCM 列數，控制編號一一對齊。")
        ),
        p(class = "small text-muted mb-0",
          "本 APP 僅產出設計欄位；控制現況描述／分析評估等後續欄位留空。")
      )
    ),
    card(
      class = "home-section",
      card_header("各頁籤用途"),
      div(
        class = "home-tabs-grid",
        div(class = "home-tab-card",
            strong("控制點設計"),
            "引導選取＋基本資料／風險辨識／控制設計；定稿寫入 RCM。"),
        div(class = "home-tab-card",
            strong("訪談問項設計"),
            "對齊已定稿 RCM 產出訪談題綱；請先完成控制點定稿。"),
        div(class = "home-tab-card",
            strong("自我評估測試步驟設計"),
            "CSA 測試步驟與 Form 4120SR Type／Inputs／Steps／Outputs／調查門檻。"),
        div(class = "home-tab-card",
            strong("範本庫"),
            "累積制通用範本：設計就緒入庫、CSV／JSON／RCM xlsx 匯入、自訂引導項；同 ID 覆寫累積。"),
        div(class = "home-tab-card",
            strong("參數庫"),
            "查詢系統預設、範本庫、已定稿 RCM、PBC 全部參數選項，可套用回表單。"),
        div(class = "home-tab-card",
            strong("PBC"),
            "客戶原名 → 檢視後標準命名；證據類型標示螢幕截圖／EMAIL／系統表單／政策制度。"),
        div(class = "home-tab-card",
            strong("RCM"),
            "檢視／下載已定稿 RCM 列與缺漏表（設計欄位群組對齊鯨鏈標題列）。")
      )
    ),
    card(
      class = "home-section",
      card_header("建議操作順序"),
      p(class = "mb-1",
        "① 首頁了解流程 → ② 左側選循環（可套用範本）→ ③ ",
        strong("控制點設計"), " 引導＋定稿 → ④ ",
        strong("訪談問項"), "／", strong("自我評估測試步驟"),
        " → ⑤ 需要時維護 ", strong("範本庫"), "／", strong("PBC"), "／", strong("參數庫"), "。"),
      p(class = "small text-muted mb-0",
        "測試步驟欄位填於「自我評估測試步驟設計」，定稿時會一併寫入控制點草稿。")
    )
  ),
  nav_panel(
    "訪談問項設計",
    layout_columns(
      col_widths = c(3, 9),
      card(
        card_header("訪談問項設計"),
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
    "控制點設計",
    layout_columns(
      col_widths = c(7, 5),
      card(
        full_screen = TRUE,
        card_header("引導設計（依序選取）"),
        uiOutput("cascade_step_status"),
        uiOutput("design_required_checklist"),
        uiOutput("cascade_candidate_banner"),
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
          textInput("custom_risk_factor", NULL, placeholder = "自訂風險 tag（簡短）"),
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
          actionButton("save_custom_cascade", "將自訂項存入範本庫", class = "btn-outline-success btn-sm"),
          actionButton("reset_cascade", "重設引導", class = "btn-outline-secondary btn-sm")
        ),
        uiOutput("auto_control_id_box"),
        tags$hr(),
        accordion(
          id = "rcm_design_groups",
          open = c("基本資料", "風險辨識", "控制設計"),
          accordion_panel(
            "基本資料",
            textInput("control_id", "控制編號", value = "",
                      placeholder = "自動順編（可覆寫）"),
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
            "風險辨識",
            textInput("significant_account", "會計科目", value = "",
                      placeholder = "僅報導面可填且必填"),
            uiOutput("significant_account_hint"),
            tags$p(class = "small text-muted mb-1", "三大風險屬性細節"),
            layout_columns(
              col_widths = c(4, 8),
              textInput("attr_label_fr", NULL, value = "財務報導"),
              textAreaInput("risk_attr_financial", NULL, rows = 1, placeholder = "財務報導屬性細節")
            ),
            layout_columns(
              col_widths = c(4, 8),
              textInput("attr_label_op", NULL, value = "營運"),
              textAreaInput("risk_attr_operations", NULL, rows = 1, placeholder = "營運屬性細節")
            ),
            layout_columns(
              col_widths = c(4, 8),
              textInput("attr_label_cp", NULL, value = "法令遵循"),
              textAreaInput("risk_attr_compliance", NULL, rows = 1, placeholder = "法令遵循屬性細節")
            ),
            selectizeInput(
              "assertions", "聲明（Assertions）", choices = ASSERTION_CHOICES, multiple = TRUE,
              selected = ASSERTION_CHOICES[1:2],
              options = list(create = TRUE, placeholder = "聲明（輔助）")
            ),
            selectInput("romm_classification", "RoMM 分類", choices = ROMM_CLASS_CHOICES)
          ),
          accordion_panel(
            "控制設計",
            uiOutput("oa_live_check"),
            uiOutput("type_live_check"),
            div(
              class = "d-flex gap-1 flex-wrap mb-2",
              actionButton("oa_split_suggest", "拆分建議", class = "btn-sm btn-outline-secondary"),
              actionButton("oa_swap", "對調目標/活動", class = "btn-sm btn-outline-secondary")
            ),
            selectizeInput(
              "pbc_apply", "套用 IUC／PBC 命名", choices = NULL, multiple = TRUE,
              options = list(placeholder = "原名→新名")
            )
          )
        ),
        div(
          class = "d-flex gap-1 flex-wrap mt-2",
          actionButton("finalize_rcm_row", "完成設計＝寫入 RCM 一列", class = "btn-success btn-sm"),
          actionButton("collect_ready_to_lib", "RCM列→累積範本庫", class = "btn-outline-success btn-sm")
        )
      ),
      card(
        uiOutput("live_validation"),
        uiOutput("rcm_parity_box"),
        verbatimTextOutput("live_preview"),
        DTOutput("control_table"),
        verbatimTextOutput("control_paragraph")
      )
    )
  ),
  nav_panel(
    "自我評估測試步驟設計",
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("自我評估測試步驟設計"),
        selectizeInput(
          "worksheet_controls_sa", NULL, choices = NULL, multiple = TRUE,
          options = list(placeholder = "RCM 控制點（空＝全部已定稿）")
        ),
        checkboxGroupInput("csa_elements", "測試步驟元素",
                           choices = DESIGN_ELEMENTS, selected = DEFAULT_CSA_ELEMENTS),
        actionButton("ws_select_core_csa", "自我評估核心元素", class = "btn-sm btn-primary"),
        uiOutput("csa_status"),
        tags$hr(),
        tags$strong(class = "small", "Form 4120SR 測試步驟設定"),
        selectizeInput("type", "Type", choices = TYPE_CHOICES,
                       options = list(create = TRUE, placeholder = "Form 4120SR Type")),
        textAreaInput("inputs", "Inputs", rows = 2, placeholder = "測試投入／證據來源"),
        textAreaInput("review_steps", "Steps", rows = 4, placeholder = "測試步驟（每行一步）"),
        textAreaInput("outputs", "Outputs", rows = 2, placeholder = "預期產出／文件"),
        textAreaInput("investigation_threshold", "調查門檻", rows = 1, placeholder = "調查門檻"),
        checkboxInput("pbc_also_inputs", "於「控制點設計」套用 PBC 時寫入 Inputs 對照", TRUE)
      ),
      card(
        DTOutput("csa_table"),
        downloadButton("download_csa", "下載自我評估測試步驟 CSV", class = "btn-sm")
      )
    )
  ),
  nav_panel(
    "範本庫",
    # 上：即時顯示；下：控制面板
    card(
      card_header("即時顯示"),
      DTOutput("lib_table"),
      verbatimTextOutput("lib_preview")
    ),
    card(
      card_header("累積制通用範本庫 — 選項"),
      div(
        class = "lib-options-section",
        uiOutput("lib_stats_box"),
        tags$hr(class = "my-2"),
        tags$h6(class = "small fw-bold mb-2", "匯入"),
        fileInput("upload_lib", NULL, buttonLabel = "匯入 CSV／JSON／RCM xlsx",
                  accept = c(".csv", ".json", ".xlsx", ".xls")),
        checkboxInput("lib_overwrite", "同 ID 則覆蓋（累積更新）", TRUE),
        actionButton("import_jinglian_seed", "載入內建 RCM 範本庫",
                     class = "btn-sm btn-outline-primary mb-3"),
        tags$hr(class = "my-2"),
        tags$h6(class = "small fw-bold mb-2", "收集入庫"),
        textInput("lib_title_override", NULL, placeholder = "存入時標題（可空）"),
        textInput("lib_tags", NULL, placeholder = "標籤（;分隔）"),
        div(
          class = "d-flex gap-1 flex-wrap mb-2",
          actionButton("lib_add_current", "表單→庫", class = "btn-sm btn-primary"),
          actionButton("lib_add_selected_control", "選取控制點→庫", class = "btn-sm"),
          actionButton("lib_add_all_ready", "全部就緒控制點→庫", class = "btn-sm btn-success")
        ),
        div(
          class = "lib-options-actions d-flex gap-2 flex-wrap",
          actionButton("lib_delete", "刪除選取", class = "btn-sm btn-outline-danger"),
          downloadButton("download_lib_csv", "匯出 CSV", class = "btn-sm"),
          downloadButton("download_lib_json", "匯出 JSON", class = "btn-sm")
        )
      )
    )
  ),
  nav_panel(
    "參數庫",
    # 上：選項／篩選面板；下：即時結果
    card(
      card_header("後台參數資料庫 — 選項"),
      layout_columns(
        col_widths = c(4, 4, 4),
        selectInput("param_filter", "參數類型", choices = c("全部" = "")),
        selectInput("param_source", "來源",
                    choices = c("全部" = "", "系統預設" = "系統預設",
                                "範本庫" = "範本庫", "已定稿RCM" = "已定稿RCM",
                                "PBC命名庫" = "PBC命名庫")),
        textInput("param_query", "搜尋", placeholder = "搜尋參數或選項值…")
      ),
      div(
        class = "d-flex gap-1 flex-wrap",
        actionButton("param_refresh", "從現況重建並儲存", class = "btn-sm btn-primary"),
        actionButton("param_apply_row", "套用選取列至表單", class = "btn-sm btn-outline-success"),
        downloadButton("download_params", "下載 CSV", class = "btn-sm"),
        downloadButton("download_params_json", "下載 JSON", class = "btn-sm")
      )
    ),
    card(
      card_header("即時顯示"),
      uiOutput("param_stats"),
      DTOutput("param_table")
    )
  ),
  nav_panel(
    "PBC",
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("PBC 命名整理"),
        textInput("pbc_client", NULL, placeholder = "客戶取得原名"),
        textInput("pbc_reviewed", NULL, placeholder = "檢視後新命名"),
        selectInput("pbc_kind", "證據類型（特別標示）", choices = PBC_KIND_CHOICES),
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
      uiOutput("rcm_count_box"),
      DTOutput("rcm_table"),
      downloadButton("download_rcm", "下載 RCM CSV", class = "btn-sm"),
      tags$hr(),
      tags$strong("缺漏／缺文件／控制缺失"),
      DTOutput("gap_table")
    )
  )
)

server <- function(input, output, session) {
  controls <- reactiveVal(list())
  pbc_path_csv <- file.path(data_dir, "pbc_registry.csv")
  pbc_path_json <- file.path(data_dir, "pbc_registry.json")
  pbc_reg <- reactiveVal(load_pbc_registry(pbc_path_csv, pbc_path_json))
  lib_path_json <- file.path(data_dir, "control_library.json")
  lib_path_csv <- file.path(data_dir, "control_library.csv")
  param_path_json <- file.path(data_dir, "parameter_store.json")
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
  # 啟動時若範本庫過少，合併種子／內建批次（背景執行，不依特定循環命名）
  observeEvent(TRUE, {
    cur <- lib()
    if (length(cur) >= 5) return()
    batch <- file.path(root, "data", "jinglian_it_rcm_batch.json")
    merged <- merge_libraries(cur, seed_control_library(TRUE), overwrite = FALSE)
    if (file.exists(batch)) {
      merged <- tryCatch(
        merge_libraries(merged, load_control_library(batch, fallback_seed = FALSE), overwrite = FALSE),
        error = function(e) merged
      )
    }
    xlsx <- file.path(root, "templates", "鯨鏈科技_資訊循環_RCM_v1_0820.xlsx")
    if (file.exists(xlsx)) {
      merged <- tryCatch(
        import_control_library_file(xlsx, merged, overwrite = FALSE),
        error = function(e) merged
      )
    }
    if (length(merged) > length(cur)) {
      lib(persist_lib(merged))
    }
  }, once = TRUE)

  persist_pbc <- function(reg) save_pbc_registry(reg, pbc_path_csv, pbc_path_json)
  persist_lib <- function(library) {
    save_control_library(library, lib_path_json, lib_path_csv)
    library
  }

  param_store <- reactiveVal(load_parameter_store(param_path_json))
  persist_params <- function() {
    df <- parameter_catalog(
      library = lib(), controls = controls(),
      pbc = pbc_reg()
    )
    save_parameter_store(df, param_path_json)
    param_store(df)
    df
  }

  # Seed empty parameter DB after UI is ready (avoid blocking cascade updates)
  session$onFlushed(function() {
    if (!nrow(isolate(param_store()))) {
      try(persist_params(), silent = TRUE)
    }
  }, once = TRUE)

  output$cascade_candidate_banner <- renderUI({
    cy <- input$cycle %||% ""
    n_lib <- length(lib())
    if (!nzchar(cy)) {
      return(div(class = "alert alert-warning py-2 mb-2 small",
                 tags$strong("請先在左側選擇循環。"),
                 "選定後才會載入該循環的子作業／風險／目標／活動候選。"))
    }
    rows <- cascade_rows()
    n_sub <- length(cascade_sub_process_choices(rows))
    if (n_sub > 0) {
      div(class = "alert alert-success py-1 mb-2 small",
          sprintf("引導候選已載入：本循環「%s」有 %d 個子作業選項（範本庫 %d 筆）。請先選②子作業，③～⑥才會依序出現。",
                  cy, n_sub, n_lib))
    } else {
      div(class = "alert alert-danger py-2 mb-2 small",
          tags$strong("目前沒有引導候選。"),
          sprintf("（循環＝%s，範本庫＝%d 筆）", cy, n_lib),
          "請至「範本庫」匯入 CSV／JSON／RCM xlsx，或套用左側範本。")
    }
  })

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

  # 啟動時循環維持未選，需使用者主動選擇
  observeEvent(TRUE, {
    updateSelectInput(session, "cycle", selected = "")
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
  output$lib_stats_box <- renderUI({
    st <- library_stats(lib())
    src <- if (length(st$sources)) {
      paste(sprintf("%s=%s", names(st$sources), unlist(st$sources)), collapse = "；")
    } else "—"
    div(
      class = "alert alert-secondary py-2 mb-0 small",
      tags$div(sprintf("累積筆數：%d", st$n)),
      tags$div(sprintf("涵蓋循環：%d（%s）", st$n_cycles, paste(st$cycles, collapse = "、"))),
      tags$div(sprintf("來源：%s", src))
    )
  })

  param_catalog_df <- reactive({
    input$param_refresh
    df <- param_store()
    if (!nrow(df)) df <- persist_params()
    filter_parameter_store(
      df,
      param = input$param_filter,
      query = input$param_query,
      source = input$param_source
    )
  })

  observe({
    st <- parameter_store_stats(param_store())
    updateSelectInput(session, "param_filter",
                      choices = c("全部" = "", stats::setNames(st$params, st$params)),
                      selected = input$param_filter %||% "")
  })

  output$param_stats <- renderUI({
    st <- parameter_store_stats(param_store())
    vis <- param_catalog_df()
    tags$p(
      class = "small text-muted",
      sprintf("資料庫 %d 筆／%d 類參數；目前篩選顯示 %d 筆。來源：%s",
              st$n, st$n_params, nrow(vis),
              paste(st$sources, collapse = "、"))
    )
  })

  output$param_table <- renderDT({
    df <- param_catalog_df()
    datatable(
      df, rownames = FALSE, selection = "single", filter = "top",
      options = list(pageLength = 25, scrollX = TRUE)
    )
  })

  output$download_params <- downloadHandler(
    filename = function() sprintf("param_catalog_%s.csv", format(Sys.Date(), "%Y%m%d")),
    content = function(file) utils::write.csv(param_store(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )
  output$download_params_json <- downloadHandler(
    filename = function() sprintf("param_catalog_%s.json", format(Sys.Date(), "%Y%m%d")),
    content = function(file) save_parameter_store(param_store(), file)
  )

  observeEvent(input$param_refresh, {
    df <- persist_params()
    showNotification(sprintf("參數資料庫已從現況重建並儲存（%d 筆）", nrow(df)),
                     type = "message")
  })

  observeEvent(input$param_apply_row, {
    sel <- input$param_table_rows_selected
    df <- param_catalog_df()
    if (!length(sel) || !nrow(df)) {
      return(showNotification("請先在表格選取一列", type = "warning"))
    }
    row <- df[sel[[1]], , drop = FALSE]
    param <- as.character(row$參數[[1]])
    val <- as.character(row$選項值[[1]])
    mapped <- list(
      "循環" = function() updateSelectInput(session, "cycle", selected = val),
      "子作業編號" = function() {
        updateTextInput(session, "custom_sub_id", value = val)
        updateSelectInput(session, "cascade_sub", selected = "__custom__")
      },
      "子作業名稱" = function() {
        updateTextInput(session, "custom_sub_name", value = val)
        updateSelectInput(session, "cascade_sub", selected = "__custom__")
      },
      "風險因素" = function() updateSelectInput(session, "cascade_risk", selected = val),
      "風險描述" = function() {
        updateTextAreaInput(session, "custom_risk_desc", value = val)
        updateSelectInput(session, "cascade_risk", selected = "__custom__")
      },
      "風險類別" = function() {
        updateSelectInput(session, "custom_risk_category", selected = val)
        updateSelectInput(session, "cascade_risk", selected = "__custom__")
      },
      "會計科目" = function() updateTextInput(session, "significant_account", value = val),
      "控制目標" = function() {
        updateTextAreaInput(session, "custom_objective", value = val)
        updateSelectInput(session, "cascade_objective", selected = "__custom__")
      },
      "控制活動" = function() {
        updateTextAreaInput(session, "custom_activity", value = val)
        updateSelectInput(session, "cascade_activity", selected = "__custom__")
      },
      "控制類型" = function() {
        updateSelectInput(session, "custom_nature", selected = val)
        updateSelectInput(session, "cascade_activity", selected = "__custom__")
      },
      "控制活動類型" = function() {
        updateSelectInput(session, "custom_approach", selected = val)
        updateSelectInput(session, "cascade_activity", selected = "__custom__")
      },
      "控制頻率" = function() {
        updateSelectInput(session, "custom_frequency", selected = val)
        updateSelectInput(session, "cascade_activity", selected = "__custom__")
      },
      "流程負責單位" = function() {
        updateTextInput(session, "custom_owner", value = val)
        updateSelectInput(session, "cascade_activity", selected = "__custom__")
      },
      "相關系統／IUC" = function() {
        updateTextInput(session, "custom_iuc", value = val)
        updateSelectInput(session, "cascade_iuc", selected = "__custom__")
      },
      "相關法令" = function() updateSelectizeInput(session, "related_law", selected = val),
      "相關政策或程序" = function() updateTextInput(session, "related_policy", value = val),
      "相關文件" = function() updateTextInput(session, "related_document", value = val)
    )
    fn <- mapped[[param]]
    if (is.null(fn)) {
      return(showNotification(sprintf("「%s」無對應表單欄（已複製概念：%s）", param, val),
                              type = "message"))
    }
    fn()
    showNotification(sprintf("已套用 %s＝%s", param, val), type = "message")
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
    fill_inputs_from_ctrl(session, item$control, lib_items = lib())
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
      options = list(pageLength = 15, scrollX = TRUE, dom = "ftip")
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
    sel <- resolve_cascade_selection()
    matched <- match_cascade_control(cascade_rows(), sel)
    lib_ctrl <- if (!is.null(matched) && is.list(matched)) {
      if (!is.null(matched$raw)) matched$raw else matched
    } else {
      NULL
    }
    strip_attr <- function(x) gsub("^\\[[^\\]]+\\]\\s*", "", trimws(as.character(x %||% "")))
    pick_attr <- function(field, supplement) {
      if (nzchar(supplement)) return(supplement)
      if (!is.null(lib_ctrl)) strip_attr(lib_ctrl[[field]]) else ""
    }
    rf_tag <- risk_factor_tag(sel$risk_factor)
    nature <- normalize_control_type_manual_auto(sel$nature)
    approach <- normalize_control_activity_type_pd(sel$approach)
    list(
      control_id = input$control_id %||% "",
      company = input$company %||% "",
      cycle = sel$cycle,
      sub_process_id = sel$sub_process_id,
      sub_process = sel$sub_process,
      risk_factor = rf_tag,
      risk_name = rf_tag,
      risk_description = sel$risk_description,
      risk_category = sel$risk_category,
      risk_attr_financial = labelize(
        input$attr_label_fr,
        pick_attr("risk_attr_financial", input$risk_attr_financial %||% "")
      ),
      risk_attr_operations = labelize(
        input$attr_label_op,
        pick_attr("risk_attr_operations", input$risk_attr_operations %||% "")
      ),
      risk_attr_compliance = labelize(
        input$attr_label_cp,
        pick_attr("risk_attr_compliance", input$risk_attr_compliance %||% "")
      ),
      romm_classification = input$romm_classification %||% "",
      significant_account = input$significant_account %||% "",
      assertions = paste(input$assertions %||% character(), collapse = "；"),
      control_objective = sel$control_objective,
      control_activity = sel$control_activity,
      frequency = resolve_control_frequency(nature, sel$frequency),
      responsible_unit = sel$responsible_unit,
      iuc_or_system = sel$iuc_or_system,
      related_system = sel$iuc_or_system,
      related_policy = input$related_policy %||% "",
      related_law = {
        v <- input$related_law %||% character(0)
        paste(unique(trimws(as.character(v))), collapse = "；")
      },
      related_document = input$related_document %||% "",
      nature = nature,
      approach = approach,
      control_type = nature,
      control_activity_type = approach,
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

  output$live_preview <- renderText(assemble_control_paragraph(current_draft_from_inputs()))
  output$oa_live_check <- renderUI({
    d <- current_draft_from_inputs()
    chk <- rcm_objective_activity_check(d$control_objective, d$control_activity)
    cls <- if (isTRUE(chk$ok)) "alert alert-success py-1 mb-2" else "alert alert-danger py-1 mb-2"
    div(class = cls, format_oa_check_html(chk))
  })
  output$type_live_check <- renderUI({
    d <- current_draft_from_inputs()
    tchk <- rcm_type_fields_check(d$nature, d$approach)
    cls <- if (isTRUE(tchk$ok)) "alert alert-secondary py-1 mb-2" else "alert alert-warning py-1 mb-2"
    div(class = cls, tags$small(tags$strong("類型欄防呆："), tchk$msg))
  })

  # ---- Forced cascade: cycle → 子作業 → 風險 → 目標 → 活動(單一PD) → IUC ----
  cascade_rows <- reactive({
    cy <- input$cycle %||% ""
    if (!nzchar(cy)) return(list())
    library_controls_flat(lib(), cycle = cy)
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
      if (!nzchar(nature)) {
        nature <- normalize_control_type_manual_auto(matched$nature)
      }
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
      frequency = resolve_control_frequency(nature, frequency),
      responsible_unit = owner,
      iuc_or_system = iuc,
      related_system = iuc
    )
  }

  # 會計科目：僅報導面可填且必填；其它類別鎖定並清空
  output$significant_account_hint <- renderUI({
    cat <- resolve_cascade_selection()$risk_category %||% ""
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
    cat <- resolve_cascade_selection()$risk_category %||% ""
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
    sel <- resolve_cascade_selection()
    cat <- sel$risk_category %||% ""
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
    # 引導完成且未手動填編號 → 自動順編
    ready <- cascade_selection_ready(sel)
    if (isTRUE(ready$ready) && nzchar(sel$sub_process_id) &&
        !nzchar(trimws(input$control_id %||% ""))) {
      ids <- collect_existing_control_ids(lists = list(lib(), controls()))
      updateTextInput(session, "control_id", value = next_rcm_control_id(sel$sub_process_id, ids))
    }
  })

  observeEvent(input$custom_nature, {
    if (identical(input$custom_nature, "自動")) {
      updateSelectInput(session, "custom_frequency", selected = "持續")
    }
  }, ignoreNULL = FALSE)

  observe({
    rows <- cascade_rows()
    ch_sub <- cascade_sub_process_choices(rows)
    n_lib <- length(lib())
    n_rows <- length(rows)
    label0 <- if (n_rows) {
      sprintf("② 選擇子作業…（本循環 %d 筆／庫 %d）", n_rows, n_lib)
    } else {
      sprintf("② 尚無子作業候選（範本庫 %d 筆 — 請確認循環或至範本庫匯入）", n_lib)
    }
    ch <- c(stats::setNames("", label0), ch_sub, "＋自訂新增子作業" = "__custom__")
    cur <- input$cascade_sub %||% ""
    updateSelectInput(session, "cascade_sub", choices = ch,
                      selected = if (cur %in% unname(ch)) cur else "")
  })

  observe({
    cy <- input$cycle %||% ""
    if (!nzchar(cy)) {
      updateSelectInput(session, "cascade_risk",
                        choices = c("③ 請先選擇循環…" = ""), selected = "")
      return()
    }
    rows <- cascade_rows()
    sub_key <- input$cascade_sub %||% ""
    if (nzchar(sub_key) && !identical(sub_key, "__custom__")) {
      rows <- filter_cascade_rows(rows, sub_key = sub_key)
      ch_risk <- cascade_risk_choices(rows)
      label0 <- sprintf("③ 選擇風險因素…（本子作業 %d）", length(ch_risk))
    } else if (identical(sub_key, "__custom__")) {
      ch_risk <- character()
      label0 <- "③ 自訂子作業下請自訂風險或稍後套用"
    } else {
      ch_risk <- cascade_risk_choices(rows)
      label0 <- sprintf("③ 選擇風險因素…（本循環 %d）", length(ch_risk))
    }
    ch <- c(stats::setNames("", label0), ch_risk, "＋自訂新增風險" = "__custom__")
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
                        choices = c("④ 請先選擇③風險…" = ""), selected = "")
      return()
    }
    if (nzchar(sub_key) && !identical(sub_key, "__custom__")) {
      rows <- filter_cascade_rows(rows, sub_key = sub_key)
    }
    if (!identical(rk, "__custom__")) {
      rows <- filter_cascade_rows(rows, risk_factor = rk)
    }
    ch_obj <- cascade_objective_choices(rows)
    ch <- c(stats::setNames("", sprintf("④ 選擇控制目標…（%d）", length(ch_obj))),
            ch_obj, "＋自訂新增目標" = "__custom__")
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
                        choices = c("⑤ 請先選擇④控制目標…" = ""), selected = "")
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
    ch <- c(stats::setNames("", sprintf("⑤ 選擇控制活動…（%d）", length(ch_act))),
            ch_act, "＋自訂新增活動" = "__custom__")
    cur <- input$cascade_activity %||% ""
    updateSelectInput(session, "cascade_activity", choices = ch,
                      selected = if (cur %in% unname(ch)) cur else "")
  })

  observe({
    rows <- cascade_rows()
    act <- input$cascade_activity %||% ""
    sub_key <- input$cascade_sub %||% ""
    rk <- input$cascade_risk %||% ""
    obj <- input$cascade_objective %||% ""
    if (!nzchar(act)) {
      updateSelectInput(session, "cascade_iuc",
                        choices = c("⑥ 請先選擇⑤控制活動…" = ""), selected = "")
      return()
    }
    if (nzchar(sub_key) && !identical(sub_key, "__custom__")) {
      rows <- filter_cascade_rows(rows, sub_key = sub_key)
    }
    if (nzchar(rk) && !identical(rk, "__custom__")) {
      rows <- filter_cascade_rows(rows, risk_factor = rk)
    }
    if (nzchar(obj) && !identical(obj, "__custom__")) {
      rows <- filter_cascade_rows(rows, objective = obj)
    }
    if (!identical(act, "__custom__")) {
      rows <- filter_cascade_rows(rows, activity_key_sel = act)
    }
    ch_iuc <- cascade_iuc_choices(rows, pbc_df = pbc_reg())
    ch <- c(stats::setNames("", sprintf("⑥ 選擇 IUC／相關系統…（%d）", length(ch_iuc))),
            ch_iuc, "＋自訂新增 IUC" = "__custom__")
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
        if (!ready$ready) tags$span(class = "text-muted", " — 完成引導後即可定稿"))
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
        "本循環尚無引導選項 — 請至「範本庫」匯入 RCM 或確認左側已選循環。"
      )
    )
  })

  output$auto_control_id_box <- renderUI({
    sel <- resolve_cascade_selection()
    spid <- sel$sub_process_id
    if (!nzchar(spid)) return(NULL)
    ids <- collect_existing_control_ids(lists = list(lib(), controls()))
    nid <- next_rcm_control_id(spid, ids)
    div(class = "small text-muted mb-2",
        "自動控制編號預覽：", tags$code(nid),
        "（完成引導後自動順編；定稿時寫入）")
  })

  observeEvent(input$reset_cascade, {
    updateSelectInput(session, "cascade_sub", selected = "")
    updateSelectInput(session, "cascade_risk", selected = "")
    updateSelectInput(session, "cascade_objective", selected = "")
    updateSelectInput(session, "cascade_activity", selected = "")
    updateSelectInput(session, "cascade_iuc", selected = "")
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
      return(showNotification("找不到內建 RCM 範本檔", type = "error"))
    }
    tryCatch({
      new_lib <- import_control_library_file(path, lib(), overwrite = isTRUE(input$lib_overwrite))
      lib(persist_lib(new_lib))
      refresh_lib_choices()
      showNotification(sprintf("已載入 RCM 範本庫，共 %d 筆", length(new_lib)), type = "message")
    }, error = function(e) showNotification(conditionMessage(e), type = "error"))
  })

  observeEvent(input$oa_swap, {
    if (identical(input$cascade_objective, "__custom__") &&
        identical(input$cascade_activity, "__custom__")) {
      o <- input$custom_objective %||% ""
      a <- input$custom_activity %||% ""
      updateTextAreaInput(session, "custom_objective", value = a)
      updateTextAreaInput(session, "custom_activity", value = o)
    } else {
      showNotification("請於引導④⑤選「自訂新增」後再對調目標/活動", type = "message")
    }
  })

  observeEvent(input$oa_split_suggest, {
    if (identical(input$cascade_objective, "__custom__") &&
        identical(input$cascade_activity, "__custom__")) {
      blob <- paste(c(input$custom_objective %||% "", input$custom_activity %||% ""), collapse = "。")
      sug <- suggest_objective_activity_split(blob)
      updateTextAreaInput(session, "custom_objective", value = sug$objective)
      updateTextAreaInput(session, "custom_activity", value = sug$activity)
      showNotification(sug$note, type = "message")
    } else {
      d <- current_draft_from_inputs()
      sug <- suggest_objective_activity_split(
        paste(c(d$control_objective, d$control_activity), collapse = "。")
      )
      showNotification(
        paste0("目前為範本選取，拆分建議：", sug$note),
        type = "message", duration = 8
      )
    }
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
    req <- design_required_check(d)
    if (isTRUE(ready$ready) && isTRUE(chk$ok) && isTRUE(req$ok)) {
      div(class = "alert alert-success py-1 mb-2",
          "設計必填齊全＝可寫入 RCM 一列｜", format_oa_check_html(chk))
    } else {
      high <- gaps[gaps$severity == "高", , drop = FALSE]
      summary <- if (!isTRUE(req$ok)) paste0("必填未齊：", paste(req$missing, collapse = "、"))
      else if (!isTRUE(chk$ok)) (chk$msg %||% paste(chk$issues, collapse = "；"))
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
    sel <- resolve_cascade_selection()
    ready <- cascade_selection_ready(sel)
    if (!isTRUE(ready$ready)) {
      return(showNotification(
        paste0("引導尚未完成，不能定稿：", paste(ready$missing, collapse = "、")),
        type = "error", duration = 10
      ))
    }
    if (!activity_type_ok(sel$approach)) {
      return(showNotification("控制活動須對應單一預防／偵測屬性", type = "error"))
    }
    d <- current_draft_from_inputs()
    ids <- collect_existing_control_ids(lists = list(lib(), controls()))
    fin <- finalize_control_as_rcm_row(d, existing_ids = ids, seq_hint = length(controls()) + 1L)
    if (!isTRUE(fin$ok)) {
      return(showNotification(
        paste0("尚未完成設計，不能寫入 RCM 列：", fin$msg),
        type = "error", duration = 10
      ))
    }
    pt <- fin$control
    cs <- controls()
    idx <- which(vapply(cs, function(x) identical(x$control_id, pt$control_id), logical(1)))
    if (length(idx)) cs[[idx[[1]]]] <- pt else cs[[length(cs) + 1]] <- pt
    controls(cs)
    updateTextInput(session, "control_id",
                    value = next_rcm_control_id(
                      pt$sub_process_id,
                      collect_existing_control_ids(lists = list(lib(), controls()))
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
        reviewed_name = input$pbc_reviewed, pbc_kind = input$pbc_kind,
        iuc_or_system = input$pbc_reviewed,
        cycle = input$pbc_cycle, notes = input$pbc_notes
      ))
      pbc_reg(reg)
      persist_pbc(reg)
      refresh_pbc_choices()
      updateTextInput(session, "pbc_id", value = "")
      updateTextInput(session, "pbc_client", value = "")
      updateTextInput(session, "pbc_reviewed", value = "")
      updateSelectInput(session, "pbc_kind", selected = "")
      updateTextInput(session, "pbc_notes", value = "")
      showNotification("已登錄 PBC", type = "message")
    }, error = function(e) showNotification(conditionMessage(e), type = "error"))
  })
  output$pbc_table <- renderDT({
    df <- pbc_reg()
    if (!nrow(df)) {
      empty <- data.frame(
        ID = character(), 證據類型 = character(), 客戶原名 = character(),
        檢視後 = character(), 循環 = character(), 備註 = character(),
        stringsAsFactors = FALSE
      )
      return(datatable(empty, selection = "single", rownames = FALSE,
                       options = list(pageLength = 8, scrollX = TRUE, dom = "tip")))
    }
    show <- data.frame(
      ID = df$pbc_id,
      證據類型 = ifelse(nzchar(df$pbc_kind), df$pbc_kind, "—"),
      客戶原名 = df$client_pbc_name,
      檢視後 = vapply(seq_len(nrow(df)), function(i) {
        format_pbc_reviewed_label(df$reviewed_name[i], df$pbc_kind[i])
      }, character(1)),
      循環 = df$cycle,
      備註 = df$notes,
      stringsAsFactors = FALSE
    )
    dt <- datatable(show, selection = "single", rownames = FALSE,
                    options = list(pageLength = 8, scrollX = TRUE, dom = "tip"))
    for (kind in PBC_KIND_VALUES) {
      dt <- DT::formatStyle(
        dt, "證據類型",
        target = "cell",
        backgroundColor = DT::styleEqual(
          kind,
          switch(kind,
                 "螢幕截圖" = "#E8EEF8",
                 "EMAIL" = "#F4F9E8",
                 "系統表單" = "#EAF4D4",
                 "政策制度" = "#F0F0F0",
                 "#FFFFFF")
        ),
        fontWeight = DT::styleEqual(kind, "bold")
      )
    }
    dt
  })
  observeEvent(input$pbc_table_rows_selected, {
    s <- input$pbc_table_rows_selected
    if (is.null(s)) return()
    row <- pbc_reg()[s, , drop = FALSE]
    updateTextInput(session, "pbc_id", value = row$pbc_id[[1]])
    updateTextInput(session, "pbc_client", value = row$client_pbc_name[[1]])
    updateTextInput(session, "pbc_reviewed", value = row$reviewed_name[[1]])
    updateSelectInput(session, "pbc_kind",
                      selected = normalize_pbc_kind(row$pbc_kind[[1]]))
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

  # RCM / worksheets (訪談問項、自我評估測試步驟)
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
  selected_worksheet_controls_sa <- reactive({
    cs <- controls()
    if (!length(cs)) return(list())
    ids <- input$worksheet_controls_sa
    if (!length(ids) || all(!nzchar(ids))) return(cs)
    Filter(function(c) c$control_id %in% ids, cs)
  })
  observe({
    cs <- controls()
    if (!length(cs)) {
      updateSelectizeInput(session, "worksheet_controls", choices = character(), server = TRUE)
      updateSelectizeInput(session, "worksheet_controls_sa", choices = character(), server = TRUE)
      return()
    }
    ch <- stats::setNames(
      vapply(cs, function(x) x$control_id, ""),
      vapply(cs, function(x) sprintf("%s｜%s", x$control_id, x$risk_name %||% ""), "")
    )
    updateSelectizeInput(session, "worksheet_controls", choices = ch, server = TRUE)
    updateSelectizeInput(session, "worksheet_controls_sa", choices = ch, server = TRUE)
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
               sprintf("已定稿 RCM %d 列 → 訪談問項 %d 則", n, nrow(iv)))
  })
  output$csa_status <- renderUI({
    cs <- selected_worksheet_controls_sa()
    csa <- controls_to_csa(cs, input$csa_elements, finalized_only = TRUE)
    tags$small(class = "text-muted",
               sprintf("自我評估測試步驟 %d 列（僅已定稿 RCM）", nrow(csa)))
  })
  output$interview_table <- renderDT({
    datatable(controls_to_interview(selected_worksheet_controls(), input$interview_elements,
                                    finalized_only = TRUE),
              rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10, dom = "tip"))
  })
  output$csa_table <- renderDT({
    datatable(controls_to_csa(selected_worksheet_controls_sa(), input$csa_elements,
                              finalized_only = TRUE),
              rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10, dom = "tip"))
  })
  output$gap_table <- renderDT({
    datatable(detect_gaps_many(controls()), rownames = FALSE,
              options = list(scrollX = TRUE, pageLength = 8, dom = "tip"))
  })

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
    filename = function() sprintf("self-assessment-%s.csv", format(Sys.time(), "%Y%m%d")),
    content = function(file) {
      write.csv(controls_to_csa(selected_worksheet_controls_sa(), input$csa_elements,
                                finalized_only = TRUE),
                file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui, server)
