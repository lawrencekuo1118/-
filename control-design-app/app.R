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
source(file.path(root, "R", "privilege.R"), local = TRUE)

# UI label with required asterisk
lab_req <- function(txt) {
  tagList(txt, tags$span("*", class = "text-danger ms-1", title = "設計必填"))
}
lab_opt <- function(txt) {
  tagList(txt, tags$span(class = "text-muted small ms-1", "選填"))
}

fill_inputs_from_ctrl <- function(session, ctrl, lib_items = NULL, pbc_registry = NULL) {
  if (is.null(ctrl)) return()
  apply_ctrl_to_cascade(session, ctrl)
  apply_supplement_from_ctrl(session, ctrl, pbc_registry = pbc_registry)
}

ui <- page_navbar(
  id = "main_nav",
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
  header = tags$head(
    tags$script(HTML("
    Shiny.addCustomMessageHandler('toggleAccount', function(msg) {
      var el = document.getElementById('significant_account');
      if (!el) return;
      var $el = $('#significant_account');
      if ($el.length && $el[0].selectize) {
        if (msg.enabled) $el[0].selectize.enable();
        else { $el[0].selectize.disable(); $el[0].selectize.clear(); }
      } else {
        el.disabled = !msg.enabled;
        el.readOnly = !msg.enabled;
        el.classList.toggle('bg-light', !msg.enabled);
      }
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
    Shiny.addCustomMessageHandler('toggleAssertions', function(msg) {
      var el = document.getElementById('assertions');
      if (!el) return;
      var $el = $('#assertions');
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
    Shiny.addCustomMessageHandler('toggleRelatedDocument', function(msg) {
      var el = document.getElementById('related_document_pbc');
      if (!el) return;
      var $el = $('#related_document_pbc');
      if ($el.length && $el[0].selectize) {
        if (msg.enabled) $el[0].selectize.enable();
        else { $el[0].selectize.disable(); $el[0].selectize.clear(); }
      } else {
        el.disabled = !msg.enabled;
      }
    });
  ")),
    tags$style(HTML(paste0("
      :root { --brand-blue: ", BRAND_BLUE, "; --brand-green: ", BRAND_GREEN, "; --brand-black: ", BRAND_BLACK, "; --brand-white: ", BRAND_WHITE, "; }
      .navbar { background-color: var(--brand-black) !important; border-bottom: 3px solid var(--brand-green); }
      .navbar .navbar-brand { color: var(--brand-white) !important; font-weight: 700; letter-spacing: 0.02em; }
      .navbar .navbar-brand::after { content: \"\"; display: inline-block; width: 0.45em; height: 0.45em; margin-left: 0.15em; margin-bottom: 0.05em; border-radius: 50%; background: var(--brand-green); vertical-align: middle; }
      .navbar .nav-link { color: rgba(255,255,255,0.82) !important; }
      .navbar .nav-link:hover, .navbar .nav-link.active { color: var(--brand-green) !important; }
      .bslib-sidebar-layout > .sidebar { background: var(--brand-white); border-right: 1px solid #E5E5E5; }
      .card { border-color: #E5E5E5; }
      .card-header { background: var(--brand-white); border-bottom: 2px solid var(--brand-green); color: var(--brand-blue); font-weight: 600; white-space: normal; overflow: visible; line-height: 1.35; }
      .btn-primary { background-color: var(--brand-blue); border-color: var(--brand-blue); }
      .btn-primary:hover, .btn-primary:focus { background-color: #00205B; border-color: #00205B; }
      .btn-success { background-color: var(--brand-green); border-color: var(--brand-green); color: var(--brand-black); font-weight: 600; }
      .btn-success:hover, .btn-success:focus { background-color: #6FA01E; border-color: #6FA01E; color: var(--brand-black); }
      .btn-outline-success { color: var(--brand-green); border-color: var(--brand-green); }
      .btn-outline-success:hover { background-color: var(--brand-green); border-color: var(--brand-green); color: var(--brand-black); }
      .btn-outline-primary { color: var(--brand-blue); border-color: var(--brand-blue); }
      .btn-outline-primary:hover { background-color: var(--brand-blue); color: var(--brand-white); }
      .accordion-button:not(.collapsed) { background-color: rgba(134,188,37,0.12); color: var(--brand-blue); box-shadow: inset 0 -1px 0 var(--brand-green); }
      .accordion-button:focus { box-shadow: 0 0 0 0.2rem rgba(134,188,37,0.25); }
      .form-control:focus, .form-select:focus { border-color: var(--brand-green); box-shadow: 0 0 0 0.2rem rgba(134,188,37,0.2); }
      .alert-success { background-color: rgba(134,188,37,0.15); border-color: var(--brand-green); color: #1A2E00; }
      .alert-info { background-color: rgba(0,46,130,0.08); border-color: var(--brand-blue); color: var(--brand-blue); }
      .text-danger { color: #C41E3A !important; }
      a { color: var(--brand-blue); }
      a:hover { color: var(--brand-green); }
      .lib-options-section .shiny-input-container { margin-bottom: 0.75rem; }
      .lib-options-section .form-check { margin-bottom: 0.75rem; }
      .lib-options-actions { clear: both; width: 100%; margin-top: 1rem; padding-top: 1rem; border-top: 1px solid #dee2e6; }
      .sidebar-lib-block .shiny-input-container { margin-bottom: 0.5rem; }
      .sidebar-lib-block .form-check { margin-top: 0.5rem; margin-bottom: 0.25rem; }
      /* 範本套用：避免標題／標籤／選單字句重疊 */
      .lib-apply-card .card-header { white-space: normal; overflow: visible; line-height: 1.4; padding: 0.75rem 1rem; }
      .lib-apply-card .card-body { overflow: visible !important; }
      .lib-apply-card .shiny-input-container { margin-bottom: 1rem !important; clear: both; width: 100%; }
      .lib-apply-card label { display: block; margin-bottom: 0.35rem; white-space: normal; }
      .lib-apply-card .form-select, .lib-apply-card .form-control { width: 100%; }
      .lib-apply-card .selectize-control { margin-bottom: 0; }
      .lib-apply-card .selectize-dropdown { z-index: 1060 !important; }
      .home-hero { background: linear-gradient(135deg, #000000 0%, #002E82 70%); color: #fff; padding: 1.5rem 1.75rem; border-radius: 0.5rem; margin-bottom: 1rem; border-bottom: 4px solid var(--brand-green); }
      .home-hero h2 { color: #fff; font-weight: 700; margin: 0 0 0.5rem 0; }
      .home-hero p { color: rgba(255,255,255,0.88); margin: 0; }
      .home-section h5 { color: var(--brand-blue); font-weight: 700; border-left: 4px solid var(--brand-green); padding-left: 0.6rem; margin-bottom: 0.75rem; }
      .home-steps { list-style: none; padding-left: 0; counter-reset: step; }
      .home-steps li { counter-increment: step; position: relative; padding: 0.55rem 0.75rem 0.55rem 2.6rem; margin-bottom: 0.4rem; background: #F7F9FC; border-radius: 0.35rem; border: 1px solid #E5E5E5; }
      .home-steps li::before { content: counter(step); position: absolute; left: 0.55rem; top: 0.5rem; width: 1.5rem; height: 1.5rem; border-radius: 50%; background: var(--brand-green); color: #000; font-weight: 700; font-size: 0.8rem; display: flex; align-items: center; justify-content: center; }
      .home-tabs-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(220px, 1fr)); gap: 0.75rem; overflow: visible !important; max-height: none !important; }
      .home-tab-card { border: 1px solid #E5E5E5; border-top: 3px solid var(--brand-green); border-radius: 0.4rem; padding: 0.85rem 1rem; background: #fff; }
      .home-tab-card strong { color: var(--brand-blue); display: block; margin-bottom: 0.35rem; }
      /* 全頁原則：區塊一次顯示全部內容，禁止卡片／分頁內部上下捲動（僅整頁可捲） */
      .html-fill-container, .html-fill-item, .bslib-page-main, .bslib-sidebar-layout > .main,
      .tab-content, .tab-pane, .layout-columns, .bslib-grid, .bslib-grid-item {
        height: auto !important; max-height: none !important; overflow: visible !important;
        flex: none !important; min-height: 0 !important;
      }
      .bslib-card, .card, .card-body, .accordion, .accordion-item, .accordion-body, .accordion-collapse {
        overflow: visible !important; max-height: none !important; height: auto !important; flex: none !important;
      }
      .bslib-card > .card-body, .card > .card-body {
        margin-top: 0 !important; margin-bottom: 0 !important; flex: none !important;
      }
      .home-section, .home-section > .card-body { overflow: visible !important; max-height: none !important; height: auto !important; flex: none !important; }
      .dataTables_wrapper, .dataTables_scroll, .dataTables_scrollBody {
        overflow: visible !important; max-height: none !important; height: auto !important;
      }
      .shiny-text-output pre, .shiny-plot-output, .shiny-image-output {
        overflow: visible !important; max-height: none !important;
      }
      .bslib-sidebar-layout > .main { overflow-x: hidden; overflow-y: auto; }
      /* 控制目標與聲明設定並排：等高、桌面版維持雙欄 */
      .objective-assertions-row.bslib-grid {
        display: grid !important;
        grid-template-columns: minmax(0, 1fr) minmax(0, 1fr);
        gap: 1rem;
        align-items: start;
      }
      .objective-assertions-row .shiny-input-container { margin-bottom: 0.35rem; }
      .objective-assertions-row #control_objective { min-height: 7.5rem; resize: vertical; }
      .objective-assertions-row .selectize-control { min-height: 2.5rem; }
      .objective-assertions-row .assertions-side .alert { margin-bottom: 0.35rem; }
      /* 範本庫／參數庫僅由側邊欄進入，隱藏標題列選項 */
      .navbar .nav-item:has(> a[data-value=\"範本庫\"]), .navbar .nav-item:has(> a[data-value=\"參數庫\"]) { display: none !important; }
    ")))
  ),
  sidebar = sidebar(
    width = 280,
    open = "desktop",
    div(
      class = "d-flex flex-column h-100",
      div(
        textInput("company", NULL, placeholder = "公司名稱"),
        tags$hr(class = "my-2"),
        tags$div(class = "small fw-bold mb-1", "循環（全域）"),
        selectInput(
          "cycle", NULL,
          choices = c("請選擇循環…" = "", CYCLES_NINE_CHOICES),
          selected = ""
        ),
        textInput("cycle_code", NULL, value = "", placeholder = "循環編號（自動）"),
        uiOutput("sidebar_cycle_hint")
      ),
      div(
        class = "mt-auto pt-2 sidebar-lib-block",
        tags$hr(class = "my-2"),
        tags$div(class = "small fw-bold mb-1", "範本庫"),
        uiOutput("lib_count_badge"),
        actionButton("goto_lib_tab", "開啟範本庫", class = "btn-sm btn-outline-secondary w-100 mb-2"),
        tags$hr(class = "my-2"),
        tags$div(class = "small fw-bold mb-1", "參數庫"),
        actionButton("goto_param_tab", "開啟參數庫", class = "btn-sm btn-outline-secondary w-100"),
        tags$hr(class = "my-2"),
        uiOutput("admin_auth_box")
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
    card(
      class = "home-section",
      card_header("整體設計流程"),
      tags$ol(
        class = "home-steps mb-0",
        tags$li(tags$strong("側邊欄"), "設定", strong("循環"), "（全域必選）與公司名稱；循環選定後各頁共用。"),
        tags$li(tags$strong("風險控制點設計"), "：依序選取 ",
                strong("子作業 → 風險 → 控制目標 → 控制活動（單一預防／偵測）→ IUC"),
                "（", tags$span(class = "text-danger", "須先選側邊欄循環"),
                "，再依序選取；未選上一層時，下一層沒有候選）。"),
        tags$li("補齊 ", strong("風險辨識"),
                "（風險因素、風險描述、風險類別、RoMM 分類）→ ",
                strong("控制設計"),
                "；", tags$span(class = "text-danger", "*"), " 為設計必填。"),
        tags$li(strong("完成設計＝寫入 RCM 一列"),
                "（1 控制點 ↔ 1 RCM 列；控制編號自動順編如 EC-101-01）。"),
        tags$li(tags$strong("訪談問項設計"),
                "：依循環／子作業深挖預期風險與預期控制目標／活動，以 5W1H（人事時地物）了解內控實際執行現況，並可串接 PBC。"),
        tags$li(tags$strong("控制點測試設計"),
                "：填寫 Form 4120SR Inputs／Steps／Outputs，並產製 CSA 測試程序／PBC／預期結果。")
      ),
      p(class = "small text-muted mb-0 mt-2",
        "本 APP 僅產出設計欄位；控制現況描述／分析評估等後續欄位留空。",
        "介面用語採", strong("台灣用語"), "與", strong("美式英文專有名詞"),
        "（如 SOX、RCM、CSA、PBC、IUC、Form 4120SR）；不使用港澳或中國用語。")
    ),
    card(
      class = "home-section",
      card_header("各頁籤用途"),
      div(
        class = "home-tabs-grid",
        div(class = "home-tab-card",
            strong("訪談問項設計"),
            "依側邊欄循環選子作業 → 預期風險／目標／活動 → 5W1H 題綱（可串 PBC）。"),
        div(class = "home-tab-card",
            strong("風險控制點設計"),
            "依側邊欄循環引導選子作業／風險／目標／活動／IUC；定稿寫入 RCM。"),
        div(class = "home-tab-card",
            strong("控制點測試設計"),
            "CSA 測試步驟與 Form 4120SR Type／Inputs／Steps／Outputs／調查門檻。"),
        div(class = "home-tab-card",
            strong("範本庫"),
            "可跳過套用；寫入／直接編輯時才需高權登入。"),
        div(class = "home-tab-card",
            strong("參數庫"),
            "查詢／套用表單；新增刪除／重建時才需高權登入。"),
        div(class = "home-tab-card",
            strong("PBC資料庫"),
            "客戶原名 → 檢視後標準命名；證據類型標示螢幕截圖／EMAIL／系統表單／政策制度。"),
        div(class = "home-tab-card",
            strong("RCM"),
            "檢視／下載已定稿 RCM 列與缺漏表（設計欄位群組對齊鯨鏈標題列）。")
      )
    )
  ),
  nav_panel(
    "訪談問項設計",
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header("訪談引導（依序選取）"),
        uiOutput("interview_status"),
        uiOutput("interview_guide_banner"),
        # 循環於側邊欄；此處①子作業 → ②風險／控制點
        selectInput(
          "interview_sub", NULL,
          choices = c("① 選擇子作業…" = ""),
          selected = ""
        ),
        selectizeInput(
          "worksheet_controls", NULL,
          choices = NULL, multiple = TRUE,
          options = list(placeholder = "② 選擇風險／控制點（可空＝該子作業下全部建議）")
        ),
        div(
          class = "d-flex gap-1 flex-wrap mb-2",
          actionButton("ws_select_core_iv", "深入且快速（風險／目標／活動）",
                       class = "btn-sm btn-primary"),
          actionButton("ws_select_full_iv", "完整走查（含頻率／IUC／步驟）",
                       class = "btn-sm btn-outline-primary"),
          actionButton("ws_reset_iv", "重設訪談選取", class = "btn-sm btn-outline-secondary")
        ),
        tags$hr(),
        accordion(
          id = "interview_design_groups",
          open = c("訪談焦點", "5W1H／PBC"),
          accordion_panel(
            "訪談焦點",
            p(class = "small text-muted mb-2",
              "側邊欄選定循環並選子作業後，依內建建議之預期風險與預期控制目標／活動產出題綱。"),
            checkboxGroupInput(
              "interview_elements", NULL,
              choices = INTERVIEW_ELEMENTS, selected = DEFAULT_INTERVIEW_ELEMENTS
            )
          ),
          accordion_panel(
            "5W1H／PBC",
            p(class = "small text-muted mb-2",
              "模組化拼湊回答架構與探針題；可套用 PBC 資料庫命名。"),
            checkboxGroupInput(
              "interview_5w1h", NULL,
              choices = INTERVIEW_5W1H_MODULES, selected = DEFAULT_INTERVIEW_5W1H
            ),
            checkboxInput(
              "interview_include_modules",
              "將勾選之 5W1H 模組展開為獨立探針題",
              value = TRUE
            ),
            selectizeInput(
              "interview_pbc_link", "套用 IUC／PBC 命名",
              choices = NULL, multiple = TRUE,
              options = list(placeholder = "原名→新名（寫入建議串接PBC／What）")
            )
          )
        ),
        div(
          class = "d-flex gap-1 flex-wrap mt-2",
          downloadButton("download_interview", "下載訪談題綱 CSV", class = "btn-success btn-sm")
        )
      ),
      card(
        uiOutput("interview_live_box"),
        uiOutput("interview_scaffold_preview"),
        DTOutput("interview_table"),
        verbatimTextOutput("interview_paragraph")
      )
    )
  ),
  nav_panel(
    "風險控制點設計",
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header("引導設計（依序選取）"),
        uiOutput("cascade_step_status"),
        # 循環於側邊欄；①子作業 → ⑤ IUC
        selectInput("cascade_sub", NULL, choices = c("① 選擇子作業…" = "")),
        conditionalPanel(
          "input.cascade_sub == '__custom__'",
          p(class = "small text-muted mb-2",
            "自訂子作業：請於下方「基本資料」填寫子作業編號與名稱。")
        ),
        # Step 2: 風險
        selectInput("cascade_risk", NULL, choices = c("② 選擇風險因素…" = "")),
        conditionalPanel(
          "input.cascade_risk == '__custom__'",
          p(class = "small text-muted mb-2",
            "自訂風險：請於下方「風險辨識」填寫風險因素、風險描述、風險類別與 RoMM 分類。")
        ),
        # Step 3: 目標
        selectInput("cascade_objective", NULL, choices = c("③ 選擇控制目標…" = "")),
        conditionalPanel(
          "input.cascade_objective == '__custom__'",
          textAreaInput("custom_objective", NULL, rows = 2, placeholder = "自訂控制目標（Why）")
        ),
        # Step 4: 活動（標示單一 PD）
        selectInput("cascade_activity", NULL, choices = c("④ 選擇控制活動…" = "")),
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
        # Step 5: IUC（文件／資訊；與相關系統分開設定）
        selectInput("cascade_iuc", NULL, choices = c("⑤ 選擇 IUC…" = "")),
        conditionalPanel(
          "input.cascade_iuc == '__custom__'",
          textInput("custom_iuc", NULL, placeholder = "自訂 IUC（控制執行時取得之文件／資訊）"),
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
            p(class = "small text-muted mb-2",
              "循環於左側側邊欄設定（全域共用）。此處填寫／覆寫子作業編號、名稱與控制編號。"),
            uiOutput("design_cycle_readonly"),
            layout_columns(
              col_widths = c(4, 8),
              textInput("sub_process_id", lab_req("子作業編號"), value = "",
                        placeholder = "例：EC-101"),
              textInput("sub_process", lab_req("子作業名稱"), value = "",
                        placeholder = "例：存取管理作業")
            ),
            textInput("control_id", "控制編號", value = "",
                      placeholder = "自動順編（可覆寫）")
          ),
          accordion_panel(
            "風險辨識",
            p(class = "small text-muted mb-2",
              "包含：風險因素、風險描述、風險類別、RoMM 分類（與上方引導選取同步，可覆寫）。同一控制點僅一種風險類別；需其他類別時另設控制點。"),
            textInput("risk_factor", lab_req("風險因素"), value = "",
                      placeholder = "簡短風險 tag／因素名稱"),
            textAreaInput("risk_description", lab_req("風險描述"), rows = 3,
                          placeholder = "風險情境與影響描述"),
            selectInput(
              "risk_category", lab_req("風險類別"),
              choices = c("請選擇…" = "", RISK_CATEGORY_CHOICES),
              selected = ""
            ),
            selectInput("romm_classification", "RoMM 分類", choices = ROMM_CLASS_CHOICES),
            uiOutput("significant_account_hint"),
            selectizeInput(
              "significant_account", "會計科目",
              choices = account_select_choices(),
              multiple = TRUE,
              selected = character(0),
              options = list(
                create = TRUE,
                placeholder = "報導面必填：可複選常見科目，或選「全部適用」"
              )
            ),
            actionButton("account_select_all", "全部適用", class = "btn-sm btn-outline-primary mb-2"),
            textAreaInput(
              "risk_attr_detail", lab_opt("風險屬性細節"), rows = 2,
              placeholder = "對應所選風險類別之細節（可空）"
            )
          ),
          accordion_panel(
            "控制設計",
            p(class = "small text-muted mb-2",
              "控制目標／活動與類型（可與上方引導選取同步，亦可直接覆寫）。"),
            uiOutput("oa_live_check"),
            uiOutput("type_live_check"),
            div(
              class = "d-flex gap-1 flex-wrap mb-2",
              actionButton("oa_split_suggest", "拆分建議", class = "btn-sm btn-outline-secondary"),
              actionButton("oa_swap", "對調目標/活動", class = "btn-sm btn-outline-secondary")
            ),
            layout_columns(
              col_widths = c(6, 6),
              class = "objective-assertions-row",
              textAreaInput(
                "control_objective", lab_req("控制目標"), rows = 4,
                placeholder = "Why：欲達成之控制結果（非執行步驟）"
              ),
              div(
                class = "assertions-side",
                selectizeInput(
                  "assertions", "聲明設定",
                  choices = character(0), multiple = TRUE, selected = character(0),
                  options = list(
                    create = FALSE,
                    placeholder = "依風險類別：報導面八種／營運面三種／遵循面不可選"
                  )
                ),
                uiOutput("assertions_hint")
              )
            ),
            textAreaInput(
              "control_activity", lab_req("控制活動"), rows = 3,
              placeholder = "How：具體執行行為（含誰／何時／如何）"
            ),
            layout_columns(
              col_widths = c(6, 6),
              selectInput(
                "approach", lab_req("控制活動類型"),
                choices = c("請選擇…" = "", CONTROL_ACTIVITY_TYPE_PD),
                selected = ""
              ),
              selectInput(
                "nature", lab_req("控制類型"),
                choices = c("請選擇…" = "", CONTROL_TYPE_MANUAL_AUTO),
                selected = ""
              )
            ),
            layout_columns(
              col_widths = c(6, 6),
              selectInput(
                "frequency", lab_req("控制頻率"),
                choices = c("請選擇…" = "", FREQUENCY_CHOICES),
                selected = ""
              ),
              textInput(
                "responsible_unit", lab_req("流程負責單位"),
                value = "", placeholder = "例：資訊安全單位"
              )
            ),
            textAreaInput(
              "iuc", lab_req("IUC"), rows = 2,
              placeholder = "控制執行時取得之文件／資訊（與⑤引導選取同步；命名請至 PBC資料庫套用）"
            ),
            uiOutput("related_system_label"),
            textInput(
              "related_system", label = NULL,
              placeholder = "例：ERP、AD、權限管理系統（IT／應用系統，與 IUC 不同）"
            ),
            uiOutput("related_system_hint"),
            textInput("related_policy", lab_opt("相關政策或程序")),
            selectizeInput(
              "related_law", "相關法令",
              choices = c("請選擇或輸入…" = "", RELATED_LAW_CHOICES),
              multiple = TRUE,
              options = list(create = TRUE, placeholder = "僅遵循面可填；可多選／自訂")
            ),
            uiOutput("related_law_hint"),
            selectizeInput(
              "related_document_pbc", lab_req("相關文件"),
              choices = NULL, multiple = TRUE,
              options = list(
                placeholder = "自 PBC 資料庫選取文件（可多選）",
                plugins = list("remove_button")
              )
            ),
            div(
              class = "d-flex gap-1 flex-wrap mb-1",
              actionButton("goto_pbc_tab", "開啟 PBC 資料庫", class = "btn-sm btn-outline-secondary")
            ),
            uiOutput("related_document_hint")
          )
        ),
        div(
          class = "d-flex gap-1 flex-wrap mt-2",
          actionButton("finalize_rcm_row", "完成設計＝寫入 RCM 一列", class = "btn-success btn-sm"),
          actionButton("collect_ready_to_lib", "儲存→資料庫", class = "btn-outline-success btn-sm")
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
    "控制點測試設計",
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("控制點測試設計（CSA）"),
        p(class = "small text-muted mb-2",
          "僅能選取「風險控制點設計」已定版並寫入 RCM 之控制點。",
          "同一控制點可因不同控制現況情境維護多組測試步驟。",
          "抽樣樣本數依該控制實際發生頻率訂定（PCAOB AS 2301／AS 2315；Deloitte 頻率對應表）；",
          "Higher RoMM／Fraud 時上調。"),
        selectizeInput(
          "worksheet_controls_sa", NULL, choices = NULL, multiple = TRUE,
          options = list(placeholder = "已定版風險控制點（空＝全部已定版）")
        ),
        checkboxGroupInput("csa_elements", "測試步驟元素",
                           choices = DESIGN_ELEMENTS, selected = DEFAULT_CSA_ELEMENTS),
        actionButton("ws_select_core_csa", "自我評估核心元素", class = "btn-sm btn-primary"),
        uiOutput("csa_status"),
        tags$hr(),
        tags$strong(class = "small", "頻率 → 建議最低樣本數（基準／高風險）"),
        tags$div(
          class = "small text-muted mb-2",
          tags$table(
            class = "table table-sm table-borderless mb-0",
            tags$thead(tags$tr(
              tags$th("頻率"), tags$th("基準"), tags$th("Higher／Fraud")
            )),
            tags$tbody(
              tags$tr(tags$td("每年"), tags$td("1"), tags$td("1")),
              tags$tr(tags$td("每半年"), tags$td("2"), tags$td("2")),
              tags$tr(tags$td("每季"), tags$td("2"), tags$td("3")),
              tags$tr(tags$td("每月"), tags$td("3"), tags$td("5")),
              tags$tr(tags$td("每週"), tags$td("10"), tags$td("15")),
              tags$tr(tags$td("每日／每筆交易"), tags$td("25"), tags$td("40")),
              tags$tr(tags$td("持續／自動"), tags$td("To1＋再執行1"), tags$td("To1＋再執行2")),
              tags$tr(tags$td("事件觸發／其他"), tags$td(colspan = 2, "依期間發生次數／母體"))
            )
          )
        ),
        tags$hr(),
        tags$strong(class = "small", "控制現況情境組（同一控制點可多組）"),
        p(class = "small text-muted mb-2",
          "同一已定版控制點可因不同控制現況情境，各自維護一組測試步驟（Type／Inputs／Steps／Outputs）。"),
        selectizeInput(
          "csa_edit_control", "編輯控制點", choices = NULL,
          options = list(placeholder = "選擇已定版控制點以編輯情境組")
        ),
        selectizeInput(
          "csa_scenario_pick", "情境組", choices = NULL,
          options = list(placeholder = "選擇或新增情境組")
        ),
        textInput("csa_scenario_name", "控制現況情境名稱",
                  placeholder = "例：電子簽核路徑／口頭核准路徑"),
        textAreaInput("csa_scenario_status", "該情境之控制現況說明", rows = 2,
                      placeholder = "描述此情境下公司實際怎麼做"),
        div(
          class = "d-flex gap-1 flex-wrap mb-2",
          actionButton("csa_scenario_add", "新增情境組", class = "btn-sm btn-outline-primary"),
          actionButton("csa_scenario_save", "儲存此情境組", class = "btn-sm btn-primary"),
          actionButton("csa_scenario_del", "刪除此情境組", class = "btn-sm btn-outline-danger")
        ),
        tags$strong(class = "small", "此情境組之測試步驟（Form 4120SR）"),
        selectizeInput("type", "Type", choices = TYPE_CHOICES,
                       options = list(create = TRUE, placeholder = "Form 4120SR Type")),
        textAreaInput("inputs", "Inputs", rows = 2, placeholder = "測試投入／證據來源"),
        textAreaInput("review_steps", "Steps", rows = 4, placeholder = "測試步驟（每行一步）"),
        textAreaInput("outputs", "Outputs", rows = 2, placeholder = "預期產出／文件"),
        textAreaInput("investigation_threshold", "調查門檻", rows = 1, placeholder = "調查門檻")
      ),
      card(
        DTOutput("csa_table"),
        downloadButton("download_csa", "下載自我評估測試步驟 CSV", class = "btn-sm")
      )
    )
  ),
  nav_panel(
    "範本庫",
    card(
      class = "lib-apply-card",
      card_header("範本套用"),
      p(class = "small text-muted mb-3",
        "選用既有範本填入「風險控制點設計」；可不選、直接於設計頁建立。"),
      textInput("lib_query", "搜尋", value = "",
                placeholder = "搜尋標題／風險／控制編號…"),
      selectInput(
        "lib_pick", "選擇範本",
        choices = c("未套用範本…" = "")
      ),
      div(
        class = "d-flex gap-1 flex-wrap mb-2 mt-1",
        actionButton("apply_lib", "套用選取範本", class = "btn-sm btn-primary"),
        actionButton("apply_lib_selected_row", "套用表格列", class = "btn-sm btn-outline-primary")
      ),
      p(class = "small text-muted mb-0",
        "寫入／匯入／刪除／直接編輯時會跳出高權登入。")
    ),
    uiOutput("admin_lib_edit_panel"),
    card(
      card_header("即時顯示"),
      DTOutput("lib_table"),
      verbatimTextOutput("lib_preview")
    ),
    uiOutput("admin_lib_mutate_panel"),
    card(
      card_header("匯出（唯讀可用）"),
      div(
        class = "d-flex gap-2 flex-wrap",
        downloadButton("download_lib_csv", "匯出 CSV", class = "btn-sm"),
        downloadButton("download_lib_json", "匯出 JSON", class = "btn-sm")
      )
    )
  ),
  nav_panel(
    "參數庫",
    # 上：選項／篩選面板；下：即時結果
    card(
      card_header("後台參數資料庫 — 查詢"),
      layout_columns(
        col_widths = c(4, 4, 4),
        selectInput("param_filter", "參數類型", choices = c("全部" = "")),
        selectInput("param_source", "來源",
                    choices = c("全部" = "", "系統預設" = "系統預設",
                                "範本庫" = "範本庫", "已定稿RCM" = "已定稿RCM",
                                "PBC命名庫" = "PBC命名庫", "高權維護" = "高權維護")),
        textInput("param_query", "搜尋", placeholder = "搜尋參數或選項值…")
      ),
      div(
        class = "d-flex gap-1 flex-wrap",
        actionButton("param_apply_row", "套用選取列至表單", class = "btn-sm btn-outline-success"),
        downloadButton("download_params", "下載 CSV", class = "btn-sm"),
        downloadButton("download_params_json", "下載 JSON", class = "btn-sm")
      )
    ),
    uiOutput("admin_param_edit_panel"),
    card(
      card_header("即時顯示"),
      uiOutput("param_stats"),
      DTOutput("param_table")
    )
  ),
  nav_panel(
    "PBC資料庫",
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("PBC 資料庫"),
        p(class = "small text-muted mb-2",
          "整理客戶取得原名與檢視後標準命名（公司現況／證據命名）。"),
        textInput("pbc_client", NULL, placeholder = "客戶取得原名"),
        textInput("pbc_reviewed", NULL, placeholder = "檢視後新命名"),
        selectInput("pbc_kind", "證據類型（特別標示）", choices = PBC_KIND_CHOICES),
        textInput("pbc_id", NULL, placeholder = "ID（可空）"),
        uiOutput("pbc_cycle_readonly"),
        textInput("pbc_notes", NULL, placeholder = "備註"),
        div(
          class = "d-flex gap-1 flex-wrap",
          actionButton("pbc_add", "登錄", class = "btn-primary btn-sm"),
          actionButton("pbc_delete", "刪除", class = "btn-outline-danger btn-sm"),
          actionButton("pbc_apply_to_design", "套用至控制設計",
                       class = "btn-sm btn-outline-success")
        ),
        fileInput("upload_pbc", NULL, buttonLabel = "匯入 CSV", accept = ".csv"),
        downloadButton("download_pbc", "匯出 CSV", class = "btn-sm mt-1"),
        tags$hr(),
        tags$div(class = "small fw-bold mb-1", "套用 IUC／PBC 命名"),
        p(class = "small text-muted mb-2",
          "將命名對照套用至「風險控制點設計」之 IUC（公司現況整理）。"),
        selectizeInput(
          "pbc_apply", NULL, choices = NULL, multiple = TRUE,
          options = list(placeholder = "原名→新名")
        ),
        checkboxInput("pbc_also_inputs", "一併寫入測試設計 Inputs 對照", FALSE)
      ),
      card(DTOutput("pbc_table"), verbatimTextOutput("pbc_all_status"))
    )
  ),
  nav_panel(
    "RCM",
    card(
      uiOutput("rcm_count_box"),
      uiOutput("rcm_latest_saved"),
      DTOutput("rcm_table"),
      downloadButton("download_rcm", "下載 RCM CSV", class = "btn-sm"),
      tags$hr(),
      tags$strong("缺漏／缺文件／控制缺失"),
      DTOutput("gap_table")
    )
  ),
)

server <- function(input, output, session) {
  controls <- reactiveVal(list())
  is_admin <- reactiveVal(FALSE)
  rcm_revision <- reactiveVal(0L)
  last_saved_control <- reactiveVal(NULL)

  bump_rcm_views <- function(ctrl = NULL) {
    rcm_revision(rcm_revision() + 1L)
    if (!is.null(ctrl)) last_saved_control(ctrl)
  }

  rcm_display_df <- reactive({
    rcm_revision()
    cs <- Filter(is_control_finalized_for_rcm, controls())
    if (!length(cs)) return(NULL)
    cs <- rev(cs)
    rcm <- controls_to_rcm(cs)
    if (!nrow(rcm)) return(NULL)
    saved <- vapply(cs, function(x) as.character(x$saved_at %||% ""), character(1))
    if (length(saved) == nrow(rcm)) {
      rcm <- cbind(`儲存時間` = saved, as.data.frame(rcm, stringsAsFactors = FALSE))
    }
    rcm
  })

  output$admin_auth_box <- renderUI({
    if (isTRUE(is_admin())) {
      tags$div(
        class = "small text-muted",
        tags$span("高權已登入"),
        actionLink("admin_logout", "登出", class = "ms-1 small")
      )
    } else {
      tags$div(
        class = "small text-muted",
        "高權：修改範本／參數時再登入"
      )
    }
  })

  observeEvent(input$admin_login, {
    if (verify_admin_password(input$admin_password)) {
      is_admin(TRUE)
      removeModal()
      showNotification("高權登入成功", type = "message")
    } else {
      is_admin(FALSE)
      showNotification("密碼錯誤", type = "error")
    }
  })
  observeEvent(input$admin_logout, {
    is_admin(FALSE)
    showNotification("已登出高權", type = "message")
  })

  output$admin_lib_edit_panel <- renderUI({
    if (!isTRUE(is_admin())) return(NULL)
    card(
      card_header("高權：直接編輯選取範本"),
      p(class = "small text-muted mb-2", "先於下方表格選取一列，載入後修改並儲存。"),
      actionButton("admin_lib_load_row", "載入選取列", class = "btn-sm btn-outline-primary mb-2"),
      textInput("admin_lib_id", "library_id", value = ""),
      textInput("admin_lib_title", "標題", value = ""),
      textInput("admin_lib_tags", "標籤（;分隔）", value = ""),
      textInput("admin_lib_risk", "風險因素", value = ""),
      textAreaInput("admin_lib_risk_desc", "風險描述", rows = 2, value = ""),
      selectInput("admin_lib_risk_cat", "風險類別",
                  choices = c("請選擇…" = "", RISK_CATEGORY_CHOICES)),
      textAreaInput("admin_lib_objective", "控制目標", rows = 2, value = ""),
      textAreaInput("admin_lib_activity", "控制活動", rows = 2, value = ""),
      textInput("admin_lib_iuc", "IUC", value = ""),
      textInput("admin_lib_system", "相關系統", value = ""),
      actionButton("admin_lib_save_fields", "儲存範本變更", class = "btn-sm btn-success")
    )
  })

  output$admin_lib_mutate_panel <- renderUI({
    if (!isTRUE(is_admin())) {
      return(div(
        class = "alert alert-secondary py-2 small",
        "匯入／刪除／收集入庫需高權。",
        actionButton("admin_prompt_lib", "登入高權", class = "btn-sm btn-outline-primary ms-2")
      ))
    }
    card(
      card_header("高權：範本寫入"),
      div(
        class = "lib-options-section",
        uiOutput("lib_stats_box"),
        tags$hr(class = "my-2"),
        tags$h6(class = "small fw-bold mb-2", "匯入"),
        fileInput("upload_lib", NULL, buttonLabel = "匯入 CSV／JSON／RCM xlsx",
                  accept = c(".csv", ".json", ".xlsx", ".xls")),
        checkboxInput("lib_overwrite", "同 ID 則覆蓋（累積更新）", TRUE),
        tags$hr(class = "my-2"),
        tags$h6(class = "small fw-bold mb-2", "收集入庫"),
        textInput("lib_title_override", NULL, placeholder = "存入時標題（可空）"),
        textInput("lib_tags", NULL, placeholder = "標籤（;分隔）"),
        checkboxInput("auto_collect_lib", "設計完成自動收集入庫", TRUE),
        div(
          class = "d-flex gap-1 flex-wrap mb-2",
          actionButton("lib_add_current", "表單→庫", class = "btn-sm btn-primary"),
          actionButton("lib_add_all_ready", "全部就緒→庫", class = "btn-sm btn-success"),
          actionButton("lib_delete", "刪除選取", class = "btn-sm btn-outline-danger")
        )
      )
    )
  })

  output$admin_param_edit_panel <- renderUI({
    if (!isTRUE(is_admin())) {
      return(div(
        class = "alert alert-secondary py-2 small",
        "新增／刪除／重建參數需高權。",
        actionButton("admin_prompt_param", "登入高權", class = "btn-sm btn-outline-primary ms-2")
      ))
    }
    card(
      card_header("高權：直接維護參數列"),
      layout_columns(
        col_widths = c(4, 4, 4),
        textInput("admin_param_name", "參數", placeholder = "例：風險類別"),
        textInput("admin_param_value", "選項值", placeholder = "例：營運面"),
        textInput("admin_param_source", "來源", value = "高權維護")
      ),
      div(
        class = "d-flex gap-1 flex-wrap",
        actionButton("admin_param_upsert", "新增／更新列", class = "btn-sm btn-success"),
        actionButton("admin_param_delete", "刪除選取列", class = "btn-sm btn-outline-danger"),
        actionButton("param_refresh", "從現況重建並儲存", class = "btn-sm btn-primary")
      )
    )
  })

  observeEvent(input$admin_prompt_lib, {
    show_admin_login_modal(session)
  })
  observeEvent(input$admin_prompt_param, {
    show_admin_login_modal(session)
  })

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
  # 啟動時確保內建候選就緒（九大循環可直接選；毋須使用者先匯入底稿）
  observeEvent(TRUE, {
    cur <- lib()
    builtin <- seed_control_library(TRUE)
    merged <- merge_libraries(cur, builtin, overwrite = FALSE)
    batch <- file.path(root, "data", "jinglian_it_rcm_batch.json")
    if (file.exists(batch)) {
      merged <- tryCatch(
        merge_libraries(merged, load_control_library(batch, fallback_seed = FALSE), overwrite = FALSE),
        error = function(e) merged
      )
    }
    if (length(merged) > length(cur)) {
      lib(persist_lib(merged, force = TRUE))
    }
  }, once = TRUE)

  persist_pbc <- function(reg) save_pbc_registry(reg, pbc_path_csv, pbc_path_json)
  persist_lib <- function(library, force = FALSE) {
    if (!isTRUE(force) && !require_admin(is_admin(), session)) {
      return(isolate(lib()))
    }
    save_control_library(library, lib_path_json, lib_path_csv)
    library
  }

  param_store <- reactiveVal(load_parameter_store(param_path_json))
  persist_params <- function(force = FALSE) {
    if (!isTRUE(force) && !require_admin(is_admin(), session)) {
      return(isolate(param_store()))
    }
    df <- parameter_catalog(
      library = lib(), controls = controls(),
      pbc = pbc_reg()
    )
    save_parameter_store(df, param_path_json)
    param_store(df)
    df
  }
  persist_params_df <- function(df) {
    if (!require_admin(is_admin(), session)) return(isolate(param_store()))
    save_parameter_store(df, param_path_json)
    param_store(df)
    df
  }

  # Seed empty parameter DB after UI is ready (avoid blocking cascade updates)
  session$onFlushed(function() {
    if (!nrow(isolate(param_store()))) {
      try(persist_params(force = TRUE), silent = TRUE)
    }
  }, once = TRUE)

  output$sidebar_cycle_hint <- renderUI({
    cy <- input$cycle %||% ""
    if (!nzchar(cy)) {
      tags$div(class = "small text-warning", "必選：選定後訪談／設計／PBC 皆共用此循環。")
    } else {
      tags$div(class = "small text-muted", sprintf("目前：%s", cy))
    }
  })
  output$design_cycle_readonly <- renderUI({
    cy <- input$cycle %||% ""
    cc <- trimws(input$cycle_code %||% "")
    if (!nzchar(cy)) {
      div(class = "alert alert-warning py-1 mb-2 small",
          tags$strong("尚未選定循環。"), "請先於左側側邊欄選擇循環。")
    } else {
      div(class = "alert alert-secondary py-1 mb-2 small",
          sprintf("循環：%s（編號 %s）— 於側邊欄變更。", cy, if (nzchar(cc)) cc else "—"))
    }
  })
  output$pbc_cycle_readonly <- renderUI({
    cy <- input$cycle %||% ""
    tags$div(
      class = "small text-muted mb-2",
      if (nzchar(cy)) sprintf("循環：%s（側邊欄）", cy) else "循環：共用／未選（側邊欄可指定）"
    )
  })

  refresh_lib_choices <- function() {
    ch <- library_choices(lib(), cycle_filter = input$cycle, query = input$lib_query)
    updateSelectInput(
      session, "lib_pick",
      choices = c("未套用範本…" = "", ch),
      selected = {
        cur <- input$lib_pick %||% ""
        if (nzchar(cur) && cur %in% unname(ch)) cur else ""
      }
    )
  }

  refresh_pbc_choices <- function() {
    cy <- input$cycle %||% ""
    ch <- pbc_choices(pbc_reg(), cycle_filter = if (nzchar(cy)) cy else NULL)
    updateSelectizeInput(
      session, "pbc_apply", choices = ch, server = TRUE,
      selected = intersect(input$pbc_apply %||% character(), unname(ch))
    )
    updateSelectizeInput(
      session, "interview_pbc_link", choices = ch, server = TRUE,
      selected = intersect(input$interview_pbc_link %||% character(), unname(ch))
    )
    updateSelectizeInput(
      session, "related_document_pbc", choices = ch, server = TRUE,
      selected = intersect(input$related_document_pbc %||% character(), unname(ch))
    )
  }

  interview_worksheet <- function() {
    cs <- interview_pool_controls()
    cs <- filter_controls_by_cycle_sub(
      cs,
      cycle = input$cycle %||% "",
      sub_key = input$interview_sub %||% ""
    )
    ids <- input$worksheet_controls
    if (length(ids) && any(nzchar(ids))) {
      cs <- Filter(function(c) c$control_id %in% ids, cs)
    }
    mods <- input$interview_5w1h %||% DEFAULT_INTERVIEW_5W1H
    pbc_ids <- input$interview_pbc_link %||% character()
    controls_to_interview(
      cs, input$interview_elements,
      finalized_only = FALSE,
      modules = mods,
      pbc_reg = pbc_reg(),
      pbc_ids = pbc_ids,
      include_module_rows = isTRUE(input$interview_include_modules %||% TRUE)
    )
  }

  output$interview_scaffold_preview <- renderUI({
    mods <- input$interview_5w1h %||% character()
    if (!length(mods)) {
      return(tags$div(class = "alert alert-warning py-1 mb-2 small", "尚未勾選 5W1H 模組。"))
    }
    sc <- interview_answer_scaffold(mods)
    n_pbc <- length(input$interview_pbc_link %||% character())
    tags$div(
      class = "small border rounded p-2 mb-2 bg-light",
      tags$strong("5W1H 拼湊預覽："), sc,
      if (n_pbc > 0) tags$div(class = "text-muted mt-1",
                              sprintf("已套用 PBC %d 筆（寫入 What／建議串接PBC）。", n_pbc))
    )
  })

  output$interview_guide_banner <- renderUI({
    cy <- input$cycle %||% ""
    if (!nzchar(cy)) {
      return(div(class = "alert alert-warning py-2 mb-2 small",
                 tags$strong("請先於側邊欄選擇循環。"),
                 "選定後即可直接選該循環建議之子作業。"))
    }
    rows <- library_controls_flat(cascade_source_library(lib()), cycle = cy)
    n_sub <- length(cascade_sub_process_choices(rows))
    sk <- input$interview_sub %||% ""
    if (!nzchar(sk)) {
      return(div(class = "alert alert-success py-1 mb-2 small",
                 sprintf("「%s」已載入 %d 個建議子作業，請直接選①。", cy, n_sub)))
    }
    scoped <- filter_controls_by_cycle_sub(
      interview_pool_controls(), cycle = cy, sub_key = sk
    )
    div(class = "alert alert-success py-1 mb-2 small",
        sprintf("已選子作業 → 建議控制點／風險 %d 筆（②可空＝全部）。", length(scoped)))
  })

  output$interview_live_box <- renderUI({
    iv <- interview_worksheet()
    mods <- input$interview_5w1h %||% character()
    tags$div(
      class = "mb-2",
      tags$div(class = "small text-muted",
               sprintf("題綱列數：%d｜5W1H 模組：%d｜PBC：%d",
                       nrow(iv), length(mods),
                       length(input$interview_pbc_link %||% character()))),
      tags$div(class = "small text-muted",
               "每題答案鏈：以何頻率 → 誰取得什麼文件或資訊(IUC) → 做什麼 → 下一步")
    )
  })

  output$interview_paragraph <- renderText({
    iv <- interview_worksheet()
    if (!nrow(iv)) return("（尚無訪談題綱；請先於側邊欄選循環，再選①子作業）")
    lines <- sprintf("%s. [%s] %s", iv[["題號"]], iv[["元素"]], iv[["訪談問題"]])
    paste(utils::head(lines, 12), collapse = "\n")
  })

  observeEvent(input$cycle, {
    refresh_pbc_choices()
  }, ignoreNULL = FALSE)

  # 一律以內建＋使用者庫候選為訪談來源（側邊欄循環→直接選子作業）
  interview_pool_controls <- reactive({
    library_items_as_interview_controls(cascade_source_library(lib()))
  })

  observe({
    cy <- input$cycle %||% ""
    if (!nzchar(cy)) {
      updateSelectInput(session, "interview_sub",
                        choices = c("① 請先於側邊欄選擇循環…" = ""), selected = "")
      return()
    }
    rows <- library_controls_flat(cascade_source_library(lib()), cycle = cy)
    ch_sub <- cascade_sub_process_choices(rows)
    label0 <- if (length(ch_sub)) {
      sprintf("① 選擇子作業…（本循環建議 %d）", length(ch_sub))
    } else {
      "① 選擇子作業…（本循環暫無建議）"
    }
    updateSelectInput(
      session, "interview_sub",
      choices = c(stats::setNames("", label0), ch_sub),
      selected = {
        cur <- input$interview_sub %||% ""
        if (nzchar(cur) && cur %in% unname(ch_sub)) cur else ""
      }
    )
  })

  observe({
    pool <- interview_pool_controls()
    scoped <- filter_controls_by_cycle_sub(
      pool,
      cycle = input$cycle %||% "",
      sub_key = input$interview_sub %||% ""
    )
    if (!length(scoped)) {
      updateSelectizeInput(session, "worksheet_controls", choices = character(), server = TRUE)
      return()
    }
    ch <- stats::setNames(
      vapply(scoped, function(x) x$control_id, ""),
      vapply(scoped, function(x) {
        sprintf("%s｜%s｜%s",
                x$control_id,
                x$risk_factor %||% x$risk_name %||% "",
                substr(x$control_objective %||% "", 1, 24))
      }, "")
    )
    updateSelectizeInput(
      session, "worksheet_controls", choices = ch, server = TRUE,
      selected = intersect(input$worksheet_controls %||% character(), unname(ch))
    )
  })

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

  observeEvent(input$pbc_apply_to_design, {
    ids <- input$pbc_apply
    if (!length(ids)) {
      return(showNotification("請先選擇要套用的 PBC 命名", type = "warning"))
    }
    updateTextAreaInput(session, "iuc", value = apply_pbc_to_iuc(pbc_reg(), ids))
    if (isTRUE(input$pbc_also_inputs)) {
      mapped <- format_pbc_for_inputs(pbc_reg(), ids)
      cur <- trimws(input$inputs %||% "")
      if (grepl("【IUC／PBC 命名對照】", cur, fixed = TRUE)) {
        cur <- trimws(sub("【IUC／PBC 命名對照】[\\s\\S]*$", "", cur))
      }
      new_inputs <- if (nzchar(cur)) paste(cur, mapped, sep = "\n") else mapped
      updateTextAreaInput(session, "inputs", value = new_inputs)
    }
    showNotification("已套用 PBC 命名至控制設計 IUC", type = "message")
    bslib::nav_select("main_nav", selected = "風險控制點設計", session = session)
  })

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
    if (!nrow(df)) df <- persist_params(force = TRUE)
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
    if (!require_admin(is_admin(), session)) return()
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
      "循環" = function() {
        updateSelectInput(session, "cycle", selected = val)
        updateTextInput(session, "cycle_code", value = cycle_code_for(val))
      },
      "子作業編號" = function() {
        updateTextInput(session, "sub_process_id", value = val)
        updateSelectInput(session, "cascade_sub", selected = "__custom__")
      },
      "子作業名稱" = function() {
        updateTextInput(session, "sub_process", value = val)
        updateSelectInput(session, "cascade_sub", selected = "__custom__")
      },
      "風險因素" = function() {
        updateTextInput(session, "risk_factor", value = val)
        updateSelectInput(session, "cascade_risk", selected = "__custom__")
      },
      "風險描述" = function() {
        updateTextAreaInput(session, "risk_description", value = val)
        updateSelectInput(session, "cascade_risk", selected = "__custom__")
      },
      "風險類別" = function() {
        updateSelectInput(session, "risk_category", selected = val)
        updateSelectInput(session, "cascade_risk", selected = "__custom__")
      },
      "RoMM 分類" = function() {
        updateSelectInput(session, "romm_classification", selected = val)
      },
      "聲明" = function() {
        updateSelectizeInput(session, "assertions", selected = val)
      },
      "會計科目" = function() {
        updateSelectizeInput(
          session, "significant_account",
          choices = account_select_choices(),
          selected = expand_account_selection(val)
        )
      },
      "控制目標" = function() {
        updateTextAreaInput(session, "control_objective", value = val)
        updateTextAreaInput(session, "custom_objective", value = val)
        updateSelectInput(session, "cascade_objective", selected = "__custom__")
      },
      "控制活動" = function() {
        updateTextAreaInput(session, "control_activity", value = val)
        updateTextAreaInput(session, "custom_activity", value = val)
        updateSelectInput(session, "cascade_activity", selected = "__custom__")
      },
      "控制類型" = function() {
        updateSelectInput(session, "nature", selected = val)
        updateSelectInput(session, "custom_nature", selected = val)
        updateSelectInput(session, "cascade_activity", selected = "__custom__")
      },
      "控制活動類型" = function() {
        updateSelectInput(session, "approach", selected = val)
        updateSelectInput(session, "custom_approach", selected = val)
        updateSelectInput(session, "cascade_activity", selected = "__custom__")
      },
      "控制頻率" = function() {
        updateSelectInput(session, "frequency", selected = val)
        updateSelectInput(session, "custom_frequency", selected = val)
        updateSelectInput(session, "cascade_activity", selected = "__custom__")
      },
      "流程負責單位" = function() {
        updateTextInput(session, "responsible_unit", value = val)
        updateTextInput(session, "custom_owner", value = val)
        updateSelectInput(session, "cascade_activity", selected = "__custom__")
      },
      "相關系統／IUC" = function() {
        # 舊參數庫鍵名；僅套用至 IUC（與相關系統分開）
        updateTextAreaInput(session, "iuc", value = val)
        updateTextInput(session, "custom_iuc", value = val)
        updateSelectInput(session, "cascade_iuc", selected = "__custom__")
      },
      "相關系統" = function() updateTextInput(session, "related_system", value = val),
      "IUC" = function() {
        updateTextAreaInput(session, "iuc", value = val)
        updateTextInput(session, "custom_iuc", value = val)
        updateSelectInput(session, "cascade_iuc", selected = "__custom__")
      },
      "相關法令" = function() updateSelectizeInput(session, "related_law", selected = val),
      "相關政策或程序" = function() updateTextInput(session, "related_policy", value = val),
      "相關文件" = function() {
        ids <- match_pbc_ids_from_text(pbc_reg(), val)
        if (length(ids)) {
          updateSelectizeInput(session, "related_document_pbc", selected = ids)
        } else {
          showNotification("相關文件須自 PBC 資料庫選取；請至 PBC 資料庫登錄後再選", type = "warning")
        }
      }
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
    if (!nzchar(id %||% "")) return(showNotification("請先選擇範本（或跳過此步驟）", type = "warning"))
    item <- get_library_item(lib(), id)
    if (is.null(item)) return()
    fill_inputs_from_ctrl(session, item$control, lib_items = lib(), pbc_registry = pbc_reg())
    bslib::nav_select("main_nav", selected = "風險控制點設計", session = session)
    showNotification(paste("已套用範本：", item$title), type = "message")
  })

  observeEvent(input$apply_lib_selected_row, {
    s <- input$lib_table_rows_selected
    items <- filter_library(lib(), cycle_filter = input$cycle, query = input$lib_query)
    if (is.null(s) || !length(items)) {
      return(showNotification("請先在表格選取一列範本", type = "warning"))
    }
    item <- items[[s[[1]]]]
    fill_inputs_from_ctrl(session, item$control, lib_items = lib(), pbc_registry = pbc_reg())
    bslib::nav_select("main_nav", selected = "風險控制點設計", session = session)
    showNotification(paste("已套用範本：", item$title), type = "message")
  })

  observeEvent(input$goto_lib_tab, {
    bslib::nav_select("main_nav", selected = "範本庫", session = session)
  })
  observeEvent(input$goto_param_tab, {
    bslib::nav_select("main_nav", selected = "參數庫", session = session)
  })
  observeEvent(input$goto_pbc_tab, {
    bslib::nav_select("main_nav", selected = "PBC資料庫", session = session)
  })

  observeEvent(input$lib_add_current, {
    if (!require_admin(is_admin(), session)) return()
    d <- current_draft_from_inputs()
    item <- add_ctrl_to_library(d, title = input$lib_title_override, tags = input$lib_tags, source = "form")
    showNotification(paste("已存入", item$library_id), type = "message")
  })
  observeEvent(input$lib_add_all_ready, {
    if (!require_admin(is_admin(), session)) return()
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
    if (!require_admin(is_admin(), session)) return()
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
    if (!require_admin(is_admin(), session)) return()
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
    if (!require_admin(is_admin(), session)) return()
    s <- input$lib_table_rows_selected
    if (is.null(s)) return(showNotification("請選取範本列", type = "warning"))
    df <- library_summary_df(filter_library(lib(), cycle_filter = input$cycle, query = input$lib_query))
    id <- df$library_id[s[[1]]]
    lib(persist_lib(delete_library_item(lib(), id)))
    refresh_lib_choices()
    showNotification("已刪除選取範本", type = "message")
  })

  observeEvent(input$admin_lib_load_row, {
    if (!require_admin(is_admin(), session)) return()
    s <- input$lib_table_rows_selected
    items <- filter_library(lib(), cycle_filter = input$cycle, query = input$lib_query)
    if (is.null(s) || !length(items)) {
      return(showNotification("請先選取範本列", type = "warning"))
    }
    item <- items[[s[[1]]]]
    ctrl <- item$control %||% list()
    updateTextInput(session, "admin_lib_id", value = item$library_id %||% "")
    updateTextInput(session, "admin_lib_title", value = item$title %||% "")
    updateTextInput(session, "admin_lib_tags",
                    value = paste(item$tags %||% character(), collapse = "；"))
    updateTextInput(session, "admin_lib_risk",
                    value = ctrl$risk_factor %||% ctrl$risk_name %||% "")
    updateTextAreaInput(session, "admin_lib_risk_desc", value = ctrl$risk_description %||% "")
    updateSelectInput(session, "admin_lib_risk_cat", selected = ctrl$risk_category %||% "")
    updateTextAreaInput(session, "admin_lib_objective", value = ctrl$control_objective %||% "")
    updateTextAreaInput(session, "admin_lib_activity", value = ctrl$control_activity %||% "")
    updateTextInput(session, "admin_lib_iuc",
                    value = ctrl$iuc %||% ctrl$iuc_or_system %||% "")
    updateTextInput(session, "admin_lib_system", value = ctrl$related_system %||% "")
  })

  observeEvent(input$admin_lib_save_fields, {
    if (!require_admin(is_admin(), session)) return()
    id <- trimws(input$admin_lib_id %||% "")
    if (!nzchar(id)) return(showNotification("請先載入選取列", type = "warning"))
    rf <- trimws(input$admin_lib_risk %||% "")
    patched <- patch_library_item_fields(
      lib(), id,
      title = input$admin_lib_title,
      tags = input$admin_lib_tags,
      fields = list(
        risk_factor = rf,
        risk_name = rf,
        risk_description = trimws(input$admin_lib_risk_desc %||% ""),
        risk_category = trimws(input$admin_lib_risk_cat %||% ""),
        control_objective = trimws(input$admin_lib_objective %||% ""),
        control_activity = trimws(input$admin_lib_activity %||% ""),
        iuc_or_system = trimws(input$admin_lib_iuc %||% ""),
        iuc = trimws(input$admin_lib_iuc %||% ""),
        related_system = trimws(input$admin_lib_system %||% "")
      )
    )
    lib(persist_lib(patched))
    refresh_lib_choices()
    showNotification(paste("已更新範本", id), type = "message")
  })

  observeEvent(input$admin_param_upsert, {
    if (!require_admin(is_admin(), session)) return()
    df <- upsert_parameter_row(
      param_store(),
      param = input$admin_param_name,
      value = input$admin_param_value,
      source = input$admin_param_source %||% "高權維護"
    )
    persist_params_df(df)
    showNotification("已寫入參數列", type = "message")
  })

  observeEvent(input$admin_param_delete, {
    if (!require_admin(is_admin(), session)) return()
    s <- input$param_table_rows_selected
    df_view <- param_catalog_df()
    if (!length(s) || !nrow(df_view)) {
      return(showNotification("請先選取參數列", type = "warning"))
    }
    row <- df_view[s[[1]], , drop = FALSE]
    store <- param_store()
    hit <- which(store$參數 == row$參數[[1]] & store$選項值 == row$選項值[[1]])
    if (!length(hit)) return(showNotification("找不到對應列", type = "warning"))
    persist_params_df(delete_parameter_rows(store, hit))
    showNotification("已刪除參數列", type = "message")
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
    rf_tag <- risk_factor_tag(sel$risk_factor)
    nature <- normalize_control_type_manual_auto(sel$nature)
    approach <- normalize_control_activity_type_pd(sel$approach)
    # 風險類別決定屬性種類（三擇一）；細節來自風險辨識
    kind <- risk_attr_kind_from_category(sel$risk_category)
    if (!nzchar(kind)) kind <- "operations"
    attr_ctrl <- enforce_single_risk_attr(
      list(risk_category = sel$risk_category),
      kind = kind,
      detail = input$risk_attr_detail %||% ""
    )
    list(
      control_id = input$control_id %||% "",
      company = input$company %||% "",
      cycle = sel$cycle,
      cycle_code = {
        cc <- trimws(input$cycle_code %||% "")
        if (nzchar(cc)) cc else cycle_code_for(sel$cycle)
      },
      sub_process_id = {
        sp <- trimws(input$sub_process_id %||% "")
        if (nzchar(sp)) sp else sel$sub_process_id
      },
      sub_process = {
        spn <- trimws(input$sub_process %||% "")
        if (nzchar(spn)) spn else sel$sub_process
      },
      risk_factor = rf_tag,
      risk_name = rf_tag,
      risk_description = sel$risk_description,
      risk_category = attr_ctrl$risk_category,
      risk_attr_financial = attr_ctrl$risk_attr_financial,
      risk_attr_operations = attr_ctrl$risk_attr_operations,
      risk_attr_compliance = attr_ctrl$risk_attr_compliance,
      romm_classification = input$romm_classification %||% "",
      significant_account = join_significant_accounts(input$significant_account),
      assertions = paste(input$assertions %||% character(), collapse = "；"),
      control_objective = {
        o <- trimws(input$control_objective %||% "")
        if (nzchar(o)) o else sel$control_objective
      },
      control_activity = {
        a <- trimws(input$control_activity %||% "")
        if (nzchar(a)) a else sel$control_activity
      },
      frequency = resolve_control_frequency(nature, {
        f <- trimws(input$frequency %||% "")
        if (nzchar(f)) f else sel$frequency
      }),
      responsible_unit = {
        u <- trimws(input$responsible_unit %||% "")
        if (nzchar(u)) u else sel$responsible_unit
      },
      iuc_or_system = {
        i <- trimws(input$iuc %||% "")
        if (nzchar(i)) i else sel$iuc_or_system
      },
      iuc = {
        i <- trimws(input$iuc %||% "")
        if (nzchar(i)) i else sel$iuc_or_system
      },
      related_system = trimws(input$related_system %||% ""),
      related_policy = input$related_policy %||% "",
      related_law = {
        v <- input$related_law %||% character(0)
        paste(unique(trimws(as.character(v))), collapse = "；")
      },
      related_document_pbc_ids = input$related_document_pbc %||% character(),
      related_document = {
        ids <- input$related_document_pbc %||% character()
        if (length(ids)) apply_pbc_to_related_document(pbc_reg(), ids) else ""
      },
      nature = {
        n <- normalize_control_type_manual_auto(input$nature)
        if (nzchar(n)) n else nature
      },
      approach = {
        a <- normalize_control_activity_type_pd(input$approach)
        if (nzchar(a)) a else approach
      },
      control_type = {
        n <- normalize_control_type_manual_auto(input$nature)
        if (nzchar(n)) n else nature
      },
      control_activity_type = {
        a <- normalize_control_activity_type_pd(input$approach)
        if (nzchar(a)) a else approach
      },
      type = input$type %||% "",
      inputs = input$inputs %||% "",
      review_steps = input$review_steps %||% "",
      outputs = {
        out <- trimws(input$outputs %||% "")
        if (nzchar(out)) out else {
          ids <- input$related_document_pbc %||% character()
          if (length(ids)) apply_pbc_to_related_document(pbc_reg(), ids) else ""
        }
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
  # 候選永遠含內建種子（九大循環可直接選），毋須先匯入底稿
  cascade_rows <- reactive({
    cy <- input$cycle %||% ""
    if (!nzchar(cy)) return(list())
    library_controls_flat(cascade_source_library(lib()), cycle = cy)
  })

  resolve_cascade_selection <- function() {
    sub_key <- input$cascade_sub %||% ""
    # 基本資料為子作業編號／名稱的來源；引導選取會回填這些欄位
    sp_id <- trimws(input$sub_process_id %||% "")
    sp_name <- trimws(input$sub_process %||% "")
    if (!identical(sub_key, "__custom__") && nzchar(sub_key) &&
        (!nzchar(sp_id) || !nzchar(sp_name))) {
      sp <- parse_sub_process_key(sub_key)
      if (!nzchar(sp_id)) sp_id <- sp$id
      if (!nzchar(sp_name)) sp_name <- sp$name
    }

    # 風險辨識為風險欄位來源；引導選取會回填這些欄位
    risk_factor <- trimws(input$risk_factor %||% "")
    risk_desc <- trimws(input$risk_description %||% "")
    risk_cat <- trimws(input$risk_category %||% "")
    rk <- input$cascade_risk %||% ""
    if (!identical(rk, "__custom__") && nzchar(rk) &&
        (!nzchar(risk_factor) || !nzchar(risk_desc) || !nzchar(risk_cat))) {
      det <- cascade_risk_detail(
        filter_cascade_rows(
          cascade_rows(),
          sub_key = if (!identical(sub_key, "__custom__") && nzchar(sub_key)) sub_key else NULL
        ),
        rk
      )
      if (!nzchar(risk_factor)) risk_factor <- risk_factor_tag(rk)
      if (!nzchar(risk_desc)) risk_desc <- det$risk_description %||% ""
      if (!nzchar(risk_cat)) risk_cat <- det$risk_category %||% ""
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

    # 控制設計 accordion 為可覆寫來源（引導選取會回填）
    form_obj <- trimws(input$control_objective %||% "")
    form_act <- trimws(input$control_activity %||% "")
    form_approach <- normalize_control_activity_type_pd(input$approach)
    form_nature <- normalize_control_type_manual_auto(input$nature)
    form_freq <- trimws(input$frequency %||% "")
    form_owner <- trimws(input$responsible_unit %||% "")
    form_iuc <- trimws(input$iuc %||% "")
    form_sys <- trimws(input$related_system %||% "")
    if (nzchar(form_obj)) objective <- form_obj
    if (nzchar(form_act)) activity <- form_act
    if (nzchar(form_approach)) approach <- form_approach
    if (nzchar(form_nature)) nature <- form_nature
    if (nzchar(form_freq)) frequency <- form_freq
    if (nzchar(form_owner)) owner <- form_owner
    if (nzchar(form_iuc)) iuc <- form_iuc

    list(
      cycle = input$cycle %||% "",
      cycle_code = trimws(input$cycle_code %||% ""),
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
      iuc = iuc,
      related_system = form_sys
    )
  }

  # 會計科目：僅報導面可填且必填；其他類別鎖定並清空
  output$significant_account_hint <- renderUI({
    cat <- trimws(input$risk_category %||% resolve_cascade_selection()$risk_category %||% "")
    if (is_reporting_risk_category(cat)) {
      div(class = "alert alert-info py-1 mb-2 small",
          lab_req("報導面"), " — 會計科目為必填；可複選常見財務報表科目，或按「全部適用」。")
    } else if (nzchar(cat)) {
      div(class = "alert alert-secondary py-1 mb-2 small",
          "非報導面：會計科目已鎖定不可填（將自動清空）。")
    } else {
      helpText(class = "text-muted small", "請先選擇風險類別；僅報導面可填會計科目。")
    }
  })

  # 「全部適用」：按鈕或選項 → 勾選全部常見科目
  observeEvent(input$account_select_all, {
    cat <- trimws(input$risk_category %||% resolve_cascade_selection()$risk_category %||% "")
    if (!is_reporting_risk_category(cat)) {
      return(showNotification("僅報導面可選會計科目", type = "warning"))
    }
    updateSelectizeInput(
      session, "significant_account",
      choices = account_select_choices(),
      selected = expand_account_selection(ACCOUNT_ALL_OPTION)
    )
  })
  observeEvent(input$significant_account, {
    cat <- trimws(input$risk_category %||% "")
    if (!is_reporting_risk_category(cat)) return()
    sel <- parse_account_values(input$significant_account)
    if (!(ACCOUNT_ALL_OPTION %in% sel)) return()
    # 選到「全部適用」時展開為全科目（避免只留標籤卻漏存）
    desired <- expand_account_selection(ACCOUNT_ALL_OPTION)
    if (!setequal(sel, desired)) {
      updateSelectizeInput(
        session, "significant_account",
        choices = account_select_choices(),
        selected = desired
      )
    }
  }, ignoreInit = TRUE)

  output$related_document_hint <- renderUI({
    cat <- trimws(input$risk_category %||% resolve_cascade_selection()$risk_category %||% "")
    nature <- normalize_control_type_manual_auto(input$nature %||% "")
    mode <- related_document_mode_for_ctrl(list(
      nature = nature, risk_category = cat
    ))
    if (identical(mode, "required")) {
      div(class = "alert alert-info py-1 mb-2 small",
          lab_req("人工控制"), " — 相關文件須自 ",
          tags$strong("PBC 資料庫"), " 選取（可多選）；無資料請先至 ",
          tags$strong("PBC 資料庫"), " 登錄。")
    } else if (identical(mode, "locked")) {
      reason <- c(
        if (is_automatic_control(nature)) "自動控制",
        if (is_compliance_risk_category(cat)) "遵循面風險"
      )
      div(class = "alert alert-secondary py-1 mb-2 small",
          paste0("無法設定相關文件（", paste(reason, collapse = "／"), "）。"))
    } else {
      helpText(class = "text-muted small",
               "請先選控制類型與風險類別；人工且非法遵面時，須自 PBC 資料庫選取相關文件。")
    }
  })

  output$related_system_label <- renderUI({
    nature <- normalize_control_type_manual_auto(input$nature %||% "")
    if (is_automatic_control(nature)) lab_req("相關系統") else lab_opt("相關系統")
  })

  output$related_system_hint <- renderUI({
    nature <- normalize_control_type_manual_auto(input$nature %||% "")
    mode <- related_system_mode_for_ctrl(list(nature = nature))
    if (identical(mode, "required")) {
      div(class = "alert alert-info py-1 mb-2 small",
          lab_req("自動控制"), " — 相關系統為必填（IT／應用系統名稱，與 IUC 不同）。")
    } else if (identical(mode, "optional")) {
      helpText(class = "text-muted small",
               "人工控制：相關系統為選填（例：ERP、AD）。")
    } else {
      helpText(class = "text-muted small", "請先選控制類型；自動控制時相關系統必填。")
    }
  })

  output$related_law_hint <- renderUI({
    cat <- trimws(input$risk_category %||% resolve_cascade_selection()$risk_category %||% "")
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

  output$assertions_hint <- renderUI({
    cat <- trimws(input$risk_category %||% resolve_cascade_selection()$risk_category %||% "")
    mode <- assertion_mode_for_category(cat)
    if (identical(mode, "reporting")) {
      div(class = "alert alert-info py-1 mb-2 small",
          "報導面：可複選八種 Assertions（Existence or Occurrence、Completeness、",
          "Rights and Obligations、Valuation or Allocation、Accuracy、Cutoff、",
          "Classification、Presentation）。")
    } else if (identical(mode, "operations")) {
      div(class = "alert alert-info py-1 mb-2 small",
          "營運面：聲明限縮為三種可複選——完整性、正確性、即時性。")
    } else if (identical(mode, "locked")) {
      div(class = "alert alert-secondary py-1 mb-2 small",
          "遵循面：無 Assertions 可選（已鎖定並清空）。")
    } else {
      helpText(class = "text-muted small", "請先於風險辨識選擇風險類別，以決定聲明可選範圍。")
    }
  })

  # 側邊欄循環名稱 → 自動帶入循環編號（可覆寫）
  observeEvent(input$cycle, {
    cy <- input$cycle %||% ""
    code <- cycle_code_for(cy)
    cur <- trimws(input$cycle_code %||% "")
    # 空值或仍為對照表內既有代碼時才覆寫，避免蓋掉使用者自訂編號
    known <- unname(CYCLE_CODE_MAP)
    if (!nzchar(cur) || cur %in% known) {
      updateTextInput(session, "cycle_code", value = code)
    }
  }, ignoreInit = TRUE)

  # 引導選子作業 → 回填基本資料子作業編號／名稱
  observeEvent(input$cascade_sub, {
    sub_key <- input$cascade_sub %||% ""
    if (!nzchar(sub_key) || identical(sub_key, "__custom__")) return()
    sp <- parse_sub_process_key(sub_key)
    updateTextInput(session, "sub_process_id", value = sp$id)
    updateTextInput(session, "sub_process", value = sp$name)
  }, ignoreInit = TRUE)

  # 引導選風險 → 回填風險辨識（因素／描述／類別／RoMM／屬性細節）
  observeEvent(input$cascade_risk, {
    rk <- input$cascade_risk %||% ""
    if (!nzchar(rk) || identical(rk, "__custom__")) return()
    rows <- cascade_rows()
    sub_key <- input$cascade_sub %||% ""
    if (nzchar(sub_key) && !identical(sub_key, "__custom__")) {
      rows <- filter_cascade_rows(rows, sub_key = sub_key)
    }
    apply_risk_detail_to_inputs(session, rows, rk)
  }, ignoreInit = TRUE)

  # 引導選目標 → 回填控制設計「控制目標」
  observeEvent(input$cascade_objective, {
    obj_sel <- input$cascade_objective %||% ""
    if (!nzchar(obj_sel) || identical(obj_sel, "__custom__")) return()
    updateTextAreaInput(session, "control_objective", value = obj_sel)
  }, ignoreInit = TRUE)

  # 引導選活動 → 回填控制設計活動／類型／頻率／負責單位
  observeEvent(input$cascade_activity, {
    act_sel <- input$cascade_activity %||% ""
    if (!nzchar(act_sel) || identical(act_sel, "__custom__")) return()
    ak <- parse_activity_key(act_sel)
    updateTextAreaInput(session, "control_activity", value = ak$activity %||% "")
    ap <- normalize_control_activity_type_pd(ak$approach)
    if (nzchar(ap)) updateSelectInput(session, "approach", selected = ap)
    # 自匹配列補齊類型／頻率／負責單位
    rows <- cascade_rows()
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
    rows <- filter_cascade_rows(rows, activity_key_sel = act_sel)
    if (length(rows)) {
      m <- rows[[1]]
      nt <- normalize_control_type_manual_auto(m$nature)
      if (nzchar(nt)) updateSelectInput(session, "nature", selected = nt)
      freq <- resolve_control_frequency(nt, m$frequency %||% "")
      if (nzchar(freq)) updateSelectInput(session, "frequency", selected = freq)
      if (nzchar(trimws(m$responsible_unit %||% ""))) {
        updateTextInput(session, "responsible_unit", value = m$responsible_unit)
      }
    }
  }, ignoreInit = TRUE)

  # 引導選 IUC → 回填控制設計 IUC
  observeEvent(input$cascade_iuc, {
    iuc_sel <- input$cascade_iuc %||% ""
    if (!nzchar(iuc_sel) || identical(iuc_sel, "__custom__")) return()
    updateTextAreaInput(session, "iuc", value = iuc_sel)
  }, ignoreInit = TRUE)

  # 自訂引導欄位 → 同步至控制設計
  observeEvent(input$custom_objective, {
    if (!identical(input$cascade_objective, "__custom__")) return()
    updateTextAreaInput(session, "control_objective",
                        value = trimws(input$custom_objective %||% ""))
  }, ignoreInit = TRUE)
  observeEvent(input$custom_activity, {
    if (!identical(input$cascade_activity, "__custom__")) return()
    updateTextAreaInput(session, "control_activity",
                        value = trimws(input$custom_activity %||% ""))
  }, ignoreInit = TRUE)
  observeEvent(input$custom_approach, {
    if (!identical(input$cascade_activity, "__custom__")) return()
    ap <- normalize_single_activity_type(input$custom_approach)
    if (nzchar(ap)) updateSelectInput(session, "approach", selected = ap)
  }, ignoreInit = TRUE)
  observeEvent(input$custom_nature, {
    if (!identical(input$cascade_activity, "__custom__")) return()
    nt <- normalize_control_type_manual_auto(input$custom_nature)
    if (nzchar(nt)) updateSelectInput(session, "nature", selected = nt)
    if (identical(nt, "自動")) {
      updateSelectInput(session, "custom_frequency", selected = "持續")
      updateSelectInput(session, "frequency", selected = "持續")
    }
  }, ignoreInit = TRUE)
  observeEvent(input$custom_frequency, {
    if (!identical(input$cascade_activity, "__custom__")) return()
    fr <- trimws(input$custom_frequency %||% "")
    if (nzchar(fr)) updateSelectInput(session, "frequency", selected = fr)
  }, ignoreInit = TRUE)
  observeEvent(input$custom_owner, {
    if (!identical(input$cascade_activity, "__custom__")) return()
    updateTextInput(session, "responsible_unit",
                    value = trimws(input$custom_owner %||% ""))
  }, ignoreInit = TRUE)
  observeEvent(input$custom_iuc, {
    if (!identical(input$cascade_iuc, "__custom__")) return()
    updateTextAreaInput(session, "iuc",
                        value = trimws(input$custom_iuc %||% ""))
  }, ignoreInit = TRUE)

  observeEvent(input$nature, {
    if (identical(input$nature, "自動")) {
      updateSelectInput(session, "frequency", selected = "持續")
      session$sendCustomMessage("toggleFrequency", list(enabled = FALSE))
      if (length(input$related_document_pbc %||% character())) {
        updateSelectizeInput(session, "related_document_pbc", selected = character(0))
      }
    } else {
      session$sendCustomMessage("toggleFrequency", list(enabled = TRUE))
    }
  }, ignoreNULL = FALSE)

  # 引導完成且未手動填編號 → 自動順編；風險類別驅動會計科目／法令／聲明鎖定
  observe({
    sel <- resolve_cascade_selection()
    cat <- trimws(input$risk_category %||% sel$risk_category %||% "")
    session$sendCustomMessage(
      "toggleAccount",
      list(enabled = is_reporting_risk_category(cat))
    )
    session$sendCustomMessage(
      "toggleLaw",
      list(enabled = is_compliance_risk_category(cat))
    )
    as_mode <- assertion_mode_for_category(cat)
    as_choices <- assertion_choices_for_category(cat)
    cur_as <- parse_assertion_values(input$assertions)
    keep_as <- if (length(as_choices)) intersect(cur_as, as_choices) else character(0)
    updateSelectizeInput(
      session, "assertions",
      choices = as_choices,
      selected = keep_as,
      options = list(
        create = FALSE,
        placeholder = switch(
          as_mode,
          reporting = "報導面：可複選八種 Assertions",
          operations = "營運面：完整性／正確性／即時性",
          locked = "遵循面：無 Assertions 可選",
          "請先選擇風險類別"
        )
      )
    )
    session$sendCustomMessage(
      "toggleAssertions",
      list(enabled = identical(as_mode, "reporting") || identical(as_mode, "operations"))
    )
    doc_mode <- related_document_mode_for_ctrl(list(
      nature = input$nature,
      control_type = input$nature,
      risk_category = cat
    ))
    session$sendCustomMessage(
      "toggleRelatedDocument",
      list(enabled = identical(doc_mode, "required"))
    )
    if (identical(doc_mode, "locked")) {
      if (length(input$related_document_pbc %||% character())) {
        updateSelectizeInput(session, "related_document_pbc", selected = character(0))
      }
    }
    if (nzchar(cat) && !is_reporting_risk_category(cat)) {
      if (length(parse_account_values(input$significant_account))) {
        updateSelectizeInput(session, "significant_account", selected = character(0))
      }
    }
    if (nzchar(cat) && !is_compliance_risk_category(cat)) {
      if (length(input$related_law)) {
        updateSelectizeInput(session, "related_law", selected = character(0))
      }
    }
    ready <- cascade_selection_ready(sel)
    spid <- trimws(input$sub_process_id %||% "")
    if (!nzchar(spid)) spid <- sel$sub_process_id
    if (isTRUE(ready$ready) && nzchar(spid) &&
        !nzchar(trimws(input$control_id %||% ""))) {
      ids <- collect_existing_control_ids(lists = list(lib(), controls()))
      updateTextInput(session, "control_id", value = next_rcm_control_id(spid, ids))
    }
  })

  observe({
    cy <- input$cycle %||% ""
    rows <- cascade_rows()
    if (!nzchar(cy)) {
      updateSelectInput(session, "cascade_sub",
                        choices = c("① 請先於側邊欄選擇循環…" = ""), selected = "")
      return()
    }
    ch_sub <- cascade_sub_process_choices(rows)
    n_rows <- length(rows)
    label0 <- if (n_rows) {
      sprintf("① 選擇子作業…（本循環 %d 筆候選）", n_rows)
    } else {
      "① 選擇子作業…（可直接「＋自訂新增」，毋須匯入底稿）"
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
                        choices = c("② 請先於側邊欄選擇循環…" = ""), selected = "")
      return()
    }
    rows <- cascade_rows()
    sub_key <- input$cascade_sub %||% ""
    if (nzchar(sub_key) && !identical(sub_key, "__custom__")) {
      rows <- filter_cascade_rows(rows, sub_key = sub_key)
      ch_risk <- cascade_risk_choices(rows)
      label0 <- sprintf("② 選擇風險因素…（本子作業 %d）", length(ch_risk))
    } else if (identical(sub_key, "__custom__")) {
      ch_risk <- character()
      label0 <- "② 自訂子作業下請自訂風險或稍後套用"
    } else {
      ch_risk <- cascade_risk_choices(rows)
      label0 <- sprintf("② 選擇風險因素…（本循環 %d）", length(ch_risk))
    }
    ch <- c(stats::setNames("", label0), ch_risk, "＋自訂新增風險" = "__custom__")
    cur <- input$cascade_risk %||% ""
    updateSelectInput(session, "cascade_risk", choices = ch,
                      selected = if (cur %in% unname(ch)) cur else "")
  })

  output$cascade_step_status <- renderUI({
    rows <- cascade_rows()
    sub_key <- input$cascade_sub %||% ""
    rk <- input$cascade_risk %||% ""
    if (!nzchar(rk)) {
      updateSelectInput(session, "cascade_objective",
                        choices = c("③ 請先選擇②風險…" = ""), selected = "")
      return()
    }
    if (nzchar(sub_key) && !identical(sub_key, "__custom__")) {
      rows <- filter_cascade_rows(rows, sub_key = sub_key)
    }
    if (!identical(rk, "__custom__")) {
      rows <- filter_cascade_rows(rows, risk_factor = rk)
    }
    ch_obj <- cascade_objective_choices(rows)
    ch <- c(stats::setNames("", sprintf("③ 選擇控制目標…（%d）", length(ch_obj))),
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
                        choices = c("④ 請先選擇③控制目標…" = ""), selected = "")
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
    ch <- c(stats::setNames("", sprintf("④ 選擇控制活動…（%d）", length(ch_act))),
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
                        choices = c("⑤ 請先選擇④控制活動…" = ""), selected = "")
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
    ch <- c(stats::setNames("", sprintf("⑤ 選擇 IUC…（%d）", length(ch_iuc))),
            ch_iuc, "＋自訂新增 IUC" = "__custom__")
    cur <- input$cascade_iuc %||% ""
    updateSelectInput(session, "cascade_iuc", choices = ch,
                      selected = if (cur %in% unname(ch)) cur else "")
  })

  output$cascade_step_status <- renderUI({
    sel <- resolve_cascade_selection()
    ready <- cascade_selection_ready(sel)
    steps <- c(
      sprintf("循環（側邊欄）：%s", if (nzchar(sel$cycle)) "✓" else "○"),
      sprintf("①子作業：%s", if (nzchar(sel$sub_process_id) || nzchar(sel$sub_process)) "✓" else "○"),
      sprintf("②風險：%s", if (nzchar(sel$risk_factor)) "✓" else "○"),
      sprintf("③目標：%s", if (nzchar(sel$control_objective)) "✓" else "○"),
      sprintf("④活動：%s", if (nzchar(sel$control_activity) && activity_type_ok(sel$approach)) "✓" else "○"),
      sprintf("⑤IUC：%s", if (nzchar(sel$iuc_or_system)) "✓" else "○")
    )
    cls <- if (isTRUE(ready$ready)) "alert alert-success py-1 mb-2 small" else "alert alert-secondary py-1 mb-2 small"
    div(class = cls, paste(steps, collapse = " ｜ "),
        if (!ready$ready) tags$span(class = "text-muted", " — 完成引導後即可定稿"))
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
    if (!require_admin(is_admin(), session)) return()
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

  observeEvent(input$oa_swap, {
    o <- input$control_objective %||% ""
    a <- input$control_activity %||% ""
    updateTextAreaInput(session, "control_objective", value = a)
    updateTextAreaInput(session, "control_activity", value = o)
    if (identical(input$cascade_objective, "__custom__")) {
      updateTextAreaInput(session, "custom_objective", value = a)
    }
    if (identical(input$cascade_activity, "__custom__")) {
      updateTextAreaInput(session, "custom_activity", value = o)
    }
  })

  observeEvent(input$oa_split_suggest, {
    blob <- paste(c(input$control_objective %||% "", input$control_activity %||% ""),
                  collapse = "。")
    if (!nzchar(trimws(blob))) {
      d <- current_draft_from_inputs()
      blob <- paste(c(d$control_objective, d$control_activity), collapse = "。")
    }
    sug <- suggest_objective_activity_split(blob)
    updateTextAreaInput(session, "control_objective", value = sug$objective)
    updateTextAreaInput(session, "control_activity", value = sug$activity)
    if (identical(input$cascade_objective, "__custom__")) {
      updateTextAreaInput(session, "custom_objective", value = sug$objective)
    }
    if (identical(input$cascade_activity, "__custom__")) {
      updateTextAreaInput(session, "custom_activity", value = sug$activity)
    }
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
    rcm_revision()
    parity <- assert_design_rcm_parity(controls())
    tags$p(class = "small mb-2",
           sprintf("目前已定稿 %d 個控制點＝%d 列 RCM", parity$n_controls, parity$n_rcm_rows))
  })
  output$rcm_latest_saved <- renderUI({
    rcm_revision()
    pt <- last_saved_control()
    if (is.null(pt)) {
      return(tags$p(class = "small text-muted mb-2", "尚無儲存成功的控制點。"))
    }
    row <- tryCatch(control_to_rcm_row(pt, seq_no = 1L), error = function(e) NULL)
    tags$div(
      class = "alert alert-success py-2 mb-2 small",
      tags$strong("最新儲存："),
      tags$code(pt$control_id %||% "—"),
      if (nzchar(pt$saved_at %||% "")) {
        tags$span(class = "text-muted ms-2", paste0("（", pt$saved_at, "）"))
      },
      tags$br(),
      tags$span(
        "風險：", pt$risk_factor %||% pt$risk_name %||% "—", "｜",
        "目標：", substr(pt$control_objective %||% "—", 1, 36), "｜",
        "活動：", substr(pt$control_activity %||% "—", 1, 36)
      ),
      tags$br(),
      tags$span(
        class = "text-muted",
        "類型：", pt$nature %||% "—", "／", pt$approach %||% "—",
        "｜頻率：", pt$frequency %||% "—",
        if (nzchar(pt$related_system %||% "")) paste0("｜系統：", pt$related_system) else "",
        if (nzchar(pt$iuc %||% pt$iuc_or_system %||% "")) {
          paste0("｜IUC：", substr(pt$iuc %||% pt$iuc_or_system %||% "", 1, 28))
        } else "",
        if (nzchar(pt$related_document %||% "")) {
          paste0("｜文件：", substr(pt$related_document, 1, 28))
        } else ""
      ),
      if (!is.null(row) && nrow(row)) {
        tags$div(class = "text-muted mt-1",
                 "RCM 列已同步｜子作業：", row[["子作業名稱"]] %||% "—",
                 "｜檢核：", substr(as.character(row[["設計檢核"]] %||% ""), 1, 40))
      }
    )
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
    bump_rcm_views(pt)
    bslib::nav_select("main_nav", selected = "RCM", session = session)
    updateTextInput(session, "control_id",
                    value = next_rcm_control_id(
                      pt$sub_process_id,
                      collect_existing_control_ids(lists = list(lib(), controls()))
                    ))
    if (isTRUE(input$auto_collect_lib) && isTRUE(is_admin())) {
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
      IUC = vapply(cs, function(x) x$iuc %||% x$iuc_or_system %||% "", ""),
      相關系統 = vapply(cs, function(x) x$related_system %||% "", ""),
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
        cycle = input$cycle %||% "", notes = input$pbc_notes
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
    df <- rcm_display_df()
    if (is.null(df) || !nrow(df)) {
      return(datatable(
        data.frame(訊息 = "尚無已定稿控制點；完成設計並「寫入 RCM 一列」後即時顯示於此。"),
        rownames = FALSE, options = list(dom = "t")
      ))
    }
    dt <- datatable(
      df, rownames = FALSE,
      options = list(
        scrollX = TRUE, pageLength = 15, dom = "tip",
        order = list(list(0, "desc")),
        rowCallback = DT::JS(
          "function(row, data, index) {",
          "  if (index === 0) $(row).css({'background-color': '#e8f5e9', 'font-weight': '500'});",
          "}"
        )
      )
    )
    dt
  })

  observeEvent(rcm_revision(), {
    df <- rcm_display_df()
    if (is.null(df) || !nrow(df)) return()
    proxy <- DT::dataTableProxy("rcm_table", session = session)
    DT::replaceData(proxy, df, resetPaging = FALSE, rownames = FALSE)
  }, ignoreInit = TRUE)
  selected_worksheet_controls_sa <- reactive({
    cs <- Filter(is_control_finalized_for_rcm, controls())
    if (!length(cs)) return(list())
    ids <- input$worksheet_controls_sa
    if (!length(ids) || all(!nzchar(ids))) return(cs)
    Filter(function(c) c$control_id %in% ids, cs)
  })
  csa_edit_ctrl <- reactive({
    cs <- Filter(is_control_finalized_for_rcm, controls())
    if (!length(cs)) return(NULL)
    id <- input$csa_edit_control
    if (!nzchar(id %||% "")) return(cs[[1]])
    hit <- Filter(function(c) identical(c$control_id, id), cs)
    if (length(hit)) hit[[1]] else cs[[1]]
  })
  fill_csa_scenario_form <- function(sc) {
    updateTextInput(session, "csa_scenario_name", value = sc$scenario_name %||% "")
    updateTextAreaInput(session, "csa_scenario_status", value = sc$company_status %||% "")
    if (nzchar(trimws(sc$type %||% ""))) {
      updateSelectizeInput(session, "type", selected = sc$type)
    }
    updateTextAreaInput(session, "inputs", value = sc$inputs %||% "")
    updateTextAreaInput(session, "review_steps", value = sc$review_steps %||% "")
    updateTextAreaInput(session, "outputs", value = sc$outputs %||% "")
    updateTextAreaInput(session, "investigation_threshold",
                        value = sc$investigation_threshold %||% "")
  }
  observe({
    cs <- controls()
    if (!length(cs)) {
      updateSelectizeInput(session, "worksheet_controls_sa", choices = character(), server = TRUE)
      updateSelectizeInput(session, "csa_edit_control", choices = character(), server = TRUE)
      updateSelectizeInput(session, "csa_scenario_pick", choices = character(), server = TRUE)
      return()
    }
    cs_fin <- Filter(is_control_finalized_for_rcm, cs)
    if (!length(cs_fin)) {
      updateSelectizeInput(session, "worksheet_controls_sa", choices = character(), server = TRUE)
      updateSelectizeInput(session, "csa_edit_control", choices = character(), server = TRUE)
      updateSelectizeInput(session, "csa_scenario_pick", choices = character(), server = TRUE)
    } else {
      ch_fin <- stats::setNames(
        vapply(cs_fin, function(x) x$control_id, ""),
        vapply(cs_fin, function(x) {
          plan <- control_test_sample_plan(x)
          n_sc <- length(control_csa_scenarios(x))
          sprintf("%s｜%s｜%s→%s｜%d組",
                  x$control_id,
                  x$risk_name %||% "",
                  plan$frequency,
                  plan$sample_size_label,
                  n_sc)
        }, "")
      )
      updateSelectizeInput(session, "worksheet_controls_sa", choices = ch_fin, server = TRUE)
      updateSelectizeInput(session, "csa_edit_control", choices = ch_fin, server = TRUE)
    }
  })
  observe({
    ctrl <- csa_edit_ctrl()
    if (is.null(ctrl)) {
      updateSelectizeInput(session, "csa_scenario_pick", choices = character(), server = TRUE)
      return()
    }
    ch <- csa_scenario_choices(ctrl)
    cur <- input$csa_scenario_pick
    sel <- if (nzchar(cur %||% "") && cur %in% unname(ch)) cur else unname(ch)[[1]]
    updateSelectizeInput(session, "csa_scenario_pick", choices = ch, selected = sel, server = TRUE)
  })
  observeEvent(list(input$csa_edit_control, input$csa_scenario_pick), {
    ctrl <- csa_edit_ctrl()
    if (is.null(ctrl)) return()
    scs <- control_csa_scenarios(ctrl)
    sid <- input$csa_scenario_pick
    hit <- Filter(function(x) identical(x$scenario_id, sid), scs)
    sc <- if (length(hit)) hit[[1]] else scs[[1]]
    fill_csa_scenario_form(sc)
  }, ignoreInit = FALSE)
  read_csa_scenario_from_inputs <- function(scenario_id = NULL) {
    new_csa_scenario(
      scenario_name = input$csa_scenario_name %||% "",
      company_status = input$csa_scenario_status %||% "",
      type = input$type %||% "",
      inputs = input$inputs %||% "",
      review_steps = input$review_steps %||% "",
      outputs = input$outputs %||% "",
      investigation_threshold = input$investigation_threshold %||% "",
      scenario_id = scenario_id
    )
  }
  patch_control_in_store <- function(ctrl) {
    cs <- controls()
    idx <- which(vapply(cs, function(x) identical(x$control_id, ctrl$control_id), logical(1)))
    if (!length(idx)) return(FALSE)
    cs[[idx[[1]]]] <- ctrl
    controls(cs)
    TRUE
  }
  observeEvent(input$csa_scenario_save, {
    ctrl <- csa_edit_ctrl()
    if (is.null(ctrl)) {
      return(showNotification("請先選擇已定版控制點", type = "warning"))
    }
    sid <- input$csa_scenario_pick
    if (!nzchar(sid %||% "") || identical(sid, "S1")) {
      # First explicit save: if only synthetic, create stored S1
      if (!is.list(ctrl$csa_scenarios) || !length(ctrl$csa_scenarios)) {
        sid <- "S1"
      }
    }
    sc <- read_csa_scenario_from_inputs(scenario_id = sid %||% "S1")
    if (!nzchar(trimws(sc$scenario_name))) {
      return(showNotification("請填寫控制現況情境名稱", type = "warning"))
    }
    ctrl2 <- upsert_control_csa_scenario(ctrl, sc)
    patch_control_in_store(ctrl2)
    showNotification(sprintf("已儲存情境組「%s」", sc$scenario_name), type = "message")
  })
  observeEvent(input$csa_scenario_add, {
    ctrl <- csa_edit_ctrl()
    if (is.null(ctrl)) {
      return(showNotification("請先選擇已定版控制點", type = "warning"))
    }
    # Persist current form first if named
    cur_name <- trimws(input$csa_scenario_name %||% "")
    if (nzchar(cur_name) && nzchar(input$csa_scenario_pick %||% "")) {
      ctrl <- upsert_control_csa_scenario(
        ctrl, read_csa_scenario_from_inputs(scenario_id = input$csa_scenario_pick)
      )
    } else if (!is.list(ctrl$csa_scenarios) || !length(ctrl$csa_scenarios)) {
      # Seed default before adding second
      ctrl <- upsert_control_csa_scenario(ctrl, synthetic_default_csa_scenario(ctrl))
    }
    n <- length(ctrl$csa_scenarios %||% list()) + 1L
    sc_new <- new_csa_scenario(
      scenario_name = sprintf("現況情境 %d", n),
      company_status = "",
      type = ctrl$type %||% "",
      inputs = "",
      review_steps = "",
      outputs = "",
      investigation_threshold = ""
    )
    ctrl2 <- upsert_control_csa_scenario(ctrl, sc_new)
    patch_control_in_store(ctrl2)
    updateSelectizeInput(session, "csa_scenario_pick",
                         choices = csa_scenario_choices(ctrl2),
                         selected = sc_new$scenario_id, server = TRUE)
    fill_csa_scenario_form(sc_new)
    showNotification(sprintf("已新增情境組「%s」", sc_new$scenario_name), type = "message")
  })
  observeEvent(input$csa_scenario_del, {
    ctrl <- csa_edit_ctrl()
    if (is.null(ctrl)) return(showNotification("請先選擇已定版控制點", type = "warning"))
    sid <- input$csa_scenario_pick
    if (!nzchar(sid %||% "")) return()
    if (!is.list(ctrl$csa_scenarios) || !length(ctrl$csa_scenarios)) {
      return(showNotification("目前僅有預設情境組，無需刪除", type = "warning"))
    }
    if (length(ctrl$csa_scenarios) <= 1L) {
      return(showNotification("至少保留一組情境", type = "warning"))
    }
    ctrl2 <- remove_control_csa_scenario(ctrl, sid)
    patch_control_in_store(ctrl2)
    ch <- csa_scenario_choices(ctrl2)
    updateSelectizeInput(session, "csa_scenario_pick", choices = ch,
                         selected = unname(ch)[[1]], server = TRUE)
    showNotification("已刪除情境組", type = "message")
  })
  observeEvent(input$ws_select_core_iv, {
    updateCheckboxGroupInput(session, "interview_elements", selected = DEFAULT_INTERVIEW_ELEMENTS)
    updateCheckboxGroupInput(session, "interview_5w1h", selected = DEFAULT_INTERVIEW_5W1H)
  })
  observeEvent(input$ws_select_full_iv, {
    updateCheckboxGroupInput(
      session, "interview_elements",
      selected = unique(c(DEFAULT_INTERVIEW_ELEMENTS, INTERVIEW_WALKTHROUGH_EXTRA))
    )
    updateCheckboxGroupInput(session, "interview_5w1h", selected = DEFAULT_INTERVIEW_5W1H)
  })
  observeEvent(input$ws_reset_iv, {
    updateSelectInput(session, "interview_sub", selected = "")
    updateSelectizeInput(session, "worksheet_controls", selected = character())
    updateSelectizeInput(session, "interview_pbc_link", selected = character())
    updateCheckboxGroupInput(session, "interview_elements", selected = DEFAULT_INTERVIEW_ELEMENTS)
    updateCheckboxGroupInput(session, "interview_5w1h", selected = DEFAULT_INTERVIEW_5W1H)
    updateCheckboxInput(session, "interview_include_modules", value = TRUE)
  })
  observeEvent(input$ws_select_core_csa, {
    updateCheckboxGroupInput(session, "csa_elements", selected = DEFAULT_CSA_ELEMENTS)
  })
  output$interview_status <- renderUI({
    pool <- interview_pool_controls()
    scoped <- filter_controls_by_cycle_sub(
      pool,
      cycle = input$cycle %||% "",
      sub_key = input$interview_sub %||% ""
    )
    iv <- interview_worksheet()
    steps <- c(
      sprintf("循環（側邊欄）：%s", if (nzchar(input$cycle %||% "")) "✓" else "○"),
      sprintf("①子作業：%s", if (nzchar(input$interview_sub %||% "")) "✓" else "○"),
      sprintf("②風險／控制點：%s", if (length(input$worksheet_controls)) "✓" else "○（全部）")
    )
    if (!nzchar(input$cycle %||% "")) {
      return(tagList(
        tags$small(class = "text-muted", paste(steps, collapse = "｜")),
        tags$br(),
        tags$small(class = "text-warning", "請先於側邊欄選擇循環，即可直接選該循環建議之子作業。")
      ))
    }
    if (!nzchar(input$interview_sub %||% "")) {
      return(tagList(
        tags$small(class = "text-muted", paste(steps, collapse = "｜")),
        tags$br(),
        tags$small(class = "text-muted", "請選①子作業（內建建議已就緒）。")
      ))
    }
    if (!length(scoped)) {
      return(tagList(
        tags$small(class = "text-muted", paste(steps, collapse = "｜")),
        tags$br(),
        tags$small(class = "text-warning",
                   "此子作業尚無建議列；可改選其他子作業，或至「風險控制點設計」新增後再訪談。")
      ))
    }
    tagList(
      tags$small(class = "text-muted", paste(steps, collapse = "｜")),
      tags$br(),
      tags$small(
        class = "text-muted",
        sprintf("%d 點 → 訪談問項 %d 則｜人事時地物回答架構", length(scoped), nrow(iv))
      )
    )
  })
  output$csa_status <- renderUI({
    cs <- selected_worksheet_controls_sa()
    csa <- controls_to_csa(cs, input$csa_elements, finalized_only = TRUE)
    if (!length(cs)) {
      return(tags$small(class = "text-warning",
                        "尚無已定版風險控制點；請先於「風險控制點設計」完成設計並寫入 RCM。"))
    }
    n_sc <- sum(vapply(cs, function(x) length(control_csa_scenarios(x)), integer(1)))
    plans <- lapply(cs, control_test_sample_plan)
    summary_bits <- vapply(seq_along(cs), function(i) {
      sprintf("%s：%s→%s（%d組）",
              cs[[i]]$control_id %||% "—",
              plans[[i]]$frequency,
              plans[[i]]$sample_size_label,
              length(control_csa_scenarios(cs[[i]])))
    }, character(1))
    tagList(
      tags$small(
        class = "text-muted",
        sprintf("已定版 %d 點／情境組 %d → 測試步驟 %d 列", length(cs), n_sc, nrow(csa))
      ),
      tags$br(),
      tags$small(class = "text-muted", paste(summary_bits, collapse = "；"))
    )
  })
  output$interview_table <- renderDT({
    datatable(interview_worksheet(),
              rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10, dom = "tip"))
  })
  output$csa_table <- renderDT({
    datatable(controls_to_csa(selected_worksheet_controls_sa(), input$csa_elements,
                              finalized_only = TRUE),
              rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10, dom = "tip"))
  })
  output$gap_table <- renderDT({
    rcm_revision()
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
      write.csv(interview_worksheet(),
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
