# Goddamn SOX — compact UI
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

source(file.path(root, "R", "00_constants.R"), local = TRUE)
source(file.path(root, "R", "assemble.R"), local = TRUE)
source(file.path(root, "R", "objective_activity.R"), local = TRUE)
source(file.path(root, "R", "pbc_registry.R"), local = TRUE)
source(file.path(root, "R", "rcm.R"), local = TRUE)
source(file.path(root, "R", "csa.R"), local = TRUE)
source(file.path(root, "R", "judgment_crawler.R"), local = TRUE)
source(file.path(root, "R", "judgment_rules.R"), local = TRUE)
source(file.path(root, "R", "library.R"), local = TRUE)
source(file.path(root, "R", "cascade.R"), local = TRUE)
source(file.path(root, "R", "parameter_store.R"), local = TRUE)
source(file.path(root, "R", "privilege.R"), local = TRUE)
source(file.path(root, "R", "button_interactions.R"), local = TRUE)
source(file.path(root, "R", "table_schemas.R"), local = TRUE)
source(file.path(root, "R", "data_persist.R"), local = TRUE)
source(file.path(root, "R", "app_cache.R"), local = TRUE)

# UI label with required asterisk
lab_req <- function(txt) {
  tagList(txt, tags$span("*", class = "text-danger ms-1", title = "設計必填"))
}
lab_opt <- function(txt) {
  tagList(txt, tags$span(class = "text-muted small ms-1", "選填"))
}

# DataTables 共用選項（不顯示表格「載入中」文字；全頁忙碌以左下角 spinner 表示）
dt_loading_opts <- function(pageLength = 10, scrollX = TRUE, dom = "tip",
                            ordering = NULL, emptyTable = "無資料", ...) {
  opts <- list(
    pageLength = pageLength,
    scrollX = scrollX,
    dom = dom,
    processing = FALSE,
    language = list(
      emptyTable = emptyTable
    )
  )
  if (!is.null(ordering)) opts$ordering <- ordering
  extra <- list(...)
  if (length(extra)) opts <- utils::modifyList(opts, extra)
  opts
}

with_loading <- function(expr, message = "載入中...") {
  force(expr)
}

fill_inputs_from_ctrl <- function(session, ctrl, lib_items = NULL, pbc_registry = NULL,
                                  current_cycle = NULL) {
  if (is.null(ctrl)) return()
  apply_ctrl_to_cascade(session, ctrl, current_cycle = current_cycle)
  apply_supplement_from_ctrl(session, ctrl, pbc_registry = pbc_registry)
}

ui <- page_navbar(
  id = "main_nav",
  title = "Goddamn SOX",
  window_title = "Goddamn SOX",
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
    function setInputFieldEnabled(id, enabled, opts) {
      opts = opts || {};
      var el = document.getElementById(id);
      if (!el) return;
      var on = !!enabled;
      var $el = window.jQuery ? jQuery('#' + id) : null;
      if ($el && $el.length && $el[0].selectize) {
        var s = $el[0].selectize;
        var $ctrl = $el.closest('.selectize-control');
        if (on) {
          s.enable();
          $ctrl.removeClass('input-locked');
        } else {
          s.disable();
          if (opts.clear !== false) s.clear();
          $ctrl.addClass('input-locked');
        }
        return;
      }
      el.disabled = !on;
      if (opts.readOnly !== false) el.readOnly = !on;
      el.classList.toggle('input-locked', !on);
      if (!on && opts.clearValue !== undefined) {
        el.value = '';
        try { Shiny.setInputValue(id, opts.clearValue, {priority: 'event'}); } catch (e) {}
      }
    }
    Shiny.addCustomMessageHandler('toggleAccount', function(msg) {
      setInputFieldEnabled('significant_account', msg.enabled);
    });
    Shiny.addCustomMessageHandler('toggleLaw', function(msg) {
      setInputFieldEnabled('related_law', msg.enabled);
      setInputFieldEnabled('related_law_url', msg.enabled, {clearValue: ''});
    });
    Shiny.addCustomMessageHandler('toggleAssertions', function(msg) {
      setInputFieldEnabled('assertions', msg.enabled);
    });
    Shiny.addCustomMessageHandler('toggleFrequency', function(msg) {
      setInputFieldEnabled('frequency', msg.enabled, {readOnly: false});
    });
    Shiny.addCustomMessageHandler('toggleButton', function(msg) {
      var el = document.getElementById(msg.id);
      if (!el) return;
      var on = !!msg.enabled;
      el.disabled = !on;
      el.classList.toggle('disabled', !on);
      el.setAttribute('aria-disabled', on ? 'false' : 'true');
      if (msg.title) el.setAttribute('title', msg.title);
      else el.removeAttribute('title');
    });
    Shiny.addCustomMessageHandler('toggleRelatedDocument', function(msg) {
      setInputFieldEnabled('related_document_pbc', msg.enabled);
    });
    Shiny.addCustomMessageHandler('toggleIuc', function(msg) {
      setInputFieldEnabled('iuc', msg.enabled);
    });
    // 建議選單：選項已在 selectize.options 時，聚焦／點擊強制 refreshOptions 以寫入下拉 DOM
    (function() {
      var cascadeSelectIds = ['sub_process', 'risk_factor', 'risk_description',
                              'control_objective', 'control_activity'];
      function wireCascadeSelectMenu(id) {
        var el = document.getElementById(id);
        if (!el || !el.selectize || el.selectize.__cascadeMenuWired) return;
        var s = el.selectize;
        s.__cascadeMenuWired = true;
        var openRendered = function(ev) {
          try {
            if (ev) { ev.preventDefault(); ev.stopPropagation(); }
            var keys = Object.keys(s.options || {});
            if (!keys.length) return;
            if (!s.$dropdown_content.children('[data-selectable]').length) {
              var snapshot = keys.map(function(k) { return s.options[k]; });
              s.clearOptions();
              s.addOption(snapshot);
            }
            s.refreshOptions(false);
            s.open();
          } catch (err) {}
        };
        s.$control.off('mousedown.cascadeMenu click.cascadeMenu')
          .on('mousedown.cascadeMenu click.cascadeMenu', openRendered);
        s.$control_input.off('focus.cascadeMenu')
          .on('focus.cascadeMenu', function() { openRendered(); });
      }
      function wireAllCascadeMenus() {
        cascadeSelectIds.forEach(wireCascadeSelectMenu);
      }
      $(document).on('shiny:value shiny:connected shiny:idle', function() {
        setTimeout(wireAllCascadeMenus, 30);
      });
      setInterval(wireAllCascadeMenus, 800);
    })();
    // 左下角 spinner：首次連線、伺服器忙碌、斷線重連
    (function() {
      function ensureCornerSpinner() {
        if (document.getElementById('app-corner-spinner')) return;
        var el = document.createElement('div');
        el.id = 'app-corner-spinner';
        el.setAttribute('aria-hidden', 'true');
        el.innerHTML = '<div class=\"app-spinner-ring\"></div>';
        document.body.appendChild(el);
      }
      function showBootSpinner() {
        ensureCornerSpinner();
        document.getElementById('app-corner-spinner').classList.add('app-spinner-boot');
      }
      function hideBootSpinner() {
        var el = document.getElementById('app-corner-spinner');
        if (el) el.classList.remove('app-spinner-boot');
      }
      if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', ensureCornerSpinner);
      } else {
        ensureCornerSpinner();
      }
      showBootSpinner();
      $(document).on('shiny:connected', hideBootSpinner);
      $(document).on('shiny:disconnected', showBootSpinner);
    })();
    // 預覽抽屜內 DataTable 須等 collapse 展開後再渲染（避免 Ajax error tn/7）
    (function() {
      var collapseInputMap = {
        interviewPreviewCollapse: 'interview_preview_open',
        designPreviewCollapse: 'design_preview_open',
        csaPreviewCollapse: 'csa_preview_open'
      };
      function notifyCollapseOpen(id) {
        var inputId = collapseInputMap[id];
        if (!inputId || !window.Shiny) return;
        Shiny.setInputValue(inputId, Date.now(), {priority: 'event'});
        setTimeout(function() {
          if (!window.jQuery) return;
          jQuery('#' + id).find('table.dataTable').each(function() {
            try {
              if (jQuery.fn.dataTable && jQuery.fn.dataTable.isDataTable(this)) {
                jQuery(this).DataTable().columns.adjust();
              }
            } catch (err) {}
          });
        }, 50);
      }
      document.addEventListener('shown.bs.collapse', function(ev) {
        if (ev.target && ev.target.id) notifyCollapseOpen(ev.target.id);
      });
    })();
    // 選單已選項目：雙擊即可拉回輸入框修改（create=true 時寫入新值；儲存後入參數庫）
    (function() {
      function selectizeFromItem($item) {
        var $control = $item.closest('.selectize-control');
        if (!$control.length) return null;
        var $el = $control.siblings('select.selectized, input.selectized').first();
        if (!$el.length) {
          $el = $control.parent().find('select.selectized, input.selectized').first();
        }
        return ($el[0] && $el[0].selectize) ? $el[0].selectize : null;
      }
      function itemLabel(s, value, $item) {
        var opt = s.options[value];
        if (opt) {
          if (opt.text) return String(opt.text);
          if (opt.label) return String(opt.label);
        }
        var t = ($item.clone().children().remove().end().text() || '').trim();
        return t || String(value);
      }
      $(document).on('dblclick.editSelectizeItem', '.rcm-design-tabs .selectize-control .item', function(e) {
        e.preventDefault();
        e.stopPropagation();
        var $item = $(this);
        var s = selectizeFromItem($item);
        if (!s || s.isDisabled) return;
        var value = $item.attr('data-value');
        if (value === undefined || value === null) return;
        var text = itemLabel(s, value, $item);
        try {
          s.removeItem(value, true);
          s.setTextboxValue(text);
          s.focus();
          if (typeof s.open === 'function') s.open();
        } catch (err) {}
      });
    })();
  ")),
    tags$style(HTML(paste0("
      :root { --brand-blue: ", BRAND_BLUE, "; --brand-green: ", BRAND_GREEN, "; --brand-black: ", BRAND_BLACK, "; --brand-white: ", BRAND_WHITE, "; --brand-gray: ", BRAND_GRAY, "; --input-placeholder: #ADB5BD; }
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
      .btn.disabled, .btn:disabled { opacity: 0.55; cursor: not-allowed; pointer-events: auto; }
      .accordion-button:not(.collapsed) { background-color: rgba(134,188,37,0.12); color: var(--brand-blue); box-shadow: inset 0 -1px 0 var(--brand-green); }
      .accordion-button:focus { box-shadow: 0 0 0 0.2rem rgba(134,188,37,0.25); }
      .form-control:focus, .form-select:focus { border-color: var(--brand-green); box-shadow: 0 0 0 0.2rem rgba(134,188,37,0.2); }
      /* 所有輸入框預設說明字：統一為「公司名稱」placeholder 色 (#ADB5BD) */
      .form-control, .form-select, textarea.form-control,
      input.form-control, input[type=\"text\"], input[type=\"search\"],
      input[type=\"password\"], input[type=\"number\"], input[type=\"email\"] {
        color: var(--brand-black) !important;
      }
      .form-control:placeholder-shown,
      textarea.form-control:placeholder-shown,
      input.form-control:placeholder-shown,
      input[type=\"text\"]:placeholder-shown,
      input[type=\"search\"]:placeholder-shown,
      input[type=\"password\"]:placeholder-shown,
      input[type=\"number\"]:placeholder-shown,
      input[type=\"email\"]:placeholder-shown {
        color: var(--input-placeholder) !important;
      }
      .form-select:has(option[value=\"\"]:checked),
      select.form-select:has(option[value=\"\"]:checked),
      select.shiny-input-select:has(option[value=\"\"]:checked) {
        color: var(--input-placeholder) !important;
      }
      .form-control::placeholder, .form-select::placeholder,
      textarea.form-control::placeholder,
      input::placeholder,
      .selectize-input input::placeholder {
        color: var(--input-placeholder) !important;
        opacity: 1 !important;
      }
      .selectize-input.has-items,
      .selectize-input.has-items .item,
      .selectize-input.has-items input {
        color: var(--brand-black) !important;
      }
      .selectize-input:not(.has-items),
      .selectize-input:not(.has-items) input,
      .selectize-control.single .selectize-input:not(.has-items),
      .selectize-control.single .selectize-input:not(.has-items) input {
        color: var(--input-placeholder) !important;
      }
      /* selectize 空選時顯示的 placeholder 文字節點 */
      .selectize-input .item[data-value=\"\"] {
        color: var(--input-placeholder) !important;
      }
      /* 條件未達成而鎖定：輸入欄灰底（含 selectize／原生 disabled） */
      .form-control:disabled,
      .form-select:disabled,
      select.form-control:disabled,
      textarea.form-control:disabled,
      .form-control.input-locked,
      .form-select.input-locked,
      .selectize-control.disabled .selectize-input,
      .selectize-control.input-locked .selectize-input {
        background-color: var(--brand-gray) !important;
        cursor: not-allowed;
        opacity: 1;
      }
      .selectize-dropdown .option { color: #6B7280; }
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
      /* DataTables 預覽表：允許左右拖曳／捲動（覆寫上方全頁不裁切規則） */
      .pbc-table-scroll-wrap,
      .rcm-table-scroll-wrap {
        width: 100%;
        max-width: 100%;
        overflow-x: auto;
        -webkit-overflow-scrolling: touch;
      }
      .pbc-table-scroll-wrap .dataTables_wrapper,
      .pbc-table-scroll-wrap .dataTables_scroll,
      .pbc-table-scroll-wrap .dataTables_scrollHead,
      .pbc-table-scroll-wrap .dataTables_scrollBody,
      .rcm-table-scroll-wrap .dataTables_wrapper,
      .rcm-table-scroll-wrap .dataTables_scroll,
      .rcm-table-scroll-wrap .dataTables_scrollHead,
      .rcm-table-scroll-wrap .dataTables_scrollBody {
        overflow-x: auto !important;
        max-width: 100%;
      }
      .pbc-table-scroll-wrap table.dataTable,
      .rcm-table-scroll-wrap table.dataTable {
        width: max-content !important;
        min-width: 100%;
      }
      .pbc-db-card-header .card-header,
      .pbc-db-card > .card-header {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 0.75rem;
        flex-wrap: wrap;
      }
      .pbc-db-card-header .card-header .pbc-db-card-title,
      .pbc-db-card > .card-header .pbc-db-card-title {
        flex: 1 1 auto;
        min-width: 0;
      }
      .pbc-db-card-header .card-header .pbc-export-btn,
      .pbc-db-card > .card-header .pbc-export-btn {
        flex: 0 0 auto;
        margin-left: auto;
      }
      .pbc-db-card-header .card-header .pbc-export-btn .btn,
      .pbc-db-card > .card-header .pbc-export-btn .btn {
        white-space: nowrap;
      }
      .pbc-status-footer {
        padding: 0.75rem 0 0.25rem;
        border-top: 1px solid rgba(0, 91, 170, 0.12);
      }
      .pbc-status-footer .shiny-text-output pre {
        white-space: pre-wrap;
        font-size: 0.82rem;
        margin-bottom: 0;
        background: transparent;
        border: 0;
        padding: 0;
      }
      .button-guide-card .button-guide-table {
        font-size: 0.78rem;
      }
      .button-guide-card .button-guide-table th {
        background: rgba(0, 91, 170, 0.06);
        white-space: nowrap;
      }
      .button-guide-card .table-responsive-wrap,
      .table-schema-card .table-responsive-wrap {
        overflow-x: auto;
        max-width: 100%;
      }
      .table-schema-card .table-schema-table {
        font-size: 0.78rem;
      }
      .table-schema-card .table-schema-table th {
        background: rgba(0, 91, 170, 0.06);
        white-space: nowrap;
      }
      .shiny-text-output pre, .shiny-plot-output, .shiny-image-output {
        overflow: visible !important; max-height: none !important;
      }
      .bslib-sidebar-layout > .main { overflow-x: hidden; overflow-y: auto; }
      /* 子作業 selectize 下拉勿被 tab-pane／card 裁切 */
      .rcm-design-tabs, .rcm-design-tabs .tab-content, .rcm-design-tabs .tab-pane,
      .rcm-design-tabs .card, .rcm-design-tabs .card-body {
        overflow: visible !important;
      }
      .rcm-design-tabs .selectize-dropdown {
        z-index: 2000 !important;
      }
      .rcm-design-tabs .selectize-control .item {
        cursor: text;
      }
      .rcm-design-tabs .selectize-control .item:hover {
        outline: 1px dashed rgba(0, 91, 170, 0.35);
      }
      /* 相關法規｜法規連結：左 1 : 右 2 */
      .related-law-row {
        display: grid;
        grid-template-columns: minmax(0, 1fr) minmax(0, 2fr);
        gap: 0.75rem 1rem;
        align-items: start;
        margin-bottom: 0.35rem;
      }
      .related-law-row .shiny-input-container { margin-bottom: 0; width: 100%; }
      @media (max-width: 768px) {
        .related-law-row { grid-template-columns: 1fr; }
      }

      /* PBC：客戶原名 → 檢視後新命名（1:1，中間右箭頭） */
      .pbc-name-map-row {
        display: grid;
        grid-template-columns: minmax(0, 1fr) auto minmax(0, 1fr);
        gap: 0.35rem 0.75rem;
        align-items: end;
        margin-bottom: 0.5rem;
      }
      .pbc-name-map-row .shiny-input-container { margin-bottom: 0; width: 100%; }
      .pbc-name-map-arrow {
        display: flex;
        align-items: center;
        justify-content: center;
        padding-bottom: 0.45rem;
        font-size: 1.35rem;
        line-height: 1;
        color: var(--brand-blue, #005baa);
        font-weight: 700;
        user-select: none;
      }
      @media (max-width: 768px) {
        .pbc-name-map-row {
          grid-template-columns: 1fr;
          gap: 0.35rem;
        }
        .pbc-name-map-arrow {
          padding: 0.15rem 0;
          transform: rotate(90deg);
        }
      }

      /* PBC：證據類型｜樣本檔案格式 同列並排 */
      .pbc-kind-format-row {
        display: grid;
        grid-template-columns: minmax(0, 1fr) minmax(0, 1fr);
        gap: 0.75rem 1rem;
        align-items: start;
        margin-bottom: 0.35rem;
      }
      .pbc-kind-format-row .shiny-input-container { margin-bottom: 0; width: 100%; }
      @media (max-width: 768px) {
        .pbc-kind-format-row { grid-template-columns: 1fr; }
      }

      /* PBC：規格說明｜如果存在 同列並排 */
      .pbc-spec-row {
        display: grid;
        grid-template-columns: minmax(0, 1fr) auto;
        gap: 0.5rem 1rem;
        align-items: start;
        margin-bottom: 0.35rem;
      }
      .pbc-spec-row .shiny-input-container { margin-bottom: 0; width: 100%; }
      .pbc-spec-if-exists {
        padding-top: 1.85rem;
        white-space: nowrap;
      }
      .pbc-spec-if-exists .checkbox { margin-bottom: 0; }
      @media (max-width: 768px) {
        .pbc-spec-row { grid-template-columns: 1fr; }
        .pbc-spec-if-exists { padding-top: 0; }
      }

      /* PBC：ID｜互相勾稽 同列 1:2 並排 */
      .pbc-id-related-row {
        display: grid;
        grid-template-columns: minmax(0, 1fr) minmax(0, 2fr);
        gap: 0.75rem 1rem;
        align-items: start;
        margin-bottom: 0.35rem;
      }
      .pbc-id-related-row .shiny-input-container { margin-bottom: 0; width: 100%; }
      @media (max-width: 768px) {
        .pbc-id-related-row { grid-template-columns: 1fr; }
      }

      /* 風險面向｜風險範疇 同列 1:1 並排 */
      .risk-principle-area-row {
        display: grid;
        grid-template-columns: minmax(0, 1fr) minmax(0, 1fr);
        gap: 0.75rem 1rem;
        align-items: start;
        margin-bottom: 0.35rem;
      }
      .risk-principle-area-row .shiny-input-container { margin-bottom: 0; width: 100%; }
      @media (max-width: 768px) {
        .risk-principle-area-row { grid-template-columns: 1fr; }
      }

      /* 風險因素｜風險類別 同列 2:1 並排 */
      .risk-factor-category-row {
        display: grid;
        grid-template-columns: minmax(0, 2fr) minmax(0, 1fr);
        gap: 0.75rem 1rem;
        align-items: start;
        margin-bottom: 0.35rem;
      }
      .risk-factor-category-row .shiny-input-container { margin-bottom: 0; width: 100%; }
      @media (max-width: 768px) {
        .risk-factor-category-row { grid-template-columns: 1fr; }
      }

      /* 訪談引導：風險因素｜控制點 同列 1:1 並排 */
      .interview-risk-control-row {
        display: grid;
        grid-template-columns: minmax(0, 1fr) minmax(0, 1fr);
        gap: 0.75rem 1rem;
        align-items: start;
        margin-bottom: 0.35rem;
      }
      .interview-risk-control-row .shiny-input-container { margin-bottom: 0; width: 100%; }
      @media (max-width: 768px) {
        .interview-risk-control-row { grid-template-columns: 1fr; }
      }
      .interview-5w1h-fields .form-group { margin-bottom: 0.75rem; }
      .interview-5w1h-fields textarea.form-control { min-height: 3rem; }
      .judgment-result-url-row textarea.form-control,
      .interview-5w1h-fields textarea.form-control {
        background-color: var(--brand-gray);
      }
      .interview-5w1h-fields textarea.form-control:placeholder-shown {
        color: var(--input-placeholder) !important;
      }
      .interview-5w1h-combined { margin-top: 0.25rem; }
      .interview-5w1h-combined .control-label {
        font-weight: 600;
        margin-bottom: 0.35rem;
      }
      .interview-5w1h-combined-box {
        background-color: var(--brand-gray);
        border: 1px solid #C8C8C8;
        border-radius: 0.375rem;
        padding: 0.5rem 0.75rem;
        min-height: 3rem;
        line-height: 1.5;
        white-space: pre-wrap;
        word-break: break-word;
        color: var(--brand-black);
      }

      .judgment-search-table {
        width: 100%;
        table-layout: fixed;
      }
      .judgment-search-table th {
        width: 5.75rem;
        min-width: 5.75rem;
        white-space: nowrap;
        vertical-align: middle;
        padding-right: 0.75rem;
        font-weight: 600;
        line-height: 1.35;
      }
      .judgment-search-table td {
        vertical-align: middle;
      }
      .judgment-search-table .shiny-input-container {
        margin-bottom: 0;
        width: 100%;
        max-width: 100%;
      }
      .judgment-search-table .form-control,
      .judgment-search-table .input-group {
        width: 100%;
        max-width: 100%;
      }
      .judgment-case-no-row {
        display: grid;
        grid-template-columns: minmax(4.5rem, 0.75fr) minmax(5.5rem, 1.25fr) minmax(4.5rem, 0.75fr) minmax(4.5rem, 0.75fr);
        gap: 0.5rem 0.75rem;
        align-items: end;
      }
      .judgment-period-row {
        display: grid;
        grid-template-columns: minmax(11rem, 1fr) auto minmax(11rem, 1fr);
        gap: 0.5rem 0.75rem;
        align-items: end;
      }
      .judgment-period-row .shiny-input-container { margin-bottom: 0; width: 100%; }
      .judgment-period-row .form-control { width: 100%; max-width: 100%; }
      .judgment-period-sep {
        align-self: center;
        white-space: nowrap;
        padding: 0 0.15rem 0.45rem;
        line-height: 1;
      }
      .judgment-kb-row {
        display: grid;
        grid-template-columns: minmax(5rem, 7rem) minmax(5rem, 7rem);
        gap: 0.75rem;
        align-items: end;
      }
      .judgment-history-import-row {
        align-items: center;
      }
      .judgment-history-import-row .shiny-input-container {
        margin-bottom: 0;
        width: 100%;
      }
      .judgment-history-import-row .form-group {
        margin-bottom: 0;
        display: flex;
        flex-wrap: nowrap;
        align-items: center;
        gap: 0.5rem 0.75rem;
      }
      .judgment-history-import-row .control-label {
        margin-bottom: 0;
        white-space: nowrap;
        flex: 0 0 auto;
        font-weight: 600;
        line-height: 1.35;
      }
      .judgment-history-import-row .input-group {
        flex: 1 1 auto;
        min-width: 0;
      }
      .judgment-history-import-row .input-group .form-control,
      .judgment-history-import-row .input-group .btn,
      .judgment-history-import-row .input-group .input-group-text {
        padding-top: 0.25rem;
        padding-bottom: 0.25rem;
        line-height: 1.35;
        min-height: 0;
      }
      @media (max-width: 768px) {
        .judgment-search-table th {
          width: 4.5rem;
          min-width: 4.5rem;
        }
        .judgment-case-no-row,
        .judgment-period-row {
          grid-template-columns: 1fr;
        }
        .judgment-period-sep {
          padding: 0;
          text-align: center;
        }
      }

      /* 控制目標與聲明設定並排：等高、桌面版維持雙欄 */
      .objective-assertions-row.bslib-grid {
        display: grid !important;
        grid-template-columns: minmax(0, 1fr) minmax(0, 1fr);
        gap: 1rem;
        align-items: start;
      }
      .objective-assertions-row .shiny-input-container { margin-bottom: 0.35rem; }
      #control_objective-selectized + .selectize-control .selectize-input,
      #control_activity-selectized + .selectize-control .selectize-input,
      #risk_description-selectized + .selectize-control .selectize-input { min-height: 2.5rem; }
      #control_activity-selectized + .selectize-control .selectize-input,
      #risk_description-selectized + .selectize-control .selectize-input { min-height: 5.5rem; }
      .objective-assertions-row .selectize-control { min-height: 2.5rem; }
      .assertions-side .alert { margin-bottom: 0.35rem; }
      /* 控制方式／性質／頻率：桌面三欄並排；窄螢幕改單欄 */
      .control-attr-row {
        display: grid;
        grid-template-columns: repeat(3, minmax(0, 1fr));
        gap: 0.75rem 1rem;
        align-items: start;
        margin-bottom: 0.35rem;
      }
      .control-attr-row .shiny-input-container { margin-bottom: 0; width: 100%; }
      @media (max-width: 768px) {
        .control-attr-row { grid-template-columns: 1fr; }
      }
      .design-stage-save-bar {
        display: flex;
        justify-content: flex-end;
        align-items: center;
        margin: 0 0 0.65rem 0;
        min-height: 2rem;
      }
      .design-stage-save-bar .btn { white-space: nowrap; }
      /* 各設計頁籤頂部簡約搜尋 */
      .design-tab-filter-bar {
        margin-bottom: 0.85rem;
        padding: 0.55rem 0.75rem;
        background: rgba(0, 91, 170, 0.04);
        border: 1px solid rgba(0, 91, 170, 0.12);
        border-radius: 0.35rem;
      }
      .design-tab-filter-bar .filter-title {
        font-size: 0.78rem;
        font-weight: 700;
        color: var(--brand-blue);
        margin-bottom: 0.35rem;
      }
      .design-tab-filter-hits {
        margin-top: 0.35rem;
        max-height: 9rem;
        overflow-y: auto;
      }
      .design-tab-filter-hits .btn-link {
        display: block;
        text-align: left;
        white-space: normal;
        padding: 0.15rem 0;
        font-size: 0.82rem;
        line-height: 1.35;
        text-decoration: none;
      }
      .design-tab-filter-hits .btn-link:hover { text-decoration: underline; }
      /* 風險控制點設計：三階段分頁籤 */
      .rcm-design-tabs { margin-bottom: 0.5rem; }
      .rcm-design-tabs > .nav-tabs { border-bottom: 2px solid var(--brand-green); }
      .rcm-design-tabs > .nav-tabs .nav-link {
        color: var(--brand-blue);
        font-weight: 600;
        border: none;
        border-bottom: 3px solid transparent;
        padding: 0.55rem 1rem;
      }
      .rcm-design-tabs > .nav-tabs .nav-link:hover {
        border-color: rgba(134,188,37,0.45);
        color: var(--brand-blue);
      }
      .rcm-design-tabs > .nav-tabs .nav-link.active {
        color: var(--brand-blue);
        background: rgba(134,188,37,0.12);
        border-bottom-color: var(--brand-green);
      }
      .rcm-design-tabs > .tab-content { padding-top: 0.85rem; overflow: visible !important; }
      /* 控制設計：輸入列滿版（單欄全寬） */
      .rcm-design-tabs .shiny-input-container { width: 100%; max-width: 100%; }
      .rcm-design-tabs .form-control,
      .rcm-design-tabs .form-select,
      .rcm-design-tabs textarea.form-control,
      .rcm-design-tabs .selectize-control,
      .rcm-design-tabs .selectize-input { width: 100% !important; max-width: 100%; }
      .design-validation-panel { margin-top: 0.75rem; }
      /* 設計頁預覽：預設收合於下方，可點擊展開／收回 */
      .design-preview-drawer {
        margin-top: 1rem;
        border: 1px solid #E5E5E5;
        border-radius: 0.375rem;
        background: var(--brand-white);
      }
      .design-preview-drawer > .design-preview-toggle {
        width: 100%;
        text-align: left;
        border: none;
        border-radius: 0.375rem;
        background: rgba(134,188,37,0.08);
        color: var(--brand-blue);
        font-weight: 600;
        padding: 0.65rem 1rem;
      }
      .design-preview-drawer > .design-preview-toggle:hover {
        background: rgba(134,188,37,0.16);
      }
      .design-preview-drawer > .design-preview-toggle .chevron {
        display: inline-block;
        margin-right: 0.35rem;
        transition: transform 0.15s ease;
      }
      .design-preview-drawer > .design-preview-toggle[aria-expanded=true] .chevron {
        transform: rotate(90deg);
      }
      .design-preview-drawer .design-preview-body {
        padding: 0.85rem 1rem 1rem;
        border-top: 1px solid #E5E5E5;
      }
      .design-rcm-preview-panel {
        margin-top: 0.65rem;
        padding: 0.55rem 0.65rem;
        background: rgba(0, 91, 170, 0.03);
        border: 1px solid rgba(0, 91, 170, 0.1);
        border-radius: 0.35rem;
      }
      .design-rcm-preview-panel .preview-title {
        font-size: 0.78rem;
        font-weight: 700;
        color: var(--brand-blue);
        margin-bottom: 0.35rem;
      }
      .design-rcm-preview-panel table { margin-bottom: 0; font-size: 0.82rem; }
      .design-rcm-preview-panel th {
        width: 42%;
        vertical-align: top;
        font-weight: 600;
        background: rgba(255,255,255,0.65);
      }
      .design-rcm-preview-panel td.na-cell { color: #ADB5BD; }
      /* 左下角 spinner（取代全頁遮罩／載入中文字） */
      #shiny-busy { display: none !important; }
      #app-corner-spinner {
        position: fixed;
        bottom: 1rem;
        left: 1rem;
        z-index: 10050;
        display: none;
        pointer-events: none;
      }
      body.shiny-busy #app-corner-spinner,
      #app-corner-spinner.app-spinner-boot {
        display: block;
      }
      .app-spinner-ring {
        width: 28px;
        height: 28px;
        border: 3px solid rgba(0, 91, 170, 0.18);
        border-top-color: var(--brand-blue);
        border-right-color: var(--brand-green);
        border-radius: 50%;
        animation: app-spinner-spin 0.75s linear infinite;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.08);
      }
      @keyframes app-spinner-spin {
        to { transform: rotate(360deg); }
      }
      .dataTables_processing { display: none !important; }
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
        tags$div(class = "small fw-bold mb-1", lab_req("循環")),
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
      tags$h2("Goddamn SOX"),
      p("輔助設計標準內部控制點，產出 RCM、訪談題綱與 CSA 測試步驟。")
    ),
    card(
      class = "home-section",
      card_header("整體設計流程"),
      tags$ol(
        class = "home-steps mb-0",
        tags$li(tags$strong("側邊欄"), "：循環／公司名稱。"),
        tags$li(tags$strong("風險控制點設計"), "：",
                strong("基礎設定 → 風險辨識 → 控制設計"),
                "（分頁籤；", tags$span(class = "text-danger", "*"), " 為必填）。"),
        tags$li(strong("完成設計＝寫入 RCM 一列"), "（1 控制點 ↔ 1 RCM 列）。"),
      tags$li(tags$strong("判決書查詢"), "：司法院裁判書進階查詢、抓取判決內文與結果分析。"),
        tags$li(tags$strong("訪談問項設計"), "／", tags$strong("控制點測試設計"),
                "：對齊已定稿 RCM。"),
        tags$li(tags$strong("PBC／RCM"), "檢視匯出；需要時再開",
                tags$strong("範本庫"), "／", tags$strong("參數庫"), "。")
      )
    ),
    card(
      class = "home-section",
      card_header("各頁籤用途"),
      div(
        class = "home-tabs-grid",
        div(class = "home-tab-card", strong("判決書查詢"), "司法院裁判書爬蟲、判決內文與結果分析。"),
        div(class = "home-tab-card", strong("訪談問項設計"), "已定稿 RCM → 訪談題綱。"),
        div(class = "home-tab-card", strong("風險控制點設計"), "分頁籤填寫基礎／風險／控制；定稿寫入 RCM。"),
        div(class = "home-tab-card", strong("控制點測試設計"), "CSA 測試步驟與情境組。"),
        div(class = "home-tab-card", strong("RCM"), "檢視／下載已定稿列。"),
        div(class = "home-tab-card", strong("PBC資料庫"), "客戶原名 → 標準命名。"),
        div(class = "home-tab-card", strong("範本庫"), "可跳過套用；寫入需高權。"),
        div(class = "home-tab-card", strong("參數庫"), "查詢／套用；維護需高權。")
      )
    )
  ),
  nav_panel(
    "判決書查詢",
    card(
      card_header(
        div(
          class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
          span("司法院裁判書查詢（對齊進階查詢欄位）"),
          div(
            class = "d-flex gap-1 flex-wrap",
            downloadButton("download_judgment", "下載分析結果 .xlsx",
                           class = "btn-success btn-sm"),
            actionButton("judgment_run", "開始爬取並分析",
                           class = "btn-primary btn-sm")
          )
        )
      ),
      fluidRow(
        column(4,
               textInput(
                 "judgment_target_company", "查詢標的公司（影響分析用）",
                 value = "", width = "100%",
                 placeholder = "同步自左側公司名稱"
               )),
        column(4, numericInput("judgment_max_results", "抓取筆數上限（最近期）", value = 30,
                               min = 1, max = 100, step = 1)),
        column(4, uiOutput("judgment_status_box"))
      ),
      fluidRow(
        column(4,
               selectizeInput(
                 "judgment_court", "法院（可複選；空白＝所有法院）",
                 choices = JUDGMENT_COURT_CHOICES,
                 selected = "",
                 multiple = TRUE,
                 options = list(placeholder = "所有法院")
               )),
        column(8,
               checkboxGroupInput(
                 "judgment_sys", "案件類別（未勾選＝全選）",
                 choices = JUDGMENT_CASE_TYPE_CHOICES,
                 inline = TRUE
               ))
      ),
      tags$table(class = "table table-sm table-borderless judgment-search-table mb-2",
        tags$tr(
          tags$th("裁判字號"),
          tags$td(
            div(
              class = "judgment-case-no-row",
              textInput("judgment_year", NULL, placeholder = "年度", width = "100%"),
              textInput("judgment_case", NULL, placeholder = "字別", width = "100%"),
              textInput("judgment_no", NULL, placeholder = "起始號", width = "100%"),
              textInput("judgment_no_end", NULL, placeholder = "結束號", width = "100%")
            )
          )
        ),
        tags$tr(
          tags$th("裁判期間"),
          tags$td(
            div(
              class = "judgment-period-row",
              dateInput(
                "judgment_date_start", "起日",
                value = NULL, format = "yyyy-mm-dd",
                width = "100%"
              ),
              tags$span(class = "text-muted judgment-period-sep", "至"),
              dateInput(
                "judgment_date_end", "迄日",
                value = NULL, format = "yyyy-mm-dd",
                width = "100%"
              )
            )
          )
        ),
        tags$tr(
          tags$th("裁判案由"),
          tags$td(textInput("judgment_title", NULL, placeholder = "請輸入檢索字詞", width = "100%"))
        ),
        tags$tr(
          tags$th("裁判主文"),
          tags$td(textInput("judgment_jmain", NULL, placeholder = "請輸入檢索字詞", width = "100%"))
        ),
        tags$tr(
          tags$th("全文內容"),
          tags$td(textInput("judgment_kw", NULL, placeholder = "請輸入檢索字詞", width = "100%"))
        ),
        tags$tr(
          tags$th("裁判大小"),
          tags$td(
            div(
              class = "judgment-kb-row",
              textInput("judgment_kb_start", NULL, placeholder = "起 K", width = "100%"),
              textInput("judgment_kb_end", NULL, placeholder = "迄 K", width = "100%")
            )
          )
        )
      ),
      p(
        class = "text-muted small mb-2",
        "抓取判決全文與主文，並依判斷規則（關鍵字→結果分析）產出分析結論（供審計／內控參考，非法律意見）。",
        "可匯入過去分析之 ", tags$code(".xlsx"), " 以累積學習判斷規則。",
        "若自動查詢失敗：請至官網查詢後，於左側「查詢結果」按右鍵複製完整網址，",
        "貼至「查詢結果 URL」欄位再執行。"
      ),
      fluidRow(
        class = "judgment-result-url-row mb-2",
        column(12,
               textAreaInput(
                 "judgment_result_url", "查詢結果 URL（選填）",
                 placeholder = "https://judgment.judicial.gov.tw/FJUD/qryresultlst.aspx?...",
                 rows = 2, resize = "vertical", width = "100%"
               ))
      ),
      fluidRow(
        class = "judgment-history-import-row mb-2",
        column(8,
               fileInput(
                 "judgment_history_xlsx", "匯入歷史分析結果（選填）",
                 accept = c(".xlsx", ".xls"),
                 buttonLabel = "選擇 xlsx",
                 placeholder = "含「判決分析」工作表"
               )),
        column(4,
               actionButton("judgment_learn_rules", "從歷史結果更新判斷規則",
                            class = "btn-outline-secondary btn-sm"),
               uiOutput("judgment_rules_status"))
      ),
      DTOutput("judgment_table")
    )
  ),
  nav_panel(
    "訪談問項設計",
    card(
      card_header("訪談引導（依序選取）"),
      uiOutput("interview_status_steps"),
      textOutput("interview_status_summary"),
      uiOutput("interview_guide_banner"),
      # 循環於側邊欄；此處①子作業 → ②風險 → ③控制點
      selectInput(
        "interview_sub", NULL,
        choices = c("① 選擇子作業…" = ""),
        selected = ""
      ),
      div(
        class = "interview-risk-control-row",
        selectizeInput(
          "interview_risk_pick", "② 風險因素",
          choices = NULL, multiple = TRUE,
          options = list(
            placeholder = "可空＝該子作業下全部風險",
            plugins = list("remove_button")
          )
        ),
        selectizeInput(
          "interview_control_pick", "③ 控制點",
          choices = NULL, multiple = TRUE,
          options = list(
            placeholder = "可空＝範圍內全部控制點",
            plugins = list("remove_button")
          )
        )
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
          checkboxGroupInput(
            "interview_elements", NULL,
            choices = INTERVIEW_ELEMENTS, selected = DEFAULT_INTERVIEW_ELEMENTS
          )
        ),
        accordion_panel(
          "5W1H／PBC",
          tags$div(
            class = "interview-5w1h-fields",
            tags$p(
              class = "small text-muted mb-2",
              "依 HOW → WHAT → WHEN → WHO → WHERE → NEXT 列出訪談問句；",
              "HOW 中的 XX 將代入②風險或控制點風險名稱。"
            ),
            lapply(INTERVIEW_5W1H_FIELD_ORDER, function(key) {
              textAreaInput(
                interview_5w1h_input_id(key),
                sprintf("%s", INTERVIEW_5W1H_FIELD_LABELS[[key]]),
                value = "",
                placeholder = INTERVIEW_5W1H_DEFAULT_PROMPTS[[key]],
                rows = 2,
                width = "100%"
              )
            }),
            tags$div(
              class = "interview-5w1h-combined",
              tags$label(class = "control-label", "完整訪談問項"),
              div(class = "interview-5w1h-combined-box", textOutput("interview_5w1h_combined"))
            )
          )
        )
      ),
      div(
        class = "d-flex gap-1 flex-wrap mt-2",
        downloadButton("download_interview", "下載訪談題綱 CSV", class = "btn-success btn-sm")
      ),
      textOutput("interview_worksheet_stats"),
    ),
    div(
      class = "design-preview-drawer",
      tags$button(
        class = "design-preview-toggle",
        type = "button",
        `data-bs-toggle` = "collapse",
        `data-bs-target` = "#interviewPreviewCollapse",
        `aria-expanded` = "false",
        `aria-controls` = "interviewPreviewCollapse",
        tags$span(class = "chevron", "▸"),
        "預覽列（訪談題綱）— 點擊展開或收回"
      ),
      div(
        id = "interviewPreviewCollapse",
        class = "collapse",
        div(
          class = "design-preview-body",
          DTOutput("interview_table"),
          verbatimTextOutput("interview_paragraph")
        )
      )
    )
  ),
  nav_panel(
    "風險控制點設計",
    card(
      card_header("風險控制點設計"),
      div(
        class = "rcm-design-tabs",
        navset_tab(
          id = "rcm_design_tabs",
          nav_panel(
            "① 基礎設定",
            div(
              class = "design-stage-save-bar",
              actionButton("preview_rcm_basic", "儲存", class = "btn-sm btn-outline-primary")
            ),
            uiOutput("design_cycle_readonly"),
            uiOutput("sub_process_hint"),
            textInput("sub_process_id", lab_opt("子作業編號"), value = "",
                      placeholder = "循環編號-子作業序號（例：EC-101）",
                      width = "100%"),
            uiOutput("sub_process_select_ui"),
            textInput("control_id", lab_opt("控制編號"), value = "", width = "100%",
                      placeholder = "循環編號-子作業序號-控制序號（例：EC-101-01）"),
            uiOutput("design_preview_basic")
          ),
          nav_panel(
            "② 風險辨識",
            div(
              class = "design-stage-save-bar",
              actionButton("preview_rcm_risk", "儲存", class = "btn-sm btn-outline-primary")
            ),
            div(
              class = "design-tab-filter-bar",
              tags$div(class = "filter-title", "風險類別／風險因素篩選 — 快速找出相關風險描述"),
              selectInput(
                "filter_risk_category", NULL,
                choices = c("全部風險類別…" = "", RISK_CATEGORY_CHOICES),
                selected = "", width = "100%"
              ),
              textInput("filter_risk_factor_kw", NULL, value = "", width = "100%",
                        placeholder = "風險因素關鍵字（可留空）…"),
              uiOutput("filter_risk_hits")
            ),
            div(
              class = "risk-principle-area-row",
              selectizeInput(
                "risk_principle", "風險面向",
                choices = NULL, multiple = FALSE, width = "100%",
                options = list(
                  create = TRUE, createOnBlur = TRUE, maxItems = 1,
                  placeholder = "Risk Principle；可選建議或自訂"
                )
              ),
              selectizeInput(
                "risk_area", "風險範疇",
                choices = NULL, multiple = FALSE, width = "100%",
                options = list(
                  create = TRUE, createOnBlur = TRUE, maxItems = 1,
                  placeholder = "Risk Area；可選建議或自訂"
                )
              )
            ),
            div(
              class = "risk-factor-category-row",
              uiOutput("risk_factor_select_ui"),
              selectInput(
                "risk_category", lab_req("風險類別"),
                choices = c("請選擇…" = "", RISK_CATEGORY_CHOICES),
                selected = "", width = "100%"
              )
            ),
            uiOutput("risk_factor_hint"),
            uiOutput("risk_description_select_ui"),
            uiOutput("significant_account_hint"),
            selectizeInput(
              "significant_account", "會計科目",
              choices = account_select_choices(),
              multiple = TRUE,
              selected = character(0),
              width = "100%",
              options = list(
                create = TRUE,
                placeholder = "複選科目或「全部適用」"
              )
            ),
            selectInput("romm_classification", "RoMM 分類（抽樣輔助）",
                        choices = ROMM_CLASS_CHOICES, width = "100%"),
            uiOutput("design_preview_risk")
          ),
          nav_panel(
            "③ 控制設計",
            div(
              class = "design-stage-save-bar",
              actionButton("preview_rcm_control", "儲存", class = "btn-sm btn-outline-primary")
            ),
            div(
              class = "design-tab-filter-bar",
              tags$div(class = "filter-title", "控制方式／控制性質篩選 — 快速找出相關控制活動"),
              selectInput(
                "filter_ctrl_approach", NULL,
                choices = c("全部控制方式…" = "", CONTROL_ACTIVITY_TYPE_PD),
                selected = "", width = "100%"
              ),
              selectInput(
                "filter_ctrl_nature", NULL,
                choices = c("全部控制性質…" = "", CONTROL_TYPE_MANUAL_AUTO),
                selected = "", width = "100%"
              ),
              uiOutput("filter_ctrl_hits")
            ),
            uiOutput("oa_live_check"),
            uiOutput("type_live_check"),
            uiOutput("control_objective_select_ui"),
            uiOutput("control_activity_select_ui"),
            div(
              class = "assertions-side mb-2",
              selectizeInput(
                "assertions", "控制聲明",
                choices = character(0), multiple = TRUE, selected = character(0),
                width = "100%",
                options = list(
                  create = FALSE,
                  placeholder = "依風險類別選取"
                )
              ),
              uiOutput("assertions_hint")
            ),
            div(
              class = "control-attr-row",
              selectInput(
                "approach", lab_req("控制方式"),
                choices = c("請選擇…" = "", CONTROL_ACTIVITY_TYPE_PD),
                selected = "", width = "100%"
              ),
              selectInput(
                "nature", lab_req("控制性質"),
                choices = c("請選擇…" = "", CONTROL_TYPE_MANUAL_AUTO),
                selected = "", width = "100%"
              ),
              selectInput(
                "frequency", lab_req("控制頻率"),
                choices = c("請選擇…" = "", FREQUENCY_CHOICES),
                selected = "", width = "100%"
              )
            ),
            selectizeInput(
              "responsible_unit", lab_req("控制點負責單位"),
              choices = character(0), selected = "", multiple = FALSE, width = "100%",
              options = list(
                create = TRUE,
                createOnBlur = TRUE,
                maxItems = 1,
                placeholder = "例：資訊安全單位；可選建議或雙擊修改自訂"
              )
            ),
            selectizeInput(
              "iuc", lab_req(CONTROL_IUC_DOCUMENT_LABEL),
              choices = NULL, multiple = TRUE, width = "100%",
              options = list(
                create = TRUE,
                createOnBlur = TRUE,
                placeholder = "IUC；可多選；選後可雙擊修改；自 PBC 選取或手動輸入",
                plugins = list("remove_button")
              )
            ),
            textInput(
              "related_system", lab_opt("相關系統"), width = "100%",
              placeholder = "例：ERP、AD、權限管理系統（IT／應用系統，與 IUC 不同）"
            ),
            uiOutput("related_system_hint"),
            selectizeInput(
              "related_policy", lab_opt("相關政策與制度"),
              choices = NULL, multiple = TRUE, width = "100%",
              options = list(
                create = TRUE,
                createOnBlur = TRUE,
                placeholder = "政策制度類 PBC；選後可雙擊修改",
                plugins = list("remove_button")
              )
            ),
            selectizeInput(
              "related_documents", lab_opt("相關文件"),
              choices = character(0), selected = "", multiple = FALSE, width = "100%",
              options = list(
                create = TRUE,
                createOnBlur = TRUE,
                maxItems = 1,
                placeholder = "一般相關文件；可選建議或雙擊修改自訂"
              )
            ),
            div(
              class = "related-law-row",
              selectizeInput(
                "related_law", "相關法規",
                choices = c("請選擇或輸入…" = "", RELATED_LAW_CHOICES),
                multiple = TRUE, width = "100%",
                options = list(
                  create = TRUE,
                  createOnBlur = TRUE,
                  placeholder = "僅遵循面可填；可多選／自訂；選後可雙擊修改",
                  plugins = list("remove_button")
                )
              ),
              textInput(
                "related_law_url", "法規有效網址連結",
                value = "", width = "100%",
                placeholder = "https://…（選填；該法規之有效連結）"
              )
            ),
            uiOutput("related_law_hint"),
            selectizeInput(
              "related_document_pbc", lab_req(CONTROL_EVIDENCE_DOCUMENT_LABEL),
              choices = NULL, multiple = TRUE, width = "100%",
              options = list(
                create = TRUE,
                createOnBlur = TRUE,
                placeholder = "Control Evidences；選後可雙擊修改；自 PBC 選取或手動輸入",
                plugins = list("remove_button")
              )
            ),
            div(
              class = "d-flex gap-1 flex-wrap mb-1",
              actionButton("goto_pbc_tab", "開啟 PBC 資料庫", class = "btn-sm btn-outline-secondary")
            ),
            uiOutput("related_document_hint"),
            uiOutput("design_preview_control")
          )
        )
      ),
      div(class = "design-validation-panel", uiOutput("live_validation")),
      div(
        class = "d-flex gap-1 flex-wrap mt-2",
        actionButton("finalize_rcm_row", "完成設計＝寫入 RCM 一列", class = "btn-success btn-sm"),
        actionButton("collect_ready_to_lib", "儲存→資料庫", class = "btn-outline-success btn-sm")
      )
    ),
    div(
      class = "design-preview-drawer",
      tags$button(
        class = "design-preview-toggle",
        type = "button",
        `data-bs-toggle` = "collapse",
        `data-bs-target` = "#designPreviewCollapse",
        `aria-expanded` = "false",
        `aria-controls` = "designPreviewCollapse",
        tags$span(class = "chevron", "▸"),
        "預覽列 — 點擊展開或收回"
      ),
      div(
        id = "designPreviewCollapse",
        class = "collapse",
        div(
          class = "design-preview-body",
          uiOutput("rcm_parity_box"),
          uiOutput("live_preview"),
          DTOutput("control_table"),
          verbatimTextOutput("control_paragraph")
        )
      )
    )
  ),
  nav_panel(
    "控制點測試設計",
    card(
      card_header("控制點測試設計（CSA）"),
      selectizeInput(
        "worksheet_controls_sa", NULL, choices = NULL, multiple = TRUE,
        options = list(placeholder = "已定版控制點（空＝全部）")
      ),
      checkboxGroupInput("csa_elements", "測試步驟元素",
                         choices = DESIGN_ELEMENTS, selected = DEFAULT_CSA_ELEMENTS),
      actionButton("ws_select_core_csa", "自我評估核心元素", class = "btn-sm btn-primary"),
      uiOutput("csa_status"),
      tags$hr(),
      tags$strong(class = "small", "頻率 → 建議最低樣本數"),
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
      tags$strong(class = "small", "控制現況情境組（可多組）"),
      selectizeInput(
        "csa_edit_control", "編輯控制點", choices = NULL,
        options = list(placeholder = "選擇控制點")
      ),
      selectizeInput(
        "csa_scenario_pick", "情境組", choices = NULL,
        options = list(placeholder = "選擇或新增情境組")
      ),
      textInput("csa_scenario_name", "控制現況情境名稱",
                placeholder = "例：電子簽核路徑／口頭核准路徑"),
      textAreaInput("csa_scenario_status", "該情境之控制現況說明", rows = 2,
                    placeholder = "此情境下公司實際怎麼做"),
      div(
        class = "d-flex gap-1 flex-wrap mb-2",
        actionButton("csa_scenario_add", "新增情境組", class = "btn-sm btn-outline-primary"),
        actionButton("csa_scenario_save", "儲存此情境組", class = "btn-sm btn-primary"),
        actionButton("csa_scenario_del", "刪除此情境組", class = "btn-sm btn-outline-danger")
      ),
      tags$strong(class = "small", "此情境組之測試步驟（Form 4120SR）"),
      selectizeInput("type", "Type", choices = TYPE_CHOICES,
                     options = list(create = TRUE, placeholder = "Form 4120SR Type")),
      selectizeInput(
        "inputs", "Inputs", choices = NULL, multiple = TRUE, width = "100%",
        options = list(
          create = TRUE,
          createOnBlur = TRUE,
          placeholder = "測試投入／證據來源；自 PBC 選取或手動輸入",
          plugins = list("remove_button")
        )
      ),
      textAreaInput("review_steps", "Steps", rows = 4, placeholder = "測試步驟（每行一步）"),
      textAreaInput("outputs", "Outputs", rows = 2, placeholder = "預期產出／文件"),
      textAreaInput("investigation_threshold", "調查門檻", rows = 1, placeholder = "調查門檻")
    ),
    div(
      class = "design-preview-drawer",
      tags$button(
        class = "design-preview-toggle",
        type = "button",
        `data-bs-toggle` = "collapse",
        `data-bs-target` = "#csaPreviewCollapse",
        `aria-expanded` = "false",
        `aria-controls` = "csaPreviewCollapse",
        tags$span(class = "chevron", "▸"),
        "預覽列（自我評估測試步驟）— 點擊展開或收回"
      ),
      div(
        id = "csaPreviewCollapse",
        class = "collapse",
        div(
          class = "design-preview-body",
          DTOutput("csa_table"),
          downloadButton("download_csa", "下載自我評估測試步驟 CSV", class = "btn-sm")
        )
      )
    )
  ),
  nav_panel(
    "RCM",
    card(
      uiOutput("rcm_count_box"),
      uiOutput("rcm_preview_status"),
      uiOutput("rcm_latest_saved"),
      div(class = "rcm-table-scroll-wrap", DTOutput("rcm_table")),
      downloadButton("download_rcm", "下載 RCM.xlsx", class = "btn-sm"),
      tags$hr(),
      tags$strong("缺漏／缺文件／控制缺失"),
      DTOutput("gap_table")
    )
  ),
  nav_panel(
    "PBC資料庫",
    card(
      class = "pbc-db-card",
      card_header(
        class = "pbc-db-card-header",
        tags$span(class = "pbc-db-card-title", "PBC資料庫"),
        tags$div(
          class = "pbc-export-btn",
          downloadButton(
            "download_pbc_samples", "匯出樣本需求清單.xlsx",
            class = "btn-sm btn-success"
          )
        )
      ),
      div(
        class = "d-flex gap-2 flex-wrap align-items-center mb-2",
        checkboxInput(
          "pbc_filter_by_cycle", "僅顯示側邊欄循環",
          value = TRUE, width = "auto"
        ),
        tags$span(
          class = "small text-muted",
          "複選列後匯出；未勾選列時匯出目前表內全部。"
        )
      ),
      div(class = "pbc-table-scroll-wrap", DTOutput("pbc_table")),
      uiOutput("pbc_walkthrough_box")
    ),
    card(
      card_header("PBC樣本資訊設定"),
      uiOutput("pbc_cycle_readonly"),
      p(class = "small text-muted mb-2",
        tags$strong("登錄／刪除／匯入"), "需高權登入。"),
      tags$div(
        class = "pbc-name-map-row",
        textInput(
          "pbc_client", "客戶原始取得PBC名稱", value = "",
          placeholder = "客戶取得原名", width = "100%"
        ),
        tags$div(class = "pbc-name-map-arrow", `aria-hidden` = "true", "→"),
        textInput(
          "pbc_reviewed", "檢視後新命名", value = "",
          placeholder = "檢視後新命名", width = "100%"
        )
      ),
      tags$div(
        class = "pbc-spec-row",
        textAreaInput(
          "pbc_spec", lab_opt("PBC規格說明"), rows = 3, width = "100%",
          placeholder = "選填：取得／檢附要求與規格說明"
        ),
        tags$div(
          class = "pbc-spec-if-exists",
          checkboxInput("pbc_if_exists", "如果存在", FALSE)
        )
      ),
      tags$div(
        class = "pbc-kind-format-row",
        selectInput("pbc_kind", "證據類型（特別標示）", choices = PBC_KIND_CHOICES),
        selectizeInput(
          "pbc_file_format", "樣本檔案格式",
          choices = PBC_FILE_FORMAT_CHOICES,
          options = list(
            placeholder = "例如 .jpg／.png／.pptx",
            create = TRUE,
            createOnBlur = TRUE
          )
        )
      ),
      tags$div(
        class = "pbc-id-related-row",
        textInput("pbc_id", "ID", placeholder = "ID（可空）"),
        selectizeInput(
          "pbc_related", lab_opt("互相勾稽（Walkthrough）"),
          choices = NULL, multiple = TRUE, width = "100%",
          options = list(
            placeholder = "選取相關 PBC（可多選；亦可由規格說明自動解析）",
            plugins = list("remove_button")
          )
        )
      ),
      textAreaInput(
        "pbc_notes", NULL, rows = 3, width = "100%", placeholder = "備註"
      ),
      div(
        class = "d-flex gap-1 flex-wrap",
        actionButton("pbc_add", "登錄", class = "btn-primary btn-sm"),
        actionButton("pbc_delete", "刪除", class = "btn-outline-danger btn-sm")
      ),
      fileInput("upload_pbc", NULL, buttonLabel = "匯入 CSV／Excel",
                accept = c(".csv", ".xlsx", ".xls"))
    ),
    tags$div(
      class = "pbc-status-footer design-preview-drawer mt-3",
      tags$button(
        class = "design-preview-toggle",
        type = "button",
        `data-bs-toggle` = "collapse",
        `data-bs-target` = "#pbcNameMapCollapse",
        `aria-expanded` = "false",
        `aria-controls` = "pbcNameMapCollapse",
        tags$span(class = "chevron", "▸"),
        "PBC 命名對照一覽 — 點擊展開或收回"
      ),
      div(
        id = "pbcNameMapCollapse",
        class = "collapse",
        div(
          class = "design-preview-body",
          verbatimTextOutput("pbc_all_status")
        )
      )
    )
  ),
  nav_panel(
    "範本庫",
    card(
      class = "lib-apply-card",
      card_header("範本套用"),
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
      )
    ),
    uiOutput("admin_lib_edit_panel"),
    card(
      card_header("即時顯示"),
      DTOutput("lib_table"),
      verbatimTextOutput("lib_preview")
    ),
    uiOutput("admin_lib_mutate_panel"),
    card(
      card_header("匯出"),
      div(
        class = "d-flex gap-2 flex-wrap",
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
    ),
    uiOutput("admin_button_guide_panel"),
    uiOutput("admin_table_schema_panel"),
    uiOutput("admin_db_persist_panel")
  )
)

server <- function(input, output, session) {
  controls <- reactiveVal(list())
  is_admin <- reactiveVal(FALSE)
  pending_admin_action <- reactiveVal(NULL)
  rcm_revision <- reactiveVal(0L)
  last_saved_control <- reactiveVal(NULL)
  rcm_preview_ctrl <- reactiveVal(NULL)
  applying_template <- reactiveVal(FALSE)
  lib_revision <- reactiveVal(0L)
  pbc_form_cycle <- reactiveVal("")
  db_persist_revision <- reactiveVal(0L)

  bump_rcm_views <- function(ctrl = NULL) {
    rcm_revision(rcm_revision() + 1L)
    if (!is.null(ctrl)) last_saved_control(ctrl)
  }

  rcm_display_df <- reactive({
    rcm_revision()
    preview <- rcm_preview_ctrl()
    cs <- Filter(is_control_finalized_for_rcm, controls())
    rows <- list()
    saved <- character()
    if (!is.null(preview) && length(preview)) {
      pr <- control_to_rcm_row(preview, seq_no = 0L)
      rows[[length(rows) + 1L]] <- pr
      saved <- c(saved, "")
    }
    if (length(cs)) {
      cs <- rev(cs)
      rcm <- controls_to_rcm(cs)
      if (nrow(rcm)) {
        for (i in seq_len(nrow(rcm))) rows[[length(rows) + 1L]] <- rcm[i, , drop = FALSE]
        saved <- c(saved, vapply(cs, function(x) as.character(x$saved_at %||% ""), character(1)))
      }
    }
    if (!length(rows)) return(empty_rcm_display_df())
    rcm_all <- do.call(rbind, rows)
    if (length(saved) == nrow(rcm_all)) {
      rcm_all <- cbind(`儲存時間` = saved, as.data.frame(rcm_all, stringsAsFactors = FALSE))
    }
    rcm_all
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
      pending <- pending_admin_action()
      pending_admin_action(NULL)
      if (identical(pending, "pbc_add")) execute_pbc_add()
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

  output$admin_button_guide_panel <- renderUI({
    if (!isTRUE(is_admin())) return(NULL)
    div(
      class = "table-responsive-wrap",
      button_interactions_card_ui(version = app_version_label(root))
    )
  })

  output$admin_table_schema_panel <- renderUI({
    if (!isTRUE(is_admin())) return(NULL)
    div(
      class = "table-responsive-wrap",
      table_schemas_card_ui(version = app_version_label(root))
    )
  })

  output$admin_db_persist_panel <- renderUI({
    if (!isTRUE(is_admin())) return(NULL)
    db_persist_revision()
    st <- database_persist_status(app_database_manifest(data_dir))
    card(
      class = "button-guide-card",
      card_header("高權：資料庫持久化狀態"),
      tags$p(
        class = "small text-muted mb-2",
        "修改 PBC／範本庫／參數庫後會立即寫入 ",
        tags$code("data/"),
        " 目錄；部署前請 commit 至 GitHub，",
        tags$code("rsconnect::deployApp"),
        " 才會同步至 shinyapps.io。"
      ),
      tags$pre(
        class = "small mb-2",
        style = "white-space: pre-wrap; background: transparent; border: 0; padding: 0;",
        format_database_persist_status(st)
      ),
      tags$ul(
        class = "small text-muted mb-0",
        tags$li(tags$code("pbc_registry.csv"), "／", tags$code("pbc_registry.json")),
        tags$li(tags$code("control_library.json"), "／", tags$code("control_library.csv")),
        tags$li(tags$code("parameter_store.json"))
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
  # 若尚無 PBC 庫，先以資訊／財務報導／銷售循環種子清單初始化
  if (!file.exists(pbc_path_csv) && !file.exists(pbc_path_json)) {
    for (seed_nm in c("seed_it_cycle_pbc.R", "seed_fr_cycle_pbc.R",
                      "seed_sc_cycle_pbc.R", "seed_py_cycle_pbc.R")) {
      seed_script <- file.path(root, "data", seed_nm)
      if (file.exists(seed_script)) {
        tryCatch(sys.source(seed_script, envir = new.env(parent = globalenv())),
                 error = function(e) NULL)
      }
    }
  }
  # 先以普通物件載入／補種子，勿在 reactiveVal 建立後於非 reactive 脈絡讀取
  # （Shiny 1.14+ 會拋 "Operation not allowed without an active reactive context"）
  reg0 <- get_cached_file_data(
    "pbc_registry",
    pbc_path_csv,
    function(p) load_pbc_registry(p, pbc_path_json)
  )
  seed_if_missing_cycle <- function(cycle_nm, seed_file) {
    has <- is.data.frame(reg0) && nrow(reg0) > 0 && any(reg0$cycle == cycle_nm)
    if (has) return(invisible(NULL))
    seed_path <- file.path(root, "data", seed_file)
    if (!file.exists(seed_path)) return(invisible(NULL))
    tryCatch({
      sys.source(seed_path, envir = new.env(parent = globalenv()))
      reg0 <<- load_pbc_registry(pbc_path_csv, pbc_path_json)
    }, error = function(e) NULL)
  }
  seed_if_missing_cycle("電腦化資訊系統循環", "seed_it_cycle_pbc.R")
  seed_if_missing_cycle("財務報導循環", "seed_fr_cycle_pbc.R")
  seed_if_missing_cycle("銷售及收款循環", "seed_sc_cycle_pbc.R")
  seed_if_missing_cycle("薪工循環", "seed_py_cycle_pbc.R")
  pbc_reg <- reactiveVal(reg0)
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
  lib <- reactiveVal(get_cached_file_data(
    "control_library",
    lib_path_json,
    function(p) load_control_library(p, fallback_seed = TRUE)
  ))
  # 磁碟庫已含種子／批次時，勿在每個 session 同步重跑去識別合併（約 7s），
  # 否則會堵住循環／子作業選單更新。僅在庫過小時補齊。
  session$onFlushed(function() {
    isolate({
      cur <- lib()
      if (length(cur) >= 100L) return()
      builtin <- seed_control_library(TRUE)
      if (exists("cascade_builtin_library", mode = "function")) {
        try(cascade_builtin_library(force = TRUE), silent = TRUE)
      }
      merged <- merge_libraries(cur, builtin, overwrite = TRUE)
      batch <- file.path(root, "data", "jinglian_it_rcm_batch.json")
      if (file.exists(batch)) {
        merged <- tryCatch(
          merge_libraries(merged, load_control_library(batch, fallback_seed = FALSE), overwrite = TRUE),
          error = function(e) merged
        )
      }
      pl_batch <- file.path(root, "data", "prologium_rcm_batch.json")
      if (file.exists(pl_batch)) {
        merged <- tryCatch(
          merge_libraries(merged, load_control_library(pl_batch, fallback_seed = FALSE), overwrite = TRUE),
          error = function(e) merged
        )
      }
      if (exists("deidentify_library_item", mode = "function")) {
        merged <- lapply(merged, function(it) {
          tryCatch(deidentify_library_item(it), error = function(e) it)
        })
      }
      if (exists("strip_non_design_control_fields", mode = "function")) {
        merged <- lapply(merged, function(it) {
          if (!is.null(it$control)) {
            it$control <- strip_non_design_control_fields(it$control)
          }
          it
        })
      }
      lib(persist_lib(merged, force = TRUE))
    })
  }, once = TRUE)

  session$onFlushed(function() {
    if (nrow(isolate(pbc_reg())) == 0L) {
      reg <- get_cached_file_data(
        "pbc_registry",
        pbc_path_csv,
        function(p) load_pbc_registry(p, pbc_path_json)
      )
      if (nrow(reg) == 0L) {
        for (seed_nm in c("seed_it_cycle_pbc.R", "seed_fr_cycle_pbc.R",
                          "seed_sc_cycle_pbc.R", "seed_py_cycle_pbc.R")) {
          seed_script <- file.path(root, "data", seed_nm)
          if (file.exists(seed_script)) {
            tryCatch(sys.source(seed_script, envir = new.env(parent = globalenv())),
                     error = function(e) NULL)
          }
        }
        invalidate_cached_file_data("pbc_registry")
        reg <- load_pbc_registry(pbc_path_csv, pbc_path_json)
      }
      if (nrow(reg) > 0L) pbc_reg(reg)
    }
  }, once = TRUE)

  bump_db_persist_views <- function() {
    db_persist_revision(db_persist_revision() + 1L)
  }

  persist_pbc <- function(reg, force = FALSE) {
    if (!isTRUE(force) && !require_admin(is_admin(), session)) {
      return(isolate(pbc_reg()))
    }
    out <- persist_pbc_to_disk(reg, pbc_path_csv, pbc_path_json)
    invalidate_cached_file_data("pbc_registry")
    bump_db_persist_views()
    out
  }
  persist_lib <- function(library, force = FALSE) {
    if (!isTRUE(force) && !require_admin(is_admin(), session)) {
      return(isolate(lib()))
    }
    out <- persist_library_to_disk(library, lib_path_json, lib_path_csv)
    invalidate_cached_file_data("control_library")
    bump_db_persist_views()
    out
  }

  param_store <- reactiveVal(get_cached_file_data(
    "parameter_store",
    param_path_json,
    load_parameter_store
  ))
  persist_params <- function(force = FALSE) {
    if (!isTRUE(force) && !require_admin(is_admin(), session)) {
      return(isolate(param_store()))
    }
    df <- parameter_catalog(
      library = lib(), controls = controls(),
      pbc = pbc_reg()
    )
    save_parameter_store(df, param_path_json)
    if (!verify_persist_file(param_path_json)) {
      stop("參數庫未能寫入磁碟：", param_path_json)
    }
    param_store(df)
    invalidate_cached_file_data("parameter_store")
    bump_db_persist_views()
    df
  }
  persist_params_df <- function(df) {
    if (!require_admin(is_admin(), session)) return(isolate(param_store()))
    save_parameter_store(df, param_path_json)
    if (!verify_persist_file(param_path_json)) {
      stop("參數庫未能寫入磁碟：", param_path_json)
    }
    param_store(df)
    invalidate_cached_file_data("parameter_store")
    bump_db_persist_views()
    df
  }

  # 設計儲存成功：質性／選單欄位寫入參數庫（不需高權）
  refresh_design_text_param_choices <- function() {
    store <- isolate(param_store())
    merge_ch <- function(param, cur) {
      ch <- parameter_options(store, param)
      cur <- trimws(as.character(cur %||% "")[[1]])
      if (nzchar(cur) && !(cur %in% ch)) ch <- c(ch, cur)
      stats::setNames(ch, ch)
    }
    ru <- trimws(as.character(isolate(input$responsible_unit %||% "")[[1]]))
    updateSelectizeInput(
      session, "responsible_unit",
      choices = merge_ch("控制點負責單位", ru),
      selected = ru, server = FALSE
    )
    rd <- trimws(as.character(isolate(input$related_documents %||% "")[[1]]))
    updateSelectizeInput(
      session, "related_documents",
      choices = merge_ch("相關文件", rd),
      selected = rd, server = FALSE
    )
    rs <- trimws(as.character(isolate(input$related_system %||% "")))
    # 相關系統維持 textInput；選項僅入參數庫供參數頁／下次重建使用
    invisible(rs)
  }

  persist_design_custom_params <- function(ctrl) {
    if (is.null(ctrl)) return(invisible(NULL))
    df <- ingest_ctrl_parameters(isolate(param_store()), ctrl, source = "設計自訂")
    persist_parameters_to_disk(df, param_path_json)
    param_store(df)
    invalidate_cached_file_data("parameter_store")
    bump_db_persist_views()
    try(refresh_design_text_param_choices(), silent = TRUE)
    try(refresh_sub_process_choices(force = TRUE), silent = TRUE)
    try(refresh_design_suggest_choices(force = TRUE), silent = TRUE)
    invisible(df)
  }

  session$onSessionEnded(function() {
    tryCatch(
      flush_all_app_databases(
        isolate(pbc_reg()),
        isolate(lib()),
        isolate(param_store()),
        pbc_path_csv, pbc_path_json,
        lib_path_json, lib_path_csv,
        param_path_json
      ),
      error = function(e) NULL
    )
  })

  output$sidebar_cycle_hint <- renderUI({
    cy <- input$cycle %||% ""
    if (!nzchar(cy)) {
      tags$div(class = "small text-danger", "必填：請先選定循環（訪談／設計）。")
    } else {
      tags$div(class = "small text-muted", sprintf("目前：%s", cy))
    }
  })
  output$design_cycle_readonly <- renderUI({
    cy <- input$cycle %||% ""
    cc <- trimws(input$cycle_code %||% "")
    if (!nzchar(cy)) {
      div(class = "alert alert-danger py-1 mb-2 small",
          tags$strong("循環為必填。"), "請先於左側側邊欄選擇循環後再填寫本頁。")
    } else {
      div(class = "alert alert-secondary py-1 mb-2 small",
          sprintf("循環：%s（編號 %s）— 於側邊欄變更。", cy, if (nzchar(cc)) cc else "—"))
    }
  })

  # 設計頁籤頂部簡約搜尋：依範本庫找風險描述／控制活動
  tab_filter_rows <- reactive({
    cy <- input$cycle %||% ""
    if (!nzchar(cy)) return(list())
    library_controls_flat(cascade_source_library(lib()), cycle = cy)
  })
  output$filter_risk_hits <- renderUI({
    hits <- search_risk_description_hits(
      tab_filter_rows(),
      category = input$filter_risk_category %||% "",
      factor_kw = input$filter_risk_factor_kw %||% ""
    )
    if (!length(hits)) {
      return(tags$div(class = "small text-muted",
                      if (!nzchar(input$cycle %||% "")) "請先選定循環" else "無相符風險描述"))
    }
    tags$div(
      class = "design-tab-filter-hits",
      lapply(seq_along(hits), function(i) {
        h <- hits[[i]]
        actionLink(
          paste0("filter_risk_pick_", i),
          label = h$label,
          class = "btn btn-link",
          onclick = sprintf(
            "Shiny.setInputValue('filter_risk_apply', {rf:%s, cat:%s, desc:%s, nonce:Math.random()}, {priority:'event'}); return false;",
            jsonlite::toJSON(h$risk_factor %||% "", auto_unbox = TRUE),
            jsonlite::toJSON(h$risk_category %||% "", auto_unbox = TRUE),
            jsonlite::toJSON(h$risk_description %||% "", auto_unbox = TRUE)
          )
        )
      })
    )
  })
  observeEvent(input$filter_risk_apply, {
    v <- input$filter_risk_apply
    if (is.null(v)) return()
    desc <- as.character(v$desc %||% "")
    rf <- as.character(v$rf %||% "")
    cat <- as.character(v$cat %||% "")
    if (nzchar(desc)) {
      freezeReactiveValue(input, "risk_description")
      updateSelectizeInput(session, "risk_description", selected = desc)
      refresh_design_suggest_choices(force = TRUE)
    }
    if (nzchar(cat)) {
      freezeReactiveValue(input, "risk_category")
      updateSelectInput(session, "risk_category", selected = cat)
    }
    if (nzchar(rf)) {
      sel <- parse_risk_factor_values(rf)
      refresh_design_suggest_choices(force = TRUE)
      freezeReactiveValue(input, "risk_factor")
      updateSelectizeInput(session, "risk_factor", selected = sel)
    }
    showNotification("已帶入風險描述", type = "message", duration = 3)
  })
  output$filter_ctrl_hits <- renderUI({
    hits <- search_control_activity_hits(
      tab_filter_rows(),
      approach = input$filter_ctrl_approach %||% "",
      nature = input$filter_ctrl_nature %||% ""
    )
    if (!length(hits)) {
      return(tags$div(class = "small text-muted",
                      if (!nzchar(input$cycle %||% "")) "請先選定循環" else "無相符控制活動"))
    }
    tags$div(
      class = "design-tab-filter-hits",
      lapply(seq_along(hits), function(i) {
        h <- hits[[i]]
        actionLink(
          paste0("filter_ctrl_pick_", i),
          label = h$label,
          class = "btn btn-link",
          onclick = sprintf(
            "Shiny.setInputValue('filter_ctrl_apply', {act:%s, ap:%s, nat:%s, nonce:Math.random()}, {priority:'event'}); return false;",
            jsonlite::toJSON(h$control_activity %||% "", auto_unbox = TRUE),
            jsonlite::toJSON(h$approach %||% "", auto_unbox = TRUE),
            jsonlite::toJSON(h$nature %||% "", auto_unbox = TRUE)
          )
        )
      })
    )
  })
  observeEvent(input$filter_ctrl_apply, {
    v <- input$filter_ctrl_apply
    if (is.null(v)) return()
    act <- as.character(v$act %||% "")
    ap <- as.character(v$ap %||% "")
    nat <- as.character(v$nat %||% "")
    if (nzchar(act)) {
      freezeReactiveValue(input, "control_activity")
      updateSelectizeInput(session, "control_activity", selected = act)
      refresh_design_suggest_choices(force = TRUE)
    }
    if (nzchar(ap)) {
      ap_n <- if (exists("normalize_single_activity_type", mode = "function"))
        normalize_single_activity_type(ap) else ap
      if (nzchar(ap_n)) updateSelectInput(session, "approach", selected = ap_n)
    }
    if (nzchar(nat)) {
      nat_n <- if (exists("normalize_control_type_manual_auto", mode = "function"))
        normalize_control_type_manual_auto(nat) else nat
      if (nzchar(nat_n)) updateSelectInput(session, "nature", selected = nat_n)
    }
    showNotification("已帶入控制活動", type = "message", duration = 3)
  })
  output$risk_factor_hint <- renderUI({
    cy <- input$cycle %||% ""
    if (!nzchar(cy)) {
      return(div(class = "alert alert-warning py-1 mb-2 small",
                 "請先於側邊欄選擇循環，以載入建議風險因素。"))
    }
    sub_key <- sub_process_filter_key(input$sub_process_id %||% "", input$sub_process %||% "")
    rows <- library_controls_flat(cascade_source_library(lib()), cycle = cy)
    if (nzchar(sub_key)) {
      rows <- filter_cascade_rows(rows, sub_key = sub_key)
    }
    n_risk <- length(cascade_risk_choices(rows))
    if (n_risk > 0) {
      scope <- if (nzchar(sub_key)) "所選子作業" else "本循環"
      div(class = "alert alert-success py-1 mb-2 small",
          sprintf("%s已載入 %d 個建議風險因素 TAG；均可自訂新增。風險描述請以質性文字撰寫。",
                  scope, n_risk))
    } else {
      div(class = "alert alert-secondary py-1 mb-2 small",
          "暫無建議風險因素，請直接輸入 TAG 或先選子作業；風險描述請以質性文字撰寫。")
    }
  })

  output$sub_process_hint <- renderUI({
    cy <- input$cycle %||% ""
    if (!nzchar(cy)) return(NULL)
    rows <- library_controls_flat(cascade_source_library(lib()), cycle = cy)
    n_sub <- length(cascade_sub_process_choices(rows))
    if (n_sub > 0) {
      div(class = "alert alert-success py-1 mb-2 small",
          sprintf("「%s」已載入 %d 個建議子作業，可從子作業名稱選取（選後自動帶入編號）。", cy, n_sub))
    } else {
      div(class = "alert alert-secondary py-1 mb-2 small",
          sprintf("「%s」暫無建議子作業，請直接輸入子作業名稱。", cy))
    }
  })
  output$pbc_cycle_readonly <- renderUI({
    edit_id <- trimws(input$pbc_id %||% "")
    form_cy <- trimws(pbc_form_cycle() %||% "")
    sidebar_cy <- input$cycle %||% ""
    if (nzchar(edit_id) && nzchar(form_cy)) {
      return(tags$div(
        class = "small text-muted mb-2",
        sprintf("編輯中：循環將保留為「%s」。", form_cy)
      ))
    }
    tags$div(
      class = "small text-muted mb-2",
      "上方資料庫顯示全部循環，無需先選側邊欄循環。",
      if (nzchar(sidebar_cy)) {
        tags$span(sprintf(" 登錄新筆將寫入循環：%s。", sidebar_cy))
      } else {
        tags$span(" 登錄新筆時循環欄可留空。")
      }
    )
  })

  refresh_lib_choices <- function() {
    ch <- library_choices(isolate(lib()), cycle_filter = isolate(input$cycle %||% ""),
                          query = isolate(input$lib_query %||% ""))
    cur <- isolate(input$lib_pick %||% "")
    sel <- if (nzchar(cur) && cur %in% unname(ch)) cur else ""
    updateSelectInput(
      session, "lib_pick",
      choices = c("未套用範本…" = "", ch),
      selected = sel
    )
  }

  apply_template_to_form <- function(ctrl) {
    if (is.null(ctrl)) return(invisible(NULL))
    applying_template(TRUE)
    on.exit(applying_template(FALSE), add = TRUE)
    fill_inputs_from_ctrl(
      session, ctrl,
      lib_items = isolate(lib()),
      pbc_registry = isolate(pbc_reg()),
      current_cycle = isolate(input$cycle %||% "")
    )
    refresh_sub_process_choices(force = TRUE)
    refresh_design_suggest_choices(force = TRUE)
    refresh_pbc_choices()
    invisible(NULL)
  }

  # 子作業名稱以 renderUI 重建 selectize（choices 寫入 HTML），
  # 避免分頁尚未綁定時 updateSelectizeInput 訊息丟失。
  sub_process_ui_state <- new.env(parent = emptyenv())
  sub_process_ui_state$cycle <- NULL
  sub_process_ui_tick <- reactiveVal(0L)
  design_suggest_ui_state <- new.env(parent = emptyenv())
  design_suggest_ui_state$scope <- NULL
  design_suggest_ui_tick <- reactiveVal(0L)

  design_cascade_scope <- function(cy, sub_process_id = "", sub_process = "") {
    design_cascade_scope_key(cy, sub_process_id, sub_process)
  }

  reset_design_cascade_scope <- function() {
    design_suggest_ui_state$scope <- NULL
  }

  refresh_sub_process_choices <- function(force = FALSE) {
    # renderUI 已依 cycle／lib 重繪；force 時再 bump 一次以重掛載選單
    if (isTRUE(force)) {
      sub_process_ui_tick(isolate(sub_process_ui_tick()) + 1L)
    }
  }
  refresh_design_suggest_choices <- function(force = FALSE) {
    if (isTRUE(force)) {
      design_suggest_ui_tick(isolate(design_suggest_ui_tick()) + 1L)
    }
  }

  output$sub_process_select_ui <- renderUI({
    tick <- sub_process_ui_tick()
    cy <- input$cycle %||% ""
    lib_items <- lib()
    ch <- character()
    sel <- ""
    if (nzchar(cy)) {
      rows <- library_controls_flat(cascade_source_library(lib_items), cycle = cy)
      ch <- cascade_sub_process_choices(rows)
      param_names <- tryCatch(
        parameter_options(isolate(param_store()), "子作業名稱"),
        error = function(e) character()
      )
      if (length(param_names)) {
        extra <- param_names[!param_names %in% unname(ch)]
        if (length(extra)) ch <- c(ch, stats::setNames(extra, extra))
      }
      cur <- sub_process_name_from_value(isolate(input$sub_process) %||% "")
      spid <- trimws(isolate(input$sub_process_id) %||% "")
      if (nzchar(spid) && !id_matches_cycle_code(spid, cycle_code_for(cy))) {
        cur <- ""
      }
      # 循環變更時清空選取；同循環僅 bump／庫更新時保留有效名稱
      if (!identical(sub_process_ui_state$cycle, cy)) {
        sub_process_ui_state$cycle <- cy
        sel <- ""
      } else {
        sel <- cur
        if (nzchar(sel) && !sel %in% unname(ch)) {
          ch <- c(ch, stats::setNames(sel, sel))
        }
      }
    } else {
      sub_process_ui_state$cycle <- ""
    }
    # 避免重建時舊值回寫造成閃跳
    freezeReactiveValue(input, "sub_process")
    # 開頭放空選項，避免 selectize 自動選第一筆；允許空白
    ch_ui <- if (length(ch)) c("(請選擇或輸入名稱)" = "", ch) else c("(請選擇或輸入名稱)" = "")
    selectizeInput(
      "sub_process", lab_req("子作業名稱"),
      choices = ch_ui,
      selected = if (nzchar(sel)) sel else "",
      width = "100%",
      options = list(
        create = TRUE,
        createOnBlur = TRUE,
        placeholder = "選建議後可雙擊修改；或直接輸入名稱",
        maxItems = 1,
        openOnFocus = TRUE,
        maxOptions = 1000,
        closeAfterSelect = TRUE,
        allowEmptyOption = TRUE,
        showEmptyOptionInDropdown = FALSE
      )
    )
  })

  output$risk_factor_select_ui <- renderUI({
    tick <- design_suggest_ui_tick()
    cy <- input$cycle %||% ""
    spid <- input$sub_process_id %||% ""
    spnm <- input$sub_process %||% ""
    ch <- if (nzchar(cy)) {
      design_tab_risk_factor_choices(lib(), cy, spid, spnm)
    } else {
      character()
    }
    cur <- parse_risk_factor_values(isolate(input$risk_factor %||% character()))
    if (length(cur)) {
      extra <- cur[!cur %in% unname(ch)]
      if (length(extra)) {
        ch <- c(ch, stats::setNames(extra, vapply(extra, risk_factor_tag, character(1))))
      }
    }
    freezeReactiveValue(input, "risk_factor")
    placeholder <- if (nzchar(cy)) {
      sprintf("可複選本循環建議 TAG 或手動新增（已載入 %d 項）", length(ch))
    } else {
      "請先於側邊欄選擇循環，以載入本循環建議 TAG"
    }
    selectizeInput(
      "risk_factor", lab_req("風險因素"),
      choices = ch,
      selected = cur,
      multiple = TRUE,
      width = "100%",
      options = cascade_selectize_field_options(placeholder, multiple = TRUE)
    )
  })

  output$risk_description_select_ui <- renderUI({
    tick <- design_suggest_ui_tick()
    cy <- input$cycle %||% ""
    rows <- if (nzchar(cy)) {
      design_tab_cascade_rows(lib(), cy, input$sub_process_id %||% "", input$sub_process %||% "")
    } else list()
    rf <- input$risk_factor %||% character()
    descs <- if (length(rows)) cascade_risk_description_choices(rows, rf) else character(0)
    ch <- if (length(descs)) risk_description_select_choices(descs) else character()
    cur <- trimws(isolate(input$risk_description %||% ""))
    ch <- ensure_value_in_choices(ch, cur)
    freezeReactiveValue(input, "risk_description")
    ch_ui <- if (length(ch)) c("(請選擇或輸入描述)" = "", ch) else c("(請選擇或輸入描述)" = "")
    selectizeInput(
      "risk_description", lab_req("風險描述"),
      choices = ch_ui,
      selected = if (nzchar(cur)) cur else "",
      width = "100%",
      options = cascade_selectize_field_options(
        "選建議後可雙擊修改；或直接輸入風險描述"
      )
    )
  })

  output$control_objective_select_ui <- renderUI({
    tick <- design_suggest_ui_tick()
    cy <- input$cycle %||% ""
    rows <- if (nzchar(cy)) {
      design_tab_cascade_rows(lib(), cy, input$sub_process_id %||% "", input$sub_process %||% "")
    } else list()
    ch <- if (length(rows)) cascade_objective_choices(rows) else character()
    cur <- trimws(isolate(input$control_objective %||% ""))
    ch <- ensure_value_in_choices(ch, cur)
    freezeReactiveValue(input, "control_objective")
    ch_ui <- if (length(ch)) c("(請選擇或輸入目標)" = "", ch) else c("(請選擇或輸入目標)" = "")
    selectizeInput(
      "control_objective", lab_req("控制目標"),
      choices = ch_ui,
      selected = if (nzchar(cur)) cur else "",
      width = "100%",
      options = cascade_selectize_field_options(
        "Why：欲達成之控制結果（非執行步驟）"
      )
    )
  })

  output$control_activity_select_ui <- renderUI({
    tick <- design_suggest_ui_tick()
    cy <- input$cycle %||% ""
    rows <- if (nzchar(cy)) {
      design_tab_cascade_rows(lib(), cy, input$sub_process_id %||% "", input$sub_process %||% "")
    } else list()
    ch <- if (length(rows)) cascade_activity_text_choices(rows) else character()
    cur <- trimws(isolate(input$control_activity %||% ""))
    ch <- ensure_value_in_choices(ch, cur)
    freezeReactiveValue(input, "control_activity")
    ch_ui <- if (length(ch)) c("(請選擇或輸入活動)" = "", ch) else c("(請選擇或輸入活動)" = "")
    selectizeInput(
      "control_activity", lab_req("控制活動"),
      choices = ch_ui,
      selected = if (nzchar(cur)) cur else "",
      width = "100%",
      options = cascade_selectize_field_options(
        "How：具體執行行為（含誰／何時／如何）"
      )
    )
  })

  # PBC／Assertions 選單快取：避免同內容反覆 update 造成跳閃
  pbc_choices_cache <- new.env(parent = emptyenv())
  assertions_ui_cache <- new.env(parent = emptyenv())
  interview_choices_cache <- new.env(parent = emptyenv())

  choice_maps_equal <- function(ch, prev_ch) {
    identical(unname(ch), unname(prev_ch)) && identical(names(ch), names(prev_ch))
  }
  selection_equal <- function(sel, prev_sel) {
    identical(as.character(sel), as.character(prev_sel))
  }
  update_interview_selectize <- function(input_id, ch, sel) {
    prev <- interview_choices_cache[[input_id]] %||% list(ch = NULL, sel = NULL)
    if (choice_maps_equal(ch, prev$ch) && selection_equal(sel, prev$sel)) {
      return(invisible(NULL))
    }
    interview_choices_cache[[input_id]] <- list(ch = ch, sel = sel)
    updateSelectizeInput(session, input_id, choices = ch, selected = sel)
  }
  update_interview_sub_choices <- function(ch, sel) {
    prev <- interview_choices_cache[["interview_sub"]] %||% list(ch = NULL, sel = NULL)
    if (choice_maps_equal(ch, prev$ch) && selection_equal(sel, prev$sel)) {
      return(invisible(NULL))
    }
    interview_choices_cache[["interview_sub"]] <- list(ch = ch, sel = sel)
    updateSelectInput(session, "interview_sub", choices = ch, selected = sel)
  }
  clear_interview_choices_cache <- function() {
    for (id in c("interview_sub", "interview_risk_pick", "interview_control_pick")) {
      interview_choices_cache[[id]] <- NULL
    }
  }
  refresh_interview_control_pick <- function(scoped, sel_risk) {
    empty_iv <- stats::setNames("", "（請先選子作業）")
    if (!length(scoped)) {
      update_interview_selectize("interview_control_pick", empty_iv, character())
      return(invisible(NULL))
    }
    ch_ctrl <- interview_control_choices(
      filter_interview_controls_by_risk(scoped, sel_risk)
    )
    cur_ctrl <- isolate(input$interview_control_pick %||% character())
    update_interview_selectize(
      "interview_control_pick", ch_ctrl,
      intersect(cur_ctrl, unname(ch_ctrl))
    )
  }

  refresh_pbc_choices <- function() {
    cy <- isolate(input$cycle %||% "")
    reg <- isolate(pbc_reg())
    cf_design <- if (nzchar(cy)) cy else NULL
    # PBC 資料庫頁：不因側邊欄循環縮限選項／表格
    ch_iuc_design <- pbc_non_policy_choices(reg, cycle_filter = cf_design)
    ch_policy_design <- pbc_policy_choices(reg, cycle_filter = cf_design)
    ch_pbc_design <- pbc_choices(reg, cycle_filter = cf_design)
    merge_selected <- function(cur, ch) {
      cur <- parse_text_list_values(cur)
      if (!length(cur)) return(cur)
      extra <- cur[!cur %in% unname(ch)]
      if (length(extra)) ch <<- c(ch, stats::setNames(extra, extra))
      cur
    }
    update_selectize <- function(input_id, ch) {
      cur <- merge_selected(isolate(input[[input_id]] %||% character()), ch)
      cache_key <- input_id
      prev <- pbc_choices_cache[[cache_key]] %||% list(ch = NULL, sel = NULL)
      same_ch <- identical(unname(ch), unname(prev$ch)) && identical(names(ch), names(prev$ch))
      same_sel <- identical(as.character(cur), as.character(prev$sel))
      if (same_ch && same_sel) return(invisible(NULL))
      pbc_choices_cache[[cache_key]] <- list(ch = ch, sel = cur)
      updateSelectizeInput(session, input_id, choices = ch, server = TRUE, selected = cur)
    }
    update_selectize("related_document_pbc", ch_iuc_design)
    update_selectize("iuc", ch_iuc_design)
    update_selectize("related_policy", ch_policy_design)
    update_selectize("inputs", ch_pbc_design)
    # 勾稽選單：排除目前編輯中的 ID
    excl <- trimws(isolate(input$pbc_id %||% ""))
    ch_rel <- pbc_related_link_choices(reg, exclude_id = excl)
    cur_rel <- parse_pbc_id_values(isolate(input$pbc_related %||% character()))
    cur_rel <- cur_rel[cur_rel %in% unname(ch_rel) | cur_rel %in% known_pbc_ids(reg)]
    extra_rel <- cur_rel[!cur_rel %in% unname(ch_rel)]
    if (length(extra_rel)) {
      ch_rel <- c(ch_rel, stats::setNames(extra_rel, extra_rel))
    }
    prev_rel <- pbc_choices_cache[["pbc_related"]] %||% list(ch = NULL, sel = NULL)
    same_ch <- identical(unname(ch_rel), unname(prev_rel$ch)) &&
      identical(names(ch_rel), names(prev_rel$ch))
    same_sel <- identical(as.character(cur_rel), as.character(prev_rel$sel))
    if (!(same_ch && same_sel)) {
      pbc_choices_cache[["pbc_related"]] <- list(ch = ch_rel, sel = cur_rel)
      updateSelectizeInput(session, "pbc_related", choices = ch_rel,
                           server = TRUE, selected = cur_rel)
    }
  }

  # 首屏 flush 後刷新選單（勿在 onFlushed 內讀寫 reactiveVal，Shiny 1.14+ 會崩潰斷線）
  session$onFlushed(function() {
    if (!nrow(isolate(param_store()))) {
      try(persist_params(force = TRUE), silent = TRUE)
    }
    try(refresh_pbc_choices(), silent = TRUE)
    try(refresh_design_text_param_choices(), silent = TRUE)
  }, once = TRUE)

  interview_primary_risk_label <- function() {
    picks <- unique(trimws(as.character(input$interview_risk_pick %||% character())))
    picks <- picks[nzchar(picks)]
    if (length(picks)) return(picks[[1]])
    scoped <- interview_scoped_controls()
    if (length(scoped)) {
      for (ctrl in scoped) {
        tags <- parse_risk_factor_values(ctrl$risk_factor %||% ctrl$risk_name %||% "")
        tags <- tags[nzchar(tags)]
        if (length(tags)) return(tags[[1]])
      }
    }
    "該"
  }

  interview_worksheet <- function() {
    cs <- interview_pool_controls()
    cs <- filter_controls_by_cycle_sub(
      cs,
      cycle = input$cycle %||% "",
      sub_key = input$interview_sub %||% ""
    )
    cs <- filter_interview_controls_by_risk(cs, input$interview_risk_pick %||% character())
    cs <- filter_interview_controls_by_ids(cs, input$interview_control_pick %||% character())
    prompts <- interview_5w1h_prompts_from_values(
      input,
      risk_label = interview_primary_risk_label()
    )
    controls_to_interview(
      cs, input$interview_elements,
      finalized_only = FALSE,
      modules = INTERVIEW_5W1H_FIELD_ORDER,
      pbc_reg = pbc_reg(),
      include_module_rows = TRUE,
      custom_5w1h_prompts = prompts
    )
  }

  output$interview_5w1h_combined <- renderText({
    prompts <- interview_5w1h_prompts_from_values(
      input,
      risk_label = interview_primary_risk_label()
    )
    interview_5w1h_combined_question(prompts)
  })

  output$interview_worksheet_stats <- renderText({
    iv <- interview_worksheet()
    sprintf("題綱列數：%d｜5W1H 面向：%d", nrow(iv), length(INTERVIEW_5W1H_FIELD_ORDER))
  })

  reset_interview_5w1h_fields <- function() {
    for (key in INTERVIEW_5W1H_FIELD_ORDER) {
      updateTextAreaInput(
        session, interview_5w1h_input_id(key),
        value = ""
      )
    }
  }

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
        sprintf("已選子作業 → 建議 %d 筆；②風險／③控制點可空＝全部。",
                length(scoped)))
  })

  output$interview_paragraph <- renderText({
    iv <- interview_worksheet()
    if (!nrow(iv)) return("（尚無訪談題綱；請先於側邊欄選循環，再選①子作業）")
    lines <- sprintf("%s. [%s] %s", iv[["題號"]], iv[["元素"]], iv[["訪談問題"]])
    paste(utils::head(lines, 12), collapse = "
")
  })

  observeEvent(input$cycle, {
    if (isTRUE(applying_template())) return()
    # 切換循環時清空子作業名稱；編號改由循環編號觀察器預填 stub
    freezeReactiveValue(input, "sub_process")
    freezeReactiveValue(input, "sub_process_id")
    freezeReactiveValue(input, "control_id")
    freezeReactiveValue(input, "risk_factor")
    freezeReactiveValue(input, "risk_description")
    freezeReactiveValue(input, "control_objective")
    freezeReactiveValue(input, "control_activity")
    sub_process_ui_state$cycle <- NULL
    reset_design_cascade_scope()
    updateTextInput(session, "sub_process_id", value = "")
    updateTextInput(session, "control_id", value = "")
    refresh_sub_process_choices(force = TRUE)
    refresh_design_suggest_choices(force = TRUE)
  }, ignoreNULL = FALSE)

  observeEvent(lib(), {
    lib_revision(isolate(lib_revision()) + 1L)
  }, ignoreInit = TRUE)

  # 一律以內建＋使用者庫候選為訪談來源（側邊欄循環→直接選子作業）
  interview_pool_controls <- reactive({
    library_items_as_interview_controls(cascade_source_library(lib()))
  })

  # 設計分頁／基礎設定可能延遲顯示；進入時 bump 重掛載選單
  observeEvent(input$main_nav, {
    if (identical(input$main_nav, "風險控制點設計")) {
      refresh_sub_process_choices(force = TRUE)
      refresh_design_suggest_choices(force = TRUE)
    }
  }, ignoreInit = TRUE)


  observeEvent(input$rcm_design_tabs, {
    if (identical(input$main_nav, "風險控制點設計")) {
      refresh_sub_process_choices(force = TRUE)
      refresh_design_suggest_choices(force = TRUE)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$sub_process, {
    val <- trimws(input$sub_process %||% "")
    nm <- sub_process_name_from_value(val)
    if (!nzchar(nm)) return()
    # 選名稱後由範本庫帶入關聯編號（畫面名稱與編號脫鉤）
    cy <- isolate(input$cycle) %||% ""
    if (nzchar(cy)) {
      rows <- library_controls_flat(cascade_source_library(isolate(lib())), cycle = cy)
      cur_id <- trimws(isolate(input$sub_process_id) %||% "")
      new_id <- lookup_sub_process_id_for_name(rows, nm, preferred_id = cur_id)
      if (nzchar(new_id) && !identical(new_id, cur_id)) {
        freezeReactiveValue(input, "sub_process_id")
        updateTextInput(session, "sub_process_id", value = new_id)
      }
    } else if (grepl("\\|\\|", val, fixed = FALSE)) {
      # 相容舊 key：仍可解析編號
      sp <- parse_sub_process_key(val)
      if (nzchar(sp$id)) {
        cur_id <- trimws(isolate(input$sub_process_id) %||% "")
        if (!identical(sp$id, cur_id)) {
          freezeReactiveValue(input, "sub_process_id")
          updateTextInput(session, "sub_process_id", value = sp$id)
        }
      }
    }
    # 若選單殘留 id||name，改回純名稱顯示
    if (!identical(val, nm)) {
      freezeReactiveValue(input, "sub_process")
      updateSelectizeInput(session, "sub_process", selected = nm)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$risk_factor, {
    cy <- isolate(input$cycle %||% "")
    if (!nzchar(cy)) return()
    sub_key <- sub_process_filter_key(
      isolate(input$sub_process_id %||% ""),
      isolate(input$sub_process %||% "")
    )
    rows <- library_controls_flat(cascade_source_library(isolate(lib())), cycle = cy)
    if (nzchar(sub_key)) {
      rows <- filter_cascade_rows(rows, sub_key = sub_key)
    }
    sel <- isolate(input$risk_factor %||% character())
    if (!length(sel)) return()
    cats <- character()
    romms <- character()
    for (rf in sel) {
      det <- cascade_risk_detail(rows, rf)
      if (nzchar(det$risk_category)) cats <- c(cats, det$risk_category)
      r <- det$sample
      if (is.list(r) && length(r) && nzchar(trimws(r$romm_classification %||% ""))) {
        romms <- c(romms, r$romm_classification)
      }
    }
    cats <- unique(cats[nzchar(cats)])
    if (length(cats) == 1L) {
      cur_cat <- trimws(isolate(input$risk_category %||% ""))
      if (!identical(cur_cat, cats[[1]])) {
        freezeReactiveValue(input, "risk_category")
        updateSelectInput(session, "risk_category", selected = cats[[1]])
      }
    }
    romms <- unique(romms[nzchar(romms)])
    if (length(romms) == 1L) {
      cur_romm <- trimws(isolate(input$romm_classification %||% ""))
      if (!identical(cur_romm, romms[[1]])) {
        freezeReactiveValue(input, "romm_classification")
        updateSelectInput(session, "romm_classification", selected = romms[[1]])
      }
    }
  }, ignoreInit = TRUE)

  observe({
    # 循環／子作業範圍變更時清空連動建議欄，並刷新建議 TAG 與 PBC 選單
    cy <- input$cycle %||% ""
    spid <- input$sub_process_id %||% ""
    spnm <- input$sub_process %||% ""
    scope_key <- design_cascade_scope(cy, spid, spnm)
    if (!identical(design_suggest_ui_state$scope, scope_key)) {
      design_suggest_ui_state$scope <- scope_key
      if (!isTRUE(applying_template())) {
        freezeReactiveValue(input, "risk_factor")
        freezeReactiveValue(input, "risk_description")
        freezeReactiveValue(input, "control_objective")
        freezeReactiveValue(input, "control_activity")
      }
    }
    input$cycle
    input$sub_process
    input$sub_process_id
    lib_revision()
    isolate(refresh_design_suggest_choices(force = TRUE))
    isolate(refresh_pbc_choices())
  })

  interview_sub_ui_state <- new.env(parent = emptyenv())
  interview_sub_ui_state$cycle <- NULL

  interview_scoped_controls <- reactive({
    filter_controls_by_cycle_sub(
      interview_pool_controls(),
      cycle = input$cycle %||% "",
      sub_key = input$interview_sub %||% ""
    )
  })

  observe({
    cy <- input$cycle %||% ""
    lib_revision()
    if (!nzchar(cy)) {
      interview_sub_ui_state$cycle <- ""
      update_interview_sub_choices(
        c("① 請先於側邊欄選擇循環…" = ""),
        ""
      )
      return()
    }
    rows <- library_controls_flat(cascade_source_library(isolate(lib())), cycle = cy)
    ch_sub <- cascade_sub_process_choices(rows)
    label0 <- if (length(ch_sub)) {
      sprintf("① 選擇子作業…（本循環建議 %d）", length(ch_sub))
    } else {
      "① 選擇子作業…（本循環暫無建議）"
    }
    cur <- trimws(isolate(input$interview_sub %||% ""))
    sel <- if (!identical(interview_sub_ui_state$cycle, cy)) {
      interview_sub_ui_state$cycle <- cy
      ""
    } else if (nzchar(cur) && cur %in% unname(ch_sub)) {
      cur
    } else {
      ""
    }
    update_interview_sub_choices(c(stats::setNames("", label0), ch_sub), sel)
  })

  observe({
    input$cycle
    input$interview_sub
    lib_revision()
    scoped <- interview_scoped_controls()
    empty_iv <- stats::setNames("", "（請先選子作業）")
    if (!length(scoped)) {
      update_interview_selectize("interview_risk_pick", empty_iv, character())
      update_interview_selectize("interview_control_pick", empty_iv, character())
      return()
    }
    ch_risk <- interview_risk_choices(scoped)
    cur_risk <- isolate(input$interview_risk_pick %||% character())
    sel_risk <- intersect(cur_risk, unname(ch_risk))
    update_interview_selectize("interview_risk_pick", ch_risk, sel_risk)
    refresh_interview_control_pick(scoped, sel_risk)
  })

  observeEvent(input$interview_risk_pick, {
    scoped <- interview_scoped_controls()
    if (!length(scoped)) return()
    sel_risk <- input$interview_risk_pick %||% character()
    refresh_interview_control_pick(scoped, sel_risk)
  }, ignoreInit = TRUE)

  observe({
    input$cycle
    input$lib_query
    refresh_lib_choices()
  })

  # 啟動時循環維持未選（selectInput 預設已是 ""）；勿再延遲 updateSelectInput 清空，
  # 否則會與使用者剛選的循環競態，把選單／循環編號沖掉。

  output$pbc_all_status <- renderText({
    lines <- format_pbc_status_lines(pbc_reg())
    if (!length(lines)) "（命名庫尚無資料）" else paste(lines, collapse = "\n")
  })

  output$pbc_walkthrough_box <- renderUI({
    s <- input$pbc_table_rows_selected
    reg <- pbc_table_view()
    if (is.null(s) || !length(s) || !nrow(reg)) {
      return(tags$div(
        class = "small text-muted mt-2 mb-1",
        "選取一列以檢視 Walkthrough 勾稽鏈。"
      ))
    }
    pid <- reg$pbc_id[s[[1]]]
    lines <- format_pbc_walkthrough_lines(reg, pid)
    tags$div(
      class = "alert alert-secondary py-2 mt-2 mb-1 small",
      tags$div(class = "fw-bold mb-1", sprintf("Walkthrough｜%s", pid)),
      tags$pre(
        class = "mb-0 small",
        style = "white-space: pre-wrap; background: transparent; border: 0; padding: 0;",
        paste(lines, collapse = "\n")
      )
    )
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
    st <- parameter_store_stats(isolate(param_store()))
    new_choices <- c("全部" = "", stats::setNames(st$params, st$params))
    cur <- isolate(input$param_filter %||% "")
    if (identical(new_choices, assertions_ui_cache$param_filter_choices)) return()
    assertions_ui_cache$param_filter_choices <- new_choices
    updateSelectInput(
      session, "param_filter",
      choices = new_choices,
      selected = if (nzchar(cur) && cur %in% unname(new_choices)) cur else ""
    )
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
      options = dt_loading_opts(pageLength = 25, scrollX = TRUE)
    )
  })

  output$download_params <- downloadHandler(
    filename = function() sprintf("param_catalog_%s.csv", format(Sys.Date(), "%Y%m%d")),
    content = function(file) with_loading(
      utils::write.csv(param_store(), file, row.names = FALSE, fileEncoding = "UTF-8")
    )
  )
  output$download_params_json <- downloadHandler(
    filename = function() sprintf("param_catalog_%s.json", format(Sys.Date(), "%Y%m%d")),
    content = function(file) with_loading(save_parameter_store(param_store(), file))
  )

  observeEvent(input$param_refresh, {
    if (!require_admin(is_admin(), session)) return()
    with_loading({
      df <- persist_params()
      showNotification(sprintf("參數資料庫已從現況重建並儲存（%d 筆）", nrow(df)),
                       type = "message")
    })
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
        refresh_sub_process_choices()
        refresh_design_suggest_choices(force = TRUE)
      },
      "子作業名稱" = function() {
        updateSelectizeInput(session, "sub_process",
                             selected = sub_process_name_from_value(val))
        refresh_sub_process_choices()
        refresh_design_suggest_choices(force = TRUE)
      },
      "風險因素" = function() {
        sel <- parse_risk_factor_values(val)
        freezeReactiveValue(input, "risk_factor")
        updateSelectizeInput(session, "risk_factor", selected = sel)
        refresh_design_suggest_choices(force = TRUE)
      },
      "風險描述" = function() {
        freezeReactiveValue(input, "risk_description")
        updateSelectizeInput(session, "risk_description", selected = val)
        refresh_design_suggest_choices(force = TRUE)
      },
      "風險類別" = function() {
        updateSelectInput(session, "risk_category", selected = val)
      },
      "RoMM 分類" = function() {
        updateSelectInput(session, "romm_classification", selected = val)
      },
      "控制聲明" = function() {
        updateSelectizeInput(session, "assertions", selected = val)
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
        freezeReactiveValue(input, "control_objective")
        updateSelectizeInput(session, "control_objective", selected = val)
        refresh_design_suggest_choices(force = TRUE)
      },
      "控制活動" = function() {
        freezeReactiveValue(input, "control_activity")
        updateSelectizeInput(session, "control_activity", selected = val)
        refresh_design_suggest_choices(force = TRUE)
      },
      "控制性質" = function() {
        updateSelectInput(session, "nature", selected = val)
      },
      "控制類型" = function() {
        updateSelectInput(session, "nature", selected = val)
      },
      "控制方式" = function() {
        updateSelectInput(session, "approach", selected = val)
      },
      "控制活動類型" = function() {
        updateSelectInput(session, "approach", selected = val)
      },
      "控制頻率" = function() {
        updateSelectInput(session, "frequency", selected = val)
      },
      "控制點負責單位" = function() {
        updateSelectizeInput(
          session, "responsible_unit",
          choices = stats::setNames(val, val),
          selected = val, server = FALSE
        )
      },
      "流程負責單位" = function() {
        updateSelectizeInput(
          session, "responsible_unit",
          choices = stats::setNames(val, val),
          selected = val, server = FALSE
        )
      },
      "相關系統／IUC" = function() {
        # 舊參數庫鍵名；僅套用至 IUC（與相關系統分開）
        sel <- expand_pbc_selection(val, pbc_reg())
        updateSelectizeInput(session, "iuc", selected = sel)
      },
      "相關系統" = function() updateTextInput(session, "related_system", value = val),
      "IUC" = function() {
        sel <- expand_pbc_selection(val, pbc_reg())
        updateSelectizeInput(session, "iuc", selected = sel)
      },
      "相關文件-控制用文件" = function() {
        sel <- expand_pbc_selection(val, pbc_reg())
        updateSelectizeInput(session, "iuc", selected = sel)
      },
      "相關法規" = function() updateSelectizeInput(session, "related_law", selected = val),
      "相關法令" = function() updateSelectizeInput(session, "related_law", selected = val),
      "法規有效網址連結" = function() updateTextInput(session, "related_law_url", value = val),
      "相關法規連結" = function() updateTextInput(session, "related_law_url", value = val),
      "相關政策與制度" = function() {
        sel <- expand_pbc_selection(val, pbc_reg())
        updateSelectizeInput(session, "related_policy", selected = sel)
      },
      "相關政策或程序" = function() {
        sel <- expand_pbc_selection(val, pbc_reg())
        updateSelectizeInput(session, "related_policy", selected = sel)
      },
      "相關文件" = function() {
        updateSelectizeInput(
          session, "related_documents",
          choices = stats::setNames(val, val),
          selected = val, server = FALSE
        )
      },
      "風險面向" = function() {
        updateSelectizeInput(session, "risk_principle",
                             choices = stats::setNames(val, val), selected = val, server = FALSE)
      },
      "風險範疇" = function() {
        updateSelectizeInput(session, "risk_area",
                             choices = stats::setNames(val, val), selected = val, server = FALSE)
      }
    )
    mapped[[CONTROL_EVIDENCE_DOCUMENT_LABEL]] <- function() {
      sel <- expand_pbc_selection(val, pbc_reg())
      updateSelectizeInput(session, "related_document_pbc", selected = sel)
    }
    mapped[["控制佐證文件"]] <- mapped[[CONTROL_EVIDENCE_DOCUMENT_LABEL]]
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
    apply_template_to_form(item$control)
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
    apply_template_to_form(item$control)
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
      options = dt_loading_opts(pageLength = 15, dom = "ftip")
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
  output$download_lib_json <- downloadHandler(
    filename = function() sprintf("control_library-%s.json", format(Sys.time(), "%Y%m%d")),
    content = function(file) with_loading(save_control_library(lib(), file))
  )

  labelize <- function(label, body) {
    body <- trimws(body %||% "")
    lab <- trimws(label %||% "")
    if (!nzchar(body)) return("")
    if (!nzchar(lab)) return(body)
    sprintf("[%s] %s", lab, body)
  }

  current_draft_from_inputs <- function() {
    rf_tag <- format_risk_factor_text(input$risk_factor %||% character())
    nature <- normalize_control_type_manual_auto(input$nature %||% "")
    approach <- normalize_control_activity_type_pd(input$approach)
    # 風險類別決定三大屬性種類；細節由風險描述自動帶入（無獨立欄位）
    kind <- risk_attr_kind_from_category(input$risk_category %||% "")
    if (!nzchar(kind)) kind <- "operations"
    attr_ctrl <- enforce_single_risk_attr(
      list(risk_category = input$risk_category %||% ""),
      kind = kind,
      detail = trimws(input$risk_description %||% "")
    )
    list(
      control_id = input$control_id %||% "",
      company = input$company %||% "",
      cycle = input$cycle %||% "",
      cycle_code = {
        cc <- trimws(input$cycle_code %||% "")
        if (nzchar(cc)) cc else cycle_code_for(input$cycle %||% "")
      },
      sub_process_id = {
        sp_val <- trimws(input$sub_process %||% "")
        id <- trimws(input$sub_process_id %||% "")
        if (nzchar(id)) id else sub_process_id_from_value(sp_val)
      },
      sub_process = sub_process_name_from_value(input$sub_process %||% ""),
      risk_principle = trimws(input$risk_principle %||% ""),
      risk_area = trimws(input$risk_area %||% ""),
      risk_factor = rf_tag,
      risk_name = rf_tag,
      risk_description = trimws(input$risk_description %||% ""),
      risk_category = attr_ctrl$risk_category,
      risk_attr_financial = attr_ctrl$risk_attr_financial,
      risk_attr_operations = attr_ctrl$risk_attr_operations,
      risk_attr_compliance = attr_ctrl$risk_attr_compliance,
      romm_classification = input$romm_classification %||% "",
      significant_account = join_significant_accounts(input$significant_account),
      assertions = paste(input$assertions %||% character(), collapse = "；"),
      control_objective = trimws(input$control_objective %||% ""),
      control_activity = trimws(input$control_activity %||% ""),
      frequency = resolve_control_frequency(nature, input$frequency %||% ""),
      responsible_unit = trimws(input$responsible_unit %||% ""),
      iuc_or_system = {
        iuc_sel <- input$iuc %||% character()
        resolve_multi_pbc_text(iuc_sel, pbc_reg())
      },
      iuc = {
        iuc_sel <- input$iuc %||% character()
        resolve_multi_pbc_text(iuc_sel, pbc_reg())
      },
      related_system = trimws(input$related_system %||% ""),
      related_policy = resolve_multi_pbc_text(input$related_policy %||% character(), pbc_reg()),
      related_documents = trimws(input$related_documents %||% ""),
      related_law = {
        v <- input$related_law %||% character(0)
        paste(unique(trimws(as.character(v))), collapse = "；")
      },
      related_law_url = trimws(input$related_law_url %||% ""),
      related_document_pbc_ids = {
        parts <- split_pbc_selection(input$related_document_pbc %||% character(), pbc_reg())
        parts$ids
      },
      related_document_manual = {
        parts <- split_pbc_selection(input$related_document_pbc %||% character(), pbc_reg())
        paste(parts$manual, collapse = "；")
      },
      related_document = resolve_multi_pbc_text(
        input$related_document_pbc %||% character(), pbc_reg()
      ),
      nature = nature,
      approach = approach,
      control_type = nature,
      control_activity_type = approach,
      type = input$type %||% "",
      inputs = resolve_multi_pbc_text(input$inputs %||% character(), pbc_reg()),
      review_steps = input$review_steps %||% "",
      outputs = {
        out <- trimws(input$outputs %||% "")
        if (nzchar(out)) out else {
          resolve_multi_pbc_text(input$related_document_pbc %||% character(), pbc_reg())
        }
      },
      investigation_threshold = input$investigation_threshold %||% "",
      dependent_controls = "",
      key_control = "Y"
    )
  }

  render_design_rcm_preview <- function(ctrl, section = NULL, saved_sections = NULL, title = NULL) {
    df <- design_rcm_preview_fields(ctrl, section = section, saved_sections = saved_sections)
    if (!nrow(df)) return(NULL)
    hdr <- title %||% if (!is.null(section) && nzchar(section)) {
      sprintf("預覽列（%s）", section)
    } else {
      "預覽列"
    }
    tags$div(
      class = "design-rcm-preview-panel",
      tags$div(class = "preview-title", hdr),
      tags$table(
        class = "table table-sm table-bordered",
        tags$tbody(
          lapply(seq_len(nrow(df)), function(i) {
            val <- as.character(df$內容[[i]])
            tags$tr(
              tags$th(df$欄位[[i]]),
              tags$td(
                class = if (identical(val, "NA")) "na-cell" else NULL,
                val
              )
            )
          })
        )
      )
    )
  }

  output$design_preview_basic <- renderUI({
    render_design_rcm_preview(current_draft_from_inputs(), section = "基礎設定")
  })
  output$design_preview_risk <- renderUI({
    render_design_rcm_preview(current_draft_from_inputs(), section = "風險辨識")
  })
  output$design_preview_control <- renderUI({
    render_design_rcm_preview(current_draft_from_inputs(), section = "控制設計")
  })

  output$live_preview <- renderUI({
    render_design_rcm_preview(current_draft_from_inputs(), section = NULL)
  })

  push_rcm_section_preview <- function(section) {
    if (!nzchar(trimws(input$cycle %||% ""))) {
      return(showNotification("循環為必填：請先於側邊欄選定循環。", type = "error"))
    }
    draft <- current_draft_from_inputs()
    merged <- merge_design_preview_section(rcm_preview_ctrl(), draft, section)
    rcm_preview_ctrl(merged)
    bump_rcm_views()
    persist_design_custom_params(merged)
    cols <- rcm_preview_section_columns(section)
    showNotification(
      sprintf("已儲存「%s」至 RCM 表格：%s",
              section, paste(cols, collapse = "、")),
      type = "message", duration = 5
    )
  }
  observeEvent(input$preview_rcm_basic, {
    push_rcm_section_preview("基礎設定")
  }, ignoreInit = TRUE)
  observeEvent(input$preview_rcm_risk, {
    push_rcm_section_preview("風險辨識")
  }, ignoreInit = TRUE)
  observeEvent(input$preview_rcm_control, {
    push_rcm_section_preview("控制設計")
  }, ignoreInit = TRUE)

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
    div(class = cls, tags$small(tags$strong("類型欄檢核："), tchk$msg))
  })

  # 風險類別驅動會計科目／法令／聲明鎖定；子作業編號就緒時自動順編控制編號
  output$significant_account_hint <- renderUI({
    cat <- trimws(input$risk_category %||% "")
    if (is_reporting_risk_category(cat)) {
      div(class = "alert alert-info py-1 mb-2 small", lab_req("報導面"), " — 會計科目必填。")
    } else if (nzchar(cat)) {
      div(class = "alert alert-secondary py-1 mb-2 small", "非報導面：會計科目已鎖定。")
    } else {
      helpText(class = "text-muted small", "請先選擇風險類別。")
    }
  })

  account_sel_prev <- reactiveVal(character())

  observeEvent(input$significant_account, {
    cat <- trimws(isolate(input$risk_category %||% ""))
    if (!is_reporting_risk_category(cat)) return()
    sel <- parse_account_values(input$significant_account)
    if (!length(sel)) {
      account_sel_prev(character())
      return()
    }
    all_opt <- ACCOUNT_ALL_OPTION
    desired <- sel
    prev <- account_sel_prev()
    if (all_opt %in% sel && length(sel) > 1L) {
      if (!(all_opt %in% prev)) {
        desired <- all_opt
      } else {
        desired <- sel[sel != all_opt]
      }
    }
    account_sel_prev(desired)
    if (!identical(sel, desired)) {
      freezeReactiveValue(input, "significant_account")
      updateSelectizeInput(
        session, "significant_account",
        choices = account_select_choices(),
        selected = desired
      )
    }
  }, ignoreInit = TRUE)

  output$related_document_hint <- renderUI({
    cat <- trimws(input$risk_category %||% "")
    nature <- normalize_control_type_manual_auto(input$nature %||% "")
    mode <- related_document_mode_for_ctrl(list(
      nature = nature, risk_category = cat
    ))
    if (identical(mode, "required")) {
      div(class = "alert alert-info py-1 mb-2 small",
          lab_req("人工控制"), " — ", CONTROL_EVIDENCE_DOCUMENT_LABEL,
          "可多選；可自 ", tags$strong("PBC 資料庫"), " 選取，或直接輸入文件名稱。")
    } else if (identical(mode, "locked")) {
      reason <- c(
        if (is_automatic_control(nature)) "自動控制",
        if (is_compliance_risk_category(cat)) "遵循面風險"
      )
      div(class = "alert alert-secondary py-1 mb-2 small",
          paste0("無法設定", CONTROL_EVIDENCE_DOCUMENT_LABEL, "（", paste(reason, collapse = "／"), "）。"))
    } else {
      NULL
    }
  })

  # 相關系統標籤與「相關政策與制度」同列排版（選填／必填 * 接在標題後）
  observe({
    nature <- normalize_control_type_manual_auto(input$nature %||% "")
    lab <- if (is_automatic_control(nature)) lab_req("相關系統") else lab_opt("相關系統")
    updateTextInput(session, "related_system", label = lab)
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
      helpText(class = "text-muted small", "請先選控制性質；自動控制時相關系統必填。")
    }
  })

  output$related_law_hint <- renderUI({
    cat <- trimws(input$risk_category %||% "")
    if (is_compliance_risk_category(cat)) {
      div(class = "alert alert-info py-1 mb-2 small",
          lab_req("遵循面"), " — 相關法規必填。")
    } else if (nzchar(cat)) {
      div(class = "alert alert-secondary py-1 mb-2 small", "非遵循面：相關法規已鎖定。")
    } else {
      helpText(class = "text-muted small", "請先選擇風險類別。")
    }
  })

  output$assertions_hint <- renderUI({
    cat <- trimws(input$risk_category %||% "")
    mode <- assertion_mode_for_category(cat)
    if (identical(mode, "reporting")) {
      div(class = "alert alert-info py-1 mb-2 small", "報導面：八種 Assertions 可複選。")
    } else if (identical(mode, "operations")) {
      div(class = "alert alert-info py-1 mb-2 small", "營運面：完整性／正確性／即時性。")
    } else if (identical(mode, "locked")) {
      div(class = "alert alert-secondary py-1 mb-2 small", "遵循面：無 Assertions。")
    } else {
      helpText(class = "text-muted small", "請先選擇風險類別。")
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

  # 循環編號為基準 → 預填／鎖定子作業與控制編號前綴（例：CS → CS-／CS-）
  observeEvent(input$cycle_code, {
    if (isTRUE(applying_template())) return()
    code <- trimws(input$cycle_code %||% "")
    if (!nzchar(code)) return()
    spid <- trimws(input$sub_process_id %||% "")
    new_spid <- sync_sub_process_id_value(spid, code)
    if (!identical(new_spid, spid)) {
      freezeReactiveValue(input, "sub_process_id")
      updateTextInput(session, "sub_process_id", value = new_spid)
      spid <- new_spid
    }
    cid <- trimws(input$control_id %||% "")
    new_cid <- sync_control_id_value(cid, code, spid)
    if (!identical(new_cid, cid)) {
      freezeReactiveValue(input, "control_id")
      updateTextInput(session, "control_id", value = new_cid)
    }
  }, ignoreInit = TRUE)

  # 子作業編號變更 → 鎖定循環前綴，並同步控制編號（CS-102 → CS-102-）
  observeEvent(input$sub_process_id, {
    if (isTRUE(applying_template())) return()
    cc <- trimws(input$cycle_code %||% "")
    if (!nzchar(cc)) cc <- cycle_code_for(input$cycle %||% "")
    if (!nzchar(cc)) return()
    spid <- trimws(input$sub_process_id %||% "")
    new_spid <- sync_sub_process_id_value(spid, cc)
    if (!identical(new_spid, spid)) {
      freezeReactiveValue(input, "sub_process_id")
      updateTextInput(session, "sub_process_id", value = new_spid)
      spid <- new_spid
    }
    cid <- trimws(input$control_id %||% "")
    new_cid <- sync_control_id_value(cid, cc, spid)
    if (!identical(new_cid, cid)) {
      freezeReactiveValue(input, "control_id")
      updateTextInput(session, "control_id", value = new_cid)
    }
  }, ignoreInit = TRUE)

  # 控制編號變更 → 鎖定循環前綴；完整時回推子作業編號（EC-101-02 → EC-101）
  observeEvent(input$control_id, {
    if (isTRUE(applying_template())) return()
    cc <- trimws(input$cycle_code %||% "")
    if (!nzchar(cc)) cc <- cycle_code_for(input$cycle %||% "")
    if (!nzchar(cc)) return()
    cid <- trimws(input$control_id %||% "")
    spid <- trimws(input$sub_process_id %||% "")
    new_cid <- sync_control_id_value(cid, cc, spid)
    # 若控制編號已帶子作業序號，以控制編號回推子作業（循環節仍用 cc）
    sp_from_cid <- sub_process_id_from_control_id(new_cid, cc)
    if (!is.na(sp_from_cid) && nzchar(sp_from_cid)) {
      if (!identical(sp_from_cid, spid)) {
        freezeReactiveValue(input, "sub_process_id")
        updateTextInput(session, "sub_process_id", value = sp_from_cid)
        spid <- sp_from_cid
      }
      # 以回推後的子作業再正規化控制編號
      new_cid <- sync_control_id_value(new_cid, cc, spid)
    }
    if (!identical(new_cid, cid)) {
      freezeReactiveValue(input, "control_id")
      updateTextInput(session, "control_id", value = new_cid)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$nature, {
    if (identical(input$nature, "自動")) {
      updateSelectInput(session, "frequency", selected = "持續")
      session$sendCustomMessage("toggleFrequency", list(enabled = FALSE))
      if (length(isolate(input$related_document_pbc %||% character()))) {
        freezeReactiveValue(input, "related_document_pbc")
        updateSelectizeInput(session, "related_document_pbc", selected = character(0))
      }
    } else {
      session$sendCustomMessage("toggleFrequency", list(enabled = TRUE))
    }
  }, ignoreNULL = FALSE)

  sync_category_driven_fields <- function(cat, nature) {
    cat <- trimws(as.character(cat %||% ""))
    nature <- trimws(as.character(nature %||% ""))
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
    cur_as <- parse_assertion_values(isolate(input$assertions))
    keep_as <- if (length(as_choices)) intersect(cur_as, as_choices) else character(0)
    cache_key <- paste(cat, as_mode, paste(as_choices, collapse = "\t"), paste(keep_as, collapse = "\t"), sep = "|")
    if (!identical(cache_key, assertions_ui_cache$assertions_key)) {
      assertions_ui_cache$assertions_key <- cache_key
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
    }
    session$sendCustomMessage(
      "toggleAssertions",
      list(enabled = identical(as_mode, "reporting") || identical(as_mode, "operations"))
    )
    doc_mode <- related_document_mode_for_ctrl(list(
      nature = nature,
      control_type = nature,
      risk_category = cat
    ))
    session$sendCustomMessage(
      "toggleRelatedDocument",
      list(enabled = identical(doc_mode, "required"))
    )
    if (identical(doc_mode, "locked")) {
      if (length(isolate(input$related_document_pbc %||% character()))) {
        freezeReactiveValue(input, "related_document_pbc")
        updateSelectizeInput(session, "related_document_pbc", selected = character(0))
      }
    }
    if (nzchar(cat) && !is_reporting_risk_category(cat)) {
      if (length(parse_account_values(isolate(input$significant_account)))) {
        freezeReactiveValue(input, "significant_account")
        updateSelectizeInput(session, "significant_account", selected = character(0))
      }
    }
    if (nzchar(cat) && !is_compliance_risk_category(cat)) {
      if (length(isolate(input$related_law))) {
        freezeReactiveValue(input, "related_law")
        updateSelectizeInput(session, "related_law", selected = character(0))
      }
      if (nzchar(trimws(isolate(input$related_law_url %||% "")))) {
        freezeReactiveValue(input, "related_law_url")
        updateTextInput(session, "related_law_url", value = "")
      }
    }
  }

  observeEvent(
    list(input$risk_category, input$nature),
    {
      sync_category_driven_fields(input$risk_category, input$nature)
    },
    ignoreInit = FALSE
  )

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
      req_groups <- req$missing_by_group %||% empty_missing_by_group()
      group_lines <- lapply(DESIGN_ACCORDION_SECTIONS, function(sec) {
        items <- unique(req_groups[[sec]] %||% character())
        items <- items[nzchar(items)]
        if (!length(items)) return(NULL)
        tags$div(
          class = "mb-1",
          tags$strong(sec, "："),
          paste(items, collapse = "、")
        )
      })
      group_lines <- Filter(Negate(is.null), group_lines)
      other_msg <- if (!isTRUE(chk$ok)) {
        chk$msg %||% paste(chk$issues, collapse = "；")
      } else if (nrow(high)) {
        paste(sprintf("[%s] %s", high$category, high$gap_item), collapse = "；")
      } else if (nrow(gaps)) {
        paste(gaps$gap_item, collapse = "；")
      } else {
        NULL
      }
      div(
        class = "alert alert-warning py-2 mb-2 small",
        tags$div(class = "fw-semibold mb-1", "尚不可定稿 RCM 一列"),
        if (length(group_lines)) {
          tags$div(class = "mb-1", tags$span(class = "text-muted", "必填未齊（依表單分組）："), group_lines)
        },
        if (!is.null(other_msg) && nzchar(other_msg)) {
          tags$div(class = "mt-1", tags$span(class = "text-muted", "其他檢核："), other_msg)
        }
      )
    }
  })

  output$rcm_parity_box <- renderUI({
    parity <- assert_design_rcm_parity(controls())
    cls <- if (isTRUE(parity$ok)) "alert alert-secondary py-1 mb-2 small" else "alert alert-danger py-1 mb-2 small"
    div(class = cls,
        sprintf("已定稿控制點 %d ＝ RCM 列 %d", parity$n_controls, parity$n_rcm_rows),
        if (!parity$ok) "（不一致，請重新定稿）")
  })
  output$rcm_count_box <- renderUI({
    rcm_revision()
    parity <- assert_design_rcm_parity(controls())
    preview <- rcm_preview_ctrl()
    tags$p(class = "small mb-2",
           sprintf("目前已定稿 %d 個控制點＝%d 列 RCM", parity$n_controls, parity$n_rcm_rows),
           if (!is.null(preview) && length(preview)) {
             tags$span(class = "text-muted ms-2", "（含 1 列設計中資料）")
           })
  })
  output$rcm_preview_status <- renderUI({
    rcm_revision()
    preview <- rcm_preview_ctrl()
    if (is.null(preview) || !length(preview)) return(NULL)
    secs <- as.character(preview$rcm_preview_sections %||% character())
    secs <- secs[nzchar(secs)]
    tags$div(
      class = "alert alert-secondary py-2 mb-2 small",
      tags$strong("設計中資料列（尚未定稿）："),
      if (length(secs)) {
        tags$span(class = "ms-1", paste(secs, collapse = "、"))
      },
      if (nzchar(preview$rcm_preview_at %||% "")) {
        tags$span(class = "text-muted ms-2", sprintf("（%s）", preview$rcm_preview_at))
      },
      tags$br(),
      tags$span(
        class = "text-muted",
        "「儲存」會將各區塊資料合併至本表對應欄位；完成設計後按「寫入 RCM 一列」定稿。"
      )
    )
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
          paste0("｜", CONTROL_EVIDENCE_DOCUMENT_LABEL, "：", substr(pt$related_document, 1, 28))
        } else ""
      ),
      if (!is.null(row) && nrow(row)) {
        tags$div(class = "text-muted mt-1",
                 "已寫入 RCM｜子作業：", row[["子作業名稱"]] %||% "—")
      }
    )
  })

  # Primary path: 設計完成 → 直接寫入一筆控制點／RCM 列（1:1）
  observeEvent(input$finalize_rcm_row, {
    if (!nzchar(trimws(input$cycle %||% ""))) {
      return(showNotification("循環為必填：請先於側邊欄選定循環。", type = "error"))
    }
    d <- current_draft_from_inputs()
    req <- design_required_check(d)
    if (!isTRUE(req$ok)) {
      return(showNotification(
        paste0("尚未完成設計，不能定稿：",
               format_design_required_by_accordion(req$missing_by_group, req$missing)),
        type = "error", duration = 10
      ))
    }
    if (!activity_type_ok(d$approach)) {
      return(showNotification("控制活動須對應單一預防／偵測屬性", type = "error"))
    }
    with_loading({
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
      rcm_preview_ctrl(NULL)
      bump_rcm_views(pt)
      persist_design_custom_params(pt)
      bslib::nav_select("main_nav", selected = "RCM", session = session)
      updateTextInput(session, "control_id",
                      value = next_rcm_control_id(
                        pt$sub_process_id,
                        collect_existing_control_ids(lists = list(lib(), controls())),
                        cycle_code = pt$cycle_code %||% cycle_code_for(pt$cycle %||% "")
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
    })
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
    req(identical(input$main_nav, "風險控制點設計"))
    req(input$design_preview_open)
    datatable(controls_df(), selection = "single", rownames = FALSE,
              options = dt_loading_opts(pageLength = 6, dom = "t", ordering = FALSE))
  })
  output$control_paragraph <- renderText({
    s <- input$control_table_rows_selected
    cs <- controls()
    if (is.null(s) || !length(cs)) return("選取控制點以檢視")
    cs[[s]]$detailed_description
  })

  # PBC
  execute_pbc_add <- function() {
    tryCatch({
      edit_id <- trimws(input$pbc_id %||% "")
      reg0 <- pbc_reg()
      cycle_val <- if (nzchar(edit_id) && edit_id %in% reg0$pbc_id) {
        stored <- trimws(pbc_form_cycle() %||% "")
        if (nzchar(stored)) {
          stored
        } else {
          trimws(reg0$cycle[reg0$pbc_id == edit_id][[1]] %||% "")
        }
      } else {
        input$cycle %||% ""
      }
      base_row <- list(
        pbc_id = input$pbc_id, client_pbc_name = input$pbc_client,
        reviewed_name = input$pbc_reviewed, pbc_spec = input$pbc_spec,
        pbc_kind = input$pbc_kind,
        pbc_file_format = input$pbc_file_format,
        related_pbc_ids = input$pbc_related,
        iuc_or_system = input$pbc_reviewed,
        cycle = cycle_val,
        notes = apply_pbc_if_exists_note(
          input$pbc_notes,
          isTRUE(input$pbc_if_exists)
        )
      )
      rows <- expand_numbered_pbc_rows(base_row)
      reg <- pbc_reg()
      for (row in rows) {
        reg <- upsert_pbc(reg, row)
      }
      # 規格說明內 #N／「名稱」自動補勾稽
      reg <- enrich_related_pbc_from_specs(reg)
      pbc_reg(reg)
      persist_pbc(reg)
      refresh_pbc_choices()
      updateTextInput(session, "pbc_id", value = "")
      updateTextInput(session, "pbc_client", value = "")
      updateTextInput(session, "pbc_reviewed", value = "")
      updateTextAreaInput(session, "pbc_spec", value = "")
      updateCheckboxInput(session, "pbc_if_exists", value = FALSE)
      updateSelectInput(session, "pbc_kind", selected = "")
      updateSelectizeInput(session, "pbc_file_format", selected = "")
      updateSelectizeInput(session, "pbc_related", selected = character(0))
      updateTextAreaInput(session, "pbc_notes", value = "")
      pbc_form_cycle("")
      msg <- if (length(rows) > 1L) {
        sprintf("已登錄 PBC（%d 筆）", length(rows))
      } else {
        "已登錄 PBC"
      }
      showNotification(msg, type = "message")
    }, error = function(e) showNotification(conditionMessage(e), type = "error"))
  }

  observeEvent(input$pbc_add, {
    if (!require_admin_or_defer(
      is_admin(), session, "pbc_add",
      set_pending = pending_admin_action
    )) return()
    execute_pbc_add()
  })
  pbc_table_view <- reactive({
    reg <- pbc_reg()
    if (!nrow(reg)) return(reg)
    if (!isTRUE(input$pbc_filter_by_cycle)) return(reg)
    cy <- trimws(input$cycle %||% "")
    if (!nzchar(cy)) return(reg)
    filter_pbc_by_cycle(reg, cy)
  })

  pbc_rows_for_export <- reactive({
    reg <- pbc_table_view()
    if (!nrow(reg)) return(reg)
    sel <- input$pbc_table_rows_selected
    if (!is.null(sel) && length(sel)) reg <- reg[sel, , drop = FALSE]
    reg
  })

  output$pbc_table <- renderDT({
    df <- pbc_table_view()
    if (!nrow(df)) {
      empty <- data.frame(
        ID = character(), 循環 = character(), 標準名稱 = character(),
        原始名稱 = character(), 證據類型 = character(), 檔案格式 = character(),
        規格說明 = character(), 勾稽 = character(), 備註 = character(),
        stringsAsFactors = FALSE
      )
      return(datatable(empty, selection = "multiple", rownames = FALSE, width = "100%",
                       options = dt_loading_opts(pageLength = 8, autoWidth = FALSE)))
    }
    show <- data.frame(
      ID = df$pbc_id,
      循環 = df$cycle,
      標準名稱 = ifelse(nzchar(df$reviewed_name), df$reviewed_name, "—"),
      原始名稱 = df$client_pbc_name,
      證據類型 = ifelse(nzchar(df$pbc_kind), df$pbc_kind, "—"),
      檔案格式 = ifelse(nzchar(df$pbc_file_format), df$pbc_file_format, "—"),
      規格說明 = {
        sp <- if ("pbc_spec" %in% names(df)) df$pbc_spec else rep("", nrow(df))
        ifelse(nzchar(sp), substr(sp, 1L, 48L), "—")
      },
      勾稽 = {
        rel <- if ("related_pbc_ids" %in% names(df)) df$related_pbc_ids else rep("", nrow(df))
        vapply(rel, function(x) {
          ids <- parse_pbc_id_values(x)
          if (!length(ids)) "—" else paste(ids, collapse = "；")
        }, character(1))
      },
      備註 = df$notes,
      stringsAsFactors = FALSE
    )
    dt <- datatable(show, selection = "multiple", rownames = FALSE, width = "100%",
                    options = dt_loading_opts(pageLength = 8, autoWidth = FALSE))
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
                 "傳票" = "#F8EDE8",
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
    if (is.null(s) || length(s) != 1L) return()
    row <- pbc_table_view()[s, , drop = FALSE]
    pbc_form_cycle(trimws(row$cycle[[1]] %||% ""))
    updateTextInput(session, "pbc_id", value = row$pbc_id[[1]])
    updateTextInput(session, "pbc_client", value = row$client_pbc_name[[1]])
    updateTextInput(session, "pbc_reviewed", value = row$reviewed_name[[1]])
    updateTextAreaInput(
      session, "pbc_spec",
      value = if ("pbc_spec" %in% names(row)) normalize_pbc_spec(row$pbc_spec[[1]]) else ""
    )
    updateSelectInput(session, "pbc_kind",
                      selected = normalize_pbc_kind(row$pbc_kind[[1]]))
    fmt <- normalize_pbc_file_format(row$pbc_file_format[[1]])
    fmt_choices <- PBC_FILE_FORMAT_CHOICES
    if (nzchar(fmt) && !(fmt %in% unname(fmt_choices))) {
      fmt_choices <- c(fmt_choices, stats::setNames(fmt, fmt))
    }
    updateSelectizeInput(session, "pbc_file_format",
                         choices = fmt_choices, selected = fmt)
    rel_ids <- if ("related_pbc_ids" %in% names(row)) {
      parse_pbc_id_values(row$related_pbc_ids[[1]])
    } else {
      character()
    }
    ch_rel <- pbc_related_link_choices(pbc_reg(), exclude_id = row$pbc_id[[1]])
    extra <- rel_ids[!rel_ids %in% unname(ch_rel)]
    if (length(extra)) ch_rel <- c(ch_rel, stats::setNames(extra, extra))
    updateSelectizeInput(session, "pbc_related", choices = ch_rel,
                         selected = rel_ids)
    updateTextAreaInput(session, "pbc_notes", value = row$notes[[1]])
    updateCheckboxInput(
      session, "pbc_if_exists",
      value = pbc_if_exists_from_notes(row$notes[[1]])
    )
  }, ignoreInit = TRUE)
  observeEvent(input$pbc_delete, {
    if (!require_admin(is_admin(), session)) return()
    s <- input$pbc_table_rows_selected
    if (is.null(s) || !length(s)) {
      return(showNotification("請先選取要刪除的列", type = "warning"))
    }
    view <- pbc_table_view()
    ids <- view$pbc_id[s]
    reg <- delete_pbc(pbc_reg(), ids)
    pbc_reg(reg)
    persist_pbc(reg)
    refresh_pbc_choices()
  })
  observeEvent(input$upload_pbc, {
    if (!require_admin(is_admin(), session)) return()
    f <- input$upload_pbc
    if (is.null(f)) return()
    tryCatch({
      with_loading({
        reg <- import_pbc_file(
          f$datapath, pbc_reg(),
          original_name = f$name %||% f$datapath
        )
        reg <- enrich_related_pbc_from_specs(reg)
        pbc_reg(reg)
        persist_pbc(reg)
        refresh_pbc_choices()
        showNotification(sprintf("已匯入，共 %d 筆", nrow(reg)), type = "message")
      })
    }, error = function(e) showNotification(conditionMessage(e), type = "error"))
  })
  output$download_pbc_samples <- downloadHandler(
    filename = function() {
      cy <- trimws(input$cycle %||% "")
      cy_label <- if (nzchar(cy)) cy else "全部"
      sprintf("PBC樣本需求清單_%s_%s.xlsx", cy_label, format(Sys.Date(), "%Y%m%d"))
    },
    content = function(file) {
      reg <- pbc_rows_for_export()
      if (!nrow(reg)) stop("無可匯出的 PBC 列")
      with_loading(write_pbc_sample_xlsx(reg, file))
    }
  )
  # 判決書查詢（司法院裁判書爬蟲 + 摘要 + 財務影響）
  judgment_results <- reactiveVal(empty_judgment_results_frame())
  judgment_last_msg <- reactiveVal("")

  observeEvent(input$company, {
    updateTextInput(
      session,
      "judgment_target_company",
      value = trimws(input$company %||% "")
    )
  }, ignoreNULL = FALSE, ignoreInit = FALSE)

  judgment_collect_params <- function() {
    courts <- input$judgment_court %||% character()
    courts <- unique(trimws(as.character(courts)))
    courts <- courts[nzchar(courts)]
    start <- judgment_date_to_form_parts(input$judgment_date_start)
    end <- judgment_date_to_form_parts(input$judgment_date_end)
    list(
      jud_court = courts,
      jud_sys = input$judgment_sys %||% character(),
      jud_year = input$judgment_year,
      jud_case = input$judgment_case,
      jud_no = input$judgment_no,
      jud_no_end = input$judgment_no_end,
      dy1 = start$dy, dm1 = start$dm, dd1 = start$dd,
      dy2 = end$dy, dm2 = end$dm, dd2 = end$dd,
      jud_title = input$judgment_title,
      jud_jmain = input$judgment_jmain,
      jud_kw = input$judgment_kw,
      KbStart = input$judgment_kb_start,
      KbEnd = input$judgment_kb_end,
      max_results = input$judgment_max_results,
      result_url = input$judgment_result_url
    )
  }

  output$judgment_rules_status <- renderUI({
    tags$div(
      class = "small text-muted pt-1",
      judgment_rules_summary_text(data_dir = data_dir)
    )
  })

  observeEvent(input$judgment_learn_rules, {
    up <- input$judgment_history_xlsx
    if (is.null(up) || is.null(up$datapath) || !length(up$datapath)) {
      showNotification("請先選擇歷史分析 xlsx 檔", type = "warning")
      return()
    }
    merged <- tryCatch(
      judgment_update_rules_from_xlsx(up$datapath[[1]], data_dir = data_dir),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = 10)
        NULL
      }
    )
    if (is.null(merged)) return()
    n <- length(merged$rules %||% list())
    showNotification(sprintf("判斷規則已更新（共 %d 條）", n), type = "message")
  }, ignoreInit = TRUE)

  observeEvent(input$judgment_run, {
    params <- judgment_collect_params()
    target <- trimws(input$judgment_target_company %||% input$company %||% "")
    res <- NULL
    withProgress(message = "裁判書查詢中…", value = 0, {
      res <- tryCatch(
        judgment_crawl(
          params,
          target_company = target,
          data_dir = data_dir,
          progress_cb = function(msg) {
            incProgress(0.05, detail = msg)
          }
        ),
        error = function(e) {
          showNotification(conditionMessage(e), type = "error", duration = 10)
          NULL
        }
      )
    })
    if (is.null(res)) return()
    judgment_results(res)
    judgment_last_msg(sprintf(
      "完成：共 %d 筆%s",
      nrow(res),
      if (nzchar(target)) sprintf("（標的公司：%s）", target) else ""
    ))
    showNotification(
      if (nrow(res)) sprintf("判決書分析完成（%d 筆）", nrow(res)) else "查無符合條件之判決書",
      type = if (nrow(res)) "message" else "warning"
    )
  }, ignoreInit = TRUE)

  output$judgment_status_box <- renderUI({
    msg <- judgment_last_msg()
    if (!nzchar(msg)) msg <- "尚未執行查詢"
    tags$div(class = "small text-muted pt-4", msg)
  })

  output$judgment_table <- renderDT({
    df <- judgment_results()
    if (!nrow(df)) {
      return(datatable(
        empty_judgment_results_frame(),
        rownames = FALSE, width = "100%",
        options = dt_loading_opts(
          pageLength = 10,
          ordering = FALSE,
          emptyTable = "尚無結果；請設定查詢條件後按「開始爬取並分析」。"
        )
      ))
    }
    show <- df[, setdiff(names(df), "全文"), drop = FALSE]
    datatable(
      show, rownames = FALSE, width = "100%",
      options = dt_loading_opts(pageLength = 10, scrollX = TRUE, ordering = FALSE),
      escape = FALSE
    )
  })

  output$download_judgment <- downloadHandler(
    filename = function() {
      co <- trimws(input$judgment_target_company %||% input$company %||% "查詢")
      sprintf("判決書分析_%s_%s.xlsx", co, format(Sys.Date(), "%Y%m%d"))
    },
    content = function(file) {
      df <- judgment_results()
      if (!nrow(df)) stop("尚無判決書分析結果可下載")
      with_loading(
        write_judgment_xlsx(
          df, judgment_collect_params(), path = file,
          target_company = trimws(input$judgment_target_company %||% input$company %||% "")
        )
      )
    }
  )

  # RCM / worksheets (訪談問項、自我評估測試步驟)
  output$rcm_table <- renderDT({
    df <- rcm_display_df()
    if (is.null(df)) df <- empty_rcm_display_df()
    # 無資料仍顯示 RCM 標題列（欄名）；提示改由上方 rcm_count_box
    dt <- datatable(
      df, rownames = FALSE, width = "100%",
      options = dt_loading_opts(
        pageLength = 15,
        ordering = FALSE,
        autoWidth = FALSE,
        emptyTable = "尚無 RCM 列；於「風險控制點設計」各區塊按「儲存」，或完成設計後「寫入 RCM 一列」。",
        rowCallback = DT::JS(
          "function(row, data, index) {",
          "  var api = $(row).closest('table').DataTable();",
          "  var draftFirst = api.rows().count() > 0 && (api.row(0).data()[0] === '' || api.row(0).data()[0] == null);",
          "  var hi = draftFirst ? 1 : 0;",
          "  if (index === hi) $(row).css({'background-color': '#e8f5e9', 'font-weight': '500'});",
          "}"
        )
      )
    )
    dt
  })

  observeEvent(rcm_revision(), {
    df <- rcm_display_df()
    if (is.null(df)) df <- empty_rcm_display_df()
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
    updateSelectizeInput(
      session, "inputs",
      selected = expand_pbc_selection(sc$inputs %||% "", isolate(pbc_reg()))
    )
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
      inputs = resolve_multi_pbc_text(input$inputs %||% character(), pbc_reg()),
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
    reset_interview_5w1h_fields()
  })
  observeEvent(input$ws_select_full_iv, {
    updateCheckboxGroupInput(
      session, "interview_elements",
      selected = unique(c(DEFAULT_INTERVIEW_ELEMENTS, INTERVIEW_WALKTHROUGH_EXTRA))
    )
    reset_interview_5w1h_fields()
  })
  observeEvent(input$ws_reset_iv, {
    clear_interview_choices_cache()
    updateSelectInput(session, "interview_sub", selected = "")
    interview_sub_ui_state$cycle <- NULL
    updateSelectizeInput(session, "interview_risk_pick", selected = character())
    updateSelectizeInput(session, "interview_control_pick", selected = character())
    updateCheckboxGroupInput(session, "interview_elements", selected = DEFAULT_INTERVIEW_ELEMENTS)
    reset_interview_5w1h_fields()
  })
  observeEvent(input$ws_select_core_csa, {
    updateCheckboxGroupInput(session, "csa_elements", selected = DEFAULT_CSA_ELEMENTS)
  })
  output$interview_status_steps <- renderUI({
    steps <- c(
      sprintf("循環（側邊欄）：%s", if (nzchar(input$cycle %||% "")) "✓" else "○"),
      sprintf("①子作業：%s", if (nzchar(input$interview_sub %||% "")) "✓" else "○"),
      sprintf("②風險：%s", if (length(input$interview_risk_pick %||% character())) "✓" else "○（全部）"),
      sprintf("③控制點：%s", if (length(input$interview_control_pick %||% character())) "✓" else "○（全部）")
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
    scoped <- interview_scoped_controls()
    if (!length(scoped)) {
      return(tagList(
        tags$small(class = "text-muted", paste(steps, collapse = "｜")),
        tags$br(),
        tags$small(class = "text-warning",
                   "此子作業尚無建議列；可改選其他子作業，或至「風險控制點設計」新增後再訪談。")
      ))
    }
    tags$small(class = "text-muted", paste(steps, collapse = "｜"))
  })
  output$interview_status_summary <- renderText({
    if (!nzchar(input$cycle %||% "") || !nzchar(input$interview_sub %||% "")) {
      return("")
    }
    scoped <- interview_scoped_controls()
    if (!length(scoped)) return("")
    iv <- interview_worksheet()
    sprintf("%d 點 → 訪談問項 %d 則｜人事時地物回答架構", length(scoped), nrow(iv))
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
    req(identical(input$main_nav, "訪談問項設計"))
    req(input$interview_preview_open)
    df <- tryCatch(
      interview_preview_df(interview_worksheet()),
      error = function(e) empty_interview_df()
    )
    datatable(df, rownames = FALSE, options = dt_loading_opts(scrollX = TRUE))
  })
  output$csa_table <- renderDT({
    req(identical(input$main_nav, "控制點測試設計"))
    req(input$csa_preview_open)
    datatable(controls_to_csa(selected_worksheet_controls_sa(), input$csa_elements,
                              finalized_only = TRUE),
              rownames = FALSE, options = dt_loading_opts())
  })
  output$gap_table <- renderDT({
    rcm_revision()
    datatable(detect_gaps_many(controls()), rownames = FALSE,
              options = dt_loading_opts(pageLength = 8))
  })

  # ---- Button gates：條件未達成則不可按（避免單獨執行失敗）----
  observe({
    admin <- isTRUE(is_admin())
    sel <- tryCatch(resolve_cascade_selection(), error = function(e) list())
    cascade_ok <- isTRUE(tryCatch(cascade_selection_ready(sel)$ready, error = function(e) FALSE))
    draft <- tryCatch(current_draft_from_inputs(), error = function(e) list())
    design_ok <- isTRUE(tryCatch(design_required_check(draft)$ok, error = function(e) FALSE))
    oa_ok <- isTRUE(tryCatch(
      rcm_objective_activity_check(draft$control_objective, draft$control_activity)$ok,
      error = function(e) FALSE
    ))
    can_finalize <- cascade_ok && design_ok && oa_ok &&
      isTRUE(tryCatch(activity_type_ok(sel$approach), error = function(e) FALSE))

    risk_cat <- trimws(as.character(
      input$risk_category %||% sel$risk_category %||% ""
    ))
    reporting <- is_reporting_risk_category(risk_cat)

    lib_picked <- nzchar(trimws(input$lib_pick %||% ""))
    lib_row <- length(input$lib_table_rows_selected %||% integer()) > 0
    param_row <- length(input$param_table_rows_selected %||% integer()) > 0
    pbc_row <- length(input$pbc_table_rows_selected %||% integer()) > 0
    n_lib <- length(lib())
    n_ctrl <- length(controls())
    n_ready <- length(Filter(function(c) {
      isTRUE((c$rcm_ready$ready %||% is_rcm_row_ready(c)$ready))
    }, controls()))
    csa_ctrl <- tryCatch(csa_edit_ctrl(), error = function(e) NULL)
    has_csa_ctrl <- !is.null(csa_ctrl)
    n_csa_sc <- if (has_csa_ctrl) length(csa_ctrl$csa_scenarios %||% list()) else 0L
    csa_name_ok <- nzchar(trimws(input$csa_scenario_name %||% ""))
    admin_lib_id <- nzchar(trimws(input$admin_lib_id %||% ""))
    param_fields_ok <- nzchar(trimws(input$admin_param_name %||% "")) &&
      nzchar(trimws(input$admin_param_value %||% ""))
    pbc_fields_ok <- nzchar(trimws(input$pbc_client %||% "")) ||
      nzchar(trimws(input$pbc_reviewed %||% ""))

    iv_n <- tryCatch(nrow(interview_worksheet()), error = function(e) 0L)
    csa_n <- tryCatch(
      nrow(controls_to_csa(selected_worksheet_controls_sa(),
                           input$csa_elements %||% DEFAULT_CSA_ELEMENTS,
                           finalized_only = TRUE)),
      error = function(e) 0L
    )
    n_param <- nrow(param_store())
    pbc_view_n <- tryCatch(nrow(pbc_table_view()), error = function(e) 0L)
    judgment_n <- tryCatch(nrow(judgment_results()), error = function(e) 0L)

    gate <- function(id, ok, tip_off) {
      set_action_button(session, id, ok, if (isTRUE(ok)) "" else tip_off)
    }

    # 風險控制點設計
    gate("finalize_rcm_row", can_finalize,
         "需完成引導②～⑥且設計必填／目標活動分欄通過後才可定稿")
    gate("collect_ready_to_lib", admin, "需高權登入後才可累積範本庫")

    # 控制點測試設計（CSA）
    gate("csa_scenario_add", has_csa_ctrl, "請先選擇已定版控制點")
    gate("csa_scenario_save", has_csa_ctrl && csa_name_ok,
         if (!has_csa_ctrl) "請先選擇已定版控制點" else "請填寫控制現況情境名稱")
    gate("csa_scenario_del", has_csa_ctrl && n_csa_sc > 1L,
         if (!has_csa_ctrl) "請先選擇已定版控制點"
         else "至少保留一組情境（目前無可刪）")

    # 範本庫
    gate("apply_lib", lib_picked, "請先選擇範本")
    gate("apply_lib_selected_row", lib_row, "請先在表格選取一列範本")
    gate("lib_delete", admin && lib_row,
         if (!admin) "需高權登入" else "請選取範本列")
    gate("lib_add_current", admin, "需高權登入")
    gate("lib_add_all_ready", admin && n_ready > 0L,
         if (!admin) "需高權登入" else "尚無 RCM 就緒控制點")
    gate("admin_lib_load_row", admin && lib_row,
         if (!admin) "需高權登入" else "請先選取範本列")
    gate("admin_lib_save_fields", admin && admin_lib_id,
         if (!admin) "需高權登入" else "請先載入選取列")

    # 參數庫
    gate("param_apply_row", param_row, "請先在表格選取一列")
    gate("param_refresh", admin, "需高權登入")
    gate("admin_param_upsert", admin && param_fields_ok,
         if (!admin) "需高權登入" else "請填寫參數名稱與選項值")
    gate("admin_param_delete", admin && param_row,
         if (!admin) "需高權登入" else "請先選取參數列")

    # PBC
    gate("pbc_add", pbc_fields_ok,
         "請至少填「客戶原名」或「檢視後命名」")
    gate("pbc_delete", admin && pbc_row,
         if (!admin) "需高權登入" else "請先選取 PBC 列")
    gate("download_pbc_samples", pbc_view_n > 0L, "目前表內無 PBC 可匯出")
    gate("download_judgment", judgment_n > 0L, "尚無判決書分析結果可下載")

    # 下載（無資料時不可按）
    gate("download_interview", iv_n > 0L, "尚無訪談題綱可下載")
    gate("download_csa", csa_n > 0L, "尚無已定版控制點測試步驟可下載")
    gate("download_rcm", n_ctrl > 0L, "尚無 RCM 列可下載")
    gate("download_lib_json", n_lib > 0L, "範本庫尚無資料")
    gate("download_params", n_param > 0L, "參數庫尚無資料")
    gate("download_params_json", n_param > 0L, "參數庫尚無資料")

    # 高權登出僅登入後可按
    gate("admin_logout", admin, "尚未登入高權")
  })

  output$download_rcm <- downloadHandler(
    filename = function() sprintf("RCM-%s.xlsx", format(Sys.Date(), "%Y%m%d")),
    content = function(file) with_loading(
      write_rcm_xlsx(controls_to_rcm(controls()), file)
    )
  )
  output$download_interview <- downloadHandler(
    filename = function() sprintf("interview-%s.csv", format(Sys.time(), "%Y%m%d")),
    content = function(file) {
      with_loading(
        write.csv(interview_worksheet(),
                  file, row.names = FALSE, fileEncoding = "UTF-8")
      )
    }
  )
  output$download_csa <- downloadHandler(
    filename = function() sprintf("self-assessment-%s.csv", format(Sys.time(), "%Y%m%d")),
    content = function(file) {
      with_loading(
        write.csv(controls_to_csa(selected_worksheet_controls_sa(), input$csa_elements,
                                  finalized_only = TRUE),
                  file, row.names = FALSE, fileEncoding = "UTF-8")
      )
    }
  )
}

shinyApp(ui, server)