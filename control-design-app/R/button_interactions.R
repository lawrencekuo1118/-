# 按鈕互動關係說明（維護檔）
# 每次新增／移除按鈕、Gate 或跨頁流程時，請同步更新本檔與 app.R 內 gate() 區塊。

app_version_label <- function(root = ".") {
  vf <- file.path(root, "VERSION")
  if (file.exists(vf)) {
    trimws(readLines(vf, n = 1L, warn = FALSE))
  } else {
    "—"
  }
}

# 欄位：id, label, page, prereq, effect, gate
button_interaction_registry <- function() {
  list(
    list(
      title = "全域前提",
      rows = list(
        list(id = "—", label = "側邊欄「循環」",
             page = "全 App", prereq = "—",
             effect = "訪談／風險控制點設計／定稿 RCM 必填；PBC 資料庫可直接查閱",
             gate = "—"),
        list(id = "—", label = "高權登入狀態",
             page = "全 App", prereq = "admin_login 驗證",
             effect = "解鎖範本庫寫入、參數庫維護、設計頁「儲存→資料庫」",
             gate = "—"),
        list(id = "—", label = "表格選取",
             page = "多頁", prereq = "點選 DataTable 列",
             effect = "啟用刪除／套用／載入等依賴選列的按鈕",
             gate = "—")
      )
    ),
    list(
      title = "高權登入",
      rows = list(
        list(id = "admin_prompt_lib", label = "登入高權", page = "範本庫",
             prereq = "未登入", effect = "彈出密碼視窗", gate = "無"),
        list(id = "admin_prompt_param", label = "登入高權", page = "參數庫",
             prereq = "未登入", effect = "彈出密碼視窗", gate = "無"),
        list(id = "admin_login", label = "登入", page = "彈窗",
             prereq = "正確密碼", effect = "is_admin=TRUE；顯示高權維護面板與本說明",
             gate = "無"),
        list(id = "admin_logout", label = "登出", page = "側邊欄",
             prereq = "已登入", effect = "is_admin=FALSE", gate = "僅登入後可按")
      )
    ),
    list(
      title = "側邊欄導航",
      rows = list(
        list(id = "goto_lib_tab", label = "開啟範本庫", page = "側邊欄",
             prereq = "—", effect = "跳轉範本庫分頁", gate = "無"),
        list(id = "goto_param_tab", label = "開啟參數庫", page = "側邊欄",
             prereq = "—", effect = "跳轉參數庫分頁", gate = "無")
      )
    ),
    list(
      title = "訪談問項設計",
      rows = list(
        list(id = "ws_select_core_iv", label = "深入且快速", page = "訪談",
             prereq = "—", effect = "勾選核心訪談元素＋5W1H", gate = "無"),
        list(id = "ws_select_full_iv", label = "完整走查", page = "訪談",
             prereq = "—", effect = "勾選完整元素（含 Walkthrough 額外項）", gate = "無"),
        list(id = "ws_reset_iv", label = "重設訪談選取", page = "訪談",
             prereq = "—", effect = "清空子作業、控制點、PBC 串接與元素勾選", gate = "無"),
        list(id = "download_interview", label = "下載訪談題綱 CSV", page = "訪談",
             prereq = "訪談表有資料", effect = "匯出 interview_worksheet()", gate = "iv_n>0")
      )
    ),
    list(
      title = "風險控制點設計",
      rows = list(
        list(id = "preview_rcm_basic", label = "儲存（基礎設定）", page = "設計",
             prereq = "側邊欄已選循環", effect = "合併至 RCM 預覽草稿列", gate = "執行時檢查循環"),
        list(id = "preview_rcm_risk", label = "儲存（風險辨識）", page = "設計",
             prereq = "側邊欄已選循環", effect = "合併至 RCM 預覽草稿列", gate = "執行時檢查循環"),
        list(id = "preview_rcm_control", label = "儲存（控制設計）", page = "設計",
             prereq = "側邊欄已選循環", effect = "合併至 RCM 預覽草稿列", gate = "執行時檢查循環"),
        list(id = "account_select_all", label = "全部適用", page = "設計",
             prereq = "風險類別＝報導面", effect = "展開全部會計科目", gate = "僅報導面"),
        list(id = "goto_pbc_tab", label = "開啟 PBC 資料庫", page = "設計",
             prereq = "—", effect = "跳轉 PBC 分頁", gate = "無"),
        list(id = "finalize_rcm_row", label = "完成設計＝寫入 RCM 一列", page = "設計",
             prereq = "循環＋引導②～⑥完整＋目標活動分欄通過",
             effect = "寫入 controls()；跳轉 RCM；可自動入範本庫（高權＋勾選）",
             gate = "can_finalize"),
        list(id = "collect_ready_to_lib", label = "儲存→資料庫", page = "設計",
             prereq = "高權", effect = "收集就緒控制點入範本庫", gate = "需高權")
      )
    ),
    list(
      title = "控制點測試設計（CSA）",
      rows = list(
        list(id = "ws_select_core_csa", label = "自我評估核心元素", page = "CSA",
             prereq = "—", effect = "勾選 CSA 核心測試元素", gate = "無"),
        list(id = "csa_scenario_add", label = "新增情境組", page = "CSA",
             prereq = "已定版控制點", effect = "新增 CSA 情境", gate = "has_csa_ctrl"),
        list(id = "csa_scenario_save", label = "儲存此情境組", page = "CSA",
             prereq = "已定版控制點＋情境名稱", effect = "寫入控制點 csa_scenarios",
             gate = "控制點＋名稱"),
        list(id = "csa_scenario_del", label = "刪除此情境組", page = "CSA",
             prereq = "已定版控制點；情境數>1", effect = "刪除目前情境", gate = "至少保留1組"),
        list(id = "download_csa", label = "下載 CSA CSV", page = "CSA",
             prereq = "有測試步驟", effect = "匯出 CSA 表", gate = "csa_n>0")
      )
    ),
    list(
      title = "RCM",
      rows = list(
        list(id = "download_rcm", label = "下載 RCM CSV", page = "RCM",
             prereq = "有控制點", effect = "匯出 controls_to_rcm()", gate = "n_ctrl>0")
      )
    ),
    list(
      title = "PBC 資料庫",
      rows = list(
        list(id = "pbc_add", label = "登錄", page = "PBC",
             prereq = "客戶原名或檢視後命名", effect = "upsert_pbc；更新表與命名對照",
             gate = "欄位 Gate"),
        list(id = "pbc_delete", label = "刪除", page = "PBC",
             prereq = "PBC 表選列", effect = "刪除該筆", gate = "需選列"),
        list(id = "pbc_apply_to_design", label = "套用至控制設計", page = "PBC",
             prereq = "pbc_apply 有選項；不可混批政策／非政策",
             effect = "寫入 IUC 或相關政策與制度；可寫入 CSA Inputs", gate = "執行時檢查"),
        list(id = "download_pbc", label = "匯出 CSV", page = "PBC",
             prereq = "庫有資料", effect = "下載全庫", gate = "n_pbc>0"),
        list(id = "—", label = "PBC 表選列", page = "PBC",
             prereq = "點選列", effect = "回填 PBC 增列表單", gate = "—")
      )
    ),
    list(
      title = "範本庫",
      rows = list(
        list(id = "apply_lib", label = "套用選取範本", page = "範本庫",
             prereq = "下拉已選範本", effect = "填入設計表單→跳轉設計頁", gate = "需選範本"),
        list(id = "apply_lib_selected_row", label = "套用表格列", page = "範本庫",
             prereq = "lib_table 選列", effect = "同上", gate = "需選列"),
        list(id = "download_lib_csv", label = "匯出 CSV", page = "範本庫",
             prereq = "庫有資料", effect = "唯讀下載", gate = "n_lib>0"),
        list(id = "download_lib_json", label = "匯出 JSON", page = "範本庫",
             prereq = "庫有資料", effect = "唯讀下載", gate = "n_lib>0"),
        list(id = "admin_lib_load_row", label = "載入選取列", page = "範本庫",
             prereq = "高權＋選列", effect = "載入高權編輯表單", gate = "高權＋選列"),
        list(id = "admin_lib_save_fields", label = "儲存範本變更", page = "範本庫",
             prereq = "高權＋已載入 library_id", effect = "更新範本庫", gate = "高權＋有 ID"),
        list(id = "lib_add_current", label = "表單→庫", page = "範本庫",
             prereq = "高權", effect = "目前設計表單存入庫", gate = "需高權"),
        list(id = "lib_add_all_ready", label = "全部就緒→庫", page = "範本庫",
             prereq = "高權＋RCM 就緒控制點", effect = "批次入庫", gate = "高權＋有就緒列"),
        list(id = "lib_delete", label = "刪除選取", page = "範本庫",
             prereq = "高權＋選列", effect = "刪除範本", gate = "高權＋選列"),
        list(id = "upload_lib", label = "匯入檔案", page = "範本庫",
             prereq = "高權", effect = "合併入庫", gate = "require_admin")
      )
    ),
    list(
      title = "參數庫",
      rows = list(
        list(id = "param_apply_row", label = "套用選取列至表單", page = "參數庫",
             prereq = "param_table 選列", effect = "依參數類型寫入設計欄位", gate = "需選列"),
        list(id = "download_params", label = "下載 CSV", page = "參數庫",
             prereq = "庫有資料", effect = "唯讀下載", gate = "n_param>0"),
        list(id = "download_params_json", label = "下載 JSON", page = "參數庫",
             prereq = "庫有資料", effect = "唯讀下載", gate = "n_param>0"),
        list(id = "admin_param_upsert", label = "新增／更新列", page = "參數庫",
             prereq = "高權＋參數名＋值", effect = "寫入參數庫", gate = "高權＋欄位"),
        list(id = "admin_param_delete", label = "刪除選取列", page = "參數庫",
             prereq = "高權＋選列", effect = "刪除參數列", gate = "高權＋選列"),
        list(id = "param_refresh", label = "從現況重建並儲存", page = "參數庫",
             prereq = "高權", effect = "重建參數庫", gate = "需高權"),
        list(id = "—", label = "表格 SCHEMA 說明", page = "參數庫",
             prereq = "高權", effect = "顯示各 DataTable 欄位順序與來源對照",
             gate = "僅高權可見")
      )
    )
  )
}

button_interactions_card_ui <- function(version = app_version_label()) {
  sections <- button_interaction_registry()
  section_blocks <- lapply(sections, function(sec) {
    rows <- sec$rows
    tbl <- tags$table(
      class = "table table-sm table-bordered button-guide-table mb-3",
      tags$thead(
        tags$tr(
          tags$th("ID"), tags$th("按鈕"), tags$th("分頁"),
          tags$th("前置"), tags$th("效果"), tags$th("Gate")
        )
      ),
      tags$tbody(
        lapply(rows, function(r) {
          tags$tr(
            tags$td(tags$code(r$id %||% "—")),
            tags$td(r$label %||% "—"),
            tags$td(r$page %||% "—"),
            tags$td(r$prereq %||% "—"),
            tags$td(r$effect %||% "—"),
            tags$td(r$gate %||% "—")
          )
        })
      )
    )
    tags$div(
      tags$h6(class = "fw-bold mt-2 mb-1", sec$title),
      tbl
    )
  })
  card(
    class = "button-guide-card",
    card_header(
      "高權：按鈕互動關係（維護說明）",
      tags$span(class = "badge bg-secondary ms-2", sprintf("v%s", version))
    ),
    tags$p(
      class = "small text-muted mb-2",
      "本區僅高權可見。按鈕／Gate 變更時請同步更新 ",
      tags$code("R/button_interactions.R"),
      " 與 ",
      tags$code("app.R"),
      " 內 gate() 區塊。"
    ),
    section_blocks
  )
}
