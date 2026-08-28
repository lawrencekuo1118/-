# Persistent accumulative library of well-designed control points.
# Prefer selecting from library when designing new controls.

if (!exists("%||%", mode = "function", inherits = TRUE)) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

LIBRARY_CONTROL_FIELDS <- c(
  "library_id", "title", "cycle", "sub_process_id", "sub_process",
  "risk_principle", "risk_area",
  "risk_factor", "risk_name", "risk_description", "risk_category",
  "risk_attr_financial", "risk_attr_operations", "risk_attr_compliance",
  "romm_classification", "significant_account", "assertions",
  "control_objective", "control_activity", "frequency", "responsible_unit",
  "iuc_or_system", "related_system", "related_policy", "related_law",
  "related_law_url",
  "related_documents", "related_document",
  "related_document_pbc_ids",
  "company_status", "design_gap_note", "effectiveness", "residual_risk", "improvement",
  "nature", "approach", "type",
  "inputs", "review_steps", "outputs", "investigation_threshold",
  "dependent_controls", "detailed_description", "summary_description", "control_id"
)

# 非控制點「設計」欄位：輸入檔／既有 RCM 常帶入，不可污染 APP 範本庫／參數庫
# （對應 RCM「控制現況描述／差異說明」與「控制分析與評估」群組）
NON_DESIGN_CONTROL_FIELDS <- c(
  "company_status",      # 控制現況描述／公司現況
  "design_gap_note",     # 控制設計差異說明
  "effectiveness",       # 控制有效性評估
  "residual_risk",       # 可能潛在風險
  "improvement"          # 建議改善方式
)

# 執行階段組出的編號：僅設計頁／RCM 使用，不寫入範本庫
RUNTIME_ID_CONTROL_FIELDS <- c("sub_process_id", "control_id")

NON_PERSIST_CONTROL_ALIASES <- c(
  "control_status", "status_description", "design_gap", "gap_analysis",
  "控制現況描述", "控制設計差異說明", "控制有效性評估",
  "可能潛在風險", "建議改善方式", "現況描述", "差異說明",
  "差異缺失", "缺失說明", "有效性評估", "潛在風險", "改善建議",
  "改善方式", "建議改善", "公司控制現況", "公司現況", "控制現況",
  "改善計畫", "改善計劃", "分析評估", "控制分析與評估"
)

# 參數庫禁止收錄的參數名（與上列意義雷同）
PARAMETER_STORE_BLOCKED_PARAMS <- c(
  "子作業編號", "控制編號",
  "控制現況描述", "控制設計差異說明", "控制有效性評估",
  "可能潛在風險", "建議改善方式",
  "公司控制現況", "公司現況", "控制現況", "現況描述",
  "差異說明", "差異缺失", "缺失說明",
  "有效性評估", "潛在風險",
  "改善建議", "改善方式", "建議改善", "改善計畫", "改善計劃",
  "分析評估", "控制分析與評估"
)

is_blocked_parameter_name <- function(param) {
  p <- trimws(as.character(param %||% ""))
  if (!nzchar(p)) return(TRUE)
  if (p %in% PARAMETER_STORE_BLOCKED_PARAMS) return(TRUE)
  grepl("現況|改善建議|改善方式|建議改善|有效性評估|潛在風險|差異說明|差異缺失", p)
}

strip_runtime_id_fields <- function(ctrl) {
  ctrl <- as.list(ctrl)
  for (f in RUNTIME_ID_CONTROL_FIELDS) {
    ctrl[[f]] <- ""
  }
  ctrl
}

strip_non_design_control_fields <- function(ctrl) {
  ctrl <- as.list(ctrl)
  for (f in NON_DESIGN_CONTROL_FIELDS) {
    ctrl[[f]] <- ""
  }
  # 匯入檔常把「控制現況描述」誤塞進 detailed_description；設計庫一律清空後由組裝函式重建
  ctrl$detailed_description <- ""
  # 常見別名一併清空（含差異缺失／現況相關欄）
  for (alias in NON_PERSIST_CONTROL_ALIASES) {
    if (!is.null(ctrl[[alias]])) ctrl[[alias]] <- ""
  }
  ctrl
}

# 寫入／回傳前拿掉編號與非設計欄（不重清 detailed_description，以免刪掉組裝結果）
drop_non_persist_control_fields <- function(ctrl) {
  ctrl <- as.list(ctrl)
  drop <- unique(c(NON_DESIGN_CONTROL_FIELDS, RUNTIME_ID_CONTROL_FIELDS,
                   NON_PERSIST_CONTROL_ALIASES, "company"))
  for (f in drop) ctrl[[f]] <- NULL
  ctrl
}

# 範本鍵依內容特徵穩定累積（不含子作業／控制編號）
library_content_id <- function(ctrl) {
  rf <- trimws(as.character(ctrl$risk_factor %||% ctrl$risk_name %||% ""))
  if (exists("format_risk_factor_text", mode = "function")) {
    rf <- format_risk_factor_text(rf)
  }
  raw <- paste(c(
    trimws(as.character(ctrl$cycle %||% "")),
    trimws(as.character(ctrl$sub_process %||% "")),
    rf,
    trimws(as.character(ctrl$risk_description %||% "")),
    trimws(as.character(ctrl$control_objective %||% "")),
    trimws(as.character(ctrl$control_activity %||% "")),
    trimws(as.character(ctrl$iuc %||% ctrl$iuc_or_system %||% ""))
  ), collapse = "|")
  sprintf("LIB-%08x", sum(utf8ToInt(enc2utf8(raw))) %% as.integer(1e8))
}

# 企業專屬用語／文件編號／系統商品名 → 通用表述（入庫前去識別）
CLIENT_NAME_MARKERS <- c(
  "輝能科技", "ProLogium", "prologium", "PROLOGIUM",
  "鯨鏈科技", "鯨鏈RCM", "鯨鏈", "Jinglian", "jinglian"
)

is_client_identifying_tag <- function(tag) {
  tg <- trimws(as.character(tag %||% ""))
  if (!nzchar(tg)) return(FALSE)
  if (tg %in% CLIENT_NAME_MARKERS) return(TRUE)
  grepl("輝能|ProLogium|prologium|鯨鏈|Jinglian", tg, ignore.case = TRUE)
}

deidentify_client_specific_text <- function(text) {
  s <- as.character(text %||% "")
  if (!nzchar(s)) return(s)
  # 公司／集團名
  s <- gsub("輝能科技", "本公司", s, fixed = TRUE)
  s <- gsub("(?i)ProLogium", "集團", s, perl = TRUE)
  s <- gsub("鯨鏈科技", "本公司", s, fixed = TRUE)
  s <- gsub("鯨鏈RCM", "RCM", s, fixed = TRUE)
  s <- gsub("(?i)Jinglian", "本公司", s, perl = TRUE)
  s <- gsub("鯨鏈", "本公司", s, fixed = TRUE)
  # 企業內部表單／程序編號（如 A6-004-A、Q2-001、R2-001-I）
  s <- gsub("[（(]\\s*[A-Za-z]{1,3}\\d?[-－]\\d{2,4}(?:[-－][A-Za-z0-9]+)?\\s*[）)]", "", s, perl = TRUE)
  # 企業常用專屬系統商品名 → 通用系統類別
  s <- gsub("(?i)Easy\\s*flow", "電子簽核系統", s, perl = TRUE)
  s <- gsub("(?i)Shop\\s*flow", "生產流程系統", s, perl = TRUE)
  s <- gsub("\\bSPM系統\\b", "流程管理系統", s, perl = TRUE)
  s <- gsub("\\bSPM\\b", "流程管理系統", s, perl = TRUE)
  s <- gsub("\\bSFT\\b", "檔案傳輸系統", s, perl = TRUE)
  s <- gsub("鼎新", "ERP套裝", s, fixed = TRUE)
  # 台灣用語（避免陸／港澳用字進入已提交資料）
  s <- gsub("資料數據", "資料", s, fixed = TRUE)
  s <- gsub("大批量", "大量", s, fixed = TRUE)
  s <- gsub("重覆", "重複", s, fixed = TRUE)
  s <- gsub("系統帳戶", "系統帳號", s, fixed = TRUE)
  s <- gsub("安裝或設置", "安裝或設定", s, fixed = TRUE)
  s <- gsub("應設置密碼", "應設定密碼", s, fixed = TRUE)
  s <- gsub("系統資源配置", "系統資源設定", s, fixed = TRUE)
  s <- gsub("其它類別", "其他類別", s, fixed = TRUE)
  s <- gsub("信息系統", "資訊系統", s, fixed = TRUE)
  s <- gsub("軟件", "軟體", s, fixed = TRUE)
  s <- gsub("網絡", "網路", s, fixed = TRUE)
  s <- gsub("數據庫", "資料庫", s, fixed = TRUE)
  s <- gsub("默認", "預設", s, fixed = TRUE)
  s <- gsub("登录", "登入", s, fixed = TRUE)
  # 清理多餘空白
  s <- gsub("[ \\t]{2,}", " ", s)
  s <- gsub(" *\r?\n *", "\n", s)
  trimws(s)
}

deidentify_control_fields <- function(ctrl) {
  ctrl <- as.list(ctrl)
  # 非設計欄＋公司名一併清掉
  if (exists("strip_non_design_control_fields", mode = "function")) {
    ctrl <- strip_non_design_control_fields(ctrl)
  }
  ctrl$company <- ""
  text_keys <- c(
    "title", "risk_factor", "risk_name", "risk_description",
    "risk_attr_financial", "risk_attr_operations", "risk_attr_compliance",
    "control_objective", "control_activity", "responsible_unit",
    "iuc", "iuc_or_system", "related_system", "related_policy", "related_law",
    "related_law_url",
    "related_document", "inputs", "review_steps", "outputs",
    "investigation_threshold", "dependent_controls",
    "detailed_description", "summary_description", "sub_process"
  )
  for (k in text_keys) {
    if (!is.null(ctrl[[k]]) && is.character(ctrl[[k]])) {
      ctrl[[k]] <- deidentify_client_specific_text(ctrl[[k]])
    }
  }
  # 敘述含公司名時強制以去識別後欄位重建（避免殘留）
  ctrl$detailed_description <- ""
  ctrl$summary_description <- ""
  ctrl
}

deidentify_library_item <- function(item) {
  item <- as.list(item)
  if (!is.null(item$control)) {
    item$control <- deidentify_control_fields(item$control)
  }
  if (!is.null(item$title)) {
    item$title <- deidentify_client_specific_text(item$title)
  }
  # 標籤去掉企業名／企業批次標
  if (!is.null(item$tags)) {
    tg <- as.character(unlist(item$tags, use.names = FALSE))
    tg <- tg[!vapply(tg, is_client_identifying_tag, logical(1))]
    tg <- vapply(tg, deidentify_client_specific_text, character(1), USE.NAMES = FALSE)
    tg <- unique(c(tg[nzchar(tg)], "去識別範本"))
    item$tags <- tg
  }
  # 來源改為中性鍵（保留 PL-/JL- 編號前綴供追蹤）
  src <- as.character(item$source %||% "")
  if (grepl("prologium|輝能|jinglian|鯨鏈", src, ignore.case = TRUE)) {
    item$source <- "rcm_import_batch"
  }
  # 重建組裝敘述（公司欄已空 →「就公司現行…」）
  ctrl <- item$control %||% list()
  if (exists("assemble_summary_description", mode = "function")) {
    ctrl$summary_description <- tryCatch(
      assemble_summary_description(ctrl), error = function(e) ctrl$title %||% ""
    )
  }
  if (exists("assemble_control_paragraph", mode = "function")) {
    ctrl$detailed_description <- tryCatch(
      assemble_control_paragraph(ctrl), error = function(e) ""
    )
  } else if (exists("assemble_detailed_description", mode = "function")) {
    ctrl$detailed_description <- tryCatch(
      assemble_detailed_description(ctrl), error = function(e) ""
    )
  }
  item$control <- ctrl
  item
}

seed_control_library <- function(include_jinglian_batch = TRUE) {
  base <- list(
    library_item_from_control(list(
      library_id = "LIB-REV-CUTOFF-01",
      title = "銷售截止覆核（銷貨日報表）",
      cycle = "銷售及收款循環",
      risk_name = "收入截止錯誤",
      risk_description = "接近期末之出貨可能於不當會計期間認列收入",
      risk_attr_financial = "[財務報導] 營業收入完整性與截止",
      risk_attr_operations = "[營運] 出貨與開票時點不一致",
      risk_attr_compliance = "[法令遵循] 收入認列政策",
      romm_classification = "Significant Risk — Higher risk associated with the control",
      significant_account = "營業收入、應收帳款",
      assertions = "完整性 (Completeness)；截止 (Cutoff)",
      control_objective = "確保出貨交易於適當會計期間認列收入",
      control_activity = "會計人員每日將出貨單與銷貨日報表逐筆核對，差異列入截止調節並呈主管簽核",
      frequency = "每日",
      responsible_unit = "財務部會計課／會計主管",
      iuc_or_system = "銷貨日報表",
      nature = "人工 (Manual)",
      approach = "偵測性 (Detective)",
      type = "含覆核要素之控制 (Controls with a Review Element)",
      inputs = "出貨單、銷貨日報表（ERP 產製）",
      review_steps = "取得當日出貨清單與銷貨日報表\n比對出貨日與發票／認列日\n差異逾1日列入追蹤清單並查明原因\n主管覆核調節表並簽核",
      outputs = "截止調節表、差異追蹤清單、主管簽核紀錄",
      investigation_threshold = "出貨日與認列日差異逾1日",
      dependent_controls = ""
    ), tags = c("銷售", "截止", "Significant Risk"), source = "seed"),
    library_item_from_control(list(
      library_id = "LIB-AP-3WAY-01",
      title = "採購三方比對核准",
      cycle = "採購及付款循環",
      risk_name = "不實或未授權之應付帳款",
      risk_description = "供應商發票可能未經有效驗收或採購依據即入帳付款",
      risk_attr_financial = "[財務報導] 應付帳款存在／發生與正確性",
      risk_attr_operations = "[營運] 採購驗收與付款流程斷點",
      risk_attr_compliance = "[法令遵循] 核決權限表",
      romm_classification = "Significant Risk — Not higher risk associated with the control",
      significant_account = "應付帳款、存貨／費用",
      assertions = "存在或發生 (Existence or Occurrence)；正確性 (Accuracy)",
      control_objective = "僅對經授權且貨物／勞務已收受之交易認列應付帳款",
      control_activity = "應付帳款人員於入帳前執行採購單、驗收單與發票三方比對，不符者不得付款",
      frequency = "每筆交易",
      responsible_unit = "財務部應付帳款／採購單位",
      iuc_or_system = "採購單／驗收單／供應商發票（ERP AP）",
      nature = "人工",
      approach = "預防性 (Preventive)",
      type = "核對驗證 (Verifications)",
      inputs = "採購單、驗收單、供應商發票",
      review_steps = "比對數量與單價\n確認驗收完成\n確認核決權限\n系統過帳或退回供應商",
      outputs = "三方比對紀錄、系統過帳 log、退回文件",
      investigation_threshold = "數量或金額任一不符即暫停付款",
      dependent_controls = "採購核決權限控制"
    ), tags = c("採購", "三方比對"), source = "seed"),
    # 其餘九大循環：內建一筆即可直接選，毋須先匯入底稿
    library_item_from_control(list(
      library_id = "LIB-PR-BOM-01",
      title = "生產｜用料與產出核對",
      cycle = "生產循環",
      sub_process_id = "PR-101", sub_process = "用料管理作業",
      risk_factor = "用料浪費或短溢",
      risk_description = "實際用料與 BOM／工單差異未及時查明，導致成本與存貨不實",
      risk_category = "報導面",
      romm_classification = "Significant Risk — Not higher risk associated with the control",
      significant_account = "存貨、銷貨成本",
      assertions = "存在或發生 (Existence or Occurrence)；正確性 (Accuracy)",
      control_objective = "確保生產用料與產出依核准工單正確記錄",
      control_activity = "生產管理員每日比對工單用料與系統領料，差異逾門檻須呈主管簽核後調整",
      frequency = "每日", responsible_unit = "生產管理／成本會計",
      iuc_or_system = "工單／領料單／BOM",
      nature = "人工", approach = "偵測性 (Detective)",
      type = "含覆核要素之控制 (Controls with a Review Element)"
    ), tags = c("生產", "用料"), source = "seed"),
    library_item_from_control(list(
      library_id = "LIB-PY-PAYROLL-01",
      title = "薪工｜薪資異動核准",
      cycle = "薪工循環",
      sub_process_id = "PY-101", sub_process = "薪資計算作業",
      risk_factor = "未授權薪資異動",
      risk_description = "薪資、職級或加給異動未經適當核准即入薪資系統",
      risk_category = "報導面",
      romm_classification = "Significant Risk — Higher risk associated with the control",
      significant_account = "薪資費用、應付薪資",
      assertions = "發生 (Occurrence)；正確性 (Accuracy)",
      control_objective = "確保薪資異動皆經權責主管核准後才生效",
      control_activity = "人資於過帳前檢核異動單簽核完整，系統僅允許已核准異動寫入薪資主檔",
      frequency = "每筆交易", responsible_unit = "人資／財務",
      iuc_or_system = "薪資異動單／簽核紀錄",
      nature = "人工", approach = "預防性 (Preventive)",
      type = "授權與核准 (Authorizations and Approvals)"
    ), tags = c("薪工"), source = "seed"),
    library_item_from_control(list(
      library_id = "LIB-FN-LOAN-01",
      title = "融資｜借款撥款覆核",
      cycle = "融資循環",
      sub_process_id = "FN-101", sub_process = "借款管理作業",
      risk_factor = "借款條件不符或未入帳",
      risk_description = "借款合約條件與實際撥款／入帳不一致",
      risk_category = "報導面",
      romm_classification = "Significant Risk — Not higher risk associated with the control",
      significant_account = "銀行借款、利息費用",
      assertions = "完整性 (Completeness)；評價或分攤 (Valuation or Allocation)",
      control_objective = "確保借款撥款與合約條件一致並完整入帳",
      control_activity = "財務人員於撥款入帳前核對合約額度、利率與撥款通知，主管覆核後過帳",
      frequency = "每筆交易", responsible_unit = "財務部資金",
      iuc_or_system = "借款合約／撥款通知",
      nature = "人工", approach = "偵測性 (Detective)",
      type = "核對驗證 (Verifications)"
    ), tags = c("融資"), source = "seed"),
    library_item_from_control(list(
      library_id = "LIB-FA-CAPEX-01",
      title = "固定資產｜資本支出核准",
      cycle = "固定資產循環",
      sub_process_id = "FA-101", sub_process = "購置管理作業",
      risk_factor = "未核准資本支出入帳",
      risk_description = "資本支出未經核決權限核准即採購或資本化",
      risk_category = "報導面",
      romm_classification = "Significant Risk — Not higher risk associated with the control",
      significant_account = "不動產廠房及設備",
      assertions = "存在或發生 (Existence or Occurrence)；權利與義務 (Rights and Obligations)",
      control_objective = "確保固定資產購置皆經適當核准並正確資本化",
      control_activity = "資產管理員於請購／驗收入帳前檢核核准層級與金額，不符者不得入帳",
      frequency = "每筆交易", responsible_unit = "資產管理／財務",
      iuc_or_system = "資本支出申請單／驗收單",
      nature = "人工", approach = "預防性 (Preventive)",
      type = "授權與核准 (Authorizations and Approvals)"
    ), tags = c("固定資產"), source = "seed"),
    library_item_from_control(list(
      library_id = "LIB-IV-TRADE-01",
      title = "投資｜交易覆核",
      cycle = "投資循環",
      sub_process_id = "IV-101", sub_process = "投資交易作業",
      risk_factor = "投資交易未授權或不完整",
      risk_description = "投資買賣未經授權或交割結果未完整入帳",
      risk_category = "報導面",
      romm_classification = "Significant Risk — Higher risk associated with the control",
      significant_account = "透過損益按公允價值衡量之金融資產",
      assertions = "存在或發生 (Existence or Occurrence)；完整性 (Completeness)",
      control_objective = "確保投資交易經授權且交割結果完整正確入帳",
      control_activity = "投資管理員比對交易單、券商回報與帳簿，差異須於當日查明並呈主管",
      frequency = "每日", responsible_unit = "投資管理／財務",
      iuc_or_system = "交易單／券商對帳單",
      nature = "人工", approach = "偵測性 (Detective)",
      type = "核對驗證 (Verifications)"
    ), tags = c("投資"), source = "seed"),
    library_item_from_control(list(
      library_id = "LIB-RD-PROJECT-01",
      title = "研發｜專案支出歸屬覆核",
      cycle = "研發循環",
      sub_process_id = "RD-101", sub_process = "研發支出歸屬作業",
      risk_factor = "研發支出歸屬錯誤",
      risk_description = "研發專案成本歸屬錯誤或費用／資本化分類不當",
      risk_category = "報導面",
      romm_classification = "Significant Risk — Not higher risk associated with the control",
      significant_account = "研究發展費用、無形資產",
      assertions = "正確性 (Accuracy)；分類 (Classification)",
      control_objective = "確保研發支出依專案與會計政策正確歸屬",
      control_activity = "會計每月覆核專案工時／費用歸屬表，異常項目標記後由專案主管確認",
      frequency = "每月", responsible_unit = "研發管理／會計",
      iuc_or_system = "專案費用歸屬表",
      nature = "人工", approach = "偵測性 (Detective)",
      type = "含覆核要素之控制 (Controls with a Review Element)"
    ), tags = c("研發"), source = "seed")
  )

  if (!isTRUE(include_jinglian_batch)) return(base)

  # Resolve package/app root from this file when possible
  app_root <- local({
    # walk common locations
    for (p in c(
      getwd(),
      file.path(getwd(), "control-design-app"),
      if (exists("root", inherits = TRUE)) get("root", inherits = TRUE) else NULL
    )) {
      if (is.null(p)) next
      if (file.exists(file.path(p, "templates")) || file.exists(file.path(p, "data"))) {
        return(normalizePath(p))
      }
    }
    normalizePath(getwd())
  })

  out <- base
  batch_json <- file.path(app_root, "data", "jinglian_it_rcm_batch.json")
  if (file.exists(batch_json)) {
    jl <- tryCatch(load_control_library(batch_json, fallback_seed = FALSE), error = function(e) list())
    if (length(jl)) out <- merge_libraries(out, jl, overwrite = TRUE)
  } else {
    xlsx <- file.path(app_root, "templates", "鯨鏈科技_資訊循環_RCM_v1_0820.xlsx")
    if (file.exists(xlsx)) {
      jl <- tryCatch(
        import_rcm_xlsx_as_library(
          xlsx, source = "rcm_import_batch", id_prefix = "JL",
          tags = c("RCM", "資訊循環", "首批")
        ),
        error = function(e) list()
      )
      if (length(jl)) out <- merge_libraries(out, jl, overwrite = TRUE)
    }
  }
  # 輝能科技全循環 RCM 批次
  pl_json <- file.path(app_root, "data", "prologium_rcm_batch.json")
  if (file.exists(pl_json)) {
    pl <- tryCatch(load_control_library(pl_json, fallback_seed = FALSE), error = function(e) list())
    if (length(pl)) out <- merge_libraries(out, pl, overwrite = FALSE)
  }
  out
}

library_item_from_control <- function(ctrl, tags = character(), source = "manual",
                                       deidentify = TRUE) {
  ctrl <- strip_non_design_control_fields(as.list(ctrl))
  if (isTRUE(deidentify)) {
    ctrl <- deidentify_control_fields(ctrl)
  }
  # 包裝鍵：匯入可沿用 JL-/PL-；其餘依內容雜湊。編號本身不入庫。
  if (is.null(ctrl$library_id) || !nzchar(as.character(ctrl$library_id %||% ""))) {
    ctrl$library_id <- library_content_id(ctrl)
  }
  ctrl <- strip_runtime_id_fields(ctrl)
  if (is.null(ctrl$title) || !nzchar(as.character(ctrl$title %||% ""))) {
    ctrl$title <- sprintf(
      "%s｜%s",
      ctrl$cycle %||% "",
      {
        obj <- ctrl$control_objective %||% ""
        if (nzchar(obj)) {
          if (nchar(obj) > 40) paste0(substr(obj, 1, 40), "…") else obj
        } else (ctrl$control_activity %||% "控制")
      }
    )
  } else if (isTRUE(deidentify)) {
    ctrl$title <- deidentify_client_specific_text(ctrl$title)
  }
  if (is.null(ctrl$summary_description) || !nzchar(as.character(ctrl$summary_description %||% ""))) {
    if (exists("assemble_summary_description", mode = "function")) {
      ctrl$summary_description <- tryCatch(assemble_summary_description(ctrl), error = function(e) ctrl$title)
    }
  }
  if (is.null(ctrl$detailed_description) || !nzchar(as.character(ctrl$detailed_description %||% ""))) {
    if (exists("assemble_control_paragraph", mode = "function")) {
      ctrl$detailed_description <- tryCatch(assemble_control_paragraph(ctrl), error = function(e) "")
    }
  }
  tags <- unique(c(as.character(tags), as.character(ctrl$tags %||% character()), "累積範本"))
  tags <- tags[nzchar(tags)]
  if (isTRUE(deidentify)) {
    tags <- tags[!vapply(tags, is_client_identifying_tag, logical(1))]
    tags <- unique(c(vapply(tags, deidentify_client_specific_text, character(1), USE.NAMES = FALSE),
                     "去識別範本"))
    tags <- tags[nzchar(tags)]
  }
  src <- as.character(source %||% "manual")
  if (isTRUE(deidentify) && grepl("prologium|輝能|jinglian|鯨鏈", src, ignore.case = TRUE)) {
    src <- "rcm_import_batch"
  }
  # 記憶體物件亦不保留非設計／編號欄（空字串也不留）
  ctrl <- drop_non_persist_control_fields(ctrl)
  # 公司名一律不入庫
  ctrl$company <- NULL
  list(
    library_id = as.character(ctrl$library_id %||% ""),
    title = as.character(ctrl$title %||% ""),
    tags = tags,
    cycle = as.character(ctrl$cycle %||% ""),
    source = src,
    updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    control = ctrl
  )
}

# Collect one or many designed controls into accumulative library.
# quality_gate: only accept OA-clean + six-rules-ready when TRUE
collect_controls_to_library <- function(library, controls, overwrite = TRUE,
                                        tags = character(), source = "collect",
                                        quality_gate = TRUE) {
  if (!length(controls)) {
    return(list(library = library, added = 0L, updated = 0L, skipped = 0L, items = list()))
  }
  added <- 0L
  updated <- 0L
  skipped <- 0L
  items <- list()
  out <- library
  for (ctrl in controls) {
    if (isTRUE(quality_gate)) {
      oa_ok <- TRUE
      if (exists("rcm_objective_activity_check", mode = "function")) {
        oa_ok <- isTRUE(rcm_objective_activity_check(
          ctrl$control_objective, ctrl$control_activity
        )$ok)
      }
      six_ok <- TRUE
      if (exists("design_required_check", mode = "function")) {
        six_ok <- isTRUE(design_required_check(ctrl)$ok)
      }
      if (!oa_ok || !six_ok) {
        skipped <- skipped + 1L
        next
      }
    }
    item <- library_item_from_control(ctrl, tags = tags, source = source)
    existed <- !is.null(get_library_item(out, item$library_id))
    if (existed && !overwrite) {
      skipped <- skipped + 1L
      next
    }
    out <- upsert_library_item(out, item)
    items[[length(items) + 1]] <- item
    if (existed) updated <- updated + 1L else added <- added + 1L
  }
  list(library = out, added = added, updated = updated, skipped = skipped, items = items)
}

library_stats <- function(library) {
  n <- length(library)
  cycles <- unique(vapply(library, function(x) x$cycle %||% x$control$cycle %||% "", character(1)))
  cycles <- cycles[nzchar(cycles)]
  sources <- table(vapply(library, function(x) x$source %||% "unknown", character(1)))
  list(
    n = n,
    n_cycles = length(cycles),
    cycles = cycles,
    sources = as.list(sources)
  )
}

library_choices <- function(library, cycle_filter = NULL, query = NULL) {
  items <- filter_library(library, cycle_filter = cycle_filter, query = query)
  if (!length(items)) return(character())
  labels <- vapply(items, function(x) {
    sprintf("%s｜%s｜%s", x$library_id %||% "?", x$cycle %||% "", x$title %||% "未命名")
  }, character(1))
  stats::setNames(vapply(items, function(x) x$library_id %||% "", character(1)), labels)
}

get_library_item <- function(library, id) {
  for (item in library) {
    if (identical(item$library_id, id)) return(item)
  }
  NULL
}

filter_library <- function(library, cycle_filter = NULL, query = NULL) {
  items <- library
  if (!is.null(cycle_filter) && nzchar(cycle_filter)) {
    items <- Filter(function(x) identical(x$cycle %||% x$control$cycle %||% "", cycle_filter) ||
                      identical(x$control$cycle %||% "", cycle_filter), items)
  }
  if (!is.null(query) && nzchar(trimws(query))) {
    q <- tolower(trimws(query))
    items <- Filter(function(x) {
      blob <- tolower(paste(
        x$library_id, x$title, x$cycle,
        x$control$risk_name, x$control$control_objective, x$control$control_activity,
        x$control$iuc_or_system, paste(x$tags, collapse = " "),
        sep = " "
      ))
      grepl(q, blob, fixed = TRUE)
    }, items)
  }
  items
}

upsert_library_item <- function(library, item) {
  id <- item$library_id
  found <- FALSE
  for (i in seq_along(library)) {
    if (identical(library[[i]]$library_id, id)) {
      library[[i]] <- item
      found <- TRUE
      break
    }
  }
  if (!found) library[[length(library) + 1]] <- item
  library
}

delete_library_item <- function(library, id) {
  Filter(function(x) !identical(x$library_id, id), library)
}

merge_libraries <- function(base, incoming, overwrite = TRUE) {
  out <- base
  for (item in incoming) {
    exists <- !is.null(get_library_item(out, item$library_id))
    if (!exists || overwrite) out <- upsert_library_item(out, item)
  }
  out
}

library_to_flat_df <- function(library) {
  fields <- setdiff(
    LIBRARY_CONTROL_FIELDS,
    c(NON_DESIGN_CONTROL_FIELDS, RUNTIME_ID_CONTROL_FIELDS)
  )
  if (!length(library)) {
    return(data.frame(matrix(ncol = length(fields) + 2, nrow = 0,
                             dimnames = list(NULL, c(fields, "tags", "updated_at"))),
                      stringsAsFactors = FALSE))
  }
  rows <- lapply(library, function(item) {
    ctrl <- item$control
    vals <- lapply(fields, function(f) as.character(ctrl[[f]] %||% item[[f]] %||% ""))
    names(vals) <- fields
    vals$tags <- paste(item$tags %||% character(), collapse = ";")
    vals$updated_at <- item$updated_at %||% ""
    as.data.frame(vals, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

flat_row_to_library_item <- function(row) {
  row <- as.list(row)
  tags <- unlist(strsplit(as.character(row$tags %||% ""), "[;；,，|/]+"))
  tags <- trimws(tags)
  tags <- tags[nzchar(tags)]
  ctrl <- list()
  for (f in LIBRARY_CONTROL_FIELDS) {
    ctrl[[f]] <- as.character(row[[f]] %||% "")
  }
  if (!nzchar(ctrl$library_id)) ctrl$library_id <- NULL
  if (!nzchar(ctrl$title)) ctrl$title <- NULL
  library_item_from_control(ctrl, tags = tags)
}

save_control_library <- function(library, path_json, path_csv = NULL) {
  payload <- lapply(library, function(item) {
    ctrl <- drop_non_persist_control_fields(item$control %||% list())
    if (!nzchar(trimws(as.character(ctrl$detailed_description %||% ""))) &&
        exists("assemble_control_paragraph", mode = "function")) {
      ctrl$detailed_description <- tryCatch(
        assemble_control_paragraph(ctrl), error = function(e) ""
      )
    }
    list(
      library_id = item$library_id,
      title = item$title,
      tags = item$tags,
      cycle = item$cycle %||% ctrl$cycle %||% "",
      source = item$source %||% "manual",
      updated_at = item$updated_at %||% "",
      control = ctrl
    )
  })
  jsonlite::write_json(payload, path_json, auto_unbox = TRUE, pretty = TRUE, force = TRUE, null = "null")
  if (!is.null(path_csv)) {
    utils::write.csv(library_to_flat_df(library), path_csv, row.names = FALSE, fileEncoding = "UTF-8")
  }
  invisible(path_json)
}

load_persisted_library_item <- function(x) {
  ctrl <- as.list(x$control %||% x)
  list(
    library_id = as.character(x$library_id %||% ctrl$library_id %||% ""),
    title = as.character(x$title %||% ctrl$title %||% ""),
    tags = unlist(x$tags %||% list()),
    cycle = as.character(x$cycle %||% ctrl$cycle %||% ""),
    source = as.character(x$source %||% "persisted"),
    control = ctrl,
    updated_at = as.character(x$updated_at %||% "")
  )
}

is_persisted_library_payload <- function(x) {
  is.list(x) && !is.null(x$control) &&
    nzchar(trimws(as.character(x$library_id %||% "")))
}

load_control_library <- function(path_json, fallback_seed = TRUE, normalize = NULL) {
  if (!file.exists(path_json)) {
    return(if (fallback_seed) seed_control_library() else list())
  }
  raw <- jsonlite::read_json(path_json, simplifyVector = FALSE)
  if (!length(raw)) {
    return(if (fallback_seed) seed_control_library() else list())
  }
  use_fast <- if (is.null(normalize)) {
    all(vapply(raw, is_persisted_library_payload, logical(1)))
  } else {
    !isTRUE(normalize)
  }
  items <- if (use_fast) {
    lapply(raw, load_persisted_library_item)
  } else {
    lapply(raw, function(x) {
      ctrl <- x$control %||% x
      if (is.null(ctrl$library_id)) ctrl$library_id <- x$library_id
      if (is.null(ctrl$title)) ctrl$title <- x$title
      item <- library_item_from_control(
        ctrl,
        tags = unlist(x$tags %||% list()),
        source = x$source %||% "persisted"
      )
      item$updated_at <- x$updated_at %||% item$updated_at
      item
    })
  }
  if (!length(items) && fallback_seed) return(seed_control_library())
  items
}

# Bulk import CSV (flat), JSON, or 鯨鏈-style RCM xlsx (row1 groups + row2 headers)
import_control_library_file <- function(path, existing = list(), overwrite = TRUE) {
  ext <- tolower(tools::file_ext(path))
  incoming <- list()
  if (ext %in% c("xlsx", "xls")) {
    incoming <- import_rcm_xlsx_as_library(path)
  } else if (ext == "csv") {
    df <- utils::read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
    # flexible aliases
    names(df) <- gsub("\\s+", "_", names(df))
    alias_map <- c(
      library_id = "library_id|id|範本id|控制點編號|控制編號",
      title = "title|名稱|範本名稱",
      cycle = "cycle|循環|九大循環|循環名稱",
      sub_process_id = "sub_process_id|子作業編號",
      sub_process = "sub_process|子作業|子作業名稱",
      risk_factor = "risk_factor|風險因素",
      risk_name = "risk_name|風險|風險名稱|風險因素",
      risk_description = "risk_description|風險描述|RoMM",
      risk_category = "risk_category|風險類別",
      control_objective = "control_objective|控制目標|目標",
      control_activity = "control_activity|控制活動|活動",
      frequency = "frequency|頻率|控制頻率",
      responsible_unit = "responsible_unit|負責單位|owner|流程負責單位",
      iuc_or_system = "iuc_or_system|IUC|制度|iuc|相關系統",
      nature = "nature|性質|控制類型",
      approach = "approach|取向|預防偵測|控制活動類型",
      type = "type|類型",
      related_policy = "related_policy|相關政策或程序",
      related_law = "related_law|相關法令",
      related_document = paste0("related_document|", CONTROL_EVIDENCE_DOCUMENT_LABEL, "|相關文件"),
      inputs = "inputs|投入",
      review_steps = "review_steps|steps|步驟",
      outputs = "outputs|產出",
      # 故意不映射公司現況／有效性／潛在風險／改善建議，避免污染設計庫
      detailed_description = "detailed_description|控制描述|描述",
      summary_description = "summary_description|摘要",
      tags = "tags|標籤"
    )
    nm <- tolower(names(df))
    pick_col <- function(pattern) {
      pats <- tolower(unlist(strsplit(pattern, "\\|")))
      for (p in pats) {
        hit <- which(nm == p | grepl(paste0("^", p, "$"), nm))
        if (length(hit)) return(names(df)[hit[[1]]])
      }
      NA_character_
    }
    for (i in seq_len(nrow(df))) {
      row <- list()
      for (canon in names(alias_map)) {
        col <- pick_col(alias_map[[canon]])
        row[[canon]] <- if (!is.na(col)) df[[col]][i] else ""
      }
      # keep any exact LIBRARY_CONTROL_FIELDS already present
      for (f in LIBRARY_CONTROL_FIELDS) {
        if (f %in% names(df) && !nzchar(as.character(row[[f]] %||% ""))) {
          row[[f]] <- df[[f]][i]
        }
      }
      incoming[[length(incoming) + 1]] <- flat_row_to_library_item(row)
    }
  } else if (ext %in% c("json")) {
    raw <- jsonlite::read_json(path, simplifyVector = FALSE)
    if (!is.null(raw$controls)) raw <- raw$controls
    if (!is.null(raw$library)) raw <- raw$library
    incoming <- lapply(raw, function(x) {
      ctrl <- x$control %||% x
      library_item_from_control(ctrl, tags = unlist(x$tags %||% list()))
    })
  } else {
    stop("僅支援 CSV、JSON 或 RCM xlsx 匯入")
  }
  merge_libraries(existing, incoming, overwrite = overwrite)
}

# Normalize Jinglian / RCM workbook header cell → short canonical name
normalize_rcm_header_cell <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("\r?\n", "", x)
  x <- gsub("\\s+", "", x)
  # strip parenthetical English / notes
  x <- sub("（.*$", "", x)
  x <- sub("\\(.*$", "", x)
  x <- trimws(x)
  # 輝能／各版本欄名別名 → 標準鍵
  aliases <- c(
    "控制活動編號" = "控制編號",
    "控制目標編號" = "控制目標編號",
    "頻率" = "控制頻率",
    "現況描述" = "控制現況描述",
    "相關資訊系統" = "相關系統",
    "相關政策及程序" = "相關政策或程序",
    "佐證文件" = "相關文件",
    "建議" = "建議改善方式",
    "風險範籌" = "風險範疇",
    "編號" = "子作業編號",
    "名稱" = "子作業名稱"
  )
  if (nzchar(x) && x %in% names(aliases)) x <- unname(aliases[[x]])
  x
}

# 循環名稱正規化（輝能短名／別名 → APP 九大循環或擴充循環）
normalize_rcm_cycle_name <- function(cycle_raw, sheet_name = "") {
  cy <- trimws(as.character(cycle_raw %||% ""))
  sh <- trimws(as.character(sheet_name %||% ""))
  if (!nzchar(cy) && nzchar(sh)) {
    cy <- sub("^RCM[_＿]?", "", sh)
    cy <- gsub("[,，]", "、", cy)
  }
  if (!nzchar(cy)) return("")
  # already canonical
  if (cy %in% CYCLES_NINE || cy %in% c("企業層級", "財務報導循環")) return(cy)
  if (grepl("資訊|電腦", cy)) return("電腦化資訊系統循環")
  if (grepl("銷售|收款", cy)) return("銷售及收款循環")
  if (grepl("採購|付款", cy)) return("採購及付款循環")
  if (grepl("^生產|生產循環", cy)) return("生產循環")
  if (grepl("薪工|人事|薪資", cy)) return("薪工循環")
  if (grepl("融資|借款", cy)) return("融資循環")
  if (grepl("固定資產|不動產|廠房及設備|PPE", cy)) return("固定資產循環")
  if (grepl("投資", cy)) return("投資循環")
  if (grepl("研發", cy)) return("研發循環")
  if (grepl("財務報導", cy)) return("財務報導循環")
  if (grepl("企業層級|EL|Entity", cy, ignore.case = TRUE)) return("企業層級")
  cy
}

rcm_row_to_control <- function(row, sheet_name = "", id_prefix = "PL") {
  getv <- function(... ) {
    keys <- c(...)
    for (k in keys) {
      if (k %in% names(row)) {
        v <- row[[k]]
        if (!is.null(v) && !(length(v) == 1 && is.na(v))) {
          s <- trimws(as.character(v))
          if (nzchar(s) && !identical(toupper(s), "NA")) return(s)
        }
      }
    }
    ""
  }
  cycle <- normalize_rcm_cycle_name(getv("循環名稱"), sheet_name = sheet_name)
  risk_factor <- getv("風險因素")
  risk_desc <- getv("風險描述")
  risk_cat <- getv("風險類別")
  cid <- getv("控制編號", "控制活動編號")
  if (!nzchar(cid)) cid <- getv("控制目標編號")
  spid <- getv("子作業編號")
  spn <- getv("子作業名稱")
  # 企業層級：原則／關注點補進風險描述
  if (identical(cycle, "企業層級")) {
    principle <- getv("原則")
    pof <- getv("關注點")
    if (!nzchar(risk_desc) && nzchar(principle)) {
      risk_desc <- principle
    } else if (nzchar(principle)) {
      risk_desc <- paste(risk_desc, principle, sep = "／")
    }
    if (nzchar(pof)) {
      risk_desc <- paste(c(risk_desc[nzchar(risk_desc)], paste0("關注點：", pof)), collapse = "\n")
    }
  }
  ctrl <- list(
    control_id = cid,
    library_id = {
      if (nzchar(cid)) paste0(id_prefix, "-", cid) else NULL
    },
    company = getv("公司"),
    title = {
      obj <- getv("控制目標")
      if (nzchar(obj)) obj else paste(spn, risk_factor, sep = "｜")
    },
    cycle = cycle,
    cycle_code = getv("循環編號"),
    sub_process_id = spid,
    sub_process = spn,
    risk_factor = risk_factor,
    risk_name = risk_factor,
    risk_description = risk_desc,
    risk_category = risk_cat,
    risk_attr_financial = if (grepl("報導", risk_cat)) paste0("[財務報導] ", risk_desc) else "",
    risk_attr_operations = if (grepl("營運", risk_cat) || !nzchar(risk_cat)) paste0("[營運] ", risk_desc) else "",
    risk_attr_compliance = if (grepl("遵循", risk_cat)) paste0("[法令遵循] ", risk_desc) else "",
    significant_account = getv("會計科目"),
    control_objective = getv("控制目標"),
    control_activity = getv("控制活動"),
    nature = normalize_control_type_manual_auto(getv("控制性質", "控制類型")),
    approach = normalize_control_activity_type_pd(getv("控制方式", "控制活動類型")),
    frequency = getv("控制頻率", "頻率"),
    # 現況／分析評估不入庫（由 strip_non_design_control_fields 再保險清空）
    company_status = "",
    design_gap_note = "",
    related_system = getv("相關系統", "相關資訊系統"),
    iuc = getv("相關文件-控制用文件", "IUC"),
    iuc_or_system = getv("相關文件-控制用文件", "IUC", "相關系統", "相關資訊系統",
                         CONTROL_EVIDENCE_DOCUMENT_LABEL, "相關文件"),
    related_policy = getv("相關政策與制度", "相關政策或程序", "相關政策及程序"),
    related_law = getv("相關法規", "相關法令"),
    related_law_url = {
      url <- getv("相關法規連結", "法規有效網址連結")
      if (nzchar(trimws(url))) {
        trimws(url)
      } else {
        law_raw <- getv("相關法規", "相關法令")
        if (grepl("｜", law_raw, fixed = TRUE)) {
          trimws(sub("^.*｜", "", law_raw))
        } else {
          ""
        }
      }
    },
    related_documents = "",
    related_document = getv(CONTROL_EVIDENCE_DOCUMENT_LABEL, "控制佐證文件", "相關文件", "佐證文件"),
    responsible_unit = getv("控制點負責單位", "流程負責單位"),
    effectiveness = "",
    residual_risk = "",
    improvement = "",
    outputs = getv(CONTROL_EVIDENCE_DOCUMENT_LABEL, "控制佐證文件", "相關文件", "佐證文件"),
    risk_principle = getv("風險面向"),
    risk_area = getv("風險範疇", "風險範籌"),
    assertions = getv("控制聲明", "聲明"),
    detailed_description = "",
    key_control = "Y"
  )
  ctrl
}

import_rcm_xlsx_as_library <- function(path,
                                       source = "rcm_xlsx",
                                       tags = character(),
                                       id_prefix = "PL",
                                       company_default = "") {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("需要 readxl 套件以匯入 RCM xlsx：install.packages(\"readxl\")")
  }
  sheets <- readxl::excel_sheets(path)
  # 優先 RCM_ 資料頁（略過封面／文件資訊／修改）
  rcm_hit <- grep("^RCM", sheets, ignore.case = TRUE)
  if (length(rcm_hit)) {
    sheet <- sheets[[rcm_hit[[1]]]]
  } else {
    hit <- grep("循環|RCM|控制|企業層級", sheets, ignore.case = TRUE)
    sheet <- if (length(hit)) sheets[[hit[[1]]]] else sheets[[1]]
  }

  header_row <- readxl::read_excel(path, sheet = sheet, col_names = FALSE, n_max = 2)
  headers <- vapply(seq_len(ncol(header_row)), function(j) {
    normalize_rcm_header_cell(header_row[[j]][2])
  }, character(1))
  # Fall back to row1 if row2 empty
  for (j in seq_along(headers)) {
    if (!nzchar(headers[[j]])) headers[[j]] <- normalize_rcm_header_cell(header_row[[j]][1])
  }
  # Deduplicate empty / NA headers
  headers[is.na(headers) | !nzchar(headers)] <- paste0("col_", seq_along(headers))[is.na(headers) | !nzchar(headers)]
  # If duplicates, make unique
  if (any(duplicated(headers))) headers <- make.unique(headers, sep = "_")

  df <- as.data.frame(readxl::read_excel(path, sheet = sheet, skip = 2, col_names = FALSE),
                      stringsAsFactors = FALSE)
  if (!nrow(df)) return(list())
  # Align columns
  n <- min(ncol(df), length(headers))
  df <- df[, seq_len(n), drop = FALSE]
  names(df) <- headers[seq_len(n)]

  # Keep real control rows; drop leftover header echo rows
  keep <- vapply(seq_len(nrow(df)), function(i) {
    obj <- if ("控制目標" %in% names(df)) trimws(as.character(df[["控制目標"]][i] %||% "")) else ""
    act <- if ("控制活動" %in% names(df)) trimws(as.character(df[["控制活動"]][i] %||% "")) else ""
    cid <- if ("控制編號" %in% names(df)) trimws(as.character(df[["控制編號"]][i] %||% "")) else ""
    cy <- if ("循環名稱" %in% names(df)) trimws(as.character(df[["循環名稱"]][i] %||% "")) else ""
    if (identical(cid, "控制編號") || identical(obj, "控制目標") || identical(cy, "循環名稱")) {
      return(FALSE)
    }
    if (identical(cid, "") && identical(obj, "") && identical(act, "")) return(FALSE)
    nzchar(obj) || nzchar(act)
  }, logical(1))
  df <- df[keep, , drop = FALSE]

  sheet_cycle <- normalize_rcm_cycle_name("", sheet_name = sheet)
  # 不寫入企業專屬標籤／公司名；去識別於 library_item_from_control
  base_tags <- unique(c(tags, "RCM", sheet_cycle[nzchar(sheet_cycle)]))
  base_tags <- base_tags[!base_tags %in% CLIENT_NAME_MARKERS]
  items <- lapply(seq_len(nrow(df)), function(i) {
    row <- as.list(df[i, , drop = FALSE])
    row <- lapply(row, function(x) if (length(x)) x[[1]] else x)
    ctrl <- rcm_row_to_control(row, sheet_name = sheet, id_prefix = id_prefix)
    if (!nzchar(ctrl$cycle %||% "")) ctrl$cycle <- sheet_cycle
    # 公司欄一律不入庫（即使 xlsx／default 有值）
    ctrl$company <- ""
    if (identical(ctrl$control_id, "控制編號") ||
        (!nzchar(ctrl$control_objective %||% "") && !nzchar(ctrl$control_activity %||% ""))) {
      return(NULL)
    }
    library_item_from_control(
      ctrl,
      tags = unique(c(base_tags, ctrl$cycle %||% "")),
      source = source,
      deidentify = TRUE
    )
  })
  Filter(Negate(is.null), items)
}

library_summary_df <- function(library) {
  if (!length(library)) {
    return(data.frame(
      library_id = character(), cycle = character(), title = character(),
      risk = character(), objective = character(), activity = character(),
      iuc = character(), source = character(),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    library_id = vapply(library, function(x) x$library_id, ""),
    cycle = vapply(library, function(x) x$cycle %||% x$control$cycle %||% "", ""),
    title = vapply(library, function(x) x$title, ""),
    risk = vapply(library, function(x) x$control$risk_name %||% x$control$risk_factor %||% "", ""),
    objective = vapply(library, function(x) x$control$control_objective %||% "", ""),
    activity = vapply(library, function(x) x$control$control_activity %||% "", ""),
    iuc = vapply(library, function(x) x$control$iuc_or_system %||% "", ""),
    source = vapply(library, function(x) x$source %||% "", ""),
    stringsAsFactors = FALSE
  )
}

# Patch selected library item control fields (admin direct edit)
patch_library_item_fields <- function(library, library_id, fields = list(),
                                      title = NULL, tags = NULL) {
  id <- trimws(as.character(library_id %||% ""))
  if (!nzchar(id) || !length(library)) return(library)
  idx <- which(vapply(library, function(x) identical(x$library_id, id), logical(1)))
  if (!length(idx)) return(library)
  i <- idx[[1]]
  item <- library[[i]]
  ctrl <- as.list(item$control %||% list())
  for (nm in names(fields)) {
    ctrl[[nm]] <- fields[[nm]]
  }
  if (!is.null(title) && nzchar(trimws(as.character(title)))) {
    item$title <- trimws(as.character(title))
  }
  if (!is.null(tags)) {
    tag_vec <- if (is.character(tags) && length(tags) == 1L) {
      trimws(unlist(strsplit(as.character(tags), "[;；,，|/]+")))
    } else {
      trimws(as.character(tags))
    }
    tag_vec <- tag_vec[nzchar(tag_vec)]
    item$tags <- tag_vec
  }
  item$control <- ctrl
  item$cycle <- ctrl$cycle %||% item$cycle %||% ""
  item$updated_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  item$source <- "高權維護"
  library[[i]] <- item
  library
}
