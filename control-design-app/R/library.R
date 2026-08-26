# Persistent accumulative library of well-designed control points.
# Prefer selecting from library when designing new controls.

if (!exists("%||%", mode = "function", inherits = TRUE)) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

LIBRARY_CONTROL_FIELDS <- c(
  "library_id", "title", "cycle", "sub_process_id", "sub_process",
  "risk_factor", "risk_name", "risk_description", "risk_category",
  "risk_attr_financial", "risk_attr_operations", "risk_attr_compliance",
  "romm_classification", "significant_account", "assertions",
  "control_objective", "control_activity", "frequency", "responsible_unit",
  "iuc_or_system", "related_system", "related_policy", "related_law", "related_document",
  "related_document_pbc_ids",
  "company_status", "design_gap_note", "effectiveness", "residual_risk", "improvement",
  "nature", "approach", "type",
  "inputs", "review_steps", "outputs", "investigation_threshold",
  "dependent_controls", "detailed_description", "summary_description", "control_id"
)

# 非控制點「設計」欄位：輸入檔／既有 RCM 常帶入，不可污染 APP 範本庫／參數庫
NON_DESIGN_CONTROL_FIELDS <- c(
  "company_status",      # 控制現況描述／公司現況
  "design_gap_note",     # 控制設計差異說明
  "effectiveness",       # 控制有效性評估
  "residual_risk",       # 可能潛在風險
  "improvement"          # 建議改善方式
)

strip_non_design_control_fields <- function(ctrl) {
  ctrl <- as.list(ctrl)
  for (f in NON_DESIGN_CONTROL_FIELDS) {
    ctrl[[f]] <- ""
  }
  # 匯入檔常把「控制現況描述」誤塞進 detailed_description；設計庫一律清空後由組裝函式重建
  ctrl$detailed_description <- ""
  ctrl
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
    library_item_from_control(list(
      library_id = "LIB-IT-ACCESS-01",
      title = "資訊循環｜使用者權限定期覆核",
      cycle = "電腦化資訊系統循環",
      sub_process = "存取管理",
      risk_name = "不當或過時權限未及時取消",
      risk_description = "離職／轉調人員或不相容職務權限未於系統中移除，導致未授權存取",
      risk_attr_financial = "[財務報導] 可能造成未授權交易或資料竄改",
      risk_attr_operations = "[營運] 系統權限與組織職掌不一致",
      risk_attr_compliance = "[法令遵循] 個資／資安政策遵循",
      romm_classification = "Significant Risk — Higher risk associated with the control",
      significant_account = "多科目（視系統涵蓋流程）",
      assertions = "存在或發生 (Existence or Occurrence)；權利與義務 (Rights and Obligations)",
      control_objective = "確保系統使用者權限與現職及職責分離原則一致",
      control_activity = "權限管理員每季產出使用者權限清冊，由各單位主管覆核後回簽，並於期限內完成異動",
      frequency = "每季",
      responsible_unit = "資訊部／各業務單位主管",
      iuc_or_system = "使用者權限清冊",
      nature = "人工",
      approach = "偵測性 (Detective)",
      type = "含覆核要素之控制 (Controls with a Review Element)",
      inputs = "HR 在職名單、系統權限清冊",
      review_steps = "產製權限清冊\n比對在職與職務\n主管標註應移除／調整權限\n資訊部於期限完成異動並留存軌跡",
      outputs = "覆核簽回清冊、權限異動單、系統 log",
      investigation_threshold = "任何不應存在之權限均須異動；逾期未回簽列入追蹤",
      dependent_controls = "入離職帳號開立／停用控制",
      key_control = "Y"
    ), tags = c("資訊循環", "存取管理", "ITGC"), source = "seed"),
    library_item_from_control(list(
      library_id = "LIB-IT-CHANGE-01",
      title = "資訊循環｜程式變更上線核准",
      cycle = "電腦化資訊系統循環",
      sub_process = "變更管理",
      risk_name = "未經核准之程式變更上線",
      risk_description = "開發或維護程式未經適當測試與核准即移轉正式環境，導致財務資料錯誤",
      risk_attr_financial = "[財務報導] 系統處理正確性／完整性受影響",
      risk_attr_operations = "[營運] 變更失控造成服務中斷",
      risk_attr_compliance = "[法令遵循] 變更管理政策",
      romm_classification = "Significant Risk — Higher risk associated with the control",
      significant_account = "多科目（視應用系統）",
      assertions = "正確性 (Accuracy)；完整性 (Completeness)",
      control_objective = "確保正式環境之程式變更皆經測試通過且獲適當權限核准",
      control_activity = "變更管理員於移轉前檢核變更單之測試結果與核准簽核，系統僅允許已核准單號執行移轉",
      frequency = "持續",
      responsible_unit = "資訊部變更管理／系統擁有者",
      iuc_or_system = "變更管理單／移轉 log",
      nature = "自動",
      approach = "預防性 (Preventive)",
      type = "授權與核准 (Authorizations and Approvals)",
      inputs = "變更申請單、測試報告",
      review_steps = "確認測試環境結果\n確認核准層級\n核對移轉物件清單\n執行移轉並留存 log",
      outputs = "已核准變更單、移轉成功 log",
      investigation_threshold = "缺測試或缺核准不得移轉",
      dependent_controls = "開發／營運環境職責分離",
      key_control = "Y"
    ), tags = c("資訊循環", "變更管理", "ITGC"), source = "seed"),
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

  batch_json <- file.path(app_root, "data", "jinglian_it_rcm_batch.json")
  if (file.exists(batch_json)) {
    jl <- tryCatch(load_control_library(batch_json, fallback_seed = FALSE), error = function(e) list())
    if (length(jl)) return(merge_libraries(base, jl, overwrite = TRUE))
  }
  xlsx <- file.path(app_root, "templates", "鯨鏈科技_資訊循環_RCM_v1_0820.xlsx")
  if (file.exists(xlsx)) {
    jl <- tryCatch(import_rcm_xlsx_as_library(xlsx), error = function(e) list())
    if (length(jl)) return(merge_libraries(base, jl, overwrite = TRUE))
  }
  base
}

library_item_from_control <- function(ctrl, tags = character(), source = "manual") {
  ctrl <- strip_non_design_control_fields(as.list(ctrl))
  # Stable accumulative ID: prefer real RCM 控制編號 so re-save updates same template
  if (is.null(ctrl$library_id) || !nzchar(as.character(ctrl$library_id %||% ""))) {
    cid <- trimws(as.character(ctrl$control_id %||% ""))
    if (nzchar(cid) && !grepl("^CD-", cid)) {
      ctrl$library_id <- if (grepl("^JL-", cid)) cid else paste0("LIB-", cid)
    } else {
      raw <- paste(c(ctrl$cycle, ctrl$sub_process_id, ctrl$risk_factor %||% ctrl$risk_name,
                     ctrl$control_objective, ctrl$iuc_or_system), collapse = "|")
      ctrl$library_id <- sprintf("LIB-%08x", sum(utf8ToInt(enc2utf8(raw))) %% as.integer(1e8))
    }
  }
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
  list(
    library_id = as.character(ctrl$library_id),
    title = as.character(ctrl$title),
    tags = tags,
    cycle = as.character(ctrl$cycle %||% ""),
    source = as.character(source %||% "manual"),
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
  if (!length(library)) {
    return(data.frame(matrix(ncol = length(LIBRARY_CONTROL_FIELDS) + 2, nrow = 0,
                             dimnames = list(NULL, c(LIBRARY_CONTROL_FIELDS, "tags", "updated_at"))),
                      stringsAsFactors = FALSE))
  }
  rows <- lapply(library, function(item) {
    ctrl <- item$control
    vals <- lapply(LIBRARY_CONTROL_FIELDS, function(f) as.character(ctrl[[f]] %||% item[[f]] %||% ""))
    names(vals) <- LIBRARY_CONTROL_FIELDS
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
    ctrl <- strip_non_design_control_fields(item$control %||% list())
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

load_control_library <- function(path_json, fallback_seed = TRUE) {
  if (!file.exists(path_json)) {
    return(if (fallback_seed) seed_control_library() else list())
  }
  raw <- jsonlite::read_json(path_json, simplifyVector = FALSE)
  items <- lapply(raw, function(x) {
    ctrl <- x$control %||% x
    if (is.null(ctrl$library_id)) ctrl$library_id <- x$library_id
    if (is.null(ctrl$title)) ctrl$title <- x$title
    item <- library_item_from_control(ctrl, tags = unlist(x$tags %||% list()),
                                      source = x$source %||% "persisted")
    item$updated_at <- x$updated_at %||% item$updated_at
    item
  })
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
  trimws(x)
}

rcm_row_to_control <- function(row) {
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
  cycle_raw <- getv("循環名稱")
  cycle <- if (grepl("資訊", cycle_raw)) "電腦化資訊系統循環" else cycle_raw
  risk_factor <- getv("風險因素")
  risk_desc <- getv("風險描述")
  risk_cat <- getv("風險類別")
  ctrl <- list(
    control_id = getv("控制編號"),
    library_id = {
      cid <- getv("控制編號")
      if (nzchar(cid)) paste0("JL-", cid) else NULL
    },
    title = {
      obj <- getv("控制目標")
      if (nzchar(obj)) obj else paste(getv("子作業名稱"), getv("風險因素"), sep = "｜")
    },
    cycle = cycle,
    sub_process_id = getv("子作業編號"),
    sub_process = getv("子作業名稱"),
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
    nature = normalize_control_type_manual_auto(getv("控制類型")),
    approach = normalize_control_activity_type_pd(getv("控制活動類型")),
    frequency = getv("控制頻率"),
    # 現況／分析評估不入庫（由 strip_non_design_control_fields 再保險清空）
    company_status = "",
    design_gap_note = "",
    related_system = getv("相關系統"),
    iuc_or_system = getv("相關系統", CONTROL_EVIDENCE_DOCUMENT_LABEL, "相關文件"),
    related_policy = getv("相關政策或程序"),
    related_law = getv("相關法令"),
    related_document = getv(CONTROL_EVIDENCE_DOCUMENT_LABEL, "相關文件"),
    responsible_unit = getv("流程負責單位"),
    effectiveness = "",
    residual_risk = "",
    improvement = "",
    outputs = getv(CONTROL_EVIDENCE_DOCUMENT_LABEL, "相關文件"),
    detailed_description = "",
    key_control = "Y"
  )
  ctrl
}

import_rcm_xlsx_as_library <- function(path) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("需要 readxl 套件以匯入 RCM xlsx：install.packages(\"readxl\")")
  }
  sheets <- readxl::excel_sheets(path)
  sheet <- sheets[[1]]
  # Prefer sheet named like 資訊循環 / RCM
  hit <- grep("循環|RCM|控制", sheets, ignore.case = TRUE)
  if (length(hit)) sheet <- sheets[[hit[[1]]]]

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
    # Prefer rows with an EC-/SP- style id or non-empty objective+activity
    nzchar(obj) || nzchar(act)
  }, logical(1))
  df <- df[keep, , drop = FALSE]

  items <- lapply(seq_len(nrow(df)), function(i) {
    row <- as.list(df[i, , drop = FALSE])
    # unlist length-1
    row <- lapply(row, function(x) if (length(x)) x[[1]] else x)
    ctrl <- rcm_row_to_control(row)
    # Skip if control_id still looks like a header label
    if (identical(ctrl$control_id, "控制編號") ||
        (!nzchar(ctrl$control_objective %||% "") && !nzchar(ctrl$control_activity %||% ""))) {
      return(NULL)
    }
    library_item_from_control(ctrl, tags = c("鯨鏈RCM", "資訊循環", "首批"),
                              source = "jinglian_batch")
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
