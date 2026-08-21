# Persistent accumulative library of well-designed control points.
# Prefer selecting from library when designing new controls.

LIBRARY_CONTROL_FIELDS <- c(
  "library_id", "title", "cycle", "risk_name", "risk_description",
  "risk_attr_financial", "risk_attr_operations", "risk_attr_compliance",
  "romm_classification", "significant_account", "assertions",
  "control_objective", "control_activity", "frequency", "responsible_unit",
  "iuc_or_system", "nature", "approach", "type",
  "inputs", "review_steps", "outputs", "investigation_threshold",
  "dependent_controls", "detailed_description", "summary_description"
)

seed_control_library <- function() {
  list(
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
    ), tags = c("銷售", "截止", "Significant Risk")),
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
      assertions = "存在／發生 (Existence/Occurrence)；正確性 (Accuracy)",
      control_objective = "僅對經授權且貨物／勞務已收受之交易認列應付帳款",
      control_activity = "應付帳款人員於入帳前執行採購單、驗收單與發票三方比對，不符者不得付款",
      frequency = "每筆交易",
      responsible_unit = "財務部應付帳款／採購單位",
      iuc_or_system = "採購單／驗收單／供應商發票（ERP AP）",
      nature = "人工＋自動化混合",
      approach = "預防性 (Preventive)",
      type = "核對驗證 (Verifications)",
      inputs = "採購單、驗收單、供應商發票",
      review_steps = "比對數量與單價\n確認驗收完成\n確認核決權限\n系統過帳或退回供應商",
      outputs = "三方比對紀錄、系統過帳 log、退回文件",
      investigation_threshold = "數量或金額任一不符即暫停付款",
      dependent_controls = "採購核決權限控制"
    ), tags = c("採購", "三方比對")),
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
      assertions = "存在／發生 (Existence/Occurrence)；權利與義務 (Rights and Obligations)",
      control_objective = "確保系統使用者權限與現職及職責分離原則一致",
      control_activity = "權限管理員每季產出使用者權限清冊，由各單位主管覆核後回簽，並於期限內完成異動",
      frequency = "每季",
      responsible_unit = "資訊部／各業務單位主管",
      iuc_or_system = "使用者權限清冊",
      nature = "人工＋自動化混合",
      approach = "偵測性 (Detective)",
      type = "含覆核要素之控制 (Controls with a Review Element)",
      inputs = "HR 在職名單、系統權限清冊",
      review_steps = "產製權限清冊\n比對在職與職務\n主管標註應移除／調整權限\n資訊部於期限完成異動並留存軌跡",
      outputs = "覆核簽回清冊、權限異動單、系統 log",
      investigation_threshold = "任何不應存在之權限均須異動；逾期未回簽列入追蹤",
      dependent_controls = "入離職帳號開立／停用控制",
      key_control = "Y"
    ), tags = c("資訊循環", "存取管理", "ITGC")),
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
      frequency = "即時／每筆交易",
      responsible_unit = "資訊部變更管理／系統擁有者",
      iuc_or_system = "變更管理單／移轉 log",
      nature = "人工＋自動化混合",
      approach = "預防性 (Preventive)",
      type = "授權與核准 (Authorizations and Approvals)",
      inputs = "變更申請單、測試報告",
      review_steps = "確認測試環境結果\n確認核准層級\n核對移轉物件清單\n執行移轉並留存 log",
      outputs = "已核准變更單、移轉成功 log",
      investigation_threshold = "缺測試或缺核准不得移轉",
      dependent_controls = "開發／營運環境職責分離",
      key_control = "Y"
    ), tags = c("資訊循環", "變更管理", "ITGC"))
  )
}

library_item_from_control <- function(ctrl, tags = character()) {
  ctrl <- as.list(ctrl)
  if (is.null(ctrl$library_id) || !nzchar(as.character(ctrl$library_id %||% ""))) {
    raw <- paste(c(ctrl$cycle, ctrl$risk_name, ctrl$control_objective, ctrl$iuc_or_system), collapse = "|")
    ctrl$library_id <- sprintf("LIB-%08x", sum(utf8ToInt(enc2utf8(raw))) %% as.integer(1e8))
  }
  if (is.null(ctrl$title) || !nzchar(as.character(ctrl$title %||% ""))) {
    ctrl$title <- sprintf(
      "%s｜%s",
      ctrl$cycle %||% "",
      {
        obj <- ctrl$control_objective %||% ""
        if (nzchar(obj)) obj else (ctrl$control_activity %||% "控制")
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
  tags <- unique(c(as.character(tags), as.character(ctrl$tags %||% character())))
  tags <- tags[nzchar(tags)]
  list(
    library_id = as.character(ctrl$library_id),
    title = as.character(ctrl$title),
    tags = tags,
    cycle = as.character(ctrl$cycle %||% ""),
    updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    control = ctrl
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
    list(
      library_id = item$library_id,
      title = item$title,
      tags = item$tags,
      cycle = item$cycle %||% item$control$cycle %||% "",
      updated_at = item$updated_at %||% "",
      control = item$control
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
    library_item_from_control(ctrl, tags = unlist(x$tags %||% list()))
  })
  if (!length(items) && fallback_seed) return(seed_control_library())
  items
}

# Bulk import CSV (flat) or JSON (list of items/controls)
import_control_library_file <- function(path, existing = list(), overwrite = TRUE) {
  ext <- tolower(tools::file_ext(path))
  incoming <- list()
  if (ext == "csv") {
    df <- utils::read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
    # flexible aliases
    names(df) <- gsub("\\s+", "_", names(df))
    alias_map <- c(
      library_id = "library_id|id|範本id|控制點編號",
      title = "title|名稱|範本名稱",
      cycle = "cycle|循環|九大循環",
      risk_name = "risk_name|風險|風險名稱",
      risk_description = "risk_description|風險描述|RoMM",
      control_objective = "control_objective|控制目標|目標",
      control_activity = "control_activity|控制活動|活動",
      frequency = "frequency|頻率|控制頻率",
      responsible_unit = "responsible_unit|負責單位|owner",
      iuc_or_system = "iuc_or_system|IUC|制度|iuc",
      nature = "nature|性質",
      approach = "approach|取向|預防偵測",
      type = "type|類型",
      inputs = "inputs|投入",
      review_steps = "review_steps|steps|步驟",
      outputs = "outputs|產出",
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
    stop("僅支援 CSV 或 JSON 匯入")
  }
  merge_libraries(existing, incoming, overwrite = overwrite)
}

library_summary_df <- function(library) {
  if (!length(library)) {
    return(data.frame(
      library_id = character(), cycle = character(), title = character(),
      risk = character(), objective = character(), activity = character(),
      iuc = character(), tags = character(), stringsAsFactors = FALSE
    ))
  }
  data.frame(
    library_id = vapply(library, function(x) x$library_id, ""),
    cycle = vapply(library, function(x) x$cycle %||% x$control$cycle %||% "", ""),
    title = vapply(library, function(x) x$title, ""),
    risk = vapply(library, function(x) x$control$risk_name %||% "", ""),
    objective = vapply(library, function(x) x$control$control_objective %||% "", ""),
    activity = vapply(library, function(x) x$control$control_activity %||% "", ""),
    iuc = vapply(library, function(x) x$control$iuc_or_system %||% "", ""),
    tags = vapply(library, function(x) paste(x$tags %||% character(), collapse = ";"), ""),
    stringsAsFactors = FALSE
  )
}
