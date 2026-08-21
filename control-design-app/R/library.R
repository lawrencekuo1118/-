# Reusable library of well-designed control points (seed + lookup).

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
    ), tags = c("採購", "三方比對"))
  )
}

library_item_from_control <- function(ctrl, tags = character()) {
  if (is.null(ctrl$library_id) || !nzchar(ctrl$library_id)) {
    raw <- paste(c(ctrl$cycle, ctrl$risk_name, ctrl$control_objective, ctrl$iuc_or_system), collapse = "|")
    ctrl$library_id <- sprintf("LIB-%08x", sum(utf8ToInt(enc2utf8(raw))) %% as.integer(1e8))
  }
  if (is.null(ctrl$title) || !nzchar(ctrl$title)) {
    ctrl$title <- sprintf("%s｜%s", ctrl$cycle %||% "", ctrl$control_objective %||% "控制")
  }
  list(
    library_id = ctrl$library_id,
    title = ctrl$title,
    tags = tags,
    control = ctrl
  )
}

library_choices <- function(library) {
  if (!length(library)) return(character())
  labels <- vapply(library, function(x) {
    sprintf("%s｜%s", x$library_id %||% "?", x$title %||% "未命名")
  }, character(1))
  ids <- vapply(library, function(x) x$library_id %||% "", character(1))
  stats::setNames(ids, labels)
}

get_library_item <- function(library, id) {
  for (item in library) {
    if (identical(item$library_id, id)) return(item)
  }
  NULL
}
