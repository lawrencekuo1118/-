# RCM / CSA / interview generation + design-gap detection
# Keep 控制目標 vs 控制活動 strictly separated in RCM columns.

control_to_rcm_row <- function(ctrl) {
  data.frame(
    control_id = ctrl$control_id %||% "",
    cycle = ctrl$cycle %||% "",
    risk = ctrl$risk_name %||% "",
    risk_attributes = paste(
      c(ctrl$risk_attr_financial, ctrl$risk_attr_operations, ctrl$risk_attr_compliance),
      collapse = "｜"
    ),
    assertion = ctrl$assertions %||% "",
    significant_account = ctrl$significant_account %||% "",
    # RCM: objective = purpose only (why). Never put how-to steps here.
    control_objective = trimws(ctrl$control_objective %||% ""),
    # RCM: activity = what is performed (how). Not the objective restated.
    control_activity = trimws(ctrl$control_activity %||% ""),
    frequency = ctrl$frequency %||% "",
    owner = ctrl$responsible_unit %||% "",
    iuc_or_system = ctrl$iuc_or_system %||% "",
    nature = ctrl$nature %||% "",
    approach = ctrl$approach %||% "",
    type = ctrl$type %||% "",
    stringsAsFactors = FALSE
  )
}

controls_to_rcm <- function(controls) {
  if (!length(controls)) {
    return(control_to_rcm_row(list())[0, , drop = FALSE])
  }
  do.call(rbind, lapply(controls, control_to_rcm_row))
}

# Interview questions derived from design elements (for walkthrough)
control_to_interview <- function(ctrl) {
  qs <- c(
    sprintf("【循環／風險】請說明「%s」下「%s」風險如何發生？近期是否有實例？",
            nzchar_or(ctrl$cycle, "本循環"), nzchar_or(ctrl$risk_name, "該")),
    sprintf("【控制目標】貴單位如何確保達成「%s」？與財務報導／營運／法令遵循各如何對應？",
            nzchar_or(ctrl$control_objective, "此控制目標")),
    sprintf("【控制活動】請逐步示範「%s」；誰執行、誰覆核？",
            nzchar_or(ctrl$control_activity, "該控制活動")),
    sprintf("【頻率／Owner】實際執行頻率是否為「%s」？負責單位「%s」是否具權限與能力？",
            nzchar_or(ctrl$frequency, "所訂頻率"), nzchar_or(ctrl$responsible_unit, "負責單位")),
    sprintf("【IUC／制度】執行時使用哪些報表／系統／辦法（含「%s」）？資料如何產生？",
            nzchar_or(ctrl$iuc_or_system, "所列 IUC")),
    sprintf("【Inputs／Steps／Outputs】投入為何？關鍵步驟？產出／留存軌跡為何（預期：%s）？",
            nzchar_or(ctrl$outputs, "簽核或調節紀錄")),
    sprintf("【例外】若發現差異，調查門檻與追蹤方式為何%s？",
            if (nzchar(trimws(ctrl$investigation_threshold %||% "")))
              paste0("（設計：", ctrl$investigation_threshold, "）") else "")
  )
  data.frame(
    control_id = ctrl$control_id %||% "",
    question_no = seq_along(qs),
    element = c("風險", "控制目標", "控制活動", "頻率／Owner", "IUC", "IOU敘述", "例外追蹤"),
    interview_question = qs,
    stringsAsFactors = FALSE
  )
}

# CSA (內部控制自我評估) worksheet lines
control_to_csa <- function(ctrl) {
  steps <- trimws(unlist(strsplit(as.character(ctrl$review_steps %||% ""), "\n")))
  steps <- steps[nzchar(steps)]
  if (!length(steps)) {
    steps <- nzchar_or(ctrl$control_activity, "（待補控制活動步驟）")
  }
  data.frame(
    control_id = ctrl$control_id %||% "",
    cycle = ctrl$cycle %||% "",
    risk = ctrl$risk_name %||% "",
    control_objective = ctrl$control_objective %||% "",
    csa_step_no = seq_along(steps),
    csa_self_test_step = steps,
    evidence_to_retain = nzchar_or(ctrl$outputs, "執行軌跡／簽核"),
    iuc_referenced = ctrl$iuc_or_system %||% "",
    self_assessment_result = "",
    exception_noted = "",
    stringsAsFactors = FALSE
  )
}

# Detect missing information / documents / likely design deficiencies
detect_design_gaps <- function(ctrl) {
  gaps <- list()
  add <- function(severity, item, action) {
    gaps[[length(gaps) + 1]] <<- data.frame(
      control_id = ctrl$control_id %||% "",
      severity = severity,
      gap_item = item,
      suggested_action = action,
      stringsAsFactors = FALSE
    )
  }
  v <- validate_control_design(ctrl)
  for (m in v$missing) {
    add("高", paste0("缺漏設計元素：", m), "補齊後方可定稿 RCM／CSA")
  }
  obj <- trimws(ctrl$control_objective %||% "")
  act <- trimws(ctrl$control_activity %||% "")
  if (nzchar(obj) && nzchar(act) && identical(obj, act)) {
    add("高", "控制目標與控制活動文字相同",
        "目標應描述「要達成什麼／對應何風險與聲明」；活動應描述「如何執行」")
  }
  if (nzchar(obj) && nzchar(act) && grepl(obj, act, fixed = TRUE) && nchar(act) < nchar(obj) + 8) {
    add("中", "控制活動幾乎只重複控制目標",
        "在活動欄補上執行步驟、表單或覆核動作，勿僅重述目標")
  }
  if (!nzchar(trimws(ctrl$iuc_or_system %||% ""))) {
    add("高", "未指定 IUC／制度", "向客戶取得 PBC／報表清單並登錄命名對照後套用")
  }
  if (!nzchar(trimws(ctrl$outputs %||% ""))) {
    add("中", "缺少控制產出／軌跡", "確認可驗證之證據（簽核、調節表、系統 log）")
  }
  if (!nzchar(trimws(ctrl$review_steps %||% ""))) {
    add("中", "缺少分步 Specific Activities", "依 Form 4120SR Note 1 拆成可測試步驟")
  }
  if (!nzchar(trimws(ctrl$investigation_threshold %||% "")) &&
      grepl("Review|覆核|調節|Reconcili", paste(ctrl$type, ctrl$control_activity), ignore.case = TRUE)) {
    add("中", "含覆核／調節性質但未訂調查門檻", "補 Design Factor 5：門檻與追蹤流程")
  }
  if (!length(gaps)) {
    return(data.frame(
      control_id = character(), severity = character(),
      gap_item = character(), suggested_action = character(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, gaps)
}

detect_gaps_many <- function(controls) {
  if (!length(controls)) {
    return(detect_design_gaps(list()))
  }
  do.call(rbind, lapply(controls, detect_design_gaps))
}
