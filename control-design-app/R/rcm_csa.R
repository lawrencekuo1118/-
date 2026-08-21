# RCM / CSA / interview generation + design-gap detection
# Keep 控制目標 vs 控制活動 strictly separated in RCM columns.

# Selectable design-element catalogue (Form 4120SR + 九大循環設計元素)
DESIGN_ELEMENTS <- c(
  risk = "循環／風險",
  risk_attributes = "風險三大屬性",
  control_objective = "控制目標",
  control_activity = "控制活動",
  frequency_owner = "頻率／負責單位",
  iuc = "IUC／制度",
  nature_approach_type = "Nature／Approach／Type",
  inputs = "Inputs（投入）",
  steps = "Steps（執行步驟）",
  outputs = "Outputs（產出／軌跡）",
  exception = "例外／調查門檻",
  assertion_account = "科目／聲明"
)

DEFAULT_INTERVIEW_ELEMENTS <- c(
  "risk", "control_objective", "control_activity",
  "frequency_owner", "iuc", "inputs", "steps", "outputs", "exception"
)

DEFAULT_CSA_ELEMENTS <- c(
  "control_objective", "control_activity", "steps",
  "iuc", "outputs", "exception", "frequency_owner"
)

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

# Build per-element interview question bank, then filter by selection
interview_element_bank <- function(ctrl) {
  list(
    risk = list(
      element = "循環／風險",
      question = sprintf(
        "請說明「%s」下「%s」風險如何發生？近期是否有實例？%s",
        nzchar_or(ctrl$cycle, "本循環"),
        nzchar_or(ctrl$risk_name, "該"),
        if (nzchar(trimws(ctrl$risk_description %||% "")))
          paste0("（設計描述：", ctrl$risk_description, "）") else ""
      )
    ),
    risk_attributes = list(
      element = "風險三大屬性",
      question = sprintf(
        "此風險在財務報導／營運／法令遵循屬性分別為何？設計記載：%s｜%s｜%s",
        nzchar_or(ctrl$risk_attr_financial, "（未填）"),
        nzchar_or(ctrl$risk_attr_operations, "（未填）"),
        nzchar_or(ctrl$risk_attr_compliance, "（未填）")
      )
    ),
    control_objective = list(
      element = "控制目標",
      question = sprintf(
        "貴單位如何確保達成控制目標「%s」？該目標對應哪些風險與聲明？",
        nzchar_or(ctrl$control_objective, "（待補目標）")
      )
    ),
    control_activity = list(
      element = "控制活動",
      question = sprintf(
        "請示範控制活動「%s」；誰執行、誰覆核？與控制目標如何區隔？",
        nzchar_or(ctrl$control_activity, "（待補活動）")
      )
    ),
    frequency_owner = list(
      element = "頻率／負責單位",
      question = sprintf(
        "實際執行頻率是否為「%s」？負責單位「%s」是否具備權限與能力？",
        nzchar_or(ctrl$frequency, "所訂頻率"),
        nzchar_or(ctrl$responsible_unit, "負責單位")
      )
    ),
    iuc = list(
      element = "IUC／制度",
      question = sprintf(
        "執行時使用哪些報表／系統／辦法（設計：%s）？資料如何產生？完整性／正確性如何確保？",
        nzchar_or(ctrl$iuc_or_system, "未指定 IUC")
      )
    ),
    nature_approach_type = list(
      element = "Nature／Approach／Type",
      question = sprintf(
        "此控制性質／取向／類型是否為「%s／%s／%s」？實務是否一致？",
        nzchar_or(ctrl$nature, "—"),
        nzchar_or(ctrl$approach, "—"),
        nzchar_or(ctrl$type, "—")
      )
    ),
    inputs = list(
      element = "Inputs（投入）",
      question = sprintf(
        "執行控制的投入資訊為何（設計：%s）？由誰提供、如何取得？",
        nzchar_or(ctrl$inputs, "待補 Inputs")
      )
    ),
    steps = list(
      element = "Steps（執行步驟）",
      question = sprintf(
        "請依序說明執行步驟：%s",
        {
          st <- trimws(unlist(strsplit(as.character(ctrl$review_steps %||% ""), "\n")))
          st <- st[nzchar(st)]
          if (!length(st)) "（尚未拆分步驟）" else paste(paste0(seq_along(st), ".", st), collapse = "；")
        }
      )
    ),
    outputs = list(
      element = "Outputs（產出／軌跡）",
      question = sprintf(
        "控制產出／留存軌跡為何（預期：%s）？何處可取得？",
        nzchar_or(ctrl$outputs, "簽核或調節紀錄")
      )
    ),
    exception = list(
      element = "例外／調查門檻",
      question = sprintf(
        "若發現差異，調查門檻與追蹤方式為何%s？",
        if (nzchar(trimws(ctrl$investigation_threshold %||% "")))
          paste0("（設計：", ctrl$investigation_threshold, "）") else "（設計尚未訂門檻）"
      )
    ),
    assertion_account = list(
      element = "科目／聲明",
      question = sprintf(
        "此控制涵蓋重大科目「%s」與聲明「%s」是否完整？有無遺漏？",
        nzchar_or(ctrl$significant_account, "（未填）"),
        nzchar_or(ctrl$assertions, "（未填）")
      )
    )
  )
}

control_to_interview <- function(ctrl, elements = DEFAULT_INTERVIEW_ELEMENTS) {
  bank <- interview_element_bank(ctrl)
  elements <- intersect(as.character(elements %||% character()), names(bank))
  if (!length(elements)) {
    return(data.frame(
      control_id = character(), question_no = integer(),
      element_key = character(), element = character(),
      interview_question = character(), stringsAsFactors = FALSE
    ))
  }
  rows <- lapply(seq_along(elements), function(i) {
    key <- elements[[i]]
    item <- bank[[key]]
    data.frame(
      control_id = ctrl$control_id %||% "",
      question_no = i,
      element_key = key,
      element = item$element,
      interview_question = item$question,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# CSA worksheet rows derived from selected elements (not only Steps)
csa_element_bank <- function(ctrl) {
  step_lines <- trimws(unlist(strsplit(as.character(ctrl$review_steps %||% ""), "\n")))
  step_lines <- step_lines[nzchar(step_lines)]
  list(
    risk = list(
      element = "循環／風險",
      steps = sprintf("確認本控制所因應風險「%s」（%s）仍適用現況",
                      nzchar_or(ctrl$risk_name, "未命名風險"),
                      nzchar_or(ctrl$cycle, "未選循環"))
    ),
    risk_attributes = list(
      element = "風險三大屬性",
      steps = sprintf("自我評估風險三大屬性是否仍為：%s／%s／%s",
                      nzchar_or(ctrl$risk_attr_financial, "—"),
                      nzchar_or(ctrl$risk_attr_operations, "—"),
                      nzchar_or(ctrl$risk_attr_compliance, "—"))
    ),
    control_objective = list(
      element = "控制目標",
      steps = sprintf("確認控制目標「%s」已達成（勿與活動混淆）",
                      nzchar_or(ctrl$control_objective, "（待補目標）"))
    ),
    control_activity = list(
      element = "控制活動",
      steps = sprintf("依控制活動「%s」執行自我測試（記錄實際作為，非重述目標）",
                      nzchar_or(ctrl$control_activity, "（待補活動）"))
    ),
    frequency_owner = list(
      element = "頻率／負責單位",
      steps = sprintf("確認由「%s」依「%s」頻率執行並留存軌跡",
                      nzchar_or(ctrl$responsible_unit, "負責單位"),
                      nzchar_or(ctrl$frequency, "頻率"))
    ),
    iuc = list(
      element = "IUC／制度",
      steps = sprintf("取得並核對 IUC／制度「%s」之完整性與正確性來源",
                      nzchar_or(ctrl$iuc_or_system, "（未指定）"))
    ),
    nature_approach_type = list(
      element = "Nature／Approach／Type",
      steps = sprintf("確認控制仍屬 %s／%s／%s",
                      nzchar_or(ctrl$nature, "—"),
                      nzchar_or(ctrl$approach, "—"),
                      nzchar_or(ctrl$type, "—"))
    ),
    inputs = list(
      element = "Inputs（投入）",
      steps = sprintf("核對投入資訊：%s", nzchar_or(ctrl$inputs, "（待補 Inputs）"))
    ),
    steps = list(
      element = "Steps（執行步驟）",
      steps = if (length(step_lines)) step_lines else nzchar_or(ctrl$control_activity, "（待補步驟）")
    ),
    outputs = list(
      element = "Outputs（產出／軌跡）",
      steps = sprintf("留存／檢查產出證據：%s", nzchar_or(ctrl$outputs, "執行軌跡／簽核"))
    ),
    exception = list(
      element = "例外／調查門檻",
      steps = sprintf("依門檻「%s」辨識例外並完成追蹤結案",
                      nzchar_or(ctrl$investigation_threshold, "（未訂門檻，請先補）"))
    ),
    assertion_account = list(
      element = "科目／聲明",
      steps = sprintf("確認涵蓋科目「%s」與聲明「%s」無遺漏",
                      nzchar_or(ctrl$significant_account, "—"),
                      nzchar_or(ctrl$assertions, "—"))
    )
  )
}

control_to_csa <- function(ctrl, elements = DEFAULT_CSA_ELEMENTS) {
  bank <- csa_element_bank(ctrl)
  elements <- intersect(as.character(elements %||% character()), names(bank))
  if (!length(elements)) {
    return(data.frame(
      control_id = character(), cycle = character(), risk = character(),
      control_objective = character(), element_key = character(), element = character(),
      csa_step_no = integer(), csa_self_test_step = character(),
      evidence_to_retain = character(), iuc_referenced = character(),
      self_assessment_result = character(), exception_noted = character(),
      stringsAsFactors = FALSE
    ))
  }
  rows <- list()
  step_no <- 0L
  for (key in elements) {
    item <- bank[[key]]
    st <- item$steps
    if (length(st) == 1L) st <- as.character(st)
    for (s in st) {
      step_no <- step_no + 1L
      rows[[length(rows) + 1]] <- data.frame(
        control_id = ctrl$control_id %||% "",
        cycle = ctrl$cycle %||% "",
        risk = ctrl$risk_name %||% "",
        control_objective = ctrl$control_objective %||% "",
        element_key = key,
        element = item$element,
        csa_step_no = step_no,
        csa_self_test_step = s,
        evidence_to_retain = if (identical(key, "outputs")) {
          nzchar_or(ctrl$outputs, "執行軌跡／簽核")
        } else if (identical(key, "iuc")) {
          nzchar_or(ctrl$iuc_or_system, "IUC 來源文件")
        } else {
          nzchar_or(ctrl$outputs, "執行軌跡／簽核")
        },
        iuc_referenced = ctrl$iuc_or_system %||% "",
        self_assessment_result = "",
        exception_noted = "",
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

controls_to_interview <- function(controls, elements = DEFAULT_INTERVIEW_ELEMENTS) {
  if (!length(controls)) return(control_to_interview(list(), elements))
  do.call(rbind, lapply(controls, control_to_interview, elements = elements))
}

controls_to_csa <- function(controls, elements = DEFAULT_CSA_ELEMENTS) {
  if (!length(controls)) return(control_to_csa(list(), elements))
  do.call(rbind, lapply(controls, control_to_csa, elements = elements))
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
