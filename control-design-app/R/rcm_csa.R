# RCM / CSA / interview + gap detection
# RCM: 控制目標(Why) 與 控制活動(How／現況) 嚴格分欄防呆
# 完成一筆控制點設計 = 完成 RCM 一列

# ---- Selectable design elements ----
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

# Standard Taiwan-style RCM headers (資訊循環／九大循環通用)
# Learned pattern: keep related fields grouped and visually separable.
RCM_HEADERS <- c(
  "控制點編號",
  "循環",
  "子作業",
  "風險編號",
  "風險描述",
  "風險屬性_財務報導",
  "風險屬性_營運",
  "風險屬性_法令遵循",
  "控制目標",
  "控制活動",
  "控制現況描述",
  "控制頻率",
  "負責單位",
  "控制屬性_預防偵測",
  "執行方式_人工系統",
  "控制類型",
  "關鍵控制",
  "相關IUC_表單_系統",
  "相關科目",
  "相關聲明",
  "設計檢核"
)

# Strip attribute label prefixes like [財務報導]
strip_attr_label <- function(x) {
  gsub("^\\[[^\\]]+\\]\\s*", "", trimws(as.character(x %||% "")))
}

is_blank <- function(x) !nzchar(trimws(as.character(x %||% "")))

# rcm_objective_activity_check() lives in R/objective_activity.R

derive_risk_id <- function(ctrl, seq_no = 1L) {
  if (!is_blank(ctrl$risk_id)) return(trimws(ctrl$risk_id))
  cycle <- ctrl$cycle %||% ""
  prefix <- if (grepl("資訊|電腦|IT", cycle)) "IT-R" else if (grepl("銷售", cycle)) "SA-R"
  else if (grepl("採購", cycle)) "PU-R" else if (grepl("薪工", cycle)) "HR-R"
  else if (grepl("固定", cycle)) "FA-R" else if (grepl("融資", cycle)) "FN-R"
  else if (grepl("投資", cycle)) "IV-R" else if (grepl("研發", cycle)) "RD-R"
  else if (grepl("生產", cycle)) "PR-R" else "IC-R"
  sprintf("%s-%02d", prefix, as.integer(seq_no))
}

derive_control_id <- function(ctrl, seq_no = 1L) {
  if (!is_blank(ctrl$control_id) && !grepl("^CD-|^CP-", ctrl$control_id)) {
    return(trimws(ctrl$control_id))
  }
  cycle <- ctrl$cycle %||% ""
  prefix <- if (grepl("資訊|電腦|IT", cycle)) "IT-C" else if (grepl("銷售", cycle)) "SA-C"
  else if (grepl("採購", cycle)) "PU-C" else if (grepl("薪工", cycle)) "HR-C"
  else if (grepl("固定", cycle)) "FA-C" else if (grepl("融資", cycle)) "FN-C"
  else if (grepl("投資", cycle)) "IV-C" else if (grepl("研發", cycle)) "RD-C"
  else if (grepl("生產", cycle)) "PR-C" else "IC-C"
  sprintf("%s-%03d", prefix, as.integer(seq_no))
}

control_to_rcm_row <- function(ctrl, seq_no = 1L) {
  chk <- rcm_objective_activity_check(ctrl$control_objective, ctrl$control_activity)
  status_desc <- ctrl$company_status %||% ctrl$detailed_description %||% ""
  if (is_blank(status_desc) && exists("assemble_control_paragraph", mode = "function")) {
    status_desc <- tryCatch(assemble_control_paragraph(ctrl), error = function(e) "")
  }
  # 控制現況可含公司脈絡，但不得把「目標」貼進「活動」欄
  approach <- ctrl$approach %||% ""
  nature <- ctrl$nature %||% ""
  data.frame(
    `控制點編號` = derive_control_id(ctrl, seq_no),
    `循環` = ctrl$cycle %||% "",
    `子作業` = ctrl$sub_process %||% "",
    `風險編號` = derive_risk_id(ctrl, seq_no),
    `風險描述` = {
      if (!is_blank(ctrl$risk_description)) ctrl$risk_description
      else ctrl$risk_name %||% ""
    },
    `風險屬性_財務報導` = strip_attr_label(ctrl$risk_attr_financial),
    `風險屬性_營運` = strip_attr_label(ctrl$risk_attr_operations),
    `風險屬性_法令遵循` = strip_attr_label(ctrl$risk_attr_compliance),
    `控制目標` = trimws(ctrl$control_objective %||% ""),
    `控制活動` = trimws(ctrl$control_activity %||% ""),
    `控制現況描述` = status_desc,
    `控制頻率` = ctrl$frequency %||% "",
    `負責單位` = ctrl$responsible_unit %||% "",
    `控制屬性_預防偵測` = approach,
    `執行方式_人工系統` = nature,
    `控制類型` = ctrl$type %||% "",
    `關鍵控制` = if (!is_blank(ctrl$key_control)) ctrl$key_control else "Y",
    `相關IUC_表單_系統` = ctrl$iuc_or_system %||% "",
    `相關科目` = ctrl$significant_account %||% "",
    `相關聲明` = ctrl$assertions %||% "",
    `設計檢核` = if (isTRUE(chk$ok)) {
      sprintf("通過（目標：%s；活動：%s）", chk$objective_verdict, chk$activity_verdict)
    } else {
      paste0("待修：", chk$msg)
    },
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

controls_to_rcm <- function(controls) {
  if (!length(controls)) {
    empty <- control_to_rcm_row(list())
    return(empty[0, , drop = FALSE])
  }
  do.call(rbind, lapply(seq_along(controls), function(i) {
    control_to_rcm_row(controls[[i]], seq_no = i)
  }))
}

# ---- Interview ----
interview_element_bank <- function(ctrl) {
  list(
    risk = list(
      element = "循環／風險",
      question = sprintf(
        "請說明「%s」下「%s」風險如何發生？近期是否有實例？%s",
        nzchar_or(ctrl$cycle, "本循環"),
        nzchar_or(ctrl$risk_name, "該"),
        if (!is_blank(ctrl$risk_description)) paste0("（設計描述：", ctrl$risk_description, "）") else ""
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
        "貴單位如何確保達成控制目標「%s」？該目標對應哪些風險與聲明？（勿與活動混淆）",
        nzchar_or(ctrl$control_objective, "（待補目標）")
      )
    ),
    control_activity = list(
      element = "控制活動",
      question = sprintf(
        "請示範控制活動「%s」；誰執行、誰覆核？實際步驟為何？",
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
        "執行時使用哪些報表／系統／辦法（設計：%s）？如何確保完整性與正確性？可否提供 PBC？",
        nzchar_or(ctrl$iuc_or_system, "未指定 IUC")
      )
    ),
    nature_approach_type = list(
      element = "Nature／Approach／Type",
      question = sprintf(
        "此控制性質／取向／類型是否為「%s／%s／%s」？實務是否一致？",
        nzchar_or(ctrl$nature, "—"), nzchar_or(ctrl$approach, "—"), nzchar_or(ctrl$type, "—")
      )
    ),
    inputs = list(
      element = "Inputs（投入）",
      question = sprintf("執行控制的投入資訊為何（設計：%s）？由誰提供？",
                        nzchar_or(ctrl$inputs, "待補 Inputs"))
    ),
    steps = list(
      element = "Steps（執行步驟）",
      question = sprintf("請依序說明執行步驟：%s", {
        st <- trimws(unlist(strsplit(as.character(ctrl$review_steps %||% ""), "\n")))
        st <- st[nzchar(st)]
        if (!length(st)) "（尚未拆分步驟）" else paste(paste0(seq_along(st), ".", st), collapse = "；")
      })
    ),
    outputs = list(
      element = "Outputs（產出／軌跡）",
      question = sprintf("控制產出／留存軌跡為何（預期：%s）？何處可取得？",
                        nzchar_or(ctrl$outputs, "簽核或調節紀錄"))
    ),
    exception = list(
      element = "例外／調查門檻",
      question = sprintf(
        "若發現差異，調查門檻與追蹤方式為何%s？",
        if (!is_blank(ctrl$investigation_threshold)) paste0("（設計：", ctrl$investigation_threshold, "）")
        else "（設計尚未訂門檻）"
      )
    ),
    assertion_account = list(
      element = "科目／聲明",
      question = sprintf(
        "此控制涵蓋重大科目「%s」與聲明「%s」是否完整？",
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
  do.call(rbind, lapply(seq_along(elements), function(i) {
    key <- elements[[i]]
    item <- bank[[key]]
    data.frame(
      control_id = derive_control_id(ctrl, 1L),
      question_no = i,
      element_key = key,
      element = item$element,
      interview_question = item$question,
      stringsAsFactors = FALSE
    )
  }))
}

# ---- CSA test-step worksheet (測試步驟設計) ----
# Not only self-check slogans: concrete test procedures, evidence, expected result
control_to_csa <- function(ctrl, elements = DEFAULT_CSA_ELEMENTS) {
  cid <- derive_control_id(ctrl, 1L)
  obj <- nzchar_or(ctrl$control_objective, "（待補控制目標）")
  act <- nzchar_or(ctrl$control_activity, "（待補控制活動）")
  iuc <- nzchar_or(ctrl$iuc_or_system, "（待補 IUC／PBC）")
  outp <- nzchar_or(ctrl$outputs, "執行軌跡／簽核")
  steps <- trimws(unlist(strsplit(as.character(ctrl$review_steps %||% ""), "\n")))
  steps <- steps[nzchar(steps)]
  if (!length(steps)) steps <- act

  rows <- list()
  add_row <- function(element_key, element, purpose, procedure, evidence, expected, sample = "依風險與頻率訂定樣本") {
    rows[[length(rows) + 1]] <<- data.frame(
      `控制點編號` = cid,
      `循環` = ctrl$cycle %||% "",
      `控制目標` = obj,
      `元素` = element,
      `element_key` = element_key,
      `測試步驟序號` = length(rows) + 1L,
      `測試目的` = purpose,
      `測試程序` = procedure,
      `抽樣或範圍` = sample,
      `所需文件_PBC` = evidence,
      `預期結果` = expected,
      `實際結果` = "",
      `例外說明` = "",
      `步驟結論` = "",
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }

  elements <- intersect(as.character(elements %||% character()), names(DESIGN_ELEMENTS))
  if (!length(elements)) {
    return(data.frame(
      `控制點編號` = character(), `循環` = character(), `控制目標` = character(),
      `元素` = character(), element_key = character(), `測試步驟序號` = integer(),
      `測試目的` = character(), `測試程序` = character(), `抽樣或範圍` = character(),
      `所需文件_PBC` = character(), `預期結果` = character(), `實際結果` = character(),
      `例外說明` = character(), `步驟結論` = character(),
      check.names = FALSE, stringsAsFactors = FALSE
    ))
  }

  for (key in elements) {
    if (identical(key, "control_objective")) {
      add_row(key, "控制目標",
              "確認控制目標與風險／聲明對應且可衡量",
              sprintf("訪談／檢視制度，確認目標「%s」未被改寫成活動步驟，並對應風險「%s」", obj, nzchar_or(ctrl$risk_name, "—")),
              "風險矩陣／制度文件／前一年度 RCM",
              "目標清楚、可對應風險與聲明，且與活動文字不同")
    } else if (identical(key, "control_activity")) {
      add_row(key, "控制活動",
              "驗證控制活動實際執行方式與設計一致",
              sprintf("取得執行軌跡，觀察或重行執行活動「%s」之關鍵動作", act),
              paste(iuc, outp, sep = "；"),
              "活動依設計執行，執行人／覆核人角色清楚")
    } else if (identical(key, "steps")) {
      for (s in steps) {
        add_row(key, "Steps（執行步驟）",
                "測試單一執行步驟之有效性",
                sprintf("依步驟執行並留存證據：%s", s),
                paste(iuc, outp, sep = "；"),
                "該步驟有完整執行軌跡且無未結例外")
      }
    } else if (identical(key, "iuc")) {
      add_row(key, "IUC／制度",
              "確認 IUC／PBC 完整正確且與控制依賴一致",
              sprintf("取得「%s」，核對來源、參數、邏輯或產生流程；比對客戶原名與檢視後命名", iuc),
              iuc,
              "IUC 完整正確，足以支撐控制結論")
    } else if (identical(key, "outputs")) {
      add_row(key, "Outputs（產出／軌跡）",
              "確認產出證據足以證明控制已發生",
              sprintf("抽查產出「%s」之完整性、簽核及時性與內容妥適性", outp),
              outp,
              "產出齊備、簽核適當、可追溯至母體")
    } else if (identical(key, "exception")) {
      add_row(key, "例外／調查門檻",
              "確認例外辨識與追蹤有效",
              sprintf("依門檻「%s」選取例外案件，追蹤至結案", nzchar_or(ctrl$investigation_threshold, "（未訂）")),
              paste(outp, "例外追蹤清單", sep = "；"),
              "例外均被辨識且追蹤結案，門檻合理")
    } else if (identical(key, "frequency_owner")) {
      add_row(key, "頻率／負責單位",
              "確認執行頻率與權責符合設計",
              sprintf("檢查「%s」是否由「%s」依設計頻率執行", act, nzchar_or(ctrl$responsible_unit, "負責單位")),
              "權責表／出勤或系統 log／簽核紀錄",
              sprintf("頻率為「%s」且執行者具權限", nzchar_or(ctrl$frequency, "—")))
    } else if (identical(key, "risk")) {
      add_row(key, "循環／風險",
              "確認風險仍適用",
              sprintf("與管理階層確認「%s」風險情境與現行流程", nzchar_or(ctrl$risk_name, "該風險")),
              "流程說明／系統架構／前一年度缺失",
              "風險描述與現況一致")
    } else if (identical(key, "risk_attributes")) {
      add_row(key, "風險三大屬性",
              "確認三大屬性評估仍妥適",
              "覆核財務報導／營運／法令遵循屬性記載是否需更新",
              "風險評估底稿",
              "三大屬性完整且與控制對應")
    } else if (identical(key, "inputs")) {
      add_row(key, "Inputs（投入）",
              "確認投入資訊來源可靠",
              sprintf("追蹤投入「%s」之取得與完整性", nzchar_or(ctrl$inputs, "—")),
              nzchar_or(ctrl$inputs, iuc),
              "投入完整且與母體一致")
    } else if (identical(key, "nature_approach_type")) {
      add_row(key, "Nature／Approach／Type",
              "確認控制屬性分類正確（影響測試策略）",
              sprintf("評估實務是否為 %s／%s／%s", nzchar_or(ctrl$nature, "—"),
                      nzchar_or(ctrl$approach, "—"), nzchar_or(ctrl$type, "—")),
              "控制說明／系統設定截圖",
              "分類正確，測試性質與範圍與之匹配")
    } else if (identical(key, "assertion_account")) {
      add_row(key, "科目／聲明",
              "確認科目與聲明涵蓋完整",
              sprintf("比對 RCM 科目「%s」與聲明「%s」是否遺漏",
                      nzchar_or(ctrl$significant_account, "—"),
                      nzchar_or(ctrl$assertions, "—")),
              "財務報表科目映射／前一年度 RCM",
              "科目與聲明無遺漏")
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

# ---- Gap / deficiency detection ----
# Categories: 缺資訊 | 缺文件 | 控制缺失
detect_design_gaps <- function(ctrl) {
  gaps <- list()
  add <- function(category, severity, item, action) {
    gaps[[length(gaps) + 1]] <<- data.frame(
      control_id = derive_control_id(ctrl, 1L),
      category = category,
      severity = severity,
      gap_item = item,
      suggested_action = action,
      stringsAsFactors = FALSE
    )
  }

  if (is_blank(ctrl$cycle)) add("缺資訊", "高", "未選九大循環", "先選定循環以帶出子作業／範本")
  if (is_blank(ctrl$risk_name) && is_blank(ctrl$risk_description)) {
    add("缺資訊", "高", "缺少風險名稱／描述", "補 RoMM 全文，勿只填編號")
  }
  if (is_blank(ctrl$risk_attr_financial) || is_blank(ctrl$risk_attr_operations) ||
      is_blank(ctrl$risk_attr_compliance)) {
    add("缺資訊", "中", "風險三大屬性不完整", "補財務報導／營運／法令遵循屬性")
  }
  if (is_blank(ctrl$control_objective)) add("缺資訊", "高", "缺少控制目標", "以 Why／對應風險與聲明撰寫")
  if (is_blank(ctrl$control_activity)) add("缺資訊", "高", "缺少控制活動", "以 How／執行作為撰寫，勿重述目標")
  if (is_blank(ctrl$frequency)) add("缺資訊", "中", "缺少控制頻率", "補頻率以決定 CSA 抽樣")
  if (is_blank(ctrl$responsible_unit)) add("缺資訊", "中", "缺少負責單位", "指定 Control Owner")
  if (is_blank(ctrl$approach)) add("缺資訊", "中", "缺少預防／偵測屬性", "每個活動僅對應一種控制屬性")
  if (is_blank(ctrl$nature)) add("缺資訊", "低", "缺少人工／系統執行方式", "補 Nature")
  if (is_blank(ctrl$significant_account)) add("缺資訊", "中", "缺少相關科目", "對應財務報表科目")
  if (is_blank(ctrl$assertions)) add("缺資訊", "中", "缺少相關聲明", "對應 assertion")

  chk <- rcm_objective_activity_check(ctrl$control_objective, ctrl$control_activity)
  if (!isTRUE(chk$ok)) {
    add("控制缺失", "高", chk$msg,
        paste(c("重寫使目標＝Why、活動＝How", chk$hints), collapse = "；"))
  }

  if (is_blank(ctrl$iuc_or_system)) {
    add("缺文件", "高", "未指定 IUC／制度／PBC", "向客戶取 PBC 並登錄命名庫後套用")
  }
  if (is_blank(ctrl$inputs)) {
    add("缺文件", "中", "缺少 Inputs 說明", "補投入報表／資料來源（可附 PBC 對照）")
  }
  if (is_blank(ctrl$outputs)) {
    add("缺文件", "高", "缺少產出／軌跡文件", "確認可驗證證據（簽核、log、調節表）作為 CSA PBC")
  }
  if (is_blank(ctrl$review_steps)) {
    add("缺資訊", "中", "缺少可測試步驟拆分", "拆成 Steps 以產 CSA 測試程序")
  }
  if (is_blank(ctrl$investigation_threshold) &&
      grepl("覆核|Review|調節|Reconcili|偵測", paste(ctrl$type, ctrl$approach, ctrl$control_activity), ignore.case = TRUE)) {
    add("控制缺失", "中", "含覆核／偵測性質但未訂調查門檻", "補門檻與追蹤，否則控制精度不足")
  }
  if (is_blank(ctrl$sub_process) && grepl("資訊|電腦", ctrl$cycle %||% "")) {
    add("缺資訊", "低", "資訊循環未填子作業", "例：存取管理／變更管理／營運／開發")
  }

  if (!length(gaps)) {
    return(data.frame(
      control_id = character(), category = character(), severity = character(),
      gap_item = character(), suggested_action = character(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, gaps)
}

detect_gaps_many <- function(controls) {
  if (!length(controls)) return(detect_design_gaps(list()))
  do.call(rbind, lapply(controls, detect_design_gaps))
}

# Ready-for-RCM? design complete iff no 高 severity gaps and objective/activity split OK
is_rcm_row_ready <- function(ctrl) {
  gaps <- detect_design_gaps(ctrl)
  high <- gaps[gaps$severity == "高", , drop = FALSE]
  list(ready = nrow(high) == 0, gaps = gaps)
}
