# RCM / CSA / interview + gap detection
# RCM: 控制目標(Why) 與 控制活動(How／現況) 嚴格分欄防呆
# 完成一筆控制點設計 = 完成 RCM 一列

# ---- Selectable design elements ----
# Priority: 訪談問題 + RCM 控制點設計 first; CSA second
DESIGN_ELEMENTS <- c(
  risk = "循環／風險",
  risk_attributes = "風險三大屬性／類別",
  control_objective = "控制目標",
  control_activity = "控制活動",
  control_types = "控制類型／活動類型",
  frequency_owner = "頻率／負責單位",
  iuc = "IUC／相關系統",
  company_status = "控制現況描述",
  design_gap = "控制設計差異",
  nature_approach_type = "Nature／Approach／Type（4120SR）",
  inputs = "Inputs（投入）",
  steps = "Steps（執行步驟）",
  outputs = "Outputs／相關文件",
  exception = "例外／調查門檻",
  assertion_account = "科目／聲明"
)

# Phase-1 interview core (對齊 RCM 定稿列)
DEFAULT_INTERVIEW_ELEMENTS <- c(
  "risk", "risk_attributes", "control_objective", "control_activity",
  "control_types", "frequency_owner", "iuc", "company_status", "outputs"
)

# Phase-2 CSA (after interview + RCM)
DEFAULT_CSA_ELEMENTS <- c(
  "control_objective", "control_activity", "steps",
  "iuc", "outputs", "exception", "frequency_owner"
)

# ---- RCM headers learned from 鯨鏈科技_資訊循環_RCM v1 (0820).xlsx ----
# Row1 groups + Row2 columns. Keep related fields grouped; 防呆分欄。
RCM_HEADER_GROUPS <- list(
  `流程資訊` = c("循環名稱", "子作業編號", "子作業名稱"),
  `風險資訊` = c("風險因素", "風險描述", "風險類別", "會計科目"),
  `控制資訊` = c(
    "控制目標", "控制編號", "控制活動", "控制類型", "控制活動類型",
    "控制頻率", "控制現況描述", "控制設計差異說明",
    "相關系統", "相關政策或程序", "相關法令", "相關文件", "流程負責單位"
  ),
  `控制分析與評估` = c("控制有效性評估", "可能潛在風險", "建議改善方式", "設計檢核")
)

RCM_HEADERS <- unlist(RCM_HEADER_GROUPS, use.names = FALSE)

# Exact labels matching workbook row 2 (display aliases cleaned)
RCM_HEADER_LABELS <- c(
  `循環名稱` = "循環名稱",
  `子作業編號` = "子作業編號",
  `子作業名稱` = "子作業名稱",
  `風險因素` = "風險因素（Risk Factor）",
  `風險描述` = "風險描述（Risk Description）",
  `風險類別` = "風險類別（報導面／營運面／遵循面）",
  `會計科目` = "會計科目",
  `控制目標` = "控制目標",
  `控制編號` = "控制編號",
  `控制活動` = "控制活動",
  `控制類型` = "控制類型（人工/自動）",
  `控制活動類型` = "控制活動類型（預防性控制/偵測性控制）",
  `控制頻率` = "控制頻率",
  `控制現況描述` = "控制現況描述",
  `控制設計差異說明` = "控制設計差異說明",
  `相關系統` = "相關系統",
  `相關政策或程序` = "相關政策或程序",
  `相關法令` = "相關法令",
  `相關文件` = "相關文件",
  `流程負責單位` = "流程負責單位",
  `控制有效性評估` = "控制有效性評估（有效/無效）",
  `可能潛在風險` = "可能潛在風險",
  `建議改善方式` = "建議改善方式",
  `設計檢核` = "設計檢核（App 防呆）"
)

RISK_CATEGORY_CHOICES <- c("報導面", "營運面", "遵循面")
CONTROL_TYPE_MANUAL_AUTO <- c("人工", "自動", "人工＋自動")
CONTROL_ACTIVITY_TYPE_PD <- c("預防性控制", "偵測性控制")

strip_attr_label <- function(x) {
  gsub("^\\[[^\\]]+\\]\\s*", "", trimws(as.character(x %||% "")))
}

is_blank <- function(x) !nzchar(trimws(as.character(x %||% "")))

# Map legacy Nature/Approach free text → Jinglian enums
normalize_control_type_manual_auto <- function(x) {
  x <- trimws(as.character(x %||% ""))
  if (!nzchar(x)) return("")
  if (grepl("自動|Automated|系統", x, ignore.case = TRUE) &&
      grepl("人工|Manual|混合", x, ignore.case = TRUE)) return("人工＋自動")
  if (grepl("自動|Automated", x, ignore.case = TRUE)) return("自動")
  if (grepl("人工|Manual", x, ignore.case = TRUE)) return("人工")
  if (x %in% CONTROL_TYPE_MANUAL_AUTO) return(x)
  x
}

normalize_control_activity_type_pd <- function(x) {
  x <- trimws(as.character(x %||% ""))
  if (!nzchar(x)) return("")
  if (grepl("預防|Preventive", x, ignore.case = TRUE)) return("預防性控制")
  if (grepl("偵測|Detective", x, ignore.case = TRUE)) return("偵測性控制")
  if (x %in% CONTROL_ACTIVITY_TYPE_PD) return(x)
  x
}

normalize_risk_category <- function(ctrl) {
  if (!is_blank(ctrl$risk_category)) {
    x <- trimws(ctrl$risk_category)
    if (grepl("報導|財務", x)) return("報導面")
    if (grepl("遵循|法令|合規", x)) return("遵循面")
    if (grepl("營運|作業", x)) return("營運面")
    if (x %in% RISK_CATEGORY_CHOICES) return(x)
  }
  fr <- strip_attr_label(ctrl$risk_attr_financial)
  op <- strip_attr_label(ctrl$risk_attr_operations)
  cp <- strip_attr_label(ctrl$risk_attr_compliance)
  filled <- c(報導面 = nzchar(fr), 營運面 = nzchar(op), 遵循面 = nzchar(cp))
  if (sum(filled) == 1) return(names(filled)[filled][1])
  if (sum(filled) > 1) return("營運面")
  ""
}

# rcm_objective_activity_check() lives in R/objective_activity.R

derive_sub_process_id <- function(ctrl, seq_no = 1L) {
  if (!is_blank(ctrl$sub_process_id)) return(trimws(ctrl$sub_process_id))
  cycle <- ctrl$cycle %||% ""
  if (grepl("資訊|電腦", cycle)) sprintf("EC-%03d", 100L + as.integer(seq_no))
  else sprintf("SP-%03d", as.integer(seq_no))
}

derive_risk_id <- function(ctrl, seq_no = 1L) {
  if (!is_blank(ctrl$risk_id)) return(trimws(ctrl$risk_id))
  # Jinglian uses 風險因素 as qualitative factor name, not ID — keep helper for CSA
  ctrl$risk_name %||% sprintf("RF-%02d", as.integer(seq_no))
}

derive_control_id <- function(ctrl, seq_no = 1L) {
  # Prefer Jinglian style: {子作業編號}-{序號} e.g. EC-101-01
  if (!is_blank(ctrl$control_id) && !grepl("^CD-|^CP-|^IT-C", ctrl$control_id)) {
    return(trimws(ctrl$control_id))
  }
  sp <- derive_sub_process_id(ctrl, seq_no)
  sprintf("%s-%02d", sp, as.integer(seq_no))
}

# 防呆：類型欄位不可混用（人工/自動 ≠ 預防/偵測）
rcm_type_fields_check <- function(control_type, activity_type) {
  ct <- normalize_control_type_manual_auto(control_type)
  at <- normalize_control_activity_type_pd(activity_type)
  issues <- character()
  if (nzchar(as.character(control_type %||% "")) && !nzchar(ct)) {
    issues <- c(issues, "控制類型應為：人工／自動／人工＋自動")
  }
  if (nzchar(as.character(activity_type %||% "")) && !nzchar(at)) {
    issues <- c(issues, "控制活動類型應為：預防性控制／偵測性控制")
  }
  # swapped fields detection
  if (grepl("預防|偵測", as.character(control_type %||% "")) &&
      grepl("人工|自動", as.character(activity_type %||% ""))) {
    issues <- c(issues, "控制類型與控制活動類型疑似對調（前者=人工/自動，後者=預防/偵測）")
  }
  list(ok = !length(issues), msg = if (!length(issues)) "OK" else paste(issues, collapse = "；"),
       control_type = ct, activity_type = at)
}

control_to_rcm_row <- function(ctrl, seq_no = 1L) {
  chk <- rcm_objective_activity_check(ctrl$control_objective, ctrl$control_activity)
  tchk <- rcm_type_fields_check(ctrl$nature %||% ctrl$control_type, ctrl$approach %||% ctrl$control_activity_type)
  status_desc <- ctrl$company_status %||% ctrl$detailed_description %||% ""
  if (is_blank(status_desc) && exists("assemble_control_paragraph", mode = "function")) {
    status_desc <- tryCatch(assemble_control_paragraph(ctrl), error = function(e) "")
  }
  design_gap <- ctrl$design_gap_note %||% ctrl$investigation_threshold %||% ""
  # If OA or type check fails, force 設計檢核
  design_ok <- isTRUE(chk$ok) && isTRUE(tchk$ok)
  design_msg <- if (design_ok) {
    sprintf("通過（目標：%s；活動：%s；類型欄位正確）", chk$objective_verdict, chk$activity_verdict)
  } else {
    paste0("待修：", paste(c(if (!chk$ok) chk$msg, if (!tchk$ok) tchk$msg), collapse = "；"))
  }

  data.frame(
    `循環名稱` = {
      cy <- ctrl$cycle %||% ""
      if (grepl("資訊|電腦", cy)) "資訊循環" else cy
    },
    `子作業編號` = derive_sub_process_id(ctrl, seq_no),
    `子作業名稱` = ctrl$sub_process %||% "",
    `風險因素` = ctrl$risk_factor %||% ctrl$risk_name %||% "",
    `風險描述` = {
      if (!is_blank(ctrl$risk_description)) ctrl$risk_description
      else ctrl$risk_name %||% ""
    },
    `風險類別` = normalize_risk_category(ctrl),
    `會計科目` = {
      ac <- trimws(as.character(ctrl$significant_account %||% ""))
      if (is_reporting_risk_category(ctrl$risk_category %||% "")) {
        if (!nzchar(ac)) "" else ac
      } else {
        ""
      }
    },
    `控制目標` = trimws(ctrl$control_objective %||% ""),
    `控制編號` = derive_control_id(ctrl, seq_no),
    `控制活動` = trimws(ctrl$control_activity %||% ""),
    `控制類型` = tchk$control_type,
    `控制活動類型` = tchk$activity_type,
    `控制頻率` = ctrl$frequency %||% "",
    `控制現況描述` = status_desc,
    `控制設計差異說明` = design_gap,
    `相關系統` = ctrl$related_system %||% ctrl$iuc_or_system %||% "",
    `相關政策或程序` = ctrl$related_policy %||% "",
    `相關法令` = ctrl$related_law %||% "",
    `相關文件` = ctrl$related_document %||% ctrl$outputs %||% "",
    `流程負責單位` = ctrl$responsible_unit %||% "",
    `控制有效性評估` = ctrl$effectiveness %||% "",
    `可能潛在風險` = ctrl$residual_risk %||% "",
    `建議改善方式` = ctrl$improvement %||% "",
    `設計檢核` = design_msg,
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

# Column groups for UI / export presentation
rcm_group_for_column <- function(col) {
  for (g in names(RCM_HEADER_GROUPS)) {
    if (col %in% RCM_HEADER_GROUPS[[g]]) return(g)
  }
  "其他"
}

# ---- Interview (Phase-1: prioritize with RCM) ----
interview_element_bank <- function(ctrl) {
  risk_label <- nzchar_or(ctrl$risk_factor %||% ctrl$risk_name, "該風險")
  risk_desc <- nzchar_or(ctrl$risk_description, "（設計尚未填風險描述）")
  risk_cat <- nzchar_or(normalize_risk_category(ctrl), "（未填類別）")
  obj <- nzchar_or(ctrl$control_objective, "（待補控制目標）")
  act <- nzchar_or(ctrl$control_activity, "（待補控制活動）")
  ct <- normalize_control_type_manual_auto(ctrl$nature %||% ctrl$control_type)
  at <- normalize_control_activity_type_pd(ctrl$approach %||% ctrl$control_activity_type)
  iuc <- nzchar_or(ctrl$related_system %||% ctrl$iuc_or_system, "（待補 IUC／相關系統）")
  status <- nzchar_or(ctrl$company_status, "（尚未書寫現況）")
  gap <- nzchar_or(ctrl$design_gap_note, "（無設計差異說明）")
  outp <- nzchar_or(ctrl$related_document %||% ctrl$outputs, "簽核／軌跡文件")
  list(
    risk = list(
      element = "循環／風險",
      question = sprintf(
        "請說明「%s」下子作業「%s」中，風險因素「%s」如何發生？近期實例？設計描述：%s",
        nzchar_or(ctrl$cycle, "本循環"),
        nzchar_or(ctrl$sub_process, "（子作業）"),
        risk_label, risk_desc
      ),
      evidence = "流程說明／系統架構／前一年度缺失或事件"
    ),
    risk_attributes = list(
      element = "風險三大屬性／類別",
      question = sprintf(
        "此風險類別是否為「%s」？財務報導／營運／法令遵循屬性分別為何？設計：%s｜%s｜%s",
        risk_cat,
        nzchar_or(ctrl$risk_attr_financial, "（未填）"),
        nzchar_or(ctrl$risk_attr_operations, "（未填）"),
        nzchar_or(ctrl$risk_attr_compliance, "（未填）")
      ),
      evidence = "風險評估底稿／RCM 風險資訊欄"
    ),
    control_objective = list(
      element = "控制目標",
      question = sprintf(
        "如何確保達成控制目標「%s」？該目標對應哪些風險與聲明？（請勿用活動步驟回答）",
        obj
      ),
      evidence = "制度／政策／前一年度 RCM 控制目標欄"
    ),
    control_activity = list(
      element = "控制活動",
      question = sprintf(
        "請示範控制活動「%s」：誰執行、誰覆核、用什麼表單／系統？實際步驟為何？",
        act
      ),
      evidence = "操作示範／螢幕錄影或逐步說明"
    ),
    control_types = list(
      element = "控制類型／活動類型",
      question = sprintf(
        "實務上此控制類型是否為「%s」（人工/自動），活動類型是否為「%s」（預防/偵測，僅一種）？",
        nzchar_or(ct, "（未填）"), nzchar_or(at, "（未填）")
      ),
      evidence = "系統設定截圖／職責說明"
    ),
    frequency_owner = list(
      element = "頻率／負責單位",
      question = sprintf(
        "實際執行頻率是否為「%s」？流程負責單位「%s」是否具備權限與能力？有無代理機制？",
        nzchar_or(ctrl$frequency, "所訂頻率"),
        nzchar_or(ctrl$responsible_unit, "負責單位")
      ),
      evidence = "權責表／簽核紀錄／出勤或系統 log"
    ),
    iuc = list(
      element = "IUC／相關系統",
      question = sprintf(
        "執行時使用哪些相關系統／IUC（設計：%s）？如何確保完整性與正確性？可否提供 PBC？",
        iuc
      ),
      evidence = paste(iuc, "PBC 命名對照", sep = "；")
    ),
    company_status = list(
      element = "控制現況描述",
      question = sprintf(
        "請對照設計之控制現況「%s」，說明公司目前實際怎麼做？與設計差異為何？",
        status
      ),
      evidence = "訪談紀錄／現場觀察／現況文件"
    ),
    design_gap = list(
      element = "控制設計差異",
      question = sprintf(
        "設計差異說明記載「%s」。管理階層是否同意？改善時程與負責人？",
        gap
      ),
      evidence = "改善計畫／會議紀錄"
    ),
    nature_approach_type = list(
      element = "Nature／Approach／Type（4120SR）",
      question = sprintf(
        "Form 4120SR 記載性質／取向／類型為「%s／%s／%s」，實務是否一致？",
        nzchar_or(ctrl$nature, "—"), nzchar_or(ctrl$approach, "—"), nzchar_or(ctrl$type, "—")
      ),
      evidence = "控制說明／系統設定"
    ),
    inputs = list(
      element = "Inputs（投入）",
      question = sprintf("執行控制的投入資訊為何（設計：%s）？由誰提供、如何確保完整？",
                        nzchar_or(ctrl$inputs, "待補 Inputs")),
      evidence = nzchar_or(ctrl$inputs, iuc)
    ),
    steps = list(
      element = "Steps（執行步驟）",
      question = sprintf("請依序說明執行步驟：%s", {
        st <- trimws(unlist(strsplit(as.character(ctrl$review_steps %||% ""), "\n")))
        st <- st[nzchar(st)]
        if (!length(st)) "（尚未拆分步驟，請依控制活動說明）"
        else paste(paste0(seq_along(st), ".", st), collapse = "；")
      }),
      evidence = "逐步操作軌跡"
    ),
    outputs = list(
      element = "Outputs／相關文件",
      question = sprintf("控制產出／相關文件為何（預期：%s）？何處可取得？留存多久？", outp),
      evidence = outp
    ),
    exception = list(
      element = "例外／調查門檻",
      question = sprintf(
        "若發現差異，調查門檻與追蹤方式為何%s？",
        if (!is_blank(ctrl$investigation_threshold)) paste0("（設計：", ctrl$investigation_threshold, "）")
        else "（設計尚未訂門檻）"
      ),
      evidence = "例外追蹤清單／結案紀錄"
    ),
    assertion_account = list(
      element = "科目／聲明",
      question = sprintf(
        "此控制涵蓋會計科目「%s」與聲明「%s」是否完整？",
        nzchar_or(ctrl$significant_account, "（未填／NA）"),
        nzchar_or(ctrl$assertions, "（未填）")
      ),
      evidence = "科目映射／前一年度 RCM"
    )
  )
}

empty_interview_df <- function() {
  data.frame(
    `控制編號` = character(), `循環` = character(), `子作業` = character(),
    `控制目標` = character(), `題號` = integer(), `元素` = character(),
    element_key = character(), `訪談問題` = character(),
    `設計摘要` = character(), `預期佐證_PBC` = character(),
    `受訪者回答` = character(), `佐證取得` = character(), `結論` = character(),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

control_to_interview <- function(ctrl, elements = DEFAULT_INTERVIEW_ELEMENTS) {
  bank <- interview_element_bank(ctrl)
  elements <- intersect(as.character(elements %||% character()), names(bank))
  if (!length(elements)) return(empty_interview_df())
  cid <- derive_control_id(ctrl, 1L)
  do.call(rbind, lapply(seq_along(elements), function(i) {
    key <- elements[[i]]
    item <- bank[[key]]
    data.frame(
      `控制編號` = cid,
      `循環` = ctrl$cycle %||% "",
      `子作業` = paste(ctrl$sub_process_id %||% "", ctrl$sub_process %||% "", sep = " "),
      `控制目標` = trimws(ctrl$control_objective %||% ""),
      `題號` = i,
      `元素` = item$element,
      element_key = key,
      `訪談問題` = item$question,
      `設計摘要` = {
        # short design cue for interviewer
        switch(key,
          risk = nzchar_or(ctrl$risk_description, ctrl$risk_factor %||% ""),
          control_objective = ctrl$control_objective %||% "",
          control_activity = ctrl$control_activity %||% "",
          company_status = ctrl$company_status %||% "",
          iuc = ctrl$iuc_or_system %||% "",
          nzchar_or(item$evidence, "")
        )
      },
      `預期佐證_PBC` = item$evidence %||% "",
      `受訪者回答` = "",
      `佐證取得` = "",
      `結論` = "",
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }))
}

# Only finalized RCM rows (設計完成＝RCM一列) feed interview by default
controls_to_interview <- function(controls, elements = DEFAULT_INTERVIEW_ELEMENTS,
                                  finalized_only = TRUE) {
  if (!length(controls)) return(empty_interview_df())
  if (isTRUE(finalized_only)) {
    controls <- Filter(function(c) {
      isTRUE(c$rcm_ready$ready) || isTRUE(is_rcm_row_ready(c)$ready)
    }, controls)
  }
  if (!length(controls)) return(empty_interview_df())
  do.call(rbind, lapply(controls, control_to_interview, elements = elements))
}

# ---- CSA test-step worksheet (Phase-2: after interview + RCM) ----
# Not only self-check slogans: concrete test procedures, evidence, expected result
control_to_csa <- function(ctrl, elements = DEFAULT_CSA_ELEMENTS) {
  cid <- derive_control_id(ctrl, 1L)
  obj <- nzchar_or(ctrl$control_objective, "（待補控制目標）")
  act <- nzchar_or(ctrl$control_activity, "（待補控制活動）")
  iuc <- nzchar_or(ctrl$related_system %||% ctrl$iuc_or_system, "（待補 IUC／PBC）")
  outp <- nzchar_or(ctrl$related_document %||% ctrl$outputs, "執行軌跡／簽核")
  steps <- trimws(unlist(strsplit(as.character(ctrl$review_steps %||% ""), "\n")))
  steps <- steps[nzchar(steps)]
  if (!length(steps)) {
    # derive crude steps from activity for CSA when Steps blank
    steps <- act
  }

  rows <- list()
  add_row <- function(element_key, element, purpose, procedure, evidence, expected,
                      sample = NULL) {
    if (is.null(sample)) {
      sample <- sprintf("依頻率「%s」與風險「%s」訂定樣本",
                        nzchar_or(ctrl$frequency, "—"),
                        nzchar_or(ctrl$risk_factor %||% ctrl$risk_name, "—"))
    }
    rows[[length(rows) + 1]] <<- data.frame(
      `控制編號` = cid,
      `循環` = ctrl$cycle %||% "",
      `子作業` = paste(ctrl$sub_process_id %||% "", ctrl$sub_process %||% "", sep = " "),
      `控制目標` = obj,
      `控制活動` = act,
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
      `控制編號` = character(), `循環` = character(), `子作業` = character(),
      `控制目標` = character(), `控制活動` = character(),
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
              "確認控制目標與風險／聲明對應且可衡量（對照 RCM 列）",
              sprintf("訪談／檢視制度，確認目標「%s」未被改寫成活動步驟，並對應風險「%s」",
                      obj, nzchar_or(ctrl$risk_factor %||% ctrl$risk_name, "—")),
              "風險矩陣／制度文件／RCM",
              "目標清楚、可對應風險與聲明，且與活動文字不同")
    } else if (identical(key, "control_activity")) {
      add_row(key, "控制活動",
              "驗證控制活動實際執行方式與 RCM 設計一致",
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
      add_row(key, "IUC／相關系統",
              "確認 IUC／PBC 完整正確且與控制依賴一致",
              sprintf("取得「%s」，核對來源、參數、邏輯或產生流程；比對客戶原名與檢視後命名", iuc),
              iuc,
              "IUC 完整正確，足以支撐控制結論")
    } else if (identical(key, "outputs")) {
      add_row(key, "Outputs／相關文件",
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
              sprintf("與管理階層確認「%s」風險情境與現行流程", nzchar_or(ctrl$risk_factor %||% ctrl$risk_name, "該風險")),
              "流程說明／系統架構／前一年度缺失",
              "風險描述與現況一致")
    } else if (identical(key, "risk_attributes")) {
      add_row(key, "風險三大屬性／類別",
              "確認風險類別與三大屬性評估仍妥適",
              sprintf("覆核風險類別「%s」及財務報導／營運／法令遵循屬性是否需更新",
                      nzchar_or(normalize_risk_category(ctrl), "—")),
              "風險評估底稿／RCM",
              "類別與屬性完整且與控制對應")
    } else if (identical(key, "inputs")) {
      add_row(key, "Inputs（投入）",
              "確認投入資訊來源可靠",
              sprintf("追蹤投入「%s」之取得與完整性", nzchar_or(ctrl$inputs, "—")),
              nzchar_or(ctrl$inputs, iuc),
              "投入完整且與母體一致")
    } else if (identical(key, "nature_approach_type") || identical(key, "control_types")) {
      add_row(key, DESIGN_ELEMENTS[[key]] %||% "控制類型",
              "確認控制屬性分類正確（影響測試策略）",
              sprintf("評估實務是否為控制類型「%s」／活動類型「%s」",
                      nzchar_or(normalize_control_type_manual_auto(ctrl$nature), "—"),
                      nzchar_or(normalize_control_activity_type_pd(ctrl$approach), "—")),
              "控制說明／系統設定截圖／RCM",
              "分類正確，測試性質與範圍與之匹配")
    } else if (identical(key, "assertion_account")) {
      add_row(key, "科目／聲明",
              "確認科目與聲明涵蓋完整",
              sprintf("比對 RCM 科目「%s」與聲明「%s」是否遺漏",
                      nzchar_or(ctrl$significant_account, "—"),
                      nzchar_or(ctrl$assertions, "—")),
              "財務報表科目映射／前一年度 RCM",
              "科目與聲明無遺漏")
    } else if (identical(key, "company_status")) {
      add_row(key, "控制現況描述",
              "確認現況與設計一致或差異已記錄",
              sprintf("比對現況描述與實地觀察：%s", nzchar_or(ctrl$company_status, "（未填）")),
              "訪談紀錄／現場觀察",
              "現況可驗證；差異已於 RCM 設計差異欄揭露")
    } else if (identical(key, "design_gap")) {
      add_row(key, "控制設計差異",
              "確認設計差異改善追蹤",
              sprintf("追蹤差異「%s」之改善狀態", nzchar_or(ctrl$design_gap_note, "（無）")),
              "改善計畫",
              "差異有負責人與時程，或已關閉")
    }
  }
  do.call(rbind, rows)
}

controls_to_csa <- function(controls, elements = DEFAULT_CSA_ELEMENTS,
                            finalized_only = TRUE) {
  if (!length(controls)) return(control_to_csa(list(), elements))
  if (isTRUE(finalized_only)) {
    controls <- Filter(function(c) {
      isTRUE(c$rcm_ready$ready) || isTRUE(is_rcm_row_ready(c)$ready)
    }, controls)
  }
  if (!length(controls)) return(control_to_csa(list(), elements))
  do.call(rbind, lapply(controls, control_to_csa, elements = elements))
}

# ---- 設計必填欄位（對齊鯨鏈 RCM 核心欄；定稿＝RCM 一列前置）----
# 選填：控制設計差異／相關政策法令文件／有效性評估／4120SR 進階欄
# 會計科目：僅風險類別＝報導面時必填；其餘類別鎖定不可填
DESIGN_REQUIRED_FIELDS <- c(
  cycle = "循環名稱",
  sub_process_id = "子作業編號",
  sub_process = "子作業名稱",
  risk_factor = "風險因素",
  risk_description = "風險描述",
  risk_category = "風險類別",
  control_objective = "控制目標",
  control_activity = "控制活動",
  nature = "控制類型（人工／自動）",
  approach = "控制活動類型（預防／偵測）",
  frequency = "控制頻率",
  responsible_unit = "流程負責單位",
  iuc_or_system = "相關系統／IUC"
)

DESIGN_OPTIONAL_FIELDS <- c(
  significant_account = "會計科目（僅報導面必填；其它類別不可填）",
  related_law = "相關法令（僅遵循面必填；其它類別不可填）",
  company_status = "控制現況描述（六大就緒後書寫；定稿時可自動帶入）",
  design_gap_note = "控制設計差異說明",
  related_policy = "相關政策或程序",
  related_document = "相關文件",
  effectiveness = "控制有效性評估",
  residual_risk = "可能潛在風險",
  improvement = "建議改善方式"
)

is_reporting_risk_category <- function(cat) {
  grepl("^報導", trimws(as.character(cat %||% "")))
}

is_compliance_risk_category <- function(cat) {
  grepl("^遵循", trimws(as.character(cat %||% "")))
}

# TRUE if account is considered "filled" for 報導面
account_is_filled <- function(x) {
  v <- trimws(as.character(x %||% ""))
  nzchar(v) && !identical(toupper(v), "NA") && !identical(v, "—") && !identical(v, "-")
}

law_is_filled <- function(x) {
  v <- trimws(as.character(x %||% ""))
  # allow multi values joined by ； or ;
  vals <- unlist(strsplit(v, "[;；|/]+"))
  vals <- trimws(vals)
  any(nzchar(vals))
}

# Resolve field value with aliases used across cascade / RCM / Form 4120SR
design_field_value <- function(ctrl, field) {
  ctrl <- as.list(ctrl)
  if (identical(field, "risk_factor")) {
    return(trimws(as.character(
      ctrl$risk_factor %||% ctrl$risk_name %||% ""
    )))
  }
  if (identical(field, "iuc_or_system")) {
    return(trimws(as.character(
      ctrl$iuc_or_system %||% ctrl$related_system %||% ""
    )))
  }
  if (identical(field, "nature")) {
    return(trimws(as.character(
      normalize_control_type_manual_auto(ctrl$nature %||% ctrl$control_type)
    )))
  }
  if (identical(field, "approach")) {
    at <- normalize_control_activity_type_pd(ctrl$approach %||% ctrl$control_activity_type)
    if (exists("normalize_single_activity_type", mode = "function")) {
      at2 <- normalize_single_activity_type(ctrl$approach %||% ctrl$control_activity_type)
      if (nzchar(at2)) at <- at2
    }
    return(trimws(as.character(at %||% "")))
  }
  trimws(as.character(ctrl[[field]] %||% ""))
}

# Returns ok + missing Chinese labels for 設計必填欄位
design_required_check <- function(ctrl) {
  missing <- character()
  filled <- list()
  for (f in names(DESIGN_REQUIRED_FIELDS)) {
    val <- design_field_value(ctrl, f)
    filled[[f]] <- nzchar(val)
    if (!nzchar(val)) missing <- c(missing, DESIGN_REQUIRED_FIELDS[[f]])
  }
  # Extra rule: approach must be exactly one 預防/偵測
  if (isTRUE(filled$approach) && exists("activity_type_ok", mode = "function") &&
      !activity_type_ok(ctrl$approach %||% ctrl$control_activity_type)) {
    missing <- c(missing, "控制活動類型須為單一預防／偵測（不可混用）")
    filled$approach <- FALSE
  }
  # 會計科目：報導面必填；其它類別不得填入（僅允許空白／NA）
  cat <- design_field_value(ctrl, "risk_category")
  acct <- trimws(as.character(ctrl$significant_account %||% ""))
  if (is_reporting_risk_category(cat)) {
    filled$significant_account <- account_is_filled(acct)
    if (!account_is_filled(acct)) {
      missing <- c(missing, "會計科目（報導面必填）")
    }
  } else if (nzchar(cat)) {
    filled$significant_account <- !account_is_filled(acct)
    if (account_is_filled(acct)) {
      missing <- c(missing, "會計科目僅報導面可填（請清空）")
    }
  } else {
    filled$significant_account <- TRUE
  }
  # 相關法令：遵循面必填；其它類別不得填入
  law <- trimws(as.character(ctrl$related_law %||% ""))
  if (is_compliance_risk_category(cat)) {
    filled$related_law <- law_is_filled(law)
    if (!law_is_filled(law)) {
      missing <- c(missing, "相關法令（遵循面必填）")
    }
  } else if (nzchar(cat)) {
    filled$related_law <- !law_is_filled(law)
    if (law_is_filled(law)) {
      missing <- c(missing, "相關法令僅遵循面可填（請清空）")
    }
  } else {
    filled$related_law <- TRUE
  }
  list(
    ok = !length(missing),
    missing = unique(missing),
    filled = filled,
    required = DESIGN_REQUIRED_FIELDS,
    optional = DESIGN_OPTIONAL_FIELDS,
    account_mode = if (is_reporting_risk_category(cat)) "required" else if (nzchar(cat)) "locked" else "pending",
    law_mode = if (is_compliance_risk_category(cat)) "required" else if (nzchar(cat)) "locked" else "pending"
  )
}

# ---- Gap / deficiency detection ----
# Categories: 缺資訊 | 缺文件 | 控制缺失
# 必填缺漏 → 高（阻擋定稿）；選填／4120SR 輔助 → 中／低
detect_design_gaps <- function(ctrl) {
  gaps <- list()
  add <- function(category, severity, item, action) {
    cid <- tryCatch(as.character(derive_control_id(ctrl, 1L)), error = function(e) "")
    if (!length(cid) || is.na(cid[[1]])) cid <- ""
    cid <- cid[[1]]
    item <- paste(as.character(item %||% ""), collapse = "；")
    action <- paste(as.character(action %||% ""), collapse = "；")
    if (!nzchar(item)) item <- "（未命名缺漏）"
    gaps[[length(gaps) + 1]] <<- data.frame(
      control_id = cid,
      category = as.character(category[[1]] %||% ""),
      severity = as.character(severity[[1]] %||% ""),
      gap_item = item,
      suggested_action = action,
      stringsAsFactors = FALSE
    )
  }

  req <- design_required_check(ctrl)
  if (!isTRUE(req$ok)) {
    for (lab in req$missing) {
      add("缺資訊", "高", paste0("必填未填：", lab), "補齊設計必填欄位後才可定稿為 RCM 一列")
    }
  }

  if (is_blank(ctrl$risk_name) && is_blank(ctrl$risk_description) &&
      is_blank(ctrl$risk_factor)) {
    add("缺資訊", "高", "缺少風險名稱／描述", "補 RoMM 全文，勿只填編號")
  }
  if (is_blank(ctrl$risk_category) &&
      (is_blank(ctrl$risk_attr_financial) || is_blank(ctrl$risk_attr_operations) ||
       is_blank(ctrl$risk_attr_compliance))) {
    add("缺資訊", "中", "風險三大屬性細節不完整（類別已另列必填）",
        "可於進階區補財務報導／營運／遵循細節")
  }

  tchk <- rcm_type_fields_check(ctrl$nature %||% ctrl$control_type,
                                ctrl$approach %||% ctrl$control_activity_type)
  if (!isTRUE(tchk$ok)) {
    add("控制缺失", "高", tchk$msg %||% "類型欄錯誤",
        "控制類型＝人工/自動；控制活動類型＝預防/偵測，勿對調")
  }
  if (is_blank(ctrl$assertions)) add("缺資訊", "中", "缺少相關聲明", "對應 assertion（4120SR 輔助）")

  chk <- rcm_objective_activity_check(ctrl$control_objective, ctrl$control_activity)
  if (!isTRUE(chk$ok)) {
    oa_msg <- chk$msg %||% paste(chk$issues %||% character(), collapse = "；")
    if (!nzchar(oa_msg)) oa_msg <- "控制目標／活動分欄未通過"
    add("控制缺失", "高", oa_msg,
        paste(c("重寫使目標＝Why、活動＝How", chk$hints %||% character()), collapse = "；"))
  }

  if (is_blank(ctrl$inputs)) {
    add("缺文件", "中", "缺少 Inputs 說明", "補投入報表／資料來源（可附 PBC 對照）")
  }
  if (is_blank(ctrl$outputs) && is_blank(ctrl$related_document)) {
    add("缺文件", "中", "缺少產出／相關文件（選填）",
        "建議補可驗證證據（簽核、log、調節表）供 CSA／PBC")
  }
  if (is_blank(ctrl$review_steps) && is_blank(ctrl$company_status)) {
    add("缺資訊", "中", "缺少控制現況描述或可測試步驟",
        "六大就緒後書寫現況；定稿時可自動帶入草稿")
  }
  if (is_blank(ctrl$investigation_threshold) &&
      grepl("覆核|Review|調節|Reconcili|偵測",
            paste(ctrl$type %||% "", ctrl$approach %||% "", ctrl$control_activity %||% "",
                  sep = " "),
            ignore.case = TRUE)) {
    add("控制缺失", "中", "含覆核／偵測性質但未訂調查門檻", "補門檻與追蹤，否則控制精度不足")
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

# Ready-for-RCM? 必填齊＋無高嚴重度缺漏＋目標/活動分欄 OK
is_rcm_row_ready <- function(ctrl) {
  gaps <- detect_design_gaps(ctrl)
  high <- gaps[gaps$severity == "高", , drop = FALSE]
  req <- design_required_check(ctrl)
  list(
    ready = isTRUE(req$ok) && nrow(high) == 0,
    gaps = gaps,
    required = req
  )
}

# Invariant: 設計控制點階段完成 ＝ 完成 RCM 其中一列
# Finalize a designed control into a single RCM-row-ready control object.
finalize_control_as_rcm_row <- function(ctrl, existing_ids = character(), seq_hint = 1L) {
  ctrl <- as.list(ctrl)
  # 會計科目：報導面保留；其它類別強制清空
  if (is_reporting_risk_category(ctrl$risk_category %||% "")) {
    if (identical(toupper(trimws(ctrl$significant_account %||% "")), "NA")) {
      ctrl$significant_account <- ""
    }
  } else {
    ctrl$significant_account <- ""
  }
  # 相關法令：遵循面保留；其它類別強制清空
  if (!is_compliance_risk_category(ctrl$risk_category %||% "")) {
    ctrl$related_law <- ""
  }
  if (is_blank(ctrl$risk_name) && !is_blank(ctrl$risk_factor)) {
    ctrl$risk_name <- ctrl$risk_factor
  }
  if (is_blank(ctrl$risk_factor) && !is_blank(ctrl$risk_name)) {
    ctrl$risk_factor <- ctrl$risk_name
  }
  if (is_blank(ctrl$iuc_or_system) && !is_blank(ctrl$related_system)) {
    ctrl$iuc_or_system <- ctrl$related_system
  }
  if (is_blank(ctrl$related_system) && !is_blank(ctrl$iuc_or_system)) {
    ctrl$related_system <- ctrl$iuc_or_system
  }

  req <- design_required_check(ctrl)
  if (!isTRUE(req$ok)) {
    return(list(
      ok = FALSE, ready = FALSE, gaps = detect_design_gaps(ctrl),
      control = NULL, rcm_row = NULL, required = req,
      msg = paste0("必填未齊：", paste(req$missing, collapse = "、"))
    ))
  }

  ready <- is_rcm_row_ready(ctrl)
  if (!isTRUE(ready$ready)) {
    return(list(
      ok = FALSE,
      ready = FALSE,
      gaps = ready$gaps,
      control = NULL,
      rcm_row = NULL,
      required = req,
      msg = paste(
        ready$gaps$gap_item[ready$gaps$severity == "高"],
        collapse = "；"
      )
    ))
  }
  oa <- rcm_objective_activity_check(ctrl$control_objective, ctrl$control_activity)
  if (!isTRUE(oa$ok)) {
    return(list(ok = FALSE, ready = FALSE, gaps = ready$gaps, control = NULL,
                rcm_row = NULL, required = req, msg = oa$msg))
  }
  if (exists("activity_type_ok", mode = "function") && !activity_type_ok(ctrl$approach)) {
    return(list(ok = FALSE, ready = FALSE, gaps = ready$gaps, control = NULL,
                rcm_row = NULL, required = req,
                msg = "控制活動須對應單一預防／偵測屬性"))
  }

  spid <- ctrl$sub_process_id %||% ""
  if (!nzchar(trimws(spid)) && exists("derive_sub_process_id", mode = "function")) {
    spid <- derive_sub_process_id(ctrl, seq_hint)
  }
  ctrl$sub_process_id <- spid
  if (!nzchar(trimws(ctrl$control_id %||% "")) || grepl("^CD-", ctrl$control_id %||% "")) {
    if (exists("next_rcm_control_id", mode = "function")) {
      ctrl$control_id <- next_rcm_control_id(spid, existing_ids)
    } else {
      ctrl$control_id <- derive_control_id(ctrl, seq_hint)
    }
  }
  if (is_blank(ctrl$company_status)) {
    if (exists("assemble_control_paragraph", mode = "function")) {
      ctrl$company_status <- tryCatch(assemble_control_paragraph(ctrl), error = function(e) "")
    }
  }
  if (exists("assemble_summary_description", mode = "function")) {
    ctrl$summary_description <- tryCatch(assemble_summary_description(ctrl), error = function(e) "")
  }
  ctrl$detailed_description <- ctrl$company_status %||% ctrl$detailed_description %||% ""
  ctrl$rcm_ready <- list(ready = TRUE, gaps = ready$gaps)
  ctrl$validation <- if (exists("validate_control_design", mode = "function")) {
    validate_control_design(ctrl)
  } else list(ok = TRUE, missing = character())

  row <- control_to_rcm_row(ctrl, seq_no = seq_hint)
  # Parity check: designed control id == RCM 控制編號
  if (!identical(as.character(ctrl$control_id), as.character(row[["控制編號"]]))) {
    ctrl$control_id <- as.character(row[["控制編號"]])
  }
  list(
    ok = TRUE,
    ready = TRUE,
    gaps = ready$gaps,
    control = ctrl,
    rcm_row = row,
    msg = sprintf("已完成設計＝RCM 一列（%s）", ctrl$control_id)
  )
}

# Always true by construction when using controls_to_rcm
assert_design_rcm_parity <- function(controls) {
  n <- length(controls)
  rcm <- controls_to_rcm(controls)
  ids_c <- if (!n) character() else vapply(controls, function(x) as.character(x$control_id %||% ""), "")
  ids_r <- if (!nrow(rcm)) character() else as.character(rcm[["控制編號"]])
  list(
    ok = (n == nrow(rcm)) && identical(ids_c, ids_r),
    n_controls = n,
    n_rcm_rows = nrow(rcm),
    control_ids = ids_c,
    rcm_ids = ids_r
  )
}
