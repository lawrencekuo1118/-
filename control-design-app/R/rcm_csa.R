# RCM / CSA / interview + gap detection
# RCM: 控制目標(Why) 與 控制活動(How) 嚴格分欄防呆
# 完成一筆控制點設計 = 完成 RCM 一列（設計欄位；現況／分析評估留空）

# ---- Selectable design elements ----
# Three workflow tabs: 訪談問項設計 → 風險控制點設計 → 控制點測試設計
# Priority: 風險控制點設計（RCM 列）→ 訪談問項 → 控制點測試
# Supporting: 範本庫／參數庫（側邊欄最下方）→ PBC資料庫 → RCM
DESIGN_ELEMENTS <- c(
  risk = "循環／風險",
  risk_attributes = "風險三大屬性／類別",
  control_objective = "控制目標",
  control_activity = "控制活動",
  control_types = "控制類型／活動類型",
  frequency_owner = "頻率／負責單位",
  iuc = "IUC",
  company_status = "控制現況描述",
  design_gap = "控制設計差異",
  nature_approach_type = "Nature／Approach／Type（4120SR）",
  inputs = "Inputs（投入）",
  steps = "Steps（執行步驟）",
  outputs = "Outputs／控制佐證文件",
  exception = "例外／調查門檻",
  assertion_account = "科目／聲明"
)

# IUC 與 相關系統為獨立欄位；iuc_or_system 保留向後相容（僅對應 IUC）
ctrl_iuc_value <- function(ctrl) {
  join_text_list_values(ctrl$iuc %||% ctrl$iuc_or_system %||% "")
}

ctrl_related_system_value <- function(ctrl) {
  trimws(as.character(ctrl$related_system %||% ""))
}

sync_iuc_aliases <- function(ctrl) {
  iuc <- ctrl_iuc_value(ctrl)
  if (nzchar(iuc)) {
    ctrl$iuc <- iuc
    ctrl$iuc_or_system <- iuc
  }
  ctrl
}

# 訪談問項設計
# 主目標：針對不同循環／子作業下之「預期風險」與「預期控制目標／活動」深入且快速了解
# 每題答案必須含人事時地物鏈：
#   以何頻率 → 誰取得什麼文件或資訊(IUC) → 做什麼（具體控制行為）→ 才會進行什麼下一步
DEFAULT_INTERVIEW_ELEMENTS <- c(
  "risk", "control_objective", "control_activity"
)

# 訪談焦點勾選標籤（主軸＝預期風險／目標／活動）
INTERVIEW_ELEMENTS <- c(
  risk = "預期風險（循環／子作業）",
  risk_attributes = "風險類別／屬性",
  control_objective = "預期控制目標",
  control_activity = "預期控制活動（走查）",
  control_types = "控制類型／活動類型",
  frequency_owner = "頻率／權責",
  iuc = "IUC／PBC",
  company_status = "控制現況描述",
  design_gap = "控制設計差異",
  nature_approach_type = "Nature／Approach／Type",
  inputs = "Inputs（投入）",
  steps = "Steps（逐步現況）",
  outputs = "Outputs／產出",
  exception = "例外／門檻",
  assertion_account = "科目／聲明"
)

# 完整走查時可一鍵擴充的元素（頻率／IUC／步驟／產出／例外）
INTERVIEW_WALKTHROUGH_EXTRA <- c(
  "frequency_owner", "iuc", "steps", "outputs", "exception"
)

INTERVIEW_ANSWER_SCAFFOLD <- paste0(
  "請以人事時地物回答：",
  "以何頻率 → ",
  "誰取得什麼文件或資訊(IUC) → ",
  "做什麼（具體控制行為）→ ",
  "才會進行什麼下一步"
)

# 可勾選之 5W1H 模組（拼湊組建；預設組合成上列鏈；What 可串 PBC）
INTERVIEW_5W1H_MODULES <- c(
  when = "以何頻率",
  who = "誰（執行／覆核）",
  what = "取得什麼文件或資訊(IUC)",
  how = "做什麼（具體控制行為）",
  next_step = "才會進行什麼下一步"
)

DEFAULT_INTERVIEW_5W1H <- names(INTERVIEW_5W1H_MODULES)

# 各模組對應之獨立探針題（可依勾選拼湊成題綱列）
INTERVIEW_5W1H_PROBE_LABELS <- c(
  when = "模組｜以何頻率",
  who = "模組｜誰",
  what = "模組｜IUC／PBC",
  how = "模組｜具體控制行為",
  next_step = "模組｜下一步"
)

# 自我評估／控制點測試設計（控制點定稿後）
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
  `相關文件` = CONTROL_EVIDENCE_DOCUMENT_LABEL,
  `流程負責單位` = "流程負責單位",
  `控制有效性評估` = "控制有效性評估（有效/無效）",
  `可能潛在風險` = "可能潛在風險",
  `建議改善方式` = "建議改善方式",
  `設計檢核` = "設計檢核（App 防呆）"
)

RISK_CATEGORY_CHOICES <- c("報導面", "營運面", "遵循面")
CONTROL_TYPE_MANUAL_AUTO <- c("人工", "自動")
CONTROL_ACTIVITY_TYPE_PD <- c("預防性控制", "偵測性控制")

strip_attr_label <- function(x) {
  gsub("^\\[[^\\]]+\\]\\s*", "", trimws(as.character(x %||% "")))
}

is_blank <- function(x) !nzchar(trimws(as.character(x %||% "")))

# Map legacy Nature/Approach free text → Jinglian enums
normalize_control_type_manual_auto <- function(x) {
  x <- trimws(as.character(x %||% ""))
  if (!nzchar(x)) return("")
  if (grepl("人工.*[＋\\+].*自動|自動.*[＋\\+].*人工|混合", x)) return("")
  if (x %in% CONTROL_TYPE_MANUAL_AUTO) return(x)
  if (grepl("自動|Automated", x, ignore.case = TRUE)) return("自動")
  if (grepl("人工|Manual", x, ignore.case = TRUE)) return("人工")
  ""
}

# 自動控制 → 頻率固定為持續
resolve_control_frequency <- function(nature, frequency) {
  ct <- normalize_control_type_manual_auto(nature)
  if (identical(ct, "自動")) return("持續")
  trimws(as.character(frequency %||% ""))
}

# ---------------------------------------------------------------------------
# 控制測試抽樣（CSA）— PCAOB AS 2301／AS 2315 ＋ Deloitte 頻率對應表
# ---------------------------------------------------------------------------
# PCAOB：測試性質／時間／範圍須回應 RoMM（AS 2301）；屬性抽樣樣本數考量
# 可容忍偏差、預期偏差與過度依賴風險，控制發生頻率決定期間內母體（AS 2315）。
# Deloitte 實務（營運有效性、預期偏差≈0、計畫依賴）：以控制頻率對應最低樣本數；
# Higher RoMM／Fraud／估計高風險時上調。

CONTROL_TEST_SAMPLE_BASE <- c(
  "每年" = 1L,
  "每半年" = 2L,
  "每季" = 2L,
  "每月" = 3L,
  "每週" = 10L,
  "每日" = 25L,
  "即時／每筆交易" = 25L,
  "持續" = 1L,
  "事件觸發（自訂）" = NA_integer_,
  "其他（自訂）" = NA_integer_
)

CONTROL_TEST_SAMPLE_HIGHER <- c(
  "每年" = 1L,
  "每半年" = 2L,
  "每季" = 3L,
  "每月" = 5L,
  "每週" = 15L,
  "每日" = 40L,
  "即時／每筆交易" = 40L,
  "持續" = 2L,
  "事件觸發（自訂）" = NA_integer_,
  "其他（自訂）" = NA_integer_
)

is_higher_control_romm <- function(romm) {
  x <- trimws(as.character(romm %||% ""))
  if (!nzchar(x)) return(FALSE)
  # 「Not higher risk…」不得視為高風險
  if (grepl("Not higher risk", x, ignore.case = TRUE)) return(FALSE)
  grepl("Higher risk associated|Fraud risk|estimate\\s*—\\s*higher|估計.*高風險",
        x, ignore.case = TRUE)
}

normalize_sample_frequency_key <- function(frequency) {
  f <- trimws(as.character(frequency %||% ""))
  if (!nzchar(f)) return("")
  if (f %in% names(CONTROL_TEST_SAMPLE_BASE)) return(f)
  if (grepl("持續|連續|自動", f)) return("持續")
  if (grepl("每筆|即時|多次|交易", f)) return("即時／每筆交易")
  if (grepl("每日|天天|日結", f)) return("每日")
  if (grepl("每週|週", f)) return("每週")
  if (grepl("每月|月結", f)) return("每月")
  if (grepl("每季|季", f)) return("每季")
  if (grepl("半年|半年度", f)) return("每半年")
  if (grepl("每年|年度|年結", f)) return("每年")
  if (grepl("事件|觸發", f)) return("事件觸發（自訂）")
  "其他（自訂）"
}

#' 依控制實際頻率（及 RoMM）產出控制測試建議樣本數與抽樣範圍說明
control_test_sample_plan <- function(ctrl) {
  freq_raw <- resolve_control_frequency(
    ctrl$nature %||% "",
    ctrl$frequency %||% ""
  )
  freq_key <- normalize_sample_frequency_key(freq_raw)
  if (!nzchar(freq_key) && nzchar(freq_raw)) freq_key <- "其他（自訂）"
  higher <- is_higher_control_romm(ctrl$romm_classification)
  nature <- normalize_control_type_manual_auto(ctrl$nature)
  automated <- identical(nature, "自動") || identical(freq_key, "持續")

  tbl <- if (isTRUE(higher)) CONTROL_TEST_SAMPLE_HIGHER else CONTROL_TEST_SAMPLE_BASE
  n <- if (nzchar(freq_key) && freq_key %in% names(tbl)) unname(tbl[[freq_key]]) else NA_integer_

  methodology <- paste0(
    "PCAOB AS 2301／AS 2315；Deloitte 控制頻率樣本表",
    if (isTRUE(higher)) "（Higher RoMM／Fraud 上調）" else "（基準）"
  )

  if (isTRUE(automated)) {
    n_rep <- if (is.na(n)) 1L else as.integer(n)
    sample_label <- sprintf("Test of one＋再執行 %d 筆", n_rep)
    approach <- "自動化／持續：系統邏輯 Test of one＋ITGC；再執行驗證"
    scope <- sprintf(
      "控制頻率「%s」（自動／持續）。建議：測試應用系統設定／邏輯（Test of one）並依賴相關 ITGC；另再執行至少 %d 筆以驗證營運有效性。%s",
      if (nzchar(freq_raw)) freq_raw else "持續",
      n_rep,
      methodology
    )
  } else if (identical(freq_key, "事件觸發（自訂）") || identical(freq_key, "其他（自訂）") ||
             is.na(n)) {
    sample_label <- "依期間發生次數／母體"
    approach <- "依實際發生次數全數或屬性抽樣"
    scope <- sprintf(
      "控制頻率「%s」。建議：先確定測試期間內實際發生次數（母體）；發生次數少則全數測試，否則依屬性抽樣（可容忍／預期偏差、過度依賴風險）訂定樣本。%s",
      if (nzchar(freq_raw)) freq_raw else "（未訂）",
      methodology
    )
    n <- NA_integer_
  } else if (identical(freq_key, "即時／每筆交易")) {
    sample_label <- as.character(as.integer(n))
    approach <- "屬性抽樣（高頻／每筆交易；可視母體擴大）"
    scope <- sprintf(
      "控制頻率「%s」。建議最低樣本數 %d 筆（屬性抽樣）；母體很大或風險較高時可擴大至 40–60 筆。選樣應涵蓋期間並可追溯至完整母體。%s",
      freq_raw, as.integer(n), methodology
    )
  } else {
    sample_label <- as.character(as.integer(n))
    approach <- "屬性抽樣（依控制發生頻率）"
    scope <- sprintf(
      "控制頻率「%s」。建議最低樣本數 %d 筆（測試期間內控制發生之屬性抽樣；預期偏差≈0）。選樣應涵蓋期間並可追溯至完整母體。%s",
      freq_raw, as.integer(n), methodology
    )
  }

  list(
    frequency = if (nzchar(freq_raw)) freq_raw else freq_key,
    frequency_key = freq_key,
    sample_size = if (is.na(n)) NA_integer_ else as.integer(n),
    sample_size_label = sample_label,
    higher_risk = isTRUE(higher),
    automated = isTRUE(automated),
    approach = approach,
    methodology = methodology,
    scope_text = scope
  )
}

empty_csa_frame <- function() {
  data.frame(
    `控制編號` = character(), `循環` = character(), `子作業` = character(),
    `控制目標` = character(), `控制活動` = character(),
    `情境組號` = integer(), `控制現況情境` = character(), `情境現況說明` = character(),
    `控制頻率` = character(), `建議樣本數` = character(),
    `抽樣方法論` = character(),
    `元素` = character(), element_key = character(), `測試步驟序號` = integer(),
    `測試目的` = character(), `測試程序` = character(), `抽樣或範圍` = character(),
    `所需文件_PBC` = character(), `預期結果` = character(), `實際結果` = character(),
    `例外說明` = character(), `步驟結論` = character(),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

is_control_finalized_for_rcm <- function(ctrl) {
  isTRUE(ctrl$rcm_ready$ready) || isTRUE(is_rcm_row_ready(ctrl)$ready)
}

# ---------------------------------------------------------------------------
# CSA 多情境組：同一控制點可因不同「控制現況情境」擁有多組測試步驟
# ---------------------------------------------------------------------------
new_csa_scenario <- function(scenario_name = "預設現況",
                             company_status = "",
                             type = "",
                             inputs = "",
                             review_steps = "",
                             outputs = "",
                             investigation_threshold = "",
                             iuc_or_system = "",
                             related_system = "",
                             frequency = "",
                             nature = "",
                             scenario_id = NULL) {
  sid <- trimws(as.character(scenario_id %||% ""))
  if (!nzchar(sid)) {
    sid <- sprintf("S%s", format(as.numeric(Sys.time()) * 1000, scientific = FALSE))
  }
  list(
    scenario_id = sid,
    scenario_name = {
      nm <- trimws(as.character(scenario_name %||% ""))
      if (nzchar(nm)) nm else "未命名現況情境"
    },
    company_status = as.character(company_status %||% ""),
    type = as.character(type %||% ""),
    inputs = as.character(inputs %||% ""),
    review_steps = as.character(review_steps %||% ""),
    outputs = as.character(outputs %||% ""),
    investigation_threshold = as.character(investigation_threshold %||% ""),
    iuc_or_system = as.character(iuc_or_system %||% ""),
    related_system = as.character(related_system %||% ""),
    frequency = as.character(frequency %||% ""),
    nature = as.character(nature %||% "")
  )
}

normalize_csa_scenario <- function(sc, seq_no = 1L, ctrl = list()) {
  if (!is.list(sc)) sc <- list()
  out <- new_csa_scenario(
    scenario_name = sc$scenario_name %||% sc$name %||% sprintf("現況情境 %d", as.integer(seq_no)),
    company_status = sc$company_status %||% sc$status %||% "",
    type = sc$type %||% "",
    inputs = sc$inputs %||% "",
    review_steps = sc$review_steps %||% sc$steps %||% "",
    outputs = sc$outputs %||% "",
    investigation_threshold = sc$investigation_threshold %||% "",
    iuc_or_system = sc$iuc_or_system %||% "",
    related_system = sc$related_system %||% "",
    frequency = sc$frequency %||% "",
    nature = sc$nature %||% "",
    scenario_id = sc$scenario_id %||% sprintf("S%d", as.integer(seq_no))
  )
  out$scenario_seq <- as.integer(seq_no)
  # Inherit blanks from control design so first scenario stays usable
  if (!nzchar(trimws(out$company_status)) && nzchar(trimws(ctrl$company_status %||% ""))) {
    out$company_status <- ctrl$company_status
  }
  if (!nzchar(trimws(out$type)) && nzchar(trimws(ctrl$type %||% ""))) out$type <- ctrl$type
  if (!nzchar(trimws(out$inputs)) && nzchar(trimws(ctrl$inputs %||% ""))) out$inputs <- ctrl$inputs
  if (!nzchar(trimws(out$review_steps)) && nzchar(trimws(ctrl$review_steps %||% ""))) {
    out$review_steps <- ctrl$review_steps
  }
  if (!nzchar(trimws(out$outputs)) && nzchar(trimws(ctrl$outputs %||% ""))) {
    out$outputs <- ctrl$outputs %||% ctrl$related_document %||% ""
  }
  if (!nzchar(trimws(out$investigation_threshold)) &&
      nzchar(trimws(ctrl$investigation_threshold %||% ""))) {
    out$investigation_threshold <- ctrl$investigation_threshold
  }
  if (!nzchar(trimws(out$iuc_or_system))) {
    out$iuc_or_system <- ctrl_iuc_value(ctrl)
  }
  if (!nzchar(trimws(out$related_system))) {
    out$related_system <- ctrl_related_system_value(ctrl)
  }
  if (!nzchar(trimws(out$frequency))) out$frequency <- ctrl$frequency %||% ""
  if (!nzchar(trimws(out$nature))) out$nature <- ctrl$nature %||% ""
  out
}

synthetic_default_csa_scenario <- function(ctrl) {
  normalize_csa_scenario(
    list(
      scenario_id = "S1",
      scenario_name = "預設現況",
      company_status = ctrl$company_status %||% "",
      type = ctrl$type %||% "",
      inputs = ctrl$inputs %||% "",
      review_steps = ctrl$review_steps %||% "",
      outputs = ctrl$outputs %||% ctrl$related_document %||% "",
      investigation_threshold = ctrl$investigation_threshold %||% "",
      iuc_or_system = ctrl_iuc_value(ctrl),
      related_system = ctrl_related_system_value(ctrl),
      frequency = ctrl$frequency %||% "",
      nature = ctrl$nature %||% ""
    ),
    seq_no = 1L,
    ctrl = ctrl
  )
}

#' 取出控制點之 CSA 情境組（無自訂時回傳一組預設現況）
control_csa_scenarios <- function(ctrl) {
  sc <- ctrl$csa_scenarios
  if (is.list(sc) && length(sc) > 0) {
    return(lapply(seq_along(sc), function(i) normalize_csa_scenario(sc[[i]], i, ctrl)))
  }
  list(synthetic_default_csa_scenario(ctrl))
}

overlay_csa_scenario <- function(ctrl, sc) {
  sc <- normalize_csa_scenario(sc, sc$scenario_seq %||% 1L, ctrl)
  out <- ctrl
  out$company_status <- sc$company_status
  out$type <- sc$type
  out$inputs <- sc$inputs
  out$review_steps <- sc$review_steps
  out$outputs <- sc$outputs
  out$investigation_threshold <- sc$investigation_threshold
  if (nzchar(trimws(sc$iuc_or_system))) {
    out$iuc <- sc$iuc_or_system
    out$iuc_or_system <- sc$iuc_or_system
  }
  if (nzchar(trimws(sc$related_system))) {
    out$related_system <- sc$related_system
  }
  if (nzchar(trimws(sc$frequency))) out$frequency <- sc$frequency
  if (nzchar(trimws(sc$nature))) out$nature <- sc$nature
  out$._csa_scenario_id <- sc$scenario_id
  out$._csa_scenario_name <- sc$scenario_name
  out$._csa_scenario_seq <- as.integer(sc$scenario_seq %||% 1L)
  out$._csa_scenario_status <- sc$company_status
  out
}

upsert_control_csa_scenario <- function(ctrl, scenario) {
  sc <- normalize_csa_scenario(scenario, 1L, ctrl)
  existing <- ctrl$csa_scenarios
  if (!is.list(existing)) existing <- list()
  # If only synthetic existed (no stored list), start fresh with this save
  ids <- vapply(existing, function(x) as.character(x$scenario_id %||% ""), character(1))
  hit <- which(ids == sc$scenario_id)
  if (length(hit)) {
    existing[[hit[[1]]]] <- sc[c(
      "scenario_id", "scenario_name", "company_status", "type", "inputs",
      "review_steps", "outputs", "investigation_threshold", "iuc_or_system",
      "related_system", "frequency", "nature"
    )]
  } else {
    existing[[length(existing) + 1L]] <- sc[c(
      "scenario_id", "scenario_name", "company_status", "type", "inputs",
      "review_steps", "outputs", "investigation_threshold", "iuc_or_system",
      "related_system", "frequency", "nature"
    )]
  }
  ctrl$csa_scenarios <- existing
  ctrl
}

remove_control_csa_scenario <- function(ctrl, scenario_id) {
  existing <- ctrl$csa_scenarios
  if (!is.list(existing) || !length(existing)) return(ctrl)
  sid <- trimws(as.character(scenario_id %||% ""))
  ctrl$csa_scenarios <- Filter(
    function(x) !identical(trimws(as.character(x$scenario_id %||% "")), sid),
    existing
  )
  ctrl
}

csa_scenario_choices <- function(ctrl) {
  scs <- control_csa_scenarios(ctrl)
  stats::setNames(
    vapply(scs, function(x) x$scenario_id, ""),
    vapply(scs, function(x) {
      sprintf("%d｜%s", as.integer(x$scenario_seq %||% 1L), x$scenario_name)
    }, "")
  )
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
  # 三擇一：僅在剛好一項有值時推導；複數視為未定（勿逕自改為營運面以掩蓋錯誤）
  if (sum(filled) == 1) return(names(filled)[filled][1])
  ""
}

# Map 風險類別 ↔ 三大屬性 kind（financial / operations / compliance）
risk_attr_kind_from_category <- function(cat) {
  x <- trimws(as.character(cat %||% ""))
  if (!nzchar(x)) return("")
  if (grepl("報導|財務", x)) return("financial")
  if (grepl("遵循|法令|合規", x)) return("compliance")
  if (grepl("營運|作業", x)) return("operations")
  ""
}

risk_attr_kind_label <- function(kind) {
  switch(as.character(kind %||% ""),
         financial = "財務報導",
         operations = "營運",
         compliance = "法令遵循",
         "")
}

risk_attr_kind_from_ctrl <- function(ctrl) {
  ctrl <- as.list(ctrl)
  k <- risk_attr_kind_from_category(ctrl$risk_category)
  if (nzchar(k)) return(k)
  fr <- nzchar(strip_attr_label(ctrl$risk_attr_financial))
  op <- nzchar(strip_attr_label(ctrl$risk_attr_operations))
  cp <- nzchar(strip_attr_label(ctrl$risk_attr_compliance))
  if (fr + op + cp == 1L) {
    if (fr) return("financial")
    if (op) return("operations")
    return("compliance")
  }
  # UI 預設：營運（仍須使用者確認；定稿時會清空非選項）
  "operations"
}

risk_attr_detail_from_ctrl <- function(ctrl) {
  ctrl <- as.list(ctrl)
  kind <- risk_attr_kind_from_ctrl(ctrl)
  pick <- function(x) strip_attr_label(x)
  switch(kind,
         financial = pick(ctrl$risk_attr_financial),
         operations = pick(ctrl$risk_attr_operations),
         compliance = pick(ctrl$risk_attr_compliance),
         "")
}

# 同一控制點僅保留一種風險屬性細節；複數屬性應另設控制點
enforce_single_risk_attr <- function(ctrl, kind = NULL, detail = NULL) {
  ctrl <- as.list(ctrl)
  if (is.null(kind) || !nzchar(trimws(as.character(kind)))) {
    kind <- risk_attr_kind_from_ctrl(ctrl)
  }
  kind <- as.character(kind)
  if (!kind %in% c("financial", "operations", "compliance")) {
    kind <- "operations"
  }
  strip <- function(x) strip_attr_label(x)
  if (is.null(detail)) {
    detail <- switch(kind,
                     financial = strip(ctrl$risk_attr_financial),
                     operations = strip(ctrl$risk_attr_operations),
                     compliance = strip(ctrl$risk_attr_compliance),
                     "")
    if (!nzchar(detail)) {
      for (f in c(ctrl$risk_attr_financial, ctrl$risk_attr_operations, ctrl$risk_attr_compliance)) {
        d <- strip(f)
        if (nzchar(d)) {
          detail <- d
          break
        }
      }
    }
  } else {
    detail <- strip(detail)
  }
  lab <- risk_attr_kind_label(kind)
  val <- if (nzchar(detail)) sprintf("[%s] %s", lab, detail) else ""
  ctrl$risk_attr_financial <- if (identical(kind, "financial")) val else ""
  ctrl$risk_attr_operations <- if (identical(kind, "operations")) val else ""
  ctrl$risk_attr_compliance <- if (identical(kind, "compliance")) val else ""
  cats <- c(financial = "報導面", operations = "營運面", compliance = "遵循面")
  ctrl$risk_category <- cats[[kind]]
  ctrl
}

count_filled_risk_attrs <- function(ctrl) {
  ctrl <- as.list(ctrl)
  sum(c(
    nzchar(strip_attr_label(ctrl$risk_attr_financial)),
    nzchar(strip_attr_label(ctrl$risk_attr_operations)),
    nzchar(strip_attr_label(ctrl$risk_attr_compliance))
  ))
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
    issues <- c(issues, "控制類型應為：人工／自動（不可混用）")
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
  design_gap <- ctrl$design_gap_note %||% ""
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
    `風險因素` = risk_factor_tag(ctrl$risk_factor %||% ctrl$risk_name %||% ""),
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
    `控制頻率` = resolve_control_frequency(
      tchk$control_type,
      ctrl$frequency
    ),
    `控制現況描述` = status_desc,
    `控制設計差異說明` = design_gap,
    `相關系統` = ctrl$related_system %||% "",
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

# ---- Interview：循環／子作業下預期風險與預期目標／活動（人事時地物鏈）----
interview_answer_scaffold <- function(modules = DEFAULT_INTERVIEW_5W1H) {
  mods <- intersect(as.character(modules %||% character()), names(INTERVIEW_5W1H_MODULES))
  if (!length(mods)) return(INTERVIEW_ANSWER_SCAFFOLD)
  # 全選時使用標準鏈（頻率→誰取得IUC→做什麼→下一步）
  if (setequal(mods, DEFAULT_INTERVIEW_5W1H)) return(INTERVIEW_ANSWER_SCAFFOLD)
  bits <- unname(INTERVIEW_5W1H_MODULES[mods])
  # 若同時勾 who + what，合併為「誰取得什麼文件或資訊(IUC)」
  if (all(c("who", "what") %in% mods)) {
    bits <- character()
    for (m in mods) {
      if (identical(m, "who")) {
        bits <- c(bits, "誰取得什麼文件或資訊(IUC)")
      } else if (identical(m, "what")) {
        next
      } else {
        bits <- c(bits, INTERVIEW_5W1H_MODULES[[m]])
      }
    }
  }
  paste0("請以人事時地物回答：", paste(bits, collapse = " → "))
}

# 題幹尾綴：強制每題答案含人事時地物鏈
interview_people_place_suffix <- function(modules = DEFAULT_INTERVIEW_5W1H) {
  paste0("（答案必含：", interview_answer_scaffold(modules), "）")
}

# 依勾選模組順序拼湊探針題（可串 PBC 於 what）
interview_5w1h_probe_bank <- function(ctrl, modules = DEFAULT_INTERVIEW_5W1H,
                                      pbc_hint = "") {
  mods <- intersect(as.character(modules %||% character()), names(INTERVIEW_5W1H_MODULES))
  if (!length(mods)) return(list())
  act <- nzchar_or(ctrl$control_activity, "該控制活動")
  obj <- nzchar_or(ctrl$control_objective, "該控制目標")
  risk_label <- nzchar_or(ctrl$risk_factor %||% ctrl$risk_name, "該風險")
  iuc <- nzchar_or(ctrl_iuc_value(ctrl), "（待補 IUC）")
  freq <- nzchar_or(resolve_control_frequency(ctrl$nature, ctrl$frequency), "所訂頻率")
  owner <- nzchar_or(ctrl$responsible_unit, "負責單位")
  outp <- nzchar_or(ctrl$related_document %||% ctrl$outputs, "簽核／軌跡")
  pbc_txt <- trimws(as.character(pbc_hint %||% ""))
  if (!nzchar(pbc_txt)) pbc_txt <- "（請自 PBC 資料庫選取）"
  probes <- list(
    when = list(
      element = unname(INTERVIEW_5W1H_PROBE_LABELS[["when"]]),
      question = sprintf(
        "【以何頻率】就預期活動「%s」（對應風險「%s」）：實際以何頻率／何時執行？設計頻率「%s」是否一致？",
        act, risk_label, freq
      ),
      evidence = "簽核紀錄／系統 log／排程"
    ),
    who = list(
      element = unname(INTERVIEW_5W1H_PROBE_LABELS[["who"]]),
      question = sprintf(
        "【誰】「%s」由「%s」的誰執行、誰覆核？有無代理／交接？",
        act, owner
      ),
      evidence = "權責表／簽核軌跡"
    ),
    what = list(
      element = unname(INTERVIEW_5W1H_PROBE_LABELS[["what"]]),
      question = sprintf(
        "【取得什麼文件或資訊(IUC)】執行時取得哪些文件／系統資訊？設計 IUC「%s」。請對照 PBC 資料庫：%s",
        iuc, pbc_txt
      ),
      evidence = paste(iuc, pbc_txt, sep = "；")
    ),
    how = list(
      element = unname(INTERVIEW_5W1H_PROBE_LABELS[["how"]]),
      question = sprintf(
        "【做什麼（具體控制行為）】為達成目標「%s」，實際做哪些具體步驟／判斷／比對？",
        obj
      ),
      evidence = "操作示範／逐步軌跡"
    ),
    next_step = list(
      element = unname(INTERVIEW_5W1H_PROBE_LABELS[["next_step"]]),
      question = sprintf(
        "【才會進行什麼下一步】完成「%s」後產出／交付給誰、例外如何關閉？預期產出「%s」。",
        act, outp
      ),
      evidence = outp
    )
  )
  probes[mods]
}

suggest_interview_pbc <- function(ctrl, pbc_reg = NULL, pbc_ids = NULL) {
  linked <- ""
  if (!is.null(pbc_reg) && is.data.frame(pbc_reg) && length(pbc_ids)) {
    linked <- tryCatch(format_pbc_for_inputs(pbc_reg, pbc_ids), error = function(e) "")
  }
  iuc <- trimws(as.character(ctrl_iuc_value(ctrl)))
  inputs <- trimws(as.character(ctrl$inputs %||% ""))
  outp <- trimws(as.character(ctrl$related_document %||% ctrl$outputs %||% ""))
  base <- unique(c(iuc, inputs, outp))
  base <- base[nzchar(base)]
  if (!length(base) && !nzchar(trimws(linked))) return("（待對照 PBC 資料庫）")
  label <- if (length(base)) paste(base, collapse = "；") else ""
  hits <- character()
  if (!is.null(pbc_reg) && is.data.frame(pbc_reg) && nrow(pbc_reg) && length(base)) {
    for (nm in base) {
      rows <- pbc_reg[
        grepl(nm, as.character(pbc_reg$reviewed_name %||% ""), fixed = TRUE) |
          grepl(nm, as.character(pbc_reg$client_pbc_name %||% ""), fixed = TRUE) |
          grepl(nm, as.character(pbc_reg$iuc_or_system %||% ""), fixed = TRUE),
        ,
        drop = FALSE
      ]
      if (nrow(rows)) {
        hits <- c(hits, sprintf(
          "%s→%s",
          as.character(rows$client_pbc_name[[1]] %||% ""),
          as.character(rows$reviewed_name[[1]] %||% nm)
        ))
      }
    }
  }
  parts <- unique(c(label, hits, if (nzchar(trimws(linked))) linked else character()))
  parts <- parts[nzchar(parts)]
  if (!length(parts)) "（待對照 PBC 資料庫）" else paste(parts, collapse = "｜")
}

# 依循環／子作業篩選（深入且快速鎖定範圍）
filter_controls_by_cycle_sub <- function(controls, cycle = "", sub_key = "") {
  if (!length(controls)) return(list())
  cy <- trimws(as.character(cycle %||% ""))
  sk <- trimws(as.character(sub_key %||% ""))
  out <- controls
  if (nzchar(cy)) {
    out <- Filter(function(c) identical(trimws(as.character(c$cycle %||% "")), cy), out)
  }
  if (nzchar(sk) && !identical(sk, "__all__")) {
    out <- Filter(function(c) {
      identical(sub_process_key(c$sub_process_id %||% "", c$sub_process %||% ""), sk)
    }, out)
  }
  out
}

# 範本庫 → 訪談用控制點（預期風險／目標／活動）
library_items_as_interview_controls <- function(library) {
  if (!length(library)) return(list())
  lapply(library, function(item) {
    ctrl <- item$control %||% item
    if (is.null(ctrl$control_id) || !nzchar(as.character(ctrl$control_id %||% ""))) {
      ctrl$control_id <- item$library_id %||% ctrl$library_id %||% "LIB"
    }
    if (is.null(ctrl$cycle) || !nzchar(as.character(ctrl$cycle %||% ""))) {
      ctrl$cycle <- item$cycle %||% ""
    }
    ctrl$rcm_ready <- list(ready = TRUE)
    ctrl
  })
}

interview_element_bank <- function(ctrl, modules = DEFAULT_INTERVIEW_5W1H) {
  risk_label <- nzchar_or(ctrl$risk_factor %||% ctrl$risk_name, "該風險")
  risk_desc <- nzchar_or(ctrl$risk_description, "（設計尚未填風險描述）")
  risk_cat <- nzchar_or(normalize_risk_category(ctrl), "（未填類別）")
  obj <- nzchar_or(ctrl$control_objective, "（待補控制目標）")
  act <- nzchar_or(ctrl$control_activity, "（待補控制活動）")
  ct <- normalize_control_type_manual_auto(ctrl$nature %||% ctrl$control_type)
  at <- normalize_control_activity_type_pd(ctrl$approach %||% ctrl$control_activity_type)
  iuc <- nzchar_or(ctrl_iuc_value(ctrl), "（待補 IUC）")
  status <- nzchar_or(ctrl$company_status, "（尚未書寫現況）")
  gap <- nzchar_or(ctrl$design_gap_note, "（無設計差異說明）")
  outp <- nzchar_or(ctrl$related_document %||% ctrl$outputs, "簽核／軌跡文件")
  freq <- nzchar_or(resolve_control_frequency(ctrl$nature, ctrl$frequency), "所訂頻率")
  owner <- nzchar_or(ctrl$responsible_unit, "負責單位")
  cycle_nm <- nzchar_or(ctrl$cycle, "本循環")
  sub_nm <- nzchar_or(ctrl$sub_process, "（子作業）")
  suffix <- interview_people_place_suffix(modules)
  list(
    risk = list(
      element = unname(INTERVIEW_ELEMENTS[["risk"]]),
      question = paste0(sprintf(
        "就「%s／%s」預期風險「%s」（設計：%s）：請深入且快速說明實務上如何發生、如何被偵知／防範。",
        cycle_nm, sub_nm, risk_label, risk_desc
      ), suffix),
      evidence = "流程說明／事件紀錄／前一年度缺失"
    ),
    risk_attributes = list(
      element = unname(INTERVIEW_ELEMENTS[["risk_attributes"]]),
      question = paste0(sprintf(
        "此風險實務上是否屬「%s」？對財務報導／營運／法令遵循的影響各為何？",
        risk_cat
      ), suffix),
      evidence = "風險評估底稿／RCM"
    ),
    control_objective = list(
      element = unname(INTERVIEW_ELEMENTS[["control_objective"]]),
      question = paste0(sprintf(
        "就「%s／%s」預期控制目標「%s」：請深入且快速說明實務上如何衡量／確認已達成（勿只複述活動步驟）。",
        cycle_nm, sub_nm, obj
      ), suffix),
      evidence = "制度／KPI／管理報表"
    ),
    control_activity = list(
      element = unname(INTERVIEW_ELEMENTS[["control_activity"]]),
      question = paste0(sprintf(
        "就「%s／%s」預期控制活動「%s」：請走查實際執行——以何頻率、誰取得什麼文件或資訊(IUC)、做什麼具體控制行為、才會進行什麼下一步。",
        cycle_nm, sub_nm, act
      ), suffix),
      evidence = "現場示範／螢幕錄影／逐步說明"
    ),
    control_types = list(
      element = unname(INTERVIEW_ELEMENTS[["control_types"]]),
      question = paste0(sprintf(
        "實務上是否為「%s」控制、且屬「%s」？與設計不一致時請說明現況。",
        nzchar_or(ct, "（未填）"), nzchar_or(at, "（未填）")
      ), suffix),
      evidence = "系統設定／職責說明"
    ),
    frequency_owner = list(
      element = unname(INTERVIEW_ELEMENTS[["frequency_owner"]]),
      question = paste0(sprintf(
        "實際執行頻率是否為「%s」？由「%s」的誰執行、誰覆核？有無代理／交接？",
        freq, owner
      ), suffix),
      evidence = "權責表／簽核紀錄／系統 log"
    ),
    iuc = list(
      element = unname(INTERVIEW_ELEMENTS[["iuc"]]),
      question = paste0(sprintf(
        "執行時取得哪些文件或資訊（設計 IUC：%s%s）？誰提供、如何確保完整正確？請指出可作為 PBC 的項目。",
        iuc,
        if (nzchar(ctrl_related_system_value(ctrl))) {
          sprintf("；相關系統：%s", ctrl_related_system_value(ctrl))
        } else ""
      ), suffix),
      evidence = paste(iuc, "PBC 命名對照", sep = "；")
    ),
    company_status = list(
      element = unname(INTERVIEW_ELEMENTS[["company_status"]]),
      question = paste0(sprintf(
        "請完整描述目前實際怎麼做（可對照設計現況「%s」），並標出與設計之差異。",
        status
      ), suffix),
      evidence = "訪談紀錄／現場觀察"
    ),
    design_gap = list(
      element = unname(INTERVIEW_ELEMENTS[["design_gap"]]),
      question = paste0(sprintf(
        "設計差異「%s」在實務是否仍存在？改善負責人與時程？",
        gap
      ), suffix),
      evidence = "改善計畫／會議紀錄"
    ),
    nature_approach_type = list(
      element = unname(INTERVIEW_ELEMENTS[["nature_approach_type"]]),
      question = paste0(sprintf(
        "Form 4120SR「%s／%s／%s」與實務是否一致？",
        nzchar_or(ctrl$nature, "—"), nzchar_or(ctrl$approach, "—"), nzchar_or(ctrl$type, "—")
      ), suffix),
      evidence = "控制說明／系統設定"
    ),
    inputs = list(
      element = unname(INTERVIEW_ELEMENTS[["inputs"]]),
      question = paste0(sprintf(
        "控制投入資訊為何（設計：%s）？誰提供、何時取得、如何核對完整？",
        nzchar_or(ctrl$inputs, "待補 Inputs")
      ), suffix),
      evidence = nzchar_or(ctrl$inputs, iuc)
    ),
    steps = list(
      element = unname(INTERVIEW_ELEMENTS[["steps"]]),
      question = paste0(sprintf(
        "請依實際順序說明每一步：誰做、用什麼、留下什麼證據。設計步驟：%s",
        {
          st <- trimws(unlist(strsplit(as.character(ctrl$review_steps %||% ""), "\n")))
          st <- st[nzchar(st)]
          if (!length(st)) "（尚未拆分；請依控制活動說明）"
          else paste(paste0(seq_along(st), ".", st), collapse = "；")
        }
      ), suffix),
      evidence = "逐步操作軌跡／PBC"
    ),
    outputs = list(
      element = unname(INTERVIEW_ELEMENTS[["outputs"]]),
      question = paste0(sprintf(
        "控制產出／佐證文件為何（預期：%s）？誰簽核、留存何處、留存多久？下一步給誰？",
        outp
      ), suffix),
      evidence = outp
    ),
    exception = list(
      element = unname(INTERVIEW_ELEMENTS[["exception"]]),
      question = paste0(sprintf(
        "發現差異時如何辨識、調查與結案%s？請舉一例說明誰處理、用什麼文件、何時關閉。",
        if (!is_blank(ctrl$investigation_threshold)) paste0("（設計門檻：", ctrl$investigation_threshold, "）")
        else ""
      ), suffix),
      evidence = "例外追蹤清單／結案紀錄"
    ),
    assertion_account = list(
      element = unname(INTERVIEW_ELEMENTS[["assertion_account"]]),
      question = paste0(sprintf(
        "此控制涵蓋科目「%s」、聲明「%s」在實務是否完整？有無遺漏路徑？",
        nzchar_or(ctrl$significant_account, "（未填）"),
        nzchar_or(ctrl$assertions, "（未填）")
      ), suffix),
      evidence = "科目映射／前一年度 RCM"
    )
  )
}

empty_interview_df <- function() {
  data.frame(
    `控制編號` = character(), `循環` = character(), `子作業` = character(),
    `風險因素` = character(), `控制目標` = character(), `控制活動` = character(),
    `題號` = integer(), `元素` = character(),
    element_key = character(), `訪談問題` = character(),
    `回答架構_5W1H` = character(),
    `設計摘要` = character(), `預期佐證_PBC` = character(),
    `建議串接PBC` = character(),
    `受訪者回答` = character(), `佐證取得` = character(), `結論` = character(),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

control_to_interview <- function(ctrl, elements = DEFAULT_INTERVIEW_ELEMENTS,
                                 modules = DEFAULT_INTERVIEW_5W1H,
                                 pbc_reg = NULL,
                                 pbc_ids = NULL,
                                 include_module_rows = TRUE) {
  bank <- interview_element_bank(ctrl, modules = modules)
  elements <- intersect(as.character(elements %||% character()), names(bank))
  mods <- intersect(as.character(modules %||% character()), names(INTERVIEW_5W1H_MODULES))
  cid <- derive_control_id(ctrl, 1L)
  scaffold <- interview_answer_scaffold(mods)
  pbc_hint <- suggest_interview_pbc(ctrl, pbc_reg, pbc_ids = pbc_ids)
  meta <- list(
    `控制編號` = cid,
    `循環` = ctrl$cycle %||% "",
    `子作業` = paste(ctrl$sub_process_id %||% "", ctrl$sub_process %||% "", sep = " "),
    `風險因素` = nzchar_or(ctrl$risk_factor %||% ctrl$risk_name, ""),
    `控制目標` = trimws(ctrl$control_objective %||% ""),
    `控制活動` = trimws(ctrl$control_activity %||% "")
  )
  rows <- list()
  for (key in elements) {
    item <- bank[[key]]
    rows[[length(rows) + 1L]] <- data.frame(
      `控制編號` = meta$`控制編號`,
      `循環` = meta$`循環`,
      `子作業` = meta$`子作業`,
      `風險因素` = meta$`風險因素`,
      `控制目標` = meta$`控制目標`,
      `控制活動` = meta$`控制活動`,
      `題號` = length(rows) + 1L,
      `元素` = item$element,
      element_key = key,
      `訪談問題` = item$question,
      `回答架構_5W1H` = scaffold,
      `設計摘要` = {
        switch(key,
          risk = nzchar_or(ctrl$risk_description, ctrl$risk_factor %||% ""),
          control_objective = ctrl$control_objective %||% "",
          control_activity = ctrl$control_activity %||% "",
          company_status = ctrl$company_status %||% "",
          iuc = ctrl_iuc_value(ctrl),
          steps = ctrl$review_steps %||% "",
          nzchar_or(item$evidence, "")
        )
      },
      `預期佐證_PBC` = item$evidence %||% "",
      `建議串接PBC` = pbc_hint,
      `受訪者回答` = "",
      `佐證取得` = "",
      `結論` = "",
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }
  if (isTRUE(include_module_rows) && length(mods)) {
    probes <- interview_5w1h_probe_bank(ctrl, modules = mods, pbc_hint = pbc_hint)
    for (key in names(probes)) {
      item <- probes[[key]]
      rows[[length(rows) + 1L]] <- data.frame(
        `控制編號` = meta$`控制編號`,
        `循環` = meta$`循環`,
        `子作業` = meta$`子作業`,
        `風險因素` = meta$`風險因素`,
        `控制目標` = meta$`控制目標`,
        `控制活動` = meta$`控制活動`,
        `題號` = length(rows) + 1L,
        `元素` = item$element,
        element_key = paste0("5w1h_", key),
        `訪談問題` = paste0(item$question, interview_people_place_suffix(mods)),
        `回答架構_5W1H` = scaffold,
        `設計摘要` = INTERVIEW_5W1H_MODULES[[key]],
        `預期佐證_PBC` = item$evidence %||% "",
        `建議串接PBC` = pbc_hint,
        `受訪者回答` = "",
        `佐證取得` = "",
        `結論` = "",
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) return(empty_interview_df())
  do.call(rbind, rows)
}

# Only finalized RCM rows (設計完成＝RCM一列) feed interview by default
controls_to_interview <- function(controls, elements = DEFAULT_INTERVIEW_ELEMENTS,
                                  finalized_only = TRUE,
                                  modules = DEFAULT_INTERVIEW_5W1H,
                                  pbc_reg = NULL,
                                  pbc_ids = NULL,
                                  include_module_rows = TRUE) {
  if (!length(controls)) return(empty_interview_df())
  if (isTRUE(finalized_only)) {
    controls <- Filter(is_control_finalized_for_rcm, controls)
  }
  if (!length(controls)) return(empty_interview_df())
  do.call(rbind, lapply(controls, control_to_interview,
                        elements = elements, modules = modules,
                        pbc_reg = pbc_reg, pbc_ids = pbc_ids,
                        include_module_rows = include_module_rows))
}

# ---- CSA test-step worksheet (Phase-2: after interview + RCM) ----
# Not only self-check slogans: concrete test procedures, evidence, expected result
# Same control may expand to multiple scenario groups (不同控制現況情境 → 多組測試步驟)
control_to_csa_one <- function(ctrl, elements = DEFAULT_CSA_ELEMENTS,
                               scenario_seq = 1L,
                               scenario_name = "預設現況",
                               scenario_status = "") {
  cid <- derive_control_id(ctrl, 1L)
  obj <- nzchar_or(ctrl$control_objective, "（待補控制目標）")
  act <- nzchar_or(ctrl$control_activity, "（待補控制活動）")
  iuc <- nzchar_or(ctrl_iuc_value(ctrl), "（待補 IUC）")
  outp <- nzchar_or(ctrl$related_document %||% ctrl$outputs, "執行軌跡／簽核")
  steps <- trimws(unlist(strsplit(as.character(ctrl$review_steps %||% ""), "\n")))
  steps <- steps[nzchar(steps)]
  if (!length(steps)) {
    steps <- act
  }
  plan <- control_test_sample_plan(ctrl)
  scen_nm <- nzchar_or(scenario_name, "預設現況")
  scen_st <- as.character(scenario_status %||% "")
  seq_no <- as.integer(scenario_seq %||% 1L)

  rows <- list()
  add_row <- function(element_key, element, purpose, procedure, evidence, expected,
                      sample = NULL) {
    if (is.null(sample)) sample <- plan$scope_text
    rows[[length(rows) + 1]] <<- data.frame(
      `控制編號` = cid,
      `循環` = ctrl$cycle %||% "",
      `子作業` = paste(ctrl$sub_process_id %||% "", ctrl$sub_process %||% "", sep = " "),
      `控制目標` = obj,
      `控制活動` = act,
      `情境組號` = seq_no,
      `控制現況情境` = scen_nm,
      `情境現況說明` = scen_st,
      `控制頻率` = plan$frequency,
      `建議樣本數` = plan$sample_size_label,
      `抽樣方法論` = plan$methodology,
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
    return(empty_csa_frame())
  }

  for (key in elements) {
    if (identical(key, "control_objective")) {
      add_row(key, "控制目標",
              "確認控制目標與風險／聲明對應且可衡量（對照 RCM 列）",
              sprintf("訪談／檢視制度，確認目標「%s」未被改寫成活動步驟，並對應風險「%s」（情境：%s）",
                      obj, nzchar_or(ctrl$risk_factor %||% ctrl$risk_name, "—"), scen_nm),
              "風險矩陣／制度文件／RCM",
              "目標清楚、可對應風險與聲明，且與活動文字不同",
              sample = "設計有效性／文件檢視（不依頻率抽樣）")
    } else if (identical(key, "control_activity")) {
      add_row(key, "控制活動",
              sprintf("驗證控制活動於「%s」情境下實際執行方式與 RCM 設計一致", scen_nm),
              sprintf("就現況情境「%s」取得執行軌跡，觀察或重行執行活動「%s」之關鍵動作",
                      scen_nm, act),
              paste(iuc, outp, sep = "；"),
              "活動依設計執行，執行人／覆核人角色清楚")
    } else if (identical(key, "steps")) {
      for (s in steps) {
        add_row(key, "Steps（執行步驟）",
                sprintf("測試「%s」情境下單一執行步驟之有效性", scen_nm),
                sprintf("【%s】依步驟執行並留存證據：%s", scen_nm, s),
                paste(iuc, outp, sep = "；"),
                "該步驟有完整執行軌跡且無未結例外")
      }
    } else if (identical(key, "iuc")) {
      add_row(key, "IUC",
              sprintf("確認「%s」情境依賴之 IUC／PBC 完整正確", scen_nm),
              sprintf("取得「%s」，核對來源、參數、邏輯或產生流程；比對客戶原名與檢視後命名", iuc),
              iuc,
              "IUC 完整正確，足以支撐控制結論",
              sample = "依 IUC 依賴範圍；與控制抽樣樣本勾稽")
    } else if (identical(key, "outputs")) {
      add_row(key, "Outputs／控制佐證文件",
              sprintf("確認「%s」情境產出證據足以證明控制已發生", scen_nm),
              sprintf("抽查產出「%s」之完整性、簽核及時性與內容妥適性", outp),
              outp,
              "產出齊備、簽核適當、可追溯至母體")
    } else if (identical(key, "exception")) {
      add_row(key, "例外／調查門檻",
              sprintf("確認「%s」情境之例外辨識與追蹤有效", scen_nm),
              sprintf("依門檻「%s」選取例外案件，追蹤至結案", nzchar_or(ctrl$investigation_threshold, "（未訂）")),
              paste(outp, "例外追蹤清單", sep = "；"),
              "例外均被辨識且追蹤結案，門檻合理",
              sample = "期間內例外案件全數或重大項目")
    } else if (identical(key, "frequency_owner")) {
      add_row(key, "頻率／負責單位",
              sprintf("確認「%s」情境之執行頻率與權責符合設計", scen_nm),
              sprintf("檢查「%s」是否由「%s」依設計頻率執行", act, nzchar_or(ctrl$responsible_unit, "負責單位")),
              "權責表／出勤或系統 log／簽核紀錄",
              sprintf("頻率為「%s」且執行者具權限；建議樣本數 %s",
                      plan$frequency, plan$sample_size_label))
    } else if (identical(key, "risk")) {
      add_row(key, "循環／風險",
              "確認風險仍適用",
              sprintf("與管理階層確認「%s」風險情境與現行流程（測試情境：%s）",
                      nzchar_or(ctrl$risk_factor %||% ctrl$risk_name, "該風險"), scen_nm),
              "流程說明／系統架構／前一年度缺失",
              "風險描述與現況一致",
              sample = "設計有效性／詢問與觀察（不依頻率抽樣）")
    } else if (identical(key, "risk_attributes")) {
      add_row(key, "風險三大屬性／類別",
              "確認風險類別與三大屬性評估仍妥適",
              sprintf("覆核風險類別「%s」及財務報導／營運／法令遵循屬性是否需更新",
                      nzchar_or(normalize_risk_category(ctrl), "—")),
              "風險評估底稿／RCM",
              "類別與屬性完整且與控制對應",
              sample = "設計有效性／文件檢視（不依頻率抽樣）")
    } else if (identical(key, "inputs")) {
      add_row(key, "Inputs（投入）",
              sprintf("確認「%s」情境投入資訊來源可靠", scen_nm),
              sprintf("追蹤投入「%s」之取得與完整性", nzchar_or(ctrl$inputs, "—")),
              nzchar_or(ctrl$inputs, iuc),
              "投入完整且與母體一致")
    } else if (identical(key, "nature_approach_type") || identical(key, "control_types")) {
      add_row(key, DESIGN_ELEMENTS[[key]] %||% "控制類型",
              "確認控制屬性分類正確（影響測試策略）",
              sprintf("評估「%s」情境實務是否為控制類型「%s」／活動類型「%s」",
                      scen_nm,
                      nzchar_or(normalize_control_type_manual_auto(ctrl$nature), "—"),
                      nzchar_or(normalize_control_activity_type_pd(ctrl$approach), "—")),
              "控制說明／系統設定截圖／RCM",
              "分類正確，測試性質與範圍與之匹配",
              sample = "設計有效性／分類覆核（不依頻率抽樣）")
    } else if (identical(key, "assertion_account")) {
      add_row(key, "科目／聲明",
              "確認科目與聲明涵蓋完整",
              sprintf("比對 RCM 科目「%s」與聲明「%s」是否遺漏",
                      nzchar_or(ctrl$significant_account, "—"),
                      nzchar_or(ctrl$assertions, "—")),
              "財務報表科目映射／前一年度 RCM",
              "科目與聲明無遺漏",
              sample = "設計有效性／對照表檢視（不依頻率抽樣）")
    } else if (identical(key, "company_status")) {
      add_row(key, "控制現況描述",
              sprintf("確認情境「%s」現況與設計一致或差異已記錄", scen_nm),
              sprintf("比對現況描述與實地觀察：%s", nzchar_or(scen_st, nzchar_or(ctrl$company_status, "（未填）"))),
              "訪談紀錄／現場觀察",
              "現況可驗證；差異已於 RCM 設計差異欄揭露",
              sample = "詢問與觀察（不依頻率抽樣）")
    } else if (identical(key, "design_gap")) {
      add_row(key, "控制設計差異",
              "確認設計差異改善追蹤",
              sprintf("追蹤差異「%s」之改善狀態（情境：%s）",
                      nzchar_or(ctrl$design_gap_note, "（無）"), scen_nm),
              "改善計畫",
              "差異有負責人與時程，或已關閉",
              sample = "差異項目追蹤（不依頻率抽樣）")
    }
  }
  if (!length(rows)) return(empty_csa_frame())
  do.call(rbind, rows)
}

control_to_csa <- function(ctrl, elements = DEFAULT_CSA_ELEMENTS) {
  if (!length(ctrl) || (!is.list(ctrl) && !is.environment(ctrl))) {
    return(empty_csa_frame())
  }
  if (!length(names(ctrl)) && !length(ctrl)) return(empty_csa_frame())

  scs <- control_csa_scenarios(ctrl)
  parts <- lapply(scs, function(sc) {
    overlaid <- overlay_csa_scenario(ctrl, sc)
    control_to_csa_one(
      overlaid,
      elements = elements,
      scenario_seq = sc$scenario_seq %||% 1L,
      scenario_name = sc$scenario_name,
      scenario_status = sc$company_status
    )
  })
  parts <- Filter(function(df) nrow(df) > 0, parts)
  if (!length(parts)) return(empty_csa_frame())
  do.call(rbind, parts)
}

controls_to_csa <- function(controls, elements = DEFAULT_CSA_ELEMENTS,
                            finalized_only = TRUE) {
  if (!length(controls)) return(empty_csa_frame())
  if (isTRUE(finalized_only)) {
    controls <- Filter(is_control_finalized_for_rcm, controls)
  }
  if (!length(controls)) return(empty_csa_frame())
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
  iuc_or_system = "IUC（控制執行取得之文件／資訊）"
)

DESIGN_ACCORDION_SECTIONS <- c("基礎設定", "風險辨識", "控制設計")

empty_missing_by_group <- function() {
  stats::setNames(
    rep(list(character()), length(DESIGN_ACCORDION_SECTIONS)),
    DESIGN_ACCORDION_SECTIONS
  )
}

design_field_section <- function(field_key) {
  if (field_key %in% c("cycle", "sub_process_id", "sub_process")) return("基礎設定")
  if (field_key %in% c("risk_factor", "risk_description", "risk_category")) return("風險辨識")
  "控制設計"
}

format_design_required_by_accordion <- function(missing_by_group = NULL, missing_fallback = character()) {
  groups <- missing_by_group
  if (is.null(groups) || !length(unlist(groups))) {
    groups <- group_design_required_missing(missing_fallback)
  }
  lines <- character()
  for (sec in DESIGN_ACCORDION_SECTIONS) {
    items <- unique(groups[[sec]] %||% character())
    items <- items[nzchar(items)]
    if (length(items)) {
      lines <- c(lines, sprintf("%s：%s", sec, paste(items, collapse = "、")))
    }
  }
  if (!length(lines) && length(missing_fallback)) {
    return(paste0("必填未齊：", paste(unique(missing_fallback), collapse = "、")))
  }
  paste(lines, collapse = "｜")
}

group_design_required_missing <- function(missing_labels) {
  groups <- empty_missing_by_group()
  for (lab in unique(missing_labels)) {
    if (!nzchar(lab)) next
    sec <- if (lab %in% c("循環名稱", "子作業編號", "子作業名稱")) {
      "基礎設定"
    } else if (grepl("^風險|^會計科目|^相關法令", lab)) {
      "風險辨識"
    } else {
      "控制設計"
    }
    groups[[sec]] <- c(groups[[sec]], lab)
  }
  lapply(groups, unique)
}

DESIGN_OPTIONAL_FIELDS <- c(
  significant_account = "會計科目（僅報導面必填；常見科目複選／全部適用；其他類別不可填）",
  related_law = "相關法令（僅遵循面必填；其他類別不可填）",
  assertions = "聲明（報導面八種／營運面三種可複選；遵循面不可選）",
  related_policy = "相關政策或程序",
  related_system = "相關系統（IT／應用系統；自動控制必填）",
  related_document_pbc = paste0(
    CONTROL_EVIDENCE_DOCUMENT_LABEL, "（可多選；自 PBC 資料庫選取或手動輸入；自動控制／遵循面不可填）"
  )
)

is_automatic_control <- function(nature) {
  identical(normalize_control_type_manual_auto(nature), "自動")
}

# 相關系統：自動控制必填；人工控制選填
related_system_mode_for_ctrl <- function(ctrl) {
  ctrl <- as.list(ctrl)
  nature <- normalize_control_type_manual_auto(ctrl$nature %||% ctrl$control_type)
  if (is_automatic_control(nature)) return("required")
  if (identical(nature, "人工")) return("optional")
  "pending"
}

# 控制佐證文件：人工＋非法遵面必填；自動或遵循面鎖定
related_document_mode_for_ctrl <- function(ctrl) {
  ctrl <- as.list(ctrl)
  nature <- normalize_control_type_manual_auto(ctrl$nature %||% ctrl$control_type)
  cat <- trimws(as.character(ctrl$risk_category %||% ""))
  if (is_automatic_control(nature) || is_compliance_risk_category(cat)) {
    return("locked")
  }
  if (nzchar(nature) && nzchar(cat)) return("required")
  "pending"
}

related_document_pbc_value <- function(ctrl, registry = NULL) {
  txt <- join_text_list_values(ctrl$related_document %||% "")
  if (nzchar(txt)) return(txt)
  sel <- c(
    parse_pbc_id_values(ctrl$related_document_pbc_ids),
    parse_text_list_values(ctrl$related_document_manual %||% "")
  )
  resolve_multi_pbc_text(sel, registry)
}

is_reporting_risk_category <- function(cat) {
  grepl("^報導", trimws(as.character(cat %||% "")))
}

is_operations_risk_category <- function(cat) {
  grepl("^營運", trimws(as.character(cat %||% "")))
}

is_compliance_risk_category <- function(cat) {
  grepl("^遵循", trimws(as.character(cat %||% "")))
}

# 聲明（Assertions）依風險類別：報導面八種／營運面三種／遵循面不可選
assertion_choices_for_category <- function(cat) {
  if (is_reporting_risk_category(cat)) {
    if (exists("ASSERTION_CHOICES_REPORTING")) ASSERTION_CHOICES_REPORTING else character()
  } else if (is_operations_risk_category(cat)) {
    if (exists("ASSERTION_CHOICES_OPERATIONS")) ASSERTION_CHOICES_OPERATIONS else character()
  } else {
    character()
  }
}

assertion_mode_for_category <- function(cat) {
  if (is_reporting_risk_category(cat)) "reporting"
  else if (is_operations_risk_category(cat)) "operations"
  else if (is_compliance_risk_category(cat)) "locked"
  else "pending"
}

parse_assertion_values <- function(x) {
  if (is.null(x)) return(character())
  if (length(x) > 1L) {
    vals <- trimws(as.character(x))
  } else {
    raw <- trimws(as.character(x %||% ""))
    if (!nzchar(raw)) return(character())
    vals <- trimws(unlist(strsplit(raw, "[;；|/]+")))
  }
  vals[nzchar(vals)]
}

assertions_are_filled <- function(x) {
  length(parse_assertion_values(x)) > 0L
}

# 依類別過濾／清空聲明；回傳分號連接字串
normalize_assertions_for_category <- function(assertions, cat) {
  vals <- parse_assertion_values(assertions)
  allowed <- assertion_choices_for_category(cat)
  if (!length(allowed)) return("")
  keep <- vals[vals %in% allowed]
  paste(unique(keep), collapse = "；")
}

assertions_allowed_ok <- function(assertions, cat) {
  vals <- parse_assertion_values(assertions)
  mode <- assertion_mode_for_category(cat)
  if (identical(mode, "locked") || identical(mode, "pending")) {
    return(!length(vals))
  }
  allowed <- assertion_choices_for_category(cat)
  all(vals %in% allowed)
}

# TRUE if account is considered "filled" for 報導面
account_is_filled <- function(x) {
  vals <- parse_account_values(x)
  length(vals) > 0L
}

parse_account_values <- function(x) {
  if (is.null(x)) return(character(0))
  if (is.character(x) && length(x) > 1L) {
    vals <- trimws(as.character(x))
  } else {
    raw <- trimws(as.character(x %||% ""))
    if (!nzchar(raw)) return(character(0))
    vals <- trimws(unlist(strsplit(raw, "[;；|/、,，]+")))
  }
  vals <- vals[nzchar(vals)]
  vals <- vals[!toupper(vals) %in% c("NA", "—", "-")]
  unique(vals)
}

#' 正規化會計科目選取：含「全部適用」或已勾選全部常見科目 → 存「全部適用」
join_significant_accounts <- function(vals) {
  vals <- parse_account_values(vals)
  if (!length(vals)) return("")
  all_opt <- if (exists("ACCOUNT_ALL_OPTION")) ACCOUNT_ALL_OPTION else "全部適用"
  std <- if (exists("ACCOUNT_CHOICES")) ACCOUNT_CHOICES else character(0)
  if (all_opt %in% vals) return(all_opt)
  extras <- setdiff(vals, std)
  if (length(std) && !length(extras) && all(std %in% vals)) return(all_opt)
  paste(vals, collapse = "；")
}

#' UI／回填用：將存檔值展開為 selectize selected 向量
expand_account_selection <- function(x) {
  vals <- parse_account_values(x)
  if (!length(vals)) return(character(0))
  all_opt <- if (exists("ACCOUNT_ALL_OPTION")) ACCOUNT_ALL_OPTION else "全部適用"
  std <- if (exists("ACCOUNT_CHOICES")) ACCOUNT_CHOICES else character(0)
  if (all_opt %in% vals) {
    return(c(all_opt, std))
  }
  vals
}

account_select_choices <- function() {
  all_opt <- if (exists("ACCOUNT_ALL_OPTION")) ACCOUNT_ALL_OPTION else "全部適用"
  std <- if (exists("ACCOUNT_CHOICES")) ACCOUNT_CHOICES else character(0)
  c(all_opt, std)
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
    return(risk_factor_tag(trimws(as.character(
      ctrl$risk_factor %||% ctrl$risk_name %||% ""
    ))))
  }
  if (identical(field, "iuc_or_system")) {
    return(ctrl_iuc_value(ctrl))
  }
  if (identical(field, "related_system")) {
    return(ctrl_related_system_value(ctrl))
  }
  if (identical(field, "nature")) {
    return(trimws(as.character(
      normalize_control_type_manual_auto(ctrl$nature %||% ctrl$control_type)
    )))
  }
  if (identical(field, "frequency")) {
    return(resolve_control_frequency(
      ctrl$nature %||% ctrl$control_type,
      ctrl$frequency
    ))
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

# Returns ok + missing Chinese labels for 設計必填欄位（含 accordion 分組）
design_required_check <- function(ctrl) {
  missing <- character()
  missing_by_group <- empty_missing_by_group()
  add_miss <- function(label, section) {
    missing <<- c(missing, label)
    missing_by_group[[section]] <<- c(missing_by_group[[section]], label)
  }
  filled <- list()
  for (f in names(DESIGN_REQUIRED_FIELDS)) {
    val <- design_field_value(ctrl, f)
    filled[[f]] <- nzchar(val)
    if (!nzchar(val)) {
      add_miss(DESIGN_REQUIRED_FIELDS[[f]], design_field_section(f))
    }
  }
  # Extra rule: nature must be exactly one 人工/自動
  raw_nature <- trimws(as.character(ctrl$nature %||% ctrl$control_type %||% ""))
  norm_nature <- normalize_control_type_manual_auto(raw_nature)
  if (nzchar(raw_nature) && !nzchar(norm_nature)) {
    add_miss("控制類型須為單一人工或自動（不可混用）", "控制設計")
    filled$nature <- FALSE
  }
  # Extra rule: approach must be exactly one 預防/偵測
  if (isTRUE(filled$approach) && exists("activity_type_ok", mode = "function") &&
      !activity_type_ok(ctrl$approach %||% ctrl$control_activity_type)) {
    add_miss("控制活動類型須為單一預防／偵測（不可混用）", "控制設計")
    filled$approach <- FALSE
  }
  # 會計科目：報導面必填；其他類別不得填入（僅允許空白／NA）
  cat <- design_field_value(ctrl, "risk_category")
  acct <- trimws(as.character(ctrl$significant_account %||% ""))
  if (is_reporting_risk_category(cat)) {
    filled$significant_account <- account_is_filled(acct)
    if (!account_is_filled(acct)) {
      add_miss("會計科目（報導面必填）", "風險辨識")
    }
  } else if (nzchar(cat)) {
    filled$significant_account <- !account_is_filled(acct)
    if (account_is_filled(acct)) {
      add_miss("會計科目僅報導面可填（請清空）", "風險辨識")
    }
  } else {
    filled$significant_account <- TRUE
  }
  # 相關法令：遵循面必填；其他類別不得填入
  law <- trimws(as.character(ctrl$related_law %||% ""))
  if (is_compliance_risk_category(cat)) {
    filled$related_law <- law_is_filled(law)
    if (!law_is_filled(law)) {
      add_miss("相關法令（遵循面必填）", "風險辨識")
    }
  } else if (nzchar(cat)) {
    filled$related_law <- !law_is_filled(law)
    if (law_is_filled(law)) {
      add_miss("相關法令僅遵循面可填（請清空）", "風險辨識")
    }
  } else {
    filled$related_law <- TRUE
  }
  # 聲明（Assertions）：報導面八種／營運面三種可複選；遵循面不可填
  asrt <- parse_assertion_values(ctrl$assertions)
  mode_as <- assertion_mode_for_category(cat)
  if (identical(mode_as, "locked")) {
    filled$assertions <- !length(asrt)
    if (length(asrt)) {
      add_miss("聲明僅報導面／營運面可填（遵循面請清空）", "控制設計")
    }
  } else if (identical(mode_as, "reporting") || identical(mode_as, "operations")) {
    ok_as <- assertions_allowed_ok(asrt, cat)
    filled$assertions <- ok_as
    if (!ok_as) {
      add_miss(
        if (identical(mode_as, "operations")) {
          "聲明超出營運面可選（完整性／正確性／即時性）"
        } else {
          "聲明超出報導面可選（Thomson Reuters／AICPA 八種）"
        },
        "控制設計"
      )
    }
  } else {
    filled$assertions <- !length(asrt)
  }
  # 控制佐證文件：人工＋非法遵面必填（PBC 選取或手動輸入，可多選）；自動／遵循面不可填
  doc_mode <- related_document_mode_for_ctrl(ctrl)
  doc_sel <- c(
    parse_pbc_id_values(ctrl$related_document_pbc_ids),
    parse_text_list_values(ctrl$related_document_manual %||% "")
  )
  doc_txt <- related_document_pbc_value(ctrl)
  doc_label <- CONTROL_EVIDENCE_DOCUMENT_LABEL
  if (identical(doc_mode, "required")) {
    filled$related_document_pbc <- multi_pbc_is_filled(doc_sel) || nzchar(doc_txt)
    if (!filled$related_document_pbc) {
      add_miss(paste0(doc_label, "（可多選；自 PBC 選取或手動輸入）"), "控制設計")
    }
  } else if (identical(doc_mode, "locked")) {
    filled$related_document_pbc <- !multi_pbc_is_filled(doc_sel) && !nzchar(doc_txt)
    if (multi_pbc_is_filled(doc_sel) || nzchar(doc_txt)) {
      add_miss(paste0(doc_label, "不可設定（自動控制或遵循面風險）"), "控制設計")
    }
  } else {
    filled$related_document_pbc <- TRUE
  }
  # 相關系統：自動控制必填；人工控制選填
  sys_mode <- related_system_mode_for_ctrl(ctrl)
  sys <- ctrl_related_system_value(ctrl)
  if (identical(sys_mode, "required")) {
    filled$related_system <- nzchar(sys)
    if (!nzchar(sys)) {
      add_miss("相關系統（自動控制必填）", "控制設計")
    }
  } else {
    filled$related_system <- TRUE
  }
  list(
    ok = !length(missing),
    missing = unique(missing),
    missing_by_group = lapply(missing_by_group, unique),
    filled = filled,
    required = DESIGN_REQUIRED_FIELDS,
    optional = DESIGN_OPTIONAL_FIELDS,
    account_mode = if (is_reporting_risk_category(cat)) "required" else if (nzchar(cat)) "locked" else "pending",
    law_mode = if (is_compliance_risk_category(cat)) "required" else if (nzchar(cat)) "locked" else "pending",
    assertion_mode = mode_as,
    document_mode = doc_mode,
    related_system_mode = sys_mode
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
  n_attr <- count_filled_risk_attrs(ctrl)
  if (n_attr > 1L) {
    add("控制缺失", "高", "風險屬性細節不可複選（三擇一）",
        "同一控制點僅保留一種屬性；若需涵蓋其他屬性請另設新控制點")
  } else if (n_attr == 0L && is_blank(ctrl$risk_category)) {
    add("缺資訊", "中", "尚未選擇風險屬性（財務報導／營運／法令遵循三擇一）",
        "於風險辨識區擇一並可補屬性細節")
  }

  tchk <- rcm_type_fields_check(ctrl$nature %||% ctrl$control_type,
                                ctrl$approach %||% ctrl$control_activity_type)
  if (!isTRUE(tchk$ok)) {
    add("控制缺失", "高", tchk$msg %||% "類型欄錯誤",
        "控制類型＝人工/自動；控制活動類型＝預防/偵測，勿對調")
  }
  if (!is_blank(ctrl$assertions) &&
      (is_reporting_risk_category(ctrl$risk_category %||% "") ||
       is_operations_risk_category(ctrl$risk_category %||% "")) &&
      !assertions_allowed_ok(ctrl$assertions, ctrl$risk_category)) {
    add("缺資訊", "中", "聲明選項與風險類別不符",
        "報導面用八種 Assertions；營運面僅完整性／正確性／即時性；遵循面不可選")
  }
  if (is_blank(ctrl$assertions) &&
      (is_reporting_risk_category(ctrl$risk_category %||% "") ||
       is_operations_risk_category(ctrl$risk_category %||% ""))) {
    add("缺資訊", "中", "缺少相關聲明", "對應 assertion（4120SR 輔助；可複選）")
  }

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
  doc_mode <- related_document_mode_for_ctrl(ctrl)
  if (identical(doc_mode, "required") &&
      !multi_pbc_is_filled(ctrl$related_document_pbc_ids) &&
      is_blank(ctrl$related_document) &&
      is_blank(ctrl$related_document_manual)) {
    add("缺文件", "高", paste0("缺少", CONTROL_EVIDENCE_DOCUMENT_LABEL, "（可多選；自 PBC 選取或手動輸入）"),
        "至 PBC 資料庫選取或於控制設計手動輸入佐證文件名稱")
  } else if (is_blank(ctrl$outputs) && is_blank(ctrl$related_document) &&
             !identical(doc_mode, "locked")) {
    add("缺文件", "低", paste0("缺少產出／", CONTROL_EVIDENCE_DOCUMENT_LABEL),
        "建議補可驗證證據（簽核、log、調節表）供後續 PBC")
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
  # 三大風險屬性三擇一（定稿前強制清空非選項）
  ctrl <- enforce_single_risk_attr(ctrl)
  # 會計科目：報導面保留並正規化複選；其他類別強制清空
  if (is_reporting_risk_category(ctrl$risk_category %||% "")) {
    ctrl$significant_account <- join_significant_accounts(ctrl$significant_account)
  } else {
    ctrl$significant_account <- ""
  }
  # 相關法令：遵循面保留；其他類別強制清空
  if (!is_compliance_risk_category(ctrl$risk_category %||% "")) {
    ctrl$related_law <- ""
  }
  # 控制佐證文件：自動控制或遵循面強制清空；否則保留 PBC 選取＋手動輸入
  if (identical(related_document_mode_for_ctrl(ctrl), "locked")) {
    ctrl$related_document <- ""
    ctrl$related_document_pbc_ids <- character(0)
    ctrl$related_document_manual <- ""
  } else {
    ctrl$related_document_pbc_ids <- parse_pbc_id_values(ctrl$related_document_pbc_ids)
    ctrl$related_document_manual <- join_text_list_values(ctrl$related_document_manual)
    if (is_blank(ctrl$related_document)) {
      ctrl$related_document <- join_text_list_values(ctrl$related_document_manual)
    }
  }
  # 聲明：依風險類別過濾；遵循面強制清空
  ctrl$assertions <- normalize_assertions_for_category(
    ctrl$assertions, ctrl$risk_category %||% ""
  )
  if (is_blank(ctrl$risk_name) && !is_blank(ctrl$risk_factor)) {
    ctrl$risk_name <- ctrl$risk_factor
  }
  if (is_blank(ctrl$risk_factor) && !is_blank(ctrl$risk_name)) {
    ctrl$risk_factor <- ctrl$risk_name
  }
  ctrl <- sync_iuc_aliases(ctrl)
  if (!is_blank(ctrl$risk_factor) || !is_blank(ctrl$risk_name)) {
    tag <- risk_factor_tag(ctrl$risk_factor %||% ctrl$risk_name)
    ctrl$risk_factor <- tag
    ctrl$risk_name <- tag
  }
  ctrl$frequency <- resolve_control_frequency(
    ctrl$nature %||% ctrl$control_type,
    ctrl$frequency
  )

  req <- design_required_check(ctrl)
  if (!isTRUE(req$ok)) {
    return(list(
      ok = FALSE, ready = FALSE, gaps = detect_design_gaps(ctrl),
      control = NULL, rcm_row = NULL, required = req,
      msg = paste0("必填未齊：", format_design_required_by_accordion(req$missing_by_group, req$missing))
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
  # 設計階段：現況／分析評估欄位留空（不在本 APP 範圍）
  ctrl$company_status <- ""
  ctrl$design_gap_note <- ctrl$design_gap_note %||% ""
  ctrl$effectiveness <- ""
  ctrl$residual_risk <- ""
  ctrl$improvement <- ""
  if (exists("assemble_summary_description", mode = "function")) {
    ctrl$summary_description <- tryCatch(assemble_summary_description(ctrl), error = function(e) "")
  }
  ctrl$detailed_description <- ctrl$detailed_description %||% ""
  ctrl$rcm_ready <- list(ready = TRUE, gaps = ready$gaps)
  ctrl$saved_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
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
