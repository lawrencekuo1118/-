# CSA（控制點測試設計）
# 自我評估／控制點測試程序；依情境組展開
# 需先 source rcm.R（共用 DESIGN_ELEMENTS、derive_control_id 等）

DEFAULT_CSA_ELEMENTS <- c(
  "control_objective", "control_activity", "steps",
  "iuc", "outputs", "exception", "frequency_owner"
)
# 控制測試抽樣：PCAOB AS 2301／2315 ＋ Deloitte 頻率表（Higher RoMM／Fraud 上調）

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
# CSA 多情境組（同一控制點 × 不同控制現況）
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
# CSA rows: procedures / evidence / expected result；依情境組展開
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
