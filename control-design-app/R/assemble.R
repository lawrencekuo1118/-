# Assembly of Form 4120SR-aligned control narratives + IUC-based split.

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && (is.na(x) || !nzchar(as.character(x))))) y else x
}

normalize_iuc_key <- function(iuc_text) {
  parts <- unlist(strsplit(as.character(iuc_text %||% ""), "[\n;；,，|/]+"))
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  if (!length(parts)) return("__NO_IUC__")
  paste(sort(unique(tolower(parts))), collapse = " | ")
}

blank_to_na <- function(x) {
  x <- trimws(as.character(x %||% ""))
  if (!nzchar(x)) NA_character_ else x
}

# Split draft rows under the same risk into distinct control points when IUC differs.
split_controls_by_iuc <- function(drafts) {
  if (!length(drafts)) return(list())
  keys <- vapply(drafts, function(d) normalize_iuc_key(d$iuc_or_system), character(1))
  split_ids <- split(seq_along(drafts), keys)
  out <- list()
  for (key in names(split_ids)) {
    idxs <- split_ids[[key]]
    group <- drafts[idxs]
    # One control point per IUC key; merge complementary fields when multiple rows share IUC.
    out[[length(out) + 1]] <- merge_draft_group(group, iuc_key = key)
  }
  out
}

merge_draft_group <- function(group, iuc_key) {
  first <- group[[1]]
  pick_unique <- function(field) {
    vals <- unique(na.omit(vapply(group, function(g) blank_to_na(g[[field]]), character(1))))
    if (!length(vals)) return("")
    paste(vals, collapse = "；")
  }
  steps <- unlist(lapply(group, function(g) {
    s <- g$review_steps %||% ""
    if (!nzchar(trimws(s))) return(character())
    unlist(strsplit(s, "\n"))
  }))
  steps <- unique(trimws(steps[nzchar(trimws(steps))]))
  list(
    control_id = first$control_id %||% "",
    company = first$company %||% "",
    cycle = first$cycle %||% "",
    risk_name = first$risk_name %||% "",
    risk_description = pick_unique("risk_description"),
    risk_attr_financial = pick_unique("risk_attr_financial"),
    risk_attr_operations = pick_unique("risk_attr_operations"),
    risk_attr_compliance = pick_unique("risk_attr_compliance"),
    romm_classification = pick_unique("romm_classification"),
    significant_account = pick_unique("significant_account"),
    assertions = pick_unique("assertions"),
    control_objective = pick_unique("control_objective"),
    control_activity = pick_unique("control_activity"),
    frequency = pick_unique("frequency"),
    responsible_unit = pick_unique("responsible_unit"),
    iuc_or_system = if (identical(iuc_key, "__NO_IUC__")) pick_unique("iuc_or_system") else {
      # Prefer human-readable unique IUC labels from drafts
      vals <- unique(na.omit(vapply(group, function(g) blank_to_na(g$iuc_or_system), character(1))))
      paste(vals, collapse = "；")
    },
    iuc_key = iuc_key,
    nature = pick_unique("nature"),
    approach = pick_unique("approach"),
    type = pick_unique("type"),
    inputs = pick_unique("inputs"),
    review_steps = paste(steps, collapse = "\n"),
    outputs = pick_unique("outputs"),
    investigation_threshold = pick_unique("investigation_threshold"),
    dependent_controls = pick_unique("dependent_controls"),
    source_row_count = length(group)
  )
}

assemble_control_paragraph <- function(ctrl) {
  company <- blank_to_na(ctrl$company)
  cycle <- blank_to_na(ctrl$cycle)
  risk <- blank_to_na(ctrl$risk_name)
  risk_desc <- blank_to_na(ctrl$risk_description)

  fmt_attr <- function(val, default_label) {
    val <- trimws(as.character(val %||% ""))
    if (!nzchar(val)) return(sprintf("%s：（未填）", default_label))
    # Already labelized as [標籤] 內容
    if (grepl("^\\[[^\\]]+\\]", val)) return(val)
    sprintf("%s：%s", default_label, val)
  }
  attr_bits <- c(
    fmt_attr(ctrl$risk_attr_financial, "財務報導"),
    fmt_attr(ctrl$risk_attr_operations, "營運"),
    fmt_attr(ctrl$risk_attr_compliance, "法令遵循")
  )

  steps_raw <- ctrl$review_steps %||% ""
  step_lines <- trimws(unlist(strsplit(as.character(steps_raw), "\n")))
  step_lines <- step_lines[nzchar(step_lines)]
  if (length(step_lines)) {
    numbered <- paste0(
      seq_along(step_lines), ". ",
      sub("^[0-9]+[\\.、\\)]\\s*", "", step_lines)
    )
    steps_txt <- paste(numbered, collapse = "")
  } else {
    steps_txt <- nzchar_or(ctrl$control_activity, "（請補充覆核／執行步驟）")
  }

  inputs <- nzchar_or(ctrl$inputs, nzchar_or(ctrl$iuc_or_system, "（請補充投入資訊）"))
  outputs <- nzchar_or(ctrl$outputs, "（請補充控制產出／留存軌跡）")
  iuc <- nzchar_or(ctrl$iuc_or_system, "（未指定 IUC／制度）")
  owner <- nzchar_or(ctrl$responsible_unit, "（負責單位未填）")
  freq <- nzchar_or(ctrl$frequency, "（頻率未填）")
  objective <- nzchar_or(ctrl$control_objective, "（控制目標未填）")
  activity <- nzchar_or(ctrl$control_activity, "（控制活動未填）")
  nature <- nzchar_or(ctrl$nature, "（性質未填）")
  approach <- nzchar_or(ctrl$approach, "（取向未填）")
  type <- nzchar_or(ctrl$type, "（類型未填）")
  account <- nzchar_or(ctrl$significant_account, "（重大科目未填）")
  assertions <- nzchar_or(ctrl$assertions, "（聲明未填）")
  romm <- nzchar_or(ctrl$romm_classification, "（RoMM 分類未填）")
  threshold <- blank_to_na(ctrl$investigation_threshold)
  deps <- blank_to_na(ctrl$dependent_controls)

  lead <- if (!is.na(company)) {
    sprintf("就%s現行%s作業，", company, cycle %||% "相關")
  } else {
    sprintf("就公司現行%s作業，", cycle %||% "相關")
  }

  risk_part <- sprintf(
    "為因應「%s」風險%s（風險三大屬性—%s；RoMM：%s；重大科目／聲明：%s／%s），",
    risk %||% "（未命名風險）",
    if (!is.na(risk_desc)) paste0("——", risk_desc) else "",
    paste(attr_bits, collapse = "；"),
    romm, account, assertions
  )

  design_part <- sprintf(
    "設計控制目標為「%s」。由%s以%s方式、採%s取向、屬%s類型，依%s執行下列控制活動：%s。",
    objective, owner, nature, approach, type, freq, activity
  )

  detail_part <- sprintf(
    "控制執行時使用之 IUC／制度為「%s」。投入（Inputs）：%s。覆核／執行步驟（Specific activities）：%s。產出（Outputs）：%s。",
    iuc, inputs, steps_txt, outputs
  )

  extra <- character()
  if (!is.na(threshold)) {
    extra <- c(extra, sprintf("調查門檻與後續追蹤：%s。", threshold))
  }
  if (!is.na(deps)) {
    extra <- c(extra, sprintf("本控制依賴其他控制／資訊：%s。", deps))
  }

  paste0(lead, risk_part, design_part, detail_part, paste(extra, collapse = ""))
}

nzchar_or <- function(x, fallback) {
  x <- trimws(as.character(x %||% ""))
  if (!nzchar(x)) fallback else x
}

assemble_summary_description <- function(ctrl) {
  sprintf(
    "[%s｜%s] %s：%s（頻率：%s；負責：%s；IUC：%s）",
    nzchar_or(ctrl$cycle, "未選循環"),
    nzchar_or(ctrl$risk_name, "未命名風險"),
    nzchar_or(ctrl$control_objective, "控制目標"),
    nzchar_or(ctrl$control_activity, "控制活動"),
    nzchar_or(ctrl$frequency, "—"),
    nzchar_or(ctrl$responsible_unit, "—"),
    nzchar_or(ctrl$iuc_or_system, "—")
  )
}

# Completeness check vs Form 4120SR design narrative elements
validate_control_design <- function(ctrl) {
  missing <- character()
  req <- list(
    cycle = "九大循環",
    risk_name = "風險名稱",
    risk_attr_financial = "風險屬性—財務報導",
    risk_attr_operations = "風險屬性—營運",
    risk_attr_compliance = "風險屬性—法令遵循",
    control_objective = "控制目標",
    control_activity = "控制活動",
    frequency = "控制頻率",
    responsible_unit = "負責單位",
    iuc_or_system = "IUC／制度",
    nature = "Nature",
    approach = "Approach",
    type = "Type",
    inputs = "Inputs Used by Reviewer",
    review_steps = "Specific Activities / Steps",
    outputs = "Outputs of the Control",
    significant_account = "Significant Account(s)",
    assertions = "Related Assertion(s)"
  )
  for (nm in names(req)) {
    if (!nzchar(trimws(as.character(ctrl[[nm]] %||% "")))) {
      missing <- c(missing, req[[nm]])
    }
  }
  list(ok = !length(missing), missing = missing)
}
