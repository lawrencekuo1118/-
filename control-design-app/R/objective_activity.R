# Strict separation of RCM 控制目標 (Why) vs 控制活動 (How)
# 目標：要達成什麼／對應何風險與聲明（結果導向）
# 活動：誰、做什麼、用什麼、如何做（行動導向）
# 兩者禁止相同、互抄、錯欄

OBJECTIVE_CUES <- c(
  "確保", "達成", "防止", "避免", "及時", "正確", "完整", "有效",
  "僅對", "只有", "不得", "應能", "得以", "維持", "保障", "降低",
  "一致", "相符", "可靠", "允當"
)

ACTIVITY_CUES <- c(
  "覆核", "核對", "比對", "核准", "簽核", "產製", "產出", "執行", "抽查",
  "檢核", "確認", "登錄", "過帳", "移轉", "回簽", "追蹤", "調節", "盤點",
  "取得", "檢視", "驗證", "授權", "呈送", "呈核", "退回", "異動", "開立",
  "停用", "鎖定", "比對後", "不符者", "逐筆", "抽樣"
)

ACTIVITY_ACTOR_CUES <- c(
  "人員", "主管", "管理員", "會計", "出納", "承辦", "單位", "部門",
  "系統", "程式", "自動", "由"
)

normalize_oa_text <- function(x) {
  x <- trimws(as.character(x %||% ""))
  x <- gsub("\\s+", "", x)
  x <- gsub("[，,。．.；;：:、\\-—_（）()【】\\[\\]\"'「」]", "", x)
  tolower(x)
}

contains_any <- function(text, cues) {
  text <- as.character(text %||% "")
  any(vapply(cues, function(c) grepl(c, text, fixed = TRUE), logical(1)))
}

count_cues <- function(text, cues) {
  text <- as.character(text %||% "")
  sum(vapply(cues, function(c) grepl(c, text, fixed = TRUE), integer(1)))
}

# Returns list(ok, severity, issues=character(), objective_verdict, activity_verdict, hints)
rcm_objective_activity_check <- function(objective, activity) {
  obj <- trimws(as.character(objective %||% ""))
  act <- trimws(as.character(activity %||% ""))
  issues <- character()
  hints <- character()
  obj_verdict <- "未填"
  act_verdict <- "未填"

  if (!nzchar(obj) || !nzchar(act)) {
    if (!nzchar(obj)) issues <- c(issues, "控制目標空白")
    if (!nzchar(act)) issues <- c(issues, "控制活動空白")
    return(list(
      ok = FALSE, severity = "高", issues = issues,
      objective_verdict = if (nzchar(obj)) "已填" else "未填",
      activity_verdict = if (nzchar(act)) "已填" else "未填",
      hints = c("目標＝Why（要達成什麼）；活動＝How（誰如何做）")
    ))
  }

  n_obj <- normalize_oa_text(obj)
  n_act <- normalize_oa_text(act)

  if (identical(n_obj, n_act)) {
    issues <- c(issues, "控制目標與控制活動文字相同（禁止）")
    hints <- c(hints, "目標改寫成結果句；活動改寫成執行句")
  }

  # Near-duplicate / containment
  if (nchar(n_act) > 0 && nchar(n_obj) > 0) {
    if (grepl(n_obj, n_act, fixed = TRUE) && nchar(n_act) <= nchar(n_obj) + 8) {
      issues <- c(issues, "控制活動幾乎只重述控制目標")
      hints <- c(hints, "活動須補上執行者、表單／系統、具體動作")
    }
    if (grepl(n_act, n_obj, fixed = TRUE) && nchar(n_obj) <= nchar(n_act) + 8) {
      issues <- c(issues, "控制目標幾乎只重述控制活動")
      hints <- c(hints, "目標改為風險導向結果，勿寫執行步驟")
    }
  }

  obj_has_outcome <- contains_any(obj, OBJECTIVE_CUES)
  obj_has_action <- contains_any(obj, ACTIVITY_CUES)
  act_has_action <- contains_any(act, ACTIVITY_CUES)
  act_has_actor <- contains_any(act, ACTIVITY_ACTOR_CUES)
  act_obj_cues <- count_cues(act, OBJECTIVE_CUES)
  obj_act_cues <- count_cues(obj, ACTIVITY_CUES)

  # Objective looks like steps / procedure
  if (obj_has_action && !obj_has_outcome) {
    issues <- c(issues, "控制目標似為執行步驟（缺結果導向用語）")
    hints <- c(hints, "目標可用「確保／防止／僅對…」開句，對應風險與聲明")
  }
  if (obj_act_cues >= 2 && grepl("每[日週月季年]|逐筆|後簽核|後回簽", obj)) {
    issues <- c(issues, "控制目標混入頻率與執行細節，應下移至控制活動")
    hints <- c(hints, "頻率／誰做／如何做放活動欄；目標只留要達成的結果")
  }
  # Multi-step objective (numbered or many verbs)
  if (grepl("\n|1\\.|2\\.|①|②|步驟", obj) || obj_act_cues >= 3) {
    issues <- c(issues, "控制目標含多步驟，屬活動欄內容")
    hints <- c(hints, "步驟請放到 Steps／控制活動，目標保留一句結果")
  }

  # Activity looks like pure purpose without how
  if (!act_has_action && act_obj_cues >= 1 && nchar(act) < 24) {
    issues <- c(issues, "控制活動過像目標句，缺少可執行動作")
    hints <- c(hints, "活動應含動詞（覆核／比對／核准…）與執行者或系統")
  }
  if (act_has_action && !act_has_actor && !grepl("系統|自動|程式", act)) {
    # soft warning — still allow but flag medium if no other issues? keep as medium issue
    issues <- c(issues, "控制活動未標示執行者／系統角色（建議補）")
    hints <- c(hints, "例：「會計人員…」「系統自動…」「主管核准…」")
  }

  # Activity should not be ONLY "確保xxx" without verbs
  if (grepl("^確保", act) && !act_has_action) {
    issues <- c(issues, "控制活動以「確保」開句且無執行動作→應改寫為目標或補動作")
    hints <- c(hints, "若是結果句→移到目標；若是做法→改成「誰如何做」")
  }

  # Verdict labels
  obj_verdict <- if (obj_has_outcome && obj_act_cues <= 1) "結果導向（佳）"
  else if (obj_has_outcome) "結果導向但夾雜步驟（宜精簡）"
  else if (obj_has_action) "偏執行步驟（宜改寫）"
  else "語句中性（建議加確保／防止等）"

  act_verdict <- if (act_has_action && act_has_actor) "行動導向（佳）"
  else if (act_has_action) "有動作（建議補執行者）"
  else if (act_obj_cues >= 1) "偏目標句（宜改寫）"
  else "缺執行動詞（宜改寫）"

  # High vs medium: identical/swap/blank-like are 高; missing actor is 中
  high_pat <- "相同|重述|似為執行步驟|多步驟|過像目標|以「確保」開句|空白|混入頻率"
  high <- any(grepl(high_pat, issues))
  # If only "未標示執行者" soft issue, still ok=FALSE for clean RCM? User asked 乾淨俐落 — treat soft as medium and ok=FALSE until fixed for 設計檢核
  # Actually allow ready if only soft actor hint — 設計檢核通過 but show tip
  soft_only <- length(issues) > 0 && all(grepl("未標示執行者", issues))
  ok <- length(issues) == 0 || soft_only
  severity <- if (!length(issues)) "低" else if (soft_only) "低" else if (high) "高" else "中"

  if (soft_only) {
    # don't block RCM row for missing actor alone
    ok <- TRUE
  }

  list(
    ok = ok,
    severity = severity,
    issues = issues,
    objective_verdict = obj_verdict,
    activity_verdict = act_verdict,
    hints = unique(hints),
    msg = if (ok && !length(issues)) "OK" else if (ok) paste(issues, collapse = "；")
    else paste(issues, collapse = "；")
  )
}

# Try to split a mixed pasted sentence into objective vs activity suggestions
suggest_objective_activity_split <- function(text) {
  text <- trimws(as.character(text %||% ""))
  if (!nzchar(text)) {
    return(list(objective = "", activity = "", note = "無內容"))
  }
  # If already two lines, prefer first=obj second=act when cues match
  lines <- trimws(unlist(strsplit(text, "\n")))
  lines <- lines[nzchar(lines)]
  if (length(lines) >= 2) {
    return(list(
      objective = lines[[1]],
      activity = paste(lines[-1], collapse = ""),
      note = "已依換行拆成目標／活動，請再人工確認"
    ))
  }
  # Split on 。 or ； keeping outcome clause vs action clause
  parts <- trimws(unlist(strsplit(text, "[。；;]")))
  parts <- parts[nzchar(parts)]
  if (length(parts) >= 2) {
    scores_obj <- vapply(parts, function(p) count_cues(p, OBJECTIVE_CUES) - count_cues(p, ACTIVITY_CUES), numeric(1))
    scores_act <- vapply(parts, function(p) count_cues(p, ACTIVITY_CUES) - count_cues(p, OBJECTIVE_CUES), numeric(1))
    oi <- which.max(scores_obj)
    ai <- which.max(scores_act)
    if (!identical(oi, ai)) {
      return(list(objective = parts[[oi]], activity = parts[[ai]], note = "已依句意拆分，請確認"))
    }
  }
  # Heuristic rewrite wrappers
  if (contains_any(text, ACTIVITY_CUES) && !contains_any(text, OBJECTIVE_CUES)) {
    return(list(
      objective = paste0("確保", gsub("^確保", "", text), "之相關風險受控"),
      activity = text,
      note = "原文偏活動：已建議補一條結果型目標，請改寫得更精準"
    ))
  }
  if (contains_any(text, OBJECTIVE_CUES) && !contains_any(text, ACTIVITY_CUES)) {
    return(list(
      objective = text,
      activity = "（請補：執行者＋具體動作＋表單／系統）",
      note = "原文偏目標：活動欄待補 How"
    ))
  }
  list(objective = text, activity = text, note = "無法自動拆分，請手動分開 Why／How")
}

format_oa_check_html <- function(chk) {
  iss <- if (length(chk$issues)) paste(chk$issues, collapse = "；") else "無"
  hints <- if (length(chk$hints)) paste(chk$hints, collapse = "；") else ""
  sprintf(
    "目標欄：%s｜活動欄：%s｜檢核：%s%s",
    chk$objective_verdict, chk$activity_verdict,
    if (isTRUE(chk$ok)) "通過" else paste0("未通過—", iss),
    if (nzchar(hints)) paste0("｜建議：", hints) else ""
  )
}
