# 判決書關鍵字判斷模組：從歷史分析結果學習規則，套用於新判決
# 規則儲存於 data/judgment_rules.json（可透過匯入歷史 xlsx 累積更新）

judgment_rules_cache <- new.env(parent = emptyenv())

judgment_rules_default_path <- function(data_dir = NULL) {
  if (!is.null(data_dir) && nzchar(data_dir)) {
    return(file.path(data_dir, "judgment_rules.json"))
  }
  file.path("data", "judgment_rules.json")
}

judgment_rules_normalize <- function(rules_obj) {
  if (is.null(rules_obj) || !is.list(rules_obj)) {
    return(list(version = 1L, updated_at = format(Sys.Date(), "%Y-%m-%d"), rules = list()))
  }
  raw <- rules_obj$rules %||% list()
  if (is.data.frame(raw)) {
    raw <- lapply(seq_len(nrow(raw)), function(i) as.list(raw[i, , drop = FALSE]))
  }
  rules <- lapply(raw, function(r) {
    kws <- r$keywords %||% r$keyword %||% character()
    if (length(kws) == 1L && grepl("[、,，/\\s]", kws, perl = TRUE)) {
      kws <- unlist(strsplit(as.character(kws), "[、,，/\\s]+", perl = TRUE))
    }
    kws <- unique(trimws(as.character(kws)))
    kws <- kws[nzchar(kws)]
    list(
      keywords = kws,
      impact_level = as.character(r$impact_level %||% "無明顯影響"),
      score_weight = as.integer(r$score_weight %||% 0L),
      conclusion = as.character(r$conclusion %||% ""),
      category = as.character(r$category %||% ""),
      source_count = as.integer(r$source_count %||% 0L)
    )
  })
  rules <- Filter(function(r) length(r$keywords) > 0L, rules)
  low_impact <- rules_obj$low_impact_causes %||% judgment_cause_filters_builtin()
  list(
    version = as.integer(rules_obj$version %||% 1L),
    updated_at = as.character(rules_obj$updated_at %||% format(Sys.Date(), "%Y-%m-%d")),
    rules = rules,
    low_impact_causes = low_impact
  )
}

judgment_rules_load <- function(path = NULL, data_dir = NULL, use_cache = TRUE) {
  path <- path %||% judgment_rules_default_path(data_dir)
  cache_key <- normalizePath(path, mustWork = FALSE)
  if (isTRUE(use_cache) && exists(cache_key, envir = judgment_rules_cache, inherits = FALSE)) {
    return(get(cache_key, envir = judgment_rules_cache))
  }
  if (!file.exists(path)) {
    obj <- judgment_rules_normalize(list(rules = list()))
    assign(cache_key, obj, envir = judgment_rules_cache)
    return(obj)
  }
  raw <- if (requireNamespace("jsonlite", quietly = TRUE)) {
    jsonlite::read_json(path, simplifyVector = FALSE)
  } else {
    stop("需要 jsonlite 以讀取判決規則")
  }
  obj <- judgment_rules_normalize(raw)
  assign(cache_key, obj, envir = judgment_rules_cache)
  obj
}

judgment_rules_save <- function(rules_obj, path = NULL, data_dir = NULL) {
  path <- path %||% judgment_rules_default_path(data_dir)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  existing <- if (file.exists(path)) {
    tryCatch(judgment_rules_load(path = path, data_dir = data_dir, use_cache = FALSE), error = function(e) NULL)
  } else {
    NULL
  }
  rules_obj <- judgment_rules_normalize(rules_obj)
  if (!is.null(existing) && !length(rules_obj$low_impact_causes %||% list())) {
    rules_obj$low_impact_causes <- existing$low_impact_causes
  }
  rules_obj$updated_at <- format(Sys.Date(), "%Y-%m-%d")
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("需要 jsonlite 以儲存判決規則")
  }
  jsonlite::write_json(rules_obj, path, auto_unbox = TRUE, pretty = TRUE)
  cache_key <- normalizePath(path, mustWork = FALSE)
  assign(cache_key, rules_obj, envir = judgment_rules_cache)
  invisible(path)
}

judgment_rules_invalidate_cache <- function(path = NULL, data_dir = NULL) {
  path <- path %||% judgment_rules_default_path(data_dir)
  cache_key <- normalizePath(path, mustWork = FALSE)
  if (exists(cache_key, envir = judgment_rules_cache, inherits = FALSE)) {
    rm(list = cache_key, envir = judgment_rules_cache)
  }
  invisible(NULL)
}

judgment_rules_level_weight <- function(level) {
  switch(as.character(level),
         "高" = 18L,
         "中" = 8L,
         "低" = 2L,
         0L)
}

judgment_rules_split_keywords <- function(text) {
  text <- trimws(as.character(text %||% ""))
  if (!nzchar(text)) return(character())
  unique(trimws(unlist(strsplit(text, "[、,，/\\s]+", perl = TRUE))))
}

judgment_rules_rule_key <- function(rule) {
  kws <- sort(unique(trimws(as.character(rule$keywords %||% character()))))
  kws <- kws[nzchar(kws)]
  if (!length(kws)) return("")
  paste(kws, collapse = "|")
}

judgment_rules_merge <- function(existing, learned) {
  existing <- judgment_rules_normalize(existing)
  learned <- judgment_rules_normalize(learned)
  by_key <- list()
  for (r in c(existing$rules, learned$rules)) {
    key <- judgment_rules_rule_key(r)
    if (!nzchar(key)) next
    if (!key %in% names(by_key)) {
      by_key[[key]] <- r
      next
    }
    old <- by_key[[key]]
    old_count <- as.integer(old$source_count %||% 0L)
    new_count <- as.integer(r$source_count %||% 0L)
    pick <- if (new_count >= old_count) r else old
    by_key[[key]] <- list(
      keywords = unique(c(old$keywords, r$keywords)),
      impact_level = pick$impact_level,
      score_weight = max(as.integer(old$score_weight %||% 0L), as.integer(r$score_weight %||% 0L)),
      conclusion = if (nzchar(pick$conclusion)) pick$conclusion else old$conclusion,
      category = if (nzchar(pick$category)) pick$category else old$category,
      source_count = old_count + new_count
    )
  }
  existing$rules <- unname(by_key)
  if (is.null(learned$low_impact_causes) && !is.null(existing$low_impact_causes)) {
    # keep existing filters
  }
  existing
}

judgment_learn_from_history <- function(df) {
  if (!is.data.frame(df) || !nrow(df)) {
    return(list(version = 1L, updated_at = format(Sys.Date(), "%Y-%m-%d"), rules = list()))
  }
  kw_col <- intersect(c("命中關鍵字", "keywords"), names(df))[1]
  level_col <- intersect(c("財務營運影響等級", "impact_level"), names(df))[1]
  rationale_col <- intersect(c("影響分析說明", "裁判分析結論", "conclusion"), names(df))[1]
  if (is.na(kw_col) || is.na(level_col)) {
    stop("歷史資料需含「命中關鍵字」與「財務營運影響等級」欄位")
  }

  kw_stats <- list()
  for (i in seq_len(nrow(df))) {
    kws <- judgment_rules_split_keywords(df[[kw_col]][[i]])
    level <- trimws(as.character(df[[level_col]][[i]] %||% ""))
    if (!level %in% c("高", "中", "低", "無明顯影響")) next
    rationale <- if (!is.na(rationale_col)) trimws(as.character(df[[rationale_col]][[i]] %||% "")) else ""
    for (kw in kws) {
      if (!kw %in% names(kw_stats)) {
        kw_stats[[kw]] <- list(levels = character(), rationales = character())
      }
      kw_stats[[kw]]$levels <- c(kw_stats[[kw]]$levels, level)
      if (nzchar(rationale)) kw_stats[[kw]]$rationales <- c(kw_stats[[kw]]$rationales, rationale)
    }
  }

  rules <- lapply(names(kw_stats), function(kw) {
    st <- kw_stats[[kw]]
    tab <- sort(table(st$levels), decreasing = TRUE)
    dom_level <- names(tab)[1]
    count <- length(st$levels)
    conclusion <- if (length(st$rationales)) {
      tab_r <- sort(table(st$rationales), decreasing = TRUE)
      names(tab_r)[1]
    } else {
      sprintf("歷史分析中「%s」多與%s影響相關（共 %d 筆）", kw, dom_level, count)
    }
    list(
      keywords = kw,
      impact_level = dom_level,
      score_weight = judgment_rules_level_weight(dom_level),
      conclusion = conclusion,
      category = "歷史學習",
      source_count = count
    )
  })
  list(
    version = 1L,
    updated_at = format(Sys.Date(), "%Y-%m-%d"),
    rules = rules
  )
}

judgment_import_history_xlsx <- function(path, sheet = "判決分析") {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("需要 readxl 以讀取歷史分析 xlsx：install.packages(\"readxl\")")
  }
  sheets <- readxl::excel_sheets(path)
  if (!sheet %in% sheets) {
    sheet <- sheets[grep("判決|分析", sheets)[1] %||% 1L]
  }
  df <- as.data.frame(readxl::read_excel(path, sheet = sheet), stringsAsFactors = FALSE)
  judgment_learn_from_history(df)
}

judgment_rules_match <- function(text, rules_obj = NULL, data_dir = NULL) {
  text <- trimws(as.character(text %||% ""))
  rules_obj <- rules_obj %||% judgment_rules_load(data_dir = data_dir)
  rules_obj <- judgment_rules_normalize(rules_obj)
  matched <- list()
  for (r in rules_obj$rules) {
    kws <- as.character(r$keywords %||% character())
    hits <- kws[vapply(kws, function(k) nzchar(k) && grepl(k, text, fixed = TRUE), logical(1))]
    if (length(hits)) matched[[length(matched) + 1L]] <- c(r, list(matched_keywords = hits))
  }
  matched
}

judgment_rules_assess <- function(summary, full_text, target_company = "", rules_obj = NULL, data_dir = NULL) {
  text <- paste(summary, full_text, sep = "\n")
  matched <- judgment_rules_match(text, rules_obj = rules_obj, data_dir = data_dir)
  score <- 0L
  conclusions <- character()
  categories <- character()
  all_hits <- character()

  for (m in matched) {
    hits <- m$matched_keywords %||% character()
    w <- as.integer(m$score_weight %||% judgment_rules_level_weight(m$impact_level))
    score <- score + w * length(hits)
    all_hits <- c(all_hits, hits)
    concl <- trimws(as.character(m$conclusion %||% ""))
    if (nzchar(concl)) conclusions <- c(conclusions, concl)
    cat <- trimws(as.character(m$category %||% ""))
    if (nzchar(cat)) categories <- c(categories, cat)
  }
  all_hits <- unique(all_hits)

  company <- trimws(as.character(target_company %||% ""))
  mentions_company <- nzchar(company) && grepl(company, text, fixed = TRUE)
  if (mentions_company) score <- score + 25L
  if (grepl("億|千萬|百萬", text)) score <- score + 10L
  if (grepl("刑事|詐欺|背信|侵占|洗錢", text)) score <- score + 12L

  level <- if (score >= 70L) {
    "高"
  } else if (score >= 40L) {
    "中"
  } else if (score >= 15L) {
    "低"
  } else {
    "無明顯影響"
  }

  rationale <- character()
  if (mentions_company) {
    rationale <- c(rationale, sprintf("判決內容提及標的公司「%s」", company))
  } else if (nzchar(company)) {
    rationale <- c(rationale, sprintf("未直接提及標的公司「%s」；影響評估僅供參考", company))
  }
  if (length(matched)) {
    rationale <- c(rationale, sprintf("命中 %d 條判斷規則", length(matched)))
  }
  if (length(all_hits)) {
    rationale <- c(rationale, paste0("關鍵字：", paste(all_hits, collapse = "、")))
  }
  if (!length(rationale)) rationale <- "未命中判斷規則"

  analysis_conclusion <- if (length(conclusions)) {
    paste(unique(conclusions), collapse = "；")
  } else if (length(all_hits)) {
    sprintf("命中關鍵字「%s」，建議人工複核財務營運影響", paste(all_hits, collapse = "、"))
  } else {
    "未命中已知關鍵字模式，暫判無明顯財務營運衝擊"
  }

  list(
    財務營運影響等級 = level,
    影響分數 = as.integer(score),
    影響分析說明 = paste(rationale, collapse = "；"),
    命中關鍵字 = paste(all_hits, collapse = "、"),
    裁判分析結論 = analysis_conclusion,
    規則命中數 = as.integer(length(matched)),
    規則類別 = paste(unique(categories), collapse = "、")
  )
}

judgment_cause_filters_builtin <- function() {
  list(
    list(patterns = c("侵權行為損害賠償(交通)", "交通損害賠償"), case_types = "民事", label = "個人交通事故", group = "民事"),
    list(patterns = "返還土地權狀", case_types = "民事", label = "個人土地權狀", group = "民事"),
    list(patterns = "返還不動產", case_types = "民事", label = "個人不动产返還", group = "民事"),
    list(patterns = c("確認贈與", "撤銷贈與"), case_types = "民事", label = "個人贈與", group = "民事"),
    list(patterns = c("鄰地通行", "界址", "排水"), case_types = "民事", label = "鄰地糾紛", group = "民事"),
    list(patterns = c("分割遺產", "遺產清儀", "遺產分配"), case_types = "民事", label = "個人繼承", group = "民事"),
    list(patterns = c("離婚", "監護", "扶養", "收養", "家事保護"), case_types = "民事", label = "家事事件", group = "家事"),
    list(patterns = c("毒品", "施用毒品", "持有毒品"), case_types = "刑事", label = "個人毒品", group = "刑事"),
    list(patterns = c("公共危險", "不能安全駕駛", "酒駕", "酒後駕車"), case_types = "刑事", label = "個人酒駕", group = "刑事"),
    list(
      patterns = c("過失傷害", "過失致死"),
      case_types = "刑事",
      label = "個人過失傷害",
      group = "刑事",
      exclude_if_text_contains = c("公司", "股份", "有限", "勞工", "職業", "工安")
    ),
    list(patterns = c("妨害名譽", "誹謗", "侮辱"), case_types = "刑事", label = "個人妨害名譽", group = "刑事"),
    list(
      patterns = "傷害",
      case_types = "刑事",
      label = "個人傷害",
      group = "刑事",
      exclude_if_text_contains = c("公司", "股份", "有限", "勞工", "職業", "工安")
    ),
    list(patterns = c("通姦", "妨害家庭"), case_types = "刑事", label = "個人妨害家庭", group = "刑事"),
    list(patterns = "訴願決定", case_types = "行政", label = "個人訴願", group = "行政"),
    list(patterns = c("國民年金", "健保", "全民健康保險"), case_types = "行政", label = "個人社會保險", group = "行政"),
    list(patterns = c("入出境", "移民", "居留"), case_types = "行政", label = "個人入出境", group = "行政"),
    list(patterns = character(), case_types = "憲法", label = "憲法事件", group = "憲法"),
    list(patterns = character(), case_types = "懲戒", label = "公務員懲戒", group = "懲戒")
  )
}

judgment_cause_filters_load <- function(data_dir = NULL, rules_obj = NULL) {
  rules_obj <- rules_obj %||% judgment_rules_load(data_dir = data_dir)
  filters <- rules_obj$low_impact_causes %||% list()
  if (!length(filters)) filters <- judgment_cause_filters_builtin()
  filters
}

judgment_cause_filter_summary <- function(data_dir = NULL) {
  filters <- judgment_cause_filters_load(data_dir = data_dir)
  groups <- vapply(filters, function(f) as.character(f$group %||% ""), character(1))
  groups <- groups[nzchar(groups)]
  sprintf("低營運影響案由規則 %d 條（%s）", length(filters), paste(unique(groups), collapse = "、"))
}

judgment_cause_match_low_impact <- function(cause, case_type, full_text, filters = NULL, data_dir = NULL) {
  cause <- trimws(as.character(cause %||% ""))
  case_type <- trimws(as.character(case_type %||% ""))
  full_text <- as.character(full_text %||% "")
  filters <- filters %||% judgment_cause_filters_load(data_dir = data_dir)
  text <- paste(cause, full_text, sep = "\n")

  for (f in filters) {
    unless <- as.character(f$exclude_if_text_contains %||% character())
    if (length(unless) && any(vapply(unless, function(u) nzchar(u) && grepl(u, full_text, fixed = TRUE), logical(1)))) {
      next
    }
    ctypes <- as.character(f$case_types %||% character())
    if (length(ctypes) && nzchar(case_type) && !case_type %in% ctypes) next
    pats <- as.character(f$patterns %||% character())
    if (!length(pats) && length(ctypes) && nzchar(case_type) && case_type %in% ctypes) {
      return(list(
        excluded = TRUE,
        label = as.character(f$label %||% ctypes[1]),
        matched_pattern = case_type,
        group = as.character(f$group %||% "")
      ))
    }
    for (p in pats) {
      if (!nzchar(p)) next
      if (grepl(p, cause, fixed = TRUE) || grepl(p, full_text, fixed = TRUE)) {
        return(list(
          excluded = TRUE,
          label = as.character(f$label %||% p),
          matched_pattern = p,
          group = as.character(f$group %||% "")
        ))
      }
    }
  }
  list(excluded = FALSE, label = "", matched_pattern = "", group = "")
}

judgment_apply_cause_exclusion <- function(impact, exclusion) {
  if (!isTRUE(exclusion$excluded)) return(impact)
  impact$財務營運影響等級 <- "無明顯影響"
  impact$影響分數 <- 0L
  note <- sprintf("【案由排除：%s】%s", exclusion$label, exclusion$matched_pattern)
  impact$影響分析說明 <- paste(c(note, impact$影響分析說明), collapse = "；")
  impact$裁判分析結論 <- sprintf("案由屬低營運影響類型（%s），不納入公司財務營運風險", exclusion$label)
  impact
}

judgment_analyze_detail <- function(
    detail,
    target_company = "",
    rules_obj = NULL,
    data_dir = NULL,
    apply_cause_exclusion = TRUE) {
  summary <- judgment_summarize(detail, target_company = target_company)
  impact <- judgment_rules_assess(
    summary,
    detail$全文 %||% "",
    target_company = target_company,
    rules_obj = rules_obj,
    data_dir = data_dir
  )
  exclusion <- judgment_cause_match_low_impact(
    detail$案由 %||% "",
    detail$案件類別 %||% "",
    detail$全文 %||% "",
    data_dir = data_dir
  )
  if (isTRUE(apply_cause_exclusion) && isTRUE(exclusion$excluded)) {
    impact <- judgment_apply_cause_exclusion(impact, exclusion)
  }
  list(
    內容摘要 = summary,
    案由 = detail$案由 %||% "",
    案件類別 = detail$案件類別 %||% "",
    財務營運影響等級 = impact$財務營運影響等級,
    影響分數 = impact$影響分數,
    影響分析說明 = impact$影響分析說明,
    命中關鍵字 = impact$命中關鍵字,
    裁判分析結論 = impact$裁判分析結論,
    營運影響篩選 = if (isTRUE(exclusion$excluded)) "已排除" else "保留",
    案由排除原因 = exclusion$label
  )
}

judgment_update_rules_from_history <- function(df, data_dir = NULL, path = NULL) {
  learned <- judgment_learn_from_history(df)
  existing <- judgment_rules_load(path = path, data_dir = data_dir, use_cache = FALSE)
  merged <- judgment_rules_merge(existing, learned)
  judgment_rules_save(merged, path = path, data_dir = data_dir)
  merged
}

judgment_update_rules_from_xlsx <- function(xlsx_path, data_dir = NULL, path = NULL) {
  learned <- judgment_import_history_xlsx(xlsx_path)
  existing <- judgment_rules_load(path = path, data_dir = data_dir, use_cache = FALSE)
  merged <- judgment_rules_merge(existing, learned)
  judgment_rules_save(merged, path = path, data_dir = data_dir)
  merged
}

judgment_rules_summary_text <- function(data_dir = NULL, path = NULL) {
  obj <- judgment_rules_load(path = path, data_dir = data_dir)
  n <- length(obj$rules %||% list())
  learned_n <- sum(vapply(obj$rules, function(r) identical(r$category, "歷史學習"), logical(1)))
  sprintf("判斷規則 %d 條（其中 %d 條來自歷史學習）", n, learned_n)
}
