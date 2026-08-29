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
      conclusion = as.character(r$conclusion %||% ""),
      category = as.character(r$category %||% ""),
      source_count = as.integer(r$source_count %||% 0L)
    )
  })
  rules <- Filter(function(r) length(r$keywords) > 0L, rules)
  list(
    version = as.integer(rules_obj$version %||% 1L),
    updated_at = as.character(rules_obj$updated_at %||% format(Sys.Date(), "%Y-%m-%d")),
    rules = rules
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
  rules_obj <- judgment_rules_normalize(rules_obj)
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
      conclusion = if (nzchar(pick$conclusion)) pick$conclusion else old$conclusion,
      category = if (nzchar(pick$category)) pick$category else old$category,
      source_count = old_count + new_count
    )
  }
  existing$rules <- unname(by_key)
  existing
}

judgment_learn_from_history <- function(df) {
  if (!is.data.frame(df) || !nrow(df)) {
    return(list(version = 1L, updated_at = format(Sys.Date(), "%Y-%m-%d"), rules = list()))
  }
  kw_col <- intersect(c("命中關鍵字", "keywords"), names(df))[1]
  conclusion_col <- intersect(c("結果分析", "裁判分析結論", "conclusion"), names(df))[1]
  if (is.na(kw_col) || is.na(conclusion_col)) {
    stop("歷史資料需含「命中關鍵字」與「結果分析」欄位")
  }

  kw_stats <- list()
  for (i in seq_len(nrow(df))) {
    kws <- judgment_rules_split_keywords(df[[kw_col]][[i]])
    conclusion <- trimws(as.character(df[[conclusion_col]][[i]] %||% ""))
    if (!nzchar(conclusion)) next
    for (kw in kws) {
      if (!kw %in% names(kw_stats)) {
        kw_stats[[kw]] <- list(conclusions = character())
      }
      kw_stats[[kw]]$conclusions <- c(kw_stats[[kw]]$conclusions, conclusion)
    }
  }

  rules <- lapply(names(kw_stats), function(kw) {
    st <- kw_stats[[kw]]
    tab <- sort(table(st$conclusions), decreasing = TRUE)
    conclusion <- names(tab)[1]
    count <- length(st$conclusions)
    list(
      keywords = kw,
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

judgment_rules_assess <- function(summary, full_text, rules_obj = NULL, data_dir = NULL) {
  text <- paste(summary, full_text, sep = "\n")
  matched <- judgment_rules_match(text, rules_obj = rules_obj, data_dir = data_dir)
  conclusions <- character()
  for (m in matched) {
    concl <- trimws(as.character(m$conclusion %||% ""))
    if (nzchar(concl)) conclusions <- c(conclusions, concl)
  }
  conclusions <- unique(conclusions)
  result <- if (length(conclusions)) {
    paste(conclusions, collapse = "；")
  } else {
    "未命中已知關鍵字模式"
  }
  list(結果分析 = result)
}

judgment_analyze_detail <- function(
    detail,
    rules_obj = NULL,
    data_dir = NULL) {
  summary <- judgment_summarize(detail)
  result <- judgment_rules_assess(
    summary,
    detail$全文 %||% "",
    rules_obj = rules_obj,
    data_dir = data_dir
  )
  list(
    案由 = detail$案由 %||% "",
    案件類別 = detail$案件類別 %||% "",
    結果分析 = result$結果分析
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
