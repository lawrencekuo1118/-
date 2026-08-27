# Persistent backend catalog of all stored parameter options in the app.
# Sources: 系統預設清單 + 範本庫 + 已定稿 RCM + PBC 命名庫


empty_parameter_store <- function() {
  data.frame(
    參數 = character(),
    選項值 = character(),
    來源 = character(),
    出現次數 = integer(),
    最近更新 = character(),
    stringsAsFactors = FALSE
  )
}

app_preset_parameters <- function() {
  list(
    "循環" = unname(if (exists("CYCLES_NINE_CHOICES")) CYCLES_NINE_CHOICES else CYCLES_NINE),
    "風險類別" = if (exists("RISK_CATEGORY_CHOICES")) RISK_CATEGORY_CHOICES else c("報導面", "營運面", "遵循面"),
    "控制性質" = if (exists("CONTROL_TYPE_MANUAL_AUTO")) CONTROL_TYPE_MANUAL_AUTO else character(),
    "控制方式" = if (exists("CONTROL_ACTIVITY_TYPE_PD")) CONTROL_ACTIVITY_TYPE_PD else character(),
    "控制頻率" = if (exists("FREQUENCY_CHOICES")) FREQUENCY_CHOICES else character(),
    "相關法規" = unname(if (exists("RELATED_LAW_CHOICES")) RELATED_LAW_CHOICES else character()),
    "控制聲明" = {
      rep <- if (exists("ASSERTION_CHOICES_REPORTING")) ASSERTION_CHOICES_REPORTING else character()
      ops <- if (exists("ASSERTION_CHOICES_OPERATIONS")) ASSERTION_CHOICES_OPERATIONS else character()
      unique(c(rep, ops))
    },
    "會計科目" = {
      all_opt <- if (exists("ACCOUNT_ALL_OPTION")) ACCOUNT_ALL_OPTION else "全部適用"
      std <- if (exists("ACCOUNT_CHOICES")) ACCOUNT_CHOICES else character()
      c(all_opt, std)
    },
    "Form 4120SR Type" = if (exists("TYPE_CHOICES")) TYPE_CHOICES else character(),
    "RoMM 分類" = if (exists("ROMM_CLASS_CHOICES")) ROMM_CLASS_CHOICES else character(),
    # 不提供「控制有效性評估」等非設計參數預設，避免寫入參數庫
    "PBC 證據類型" = if (exists("PBC_KIND_VALUES")) PBC_KIND_VALUES else character(),
    "PBC 原始取得文件格式" = if (exists("PBC_FILE_FORMAT_VALUES")) {
      PBC_FILE_FORMAT_VALUES
    } else {
      character()
    }
  )
}

.split_multi <- function(x) {
  v <- trimws(as.character(unlist(x, use.names = FALSE)))
  v <- unlist(strsplit(v, "[;；|/]+"))
  v <- trimws(v)
  unique(v[nzchar(v) & !identical(v, "NA") & !identical(v, "—") & !identical(v, "-")])
}

parameter_catalog <- function(library = list(), controls = list(),
                              pbc = NULL, presets = NULL) {
  if (is.null(presets)) presets <- app_preset_parameters()
  rows <- list()
  add_vals <- function(param, values, source) {
    if (exists("is_blocked_parameter_name", mode = "function") &&
        isTRUE(is_blocked_parameter_name(param))) {
      return(invisible(NULL))
    }
    values <- .split_multi(values)
    ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    for (v in values) {
      rows[[length(rows) + 1]] <<- data.frame(
        參數 = as.character(param),
        選項值 = v,
        來源 = as.character(source),
        出現次數 = 1L,
        最近更新 = ts,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(presets)) {
    for (nm in names(presets)) add_vals(nm, presets[[nm]], "系統預設")
  }
  collect_ctrl <- function(ctrl, source) {
    if (is.null(ctrl)) return()
    c <- if (!is.null(ctrl$control)) ctrl$control else ctrl
    add_vals("循環", c$cycle %||% ctrl$cycle, source)
    add_vals("子作業名稱", c$sub_process, source)
    add_vals("風險因素", c$risk_factor %||% c$risk_name, source)
    add_vals("風險描述", c$risk_description, source)
    add_vals("風險面向", c$risk_principle, source)
    add_vals("風險範疇", c$risk_area, source)
    add_vals("風險類別", c$risk_category, source)
    add_vals("會計科目", c$significant_account, source)
    add_vals("控制目標", c$control_objective, source)
    add_vals("控制活動", c$control_activity, source)
    add_vals("控制性質", c$nature %||% c$control_type, source)
    add_vals("控制方式", c$approach %||% c$control_activity_type, source)
    add_vals("控制頻率", c$frequency, source)
    add_vals("控制點負責單位", c$responsible_unit, source)
    add_vals(CONTROL_IUC_DOCUMENT_LABEL, ctrl_iuc_value(c), source)
    add_vals("相關系統", ctrl_related_system_value(c), source)
    add_vals("相關法規", c$related_law, source)
    add_vals("相關法規連結", c$related_law_url, source)
    add_vals("相關政策與制度", c$related_policy, source)
    add_vals("相關文件", c$related_documents, source)
    add_vals(CONTROL_EVIDENCE_DOCUMENT_LABEL, c$related_document %||% c$outputs, source)
    # 不收集公司現況／有效性／改善建議／編號等非設計欄
    add_vals("控制聲明", c$assertions, source)
    add_vals("Form 4120SR Type", c$type, source)
    add_vals("RoMM 分類", c$romm_classification, source)
  }
  for (it in library) collect_ctrl(it, "範本庫")
  for (it in controls) collect_ctrl(it, "已定稿RCM")
  if (is.data.frame(pbc) && nrow(pbc)) {
    add_vals("PBC 客戶原名", pbc$client_pbc_name, "PBC命名庫")
    add_vals("PBC 檢視後命名", pbc$reviewed_name, "PBC命名庫")
    add_vals("PBC 證據類型", pbc$pbc_kind, "PBC命名庫")
    if ("pbc_file_format" %in% names(pbc)) {
      add_vals("PBC 原始取得文件格式", pbc$pbc_file_format, "PBC命名庫")
    }
    if ("pbc_spec" %in% names(pbc)) {
      add_vals("PBC 規格說明", pbc$pbc_spec, "PBC命名庫")
    }
    if (exists("is_pbc_policy_kind", mode = "function")) {
      pol_mask <- vapply(pbc$pbc_kind, is_pbc_policy_kind, logical(1))
      add_vals("相關政策與制度", pbc$iuc_or_system[pol_mask], "PBC命名庫")
      add_vals("IUC", pbc$iuc_or_system[!pol_mask], "PBC命名庫")
    } else {
      add_vals("IUC", pbc$iuc_or_system, "PBC命名庫")
    }
  }
  if (!length(rows)) return(empty_parameter_store())
  df <- do.call(rbind, rows)
  merge_parameter_store(empty_parameter_store(), df)
}

merge_parameter_store <- function(base, incoming) {
  if (!is.data.frame(base) || !nrow(base)) base <- empty_parameter_store()
  if (!is.data.frame(incoming) || !nrow(incoming)) return(base)
  need <- names(empty_parameter_store())
  for (nm in need) {
    if (!nm %in% names(base)) base[[nm]] <- if (identical(nm, "出現次數")) 0L else ""
    if (!nm %in% names(incoming)) incoming[[nm]] <- if (identical(nm, "出現次數")) 1L else ""
  }
  all <- rbind(base[, need, drop = FALSE], incoming[, need, drop = FALSE])
  key <- paste(all$參數, all$選項值, sep = "\t")
  split_idx <- split(seq_len(nrow(all)), key)
  out <- lapply(split_idx, function(idx) {
    chunk <- all[idx, , drop = FALSE]
    data.frame(
      參數 = chunk$參數[[1]],
      選項值 = chunk$選項值[[1]],
      來源 = paste(unique(unlist(strsplit(paste(chunk$來源, collapse = "＋"), "[＋+]"))), collapse = "＋"),
      出現次數 = sum(as.integer(chunk$出現次數), na.rm = TRUE),
      最近更新 = {
        ts <- chunk$最近更新[nzchar(chunk$最近更新)]
        if (length(ts)) ts[[length(ts)]] else format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      },
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, out)
  rownames(df) <- NULL
  df[order(df$參數, -df$出現次數, df$選項值), , drop = FALSE]
}

save_parameter_store <- function(df, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  payload <- lapply(seq_len(nrow(df)), function(i) {
    list(
      param = df$參數[[i]],
      value = df$選項值[[i]],
      source = df$來源[[i]],
      count = as.integer(df$出現次數[[i]]),
      updated_at = df$最近更新[[i]]
    )
  })
  jsonlite::write_json(payload, path, auto_unbox = TRUE, pretty = TRUE, force = TRUE, null = "null")
  invisible(path)
}

load_parameter_store <- function(path) {
  if (!file.exists(path)) return(empty_parameter_store())
  raw <- tryCatch(jsonlite::read_json(path, simplifyVector = FALSE), error = function(e) list())
  if (!length(raw)) return(empty_parameter_store())
  rows <- lapply(raw, function(x) {
    data.frame(
      參數 = as.character(x$param %||% x$參數 %||% ""),
      選項值 = as.character(x$value %||% x$選項值 %||% ""),
      來源 = as.character(x$source %||% x$來源 %||% ""),
      出現次數 = as.integer(x$count %||% x$出現次數 %||% 1L),
      最近更新 = as.character(x$updated_at %||% x$最近更新 %||% ""),
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, rows)
  df <- df[nzchar(df$參數) & nzchar(df$選項值), , drop = FALSE]
  if (!nrow(df)) return(empty_parameter_store())
  if (exists("is_blocked_parameter_name", mode = "function")) {
    df <- df[!vapply(df$參數, is_blocked_parameter_name, logical(1)), , drop = FALSE]
  }
  if (!nrow(df)) return(empty_parameter_store())
  merge_parameter_store(empty_parameter_store(), df)
}

rebuild_parameter_store <- function(library = list(), controls = list(),
                                    pbc = NULL, existing = NULL) {
  live <- parameter_catalog(library, controls, pbc = pbc)
  if (is.null(existing) || !nrow(existing)) return(live)
  merge_parameter_store(existing, live)
}

parameter_store_stats <- function(df) {
  if (!is.data.frame(df) || !nrow(df)) {
    return(list(n = 0L, n_params = 0L, params = character(), sources = character()))
  }
  list(
    n = nrow(df),
    n_params = length(unique(df$參數)),
    params = sort(unique(df$參數)),
    sources = sort(unique(unlist(strsplit(paste(df$來源, collapse = "＋"), "[＋+]"))))
  )
}

filter_parameter_store <- function(df, param = NULL, query = NULL, source = NULL) {
  if (!is.data.frame(df) || !nrow(df)) return(empty_parameter_store())
  out <- df
  if (!is.null(param) && nzchar(param)) out <- out[out$參數 == param, , drop = FALSE]
  if (!is.null(source) && nzchar(source)) {
    out <- out[grepl(source, out$來源, fixed = TRUE), , drop = FALSE]
  }
  if (!is.null(query) && nzchar(trimws(query))) {
    q <- trimws(query)
    out <- out[grepl(q, out$選項值, fixed = TRUE) | grepl(q, out$參數, fixed = TRUE), , drop = FALSE]
  }
  out
}

# Direct admin edit: upsert one option row (參數 + 選項值 key)
upsert_parameter_row <- function(df, param, value, source = "高權維護") {
  if (!is.data.frame(df)) df <- empty_parameter_store()
  param <- trimws(as.character(param %||% ""))
  value <- trimws(as.character(value %||% ""))
  source <- trimws(as.character(source %||% "高權維護"))
  if (!nzchar(param) || !nzchar(value)) return(df)
  if (exists("is_blocked_parameter_name", mode = "function") &&
      isTRUE(is_blocked_parameter_name(param))) {
    return(df)
  }
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  key_match <- which(df$參數 == param & df$選項值 == value)
  if (length(key_match)) {
    i <- key_match[[1]]
    df$來源[[i]] <- source
    df$出現次數[[i]] <- max(1L, as.integer(df$出現次數[[i]] %||% 1L))
    df$最近更新[[i]] <- ts
  } else {
    df <- rbind(df, data.frame(
      參數 = param, 選項值 = value, 來源 = source,
      出現次數 = 1L, 最近更新 = ts, stringsAsFactors = FALSE
    ))
  }
  rownames(df) <- NULL
  df[order(df$參數, df$選項值), , drop = FALSE]
}

delete_parameter_rows <- function(df, indices) {
  if (!is.data.frame(df) || !nrow(df)) return(empty_parameter_store())
  indices <- as.integer(indices)
  indices <- indices[indices >= 1L & indices <= nrow(df)]
  if (!length(indices)) return(df)
  df <- df[-indices, , drop = FALSE]
  rownames(df) <- NULL
  df
}

# 取出某參數之既有選項值（供設計頁選單合併自訂）
parameter_options <- function(df, param) {
  if (!is.data.frame(df) || !nrow(df)) return(character())
  param <- trimws(as.character(param %||% ""))
  if (!nzchar(param)) return(character())
  v <- trimws(as.character(df$選項值[df$參數 == param]))
  unique(v[nzchar(v)])
}

# 設計儲存成功後：將質性／選單欄位值寫入參數庫為「設計自訂」選項
DESIGN_PARAM_FIELD_MAP <- function() {
  iuc_lab <- if (exists("CONTROL_IUC_DOCUMENT_LABEL", mode = "character")) {
    CONTROL_IUC_DOCUMENT_LABEL
  } else {
    "相關文件-控制用文件"
  }
  ev_lab <- if (exists("CONTROL_EVIDENCE_DOCUMENT_LABEL", mode = "character")) {
    CONTROL_EVIDENCE_DOCUMENT_LABEL
  } else {
    "相關文件-控制佐證文件"
  }
  stats::setNames(
    c(
      "sub_process", "risk_description", "control_objective", "control_activity",
      "responsible_unit", "iuc_or_system", "related_documents", "related_document",
      "related_system", "related_policy", "related_law", "related_law_url"
    ),
    c(
      "子作業名稱", "風險描述", "控制目標", "控制活動",
      "控制點負責單位", iuc_lab, "相關文件", ev_lab,
      "相關系統", "相關政策與制度", "相關法規", "相關法規連結"
    )
  )
}

ingest_ctrl_parameters <- function(df, ctrl, source = "設計自訂") {
  if (!is.data.frame(df)) df <- empty_parameter_store()
  if (is.null(ctrl)) return(df)
  c <- if (!is.null(ctrl$control)) ctrl$control else ctrl
  fmap <- DESIGN_PARAM_FIELD_MAP()
  for (i in seq_along(fmap)) {
    param <- names(fmap)[[i]]
    key <- unname(fmap[[i]])
    raw <- c[[key]]
    if (identical(key, "iuc_or_system")) {
      raw <- c$iuc %||% c$iuc_or_system %||% ""
    }
    if (identical(key, "related_document")) {
      raw <- c$related_document %||% c$outputs %||% ""
    }
    for (v in .split_multi(raw)) {
      df <- upsert_parameter_row(df, param, v, source = source)
    }
  }
  df
}
