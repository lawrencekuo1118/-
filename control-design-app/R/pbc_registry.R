# IUC / PBC naming registry
# client_pbc_name = 客戶取得之原始 PBC 名稱
# reviewed_name   = 檢視後標準化命名（控制設計／現況撰寫時優先套用）
# pbc_spec        = PBC 規格說明（選填；取得／檢附要求）
# pbc_kind        = 證據類型特別標示（螢幕截圖／EMAIL／系統表單／傳票／人工整理／政策制度）
# pbc_file_format = 樣本檔案格式（.jpg／.png／.pptx 等）

PBC_KIND_CHOICES <- c(
  "請選擇證據類型…" = "",
  "螢幕截圖" = "螢幕截圖",
  "EMAIL" = "EMAIL",
  "系統表單" = "系統表單",
  "傳票" = "傳票",
  "人工整理" = "人工整理",
  "政策制度" = "政策制度"
)

PBC_KIND_VALUES <- unname(PBC_KIND_CHOICES[nzchar(unname(PBC_KIND_CHOICES))])
PBC_KIND_POLICY <- "政策制度"

PBC_FILE_FORMAT_CHOICES <- c(
  "請選擇格式…" = "",
  ".jpg" = ".jpg",
  ".jpeg" = ".jpeg",
  ".png" = ".png",
  ".gif" = ".gif",
  ".webp" = ".webp",
  ".pdf" = ".pdf",
  ".pptx" = ".pptx",
  ".ppt" = ".ppt",
  ".docx" = ".docx",
  ".doc" = ".doc",
  ".xlsx" = ".xlsx",
  ".xls" = ".xls",
  ".csv" = ".csv",
  ".msg" = ".msg",
  ".eml" = ".eml",
  ".mp4" = ".mp4",
  ".mov" = ".mov",
  ".zip" = ".zip",
  ".txt" = ".txt"
)

PBC_FILE_FORMAT_VALUES <- unname(
  PBC_FILE_FORMAT_CHOICES[nzchar(unname(PBC_FILE_FORMAT_CHOICES))]
)

normalize_pbc_kind <- function(x) {
  v <- trimws(as.character(x %||% ""))
  if (v %in% PBC_KIND_VALUES) v else ""
}

# 允許預設清單或自訂副檔名（如 .heic）；一律小寫並補上前導點
normalize_pbc_file_format <- function(x) {
  v <- tolower(trimws(as.character(x %||% "")))
  if (!nzchar(v)) return("")
  v <- sub("^\\*+", "", v)
  if (!startsWith(v, ".") && grepl("^[A-Za-z0-9]+$", v)) {
    v <- paste0(".", v)
  }
  v
}

normalize_pbc_spec <- function(x) {
  trimws(as.character(x %||% ""))
}

# 檢視後命名加上【類型】前綴，供 IUC／PBC 對照特別標示
format_pbc_reviewed_label <- function(reviewed_name, pbc_kind = "") {
  reviewed <- trimws(as.character(reviewed_name %||% ""))
  kind <- normalize_pbc_kind(pbc_kind)
  if (!nzchar(reviewed)) return("")
  if (!nzchar(kind)) return(reviewed)
  sprintf("【%s】%s", kind, reviewed)
}

empty_pbc_registry <- function() {
  data.frame(
    pbc_id = character(),
    client_pbc_name = character(),
    reviewed_name = character(),
    pbc_spec = character(),
    pbc_kind = character(),
    pbc_file_format = character(),
    related_pbc_ids = character(),
    iuc_or_system = character(),
    cycle = character(),
    source = character(),
    notes = character(),
    updated_at = character(),
    stringsAsFactors = FALSE
  )
}

upsert_pbc <- function(registry, row) {
  stopifnot(is.data.frame(registry))
  registry <- normalize_pbc_df(registry)
  row <- as.list(row)
  id <- trimws(as.character(row$pbc_id %||% ""))
  client <- trimws(as.character(row$client_pbc_name %||% ""))
  reviewed <- trimws(as.character(row$reviewed_name %||% ""))
  if (!nzchar(client) && !nzchar(reviewed)) {
    stop("請至少填「客戶原名」或「檢視後命名」")
  }
  if (!nzchar(reviewed)) reviewed <- client
  kind <- normalize_pbc_kind(row$pbc_kind)
  file_fmt <- normalize_pbc_file_format(row$pbc_file_format)
  spec <- normalize_pbc_spec(row$pbc_spec)
  related <- normalize_related_pbc_ids(row$related_pbc_ids)
  display <- format_pbc_reviewed_label(reviewed, kind)

  # Match existing by id, else by same client_pbc_name (reuse / update mapping)
  if (!nzchar(id)) {
    hit <- which(registry$client_pbc_name == client & nzchar(client))
    if (length(hit)) {
      id <- registry$pbc_id[hit[[1]]]
    } else {
      n <- nrow(registry) + 1L
      while (sprintf("PBC-%03d", n) %in% registry$pbc_id) n <- n + 1L
      id <- sprintf("PBC-%03d", n)
    }
  }

  new_row <- data.frame(
    pbc_id = id,
    client_pbc_name = client,
    reviewed_name = reviewed,
    pbc_spec = spec,
    pbc_kind = kind,
    pbc_file_format = file_fmt,
    related_pbc_ids = related,
    iuc_or_system = trimws(as.character(row$iuc_or_system %||% display)),
    cycle = trimws(as.character(row$cycle %||% "")),
    source = trimws(as.character(row$source %||% "client")),
    notes = trimws(as.character(row$notes %||% "")),
    updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    stringsAsFactors = FALSE
  )
  if (id %in% registry$pbc_id) {
    registry[registry$pbc_id == id, ] <- new_row
  } else {
    registry <- rbind(registry, new_row)
  }
  rownames(registry) <- NULL
  registry
}

delete_pbc <- function(registry, pbc_ids) {
  pbc_ids <- as.character(pbc_ids)
  registry[!registry$pbc_id %in% pbc_ids, , drop = FALSE]
}

is_pbc_policy_kind <- function(kind) {
  identical(normalize_pbc_kind(kind), PBC_KIND_POLICY)
}

# 勾稽 ID 清單（分號／逗號分隔）
normalize_related_pbc_ids <- function(x) {
  vals <- parse_pbc_id_values(x)
  if (!length(vals)) return("")
  paste(unique(vals), collapse = "；")
}

# 從規格說明文字解析可勾稽之 PBC（#N／PBC#N／PBC-…／「名稱」）
parse_pbc_cross_refs_from_text <- function(text, registry, prefer_cycle = "") {
  raw <- as.character(text %||% "")
  if (!nzchar(trimws(raw)) || !is.data.frame(registry) || !nrow(registry)) {
    return(character())
  }
  known <- unique(trimws(as.character(registry$pbc_id)))
  hits <- character()
  prefer_cycle <- trimws(as.character(prefer_cycle %||% ""))

  id_pat <- gregexpr("PBC-[A-Za-z0-9]+-\\d{2,4}|PBC-\\d{2,4}", raw, perl = TRUE, ignore.case = TRUE)
  if (id_pat[[1]][1] > 0) {
    found <- unique(toupper(regmatches(raw, id_pat)[[1]]))
    # restore original casing from known ids
    for (f in found) {
      hit <- known[toupper(known) == f]
      if (length(hit)) hits <- c(hits, hit[[1]])
    }
  }

  hash_pat <- gregexpr("#\\s*\\d{1,3}", raw, perl = TRUE)
  if (hash_pat[[1]][1] > 0) {
    nums <- unique(as.integer(gsub("\\D", "", regmatches(raw, hash_pat)[[1]])))
    nums <- nums[is.finite(nums) & nums >= 1L & nums <= 999L]
    id_cycle <- if ("cycle" %in% names(registry)) {
      stats::setNames(trimws(as.character(registry$cycle)), registry$pbc_id)
    } else {
      character()
    }
    for (n in nums) {
      suffix <- sprintf("-%03d", n)
      hit <- known[grepl(paste0(suffix, "$"), known, perl = TRUE)]
      if (nzchar(prefer_cycle) && length(hit)) {
        same <- hit[unname(id_cycle[hit]) == prefer_cycle]
        if (length(same)) hit <- same
      }
      if (length(hit) > 1L) {
        pref <- hit[grepl("^PBC-[A-Za-z]+-\\d{3}$", hit, perl = TRUE)]
        if (length(pref)) hit <- pref
      }
      if (length(hit)) hits <- c(hits, hit[[1]])
    }
  }

  name_pat <- gregexpr("「[^」]{1,120}」", raw, perl = TRUE)
  if (name_pat[[1]][1] > 0) {
    quoted <- gsub("[「」]", "", regmatches(raw, name_pat)[[1]])
    for (q in unique(trimws(quoted))) {
      if (!nzchar(q)) next
      idx <- which(
        registry$client_pbc_name == q |
          registry$reviewed_name == q |
          grepl(q, registry$client_pbc_name, fixed = TRUE) |
          grepl(q, registry$reviewed_name, fixed = TRUE)
      )
      if (nzchar(prefer_cycle) && length(idx) && "cycle" %in% names(registry)) {
        same <- idx[registry$cycle[idx] == prefer_cycle]
        if (length(same)) idx <- same
      }
      if (length(idx)) hits <- c(hits, registry$pbc_id[idx[[1]]])
    }
  }
  unique(hits[nzchar(hits)])
}

# 勾稽選單（排除自身）
pbc_related_link_choices <- function(registry, exclude_id = "") {
  if (!is.data.frame(registry) || !nrow(registry)) return(character())
  excl <- trimws(as.character(exclude_id %||% ""))
  df <- registry
  if (nzchar(excl)) df <- df[df$pbc_id != excl, , drop = FALSE]
  if (!nrow(df)) return(character())
  labels <- sprintf(
    "%s｜%s",
    df$pbc_id,
    ifelse(nzchar(df$reviewed_name), df$reviewed_name,
           ifelse(nzchar(df$client_pbc_name), df$client_pbc_name, "—"))
  )
  stats::setNames(df$pbc_id, labels)
}

# Walkthrough：出鏈（本列勾稽）＋入鏈（他列勾稽至本列）
pbc_walkthrough <- function(registry, pbc_id) {
  empty <- list(
    focus_id = "",
    outbound = character(),
    inbound = character(),
    related_rows = if (is.data.frame(registry)) registry[0, , drop = FALSE] else empty_pbc_registry()
  )
  id <- trimws(as.character(pbc_id %||% ""))
  if (!nzchar(id) || !is.data.frame(registry) || !nrow(registry)) return(empty)
  if (!"related_pbc_ids" %in% names(registry)) {
    registry$related_pbc_ids <- ""
  }
  focus <- registry[registry$pbc_id == id, , drop = FALSE]
  if (!nrow(focus)) return(empty)
  outbound <- parse_pbc_id_values(focus$related_pbc_ids[[1]])
  outbound <- outbound[outbound %in% registry$pbc_id & outbound != id]
  inbound <- character()
  for (i in seq_len(nrow(registry))) {
    oid <- registry$pbc_id[i]
    if (identical(oid, id)) next
    rel <- parse_pbc_id_values(registry$related_pbc_ids[i])
    if (id %in% rel) inbound <- c(inbound, oid)
  }
  inbound <- unique(inbound)
  all_ids <- unique(c(outbound, inbound))
  list(
    focus_id = id,
    outbound = outbound,
    inbound = inbound,
    related_rows = registry[registry$pbc_id %in% all_ids, , drop = FALSE]
  )
}

format_pbc_walkthrough_lines <- function(registry, pbc_id) {
  wt <- pbc_walkthrough(registry, pbc_id)
  if (!nzchar(wt$focus_id)) return(character())
  label_of <- function(ids) {
    if (!length(ids)) return(character())
    vapply(ids, function(rid) {
      row <- registry[registry$pbc_id == rid, , drop = FALSE]
      if (!nrow(row)) return(rid)
      nm <- ifelse(nzchar(row$reviewed_name[[1]]), row$reviewed_name[[1]], row$client_pbc_name[[1]])
      sprintf("%s｜%s", rid, ifelse(nzchar(nm), nm, "—"))
    }, character(1))
  }
  lines <- character()
  if (length(wt$outbound)) {
    lines <- c(lines, "→ 往下勾稽（本 PBC 指向）：", paste0("  • ", label_of(wt$outbound)))
  }
  if (length(wt$inbound)) {
    lines <- c(lines, "← 往上勾稽（他 PBC 指向本列）：", paste0("  • ", label_of(wt$inbound)))
  }
  if (!length(lines)) {
    return("尚無勾稽連結（可於「互相勾稽」選取，或由規格說明中的 #N／「名稱」自動解析）。")
  }
  lines
}

# 依規格說明自動補齊勾稽（保留既有手動連結）
enrich_related_pbc_from_specs <- function(registry) {
  registry <- normalize_pbc_df(registry)
  if (!nrow(registry)) return(registry)
  for (i in seq_len(nrow(registry))) {
    auto <- parse_pbc_cross_refs_from_text(
      registry$pbc_spec[i], registry,
      prefer_cycle = registry$cycle[i]
    )
    auto <- setdiff(auto, registry$pbc_id[i])
    cur <- parse_pbc_id_values(registry$related_pbc_ids[i])
    registry$related_pbc_ids[i] <- normalize_related_pbc_ids(c(cur, auto))
  }
  registry
}

filter_pbc_registry <- function(registry, cycle_filter = NULL,
                                include_kinds = NULL, exclude_kinds = NULL) {
  if (!is.data.frame(registry) || !nrow(registry)) return(empty_pbc_registry())
  df <- registry
  if (!is.null(cycle_filter) && nzchar(cycle_filter)) {
    keep <- !nzchar(df$cycle) | df$cycle == cycle_filter
    df <- df[keep, , drop = FALSE]
  }
  inc <- trimws(as.character(include_kinds %||% character()))
  inc <- inc[nzchar(inc)]
  if (length(inc)) {
    df <- df[vapply(df$pbc_kind, function(k) normalize_pbc_kind(k) %in% inc, logical(1)), , drop = FALSE]
  }
  exc <- trimws(as.character(exclude_kinds %||% character()))
  exc <- exc[nzchar(exc)]
  if (length(exc)) {
    df <- df[!vapply(df$pbc_kind, function(k) normalize_pbc_kind(k) %in% exc, logical(1)), , drop = FALSE]
  }
  df
}

# Choices for design UI: value = pbc_id (stable); label = ID＋檢視後命名
pbc_choices <- function(registry, cycle_filter = NULL,
                          include_kinds = NULL, exclude_kinds = NULL) {
  df <- filter_pbc_registry(
    registry,
    cycle_filter = cycle_filter,
    include_kinds = include_kinds,
    exclude_kinds = exclude_kinds
  )
  if (!nrow(df)) return(character())
  labels <- vapply(seq_len(nrow(df)), function(i) {
    reviewed <- trimws(df$reviewed_name[i])
    if (!nzchar(reviewed)) reviewed <- trimws(df$client_pbc_name[i])
    paste0(df$pbc_id[i], format_pbc_reviewed_label(reviewed, df$pbc_kind[i]))
  }, character(1))
  stats::setNames(df$pbc_id, labels)
}

# IUC／控制佐證文件／訪談 PBC 串接：排除政策制度
pbc_non_policy_choices <- function(registry, cycle_filter = NULL) {
  pbc_choices(registry, cycle_filter = cycle_filter, exclude_kinds = PBC_KIND_POLICY)
}

# 相關政策與制度：僅政策制度類 PBC
pbc_policy_choices <- function(registry, cycle_filter = NULL) {
  pbc_choices(registry, cycle_filter = cycle_filter, include_kinds = PBC_KIND_POLICY)
}

lookup_pbc <- function(registry, pbc_ids) {
  pbc_ids <- unique(as.character(pbc_ids %||% character()))
  pbc_ids <- pbc_ids[nzchar(pbc_ids)]
  if (!length(pbc_ids) || !nrow(registry)) {
    return(registry[0, , drop = FALSE])
  }
  registry[registry$pbc_id %in% pbc_ids, , drop = FALSE]
}

parse_pbc_id_values <- function(x) {
  if (is.null(x) || !length(x)) return(character())
  if (length(x) > 1L) {
    vals <- trimws(as.character(x))
    vals <- vals[!is.na(vals)]
    return(unique(vals[nzchar(vals)]))
  }
  raw <- trimws(as.character(if (is.null(x)) "" else x))
  if (length(raw) != 1L || is.na(raw) || !nzchar(raw)) return(character())
  vals <- trimws(unlist(strsplit(raw, "[;；|/、,，]+")))
  vals <- vals[!is.na(vals)]
  unique(vals[nzchar(vals)])
}

# Multi-value text／selectize（IUC、控制佐證文件等）
parse_text_list_values <- function(x) {
  parse_pbc_id_values(x)
}

join_text_list_values <- function(x) {
  vals <- parse_text_list_values(x)
  if (!length(vals)) return("")
  paste(vals, collapse = "；")
}

pbc_ids_are_filled <- function(x) {
  length(parse_pbc_id_values(x)) > 0L
}

multi_pbc_is_filled <- function(x) {
  length(parse_text_list_values(x)) > 0L
}

known_pbc_ids <- function(registry) {
  if (!is.data.frame(registry) || !nrow(registry)) return(character())
  unique(trimws(as.character(registry$pbc_id)))
}

split_pbc_selection <- function(selection, registry = NULL) {
  sel <- parse_text_list_values(selection)
  if (!length(sel)) {
    return(list(ids = character(), manual = character()))
  }
  known <- known_pbc_ids(registry)
  if (!length(known)) {
    return(list(ids = character(), manual = sel))
  }
  is_id <- sel %in% known
  list(ids = sel[is_id], manual = sel[!is_id])
}

resolve_multi_pbc_text <- function(selection, registry = NULL) {
  parts <- split_pbc_selection(selection, registry)
  labels <- character()
  if (length(parts$ids) && is.data.frame(registry) && nrow(registry)) {
    labels <- c(labels, unlist(strsplit(
      apply_pbc_to_iuc(registry, parts$ids), "；", fixed = TRUE
    )))
  }
  if (length(parts$manual)) labels <- c(labels, parts$manual)
  labels <- unique(trimws(labels))
  labels <- labels[nzchar(labels)]
  if (!length(labels)) return("")
  paste(labels, collapse = "；")
}

expand_pbc_selection <- function(text, registry = NULL, stored_ids = character()) {
  ids <- unique(c(parse_pbc_id_values(stored_ids), match_pbc_ids_from_text(registry, text)))
  parts <- parse_text_list_values(text)
  manual <- character()
  for (p in parts) {
    hit <- match_pbc_ids_from_text(registry, p)
    if (length(hit)) {
      ids <- c(ids, hit)
    } else {
      manual <- c(manual, p)
    }
  }
  unique(c(ids, manual))
}

# Apply selected PBC ids → reviewed names for IUC field
apply_pbc_to_iuc <- function(registry, pbc_ids) {
  rows <- lookup_pbc(registry, pbc_ids)
  if (!nrow(rows)) return("")
  paste(unique(vapply(seq_len(nrow(rows)), function(i) {
    format_pbc_reviewed_label(rows$reviewed_name[i], rows$pbc_kind[i])
  }, character(1))), collapse = "；")
}

# 控制佐證文件：與 IUC 相同格式（檢視後命名＋證據類型標示）
apply_pbc_to_related_document <- function(registry, pbc_ids) {
  apply_pbc_to_iuc(registry, pbc_ids)
}

# 舊資料自由文字 → 嘗試對照 PBC id（精確比對檢視後命名／原名）
match_pbc_ids_from_text <- function(registry, text) {
  raw <- trimws(as.character(text %||% ""))
  if (!nzchar(raw) || !is.data.frame(registry) || !nrow(registry)) {
    return(character())
  }
  parts <- trimws(unlist(strsplit(raw, "[;；|/]+")))
  parts <- parts[nzchar(parts)]
  if (!length(parts)) return(character())
  hits <- character()
  for (p in parts) {
    p_clean <- sub("^【[^】]+】", "", p)
    idx <- which(
      registry$pbc_id == p |
        registry$reviewed_name == p |
        registry$reviewed_name == p_clean |
        registry$client_pbc_name == p |
        registry$client_pbc_name == p_clean |
        vapply(seq_len(nrow(registry)), function(i) {
          format_pbc_reviewed_label(registry$reviewed_name[i], registry$pbc_kind[i]) == p
        }, logical(1))
    )
    if (length(idx)) hits <- c(hits, registry$pbc_id[idx[[1]]])
  }
  unique(hits)
}

# Dual-name line for 控制現況 / Inputs documentation
format_pbc_status_lines <- function(registry, pbc_ids = NULL) {
  df <- if (is.null(pbc_ids)) registry else lookup_pbc(registry, pbc_ids)
  if (!nrow(df)) return(character())
  vapply(seq_len(nrow(df)), function(i) {
    kind <- normalize_pbc_kind(df$pbc_kind[i])
    kind_txt <- if (nzchar(kind)) paste0("【", kind, "】") else ""
    sprintf(
      "%s%s：客戶取得 PBC「%s」／檢視後命名「%s」%s",
      df$pbc_id[i],
      kind_txt,
      ifelse(nzchar(df$client_pbc_name[i]), df$client_pbc_name[i], "（未填原名）"),
      format_pbc_reviewed_label(df$reviewed_name[i], df$pbc_kind[i]),
      if (nzchar(df$notes[i])) paste0("（", df$notes[i], "）") else ""
    )
  }, character(1))
}

format_pbc_for_inputs <- function(registry, pbc_ids) {
  lines <- format_pbc_status_lines(registry, pbc_ids)
  if (!length(lines)) return("")
  paste(c("【IUC／PBC 命名對照】", lines), collapse = "\n")
}

# Persist as CSV (human-editable) + JSON companion
save_pbc_registry <- function(registry, path_csv, path_json = NULL) {
  utils::write.csv(registry, path_csv, row.names = FALSE, fileEncoding = "UTF-8")
  if (!is.null(path_json)) {
    jsonlite::write_json(registry, path_json, dataframe = "rows", pretty = TRUE, auto_unbox = TRUE)
  }
  invisible(path_csv)
}

load_pbc_registry <- function(path_csv = NULL, path_json = NULL) {
  if (!is.null(path_csv) && file.exists(path_csv)) {
    df <- utils::read.csv(path_csv, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
    return(normalize_pbc_df(df))
  }
  if (!is.null(path_json) && file.exists(path_json)) {
    raw <- jsonlite::read_json(path_json, simplifyVector = TRUE)
    df <- as.data.frame(raw, stringsAsFactors = FALSE)
    return(normalize_pbc_df(df))
  }
  empty_pbc_registry()
}

normalize_pbc_df <- function(df) {
  need <- names(empty_pbc_registry())
  if (!nrow(df)) return(empty_pbc_registry())
  for (nm in need) {
    if (!nm %in% names(df)) df[[nm]] <- ""
    df[[nm]] <- as.character(df[[nm]] %||% "")
  }
  df[, need, drop = FALSE]
}

# 讀取 PBC 匯入表（CSV 或 Excel 第一個工作表）
read_pbc_import_table <- function(path, original_name = NULL) {
  path <- as.character(path %||% "")
  if (!nzchar(path) || !file.exists(path)) {
    stop("找不到匯入檔案")
  }
  label <- trimws(as.character(original_name %||% ""))
  if (!nzchar(label)) label <- path
  ext <- tolower(tools::file_ext(label))
  if (!nzchar(ext)) ext <- tolower(tools::file_ext(path))

  if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("需要 readxl 套件以匯入 Excel：install.packages(\"readxl\")")
    }
    sheets <- readxl::excel_sheets(path)
    if (!length(sheets)) stop("Excel 檔沒有工作表")
    prefer <- which(tolower(sheets) %in% c("pbc", "pbc資料庫", "pbc_registry", "sheet1"))
    sheet <- if (length(prefer)) sheets[[prefer[[1]]]] else sheets[[1]]
    raw <- as.data.frame(
      readxl::read_excel(path, sheet = sheet, col_names = TRUE),
      stringsAsFactors = FALSE
    )
  } else if (ext %in% c("csv", "txt") || !nzchar(ext)) {
    raw <- tryCatch(
      utils::read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8"),
      error = function(e) {
        utils::read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
      }
    )
  } else {
    stop("僅支援 CSV 或 Excel（.xlsx／.xls）匯入")
  }

  if (!is.data.frame(raw) || !nrow(raw)) {
    stop("匯入檔沒有資料列")
  }
  # Excel 常把空值讀成 NA
  for (nm in names(raw)) {
    raw[[nm]] <- as.character(raw[[nm]])
    raw[[nm]][is.na(raw[[nm]])] <- ""
  }
  names(raw) <- tolower(gsub("[\\s　]+", "_", names(raw)))
  raw
}

# Import loose CSV／Excel with flexible column aliases
import_pbc_file <- function(path, existing = empty_pbc_registry(),
                            original_name = NULL) {
  raw <- read_pbc_import_table(path, original_name = original_name)
  alias <- list(
    pbc_id = c("pbc_id", "id", "編號"),
    client_pbc_name = c(
      "client_pbc_name", "client_name", "pbc_name", "客戶原名", "原名", "pbc",
      "文件／檔案名稱", "文件_檔案名稱", "文件名稱", "檔案名稱"
    ),
    reviewed_name = c("reviewed_name", "new_name", "standard_name", "檢視後命名", "新命名", "iuc"),
    pbc_kind = c("pbc_kind", "kind", "證據類型", "pbc_type", "類型"),
    pbc_file_format = c(
      "pbc_file_format", "file_format", "format", "ext", "extension",
      "樣本檔案格式", "原始取得文件格式", "文件格式", "副檔名"
    ),
    pbc_spec = c(
      "pbc_spec", "spec", "specification", "規格說明", "pbc規格說明",
      "pbc_規格說明", "規格", "取得要求", "備註", "樣本需求說明", "樣本需求"
    ),
    related_pbc_ids = c(
      "related_pbc_ids", "related_pbc", "related", "links", "walkthrough",
      "勾稽", "互相勾稽", "相關pbc", "相關_pbc"
    ),
    iuc_or_system = c("iuc_or_system", "iuc", "system"),
    cycle = c("cycle", "循環"),
    source = c("source", "來源"),
    notes = c("notes", "note", "其他備註")
  )
  pick <- function(keys) {
    for (k in keys) if (k %in% names(raw)) return(raw[[k]])
    rep("", nrow(raw))
  }
  for (i in seq_len(nrow(raw))) {
    existing <- upsert_pbc(existing, list(
      pbc_id = pick(alias$pbc_id)[i],
      client_pbc_name = pick(alias$client_pbc_name)[i],
      reviewed_name = pick(alias$reviewed_name)[i],
      pbc_spec = pick(alias$pbc_spec)[i],
      pbc_kind = pick(alias$pbc_kind)[i],
      pbc_file_format = pick(alias$pbc_file_format)[i],
      related_pbc_ids = pick(alias$related_pbc_ids)[i],
      iuc_or_system = pick(alias$iuc_or_system)[i],
      cycle = pick(alias$cycle)[i],
      source = pick(alias$source)[i],
      notes = pick(alias$notes)[i]
    ))
  }
  existing
}

# 相容舊呼叫名稱
import_pbc_csv <- function(path, existing = empty_pbc_registry(),
                           original_name = NULL) {
  import_pbc_file(path, existing = existing, original_name = original_name)
}
