# IUC / PBC naming registry
# client_pbc_name = 客戶取得之原始 PBC 名稱
# reviewed_name   = 檢視後標準化命名（控制設計／現況撰寫時優先套用）
# pbc_kind        = 證據類型特別標示（螢幕截圖／EMAIL／系統表單／政策制度）

PBC_KIND_CHOICES <- c(
  "請選擇證據類型…" = "",
  "螢幕截圖" = "螢幕截圖",
  "EMAIL" = "EMAIL",
  "系統表單" = "系統表單",
  "政策制度" = "政策制度"
)

PBC_KIND_VALUES <- unname(PBC_KIND_CHOICES[nzchar(unname(PBC_KIND_CHOICES))])
PBC_KIND_POLICY <- "政策制度"

normalize_pbc_kind <- function(x) {
  v <- trimws(as.character(x %||% ""))
  if (v %in% PBC_KIND_VALUES) v else ""
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
    pbc_kind = character(),
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
  row <- as.list(row)
  id <- trimws(as.character(row$pbc_id %||% ""))
  client <- trimws(as.character(row$client_pbc_name %||% ""))
  reviewed <- trimws(as.character(row$reviewed_name %||% ""))
  if (!nzchar(client) && !nzchar(reviewed)) {
    stop("請至少填「客戶原名」或「檢視後命名」")
  }
  if (!nzchar(reviewed)) reviewed <- client
  kind <- normalize_pbc_kind(row$pbc_kind)
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
    pbc_kind = kind,
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

# Choices for design UI: value = pbc_id (stable); label shows both names
pbc_choices <- function(registry, cycle_filter = NULL,
                          include_kinds = NULL, exclude_kinds = NULL) {
  df <- filter_pbc_registry(
    registry,
    cycle_filter = cycle_filter,
    include_kinds = include_kinds,
    exclude_kinds = exclude_kinds
  )
  if (!nrow(df)) return(character())
  kind_tag <- vapply(df$pbc_kind, function(k) {
    k <- trimws(as.character(k %||% ""))
    if (nzchar(k)) paste0("【", k, "】") else ""
  }, character(1))
  reviewed_lbl <- vapply(seq_len(nrow(df)), function(i) {
    format_pbc_reviewed_label(df$reviewed_name[i], df$pbc_kind[i])
  }, character(1))
  labels <- sprintf(
    "%s%s｜原名「%s」→ 檢視後「%s」%s",
    df$pbc_id,
    kind_tag,
    ifelse(nzchar(df$client_pbc_name), df$client_pbc_name, "—"),
    reviewed_lbl,
    ifelse(nzchar(df$cycle), paste0("〔", df$cycle, "〕"), "")
  )
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
  if (is.null(x)) return(character())
  if (length(x) > 1L) {
    vals <- trimws(as.character(x))
    return(unique(vals[nzchar(vals)]))
  }
  raw <- trimws(as.character(x %||% ""))
  if (!nzchar(raw)) return(character())
  vals <- trimws(unlist(strsplit(raw, "[;；|/、,，]+")))
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

# Import loose CSV with flexible column aliases
import_pbc_csv <- function(path, existing = empty_pbc_registry()) {
  raw <- utils::read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  names(raw) <- tolower(gsub("[\\s　]+", "_", names(raw)))
  alias <- list(
    pbc_id = c("pbc_id", "id", "編號"),
    client_pbc_name = c("client_pbc_name", "client_name", "pbc_name", "客戶原名", "原名", "pbc"),
    reviewed_name = c("reviewed_name", "new_name", "standard_name", "檢視後命名", "新命名", "iuc"),
    pbc_kind = c("pbc_kind", "kind", "證據類型", "pbc_type", "類型"),
    iuc_or_system = c("iuc_or_system", "iuc", "system"),
    cycle = c("cycle", "循環"),
    source = c("source", "來源"),
    notes = c("notes", "備註", "note")
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
      pbc_kind = pick(alias$pbc_kind)[i],
      iuc_or_system = pick(alias$iuc_or_system)[i],
      cycle = pick(alias$cycle)[i],
      source = pick(alias$source)[i],
      notes = pick(alias$notes)[i]
    ))
  }
  existing
}
