# IUC / PBC naming registry
# client_pbc_name = 客戶取得之原始 PBC 名稱
# reviewed_name   = 檢視後標準化命名（控制設計／現況撰寫時優先套用）

empty_pbc_registry <- function() {
  data.frame(
    pbc_id = character(),
    client_pbc_name = character(),
    reviewed_name = character(),
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
    iuc_or_system = trimws(as.character(row$iuc_or_system %||% reviewed)),
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

# Choices for design UI: value = pbc_id (stable); label shows both names
pbc_choices <- function(registry, cycle_filter = NULL) {
  if (!nrow(registry)) return(character())
  df <- registry
  if (!is.null(cycle_filter) && nzchar(cycle_filter)) {
    keep <- !nzchar(df$cycle) | df$cycle == cycle_filter
    df <- df[keep, , drop = FALSE]
  }
  if (!nrow(df)) return(character())
  labels <- sprintf(
    "%s｜原名「%s」→ 檢視後「%s」%s",
    df$pbc_id,
    ifelse(nzchar(df$client_pbc_name), df$client_pbc_name, "—"),
    df$reviewed_name,
    ifelse(nzchar(df$cycle), paste0("〔", df$cycle, "〕"), "")
  )
  stats::setNames(df$pbc_id, labels)
}

lookup_pbc <- function(registry, pbc_ids) {
  pbc_ids <- unique(as.character(pbc_ids %||% character()))
  pbc_ids <- pbc_ids[nzchar(pbc_ids)]
  if (!length(pbc_ids) || !nrow(registry)) {
    return(registry[0, , drop = FALSE])
  }
  registry[registry$pbc_id %in% pbc_ids, , drop = FALSE]
}

# Apply selected PBC ids → reviewed names for IUC field
apply_pbc_to_iuc <- function(registry, pbc_ids) {
  rows <- lookup_pbc(registry, pbc_ids)
  if (!nrow(rows)) return("")
  paste(unique(rows$reviewed_name), collapse = "；")
}

# Dual-name line for 控制現況 / Inputs documentation
format_pbc_status_lines <- function(registry, pbc_ids = NULL) {
  df <- if (is.null(pbc_ids)) registry else lookup_pbc(registry, pbc_ids)
  if (!nrow(df)) return(character())
  vapply(seq_len(nrow(df)), function(i) {
    sprintf(
      "%s：客戶取得 PBC「%s」／檢視後命名「%s」%s",
      df$pbc_id[i],
      ifelse(nzchar(df$client_pbc_name[i]), df$client_pbc_name[i], "（未填原名）"),
      df$reviewed_name[i],
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
      iuc_or_system = pick(alias$iuc_or_system)[i],
      cycle = pick(alias$cycle)[i],
      source = pick(alias$source)[i],
      notes = pick(alias$notes)[i]
    ))
  }
  existing
}
