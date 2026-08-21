# Named session drafts (save / list / load / delete)

drafts_dir <- function(data_dir) {
  d <- file.path(data_dir, "drafts")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  d
}

sanitize_draft_name <- function(name) {
  name <- trimws(as.character(name %||% ""))
  if (!nzchar(name)) name <- format(Sys.time(), "草稿_%Y%m%d_%H%M%S")
  name <- gsub("[\\\\/:*?\"<>|]+", "_", name)
  name <- gsub("\\s+", "_", name)
  name
}

draft_file_path <- function(data_dir, name) {
  file.path(drafts_dir(data_dir), paste0(sanitize_draft_name(name), ".json"))
}

list_saved_drafts <- function(data_dir) {
  files <- list.files(drafts_dir(data_dir), pattern = "\\.json$", full.names = TRUE)
  # also include legacy session_draft.json
  legacy <- file.path(data_dir, "session_draft.json")
  if (file.exists(legacy)) files <- c(files, legacy)
  if (!length(files)) {
    return(data.frame(name = character(), path = character(), saved_at = character(),
                      n_drafts = integer(), n_controls = integer(),
                      stringsAsFactors = FALSE))
  }
  rows <- lapply(files, function(p) {
    meta <- tryCatch(jsonlite::read_json(p, simplifyVector = FALSE), error = function(e) NULL)
    nm <- if (basename(p) == "session_draft.json") "（自動／預設）" else sub("\\.json$", "", basename(p))
    data.frame(
      name = nm,
      path = p,
      saved_at = as.character(meta$saved_at %||% file.info(p)$mtime),
      n_drafts = length(meta$drafts %||% list()),
      n_controls = length(meta$controls %||% list()),
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, rows)
  df[order(df$saved_at, decreasing = TRUE), , drop = FALSE]
}

build_draft_payload <- function(drafts, controls, pbc, interview_elements, csa_elements,
                                form_snapshot = NULL, name = NULL) {
  list(
    name = name %||% "",
    drafts = drafts,
    controls = controls,
    pbc = pbc,
    interview_elements = interview_elements,
    csa_elements = csa_elements,
    form_snapshot = form_snapshot,
    saved_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
}

save_named_draft <- function(data_dir, name, payload) {
  path <- draft_file_path(data_dir, name)
  payload$name <- sanitize_draft_name(name)
  jsonlite::write_json(payload, path, auto_unbox = TRUE, pretty = TRUE, force = TRUE)
  # keep legacy pointer for quick resume
  jsonlite::write_json(payload, file.path(data_dir, "session_draft.json"),
                       auto_unbox = TRUE, pretty = TRUE, force = TRUE)
  path
}

load_draft_payload <- function(path) {
  if (!file.exists(path)) stop("草稿不存在")
  jsonlite::read_json(path, simplifyVector = FALSE)
}

delete_named_draft <- function(data_dir, name) {
  path <- draft_file_path(data_dir, name)
  if (file.exists(path)) file.remove(path)
  invisible(TRUE)
}
