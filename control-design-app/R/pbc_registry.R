# IUC / PBC naming registry
# Tracks client-provided PBC names vs reviewed/standardized names for reuse.

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
  if (!nzchar(id)) {
    id <- sprintf("PBC-%03d", nrow(registry) + 1L)
  }
  client <- trimws(as.character(row$client_pbc_name %||% ""))
  reviewed <- trimws(as.character(row$reviewed_name %||% ""))
  if (!nzchar(client) && !nzchar(reviewed)) {
    stop("client_pbc_name 或 reviewed_name 至少填一項")
  }
  if (!nzchar(reviewed)) reviewed <- client
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

pbc_choices <- function(registry) {
  if (!nrow(registry)) return(character())
  labels <- sprintf(
    "%s｜客戶原名：%s → 檢視後：%s",
    registry$pbc_id,
    ifelse(nzchar(registry$client_pbc_name), registry$client_pbc_name, "—"),
    registry$reviewed_name
  )
  stats::setNames(registry$reviewed_name, labels)
}

apply_pbc_to_iuc <- function(registry, selected_names) {
  selected_names <- unique(trimws(as.character(selected_names %||% character())))
  selected_names <- selected_names[nzchar(selected_names)]
  if (!length(selected_names)) return("")
  paste(selected_names, collapse = "；")
}
