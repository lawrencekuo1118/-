#!/usr/bin/env Rscript
# Seed 薪工循環（人力資源）PBC 清單＋規格說明 → pbc_registry
# Usage: Rscript data/seed_py_cycle_pbc.R

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg)) {
  dirname(dirname(normalizePath(sub("^--file=", "", file_arg))))
} else {
  normalizePath(getwd())
}
if (!file.exists(file.path(root, "R", "pbc_registry.R"))) {
  alt <- file.path(root, "control-design-app")
  if (file.exists(file.path(alt, "R", "pbc_registry.R"))) root <- alt
}

`%||%` <- function(x, y) if (is.null(x)) y else x
source(file.path(root, "R", "00_constants.R"), local = TRUE)
source(file.path(root, "R", "pbc_registry.R"), local = TRUE)

raw_path <- file.path(root, "data", "py_cycle_pbc_seed_raw.json")
if (!file.exists(raw_path)) stop("找不到種子來源：", raw_path)
raw <- jsonlite::fromJSON(raw_path, simplifyVector = FALSE)

cycle_name <- "薪工循環"
cc <- cycle_code_for(cycle_name)
if (!nzchar(cc)) cc <- "PY"
reg <- empty_pbc_registry()
for (i in seq_along(raw)) {
  it <- raw[[i]]
  nm <- trimws(as.character(it$name %||% ""))
  sp <- trimws(as.character(it$spec %||% ""))
  if (!nzchar(nm)) next
  reg <- upsert_pbc(reg, list(
    pbc_id = sprintf("PBC-%s-%03d", cc, i),
    client_pbc_name = nm,
    reviewed_name = nm,
    pbc_spec = sp,
    cycle = cycle_name,
    source = "py_cycle_pbc_seed",
    iuc_or_system = nm
  ))
}

out_csv <- file.path(root, "data", "pbc_registry.csv")
out_json <- file.path(root, "data", "pbc_registry.json")
existing <- load_pbc_registry(out_csv, out_json)
if (nrow(existing)) {
  drop <- existing$source == "py_cycle_pbc_seed" |
    grepl("^PBC-PY-", existing$pbc_id)
  if (any(drop)) existing <- existing[!drop, , drop = FALSE]
}
for (i in seq_len(nrow(reg))) {
  existing <- upsert_pbc(existing, as.list(reg[i, , drop = FALSE]))
}
existing <- enrich_related_pbc_from_specs(existing)
save_pbc_registry(existing, out_csv, out_json)
n_linked <- sum(vapply(existing$related_pbc_ids, function(x) {
  length(parse_pbc_id_values(x)) > 0L
}, logical(1)))
n_py <- sum(existing$cycle == cycle_name & existing$source == "py_cycle_pbc_seed")
message(sprintf(
  "Seeded PY-cycle PBC: %d rows (PY seed %d, registry %d, linked %d) → %s",
  nrow(reg), n_py, nrow(existing), n_linked, out_csv
))
