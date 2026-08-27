#!/usr/bin/env Rscript
# Seed 資訊循環（電腦化資訊系統循環）PBC 清單＋規格說明 → pbc_registry
# Usage: Rscript data/seed_it_cycle_pbc.R

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

raw_path <- file.path(root, "data", "it_cycle_pbc_seed_raw.json")
if (!file.exists(raw_path)) stop("找不到種子來源：", raw_path)
raw <- jsonlite::fromJSON(raw_path, simplifyVector = FALSE)

cycle_name <- "電腦化資訊系統循環"
reg <- empty_pbc_registry()
for (i in seq_along(raw)) {
  it <- raw[[i]]
  nm <- trimws(as.character(it$name %||% ""))
  sp <- trimws(as.character(it$spec %||% ""))
  if (!nzchar(nm)) next
  reg <- upsert_pbc(reg, list(
    pbc_id = sprintf("PBC-EC-%03d", i),
    client_pbc_name = nm,
    reviewed_name = nm,
    pbc_spec = sp,
    cycle = cycle_name,
    source = "it_cycle_pbc_seed",
    iuc_or_system = nm
  ))
}

out_csv <- file.path(root, "data", "pbc_registry.csv")
out_json <- file.path(root, "data", "pbc_registry.json")
existing <- load_pbc_registry(out_csv, out_json)
for (i in seq_len(nrow(reg))) {
  existing <- upsert_pbc(existing, as.list(reg[i, , drop = FALSE]))
}
save_pbc_registry(existing, out_csv, out_json)
message(sprintf(
  "Seeded IT-cycle PBC: %d rows (registry total %d) → %s",
  nrow(reg), nrow(existing), out_csv
))
