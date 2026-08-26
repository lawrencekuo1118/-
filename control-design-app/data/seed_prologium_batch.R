#!/usr/bin/env Rscript
# Build committed library batch from 輝能科技全循環 RCM xlsx
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg)) {
  dirname(dirname(normalizePath(sub("^--file=", "", file_arg))))
} else {
  normalizePath(getwd())
}
if (!file.exists(file.path(root, "R", "library.R"))) {
  alt <- file.path(root, "control-design-app")
  if (file.exists(file.path(alt, "R", "library.R"))) root <- alt
}
source(file.path(root, "R", "constants.R"), local = TRUE)
source(file.path(root, "R", "assemble.R"), local = TRUE)
source(file.path(root, "R", "objective_activity.R"), local = TRUE)
source(file.path(root, "R", "rcm_csa.R"), local = TRUE)
source(file.path(root, "R", "library.R"), local = TRUE)

tpl_dir <- file.path(root, "templates")
out <- file.path(root, "data", "prologium_rcm_batch.json")

# Prefer committed templates; fall back to Cursor uploads mapping
upload_dir <- "/home/ubuntu/.cursor/projects/workspace/uploads"
# sheet-name → preferred display filename
files <- list.files(tpl_dir, pattern = "^輝能科技_.*RCM.*\\.xlsx$", full.names = TRUE)
if (!length(files) && dir.exists(upload_dir)) {
  ups <- list.files(upload_dir, pattern = "RCM__.*\\.xlsx$", full.names = TRUE)
  ups <- ups[!grepl("Form_4120|0820", basename(ups))]
  files <- ups
}

if (!length(files)) stop("找不到輝能科技 RCM xlsx（templates/ 或 uploads/）")

all_items <- list()
for (p in files) {
  items <- tryCatch(
    import_rcm_xlsx_as_library(
      p,
      source = "prologium_rcm",
      id_prefix = "PL",
      company_default = "輝能科技",
      tags = c("輝能科技", "RCM")
    ),
    error = function(e) {
      message("SKIP ", basename(p), ": ", conditionMessage(e))
      list()
    }
  )
  message(sprintf("%s → %d 列", basename(p), length(items)))
  all_items <- c(all_items, items)
}

if (!length(all_items)) stop("匯入結果為空")
# de-dupe by library_id (last wins)
ids <- vapply(all_items, function(x) x$library_id %||% "", character(1))
keep <- !duplicated(ids, fromLast = TRUE)
all_items <- all_items[keep]

bad <- vapply(all_items, function(x) {
  grepl("控制編號|^PL-控制", x$library_id %||% "") ||
    !nzchar(x$control$control_objective %||% "")
}, logical(1))
if (any(bad)) {
  message("警告：略過 ", sum(bad), " 筆疑似標題／空目標列")
  all_items <- all_items[!bad]
}

save_control_library(all_items, out)
cycles <- sort(unique(vapply(all_items, function(x) x$cycle %||% "", "")))
message(sprintf("Wrote %d ProLogium RCM rows → %s", length(all_items), out))
message("Cycles: ", paste(cycles, collapse = "、"))
message("IDs sample: ", paste(vapply(all_items[seq_len(min(5, length(all_items)))], function(x) x$library_id, ""), collapse = ", "))
