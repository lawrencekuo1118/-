#!/usr/bin/env Rscript
# Build committed first-batch library data from IT-cycle RCM xlsx（去識別）
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
source(file.path(root, "R", "00_constants.R"), local = TRUE)
source(file.path(root, "R", "assemble.R"), local = TRUE)
source(file.path(root, "R", "objective_activity.R"), local = TRUE)
source(file.path(root, "R", "rcm.R"), local = TRUE)
source(file.path(root, "R", "csa.R"), local = TRUE)
source(file.path(root, "R", "library.R"), local = TRUE)

xlsx <- file.path(root, "templates", "鯨鏈科技_資訊循環_RCM_v1_0820.xlsx")
out <- file.path(root, "data", "jinglian_it_rcm_batch.json")
if (!file.exists(xlsx)) stop("找不到資訊循環 RCM xlsx：", xlsx)

jl <- import_rcm_xlsx_as_library(
  xlsx, source = "rcm_import_batch", id_prefix = "JL",
  company_default = "",
  tags = c("RCM", "資訊循環", "首批")
)
if (!length(jl)) stop("匯入結果為空")
# Guard: no header-echo IDs
bad <- vapply(jl, function(x) grepl("控制編號|^JL-控制", x$library_id %||% ""), logical(1))
if (any(bad)) stop("仍含標題列雜訊：", paste(vapply(jl[bad], function(x) x$library_id, ""), collapse = ", "))

# 再保險：整批去識別＋清空現況／差異
jl <- lapply(jl, deidentify_library_item)

save_control_library(jl, out)
message(sprintf("Wrote %d de-identified IT-cycle RCM rows → %s", length(jl), out))
message("IDs sample: ", paste(vapply(jl[seq_len(min(5, length(jl)))], function(x) x$library_id, ""), collapse = ", "))
