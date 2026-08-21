# Extended tests: IUC split, PBC registry, RCM objective/activity split, gaps, CSA
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg)) {
  dirname(dirname(normalizePath(sub("^--file=", "", file_arg))))
} else {
  normalizePath(getwd())
}
if (!file.exists(file.path(root, "R", "assemble.R"))) {
  alt <- file.path(root, "control-design-app")
  if (file.exists(file.path(alt, "R", "assemble.R"))) root <- alt
}
source(file.path(root, "R", "constants.R"), local = TRUE)
source(file.path(root, "R", "assemble.R"), local = TRUE)
source(file.path(root, "R", "pbc_registry.R"), local = TRUE)
source(file.path(root, "R", "rcm_csa.R"), local = TRUE)
source(file.path(root, "R", "library.R"), local = TRUE)

fail <- 0L
check <- function(cond, msg) {
  if (!isTRUE(cond)) {
    message("FAIL: ", msg)
    fail <<- fail + 1L
  } else message("OK: ", msg)
}

base <- list(
  company = "示範公司", cycle = "銷售及收款循環", risk_name = "收入截止錯誤",
  risk_description = "期末出貨認列時點", risk_attr_financial = "[財務報導] 截止",
  risk_attr_operations = "[營運] 出貨開票", risk_attr_compliance = "[法令遵循] 政策",
  romm_classification = ROMM_CLASS_CHOICES[[1]], significant_account = "營業收入",
  assertions = "截止 (Cutoff)", control_objective = "確保收入於適當期間認列",
  control_activity = "核對出貨單與發票日期並調節", frequency = "每日",
  responsible_unit = "財務部", nature = NATURE_CHOICES[[1]],
  approach = APPROACH_CHOICES[[2]], type = TYPE_CHOICES[[6]],
  inputs = "出貨單", review_steps = "比對日期\n列追蹤", outputs = "調節表",
  investigation_threshold = "逾1日", dependent_controls = "", control_id = "CD-001"
)

d1 <- modifyList(base, list(iuc_or_system = "銷貨日報表"))
d2 <- modifyList(base, list(iuc_or_system = "ERP AR 帳齡報表", control_activity = "覆核帳齡"))
check(length(split_controls_by_iuc(list(d1, d2))) == 2L, "IUC 分拆")

reg <- empty_pbc_registry()
reg <- upsert_pbc(reg, list(client_pbc_name = "Sales Daily raw.xlsx", reviewed_name = "銷貨日報表", cycle = "銷售及收款循環"))
reg <- upsert_pbc(reg, list(pbc_id = reg$pbc_id[1], client_pbc_name = "Sales Daily raw.xlsx", reviewed_name = "銷貨日報表_v2"))
check(nrow(reg) == 1L && identical(reg$reviewed_name[1], "銷貨日報表_v2"), "PBC upsert by id")
ch <- pbc_choices(reg)
check(length(ch) == 1L && grepl("原名", names(ch)[1]) && identical(unname(ch[[1]]), reg$pbc_id[1]),
      "PBC choices: label 含原名、value 為 pbc_id")
check(identical(apply_pbc_to_iuc(reg, reg$pbc_id[1]), "銷貨日報表_v2"), "套用後 IUC 用檢視後命名")
lines <- format_pbc_status_lines(reg, reg$pbc_id[1])
check(grepl("Sales Daily raw.xlsx", lines) && grepl("銷貨日報表_v2", lines), "現況對照含原名與新名")

# upsert by same client name without id
reg2 <- upsert_pbc(reg, list(client_pbc_name = "Sales Daily raw.xlsx", reviewed_name = "銷貨日報表_v3"))
check(nrow(reg2) == 1L && identical(reg2$reviewed_name[1], "銷貨日報表_v3"), "同客戶原名可更新對照")

tmp_csv <- tempfile(fileext = ".csv")
save_pbc_registry(reg2, tmp_csv)
loaded <- load_pbc_registry(tmp_csv)
check(nrow(loaded) == 1L && identical(loaded$client_pbc_name, "Sales Daily raw.xlsx"), "CSV 持久化往返")

rcm <- controls_to_rcm(list(modifyList(d1, list(control_id = "CP-1"))))
check(identical(rcm$control_objective, "確保收入於適當期間認列"), "RCM 目標欄位獨立")
check(identical(rcm$control_activity, "核對出貨單與發票日期並調節"), "RCM 活動欄位獨立")
check(!identical(rcm$control_objective, rcm$control_activity), "目標≠活動")

same <- modifyList(d1, list(control_activity = d1$control_objective, iuc_or_system = "銷貨日報表"))
gaps <- detect_design_gaps(same)
check(any(grepl("目標與控制活動文字相同", gaps$gap_item)), "偵測目標活動混用")

iv <- control_to_interview(d1)
check(nrow(iv) >= 5 && any(grepl("控制目標", iv$element)), "訪談題含控制目標元素")
csa <- control_to_csa(d1)
check(nrow(csa) >= 2 && "control_objective" %in% names(csa), "CSA 含目標與步驟")

lib <- seed_control_library()
check(length(lib) >= 2 && !is.null(get_library_item(lib, "LIB-REV-CUTOFF-01")), "範本庫可取用")

if (fail > 0) quit(status = 1)
message("All extended tests passed.")
