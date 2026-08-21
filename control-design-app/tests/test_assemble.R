# Extended tests: IUC split, PBC, RCM/CSA worksheets, gaps, library
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
  company = "示範公司", cycle = "電腦化資訊系統循環", sub_process = "存取管理",
  risk_name = "不當權限", risk_description = "離職未停權",
  risk_attr_financial = "[財務報導] 未授權交易", risk_attr_operations = "[營運] 權限不一致",
  risk_attr_compliance = "[法令遵循] 資安政策",
  romm_classification = ROMM_CLASS_CHOICES[[1]], significant_account = "多科目",
  assertions = "存在／發生 (Existence/Occurrence)",
  control_objective = "確保系統使用者權限與現職一致",
  control_activity = "每季覆核權限清冊並完成異動", frequency = "每季",
  responsible_unit = "資訊部", nature = NATURE_CHOICES[[3]],
  approach = APPROACH_CHOICES[[2]], type = TYPE_CHOICES[[6]],
  inputs = "在職名單、權限清冊", review_steps = "產製清冊\n主管覆核\n完成異動",
  outputs = "簽回清冊、異動 log", investigation_threshold = "任何不當權限均須異動",
  dependent_controls = "", control_id = "", iuc_or_system = "使用者權限清冊",
  key_control = "Y"
)

d1 <- modifyList(base, list(iuc_or_system = "使用者權限清冊"))
d2 <- modifyList(base, list(iuc_or_system = "AD 群組報表", control_activity = "覆核 AD 群組"))
check(length(split_controls_by_iuc(list(d1, d2))) == 2L, "IUC 分拆")

# RCM row = designed control; objective ≠ activity
rcm <- controls_to_rcm(list(d1))
check(all(RCM_HEADERS %in% names(rcm)), "RCM 含標準標題列")
check(identical(rcm[["控制目標"]], "確保系統使用者權限與現職一致"), "RCM 目標欄獨立")
check(identical(rcm[["控制活動"]], "每季覆核權限清冊並完成異動"), "RCM 活動欄獨立")
check(!identical(rcm[["控制目標"]], rcm[["控制活動"]]), "目標≠活動")
check(grepl("^IT-C-", rcm[["控制點編號"]]), "資訊循環控制點編號格式")
check(identical(rcm[["設計檢核"]], "通過"), "設計檢核通過")

bad <- modifyList(d1, list(control_activity = d1$control_objective))
rcm_bad <- controls_to_rcm(list(bad))
check(grepl("待修", rcm_bad[["設計檢核"]]), "目標活動相同時設計檢核失敗")

gaps <- detect_design_gaps(bad)
check(any(gaps$category == "控制缺失"), "缺漏分類含控制缺失")
check(any(grepl("相同", gaps$gap_item)), "偵測目標活動混用")

gaps2 <- detect_design_gaps(modifyList(d1, list(iuc_or_system = "", outputs = "")))
check(any(gaps2$category == "缺文件"), "缺 IUC／產出歸類為缺文件")

ready <- is_rcm_row_ready(d1)
check(isTRUE(ready$ready), "完整控制點可視為 RCM 列就緒")

iv <- control_to_interview(d1, elements = c("control_objective", "iuc"))
check(nrow(iv) == 2L, "訪談可依元素過濾")

csa <- control_to_csa(d1, elements = c("steps", "iuc", "outputs"))
check(all(c("測試程序", "所需文件_PBC", "預期結果") %in% names(csa)), "CSA 含測試步驟設計欄位")
check(nrow(csa) >= 3, "CSA 依元素產製多個測試步驟")
check(any(csa[["元素"]] == "IUC／制度"), "CSA 含 IUC 測試步驟")

# PBC
reg <- empty_pbc_registry()
reg <- upsert_pbc(reg, list(client_pbc_name = "user_access.xlsx", reviewed_name = "使用者權限清冊"))
check(identical(apply_pbc_to_iuc(reg, reg$pbc_id[1]), "使用者權限清冊"), "PBC 套用")

# Library IT seeds + import
lib <- seed_control_library()
check(!is.null(get_library_item(lib, "LIB-IT-ACCESS-01")), "資訊循環範本種子")
tmp_csv <- tempfile(fileext = ".csv")
utils::write.csv(library_to_flat_df(lib), tmp_csv, row.names = FALSE, fileEncoding = "UTF-8")
imported <- import_control_library_file(tmp_csv, existing = list(), overwrite = TRUE)
check(length(imported) >= 4, "CSV 匯入含資訊循環範本")

if (fail > 0) quit(status = 1)
message("All extended tests passed.")
