# Headless tests for IUC split + paragraph assembly
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg)) {
  dirname(dirname(normalizePath(sub("^--file=", "", file_arg))))
} else {
  normalizePath(getwd())
}
if (!file.exists(file.path(root, "R", "assemble.R"))) {
  # running from repo root
  alt <- file.path(root, "control-design-app")
  if (file.exists(file.path(alt, "R", "assemble.R"))) root <- alt
}
source(file.path(root, "R", "constants.R"), local = TRUE)
source(file.path(root, "R", "assemble.R"), local = TRUE)

fail <- 0L
check <- function(cond, msg) {
  if (!isTRUE(cond)) {
    message("FAIL: ", msg)
    fail <<- fail + 1L
  } else {
    message("OK: ", msg)
  }
}

base <- list(
  company = "示範公司",
  cycle = "銷售及收款循環",
  risk_name = "收入截止錯誤",
  risk_description = "接近期末之出貨可能提前認列收入",
  risk_attr_financial = "[財務報導] 營業收入完整性／截止",
  risk_attr_operations = "[營運] 出貨與開立發票不同步",
  risk_attr_compliance = "[法令遵循] 收入認列政策遵循",
  romm_classification = ROMM_CLASS_CHOICES[[1]],
  significant_account = "營業收入",
  assertions = "完整性 (Completeness)；截止 (Cutoff)",
  control_objective = "確保收入於適當會計期間認列",
  control_activity = "核對出貨單與發票日期",
  frequency = "每日",
  responsible_unit = "財務部",
  nature = NATURE_CHOICES[[1]],
  approach = APPROACH_CHOICES[[2]],
  type = TYPE_CHOICES[[6]],
  inputs = "出貨單、銷貨發票",
  review_steps = "比對出貨日與發票日\n差異逾1日列入追蹤",
  outputs = "截止測試調節表與主管簽核",
  investigation_threshold = "差異逾1日",
  dependent_controls = "",
  control_id = "CD-001"
)

d1 <- modifyList(base, list(iuc_or_system = "銷貨日報表", control_activity = "覆核銷貨日報表截止"))
d2 <- modifyList(base, list(iuc_or_system = "ERP AR 帳齡報表", control_activity = "覆核 AR 帳齡異常"))
d3 <- modifyList(base, list(iuc_or_system = "銷貨日報表", control_activity = "抽查出貨單"))

split_pts <- split_controls_by_iuc(list(d1, d2, d3))
check(length(split_pts) == 2L, "不同 IUC 應分拆為 2 個控制點（相同 IUC 合併）")

keys <- sort(vapply(split_pts, `[[`, "", "iuc_key"))
check(any(grepl("銷貨日報表", keys)), "包含銷貨日報表 IUC key")
check(any(grepl("erp ar", keys)), "包含 ERP AR IUC key")

# Same IUC variants with punctuation should collapse
d4 <- modifyList(base, list(iuc_or_system = "銷貨日報表； 銷貨日報表"))
d5 <- modifyList(base, list(iuc_or_system = "銷貨日報表"))
check(length(split_controls_by_iuc(list(d4, d5))) == 1L, "相同 IUC 正規化後合併為 1")

para <- assemble_control_paragraph(modifyList(d1, list(iuc_or_system = "銷貨日報表")))
check(grepl("銷售及收款循環", para), "段落含九大循環")
check(grepl("風險三大屬性", para), "段落含風險三大屬性")
check(grepl("控制目標", para), "段落含控制目標")
check(grepl("銷貨日報表", para), "段落含 IUC")
check(grepl("Inputs", para) || grepl("投入", para), "段落含 Inputs")
check(grepl("Outputs", para) || grepl("產出", para), "段落含 Outputs")

v <- validate_control_design(modifyList(d1, list(iuc_or_system = "銷貨日報表")))
check(isTRUE(v$ok), "完整草稿通過驗證")

v2 <- validate_control_design(list())
check(!isTRUE(v2$ok) && length(v2$missing) >= 10, "空物件應回報多項缺漏")

if (fail > 0) {
  quit(status = 1)
}
message("All tests passed.")
