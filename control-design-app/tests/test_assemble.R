# Extended tests: IUC split, PBC, RCM/CSA worksheets, gaps, library, Jinglian headers
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
source(file.path(root, "R", "objective_activity.R"), local = TRUE)
source(file.path(root, "R", "pbc_registry.R"), local = TRUE)
source(file.path(root, "R", "rcm_csa.R"), local = TRUE)
source(file.path(root, "R", "library.R"), local = TRUE)
source(file.path(root, "R", "cascade.R"), local = TRUE)

fail <- 0L
check <- function(cond, msg) {
  if (!isTRUE(cond)) {
    message("FAIL: ", msg)
    fail <<- fail + 1L
  } else message("OK: ", msg)
}

base <- list(
  company = "示範公司", cycle = "電腦化資訊系統循環",
  sub_process_id = "EC-101", sub_process = "存取管理",
  risk_factor = "不當權限", risk_name = "不當權限", risk_description = "離職未停權",
  risk_category = "營運面",
  risk_attr_financial = "[財務報導] 未授權交易", risk_attr_operations = "[營運] 權限不一致",
  risk_attr_compliance = "[法令遵循] 資安政策",
  romm_classification = ROMM_CLASS_CHOICES[[1]], significant_account = "多科目",
  assertions = "存在／發生 (Existence/Occurrence)",
  control_objective = "確保系統使用者權限與現職一致",
  control_activity = "每季覆核權限清冊並完成異動", frequency = "每季",
  responsible_unit = "資訊部", nature = "人工＋自動",
  approach = "偵測性控制", type = TYPE_CHOICES[[6]],
  inputs = "在職名單、權限清冊", review_steps = "產製清冊\n主管覆核\n完成異動",
  outputs = "簽回清冊、異動 log", investigation_threshold = "任何不當權限均須異動",
  dependent_controls = "", control_id = "", iuc_or_system = "使用者權限清冊",
  company_status = "每季產出權限清冊並由主管覆核後完成異動",
  key_control = "Y"
)

d1 <- modifyList(base, list(iuc_or_system = "使用者權限清冊"))
d2 <- modifyList(base, list(iuc_or_system = "AD 群組報表", control_activity = "覆核 AD 群組"))
check(length(split_controls_by_iuc(list(d1, d2))) == 2L, "IUC 分拆")

# RCM row = designed control; objective ≠ activity (Jinglian headers)
rcm <- controls_to_rcm(list(d1))
check(all(RCM_HEADERS %in% names(rcm)), "RCM 含鯨鏈標準標題列")
check(identical(as.character(rcm[["控制目標"]]), "確保系統使用者權限與現職一致"), "RCM 目標欄獨立")
check(identical(as.character(rcm[["控制活動"]]), "每季覆核權限清冊並完成異動"), "RCM 活動欄獨立")
check(!identical(as.character(rcm[["控制目標"]]), as.character(rcm[["控制活動"]])), "目標≠活動")
check(grepl("^EC-101-", as.character(rcm[["控制編號"]])), "資訊循環控制編號格式 EC-101-##")
check(identical(as.character(rcm[["控制類型"]]), "人工＋自動"), "控制類型＝人工/自動")
check(identical(as.character(rcm[["控制活動類型"]]), "偵測性控制"), "控制活動類型＝預防/偵測")
check(identical(as.character(rcm[["風險類別"]]), "營運面"), "風險類別映射")
check(grepl("^通過", as.character(rcm[["設計檢核"]])), "設計檢核通過")

# Type field 防呆：對調應失敗
swapped <- modifyList(d1, list(nature = "預防性控制", approach = "人工"))
tchk <- rcm_type_fields_check(swapped$nature, swapped$approach)
check(!isTRUE(tchk$ok), "類型欄對調時防呆失敗")

bad <- modifyList(d1, list(control_activity = d1$control_objective))
rcm_bad <- controls_to_rcm(list(bad))
check(grepl("待修", as.character(rcm_bad[["設計檢核"]])), "目標活動相同時設計檢核失敗")

# Extra OA cleanliness cases
chk1 <- rcm_objective_activity_check(
  "每季產出權限清冊並請主管簽核後完成異動",
  "每季產出權限清冊並請主管簽核後完成異動"
)
check(!isTRUE(chk1$ok), "步驟句不可同時當目標與活動")
chk2 <- rcm_objective_activity_check(
  "確保系統使用者權限與現職及職責分離原則一致",
  "權限管理員每季產出使用者權限清冊，由各單位主管覆核後回簽並完成異動"
)
check(isTRUE(chk2$ok), "標準 Why/How 應通過")
check(grepl("結果導向", chk2$objective_verdict), "目標判定為結果導向")
check(grepl("行動導向", chk2$activity_verdict), "活動判定為行動導向")
chk3 <- rcm_objective_activity_check("確保權限正確", "確保權限正確且完整")
check(!isTRUE(chk3$ok), "活動不可只是目標的延伸句")
sug <- suggest_objective_activity_split("確保收入於正確期間認列。會計每日比對出貨單與發票並呈主管簽核")
check(nzchar(sug$objective) && nzchar(sug$activity) && !identical(sug$objective, sug$activity),
      "拆分建議可分開 Why/How")

gaps <- detect_design_gaps(bad)
check(any(gaps$category == "控制缺失"), "缺漏分類含控制缺失")
check(any(grepl("相同", gaps$gap_item)), "偵測目標活動混用")

gaps2 <- detect_design_gaps(modifyList(d1, list(iuc_or_system = "", related_system = "", outputs = "", related_document = "")))
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

# Accumulative collect pipeline
ctrl_a <- modifyList(base, list(control_id = "EC-101-99", sub_process_id = "EC-101"))
res1 <- collect_controls_to_library(list(), list(ctrl_a), source = "test", quality_gate = TRUE)
check(res1$added == 1L, "品質通過的控制點可收集入庫")
check(!is.null(get_library_item(res1$library, "LIB-EC-101-99")), "穩定 ID 依控制編號累積")
# second save updates same id
ctrl_a2 <- modifyList(ctrl_a, list(company_status = "完善後現況描述"))
res2 <- collect_controls_to_library(res1$library, list(ctrl_a2), source = "test", quality_gate = TRUE)
check(res2$updated == 1L && res2$added == 0L, "同 ID 覆寫為累積更新")
check(length(res2$library) == 1L, "累積不產生重複筆")
bad_collect <- collect_controls_to_library(
  list(), list(modifyList(ctrl_a, list(control_activity = ctrl_a$control_objective))),
  quality_gate = TRUE
)
check(bad_collect$skipped == 1L && bad_collect$added == 0L, "品質門檻略過不合格控制點")
st <- library_stats(res2$library)
check(st$n == 1L, "library_stats 筆數")

# Jinglian RCM xlsx → library batch
xlsx <- file.path(root, "templates", "鯨鏈科技_資訊循環_RCM_v1_0820.xlsx")
jl <- list()
if (file.exists(xlsx)) {
  jl <- import_rcm_xlsx_as_library(xlsx)
  check(length(jl) >= 20, sprintf("鯨鏈 RCM 匯入至少 20 筆（實際 %d）", length(jl)))
  check(any(vapply(jl, function(x) grepl("^JL-EC-", x$library_id %||% ""), logical(1))),
        "鯨鏈匯入控制編號帶 JL-EC- 前綴")
  sample_ctrl <- jl[[1]]$control
  rcm_jl <- control_to_rcm_row(sample_ctrl, 1L)
  check(all(c("控制目標", "控制活動", "控制類型", "控制活動類型") %in% names(rcm_jl)),
        "鯨鏈列可映射回 RCM 標題")
  check(!identical(as.character(rcm_jl[["控制目標"]]), as.character(rcm_jl[["控制活動"]])) ||
          grepl("待修", as.character(rcm_jl[["設計檢核"]])),
        "鯨鏈列目標/活動分欄或設計檢核標示")
} else {
  message("SKIP: 鯨鏈 xlsx 不在 templates/")
}

# Cascade engine: cycle → sub → risk → objective → activity(single PD) → IUC
check(identical(normalize_single_activity_type("預防性控制"), "預防性控制"), "單一預防屬性")
check(identical(normalize_single_activity_type("偵測性 (Detective)"), "偵測性控制"), "單一偵測屬性")
check(!activity_type_ok("預防＋偵測"), "混合屬性不允許")
check(identical(next_rcm_control_id("EC-101", c("EC-101-01", "EC-101-03")), "EC-101-04"),
      "控制編號自動順編")
check(identical(next_rcm_control_id("EC-102", character()), "EC-102-01"), "空庫從 01 起編")

if (length(jl)) {
  rows <- library_controls_flat(jl, cycle = "電腦化資訊系統循環")
  check(length(rows) >= 20, "cascade flat 列來自鯨鏈庫")
  subs <- cascade_sub_process_choices(rows)
  check(length(subs) >= 1, "循環下有候選子作業")
  sub1 <- unname(subs)[[1]]
  rows2 <- filter_cascade_rows(rows, sub_key = sub1)
  risks <- cascade_risk_choices(rows2)
  check(length(risks) >= 1, "子作業下有風險候選")
  rk <- unname(risks)[[1]]
  det <- cascade_risk_detail(rows2, rk)
  check(nzchar(det$risk_description) || nzchar(det$risk_category) || length(det$attrs),
        "選風險後可查屬性／描述")
  rows3 <- filter_cascade_rows(rows2, risk_factor = rk)
  objs <- cascade_objective_choices(rows3)
  check(length(objs) >= 1, "風險對應控制目標候選")
  obj1 <- unname(objs)[[1]]
  rows4 <- filter_cascade_rows(rows3, objective = obj1)
  acts <- cascade_activity_choices(rows4)
  check(length(acts) >= 1, "目標對應控制活動候選")
  # every activity choice encodes a single PD attribute
  all_single <- all(vapply(unname(acts), function(k) {
    activity_type_ok(parse_activity_key(k)$approach)
  }, logical(1)))
  check(all_single, "每個活動候選僅一種預防/偵測")
  ak <- unname(acts)[[1]]
  rows5 <- filter_cascade_rows(rows4, activity_key_sel = ak)
  # IUC may be blank in Jinglian; custom path still allowed
  iucs <- cascade_iuc_choices(rows5)
  sel_incomplete <- list(cycle = "電腦化資訊系統循環")
  check(!cascade_selection_ready(sel_incomplete)$ready, "未完成引導不可就緒")
  akp <- parse_activity_key(ak)
  sp <- parse_sub_process_key(sub1)
  sel_full <- list(
    cycle = "電腦化資訊系統循環",
    sub_process_id = sp$id, sub_process = sp$name,
    risk_factor = rk, control_objective = obj1,
    control_activity = akp$activity, approach = akp$approach,
    iuc_or_system = if (length(iucs)) unname(iucs)[[1]] else "自訂IUC-測試"
  )
  check(cascade_selection_ready(sel_full)$ready, "引導完成則就緒")
  six_bad <- six_status_rules_check(list())
  check(!six_bad$ok, "空控制六大未齊")
  six_ok <- six_status_rules_check(list(
    nature = "人工", approach = "預防性控制", frequency = "持續",
    responsible_unit = "資訊部", iuc_or_system = "AD",
    control_activity = "劃分職責"
  ))
  check(six_ok$ok, "六大控制項目就緒")
  scaffold <- assemble_status_scaffold(modifyList(sel_full, list(
    nature = "人工", frequency = "持續", responsible_unit = "資訊部"
  )))
  check(grepl("六大控制項目", scaffold), "現況草稿含六大規則")
}

if (fail > 0) quit(status = 1)
message("All extended tests passed.")
