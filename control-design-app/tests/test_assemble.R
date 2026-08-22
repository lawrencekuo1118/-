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
source(file.path(root, "R", "parameter_store.R"), local = TRUE)

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
  risk_attr_financial = "",
  risk_attr_operations = "[營運] 權限不一致",
  risk_attr_compliance = "",
  romm_classification = ROMM_CLASS_CHOICES[[1]], significant_account = "",
  assertions = "完整性 (Completeness)",
  control_objective = "確保系統使用者權限與現職一致",
  control_activity = "每季覆核權限清冊並完成異動", frequency = "每季",
  responsible_unit = "資訊部", nature = "人工",
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
check(identical(as.character(rcm[["控制類型"]]), "人工"), "控制類型＝人工/自動")
check(!nzchar(normalize_control_type_manual_auto("人工＋自動")), "混合控制類型不允許")
check(!nzchar(normalize_control_type_manual_auto("人工＋自動化混合")), "混合控制類型不允許")
check(identical(resolve_control_frequency("自動", "每季"), "持續"), "自動控制頻率＝持續")
check(identical(resolve_control_frequency("人工", "每季"), "每季"), "人工控制頻率保留")
auto_ctrl <- modifyList(d1, list(nature = "自動", frequency = "每季"))
rcm_auto <- controls_to_rcm(list(auto_ctrl))
check(identical(as.character(rcm_auto[["控制頻率"]]), "持續"), "RCM 自動控制頻率＝持續")
fin_auto <- finalize_control_as_rcm_row(auto_ctrl)
check(isTRUE(fin_auto$ok), "自動控制定稿成功")
check(identical(fin_auto$control$frequency, "持續"), "定稿後頻率強制持續")
check(identical(as.character(rcm[["控制活動類型"]]), "偵測性控制"), "控制活動類型＝預防/偵測")
check(identical(as.character(rcm[["風險類別"]]), "營運面"), "風險類別映射")
check(grepl("^通過", as.character(rcm[["設計檢核"]])), "設計檢核通過")

# 風險屬性三擇一
multi_attr <- modifyList(d1, list(
  risk_attr_financial = "[財務報導] A",
  risk_attr_operations = "[營運] B",
  risk_attr_compliance = "[法令遵循] C"
))
check(count_filled_risk_attrs(multi_attr) == 3L, "複數屬性可計數")
gaps_multi <- detect_design_gaps(multi_attr)
check(any(grepl("三擇一|不可複選", gaps_multi$gap_item)), "複數屬性設計缺漏")
check(!isTRUE(is_rcm_row_ready(multi_attr)$ready), "複數屬性不可就緒")
one <- enforce_single_risk_attr(multi_attr, kind = "operations", detail = "僅營運")
check(count_filled_risk_attrs(one) == 1L, "強制三擇一後僅一項")
check(identical(one$risk_category, "營運面"), "屬性對應風險類別")
check(!nzchar(strip_attr_label(one$risk_attr_financial)), "清空財務報導屬性")
check(!nzchar(strip_attr_label(one$risk_attr_compliance)), "清空法令遵循屬性")
fin_one <- finalize_control_as_rcm_row(multi_attr)
check(isTRUE(fin_one$ok), "定稿強制三擇一後可成功")
check(count_filled_risk_attrs(fin_one$control) == 1L, "定稿後僅保留一種屬性")

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
check(any(gaps2$severity == "高" & grepl("IUC|相關系統|必填", gaps2$gap_item)), "缺 IUC 為必填高嚴重度")
check(any(gaps2$severity == "低" & grepl("產出|相關文件", gaps2$gap_item)), "缺產出改為選填低嚴重度")

# 設計必填欄位
req_ok <- design_required_check(d1)
check(isTRUE(req_ok$ok), "完整控制點必填齊全")
req_bad <- design_required_check(modifyList(d1, list(
  frequency = "", responsible_unit = "", nature = "", risk_category = ""
)))
check(!isTRUE(req_bad$ok), "缺頻率／單位／類型／類別＝必填未齊")
check(any(grepl("控制頻率", req_bad$missing)), "必填清單含控制頻率")
check(any(grepl("流程負責單位", req_bad$missing)), "必填清單含負責單位")
check(any(grepl("風險類別", req_bad$missing)), "必填清單含風險類別")
fin_req <- finalize_control_as_rcm_row(modifyList(d1, list(responsible_unit = "")))
check(!isTRUE(fin_req$ok) && grepl("必填", fin_req$msg), "缺負責單位不可定稿")

# 報導面會計科目必填；其他類別不可填
rep_ok <- design_required_check(modifyList(d1, list(
  risk_category = "報導面", significant_account = "應收帳款"
)))
check(isTRUE(rep_ok$ok), "報導面＋科目＝必填通過")
rep_bad <- design_required_check(modifyList(d1, list(
  risk_category = "報導面", significant_account = ""
)))
check(!isTRUE(rep_bad$ok) && any(grepl("會計科目", rep_bad$missing)), "報導面缺科目不可過")
ops_lock <- design_required_check(modifyList(d1, list(
  risk_category = "營運面", significant_account = "存貨"
)))
check(!isTRUE(ops_lock$ok) && any(grepl("不可填|清空", ops_lock$missing)), "營運面填科目應擋下")
ops_ok <- design_required_check(modifyList(d1, list(
  risk_category = "營運面", significant_account = ""
)))
check(isTRUE(ops_ok$ok), "營運面空白科目可過")
fin_ops <- finalize_control_as_rcm_row(modifyList(d1, list(
  risk_category = "營運面", significant_account = "存貨"
)), existing_ids = character())
check(isTRUE(fin_ops$ok) && identical(fin_ops$control$significant_account, ""),
      "營運面定稿自動清空誤填科目")
fin_clear <- finalize_control_as_rcm_row(modifyList(d1, list(
  risk_category = "營運面", significant_account = ""
)), existing_ids = character())
check(isTRUE(fin_clear$ok) && identical(fin_clear$control$significant_account, ""),
      "營運面定稿科目清空")
fin_rep <- finalize_control_as_rcm_row(modifyList(d1, list(
  risk_category = "報導面", significant_account = ""
)), existing_ids = character())
check(!isTRUE(fin_rep$ok) && grepl("會計科目", fin_rep$msg), "報導面缺科目不可定稿")

# 遵循面相關法令必填；其他類別不可填
comp_ok <- design_required_check(modifyList(d1, list(
  risk_category = "遵循面", related_law = "證券交易法", significant_account = "",
  assertions = ""
)))
check(isTRUE(comp_ok$ok), "遵循面＋法令＝必填通過")
comp_bad <- design_required_check(modifyList(d1, list(
  risk_category = "遵循面", related_law = "", significant_account = "",
  assertions = ""
)))
check(!isTRUE(comp_bad$ok) && any(grepl("相關法令", comp_bad$missing)), "遵循面缺法令不可過")
comp_as_bad <- design_required_check(modifyList(d1, list(
  risk_category = "遵循面", related_law = "證券交易法", significant_account = "",
  assertions = "完整性 (Completeness)"
)))
check(!isTRUE(comp_as_bad$ok) && any(grepl("聲明", comp_as_bad$missing)),
      "遵循面填聲明應擋下")
comp_lock <- design_required_check(modifyList(d1, list(
  risk_category = "營運面", related_law = "SOX", significant_account = ""
)))
check(!isTRUE(comp_lock$ok) && any(grepl("法令", comp_lock$missing)), "營運面填法令應擋下")
check(length(RELATED_LAW_CHOICES_TW) >= 15 && length(RELATED_LAW_CHOICES_US) >= 10,
      sprintf("相關法令預設含台美（台%d／美%d）", length(RELATED_LAW_CHOICES_TW), length(RELATED_LAW_CHOICES_US)))
check(any(grepl("證券交易法", RELATED_LAW_CHOICES_TW)) && any(grepl("Sarbanes-Oxley", RELATED_LAW_CHOICES_US)),
      "預設含證交法與 SOX")
pcat <- parameter_catalog(list(), list(), list(), presets = list("相關法令" = unname(RELATED_LAW_CHOICES)))
check(nrow(pcat) >= 20 && any(pcat$參數 == "相關法令"), "參數庫可查詢預設法令")
seeded <- seed_control_library(TRUE)
pcat2 <- parameter_catalog(seeded, list(), list())
check(any(pcat2$參數 == "子作業編號") && any(grepl("EC-101", pcat2$選項值)), "參數庫含資訊循環子作業")
check(any(pcat2$參數 == "控制目標"), "參數庫含控制目標選項")
tmp_ps <- tempfile(fileext = ".json")
save_parameter_store(pcat2, tmp_ps)
reloaded <- load_parameter_store(tmp_ps)
check(nrow(reloaded) == nrow(pcat2), "參數資料庫 JSON 可持久化")
check(nrow(filter_parameter_store(reloaded, param = "風險類別")) >= 1, "參數庫可依類型篩選")

# 空表單不可因 gaps 崩潰（曾導致引導選單無法更新）
empty_draft <- list(cycle = "電腦化資訊系統循環", frequency = "每季")
gaps_empty <- tryCatch(detect_design_gaps(empty_draft), error = function(e) e)
check(is.data.frame(gaps_empty), "空草稿 detect_design_gaps 不崩潰")
check(nrow(gaps_empty) > 0, "空草稿仍回報必填缺漏")

ready <- is_rcm_row_ready(d1)
check(isTRUE(ready$ready), "完整控制點可視為 RCM 列就緒")

# 設計完成＝RCM 一列（1:1）
fin <- finalize_control_as_rcm_row(d1, existing_ids = character(), seq_hint = 1L)
check(isTRUE(fin$ok), "finalize 成功＝寫入 RCM 一列")
check(!is.null(fin$rcm_row), "finalize 產出 rcm_row")
check(identical(fin$control$control_id, as.character(fin$rcm_row[["控制編號"]])),
      "控制點編號＝RCM 控制編號")
check(nrow(controls_to_rcm(list(fin$control))) == 1L, "一控制點對應一 RCM 列")
parity <- assert_design_rcm_parity(list(fin$control, finalize_control_as_rcm_row(
  modifyList(d1, list(control_id = "", iuc_or_system = "另一IUC",
                      control_activity = "每月覆核另一清冊")),
  existing_ids = fin$control$control_id, seq_hint = 2L
)$control))
# rebuild properly
fin2 <- finalize_control_as_rcm_row(
  modifyList(d1, list(control_id = "", iuc_or_system = "另一IUC",
                      control_activity = "資訊每月覆核另一清冊並簽核")),
  existing_ids = c(fin$control$control_id), seq_hint = 2L
)
check(isTRUE(fin2$ok), "第二點亦可定稿")
parity <- assert_design_rcm_parity(list(fin$control, fin2$control))
check(isTRUE(parity$ok) && parity$n_controls == 2L && parity$n_rcm_rows == 2L,
      "兩控制點＝兩 RCM 列且 ID 對齊")
fin_bad <- finalize_control_as_rcm_row(modifyList(d1, list(control_activity = d1$control_objective)))
check(!isTRUE(fin_bad$ok), "未完成設計不可定稿為 RCM 列")

iv <- control_to_interview(d1, elements = c("control_objective", "iuc"))
check(nrow(iv) == 2L, "訪談可依元素過濾")
check(all(c("控制編號", "訪談問題", "預期佐證_PBC", "受訪者回答") %in% names(iv)),
      "訪談工作底稿含標準欄位")
check(identical(as.character(iv[["控制編號"]][1]), derive_control_id(d1, 1L)),
      "訪談對齊控制編號")

# finalized-only: unsigned control excluded from multi helper when not ready
not_ready <- modifyList(d1, list(control_activity = d1$control_objective, rcm_ready = list(ready = FALSE)))
fin_ok <- finalize_control_as_rcm_row(d1, existing_ids = character())$control
check(isTRUE(finalize_control_as_rcm_row(modifyList(d1, list(company_status = "")), existing_ids = character())$ok),
      "無公司現況亦可定稿")
fin_blank <- finalize_control_as_rcm_row(modifyList(d1, list(company_status = "")), existing_ids = character())
check(!nzchar(trimws(as.character(fin_blank$rcm_row[["控制現況描述"]] %||% ""))),
      "定稿 RCM 控制現況描述留空")
iv_multi <- controls_to_interview(list(fin_ok, not_ready), finalized_only = TRUE)
check(all(iv_multi[["控制編號"]] == fin_ok$control_id), "訪談僅取已定稿 RCM 列")

csa <- control_to_csa(d1, elements = c("steps", "iuc", "outputs"))
check(all(c("測試程序", "所需文件_PBC", "預期結果", "控制編號") %in% names(csa)),
      "CSA 含測試步驟設計欄位")
check(nrow(csa) >= 3, "CSA 依元素產製多個測試步驟")
check(any(csa[["元素"]] == "IUC／相關系統"), "CSA 含 IUC 測試步驟")
csa_multi <- controls_to_csa(list(fin_ok, not_ready), finalized_only = TRUE)
check(all(csa_multi[["控制編號"]] == fin_ok$control_id), "CSA 僅取已定稿 RCM 列")
# Phase order evidence: interview columns ready independently of CSA
check(nrow(control_to_interview(fin_ok, DEFAULT_INTERVIEW_ELEMENTS)) >= 5,
      "訪談核心元素可產出完整題綱")

# PBC
reg <- empty_pbc_registry()
reg <- upsert_pbc(reg, list(client_pbc_name = "user_access.xlsx", reviewed_name = "使用者權限清冊"))
check(identical(apply_pbc_to_iuc(reg, reg$pbc_id[1]), "使用者權限清冊"), "PBC 套用")
reg2 <- upsert_pbc(reg, list(
  client_pbc_name = "outlook.msg", reviewed_name = "核准信", pbc_kind = "EMAIL"))
check(identical(apply_pbc_to_iuc(reg2, reg2$pbc_id[2]), "【EMAIL】核准信"), "PBC 證據類型標示套用")
check(identical(format_pbc_reviewed_label("制度手冊", "政策制度"), "【政策制度】制度手冊"),
      "PBC 格式化標示")

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
  check(!any(vapply(jl, function(x) grepl("控制編號", x$library_id %||% ""), logical(1))),
        "鯨鏈匯入不含標題列雜訊")
  # Seed includes Jinglian as first batch
  seeded <- seed_control_library(TRUE)
  jl_seed <- sum(vapply(seeded, function(x) grepl("^JL-EC-", x$library_id %||% ""), logical(1)))
  check(jl_seed >= 20, sprintf("種子庫含鯨鏈首批（JL-EC 實際 %d）", jl_seed))
  batch_file <- file.path(root, "data", "jinglian_it_rcm_batch.json")
  check(file.exists(batch_file), "已提交 jinglian_it_rcm_batch.json 首批資料")
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

seeded <- seed_control_library(TRUE)
it_rows <- cycle_risk_rows(seeded, "電腦化資訊系統循環")
check(length(it_rows) >= 20, sprintf("資訊循環風險列至少 20（實際 %d）", length(it_rows)))
it_risks <- cascade_risk_choices(it_rows)
check(length(it_risks) >= 10, sprintf("資訊循環風險因素候選至少 10（實際 %d）", length(it_risks)))
check(!any(grepl("\\[|\\]", names(it_risks))), "風險因素選項標籤不含[]")
check(all(nchar(names(it_risks)) <= 28), "風險因素選項標籤簡短")
check(identical(risk_factor_tag("密碼管理 / 制度與程序"), "密碼管理"), "風險因素tag取主段")
check(identical(risk_factor_tag("[報導面] 測試"), "報導面 測試"), "風險因素tag移除[]")
ch <- build_risk_factor_choices(it_rows, extra_selected = "不存在風險")
check("不存在風險" %in% unname(ch), "自訂/額外風險可併入選單")
check("__custom__" %in% unname(ch), "風險選單含自訂選項")

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

# 按鈕／下載皆須有對應 handler（防呆回歸）
app_src <- paste(readLines(file.path(root, "app.R"), encoding = "UTF-8"), collapse = "\n")
btn_ids <- unique(regmatches(app_src, gregexpr('actionButton\\(\\s*"([^"]+)"', app_src, perl = TRUE))[[1]])
btn_ids <- sub('actionButton\\(\\s*"([^"]+)".*', "\\1", btn_ids, perl = TRUE)
dl_ids <- unique(regmatches(app_src, gregexpr('downloadButton\\(\\s*"([^"]+)"', app_src, perl = TRUE))[[1]])
dl_ids <- sub('downloadButton\\(\\s*"([^"]+)".*', "\\1", dl_ids, perl = TRUE)
obs_ids <- unique(regmatches(app_src, gregexpr('observeEvent\\(\\s*input\\$([A-Za-z0-9_]+)', app_src, perl = TRUE))[[1]])
obs_ids <- sub('observeEvent\\(\\s*input\\$', "", obs_ids, perl = TRUE)
dlh_ids <- unique(regmatches(app_src, gregexpr('output\\$([A-Za-z0-9_]+)\\s*<-\\s*downloadHandler', app_src, perl = TRUE))[[1]])
dlh_ids <- sub('output\\$', "", sub('\\s*<-\\s*downloadHandler', "", dlh_ids, perl = TRUE), perl = TRUE)
miss_btn <- setdiff(btn_ids, obs_ids)
miss_dl <- setdiff(dl_ids, dlh_ids)
check(!length(miss_btn), sprintf("全部 actionButton 有 observeEvent（缺：%s）", paste(miss_btn, collapse = ",")))
check(!length(miss_dl), sprintf("全部 downloadButton 有 downloadHandler（缺：%s）", paste(miss_dl, collapse = ",")))
check(length(btn_ids) >= 16, sprintf("設計頁按鈕數量合理（實際 %d）", length(btn_ids)))
check(length(dl_ids) >= 5, sprintf("下載按鈕數量合理（實際 %d）", length(dl_ids)))

check(identical(cycle_code_for("電腦化資訊系統循環"), "EC"), "資訊循環編號＝EC")
check(identical(cycle_code_for("銷售及收款循環"), "SC"), "銷售循環編號＝SC")

# 風險辨識區塊：風險因素、風險描述、風險類別、RoMM 分類
check(grepl('accordion_panel\\(\\s*"風險辨識"', app_src), "有風險辨識 accordion")
check(grepl('textInput\\(\\s*"risk_factor"', app_src), "風險辨識含風險因素")
check(grepl('textAreaInput\\(\\s*"risk_description"', app_src), "風險辨識含風險描述")
check(grepl('selectInput\\(\\s*"risk_category"', app_src), "風險辨識含風險類別")
check(grepl('selectInput\\(\\s*"romm_classification"', app_src), "風險辨識含 RoMM 分類")
check(!grepl("custom_risk_factor|custom_risk_desc|custom_risk_category", app_src),
      "已移除自訂風險獨立輸入（改由風險辨識）")
check(!grepl('"(risk_attr_kind)"|input\\$risk_attr_kind|updateRadioButtons\\(\\s*session,\\s*"risk_attr_kind"', app_src),
      "已移除風險屬性 radio（改由風險類別）")

# 聲明（Assertions）依風險類別
check(length(ASSERTION_CHOICES_REPORTING) == 8L, "報導面 Assertions 為 Thomson Reuters 八種")
check(all(c(
  "存在或發生 (Existence or Occurrence)", "完整性 (Completeness)",
  "權利與義務 (Rights and Obligations)", "評價或分攤 (Valuation or Allocation)",
  "正確性 (Accuracy)", "截止 (Cutoff)", "分類 (Classification)", "表達 (Presentation)"
) %in% ASSERTION_CHOICES_REPORTING), "報導面含八種英文對照")
check(identical(ASSERTION_CHOICES_OPERATIONS, c(
  "完整性 (Completeness)", "正確性 (Accuracy)", "即時性 (Timeliness)"
)), "營運面僅完整性／正確性／即時性")
check(identical(assertion_mode_for_category("報導面"), "reporting"), "報導面 assertion mode")
check(identical(assertion_mode_for_category("營運面"), "operations"), "營運面 assertion mode")
check(identical(assertion_mode_for_category("遵循面"), "locked"), "遵循面 assertion 鎖定")
check(!length(assertion_choices_for_category("遵循面")), "遵循面無可選 Assertions")
check(identical(
  normalize_assertions_for_category(
    "存在或發生 (Existence or Occurrence)；即時性 (Timeliness)", "營運面"
  ),
  "即時性 (Timeliness)"
), "營運面過濾掉非允許聲明、保留即時性")
check(identical(
  normalize_assertions_for_category(
    "存在或發生 (Existence or Occurrence)", "營運面"
  ),
  ""
), "營運面僅報導面聲明時清空")
check(identical(
  normalize_assertions_for_category(
    paste(ASSERTION_CHOICES_OPERATIONS, collapse = "；"), "營運面"
  ),
  paste(ASSERTION_CHOICES_OPERATIONS, collapse = "；")
), "營運面三種可保留")
check(identical(normalize_assertions_for_category("完整性 (Completeness)", "遵循面"), ""),
      "遵循面定稿清空聲明")
fin_as <- finalize_control_as_rcm_row(modifyList(base, list(
  assertions = "存在或發生 (Existence or Occurrence)；即時性 (Timeliness)"
)))
check(isTRUE(fin_as$ok), "營運面含非法聲明仍可定稿（自動過濾）")
check(!grepl("存在或發生", fin_as$control$assertions %||% ""), "定稿後僅保留營運面允許聲明")
check(grepl("即時性", fin_as$control$assertions %||% ""), "定稿保留即時性")
fin_comp <- finalize_control_as_rcm_row(modifyList(base, list(
  risk_category = "遵循面",
  risk_attr_operations = "",
  risk_attr_compliance = "[遵循] 資安政策",
  significant_account = "",
  related_law = "證券交易法",
  assertions = "完整性 (Completeness)"
)))
check(isTRUE(fin_comp$ok), "遵循面可定稿")
check(!nzchar(trimws(fin_comp$control$assertions %||% "")), "遵循面定稿無 Assertions")

# Locale: ban Mainland / HK-Macau terms in UI + committed seed (Taiwan + US proper nouns only)
banned_locale <- c(
  "資料數據", "大批量", "重覆", "系統帳戶", "安裝或設置", "應設置密碼",
  "系統資源配置", "其它類別", "信息系統", "軟件", "網絡", "數據庫", "默認", "登录"
)
locale_scan_files <- c(
  file.path(root, "app.R"),
  file.path(root, "R", "rcm_csa.R"),
  file.path(root, "R", "cascade.R"),
  file.path(root, "R", "constants.R"),
  file.path(root, "data", "jinglian_it_rcm_batch.json")
)
locale_hits <- character()
for (fp in locale_scan_files) {
  if (!file.exists(fp)) next
  txt <- paste(readLines(fp, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  for (b in banned_locale) {
    if (grepl(b, txt, fixed = TRUE)) locale_hits <- c(locale_hits, paste0(basename(fp), ":", b))
  }
}
check(!length(locale_hits), sprintf("用語僅台灣／美式專有名詞（違規：%s）", paste(locale_hits, collapse = ",")))

if (fail > 0) quit(status = 1)
message("All extended tests passed.")
