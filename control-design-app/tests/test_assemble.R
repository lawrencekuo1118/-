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
source(file.path(root, "R", "privilege.R"), local = TRUE)

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
check(length(ACCOUNT_CHOICES) >= 40L, "常見會計科目清單足夠")
check(ACCOUNT_ALL_OPTION %in% account_select_choices(), "選單含全部適用")
check(identical(join_significant_accounts(c("全部適用", "應收帳款")), "全部適用"),
      "含全部適用則正規化為全部適用")
check(identical(join_significant_accounts(c("應收帳款", "存貨")), "應收帳款；存貨"),
      "複選科目以分號接合")
check(ACCOUNT_ALL_OPTION %in% expand_account_selection("全部適用") &&
        length(expand_account_selection("全部適用")) > 10L,
      "全部適用展開為全科目選取")
rep_all <- design_required_check(modifyList(d1, list(
  risk_category = "報導面", significant_account = "全部適用"
)))
check(isTRUE(rep_all$ok) && isTRUE(rep_all$filled$significant_account), "報導面全部適用可過")
fin_rep_all <- finalize_control_as_rcm_row(modifyList(d1, list(
  risk_category = "報導面", significant_account = "全部適用",
  assertions = "完整性 (Completeness)"
)), existing_ids = character())
check(isTRUE(fin_rep_all$ok) && identical(fin_rep_all$control$significant_account, "全部適用"),
      "報導面全部適用可定稿")
fin_rep_multi <- finalize_control_as_rcm_row(modifyList(d1, list(
  risk_category = "報導面",
  significant_account = "應收帳款；營業收入",
  assertions = "完整性 (Completeness)"
)), existing_ids = character())
check(isTRUE(fin_rep_multi$ok) && grepl("應收帳款", fin_rep_multi$control$significant_account) &&
        grepl("營業收入", fin_rep_multi$control$significant_account),
      "報導面複選科目可定稿")

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

iv <- control_to_interview(d1, elements = c("control_objective", "iuc"),
                           include_module_rows = FALSE)
check(nrow(iv) == 2L, "訪談可依元素過濾")
check(all(c("控制編號", "訪談問題", "預期佐證_PBC", "受訪者回答",
            "回答架構_5W1H", "建議串接PBC") %in% names(iv)),
      "訪談工作底稿含標準欄位")
check(identical(as.character(iv[["控制編號"]][1]), derive_control_id(d1, 1L)),
      "訪談對齊控制編號")
check(all(grepl("以何頻率.*誰取得什麼文件或資訊\\(IUC\\).*做什麼.*下一步|人事時地物",
                iv[["回答架構_5W1H"]])),
      "訪談題含人事時地物回答鏈")
check(all(grepl("答案必含|人事時地物|以何頻率", iv[["訪談問題"]])),
      "每題問項答案須含人事時地物")
check(grepl("以何頻率 → 誰取得什麼文件或資訊\\(IUC\\) → 做什麼（具體控制行為）→ 才會進行什麼下一步",
            INTERVIEW_ANSWER_SCAFFOLD),
      "標準回答鏈＝頻率→誰取得IUC→做什麼→下一步")
check(identical(DEFAULT_INTERVIEW_ELEMENTS,
                c("risk", "control_objective", "control_activity")),
      "預設焦點＝預期風險／目標／活動（深入且快速）")
check(any(grepl("預期|實際|現況|走查|誰", iv[["訪談問題"]])),
      "訪談問題導向預期風險／目標／活動與實際執行現況")
iv_act <- control_to_interview(d1, elements = c("control_activity"),
                               include_module_rows = FALSE)
check(grepl("以何頻率.*誰取得什麼文件或資訊\\(IUC\\).*做什麼.*下一步",
            iv_act[["訪談問題"]][1]),
      "控制活動題明示人事時地物回答鏈")
iv_mod <- control_to_interview(d1, elements = c("iuc"), modules = c("what", "who"),
                               include_module_rows = FALSE)
check(grepl("誰取得什麼文件或資訊\\(IUC\\)", iv_mod[["回答架構_5W1H"]][1]),
      "5W1H 模組可拼湊組建（who+what 合併）")
check(grepl("使用者權限清冊", suggest_interview_pbc(d1)),
      "建議串接 PBC 帶出 IUC")
# 模組化探針題：勾選模組 → 獨立列；What 串 PBC
iv_probes <- control_to_interview(
  d1, elements = character(), modules = c("when", "what", "how"),
  include_module_rows = TRUE
)
check(nrow(iv_probes) == 3L, "5W1H 模組可拼湊為獨立探針題")
check(all(grepl("^5w1h_", iv_probes$element_key)), "探針題 element_key 標示 5w1h_")
check(any(grepl("PBC", iv_probes[["訪談問題"]])), "What 模組探針可串接 PBC 文案")
reg_iv <- empty_pbc_registry()
reg_iv <- upsert_pbc(reg_iv, list(
  client_pbc_name = "客戶權限報表", reviewed_name = "使用者權限清冊",
  pbc_kind = "系統表單", iuc_or_system = "使用者權限清冊", cycle = "資訊循環"
))
iv_pbc <- control_to_interview(
  d1, elements = c("iuc"), modules = c("what"),
  pbc_reg = reg_iv, pbc_ids = reg_iv$pbc_id[[1]],
  include_module_rows = TRUE
)
check(any(grepl("客戶權限報表|使用者權限清冊", iv_pbc[["建議串接PBC"]])),
      "訪談可串接 PBC 資料庫選取列")
check(any(grepl("PBC 資料庫|客戶權限報表|使用者權限清冊",
                iv_pbc[["訪談問題"]][iv_pbc$element_key == "5w1h_what"])),
      "What 探針題寫入已串接 PBC")
sc_partial <- interview_answer_scaffold(c("when", "how", "next_step"))
check(grepl("以何頻率.*做什麼（具體控制行為）.*才會進行什麼下一步", sc_partial) &&
        grepl("→", sc_partial),
      "使用者可勾選模組拼湊回答架構")
lib_iv <- library_items_as_interview_controls(list(
  list(library_id = "LIB-T", cycle = "資訊循環",
       control = list(cycle = "資訊循環", sub_process = "存取管理作業",
                      risk_factor = "未授權存取", control_objective = "確保授權",
                      control_activity = "定期覆核權限", iuc_or_system = "使用者權限清冊",
                      control_id = "LIB-T"))
))
check(length(lib_iv) == 1L && grepl("LIB", lib_iv[[1]]$control_id),
      "範本庫可轉為訪談預期控制點")
filt <- filter_controls_by_cycle_sub(lib_iv, cycle = "資訊循環", sub_key = "")
check(length(filt) == 1L, "訪談可依循環篩選")
check(length(filter_controls_by_cycle_sub(lib_iv, cycle = "銷售循環")) == 0L,
      "訪談循環篩選排除不符列")
check(all(c("risk", "control_objective", "control_activity") %in% names(INTERVIEW_ELEMENTS)),
      "訪談焦點含預期風險／目標／活動")
check(grepl("訪談引導（依序選取）", paste(readLines(file.path(root, "app.R"), encoding = "UTF-8"), collapse = "\n")) &&
        grepl("引導設計（依序選取）", paste(readLines(file.path(root, "app.R"), encoding = "UTF-8"), collapse = "\n")),
      "訪談與風險控制點設計皆為「引導（依序選取）」左欄標題")
app_txt <- paste(readLines(file.path(root, "app.R"), encoding = "UTF-8"), collapse = "\n")
check(grepl('col_widths = c\\(7, 5\\)', app_txt) &&
        grepl("interview_design_groups", app_txt) &&
        grepl("rcm_design_groups", app_txt) &&
        grepl('accordion_panel\\(\\s*"基本資料"', app_txt) &&
        grepl("interview_guide_banner", app_txt) &&
        grepl("interview_live_box", app_txt) &&
        grepl("interview_paragraph", app_txt),
      "訪談版面與風險控制點設計趨於一致（7/5、引導、accordion、右側預覽）")
check(grepl("套用 IUC／PBC 命名", app_txt),
      "訪談 5W1H／PBC 區標籤對齊風險控制點設計 PBC 套用")
check(!grepl("interview_source", app_txt) &&
        !grepl("① 題綱來源", app_txt) &&
        !grepl("已定稿 RCM（實際設計列）", app_txt) &&
        !grepl("範本庫預期（風險／目標／活動）", app_txt) &&
        !grepl("INTERVIEW_SOURCE_CHOICES", paste(readLines(file.path(root, "R/rcm_csa.R"), encoding = "UTF-8"), collapse = "\n")) &&
        !grepl("interview_cycle", app_txt) &&
        grepl("interview_sub", app_txt) &&
        grepl("cascade_source_library\\(lib\\(\\)\\)", app_txt) &&
        grepl("循環（全域）", app_txt) &&
        length(gregexpr('selectInput\\(\\s*"cycle"', app_txt, perl = TRUE)[[1]]) == 1L,
      "訪談／設計共用側邊欄循環（無題綱來源、無頁內循環選框）")

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
check(all(c("測試程序", "所需文件_PBC", "預期結果", "控制編號",
            "控制頻率", "建議樣本數", "抽樣方法論", "抽樣或範圍") %in% names(csa)),
      "CSA 含測試步驟設計欄位")
check(nrow(csa) >= 3, "CSA 依元素產製多個測試步驟")
check(any(csa[["元素"]] == "IUC／相關系統"), "CSA 含 IUC 測試步驟")
csa_multi <- controls_to_csa(list(fin_ok, not_ready), finalized_only = TRUE)
check(all(csa_multi[["控制編號"]] == fin_ok$control_id), "CSA 僅取已定稿 RCM 列")

# CSA 抽樣：依頻率（PCAOB／Deloitte）
romm_base <- "Significant Risk — Not higher risk associated with the control"
romm_hi <- "Significant Risk — Higher risk associated with the control"
plan_q <- control_test_sample_plan(modifyList(d1, list(frequency = "每季", nature = "人工",
  romm_classification = romm_base)))
check(identical(plan_q$sample_size, 2L) && identical(plan_q$sample_size_label, "2"),
      "每季基準樣本數＝2")
plan_m_hi <- control_test_sample_plan(modifyList(d1, list(frequency = "每月", nature = "人工",
  romm_classification = romm_hi)))
check(identical(plan_m_hi$sample_size, 5L), "每月 Higher RoMM 樣本數＝5")
plan_day <- control_test_sample_plan(modifyList(d1, list(frequency = "每日", nature = "人工",
  romm_classification = romm_base)))
check(identical(plan_day$sample_size, 25L), "每日基準樣本數＝25")
plan_auto <- control_test_sample_plan(modifyList(d1, list(nature = "自動", frequency = "每季")))
check(isTRUE(plan_auto$automated) && grepl("Test of one", plan_auto$sample_size_label),
      "自動控制＝持續／Test of one")
csa_freq <- control_to_csa(modifyList(d1, list(frequency = "每月", romm_classification = romm_base)),
                           elements = c("control_activity", "outputs"))
check(all(csa_freq[["建議樣本數"]] == "3"), "CSA 每月建議樣本數寫入")
check(all(grepl("每月|樣本數 3", csa_freq[["抽樣或範圍"]])), "CSA 抽樣或範圍含頻率樣本說明")
check(nrow(controls_to_csa(list(not_ready), finalized_only = TRUE)) == 0L,
      "未定版控制點不產出 CSA")

# CSA 多情境組：同一控制點不同現況 → 多組測試步驟
ctrl_sc <- fin_ok
ctrl_sc$csa_scenarios <- list(
  new_csa_scenario(
    scenario_name = "電子簽核路徑",
    company_status = "經 EasyFlow 申請後主管核准",
    review_steps = "抽核電子簽核單\n核對核准層級",
    outputs = "EasyFlow 簽核紀錄",
    scenario_id = "S1"
  ),
  new_csa_scenario(
    scenario_name = "紙本／口頭路徑",
    company_status = "會議口頭討論後執行，無正式簽核",
    review_steps = "訪談執行人\n取得會議紀錄或郵件",
    outputs = "會議紀錄／郵件",
    scenario_id = "S2"
  )
)
csa_sc <- control_to_csa(ctrl_sc, elements = c("steps", "outputs"))
check(length(unique(csa_sc[["情境組號"]])) == 2L, "CSA 兩情境組各一組號")
check(all(c("電子簽核路徑", "紙本／口頭路徑") %in% unique(csa_sc[["控制現況情境"]])),
      "CSA 含兩種控制現況情境名稱")
check(any(grepl("電子簽核", csa_sc[["測試程序"]])) && any(grepl("訪談執行人", csa_sc[["測試程序"]])),
      "各情境組測試步驟內容不同")
check(all(c("情境組號", "控制現況情境", "情境現況說明") %in% names(csa_sc)),
      "CSA 含情境組欄位")
ctrl_up <- upsert_control_csa_scenario(fin_ok, new_csa_scenario(
  scenario_name = "唯一情境", review_steps = "一步", scenario_id = "S9"
))
check(length(ctrl_up$csa_scenarios) == 1L &&
        identical(ctrl_up$csa_scenarios[[1]]$scenario_name, "唯一情境"),
      "upsert 可寫入 csa_scenarios")
ctrl_up2 <- upsert_control_csa_scenario(ctrl_up, new_csa_scenario(
  scenario_name = "第二情境", review_steps = "另一步", scenario_id = "S10"
))
check(length(control_csa_scenarios(ctrl_up2)) == 2L, "同一控制可累積兩情境組")
ctrl_rm <- remove_control_csa_scenario(ctrl_up2, "S9")
check(length(ctrl_rm$csa_scenarios) == 1L &&
        identical(ctrl_rm$csa_scenarios[[1]]$scenario_id, "S10"),
      "可刪除指定情境組")
# 無自訂情境時仍產出一組預設
csa_default <- control_to_csa(fin_ok, elements = c("control_activity"))
check(nrow(csa_default) >= 1L && identical(as.character(csa_default[["控制現況情境"]][1]), "預設現況"),
      "無自訂情境時使用預設現況一組")

# Phase order evidence: interview columns ready independently of CSA
check(nrow(control_to_interview(fin_ok, DEFAULT_INTERVIEW_ELEMENTS,
                                include_module_rows = FALSE)) == 3L,
      "深入且快速預設產出風險／目標／活動三題")
check(nrow(control_to_interview(
  fin_ok, unique(c(DEFAULT_INTERVIEW_ELEMENTS, INTERVIEW_WALKTHROUGH_EXTRA)),
  include_module_rows = FALSE
)) >= 5,
      "完整走查可產出擴充題綱")
check(nrow(control_to_interview(fin_ok, DEFAULT_INTERVIEW_ELEMENTS,
                                modules = DEFAULT_INTERVIEW_5W1H,
                                include_module_rows = TRUE)) == 8L,
      "焦點三題＋五個 5W1H 模組探針可拼湊")

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
# 九大循環皆有內建候選，毋須先匯入底稿
src_lib <- cascade_source_library(list())
cycle_counts <- vapply(CYCLES_NINE, function(cy) {
  length(library_controls_flat(src_lib, cycle = cy))
}, integer(1))
check(all(cycle_counts >= 1L),
      sprintf("九大循環皆有內建引導候選（實際：%s）",
              paste(sprintf("%s=%d", CYCLES_NINE, cycle_counts), collapse = "；")))
empty_user <- cascade_source_library(list())
check(length(cascade_sub_process_choices(
  library_controls_flat(empty_user, cycle = "生產循環")
)) >= 1L, "空使用者庫時生產循環仍可直接選子作業")
app_casc <- paste(readLines(file.path(root, "app.R"), encoding = "UTF-8"), collapse = "\n")
check(grepl("cascade_source_library", app_casc) && grepl("毋須匯入底稿", app_casc),
      "引導候選採內建來源且文案不要求先匯入底稿")
check(!grepl("請至「範本庫」匯入 CSV／JSON／RCM xlsx", app_casc),
      "引導候選不再要求先匯入底稿才能選")
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

# 選項列順序與名稱
nav_titles <- regmatches(app_src, gregexpr('nav_panel\\(\\s*"([^"]+)"', app_src, perl = TRUE))[[1]]
nav_titles <- sub('nav_panel\\(\\s*"([^"]+)".*', "\\1", nav_titles, perl = TRUE)
expect_nav <- c("首頁", "訪談問項設計", "風險控制點設計", "控制點測試設計",
                "範本庫", "參數庫", "PBC資料庫", "RCM")
check(identical(nav_titles, expect_nav),
      sprintf("選項列順序正確（實際：%s）", paste(nav_titles, collapse = "｜")))
check(grepl("goto_lib_tab|開啟範本庫", app_src) && grepl("goto_param_tab|開啟參數庫", app_src),
      "側邊欄最下方含範本庫／參數庫入口")
check(grepl("data-value=\\\\\"範本庫\\\\\"", app_src) &&
        grepl("data-value=\\\\\"參數庫\\\\\"", app_src) &&
        grepl("display: none", app_src),
      "標題列隱藏範本庫／參數庫（改由側邊欄進入）")
check(!grepl('selectInput\\(\\s*"pbc_cycle"', app_src),
      "PBC 頁無獨立循環選框（改用側邊欄）")
check(!grepl('selectInput\\(\\s*"cycle".*基本資料|accordion_panel\\(\\s*"基本資料"[\\s\\S]{0,400}selectInput\\(\\s*"cycle"', app_src, perl = TRUE),
      "基本資料 accordion 內無循環名稱選框")
check(!grepl("① 優先：從範本庫套用", app_src), "側邊欄已移除強制優先套用")
check(grepl("範本套用", app_src) && grepl("未套用範本", app_src),
      "範本庫頁籤含可跳過套用設定")
check(!grepl('actionButton\\(\\s*"csa_scenario_dup"', app_src) &&
        !grepl('actionButton\\(\\s*"save_to_lib"', app_src) &&
        !grepl('actionButton\\(\\s*"lib_add_selected_control"', app_src),
      "設計區塊按鈕精簡（移除多餘收集／複製鈕）")
check(grepl("overflow: visible !important", app_src) &&
        grepl("max-height: none !important", app_src),
      "區塊一次顯示全部內容（無區塊內上下滑動）")
check(grepl("apply_lib_selected_row", app_src), "範本庫可套用表格選取列")
check(grepl("admin_login|verify_admin_password|show_admin_login_modal", app_src), "含高權登入機制")
check(grepl("admin_lib_save_fields|admin_param_upsert", app_src), "高權可直接改範本庫／參數庫")

check(identical(cycle_code_for("電腦化資訊系統循環"), "EC"), "資訊循環編號＝EC")
check(identical(cycle_code_for("銷售及收款循環"), "SC"), "銷售循環編號＝SC")

# 風險辨識區塊：風險因素、風險描述、風險類別、RoMM 分類
check(grepl('accordion_panel\\(\\s*"控制設計"', app_src) &&
        grepl('textAreaInput\\(\\s*"control_objective"', app_src) &&
        grepl('textAreaInput\\(\\s*"control_activity"', app_src) &&
        grepl('selectInput\\(\\s*"approach"', app_src) &&
        grepl('selectInput\\(\\s*"nature"', app_src),
      "控制設計區塊含控制目標／活動／預防偵測／人工自動")
check(!grepl("設計必填與防呆", app_src) && !grepl("建議操作順序", app_src),
      "首頁已移除設計必填與防呆／建議操作順序說明")
check(grepl("home-tabs-grid", app_src) && grepl("overflow: visible", app_src),
      "各頁籤用途一次顯示全部方框（不壓縮捲動）")
check(grepl("pbc_apply_to_design", app_src) &&
        grepl('nav_panel\\(\\s*"PBC資料庫"', app_src) &&
        !grepl('accordion_panel\\(\\s*"控制設計"[\\s\\S]{0,1200}pbc_apply', app_src, perl = TRUE),
      "套用 IUC／PBC 命名改在 PBC資料庫")
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

# 高權密碼與直接維護
check(isTRUE(verify_admin_password("1118")), "預設高權密碼可驗證")
check(!isTRUE(verify_admin_password("尬電SOX#Admin")), "舊預設密碼已停用")
check(!isTRUE(verify_admin_password("wrong")), "錯誤密碼拒絕")
check(!isTRUE(verify_admin_password("")), "空密碼拒絕")
check(grepl("show_admin_login_modal|showModal", paste(readLines(file.path(root, "R/privilege.R"), encoding = "UTF-8"), collapse = "\n")) &&
        grepl("admin_prompt_lib|admin_prompt_param", app_src) &&
        !grepl("高權存取", app_src),
      "高權改為角落提示＋修改時彈出登入（側邊欄不張揚）")
ps0 <- empty_parameter_store()
ps1 <- upsert_parameter_row(ps0, "風險類別", "測試面", "高權維護")
check(nrow(ps1) == 1L && identical(ps1$來源[[1]], "高權維護"), "參數庫可高權新增列")
ps2 <- upsert_parameter_row(ps1, "風險類別", "測試面", "高權維護")
check(nrow(ps2) == 1L, "參數庫同鍵更新不重複")
ps3 <- delete_parameter_rows(ps2, 1L)
check(nrow(ps3) == 0L, "參數庫可刪除列")
lib0 <- list(list(
  library_id = "LIB-TEST", title = "舊標題", tags = c("a"),
  cycle = "電腦化資訊系統循環", source = "seed",
  control = list(risk_factor = "舊", control_objective = "舊目標", control_activity = "舊活動")
))
lib1 <- patch_library_item_fields(
  lib0, "LIB-TEST", title = "新標題", tags = "x;y",
  fields = list(risk_factor = "新風險", control_objective = "新目標")
)
check(identical(lib1[[1]]$title, "新標題") && identical(lib1[[1]]$control$risk_factor, "新風險"),
      "範本庫可高權直接改欄位")

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
