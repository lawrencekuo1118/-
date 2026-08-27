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
source(file.path(root, "R", "00_constants.R"), local = TRUE)
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

reg_doc <- upsert_pbc(empty_pbc_registry(), list(
  client_pbc_name = "user_access.xlsx", reviewed_name = "使用者權限清冊",
  pbc_kind = "系統表單", iuc_or_system = "使用者權限清冊",
  cycle = "電腦化資訊系統循環"
))
pbc_doc_id <- reg_doc$pbc_id[[1]]
with_pbc_doc <- function(ctrl) {
  modifyList(ctrl, list(
    related_document_pbc_ids = pbc_doc_id,
    related_document = apply_pbc_to_related_document(reg_doc, pbc_doc_id)
  ))
}
without_pbc_doc <- function(ctrl) {
  modifyList(ctrl, list(related_document_pbc_ids = character(), related_document = ""))
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
  approach = "偵測性", type = TYPE_CHOICES[[6]],
  inputs = "在職名單、權限清冊", review_steps = "產製清冊\n主管覆核\n完成異動",
  outputs = "簽回清冊、異動 log", investigation_threshold = "任何不當權限均須異動",
  dependent_controls = "", control_id = "", iuc_or_system = "使用者權限清冊",
  company_status = "每季產出權限清冊並由主管覆核後完成異動",
  key_control = "Y"
)

d1 <- with_pbc_doc(modifyList(base, list(iuc_or_system = "使用者權限清冊")))
d2 <- with_pbc_doc(modifyList(base, list(iuc_or_system = "AD 群組報表", control_activity = "覆核 AD 群組")))
check(length(split_controls_by_iuc(list(d1, d2))) == 2L, "IUC 分拆")

# RCM row = designed control; objective ≠ activity (Jinglian headers)
rcm <- controls_to_rcm(list(d1))
check(all(RCM_HEADERS %in% names(rcm)), "RCM 含範本標準標題列")
check(identical(as.character(rcm[["相關文件-控制用文件"]]), "使用者權限清冊"), "RCM 含 IUC（控制用文件）欄")
empty_disp <- empty_rcm_display_df()
check(nrow(empty_disp) == 0L && all(RCM_HEADERS %in% names(empty_disp)) &&
        "儲存時間" %in% names(empty_disp),
      "空 RCM 顯示表仍含標題列（含儲存時間）")
check(!"RoMM 分類" %in% names(rcm), "RCM 範本不含 RoMM 分類欄")
check(identical(as.character(rcm[["控制聲明"]]), "完整性 (Completeness)"), "RCM 含控制聲明欄")
check(identical(as.character(rcm[["控制目標"]]), "確保系統使用者權限與現職一致"), "RCM 目標欄獨立")
check(identical(as.character(rcm[["控制活動"]]), "每季覆核權限清冊並完成異動"), "RCM 活動欄獨立")
check(!identical(as.character(rcm[["控制目標"]]), as.character(rcm[["控制活動"]])), "目標≠活動")
check(identical(as.character(rcm[["控制性質"]]), "人工"), "控制性質＝人工/自動")
check(!nzchar(normalize_control_type_manual_auto("人工＋自動")), "混合控制類型不允許")
check(!nzchar(normalize_control_type_manual_auto("人工＋自動化混合")), "混合控制類型不允許")
check(identical(resolve_control_frequency("自動", "每季"), "持續"), "自動控制頻率＝持續")
check(identical(resolve_control_frequency("人工", "每季"), "每季"), "人工控制頻率保留")
auto_ctrl <- without_pbc_doc(modifyList(d1, list(
  nature = "自動", frequency = "每季", related_system = "SAP ERP"
)))
rcm_auto <- controls_to_rcm(list(auto_ctrl))
check(identical(as.character(rcm_auto[["控制頻率"]]), "持續"), "RCM 自動控制頻率＝持續")
fin_auto <- finalize_control_as_rcm_row(auto_ctrl)
check(isTRUE(fin_auto$ok), "自動控制定稿成功")
check(identical(fin_auto$control$frequency, "持續"), "定稿後頻率強制持續")
check(identical(related_system_mode_for_ctrl(list(nature = "自動")), "required"),
      "自動控制：相關系統 mode＝required")
check(!isTRUE(design_required_check(without_pbc_doc(modifyList(d1, list(
  nature = "自動", frequency = "每季", related_system = ""
))))$ok), "自動控制缺相關系統不可過必填")
check(isTRUE(design_required_check(modifyList(d1, list(
  nature = "人工", related_system = ""
)))$ok), "人工控制相關系統仍可空")
check(identical(as.character(rcm[["控制方式"]]), "偵測性"), "控制方式＝預防/偵測/矯正")
check(identical(as.character(rcm[["風險類別"]]), "營運面"), "風險類別映射")
check(!"設計檢核" %in% names(rcm), "RCM 範本不含設計檢核欄")
check(all(c("風險面向", "風險範疇", "控制聲明", "控制點負責單位", "相關法規",
            "相關政策與制度", "相關文件-控制佐證文件") %in% names(rcm)),
      "RCM 含範本核心欄位")
check(identical(as.character(rcm[["會計科目"]]), "NA"), "非報導面會計科目暫列 NA")
rcm_law <- control_to_rcm_row(modifyList(d1, list(
  risk_category = "遵循面", related_law = "證券交易法",
  related_law_url = "https://law.moj.gov.tw/example",
  significant_account = ""
)))
check(grepl("證券交易法｜https://law.moj.gov.tw/example", as.character(rcm_law[["相關法規"]])),
      "RCM 相關法規可附有效網址連結")

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
check(!"設計檢核" %in% names(rcm_bad), "RCM 範本不含設計檢核欄")
check(!isTRUE(rcm_objective_activity_check(bad$control_objective, bad$control_activity)$ok),
      "目標活動相同時設計檢核失敗")
check(!isTRUE(finalize_control_as_rcm_row(bad)$ok),
      "目標活動相同時不可定稿")

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

gaps <- detect_design_gaps(bad)
check(any(gaps$category == "控制缺失"), "缺漏分類含控制缺失")
check(any(grepl("相同", gaps$gap_item)), "偵測目標活動混用")

gaps2 <- detect_design_gaps(modifyList(d1, list(
  iuc = "", iuc_or_system = "", related_system = "", outputs = "", related_document = ""
)))
check(any(gaps2$severity == "高" & grepl("IUC", gaps2$gap_item)), "缺 IUC 為必填高嚴重度")
check(any(gaps2$severity == "低" & grepl("產出|控制佐證文件", gaps2$gap_item)) ||
        any(gaps2$severity == "高" & grepl("PBC|控制佐證", gaps2$gap_item)),
      "缺產出／控制佐證文件缺漏偵測")
check(!isTRUE(design_required_check(modifyList(d1, list(
  iuc = "", iuc_or_system = "", related_system = "SAP ERP"
)))$ok), "僅填相關系統不可代替 IUC")

# 設計必填欄位
req_ok <- design_required_check(d1)
check(isTRUE(req_ok$ok), "完整控制點必填齊全")
req_bad <- design_required_check(modifyList(d1, list(
  frequency = "", responsible_unit = "", nature = "", risk_category = ""
)))
check(!isTRUE(req_bad$ok), "缺頻率／單位／類型／類別＝必填未齊")
check(any(grepl("控制頻率", req_bad$missing)), "必填清單含控制頻率")
check(any(grepl("控制點負責單位", req_bad$missing)), "必填清單含負責單位")
check(any(grepl("風險類別", req_bad$missing)), "必填清單含風險類別")
req_grouped <- design_required_check(modifyList(d1, list(
  sub_process_id = "", sub_process = "",
  risk_factor = "", risk_name = "", risk_description = "",
  control_objective = ""
)))
check(
  grepl("基礎設定：子作業名稱", format_design_required_by_accordion(req_grouped$missing_by_group)) &&
    grepl("風險辨識：風險因素", format_design_required_by_accordion(req_grouped$missing_by_group)) &&
    grepl("控制設計：控制目標", format_design_required_by_accordion(req_grouped$missing_by_group)),
  "必填缺漏依 accordion 分組顯示"
)
req_no_spid <- design_required_check(modifyList(d1, list(sub_process_id = "", control_id = "")))
check(isTRUE(req_no_spid$ok), "子作業編號與控制編號可留空")
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
comp_ok <- design_required_check(without_pbc_doc(modifyList(d1, list(
  risk_category = "遵循面", related_law = "證券交易法", significant_account = "",
  assertions = ""
))))
check(isTRUE(comp_ok$ok), "遵循面＋法令＝必填通過")
comp_bad <- design_required_check(without_pbc_doc(modifyList(d1, list(
  risk_category = "遵循面", related_law = "", significant_account = "",
  assertions = ""
))))
check(!isTRUE(comp_bad$ok) && any(grepl("相關法規", comp_bad$missing)), "遵循面缺法規不可過")
comp_as_bad <- design_required_check(without_pbc_doc(modifyList(d1, list(
  risk_category = "遵循面", related_law = "證券交易法", significant_account = "",
  assertions = "完整性 (Completeness)"
))))
check(!isTRUE(comp_as_bad$ok) && any(grepl("控制聲明|聲明", comp_as_bad$missing)),
      "遵循面填聲明應擋下")
comp_lock <- design_required_check(modifyList(d1, list(
  risk_category = "營運面", related_law = "SOX", significant_account = ""
)))
check(!isTRUE(comp_lock$ok) && any(grepl("法規", comp_lock$missing)), "營運面填法令應擋下")
check(length(RELATED_LAW_CHOICES_TW) >= 15 && length(RELATED_LAW_CHOICES_US) >= 10,
      sprintf("相關法令預設含台美（台%d／美%d）", length(RELATED_LAW_CHOICES_TW), length(RELATED_LAW_CHOICES_US)))
check(any(grepl("證券交易法", RELATED_LAW_CHOICES_TW)) && any(grepl("Sarbanes-Oxley", RELATED_LAW_CHOICES_US)),
      "預設含證交法與 SOX")
pcat <- parameter_catalog(list(), list(), list(), presets = list("相關法規" = unname(RELATED_LAW_CHOICES)))
check(nrow(pcat) >= 20 && any(pcat$參數 == "相關法規"), "參數庫可查詢預設法規")
seeded <- seed_control_library(TRUE)
pcat2 <- parameter_catalog(seeded, list(), list())
check(any(pcat2$參數 == "子作業名稱") && any(nzchar(pcat2$選項值[pcat2$參數 == "子作業名稱"])),
      "參數庫含子作業名稱")
check(!any(pcat2$參數 == "子作業編號"), "參數庫不收錄子作業編號")
check(!any(pcat2$參數 == "控制編號"), "參數庫不收錄控制編號")
check(any(pcat2$參數 == "控制目標"), "參數庫含控制目標選項")
pcat_iuc <- parameter_catalog(list(), list(modifyList(d1, list(
  iuc = "使用者權限清冊", iuc_or_system = "使用者權限清冊", related_system = "Active Directory"
))), list())
check(any(pcat_iuc$參數 == CONTROL_IUC_DOCUMENT_LABEL & grepl("使用者權限清冊", pcat_iuc$選項值)), "參數庫控制用文件（IUC）獨立收錄")
check(any(pcat_iuc$參數 == "相關系統" & grepl("Active Directory", pcat_iuc$選項值)), "參數庫相關系統獨立收錄")
check(!any(pcat_iuc$參數 == "相關系統／IUC"), "參數庫不再合併 IUC／相關系統")
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
check(nzchar(fin$control$control_id) && identical(nrow(fin$rcm_row), 1L),
      "定稿保留控制點編號且產出一列 RCM")
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
        !grepl("引導設計（依序選取）", paste(readLines(file.path(root, "app.R"), encoding = "UTF-8"), collapse = "\n")),
      "訪談保留引導；風險控制點設計已移除引導設計區塊")
app_txt <- paste(readLines(file.path(root, "app.R"), encoding = "UTF-8"), collapse = "\n")
check(grepl('col_widths = c\\(7, 5\\)', app_txt) &&
        grepl("interview_design_groups", app_txt) &&
        grepl("rcm_design_tabs", app_txt) &&
        grepl("design-preview-drawer", app_txt) &&
        grepl("designPreviewCollapse", app_txt) &&
        grepl('navset_tab\\([\\s\\S]*nav_panel\\([\\s\\S]*"① 基礎設定"', app_txt, perl = TRUE) &&
        grepl("interview_guide_banner", app_txt) &&
        grepl("interview_live_box", app_txt) &&
        grepl('download_interview[\\s\\S]{0,400}interview_live_box', app_txt, perl = TRUE) &&
        !grepl('card\\([\\s\\S]{0,120}interview_live_box', app_txt, perl = TRUE) &&
        grepl("interview_paragraph", app_txt),
      "訪談 7/5；題綱摘要移入引導區底部；右欄保留表格預覽")
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
        grepl('lab_req\\("循環"\\)', app_txt) &&
        length(gregexpr('selectInput\\(\\s*"cycle"', app_txt, perl = TRUE)[[1]]) == 1L,
      "訪談／設計共用側邊欄循環（無題綱來源、無頁內循環選框）")

# finalized-only: unsigned control excluded from multi helper when not ready
not_ready <- modifyList(d1, list(control_activity = d1$control_objective, rcm_ready = list(ready = FALSE)))
fin_ok <- finalize_control_as_rcm_row(d1, existing_ids = character())$control
check(isTRUE(finalize_control_as_rcm_row(modifyList(d1, list(company_status = "")), existing_ids = character())$ok),
      "無公司現況亦可定稿")
fin_blank <- finalize_control_as_rcm_row(modifyList(d1, list(company_status = "")), existing_ids = character())
check(!"控制現況描述" %in% names(fin_blank$rcm_row),
      "定稿 RCM 不含控制現況描述欄")
iv_multi <- controls_to_interview(list(fin_ok, not_ready), finalized_only = TRUE)
check(all(iv_multi[["控制編號"]] == fin_ok$control_id), "訪談僅取已定稿 RCM 列")

csa <- control_to_csa(d1, elements = c("steps", "iuc", "outputs"))
check(all(c("測試程序", "所需文件_PBC", "預期結果", "控制編號",
            "控制頻率", "建議樣本數", "抽樣方法論", "抽樣或範圍") %in% names(csa)),
      "CSA 含測試步驟設計欄位")
check(nrow(csa) >= 3, "CSA 依元素產製多個測試步驟")
check(any(csa[["元素"]] == "IUC"), "CSA 含 IUC 測試步驟")
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
check("傳票" %in% PBC_KIND_VALUES, "PBC 證據類型含傳票")
check(identical(format_pbc_reviewed_label("會計傳票", "傳票"), "【傳票】會計傳票"),
      "PBC 傳票類型標示套用")
check(identical(normalize_pbc_kind("傳票"), "傳票"), "PBC 傳票類型正規化")
reg_fmt <- upsert_pbc(empty_pbc_registry(), list(
  client_pbc_name = "shot.png", reviewed_name = "權限畫面",
  pbc_kind = "螢幕截圖", pbc_file_format = "PNG"
))
check(identical(reg_fmt$pbc_file_format[1], ".png"), "PBC 文件格式正規化為小寫副檔名")
check(identical(normalize_pbc_file_format(".PPTX"), ".pptx"), "PBC 文件格式大小寫正規化")
check("pbc_file_format" %in% names(empty_pbc_registry()), "PBC registry 含文件格式欄")
check("pbc_spec" %in% names(empty_pbc_registry()), "PBC registry 含規格說明欄")
reg_spec <- upsert_pbc(empty_pbc_registry(), list(
  client_pbc_name = "系統清單", reviewed_name = "系統清單",
  pbc_spec = "1. 含所有系統\n2. 檢附盤點表"
))
check(identical(reg_spec$pbc_spec[1], "1. 含所有系統\n2. 檢附盤點表"),
      "PBC 規格說明可寫入 registry")
seed_pbc <- file.path(root, "data", "pbc_registry.csv")
if (file.exists(seed_pbc)) {
  seeded <- load_pbc_registry(seed_pbc)
  check(nrow(seeded) >= 90, sprintf("資訊循環 PBC 種子至少 90 筆（實際 %d）", nrow(seeded)))
  check(any(nzchar(seeded$pbc_spec)), "資訊循環 PBC 種子含規格說明")
  check(any(seeded$cycle == "電腦化資訊系統循環"), "資訊循環 PBC 種子標示循環")
  check(sum(seeded$cycle == "財務報導循環") >= 60,
        sprintf("財務報導循環 PBC 種子至少 60 筆（實際 %d）",
                sum(seeded$cycle == "財務報導循環")))
  check(any(seeded$cycle == "財務報導循環" & grepl("^PBC-CA-", seeded$pbc_id)),
        "財務報導循環 PBC 使用 CA 編號前綴")
  check(any(seeded$cycle == "財務報導循環" & nzchar(seeded$pbc_spec)),
        "財務報導循環 PBC 種子含規格說明")
  check(sum(seeded$cycle == "銷售及收款循環") >= 160,
        sprintf("銷售及收款循環 PBC 種子至少 160 筆（實際 %d）",
                sum(seeded$cycle == "銷售及收款循環")))
  check(any(seeded$cycle == "銷售及收款循環" & grepl("^PBC-SC-", seeded$pbc_id)),
        "銷售及收款循環 PBC 使用 SC 編號前綴")
  check(any(seeded$cycle == "銷售及收款循環" & nzchar(seeded$pbc_spec)),
        "銷售及收款循環 PBC 種子含規格說明")
} else {
  message("SKIP: data/pbc_registry.csv 尚未產出")
}
reg3 <- upsert_pbc(reg2, list(
  client_pbc_name = "policy.pdf", reviewed_name = "資訊安全政策", pbc_kind = "政策制度"))
check(length(pbc_non_policy_choices(reg3)) == 2L, "IUC 選單排除政策制度 PBC")
check(length(pbc_policy_choices(reg3)) == 1L, "政策制度 PBC 僅出現在相關政策與制度選單")
check(!reg3$pbc_id[3] %in% unname(pbc_non_policy_choices(reg3)), "政策 PBC id 不在 IUC 選單")
check(reg3$pbc_id[3] %in% unname(pbc_policy_choices(reg3)), "政策 PBC id 在政策選單")
app_src_pbc <- paste(readLines(file.path(root, "app.R"), encoding = "UTF-8", warn = FALSE),
                     collapse = "\n")
check(grepl("pbc-kind-format-row", app_src_pbc, fixed = TRUE),
      "PBC 證據類型與文件格式同列並排")
check(grepl('selectizeInput\\(\\s*"pbc_file_format"', app_src_pbc, perl = TRUE),
      "PBC 樣本檔案格式輸入存在")
check(grepl("樣本檔案格式", app_src_pbc, fixed = TRUE),
      "PBC 樣本檔案格式標籤存在")
check(grepl('textAreaInput\\(\\s*"pbc_spec"', app_src_pbc, perl = TRUE) &&
        grepl("PBC規格說明", app_src_pbc, fixed = TRUE),
      "PBC 規格說明輸入位於整理表單")
check(grepl('pbc_spec[\\s\\S]{0,220}pbc-kind-format-row', app_src_pbc, perl = TRUE),
      "PBC 規格說明在證據類型／文件格式上方")
check(grepl("pbc-name-map-row", app_src_pbc, fixed = TRUE) &&
        grepl("pbc-name-map-arrow", app_src_pbc, fixed = TRUE) &&
        grepl("客戶原始取得PBC名稱", app_src_pbc, fixed = TRUE) &&
        grepl("檢視後新命名", app_src_pbc, fixed = TRUE),
      "PBC 原名與檢視後命名 1:1 並排含右箭頭")
check(grepl('pbc-name-map-row[\\s\\S]{0,500}pbc_spec', app_src_pbc, perl = TRUE),
      "PBC 規格說明在名稱並排列正下方")
pbc_panel <- sub('(?s).*nav_panel\\(\\s*"PBC資料庫"', 'nav_panel("PBC資料庫"', app_src_pbc, perl = TRUE)
pbc_panel <- sub('(?s)nav_panel\\(\\s*"範本庫".*', "", pbc_panel, perl = TRUE)
check(!grepl("layout_columns", pbc_panel, fixed = TRUE) &&
        grepl("PBC 清單預覽", pbc_panel, fixed = TRUE) &&
        grepl('card_header\\(\\s*"PBC 資料庫"', pbc_panel) &&
        regexpr("PBC 資料庫", pbc_panel, fixed = TRUE)[[1]] <
          regexpr("PBC 清單預覽", pbc_panel, fixed = TRUE)[[1]],
      "PBC 預覽列在設定畫面正下方（非左右雙欄）")
check("related_pbc_ids" %in% names(empty_pbc_registry()), "PBC registry 含互相勾稽欄")
check(grepl('selectizeInput\\(\\s*"pbc_related"', app_src_pbc, perl = TRUE) &&
        grepl("pbc_walkthrough_box", app_src_pbc, fixed = TRUE),
      "PBC 互相勾稽選單與 Walkthrough 預覽存在")
wt_reg <- empty_pbc_registry()
wt_reg <- upsert_pbc(wt_reg, list(
  pbc_id = "PBC-EC-001", client_pbc_name = "系統清單", reviewed_name = "系統清單",
  pbc_spec = "基準清單"
))
wt_reg <- upsert_pbc(wt_reg, list(
  pbc_id = "PBC-EC-002", client_pbc_name = "系統關聯圖", reviewed_name = "系統關聯圖",
  pbc_spec = "與#1「系統清單」相關。"
))
wt_reg <- enrich_related_pbc_from_specs(wt_reg)
check("PBC-EC-001" %in% parse_pbc_id_values(wt_reg$related_pbc_ids[2]),
      "規格說明 #1／名稱可自動解析勾稽")
wt <- pbc_walkthrough(wt_reg, "PBC-EC-001")
check("PBC-EC-002" %in% wt$inbound, "Walkthrough 可顯示入鏈勾稽")
wt2 <- pbc_walkthrough(wt_reg, "PBC-EC-002")
check("PBC-EC-001" %in% wt2$outbound, "Walkthrough 可顯示出鏈勾稽")
ca_reg <- empty_pbc_registry()
ca_reg <- upsert_pbc(ca_reg, list(
  pbc_id = "PBC-EC-013", client_pbc_name = "IT-13", reviewed_name = "IT-13",
  cycle = "電腦化資訊系統循環", pbc_spec = ""
))
ca_reg <- upsert_pbc(ca_reg, list(
  pbc_id = "PBC-CA-013", client_pbc_name = "匯率截圖", reviewed_name = "匯率截圖",
  cycle = "財務報導循環", pbc_spec = ""
))
ca_reg <- upsert_pbc(ca_reg, list(
  pbc_id = "PBC-CA-014", client_pbc_name = "匯率傳票", reviewed_name = "匯率傳票",
  cycle = "財務報導循環", pbc_spec = "與#13「匯率截圖」相關。"
))
ca_reg <- enrich_related_pbc_from_specs(ca_reg)
check(identical(parse_pbc_id_values(ca_reg$related_pbc_ids[3]), "PBC-CA-013"),
      "財務報導 #N 勾稽優先同循環 CA 編號")

# Library seeds + import（不再內建「存取管理／變更管理」短名子作業）
lib <- seed_control_library()
check(is.null(get_library_item(lib, "LIB-IT-ACCESS-01")), "已移除存取管理種子")
check(is.null(get_library_item(lib, "LIB-IT-CHANGE-01")), "已移除變更管理種子")
seed_subs <- unique(vapply(lib, function(x) x$control$sub_process %||% "", character(1)))
check(!"存取管理" %in% seed_subs && !"變更管理" %in% seed_subs,
      "種子庫子作業名稱不含存取管理／變更管理")
check(any(vapply(lib, function(x) identical(x$cycle %||% x$control$cycle, "電腦化資訊系統循環"), logical(1))),
      "資訊循環範本仍可由批次種子提供")
tmp_csv <- tempfile(fileext = ".csv")
utils::write.csv(library_to_flat_df(lib), tmp_csv, row.names = FALSE, fileEncoding = "UTF-8")
imported <- import_control_library_file(tmp_csv, existing = list(), overwrite = TRUE)
check(length(imported) >= 4, "CSV 匯入含資訊循環範本")

# Accumulative collect pipeline
ctrl_a <- with_pbc_doc(modifyList(base, list(control_id = "EC-101-99", sub_process_id = "EC-101")))
res1 <- collect_controls_to_library(list(), list(ctrl_a), source = "test", quality_gate = TRUE)
check(res1$added == 1L, "品質通過的控制點可收集入庫")
lid1 <- res1$library[[1]]$library_id
check(grepl("^LIB-[0-9a-f]{8}$", lid1), "穩定 ID 依內容雜湊而非控制編號")
check(!nzchar(trimws(res1$library[[1]]$control$control_id %||% "")), "入庫不存控制編號")
check(!nzchar(trimws(res1$library[[1]]$control$sub_process_id %||% "")), "入庫不存子作業編號")
check(!nzchar(trimws(res1$library[[1]]$control$company_status %||% "")), "入庫不存控制現況")
check(!nzchar(trimws(res1$library[[1]]$control$improvement %||% "")), "入庫不存改善建議")
# second save updates same content key
ctrl_a2 <- modifyList(ctrl_a, list(company_status = "完善後現況描述"))
res2 <- collect_controls_to_library(res1$library, list(ctrl_a2), source = "test", quality_gate = TRUE)
check(res2$updated == 1L && res2$added == 0L, "同內容覆寫為累積更新")
check(length(res2$library) == 1L, "累積不產生重複筆")
tmp_gov <- tempfile(fileext = ".json")
save_control_library(res2$library, tmp_gov)
gov_json <- paste(readLines(tmp_gov, encoding = "UTF-8", warn = FALSE), collapse = "\n")
check(!grepl("\"control_id\"", gov_json) && !grepl("\"sub_process_id\"", gov_json),
      "範本 JSON 不含子作業／控制編號欄")
check(!grepl("\"company_status\"", gov_json) && !grepl("\"improvement\"", gov_json),
      "範本 JSON 不含控制現況／改善建議欄")
bad_collect <- collect_controls_to_library(
  list(), list(modifyList(ctrl_a, list(control_activity = ctrl_a$control_objective))),
  quality_gate = TRUE
)
check(bad_collect$skipped == 1L && bad_collect$added == 0L, "品質門檻略過不合格控制點")
st <- library_stats(res2$library)
check(st$n == 1L, "library_stats 筆數")

# IT-cycle RCM xlsx → library batch（去識別）
xlsx <- file.path(root, "templates", "鯨鏈科技_資訊循環_RCM_v1_0820.xlsx")
jl <- list()
if (file.exists(xlsx)) {
  jl <- import_rcm_xlsx_as_library(
    xlsx, source = "rcm_import_batch", id_prefix = "JL",
    company_default = "", tags = c("RCM", "資訊循環", "首批")
  )
  check(length(jl) >= 20, sprintf("資訊循環 RCM 匯入至少 20 筆（實際 %d）", length(jl)))
  check(any(vapply(jl, function(x) grepl("^JL-EC-", x$library_id %||% ""), logical(1))),
        "匯入包裝鍵帶 JL-EC- 前綴")
  check(all(vapply(jl, function(x) !nzchar(trimws(x$control$control_id %||% "")), logical(1))),
        "匯入範本不持久化控制編號")
  check(all(vapply(jl, function(x) !nzchar(trimws(x$control$sub_process_id %||% "")), logical(1))),
        "匯入範本不持久化子作業編號")
  check(!any(vapply(jl, function(x) grepl("控制編號", x$library_id %||% ""), logical(1))),
        "匯入不含標題列雜訊")
  check(all(vapply(jl, function(x) !nzchar(trimws(x$control$company_status %||% "")), logical(1))),
        "匯入清空控制現況")
  check(!any(vapply(jl, function(x) {
    grepl("鯨鏈|Jinglian|輝能|ProLogium", paste(c(x$tags, x$source, x$control$company_status), collapse = " "),
          ignore.case = TRUE)
  }, logical(1))), "匯入結果去識別（無企業標籤／來源）")
  # Seed includes JL as first batch
  seeded <- seed_control_library(TRUE)
  jl_seed <- sum(vapply(seeded, function(x) grepl("^JL-EC-", x$library_id %||% ""), logical(1)))
  check(jl_seed >= 20, sprintf("種子庫含 JL 首批（JL-EC 實際 %d）", jl_seed))
  batch_file <- file.path(root, "data", "jinglian_it_rcm_batch.json")
  check(file.exists(batch_file), "已提交 jinglian_it_rcm_batch.json 首批資料")
  jl_batch <- load_control_library(batch_file, fallback_seed = FALSE)
  check(!any(vapply(jl_batch, function(x) {
    grepl("鯨鏈|Jinglian", paste(c(x$tags, x$source, x$control$company_status,
                                    x$control$design_gap_note), collapse = " "),
          ignore.case = TRUE)
  }, logical(1))), "已提交 JL 批次已去識別")
  check(all(vapply(jl_batch, function(x) !nzchar(trimws(x$control$company_status %||% "")), logical(1))),
        "JL 批次不含控制現況")
  sample_ctrl <- jl[[1]]$control
  rcm_jl <- control_to_rcm_row(sample_ctrl, 1L)
  check(all(c("控制目標", "控制活動", "控制性質", "控制方式") %in% names(rcm_jl)),
        "JL 列可映射回 RCM 標題")
  check(!identical(as.character(rcm_jl[["控制目標"]]), as.character(rcm_jl[["控制活動"]])) ||
          grepl("待修", as.character(rcm_jl[["設計檢核"]])),
        "JL 列目標/活動分欄或設計檢核標示")
} else {
  message("SKIP: 資訊循環 xlsx 不在 templates/")
}

# 全循環 RCM → 範本庫批次（去識別）
pl_xlsx <- file.path(root, "templates", "輝能科技_資訊循環_風險控制矩陣（RCM）.xlsx")
pl_batch <- file.path(root, "data", "prologium_rcm_batch.json")
check(identical(normalize_rcm_cycle_name("資訊"), "電腦化資訊系統循環"), "循環短名正規化：資訊")
check(identical(normalize_rcm_cycle_name("不動產、廠房及設備循環"), "固定資產循環"),
      "循環正規化：不動產→固定資產")
check(identical(normalize_rcm_cycle_name("", sheet_name = "RCM_企業層級"), "企業層級"),
      "循環正規化：企業層級 sheet")
if (file.exists(pl_xlsx)) {
  pl_it <- import_rcm_xlsx_as_library(
    pl_xlsx, source = "rcm_import_batch", id_prefix = "PL",
    company_default = "", tags = c("RCM")
  )
  check(length(pl_it) >= 20, sprintf("資訊循環匯入至少 20 筆（實際 %d）", length(pl_it)))
  check(any(vapply(pl_it, function(x) grepl("^PL-", x$library_id %||% ""), logical(1))),
        "匯入包裝鍵帶 PL- 前綴")
  check(all(vapply(pl_it, function(x) !nzchar(trimws(x$control$control_id %||% "")), logical(1))),
        "PL 匯入不持久化控制編號")
  check(all(vapply(pl_it, function(x) !nzchar(trimws(x$control$sub_process_id %||% "")), logical(1))),
        "PL 匯入不持久化子作業編號")
  check(all(vapply(pl_it, function(x) !nzchar(trimws(x$control$company_status %||% "")), logical(1))),
        "匯入清空控制現況（非設計欄）")
  check(all(vapply(pl_it, function(x) !nzchar(trimws(x$control$company %||% "")), logical(1))),
        "匯入不保留企業公司名")
  check(!any(vapply(pl_it, function(x) {
    grepl("輝能|ProLogium", paste(c(x$tags, x$control$detailed_description), collapse = " "),
          ignore.case = TRUE)
  }, logical(1))), "匯入結果去識別（無企業名）")
}
if (file.exists(pl_batch)) {
  pl_lib <- load_control_library(pl_batch, fallback_seed = FALSE)
  check(length(pl_lib) >= 100, sprintf("已提交 prologium_rcm_batch.json（實際 %d）", length(pl_lib)))
  pl_cycles <- unique(vapply(pl_lib, function(x) x$cycle %||% "", ""))
  check(all(c("電腦化資訊系統循環", "銷售及收款循環", "企業層級", "財務報導循環") %in% pl_cycles),
        "批次涵蓋資訊／銷售／企業層級／財務報導")
  check(!any(vapply(pl_lib, function(x) {
    blob <- paste(c(x$tags, x$control$company, x$control$related_document,
                    x$control$detailed_description), collapse = " ")
    grepl("輝能科技|ProLogium", blob, ignore.case = TRUE)
  }, logical(1))), "批次 JSON 已去識別企業名")
  seeded2 <- seed_control_library(TRUE)
  pl_seed <- sum(vapply(seeded2, function(x) grepl("^PL-", x$library_id %||% ""), logical(1)))
  check(pl_seed >= 100, sprintf("種子庫含 PL 批次（實際 %d）", pl_seed))
} else {
  message("SKIP: prologium_rcm_batch.json 尚未產出")
}

# Cascade engine: cycle → sub → risk → objective → activity(single PD) → IUC
check(identical(normalize_single_activity_type("預防性控制"), "預防性"), "單一預防屬性")
check(identical(normalize_single_activity_type("偵測性 (Detective)"), "偵測性"), "單一偵測屬性")
check(identical(normalize_single_activity_type("矯正性"), "矯正性"), "單一矯正屬性")
check(!activity_type_ok("預防＋偵測"), "混合屬性不允許")
check(identical(next_rcm_control_id("EC-101", c("EC-101-01", "EC-101-03")), "EC-101-04"),
      "控制編號自動順編")
check(identical(next_rcm_control_id("EC-102", character()), "EC-102-01"), "空庫從 01 起編")
check(identical(compose_control_id("EC", "101", 1L), "EC-101-01"),
      "控制編號＝循環-子作業-控制序號")
check(identical(compose_sub_process_id("SC", "101"), "SC-101"),
      "子作業編號＝循環-子作業序號")
check(identical(parse_rcm_id_parts("EC-101-01")$sub, "101") &&
        identical(parse_rcm_id_parts("EC-101-01")$ctrl, "01"),
      "可解析控制編號三節")
check(identical(recode_id_cycle_prefix("EC-101-01", "SC"), "SC-101-01"),
      "循環編號變更時控制編號前綴同步")
check(identical(recode_id_cycle_prefix("EC-101", "SC"), "SC-101"),
      "循環編號變更時子作業編號前綴同步")
check(identical(sync_sub_process_id_value("", "CS"), "CS-"),
      "循環編號已填 → 子作業預填 CS-")
check(identical(sync_control_id_value("", "CS", ""), "CS-"),
      "循環編號已填 → 控制編號預填 CS-")
check(identical(sync_control_id_value("CS-", "CS", "CS-102"), "CS-102-"),
      "子作業完整 → 控制編號預填 CS-102-")
check(identical(sync_control_id_value("CS-099-02", "CS", "CS-102"), "CS-102-02"),
      "子作業變更時控制編號中段同步並保留序號")
check(identical(sub_process_id_from_control_id("EC-101-02", "EC"), "EC-101"),
      "控制編號完整 → 回推子作業編號")
check(identical(sync_control_id_value("EC-101-02", "CS", ""), "CS-101-02"),
      "控制編號不可改寫循環節（強制為基準循環）")
check(identical(sync_sub_process_id_value("EC-101", "CS"), "CS-101"),
      "子作業編號不可改寫循環節")

# 設計自訂選項入參數庫
{
  ps0 <- empty_parameter_store()
  ps_in <- ingest_ctrl_parameters(ps0, list(
    sub_process = "權限清冊覆核作業",
    risk_description = "離職未即時停權",
    control_objective = "確保權限與現職一致",
    control_activity = "每季產出清冊並請主管覆核",
    responsible_unit = "資訊安全單位",
    iuc = "使用者權限清冊",
    related_documents = "權限申請單",
    related_document = "覆核簽核檔",
    related_system = "AD",
    related_policy = "資訊安全管理辦法",
    related_law = "個資法"
  ), source = "設計自訂")
  check(nrow(ps_in) >= 11L, "設計欄位可入參數庫")
  check(any(ps_in$參數 == "子作業名稱" & ps_in$選項值 == "權限清冊覆核作業" &
              ps_in$來源 == "設計自訂"), "子作業名稱自訂入庫")
  check(any(ps_in$參數 == CONTROL_IUC_DOCUMENT_LABEL & ps_in$選項值 == "使用者權限清冊"),
        "控制用文件自訂入庫")
  check(any(ps_in$參數 == CONTROL_EVIDENCE_DOCUMENT_LABEL & ps_in$選項值 == "覆核簽核檔"),
        "控制佐證文件自訂入庫")
  check("權限清冊覆核作業" %in% parameter_options(ps_in, "子作業名稱"),
        "parameter_options 可讀出自訂選項")
}

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
check(grepl("sync_sub_process_id_value|sync_control_id_value", app_casc) &&
        grepl("sub_process_id_from_control_id", app_casc),
      "設計頁以循環編號為基準同步子作業／控制編號")
check(exists("cascade_builtin_library", mode = "function"),
      "cascade_builtin_library 快取種子")
check(grepl("refresh_sub_process_choices\\(force", app_casc) &&
        grepl("input\\$main_nav", app_casc) &&
        grepl("sub_process_select_ui", app_casc) &&
        grepl("openOnFocus", app_casc),
      "進入設計分頁強制重送子作業選單")
check(!grepl("updateSelectInput\\(session, \"cycle\", selected = \"\"\\)", app_casc),
      "啟動時不再延遲清空循環（避免競態）")

check(grepl("cascade_source_library", app_casc),
      "訪談引導仍採內建範本庫候選")
check(grepl("placeholder = \"循環編號-子作業序號-控制序號", app_casc) &&
        !grepl("control_id_compose_hint", app_casc) &&
        !grepl("編號組成：", app_casc),
      "基礎設定以 placeholder 提示編號格式（無優化註解／動態提示列）")
check(!grepl('selectInput\\(\\s*"design_sub"|selectInput\\(\\s*"cascade_sub"', app_casc) &&
        grepl('selectizeInput\\(\\s*"sub_process"', app_casc) &&
        grepl("sub_process_hint", app_casc) &&
        !grepl("save_custom_cascade", app_casc) &&
        !grepl("cascade_step_status", app_casc),
      "風險控制點設計：子作業名稱含建議選單、移除獨立子作業欄")
check(identical(sub_process_name_from_value("EC-101||存取管理作業"), "存取管理作業"),
      "子作業名稱 key 可解析為名稱")
check(identical(sub_process_id_from_value("EC-101||存取管理作業", ""), "EC-101"),
      "子作業名稱 key 可解析為編號")
check(identical(sub_process_choice_label("EC-101||存取管理作業"), "存取管理作業"),
      "子作業選項標籤僅顯示名稱")
check(identical(sub_process_name_from_value("存取管理作業"), "存取管理作業"),
      "純名稱維持原樣")
check(identical(parse_sub_process_key("存取管理作業")$name, "存取管理作業") &&
        !nzchar(parse_sub_process_key("存取管理作業")$id),
      "純名稱 parse 為 name 非 id")
check(identical(lookup_sub_process_id_for_name(
  list(list(sub_process = "存取管理作業", sub_process_id = "EC-101")),
  "存取管理作業"
), "EC-101"), "可依名稱查回關聯編號")
check(isTRUE(id_matches_cycle_code("EC-101", "EC")), "同循環編號視為相符")
check(!isTRUE(id_matches_cycle_code("EC-101", "SC")), "跨循環編號應判定不符")
check(isTRUE(id_matches_cycle_code("SP-001", "EC")), "非循環前綴不強制判定不符")
check(grepl("id_matches_cycle_code", app_casc) &&
        grepl("updateTextInput\\(session, \"sub_process_id\"", app_casc) &&
        grepl("切換循環時清空子作業|勿依賴 input\\$sub_process|isolate\\(input\\$sub_process", app_casc),
      "改循環後選子作業名稱可覆寫編號（清空殘值＋避免 refresh 競態）")
check(grepl("isolate\\(input\\$sub_process\\)", app_casc) &&
        grepl("isolate\\(input\\$sub_process_id\\)", app_casc) &&
        grepl("freezeReactiveValue\\(input, \"sub_process\"\\)", app_casc) &&
        grepl("freezeReactiveValue\\(input, \"sub_process_id\"\\)", app_casc),
      "子作業編號／名稱互改不觸發 refresh 閃跳（isolate＋freeze）")
check(grepl('card_header\\(\\s*"風險控制點設計"\\)', app_casc),
      "風險控制點設計左欄標題改為表單設計")
it_risks <- cascade_risk_choices(it_rows)
check(length(it_risks) >= 10, sprintf("資訊循環風險因素候選至少 10（實際 %d）", length(it_risks)))
check(!any(grepl("\\[|\\]", names(it_risks))), "風險因素選項標籤不含[]")
check(all(nchar(names(it_risks)) <= 28), "風險因素選項標籤簡短")
check(identical(risk_factor_tag("密碼管理 / 制度與程序"), "密碼管理"), "風險因素tag取主段")
check(identical(risk_factor_tag("[報導面] 測試"), "報導面 測試"), "風險因素tag移除[]")
tag_rows <- list(
  list(risk_factor = "不當權限", risk_description = "離職未停權"),
  list(risk_factor = "不當權限", risk_description = "權限過寬"),
  list(risk_factor = "密碼管理", risk_description = "密碼未定期更換")
)
check(setequal(cascade_risk_description_choices(tag_rows, "不當權限"),
               c("離職未停權", "權限過寬")),
      "選 TAG 推薦曾標記的風險描述")
check("密碼未定期更換" %in% cascade_risk_description_choices(tag_rows, character()),
      "未選 TAG 時描述仍可從範圍挑選")
check("自訂新描述" %in% c(cascade_risk_description_choices(tag_rows, "不當權限"), "自訂新描述"),
      "風險描述可自訂新增（不卡控必須擇一）")
ch <- build_risk_factor_choices(it_rows, extra_selected = "不存在風險")
check("不存在風險" %in% unname(ch), "自訂/額外風險可併入選單")
check("__custom__" %in% unname(ch), "風險選單含自訂選項")

if (length(jl)) {
  rows <- library_controls_flat(jl, cycle = "電腦化資訊系統循環")
  check(length(rows) >= 20, "cascade flat 列來自鯨鏈庫")
  subs <- cascade_sub_process_choices(rows)
  check(length(subs) >= 1, "循環下有候選子作業")
  check(!any(grepl("｜|\\|\\||（", names(subs))), "子作業名稱選項標籤不含編號")
  check(!any(grepl("\\|\\||^[A-Z]{2}-\\d+", unname(subs))), "子作業名稱選項值為純名稱")
  check(all(names(subs) == unname(subs)), "子作業選項 value＝label＝純名稱")
  sub1 <- unname(subs)[[1]]
  rows2 <- filter_cascade_rows(rows, sub_key = sub1)
  check(length(rows2) >= 1, "以純名稱可篩選子作業列")
  risks <- cascade_risk_choices(rows2)
  check(length(risks) >= 1, "子作業下有風險候選")
  hits <- search_sub_process_hits(rows, keyword = substr(sub1, 1, 2))
  check(length(hits) >= 1 && all(!grepl("（|\\|\\|", vapply(hits, function(h) h$label, ""))),
        "搜尋命中標籤僅純名稱")
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
  sp_name <- sub1
  sp_id <- lookup_sub_process_id_for_name(rows, sp_name)
  check(!nzchar(sp_id), "範本列不帶子作業編號（設計頁即時組號）")
  sel_full <- list(
    cycle = "電腦化資訊系統循環",
    sub_process_id = compose_sub_process_id(cycle_code_for("電腦化資訊系統循環"), "101"),
    sub_process = sp_name,
    risk_factor = rk, control_objective = obj1,
    control_activity = akp$activity, approach = akp$approach,
    iuc_or_system = if (length(iucs)) unname(iucs)[[1]] else "自訂IUC-測試"
  )
  check(cascade_selection_ready(sel_full)$ready, "引導完成則就緒")
  six_bad <- six_status_rules_check(list())
  check(!six_bad$ok, "空控制六大未齊")
  six_ok <- six_status_rules_check(list(
    nature = "人工", approach = "預防性", frequency = "持續",
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
check(grepl("related-law-row", app_src) &&
        grepl("minmax\\(0, 1fr\\) minmax\\(0, 2fr\\)", app_src) &&
        grepl('textInput\\(\\s*"related_law_url"', app_src) &&
        grepl("法規有效網址連結", app_src),
      "相關法規與法規連結同列並排（左1右2）")
check(grepl("dblclick\\.editSelectizeItem|editSelectizeItem", app_src) &&
        grepl("persist_design_custom_params", app_src) &&
        grepl("ingest_ctrl_parameters", app_src) &&
        grepl('selectizeInput\\(\\s*"responsible_unit"', app_src) &&
        grepl('selectizeInput\\(\\s*"related_documents"', app_src),
      "選單欄位可雙擊修改且儲存入參數庫")
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
nav_titles <- nav_titles[!nav_titles %in% c("① 基礎設定", "② 風險辨識", "③ 控制設計")]
expect_nav <- c("首頁", "訪談問項設計", "風險控制點設計", "控制點測試設計",
                "RCM", "PBC資料庫", "範本庫", "參數庫")
check(identical(nav_titles, expect_nav),
      sprintf("選項列順序正確（實際：%s）", paste(nav_titles, collapse = "｜")))
visible_nav <- nav_titles[!nav_titles %in% c("範本庫", "參數庫")]
check(identical(tail(visible_nav, 2), c("RCM", "PBC資料庫")),
      "標題列可見末兩項為 RCM｜PBC資料庫")
check(grepl("goto_lib_tab|開啟範本庫", app_src) && grepl("goto_param_tab|開啟參數庫", app_src),
      "側邊欄最下方含範本庫／參數庫入口")
check(grepl("data-value=\\\\\"範本庫\\\\\"", app_src) &&
        grepl("data-value=\\\\\"參數庫\\\\\"", app_src) &&
        grepl("display: none", app_src),
      "標題列隱藏範本庫／參數庫（改由側邊欄進入）")
check(!grepl('selectInput\\(\\s*"pbc_cycle"', app_src),
      "PBC 頁無獨立循環選框（改用側邊欄）")
check(!grepl('selectInput\\(\\s*"cycle".*基礎設定|nav_panel\\([\\s\\S]{0,80}"① 基礎設定"[\\s\\S]{0,400}selectInput\\(\\s*"cycle"', app_src, perl = TRUE),
      "基礎設定分頁內無循環名稱選框")
check(!grepl("① 優先：從範本庫套用", app_src), "側邊欄已移除強制優先套用")
check(grepl("範本套用", app_src) && grepl("未套用範本", app_src) &&
        !grepl("從範本庫套用（可跳過）", app_src) &&
        grepl("lib-apply-card", app_src),
      "範本庫頁籤含可跳過套用設定（無重疊舊標題）")
check(!grepl('actionButton\\(\\s*"csa_scenario_dup"', app_src) &&
        !grepl('actionButton\\(\\s*"save_to_lib"', app_src) &&
        !grepl('actionButton\\(\\s*"lib_add_selected_control"', app_src) &&
        !grepl('actionButton\\(\\s*"import_jinglian_seed"', app_src),
      "設計區塊按鈕精簡（每區至多三個）")
check(grepl("overflow: visible !important", app_src) &&
        grepl("max-height: none !important", app_src),
      "區塊一次顯示全部內容（無區塊內上下滑動）")
check(grepl("apply_lib_selected_row", app_src), "範本庫可套用表格選取列")
check(grepl("admin_login|verify_admin_password|show_admin_login_modal", app_src), "含高權登入機制")
check(grepl("admin_lib_save_fields|admin_param_upsert", app_src), "高權可直接改範本庫／參數庫")

check(identical(cycle_code_for("電腦化資訊系統循環"), "EC"), "資訊循環編號＝EC")
check(identical(cycle_code_for("銷售及收款循環"), "SC"), "銷售循環編號＝SC")

# 風險辨識分頁：風險因素、風險描述、風險類別、RoMM 分類
check(grepl('nav_panel\\([\\s\\S]*"③ 控制設計"', app_src, perl = TRUE) &&
        grepl('textAreaInput\\(\\s*"control_objective"', app_src) &&
        grepl('textAreaInput\\(\\s*"control_activity"', app_src) &&
        grepl('selectInput\\(\\s*"approach"', app_src) &&
        grepl('selectInput\\(\\s*"nature"', app_src),
      "控制設計分頁含控制目標／活動／預防偵測／人工自動")
check(!grepl("設計必填與防呆", app_src) && !grepl("建議操作順序", app_src),
      "首頁已移除設計必填與防呆／建議操作順序說明")
check(grepl("home-tabs-grid", app_src) &&
        !grepl('fillable\\s*=\\s*c\\(', app_src) &&
        grepl("html-fill-container", app_src) &&
        grepl("overflow: visible !important", app_src) &&
        grepl("max-height: none !important", app_src),
      "全頁區塊一次顯示（無 fillable 壓縮、無區塊內上下滑動）")
check(!grepl('uiOutput\\(\\s*"design_required_checklist"', app_src) &&
        !grepl('uiOutput\\(\\s*"cascade_risk_detail"', app_src),
      "已移除重複之設計必填清單／風險屬性預覽")
check(!grepl('cascade_candidate_banner', app_src),
      "已移除引導設計上方重複提示框")
check(grepl('selectizeInput\\(\\s*"iuc"', app_src) &&
        grepl('create = TRUE', app_src) &&
        grepl('textInput\\(\\s*"related_system"', app_src) &&
        !grepl('textAreaInput\\(\\s*"iuc_or_system"', app_src),
      "IUC 與相關系統分開設定（IUC 為可多選 selectize）")
check(grepl('textInput\\(\\s*"related_system"\\s*,\\s*lab_opt\\(\\s*"相關系統"', app_src) &&
        grepl('selectizeInput\\(\\s*"related_policy"', app_src) &&
        grepl("pbc_policy_choices", app_src) &&
        grepl("pbc_non_policy_choices", app_src) &&
        grepl("updateTextInput\\(session, \"related_system\", label", app_src) &&
        grepl('related_system_mode_for_ctrl', app_src),
      "相關系統選填標籤與相關政策與制度同列排版")
check(!grepl('layout_columns[\\s\\S]{0,500}control_objective[\\s\\S]{0,500}assertions', app_src, perl = TRUE),
      "控制目標不再與控制聲明並排（改全寬）")
# 僅檢查「風險控制點設計」分頁內不用 layout_columns；方式／性質／頻率為三欄並排
design_panel <- sub('(?s).*nav_panel\\(\\s*"風險控制點設計"', "", app_src, perl = TRUE)
design_panel <- sub('(?s)nav_panel\\(\\s*"控制點測試設計".*', "", design_panel, perl = TRUE)
check(!grepl("layout_columns", design_panel, fixed = TRUE) &&
        grepl("rcm-design-tabs .shiny-input-container { width: 100%", app_src, fixed = TRUE) &&
        grepl('textInput\\(\\s*"sub_process_id"[\\s\\S]*width\\s*=\\s*"100%"', app_src, perl = TRUE),
      "風險控制點設計不以 layout_columns 排版；其餘輸入滿寬")
check(grepl("control-attr-row", app_src) &&
        grepl("repeat\\(3, minmax\\(0, 1fr\\)\\)", app_src) &&
        grepl('control-attr-row[\\s\\S]*"approach"[\\s\\S]*"nature"[\\s\\S]*"frequency"', app_src, perl = TRUE),
      "控制方式／性質／頻率三欄並排")
check(!grepl("請先選控制性質與風險類別；人工且非法遵面時", app_src),
      "控制佐證文件無待選灰色說明")
check(grepl('assertions-side[\\s\\S]*control_objective', app_src, perl = TRUE),
      "控制聲明在控制目標上方")
check(grepl('placeholder-shown', app_src) &&
        grepl('selectize-input\\.has-items', app_src) &&
        grepl('--input-placeholder:\\s*#ADB5BD', app_src) &&
        grepl('var\\(--input-placeholder\\)', app_src) &&
        grepl('公司名稱', app_src) &&
        grepl('輸入／選定後黑色|輸入框：未填淺灰|預設說明字', app_src),
      "輸入框預設說明字統一為公司名稱 placeholder 色 (#ADB5BD)")
check(grepl('navset_tab\\([\\s\\S]*rcm_design_tabs', app_src, perl = TRUE) &&
        grepl('div\\([\\s\\S]*class\\s*=\\s*"rcm-design-tabs"', app_src, perl = TRUE) &&
        grepl('"① 基礎設定"', app_src) &&
        grepl('"② 風險辨識"', app_src) &&
        grepl('"③ 控制設計"', app_src),
      "風險控制點設計以三階段分頁籤排版")
check(grepl('"控制聲明"', app_src), "聲明欄位標籤為控制聲明")
check(grepl("rcm_latest_saved|bump_rcm_views|last_saved_control|rcm_display_df", app_src),
      "RCM 頁籤含最新儲存即時顯示")
check(grepl("empty_rcm_display_df", app_src) &&
        grepl("emptyTable", app_src) &&
        !grepl('data\\.frame\\(訊息\\s*=\\s*"尚無 RCM 列', app_src),
      "RCM 頁無資料時仍渲染標題列而非單欄訊息")
merged_basic <- merge_design_preview_section(NULL, d1, "基礎設定")
check(isTRUE(merged_basic$rcm_preview), "預覽合併標記 rcm_preview")
check("基礎設定" %in% merged_basic$rcm_preview_sections, "預覽記錄區塊名稱")
merged_all <- merge_design_preview_section(
  merge_design_preview_section(merged_basic, d1, "風險辨識"),
  d1, "控制設計"
)
rcm_prev <- control_to_rcm_row(merged_all, 0L)
check(nzchar(as.character(rcm_prev[["子作業名稱"]][[1]])), "預覽列含基礎設定欄")
check(nzchar(as.character(rcm_prev[["風險描述"]][[1]])), "預覽列含風險辨識欄")
check(nzchar(as.character(rcm_prev[["控制活動"]][[1]])), "預覽列含控制設計欄")
check(grepl('preview_rcm_basic", "儲存"', app_src) &&
        grepl('preview_rcm_risk", "儲存"', app_src) &&
        grepl('preview_rcm_control", "儲存"', app_src),
      "三個設計區塊皆有儲存按鈕")
check(grepl("design-stage-save-bar", app_src) &&
        !grepl("design-section-preview-bar", app_src) &&
        grepl('nav_panel\\(\\s*"① 基礎設定"[\\s\\S]{0,200}design-stage-save-bar', app_src, perl = TRUE) &&
        grepl('nav_panel\\(\\s*"② 風險辨識"[\\s\\S]{0,200}design-stage-save-bar', app_src, perl = TRUE) &&
        grepl('nav_panel\\(\\s*"③ 控制設計"[\\s\\S]{0,200}design-stage-save-bar', app_src, perl = TRUE),
      "各階段儲存按鈕置於右上角")
check(grepl("design-tab-filter-bar", app_src) &&
        grepl("filter_basic_kw", app_src) &&
        grepl("filter_risk_category", app_src) &&
        grepl("filter_ctrl_approach", app_src) &&
        grepl("search_sub_process_hits", paste(readLines(file.path(root, "R/cascade.R"), encoding = "UTF-8"), collapse = "\n")),
      "三階段頁籤有簡約搜尋篩選")
basic_tab <- sub('(?s).*nav_panel\\(\\s*"① 基礎設定"', 'nav_panel("① 基礎設定"', app_src, perl = TRUE)
basic_tab <- sub('(?s)nav_panel\\(\\s*"② 風險辨識".*', "", basic_tab, perl = TRUE)
check(grepl('sub_process_select_ui[\\s\\S]{0,500}filter_basic_kw', basic_tab, perl = TRUE),
      "關鍵字篩選在子作業名稱輸入欄正下方")
check(grepl("design_preview_basic", app_src) &&
        grepl("design_preview_risk", app_src) &&
        grepl("design_preview_control", app_src) &&
        grepl("design_rcm_preview_fields", app_src) &&
        grepl("uiOutput\\(\"live_preview\"\\)", app_src),
      "各階段有設計預覽列")
check(identical(
  rcm_preview_columns_through_section("基礎設定"),
  c("循環名稱", "子作業名稱")
), "基礎設定預覽欄位順序")
check(identical(
  rcm_preview_columns_through_section("風險辨識"),
  c("循環名稱", "子作業名稱", "風險面向", "風險範疇", "風險因素",
    "風險描述", "風險類別", "會計科目")
), "風險辨識預覽欄位順序")
check(identical(
  rcm_preview_columns_through_section("控制設計"),
  RCM_HEADERS
), "控制設計預覽欄位＝完整範本")
prev_basic <- design_rcm_preview_fields(d1, section = "基礎設定")
check(nrow(prev_basic) == 2L && identical(prev_basic$欄位[[1]], RCM_HEADER_LABELS[["循環名稱"]]),
      "基礎設定預覽列欄位標籤")
check(grepl("csaPreviewCollapse", app_src) &&
        grepl("csaPreviewCollapse", app_src) &&
        grepl("data-bs-target", app_src) &&
        grepl("#csaPreviewCollapse", app_src, fixed = TRUE) &&
        grepl("預覽列（自我評估測試步驟）", app_src),
      "控制點測試設計預覽為底部可收合列")
csa_panel <- sub('(?s).*nav_panel\\(\\s*"控制點測試設計"', 'nav_panel("控制點測試設計"', app_src, perl = TRUE)
csa_panel <- sub('(?s)nav_panel\\(\\s*"RCM".*', "", csa_panel, perl = TRUE)
check(!grepl("layout_columns", csa_panel, fixed = TRUE) &&
        grepl("design-preview-drawer", csa_panel, fixed = TRUE),
      "CSA 頁不再用右側雙欄，改底部收合預覽")
check(grepl('lab_req\\("循環"\\)', app_src) &&
        grepl("循環為必填", app_src) &&
        !grepl("循環（全域）", app_src),
      "循環標示必填並於儲存／定稿門檻檢查（無優化註解）")
check(!grepl("不變條件", app_src) &&
        !grepl("類型欄防呆", app_src) &&
        !grepl("不入 RCM 範本欄", app_src) &&
        !grepl("自訂選項已入參數庫", app_src) &&
        !grepl("寫入 What／建議串接PBC", app_src) &&
        !grepl("台灣用語", app_src) &&
        !grepl("對齊鯨鏈", app_src),
      "介面不顯示優化／實作相關註解")
check(grepl("rcm_preview_ctrl|push_rcm_section_preview", app_src),
      "RCM 預覽合併邏輯")
check(!grepl("push_rcm_section_preview[\\s\\S]{0,400}nav_select\\(\\s*\"main_nav\"\\s*,\\s*selected\\s*=\\s*\"RCM\"", app_src, perl = TRUE),
      "預覽不自動切換至 RCM 分頁")
check(grepl("已儲存", app_src), "儲存通知文案")
check(!grepl("預覽中", app_src), "RCM 表格不再標示預覽中列")
check(grepl("rcm_preview_status", app_src), "RCM 頁顯示預覽狀態")
check(!is.null(fin$control$saved_at) && nzchar(fin$control$saved_at),
      "定稿控制點含儲存時間戳")
check(length(gregexpr("整體設計流程", app_src, fixed = TRUE)[[1]]) == 1 &&
        length(gregexpr("各頁籤用途", app_src, fixed = TRUE)[[1]]) == 1,
      "整體設計流程／各頁籤用途僅在首頁")
{
  home_start <- regexpr('nav_panel\\(\\s*"首頁"', app_src, perl = TRUE)[1]
  home_end <- regexpr('nav_panel\\(\\s*"訪談問項設計"', app_src, perl = TRUE)[1]
  home_chunk <- substr(app_src, home_start, home_end - 1L)
  other_chunk <- substr(app_src, home_end, nchar(app_src))
  check(grepl("整體設計流程", home_chunk, fixed = TRUE) &&
          grepl("各頁籤用途", home_chunk, fixed = TRUE),
        "整體設計流程／各頁籤用途位於首頁 nav_panel 內")
  check(!grepl("整體設計流程", other_chunk, fixed = TRUE) &&
          !grepl("各頁籤用途", other_chunk, fixed = TRUE),
        "其他頁籤不含整體設計流程／各頁籤用途")
}
check(grepl("pbc_apply_to_design", app_src) &&
        grepl('nav_panel\\(\\s*"PBC資料庫"', app_src) &&
        !grepl('nav_panel\\([\\s\\S]*"③ 控制設計"[\\s\\S]{0,1200}pbc_apply', app_src, perl = TRUE),
      "套用 IUC／PBC 命名改在 PBC資料庫")
check(grepl("missing_by_group", app_src) &&
        grepl("DESIGN_ACCORDION_SECTIONS", app_src) &&
        grepl("必填未齊（依表單分組）", app_src) &&
        grepl('rcm_design_tabs[\\s\\S]*design-validation-panel[\\s\\S]*live_validation', app_src, perl = TRUE) &&
        grepl('live_validation[\\s\\S]*finalize_rcm_row', app_src, perl = TRUE),
      "必填缺漏檢核置於三頁籤下方、定稿按鈕上方")
check(grepl('lab_req\\(CONTROL_EVIDENCE_DOCUMENT_LABEL\\)', app_src) &&
        grepl('selectizeInput\\(\\s*"related_document_pbc"', app_src) &&
        grepl("goto_pbc_tab", app_src) &&
        grepl("toggleRelatedDocument", app_src) &&
        !grepl('textInput\\(\\s*"related_document"', app_src),
      "控制佐證文件為可多選 selectize（PBC 選取或手動輸入；自動／遵循面鎖定）")
check(identical(related_document_mode_for_ctrl(list(nature = "人工", risk_category = "營運面")), "required"),
      "人工＋非法遵面：控制佐證文件必填")
check(identical(related_document_mode_for_ctrl(list(nature = "自動", risk_category = "營運面")), "locked"),
      "自動控制：控制佐證文件鎖定")
check(identical(related_document_mode_for_ctrl(list(nature = "人工", risk_category = "遵循面")), "locked"),
      "遵循面：控制佐證文件鎖定")
check(identical(CONTROL_EVIDENCE_DOCUMENT_LABEL, "相關文件-控制佐證文件"), "控制佐證文件標籤常數")
check(!isTRUE(design_required_check(without_pbc_doc(d1))$ok),
      "未選 PBC 文件且無手動輸入時必填檢核失敗")
check(isTRUE(design_required_check(modifyList(without_pbc_doc(d1), list(
  related_document_manual = "簽核紀錄", related_document = "簽核紀錄"
)))$ok),
      "控制佐證文件可改以手動輸入通過必填")
check(identical(
  ctrl_iuc_value(list(iuc = c("使用者權限清冊", "在職名單"))),
  "使用者權限清冊；在職名單"
), "IUC 複選以分號接合")
check(isTRUE(design_required_check(modifyList(d1, list(
  iuc = "手動 IUC A；手動 IUC B", iuc_or_system = "手動 IUC A；手動 IUC B"
)))$ok),
      "IUC 手動複選文字可通過必填")
check(grepl('selectizeInput\\(\\s*"risk_factor"', app_src), "風險辨識含風險因素複選選單")
check(grepl('multiple\\s*=\\s*TRUE', app_src) && grepl('"risk_factor"', app_src),
      "風險因素為複選 selectize")
check(grepl("isolate\\(refresh_risk_factor_choices", app_src) &&
        grepl("isolate\\(input\\$risk_factor", app_src) &&
        grepl("risk_factor_choices_cache", app_src),
      "風險因素多選不因 observe 誤追蹤而反覆重建選單")
check(grepl("applying_template|apply_template_to_form", app_src) &&
        grepl("applying_template\\(\\)\\) return\\(\\)", app_src),
      "套用範本時不觸發循環清空子作業")
check(grepl("lib_revision", app_src) &&
        grepl("pbc_choices_cache", app_src) &&
        grepl("sync_category_driven_fields", app_src),
      "PBC／風險類別連動選單具快取與範本庫修訂追蹤")
check(grepl("current_cycle", paste(readLines(file.path(root, "R/cascade.R"), encoding = "UTF-8"), collapse = "\n")),
      "套用範本時相同循環不重設 cycle 避免連鎖刷新")
check(identical(
  format_risk_factor_text(c("密碼管理 / 制度與程序", "密碼管理")),
  "密碼管理"
), "風險因素複選去重 tag 後以分號接合")
check(identical(
  format_risk_factor_text("密碼管理 / 制度與程序；使用者帳號管理"),
  "密碼管理；使用者帳號管理"
), "風險因素複選文字可正規化")
check(isTRUE(design_required_check(modifyList(d1, list(
  risk_factor = "密碼管理；使用者帳號管理", risk_name = "密碼管理；使用者帳號管理"
)))$ok),
      "風險因素複選可通過必填")
check(grepl('textAreaInput\\(\\s*"risk_description"', app_src), "風險辨識含風險描述質性輸入")
check(grepl(
  'risk_area[\\s\\S]{0,500}risk_factor[\\s\\S]{0,500}risk_description',
  app_src, perl = TRUE
), "風險因素位於風險範疇與風險描述之間")
check(grepl('nav_panel\\([\\s\\S]*"② 風險辨識"[\\s\\S]*textAreaInput\\(\\s*"risk_description"', app_src, perl = TRUE),
      "風險描述位於風險辨識分頁內")
check(grepl("風險因素是風險描述上的標記", app_src), "風險因素以 TAG 說明")
check(grepl('textAreaInput\\(\\s*"risk_description"[\\s\\S]{0,200}rows\\s*=\\s*3', app_src, perl = TRUE) &&
        grepl('textAreaInput\\(\\s*"control_objective"', app_src) &&
        grepl('textAreaInput\\(\\s*"control_activity"', app_src),
      "風險描述與控制目標／活動同為質性 textArea")
check(grepl('selectizeInput\\(\\s*"risk_factor"[\\s\\S]{0,400}create\\s*=\\s*TRUE', app_src, perl = TRUE),
      "風險因素可自訂新增")
check(!grepl("refresh_risk_description_choices", app_src) &&
        grepl("updateTextAreaInput\\(session, \"risk_description\"", app_src),
      "風險描述改質性輸入、不以選單刷新覆寫")
check(grepl('selectInput\\(\\s*"risk_category"', app_src), "風險辨識含風險類別")
check(grepl('selectInput\\(\\s*"romm_classification"', app_src), "風險辨識含 RoMM 分類")
check(grepl('significant_account[\\s\\S]*romm_classification', app_src, perl = TRUE),
      "會計科目在 RoMM 分類上方")
check(!grepl("custom_risk_factor|custom_risk_desc|custom_risk_category", app_src),
      "已移除自訂風險獨立輸入（改由風險辨識）")
check(!grepl('"(risk_attr_kind)"|input\\$risk_attr_kind|updateRadioButtons\\(\\s*session,\\s*"risk_attr_kind"', app_src),
      "已移除風險屬性 radio（改由風險類別）")
check(!grepl('textAreaInput\\(\\s*"risk_attr_detail"|input\\$risk_attr_detail', app_src),
      "已移除風險屬性細節獨立欄位（改由風險描述＋風險類別）")
check(identical(
  enforce_single_risk_attr(
    list(risk_category = "營運面"),
    kind = "operations",
    detail = "權限不一致"
  )$risk_attr_operations,
  "[營運] 權限不一致"
), "風險描述自動寫入對應風險屬性")

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

# 輸入檔非設計欄位不得污染範本庫／參數庫
polluted <- list(
  risk_factor = "測試風險", control_objective = "確保測試",
  control_activity = "執行測試步驟", cycle = "電腦化資訊系統循環",
  company_status = "公司目前實際做法一大段",
  design_gap_note = "與設計差異",
  effectiveness = "有效", residual_risk = "殘餘風險", improvement = "建議改善",
  detailed_description = "這其實是控制現況描述"
)
clean_item <- library_item_from_control(polluted, source = "import")
check(!nzchar(trimws(clean_item$control$company_status %||% "")), "入庫清空公司現況")
check(!nzchar(trimws(clean_item$control$effectiveness %||% "")), "入庫清空有效性評估")
check(!nzchar(trimws(clean_item$control$residual_risk %||% "")), "入庫清空潛在風險")
check(!nzchar(trimws(clean_item$control$improvement %||% "")), "入庫清空改善建議")
check(!nzchar(trimws(clean_item$control$design_gap_note %||% "")), "入庫清空設計差異")
check(!identical(clean_item$control$detailed_description %||% "", "這其實是控制現況描述"),
      "入庫不以現況文字當 detailed_description")
# RCM 列不得回填現況／差異／有效性（即使控制物件帶值）
rcm_pollute <- control_to_rcm_row(modifyList(polluted, list(
  detailed_description = "誤塞的現況長文",
  company_status = "實際做法",
  design_gap_note = "差異說明",
  effectiveness = "有效", residual_risk = "風險", improvement = "改善"
)))
check(!"控制現況描述" %in% names(rcm_pollute),
      "RCM 範本不含控制現況描述")
check(!"控制設計差異說明" %in% names(rcm_pollute),
      "RCM 範本不含控制設計差異說明")
check(!"控制有效性評估" %in% names(rcm_pollute),
      "RCM 範本不含控制有效性評估")
check(!"可能潛在風險" %in% names(rcm_pollute),
      "RCM 範本不含可能潛在風險")
check(!"建議改善方式" %in% names(rcm_pollute),
      "RCM 範本不含建議改善方式")
pcat_pollute <- parameter_catalog(
  list(clean_item),
  list(modifyList(polluted, list(control_id = "X-1"))),
  presets = list()
)
check(!any(pcat_pollute$參數 == "控制現況描述"), "參數庫不收集控制現況描述")
check(!any(grepl("公司目前實際做法", pcat_pollute$選項值)), "參數庫不含現況原文")
check(!any(pcat_pollute$參數 == "控制有效性評估"), "參數庫不收集控制有效性評估")
check(isTRUE(is_blocked_parameter_name("建議改善方式")), "建議改善方式列入參數黑名單")
check(isTRUE(is_blocked_parameter_name("可能潛在風險")), "可能潛在風險列入參數黑名單")
check(isTRUE(is_blocked_parameter_name("控制設計差異說明")), "差異說明列入參數黑名單")
blocked_try <- upsert_parameter_row(empty_parameter_store(), "控制現況描述", "不應寫入")
check(nrow(blocked_try) == 0L, "高權亦不可把控制現況寫入參數庫")
ps_loaded <- load_parameter_store(file.path(root, "data", "parameter_store.json"))
check(!any(vapply(ps_loaded$參數, is_blocked_parameter_name, logical(1))),
      "已提交參數庫不含現況／改善／編號等禁收參數")
jl_disk <- load_control_library(file.path(root, "data", "jinglian_it_rcm_batch.json"),
                                fallback_seed = FALSE)
check(length(jl_disk) >= 20 &&
        all(vapply(jl_disk, function(x) {
          is.null(x$control$company_status) && is.null(x$control$improvement) &&
            is.null(x$control$effectiveness) && is.null(x$control$residual_risk) &&
            is.null(x$control$design_gap_note)
        }, logical(1))),
      "已提交 JL 批次物件不含現況／改善等欄位鍵")

# 企業專屬用語／文件編號去識別
dei_raw <- list(
  company = "輝能科技",
  cycle = "投資循環",
  control_objective = "確認關係人",
  control_activity = "覆核名單",
  related_document = "1. ProLogium集團組織圖\n2. 輝能科技關係人名單\n3. 簽呈（A6-004-A）",
  iuc_or_system = "Easyflow 申請單；SPM系統",
  outputs = "簽呈（A6-004-A）"
)
dei_item <- library_item_from_control(dei_raw, tags = c("輝能科技", "RCM"), source = "prologium_rcm")
check(!nzchar(trimws(dei_item$control$company %||% "")), "去識別後不保留公司名")
check(!grepl("輝能|ProLogium|prologium", jsonlite::toJSON(dei_item, auto_unbox = TRUE), ignore.case = TRUE),
      "去識別後 JSON 不含企業名")
check(!grepl("A6-004-A", dei_item$control$related_document %||% ""), "去識別後去掉表單編號")
check(grepl("電子簽核系統", dei_item$control$iuc_or_system %||% ""), "Easyflow 改為通用系統名")
check(grepl("流程管理系統", dei_item$control$iuc_or_system %||% ""), "SPM 改為通用系統名")
check(!"輝能科技" %in% (dei_item$tags %||% character()), "標籤不含企業名")
check(identical(dei_item$source, "rcm_import_batch"), "來源改為中性 rcm_import_batch")
check(!grepl("輝能", dei_item$control$detailed_description %||% ""), "組裝敘述不含企業名")

fin_as <- finalize_control_as_rcm_row(with_pbc_doc(modifyList(base, list(
  assertions = "存在或發生 (Existence or Occurrence)；即時性 (Timeliness)"
))))
check(isTRUE(fin_as$ok), "營運面含非法聲明仍可定稿（自動過濾）")
check(!grepl("存在或發生", fin_as$control$assertions %||% ""), "定稿後僅保留營運面允許聲明")
check(grepl("即時性", fin_as$control$assertions %||% ""), "定稿保留即時性")
fin_comp <- finalize_control_as_rcm_row(without_pbc_doc(modifyList(base, list(
  risk_category = "遵循面",
  risk_attr_operations = "",
  risk_attr_compliance = "[遵循] 資安政策",
  significant_account = "",
  related_law = "證券交易法",
  assertions = "完整性 (Completeness)"
))))
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
  file.path(root, "R", "00_constants.R"),
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
