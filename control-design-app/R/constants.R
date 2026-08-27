# Form 4120SR Significant Risk — design-side field catalogue
# and Taiwan internal-control 「九大循環」 defaults.

# Brand palette (Deloitte-aligned: navy + green accent on black/white)
BRAND_BLUE <- "#002E82"
BRAND_GREEN <- "#86BC25"
BRAND_BLACK <- "#000000"
BRAND_WHITE <- "#FFFFFF"
BRAND_GRAY <- "#F5F5F5"

# Locale policy: Traditional Chinese (Taiwan) + American English proper nouns only.
# Prefer: 資訊、軟體、網路、資料庫、預設、登入、使用者、資料、大量／批次、設定／組態、帳號、檔案、螢幕、伺服器。
# Do not use Mainland China / Hong Kong / Macau equivalents of the above.
# Keep English proper nouns as-is: SOX, RCM, CSA, PBC, IUC, Form 4120SR, RoMM, Assertions, Inputs/Steps/Outputs, Type.

# 設計表單／RCM 範本欄名（對齊 Internal Control Lab「範本_RCM」）
CONTROL_EVIDENCE_DOCUMENT_LABEL <- "相關文件-控制佐證文件"
CONTROL_IUC_DOCUMENT_LABEL <- "相關文件-控制用文件"
RCM_COLUMN_RELATED_DOCUMENT <- "相關文件"
RCM_COLUMN_CONTROL_ASSERTION <- "控制聲明"
RCM_COLUMN_CONTROL_NATURE <- "控制性質"
RCM_COLUMN_CONTROL_APPROACH <- "控制方式"
RCM_COLUMN_CONTROL_OWNER <- "控制點負責單位"
RCM_COLUMN_RELATED_REGULATION <- "相關法規"
RCM_COLUMN_RELATED_POLICY <- "相關政策與制度"

CYCLES_NINE <- c(
  "銷售及收款循環",
  "採購及付款循環",
  "生產循環",
  "薪工循環",
  "融資循環",
  "固定資產循環",
  "投資循環",
  "研發循環",
  "電腦化資訊系統循環"
)

# 擴充循環（輝能 RCM 另含企業層級／財務報導）
CYCLES_EXTENDED <- c(CYCLES_NINE, "財務報導循環", "企業層級")

# Display labels (value remains CYCLES_NINE entry or extended)
CYCLES_NINE_CHOICES <- c(
  "銷售及收款循環" = "銷售及收款循環",
  "採購及付款循環" = "採購及付款循環",
  "生產循環" = "生產循環",
  "薪工循環" = "薪工循環",
  "融資循環" = "融資循環",
  "固定資產循環" = "固定資產循環",
  "投資循環" = "投資循環",
  "研發循環" = "研發循環",
  "資訊循環（電腦化資訊系統循環）" = "電腦化資訊系統循環",
  "財務報導循環" = "財務報導循環",
  "企業層級" = "企業層級"
)

# 循環編號（設計基本資料；資訊循環對齊鯨鏈 EC 前綴）
CYCLE_CODE_MAP <- c(
  "銷售及收款循環" = "SC",
  "採購及付款循環" = "PP",
  "生產循環" = "PR",
  "薪工循環" = "PY",
  "融資循環" = "FN",
  "固定資產循環" = "FA",
  "投資循環" = "IV",
  "研發循環" = "RD",
  "電腦化資訊系統循環" = "EC",
  "財務報導循環" = "CA",
  "企業層級" = "EL"
)

cycle_code_for <- function(cycle_name) {
  cy <- trimws(as.character(cycle_name %||% ""))
  if (!nzchar(cy)) return("")
  code <- unname(CYCLE_CODE_MAP[cy])
  if (length(code) && !is.na(code) && nzchar(code)) code else ""
}

# 風險三大屬性（COSO 三類目標；同一控制點三擇一，不可複選）
RISK_ATTR_KIND_CHOICES <- c(
  "財務報導" = "financial",
  "營運" = "operations",
  "法令遵循" = "compliance"
)
RISK_ATTR_DEFAULTS <- list(
  financial_reporting = list(
    label = "財務報導",
    prompt = "與財務報導目標相關之風險屬性說明"
  ),
  operations = list(
    label = "營運",
    prompt = "與營運目標相關之風險屬性說明"
  ),
  compliance = list(
    label = "法令遵循",
    prompt = "與法令遵循目標相關之風險屬性說明"
  )
)

NATURE_CHOICES <- c("人工 (Manual)", "自動化 (Automated)")
APPROACH_CHOICES <- c("預防性 (Preventive)", "偵測性 (Detective)", "預防＋偵測")
TYPE_CHOICES <- c(
  "核對驗證 (Verifications)",
  "資訊完整性／正確性控制 (Controls over IUC)",
  "實體保管與盤點 (Physical Controls and Counts)",
  "調節核對 (Reconciliations)",
  "授權與核准 (Authorizations and Approvals)",
  "含覆核要素之控制 (Controls with a Review Element)"
)

FREQUENCY_CHOICES <- c(
  "持續",
  "即時／每筆交易",
  "每日",
  "每週",
  "每月",
  "每季",
  "每半年",
  "每年",
  "事件觸發（自訂）",
  "其他（自訂）"
)

ROMM_CLASS_CHOICES <- c(
  "Significant Risk — Higher risk associated with the control",
  "Significant Risk — Not higher risk associated with the control",
  "Fraud risk",
  "Accounting estimate — higher risk",
  "其他／自訂"
)

# 報導面會計科目（常見財務報表科目；可複選；另有「全部適用」）
ACCOUNT_ALL_OPTION <- "全部適用"
ACCOUNT_CHOICES <- c(
  # 流動資產
  "現金及約當現金",
  "銀行存款",
  "透過損益按公允價值衡量之金融資產—流動",
  "透過其他綜合損益按公允價值衡量之金融資產—流動",
  "按攤銷後成本衡量之金融資產—流動",
  "應收票據",
  "應收帳款",
  "其他應收款",
  "存貨",
  "預付款項",
  "合約資產—流動",
  "待出售非流動資產",
  "其他流動資產",
  # 非流動資產
  "透過損益按公允價值衡量之金融資產—非流動",
  "透過其他綜合損益按公允價值衡量之金融資產—非流動",
  "按攤銷後成本衡量之金融資產—非流動",
  "採用權益法之投資",
  "不動產、廠房及設備",
  "使用權資產",
  "投資性不動產",
  "無形資產",
  "遞延所得稅資產",
  "其他非流動資產",
  # 流動負債
  "短期借款",
  "透過損益按公允價值衡量之金融負債—流動",
  "應付票據",
  "應付帳款",
  "其他應付款",
  "應付費用",
  "本期所得稅負債",
  "負債準備—流動",
  "租賃負債—流動",
  "合約負債—流動",
  "預收款項",
  "其他流動負債",
  # 非流動負債
  "長期借款",
  "應付公司債",
  "租賃負債—非流動",
  "負債準備—非流動",
  "遞延所得稅負債",
  "合約負債—非流動",
  "其他非流動負債",
  # 權益
  "股本",
  "資本公積",
  "保留盈餘",
  "法定盈餘公積",
  "特別盈餘公積",
  "未分配盈餘",
  "其他權益",
  "庫藏股票",
  "非控制權益",
  # 損益／綜合損益
  "營業收入",
  "銷貨收入",
  "勞務收入",
  "營業成本",
  "銷貨成本",
  "營業費用",
  "推銷費用",
  "管理費用",
  "研發費用",
  "薪資費用",
  "折舊及攤銷",
  "預期信用減損損失",
  "其他收益及費損淨額",
  "財務成本",
  "採用權益法認列之損益份額",
  "所得稅費用",
  "繼續營業單位本期淨利",
  "其他綜合損益",
  "本期綜合損益總額"
)

# 報導面：Thomson Reuters / AICPA GAAS 八種（可複選）
# https://tax.thomsonreuters.com/blog/audit-assertions-explained-types-risks-and-best-practices/
ASSERTION_CHOICES_REPORTING <- c(
  "存在或發生 (Existence or Occurrence)",
  "完整性 (Completeness)",
  "權利與義務 (Rights and Obligations)",
  "評價或分攤 (Valuation or Allocation)",
  "正確性 (Accuracy)",
  "截止 (Cutoff)",
  "分類 (Classification)",
  "表達 (Presentation)"
)

# 營運面：僅三種可複選
ASSERTION_CHOICES_OPERATIONS <- c(
  "完整性 (Completeness)",
  "正確性 (Accuracy)",
  "即時性 (Timeliness)"
)

# 向後相容：預設＝報導面完整清單
ASSERTION_CHOICES <- ASSERTION_CHOICES_REPORTING

# 相關法規預設（台灣＋美國財務報導／商業常見）
RELATED_LAW_CHOICES_TW <- c(
  # 財務報導／資本市場核心
  "證券交易法",
  "證券發行人財務報告編製準則",
  "公開發行公司年報應行記載事項準則",
  "公開發行公司建立內部控制制度處理準則",
  "公開發行公司取得或處分資產處理準則",
  "公開發行公司資金貸與及背書保證處理準則",
  "會計師查核簽證財務報表規則",
  "商業會計法",
  "企業會計準則公報",
  "國際財務報導準則（IFRS）／IAS",
  "公司法",
  # 產業／金融監理
  "金融控股公司法",
  "銀行法",
  "保險法",
  "洗錢防制法",
  # 治理／稅務／其他常見商業法
  "企業併購法",
  "公平交易法",
  "營業秘密法",
  "個人資料保護法",
  "資通安全管理法",
  "所得稅法",
  "加值型及非加值型營業稅法",
  "勞動基準法"
)

RELATED_LAW_CHOICES_US <- c(
  # Financial reporting / securities
  "Securities Act of 1933",
  "Securities Exchange Act of 1934",
  "Sarbanes-Oxley Act (SOX)",
  "SOX Section 302 / 404 (ICFR)",
  "Dodd-Frank Act",
  "Investment Company Act of 1940",
  "PCAOB Auditing Standards",
  "US GAAP (FASB ASC)",
  "SEC Regulation S-X",
  "SEC Regulation S-K",
  "Internal Control over Financial Reporting (ICFR)",
  "COSO Internal Control — Integrated Framework",
  # Related commercial / compliance often cited with FR
  "Foreign Corrupt Practices Act (FCPA)",
  "Bank Secrecy Act / AML",
  "GLBA",
  "HIPAA",
  "CCPA / CPRA"
)

RELATED_LAW_CHOICES <- c(
  stats::setNames(RELATED_LAW_CHOICES_TW, paste0("台灣｜", RELATED_LAW_CHOICES_TW)),
  stats::setNames(RELATED_LAW_CHOICES_US, paste0("美國｜", RELATED_LAW_CHOICES_US))
)

# Required Form 4120SR design-description elements (Note 1 + design factors used in narrative)
REQUIRED_DESIGN_ELEMENTS <- c(
  "cycle",
  "risk_name",
  "risk_attr_financial",
  "risk_attr_operations",
  "risk_attr_compliance",
  "control_objective",
  "control_activity",
  "frequency",
  "responsible_unit",
  "iuc_or_system",
  "nature",
  "approach",
  "type",
  "inputs",
  "review_steps",
  "outputs",
  "significant_account",
  "assertions"
)
