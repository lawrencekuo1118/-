# Form 4120SR Significant Risk — design-side field catalogue
# and Taiwan internal-control 「九大循環」 defaults.

# Brand palette (Deloitte-aligned: navy + green accent on black/white)
BRAND_BLUE <- "#002E82"
BRAND_GREEN <- "#86BC25"
BRAND_BLACK <- "#000000"
BRAND_WHITE <- "#FFFFFF"
BRAND_GRAY <- "#F5F5F5"

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

# Display labels (value remains CYCLES_NINE entry)
CYCLES_NINE_CHOICES <- c(
  "銷售及收款循環" = "銷售及收款循環",
  "採購及付款循環" = "採購及付款循環",
  "生產循環" = "生產循環",
  "薪工循環" = "薪工循環",
  "融資循環" = "融資循環",
  "固定資產循環" = "固定資產循環",
  "投資循環" = "投資循環",
  "研發循環" = "研發循環",
  "資訊循環（電腦化資訊系統循環）" = "電腦化資訊系統循環"
)

# 風險三大屬性（COSO 三類目標；使用者可改寫標籤與內容）
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

ASSERTION_CHOICES <- c(
  "存在／發生 (Existence/Occurrence)",
  "完整性 (Completeness)",
  "權利與義務 (Rights and Obligations)",
  "評價與分攤 (Valuation/Allocation)",
  "表達與揭露 (Presentation/Disclosure)",
  "截止 (Cutoff)",
  "正確性 (Accuracy)",
  "其他／自訂"
)

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
