# Form 4120SR Significant Risk — design-side field catalogue
# and Taiwan internal-control 「九大循環」 defaults.

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

NATURE_CHOICES <- c("人工 (Manual)", "自動化 (Automated)", "人工＋自動化混合")
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
