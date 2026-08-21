# Control Design Assistant (Shiny) — Form 4120SR / RCM / CSA

輔助**快速且精準**設計標準內部控制點，並產出 RCM、訪談問題與 CSA 底稿。

## 能力

| 面向 | 說明 |
|------|------|
| 九大循環／風險三大屬性 | 可自訂標籤與內容 |
| Form 4120SR 元素 | Nature／Approach／Type、Inputs／Steps／Outputs、Owner、頻率、IUC |
| 自動拼湊 | 公司現況一段控制描述 |
| IUC 分拆 | 同風險不同 IUC → 不同控制點 |
| IUC／PBC 命名庫 | 客戶原名 ↔ 檢視後命名，設計時可套用 |
| 範本庫 | 精選控制點可一鍵套用（可再擴充） |
| RCM | **控制目標**與**控制活動**分欄，禁止混用 |
| 訪談／CSA | 依元素拆出訪談題與自我評估步驟 |
| 缺漏偵測 | 缺資訊／文件／目標活動混淆等 |
| 草稿 | 本機 `data/session_draft.json` 儲存／載入 |

## 執行

```r
shiny::runApp("control-design-app")
```

依賴：`shiny`, `bslib`, `DT`, `jsonlite`

## 測試

```bash
cd control-design-app && Rscript tests/test_assemble.R
```
