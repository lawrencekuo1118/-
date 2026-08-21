# Control Design Assistant (Shiny) — Form 4120SR / RCM / CSA

輔助**快速且精準**設計標準內部控制點，並產出 RCM、訪談問題與 CSA 底稿。

## 能力

| 面向 | 說明 |
|------|------|
| 九大循環／風險三大屬性 | 可自訂標籤與內容 |
| Form 4120SR 元素 | Nature／Approach／Type、Inputs／Steps／Outputs、Owner、頻率、IUC |
| 自動拼湊 | 公司現況一段控制描述 |
| IUC 分拆 | 同風險不同 IUC → 不同控制點 |
| IUC／PBC 命名庫 | **客戶取得原名 ↔ 檢視後新命名**；CSV 持久化／匯入匯出；設計頁多選套用至 IUC 與 Inputs 對照 |
| 範本庫 | **累積制**完美控制點庫；CSV／JSON 大量匯入；設計時依循環／搜尋**優先套用**；可將表單／控制點存回庫 |
| RCM | **控制目標**與**控制活動**分欄，禁止混用 |
| 訪談／CSA | 設計元素可勾選，分別產製訪談問題與 CSA 自我評估底稿（可限定控制點） |
| 缺漏偵測 | 缺資訊／文件／目標活動混淆等 |
| 草稿 | 具名草稿儲存／載入／刪除、自動儲存、啟動還原（`data/drafts/`） |

## 執行

```r
shiny::runApp("control-design-app")
```

依賴：`shiny`, `bslib`, `DT`, `jsonlite`

## 測試

```bash
cd control-design-app && Rscript tests/test_assemble.R
```
