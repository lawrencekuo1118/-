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
| RCM | 標準標題列；完成控制點＝完成一列；目標≠活動防呆；設計檢核 |
| 訪談／CSA | 元素可勾選；CSA 含測試程序／PBC／預期結果等測試步驟設計欄位 |
| 缺漏偵測 | 分類：缺資訊／缺文件／控制缺失；高嚴重度阻擋 RCM 定稿 |
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
