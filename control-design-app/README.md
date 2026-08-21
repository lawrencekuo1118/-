# Control Design Assistant (Shiny)

輔助設計**標準內部控制點**，對齊 Form 4120SR（Significant Risk）控制設計敘述元素。

## 功能

- 依**九大循環**區辨風險，並可自訂風險名稱／完整 RoMM 文字
- **風險三大屬性**預設為財務報導／營運／法令遵循，標籤與內容皆可自訂
- 可編輯：控制目標、控制活動、頻率、負責單位、IUC／制度、Nature／Approach／Type、Inputs／Steps／Outputs
- **自動拼湊**成符合公司現況的一段控制描述（Summary + Detailed）
- 同一風險下若 **IUC／制度不同，自動分拆為不同控制點**；相同 IUC 則合併

## 執行

```r
# 建議先安裝：shiny, bslib, DT, jsonlite, shinyjs
shiny::runApp("control-design-app")
```

或於專案根目錄：

```bash
Rscript -e 'shiny::runApp("control-design-app", host="0.0.0.0", port=3838)'
```

## 測試

```bash
cd control-design-app && Rscript tests/test_assemble.R
```

## 範本

`templates/` 內含 Form 4120SR Word／Excel 參考檔。
