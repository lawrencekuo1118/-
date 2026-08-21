# Control Design Assistant (Shiny) — Form 4120SR / RCM / CSA v1

輔助**快速且精準**設計標準內部控制點，並產出 RCM、訪談問題與 CSA 底稿。

RCM 標題列對齊 **鯨鏈科技_資訊循環_RCM v1 (0820).xlsx**（流程｜風險｜控制｜分析），並以防呆清楚區隔關聯欄位。

## 能力

| 面向 | 說明 |
|------|------|
| 引導選取 | 循環 → 子作業 → 風險因素 → 控制目標 → 控制活動（單一預防/偵測）→ IUC；候選來自累積範本庫 |
| 九大循環／風險三大屬性 | 可自訂；風險類別對齊報導面／營運面／遵循面 |
| Form 4120SR 元素 | 進階區；與鯨鏈 RCM 欄位並存 |
| 自動拼湊 | 公司現況一段控制描述 |
| IUC 分拆 | 同風險不同 IUC → 不同控制點／RCM 列 |
| IUC／PBC 命名庫 | **客戶取得原名 ↔ 檢視後新命名**；設計頁套用 |
| 範本庫 | **累積制**；CSV／JSON／**RCM xlsx** 匯入；可一鍵載入鯨鏈資訊循環首批 |
| RCM | 鯨鏈標題列；完成控制點＝完成一列；**目標≠活動**、**控制類型≠控制活動類型** 防呆 |
| 訪談／CSA | 元素可勾選；優先完成訪談＋RCM，再補 CSA 測試步驟 |
| 缺漏偵測 | 缺資訊／缺文件／控制缺失 |
| 草稿 | 具名草稿、自動儲存（`data/drafts/`） |

## 執行

```r
shiny::runApp("control-design-app")
```

依賴：`shiny`, `bslib`, `DT`, `jsonlite`；匯入 xlsx 需 `readxl`。

範本檔：`templates/鯨鏈科技_資訊循環_RCM_v1_0820.xlsx`

## 測試

```bash
cd control-design-app && Rscript tests/test_assemble.R
```
