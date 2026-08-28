# Godamn SOX — v1.0.0

第一版交付（2026-08-21）

線上版：<https://hopesmasher1118.shinyapps.io/godamn-sox/>

## 範圍
- 引導設計：循環 → 子作業 → 風險 → 目標 → 活動（單一預防/偵測）→ IUC → 六大就緒後公司現況
- 設計完成＝RCM 一列（鯨鏈標題列；防呆）
- ① 訪談題綱（對齊已定稿 RCM）
- ② CSA 測試步驟（測試程序／PBC／預期結果）
- 累積制範本庫（**首批資料＝鯨鏈資訊循環 RCM**，約 87 列，見 `data/jinglian_it_rcm_batch.json`）

## 執行（本機部署）
```r
# 建議工作目錄為 repo 根目錄
shiny::runApp("control-design-app", launch.browser = TRUE)
```
或：
```bash
cd control-design-app && Rscript -e 'shiny::runApp(launch.browser=TRUE)'
```

依賴：`shiny`, `bslib`, `DT`, `jsonlite`；匯入 xlsx 需 `readxl`。

## 驗證
```bash
cd control-design-app && Rscript tests/test_assemble.R
```

## 標籤
Git tag: `control-design-v1.0.0`
