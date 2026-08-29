# Goddamn SOX (Shiny) — Form 4120SR / RCM / CSA **v1.0.0**

線上版：<https://hopesmasher1118.shinyapps.io/goddamn-sox/>

輔助**快速且精準**設計標準內部控制點，並產出 RCM、訪談問題與 CSA 底稿。

詳見 [`RELEASE_v1.md`](RELEASE_v1.md)、[`VERSION`](VERSION)。

RCM 標題列對齊 **鯨鏈科技_資訊循環_RCM v1 (0820).xlsx**；設計採**強制引導流程**。

## 引導操作流程

1. 選擇**循環** → 候選**子作業**（可自訂）
2. 選子作業 → 查**風險屬性／描述**（可自訂）
3. 選風險 → **控制目標**候選
4. 選目標 → **控制活動**候選（每個活動僅對應一種**預防性／偵測性**）
5. 選 **IUC**（無則自訂並可存入 APP 庫／PBC）
6. 六大控制項目就緒後才顯示**公司現況**欄（可帶入規則草稿）
7. **控制編號**自動順編（如 `EC-101-01`）
8. 按 **「完成設計＝寫入 RCM 一列」**：設計完成＝RCM 其中一列（1 控制點 ↔ 1 RCM 列）

六大：控制類型、控制活動類型、頻率、負責單位、IUC、控制活動。

**不變條件**：`assert_design_rcm_parity` — 已定稿控制點數＝RCM 列數，且控制編號一一對齊。

## 開發優先順序

1. **RCM 控制點設計**（引導＋定稿＝一列）
2. **訪談問題**（對齊已定稿 RCM；可勾選元素／下載題綱）
3. **CSA 測試步驟**（測試程序／PBC／預期結果；僅已定稿列）

## 累積制通用範本庫

收集管道（可並用）：
- 設計頁「就緒→累積範本庫」／產生 RCM 後自動入庫（可勾選）
- 範本庫頁：表單→庫、選取控制點→庫、全部就緒→庫、佇列→庫
- 匯入 CSV／JSON／RCM xlsx（含鯨鏈首批）
- 引導自訂項存庫

同控制編號穩定覆寫（累積更新、不重複）；設計時側欄優先套用。

## 執行

```r
shiny::runApp("control-design-app")
```

依賴：`shiny`, `bslib`, `DT`, `jsonlite`；匯入 xlsx 需 `readxl`。

## 部署（shinyapps.io）

僅部署至 **goddamn-sox**（舊版 `control-design` 已退役，請勿再更新）：

```r
setwd("control-design-app")
rsconnect::deployApp(appDir = ".", appName = "goddamn-sox", account = "hopesmasher1118")
```

或：

```bash
cd control-design-app
export SHINYAPPS_TOKEN="..."
export SHINYAPPS_SECRET="..."
Rscript deploy.R
```

**GitHub Actions 自動部署**：push 至 `master` 且 `control-design-app/**` 有變更時，workflow `.github/workflows/deploy-goddamn-sox.yml` 會部署至 shinyapps.io。請在 repo Secrets 設定 `SHINYAPPS_TOKEN` 與 `SHINYAPPS_SECRET`（取自 [shinyapps.io Tokens](https://www.shinyapps.io/admin/#/tokens)）。

修改 PBC／範本庫／參數庫後，請先 commit `data/` 至 GitHub 再 deploy，才會同步至線上。

## 測試

```bash
cd control-design-app && Rscript tests/test_assemble.R
```
