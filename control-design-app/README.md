# Control Design Assistant (Shiny) — Form 4120SR / RCM / CSA v1

輔助**快速且精準**設計標準內部控制點，並產出 RCM、訪談問題與 CSA 底稿。

RCM 標題列對齊 **鯨鏈科技_資訊循環_RCM v1 (0820).xlsx**；設計採**強制引導流程**。

## 引導操作流程

1. 選擇**循環** → 候選**子作業**（可自訂）
2. 選子作業 → 查**風險屬性／描述**（可自訂）
3. 選風險 → **控制目標**候選
4. 選目標 → **控制活動**候選（每個活動僅對應一種**預防性／偵測性**）
5. 選 **IUC**（無則自訂並可存入 APP 庫／PBC）
6. 六大控制項目就緒後才顯示**公司現況**欄（可帶入規則草稿）
7. **控制編號**自動順編（如 `EC-101-01`）；完成一點＝RCM 一列

六大：控制類型、控制活動類型、頻率、負責單位、IUC、控制活動。

## 執行

```r
shiny::runApp("control-design-app")
```

依賴：`shiny`, `bslib`, `DT`, `jsonlite`；匯入 xlsx 需 `readxl`。

## 測試

```bash
cd control-design-app && Rscript tests/test_assemble.R
```
