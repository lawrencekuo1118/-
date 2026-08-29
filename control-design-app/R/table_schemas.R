# 各表格 SCHEMA 說明（維護檔）
# 調整 DataTable 欄位順序／標題／來源對照時，請同步更新本檔與 app.R 內 renderDT。

.schema_col <- function(col, ord, source = "—", note = "—") {
  list(col = col, ord = ord, source = source, note = note)
}

.schema_table <- function(table_id, title, page, dt_output, rows, notes = "—") {
  list(
    table_id = table_id,
    title = title,
    page = page,
    dt_output = dt_output,
    rows = rows,
    notes = notes
  )
}

table_schema_registry <- function() {
  rcm_cols <- if (exists("RCM_HEADERS")) {
    RCM_HEADERS
  } else {
    character()
  }
  rcm_rows <- c(
    list(.schema_col("儲存時間", 1L, "control$saved_at", "僅已定稿列；草稿列為空")),
    lapply(seq_along(rcm_cols), function(i) {
      .schema_col(rcm_cols[[i]], i + 1L, "control_to_rcm_row()", "RCM 範本標準欄")
    })
  )

  param_cols <- if (exists("empty_parameter_store", mode = "function")) {
    names(empty_parameter_store())
  } else {
    c("參數", "選項值", "來源", "出現次數", "最近更新")
  }
  param_rows <- lapply(seq_along(param_cols), function(i) {
    src <- switch(
      param_cols[[i]],
      "參數" = "parameter_catalog / upsert",
      "選項值" = "各來源彙整",
      "來源" = "系統預設／範本庫／RCM／PBC／高權維護",
      "出現次數" = "merge 累計",
      "最近更新" = "Sys.time()",
      "—"
    )
    .schema_col(param_cols[[i]], i, src, "—")
  })

  lib_rows <- list(
    .schema_col("library_id", 1L, "item$library_id", "主鍵"),
    .schema_col("cycle", 2L, "item$cycle", "—"),
    .schema_col("title", 3L, "item$title", "—"),
    .schema_col("risk", 4L, "control$risk_factor", "—"),
    .schema_col("objective", 5L, "control$control_objective", "—"),
    .schema_col("activity", 6L, "control$control_activity", "—"),
    .schema_col("iuc", 7L, "control$iuc_or_system", "—"),
    .schema_col("source", 8L, "item$source", "—")
  )

  pbc_rows <- list(
    .schema_col("ID", 1L, "pbc_id", "—"),
    .schema_col("循環", 2L, "cycle", "—"),
    .schema_col("標準名稱", 3L, "reviewed_name", "不含證據類型前綴（另有證據類型欄）"),
    .schema_col("原始名稱", 4L, "client_pbc_name", "—"),
    .schema_col("證據類型", 5L, "pbc_kind", "依類型上色"),
    .schema_col("檔案格式", 6L, "pbc_file_format", "—"),
    .schema_col("規格說明", 7L, "pbc_spec", "截斷 48 字"),
    .schema_col("勾稽", 8L, "related_pbc_ids", "—"),
    .schema_col("備註", 9L, "notes", "—")
  )

  ctrl_rows <- list(
    .schema_col("控制編號", 1L, "control_id", "—"),
    .schema_col("IUC", 2L, "iuc / iuc_or_system", "—"),
    .schema_col("相關系統", 3L, "related_system", "—"),
    .schema_col("RCM列", 4L, "rcm_ready$ready", "已定稿＝1列／待補"),
    .schema_col("目標", 5L, "RCM 控制目標", "—"),
    .schema_col("活動", 6L, "RCM 控制活動", "—")
  )

  iv_full <- if (exists("empty_interview_df", mode = "function")) {
    names(empty_interview_df())
  } else {
    character()
  }
  iv_hidden <- if (exists("INTERVIEW_PREVIEW_HIDDEN_COLS")) {
    INTERVIEW_PREVIEW_HIDDEN_COLS
  } else {
    character()
  }
  iv_preview <- setdiff(iv_full, iv_hidden)
  iv_rows <- lapply(seq_along(iv_preview), function(i) {
    col <- iv_preview[[i]]
    note <- if (col %in% iv_hidden) "預覽隱藏" else "預覽顯示"
    .schema_col(col, i, "interview_worksheet()", note)
  })

  csa_cols <- if (exists("empty_csa_frame", mode = "function")) {
    names(empty_csa_frame())
  } else {
    character()
  }
  csa_rows <- lapply(seq_along(csa_cols), function(i) {
    .schema_col(csa_cols[[i]], i, "control_to_csa()", "—")
  })

  gap_rows <- list(
    .schema_col("control_id", 1L, "derive_control_id()", "—"),
    .schema_col("category", 2L, "detect_design_gaps()", "缺資訊／缺文件等"),
    .schema_col("severity", 3L, "detect_design_gaps()", "高／中／低"),
    .schema_col("gap_item", 4L, "detect_design_gaps()", "—"),
    .schema_col("suggested_action", 5L, "detect_design_gaps()", "—")
  )

  list(
    .schema_table(
      "rcm_table", "RCM 表格", "RCM", "rcm_table", rcm_rows,
      "無資料仍顯示標題列；首列綠底＝設計預覽草稿"
    ),
    .schema_table(
      "pbc_table", "PBC 資料庫", "PBC", "pbc_table", pbc_rows,
      "可勾「僅顯示側邊欄循環」；複選列匯出樣本需求 xlsx"
    ),
    .schema_table(
      "lib_table", "範本庫摘要", "範本庫", "lib_table", lib_rows,
      "filter_library() 後輸出"
    ),
    .schema_table(
      "param_table", "參數庫", "參數庫", "param_table", param_rows,
      "filter_parameter_store() 後輸出；頂部篩選列"
    ),
    .schema_table(
      "control_table", "控制點清單", "風險控制點設計", "control_table", ctrl_rows,
      "僅摘要；選列顯示 detailed_description"
    ),
    .schema_table(
      "interview_table", "訪談題綱預覽", "訪談問項設計", "interview_table", iv_rows,
      paste0("預覽隱藏：", paste(iv_hidden, collapse = "、"))
    ),
    .schema_table(
      "csa_table", "CSA 測試步驟", "控制點測試設計", "csa_table", csa_rows,
      "僅已定稿控制點；含情境組欄"
    ),
    .schema_table(
      "gap_table", "設計缺漏", "風險控制點設計", "gap_table", gap_rows,
      "detect_gaps_many(controls())"
    )
  )
}

table_schemas_card_ui <- function(version = app_version_label()) {
  tables <- table_schema_registry()
  blocks <- lapply(tables, function(tbl) {
    rows <- tbl$rows
    schema_tbl <- tags$table(
      class = "table table-sm table-bordered table-schema-table mb-2",
      tags$thead(
        tags$tr(
          tags$th("順序"), tags$th("欄位"), tags$th("來源"), tags$th("備註")
        )
      ),
      tags$tbody(
        lapply(rows, function(r) {
          tags$tr(
            tags$td(as.character(r$ord %||% "—")),
            tags$td(tags$code(r$col %||% "—")),
            tags$td(r$source %||% "—"),
            tags$td(r$note %||% "—")
          )
        })
      )
    )
    tags$div(
      class = "table-schema-section mb-3",
      tags$h6(
        class = "fw-bold mb-1",
        sprintf("%s ", tbl$title),
        tags$span(class = "badge bg-light text-dark", tbl$table_id)
      ),
      tags$p(
        class = "small text-muted mb-1",
        sprintf("分頁：%s｜DT：%s｜%s",
                tbl$page %||% "—", tbl$dt_output %||% "—", tbl$notes %||% "—")
      ),
      schema_tbl
    )
  })
  card(
    class = "table-schema-card",
    card_header(
      "高權：各表格 SCHEMA 設定",
      tags$span(class = "badge bg-secondary ms-2", sprintf("v%s", version))
    ),
    tags$p(
      class = "small text-muted mb-2",
      "本區僅高權可見。調整表格欄位時請同步更新 ",
      tags$code("R/table_schemas.R"),
      " 與 ",
      tags$code("app.R"),
      " 內對應 ",
      tags$code("renderDT"),
      "。"
    ),
    blocks
  )
}
