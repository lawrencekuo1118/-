# 應用程式資料庫持久化（PBC／範本庫／參數庫）
# 所有寫入必須落盤至 data/，並納入 Git 以便部署與 shinyapps 同步。

verify_persist_file <- function(path) {
  path <- as.character(path %||% "")
  if (!nzchar(path) || !file.exists(path)) return(FALSE)
  isTRUE(file.info(path)$size > 0)
}

persist_pbc_to_disk <- function(registry, path_csv, path_json = NULL) {
  save_pbc_registry(registry, path_csv, path_json)
  if (!verify_persist_file(path_csv)) {
    stop("PBC 資料庫未能寫入磁碟：", path_csv)
  }
  invisible(registry)
}

persist_library_to_disk <- function(library, path_json, path_csv = NULL) {
  save_control_library(library, path_json, path_csv)
  if (!verify_persist_file(path_json)) {
    stop("範本庫未能寫入磁碟：", path_json)
  }
  invisible(library)
}

persist_parameters_to_disk <- function(df, path_json) {
  save_parameter_store(df, path_json)
  if (!verify_persist_file(path_json)) {
    stop("參數庫未能寫入磁碟：", path_json)
  }
  invisible(df)
}

app_database_manifest <- function(data_dir) {
  list(
    pbc = list(
      label = "PBC 命名庫",
      primary = file.path(data_dir, "pbc_registry.csv"),
      secondary = file.path(data_dir, "pbc_registry.json")
    ),
    library = list(
      label = "範本庫",
      primary = file.path(data_dir, "control_library.json"),
      secondary = file.path(data_dir, "control_library.csv")
    ),
    parameters = list(
      label = "參數庫",
      primary = file.path(data_dir, "parameter_store.json"),
      secondary = NA_character_
    )
  )
}

database_persist_status <- function(manifest) {
  lapply(manifest, function(entry) {
    paths <- c(entry$primary, entry$secondary)
    paths <- paths[!is.na(paths) & nzchar(paths) & file.exists(paths)]
    if (!length(paths)) {
      return(list(
        label = entry$label,
        ok = FALSE,
        paths = character(),
        mtime = NA,
        bytes = 0L
      ))
    }
    info <- file.info(paths)
    list(
      label = entry$label,
      ok = TRUE,
      paths = paths,
      mtime = max(info$mtime, na.rm = TRUE),
      bytes = as.integer(sum(info$size, na.rm = TRUE))
    )
  })
}

format_database_persist_status <- function(status_list) {
  lines <- vapply(status_list, function(st) {
    if (!isTRUE(st$ok)) {
      return(sprintf("%s：尚未寫入", st$label))
    }
    sprintf(
      "%s：%s（%s KB）",
      st$label,
      format(st$mtime, "%Y-%m-%d %H:%M:%S"),
      format(round(st$bytes / 1024), big.mark = ",")
    )
  }, character(1))
  paste(lines, collapse = "\n")
}

flush_all_app_databases <- function(pbc_reg, library, param_df,
                                    pbc_path_csv, pbc_path_json,
                                    lib_path_json, lib_path_csv,
                                    param_path_json) {
  persist_pbc_to_disk(pbc_reg, pbc_path_csv, pbc_path_json)
  persist_library_to_disk(library, lib_path_json, lib_path_csv)
  persist_parameters_to_disk(param_df, param_path_json)
  invisible(TRUE)
}
