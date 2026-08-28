# Process-level cache for large on-disk catalogs (shared across Shiny sessions).

.app_data_cache <- new.env(parent = emptyenv())

cache_file_key <- function(path) {
  path <- as.character(path %||% "")
  if (!nzchar(path) || !file.exists(path)) return(NA_character_)
  info <- file.info(path)
  paste0(normalizePath(path), "|", info$mtime, "|", info$size)
}

get_cached_file_data <- function(cache_name, path, loader) {
  key <- cache_file_key(path)
  if (is.na(key)) return(loader(path))
  key_name <- paste0(cache_name, "_key")
  if (!is.null(.app_data_cache[[key_name]]) &&
      identical(.app_data_cache[[key_name]], key)) {
    return(.app_data_cache[[cache_name]])
  }
  value <- loader(path)
  .app_data_cache[[cache_name]] <- value
  .app_data_cache[[key_name]] <- key
  value
}

invalidate_cached_file_data <- function(cache_name) {
  keys <- c(cache_name, paste0(cache_name, "_key"))
  keys <- keys[keys %in% ls(.app_data_cache)]
  if (length(keys)) {
    rm(list = keys, envir = .app_data_cache, inherits = FALSE)
  }
  invisible(NULL)
}
