#!/usr/bin/env Rscript
# Deploy / run Control Design Assistant v1 locally
root <- if (file.exists("app.R")) {
  normalizePath(".")
} else if (file.exists("control-design-app/app.R")) {
  normalizePath("control-design-app")
} else {
  stop("找不到 control-design-app/app.R")
}
ver <- tryCatch(readLines(file.path(root, "VERSION"), warn = FALSE)[[1]], error = function(e) "dev")
message("Control Design Assistant v", ver, " — ", root)
shiny::runApp(root, launch.browser = TRUE)
