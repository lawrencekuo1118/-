#!/usr/bin/env Rscript
# deploy.R — deploy Godamn SOX to shinyapps.io
#
# Required environment variables (https://www.shinyapps.io/admin/#/tokens):
#   SHINYAPPS_ACCOUNT  (default: hopesmasher1118)
#   SHINYAPPS_APPNAME  (default: godamn-sox)
#   SHINYAPPS_TOKEN
#   SHINYAPPS_SECRET
#
# Usage:
#   export SHINYAPPS_TOKEN="..."
#   export SHINYAPPS_SECRET="..."
#   Rscript deploy.R

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop("Package 'rsconnect' is required. Install it or run via GitHub Actions setup-r-dependencies.",
       call. = FALSE)
}

library(rsconnect)

account <- Sys.getenv("SHINYAPPS_ACCOUNT", unset = "hopesmasher1118")
app_name <- Sys.getenv("SHINYAPPS_APPNAME", unset = "godamn-sox")
token <- Sys.getenv("SHINYAPPS_TOKEN", unset = "")
secret <- Sys.getenv("SHINYAPPS_SECRET", unset = "")

if (!nzchar(token) || !nzchar(secret)) {
  stop(
    "Missing shinyapps.io credentials.\n",
    "Set SHINYAPPS_TOKEN and SHINYAPPS_SECRET ",
    "(from https://www.shinyapps.io/admin/#/tokens), then re-run:\n",
    "  Rscript deploy.R\n",
    call. = FALSE
  )
}

rsconnect::setAccountInfo(
  name = account,
  token = token,
  secret = secret
)

message("Deploying '", app_name, "' to account '", account, "' ...")

rsconnect::deployApp(
  appDir = ".",
  appName = app_name,
  account = account,
  server = "shinyapps.io",
  launch.browser = FALSE,
  forceUpdate = TRUE
)

message("Deploy complete: https://", account, ".shinyapps.io/", app_name, "/")
