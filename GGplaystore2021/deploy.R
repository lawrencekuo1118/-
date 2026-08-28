# deploy.R — deploy GGplaystore2021 to shinyapps.io
#
# Required environment variables (from https://www.shinyapps.io/admin/#/tokens):
#   SHINYAPPS_ACCOUNT  (default: hopesmasher1118)
#   SHINYAPPS_TOKEN
#   SHINYAPPS_SECRET
#
# Usage:
#   export SHINYAPPS_TOKEN="..."
#   export SHINYAPPS_SECRET="..."
#   Rscript deploy.R

.libPaths(c("/home/ubuntu/R/library", .libPaths()))

for (pkg in c("rsconnect", "shiny", "shinydashboard", "dplyr", "ggplot2",
              "DT", "tidyr", "scales", "readr")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

library(rsconnect)

account <- Sys.getenv("SHINYAPPS_ACCOUNT", unset = "hopesmasher1118")
token <- Sys.getenv("SHINYAPPS_TOKEN", unset = "")
secret <- Sys.getenv("SHINYAPPS_SECRET", unset = "")
app_name <- Sys.getenv("SHINYAPPS_APPNAME", unset = "GGplaystore2021")

if (!nzchar(token) || !nzchar(secret)) {
  stop(
    "Missing shinyapps.io credentials.\n",
    "Set SHINYAPPS_TOKEN and SHINYAPPS_SECRET ",
    "(from https://www.shinyapps.io/admin/#/tokens), then re-run:\n",
    "  Rscript deploy.R\n",
    call. = FALSE
  )
}

# Register / refresh account credentials for this machine
rsconnect::setAccountInfo(
  name = account,
  token = token,
  secret = secret
)

message("Deploying app '", app_name, "' to account '", account, "' ...")

rsconnect::deployApp(
  appDir = ".",
  appFiles = c(
    "global.R",
    "ui.R",
    "server.R",
    "data/playstore.csv.gz",
    "README.md"
  ),
  appName = app_name,
  account = account,
  server = "shinyapps.io",
  launch.browser = FALSE,
  forceUpdate = TRUE,
  logLevel = "verbose"
)

message(
  "Deploy complete: https://", account, ".shinyapps.io/", app_name, "/"
)
