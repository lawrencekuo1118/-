# global.R — shared setup for GGplaystore2021
# Restored from the deployed app:
# https://hopesmasher1118.shinyapps.io/GGplaystore2021/

packages <- c(
  "shiny", "shinydashboard", "dplyr", "ggplot2", "DT",
  "tidyr", "scales", "readr"
)

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  library(pkg, character.only = TRUE)
}

options(scipen = 20)

data_path <- file.path("data", "playstore.csv.gz")
if (!file.exists(data_path)) {
  stop(
    "Missing data/playstore.csv.gz. Run prepare_data.R first ",
    "(or see README.md)."
  )
}

playstore <- read_csv(
  data_path,
  col_types = cols(
    App.Name = col_character(),
    Category = col_character(),
    Price = col_double(),
    Install.Count = col_double(),
    Rating = col_double(),
    Rating.Count = col_double(),
    Size.MB = col_double(),
    FreeOrNot = col_logical(),
    Editors.Choice = col_logical(),
    Released.Date = col_date(),
    Last.Updated = col_date(),
    Content.Rating = col_character(),
    Ad.Supported = col_logical(),
    In.App.Purchases = col_logical(),
    Install.Level = col_character(),
    Size = col_character(),
    Performance = col_double(),
    Exp.Revenue = col_double()
  ),
  show_col_types = FALSE
)

playstore <- playstore %>%
  mutate(
    Rating = ifelse(is.na(Rating), 0, Rating),
    Rating.Count = ifelse(is.na(Rating.Count), 0, Rating.Count),
    Performance = Rating * Install.Count,
    Exp.Revenue = Price * Install.Count,
    Category = factor(Category)
  )

category_choices <- sort(unique(as.character(playstore$Category)))

slider_max <- list(
  rating = 5,
  price = max(playstore$Price, na.rm = TRUE),
  install.count = max(playstore$Install.Count, na.rm = TRUE),
  performance = max(playstore$Performance, na.rm = TRUE),
  rating.count = max(playstore$Rating.Count, na.rm = TRUE),
  app_size = max(playstore$Size.MB, na.rm = TRUE)
)
