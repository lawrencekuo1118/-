# prepare_data.R
# Build data/playstore.csv.gz from Gautham Prakash's Google Play Store dataset
# (June 2021 scrape; matches slider maxima of the live shinyapps.io app).
#
# Source: https://github.com/gauthamp10/Google-Playstore-Dataset
# Kaggle: https://www.kaggle.com/datasets/gauthamp10/google-playstore-apps
#
# Cleaning matches the live app numeric slider ceilings:
#   Price max 399.99
#   Install.Count max 6,156,518,915 (Google TV)
#   Performance max 24,626,075,660
#   Rating.Count max 120,206,190 (Instagram)
#   Size.MB max 985

args <- commandArgs(trailingOnly = TRUE)
force <- "--force" %in% args

out_dir <- "data"
out_file <- file.path(out_dir, "playstore.csv.gz")
if (file.exists(out_file) && !force) {
  message("Already exists: ", out_file, " (pass --force to rebuild)")
  quit(save = "no", status = 0)
}

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create("data/raw", showWarnings = FALSE, recursive = TRUE)

part_url <- paste0(
  "https://raw.githubusercontent.com/gauthamp10/Google-Playstore-Dataset/",
  "main/dataset/Part1.csv.tar.gz"
)
raw_tar <- "data/raw/Part1.csv.tar.gz"
raw_csv <- "data/raw/Part1.csv"

if (!file.exists(raw_csv)) {
  message("Downloading Part1.csv.tar.gz ...")
  download.file(part_url, raw_tar, mode = "wb")
  untar(raw_tar, exdir = "data/raw")
}

for (pkg in c("readr", "dplyr")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

library(readr)
library(dplyr)

size_to_mb <- function(x) {
  x <- trimws(as.character(x))
  out <- rep(NA_real_, length(x))
  varies <- tolower(x) == "varies with device"
  out[varies] <- 0
  m <- regexec("^([0-9.]+)([kKmMgG])$", x)
  parts <- regmatches(x, m)
  ok <- vapply(parts, length, integer(1)) > 0 & !varies
  if (any(ok)) {
    vals <- vapply(parts[ok], function(p) as.numeric(p[2]), numeric(1))
    units <- vapply(parts[ok], function(p) toupper(p[3]), character(1))
    mb <- vals
    mb[units == "K"] <- vals[units == "K"] / 1024
    mb[units == "G"] <- NA_real_
    out[ok] <- mb
  }
  out
}

parse_play_date <- function(x) {
  # Dataset dates look like "Feb 26, 2020"
  as.Date(x, format = "%b %d, %Y")
}

message("Reading ", raw_csv, " ...")
raw <- read_csv(raw_csv, show_col_types = FALSE)

playstore <- raw %>%
  filter(Price <= 399.99) %>%
  mutate(
    Size.MB = size_to_mb(Size),
    Install.Count = `Maximum Installs`
  ) %>%
  filter(
    !is.na(Size.MB),
    Size.MB <= 985,
    !is.na(Install.Count),
    Install.Count <= 6156518915
  ) %>%
  transmute(
    App.Name = `App Name`,
    Category,
    Price = as.numeric(Price),
    Install.Count = as.numeric(Install.Count),
    Rating = as.numeric(Rating),
    Rating.Count = as.numeric(coalesce(`Rating Count`, 0)),
    Size.MB = as.numeric(Size.MB),
    FreeOrNot = as.logical(Free),
    Editors.Choice = as.logical(`Editors Choice`),
    Released.Date = parse_play_date(Released),
    Last.Updated = parse_play_date(`Last Updated`),
    Content.Rating = `Content Rating`,
    Ad.Supported = as.logical(`Ad Supported`),
    In.App.Purchases = as.logical(`In App Purchases`),
    Install.Level = Installs,
    Size = as.character(Size),
    Performance = Rating * Install.Count,
    Exp.Revenue = Price * Install.Count
  ) %>%
  filter(!is.na(App.Name), !is.na(Category))

message(
  "Rows: ", nrow(playstore),
  " | max Install.Count=", max(playstore$Install.Count),
  " | max Performance=", max(playstore$Performance),
  " | max Rating.Count=", max(playstore$Rating.Count),
  " | max Size.MB=", max(playstore$Size.MB),
  " | max Price=", max(playstore$Price)
)

write_csv(playstore, out_file)
message("Wrote ", out_file)
