# GGplaystore2021 — Google Playstore APP Analytics Dashboard

Restored R Shiny source for the app originally deployed at:

**https://hopesmasher1118.shinyapps.io/GGplaystore2021/**

Author credit (from the live UI): **LU-LEI KUO**

## What’s included

| File | Role |
|------|------|
| `global.R` | Load packages + `data/playstore.csv.gz` |
| `ui.R` | `shinydashboard` UI (sidebar filters, tabs, value boxes) |
| `server.R` | Reactive filters, plots (`ggplot2`), tables (`DT`) |
| `prepare_data.R` | Rebuild the cleaned dataset from the public 2021 scrape |
| `app.R` | Optional runner / deploy entry hint |
| `data/playstore.csv.gz` | Cleaned app table used by the dashboard |

## Features (matched to the live app)

- Skin `black`, collapsed sidebar, sidebar search form
- Filters: Category, App Rating, Price, Installs, Performance, Rating Counts, Size, Ad.Supported, In.App.Purchases, Free/Paid, Editors Choice
- Value boxes: QUANTITY / AVERAGE RATING / INSTALLATIONS
- Tabs: General View, Basic Information, Value Analysis, Performance
- Statistic Facts: Data Summary + Correlation Heatmap
- Metrics: `Performance = Rating * Install.Count`, `Exp.Revenue = Price * Install.Count`

## How to run

1. Open this folder in RStudio (or setwd to `GGplaystore2021/`).
2. Install packages if needed:

```r
install.packages(c(
  "shiny", "shinydashboard", "dplyr", "ggplot2",
  "DT", "tidyr", "scales", "readr"
))
```

3. Launch:

```r
shiny::runApp(".")
```

## Rebuild data (optional)

```r
# from GGplaystore2021/
source("prepare_data.R")          # skip if data already present
# source("prepare_data.R"); # or: Rscript prepare_data.R --force
```

Data source: [gauthamp10/Google-Playstore-Dataset](https://github.com/gauthamp10/Google-Playstore-Dataset) (Part1), cleaned so numeric slider maxima match the live app (Install.Count 6156518915, Performance 24626075660, Rating.Count 120206190, Size.MB 985, Price 399.99).

## Deploy

```r
rsconnect::deployApp(
  appDir = ".",
  appName = "GGplaystore2021",
  account = "hopesmasher1118"
)
```

## Notes on restoration

The original `.R` sources were not in this repository and are not exposed by shinyapps.io. This tree was reconstructed by inspecting the live app’s HTML (input/output IDs, labels, layout) and interactive behavior, then wiring server logic to those IDs. Plot geometry follows the live charts (axis titles, formulas, and section copy).
