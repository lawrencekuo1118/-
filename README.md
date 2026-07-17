# Group6 — Ta Feng Grocery Analytics (R)

This repository contains a group data-mining project and course exercises built
with R/RMarkdown, plus an upgraded **image title scraper** toolkit.

## Project purpose

The main deliverable is the **Ta Feng Grocery** analysis in `final/`, which
focuses on:

- customer behavior feature engineering (RFM-style variables),
- customer segmentation with k-means,
- purchase propensity modeling (logistic regression),
- purchase amount modeling (linear regression on transformed features),
- turning model outputs into business-facing customer insights.

Other folders (`unit03`, `unit06`, `unit09`, `unit12`, `unit13`, `midterm`)
contain coursework notebooks and supporting files.

## Repository structure

- `final/` — final project notebooks, data, and rendered HTML reports
- `midterm/` — midterm reports and visualizations
- `unit*/` — assignment notebooks and teaching materials
- `image-title-scraper/` — browser + Python toolkit for native image-title extraction
- `Group6.Rproj` — RStudio project file

## How to run

1. Open `Group6.Rproj` in RStudio.
2. Install required packages referenced by each `.Rmd` file.
3. Knit the desired RMarkdown document (for example,
   `final/group6_final_1.Rmd` or `final/group6_final_2.Rmd`).

## Image title scraper

See [`image-title-scraper/README.md`](./image-title-scraper/README.md).

Browser console extractor (v5) mines native image titles (Bing metadata, Google
`/imgres` params, `a[title]`, alt/caption fallbacks) with deep discovery
(`srcset` / lazy attrs, Shadow DOM), collects during scroll for virtualized
galleries, and exports JSON/CSV manifests; Python downloads files concurrently
with those titles as filenames, sniffing real types from magic bytes (avoids
Chrome `.crdownload` stalls).

## Upgrade notes

- R Markdown reproducibility: removed a machine-specific dataset path and made
  package-loading/setup logic safer for clean environments.
- Image title scraper v5: control panel, deep media discovery, Google Images
  path, canvas download fallback, and a Python downloader with Referer /
  `--skip-existing` / parallel workers.
