# Group6 — Ta Feng Grocery Analytics (R)

This repository contains a group data-mining project and course exercises built
with R/RMarkdown, plus several media-extraction tools.

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
- `media-extractor/` — Chrome/Edge Manifest V3 media-extractor extension
- `tools/` — standalone browser-console media extractor
- `Group6.Rproj` — RStudio project file

## How to run

1. Open `Group6.Rproj` in RStudio.
2. Install required packages referenced by each `.Rmd` file.
3. Knit the desired RMarkdown document (for example,
   `final/group6_final_1.Rmd` or `final/group6_final_2.Rmd`).

## Media extraction tools

The repository currently includes three implementations of the shared
image-title/media downloader concept:

- [`image-title-scraper/`](image-title-scraper/) — a browser extractor that
  exports JSON/CSV manifests plus a concurrent Python downloader. It supports
  native title scoring, virtualized galleries, referers, and file-type
  detection from magic bytes.
- [`media-extractor/`](media-extractor/) — a Chrome/Edge Manifest V3 extension
  that uses browser download APIs and runs only on the user-activated tab.
- [`tools/media-extractor.js`](tools/media-extractor.js) — a standalone script
  that can be pasted into a page's developer console.

See each implementation's README for installation, behavior, and limitations.

## Upgrade notes

- R Markdown reproducibility: removed a machine-specific dataset path and made
  package-loading/setup logic safer for clean environments.
- Image title scraper: multi-source title scoring plus a Python downloader path
  for reliable named downloads.
