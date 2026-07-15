# Group6

Course materials (R units / midterm / final) plus an upgraded **image title scraper** toolkit.

## Image title scraper

See [`image-title-scraper/README.md`](./image-title-scraper/README.md).

Browser console extractor mines native image titles (Bing metadata, `a[title]`, alt/caption fallbacks) and exports a JSON manifest; Python downloads files with those titles as filenames (avoids Chrome `.crdownload` stalls).
