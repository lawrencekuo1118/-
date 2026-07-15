# Image Title Scraper (v5)

Browser + Python toolkit that finds images/videos on a page, mines their **native titles**, and downloads them with meaningful filenames.

Based on the optimization discussion in the Gemini share ([圖片標題抓取器優化建議](https://share.gemini.google/iIuifAx77lif)).

## Purpose

Earlier console-only scrapers often failed to recover real titles because they assumed a single DOM path (e.g. `li` → `.infopt a`). Downloads also frequently stalled as Chrome `.crdownload` files when many cross-origin `a.download` clicks were fired.

This upgrade:

1. **Scores multiple title sources** (Bing `.iusc` metadata, parent `a[title]`, card containers, `figcaption`, `data-*`, `alt`/`title`).
2. **Prefers high-resolution media URLs** from `srcset` / `<picture><source>`.
3. **Optionally scans CSS `background-image` cards** for sites that lazy-render media in divs.
4. **Exports a JSON manifest** from the browser.
5. **Downloads with Python** so filenames are real image/video extensions (no `.crdownload` rename fights).

## Quick start

### 1) Extract titles in the browser

1. Open the target page (for Bing: the **image results list**, not the detail viewer).
2. DevTools → **Console**.
3. Paste [`browser-extractor.js`](./browser-extractor.js) and run it.
4. Choose `export` when prompted (default).
5. Save the clipboard JSON as `manifest.json`.

### 2) Download with Python

```bash
cd image-title-scraper
python3 -m venv .venv
source .venv/bin/activate   # Windows: .venv\Scripts\activate
pip install -r requirements.txt
python download.py manifest.json --out downloads
```

Useful flags:

```bash
python download.py manifest.json --limit 30 --delay 0.8
python download.py manifest.json --offset 30 --limit 30
```

## Title priority (scoring)

| Score | Source |
|------:|--------|
| 100 | Bing `m` JSON field `t` / `title` |
| 95–93 | Parent / card `a[title]`, `.infopt a` |
| 92–88 | `img` alt / title / aria-label / data-title |
| 96 | `figcaption` |
| 80 | Nearby headings |
| 55–70 | Clean URL filename (non-`OIP` noise) |

Garbage titles (`image`, `loading`, pure digits, length &lt; 3) are discarded.

## URL cleanup and dedupe improvements

- Strips common tracker query params (`utm_*`, `gclid`, `fbclid`, `msclkid`, etc.).
- Removes common resize params (`w`, `h`, `width`, `height`, `q`).
- Canonicalizes URLs before dedupe so thumbnail variants don't create duplicates.
- Drops tiny image elements when dimensions indicate likely trackers/icons.

## Browser-only download (optional)

In the console prompt, choose `download` or `both`.

Before running:

- Allow **multiple automatic downloads** if Chrome blocks them.
- Turn **OFF** “Ask where to save each file before downloading”.
- Prefer batches of ~20–30 on slower disks.

CORS-blocked items open in a rescue gallery; prefer the Python path for those.

## Files

| File | Role |
|------|------|
| `browser-extractor.js` | Console extractor + title scoring + manifest export |
| `download.py` | Reliable HTTP downloader with native-title filenames |
| `requirements.txt` | Python deps |

## Notes

- Respect site terms of use and copyright; this tool is for pages you are allowed to archive.
- Bing DOM class names can change; if collection returns 0, inspect `.iusc` / `m` attributes and update selectors.
