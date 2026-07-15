# Image Title Scraper (v5)

Browser + Python toolkit that finds images/videos on a page, mines their **native titles**, and downloads them with meaningful filenames.

Based on the optimization discussion in the Gemini share ([圖片標題抓取器優化建議](https://share.gemini.google/iIuifAx77lif)).

## Purpose

Earlier console-only scrapers often failed to recover real titles because they assumed a single DOM path (e.g. `li` → `.infopt a`). Downloads also frequently stalled as Chrome `.crdownload` files when many cross-origin `a.download` clicks were fired.

This version:

1. **Scores multiple title sources** (Bing `.iusc` metadata, parent `a[title]`, card containers, `figcaption`, `data-*`, `alt`/`title`).
2. **Discovers modern media** in `srcset`, lazy-load attributes, open shadow roots, inline CSS backgrounds, video sources, and Open Graph/Twitter metadata.
3. **Exports a versioned JSON manifest** with source pages, dimensions, thumbnails, and title provenance.
4. **Downloads concurrently and atomically with Python**, resumes existing files, rejects HTML error pages, and writes a retryable failure manifest.

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
python download.py manifest.json --out downloads --workers 4
```

Useful flags:

```bash
python download.py manifest.json --limit 30 --delay 0.8
python download.py manifest.json --offset 30 --limit 30
python download.py manifest.json --max-mb 100 --retries 4
python download.py manifest.json --overwrite
```

Downloads are written to `.part` files and renamed only after completion. Re-running
the command skips completed files by default. Failures are saved to
`downloads/failures.json`; that file is itself a valid manifest and can be passed
back to `download.py` after fixing connectivity or access.

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
Hash-like and URL-only titles are also ignored, and Windows-reserved filenames
are made safe.

## Browser discovery

The extractor scans:

- Bing result metadata for original image URLs, titles, and source pages.
- `<img>`, `<video>`, and `<source>`, including lazy attributes and the largest
  `srcset` candidate.
- Media inside open shadow DOM.
- Inline CSS `background-image` values.
- `og:image`, `twitter:image`, and `link[rel=image_src]` page metadata.

Auto-scroll stops after 60 steps even on endless feeds. Original URLs are
preserved instead of stripping resize query parameters, because those parameters
can be required by signed CDN links.

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
| `tests/test_download.py` | Downloader unit tests |

## Downloader options

| Option | Meaning |
|---|---|
| `--workers N` | Concurrent downloads (default `4`). |
| `--retries N` | Retries per item with exponential backoff. |
| `--overwrite` | Replace matching files instead of resuming. |
| `--max-mb N` | Reject an individual response larger than this limit. |
| `--failures PATH` | Choose the JSON failure-report path. |

## Notes

- Respect site terms of use and copyright; this tool is for pages you are allowed to archive.
- Bing DOM class names can change; if collection returns 0, inspect `.iusc` / `m` attributes and update selectors.
