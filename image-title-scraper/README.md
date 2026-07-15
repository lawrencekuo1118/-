# Image Title Scraper (v5)

Browser + Python toolkit that finds images/videos on a page, mines their **native titles**, and downloads them with meaningful filenames.

Based on the optimization discussion in the Gemini share ([圖片標題抓取器優化建議](https://share.gemini.google/iIuifAx77lif)).

## Purpose

Earlier console-only scrapers often failed to recover real titles because they assumed a single DOM path (e.g. `li` → `.infopt a`). Downloads also frequently stalled as Chrome `.crdownload` files when many cross-origin `a.download` clicks were fired.

This toolkit:

1. **Scores multiple title sources** (Bing `.iusc` metadata, Google `/imgres` params, parent `a[title]`, card containers, `figcaption`, `data-*`, `alt`/`title`).
2. **Exports a JSON (and CSV) manifest** from the browser.
3. **Downloads with Python** so filenames are real image/video extensions (no `.crdownload` rename fights).

## What's new in v5

Browser extractor:

- **Incremental collection during scrolling** — virtualized galleries (Bing/Google) unload offscreen cards; v4 scanned only after scrolling finished and silently lost those items. v5 harvests on every scroll round.
- **Best-resolution source resolution** — picks the widest `srcset` / `<picture>` candidate and honors lazy-load attributes (`data-src`, `data-lazy-src`, `data-original`, …) instead of the rendered thumbnail.
- **Google Images adapter** — parses `/imgres?imgurl=…` anchors for the true full-size URL plus `imgrefurl` referer.
- **Shadow DOM + same-origin iframe traversal** and **CSS `background-image` harvesting**.
- **Size filter** (`minImageSize`, default 80 px) to skip icons/sprites/trackers, plus `maxItems` cap and `maxScrollRounds` safety cap for infinite feeds.
- **Safe URL normalization** — strips tracker params (`utm_*`, `gclid`, `fbclid`, `msclkid`, `mc_*`) while preserving resize and quality parameters required by signed CDN URLs.
- **Per-item `referer` + dimensions** included in the manifest (many CDNs reject referer-less requests); manifest also carries `schemaVersion` and image/video `stats`.
- **Runtime config override** via `window.__SCRAPER_CONFIG__` and an emergency stop via `window.__SCRAPER_STOP__ = true` during in-browser downloads.

Python downloader:

- **Concurrent downloads** (`--workers`, default 4) with a **per-host politeness delay** so parallelism never hammers one server.
- **Magic-byte sniffing** (JPEG/PNG/GIF/WebP/AVIF/HEIC/BMP/SVG/MP4/WebM) so extensions match actual bytes even when servers send wrong `Content-Type`.
- **Sends the manifest `referer`** per item — fixes 403s from hotlink-protected CDNs.
- **Atomic writes** through `.part` temp files (no half-written files after a crash).
- **Verified resume mode** is enabled by default and checks URL, filename, and byte size; `--min-bytes` rejects small block pages.
- **Honors `Retry-After`** on HTTP 429/503.
- Writes **`report.csv`** and unique, re-runnable **`failures-*.json`** manifests.

## Quick start

### 1) Extract titles in the browser

1. Open the target page (for Bing/Google: the **image results list**, not the detail viewer).
2. DevTools → **Console**.
3. (Optional) tweak behavior first, e.g.:

```javascript
window.__SCRAPER_CONFIG__ = { minImageSize: 150, maxItems: 200 };
```

4. Paste [`browser-extractor.js`](./browser-extractor.js) and run it.
5. Choose `export` when prompted (default). The JSON is copied to your clipboard and a CSV manifest is downloaded.
6. Save the clipboard JSON as `manifest.json` (also available as `window.__IMAGE_TITLE_MANIFEST__`).

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
python download.py manifest.json --workers 8 --delay 0.3
python download.py manifest.json --limit 30 --offset 30
python download.py manifest.json --max-mb 100 --min-bytes 512
python download.py downloads/failures-*.json
```

All flags:

| Flag | Default | Meaning |
|------|--------:|---------|
| `--out` | `downloads` | Output directory |
| `--workers` | `4` | Concurrent downloads (1 = sequential) |
| `--delay` | `0.6` | Seconds between requests **to the same host** |
| `--timeout` | `20` | Per-request timeout |
| `--retries` | `2` | Retries per item |
| `--limit` / `--offset` | `0` / `0` | Item windowing |
| `--min-bytes` | `512` | Reject bodies smaller than this (block pages, pixels) |
| `--overwrite` | off | Replace verified existing files instead of resuming |
| `--max-mb` | `0` | Maximum size per file (`0` = unlimited) |
| `--failures` | automatic | Override the failure-manifest path |

## Title priority (scoring)

| Score | Source |
|------:|--------|
| 100 | Bing `m` JSON field `t` / `title` |
| 96 | `figcaption`, Google result `aria-label` |
| 95–93 | Parent / card `a[title]`, `.infopt a`, `img data-title` |
| 92–86 | `img` alt / title / aria-label / `data-*` caption-like keys |
| 80 | Nearby headings / `[class*=title]` / `[class*=caption]` |
| 55–72 | Link text, clean URL filename (non-`OIP`/hash noise) |

Garbage titles (`image`, `loading`, pure digits, hex ids, bare extensions, length < 3) are discarded.

## Browser-only download (optional)

In the console prompt, choose `download` or `both`.

Before running:

- Allow **multiple automatic downloads** if Chrome blocks them.
- Turn **OFF** “Ask where to save each file before downloading”.
- Prefer batches of ~20–30 on slower disks.
- Abort anytime with `window.__SCRAPER_STOP__ = true`.

CORS-blocked items open in a rescue gallery; prefer the Python path for those.

## Files

| File | Role |
|------|------|
| `browser-extractor.js` | Console extractor + title scoring + manifest export (JSON + CSV) |
| `download.py` | Concurrent HTTP downloader with native-title filenames, sniffed extensions, reports |
| `requirements.txt` | Python deps |
| `tests/test_download.py` | Downloader unit tests |

## Notes

- Respect site terms of use and copyright; this tool is for pages you are allowed to archive.
- Bing/Google DOM class names can change; if collection returns 0, inspect `.iusc` / `m` attributes (Bing) or `/imgres` anchors (Google) and update selectors.
