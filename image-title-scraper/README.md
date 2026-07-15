# Image Title Scraper (v5)

Browser + Python toolkit that finds images/videos on a page, mines their **native titles**, and downloads them with meaningful filenames.

Based on the optimization discussion in the Gemini share ([圖片標題抓取器優化建議](https://share.gemini.google/iIuifAx77lif)), with deep-discovery capabilities from the universal media extractor.

## Purpose

Earlier console-only scrapers often failed to recover real titles because they assumed a single DOM path (e.g. `li` → `.infopt a`). Downloads also frequently stalled as Chrome `.crdownload` files when many cross-origin `a.download` clicks were fired.

This upgrade:

1. **Scores multiple title sources** (Bing `.iusc` / Google metadata, parent `a[title]`, card containers, `figcaption`, `data-*`, `alt`/`title`).
2. **Discovers media deeply** — `srcset`, lazy `data-*`, CSS `background-image`, open Shadow DOM, and an HTML/JSON URL sweep.
3. **Exports a JSON manifest** from an on-page control panel.
4. **Downloads with Python** so filenames are real image/video extensions (no `.crdownload` rename fights).

## Quick start

### 1) Extract titles in the browser

1. Open the target page (for Bing/Google: the **image results list**, not the detail viewer).
2. DevTools → **Console**.
3. Paste [`browser-extractor.js`](./browser-extractor.js) and run it.
4. Use the panel (top-right): **Scan page** → **Export JSON**.
5. Save the downloaded / clipboard JSON as `manifest.json`.

Re-pasting the script reopens the existing panel (idempotent).

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
python download.py manifest.json --skip-existing
python download.py manifest.json --workers 4 --failures failed.json
python download.py failed.json --out downloads --skip-existing
```

`--workers 1` (default) is the polite serial path. Raise workers only when the host tolerates parallel fetches. The downloader sends the manifest `pageUrl` as `Referer` automatically.

## Title priority (scoring)

| Score | Source |
|------:|--------|
| 100–98 | Bing `m` JSON `t` / `title`, Google `pt` / `s` |
| 96 | `figcaption` |
| 95–93 | Parent / card `a[title]`, `.infopt a` |
| 92–88 | `img` alt / title / aria-label / data-title |
| 80 | Nearby headings |
| 55–70 | Clean URL filename (non-`OIP` / non-hash noise) |

Garbage titles (`image`, `loading`, logos/icons, pure digits, hash-like strings, length &lt; 3) are discarded. Tiny rendered images (&lt; 64px) are skipped.

## Discovery sources

| Source | Notes |
|--------|--------|
| Bing `.iusc` `m` JSON | Original URL (`murl`) + native title (`t`) |
| Google Images tiles | Parses embedded `ou` / `pt` metadata when present |
| `<img>` / `<video>` / `<source>` | Includes highest-res `srcset` candidate |
| Lazy attributes | `data-src`, `data-original`, `data-lazy-src`, `data-url`, … |
| CSS `background-image` | Inline styles |
| Open Shadow DOM | Walks `shadowRoot` trees |
| HTML regex sweep | Finds media URLs hidden in scripts / JSON blobs |

## Browser-only download (optional)

In the panel, click **Download** after scanning.

Before running:

- Allow **multiple automatic downloads** if Chrome blocks them.
- Turn **OFF** “Ask where to save each file before downloading”.
- Prefer the **Export JSON → Python** path for large batches or CORS-heavy hosts.

Downloads use bounded concurrency, blob fetch, then a canvas re-encode fallback. CORS-blocked items appear in a rescue gallery; prefer Python for those.

## Files

| File | Role |
|------|------|
| `browser-extractor.js` | Console extractor + title scoring + panel + manifest export |
| `download.py` | Reliable HTTP downloader with native-title filenames |
| `requirements.txt` | Python deps |

## Notes

- Respect site terms of use and copyright; this tool is for pages you are allowed to archive.
- Bing / Google DOM class names can change; if collection returns 0, inspect metadata attributes and update selectors.
- For a download-everything UI without the Python workflow, see also [`../tools/media-extractor.js`](../tools/media-extractor.js).
