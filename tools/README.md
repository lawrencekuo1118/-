# Universal Media Extractor

A single-file, paste-into-the-console browser utility that finds and downloads
every image and video resource on the current web page.

## What this is / where it came from

This tool is a consolidated, upgraded rewrite of a series of browser-console
scripts that were developed iteratively (a "media / image-title grabber").
Across the original iterations the script kept gaining a good idea and then
losing an older one in the next rewrite — for example, later "brute force,
download everything" versions dropped the smart filename-inference logic that
earlier versions had, and the download strategy oscillated between opening
popup tabs and silently skipping files.

`media-extractor.js` re-unifies all of the good behaviors into one coherent
script.

## What it does

1. **Auto-scroll** the page to trigger lazy-loaded content (optional, on by default).
2. **Discover** media from every practical source:
   - `<img>`, `<video>`, `<source>` elements, including `srcset` (picks the highest-resolution candidate).
   - Lazy-load attributes: `data-src`, `data-original`, `data-lazy-src`, `data-url`.
   - CSS `background-image` on any element.
   - **Shadow DOM** (walks open shadow roots).
   - A **brute-force regex sweep** of the raw HTML/JSON for URLs hidden in
     scripts and data blobs.
   - A **Bing Image Search fast-path** that reads the original-image metadata
     from each result card's `.iusc` `m` JSON.
3. **Name files intelligently** via a ranked scoring engine (figure captions,
   link titles, `alt`/`title`, `aria-label`, `data-*`, headings, and finally
   the URL filename), filtering out junk names like `image`, `thumbnail`, or
   hash-like strings.
4. **Download safely**:
   - Fetch as a Blob to preserve original bytes/format.
   - Fall back to a canvas re-encode when the server allows anonymous `<img>`
     loads but blocks `fetch` (CORS).
   - **Never opens popup tabs.** Files that cannot be fetched silently are
     listed in a manual "rescue gallery" instead.
5. **Avoid `.crdownload` residue** through bounded concurrency, per-file and
   per-batch cool-downs, and delayed `URL.revokeObjectURL` so the browser has
   time to finish writing to disk.
6. **Export a URL manifest** (`media_manifest.txt`) so you can hand the list to
   an external download manager (JDownloader, Tab Save, IDM, etc.).

## Upgrades over the original scripts

- Unified filename scoring engine **and** brute-force discovery in one script
  (previous versions had one or the other).
- Highest-resolution `srcset` selection and resize-parameter stripping.
- Shadow-DOM traversal for modern component-based sites.
- A real on-page **control panel** with a scan step, live progress bar, and a
  rescue gallery — instead of `alert()`/`confirm()` and console-only output.
- Bounded-concurrency worker pool (configurable) rather than a strict
  one-at-a-time loop, while still throttling to avoid disk-IO thrashing.
- Idempotent: re-running just reopens the existing panel instead of stacking
  duplicate listeners/overlays.
- No `window.open`, no `target="_blank"` fallback — genuinely zero-tab.

## Usage

1. Open the target page in Chrome, Edge, or Firefox.
2. Open DevTools (**F12**) and switch to the **Console** tab.
3. Paste the entire contents of [`media-extractor.js`](./media-extractor.js) and
   press **Enter**.
4. Use the panel in the top-right corner: **Scan page**, then **Download all**
   (or **Export URLs**).

### Bookmarklet

You can also wrap the file in `javascript:(function(){ ...file contents... })();`
and save it as a bookmark for one-click use.

## Configuration

Tweak the `CONFIG` object at the top of the script:

| Key | Meaning |
| --- | --- |
| `scrollDelay` | Delay between auto-scroll steps (ms). |
| `concurrency` | Simultaneous downloads. |
| `downloadDelay` / `batchDelay` | Throttling between files / batches (ms). |
| `revokeDelay` | Delay before releasing a blob URL — raise this if you still see `.crdownload` files. |
| `minImageSize` | Ignore rendered images smaller than this (px) to skip icons/tracking pixels. |

## Notes & limitations

- Some servers enforce strict CORS and cannot be downloaded from the page at
  all; those appear in the rescue gallery for manual saving or manifest export.
- If you download a very large number of files, disable your browser's
  "Ask where to save each file before downloading" setting first.
- Use responsibly and only on content you have the right to download.
