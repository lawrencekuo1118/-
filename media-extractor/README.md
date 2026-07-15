# Page Media Extractor

A Chrome/Edge Manifest V3 extension that discovers images and videos used by the
current page and sends selected resources to the browser's download manager.
It is an upgrade of the shared developer-console script, with the browser APIs
needed for reliable cross-origin downloads and filename handling.

## What it scans

- Standard `img`, `video`, `source`, and image-input elements
- `srcset`, video posters, and common lazy-load attributes
- Linked media files
- Inline and computed CSS image URLs
- Bing Images metadata
- Media URLs embedded in the rendered HTML
- Media observed by the Resource Timing API

Results are deduplicated by absolute URL. Titles are selected from captions,
accessible labels, element/link titles, headings, Bing metadata, and URL
filenames. Downloads use sanitized, unique filenames under
`Downloads/Page Media/<page title>/`.

## Install in Chrome or Edge

1. Open `chrome://extensions` or `edge://extensions`.
2. Enable **Developer mode**.
3. Choose **Load unpacked** and select this `media-extractor` directory.
4. Open a page and click the extension. Select **Scroll to load lazy media**
   when the page loads content only while scrolling.
5. Review the result list and click **Download selected**.

The browser may ask once whether the site can download multiple files. Keep
Safe Browsing enabled; the extension does not need it disabled.

## Why an extension

A script pasted into a page's console inherits that page's CORS restrictions.
When a cross-origin `fetch` fails, clicking an external URL can navigate or open
a tab, and extending `URL.revokeObjectURL()` delays does not control when a
browser finishes a network download. This extension instead uses:

- temporary `activeTab` access only after the user opens the popup;
- the `downloads` API for cross-origin download requests;
- a persistent, one-at-a-time queue that survives service-worker restarts;
- the browser's conflict handling and safety checks;
- completion and interruption reporting from the download manager.

An active `.crdownload` file is Chrome/Edge's normal temporary file. If it
remains after the browser reports a failure, inspect the download manager for
the actual network, permission, disk-space, or security error. Renaming that
partial file does not complete it.

## Limitations

- Browser-internal pages, extension stores, and some sandboxed frames cannot be
  inspected.
- Encrypted streams, DRM media, Media Source blobs, and URLs generated only
  after future user interaction are not downloadable page resources.
- A discovered manifest such as `.m3u8` is downloaded as a manifest; combining
  its segments into a playable file requires a dedicated media tool.
- Only download content you are authorized to retain.

## Development

No dependencies are required.

```sh
npm test
npm run check
```
