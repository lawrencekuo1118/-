#!/usr/bin/env python3
"""
Image Title Scraper — Python downloader (v5)

Purpose:
  Reliably download media listed in a JSON manifest produced by
  browser-extractor.js, using native titles as filenames.

  This avoids Chrome/Edge `.crdownload` stalls that happen when a
  console script fires many cross-origin `a.download` clicks.

Usage:
  python download.py manifest.json
  python download.py manifest.json --out downloads --delay 0.8 --limit 50
  python download.py manifest.json --workers 4 --skip-existing
  python download.py manifest.json --retries 3 --failures failed.json

Manifest format (from browser-extractor.js):
  {
    "pageUrl": "https://...",
    "items": [
      {"index": 1, "url": "https://...", "title": "Some_Native_Title", "type": "image"}
    ]
  }
"""

from __future__ import annotations

import argparse
import json
import mimetypes
import re
import sys
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path
from typing import Any
from urllib.parse import unquote, urlparse

try:
    import requests
except ImportError:
    print("Missing dependency: requests\n  pip install -r requirements.txt", file=sys.stderr)
    sys.exit(1)


DEFAULT_HEADERS = {
    "User-Agent": (
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
        "AppleWebKit/537.36 (KHTML, like Gecko) "
        "Chrome/120.0.0.0 Safari/537.36"
    ),
    "Accept": "image/avif,image/webp,image/apng,image/*,*/*;q=0.8",
    "Accept-Language": "en-US,en;q=0.9",
}

INVALID_FS_CHARS = re.compile(r'[\\/*?:"<>|\n\r\t]+')
MIME_EXT = {
    "image/jpeg": ".jpg",
    "image/jpg": ".jpg",
    "image/png": ".png",
    "image/gif": ".gif",
    "image/webp": ".webp",
    "image/avif": ".avif",
    "image/svg+xml": ".svg",
    "image/bmp": ".bmp",
    "video/mp4": ".mp4",
    "video/webm": ".webm",
    "video/quicktime": ".mov",
}


def sanitize_filename(text: str, max_len: int = 100) -> str:
    text = INVALID_FS_CHARS.sub("", text or "").strip()
    text = re.sub(r"\s+", "_", text)
    text = text.strip("._")
    if not text:
        text = "image"
    return text[:max_len]


def guess_ext(url: str, content_type: str | None, media_type: str) -> str:
    if content_type:
        ct = content_type.split(";")[0].strip().lower()
        if ct in MIME_EXT:
            return MIME_EXT[ct]
        guessed = mimetypes.guess_extension(ct)
        if guessed:
            return ".jpg" if guessed == ".jpe" else guessed

    path = unquote(urlparse(url).path)
    suffix = Path(path).suffix.lower()
    if suffix in {".jpg", ".jpeg", ".png", ".gif", ".webp", ".avif", ".svg", ".bmp", ".mp4", ".webm", ".mov"}:
        return ".jpg" if suffix == ".jpeg" else suffix

    return ".mp4" if media_type == "video" else ".jpg"


def load_manifest(path: Path) -> tuple[list[dict[str, Any]], dict[str, Any]]:
    data = json.loads(path.read_text(encoding="utf-8"))
    meta: dict[str, Any] = {}
    if isinstance(data, list):
        items = data
    elif isinstance(data, dict) and "items" in data:
        items = data["items"]
        meta = {
            "pageUrl": data.get("pageUrl") or data.get("page_url") or "",
            "pageTitle": data.get("pageTitle") or data.get("page_title") or "",
            "generatedAt": data.get("generatedAt") or "",
            "version": data.get("version"),
        }
    else:
        raise ValueError("Manifest must be a list or an object with an 'items' array")

    normalized = []
    for i, item in enumerate(items, start=1):
        if isinstance(item, str):
            normalized.append({"index": i, "url": item, "title": f"image_{i}", "type": "image"})
            continue
        url = item.get("url") or item.get("murl") or item.get("src")
        if not url:
            continue
        normalized.append(
            {
                "index": int(item.get("index") or i),
                "url": url,
                "title": item.get("title") or item.get("suggestedName") or f"image_{i}",
                "type": item.get("type") or "image",
                "titleSource": item.get("titleSource") or "",
                "source": item.get("source") or "",
                "thumbnail": item.get("thumbnail") or "",
            }
        )
    return normalized, meta


def unique_path(directory: Path, stem: str, ext: str) -> Path:
    candidate = directory / f"{stem}{ext}"
    if not candidate.exists():
        return candidate
    n = 2
    while True:
        candidate = directory / f"{stem}_{n}{ext}"
        if not candidate.exists():
            return candidate
        n += 1


def existing_for_stem(directory: Path, stem: str) -> Path | None:
    """Return an existing file that already matches this numbered title stem."""
    matches = sorted(directory.glob(f"{stem}.*")) + sorted(directory.glob(f"{stem}_*.*"))
    for path in matches:
        if path.is_file() and path.stat().st_size >= 32:
            return path
    return None


def build_headers(page_url: str, item_url: str) -> dict[str, str]:
    headers = dict(DEFAULT_HEADERS)
    referer = page_url or ""
    if not referer:
        try:
            parsed = urlparse(item_url)
            referer = f"{parsed.scheme}://{parsed.netloc}/"
        except Exception:  # noqa: BLE001
            referer = ""
    if referer:
        headers["Referer"] = referer
    return headers


def download_one(
    session: requests.Session,
    item: dict[str, Any],
    out_dir: Path,
    timeout: float,
    page_url: str,
    skip_existing: bool,
) -> tuple[str, Path | None]:
    """
    Returns (status, path) where status is "saved" | "skipped" | "failed".
    Raises on hard download failure so callers can retry.
    """
    url = item["url"]
    index = item["index"]
    title = sanitize_filename(str(item["title"]))
    media_type = item.get("type") or "image"
    stem = f"{index:03d}_{title}"

    if skip_existing:
        existing = existing_for_stem(out_dir, stem)
        if existing is not None:
            return "skipped", existing

    headers = build_headers(page_url, url)
    resp = session.get(url, headers=headers, timeout=timeout, stream=True)
    resp.raise_for_status()

    ext = guess_ext(url, resp.headers.get("Content-Type"), media_type)
    dest = unique_path(out_dir, stem, ext)

    with dest.open("wb") as fh:
        for chunk in resp.iter_content(chunk_size=64 * 1024):
            if chunk:
                fh.write(chunk)

    if dest.stat().st_size < 32:
        dest.unlink(missing_ok=True)
        raise RuntimeError("downloaded file too small (likely blocked)")

    return "saved", dest


def process_item(
    item: dict[str, Any],
    out_dir: Path,
    timeout: float,
    page_url: str,
    skip_existing: bool,
    retries: int,
) -> tuple[dict[str, Any], str, Path | None, str | None]:
    label = f"[{item['index']}] {str(item['title'])[:60]}"
    last_err: Exception | None = None
    with requests.Session() as session:
        for attempt in range(1, retries + 2):
            try:
                status, dest = download_one(
                    session, item, out_dir, timeout, page_url, skip_existing
                )
                return item, status, dest, None
            except Exception as exc:  # noqa: BLE001
                last_err = exc
                time.sleep(min(2.0 * attempt, 5.0))
    return item, "failed", None, f"{label}: {last_err}"


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Download media from an image-title-scraper JSON manifest"
    )
    parser.add_argument("manifest", type=Path, help="Path to manifest.json from browser-extractor.js")
    parser.add_argument("--out", type=Path, default=Path("downloads"), help="Output directory")
    parser.add_argument("--delay", type=float, default=0.6, help="Seconds between requests (serial mode)")
    parser.add_argument("--timeout", type=float, default=20.0, help="Per-request timeout seconds")
    parser.add_argument("--limit", type=int, default=0, help="Optional max items (0 = all)")
    parser.add_argument("--offset", type=int, default=0, help="Skip the first N items")
    parser.add_argument("--retries", type=int, default=2, help="Retries per item")
    parser.add_argument(
        "--workers",
        type=int,
        default=1,
        help="Parallel download workers (1 = serial, recommended for polite scraping)",
    )
    parser.add_argument(
        "--skip-existing",
        action="store_true",
        help="Skip items whose numbered title file already exists in --out",
    )
    parser.add_argument(
        "--failures",
        type=Path,
        default=None,
        help="Write failed items to this JSON file for retry",
    )
    parser.add_argument(
        "--referer",
        default="",
        help="Override Referer header (defaults to manifest pageUrl)",
    )
    args = parser.parse_args()

    if not args.manifest.exists():
        print(f"Manifest not found: {args.manifest}", file=sys.stderr)
        return 1

    items, meta = load_manifest(args.manifest)
    if args.offset:
        items = items[args.offset :]
    if args.limit and args.limit > 0:
        items = items[: args.limit]

    page_url = args.referer or meta.get("pageUrl") or ""
    args.out.mkdir(parents=True, exist_ok=True)
    workers = max(1, args.workers)
    print(f"📦 {len(items)} items → {args.out.resolve()} (workers={workers})")
    if page_url:
        print(f"🔗 Referer: {page_url}")

    ok = 0
    skipped = 0
    failed: list[str] = []
    failed_items: list[dict[str, Any]] = []

    if workers == 1:
        with requests.Session() as session:
            for item in items:
                label = f"[{item['index']}] {str(item['title'])[:60]}"
                success = False
                last_err: Exception | None = None
                for attempt in range(1, args.retries + 2):
                    try:
                        status, dest = download_one(
                            session,
                            item,
                            args.out,
                            args.timeout,
                            page_url,
                            args.skip_existing,
                        )
                        if status == "skipped":
                            print(f"⏭️  {label} → already exists ({dest.name if dest else '?'})")
                            skipped += 1
                        else:
                            print(f"✅ {label} → {dest.name if dest else '?'}")
                            ok += 1
                        success = True
                        break
                    except Exception as exc:  # noqa: BLE001
                        last_err = exc
                        print(f"⚠️  {label} attempt {attempt} failed: {exc}")
                        time.sleep(min(2.0 * attempt, 5.0))
                if not success:
                    failed.append(f"{label}: {last_err}")
                    failed_items.append(item)
                time.sleep(args.delay)
    else:
        with ThreadPoolExecutor(max_workers=workers) as pool:
            futures = [
                pool.submit(
                    process_item,
                    item,
                    args.out,
                    args.timeout,
                    page_url,
                    args.skip_existing,
                    args.retries,
                )
                for item in items
            ]
            for fut in as_completed(futures):
                item, status, dest, err = fut.result()
                label = f"[{item['index']}] {str(item['title'])[:60]}"
                if status == "saved":
                    print(f"✅ {label} → {dest.name if dest else '?'}")
                    ok += 1
                elif status == "skipped":
                    print(f"⏭️  {label} → already exists ({dest.name if dest else '?'})")
                    skipped += 1
                else:
                    print(f"❌ {err}")
                    failed.append(err or label)
                    failed_items.append(item)

    print(f"\n🎉 Done. saved={ok} skipped={skipped} failed={len(failed)}")
    if failed:
        print("Failures:")
        for line in failed:
            print(f"  - {line}")
        if args.failures:
            payload = {
                "generatedAt": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
                "pageUrl": page_url,
                "count": len(failed_items),
                "items": failed_items,
            }
            args.failures.write_text(json.dumps(payload, indent=2), encoding="utf-8")
            print(f"💾 Failed items written to {args.failures}")
        return 2
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
