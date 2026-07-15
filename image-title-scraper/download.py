#!/usr/bin/env python3
"""
Image Title Scraper — Python downloader (v4)

Purpose:
  Reliably download media listed in a JSON manifest produced by
  browser-extractor.js, using native titles as filenames.

  This avoids Chrome/Edge `.crdownload` stalls that happen when a
  console script fires many cross-origin `a.download` clicks.

Usage:
  python download.py manifest.json
  python download.py manifest.json --out downloads --delay 0.8 --limit 50

Manifest format (from browser-extractor.js):
  {
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
    "video/mp4": ".mp4",
    "video/webm": ".webm",
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
    if suffix in {".jpg", ".jpeg", ".png", ".gif", ".webp", ".avif", ".svg", ".mp4", ".webm"}:
        return ".jpg" if suffix == ".jpeg" else suffix

    return ".mp4" if media_type == "video" else ".jpg"


def load_manifest(path: Path) -> list[dict[str, Any]]:
    data = json.loads(path.read_text(encoding="utf-8"))
    if isinstance(data, list):
        items = data
    elif isinstance(data, dict) and "items" in data:
        items = data["items"]
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
            }
        )
    return normalized


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


def download_one(
    session: requests.Session,
    item: dict[str, Any],
    out_dir: Path,
    timeout: float,
) -> Path | None:
    url = item["url"]
    index = item["index"]
    title = sanitize_filename(str(item["title"]))
    media_type = item.get("type") or "image"

    resp = session.get(url, headers=DEFAULT_HEADERS, timeout=timeout, stream=True)
    resp.raise_for_status()

    ext = guess_ext(url, resp.headers.get("Content-Type"), media_type)
    stem = f"{index:03d}_{title}"
    dest = unique_path(out_dir, stem, ext)

    with dest.open("wb") as fh:
        for chunk in resp.iter_content(chunk_size=64 * 1024):
            if chunk:
                fh.write(chunk)

    # Reject empty / tiny failure bodies
    if dest.stat().st_size < 32:
        dest.unlink(missing_ok=True)
        raise RuntimeError("downloaded file too small (likely blocked)")

    return dest


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Download media from an image-title-scraper JSON manifest"
    )
    parser.add_argument("manifest", type=Path, help="Path to manifest.json from browser-extractor.js")
    parser.add_argument("--out", type=Path, default=Path("downloads"), help="Output directory")
    parser.add_argument("--delay", type=float, default=0.6, help="Seconds between requests")
    parser.add_argument("--timeout", type=float, default=20.0, help="Per-request timeout seconds")
    parser.add_argument("--limit", type=int, default=0, help="Optional max items (0 = all)")
    parser.add_argument("--offset", type=int, default=0, help="Skip the first N items")
    parser.add_argument("--retries", type=int, default=2, help="Retries per item")
    args = parser.parse_args()

    if not args.manifest.exists():
        print(f"Manifest not found: {args.manifest}", file=sys.stderr)
        return 1

    items = load_manifest(args.manifest)
    if args.offset:
        items = items[args.offset :]
    if args.limit and args.limit > 0:
        items = items[: args.limit]

    args.out.mkdir(parents=True, exist_ok=True)
    print(f"📦 {len(items)} items → {args.out.resolve()}")

    ok = 0
    failed: list[str] = []

    with requests.Session() as session:
        for item in items:
            label = f"[{item['index']}] {item['title'][:60]}"
            success = False
            last_err: Exception | None = None
            for attempt in range(1, args.retries + 2):
                try:
                    dest = download_one(session, item, args.out, args.timeout)
                    print(f"✅ {label} → {dest.name}")
                    ok += 1
                    success = True
                    break
                except Exception as exc:  # noqa: BLE001 - report any download failure
                    last_err = exc
                    print(f"⚠️  {label} attempt {attempt} failed: {exc}")
                    time.sleep(min(2.0 * attempt, 5.0))
            if not success:
                failed.append(f"{label}: {last_err}")
            time.sleep(args.delay)

    print(f"\n🎉 Done. saved={ok} failed={len(failed)}")
    if failed:
        print("Failures:")
        for line in failed:
            print(f"  - {line}")
        return 2
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
