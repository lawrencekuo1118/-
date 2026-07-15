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
  python download.py manifest.json --out downloads --workers 4 --limit 50
  python download.py downloads/failed-manifest.json   # retry only failures

Upgrades vs v4:
  - Concurrent downloads (--workers, default 4) with per-host politeness delay
  - Magic-byte content sniffing so extensions match actual bytes even when
    servers send wrong/missing Content-Type
  - Sends per-item Referer (from manifest) — many CDNs reject bare requests
  - Atomic writes via .part temp files (no half-written files on crash)
  - --skip-existing resume mode and --min-bytes small-file rejection
  - Honors HTTP 429/503 Retry-After between attempts
  - Writes failed-manifest.json (re-runnable) and report.csv into --out
"""

from __future__ import annotations

import argparse
import csv
import json
import mimetypes
import re
import sys
import threading
import time
from collections import defaultdict
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
    "Accept": "image/avif,image/webp,image/apng,image/*,video/*,*/*;q=0.8",
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
}
KNOWN_SUFFIXES = {
    ".jpg", ".jpeg", ".png", ".gif", ".webp", ".avif", ".heic", ".svg", ".bmp", ".mp4", ".webm",
}


def sniff_ext(head: bytes) -> str | None:
    """Identify the real file type from magic bytes (servers often lie)."""
    if head.startswith(b"\xff\xd8\xff"):
        return ".jpg"
    if head.startswith(b"\x89PNG\r\n\x1a\n"):
        return ".png"
    if head.startswith((b"GIF87a", b"GIF89a")):
        return ".gif"
    if head[:4] == b"RIFF" and head[8:12] == b"WEBP":
        return ".webp"
    if head.startswith(b"BM"):
        return ".bmp"
    if head[4:8] == b"ftyp":
        brand = head[8:12]
        if brand in (b"avif", b"avis"):
            return ".avif"
        if brand in (b"heic", b"heix", b"mif1"):
            return ".heic"
        return ".mp4"
    if head.startswith(b"\x1a\x45\xdf\xa3"):
        return ".webm"
    stripped = head.lstrip()
    if stripped.startswith((b"<?xml", b"<svg")):
        return ".svg"
    return None


def sanitize_filename(text: str, max_len: int = 100) -> str:
    text = INVALID_FS_CHARS.sub("", text or "").strip()
    text = re.sub(r"\s+", "_", text)
    text = text.strip("._")
    if not text:
        text = "image"
    return text[:max_len]


def guess_ext(url: str, content_type: str | None, media_type: str, head: bytes) -> str:
    sniffed = sniff_ext(head)
    if sniffed:
        return sniffed

    if content_type:
        ct = content_type.split(";")[0].strip().lower()
        if ct in MIME_EXT:
            return MIME_EXT[ct]
        guessed = mimetypes.guess_extension(ct)
        if guessed:
            return ".jpg" if guessed == ".jpe" else guessed

    path = unquote(urlparse(url).path)
    suffix = Path(path).suffix.lower()
    if suffix in KNOWN_SUFFIXES:
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
            normalized.append({"index": i, "url": item, "title": f"image_{i}", "type": "image", "referer": ""})
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
                "referer": item.get("referer") or "",
            }
        )
    return normalized


_path_lock = threading.Lock()


def unique_path(directory: Path, stem: str, ext: str) -> Path:
    with _path_lock:
        candidate = directory / f"{stem}{ext}"
        if not candidate.exists():
            candidate.touch()  # reserve to avoid concurrent collisions
            return candidate
        n = 2
        while True:
            candidate = directory / f"{stem}_{n}{ext}"
            if not candidate.exists():
                candidate.touch()
                return candidate
            n += 1


def existing_for_stem(directory: Path, stem: str) -> Path | None:
    for suffix in KNOWN_SUFFIXES:
        candidate = directory / f"{stem}{suffix}"
        if candidate.exists() and candidate.stat().st_size > 0:
            return candidate
    return None


class HostThrottle:
    """Per-host politeness delay that still allows cross-host concurrency."""

    def __init__(self, delay: float) -> None:
        self.delay = delay
        self._lock = threading.Lock()
        self._next_ok: dict[str, float] = defaultdict(float)

    def wait(self, url: str) -> None:
        if self.delay <= 0:
            return
        host = urlparse(url).netloc
        while True:
            with self._lock:
                now = time.monotonic()
                if now >= self._next_ok[host]:
                    self._next_ok[host] = now + self.delay
                    return
                wait_for = self._next_ok[host] - now
            time.sleep(min(wait_for, self.delay))


def download_one(
    session: requests.Session,
    item: dict[str, Any],
    out_dir: Path,
    timeout: float,
    min_bytes: int,
) -> Path:
    url = item["url"]
    index = item["index"]
    title = sanitize_filename(str(item["title"]))
    media_type = item.get("type") or "image"

    headers = dict(DEFAULT_HEADERS)
    if item.get("referer"):
        headers["Referer"] = item["referer"]

    resp = session.get(url, headers=headers, timeout=timeout, stream=True)
    if resp.status_code in (429, 503):
        retry_after = resp.headers.get("Retry-After")
        resp.close()
        pause = 5.0
        if retry_after:
            try:
                pause = min(float(retry_after), 30.0)
            except ValueError:
                pass
        raise RetryableError(f"HTTP {resp.status_code} (rate limited)", pause)
    resp.raise_for_status()

    chunks = resp.iter_content(chunk_size=64 * 1024)
    head = b""
    for chunk in chunks:
        if chunk:
            head = chunk
            break

    ext = guess_ext(url, resp.headers.get("Content-Type"), media_type, head)
    stem = f"{index:03d}_{title}"
    dest = unique_path(out_dir, stem, ext)
    part = dest.with_suffix(dest.suffix + ".part")

    try:
        with part.open("wb") as fh:
            fh.write(head)
            for chunk in chunks:
                if chunk:
                    fh.write(chunk)
        if part.stat().st_size < min_bytes:
            raise RuntimeError(
                f"downloaded file too small ({part.stat().st_size} B < {min_bytes} B; likely blocked)"
            )
        part.replace(dest)
    except BaseException:
        part.unlink(missing_ok=True)
        dest.unlink(missing_ok=True)  # remove the reserved placeholder
        raise

    return dest


class RetryableError(RuntimeError):
    def __init__(self, message: str, pause: float = 0.0) -> None:
        super().__init__(message)
        self.pause = pause


def process_item(
    session: requests.Session,
    item: dict[str, Any],
    args: argparse.Namespace,
    throttle: HostThrottle,
) -> tuple[dict[str, Any], str, str]:
    """Returns (item, status, detail); status in {ok, skipped, failed}."""
    title = sanitize_filename(str(item["title"]))
    stem = f"{item['index']:03d}_{title}"

    if args.skip_existing:
        existing = existing_for_stem(args.out, stem)
        if existing:
            return item, "skipped", existing.name

    last_err: Exception | None = None
    for attempt in range(1, args.retries + 2):
        throttle.wait(item["url"])
        try:
            dest = download_one(session, item, args.out, args.timeout, args.min_bytes)
            return item, "ok", dest.name
        except Exception as exc:  # noqa: BLE001 - report any download failure
            last_err = exc
            pause = getattr(exc, "pause", 0.0) or min(2.0 * attempt, 5.0)
            if attempt <= args.retries:
                time.sleep(pause)
    return item, "failed", str(last_err)


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Download media from an image-title-scraper JSON manifest"
    )
    parser.add_argument("manifest", type=Path, help="Path to manifest.json from browser-extractor.js")
    parser.add_argument("--out", type=Path, default=Path("downloads"), help="Output directory")
    parser.add_argument("--workers", type=int, default=4, help="Concurrent downloads (1 = sequential)")
    parser.add_argument("--delay", type=float, default=0.6, help="Seconds between requests to the same host")
    parser.add_argument("--timeout", type=float, default=20.0, help="Per-request timeout seconds")
    parser.add_argument("--limit", type=int, default=0, help="Optional max items (0 = all)")
    parser.add_argument("--offset", type=int, default=0, help="Skip the first N items")
    parser.add_argument("--retries", type=int, default=2, help="Retries per item")
    parser.add_argument("--min-bytes", type=int, default=512, help="Reject files smaller than this")
    parser.add_argument(
        "--skip-existing",
        action="store_true",
        help="Skip items whose numbered file already exists in --out (resume mode)",
    )
    parser.add_argument(
        "--no-report",
        action="store_true",
        help="Do not write report.csv / failed-manifest.json into --out",
    )
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
    workers = max(1, args.workers)
    print(f"📦 {len(items)} items → {args.out.resolve()} (workers={workers})")

    throttle = HostThrottle(args.delay)
    results: list[tuple[dict[str, Any], str, str]] = []
    ok = skipped = 0
    failed_items: list[dict[str, Any]] = []

    started = time.monotonic()
    with requests.Session() as session:
        adapter = requests.adapters.HTTPAdapter(pool_connections=workers, pool_maxsize=workers * 2)
        session.mount("http://", adapter)
        session.mount("https://", adapter)

        with ThreadPoolExecutor(max_workers=workers) as pool:
            futures = {
                pool.submit(process_item, session, item, args, throttle): item for item in items
            }
            for future in as_completed(futures):
                item, status, detail = future.result()
                results.append((item, status, detail))
                label = f"[{item['index']}] {str(item['title'])[:60]}"
                if status == "ok":
                    ok += 1
                    print(f"✅ {label} → {detail}")
                elif status == "skipped":
                    skipped += 1
                    print(f"⏭️  {label} (exists: {detail})")
                else:
                    failed_items.append(item)
                    print(f"❌ {label}: {detail}")

    elapsed = time.monotonic() - started
    print(f"\n🎉 Done in {elapsed:.1f}s. saved={ok} skipped={skipped} failed={len(failed_items)}")

    if not args.no_report:
        results.sort(key=lambda r: r[0]["index"])
        report_path = args.out / "report.csv"
        with report_path.open("w", newline="", encoding="utf-8") as fh:
            writer = csv.writer(fh)
            writer.writerow(["index", "status", "title", "url", "detail"])
            for item, status, detail in results:
                writer.writerow([item["index"], status, item["title"], item["url"], detail])
        print(f"📄 Report: {report_path}")

        if failed_items:
            failed_path = args.out / "failed-manifest.json"
            failed_path.write_text(
                json.dumps({"items": sorted(failed_items, key=lambda x: x["index"])}, indent=2, ensure_ascii=False),
                encoding="utf-8",
            )
            print(f"🔁 Retry failures with: python download.py {failed_path}")

    return 2 if failed_items else 0


if __name__ == "__main__":
    raise SystemExit(main())
