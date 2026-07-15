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

Manifest format (from browser-extractor.js):
  {
    "items": [
      {"index": 1, "url": "https://...", "title": "Some_Native_Title", "type": "image"}
    ]
  }
"""

from __future__ import annotations

import argparse
import concurrent.futures
import csv
import hashlib
import json
import mimetypes
import os
import re
import sys
import threading
import time
from collections import defaultdict
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
MIN_FILE_SIZE = 32
ALLOWED_SCHEMES = {"http", "https"}

INVALID_FS_CHARS = re.compile(r'[\\/*?:"<>|\n\r\t]+')
MIME_EXT = {
    "image/jpeg": ".jpg",
    "image/jpg": ".jpg",
    "image/png": ".png",
    "image/gif": ".gif",
    "image/webp": ".webp",
    "image/avif": ".avif",
    "image/heic": ".heic",
    "image/svg+xml": ".svg",
    "video/mp4": ".mp4",
    "video/webm": ".webm",
    "video/quicktime": ".mov",
    "video/ogg": ".ogv",
}
WINDOWS_RESERVED_NAMES = {
    "con", "prn", "aux", "nul",
    *(f"com{i}" for i in range(1, 10)),
    *(f"lpt{i}" for i in range(1, 10)),
}
_thread_local = threading.local()
_path_lock = threading.Lock()


def sniff_ext(head: bytes) -> str | None:
    """Identify media from magic bytes when servers omit or misstate MIME."""
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
    if head.lstrip().startswith((b"<?xml", b"<svg")):
        return ".svg"
    return None


def sanitize_filename(text: str, max_len: int = 100) -> str:
    text = INVALID_FS_CHARS.sub("", text or "").strip()
    text = re.sub(r"\s+", "_", text)
    text = text.strip("._")
    if not text:
        text = "image"
    if text.casefold() in WINDOWS_RESERVED_NAMES:
        text = f"_{text}"
    return text[:max_len]


def guess_ext(
    url: str, content_type: str | None, media_type: str, head: bytes = b""
) -> str:
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
    if suffix in {
        ".jpg", ".jpeg", ".png", ".gif", ".webp", ".avif", ".heic", ".svg",
        ".bmp", ".mp4", ".webm", ".mov", ".ogv",
    }:
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

    if not isinstance(items, list):
        raise ValueError("Manifest 'items' must be an array")

    normalized = []
    used_indices: set[int] = set()
    for i, item in enumerate(items, start=1):
        if isinstance(item, str):
            url = item.strip()
            if urlparse(url).scheme.lower() not in ALLOWED_SCHEMES:
                continue
            used_indices.add(i)
            normalized.append(
                {
                    "index": i,
                    "url": url,
                    "title": f"image_{i}",
                    "type": "image",
                    "sourcePage": "",
                    "referer": "",
                }
            )
            continue
        if not isinstance(item, dict):
            continue
        url = str(item.get("url") or item.get("murl") or item.get("src") or "").strip()
        if not url or urlparse(url).scheme.lower() not in ALLOWED_SCHEMES:
            continue
        try:
            index = int(item.get("index") or i)
        except (TypeError, ValueError):
            index = i
        if index < 1 or index in used_indices:
            index = i
            while index in used_indices:
                index += 1
        used_indices.add(index)
        normalized.append(
            {
                "index": index,
                "url": url,
                "title": item.get("title") or item.get("suggestedName") or f"image_{i}",
                "type": item.get("type") or "image",
                "sourcePage": (
                    item.get("sourcePage")
                    or item.get("referer")
                    or data.get("pageUrl", "")
                    if isinstance(data, dict)
                    else item.get("sourcePage") or item.get("referer") or ""
                ),
                "referer": item.get("referer") or item.get("sourcePage") or "",
            }
        )
    return normalized


def reserve_path(
    directory: Path, stem: str, ext: str, overwrite: bool = False
) -> tuple[Path, Path]:
    with _path_lock:
        if overwrite:
            candidate = directory / f"{stem}{ext}"
            partial = candidate.with_suffix(
                f"{candidate.suffix}.{os.getpid()}.{threading.get_ident()}.part"
            )
            fd = os.open(partial, os.O_CREAT | os.O_EXCL | os.O_WRONLY, 0o600)
            os.close(fd)
            return candidate, partial

        n = 1
        while True:
            suffix = "" if n == 1 else f"_{n}"
            candidate = directory / f"{stem}{suffix}{ext}"
            partial = candidate.with_suffix(f"{candidate.suffix}.part")
            if candidate.exists():
                n += 1
                continue
            try:
                fd = os.open(partial, os.O_CREAT | os.O_EXCL | os.O_WRONLY, 0o600)
            except FileExistsError:
                n += 1
                continue
            os.close(fd)
            return candidate, partial


class DownloadRegistry:
    """Persist per-item completion data for trustworthy, process-safe resumes."""

    def __init__(self, directory: Path):
        self.directory = directory
        self.state_dir = directory / ".image-title-scraper-state"
        self.lock = threading.Lock()

    def _record_path(self, stem: str) -> Path:
        digest = hashlib.sha256(stem.encode("utf-8")).hexdigest()
        return self.state_dir / f"{digest}.json"

    def _read(self, stem: str) -> dict[str, Any]:
        try:
            data = json.loads(self._record_path(stem).read_text(encoding="utf-8"))
            return data if isinstance(data, dict) else {}
        except (OSError, json.JSONDecodeError):
            return {}

    def find(self, stem: str, url: str) -> Path | None:
        with self.lock:
            record = self._read(stem)
            if record.get("url") != url:
                return None
            path = self.directory / str(record.get("filename") or "")
            expected_size = int(record.get("size") or 0)
            if (
                path.is_file()
                and expected_size >= MIN_FILE_SIZE
                and path.stat().st_size == expected_size
            ):
                return path
            return None

    def record(self, stem: str, url: str, path: Path) -> None:
        with self.lock:
            previous = self._read(stem)
            record = {
                "stem": stem,
                "url": url,
                "filename": path.name,
                "size": path.stat().st_size,
            }
            self.state_dir.mkdir(parents=True, exist_ok=True)
            record_path = self._record_path(stem)
            temporary = record_path.with_name(
                f"{record_path.name}.{os.getpid()}.{threading.get_ident()}.tmp"
            )
            temporary.write_text(json.dumps(record, indent=2), encoding="utf-8")
            temporary.replace(record_path)
            old_name = str(previous.get("filename") or "")
            if old_name and old_name != path.name:
                (self.directory / old_name).unlink(missing_ok=True)


def get_session() -> requests.Session:
    session = getattr(_thread_local, "session", None)
    if session is None:
        session = requests.Session()
        _thread_local.session = session
    return session


class HostThrottle:
    """Apply a politeness delay per host while allowing cross-host concurrency."""

    def __init__(self, delay: float):
        self.delay = delay
        self.lock = threading.Lock()
        self.next_allowed: dict[str, float] = defaultdict(float)

    def wait(self, url: str) -> None:
        if self.delay <= 0:
            return
        host = urlparse(url).netloc.lower()
        while True:
            with self.lock:
                now = time.monotonic()
                wait_for = self.next_allowed[host] - now
                if wait_for <= 0:
                    self.next_allowed[host] = now + self.delay
                    return
            time.sleep(min(wait_for, self.delay))


class RetryableError(RuntimeError):
    def __init__(self, message: str, pause: float):
        super().__init__(message)
        self.pause = pause


def download_one(
    item: dict[str, Any],
    out_dir: Path,
    timeout: float,
    overwrite: bool = False,
    max_bytes: int = 0,
    min_bytes: int = MIN_FILE_SIZE,
    registry: DownloadRegistry | None = None,
) -> tuple[Path, bool]:
    url = item["url"]
    index = item["index"]
    title = sanitize_filename(str(item["title"]))
    media_type = item.get("type") or "image"
    stem = f"{index:03d}_{title}"
    registry = registry or DownloadRegistry(out_dir)

    if not overwrite:
        existing = registry.find(stem, url)
        if existing:
            return existing, True

    headers = dict(DEFAULT_HEADERS)
    source_page = str(item.get("sourcePage") or item.get("referer") or "")
    if urlparse(source_page).scheme.lower() in ALLOWED_SCHEMES:
        headers["Referer"] = source_page

    session = get_session()
    with session.get(
        url,
        headers=headers,
        timeout=(min(timeout, 10.0), timeout),
        stream=True,
        allow_redirects=True,
    ) as resp:
        if getattr(resp, "status_code", 0) in (429, 503):
            retry_after = resp.headers.get("Retry-After")
            try:
                pause = min(float(retry_after), 30.0) if retry_after else 5.0
            except ValueError:
                pause = 5.0
            raise RetryableError(f"HTTP {resp.status_code} (rate limited)", pause)
        resp.raise_for_status()
        content_type = resp.headers.get("Content-Type", "")
        normalized_type = content_type.split(";", 1)[0].strip().lower()

        try:
            content_length = int(resp.headers.get("Content-Length") or 0)
        except ValueError:
            content_length = 0
        if max_bytes and content_length > max_bytes:
            raise RuntimeError(
                f"content length {content_length} exceeds limit {max_bytes}"
            )

        chunks = resp.iter_content(chunk_size=128 * 1024)
        first_chunk = next((chunk for chunk in chunks if chunk), b"")
        sample = first_chunk[:1024].lstrip().lower()
        if sample.startswith((b"<!doctype html", b"<html")):
            raise RuntimeError("server returned an HTML page, not media")
        sniffed = sniff_ext(first_chunk)
        expected_prefix = "video/" if media_type == "video" else "image/"
        if not sniffed and normalized_type.startswith(
            ("text/", "application/json", "application/xml")
        ):
            raise RuntimeError(f"server returned {normalized_type}, not media")
        if (
            not sniffed
            and normalized_type
            and normalized_type != "application/octet-stream"
            and not normalized_type.startswith(expected_prefix)
        ):
            raise RuntimeError(
                f"server returned {normalized_type}, expected {expected_prefix}*"
            )

        ext = guess_ext(url, content_type, media_type, first_chunk)
        dest, partial = reserve_path(out_dir, stem, ext, overwrite=overwrite)
        written = len(first_chunk)
        if max_bytes and written > max_bytes:
            partial.unlink(missing_ok=True)
            raise RuntimeError(f"download exceeded size limit {max_bytes}")
        try:
            with partial.open("wb") as fh:
                if first_chunk:
                    fh.write(first_chunk)
                for chunk in chunks:
                    if not chunk:
                        continue
                    written += len(chunk)
                    if max_bytes and written > max_bytes:
                        raise RuntimeError(
                            f"download exceeded size limit {max_bytes}"
                        )
                    fh.write(chunk)
                fh.flush()
                os.fsync(fh.fileno())

            if written < min_bytes:
                raise RuntimeError(
                    f"downloaded file too small ({written} B < {min_bytes} B)"
                )
            partial.replace(dest)
            registry.record(stem, url, dest)
        except BaseException:
            partial.unlink(missing_ok=True)
            raise

    return dest, False


def download_with_retries(
    item: dict[str, Any],
    out_dir: Path,
    timeout: float,
    retries: int,
    overwrite: bool,
    max_bytes: int,
    min_bytes: int,
    registry: DownloadRegistry,
    throttle: HostThrottle,
) -> tuple[dict[str, Any], Path | None, bool, str | None]:
    last_error: Exception | None = None
    for attempt in range(retries + 1):
        try:
            throttle.wait(item["url"])
            path, skipped = download_one(
                item,
                out_dir,
                timeout,
                overwrite=overwrite,
                max_bytes=max_bytes,
                min_bytes=min_bytes,
                registry=registry,
            )
            return item, path, skipped, None
        except Exception as exc:  # noqa: BLE001 - returned in the failure report
            last_error = exc
            if attempt < retries:
                time.sleep(getattr(exc, "pause", min(2 ** attempt, 8)))
    return item, None, False, str(last_error)


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Download media from an image-title-scraper JSON manifest"
    )
    parser.add_argument("manifest", type=Path, help="Path to manifest.json from browser-extractor.js")
    parser.add_argument("--out", type=Path, default=Path("downloads"), help="Output directory")
    parser.add_argument(
        "--delay",
        type=float,
        default=0.6,
        help="Seconds between requests to the same host",
    )
    parser.add_argument("--timeout", type=float, default=20.0, help="Per-request timeout seconds")
    parser.add_argument("--limit", type=int, default=0, help="Optional max items (0 = all)")
    parser.add_argument("--offset", type=int, default=0, help="Skip the first N items")
    parser.add_argument("--retries", type=int, default=2, help="Retries per item")
    parser.add_argument(
        "--min-bytes",
        type=int,
        default=512,
        help="Reject files smaller than this (default: 512)",
    )
    parser.add_argument("--workers", type=int, default=4, help="Concurrent downloads (default: 4)")
    parser.add_argument(
        "--overwrite",
        action="store_true",
        help="Download again instead of resuming from existing files",
    )
    parser.add_argument(
        "--max-mb",
        type=float,
        default=0,
        help="Reject individual files larger than this many MiB (0 = unlimited)",
    )
    parser.add_argument(
        "--failures",
        type=Path,
        default=None,
        help="Failure report path (default: unique file under <out>)",
    )
    args = parser.parse_args()

    if not args.manifest.exists():
        print(f"Manifest not found: {args.manifest}", file=sys.stderr)
        return 1
    if args.workers < 1 or args.retries < 0 or args.timeout <= 0 or args.delay < 0:
        parser.error("--workers must be >= 1; retries/delay >= 0; timeout > 0")
    if args.max_mb < 0:
        parser.error("--max-mb must be >= 0")
    if args.min_bytes < 1:
        parser.error("--min-bytes must be >= 1")

    try:
        items = load_manifest(args.manifest)
    except (OSError, json.JSONDecodeError, ValueError) as exc:
        print(f"Invalid manifest: {exc}", file=sys.stderr)
        return 1
    if args.offset:
        items = items[args.offset :]
    if args.limit and args.limit > 0:
        items = items[: args.limit]

    args.out.mkdir(parents=True, exist_ok=True)
    workers = min(args.workers, len(items)) if items else 0
    print(f"📦 {len(items)} items → {args.out.resolve()} ({workers} workers)")

    ok = 0
    skipped = 0
    failed: list[dict[str, Any]] = []
    results: list[tuple[dict[str, Any], str, str]] = []
    max_bytes = int(args.max_mb * 1024 * 1024) if args.max_mb else 0
    registry = DownloadRegistry(args.out)
    throttle = HostThrottle(args.delay)

    if items:
        with concurrent.futures.ThreadPoolExecutor(max_workers=workers) as executor:
            futures = [
                executor.submit(
                    download_with_retries,
                    item,
                    args.out,
                    args.timeout,
                    args.retries,
                    args.overwrite,
                    max_bytes,
                    args.min_bytes,
                    registry,
                    throttle,
                )
                for item in items
            ]
            try:
                for future in concurrent.futures.as_completed(futures):
                    item, dest, was_skipped, error = future.result()
                    label = f"[{item['index']}] {str(item['title'])[:60]}"
                    if dest:
                        if was_skipped:
                            skipped += 1
                            results.append((item, "skipped", dest.name))
                            print(f"⏭️  {label} already exists")
                        else:
                            ok += 1
                            results.append((item, "saved", dest.name))
                            print(f"✅ {label} → {dest.name}")
                    else:
                        failed.append({**item, "error": error})
                        results.append((item, "failed", error or "unknown error"))
                        print(f"⚠️  {label} failed: {error}")
            except KeyboardInterrupt:
                for future in futures:
                    future.cancel()
                print("\nInterrupted; completed files were kept.", file=sys.stderr)
                return 130

    failure_stamp = time.strftime("%Y%m%dT%H%M%SZ", time.gmtime())
    failures_path = args.failures or (
        args.out / f"failures-{failure_stamp}-{os.getpid()}.json"
    )
    if failed:
        report = {
            "generatedAt": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
            "count": len(failed),
            "items": failed,
        }
        failures_path.parent.mkdir(parents=True, exist_ok=True)
        temporary = failures_path.with_name(
            f"{failures_path.name}.{os.getpid()}.tmp"
        )
        temporary.write_text(json.dumps(report, indent=2), encoding="utf-8")
        temporary.replace(failures_path)
        print(f"Failure report: {failures_path}")
    elif args.failures:
        failures_path.unlink(missing_ok=True)

    report_path = args.out / "report.csv"
    with report_path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.writer(handle)
        writer.writerow(["index", "status", "title", "url", "detail"])
        for item, status, detail in sorted(
            results, key=lambda result: result[0]["index"]
        ):
            writer.writerow(
                [item["index"], status, item["title"], item["url"], detail]
            )
    print(f"Report: {report_path}")

    print(f"\n🎉 Done. saved={ok} skipped={skipped} failed={len(failed)}")
    if failed:
        return 2
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
