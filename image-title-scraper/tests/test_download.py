import importlib.util
import json
import tempfile
import unittest
from pathlib import Path
from unittest import mock


MODULE_PATH = Path(__file__).parents[1] / "download.py"
SPEC = importlib.util.spec_from_file_location("image_title_download", MODULE_PATH)
download = importlib.util.module_from_spec(SPEC)
assert SPEC.loader
SPEC.loader.exec_module(download)


class FakeResponse:
    def __init__(self, body=b"x" * 64, content_type="image/png", status=200):
        self.body = body
        self.headers = {
            "Content-Type": content_type,
            "Content-Length": str(len(body)),
        }
        self.status = status

    def __enter__(self):
        return self

    def __exit__(self, *_args):
        return False

    def raise_for_status(self):
        if self.status >= 400:
            raise RuntimeError(f"HTTP {self.status}")

    def iter_content(self, chunk_size):
        yield self.body[:chunk_size]
        yield self.body[chunk_size:]


class FakeSession:
    def __init__(self, response):
        self.response = response
        self.last_headers = None

    def get(self, _url, **kwargs):
        self.last_headers = kwargs["headers"]
        return self.response


class DownloaderTests(unittest.TestCase):
    def test_sanitize_filename_handles_reserved_and_invalid_names(self):
        self.assertEqual(download.sanitize_filename("CON"), "_CON")
        self.assertEqual(download.sanitize_filename(' a/b: c?.jpg '), "ab_c.jpg")

    def test_guess_ext_prefers_content_type(self):
        self.assertEqual(
            download.guess_ext("https://example.test/file.jpg", "image/webp", "image"),
            ".webp",
        )

    def test_manifest_filters_urls_and_repairs_duplicate_indices(self):
        payload = {
            "pageUrl": "https://example.test/gallery",
            "items": [
                {"index": 1, "url": "https://cdn.test/a.jpg", "title": "A"},
                {"index": 1, "url": "https://cdn.test/b.jpg", "title": "B"},
                {"url": "file:///etc/passwd", "title": "bad"},
            ],
        }
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "manifest.json"
            path.write_text(json.dumps(payload), encoding="utf-8")
            items = download.load_manifest(path)

        self.assertEqual([item["index"] for item in items], [1, 2])
        self.assertTrue(all(item["sourcePage"] == payload["pageUrl"] for item in items))

    def test_download_is_atomic_and_resumable(self):
        item = {
            "index": 1,
            "url": "https://cdn.test/a",
            "title": "A",
            "type": "image",
            "sourcePage": "https://example.test/gallery",
        }
        session = FakeSession(FakeResponse())
        with tempfile.TemporaryDirectory() as tmp, mock.patch.object(
            download, "get_session", return_value=session
        ):
            out = Path(tmp)
            path, skipped = download.download_one(item, out, timeout=2)
            self.assertFalse(skipped)
            self.assertEqual(path.name, "001_A.png")
            self.assertFalse(list(out.glob("*.part")))
            self.assertEqual(session.last_headers["Referer"], item["sourcePage"])

            resumed_path, skipped = download.download_one(item, out, timeout=2)
            self.assertTrue(skipped)
            self.assertEqual(resumed_path, path)

            changed = {**item, "url": "https://cdn.test/different"}
            changed_path, skipped = download.download_one(changed, out, timeout=2)
            self.assertFalse(skipped)
            self.assertNotEqual(changed_path, path)

    def test_path_reservation_is_exclusive(self):
        with tempfile.TemporaryDirectory() as tmp:
            out = Path(tmp)
            first, first_partial = download.reserve_path(out, "001_A", ".jpg")
            second, second_partial = download.reserve_path(out, "001_A", ".jpg")
            self.assertEqual(first.name, "001_A.jpg")
            self.assertEqual(second.name, "001_A_2.jpg")
            first_partial.unlink()
            second_partial.unlink()

    def test_rejects_html_without_leaving_partial_file(self):
        item = {
            "index": 1,
            "url": "https://cdn.test/blocked",
            "title": "Blocked",
            "type": "image",
        }
        with tempfile.TemporaryDirectory() as tmp, mock.patch.object(
            download,
            "get_session",
            return_value=FakeSession(FakeResponse(content_type="text/html")),
        ):
            out = Path(tmp)
            with self.assertRaisesRegex(RuntimeError, "not media"):
                download.download_one(item, out, timeout=2)
            self.assertEqual(list(out.iterdir()), [])

    def test_rejects_disguised_html_body(self):
        item = {
            "index": 1,
            "url": "https://cdn.test/challenge",
            "title": "Challenge",
            "type": "image",
        }
        body = b"<!doctype html><html>" + b"x" * 64
        with tempfile.TemporaryDirectory() as tmp, mock.patch.object(
            download,
            "get_session",
            return_value=FakeSession(
                FakeResponse(body=body, content_type="application/octet-stream")
            ),
        ):
            out = Path(tmp)
            with self.assertRaisesRegex(RuntimeError, "HTML"):
                download.download_one(item, out, timeout=2)
            self.assertFalse(list(out.glob("*.part")))


if __name__ == "__main__":
    unittest.main()
