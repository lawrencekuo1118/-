#!/usr/bin/env python3
"""Search the exported law-article records (JSON/NDJSON).

Examples:
  python3 search.py 抗告
  python3 search.py 民訴 249 --sheet 上訴
  python3 search.py Acceptance --json
  python3 search.py 代位 --limit 20
"""

from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent
RECORDS_JSON = ROOT / "json" / "records.json"
RECORDS_NDJSON = ROOT / "json" / "records.ndjson"


def load_records() -> list[dict]:
    if RECORDS_JSON.exists():
        return json.loads(RECORDS_JSON.read_text(encoding="utf-8"))
    if RECORDS_NDJSON.exists():
        records = []
        with RECORDS_NDJSON.open(encoding="utf-8") as f:
            for line in f:
                line = line.strip()
                if line:
                    records.append(json.loads(line))
        return records
    raise SystemExit(f"No records found. Expected {RECORDS_JSON} or {RECORDS_NDJSON}")


def match_record(rec: dict, terms: list[str], sheet_pat: re.Pattern | None) -> bool:
    if sheet_pat and not sheet_pat.search(rec.get("sheet", "")):
        return False
    hay = rec.get("search_text", "").lower()
    return all(term.lower() in hay for term in terms)


def format_text(rec: dict) -> str:
    fields = rec.get("fields") or {}
    body = " | ".join(f"{k}: {v}" for k, v in fields.items())
    return f"[{rec['id']}] {rec['sheet']}#{rec['row']}\n  {body}"


def main() -> int:
    parser = argparse.ArgumentParser(description="Search law-article CSV/JSON exports")
    parser.add_argument("terms", nargs="+", help="All terms must match (AND)")
    parser.add_argument("--sheet", "-s", help="Regex filter on sheet name")
    parser.add_argument("--limit", "-n", type=int, default=50, help="Max results (default 50)")
    parser.add_argument("--json", action="store_true", help="Print matches as JSON")
    args = parser.parse_args()

    sheet_pat = re.compile(args.sheet, re.I) if args.sheet else None
    records = load_records()
    hits = [r for r in records if match_record(r, args.terms, sheet_pat)]
    hits = hits[: max(0, args.limit)]

    if args.json:
        json.dump(hits, sys.stdout, ensure_ascii=False, indent=2)
        sys.stdout.write("\n")
    else:
        print(f"{len(hits)} hit(s) (limit={args.limit})")
        for rec in hits:
            print(format_text(rec))
            print()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
