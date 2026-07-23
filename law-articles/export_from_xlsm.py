#!/usr/bin/env python3
"""Re-export 藍絲法律_法條分群 xlsm into searchable CSV/JSON.

Requires: pip install python-calamine
"""

from __future__ import annotations

import csv
import json
import re
from pathlib import Path

from python_calamine import CalamineWorkbook

ROOT = Path(__file__).resolve().parent
SRC = ROOT / "藍絲法律_法條分群(260719更).xlsm"
CSV_DIR = ROOT / "csv"
JSON_DIR = ROOT / "json"


def safe_name(name: str) -> str:
    s = re.sub(r'[\\/:*?"<>|]+', "_", name)
    s = re.sub(r"\s+", " ", s).strip()
    return s[:80] or "sheet"


def cell_str(c) -> str:
    if c is None:
        return ""
    if isinstance(c, float) and c.is_integer():
        return str(int(c))
    return str(c)


def unique_headers(headers: list[str]) -> list[str]:
    seen: dict[str, int] = {}
    out: list[str] = []
    for h in headers:
        h = (h or "").strip() or "col"
        if h in seen:
            seen[h] += 1
            out.append(f"{h}_{seen[h]}")
        else:
            seen[h] = 1
            out.append(h)
    return out


def main() -> None:
    if not SRC.exists():
        raise SystemExit(f"Missing source workbook: {SRC}")

    CSV_DIR.mkdir(parents=True, exist_ok=True)
    JSON_DIR.mkdir(parents=True, exist_ok=True)

    wb = CalamineWorkbook.from_path(str(SRC))
    sheets_meta = []
    flat_records = []
    all_sheets: dict[str, list[list[str]]] = {}
    record_id = 0

    for sheet_name in wb.sheet_names:
        raw = wb.get_sheet_by_name(sheet_name).to_python()
        rows: list[list[str]] = []
        for r in raw:
            nr = [cell_str(c) for c in r]
            while nr and nr[-1] == "":
                nr.pop()
            if any(x.strip() != "" for x in nr):
                rows.append(nr)
        all_sheets[sheet_name] = rows

        headers: list[str] = []
        data_rows = rows
        if rows:
            first = rows[0]
            labeled = sum(
                1
                for c in first
                if c and not re.fullmatch(r"\d+(\.\d+)?", c) and len(c) <= 40
            )
            if first and labeled >= max(1, len(first) // 2):
                headers = first
                data_rows = rows[1:]
            else:
                headers = [f"col_{i + 1}" for i in range(max(len(r) for r in rows))]
                data_rows = rows

        width = max([len(headers)] + [len(r) for r in data_rows], default=0)
        headers = (headers + [f"col_{i + 1}" for i in range(len(headers), width)])[:width]
        headers = unique_headers(headers)

        fname = safe_name(sheet_name) + ".csv"
        with (CSV_DIR / fname).open("w", newline="", encoding="utf-8-sig") as f:
            w = csv.writer(f)
            if width:
                w.writerow(headers)
            for r in data_rows:
                padded = r + [""] * (width - len(r))
                w.writerow(padded[:width])

        for i, r in enumerate(data_rows, start=1):
            padded = r + [""] * (width - len(r))
            fields = {headers[j]: padded[j] for j in range(width) if padded[j] != ""}
            search_text = " ".join([sheet_name] + [padded[j] for j in range(width) if padded[j] != ""])
            record_id += 1
            flat_records.append(
                {
                    "id": record_id,
                    "sheet": sheet_name,
                    "row": i,
                    "fields": fields,
                    "search_text": search_text,
                }
            )

        sheets_meta.append(
            {
                "sheet": sheet_name,
                "csv": f"csv/{fname}",
                "rows": len(data_rows),
                "columns": headers,
            }
        )
        print(f"{sheet_name}: {len(data_rows)} rows -> {fname}")

    (JSON_DIR / "sheets.json").write_text(
        json.dumps(all_sheets, ensure_ascii=False, indent=2), encoding="utf-8"
    )
    (JSON_DIR / "manifest.json").write_text(
        json.dumps(
            {
                "source": SRC.name,
                "sheet_count": len(sheets_meta),
                "record_count": len(flat_records),
                "sheets": sheets_meta,
            },
            ensure_ascii=False,
            indent=2,
        ),
        encoding="utf-8",
    )
    (JSON_DIR / "records.json").write_text(
        json.dumps(flat_records, ensure_ascii=False, indent=2), encoding="utf-8"
    )
    with (JSON_DIR / "records.ndjson").open("w", encoding="utf-8") as f:
        for rec in flat_records:
            f.write(json.dumps(rec, ensure_ascii=False) + "\n")

    print(f"Done: {len(flat_records)} searchable records across {len(sheets_meta)} sheets")


if __name__ == "__main__":
    main()
