# 藍絲法律 · 法條分群 (searchable export)

Structured CSV/JSON export of the law-article workbook. Statute
corrections are applied **in place** on the original `.xlsm` with red font
（全國法規資料庫，UpdateDate 2026/7/24；VBA retained）.

## Contents

| Path | Description |
|---|---|
| `藍絲法律_法條分群(260719更).xlsm` | Workbook（就地紅字校正；含`診斷報告`；保留 VBA） |
| `diagnosis/` | Diagnosis notes, correction log, pre-edit backup |
| `csv/` | One CSV per sheet (UTF-8 BOM, from edited workbook) |
| `json/manifest.json` | Sheet index: names, columns, row counts, CSV paths |
| `json/sheets.json` | Full sheet matrices (`sheet -> rows[][]`) |
| `json/records.json` | Flat searchable records |
| `json/records.ndjson` | Same records, one JSON object per line |
| `search.py` | CLI search over `records.json` |
| `export_from_xlsm.py` | Re-run export after updating the source workbook |

Each searchable record looks like:

```json
{
  "id": 42,
  "sheet": "抗告對照表",
  "row": 3,
  "fields": { "主題": "...", "比較項目": "...", "民事訴訟法 (條文/內容)": "..." },
  "search_text": "抗告對照表 主題 ... "
}
```

## Search

```bash
# keyword AND search
python3 law-articles/search.py 抗告
python3 law-articles/search.py 民訴 249 --sheet 上訴
python3 law-articles/search.py Acceptance --json
python3 law-articles/search.py 代位 --limit 20
```

## Re-export

```bash
pip install python-calamine
python3 law-articles/export_from_xlsm.py
```

## Notes

- Corrections are written **directly** into the `.xlsm` (red font + `診斷報告` sheet).
- Pre-edit backup: `diagnosis/source_before_edit.xlsm`.
- Empty sheet `領域 >>` may export as an empty CSV.
- Numeric Excel values like `249.0` are normalized to `249` in exports.
- VBA (`CreateSheetIndex`) is retained after in-place edits.
