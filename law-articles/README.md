# 藍絲法律 · 法條分群 (searchable export)

Structured CSV/JSON export of `藍絲法律_法條分群(260719更).xlsm`
(Taiwanese law article groupings / comparison tables).

## Contents

| Path | Description |
|---|---|
| `藍絲法律_法條分群(260719更).xlsm` | Source workbook |
| `csv/` | One CSV per sheet (UTF-8 BOM, header row when detected) |
| `json/manifest.json` | Sheet index: names, columns, row counts, CSV paths |
| `json/sheets.json` | Full sheet matrices (`sheet -> rows[][]`) |
| `json/records.json` | Flat searchable records (966 rows) |
| `json/records.ndjson` | Same records, one JSON object per line |
| `search.py` | CLI search over `records.json` |
| `export_from_xlsm.py` | Re-run export after updating the `.xlsm` |

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

- Source has 36 sheets; empty sheet `領域 >>` exports as an empty CSV.
- Numeric Excel values like `249.0` are normalized to `249` in exports.
- VBA in the workbook only builds an Index sheet; it is not needed for search.
