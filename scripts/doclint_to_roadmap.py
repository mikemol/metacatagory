#!/usr/bin/env python3
"""
Convert docs-lint.json into a roadmap-like JSON fragment so missing docs show
up as actionable items. Output: build/doclint_roadmap.json containing a list
of items with fields compatible with RoadmapItem.
"""
from __future__ import annotations

import json
import os
from pathlib import Path
import sys

ROOT = Path(__file__).resolve().parent.parent
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

report_dir_env = os.getenv("CI_REPORT_DIR")
if report_dir_env:
    report_dir = Path(report_dir_env)
    if not report_dir.is_absolute():
        report_dir = ROOT / report_dir
else:
    report_dir = ROOT / "build" / "reports"

REPORT = report_dir / "docs-lint.json"
OUT = ROOT / "build" / "doclint_roadmap.json"


def make_item(file_path: str, kind: str) -> dict:
    return {
        "id": f"DOC-LINT::{file_path}",
        "title": f"Add docs for {file_path}",
        "status": "not-started",
        "category": "Documentation",
        "source": "DocLint",
        "files": [file_path],
        "tags": ["Docs", kind],
        "dependsOn": [],
        "provenance": [str(REPORT)],
        "related": [],
    }


def main() -> int:
    if not REPORT.exists():
        print(f"docs-lint report not found at {REPORT}. Run lint first.")
        return 1

    data = json.loads(REPORT.read_text())
    items = []

    for path in data.get("missing_module", []):
        items.append(make_item(path, "ModuleDoc"))

    for path, lines in data.get("missing_decls", {}).items():
        items.append(make_item(path, f"DeclDocs@{','.join(map(str, lines))}"))

    OUT.parent.mkdir(parents=True, exist_ok=True)
    OUT.write_text(json.dumps(items, indent=2))
    print(f"Wrote {len(items)} doc-lint roadmap items to {OUT}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
