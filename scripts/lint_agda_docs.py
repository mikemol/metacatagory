#!/usr/bin/env python3
"""
Lint Agda sources for missing doc comments.
- Module-level: first non-blank line should start with "--".
- Declarations: each line starting with "record ", "data ", or "postulate "
  should have a doc line immediately above it (ignoring blank lines).
This is a lightweight pass to catch obvious gaps; it does not parse Agda.
"""
from __future__ import annotations

import sys
from pathlib import Path
import json
import os

ROOT = Path(__file__).resolve().parent.parent
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

from scripts.shared.paths import REPORTS_DIR
from scripts.shared.config import get_config


def allow_report_write() -> bool:
    return os.environ.get("MUTATE_OK") == "1" and get_config().report_mode == "write"
SRC = ROOT / "src" / "agda"

DECL_PREFIXES = ("record ", "data ", "postulate ")


def read_lines(path: Path) -> list[str]:
    try:
        return path.read_text(encoding="utf-8").splitlines()
    except Exception:
        return []


def has_module_doc(lines: list[str]) -> bool:
    for line in lines:
        if line.strip() == "":
            continue
        stripped = line.strip()
        # Ignore pragmas/options at the top.
        if stripped.startswith("{-#"):
            continue
        return stripped.startswith("--")
    return False


def decls_missing_docs(lines: list[str]) -> list[int]:
    missing = []
    for idx, line in enumerate(lines):
        stripped = line.strip()
        if not stripped.startswith(DECL_PREFIXES):
            continue
        # Look upward for the nearest non-blank line.
        j = idx - 1
        while j >= 0 and lines[j].strip() == "":
            j -= 1
        if j < 0 or not lines[j].strip().startswith("--"):
            missing.append(idx + 1)  # 1-based for readability
    return missing


def main() -> int:
    agda_files = list(SRC.rglob("*.agda"))
    missing_module = []
    missing_decls = {}

    for path in agda_files:
        rel = path.relative_to(ROOT)
        lines = read_lines(path)
        if not has_module_doc(lines):
            missing_module.append(rel)
        bad = decls_missing_docs(lines)
        if bad:
            missing_decls[str(rel)] = bad

    ok = True
    report = {
        "checked": len(agda_files),
        "missing_module": sorted(str(p) for p in missing_module),
        "missing_decls": {str(p): locs for p, locs in missing_decls.items()},
    }

    if missing_module:
        ok = False
        print("Missing module doc comment (-- ...) in:")
        for p in sorted(missing_module):
            print(f"  - {p}")

    if missing_decls:
        ok = False
        print("Declarations lacking doc comment immediately above:")
        for p in sorted(missing_decls):
            locs = ", ".join(str(n) for n in missing_decls[p])
            print(f"  - {p}: lines {locs}")

    if not ok:
        if allow_report_write():
            REPORTS_DIR.mkdir(parents=True, exist_ok=True)
            (REPORTS_DIR / "docs-lint.json").write_text(json.dumps(report, indent=2))
        else:
            print(json.dumps(report, indent=2))
            print("docs-lint report suppressed (report writing disabled).")
        return 1

    print(f"✓ Doc lint passed ({len(agda_files)} files checked)")
    if allow_report_write():
        REPORTS_DIR.mkdir(parents=True, exist_ok=True)
        (REPORTS_DIR / "docs-lint.json").write_text(json.dumps(report, indent=2))
    else:
        print(json.dumps(report, indent=2))
        print("docs-lint report suppressed (report writing disabled).")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
