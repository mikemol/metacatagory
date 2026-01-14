#!/usr/bin/env python3
"""Validate ROADMAP.md contains items from canonical using shared composition."""

from pathlib import Path
import sys

# Ensure repository root is importable as a package (scripts.*)
_REPO_ROOT = Path(__file__).resolve().parent.parent
if str(_REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(_REPO_ROOT))

from scripts.shared.composition import run_validate_roadmap_md
from scripts.shared.logging import StructuredLogger


def validate() -> int:
    base = Path(__file__).resolve().parent.parent
    logger = StructuredLogger("validate_md")
    exit_code, ctx = run_validate_roadmap_md(base, logger=logger, strict=True)

    report = ctx.get('validation_report', {})
    missing = report.get('missing_sample', [])
    extra = report.get('extra_sample', [])
    overlap = report.get('overlap', 0)

    if report.get('missing_count', 0):
        print(f"❌ {report['missing_count']} canonical items missing from ROADMAP.md:")
        for title in missing:
            print(f"  - {title}")
        if report['missing_count'] > len(missing):
            print(f"  ... and {report['missing_count'] - len(missing)} more")

    if report.get('extra_count', 0):
        print(f"❌ {report['extra_count']} items in ROADMAP.md not in canonical:")
        for title in extra:
            print(f"  - {title}")
        if report['extra_count'] > len(extra):
            print(f"  ... and {report['extra_count'] - len(extra)} more")

    if exit_code == 0:
        print(f"✓ ROADMAP.md matches canonical ({overlap} items)")
    else:
        print(f"\n⚠️  Partial match: {overlap} items overlap")
        print("❌ Validation failed: run 'make ROADMAP.md' to sync")
    return exit_code

if __name__ == "__main__":
    sys.exit(validate())
