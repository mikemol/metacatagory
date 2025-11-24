#!/usr/bin/env python3
"""
Test report generator for MetaCategory Agda tests.

Scans src/agda/Tests/*.agda files and extracts:
- Declared adapter instances (type names like A.SomeAdapter)
- Status assertions of the form `… ≡ true`
- Count per file and per adapter type

Outputs:
- JSON summary to build/reports/test-report.json
- Markdown summary to build/reports/test-report.md

Heuristic/static analysis only; does not attempt to parse Agda fully.
"""

from __future__ import annotations
import argparse
import json
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
TESTS_DIR = ROOT / "src" / "agda" / "Tests"
OUT_DIR = ROOT / "build" / "reports"

ADAPTER_TYPE_RE = re.compile(r"^\s*([a-zA-Z0-9_\-']+)\s*:\s*A\.([A-Za-z0-9_]+)\b")
STATUS_ASSERT_RE = re.compile(
    r"^\s*([a-zA-Z0-9_\-']+)\s*:\s*A\.[A-Za-z0-9_]+\s+[a-zA-Z0-9_\-']+\s*≡\s*(?:B\.)?true\s*$"
)
MODULE_RE = re.compile(r"^\s*module\s+([A-Za-z0-9_.]+)\s+where\s*$")

# Allow True/False variants some files use
ALT_TRUE_RE = re.compile(r"\b(?:B\.)?true\b|\bTrue\b")


def scan_file(path: Path):
    adapters = []  # list of (name, type)
    statuses = 0
    module_name = None
    for line in path.read_text(encoding="utf-8").splitlines():
        m = MODULE_RE.match(line)
        if m:
            module_name = m.group(1)
        m = ADAPTER_TYPE_RE.match(line)
        if m:
            adapters.append((m.group(1), m.group(2)))
        if STATUS_ASSERT_RE.match(line):
            statuses += 1
    return {
        "file": str(path.relative_to(ROOT)),
        "module": module_name,
        "adapters": adapters,
        "status_assertions": statuses,
    }


def summarize(file_reports):
    total_status = sum(fr["status_assertions"] for fr in file_reports)
    adapter_counts = {}
    for fr in file_reports:
        for _, t in fr["adapters"]:
            adapter_counts[t] = adapter_counts.get(t, 0) + 1
    return {
        "files": file_reports,
        "total_status_assertions": total_status,
        "adapter_type_counts": adapter_counts,
    }


def write_outputs(summary, out_dir: Path):
    out_dir.mkdir(parents=True, exist_ok=True)
    (out_dir / "test-report.json").write_text(
        json.dumps(summary, indent=2), encoding="utf-8"
    )
    # Markdown
    lines = []
    lines.append("# MetaCategory Test Report\n")
    lines.append(f"Total status assertions: {summary['total_status_assertions']}\n")
    lines.append("\n## Adapter counts by type\n")
    for k, v in sorted(summary["adapter_type_counts"].items()):
        lines.append(f"- {k}: {v}")
    lines.append("\n## Files\n")
    for fr in summary["files"]:
        lines.append(f"### {fr['file']}")
        if fr.get("module"):
            lines.append(f"- module: `{fr['module']}`")
        lines.append(f"- status assertions: {fr['status_assertions']}")
        if fr["adapters"]:
            lines.append("- adapters:")
            for name, typ in fr["adapters"]:
                lines.append(f"  - `{name}` : `{typ}`")
        lines.append("")
    (out_dir / "test-report.md").write_text("\n".join(lines), encoding="utf-8")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--tests-dir", default=str(TESTS_DIR), help="Directory with Agda Tests/*.agda"
    )
    parser.add_argument(
        "--out-dir", default=str(OUT_DIR), help="Output directory for reports"
    )
    args = parser.parse_args()

    tests_dir = Path(args.tests_dir)
    out_dir = Path(args.out_dir)

    if not tests_dir.exists():
        print(f"Tests dir not found: {tests_dir}", file=sys.stderr)
        return 2

    files = sorted(tests_dir.glob("*.agda"))
    file_reports = [scan_file(p) for p in files]
    summary = summarize(file_reports)
    write_outputs(summary, out_dir)
    print(f"Wrote report to {out_dir}/test-report.(json|md)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
