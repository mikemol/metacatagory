#!/usr/bin/env python3

"""
SPPF-Composable Onboarding Header

Roadmap: src/agda/Plan/CIM/Utility.agda
Architecture: ARCHITECTURE.md
Onboarding: .github/copilot-instructions.md

Constructive Proof Semantics:
- This script participates in the composable SPPF model, mirroring Agda record patterns for protocol,
    witness, and universal property semantics.
- All logic should be traceable to roadmap nodes and architectural principles.
- For onboarding, review the architecture and roadmap, and recursively revisit related nodes for
    context and composability.

Test report generator for MetaCategory Agda tests.

Scans src/agda/Tests/*.agda files and extracts:
- Declared adapter instances (type names like A.SomeAdapter)
- Status assertions of the form `… ≡ true`
- Count per file and per adapter type

Outputs:
- JSON summary to build/reports/test-report.json (or CI_REPORT_DIR override)
- Markdown summary to build/reports/test-report.md (or CI_REPORT_DIR override)

Heuristic/static analysis only; does not attempt to parse Agda fully.
"""

from __future__ import annotations
import argparse
import json
import sys
from pathlib import Path
from typing import Any

SCRIPT_DIR = Path(__file__).resolve().parent
ROOT = SCRIPT_DIR.parent
sys.path.insert(0, str(ROOT))

from scripts.shared.paths import REPORTS_DIR
from scripts.shared.io import save_json
from scripts.shared.agda_tests import (
    scan_agda_test_file,
    ADAPTER_TYPE_RE,
    MODULE_RE,
    STATUS_ASSERT_RE,
)
from scripts.audax_doc import (
    AUDAXDoc,
    Field,
    Header,
    ListLike,
    Para,
    Raw,
    Str,
    Table,
    render_doc,
)
TESTS_DIR = ROOT / "src" / "agda" / "Tests"
OUT_DIR = REPORTS_DIR

def scan_file(path: Path) -> dict[str, Any]:
    scan = scan_agda_test_file(path)
    return {
        "file": str(path.relative_to(ROOT)),
        "module": scan.module,
        "adapters": scan.adapters,
        "status_assertions": scan.status_assertions,
    }  # type: dict[str, Any]

def summarize(file_reports: list[dict[str, Any]]) -> dict[str, Any]:
    total_status = sum(fr["status_assertions"] for fr in file_reports)
    adapter_counts = {}
    for fr in file_reports:
        for _, t in fr["adapters"]:
            adapter_counts[t] = adapter_counts.get(t, 0) + 1
    return {
        "files": file_reports,
        "total_status_assertions": total_status,
        "adapter_type_counts": adapter_counts,
    }  # type: dict[str, Any]

def build_test_report_doc(summary: dict[str, Any]) -> AUDAXDoc:
    blocks: list[AUDAXBlock] = [
        Header(1, ListLike([Str("MetaCategory Test Report")])),
        Para(
            ListLike(
                [
                    Str(
                        f"Total status assertions: {summary['total_status_assertions']}"
                    )
                ]
            )
        ),
        Header(2, ListLike([Str("Adapter counts by type")])),
        Table(
            header=["Adapter", "Count"],
            rows=[
                ListLike([Str(k), Str(str(v))])
                for k, v in sorted(summary["adapter_type_counts"].items())
            ],
        ),
        Header(2, ListLike([Str("Files")])),
    ]

    for fr in summary["files"]:
        blocks.append(Header(3, ListLike([Str(fr["file"])])))
        if fr.get("module"):
            blocks.append(Field("Module", f"`{fr['module']}`"))
        blocks.append(Field("Status assertions", str(fr["status_assertions"])))
        if fr["adapters"]:
            adapters_md = "\n".join(
                f"- `{name}` : `{typ}`" for name, typ in fr["adapters"]
            )
            blocks.append(Field("Adapters", adapters_md))
        blocks.append(Raw(""))

    return AUDAXDoc(ListLike(blocks))

def write_outputs(summary: dict[str, Any], out_dir: Path) -> None:
    out_dir.mkdir(parents=True, exist_ok=True)
    save_json(out_dir / "test-report.json", summary)
    doc = build_test_report_doc(summary)
    (out_dir / "test-report.md").write_text(render_doc(doc), encoding="utf-8")

def main() -> int:
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
