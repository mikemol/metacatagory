#!/usr/bin/env python3
"""
Hybrid test coverage reporter: Agda-validated metadata + Python generation

Approach:
1. Tests/CoverageReport.agda contains type-checked adapter registry
2. This script extracts data from that structured Agda file (not random source)
3. Cross-validates against actual Test/*.agda files
4. Generates reports with confidence

This is more robust than pure regex while being immediately implementable.
"""

from pathlib import Path
import re

ROOT = Path(__file__).resolve().parents[1]
TESTS_DIR = ROOT / "src" / "agda" / "Tests"
COVERAGE_REPORT_FILE = TESTS_DIR / "CoverageReport.agda"
OUT_DIR = ROOT / "build" / "reports"


def parse_coverage_metadata():
    """
    Parse the structured metadata from Tests/CoverageReport.agda

    This file contains a type-checked registry of all adapters.
    If an adapter is listed there but doesn't exist, Agda won't compile.
    If an adapter exists but isn't listed, our totalAssertions check fails.
    """
    if not COVERAGE_REPORT_FILE.exists():
        print(f"Warning: {COVERAGE_REPORT_FILE} not found, falling back to regex scan")
        return None

    content = COVERAGE_REPORT_FILE.read_text()

    # Extract the expected total from the type-checked equality
    # This line MUST match reality or Agda compilation fails
    total_match = re.search(r"totalAssertions ≡ (\d+)", content)
    if total_match:
        expected_total = int(total_match.group(1))
        print(f"Agda-verified expected total: {expected_total}")
        return {"expected_total": expected_total}

    return None


def scan_checklists_with_validation(expected_metadata):
    """
    Scan actual checklist files and validate against Agda metadata
    """
    from scripts.test_report import scan_file  # Import existing scanner

    checklist_files = sorted(TESTS_DIR.glob("*Checklist.agda"))
    file_reports = [scan_file(f) for f in checklist_files]

    actual_total = sum(fr["status_assertions"] for fr in file_reports)

    if expected_metadata and "expected_total" in expected_metadata:
        expected = expected_metadata["expected_total"]
        if actual_total != expected:
            print(f"\n⚠️  WARNING: Metadata mismatch!")
            print(f"   Expected (from Agda): {expected}")
            print(f"   Actual (from scan):   {actual_total}")
            print(f"   Someone forgot to update Tests/CoverageReport.agda!")
        else:
            print(f"✓ Validation passed: {actual_total} assertions match Agda metadata")

    return file_reports, actual_total


def main():
    """Generate coverage report with Agda validation"""
    print("Hybrid Coverage Reporter")
    print("=" * 60)

    # Step 1: Load type-checked metadata from Agda
    metadata = parse_coverage_metadata()

    # Step 2: Scan actual files (using existing logic)
    file_reports, actual_total = scan_checklists_with_validation(metadata)

    # Step 3: Generate output (reuse existing formatting)
    from scripts.test_report import summarize, write_outputs

    summary = summarize(file_reports)
    write_outputs(summary, OUT_DIR)

    print(f"\nReport written to {OUT_DIR}/test-report.md")
    print(f"Total assertions: {actual_total}")


if __name__ == "__main__":
    main()
