#!/usr/bin/env python3
"""Shared parsing utilities for Agda test files."""

from __future__ import annotations

import re
from dataclasses import dataclass
from pathlib import Path
from typing import Any

MODULE_RE = re.compile(r"^\s*module\s+([A-Za-z0-9_.]+)\s+where\s*$")
# E.g., `chk2s2B : S2.KernelPairDeclaration`
RECORD_DECL_RE = re.compile(r"^\s*([A-Za-z0-9_']+)\s*:\s*([A-Za-z0-9_.]+)\.([A-Za-z0-9_]+)\s*$")
# E.g., `foo : A.KernelPairAdapter` (also matches status lines in tests)
ADAPTER_TYPE_RE = re.compile(r"^\s*([a-zA-Z0-9_\-']+)\s*:\s*A\.([A-Za-z0-9_]+)\b")
STATUS_ASSERT_RE = re.compile(
    r"^\s*([a-zA-Z0-9_\-']+)\s*:\s*A\.[A-Za-z0-9_]+\s+[a-zA-Z0-9_\-']+\s*≡\s*(?:B\.)?true\s*$"
)
TOTAL_ASSERTIONS_RE = re.compile(r"totalAssertions ≡ (\d+)")


@dataclass
class AgdaTestScan:
    module: str | None
    records: list[str]
    adapters: list[tuple[str, str]]
    status_assertions: int


def scan_agda_test_file(path: Path) -> AgdaTestScan:
    """Scan an Agda test file for module, record, adapter, and status assertions."""
    module_name: str | None = None
    records: set[str] = set()
    adapters: list[tuple[str, str]] = []
    status_assertions = 0

    for line in path.read_text(encoding="utf-8").splitlines():
        mod_match = MODULE_RE.match(line)
        if mod_match:
            module_name = mod_match.group(1)
        record_match = RECORD_DECL_RE.match(line)
        if record_match:
            records.add(record_match.group(3))
        adapter_match = ADAPTER_TYPE_RE.match(line)
        if adapter_match:
            adapters.append((adapter_match.group(1), adapter_match.group(2)))
        if STATUS_ASSERT_RE.match(line):
            status_assertions += 1

    return AgdaTestScan(
        module=module_name,
        records=sorted(records),
        adapters=adapters,
        status_assertions=status_assertions,
    )


def parse_total_assertions(content: str) -> int | None:
    """Parse totalAssertions value from CoverageReport.agda content."""
    match = TOTAL_ASSERTIONS_RE.search(content)
    if not match:
        return None
    return int(match.group(1))
