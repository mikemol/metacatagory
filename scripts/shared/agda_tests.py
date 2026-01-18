#!/usr/bin/env python3
"""Shared parsing utilities for Agda test files."""

from __future__ import annotations

import re
from dataclasses import dataclass
from pathlib import Path
from typing import Any

from .agda import extract_module_name
# E.g., `chk2s2B : S2.KernelPairDeclaration`
RECORD_DECL_RE = re.compile(r"^\s*([A-Za-z0-9_']+)\s*:\s*([A-Za-z0-9_.]+)\.([A-Za-z0-9_]+)\s*$")
# E.g., `foo : A.KernelPairAdapter` (also matches status lines in tests)
ADAPTER_TYPE_RE = re.compile(r"^\s*([a-zA-Z0-9_\-']+)\s*:\s*A\.([A-Za-z0-9_]+)\b")
STATUS_ASSERT_RE = re.compile(
    r"^\s*([a-zA-Z0-9_\-']+)\s*:\s*A\.[A-Za-z0-9_]+\s+[a-zA-Z0-9_\-']+\s*≡\s*(?:B\.)?true\s*$"
)
TOTAL_ASSERTIONS_RE = re.compile(r"totalAssertions ≡ (\d+)")
CHECKLIST_ADAPTER_RE = re.compile(r"(\w+)-adapter\s*:\s*A\.(\w+)", re.MULTILINE)
CHAPTER_NAME_RE = re.compile(r"(Chapter\d+)")
SECTION_HEADER_RE = re.compile(r"^-+\s*\n--\s+Level\d+sub(\d+)", re.MULTILINE)
LINK_ASSERT_RE = re.compile(r"(\w+)-\w+-link\s*:\s*.*?≡\s*([\w.]+)", re.MULTILINE)


@dataclass
class AgdaTestScan:
    module: str | None
    records: list[str]
    adapters: list[tuple[str, str]]
    status_assertions: int


def scan_agda_test_file(path: Path) -> AgdaTestScan:
    """Scan an Agda test file for module, record, adapter, and status assertions."""
    content = path.read_text(encoding="utf-8")
    module_name = extract_module_name(content)
    records: set[str] = set()
    adapters: list[tuple[str, str]] = []
    status_assertions = 0

    for line in content.splitlines():
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


def iter_checklist_adapters(content: str) -> list[tuple[str, str, int]]:
    """Return checklist adapter name/type/position triples."""
    return [
        (match.group(1), match.group(2), match.start())
        for match in CHECKLIST_ADAPTER_RE.finditer(content)
    ]


def extract_chapter_from_filename(filename: str) -> str | None:
    """Extract ChapterN from a filename."""
    match = CHAPTER_NAME_RE.match(filename)
    return match.group(1) if match else None


def extract_sections_from_content(content: str) -> list[str]:
    """Extract section numbers from checklist content."""
    sections = {match.group(1) for match in SECTION_HEADER_RE.finditer(content)}
    return sorted(sections, key=int)


def iter_checklist_links(content: str) -> list[tuple[str, str]]:
    """Return source adapter names and target references for checklist links."""
    return [
        (match.group(1), match.group(2))
        for match in LINK_ASSERT_RE.finditer(content)
    ]


def infer_section_from_preceding(preceding_text: str) -> str:
    """Infer section number from preceding context."""
    level_matches = re.findall(r"Level\d+sub(\d+)", preceding_text)
    if level_matches:
        return level_matches[-1]
    chk_matches = re.findall(r"chk\d+s(\d+)", preceding_text)
    if chk_matches:
        return chk_matches[-1]
    return "0"
