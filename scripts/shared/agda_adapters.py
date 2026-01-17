#!/usr/bin/env python3
"""Shared parser utilities for Agda adapter records."""

from __future__ import annotations

import re
from dataclasses import dataclass
from typing import Optional


@dataclass
class AdapterRecord:
    """Represents an Agda adapter record definition."""

    name: str
    decl_field: str
    decl_type: str
    has_status: bool
    fields: list[tuple[str, str]]
    constructor_name: str
    start_line: int
    end_line: int


RECORD_RE = re.compile(r"record\s+(\w+Adapter)\s*:")
FIELD_RE = re.compile(r"(\w+)\s*:\s*(.+)")
ADAPTER_RECORD_COUNT_RE = re.compile(r"record\s+\w+Adapter\s*:")


def parse_adapter_record(lines: list[str], start_idx: int) -> Optional[AdapterRecord]:
    """Parse an adapter record definition from Agda source."""
    record_match = RECORD_RE.match(lines[start_idx])
    if not record_match:
        return None

    adapter_name = record_match.group(1)
    fields: list[tuple[str, str]] = []
    decl_field = None
    decl_type = None
    has_status = False

    i = start_idx + 1
    while i < len(lines) and not lines[i].strip().startswith("mk"):
        line = lines[i].strip()

        field_match = FIELD_RE.match(line)
        if field_match:
            field_name = field_match.group(1)
            field_type = field_match.group(2)
            fields.append((field_name, field_type))

            if field_name == "decl":
                decl_field = field_name
                decl_type = field_type
            elif field_name == "status":
                has_status = True

        i += 1

    constructor_name = f"mk{adapter_name}"

    return AdapterRecord(
        name=adapter_name,
        decl_field=decl_field or "decl",
        decl_type=decl_type or "Unknown",
        has_status=has_status,
        fields=fields,
        constructor_name=constructor_name,
        start_line=start_idx,
        end_line=i,
    )


def count_adapter_records(content: str) -> int:
    """Count adapter record definitions in content."""
    return len(ADAPTER_RECORD_COUNT_RE.findall(content))
