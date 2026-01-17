#!/usr/bin/env python3
"""Shared parser for Agda declaration indexing."""

from __future__ import annotations

import re
from dataclasses import dataclass
from pathlib import Path
from typing import Any

DECL_RE = re.compile(
    r"^\s*(?:record|data|postulate|module)\b.*$|^\s*constructor\s+[A-Za-z0-9_]+\b.*$",
    re.MULTILINE,
)
NAME_CAPTURE_RE = re.compile(
    r"^\s*record\s+([A-Za-z0-9_]+)\b|^\s*constructor\s+([A-Za-z0-9_]+)\b"
)


@dataclass(frozen=True)
class AgdaDeclaration:
    file: str
    line: int
    text: str
    name: str | None


def scan_agda_declarations(path: Path) -> list[AgdaDeclaration]:
    """Extract declaration lines from an Agda source file."""
    entries: list[AgdaDeclaration] = []
    text = path.read_text(encoding="utf-8", errors="ignore")
    for i, line in enumerate(text.splitlines(), start=1):
        if DECL_RE.match(line):
            name: str | None = None
            match = NAME_CAPTURE_RE.match(line)
            if match:
                name = match.group(1) or match.group(2)
            entries.append(
                AgdaDeclaration(
                    file=str(path),
                    line=i,
                    text=line.strip(),
                    name=name,
                )
            )
    return entries
