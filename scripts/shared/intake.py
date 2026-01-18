#!/usr/bin/env python3
"""Shared helpers for intake file parsing."""

from __future__ import annotations

import re
from pathlib import Path

ID_PATTERN = re.compile(
    r"\b(?:PHASE-[A-Za-z0-9.\-]+|ROADMAP-MD-\d+|GP-[A-Za-z0-9.\-]+)\b"
)
SHARD_PATTERN = re.compile(r"__(\(\d+\))?\.md$")


def find_roadmap_ids(text: str) -> set[str]:
    """Find roadmap IDs within text."""
    return set(ID_PATTERN.findall(text))


def is_shard_filename(name: str) -> bool:
    """Return True when a filename looks like an intake shard."""
    return bool(SHARD_PATTERN.search(name))


def classify_intake_filename(name: str) -> str:
    """Classify an intake filename."""
    lower = name.lower()
    if is_shard_filename(name):
        return "shard"
    if "candidate" in lower or "draft" in lower:
        return "candidate"
    if any(
        keyword in lower
        for keyword in ["context", "summary", "enrichment", "session", "codex"]
    ):
        return "substrate"
    return "formalized"


def read_intake_text(path: Path) -> str:
    """Load intake file content with consistent encoding handling."""
    return path.read_text(errors="ignore")
