#!/usr/bin/env python3
"""Shared rendering helpers for GP roadmap Agda records."""

from __future__ import annotations

import re
from typing import Dict, List


def sanitize_string(value: str) -> str:
    """Prepare text to embed inside Agda string literals."""
    escaped = value.replace('"', "'")
    escaped = escaped.replace("\\", "\\\\")
    return " ".join(escaped.split()).strip()


def build_implication(metadata: Dict[str, str]) -> str:
    """Build implication text from structured metadata."""
    insight = sanitize_string(metadata.get("insight", ""))
    gap = sanitize_string(metadata.get("gap", ""))
    fix = sanitize_string(metadata.get("fix", ""))

    parts: List[str] = []
    if insight:
        parts.append(f"Insight: {insight[:120]}")
    if gap:
        parts.append(f"Gap: {gap[:120]}")
    if fix:
        parts.append(f"Fix: {fix[:120]}")
    return " | ".join(parts) if parts else "Implication TBD from intake."


def build_step_summary(metadata: Dict[str, str]) -> str:
    """Use the structured summary for the roadmap step."""
    return sanitize_string(metadata.get("summary", ""))


def build_implication_from_concepts(concepts: List[str]) -> str:
    """Add a compact concepts clause to the implication string."""
    if not concepts:
        return ""
    return f"Concepts: {', '.join(concepts[:5])}"


def record_name_for_gp(gp_id: str) -> str:
    """Normalize GP id to Agda record name suffix."""
    safe = gp_id.replace("/", "").lower()
    safe = re.sub(r"[^a-z0-9]", "", safe)
    if not safe.startswith("gp"):
        safe = f"gp{safe}"
    return safe


def render_roadmap_step(
    gp_id: str,
    title: str,
    step: str,
    implication: str,
    target_module: str,
    status: str = "not-started",
) -> str:
    """Render a RoadmapStep record in Agda."""
    record_name = record_name_for_gp(gp_id)
    title_safe = sanitize_string(title)[:80]
    step_safe = sanitize_string(step)
    implication_safe = sanitize_string(implication)
    target_safe = sanitize_string(target_module)

    return f'''roadmap{record_name.capitalize()} : RoadmapStep
roadmap{record_name.capitalize()} = record
    {{ provenance   = "{gp_id}: {title_safe}"
    ; relatedNodes = []
    ; step         = "{step_safe}"
    ; implication  = "{implication_safe}"
    ; status       = "{status}"
    ; targetModule = "{target_safe}"
    ; next         = []
    }}
'''
