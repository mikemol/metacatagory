#!/usr/bin/env python3
"""Shared Agda roadmap parsing helpers."""

from __future__ import annotations

import re
from pathlib import Path
from typing import Dict, List

from .normalization import ensure_provenance


def _unescape_string(value: str) -> str:
    try:
        return bytes(value, "utf-8").decode("unicode_escape")
    except Exception:
        return value


def parse_ingested_agda(base_path: Path) -> List[Dict]:
    """Extract roadmap steps from IngestedRoadmaps/*.agda modules."""
    items: List[Dict] = []
    ingested_dir = base_path / "src/agda/Plan/CIM/IngestedRoadmaps"

    if not ingested_dir.exists():
        return items

    str_lit = r'"((?:[^"\\\\]|\\\\.)+)"'
    pattern = (
        rf'roadmap(Gp\d+) : RoadmapStep\s+roadmap\1 = record\s+\{{[^}}]+'
        rf'provenance\s+=\s+{str_lit}[^}}]+'
        rf'step\s+=\s+{str_lit}[^}}]+'
        rf'status\s+=\s+{str_lit}[^}}]+'
        rf'targetModule\s+=\s+{str_lit}'
    )

    for agda_file in ingested_dir.glob("*.agda"):
        content = agda_file.read_text(encoding="utf-8")
        matches = re.finditer(pattern, content, re.DOTALL)

        for match in matches:
            gp_id, provenance, step, status, target = match.groups()
            item = {
                "id": f"GP-{gp_id}",
                "title": _unescape_string(provenance.strip()),
                "description": _unescape_string(step.strip()),
                "status": _unescape_string(status.strip()),
                "category": "IngestedGP",
                "source": f"Plan/CIM/IngestedRoadmaps/{agda_file.name}",
                "files": [_unescape_string(target.strip())],
                "tags": ["GP"],
                "dependsOn": [],
                "related": [],
                "provenance": [],
            }
            ensure_provenance(item)
            items.append(item)

    return items


def parse_legacy_agda(base_path: Path) -> List[Dict]:
    """Extract items from legacy roadmap-*.agda files."""
    items: List[Dict] = []

    for agda_file in base_path.glob("roadmap-*.agda"):
        content = agda_file.read_text(encoding="utf-8")
        if len(content) > 100000:
            continue

        type_pattern = r'data (\w+) : Set where'
        for match in re.finditer(type_pattern, content):
            type_name = match.group(1)
            item = {
                "id": f"LEGACY-{agda_file.stem}-{type_name}",
                "title": f"Type: {type_name} from {agda_file.name}",
                "status": "completed",
                "category": "LegacyAgda",
                "source": agda_file.name,
                "files": [str(agda_file)],
                "tags": ["legacy", "agda"],
                "dependsOn": [],
                "related": [],
                "provenance": [],
            }
            ensure_provenance(item)
            items.append(item)

    return items
