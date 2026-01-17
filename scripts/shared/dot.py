#!/usr/bin/env python3
"""Shared helpers for parsing DOT dependency graphs."""

from __future__ import annotations

import re
from pathlib import Path
from typing import Dict, List

NODE_LABEL_RE = re.compile(r'm(\d+)\[label="([^"]+)"\]')
EDGE_RE = re.compile(r'm(\d+)\s*->\s*m(\d+)')


def parse_dependency_graph(
    dot_path: Path,
    exclude_prefixes: tuple[str, ...] = (
        "Agda.",
        "Data.",
        "Relation.",
        "Function.",
    ),
) -> Dict[str, List[str]]:
    """Parse Agda dependency graph DOT file into a module->deps mapping."""
    if not dot_path.exists():
        return {}

    content = dot_path.read_text(encoding="utf-8")
    node_labels: dict[str, str] = {}
    for match in NODE_LABEL_RE.finditer(content):
        node_id = f"m{match.group(1)}"
        node_labels[node_id] = match.group(2)

    dependencies: Dict[str, List[str]] = {}
    for match in EDGE_RE.finditer(content):
        src_id = f"m{match.group(1)}"
        dst_id = f"m{match.group(2)}"

        src_module = node_labels.get(src_id)
        dst_module = node_labels.get(dst_id)

        if not src_module or not dst_module:
            continue
        if dst_module.startswith(exclude_prefixes):
            continue
        dependencies.setdefault(src_module, []).append(dst_module)

    return dependencies
