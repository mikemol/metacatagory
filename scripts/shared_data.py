#!/usr/bin/env python3
"""Shared data loading and normalization module.

Provides unified access to planning index, markdown, and YAML data.
Centralizes data loading patterns to reduce duplication across scripts.
"""

import json
from pathlib import Path
from typing import Any, Dict, List, Tuple, Optional

REPO_ROOT = Path(__file__).parent.parent

def _load_items(path: Path) -> Tuple[Optional[List[Dict[str, Any]]], str]:
    if not path.exists():
        return None, "missing"
    with open(path) as f:
        data = json.load(f)
    if isinstance(data, list):
        return data, "ok"
    if isinstance(data, dict):
        return data.get("items", []), "ok"
    return None, "invalid"


def load_planning_index() -> List[Dict[str, Any]]:
    """Load planning index from JSON (supports list or {items:[...]}).
    
    Returns:
        List of planning items
        
    Raises:
        FileNotFoundError: If data/planning_index.json doesn't exist
    """
    build_path = REPO_ROOT / "build" / "planning_index.json"
    data_path = REPO_ROOT / "data" / "planning_index.json"
    data_items, data_state = _load_items(data_path)
    build_items, build_state = _load_items(build_path)
    if data_items:
        return data_items
    if build_items:
        return build_items
    if data_state == "ok":
        return data_items or []
    if build_state == "ok":
        return build_items or []
    if data_state == "invalid" or build_state == "invalid":
        raise ValueError(f"Unexpected JSON shape in {data_path}")
    raise FileNotFoundError(
        f"Planning index not found at {data_path}. "
        "Run: make data/planning_index.json"
    )


def load_roadmap_markdown() -> Tuple[List[str], List[Dict[str, Any]]]:
    """Extract roadmap item IDs and frontmatter from ROADMAP.md.
    
    Returns:
        (sorted list of IDs, list of frontmatter dicts)
    """
    import re
    try:
        import yaml  # type: ignore
    except ImportError:
        yaml = None
    
    md_path = REPO_ROOT / "ROADMAP.md"
    if not md_path.exists():
        raise FileNotFoundError(f"ROADMAP.md not found at {md_path}")
    
    content = md_path.read_text()
    
    # Extract YAML frontmatter blocks
    frontmatter_items = []
    yaml_blocks = re.findall(r'```yaml\n(.*?)\n```', content, re.DOTALL)

    if yaml is not None:
        for yaml_block in yaml_blocks:
            try:
                data = yaml.safe_load(yaml_block)
                if data and isinstance(data, dict):
                    frontmatter_items.append(data)
            except yaml.YAMLError:
                pass  # Skip malformed blocks
    else:
        # Robust fallback: parse key/value scalars and basic list blocks.
        # Supports list values under keys like dependencies/tags/files.
        for yaml_block in yaml_blocks:
            entry: Dict[str, Any] = {}
            current_key: str | None = None
            for line in yaml_block.splitlines():
                raw = line.rstrip()
                if not raw.strip():
                    continue
                if raw.lstrip().startswith("- "):
                    if current_key is None:
                        continue
                    item = raw.lstrip()[2:].strip()
                    if item.startswith('"') and item.endswith('"'):
                        item = item[1:-1]
                    entry.setdefault(current_key, []).append(item)
                    continue
                if ":" in raw:
                    key, value = raw.split(":", 1)
                    key = key.strip()
                    value = value.strip()
                    if key.startswith('"') and key.endswith('"'):
                        key = key[1:-1]
                    if value.startswith('"') and value.endswith('"'):
                        value = value[1:-1]
                    if value == "":
                        entry[key] = []
                        current_key = key
                    else:
                        entry[key] = value
                        current_key = None
            if entry:
                frontmatter_items.append(entry)
    
    # Extract IDs from frontmatter and text fallback
    ids = set()
    for item in frontmatter_items:
        if 'id' in item and isinstance(item['id'], str):
            ids.add(item['id'])
    
    for match in re.finditer(r'\[([A-Z]+-\d+)\]', content):
        ids.add(match.group(1))
    
    return sorted(ids), frontmatter_items
