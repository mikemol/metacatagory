#!/usr/bin/env python3
"""Shared data loading and normalization module.

Provides unified access to planning index, markdown, and YAML data.
Centralizes data loading patterns to reduce duplication across scripts.
"""

import json
from pathlib import Path
from typing import Any, Dict, List, Tuple, Optional

REPO_ROOT = Path(__file__).parent.parent

def load_planning_index() -> List[Dict[str, Any]]:
    """Load planning index from JSON (supports list or {items:[...]}).
    
    Returns:
        List of planning items
        
    Raises:
        FileNotFoundError: If build/planning_index.json doesn't exist
    """
    json_path = REPO_ROOT / "build" / "planning_index.json"
    if not json_path.exists():
        raise FileNotFoundError(
            f"Planning index not found at {json_path}. "
            "Run: make planning-index-json"
        )
    
    with open(json_path) as f:
        data = json.load(f)
    
    if isinstance(data, list):
        return data
    if isinstance(data, dict):
        return data.get("items", [])
    
    raise ValueError(f"Unexpected JSON shape in {json_path}")


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
        # Minimal fallback: parse key: value lines
        for yaml_block in yaml_blocks:
            entry: Dict[str, Any] = {}
            for line in yaml_block.splitlines():
                if ":" in line:
                    key, value = line.split(":", 1)
                    entry[key.strip()] = value.strip()
            if entry:
                frontmatter_items.append(entry)
    
    # Extract IDs from frontmatter and text fallback
    ids = set()
    for item in frontmatter_items:
        if 'id' in item:
            ids.add(item['id'])
    
    for match in re.finditer(r'\[([A-Z]+-\d+)\]', content):
        ids.add(match.group(1))
    
    return sorted(ids), frontmatter_items
