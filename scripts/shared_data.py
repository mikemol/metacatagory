#!/usr/bin/env python3
"""Shared data loading and normalization module.

Provides unified access to planning index, markdown, and YAML data.
Centralizes data loading patterns to reduce duplication across scripts.
"""

import json
from json import JSONDecodeError
from pathlib import Path
from typing import Any, Dict, List, Tuple, Optional

REPO_ROOT = Path(__file__).parent.parent

def _load_items(path: Path) -> Tuple[Optional[List[Dict[str, Any]]], str]:
    try:
        with open(path) as f:
            data = json.load(f)
    except FileNotFoundError:
        return None, "missing"
    except JSONDecodeError:
        return None, "invalid"
    if isinstance(data, list):
        return data, "ok"
    if isinstance(data, dict):
        return data.get("items", []), "ok"
    return None, "invalid"


def resolve_planning_path(repo_root: Optional[Path] = None) -> Path:
    """Resolve preferred planning index path (data first, then build)."""
    root = repo_root or REPO_ROOT
    build_path = root / "build" / "planning_index.json"
    data_path = root / "data" / "planning_index.json"
    data_items, _ = _load_items(data_path)
    if data_items:
        return data_path
    build_items, build_state = _load_items(build_path)
    if build_items is not None or build_state == "ok":
        return build_path
    return data_path


def resolve_tasks_path(repo_root: Optional[Path] = None) -> Path:
    """Resolve tasks.json path."""
    root = repo_root or REPO_ROOT
    return root / ".github" / "roadmap" / "tasks.json"


def load_planning_index_from(path: Path) -> List[Dict[str, Any]]:
    """Load planning index from an explicit path."""
    items, state = _load_items(path)
    if state == "missing":
        raise FileNotFoundError(
            f"Planning index not found at {path}. "
            "Run: make data/planning_index.json"
        )
    if state == "invalid":
        raise ValueError(f"Unexpected JSON shape in {path}")
    return items or []


def load_tasks_json_from(path: Path, required: bool = True) -> List[Dict[str, Any]]:
    """Load tasks.json from an explicit path."""
    items, state = _load_items(path)
    if state == "missing":
        if not required:
            return []
        raise FileNotFoundError(
            f"tasks.json not found at {path}. "
            "Run: make .github/roadmap/tasks.json"
        )
    if state == "invalid":
        raise ValueError(f"Unexpected JSON shape in {path}")
    return items or []


def load_tasks_json(
    repo_root: Optional[Path] = None,
    required: bool = True,
) -> List[Dict[str, Any]]:
    """Load tasks.json from repo root."""
    path = resolve_tasks_path(repo_root=repo_root)
    return load_tasks_json_from(path, required=required)


def load_planning_index(
    repo_root: Optional[Path] = None,
    filter_legacy: bool = False,
) -> List[Dict[str, Any]]:
    """Load planning index from JSON (supports list or {items:[...]}).
    
    Returns:
        List of planning items
        
    Raises:
        FileNotFoundError: If data/planning_index.json doesn't exist
    """
    root = repo_root or REPO_ROOT
    build_path = root / "build" / "planning_index.json"
    data_path = root / "data" / "planning_index.json"
    data_items, data_state = _load_items(data_path)
    build_items, build_state = _load_items(build_path)
    if data_items:
        items = data_items
    elif build_items:
        items = build_items
    elif data_state == "ok":
        items = data_items or []
    elif build_state == "ok":
        items = build_items or []
    else:
        if data_state == "invalid" or build_state == "invalid":
            raise ValueError(f"Unexpected JSON shape in {data_path}")
        raise FileNotFoundError(
            f"Planning index not found at {data_path}. "
            "Run: make data/planning_index.json"
        )
    if filter_legacy:
        items = [
            item for item in items
            if not str(item.get("id", "")).startswith("LEGACY-")
        ]
    return items


def load_roadmap_markdown_from(md_path: Path) -> Tuple[List[str], List[Dict[str, Any]]]:
    """Extract roadmap item IDs and frontmatter from an explicit path."""
    from scripts.shared.markdown import (
        extract_bracketed_ids,
        parse_yaml_fenced_blocks_fallback,
    )

    if not md_path.exists():
        raise FileNotFoundError(f"ROADMAP.md not found at {md_path}")

    content = md_path.read_text()
    
    # Extract YAML frontmatter blocks (with fallback parsing)
    frontmatter_items: List[Dict[str, Any]] = []
    for data in parse_yaml_fenced_blocks_fallback(content):
        if data and isinstance(data, dict):
            frontmatter_items.append(data)
    
    # Extract IDs from frontmatter and text fallback
    ids = set()
    for item in frontmatter_items:
        if 'id' in item and isinstance(item['id'], str):
            ids.add(item['id'])
    
    for item_id in extract_bracketed_ids(content):
        ids.add(item_id)
    
    return sorted(ids), frontmatter_items


def load_roadmap_markdown() -> Tuple[List[str], List[Dict[str, Any]]]:
    """Extract roadmap item IDs and frontmatter from ROADMAP.md.
    
    Returns:
        (sorted list of IDs, list of frontmatter dicts)
    """
    md_path = REPO_ROOT / "ROADMAP.md"
    return load_roadmap_markdown_from(md_path)


def validate_roadmap_frontmatter(items: List[Dict[str, Any]]):
    """Validate roadmap frontmatter against the RoadmapItem schema."""
    from scripts.shared.validation import roadmap_item_validator, ValidationResult

    result = ValidationResult()
    for idx, item in enumerate(items):
        result.merge(roadmap_item_validator(item, path=f"items[{idx}]"))
    return result
