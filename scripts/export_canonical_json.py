#!/usr/bin/env python3
"""Export unified planning index to tasks.json format."""

import json
from pathlib import Path


ROOT = Path(__file__).resolve().parent.parent
PLANNING_PATH = ROOT / "build" / "planning_index.json"
DATA_PLANNING_PATH = ROOT / "data" / "planning_index.json"
OUTPUT_PATH = ROOT / ".github" / "roadmap" / "tasks.json"


def _load_items(path: Path):
    try:
        data = json.loads(path.read_text())
    except (FileNotFoundError, json.JSONDecodeError):
        return None
    return data if isinstance(data, list) else None


def resolve_planning_path() -> Path:
    data_items = _load_items(DATA_PLANNING_PATH)
    if data_items:
        return DATA_PLANNING_PATH
    build_items = _load_items(PLANNING_PATH)
    if build_items is not None:
        return PLANNING_PATH
    return DATA_PLANNING_PATH


def export_tasks_json(source_path: Path, output_path: Path):
    """Export planning index to GitHub tasks.json."""
    with open(source_path) as f:
        canonical = json.load(f)
    
    # Filter out legacy items that shouldn't sync to GitHub
    filtered = [
        item for item in canonical
        if not item["id"].startswith("LEGACY-")
    ]
    
    # tasks.json format matches RoadmapItem schema
    with open(output_path, 'w') as f:
        json.dump(filtered, f, indent=4)
    
    print(f"Exported {len(filtered)} items to {output_path}")

if __name__ == "__main__":
    export_tasks_json(resolve_planning_path(), OUTPUT_PATH)
