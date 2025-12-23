#!/usr/bin/env python3
"""Export canonical roadmap to tasks.json format."""

import json
from pathlib import Path

def export_tasks_json(canonical_path: Path, output_path: Path):
    """Export canonical to GitHub tasks.json."""
    with open(canonical_path) as f:
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
    base = Path("/home/mikemol/github/metacatagory")
    export_tasks_json(
        base / "build/canonical_roadmap.json",
        base / ".github/roadmap/tasks.json"
    )
