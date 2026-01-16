#!/usr/bin/env python3
"""Export unified planning index to tasks.json format."""

import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

from scripts import shared_data
from scripts.shared.io import save_json


OUTPUT_PATH = shared_data.resolve_tasks_path(repo_root=ROOT)


def export_tasks_json(source_path: Path, output_path: Path):
    """Export planning index to GitHub tasks.json."""
    canonical = shared_data.load_planning_index_from(source_path)
    
    # Filter out legacy items that shouldn't sync to GitHub
    filtered = [
        item for item in canonical
        if not item["id"].startswith("LEGACY-")
    ]
    
    # tasks.json format matches RoadmapItem schema
    save_json(output_path, filtered, indent=4)
    
    print(f"Exported {len(filtered)} items to {output_path}")


def export_tasks_from_planning(output_path: Path, repo_root: Path | None = None) -> None:
    """Export tasks.json using the shared planning loader."""
    items = shared_data.load_planning_index(repo_root=repo_root or ROOT, filter_legacy=True)
    save_json(output_path, items, indent=4)
    print(f"Exported {len(items)} items to {output_path}")

if __name__ == "__main__":
    export_tasks_from_planning(OUTPUT_PATH, repo_root=ROOT)
