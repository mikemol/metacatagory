#!/usr/bin/env python3
"""Validate tasks.json against canonical index."""

import json
from pathlib import Path
import sys

def normalize_item(item):
    """Normalize for comparison."""
    return {
        "id": item["id"],
        "title": item["title"].strip(),
        "status": item["status"],
        "category": item["category"],
        "files": sorted(item["files"]),
        "tags": sorted(item["tags"]),
        "dependsOn": sorted(item["dependsOn"]),
        "provenance": sorted(item.get("provenance", [])),
    }

def validate():
    base = Path(__file__).resolve().parent.parent
    canonical = json.load(open(base / "build/canonical_roadmap.json"))
    tasks = json.load(open(base / ".github/roadmap/tasks.json"))
    
    # Build canonical dict (exclude legacy items not in tasks.json)
    canonical_dict = {
        item["id"]: normalize_item(item)
        for item in canonical
        if not item["id"].startswith("LEGACY-")
    }
    
    # Build tasks dict
    tasks_dict = {item["id"]: normalize_item(item) for item in tasks}
    
    # Compare
    canonical_ids = set(canonical_dict.keys())
    tasks_ids = set(tasks_dict.keys())
    
    only_canonical = canonical_ids - tasks_ids
    only_tasks = tasks_ids - canonical_ids
    
    if only_canonical:
        print(f"❌ {len(only_canonical)} items only in canonical:")
        for item_id in sorted(only_canonical):
            print(f"  - {item_id}")
    
    if only_tasks:
        print(f"❌ {len(only_tasks)} items only in tasks.json:")
        for item_id in sorted(only_tasks):
            print(f"  - {item_id}")
    
    # Check matching items for content drift
    common = canonical_ids & tasks_ids
    drifted = []
    for item_id in common:
        if canonical_dict[item_id] != tasks_dict[item_id]:
            drifted.append(item_id)
    
    if drifted:
        print(f"❌ {len(drifted)} items drifted:")
        for item_id in sorted(drifted):
            print(f"  - {item_id}")
    
    if not (only_canonical or only_tasks or drifted):
        print(f"✓ tasks.json matches canonical ({len(common)} items)")
        return 0
    else:
        print(f"\n❌ Validation failed: run 'make roadmap-export-json' to sync")
        return 1

if __name__ == "__main__":
    sys.exit(validate())
