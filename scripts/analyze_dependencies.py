#!/usr/bin/env python3
"""
Analyze suggested dependencies and optionally promote them to the planning index.

Shows which tasks have import-based dependency suggestions,
and provides a tool to promote them to explicit dependsOn in the canonical roadmap.
"""

import json
from pathlib import Path
from typing import List, Dict

from scripts import shared_data

REPO_ROOT = Path(__file__).parent.parent
ENRICHED_JSON = REPO_ROOT / "build" / "canonical_enriched.json"
CANONICAL_JSON = REPO_ROOT / "data" / "planning_index.json"

def analyze_suggestions() -> None:
    """Analyze suggested dependencies."""
    if not ENRICHED_JSON.exists():
        print("Error: canonical_enriched.json not found. Run 'make roadmap-all-enriched' first.")
        return
    
    with open(ENRICHED_JSON, "r") as f:
        enriched = json.load(f)
    
    # Group by task
    tasks_with_suggestions = []
    total_suggestions = 0
    
    for item in enriched:
        suggested = item.get("suggestedDependencies", [])
        if suggested:
            tasks_with_suggestions.append((item.get("id", ""), item.get("title", ""), suggested))
            total_suggestions += len(suggested)
    
    print(f"Dependency Analysis")
    print(f"==================")
    print(f"\nTotal tasks with suggestions: {len(tasks_with_suggestions)}")
    print(f"Total suggested dependencies: {total_suggestions}")
    print(f"Average per task: {total_suggestions / len(tasks_with_suggestions) if tasks_with_suggestions else 0:.1f}")
    
    if tasks_with_suggestions:
        print(f"\nTasks with suggestions (sorted by count):")
        sorted_tasks = sorted(tasks_with_suggestions, key=lambda x: len(x[2]), reverse=True)
        
        for task_id, title, suggestions in sorted_tasks[:15]:  # Top 15
            print(f"\n  {task_id}: {title[:60]}")
            print(f"    Suggests: {', '.join(suggestions[:3])}")
            if len(suggestions) > 3:
                print(f"    (+ {len(suggestions) - 3} more)")
    
    print("\n\nTo promote suggestions to explicit dependencies:")
    print("  python3 scripts/analyze_dependencies.py --promote")
    print("\nTo see a specific task's suggestions:")
    print("  python3 scripts/analyze_dependencies.py --task PHASE-IV.2")

def promote_suggestions() -> None:
    """Promote selected suggestions to canonical dependsOn."""
    if not ENRICHED_JSON.exists() or not CANONICAL_JSON.exists():
        print("Error: Missing enriched or canonical JSON. Run 'make roadmap-all-enriched' first.")
        return
    
    with open(ENRICHED_JSON, "r") as f:
        enriched = json.load(f)
    
    canonical = shared_data.load_planning_index_from(CANONICAL_JSON)
    
    # Build ID->canonical mapping
    canonical_by_id = {item["id"]: item for item in canonical}
    
    promoted_count = 0
    
    # Promote strong suggestions (high confidence)
    # For now, promote all that don't conflict with existing dependsOn
    for item in enriched:
        task_id = item.get("id", "")
        suggested = item.get("suggestedDependencies", [])
        
        if not suggested or task_id not in canonical_by_id:
            continue
        
        canonical_item = canonical_by_id[task_id]
        existing_deps = set(canonical_item.get("dependsOn", []))
        
        # Add suggestions not already present
        for dep_id in suggested:
            if dep_id not in existing_deps:
                canonical_item.setdefault("dependsOn", []).append(dep_id)
                promoted_count += 1
    
    # Write back
    with open(CANONICAL_JSON, "w") as f:
        json.dump(canonical, f, indent=4, ensure_ascii=False)
    
    print(f"✓ Promoted {promoted_count} dependencies to canonical")
    print(f"  Updated: {CANONICAL_JSON}")
    print("\nRun the pipeline again to see updated enrichment:")
    print("  make roadmap-all-enriched")

def show_task_suggestions(task_id: str) -> None:
    """Show suggestions for a specific task."""
    if not ENRICHED_JSON.exists():
        print("Error: canonical_enriched.json not found. Run 'make roadmap-all-enriched' first.")
        return
    
    with open(ENRICHED_JSON, "r") as f:
        enriched = json.load(f)
    
    item = next((i for i in enriched if i.get("id") == task_id), None)
    
    if not item:
        print(f"Task {task_id} not found")
        return
    
    print(f"Task: {item.get('title', '')}")
    print(f"ID: {task_id}")
    print(f"Status: {item.get('status', '')}")
    print(f"Category: {item.get('category', '')}")
    
    print(f"\nModule anchors:")
    for mod in item.get("moduleAnchors", []):
        print(f"  - {mod}")
    
    print(f"\nSuggested dependencies ({len(item.get('suggestedDependencies', []))}):")
    for dep_id in item.get("suggestedDependencies", []):
        print(f"  - {dep_id}")
    
    print(f"\nExisting dependencies ({len(item.get('dependsOn', []))}):")
    for dep_id in item.get("dependsOn", []):
        print(f"  - {dep_id}")

if __name__ == "__main__":
    import sys
    
    if len(sys.argv) > 1:
        if sys.argv[1] == "--promote":
            promote_suggestions()
        elif sys.argv[1] == "--task" and len(sys.argv) > 2:
            show_task_suggestions(sys.argv[2])
        else:
            print("Usage:")
            print("  python3 analyze_dependencies.py              # Show analysis")
            print("  python3 analyze_dependencies.py --promote    # Promote to canonical")
            print("  python3 analyze_dependencies.py --task ID    # Show task details")
    else:
        analyze_suggestions()
