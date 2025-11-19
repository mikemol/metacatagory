#!/usr/bin/env python3
"""
generate-badges.py
Generates JSON endpoints for Shields.io dynamic badges based on roadmap tasks and deferred items.
Outputs badge data files that can be served via GitHub Pages or committed to the repo.
"""

import json
import sys
from pathlib import Path
from datetime import datetime
from typing import Dict, Any


def load_json_file(filepath: Path) -> Dict[str, Any]:
    """Load and parse a JSON file."""
    if not filepath.exists():
        print(f"Warning: {filepath} not found", file=sys.stderr)
        return {}
    
    with open(filepath, 'r') as f:
        return json.load(f)


def generate_roadmap_badges(tasks: list) -> Dict[str, Dict[str, Any]]:
    """Generate badge data for roadmap tasks."""
    badges = {}
    
    # Count tasks by status
    status_counts = {
        'not-started': 0,
        'in-progress': 0,
        'completed': 0,
        'deferred': 0
    }
    
    for task in tasks:
        status = task.get('status', 'not-started')
        status_counts[status] = status_counts.get(status, 0) + 1
    
    total = len(tasks)
    completed = status_counts['completed']
    in_progress = status_counts['in-progress']
    not_started = status_counts['not-started']
    
    # Overall progress badge
    if total > 0:
        progress_pct = int((completed / total) * 100)
        color = 'red' if progress_pct < 30 else 'yellow' if progress_pct < 70 else 'brightgreen'
    else:
        progress_pct = 0
        color = 'lightgrey'
    
    badges['roadmap-progress'] = {
        "schemaVersion": 1,
        "label": "roadmap",
        "message": f"{progress_pct}% ({completed}/{total})",
        "color": color
    }
    
    # In-progress badge
    badges['roadmap-active'] = {
        "schemaVersion": 1,
        "label": "in progress",
        "message": str(in_progress),
        "color": "blue" if in_progress > 0 else "lightgrey"
    }
    
    # Not started badge
    badges['roadmap-todo'] = {
        "schemaVersion": 1,
        "label": "todo",
        "message": str(not_started),
        "color": "orange" if not_started > 0 else "lightgrey"
    }
    
    return badges


def generate_deferred_badges(summary: Dict[str, Any]) -> Dict[str, Dict[str, Any]]:
    """Generate badge data for deferred items."""
    badges = {}
    
    total = summary.get('total', 0)
    deviation_log = summary.get('deviation_log', 0)
    postulates = summary.get('postulates', 0)
    todo = summary.get('todo', 0)
    planned = summary.get('planned', 0)
    fixme = summary.get('fixme', 0)
    
    # Total deferred items badge
    if total == 0:
        color = 'brightgreen'
    elif total < 100:
        color = 'yellow'
    elif total < 300:
        color = 'orange'
    else:
        color = 'red'
    
    badges['deferred-total'] = {
        "schemaVersion": 1,
        "label": "deferred items",
        "message": str(total),
        "color": color
    }
    
    # DeviationLog badge (critical items)
    badges['deferred-deviations'] = {
        "schemaVersion": 1,
        "label": "deviations",
        "message": str(deviation_log),
        "color": "red" if deviation_log > 0 else "brightgreen"
    }
    
    # Postulates badge
    badges['deferred-postulates'] = {
        "schemaVersion": 1,
        "label": "postulates",
        "message": str(postulates),
        "color": "orange" if postulates > 50 else "yellow" if postulates > 20 else "brightgreen"
    }
    
    # TODO items badge
    badges['deferred-todo'] = {
        "schemaVersion": 1,
        "label": "TODO",
        "message": str(todo),
        "color": "blue" if todo > 0 else "lightgrey"
    }
    
    # FIXME items badge
    badges['deferred-fixme'] = {
        "schemaVersion": 1,
        "label": "FIXME",
        "message": str(fixme),
        "color": "red" if fixme > 10 else "orange" if fixme > 0 else "lightgrey"
    }
    
    return badges


def generate_build_badge() -> Dict[str, Any]:
    """Generate a build status badge placeholder (actual status from CI)."""
    # This is a static badge; actual build status comes from GitHub Actions badge
    # We can generate a timestamp badge instead
    return {
        "schemaVersion": 1,
        "label": "last updated",
        "message": datetime.utcnow().strftime("%Y-%m-%d"),
        "color": "informational"
    }


def main():
    """Main entry point."""
    # Paths
    repo_root = Path(__file__).parent.parent
    tasks_file = repo_root / '.github' / 'roadmap' / 'tasks.json'
    deferred_summary = repo_root / 'deferred-summary.json'
    output_dir = repo_root / '.github' / 'badges'
    
    # Create output directory
    output_dir.mkdir(parents=True, exist_ok=True)
    
    # Load data
    tasks = load_json_file(tasks_file)
    if isinstance(tasks, dict):
        tasks = []  # Handle empty or malformed file
    
    deferred = load_json_file(deferred_summary)
    
    # Generate all badges
    all_badges = {}
    
    # Roadmap badges
    roadmap_badges = generate_roadmap_badges(tasks)
    all_badges.update(roadmap_badges)
    
    # Deferred items badges
    deferred_badges = generate_deferred_badges(deferred)
    all_badges.update(deferred_badges)
    
    # Last updated badge
    all_badges['last-updated'] = generate_build_badge()
    
    # Write individual badge JSON files
    for badge_name, badge_data in all_badges.items():
        output_file = output_dir / f'{badge_name}.json'
        with open(output_file, 'w') as f:
            json.dump(badge_data, f, indent=2)
        print(f"Generated: {output_file}")
    
    # Write manifest file listing all badges
    manifest = {
        "generated": datetime.utcnow().isoformat() + "Z",
        "badges": list(all_badges.keys())
    }
    manifest_file = output_dir / 'manifest.json'
    with open(manifest_file, 'w') as f:
        json.dump(manifest, f, indent=2)
    print(f"Generated: {manifest_file}")
    
    print(f"\n✅ Generated {len(all_badges)} badge JSON files in {output_dir}")


if __name__ == '__main__':
    main()
