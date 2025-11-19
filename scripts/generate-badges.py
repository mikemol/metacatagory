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
from typing import Dict, Any, Iterable, List, Tuple

# Constants controlling repository scan behavior
FILE_SCAN_EXTENSIONS = {".agda", ".md", ".txt", ".py", ".sh", ".json", ".yml", ".yaml"}
EXCLUDED_DIRS = {".git", "venv", ".github/badges"}

# Thresholds: list of (limit, color) evaluated in order for value < limit
ROADMAP_PROGRESS_THRESHOLDS: List[Tuple[int, str]] = [
    (30, "red"),
    (70, "yellow"),
    (101, "brightgreen"),
]
DEFERRED_TOTAL_THRESHOLDS: List[Tuple[int, str]] = [
    (1, "brightgreen"),
    (100, "yellow"),
    (300, "orange"),
    (10_000_000, "red"),
]
POSTULATES_THRESHOLDS: List[Tuple[int, str]] = [
    (21, "brightgreen"),
    (51, "yellow"),
    (10_000_000, "orange"),
]
TODO_THRESHOLDS: List[Tuple[int, str]] = [
    (1, "lightgrey"),
    (10_000_000, "blue"),
]
FIXME_THRESHOLDS: List[Tuple[int, str]] = [
    (1, "lightgrey"),
    (11, "orange"),
    (10_000_000, "red"),
]


def color_for(value: int, thresholds: List[Tuple[int, str]], default: str = "lightgrey") -> str:
    for limit, color in thresholds:
        if value < limit:
            return color
    return default


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
        "not-started": 0,
        "in-progress": 0,
        "completed": 0,
        "deferred": 0,
        "planned": 0,
    }
    
    for task in tasks:
        status = task.get('status', 'not-started')
        status_counts[status] = status_counts.get(status, 0) + 1
    
    total = len(tasks)
    completed = status_counts['completed']
    in_progress = status_counts['in-progress']
    not_started = status_counts['not-started']
    deferred_count = status_counts["deferred"]
    
    # Overall progress badge
    if total > 0:
        progress_pct = int((completed / total) * 100)
        color = color_for(progress_pct, ROADMAP_PROGRESS_THRESHOLDS, default="lightgrey")
    else:
        progress_pct = 0
        color = "lightgrey"
    
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

    # Deferred tasks badge
    badges["roadmap-deferred"] = {
        "schemaVersion": 1,
        "label": "deferred",
        "message": str(deferred_count),
        "color": "red" if deferred_count > 0 else "brightgreen",
    }
    # Planned tasks badge
    planned_count = status_counts["planned"]
    badges["roadmap-planned"] = {
        "schemaVersion": 1,
        "label": "planned",
        "message": str(planned_count),
        "color": "blue" if planned_count > 0 else "lightgrey",
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
    else:
        color = color_for(total, DEFERRED_TOTAL_THRESHOLDS, default='red')
    
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
        "color": color_for(postulates, POSTULATES_THRESHOLDS, default='orange')
    }
    
    # TODO items badge
    badges['deferred-todo'] = {
        "schemaVersion": 1,
        "label": "TODO",
        "message": str(todo),
        "color": color_for(todo, TODO_THRESHOLDS, default='lightgrey')
    }
    
    # FIXME items badge
    badges['deferred-fixme'] = {
        "schemaVersion": 1,
        "label": "FIXME",
        "message": str(fixme),
        "color": color_for(fixme, FIXME_THRESHOLDS, default='red')
    }
    # Planned items badge
    badges["deferred-planned"] = {
        "schemaVersion": 1,
        "label": "planned",
        "message": str(planned),
        "color": "blue" if planned > 0 else "lightgrey",
    }
    
    return badges


def scan_repository_for_deferred(repo_root: Path) -> Dict[str, Any]:
    """Scan source tree for postulates, TODO, FIXME, and deviation markers.
    Skips excluded directories and only processes files with allowed extensions.
    Returns a summary dict compatible with generate_deferred_badges.
    """
    postulates = 0
    todo = 0
    fixme = 0
    deviation_log = 0
    file_counts: Dict[str, Dict[str, int]] = {}

    for path in repo_root.rglob("*"):
        if not path.is_file():
            continue
        # Skip excluded directories early
        rel_parts: Iterable[str] = path.relative_to(repo_root).parts
        if any(part in EXCLUDED_DIRS for part in rel_parts):
            continue
        ext = path.suffix.lower()
        if ext not in FILE_SCAN_EXTENSIONS:
            continue
        rel_file = str(path.relative_to(repo_root))
        local_postulates = 0
        local_todo = 0
        local_fixme = 0
        local_deviation = 0
        try:
            with open(path, "r", encoding="utf-8", errors="ignore") as f:
                for line in f:
                    stripped = line.strip()
                    if "postulate" in stripped and not stripped.startswith("--"):
                        c = stripped.count("postulate")
                        postulates += c
                        local_postulates += c
                    if "TODO" in line:
                        c = line.count("TODO")
                        todo += c
                        local_todo += c
                    if "FIXME" in line:
                        c = line.count("FIXME")
                        fixme += c
                        local_fixme += c
                    if "DeviationLog" in line or "DEVIATION" in line:
                        deviation_log += 1
                        local_deviation += 1
        except Exception:
            continue
        if any([local_postulates, local_todo, local_fixme, local_deviation]):
            file_counts[rel_file] = {
                "postulates": local_postulates,
                "todo": local_todo,
                "fixme": local_fixme,
                "deviation": local_deviation,
                "total": local_postulates + local_todo + local_fixme + local_deviation,
            }

    # Planned tasks count derived from tasks.json for completeness
    tasks_file = repo_root / ".github" / "roadmap" / "tasks.json"
    planned = 0
    if tasks_file.exists():
        try:
            with open(tasks_file, "r", encoding="utf-8") as f:
                tasks_data = json.load(f)
            if isinstance(tasks_data, list):
                planned = sum(1 for t in tasks_data if t.get("status") == "planned")
        except Exception:
            pass

    total = postulates + todo + fixme + deviation_log
    return {
        "total": total,
        "deviation_log": deviation_log,
        "postulates": postulates,
        "todo": todo,
        "planned": planned,
        "fixme": fixme,
        "files": file_counts,
    }


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
    # If no deferred summary or empty, dynamically compute from source tree
    if not deferred or deferred.get("total", 0) == 0:
        deferred = scan_repository_for_deferred(repo_root)
    
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
    # Optionally write back a refreshed deferred summary for future runs
    refreshed_summary = repo_root / "deferred-summary.json"
    with open(refreshed_summary, "w") as f:
        json.dump({k: v for k, v in deferred.items() if k != "files"}, f, indent=2)
    print(
        "Refreshed deferred-summary.json "
        f"postulates={deferred.get('postulates')} "
        f"TODO={deferred.get('todo')} "
        f"FIXME={deferred.get('fixme')}"
    )

    # Write per-file detailed counts
    detailed_file = output_dir / "deferred-files.json"
    files_sorted = sorted(deferred.get("files", {}).items(), key=lambda x: x[1]["total"], reverse=True)
    with open(detailed_file, "w") as f:
        json.dump({fn: data for fn, data in files_sorted}, f, indent=2)
    print(f"Generated: {detailed_file} (per-file counts)")


if __name__ == '__main__':
    main()
