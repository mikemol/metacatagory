#!/usr/bin/env python3
"""
SPPF-Composable Onboarding Header

Roadmap: src/agda/Plan/CIM/Utility.agda
Architecture: ARCHITECTURE.md
Onboarding: COPILOT_SYNERGY.md

Constructive Proof Semantics:
- This script participates in the composable SPPF model, mirroring Agda record patterns for protocol,
  witness, and universal property semantics.
- All logic should be traceable to roadmap nodes and architectural principles.
- For onboarding, review the architecture and roadmap, and recursively revisit related nodes for context
  and composability.

generate-badges.py
Generates JSON endpoints for Shields.io dynamic badges based on roadmap tasks and deferred items.
Outputs badge data files that can be served via GitHub Pages or committed to the repo.
"""

import json
import os
import sys
from pathlib import Path
from datetime import datetime, timezone
from typing import Any, Iterable

# Load configuration from external files for modularity
def load_config():
    """Load thresholds, weights, and scan configuration from JSON files."""
    repo_root = Path(__file__).parent.parent
    config_dir = repo_root / ".github" / "badges"
    
    # Load thresholds
    thresholds_file = config_dir / "thresholds.json"
    if thresholds_file.exists():
        with open(thresholds_file) as f:
            threshold_data = json.load(f)
    else:
        # Fallback defaults if config missing
        threshold_data = {
            "roadmap_progress": [{"limit": 30, "color": "red"}, {"limit": 70, "color": "yellow"}, {"limit": 101, "color": "brightgreen"}],
            "deferred_total": [{"limit": 1, "color": "brightgreen"}, {"limit": 100, "color": "yellow"}, {"limit": 300, "color": "orange"}, {"limit": 10000000, "color": "red"}],
            "postulates": [{"limit": 21, "color": "brightgreen"}, {"limit": 51, "color": "yellow"}, {"limit": 10000000, "color": "orange"}],
            "todo": [{"limit": 1, "color": "lightgrey"}, {"limit": 10000000, "color": "blue"}],
            "fixme": [{"limit": 1, "color": "lightgrey"}, {"limit": 11, "color": "orange"}, {"limit": 10000000, "color": "red"}],
            "weighted_total": [{"limit": 50, "color": "brightgreen"}, {"limit": 150, "color": "yellow"}, {"limit": 400, "color": "orange"}, {"limit": 10000000, "color": "red"}]
        }
    
    # Load scan configuration
    scan_config_file = config_dir / "scan-config.json"
    if scan_config_file.exists():
        with open(scan_config_file) as f:
            scan_config = json.load(f)
    else:
        scan_config = {
            "file_extensions": [".agda", ".md", ".txt", ".py", ".sh", ".json", ".yml", ".yaml"],
            "excluded_dirs": [".git", "venv", ".github"],
            "top_offenders_limit": 15,
            "max_history_entries": 60
        }
    
    # Load weights (may contain profiles)
    weights_file = config_dir / "weights.json"
    if weights_file.exists():
        with open(weights_file) as f:
            weights = json.load(f)
    else:
        weights = {"postulate": 2.0, "todo": 1.0, "fixme": 1.5, "deviation": 3.0}
    
    return threshold_data, scan_config, weights

# Global configuration loaded at module level
THRESHOLD_DATA, SCAN_CONFIG, DEFAULT_WEIGHTS = load_config()

# Convert threshold dicts to tuples for backwards compatibility
ROADMAP_PROGRESS_THRESHOLDS = [(t["limit"], t["color"]) for t in THRESHOLD_DATA["roadmap_progress"]]
DEFERRED_TOTAL_THRESHOLDS = [(t["limit"], t["color"]) for t in THRESHOLD_DATA["deferred_total"]]
POSTULATES_THRESHOLDS = [(t["limit"], t["color"]) for t in THRESHOLD_DATA["postulates"]]
TODO_THRESHOLDS = [(t["limit"], t["color"]) for t in THRESHOLD_DATA["todo"]]
FIXME_THRESHOLDS = [(t["limit"], t["color"]) for t in THRESHOLD_DATA["fixme"]]
WEIGHTED_TOTAL_THRESHOLDS = [(t["limit"], t["color"]) for t in THRESHOLD_DATA["weighted_total"]]

FILE_SCAN_EXTENSIONS = set(SCAN_CONFIG["file_extensions"])
EXCLUDED_DIRS = set(SCAN_CONFIG["excluded_dirs"])
MAX_HISTORY_ENTRIES = SCAN_CONFIG["max_history_entries"]
TOP_OFFENDERS_LIMIT = SCAN_CONFIG.get("top_offenders_limit", 15)


def color_for(
    value: int, thresholds: list[tuple[int, str]], default: str = "lightgrey"
) -> str:
    for limit, color in thresholds:
        if value < limit:
            return color
    return default


def load_json_file(filepath: Path) -> dict[str, Any]:
    """Load and parse a JSON file."""
    if not filepath.exists():
        print(f"Warning: {filepath} not found", file=sys.stderr)
        return {}

    with open(filepath, "r") as f:
        return json.load(f)


def _resolve_weight_profile(raw: dict[str, Any], profile: str) -> dict[str, float] | None:
    """Select a weight profile from raw config, supporting legacy flat dicts."""
    if not isinstance(raw, dict):
        return None

    # New shape: {"profiles": {"default": {...}, "ffi": {...}}, "active": "default"}
    profiles = raw.get("profiles") if isinstance(raw.get("profiles"), dict) else None
    if profiles:
        # Active profile can come from config; env overrides both
        active = profile or raw.get("active") or "default"
        if active in profiles:
            return profiles[active]
        # Fallback to first profile
        first_profile = next(iter(profiles.values())) if profiles else None
        if first_profile:
            print(
                f"Weight profile '{active}' not found; using first available profile",
                file=sys.stderr,
            )
            return first_profile

    # Legacy shape: flat dict of weights
    if all(isinstance(v, (int, float)) for v in raw.values()):
        return raw  # Backward compatible single-profile

    return None


def load_weights(output_dir: Path) -> dict[str, float]:
    """Load severity weights from external JSON or use defaults.

    Supports profile selection via env BADGE_WEIGHTS_PROFILE. Accepts either:
    - Flat dict {"postulate": 2.0, ...} (legacy)
    - Profile map {"profiles": {"default": {...}, "ffiSafety": {...}}, "active": "default"}
    """

    profile = os.environ.get("BADGE_WEIGHTS_PROFILE", "default")
    weights_file = output_dir / "weights.json"

    if weights_file.exists():
        try:
            weights_raw = load_json_file(weights_file)
            selected = _resolve_weight_profile(weights_raw, profile)
            if selected:
                print(
                    f"Loaded weights from {weights_file} (profile='{profile}')"
                )
                return selected
            else:
                print(
                    f"Weights file {weights_file} missing profile '{profile}' and no compatible fallback; using defaults",
                    file=sys.stderr,
                )
        except Exception as e:
            print(f"Error loading weights: {e}, using defaults", file=sys.stderr)

    # Write default weights if not present or unusable
    with open(weights_file, "w") as wf:
        json.dump(DEFAULT_WEIGHTS, wf, indent=2)
    print(f"Initialized default weights: {weights_file}")
    return DEFAULT_WEIGHTS.copy()


def generate_roadmap_badges(tasks: list) -> dict[str, dict[str, Any]]:
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
        status = task.get("status", "not-started")
        status_counts[status] = status_counts.get(status, 0) + 1

    total = len(tasks)
    completed = status_counts["completed"]
    in_progress = status_counts["in-progress"]
    not_started = status_counts["not-started"]
    deferred_count = status_counts["deferred"]

    # Overall progress badge
    if total > 0:
        progress_pct = int((completed / total) * 100)
        color = color_for(
            progress_pct, ROADMAP_PROGRESS_THRESHOLDS, default="lightgrey"
        )
    else:
        progress_pct = 0
        color = "lightgrey"

    badges["roadmap-progress"] = {
        "schemaVersion": 1,
        "label": "roadmap",
        "message": f"{progress_pct}% ({completed}/{total})",
        "color": color,
    }

    # In-progress badge
    badges["roadmap-active"] = {
        "schemaVersion": 1,
        "label": "in progress",
        "message": str(in_progress),
        "color": "blue" if in_progress > 0 else "lightgrey",
    }

    # Not started badge
    badges["roadmap-todo"] = {
        "schemaVersion": 1,
        "label": "todo",
        "message": str(not_started),
        "color": "orange" if not_started > 0 else "lightgrey",
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


def generate_deferred_badges(summary: dict[str, Any]) -> dict[str, dict[str, Any]]:
    """Generate badge data for deferred items (raw counts + weighted + trend if provided)."""
    badges = {}

    total = summary.get("total", 0)
    deviation_log = summary.get("deviation_log", 0)
    postulates = summary.get("postulates", 0)
    todo = summary.get("todo", 0)
    planned = summary.get("planned", 0)
    fixme = summary.get("fixme", 0)
    weighted_total = summary.get("weighted_total", 0)
    trend_delta = summary.get("trend_delta")  # may be None

    # Total deferred items badge
    if total == 0:
        color = "brightgreen"
    else:
        color = color_for(total, DEFERRED_TOTAL_THRESHOLDS, default="red")

    badges["deferred-total"] = {
        "schemaVersion": 1,
        "label": "deferred items",
        "message": str(total),
        "color": color,
    }

    # Weighted technical debt badge
    weighted_color = color_for(
        int(weighted_total), WEIGHTED_TOTAL_THRESHOLDS, default="red"
    )
    badges["deferred-weighted"] = {
        "schemaVersion": 1,
        "label": "weighted debt",
        "message": str(int(weighted_total)),
        "color": weighted_color,
    }

    # Trend badge (delta from previous snapshot; negative is improvement)
    if trend_delta is not None:
        if trend_delta < 0:
            trend_color = "brightgreen"
        elif trend_delta == 0:
            trend_color = "lightgrey"
        else:
            trend_color = "red"
        sign = "+" if trend_delta > 0 else ""  # negative already has '-'
        badges["deferred-trend"] = {
            "schemaVersion": 1,
            "label": "debt Δ",
            "message": f"{sign}{trend_delta}",
            "color": trend_color,
        }

    # Weekly average trend badge (7-day moving average)
    weekly_avg = summary.get("weekly_avg_delta")
    if weekly_avg is not None:
        if weekly_avg < 0:
            weekly_color = "brightgreen"
        elif abs(weekly_avg) < 5:
            weekly_color = "lightgrey"
        else:
            weekly_color = "red"
        sign = "+" if weekly_avg > 0 else ""
        badges["deferred-trend-weekly"] = {
            "schemaVersion": 1,
            "label": "7d avg Δ",
            "message": f"{sign}{weekly_avg:.1f}",
            "color": weekly_color,
        }

    # DeviationLog badge (critical items)
    badges["deferred-deviations"] = {
        "schemaVersion": 1,
        "label": "deviations",
        "message": str(deviation_log),
        "color": "red" if deviation_log > 0 else "brightgreen",
    }

    # Postulates badge
    badges["deferred-postulates"] = {
        "schemaVersion": 1,
        "label": "postulates",
        "message": str(postulates),
        "color": color_for(postulates, POSTULATES_THRESHOLDS, default="orange"),
    }

    # TODO items badge
    badges["deferred-todo"] = {
        "schemaVersion": 1,
        "label": "TODO",
        "message": str(todo),
        "color": color_for(todo, TODO_THRESHOLDS, default="lightgrey"),
    }

    # FIXME items badge
    badges["deferred-fixme"] = {
        "schemaVersion": 1,
        "label": "FIXME",
        "message": str(fixme),
        "color": color_for(fixme, FIXME_THRESHOLDS, default="red"),
    }
    # Planned items badge
    badges["deferred-planned"] = {
        "schemaVersion": 1,
        "label": "planned",
        "message": str(planned),
        "color": "blue" if planned > 0 else "lightgrey",
    }

    return badges


def scan_repository_for_deferred(
    repo_root: Path, weights: dict[str, float]
) -> dict[str, Any]:
    """Scan source tree for postulates, TODO, FIXME, and deviation markers.
    Skips excluded directories and only processes files with allowed extensions.
    Returns a summary dict compatible with generate_deferred_badges including weighted totals.
    """
    postulates = 0
    todo = 0
    fixme = 0
    deviation_log = 0
    file_counts: dict[str, dict[str, float]] = {}

    for path in repo_root.rglob("*"):
        if not path.is_file():
            continue
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

                    # Postulate: only in .agda files, skip comments and string literals
                    if ext == ".agda" and "postulate" in stripped:
                        if not stripped.startswith("--"):
                            # Exclude variable names (postulates, postulated), dictionary keys, and string literals
                            # But count standalone 'postulate' keyword
                            words = line.split()
                            if (
                                '"postulate' not in line
                                and "'postulate" not in line
                                and "postulates" not in words
                                and "postulated" not in words
                            ):
                                c = stripped.count("postulate")
                                postulates += c
                                local_postulates += c

                    # TODO/FIXME: look for comment-like patterns
                    if "TODO" in line:
                        # Exclude label strings in JSON/badges
                        if '"TODO"' not in line and "'TODO'" not in line:
                            c = line.count("TODO")
                            todo += c
                            local_todo += c

                    if "FIXME" in line:
                        # Exclude label strings in JSON/badges
                        if '"FIXME"' not in line and "'FIXME'" not in line:
                            c = line.count("FIXME")
                            fixme += c
                            local_fixme += c

                    # Deviation: exclude string literals in search patterns
                    if "DeviationLog" in line or "DEVIATION" in line:
                        if (
                            '"DEVIATION"' not in line
                            and "'DEVIATION'" not in line
                            and '"DeviationLog"' not in line
                        ):
                            deviation_log += 1
                            local_deviation += 1
        except Exception:
            continue
        if any([local_postulates, local_todo, local_fixme, local_deviation]):
            weighted_local = (
                local_postulates * weights["postulate"]
                + local_todo * weights["todo"]
                + local_fixme * weights["fixme"]
                + local_deviation * weights["deviation"]
            )
            file_counts[rel_file] = {
                "postulates": local_postulates,
                "todo": local_todo,
                "fixme": local_fixme,
                "deviation": local_deviation,
                "total": local_postulates + local_todo + local_fixme + local_deviation,
                "weighted_total": weighted_local,
            }

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
    weighted_total = (
        postulates * weights["postulate"]
        + todo * weights["todo"]
        + fixme * weights["fixme"]
        + deviation_log * weights["deviation"]
    )
    return {
        "total": total,
        "weighted_total": weighted_total,
        "deviation_log": deviation_log,
        "postulates": postulates,
        "todo": todo,
        "planned": planned,
        "fixme": fixme,
        "files": file_counts,
    }


def generate_build_badge() -> dict[str, Any]:
    """Generate a build status badge placeholder (actual status from CI)."""
    # This is a static badge; actual build status comes from GitHub Actions badge
    # We can generate a timestamp badge instead
    return {
        "schemaVersion": 1,
        "label": "last updated",
        "message": datetime.now(timezone.utc).strftime("%Y-%m-%d"),
        "color": "informational",
    }


def main():
    """Main entry point."""
    # Paths
    repo_root = Path(__file__).parent.parent
    tasks_file = repo_root / ".github" / "roadmap" / "tasks.json"
    output_dir = repo_root / ".github" / "badges"

    # Create output directory
    output_dir.mkdir(parents=True, exist_ok=True)

    # Load weights
    weights = load_weights(output_dir)

    # Load data
    tasks = load_json_file(tasks_file)
    if isinstance(tasks, dict):
        tasks = []  # Handle empty or malformed file

    # Always compute fresh deferred data from the source tree to ensure per-file details
    # This avoids stale summaries without 'files' causing empty top-offenders tables.
    deferred = scan_repository_for_deferred(repo_root, weights)

    # History tracking for trend badge
    history_file = output_dir / "deferred-history.json"
    history: list[dict[str, Any]] = []
    if history_file.exists():
        try:
            with open(history_file, "r") as hf:
                data = json.load(hf)
                if isinstance(data, list):
                    history = data
        except Exception:
            history = []
    today = datetime.now(timezone.utc).strftime("%Y-%m-%d")
    last_entry_date = history[-1]["date"] if history else None
    current_weighted = deferred.get("weighted_total", 0)

    if last_entry_date != today:
        history.append(
            {
                "date": today,
                "total": deferred.get("total", 0),
                "weighted_total": current_weighted,
            }
        )
    else:
        # Update today's entry if re-running same day
        history[-1]["total"] = deferred.get("total", 0)
        history[-1]["weighted_total"] = current_weighted

    # Limit history to MAX_HISTORY_ENTRIES (keep most recent)
    if len(history) > MAX_HISTORY_ENTRIES:
        history = history[-MAX_HISTORY_ENTRIES:]
        print(f"Trimmed history to last {MAX_HISTORY_ENTRIES} entries")

    # Compute trend delta (weighted)
    trend_delta = None
    if len(history) >= 2:
        trend_delta = int(history[-1]["weighted_total"] - history[-2]["weighted_total"])
    deferred["trend_delta"] = trend_delta

    # Compute 7-day moving average delta
    weekly_avg_delta = None
    if len(history) >= 8:  # need at least 8 to compute 7 deltas
        recent = history[-8:]
        deltas = [
            recent[i]["weighted_total"] - recent[i - 1]["weighted_total"]
            for i in range(1, len(recent))
        ]
        weekly_avg_delta = sum(deltas) / len(deltas)
    deferred["weekly_avg_delta"] = weekly_avg_delta

    # Generate all badges
    all_badges = {}

    # Roadmap badges
    roadmap_badges = generate_roadmap_badges(tasks)
    all_badges.update(roadmap_badges)

    # Deferred items badges (now includes weighted + trend)
    deferred_badges = generate_deferred_badges(deferred)
    all_badges.update(deferred_badges)

    # Last updated badge
    all_badges["last-updated"] = generate_build_badge()

    # Persist history
    with open(history_file, "w") as hf:
        json.dump(history, hf, indent=2)
    print(f"Updated history: {history_file} entries={len(history)}")

    # Write individual badge JSON files
    for badge_name, badge_data in all_badges.items():
        output_file = output_dir / f"{badge_name}.json"
        with open(output_file, "w") as f:
            json.dump(badge_data, f, indent=2)
        print(f"Generated: {output_file}")

    # Write manifest file listing all badges
    manifest = {
        "generated": datetime.now(timezone.utc).isoformat().replace("+00:00", "Z"),
        "badges": list(all_badges.keys()),
    }
    manifest_file = output_dir / "manifest.json"
    with open(manifest_file, "w") as f:
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

    # Write per-file detailed counts (sorted by weighted_total then total)
    detailed_file = output_dir / "deferred-files.json"
    files_sorted = sorted(
        deferred.get("files", {}).items(),
        key=lambda x: (x[1].get("weighted_total", 0), x[1]["total"]),
        reverse=True,
    )
    with open(detailed_file, "w") as f:
        json.dump({fn: data for fn, data in files_sorted}, f, indent=2)
    print(f"Generated: {detailed_file} (per-file counts)")

    # Top offenders markdown (top N from config)
    top_md_file = output_dir / "top-offenders.md"
    top_n = files_sorted[:TOP_OFFENDERS_LIMIT]
    lines = [
        "# Top Technical Debt Offenders (Weighted)",
        "",
        f"Generated: {datetime.now(timezone.utc).isoformat().replace('+00:00', 'Z')}",
        "",
        "| Rank | File | P | TODO | FIXME | DEV | Raw | Wtd |",
        "|------|------|---|------|-------|-----|-----|-----|",
    ]
    for idx, (fn, data) in enumerate(top_n, start=1):
        lines.append(
            "| {rank} | {file} | {p} | {todo} | {fixme} | {dev} | {raw} | {wtd} |".format(
                rank=idx,
                file=fn,
                p=data["postulates"],
                todo=data["todo"],
                fixme=data["fixme"],
                dev=data["deviation"],
                raw=data["total"],
                wtd=int(data.get("weighted_total", 0)),
            )
        )
    with open(top_md_file, "w") as f:
        f.write("\n".join(lines) + "\n")
    print(f"Generated: {top_md_file} (markdown table)")


if __name__ == "__main__":
    main()
