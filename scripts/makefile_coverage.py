from __future__ import annotations

import argparse
import json
import os
import subprocess
from pathlib import Path
from typing import Iterable, List

from scripts.makefile_graph import (
    parse_make_database,
    parse_phony_targets,
    reachable_nodes,
)
from scripts.shared.io import save_json, load_json

DEFAULT_ROOTS: List[str] = ["check", "regen-makefile"]
FAILURES_PATH = Path("build/reports/last_failures.json")
EXECUTION_PRIORITY: List[str] = ["regen-makefile"]


def load_failures() -> list[str]:
    if not FAILURES_PATH.exists():
        return []
    try:
        data = load_json(FAILURES_PATH)
    except json.JSONDecodeError:
        return []
    return [item for item in data if isinstance(item, str)]


def save_failures(targets: list[str]) -> None:
    save_json(FAILURES_PATH, targets)


def clear_failures() -> None:
    if FAILURES_PATH.exists():
        FAILURES_PATH.unlink()


def load_make_graph() -> dict[str, set[str]]:
    result = subprocess.run(
        ["make", "-qp"], capture_output=True, text=True, check=False
    )
    return parse_make_database(result.stdout)


def run_targets(targets: Iterable[str], prioritize_failures: bool = True) -> None:
    env = os.environ.copy()
    env.setdefault("MUTATE_OK", "1")
    skip_act = bool(env.get("ACT"))
    skip_targets_env = env.get("MAKEFILE_COVERAGE_SKIP", "")
    skip_targets = {
        item.strip()
        for item in skip_targets_env.replace(",", " ").split()
        if item.strip()
    }
    target_list = list(targets)
    if EXECUTION_PRIORITY:
        prioritized = [t for t in EXECUTION_PRIORITY if t in target_list]
        target_list = prioritized + [t for t in target_list if t not in prioritized]
    if prioritize_failures:
        failures = load_failures()
        failed_first = [t for t in failures if t in target_list]
        remaining = [t for t in target_list if t not in failed_first]
        target_list = failed_first + remaining
    for target in target_list:
        if target in skip_targets:
            print(f"\n== Skipping make {target} (MAKEFILE_COVERAGE_SKIP) ==")
            continue
        if skip_act and target.startswith("act-"):
            print(f"\n== Skipping make {target} (ACT environment) ==")
            continue
        if skip_act and target.startswith("docker-"):
            print(f"\n== Skipping make {target} (ACT environment) ==")
            continue
        if target.startswith("docker-"):
            if not env.get("GHCR_REGISTRY") or not env.get("GHCR_USERNAME"):
                print(f"\n== Skipping make {target} (missing GHCR env) ==")
                continue
        print(f"\n== Executing make {target} ==")
        try:
            subprocess.run(["make", target], check=True, env=env)
        except subprocess.CalledProcessError:
            save_failures([target])
            raise
    clear_failures()


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Report which Makefile phony targets are reachable from chosen roots."
    )
    parser.add_argument(
        "--roots",
        nargs="+",
        default=DEFAULT_ROOTS,
        help="Root targets assumed to have executed (default: check regen-makefile)",
    )
    parser.add_argument(
        "--run-targets",
        action="store_true",
        help="Execute each phony target before computing reachability",
    )
    parser.add_argument(
        "--no-prioritize-failures",
        action="store_true",
        help="Disable failed-first ordering for target execution",
    )

    args = parser.parse_args()

    graph = load_make_graph()
    phony = parse_phony_targets(graph)

    roots_for_coverage = args.roots

    if args.run_targets:
        execution_targets = sorted(phony)
        if not execution_targets:
            print("No phony targets were discovered; skipping execution step.")
        else:
            run_targets(
                execution_targets,
                prioritize_failures=not args.no_prioritize_failures,
            )
        graph = load_make_graph()
        phony = parse_phony_targets(graph)
        if args.roots == DEFAULT_ROOTS:
            roots_for_coverage = execution_targets

    reachable = reachable_nodes(roots_for_coverage, graph)
    unreachable_phony = sorted(phony - reachable)

    print("Phony targets reachable from", roots_for_coverage)
    if unreachable_phony:
        print("Targets that remain uncovered:")
        for target in unreachable_phony:
            print("  -", target)
    else:
        print("All phony targets are reachable from", roots_for_coverage)


if __name__ == "__main__":
    main()
