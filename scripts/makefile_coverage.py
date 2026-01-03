from __future__ import annotations

import argparse
import subprocess
from typing import Iterable, List

from scripts.makefile_graph import (
    parse_make_database,
    parse_phony_targets,
    reachable_nodes,
)

DEFAULT_ROOTS: List[str] = ["check", "regen-makefile"]


def load_make_graph() -> dict[str, set[str]]:
    result = subprocess.run(
        ["make", "-qp"], capture_output=True, text=True, check=False
    )
    return parse_make_database(result.stdout)


def run_targets(targets: Iterable[str]) -> None:
    for target in targets:
        print(f"\n== Executing make {target} ==")
        subprocess.run(["make", target], check=True)


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

    args = parser.parse_args()

    graph = load_make_graph()
    phony = parse_phony_targets(graph)

    roots_for_coverage = args.roots

    if args.run_targets:
        execution_targets = sorted(phony)
        if not execution_targets:
            print("No phony targets were discovered; skipping execution step.")
        else:
            run_targets(execution_targets)
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
