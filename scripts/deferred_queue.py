#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
from pathlib import Path
from typing import List

from scripts.shared.io import save_json

BADGE_DIR = Path(".github/badges")
OUTPUT_PATH = Path(".github/deferred-queue.json")


def load_deferred_files(path: Path) -> dict[str, dict]:
    with path.open("r", encoding="utf-8") as fh:
        return json.load(fh)


def sorted_queue(
    data: dict[str, dict], limit: int | None = None
) -> List[dict]:
    items = []
    for path, metrics in data.items():
        items.append(
            {
                "path": path,
                "postulates": metrics.get("postulates", 0),
                "todo": metrics.get("todo", 0),
                "fixme": metrics.get("fixme", 0),
                "deviation": metrics.get("deviation", 0),
                "total": metrics.get("total", 0),
                "weighted_total": metrics.get("weighted_total", 0),
            }
        )
    items.sort(key=lambda row: row["weighted_total"], reverse=True)
    if limit:
        items = items[:limit]
    return items


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Produce a machine-readable work queue from deferred files."
    )
    parser.add_argument(
        "--limit",
        type=int,
        help="Emit only the top N queue entries.",
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=OUTPUT_PATH,
        help="Output path for the generated queue JSON.",
    )

    args = parser.parse_args()

    source = BADGE_DIR / "deferred-files.json"
    if not source.exists():
        raise SystemExit(f"Missing badge data at {source}")

    queue = sorted_queue(load_deferred_files(source), args.limit)
    save_json(args.output, {"queue": queue})
    print(f"Wrote queue snapshot to {args.output}")


if __name__ == "__main__":
    main()
