#!/usr/bin/env python3
"""Validate a roadmap item list JSON file against the shared schema."""

from __future__ import annotations

import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from scripts import shared_data


def main() -> int:
    if len(sys.argv) < 2:
        print("usage: validate_roadmap_items.py <path>", file=sys.stderr)
        return 2

    path = Path(sys.argv[1])
    shared_data.load_planning_index_validated_from(path)
    print(f"✓ Roadmap item schema valid: {path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
