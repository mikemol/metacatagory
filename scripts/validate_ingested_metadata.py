#!/usr/bin/env python3
"""Validate ingested_metadata.json against the shared schema."""

from __future__ import annotations

import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from scripts import shared_data


def main() -> int:
    path = Path(sys.argv[1]) if len(sys.argv) > 1 else REPO_ROOT / "build" / "ingested_metadata.json"
    shared_data.load_ingested_metadata_from(path, required=True)
    print(f"✓ Ingested metadata schema valid: {path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
