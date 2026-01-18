#!/usr/bin/env python3
"""Ingest formalism adapter spec and emit canonical construction JSON."""

from __future__ import annotations

import json
from dataclasses import asdict
from pathlib import Path
import sys

ROOT = Path(__file__).resolve().parent.parent
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

from scripts.shared.formalism_adapter import load_formalism_adapter
from scripts.shared.io import save_json


def build_output(adapter_path: Path) -> dict:
    adapter = load_formalism_adapter(adapter_path)
    constructions = [asdict(item) for item in adapter.constructions]
    return {
        "formalism_id": adapter.formalism_id,
        "version": adapter.version,
        "constructions": constructions,
        "metadata": adapter.metadata,
    }


def main() -> None:
    adapter_path = ROOT / "data" / "formalisms" / "formalism_adapter.json"
    output_path = ROOT / "build" / "formalisms" / "canonical_constructions.json"

    if not adapter_path.exists():
        raise SystemExit(f"Missing formalism adapter: {adapter_path}")

    output_path.parent.mkdir(parents=True, exist_ok=True)
    payload = build_output(adapter_path)
    save_json(output_path, payload)
    print(f"✓ Wrote canonical constructions to {output_path}")


if __name__ == "__main__":
    main()
