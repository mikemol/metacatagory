#!/usr/bin/env python3
"""Validate canonical constructions emitted by the formalism ingest."""

from __future__ import annotations

from pathlib import Path
import sys
from typing import Any, Dict, List

ROOT = Path(__file__).resolve().parent.parent
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

from scripts.shared.formalism_adapter import formalism_adapter_validator
from scripts.shared.io import load_json


def _collect_duplicates(values: List[str]) -> List[str]:
    seen = set()
    dupes = set()
    for value in values:
        if value in seen:
            dupes.add(value)
        else:
            seen.add(value)
    return sorted(dupes)


def validate_payload(payload: Dict[str, Any], source: str) -> bool:
    result = formalism_adapter_validator(payload, path=source)
    try:
        result.raise_if_invalid("Formalism constructions schema validation failed")
    except Exception as exc:
        print(f"❌ {exc}")
        return False

    constructions = payload.get("constructions", [])
    if not constructions:
        print("❌ Formalism constructions validation FAILED (no constructions found)")
        return False

    ids = [item.get("construction_id", "") for item in constructions]
    missing_ids = [value for value in ids if not value]
    if missing_ids:
        print("❌ Formalism constructions validation FAILED (missing construction_id)")
        return False

    duplicates = _collect_duplicates(ids)
    if duplicates:
        print(
            "❌ Formalism constructions validation FAILED (duplicate construction_id): "
            + ", ".join(duplicates)
        )
        return False

    print("✓ Formalism constructions validated")
    print(f"  Formalism: {payload.get('formalism_id', '<unknown>')}")
    print(f"  Version:   {payload.get('version', '<unknown>')}")
    print(f"  Count:     {len(constructions)}")
    return True


def main() -> int:
    path = Path(sys.argv[1]) if len(sys.argv) > 1 else ROOT / "build" / "formalisms" / "canonical_constructions.json"
    payload = load_json(
        path,
        required=True,
        error_msg=f"Missing formalism constructions: {path}",
    )
    return 0 if validate_payload(payload, str(path)) else 1


if __name__ == "__main__":
    raise SystemExit(main())
