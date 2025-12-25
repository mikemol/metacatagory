#!/usr/bin/env python3
"""
PRIO-ADOPT-1: Bridge Agda-generated priority strategies into badge weights.

- Agda owns logic and formatting (TechnicalDebt.PriorityOrchestrationFFI).
- This script normalizes Agda weights into the badge weight profile shape that
  scripts/generate-badges.py consumes.
"""

from __future__ import annotations

import argparse
import json
from pathlib import Path
from typing import Any, Dict, Optional


CATEGORY_NORMALIZATION: Dict[str, float] = {
    "postulate": 100.0,
    "todo": 50.0,
    "fixme": 100.0,
    "deviation": 500.0,
}


def load_agda_output(path: Path) -> Dict[str, Any]:
    """Load Agda-generated strategy profiles."""
    if not path.exists():
        raise FileNotFoundError(f"Agda priority output not found: {path}")

    with path.open() as handle:
        data = json.load(handle)

    if not isinstance(data, dict):
        raise ValueError("Agda output must be a JSON object")
    if not isinstance(data.get("strategies"), dict):
        raise ValueError("Agda output missing 'strategies' map")

    return data


def normalize_strategy(weights: Dict[str, Any]) -> Dict[str, float]:
    """Normalize raw integer weights into badge-friendly floats."""
    normalized: Dict[str, float] = {}
    for category, divisor in CATEGORY_NORMALIZATION.items():
        if category not in weights:
            raise ValueError(f"Strategy weights missing '{category}'")

        value = weights[category]
        if not isinstance(value, (int, float)):
            raise ValueError(f"Weight '{category}' must be numeric")

        normalized[category] = value / divisor

    return normalized


def convert_agda_profiles(
    raw: Dict[str, Any], active_override: Optional[str] = None
) -> Dict[str, Any]:
    """Convert Agda strategy map into badge weights profile map."""
    strategies = raw.get("strategies") or {}
    profiles: Dict[str, Dict[str, float]] = {}

    for name, content in strategies.items():
        weights = content.get("weights") if isinstance(content, dict) else None
        if weights is None:
            raise ValueError(f"Strategy '{name}' missing weights")

        profiles[name] = normalize_strategy(weights)

    if not profiles:
        raise ValueError("No strategies found in Agda output")

    active = active_override or raw.get("active") or "default"
    if active not in profiles:
        active = "default" if "default" in profiles else next(iter(profiles))

    return {
        "_comment": "Generated from Agda TechnicalDebt.PriorityOrchestrationFFI (normalized for badges)",
        "active": active,
        "profiles": profiles,
    }


def write_json(path: Path, data: Dict[str, Any]) -> None:
    """Persist JSON with stable formatting."""
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w") as handle:
        json.dump(data, handle, indent=2)
        handle.write("\n")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Normalize Agda priority strategy profiles into badge weights."
    )
    parser.add_argument(
        "--input",
        type=Path,
        default=Path("build/priority_strategy_profiles.json"),
        help="Agda-generated strategy profiles JSON",
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=Path(".github/badges/weights.json"),
        help="Destination badge weights file",
    )
    parser.add_argument(
        "--active",
        help="Override active profile (defaults to Agda 'active' field or 'default')",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()

    agda_raw = load_agda_output(args.input)
    badge_weights = convert_agda_profiles(agda_raw, args.active)
    write_json(args.output, badge_weights)

    print(
        f"Wrote badge weights to {args.output} (active='{badge_weights['active']}')"
    )


if __name__ == "__main__":
    main()
