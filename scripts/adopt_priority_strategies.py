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
import sys
from pathlib import Path
from typing import Any, Dict, Optional, Tuple

from scripts.shared.io import save_json, load_json
CATEGORY_NORMALIZATION: Dict[str, float] = {
    "postulate": 100.0,
    "todo": 50.0,
    "fixme": 100.0,
    "deviation": 500.0,
}

# Pure AGDA-derived strategies for testing and documentation.
AGDA_STRATEGIES: Dict[str, Dict[str, int]] = {
    "default": {
        "minimal": 1,
        "low": 10,
        "medium": 50,
        "high": 100,
        "critical": 500,
        "testFixture": 1,
        "documentation": 25,
        "performance": 75,
        "safety": 200,
        "proof": 150,
    },
    "ffiSafety": {
        "minimal": 1,
        "low": 5,
        "medium": 25,
        "high": 50,
        "critical": 1000,
        "testFixture": 1,
        "documentation": 10,
        "performance": 100,
        "safety": 800,
        "proof": 30,
    },
    "proofCompleteness": {
        "minimal": 1,
        "low": 10,
        "medium": 50,
        "high": 200,
        "critical": 500,
        "testFixture": 1,
        "documentation": 20,
        "performance": 30,
        "safety": 150,
        "proof": 300,
    },
    "rapidDevelopment": {
        "minimal": 1,
        "low": 5,
        "medium": 20,
        "high": 50,
        "critical": 100,
        "testFixture": 1,
        "documentation": 5,
        "performance": 10,
        "safety": 75,
        "proof": 25,
    },
    "production": {
        "minimal": 10,
        "low": 50,
        "medium": 150,
        "high": 300,
        "critical": 1000,
        "testFixture": 5,
        "documentation": 100,
        "performance": 200,
        "safety": 500,
        "proof": 400,
    },
}

# Mapping from badge categories back to AGDA priority fields/divisors.
CATEGORY_FIELD_MAPPING: Dict[str, Tuple[str, float]] = {
    "postulate": ("proof", CATEGORY_NORMALIZATION["postulate"]),
    "todo": ("documentation", CATEGORY_NORMALIZATION["todo"]),
    "fixme": ("safety", CATEGORY_NORMALIZATION["fixme"]),
    "deviation": ("critical", CATEGORY_NORMALIZATION["deviation"]),
}


def normalize_agda_strategy(strategy: Dict[str, int]) -> Dict[str, float]:
    """Normalize AGDA priority fields into badge-friendly floats."""
    normalized: Dict[str, float] = {}
    for category, (agda_field, divisor) in CATEGORY_FIELD_MAPPING.items():
        if agda_field not in strategy:
            raise ValueError(f"Strategy missing AGDA field '{agda_field}'")

        normalized[category] = strategy[agda_field] / divisor

    return normalized


def agda_to_python_weights(strategy_name: str) -> Dict[str, float]:
    """Pure mapping from AGDA strategies to badge categories."""
    if strategy_name not in AGDA_STRATEGIES:
        raise ValueError(f"Unknown strategy '{strategy_name}'")

    return normalize_agda_strategy(AGDA_STRATEGIES[strategy_name])


def build_weight_profiles() -> Dict[str, Dict[str, float]]:
    """Compute normalized weights for every AGDA priority strategy."""
    return {
        name: agda_to_python_weights(name) for name in AGDA_STRATEGIES.keys()
    }


def format_weights_json(
    profiles: Dict[str, Dict[str, float]], active: str = "default"
) -> Dict[str, Any]:
    """Wrap normalized weights with JSON metadata for badges."""
    return {
        "_comment": "Generated from Agda TechnicalDebt.Priorities via PRIO-ADOPT-1",
        "active": active,
        "profiles": profiles,
    }

def default_profiles() -> Dict[str, Any]:
    """Minimal fallback profile set when Agda output is unavailable/invalid."""
    baseline = {k: CATEGORY_NORMALIZATION[k] for k in CATEGORY_NORMALIZATION}
    return {
        "active": "default",
        "strategies": {
            "default": {"weights": baseline},
        },
    }

def load_agda_output(path: Path) -> Dict[str, Any]:
    """Load Agda-generated strategy profiles."""
    if not path.exists():
        raise FileNotFoundError(f"Agda priority output not found: {path}")

    text = path.read_text()
    if "placeholder" in text.lower():
        print(
            f"⚠️  Agda priority output contained a placeholder string ({path}); using default profiles.",
            file=sys.stderr,
        )
        return default_profiles()
    try:
        data = load_json(path)
    except json.JSONDecodeError:
        # Fallback: Agda export sometimes emits a placeholder; use a minimal default.
        print(
            f"⚠️  Agda priority output is not valid JSON ({path}); using default profiles.",
            file=sys.stderr,
        )
        return default_profiles()

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

    return format_weights_json(profiles, active)

def write_json(path: Path, data: Dict[str, Any]) -> None:
    """Persist JSON with stable formatting."""
    save_json(path, data)

def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Normalize Agda priority strategy profiles into badge weights."
    )
    parser.add_argument(
        "--input",
        type=Path,
        default=Path("data/priority_strategy_profiles.json"),
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
