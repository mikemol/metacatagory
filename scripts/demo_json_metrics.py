#!/usr/bin/env python3
"""Compute basic metrics for demo JSON decomposition."""

from __future__ import annotations

import argparse
import os
from pathlib import Path
from typing import Any

from scripts.shared.io import load_json


def count_elements(obj: Any) -> int:
    if isinstance(obj, dict):
        return len(obj) + sum(count_elements(v) for v in obj.values())
    if isinstance(obj, list):
        return len(obj) + sum(count_elements(v) for v in obj)
    return 1


def max_depth(obj: Any, depth: int = 0) -> int:
    if isinstance(obj, dict):
        return max((max_depth(v, depth + 1) for v in obj.values()), default=depth)
    if isinstance(obj, list):
        return max((max_depth(v, depth + 1) for v in obj), default=depth)
    return depth


def main() -> None:
    parser = argparse.ArgumentParser(description="Compute demo JSON metrics.")
    parser.add_argument("input_json", type=Path)
    parser.add_argument("fragments_dir", type=Path)
    parser.add_argument("recomposed_json", type=Path, nargs="?")
    args = parser.parse_args()

    fragment_files = [
        f for f in os.listdir(args.fragments_dir) if f.endswith(".json")
    ]
    fragment_files = [f for f in fragment_files if f != "MANIFEST.json"]
    fragment_count = len(fragment_files)
    total_fragment_size = sum(
        (args.fragments_dir / f).stat().st_size for f in fragment_files
    )

    original_size = args.input_json.stat().st_size
    recomposed_size = (
        args.recomposed_json.stat().st_size if args.recomposed_json else 0
    )

    print("Decomposition Results:")
    print(f"  • Fragments created: {fragment_count}")
    print(f"  • Fragment storage: {total_fragment_size} bytes")
    print(f"  • Original size: {original_size} bytes")
    if args.recomposed_json:
        print(f"  • Recomposed size: {recomposed_size} bytes")
    print("")

    data = load_json(args.input_json, required=True)
    total_elements = count_elements(data)
    depth = max_depth(data)

    print("Structure Analysis:")
    print(f"  • Total JSON elements: {total_elements}")
    print(f"  • Maximum nesting depth: {depth} levels")


if __name__ == "__main__":
    main()
