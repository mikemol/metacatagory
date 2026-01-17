#!/usr/bin/env python3
"""Recompose a JSON document from fragment files."""

from __future__ import annotations

import argparse
from pathlib import Path
from typing import Any

from scripts.shared.io import load_json, save_json


def _resolve_fragment_path(frag_path: str, fragments_dir: Path) -> Path:
    path = Path(frag_path)
    if path.is_absolute():
        return path
    return fragments_dir / path


def main() -> None:
    parser = argparse.ArgumentParser(description="Recompose JSON from fragments.")
    parser.add_argument("fragments_dir", type=Path)
    parser.add_argument("output_json", type=Path)
    parser.add_argument("--manifest", type=Path, default=None)
    parser.add_argument("--wrap-key", dest="wrap_key", default=None)
    args = parser.parse_args()

    manifest_path = args.manifest or (args.fragments_dir / "MANIFEST.json")
    manifest = load_json(manifest_path, required=True)

    fragments = manifest.get("fragments", [])
    if not fragments:
        print("⚠️  No fragment metadata found")
        save_json(args.output_json, {})
        return

    largest = max(fragments, key=lambda item: item.get("size", 0))
    fragment_path = _resolve_fragment_path(largest["file"], args.fragments_dir)
    data: Any = load_json(fragment_path, required=True)

    if args.wrap_key:
        recomposed = {args.wrap_key: data}
    else:
        recomposed = data

    save_json(args.output_json, recomposed)
    print(f"Recomposed JSON written: {args.output_json}")
    print(f"Main fragment: {fragment_path}")


if __name__ == "__main__":
    main()
