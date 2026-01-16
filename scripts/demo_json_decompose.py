#!/usr/bin/env python3
"""Decompose a JSON document into fragment files and a manifest."""

from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path
from typing import Any

from scripts.shared.io import load_json, save_json


def _fragment_name(path: str, index: int, style: str) -> str:
    if style == "numbered":
        key = path.split(".")[-1].replace("[", "_").replace("]", "_") or "root"
        return f"{index:02d}_{key}.json"
    safe = path.replace(".", "_").replace("[", "_").replace("]", "_") or "root"
    return f"{safe}.json"


def decompose(data: Any, fragments_dir: Path, style: str) -> list[dict[str, Any]]:
    fragments: list[dict[str, Any]] = []

    def extract(obj: Any, path: str = "root") -> None:
        nonlocal fragments
        if isinstance(obj, dict):
            for key, value in obj.items():
                current_path = f"{path}.{key}" if path else key
                if isinstance(value, (dict, list)):
                    fragment_file = _fragment_name(current_path, len(fragments), style)
                    fragment_path = fragments_dir / fragment_file
                    save_json(fragment_path, value)
                    fragments.append(
                        {
                            "path": current_path,
                            "type": type(value).__name__,
                            "size": len(json.dumps(value)),
                            "file": str(fragment_path),
                        }
                    )
                    extract(value, current_path)
        elif isinstance(obj, list):
            for i, item in enumerate(obj):
                current_path = f"{path}[{i}]"
                if isinstance(item, (dict, list)):
                    extract(item, current_path)

    extract(data, "root")
    return fragments


def main() -> None:
    parser = argparse.ArgumentParser(description="Decompose JSON into fragments.")
    parser.add_argument("input_json", type=Path)
    parser.add_argument("fragments_dir", type=Path)
    parser.add_argument("--style", choices=["path", "numbered"], default="path")
    args = parser.parse_args()

    args.fragments_dir.mkdir(parents=True, exist_ok=True)

    data = load_json(args.input_json)
    fragments = decompose(data, args.fragments_dir, args.style)

    manifest = {
        "input_file": str(args.input_json),
        "total_fragments": len(fragments),
        "fragments": fragments,
        "input_hash": hashlib.md5(
            json.dumps(data, sort_keys=True).encode()
        ).hexdigest(),
    }
    save_json(args.fragments_dir / "MANIFEST.json", manifest)

    print(f"Extracted {len(fragments)} fragments")
    for frag in fragments[:5]:
        print(f"  • {frag['path']} ({frag['type']}, {frag['size']} bytes)")
    if len(fragments) > 5:
        print(f"  ... and {len(fragments) - 5} more")


if __name__ == "__main__":
    main()
