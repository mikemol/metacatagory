#!/usr/bin/env python3
"""Verify roundtrip property between original and recomposed JSON."""

from __future__ import annotations

import argparse
import hashlib
import json

from scripts.shared.io import load_json


def main() -> None:
    parser = argparse.ArgumentParser(description="Verify JSON roundtrip.")
    parser.add_argument("original_json")
    parser.add_argument("recomposed_json")
    parser.add_argument("--strict", action="store_true")
    args = parser.parse_args()

    original_data = load_json(args.original_json, required=True)
    recomposed_data = load_json(args.recomposed_json, required=True)

    original_canon = json.dumps(original_data, sort_keys=True, separators=(",", ":"))
    recomposed_canon = json.dumps(recomposed_data, sort_keys=True, separators=(",", ":"))

    original_hash = hashlib.md5(original_canon.encode()).hexdigest()
    recomposed_hash = hashlib.md5(recomposed_canon.encode()).hexdigest()

    print(f"Original JSON hash:   {original_hash}")
    print(f"Recomposed JSON hash: {recomposed_hash}")

    orig_keys = set(original_data.keys()) if isinstance(original_data, dict) else set()
    recomp_keys = (
        set(recomposed_data.keys()) if isinstance(recomposed_data, dict) else set()
    )

    if orig_keys == recomp_keys:
        print(f"✓ Root keys preserved: {sorted(orig_keys)}")
    else:
        print("✗ Root keys differ")
        print(f"  Original: {sorted(orig_keys)}")
        print(f"  Recomposed: {sorted(recomp_keys)}")

    if original_data == recomposed_data:
        print("✅ ROUNDTRIP PROPERTY VERIFIED")
        print("   backward (forward strat m) ≡ m HOLDS")
        return

    print("⚠️  Structural difference detected")
    if original_canon == recomposed_canon:
        print("✓ Structurally identical (normalization only)")
        return

    print("✗ Structural difference detected")
    if args.strict:
        raise SystemExit(1)


if __name__ == "__main__":
    main()
