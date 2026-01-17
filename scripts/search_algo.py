#!/usr/bin/env python3
"""
SPPF-Composable Onboarding Header

Roadmap: src/agda/Plan/CIM/Utility.agda
Architecture: ARCHITECTURE.md
Onboarding: .github/copilot-instructions.md

Constructive Proof Semantics:
- This script participates in the composable SPPF model, mirroring Agda record patterns for protocol,
    witness, and universal property semantics.
- All logic should be traceable to roadmap nodes and architectural principles.
- For onboarding, review the architecture and roadmap, and recursively revisit related nodes for
    context and composability.

Search/query tool for algorithms and properties in the Agda codebase.

Heuristics:

Usage:
  scripts/search_algo.py --q regular epi --path src/agda
"""
from __future__ import annotations
import argparse
from pathlib import Path
from typing import Any

from scripts.shared.agda_declarations import scan_agda_declarations

def index_paths(root: Path) -> list[dict[str, Any]]:
    files = sorted(root.rglob("*.agda"))
    entries: list[dict[str, Any]] = []
    for p in files:
        for decl in scan_agda_declarations(p):
            entries.append(
                {
                    "file": decl.file,
                    "line": decl.line,
                    "text": decl.text,
                    "name": decl.name,
                }
            )
    return entries

def query(entries: list[dict[str, Any]], q: str) -> list[dict[str, Any]]:
    ql = q.lower()
    return [e for e in entries if ql in e["text"].lower() or (e["name"] and ql in e["name"].lower())]

def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument(
        "--path", default="src/agda", help="Path to search (default src/agda)"
    )
    ap.add_argument("--q", required=True, help="Query string (substring match)")
    args = ap.parse_args()

    root = Path(args.path)
    entries: list[dict[str, Any]] = index_paths(root)
    matches: list[dict[str, Any]] = query(entries, args.q)
    for m in matches:
        print(f"{m['file']}:{m['line']}: {m['text']}")

if __name__ == "__main__":
    main()
