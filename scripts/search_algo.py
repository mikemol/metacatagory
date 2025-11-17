#!/usr/bin/env python3
"""
Search/query tool for algorithms and properties in the Agda codebase.

Heuristics:
- Index declarations like `record Name : Set where`, `constructor THEOREM_*`, and `postulate` entries.
- Allow substring queries across names and lines.

Usage:
  scripts/search_algo.py --q regular epi --path src/agda
"""
from __future__ import annotations
import argparse
import re
from pathlib import Path

DECL_RE = re.compile(r'^\s*(?:record|data|postulate|module)\b.*$|^\s*constructor\s+[A-Za-z0-9_]+\b.*$', re.MULTILINE)
NAME_CAPTURE_RE = re.compile(r'^\s*record\s+([A-Za-z0-9_]+)\b|^\s*constructor\s+([A-Za-z0-9_]+)\b')


def index_paths(root: Path):
    files = sorted(root.rglob("*.agda"))
    entries = []
    for p in files:
        text = p.read_text(encoding="utf-8", errors="ignore")
        for i, line in enumerate(text.splitlines(), start=1):
            if DECL_RE.match(line):
                name = None
                m = NAME_CAPTURE_RE.match(line)
                if m:
                    name = m.group(1) or m.group(2)
                entries.append({
                    "file": str(p),
                    "line": i,
                    "text": line.strip(),
                    "name": name,
                })
    return entries


def query(entries, q: str):
    ql = q.lower()
    return [e for e in entries if ql in e["text"].lower() or (e["name"] and ql in e["name"].lower())]


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--path", default="src/agda", help="Path to search (default src/agda)")
    ap.add_argument("--q", required=True, help="Query string (substring match)")
    args = ap.parse_args()

    root = Path(args.path)
    entries = index_paths(root)
    matches = query(entries, args.q)
    for m in matches:
        print(f"{m['file']}:{m['line']}: {m['text']}")


if __name__ == "__main__":
    main()
