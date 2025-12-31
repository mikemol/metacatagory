#!/usr/bin/env python3
"""
Export meta-index markdown from YAML into lint-friendly markdown using an Agda-side renderer schema.

Schema per file:
title: "Meta-Index: Core.ABNF"
sections:
  - name: "Scope"
    bullets:
      - "Formal ABNF syntax (RFC 5234) as an inductive AST for protocol grammars."
  - name: "Key elements"
    bullets:
      - "`ABNF` data type; example `abnfDigit`."
      - "List helpers `_++_`, `concatMap` for grammar assembly."
  ...
"""

import sys
import yaml
from pathlib import Path

def render_meta_index(mi: dict) -> str:
    title = mi.get("title", "").strip()
    out = []
    out.append(f"# {title}")
    out.append("")
    for section in mi.get("sections", []):
        name = section.get("name", "").strip()
        bullets = section.get("bullets", []) or []
        out.append(f"## {name}")
        out.append("")
        for b in bullets:
            out.append(f"* {b}")
        out.append("")
    return "\n".join(out).strip() + "\n"

def load_yaml(path: Path) -> dict:
    return yaml.safe_load(path.read_text(encoding="utf-8")) or {}

def export_file(yaml_path: Path):
    mi = load_yaml(yaml_path)
    md = render_meta_index(mi)
    target = yaml_path.with_suffix(".md")
    target.write_text(md, encoding="utf-8")
    print(f"✓ {yaml_path} -> {target}")

def main():
    if len(sys.argv) < 2:
        print("Usage: export_meta_index.py <dir or yaml file> [...]")
        sys.exit(1)
    paths = [Path(p) for p in sys.argv[1:]]
    yaml_files = []
    for p in paths:
        if p.is_dir():
            yaml_files.extend(sorted(p.rglob("*.yml")))
            yaml_files.extend(sorted(p.rglob("*.yaml")))
        elif p.suffix in {".yml", ".yaml"}:
            yaml_files.append(p)
    if not yaml_files:
        print("No YAML files found")
        sys.exit(0)
    for yf in yaml_files:
        export_file(yf)

if __name__ == "__main__":
    main()

