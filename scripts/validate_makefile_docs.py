#!/usr/bin/env python3
"""
Validate the Makefile "triangle identity" among:
1) The Agda exporter (Source of Truth) -> build/makefile_targets_generated.md
2) The Checked-in Documentation -> docs/automation/MAKEFILE-TARGETS.md

The script enforces that the checked-in documentation matches the generated truth.
"""
from __future__ import annotations

import argparse
import sys
from pathlib import Path
from typing import Dict, Set

ROOT = Path(__file__).resolve().parent.parent
GENERATED_DOC = ROOT / "build" / "makefile_targets_generated.md"
CHECKED_IN_DOC = ROOT / "docs" / "automation" / "MAKEFILE-TARGETS.md"

def parse_markdown_table(text: str) -> Dict[str, str]:
    """Parse a Markdown table into a {Target: Description} dictionary."""
    entries = {}
    lines = text.splitlines()
    for line in lines:
        # Matches row: | `target` | Description |
        if not line.strip().startswith("|") or "---" in line or "Target" in line:
            continue
        
        parts = [p.strip() for p in line.split("|")]
        if len(parts) >= 3:
            target_cell = parts[1]
            desc_cell = parts[2]
            
            # Extract target name from backticks
            target = target_cell.strip("` ")
            entries[target] = desc_cell
            
    return entries

def main() -> int:
    print("Makefile Documentation Triangle Identity")
    print("=" * 60)
    
    if not GENERATED_DOC.exists():
        print(f"✗ Generated docs not found at {GENERATED_DOC}")
        print("  Run 'make regen-makefile' first.")
        return 1
        
    if not CHECKED_IN_DOC.exists():
        print(f"✗ Checked-in docs not found at {CHECKED_IN_DOC}")
        return 1
        
    gen_data = parse_markdown_table(GENERATED_DOC.read_text())
    checked_data = parse_markdown_table(CHECKED_IN_DOC.read_text())
    
    gen_set = set(gen_data.keys())
    checked_set = set(checked_data.keys())
    
    valid = True
    
    # Check 1: Existence
    missing = gen_set - checked_set
    extra = checked_set - gen_set
    
    if missing:
        print(f"✗ Targets missing from documentation ({len(missing)}):")
        for t in sorted(missing):
            print(f"  - {t}")
        valid = False
        
    if extra:
        print(f"✗ Undocumented/Extra targets in documentation ({len(extra)}):")
        for t in sorted(extra):
            print(f"  - {t}")
        valid = False
        
    # Check 2: Content Identity
    mismatches = []
    for t in gen_set.intersection(checked_set):
        gen_desc = gen_data[t]
        checked_desc = checked_data[t]
        
        if gen_desc != checked_desc:
            mismatches.append((t, gen_desc, checked_desc))
            
    if mismatches:
        print(f"✗ Description Mismatches ({len(mismatches)}):")
        for t, gen, checked in mismatches:
            print(f"  Target: {t}")
            print(f"    Gen: {gen}")
            print(f"    Doc: {checked}")
        valid = False
        
    if valid:
        print(f"✓ Documentation is isomorphic to Source of Truth ({len(gen_set)} targets verified)")
        return 0
    else:
        return 1

if __name__ == "__main__":
    raise SystemExit(main())
