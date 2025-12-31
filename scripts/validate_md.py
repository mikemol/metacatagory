#!/usr/bin/env python3
"""Validate ROADMAP.md contains items from canonical."""

import json
import re
from pathlib import Path
import sys

def extract_md_items(md_path):
    """Extract item IDs from ROADMAP.md structured sections."""
    with open(md_path) as f:
        content = f.read()
    
    # Pattern for item titles (can use titles as proxy for IDs)
    pattern = r'\* \*\*(.+?)\*\*'
    titles = set(match.group(1).strip() for match in re.finditer(pattern, content))
    return titles

def validate():
    base = Path(__file__).resolve().parent.parent
    canonical = json.load(open(base / "build/canonical_roadmap.json"))
    md_titles = extract_md_items(base / "ROADMAP.md")
    
    # Get canonical titles
    canonical_titles = {item["title"].strip() for item in canonical}
    
    missing = canonical_titles - md_titles
    extra = md_titles - canonical_titles
    
    if missing:
        print(f"❌ {len(missing)} canonical items missing from ROADMAP.md:")
        for title in sorted(list(missing)[:10]):
            print(f"  - {title}")
        if len(missing) > 10:
            print(f"  ... and {len(missing) - 10} more")
    
    if extra:
        print(f"❌ {len(extra)} items in ROADMAP.md not in canonical:")
        for title in sorted(list(extra)[:10]):
            print(f"  - {title}")
        if len(extra) > 10:
            print(f"  ... and {len(extra) - 10} more")
    
    overlap = len(canonical_titles & md_titles)
    
    if not (missing or extra):
        print(f"✓ ROADMAP.md matches canonical ({overlap} items)")
        return 0
    else:
        print(f"\n⚠️  Partial match: {overlap} items overlap")
        print(f"❌ Validation failed: run 'make roadmap-export-md' to sync")
        return 1

if __name__ == "__main__":
    sys.exit(validate())
