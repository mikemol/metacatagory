#!/usr/bin/env python3
"""
Validate triangle identity across documentation formats.

Checks consistency between:
- Agda source data (Plan.CIM.CanonicalRoadmap)
- JSON exports (build/canonical_roadmap.json)
- Markdown exports (docs/planning/ROADMAP.md)

This enforces the triangle identity:
    Agda → JSON → Markdown → Validation
      ↑                          ↓
      └──────────────────────────┘
"""

import json
import re
import sys
from pathlib import Path
from typing import Dict, List, Set, Tuple

REPO_ROOT = Path(__file__).parent.parent


def load_canonical_json() -> List[Dict]:
    """Load canonical roadmap from JSON."""
    json_path = REPO_ROOT / "build" / "canonical_roadmap.json"
    if not json_path.exists():
        print(f"✗ {json_path} not found")
        print("  Run: make roadmap-merge")
        return []
    
    with open(json_path) as f:
        data = json.load(f)
    
    return data.get("items", [])


def load_roadmap_markdown() -> List[str]:
    """Extract roadmap item IDs from ROADMAP.md."""
    md_path = REPO_ROOT / "docs" / "planning" / "ROADMAP.md"
    if not md_path.exists():
        print(f"✗ {md_path} not found")
        return []
    
    content = md_path.read_text()
    
    # Extract roadmap IDs (patterns like GP-001, TASK-123, etc.)
    # Look for patterns in headings or list items
    ids = set()
    
    # Pattern 1: "### [ID] Title" or "- **[ID]** Title"
    for match in re.finditer(r'\[([A-Z]+-\d+)\]', content):
        ids.add(match.group(1))
    
    # Pattern 2: "id: TASK-123" in metadata blocks
    for match in re.finditer(r'id:\s*([A-Z]+-\d+)', content):
        ids.add(match.group(1))
    
    return sorted(ids)


def validate_json_to_markdown(json_items: List[Dict], md_ids: List[str]) -> Tuple[bool, str]:
    """Validate JSON items match markdown IDs."""
    json_ids = {item.get("id") for item in json_items if item.get("id")}
    md_id_set = set(md_ids)
    
    missing_in_md = json_ids - md_id_set
    extra_in_md = md_id_set - json_ids
    
    messages = []
    valid = True
    
    if missing_in_md:
        valid = False
        messages.append(f"✗ {len(missing_in_md)} items in JSON but not in Markdown:")
        for item_id in sorted(missing_in_md)[:5]:
            messages.append(f"    - {item_id}")
        if len(missing_in_md) > 5:
            messages.append(f"    ... and {len(missing_in_md) - 5} more")
    
    if extra_in_md:
        valid = False
        messages.append(f"✗ {len(extra_in_md)} items in Markdown but not in JSON:")
        for item_id in sorted(extra_in_md)[:5]:
            messages.append(f"    - {item_id}")
        if len(extra_in_md) > 5:
            messages.append(f"    ... and {len(extra_in_md) - 5} more")
    
    if valid:
        messages.append(f"✓ Triangle identity: JSON ↔ Markdown ({len(json_ids)} items match)")
    
    return valid, "\n".join(messages)


def validate_item_content(json_items: List[Dict]) -> Tuple[bool, str]:
    """Validate required fields in JSON items."""
    messages = []
    valid = True
    
    required_fields = ["id", "title"]
    recommended_fields = ["status", "description"]
    
    issues = []
    for item in json_items:
        item_id = item.get("id", "UNKNOWN")
        
        # Check required fields
        for field in required_fields:
            if not item.get(field):
                issues.append(f"  {item_id}: missing required field '{field}'")
                valid = False
        
        # Check recommended fields
        for field in recommended_fields:
            if not item.get(field):
                issues.append(f"  {item_id}: missing recommended field '{field}'")
    
    if issues:
        messages.append(f"✗ Content validation issues:")
        messages.extend(issues[:10])
        if len(issues) > 10:
            messages.append(f"  ... and {len(issues) - 10} more issues")
    else:
        messages.append(f"✓ All items have required fields")
    
    return valid, "\n".join(messages)


def main():
    """Run triangle identity validation."""
    print("=" * 60)
    print("Triangle Identity Validation")
    print("=" * 60)
    print()
    
    # Load data
    print("Loading data sources...")
    json_items = load_canonical_json()
    md_ids = load_roadmap_markdown()
    
    if not json_items:
        print("✗ No JSON data found")
        sys.exit(1)
    
    print(f"  JSON items: {len(json_items)}")
    print(f"  Markdown IDs: {len(md_ids)}")
    print()
    
    # Validate content
    print("Validating item content...")
    content_valid, content_msg = validate_item_content(json_items)
    print(content_msg)
    print()
    
    # Validate triangle identity
    print("Validating triangle identity (JSON ↔ Markdown)...")
    triangle_valid, triangle_msg = validate_json_to_markdown(json_items, md_ids)
    print(triangle_msg)
    print()
    
    # Summary
    print("=" * 60)
    if content_valid and triangle_valid:
        print("✓ All validations passed")
        sys.exit(0)
    else:
        print("✗ Some validations failed")
        sys.exit(1)


if __name__ == "__main__":
    main()
