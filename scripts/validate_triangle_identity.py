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
import yaml
import re
import sys
from pathlib import Path
from typing import Dict, List, Set, Tuple

REPO_ROOT = Path(__file__).parent.parent


def validate_descriptions(items: List[Dict]) -> Tuple[List[Dict], int, List[str]]:
    """Verify every item has a description.
    
    Unlike the previous 'backfill' logic, this enforces strict completeness.
    Missing descriptions are treated as validation errors.

    Returns: items (unmodified), count of missing, list of IDs missing descriptions.
    """
    missing = 0
    missing_ids: List[str] = []
    
    for item in items:
        desc = item.get("description")
        # Strict check: Description must exist and not be the placeholder artifact
        if not desc or desc == "TODO: description":
            missing += 1
            item_id = item.get("id", "<unknown>")
            missing_ids.append(item_id)
            
    return items, missing, missing_ids


def load_canonical_json() -> List[Dict]:
    """Load canonical roadmap from JSON (supports list or {items:[...]})."""
    json_path = REPO_ROOT / "build" / "canonical_roadmap.json"
    if not json_path.exists():
        print(f"✗ {json_path} not found")
        print("  Run: make roadmap-merge")
        return []
    
    with open(json_path) as f:
        data = json.load(f)
    
    if isinstance(data, list):
        return data
    if isinstance(data, dict):
        return data.get("items", [])
    
    print("✗ Unexpected JSON shape in canonical_roadmap.json")
    return []


def load_roadmap_markdown() -> Tuple[List[str], List[Dict]]:
    """Extract roadmap item IDs and frontmatter from ROADMAP.md."""
    # The export script writes to repo root (ROADMAP.md)
    md_path = REPO_ROOT / "ROADMAP.md"
    if not md_path.exists():
        print(f"✗ {md_path} not found")
        return [], []
    
    content = md_path.read_text()
    
    # Extract YAML frontmatter blocks
    frontmatter_items = []
    yaml_blocks = re.findall(r'```yaml\n(.*?)\n```', content, re.DOTALL)
    
    for yaml_block in yaml_blocks:
        try:
            data = yaml.safe_load(yaml_block)
            if data and isinstance(data, dict):
                frontmatter_items.append(data)
        except yaml.YAMLError as e:
            print(f"Warning: Failed to parse YAML block: {e}")
    
    # Extract IDs from frontmatter
    ids = set()
    for item in frontmatter_items:
        if 'id' in item:
            ids.add(item['id'])
    
    # Fallback: extract IDs from text (for backwards compatibility)
    for match in re.finditer(r'\[([A-Z]+-\d+)\]', content):
        ids.add(match.group(1))
    
    return sorted(ids), frontmatter_items


def validate_json_to_markdown(json_items: List[Dict], md_ids: List[str], md_frontmatter: List[Dict]) -> Tuple[bool, str]:
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
    
        # Validate frontmatter content matches JSON
        if md_frontmatter:
            frontmatter_errors = []
            json_by_id = {item.get('id'): item for item in json_items if item.get('id')}
        
            for fm in md_frontmatter:
                fm_id = fm.get('id')
                if not fm_id:
                    continue
            
                json_item = json_by_id.get(fm_id)
                if not json_item:
                    continue
            
                # Validate key fields match
                for field in ['title', 'status', 'category']:
                    fm_value = fm.get(field)
                    json_value = json_item.get(field)
                
                    if fm_value != json_value:
                        frontmatter_errors.append(
                            f"  {fm_id}: {field} mismatch (FM: {fm_value!r}, JSON: {json_value!r})"
                        )
            
                # Validate dependencies (if present)
                fm_deps = set(fm.get('dependencies', []))
                json_deps = set(json_item.get('dependsOn', []))
            
                if fm_deps != json_deps:
                    missing = json_deps - fm_deps
                    extra = fm_deps - json_deps
                    if missing or extra:
                        frontmatter_errors.append(
                            f"  {fm_id}: dependency mismatch (missing: {missing}, extra: {extra})"
                        )
        
            if frontmatter_errors:
                valid = False
                messages.append(f"✗ Frontmatter validation issues:")
                messages.extend(frontmatter_errors[:10])
                if len(frontmatter_errors) > 10:
                    messages.append(f"  ... and {len(frontmatter_errors) - 10} more issues")
            else:
                messages.append(f"✓ Frontmatter matches JSON ({len(md_frontmatter)} items validated)")
    
    return valid, "\n".join(messages)


def validate_item_content(json_items: List[Dict]) -> Tuple[bool, str]:
    """Validate required fields in JSON items."""
    messages = []
    valid = True
    
    required_fields = ["id", "title", "description"]
    recommended_fields = ["status"]
    
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
    print("Triangle Identity Validation (Strict Mode)")
    print("=" * 60)
    print()
    
    # Load data
    print("Loading data sources...")
    json_items = load_canonical_json()
    
    # Perform strict description validation
    _, missing_desc_count, missing_desc_ids = validate_descriptions(json_items)
    
    md_ids, md_frontmatter = load_roadmap_markdown()
    
    if not json_items:
        print("✗ No JSON data found")
        sys.exit(1)
    
    print(f"  JSON items: {len(json_items)}")
    print(f"  Markdown IDs: {len(md_ids)}")
    print(f"  Frontmatter blocks: {len(md_frontmatter)}")
    print()
    
    # Validate content
    print("Validating item content...")
    content_valid, content_msg = validate_item_content(json_items)
    print(content_msg)
    print()
    
    # Validate triangle identity
    print("Validating triangle identity (JSON ↔ Markdown)...")
    triangle_valid, triangle_msg = validate_json_to_markdown(json_items, md_ids, md_frontmatter)
    print(triangle_msg)
    print()
    
    # Summary
    print("=" * 60)
    
    # Strictness Check
    if missing_desc_count > 0:
        print(f"✗ {missing_desc_count} item(s) are missing descriptions (STRICT ENFORCEMENT). Examples:")
        for item_id in missing_desc_ids[:10]:
            print(f"  - {item_id}")
        if len(missing_desc_ids) > 10:
            print(f"  ... and {len(missing_desc_ids) - 10} more")
        descriptions_valid = False
    else:
        print("✓ All items have valid descriptions")
        descriptions_valid = True

    overall_valid = content_valid and triangle_valid and descriptions_valid
    
    if overall_valid:
        print("✓ All validations passed")
        sys.exit(0)
    else:
        print("✗ Some validations failed")
        sys.exit(1)


if __name__ == "__main__":
    main()
