#!/usr/bin/env python3
"""Validation strategies for triangle identity (Agda ↔ JSON ↔ Markdown).

Separates validation concerns: content validation, format validation, identity validation.
Each validator is composable and reusable.
"""

import sys
from pathlib import Path
from typing import Dict, List, Tuple
from concurrent.futures import ThreadPoolExecutor

from scripts.shared_yaml import normalize_field_comparison, normalize_dependencies
from scripts.shared.parallel import get_parallel_settings
from scripts import shared_data


def validate_descriptions(items: List[Dict]) -> Tuple[int, List[str]]:
    """Validate all items have descriptions (strict enforcement).
    
    Args:
        items: Planning items from JSON
        
    Returns:
        (count of missing, list of IDs with missing descriptions)
    """
    missing = 0
    missing_ids: List[str] = []
    
    for item in items:
        desc = item.get("description")
        if not desc or desc == "TODO: description":
            missing += 1
            missing_ids.append(item.get("id", "<unknown>"))
    
    return missing, missing_ids


def validate_item_content(json_items: List[Dict]) -> Tuple[bool, List[str]]:
    """Validate required and recommended fields in JSON items.
    
    Args:
        json_items: Items from planning index
        
    Returns:
        (is_valid, list of error messages)
    """
    required_fields = ["id", "title", "description"]
    recommended_fields = ["status"]
    
    messages: List[str] = []
    valid = True
    issues = []
    
    for item in json_items:
        item_id = item.get("id", "UNKNOWN")
        
        for field in required_fields:
            if not item.get(field):
                issues.append(f"  {item_id}: missing required field '{field}'")
                valid = False
        
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
    
    return valid, messages


def validate_json_to_markdown(
    json_items: List[Dict],
    md_ids: List[str],
    md_frontmatter: List[Dict]
) -> Tuple[bool, List[str]]:
    """Validate JSON items match markdown structure.
    
    Args:
        json_items: Items from planning index
        md_ids: IDs extracted from ROADMAP.md
        md_frontmatter: Frontmatter blocks from ROADMAP.md
        
    Returns:
        (is_valid, list of messages)
    """
    json_ids = {item.get("id") for item in json_items if item.get("id")}
    md_id_set = set(md_ids)
    
    missing_in_md = json_ids - md_id_set
    extra_in_md = md_id_set - json_ids
    
    messages: List[str] = []
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
                
                # Validate key fields match (with Unicode normalization)
                for field in ['title', 'status', 'category']:
                    fm_value = fm.get(field)
                    json_value = json_item.get(field)
                    fm_norm, json_norm = normalize_field_comparison(fm_value, json_value)
                    
                    if fm_norm != json_norm:
                        frontmatter_errors.append(
                            f"  {fm_id}: {field} mismatch (FM: {fm_norm!r}, JSON: {json_norm!r})"
                        )
                
                # Validate dependencies (field name normalized)
                fm_deps, json_deps = normalize_dependencies(
                    fm.get('dependencies'),
                    json_item.get('dependsOn')
                )
                
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

        schema_result = shared_data.validate_roadmap_frontmatter(md_frontmatter)
        if not schema_result.is_valid():
            valid = False
            messages.append("✗ Frontmatter schema validation issues:")
            errors = [str(err) for err in schema_result.errors]
            messages.extend(errors[:10])
            if len(errors) > 10:
                messages.append(f"  ... and {len(errors) - 10} more issues")
        else:
            messages.append(f"✓ Frontmatter schema valid ({len(md_frontmatter)} items)")
    
    return valid, messages


def run_all_validations(base_dir: Path | None = None) -> bool:
    """Run complete triangle identity validation suite.
    
    Returns:
        True if all validations pass, False otherwise
    """
    base_dir = base_dir or shared_data.REPO_ROOT
    print("=" * 60)
    print("Triangle Identity Validation (Strict Mode)")
    print("=" * 60)
    print()
    
    # Load data
    print("Loading data sources...")
    try:
        parallel, workers = get_parallel_settings()
        planning_path = shared_data.resolve_planning_path(repo_root=base_dir)
        roadmap_path = base_dir / "ROADMAP.md"
        if parallel and workers > 1:
            with ThreadPoolExecutor(max_workers=2) as executor:
                json_future = executor.submit(
                    shared_data.load_planning_index_from, planning_path
                )
                md_future = executor.submit(
                    shared_data.load_roadmap_markdown_from, roadmap_path
                )
                json_items = json_future.result()
                md_ids, md_frontmatter = md_future.result()
        else:
            json_items = shared_data.load_planning_index_from(planning_path)
            md_ids, md_frontmatter = shared_data.load_roadmap_markdown_from(roadmap_path)
    except FileNotFoundError as e:
        print(f"✗ {e}")
        return False
    
    print(f"  JSON items: {len(json_items)}")
    print(f"  Markdown IDs: {len(md_ids)}")
    print(f"  Frontmatter blocks: {len(md_frontmatter)}")
    print()
    
    # Validate descriptions (strict)
    missing_desc_count, missing_desc_ids = validate_descriptions(json_items)
    
    # Validate content
    print("Validating item content...")
    content_valid, content_msgs = validate_item_content(json_items)
    for msg in content_msgs:
        print(msg)
    print()
    
    # Validate triangle identity
    print("Validating triangle identity (JSON ↔ Markdown)...")
    triangle_valid, triangle_msgs = validate_json_to_markdown(json_items, md_ids, md_frontmatter)
    for msg in triangle_msgs:
        print(msg)
    print()
    
    # Summary
    print("=" * 60)
    
    if missing_desc_count > 0:
        print(f"✗ {missing_desc_count} item(s) are missing descriptions (STRICT ENFORCEMENT).")
        for item_id in missing_desc_ids[:5]:
            print(f"  - {item_id}")
        if len(missing_desc_ids) > 5:
            print(f"  ... and {len(missing_desc_ids) - 5} more")
        descriptions_valid = False
    else:
        print("✓ All items have valid descriptions")
        descriptions_valid = True
    
    overall_valid = content_valid and triangle_valid and descriptions_valid
    
    if overall_valid:
        print("✓ All validations passed")
    else:
        print("✗ Some validations failed")
    
    return overall_valid


if __name__ == "__main__":
    success = run_all_validations()
    sys.exit(0 if success else 1)
