#!/usr/bin/env python3
"""Shared normalization and validation utilities.

Provides string normalization, data validation, and field defaulting helpers.
"""

import re
from typing import Any, Dict, List, Optional, Set


def normalize_title(title: str) -> str:
    """Normalize title for comparison and deduplication.
    
    Removes special characters, normalizes whitespace, converts to lowercase.
    
    Args:
        title: Original title
        
    Returns:
        Normalized title
        
    Example:
        >>> normalize_title("Task-001: Fix! Issues?")
        'task 001 fix issues'
    """
    return re.sub(r'\W+', ' ', title).strip().lower()


def normalize_id(item_id: str) -> str:
    """Normalize ID for comparison.
    
    Converts to lowercase, removes extra whitespace.
    
    Args:
        item_id: Original ID
        
    Returns:
        Normalized ID
    """
    return item_id.strip().lower()


def ensure_item_fields(
    item: Dict[str, Any],
    defaults: Optional[Dict[str, Any]] = None
) -> None:
    """Ensure item has all required fields with sensible defaults.
    
    Modifies item in-place.
    
    Args:
        item: Item dictionary to validate
        defaults: Custom default values (merged with standard defaults)
    """
    standard_defaults = {
        'id': '',
        'title': '',
        'status': 'planned',
        'category': 'other',
        'dependsOn': [],
        'tags': [],
        'files': [],
        'provenance': [],
    }
    
    if defaults:
        standard_defaults.update(defaults)
    
    for key, default_value in standard_defaults.items():
        item.setdefault(key, default_value)


def ensure_provenance(
    item: Dict[str, Any],
    source: Optional[str] = None
) -> None:
    """Ensure item has provenance field tracking its source.
    
    Modifies item in-place.
    
    Args:
        item: Item dictionary
        source: Source identifier to add (e.g., "roadmap", "doclint")
            If None, uses item.get("source", "")
    """
    if source is None:
        source = item.get('source', '')
    
    item.setdefault('provenance', [])
    
    if not item['provenance']:
        item_id = item.get('id', '')
        item['provenance'] = [f"{item_id}|{source}"]


def deduplicate_items_by_id(
    items: List[Dict[str, Any]],
    merge_lists: Optional[List[str]] = None
) -> List[Dict[str, Any]]:
    """Deduplicate items by ID, merging list fields.
    
    Args:
        items: List of item dictionaries
        merge_lists: List field names to merge (defaults to dependsOn, related, tags)
        
    Returns:
        Deduplicated list of items
    """
    if merge_lists is None:
        merge_lists = ['dependsOn', 'related', 'tags']
    
    seen: Dict[str, Dict[str, Any]] = {}
    
    for item in items:
        item_id = item.get('id', '')
        if not item_id:
            continue
        
        if item_id in seen:
            existing = seen[item_id]
            # Merge list fields
            for field in merge_lists:
                if field in item:
                    existing_vals = existing.get(field, [])
                    new_vals = item.get(field, [])
                    existing[field] = list(set(existing_vals) | set(new_vals))
            
            # Prefer non-empty string fields
            for key, value in item.items():
                if key not in merge_lists and value and not existing.get(key):
                    existing[key] = value
        else:
            seen[item_id] = item.copy()
    
    return list(seen.values())


def merge_dependencies(
    *dep_lists: List[str]
) -> List[str]:
    """Merge multiple dependency lists, removing duplicates.
    
    Args:
        *dep_lists: Variable number of dependency lists
        
    Returns:
        Deduplicated, sorted list of dependencies
    """
    merged: Set[str] = set()
    for dep_list in dep_lists:
        if dep_list:
            merged.update(dep_list)
    return sorted(merged)


def validate_item_structure(
    item: Dict[str, Any],
    required_fields: Optional[List[str]] = None
) -> List[str]:
    """Validate item has required structure.
    
    Args:
        item: Item dictionary to validate
        required_fields: List of required field names
        
    Returns:
        List of validation errors (empty if valid)
    """
    if required_fields is None:
        required_fields = ['id', 'title', 'status']
    
    errors = []
    
    for field in required_fields:
        if field not in item:
            errors.append(f"Missing required field: {field}")
        elif not item[field]:
            errors.append(f"Empty required field: {field}")
    
    # Validate types
    list_fields = ['dependsOn', 'tags', 'files', 'provenance']
    for field in list_fields:
        if field in item and not isinstance(item[field], list):
            errors.append(f"Field {field} must be a list, got {type(item[field])}")
    
    return errors


def clean_empty_strings(data: Any) -> Any:
    """Recursively remove empty strings from data structures.
    
    Useful for cleaning up JSON before export.
    
    Args:
        data: Data structure to clean
        
    Returns:
        Cleaned data structure
    """
    if isinstance(data, dict):
        return {k: clean_empty_strings(v) for k, v in data.items() if v != ''}
    elif isinstance(data, list):
        return [clean_empty_strings(item) for item in data if item != '']
    else:
        return data


def unescape_string(value: str) -> str:
    """Best-effort unescape of escaped sequences in strings."""
    try:
        return bytes(value, "utf-8").decode("unicode_escape")
    except Exception:
        return value


def extract_keywords_from_text(text: str, *, min_len: int = 4) -> Set[str]:
    """Extract lowercase keywords from free-form text."""
    words = re.split(r"\W+", text.lower())
    return {word for word in words if len(word) >= min_len}
