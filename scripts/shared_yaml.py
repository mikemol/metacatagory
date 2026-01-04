#!/usr/bin/env python3
"""YAML serialization utilities with proper Unicode handling.

Provides composable YAML dumping and normalization for consistent
serialization across all scripts.
"""

from typing import Any, Dict, Tuple

try:
    import yaml  # type: ignore
except ImportError:
    yaml = None


def dump_yaml(data: Dict[str, Any]) -> str:
    """Serialize dict to YAML with Unicode support.
    
    Uses PyYAML if available (with allow_unicode=True), falls back to
    minimal key:value serialization.
    
    Args:
        data: Dictionary to serialize
        
    Returns:
        YAML string representation
    """
    if yaml is not None:
        # Use default_style='"' to force quoting of all scalars
        # This prevents YAML parsing errors when values contain colons
        return yaml.dump(
            data,
            default_flow_style=False,
            sort_keys=False,
            allow_unicode=True,
            default_style='"'
        ).rstrip()

    # Minimal fallback for dict[str, str | list[str]]
    lines = []
    for key, value in data.items():
        if isinstance(value, list):
            lines.append(f"{key}:")
            for item in value:
                lines.append(f"  - {item}")
        else:
            lines.append(f"{key}: {value}")
    return "\n".join(lines)


def normalize_unicode(value: Any) -> Any:
    """Normalize Unicode representation for consistent comparison.
    
    Modern Python (3.x) strings are already Unicode. This function is now
    a passthrough, but kept for API compatibility.
    
    Args:
        value: String or other value to normalize
        
    Returns:
        Input value unchanged
    """
    return value


def normalize_field_comparison(fm_value: Any, json_value: Any) -> Tuple[Any, Any]:
    """Prepare field values for comparison, normalizing Unicode.
    
    Args:
        fm_value: Frontmatter value
        json_value: JSON value
        
    Returns:
        (normalized_fm_value, normalized_json_value)
    """
    # Both are already Unicode strings in Python 3
    fm_normalized = str(fm_value) if fm_value else fm_value
    json_normalized = str(json_value) if json_value else json_value
    return fm_normalized, json_normalized


def normalize_dependencies(fm_deps: Any, json_deps: Any) -> Tuple[set, set]:
    """Normalize dependency lists for comparison.
    
    Handles field name differences (dependencies vs dependsOn) and missing values.
    
    Args:
        fm_deps: Frontmatter dependencies (or None)
        json_deps: JSON dependencies (or None)
        
    Returns:
        (fm_deps_set, json_deps_set)
    """
    fm_set = set(fm_deps or [])
    json_set = set(json_deps or [])
    return fm_set, json_set
