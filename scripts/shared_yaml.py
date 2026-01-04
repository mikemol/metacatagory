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
        return yaml.dump(
            data,
            default_flow_style=False,
            sort_keys=False,
            allow_unicode=True
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
    
    Handles both literal Unicode characters (→) and escaped sequences (\\u2192).
    
    Args:
        value: String or other value to normalize
        
    Returns:
        Normalized value (or input if not a string)
    """
    if not isinstance(value, str):
        return value
    
    try:
        # Decode any escaped sequences
        return value.encode().decode('unicode-escape')
    except (UnicodeDecodeError, AttributeError):
        return value


def normalize_field_comparison(fm_value: Any, json_value: Any) -> Tuple[Any, Any]:
    """Prepare field values for comparison, normalizing Unicode.
    
    Args:
        fm_value: Frontmatter value
        json_value: JSON value
        
    Returns:
        (normalized_fm_value, normalized_json_value)
    """
    fm_normalized = str(normalize_unicode(fm_value)) if fm_value else fm_value
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
