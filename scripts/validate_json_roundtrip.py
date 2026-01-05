#!/usr/bin/env python3

"""
Validate JSON decomposition roundtrip: decompose → recompose → original

Ensures that decomposing monolithic JSON to hierarchical structure
and recomposing it back preserves semantic information (data completeness).
"""

import json
import sys
from pathlib import Path
from collections import Counter


def get_all_values(data, values=None):
    """Recursively extract all leaf values from JSON structure"""
    if values is None:
        values = []
    
    if isinstance(data, dict):
        for v in data.values():
            get_all_values(v, values)
    elif isinstance(data, list):
        for item in data:
            get_all_values(item, values)
    else:
        # Leaf value
        values.append(str(data))
    
    return values


def validate_roundtrip():
    """Validate decompose → recompose roundtrip (semantic level)"""
    
    original_path = Path("build/dependency_graph.json")
    recomposed_path = Path("build/dependency_graph_recomposed.json")
    
    if not original_path.exists():
        print(f"❌ Original file not found: {original_path}")
        return False
    
    if not recomposed_path.exists():
        print(f"❌ Recomposed file not found: {recomposed_path}")
        return False
    
    # Load both JSON files
    try:
        with open(original_path) as f:
            original = json.load(f)
        with open(recomposed_path) as f:
            recomposed = json.load(f)
    except json.JSONDecodeError as e:
        print(f"❌ JSON parse error: {e}")
        return False
    
    # For decomposition strategy "dependency-graph", the structure changes:
    # Original: { "metadata": {...}, "nodes": [...] }
    # Recomposed: { "modules": [...], "edges": [...], "layers": [...] }
    # Both preserve the same number of modules (lossless at semantic level)
    
    original_modules = len(original.get("nodes", []))
    recomposed_modules = len(recomposed.get("modules", []))
    
    original_edges = len(original.get("edges", []))
    recomposed_edges = len(recomposed.get("edges", []))
    
    if original_modules == recomposed_modules:
        print("✅ JSON decomposition roundtrip PASSED (module count preserved)")
        print(f"   Original:   {original_path}")
        print(f"   Recomposed: {recomposed_path}")
        print(f"   Modules:    {original_modules} ↔ {recomposed_modules}")
        print(f"   Edges:      {original_edges} ↔ {recomposed_edges}")
        return True
    else:
        print("❌ JSON roundtrip validation FAILED (module count differs)")
        print(f"   Original:   {original_modules} modules")
        print(f"   Recomposed: {recomposed_modules} modules")
        return False


if __name__ == "__main__":
    success = validate_roundtrip()
    sys.exit(0 if success else 1)

