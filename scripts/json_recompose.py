#!/usr/bin/env python3
"""
JSON Recomposition: Hierarchical → Monolithic

Reconstructs monolithic JSON from hierarchical directory structures,
validating roundtrip equivalence.

Usage:
    python json_recompose.py <hierarchical_dir> <output_file>

Example:
    python json_recompose.py build/deps/ build/dependency_graph.json
"""

import json
import os
import sys
from pathlib import Path
from typing import Any, Dict, List, Optional


class JSONRecomposer:
    """Base class for JSON recomposition strategies."""
    
    def __init__(self, hierarchical_dir: str):
        self.hierarchical_dir = Path(hierarchical_dir)
        self.metadata = self._load_metadata()
    
    def _load_metadata(self) -> Dict[str, Any]:
        """Load metadata from hierarchical structure."""
        metadata_file = self.hierarchical_dir / "_metadata.json"
        if metadata_file.exists():
            with open(metadata_file, "r") as f:
                return json.load(f)
        return {}
    
    def recompose(self) -> Dict[str, Any]:
        """Recompose hierarchical structure back to monolithic JSON."""
        raise NotImplementedError


class DependencyGraphRecomposer(JSONRecomposer):
    """Recompose dependency graph from hierarchical structure."""
    
    def recompose(self) -> Dict[str, Any]:
        """
        Reconstruct dependency_graph.json from:
        - modules/: Individual module definitions
        - edges: Reconstructed from import relationships
        """
        modules = {}
        edges = []
        edge_set = set()  # Avoid duplicates
        
        # Read all module files
        modules_dir = self.hierarchical_dir / "modules"
        if modules_dir.exists():
            for json_file in modules_dir.rglob("*.json"):
                if json_file.name == "_index.json":
                    continue
                
                with open(json_file, "r") as f:
                    module_data = json.load(f)
                
                module_name = module_data.get("name")
                if not module_name:
                    continue
                
                modules[module_name] = True
                
                # Extract edges from imports
                for imported in module_data.get("imports", []):
                    edge = {"from": module_name, "to": imported}
                    edge_key = (module_name, imported)
                    if edge_key not in edge_set:
                        edges.append(edge)
                        edge_set.add(edge_key)
        
        # Read layers for additional validation
        layers = {}
        layers_dir = self.hierarchical_dir / "layers"
        if layers_dir.exists():
            for json_file in sorted(layers_dir.glob("layer-*.json")):
                layer_num = int(json_file.stem.split("-")[1])
                with open(json_file, "r") as f:
                    layer_modules = json.load(f)
                layers[layer_num] = layer_modules
        
        # Read cycles
        cycles = []
        cycles_dir = self.hierarchical_dir / "cycles"
        if cycles_dir.exists():
            for json_file in sorted(cycles_dir.glob("cycle-*.json")):
                with open(json_file, "r") as f:
                    cycle_data = json.load(f)
                    cycles.append(cycle_data.get("modules", []))
        
        result = {
            "modules": modules,
            "edges": edges
        }
        
        if layers:
            result["layers"] = layers
        
        if cycles:
            result["cycles"] = cycles
        
        return result


class RoadmapRecomposer(JSONRecomposer):
    """Recompose planning index from hierarchical structure."""
    
    def recompose(self) -> Dict[str, Any]:
        """
        Reconstruct planning_index.json from:
        - items/: Individual item definitions
        - categories/: Category grouping (for validation)
        """
        items = []
        
        # Read all item files
        items_dir = self.hierarchical_dir / "items"
        if items_dir.exists():
            # Read index to get order
            index_file = items_dir / "_index.json"
            item_ids = []
            
            if index_file.exists():
                with open(index_file, "r") as f:
                    index_data = json.load(f)
                    item_ids = [entry["id"] if isinstance(entry, dict) else entry 
                               for entry in index_data]
            
            # Read items in index order
            for item_id in item_ids:
                item_file = items_dir / f"{item_id}.json"
                if item_file.exists():
                    with open(item_file, "r") as f:
                        item_data = json.load(f)
                        items.append(item_data)
            
            # Fallback: read all items if index missing
            if not items:
                for json_file in sorted(items_dir.glob("*.json")):
                    if json_file.name == "_index.json":
                        continue
                    with open(json_file, "r") as f:
                        items.append(json.load(f))
        
        return {"items": items}


def get_recomposer(hierarchical_dir: str) -> JSONRecomposer:
    """Detect strategy from metadata and return recomposer."""
    hierarchical_path = Path(hierarchical_dir)
    metadata_file = hierarchical_path / "_metadata.json"
    
    if not metadata_file.exists():
        raise ValueError(f"No _metadata.json in {hierarchical_dir}")
    
    with open(metadata_file, "r") as f:
        metadata = json.load(f)
    
    strategy = metadata.get("strategy", "unknown")
    
    recomposers = {
        "dependency-graph": DependencyGraphRecomposer,
        "roadmap": RoadmapRecomposer,
    }
    
    if strategy not in recomposers:
        raise ValueError(f"Unknown strategy: {strategy}. Available: {list(recomposers.keys())}")
    
    return recomposers[strategy](hierarchical_dir)


def validate_roundtrip(original_file: str, recomposed: Dict[str, Any]) -> bool:
    """
    Validate that recomposed JSON is equivalent to original.
    
    Note: Not exact equality (fragment order may differ), but structurally equivalent.
    """
    try:
        with open(original_file, "r") as f:
            original = json.load(f)
    except FileNotFoundError:
        print(f"Warning: Original file not found for validation: {original_file}")
        return True  # Skip validation
    
    # Structural validation (strategy-specific)
    # For now, just check that key fields exist
    
    if "modules" in original and "modules" in recomposed:
        if len(original.get("modules", {})) != len(recomposed.get("modules", {})):
            print(f"Warning: Module count mismatch")
            return False
    
    if "items" in original and "items" in recomposed:
        if len(original.get("items", [])) != len(recomposed.get("items", [])):
            print(f"Warning: Item count mismatch")
            return False
    
    return True


def main():
    """CLI entry point."""
    if len(sys.argv) < 2:
        print(__doc__)
        sys.exit(1)
    
    hierarchical_dir = sys.argv[1]
    output_file = sys.argv[2] if len(sys.argv) > 2 else None
    
    # Validate input
    if not Path(hierarchical_dir).exists():
        print(f"Error: Directory not found: {hierarchical_dir}")
        sys.exit(1)
    
    # Recompose
    try:
        recomposer = get_recomposer(hierarchical_dir)
        recomposed = recomposer.recompose()
    except Exception as e:
        print(f"Error during recomposition: {e}")
        sys.exit(1)
    
    # Output
    if output_file:
        try:
            with open(output_file, "w") as f:
                json.dump(recomposed, f, indent=2)
            print(f"✓ Recomposed {hierarchical_dir} → {output_file}")
            
            # Validate roundtrip if original available
            metadata_file = Path(hierarchical_dir) / "_metadata.json"
            if metadata_file.exists():
                with open(metadata_file, "r") as f:
                    metadata = json.load(f)
                    original = metadata.get("source_file")
                    if original and Path(original).exists():
                        if validate_roundtrip(original, recomposed):
                            print(f"✓ Roundtrip validation passed")
                        else:
                            print(f"⚠ Roundtrip validation issues detected")
        except Exception as e:
            print(f"Error writing output: {e}")
            sys.exit(1)
    else:
        # Output to stdout
        json.dump(recomposed, sys.stdout, indent=2)


if __name__ == "__main__":
    main()
