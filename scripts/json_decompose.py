#!/usr/bin/env python3
"""
JSON Decomposition: Monolithic → Hierarchical

Converts large monolithic JSON files into hierarchical directory structures
with index manifests, enabling better diffs, parallelism, and queries.

Usage:
    python json_decompose.py <monolithic.json> <output_dir> --strategy <strategy>

Strategies:
    dependency-graph: Split dependency_graph.json into module hierarchy
    roadmap:         Split planning_index.json into item hierarchy
    enriched:        Split canonical_enriched.json into item+annotation hierarchy

Example:
    python json_decompose.py build/dependency_graph.json build/deps/ \\
        --strategy dependency-graph
"""

import json
import os
import sys
from pathlib import Path
from typing import Any, Dict, List, Optional
from dataclasses import dataclass, asdict
from datetime import datetime


@dataclass
class DecompositionMetadata:
    """Metadata written to all hierarchical outputs."""
    timestamp: str
    strategy: str
    source_file: str
    total_items: int
    fragment_count: int
    version: str = "1.0"


class JSONDecomposer:
    """Base class for JSON decomposition strategies."""
    
    def __init__(self, output_dir: str):
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(parents=True, exist_ok=True)
        self.metadata = None
    
    def decompose(self, data: Any, source_file: str, strategy: str) -> None:
        """Decompose monolithic JSON into hierarchical structure."""
        raise NotImplementedError
    
    def write_metadata(self, total_items: int, fragment_count: int) -> None:
        """Write metadata file."""
        self.metadata = DecompositionMetadata(
            timestamp=datetime.utcnow().isoformat(),
            strategy=self.strategy_name(),
            source_file=self.source_file,
            total_items=total_items,
            fragment_count=fragment_count
        )
        with open(self.output_dir / "_metadata.json", "w") as f:
            json.dump(asdict(self.metadata), f, indent=2)
    
    def strategy_name(self) -> str:
        """Return strategy name."""
        raise NotImplementedError
    
    def write_json(self, path: Path, data: Any) -> None:
        """Write JSON file with proper formatting."""
        path.parent.mkdir(parents=True, exist_ok=True)
        with open(path, "w") as f:
            json.dump(data, f, indent=2)


class DependencyGraphDecomposer(JSONDecomposer):
    """Decompose dependency_graph.json into module hierarchy."""
    
    def strategy_name(self) -> str:
        return "dependency-graph"
    
    def decompose(self, data: Dict[str, Any], source_file: str, strategy: str) -> None:
        """
        Decompose dependency graph into:
        - modules/: Hierarchical module structure
        - layers/: Stratified by dependency depth
        - cycles/: Circular dependencies (if any)
        """
        self.source_file = source_file
        
        # Handle both old format (modules + edges) and new format (nodes + metadata)
        if "nodes" in data:
            # New format: nodes with imports/imported_by
            modules = {}
            edges = []
            edge_set = set()
            
            for node in data.get("nodes", []):
                module_name = node.get("module")
                if not module_name:
                    continue
                
                modules[module_name] = True
                
                # Extract edges from imports
                for imported in node.get("imports", []):
                    edge = {"from": module_name, "to": imported}
                    edge_key = (module_name, imported)
                    if edge_key not in edge_set:
                        edges.append(edge)
                        edge_set.add(edge_key)
        else:
            # Old format: modules dict + edges list
            modules = data.get("modules", {})
            edges = data.get("edges", [])
        
        fragment_count = 0
        
        # Create modules directory structure
        modules_dir = self.output_dir / "modules"
        modules_dir.mkdir(parents=True, exist_ok=True)
        
        module_index = []
        for module_name in sorted(modules.keys()):
            module_index.append(module_name)
            
            # Create hierarchical path: Algebra/Groups/Abelian.json
            parts = module_name.split(".")
            module_path = modules_dir / "/".join(parts[:-1])
            module_path.mkdir(parents=True, exist_ok=True)
            
            # Write individual module file
            module_file = module_path / f"{parts[-1]}.json"
            module_data = {
                "name": module_name,
                "imports": [e["to"] for e in edges if e["from"] == module_name],
                "imported_by": [e["from"] for e in edges if e["to"] == module_name]
            }
            self.write_json(module_file, module_data)
            fragment_count += 1
        
        # Write modules index
        self.write_json(modules_dir / "_index.json", module_index)
        fragment_count += 1
        
        # Create layers (dependency depth stratification)
        layers = self._stratify_by_depth(modules, edges)
        layers_dir = self.output_dir / "layers"
        layers_dir.mkdir(parents=True, exist_ok=True)
        
        layer_index = []
        for layer_num in sorted(layers.keys()):
            layer_index.append(f"layer-{layer_num}")
            layer_file = layers_dir / f"layer-{layer_num}.json"
            self.write_json(layer_file, layers[layer_num])
            fragment_count += 1
        
        self.write_json(layers_dir / "_index.json", layer_index)
        fragment_count += 1
        
        # Detect and write cycles
        cycles = self._find_cycles(modules, edges)
        if cycles:
            cycles_dir = self.output_dir / "cycles"
            cycles_dir.mkdir(parents=True, exist_ok=True)
            
            cycle_index = []
            for cycle_num, cycle in enumerate(cycles):
                cycle_index.append(f"cycle-{cycle_num:03d}")
                cycle_file = cycles_dir / f"cycle-{cycle_num:03d}.json"
                self.write_json(cycle_file, {"modules": cycle})
                fragment_count += 1
            
            self.write_json(cycles_dir / "_index.json", cycle_index)
            fragment_count += 1
        
        # Write metadata
        self.write_metadata(len(modules), fragment_count)
    
    def _stratify_by_depth(self, modules: Dict, edges: List) -> Dict[int, List]:
        """Stratify modules by dependency depth."""
        # Calculate depth for each module
        depths = {}
        changed = True
        while changed:
            changed = False
            for module in modules.keys():
                if module not in depths:
                    # Check if all dependencies have depth assigned
                    deps = [e["to"] for e in edges if e["from"] == module]
                    if not deps:
                        depths[module] = 0
                        changed = True
                    elif all(d in depths for d in deps):
                        depths[module] = max((depths[d] for d in deps), default=0) + 1
                        changed = True
        
        # Group by depth
        layers = {}
        for module, depth in depths.items():
            if depth not in layers:
                layers[depth] = []
            layers[depth].append(module)
        
        return layers
    
    def _find_cycles(self, modules: Dict, edges: List) -> List[List[str]]:
        """Find circular dependencies (simplified detection)."""
        # Build adjacency list (only for modules that exist)
        adj = {m: [] for m in modules}
        for edge in edges:
            if edge["from"] in adj:  # Only add if source exists
                adj[edge["from"]].append(edge["to"])
        
        cycles = []
        visited = set()
        rec_stack = set()
        
        def dfs(node, path):
            if node not in adj:  # Skip if node doesn't exist in modules
                return
            
            visited.add(node)
            rec_stack.add(node)
            path.append(node)
            
            for neighbor in adj.get(node, []):
                if neighbor not in adj:  # Skip external modules
                    continue
                if neighbor not in visited:
                    dfs(neighbor, path.copy())
                elif neighbor in rec_stack:
                    # Found cycle
                    try:
                        cycle_start = path.index(neighbor)
                        cycle = path[cycle_start:] + [neighbor]
                        if cycle not in cycles:  # Avoid duplicates
                            cycles.append(cycle)
                    except ValueError:
                        pass
            
            rec_stack.discard(node)
        
        for module in modules:
            if module not in visited:
                dfs(module, [])
        
        return cycles


class RoadmapDecomposer(JSONDecomposer):
    """Decompose planning_index.json into item hierarchy."""
    
    def strategy_name(self) -> str:
        return "roadmap"
    
    def decompose(self, data: Dict[str, Any], source_file: str, strategy: str) -> None:
        """
        Decompose planning index into:
        - items/: Individual roadmap items
        - categories/: Items grouped by category
        - sources/: Provenance information
        """
        self.source_file = source_file
        
        items = data.get("items", [])
        fragment_count = 0
        
        # Create items directory
        items_dir = self.output_dir / "items"
        items_dir.mkdir(parents=True, exist_ok=True)
        
        item_index = []
        categories = {}
        
        for idx, item in enumerate(items):
            item_id = item.get("id", f"item-{idx:04d}")
            
            # Create simple item index entry
            index_entry = {
                "id": item_id,
                "title": item.get("title", ""),
                "status": item.get("status", "not-started"),
                "category": item.get("category", "")
            }
            item_index.append(index_entry)
            
            # Write full item file
            item_file = items_dir / f"{item_id}.json"
            self.write_json(item_file, item)
            fragment_count += 1
            
            # Track categories
            category = item.get("category", "uncategorized")
            if category not in categories:
                categories[category] = []
            categories[category].append(item_id)
        
        # Write items index
        self.write_json(items_dir / "_index.json", item_index)
        fragment_count += 1
        
        # Create categories directory
        if categories:
            categories_dir = self.output_dir / "categories"
            categories_dir.mkdir(parents=True, exist_ok=True)
            
            for category, item_ids in sorted(categories.items()):
                safe_name = category.lower().replace(" ", "_").replace("/", "_")
                cat_file = categories_dir / f"{safe_name}.json"
                self.write_json(cat_file, {"category": category, "items": item_ids})
                fragment_count += 1
            
            self.write_json(categories_dir / "_index.json", list(categories.keys()))
            fragment_count += 1
        
        # Write metadata
        self.write_metadata(len(items), fragment_count)


def get_decomposer(strategy: str) -> JSONDecomposer.__class__:
    """Get decomposer class for strategy."""
    decomposers = {
        "dependency-graph": DependencyGraphDecomposer,
        "roadmap": RoadmapDecomposer,
        # "enriched": EnrichedDecomposer,  # Future
    }
    
    if strategy not in decomposers:
        raise ValueError(f"Unknown strategy: {strategy}. Available: {list(decomposers.keys())}")
    
    return decomposers[strategy]


def main():
    """CLI entry point."""
    if len(sys.argv) < 3:
        print(__doc__)
        sys.exit(1)
    
    monolithic_file = sys.argv[1]
    output_dir = sys.argv[2]
    
    # Parse strategy
    strategy = "dependency-graph"  # default
    if "--strategy" in sys.argv:
        strategy = sys.argv[sys.argv.index("--strategy") + 1]
    
    # Load JSON
    try:
        with open(monolithic_file, "r") as f:
            data = json.load(f)
    except FileNotFoundError:
        print(f"Error: File not found: {monolithic_file}")
        sys.exit(1)
    except json.JSONDecodeError as e:
        print(f"Error: Invalid JSON: {e}")
        sys.exit(1)
    
    # Decompose
    decomposer_class = get_decomposer(strategy)
    decomposer = decomposer_class(output_dir)
    decomposer.decompose(data, monolithic_file, strategy)
    
    print(f"✓ Decomposed {monolithic_file} → {output_dir}/")
    if decomposer.metadata:
        print(f"  Fragments: {decomposer.metadata.fragment_count}")
        print(f"  Items: {decomposer.metadata.total_items}")


if __name__ == "__main__":
    main()
