#!/usr/bin/env python3
"""
Dependency Graph Builder - Phase 4 Cross-Reference Resolution
Constructs detailed dependency graphs for Agda modules based on:
- Import statements
- Module relationships
- Transitive dependencies
"""

import json
import re
from pathlib import Path
from typing import Dict, List, Set, Tuple
from dataclasses import dataclass, field
from collections import defaultdict, deque

@dataclass
class DependencyNode:
    """Represents a node in the dependency graph."""
    module_name: str
    imports: List[str] = field(default_factory=list)
    imported_by: List[str] = field(default_factory=list)
    depth: int = 0
    
    def __hash__(self):
        return hash(self.module_name)

class DependencyGraphBuilder:
    """Builds and analyzes module dependency graphs."""
    
    def __init__(self, workspace_root: str = "/home/mikemol/github/metacatagory"):
        self.workspace_root = workspace_root
        self.nodes: Dict[str, DependencyNode] = {}
        self.graph: Dict[str, Set[str]] = defaultdict(set)
        self.reverse_graph: Dict[str, Set[str]] = defaultdict(set)
        
    def load_module_mappings(
        self,
        mapping_file: str = "build/module_mappings.json"
    ) -> None:
        """Load module mappings from Phase 4 ModuleMatcher."""
        print("📥 Loading module mappings...")
        
        mapping_path = Path(self.workspace_root) / mapping_file
        with open(mapping_path, 'r') as f:
            data = json.load(f)
        
        module_index = data['module_index']
        
        # Build dependency graph from imports
        for qualified_name, module_data in module_index.items():
            imports = module_data.get('imports', [])
            
            # Create node if doesn't exist
            if qualified_name not in self.nodes:
                self.nodes[qualified_name] = DependencyNode(module_name=qualified_name)
            
            # Store imports
            self.nodes[qualified_name].imports = imports
            
            # Build forward graph
            self.graph[qualified_name].update(imports)
            
            # Build reverse graph (who imports this module)
            for imported in imports:
                self.reverse_graph[imported].add(qualified_name)
                
                # Ensure imported module exists in nodes
                if imported not in self.nodes:
                    self.nodes[imported] = DependencyNode(module_name=imported)
        
        # Update imported_by lists
        for module_name, node in self.nodes.items():
            node.imported_by = list(self.reverse_graph[module_name])
        
        print(f"   ✓ Loaded {len(self.nodes)} modules")
        print(f"   ✓ Total dependencies: {sum(len(deps) for deps in self.graph.values())}")
    
    def compute_depths(self) -> None:
        """Compute depth of each module in dependency tree."""
        print("\n📊 Computing module depths...")
        
        # Find root nodes (no imports or only import Agda.Builtin)
        roots = []
        for module_name, node in self.nodes.items():
            if not node.imports:
                roots.append(module_name)
        
        # BFS to compute depths
        visited = set()
        queue = deque([(root, 0) for root in roots])
        
        while queue:
            module_name, depth = queue.popleft()
            
            if module_name in visited:
                continue
            
            visited.add(module_name)
            
            if module_name in self.nodes:
                self.nodes[module_name].depth = depth
                
                # Add importers to queue
                for importer in self.reverse_graph.get(module_name, []):
                    if importer not in visited:
                        queue.append((importer, depth + 1))
        
        print(f"   ✓ Computed depths for {len(visited)} modules")
    
    def find_strongly_connected_components(self) -> List[Set[str]]:
        """Find strongly connected components (circular dependencies)."""
        print("\n🔍 Detecting circular dependencies...")
        
        # Tarjan's algorithm for SCC
        index_counter = [0]
        stack = []
        lowlinks = {}
        index = {}
        on_stack = defaultdict(bool)
        sccs = []
        
        def strongconnect(node):
            index[node] = index_counter[0]
            lowlinks[node] = index_counter[0]
            index_counter[0] += 1
            stack.append(node)
            on_stack[node] = True
            
            # Consider successors
            for successor in self.graph.get(node, []):
                if successor not in index:
                    strongconnect(successor)
                    lowlinks[node] = min(lowlinks[node], lowlinks[successor])
                elif on_stack[successor]:
                    lowlinks[node] = min(lowlinks[node], index[successor])
            
            # If node is a root, pop the stack
            if lowlinks[node] == index[node]:
                scc = set()
                while True:
                    w = stack.pop()
                    on_stack[w] = False
                    scc.add(w)
                    if w == node:
                        break
                sccs.append(scc)
        
        for node in self.nodes:
            if node not in index:
                strongconnect(node)
        
        # Filter to only cycles (SCC with > 1 node)
        cycles = [scc for scc in sccs if len(scc) > 1]
        
        print(f"   ✓ Found {len(cycles)} circular dependency groups")
        
        return cycles
    
    def find_transitive_dependencies(
        self,
        module_name: str,
        max_depth: int = None
    ) -> Set[str]:
        """Find all transitive dependencies of a module."""
        if module_name not in self.nodes:
            return set()
        
        dependencies = set()
        visited = set()
        queue = deque([(module_name, 0)])
        
        while queue:
            current, depth = queue.popleft()
            
            if current in visited:
                continue
            
            if max_depth is not None and depth > max_depth:
                continue
            
            visited.add(current)
            
            # Add direct dependencies
            for dep in self.graph.get(current, []):
                dependencies.add(dep)
                if dep not in visited:
                    queue.append((dep, depth + 1))
        
        return dependencies
    
    def find_reverse_dependencies(
        self,
        module_name: str,
        max_depth: int = None
    ) -> Set[str]:
        """Find all modules that depend on this module (reverse dependencies)."""
        if module_name not in self.nodes:
            return set()
        
        dependents = set()
        visited = set()
        queue = deque([(module_name, 0)])
        
        while queue:
            current, depth = queue.popleft()
            
            if current in visited:
                continue
            
            if max_depth is not None and depth > max_depth:
                continue
            
            visited.add(current)
            
            # Add reverse dependencies
            for dep in self.reverse_graph.get(current, []):
                dependents.add(dep)
                if dep not in visited:
                    queue.append((dep, depth + 1))
        
        return dependents
    
    def find_critical_path(self) -> List[str]:
        """Find the critical path (longest dependency chain)."""
        print("\n🎯 Finding critical path...")
        
        # Topological sort with path tracking
        in_degree = {node: len(self.graph.get(node, [])) for node in self.nodes}
        longest_path = {node: 0 for node in self.nodes}
        predecessors = {node: None for node in self.nodes}
        
        # Find nodes with no dependencies
        queue = deque([node for node, degree in in_degree.items() if degree == 0])
        
        while queue:
            current = queue.popleft()
            
            # Update dependents
            for dependent in self.reverse_graph.get(current, []):
                # Update longest path
                if longest_path[current] + 1 > longest_path[dependent]:
                    longest_path[dependent] = longest_path[current] + 1
                    predecessors[dependent] = current
                
                # Decrease in-degree
                in_degree[dependent] -= 1
                if in_degree[dependent] == 0:
                    queue.append(dependent)
        
        # Find the node with longest path
        max_node = max(longest_path, key=longest_path.get)
        path_length = longest_path[max_node]
        
        # Reconstruct path
        path = []
        current = max_node
        while current is not None:
            path.append(current)
            current = predecessors[current]
        
        path.reverse()
        
        print(f"   ✓ Critical path length: {path_length}")
        
        return path
    
    def get_dependency_layers(self) -> List[List[str]]:
        """Get modules organized in dependency layers (topological ordering)."""
        print("\n📐 Computing dependency layers...")
        
        # Topological sort with layer tracking
        in_degree = {node: len(self.graph.get(node, [])) for node in self.nodes}
        layers = []
        
        while any(degree == 0 for degree in in_degree.values()):
            # Find all nodes with no dependencies
            current_layer = [
                node for node, degree in in_degree.items() 
                if degree == 0
            ]
            
            if not current_layer:
                break
            
            layers.append(current_layer)
            
            # Remove processed nodes
            for node in current_layer:
                in_degree[node] = -1  # Mark as processed
                
                # Decrease in-degree of dependents
                for dependent in self.reverse_graph.get(node, []):
                    if in_degree[dependent] > 0:
                        in_degree[dependent] -= 1
        
        print(f"   ✓ Found {len(layers)} dependency layers")
        
        return layers
    
    def generate_report(
        self,
        output_file: str = "build/dependency_graph.json"
    ) -> None:
        """Generate dependency graph report."""
        print(f"\n📄 Generating dependency graph report...")
        
        # Compute additional metrics
        cycles = self.find_strongly_connected_components()
        critical_path = self.find_critical_path()
        layers = self.get_dependency_layers()
        
        # Build report
        report = {
            'metadata': {
                'total_modules': len(self.nodes),
                'total_dependencies': sum(len(deps) for deps in self.graph.values()),
                'cycles_detected': len(cycles),
                'critical_path_length': len(critical_path),
                'dependency_layers': len(layers),
                'timestamp': self._get_timestamp()
            },
            'nodes': [
                {
                    'module': node.module_name,
                    'imports': node.imports,
                    'imported_by': node.imported_by,
                    'depth': node.depth,
                    'direct_deps': len(node.imports),
                    'reverse_deps': len(node.imported_by)
                }
                for node in sorted(self.nodes.values(), 
                                  key=lambda n: n.depth, 
                                  reverse=True)
            ],
            'cycles': [
                {
                    'id': i,
                    'modules': list(cycle),
                    'size': len(cycle)
                }
                for i, cycle in enumerate(cycles)
            ],
            'critical_path': critical_path,
            'layers': [
                {
                    'layer_id': i,
                    'modules': layer,
                    'count': len(layer)
                }
                for i, layer in enumerate(layers)
            ]
        }
        
        # Write report
        output_path = Path(self.workspace_root) / output_file
        output_path.parent.mkdir(parents=True, exist_ok=True)
        
        with open(output_path, 'w') as f:
            json.dump(report, f, indent=2)
        
        print(f"   ✓ Report saved to: {output_file}")
        
        # Print summary
        self._print_summary(report)
    
    def _get_timestamp(self) -> str:
        """Get current timestamp."""
        from datetime import datetime
        return datetime.now().isoformat()
    
    def _print_summary(self, report: Dict) -> None:
        """Print summary statistics."""
        print("\n" + "="*70)
        print("DEPENDENCY GRAPH SUMMARY")
        print("="*70)
        
        meta = report['metadata']
        
        print(f"\nGraph Statistics:")
        print(f"  Total Modules: {meta['total_modules']}")
        print(f"  Total Dependencies: {meta['total_dependencies']}")
        print(f"  Avg Dependencies/Module: {meta['total_dependencies']/meta['total_modules']:.1f}")
        
        print(f"\nStructure:")
        print(f"  Dependency Layers: {meta['dependency_layers']}")
        print(f"  Critical Path Length: {meta['critical_path_length']}")
        print(f"  Circular Dependencies: {meta['cycles_detected']}")
        
        # Top modules by reverse dependencies
        nodes_by_reverse_deps = sorted(
            report['nodes'],
            key=lambda n: n['reverse_deps'],
            reverse=True
        )[:10]
        
        print(f"\nMost Depended-Upon Modules:")
        for node in nodes_by_reverse_deps:
            print(f"  {node['module']}: {node['reverse_deps']} importers")
        
        print("\n" + "="*70)

def main():
    """Main entry point."""
    print("="*70)
    print("PHASE 4: DEPENDENCY GRAPH BUILDER")
    print("="*70)
    
    builder = DependencyGraphBuilder()
    
    # Load module mappings
    builder.load_module_mappings()
    
    # Compute depths
    builder.compute_depths()
    
    # Generate report
    builder.generate_report()
    
    print("\n✓ Dependency graph analysis complete!")

if __name__ == '__main__':
    main()
