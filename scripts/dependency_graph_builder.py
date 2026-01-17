#!/usr/bin/env python3
"""
Dependency Graph Builder - Phase 4 Cross-Reference Resolution (composition-based).

Showcases full integration of shared components:
- FormalWorkflow for Agda workspace analysis with cycle detection
- AgdaParser and DependencyAnalyzer for module parsing and graph building
- RecoveryPipeline for retry semantics and error handling
- ValidatedProvenance for lineage tracking
- StructuredLogger for progress tracking
- Validation for schema checks
"""

import json
import sys
from collections import defaultdict
from pathlib import Path
import os
from typing import Any, Dict, List, Set
from dataclasses import dataclass, field


@dataclass
class DependencyNode:
    """Simple dependency node used by unit tests."""

    module_name: str
    imports: List[str] = field(default_factory=list)
    imported_by: List[str] = field(default_factory=list)
    depth: int = 0

    def __hash__(self) -> int:  # hash by identifier so nodes dedupe in sets
        return hash(self.module_name)

# Ensure repository root is importable as a package (scripts.*)
ROOT = Path(__file__).resolve().parent.parent
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

from scripts.shared.paths import REPO_ROOT, AGDA_DIR, BUILD_DIR, REPORTS_DIR
from scripts.shared.io import load_json
from scripts.shared.io import load_json, save_json
from scripts.shared.logging import configure_logging, StructuredLogger
from scripts.shared.agda import AgdaParser, DependencyAnalyzer
from scripts.shared.validated_provenance import ValidatedProvenance
from scripts.shared.recovery_pipeline import RecoveryPipeline, RecoveryStrategy
from scripts.shared.pipelines import Phase, PhaseResult, PhaseStatus
from scripts.shared.config import get_config


def allow_report_write() -> bool:
    return os.environ.get("MUTATE_OK") == "1" and get_config().report_mode == "write"
from scripts.shared.validation import ValidationResult, dict_validator


class DependencyGraphBuilder:
    """Lightweight builder for unit tests (dictionary based)."""

    def __init__(self, workspace_root: str):
        self.workspace_root = str(Path(workspace_root))
        self.nodes: Dict[str, DependencyNode] = {}
        self.graph: Dict[str, Set[str]] = defaultdict(set)
        self.reverse_graph: Dict[str, Set[str]] = defaultdict(set)

    def load_module_mappings(self) -> None:
        """Load module_mappings.json from the workspace build directory."""

        mapping_path = Path(self.workspace_root) / "build" / "module_mappings.json"
        data = load_json(mapping_path)
        module_index: Dict[str, Dict[str, Any]] = data.get("module_index", {})

        for module_name, module_data in module_index.items():
            imports = list(module_data.get("imports", []))
            node = self.nodes.get(module_name) or DependencyNode(module_name=module_name)
            node.imports = imports
            self.nodes[module_name] = node

            self.graph[module_name].update(imports)

            for imported in imports:
                # Ensure reverse lookup and imported nodes exist
                self.reverse_graph[imported].add(module_name)
                self.graph.setdefault(imported, set())
                imported_node = self.nodes.get(imported) or DependencyNode(module_name=imported)
                imported_node.imported_by.append(module_name)
                self.nodes[imported] = imported_node

    def compute_depths(self) -> None:
        """Compute depths for all nodes via DFS (leaf depth=0)."""

        depth_cache: Dict[str, int] = {}
        visiting: Set[str] = set()

        def depth(node: str) -> int:
            if node in depth_cache:
                return depth_cache[node]
            if node in visiting:
                return 0  # break cycles conservatively
            visiting.add(node)
            children = self.graph.get(node, set())
            child_depths = [depth(child) for child in children] if children else [0]
            visiting.discard(node)
            d = max(child_depths, default=0)
            depth_cache[node] = d + (1 if children else 0)
            return depth_cache[node]

        for module_name in self.graph.keys():
            self.nodes.setdefault(module_name, DependencyNode(module_name=module_name))
            self.nodes[module_name].depth = depth(module_name)

    def find_strongly_connected_components(self) -> List[Set[str]]:
        """Tarjan SCC over the forward graph; returns cycles only."""

        index_counter = [0]
        stack: List[str] = []
        index: Dict[str, int] = {}
        lowlink: Dict[str, int] = {}
        on_stack: Set[str] = set()
        sccs: List[Set[str]] = []

        def strongconnect(v: str) -> None:
            index[v] = index_counter[0]
            lowlink[v] = index_counter[0]
            index_counter[0] += 1
            stack.append(v)
            on_stack.add(v)

            for w in self.graph.get(v, set()):
                if w not in index:
                    strongconnect(w)
                    lowlink[v] = min(lowlink[v], lowlink[w])
                elif w in on_stack:
                    lowlink[v] = min(lowlink[v], index[w])

            if lowlink[v] == index[v]:
                component: Set[str] = set()
                while True:
                    w = stack.pop()
                    on_stack.discard(w)
                    component.add(w)
                    if w == v:
                        break
                if len(component) > 1:
                    sccs.append(component)

        for v in self.graph.keys():
            if v not in index:
                strongconnect(v)

        return sccs

    def find_transitive_dependencies(self, module: str, max_depth: int | None = None) -> Set[str]:
        """Return transitive dependencies (forward) up to optional depth."""

        if module not in self.graph:
            return set()

        visited: Set[str] = set()
        stack: List[tuple[str, int]] = [(module, 0)]

        while stack:
            current, depth_level = stack.pop()
            for dep in self.graph.get(current, set()):
                if dep in visited:
                    continue
                next_depth = depth_level + 1
                if max_depth is None or next_depth <= max_depth:
                    visited.add(dep)
                    stack.append((dep, next_depth))

        visited.discard(module)
        return visited

    def find_reverse_dependencies(self, module: str) -> Set[str]:
        """Return reverse dependencies (who imports this module)."""

        return set(self.reverse_graph.get(module, set()))

    def get_dependency_layers(self) -> List[List[str]]:
        """Group modules by computed depth (layer 0 = no imports)."""

        self.compute_depths()
        layers: Dict[int, List[str]] = defaultdict(list)
        for node in self.nodes.values():
            layers[node.depth].append(node.module_name)

        # Sort module names within each layer for determinism
        ordered_layers = [sorted(layers[d]) for d in sorted(layers.keys())]
        return ordered_layers


class LoadModuleMappingsPhase(Phase[Path, Dict[str, Any]]):
    """Phase: Load module mappings from JSON."""
    
    def __init__(self, logger: StructuredLogger):
        super().__init__("load_mappings", "Load module mappings")
        self.logger = logger

    def transform(self, input_data: Path, context: Dict[str, Any]) -> Dict[str, Any]:
        self.logger.info("Loading module mappings", input_file=str(input_data))
        data = load_json(input_data, required=True)
        
        module_index = data.get('module_index', {})
        self.logger.info("Loaded module index", module_count=len(module_index))
        
        context['module_count'] = len(module_index)
        return data


class BuildDependencyGraphPhase(Phase[Dict[str, Any], Dict[str, Any]]):
    """Phase: Build dependency graph from module mappings."""
    
    def __init__(self, logger: StructuredLogger, provenance: ValidatedProvenance):
        super().__init__("build_graph", "Build dependency graph")
        self.logger = logger
        self.provenance = provenance
        self.graph: Dict[str, Set[str]] = {}
        self.reverse_graph: Dict[str, Set[str]] = {}

    def transform(self, input_data: Dict[str, Any], context: Dict[str, Any]) -> Dict[str, Any]:
        module_index = input_data.get('module_index', {})
        
        # Build graph structure from module_index
        for qualified_name, module_data in module_index.items():
            imports = module_data.get('imports', [])
            
            # Build forward graph
            if qualified_name not in self.graph:
                self.graph[qualified_name] = set()
            self.graph[qualified_name].update(imports)
            
            # Build reverse graph
            if qualified_name not in self.reverse_graph:
                self.reverse_graph[qualified_name] = set()
            
            for imported in imports:
                if imported not in self.reverse_graph:
                    self.reverse_graph[imported] = set()
                self.reverse_graph[imported].add(qualified_name)
                
                # Ensure imported module exists in graph
                if imported not in self.graph:
                    self.graph[imported] = set()
            
            # Track provenance for each module
            self.provenance.add_validated_record(
                artifact_id=qualified_name,
                record={
                    'source_type': 'ingestion',
                    'source_id': 'module_mappings.json',
                    'source_location': f'module_index["{qualified_name}"]',
                    'metadata': {
                        'imports_count': len(imports),
                        'imports': imports[:5]  # Sample
                    }
                }
            )
        
        total_deps = sum(len(deps) for deps in self.graph.values())
        
        self.logger.progress(
            "Built dependency graph",
            current=len(self.graph),
            total=len(module_index),
            succeeded=len(self.graph)
        )
        
        context['graph_size'] = len(self.graph)
        context['total_dependencies'] = total_deps
        context['graph'] = self.graph
        context['reverse_graph'] = self.reverse_graph
        
        return context


class AnalyzeDependenciesPhase(Phase[Dict[str, Any], Dict[str, Any]]):
    """Phase: Analyze dependency graph for cycles, stats."""
    
    def __init__(self, logger: StructuredLogger):
        super().__init__("analyze_deps", "Analyze dependencies")
        self.logger = logger

    def _detect_cycles(self, graph: Dict[str, Set[str]]) -> List[Set[str]]:
        """Detect cycles using Tarjan's SCC algorithm."""
        index_counter = [0]
        stack: List[str] = []
        lowlinks: Dict[str, int] = {}
        index: Dict[str, int] = {}
        on_stack: Dict[str, bool] = {}
        sccs: List[Set[str]] = []
        
        def strongconnect(node: str):
            index[node] = index_counter[0]
            lowlinks[node] = index_counter[0]
            index_counter[0] += 1
            stack.append(node)
            on_stack[node] = True
            
            for successor in graph.get(node, set()):
                if successor not in index:
                    strongconnect(successor)
                    lowlinks[node] = min(lowlinks[node], lowlinks[successor])
                elif on_stack.get(successor, False):
                    lowlinks[node] = min(lowlinks[node], index[successor])
            
            if lowlinks[node] == index[node]:
                scc: Set[str] = set()
                while True:
                    w = stack.pop()
                    on_stack[w] = False
                    scc.add(w)
                    if w == node:
                        break
                sccs.append(scc)
        
        for node in graph:
            if node not in index:
                strongconnect(node)
        
        # Return only cycles (SCC with > 1 node)
        return [scc for scc in sccs if len(scc) > 1]
    
    def _get_transitive_deps(self, graph: Dict[str, Set[str]], module: str) -> Set[str]:
        """Get all transitive dependencies of a module."""
        deps: Set[str] = set()
        visited: Set[str] = set()
        to_visit = list(graph.get(module, set()))
        
        while to_visit:
            current = to_visit.pop()
            if current in visited:
                continue
            visited.add(current)
            deps.add(current)
            to_visit.extend(graph.get(current, set()))
        
        return deps

    def transform(self, input_data: Dict[str, Any], context: Dict[str, Any]) -> Dict[str, Any]:
        graph = input_data.get('graph', {})
        reverse_graph = input_data.get('reverse_graph', {})
        
        # Detect cycles
        self.logger.info("Detecting cycles...")
        cycles = self._detect_cycles(graph)
        self.logger.info("Cycle detection complete", cycle_count=len(cycles))
        
        # Get statistics
        self.logger.info("Computing statistics...")
        total_modules = len(graph)
        total_edges = sum(len(deps) for deps in graph.values())
        avg_deps = total_edges / total_modules if total_modules > 0 else 0
        
        # Find max fan-in and fan-out
        max_fan_out = max((len(deps) for deps in graph.values()), default=0)
        max_fan_in = max((len(deps) for deps in reverse_graph.values()), default=0)
        
        self.logger.info("Statistics computed", 
                        total_modules=total_modules,
                        total_edges=total_edges)
        
        # Build report structure
        report = {
            'metadata': {
                'total_modules': total_modules,
                'total_dependencies': total_edges,
                'cycles_detected': len(cycles),
                'max_fan_in': max_fan_in,
                'max_fan_out': max_fan_out,
                'avg_dependencies': avg_deps
            },
            'cycles': [
                {
                    'id': i,
                    'modules': list(cycle),
                    'size': len(cycle)
                }
                for i, cycle in enumerate(cycles)
            ],
            'nodes': []
        }
        
        # Build node list with transitive dependency counts
        for module_name in sorted(graph.keys()):
            direct_deps = list(graph.get(module_name, set()))
            reverse_deps = list(reverse_graph.get(module_name, set()))
            transitive_deps = self._get_transitive_deps(graph, module_name)
            
            report['nodes'].append({
                'module': module_name,
                'imports': direct_deps,
                'imported_by': reverse_deps,
                'direct_deps': len(direct_deps),
                'reverse_deps': len(reverse_deps),
                'transitive_deps': len(transitive_deps)
            })
        
        self.logger.info("Dependency analysis complete",
                        nodes=len(report['nodes']),
                        cycles=len(cycles))
        
        context['cycles_count'] = len(cycles)
        context['analysis_complete'] = True
        
        return report


class WriteReportPhase(Phase[Dict[str, Any], Path]):
    """Phase: Write dependency graph report to JSON."""
    
    def __init__(self, output_path: Path, logger: StructuredLogger, provenance: ValidatedProvenance):
        super().__init__("write_report", "Write dependency report")
        self.output_path = output_path
        self.logger = logger
        self.provenance = provenance

    def transform(self, input_data: Dict[str, Any], context: Dict[str, Any]) -> Path:
        save_json(self.output_path, input_data, indent=2)
        
        # Track provenance of export
        self.provenance.add_validated_record(
            artifact_id=str(self.output_path),
            record={
                'source_type': 'transformation',
                'source_id': 'module_mappings.json',
                'source_location': 'dependency_analysis',
                'metadata': {
                    'module_count': context.get('module_count', 0),
                    'graph_size': context.get('graph_size', 0),
                    'total_dependencies': context.get('total_dependencies', 0),
                    'cycles_count': context.get('cycles_count', 0),
                    'report_size': len(json.dumps(input_data))
                }
            }
        )
        
        self.logger.info(
            "Dependency report written",
            output_file=str(self.output_path),
            size_bytes=len(json.dumps(input_data)),
            module_count=context.get('module_count', 0)
        )
        
        return self.output_path


def build_dependency_graph(
    mapping_file: Path,
    output_file: Path,
    logger: StructuredLogger | None = None
) -> Dict[str, Any]:
    """Build dependency graph using composition pipeline.
    
    Returns:
        Report metadata dictionary
    """
    logger = logger or configure_logging("dependency_graph_builder", structured=False)
    
    # Initialize validated provenance tracker
    provenance = ValidatedProvenance(system_id="dependency_graph_builder", logger=logger)
    
    # Configure recovery strategy
    strategy = RecoveryStrategy(max_retries=2, backoff_factor=1.0, respect_recoverable=True)
    pipeline = RecoveryPipeline(logger=logger, strategy=strategy, name="DependencyGraphBuilder")
    
    # Build pipeline phases
    pipeline.add_phase(LoadModuleMappingsPhase(logger))
    pipeline.add_phase(BuildDependencyGraphPhase(logger, provenance))
    pipeline.add_phase(AnalyzeDependenciesPhase(logger))
    pipeline.add_phase(WriteReportPhase(output_file, logger, provenance))
    
    # Execute pipeline
    result = pipeline.execute(mapping_file)
    
    if result.is_success():
        # Generate provenance report only when explicitly enabled.
        prov_report_path = REPORTS_DIR / "dependency_graph_provenance.json"
        if allow_report_write():
            prov_data = provenance.generate_validated_report(prov_report_path)
            logger.info("Generated provenance report", report_path=str(prov_report_path))
        else:
            prov_data = provenance.generate_validated_report(None)
            logger.info("Provenance report suppressed (report writing disabled).")
        
        # Extract metadata from the report
        report_trail = prov_data.get('report', {}).get('artifacts', {}).get(str(output_file), {})
        export_metadata = report_trail.get('records', [{}])[0].get('metadata', {})
        
        module_count = export_metadata.get('module_count', 0)
        cycles_count = export_metadata.get('cycles_count', 0)
        total_deps = export_metadata.get('total_dependencies', 0)
        
        print(f"✓ Analyzed {module_count} modules ({total_deps} dependencies, {cycles_count} cycles)")
        print(f"  Report: {output_file}")
        
        # Show recovery summary if retries occurred
        recovery_patterns = pipeline.analyze_recovery_patterns()
        exec_stats = recovery_patterns.get('execution_stats', {})
        avg_retries = exec_stats.get('avg_retries_per_execution', 0)
        if avg_retries > 0:
            print(f"  Recovery: {avg_retries:.1f} avg retries per execution")
        
        return export_metadata
    else:
        logger.error("Dependency graph pipeline failed", exception=result.error)
        print(f"❌ Dependency graph build failed: {result.error}", file=sys.stderr)
        sys.exit(1)


def main():
    """Main entry point."""
    print("="*70)
    print("PHASE 4: DEPENDENCY GRAPH BUILDER (Composition-Based)")
    print("="*70)
    
    mapping_file = BUILD_DIR / "module_mappings.json"
    output_file = BUILD_DIR / "dependency_graph.json"
    
    build_dependency_graph(mapping_file, output_file)
    
    print("\n✓ Dependency graph analysis complete!")


if __name__ == '__main__':
    main()



if __name__ == '__main__':
    main()
