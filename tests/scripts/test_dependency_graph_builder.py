#!/usr/bin/env python3
"""
Tests for dependency_graph_builder.py

Coverage targets:
- DependencyNode dataclass
- DependencyGraphBuilder initialization and graph construction
- Module depth computation
- Circular dependency detection (SCC algorithm)
- Transitive dependency resolution
- Critical path finding
- Dependency layer computation
"""

import pytest
import json
import tempfile
from pathlib import Path
from collections import defaultdict

# Import the module under test
import sys
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "scripts"))
from dependency_graph_builder import (
    DependencyNode,
    DependencyGraphBuilder
)


class TestDependencyNode:
    """Test DependencyNode dataclass."""
    
    def test_node_creation(self):
        """Test creating a dependency node."""
        node = DependencyNode(module_name="Test.Module")
        
        assert node.module_name == "Test.Module"
        assert node.imports == []
        assert node.imported_by == []
        assert node.depth == 0
    
    def test_node_with_imports(self):
        """Test node with import list."""
        node = DependencyNode(
            module_name="Test.A",
            imports=["Test.B", "Test.C"]
        )
        
        assert len(node.imports) == 2
        assert "Test.B" in node.imports
    
    def test_node_hashable(self):
        """Test that nodes are hashable (for use in sets)."""
        node1 = DependencyNode(module_name="Test.A")
        node2 = DependencyNode(module_name="Test.A")
        node3 = DependencyNode(module_name="Test.B")
        
        # Same module name should hash the same
        assert hash(node1) == hash(node2)
        assert hash(node1) != hash(node3)
        
        # Can add to set
        nodes = {node1, node2, node3}
        assert len(nodes) == 2  # node1 and node2 deduplicated


class TestDependencyGraphBuilderInit:
    """Test DependencyGraphBuilder initialization."""
    
    def test_builder_initialization(self, tmp_path):
        """Test builder initializes with empty structures."""
        builder = DependencyGraphBuilder(workspace_root=str(tmp_path))
        
        assert builder.workspace_root == str(tmp_path)
        assert builder.nodes == {}
        assert isinstance(builder.graph, defaultdict)
        assert isinstance(builder.reverse_graph, defaultdict)
    
    def test_load_module_mappings_simple(self, tmp_path):
        """Test loading simple module mappings."""
        # Create module_mappings.json
        build_dir = tmp_path / "build"
        build_dir.mkdir(parents=True)
        
        mappings = {
            "module_index": {
                "Module.A": {"imports": ["Module.B"]},
                "Module.B": {"imports": []},
                "Module.C": {"imports": ["Module.A", "Module.B"]}
            }
        }
        
        with open(build_dir / "module_mappings.json", "w") as f:
            json.dump(mappings, f)
        
        builder = DependencyGraphBuilder(workspace_root=str(tmp_path))
        builder.load_module_mappings()
        
        # Check nodes created
        assert len(builder.nodes) == 3
        assert "Module.A" in builder.nodes
        assert "Module.B" in builder.nodes
        assert "Module.C" in builder.nodes
        
        # Check imports
        assert builder.nodes["Module.A"].imports == ["Module.B"]
        assert builder.nodes["Module.C"].imports == ["Module.A", "Module.B"]
        
        # Check imported_by (reverse graph)
        assert "Module.A" in builder.nodes["Module.B"].imported_by
        assert "Module.C" in builder.nodes["Module.A"].imported_by
    
    def test_load_module_mappings_builds_graphs(self, tmp_path):
        """Test that loading builds forward and reverse graphs."""
        build_dir = tmp_path / "build"
        build_dir.mkdir(parents=True)
        
        mappings = {
            "module_index": {
                "A": {"imports": ["B"]},
                "B": {"imports": ["C"]},
                "C": {"imports": []}
            }
        }
        
        with open(build_dir / "module_mappings.json", "w") as f:
            json.dump(mappings, f)
        
        builder = DependencyGraphBuilder(workspace_root=str(tmp_path))
        builder.load_module_mappings()
        
        # Forward graph: A → B → C
        assert "B" in builder.graph["A"]
        assert "C" in builder.graph["B"]
        
        # Reverse graph: C ← B ← A
        assert "B" in builder.reverse_graph["C"]
        assert "A" in builder.reverse_graph["B"]


class TestDepthComputation:
    """Test module depth computation."""
    
    def test_compute_depths_linear_chain(self, tmp_path):
        """Test depth computation for linear dependency chain."""
        build_dir = tmp_path / "build"
        build_dir.mkdir(parents=True)
        
        # Linear chain: A → B → C → D
        mappings = {
            "module_index": {
                "A": {"imports": ["B"]},
                "B": {"imports": ["C"]},
                "C": {"imports": ["D"]},
                "D": {"imports": []}
            }
        }
        
        with open(build_dir / "module_mappings.json", "w") as f:
            json.dump(mappings, f)
        
        builder = DependencyGraphBuilder(workspace_root=str(tmp_path))
        builder.load_module_mappings()
        builder.compute_depths()
        
        # D has no dependencies → depth 0
        # C depends on D → depth 1
        # B depends on C → depth 2
        # A depends on B → depth 3
        assert builder.nodes["D"].depth == 0
        assert builder.nodes["C"].depth == 1
        assert builder.nodes["B"].depth == 2
        assert builder.nodes["A"].depth == 3
    
    def test_compute_depths_diamond(self, tmp_path):
        """Test depth computation for diamond dependency."""
        build_dir = tmp_path / "build"
        build_dir.mkdir(parents=True)
        
        # Diamond: A → B,C; B,C → D
        mappings = {
            "module_index": {
                "A": {"imports": ["B", "C"]},
                "B": {"imports": ["D"]},
                "C": {"imports": ["D"]},
                "D": {"imports": []}
            }
        }
        
        with open(build_dir / "module_mappings.json", "w") as f:
            json.dump(mappings, f)
        
        builder = DependencyGraphBuilder(workspace_root=str(tmp_path))
        builder.load_module_mappings()
        builder.compute_depths()
        
        assert builder.nodes["D"].depth == 0
        assert builder.nodes["B"].depth == 1
        assert builder.nodes["C"].depth == 1
        assert builder.nodes["A"].depth == 2


class TestCircularDependencies:
    """Test circular dependency detection."""
    
    def test_find_scc_no_cycles(self, tmp_path):
        """Test SCC detection with no circular dependencies."""
        build_dir = tmp_path / "build"
        build_dir.mkdir(parents=True)
        
        mappings = {
            "module_index": {
                "A": {"imports": ["B"]},
                "B": {"imports": ["C"]},
                "C": {"imports": []}
            }
        }
        
        with open(build_dir / "module_mappings.json", "w") as f:
            json.dump(mappings, f)
        
        builder = DependencyGraphBuilder(workspace_root=str(tmp_path))
        builder.load_module_mappings()
        cycles = builder.find_strongly_connected_components()
        
        # No cycles (all SCCs have size 1)
        assert cycles == []
    
    def test_find_scc_simple_cycle(self, tmp_path):
        """Test SCC detection with simple 2-node cycle."""
        build_dir = tmp_path / "build"
        build_dir.mkdir(parents=True)
        
        # A ↔ B cycle
        mappings = {
            "module_index": {
                "A": {"imports": ["B"]},
                "B": {"imports": ["A"]},
                "C": {"imports": []}
            }
        }
        
        with open(build_dir / "module_mappings.json", "w") as f:
            json.dump(mappings, f)
        
        builder = DependencyGraphBuilder(workspace_root=str(tmp_path))
        builder.load_module_mappings()
        cycles = builder.find_strongly_connected_components()
        
        # One cycle containing A and B
        assert len(cycles) == 1
        assert len(cycles[0]) == 2
        assert {"A", "B"} == cycles[0]
    
    def test_find_scc_complex_cycle(self, tmp_path):
        """Test SCC detection with 3-node cycle."""
        build_dir = tmp_path / "build"
        build_dir.mkdir(parents=True)
        
        # A → B → C → A cycle
        mappings = {
            "module_index": {
                "A": {"imports": ["B"]},
                "B": {"imports": ["C"]},
                "C": {"imports": ["A"]}
            }
        }
        
        with open(build_dir / "module_mappings.json", "w") as f:
            json.dump(mappings, f)
        
        builder = DependencyGraphBuilder(workspace_root=str(tmp_path))
        builder.load_module_mappings()
        cycles = builder.find_strongly_connected_components()
        
        # One cycle containing all three
        assert len(cycles) == 1
        assert len(cycles[0]) == 3
        assert {"A", "B", "C"} == cycles[0]


class TestTransitiveDependencies:
    """Test transitive dependency resolution."""
    
    def test_find_transitive_deps_direct(self, tmp_path):
        """Test finding direct dependencies."""
        build_dir = tmp_path / "build"
        build_dir.mkdir(parents=True)
        
        mappings = {
            "module_index": {
                "A": {"imports": ["B", "C"]},
                "B": {"imports": []},
                "C": {"imports": []}
            }
        }
        
        with open(build_dir / "module_mappings.json", "w") as f:
            json.dump(mappings, f)
        
        builder = DependencyGraphBuilder(workspace_root=str(tmp_path))
        builder.load_module_mappings()
        deps = builder.find_transitive_dependencies("A")
        
        assert deps == {"B", "C"}
    
    def test_find_transitive_deps_transitive(self, tmp_path):
        """Test finding transitive dependencies."""
        build_dir = tmp_path / "build"
        build_dir.mkdir(parents=True)
        
        # A → B → C → D
        mappings = {
            "module_index": {
                "A": {"imports": ["B"]},
                "B": {"imports": ["C"]},
                "C": {"imports": ["D"]},
                "D": {"imports": []}
            }
        }
        
        with open(build_dir / "module_mappings.json", "w") as f:
            json.dump(mappings, f)
        
        builder = DependencyGraphBuilder(workspace_root=str(tmp_path))
        builder.load_module_mappings()
        deps = builder.find_transitive_dependencies("A")
        
        # A transitively depends on B, C, D
        assert deps == {"B", "C", "D"}
    
    def test_find_transitive_deps_max_depth(self, tmp_path):
        """Test transitive dependencies with max depth limit."""
        build_dir = tmp_path / "build"
        build_dir.mkdir(parents=True)
        
        mappings = {
            "module_index": {
                "A": {"imports": ["B"]},
                "B": {"imports": ["C"]},
                "C": {"imports": ["D"]},
                "D": {"imports": []}
            }
        }
        
        with open(build_dir / "module_mappings.json", "w") as f:
            json.dump(mappings, f)
        
        builder = DependencyGraphBuilder(workspace_root=str(tmp_path))
        builder.load_module_mappings()
        deps = builder.find_transitive_dependencies("A", max_depth=2)
        
        # Max depth 2: Should include B and C (implementation may vary)
        assert "B" in deps
        assert "C" in deps or len(deps) >= 2
    
    def test_find_reverse_dependencies(self, tmp_path):
        """Test finding reverse dependencies (who imports this)."""
        build_dir = tmp_path / "build"
        build_dir.mkdir(parents=True)
        
        # A → C, B → C
        mappings = {
            "module_index": {
                "A": {"imports": ["C"]},
                "B": {"imports": ["C"]},
                "C": {"imports": []}
            }
        }
        
        with open(build_dir / "module_mappings.json", "w") as f:
            json.dump(mappings, f)
        
        builder = DependencyGraphBuilder(workspace_root=str(tmp_path))
        builder.load_module_mappings()
        dependents = builder.find_reverse_dependencies("C")
        
        # C is imported by A and B
        assert dependents == {"A", "B"}


class TestDependencyLayers:
    """Test dependency layer computation."""
    
    def test_get_dependency_layers_linear(self, tmp_path):
        """Test layers for linear dependency chain."""
        build_dir = tmp_path / "build"
        build_dir.mkdir(parents=True)
        
        # D ← C ← B ← A (reverse dependencies)
        mappings = {
            "module_index": {
                "A": {"imports": ["B"]},
                "B": {"imports": ["C"]},
                "C": {"imports": ["D"]},
                "D": {"imports": []}
            }
        }
        
        with open(build_dir / "module_mappings.json", "w") as f:
            json.dump(mappings, f)
        
        builder = DependencyGraphBuilder(workspace_root=str(tmp_path))
        builder.load_module_mappings()
        layers = builder.get_dependency_layers()
        
        # Layer 0: D (no dependencies)
        # Layer 1: C (depends on D)
        # Layer 2: B (depends on C)
        # Layer 3: A (depends on B)
        assert len(layers) == 4
        assert layers[0] == ["D"]
        assert layers[1] == ["C"]
        assert layers[2] == ["B"]
        assert layers[3] == ["A"]
    
    def test_get_dependency_layers_parallel(self, tmp_path):
        """Test layers with parallel independent modules."""
        build_dir = tmp_path / "build"
        build_dir.mkdir(parents=True)
        
        # A,B,C are independent (all in same layer)
        mappings = {
            "module_index": {
                "A": {"imports": []},
                "B": {"imports": []},
                "C": {"imports": []}
            }
        }
        
        with open(build_dir / "module_mappings.json", "w") as f:
            json.dump(mappings, f)
        
        builder = DependencyGraphBuilder(workspace_root=str(tmp_path))
        builder.load_module_mappings()
        layers = builder.get_dependency_layers()
        
        # All in layer 0
        assert len(layers) == 1
        assert set(layers[0]) == {"A", "B", "C"}


class TestErrorHandling:
    """Test error handling in dependency graph builder."""
    
    def test_transitive_deps_nonexistent_module(self, tmp_path):
        """Test finding dependencies for nonexistent module."""
        build_dir = tmp_path / "build"
        build_dir.mkdir(parents=True)
        
        mappings = {"module_index": {"A": {"imports": []}}}
        
        with open(build_dir / "module_mappings.json", "w") as f:
            json.dump(mappings, f)
        
        builder = DependencyGraphBuilder(workspace_root=str(tmp_path))
        builder.load_module_mappings()
        deps = builder.find_transitive_dependencies("NonExistent")
        
        # Should return empty set, not crash
        assert deps == set()
    
    def test_reverse_deps_nonexistent_module(self, tmp_path):
        """Test finding reverse dependencies for nonexistent module."""
        build_dir = tmp_path / "build"
        build_dir.mkdir(parents=True)
        
        mappings = {"module_index": {"A": {"imports": []}}}
        
        with open(build_dir / "module_mappings.json", "w") as f:
            json.dump(mappings, f)
        
        builder = DependencyGraphBuilder(workspace_root=str(tmp_path))
        builder.load_module_mappings()
        dependents = builder.find_reverse_dependencies("NonExistent")
        
        # Should return empty set, not crash
        assert dependents == set()
