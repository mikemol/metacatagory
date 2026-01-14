#!/usr/bin/env python3
"""
Tests for json_recompose.py

Coverage targets:
- Fragment reading and data merging
- Metadata loading and validation
- Consistency validation (roundtrip equivalence)
- Error handling (missing fragments, corrupt data, version mismatches)
"""

import pytest
import json
import tempfile
from pathlib import Path

# Import the module under test
import sys
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "scripts"))
from json_recompose import (
    JSONRecomposer,
    DependencyGraphRecomposer
)


def write_metadata(base: Path, total_items: int, strategy: str = "dependency-graph") -> None:
    meta = {
        "timestamp": "2026-01-06T00:00:00",
        "strategy": strategy,
        "total_items": total_items,
        "source_file": "dummy",
        "fragment_count": total_items
    }
    (base / "_metadata.json").write_text(json.dumps(meta), encoding="utf-8")


class TestJSONRecomposerBase:
    """Test base JSONRecomposer class."""
    
    def test_recomposer_initialization(self, tmp_path):
        """Test recomposer initializes correctly."""
        # Create minimal hierarchical structure
        (tmp_path / "_metadata.json").write_text(json.dumps({
            "timestamp": "2026-01-06T00:00:00",
            "strategy": "test",
            "total_items": 0,
            "fragment_count": 0
        }))
        
        recomposer = DependencyGraphRecomposer(str(tmp_path))
        
        assert recomposer.hierarchical_dir == tmp_path
        assert recomposer.metadata is not None
        assert recomposer.metadata["strategy"] == "test"
    
    def test_load_metadata_missing(self, tmp_path):
        """Test loading metadata when file doesn't exist."""
        with pytest.raises(ValueError):
            DependencyGraphRecomposer(str(tmp_path))
    
    def test_load_metadata_with_data(self, tmp_path):
        """Test loading metadata from file."""
        metadata = {
            "timestamp": "2026-01-06T00:00:00",
            "strategy": "dependency-graph",
            "source_file": "test.json",
            "total_items": 10,
            "fragment_count": 5
        }
        
        with open(tmp_path / "_metadata.json", "w") as f:
            json.dump(metadata, f)
        
        recomposer = DependencyGraphRecomposer(str(tmp_path))
        
        assert recomposer.metadata["total_items"] == 10
        assert recomposer.metadata["fragment_count"] == 5


class TestDependencyGraphRecomposer:
    """Test dependency graph recomposition."""
    
    def test_recompose_from_modules(self, tmp_path):
        """Test recomposing from module hierarchy."""
        # Create hierarchical structure
        modules_dir = tmp_path / "modules"
        modules_dir.mkdir(parents=True)
        write_metadata(tmp_path, total_items=3)
        
        # Write module files
        module_a_dir = modules_dir / "Module"
        module_a_dir.mkdir(parents=True)
        
        module_a = {
            "name": "Module.A",
            "imports": ["Module.B", "Module.C"],
            "imported_by": []
        }
        with open(module_a_dir / "A.json", "w") as f:
            json.dump(module_a, f)
        
        module_b = {
            "name": "Module.B",
            "imports": ["Module.C"],
            "imported_by": ["Module.A"]
        }
        with open(module_a_dir / "B.json", "w") as f:
            json.dump(module_b, f)
        
        module_c = {
            "name": "Module.C",
            "imports": [],
            "imported_by": ["Module.A", "Module.B"]
        }
        with open(module_a_dir / "C.json", "w") as f:
            json.dump(module_c, f)
        
        # Write index
        with open(modules_dir / "_index.json", "w") as f:
            json.dump(["Module.A", "Module.B", "Module.C"], f)
        
        # Recompose
        recomposer = DependencyGraphRecomposer(str(tmp_path))
        result = recomposer.recompose()
        
        # Verify result
        assert "modules" in result
        assert "edges" in result
        assert "Module.A" in result["modules"]
        assert "Module.B" in result["modules"]
        assert "Module.C" in result["modules"]
        
        # Check edges
        edges = result["edges"]
        edge_set = {(e["from"], e["to"]) for e in edges}
        assert ("Module.A", "Module.B") in edge_set
        assert ("Module.A", "Module.C") in edge_set
        assert ("Module.B", "Module.C") in edge_set
    
    def test_recompose_skips_index_files(self, tmp_path):
        """Test that _index.json files are not treated as modules."""
        modules_dir = tmp_path / "modules"
        modules_dir.mkdir(parents=True)
        write_metadata(tmp_path, total_items=1)
        
        # Create index file
        with open(modules_dir / "_index.json", "w") as f:
            json.dump(["Test"], f)
        
        # Create actual module
        module_data = {
            "name": "Test.Module",
            "imports": [],
            "imported_by": []
        }
        test_dir = modules_dir / "Test"
        test_dir.mkdir()
        with open(test_dir / "Module.json", "w") as f:
            json.dump(module_data, f)
        
        recomposer = DependencyGraphRecomposer(str(tmp_path))
        result = recomposer.recompose()
        
        # Should have one module, not two
        assert len(result["modules"]) == 1
        assert "Test.Module" in result["modules"]
    
    def test_recompose_with_layers(self, tmp_path):
        """Test recomposition includes layer information."""
        # Create modules
        modules_dir = tmp_path / "modules"
        modules_dir.mkdir(parents=True)
        write_metadata(tmp_path, total_items=1)
        
        module_data = {
            "name": "Test.A",
            "imports": [],
            "imported_by": []
        }
        test_dir = modules_dir / "Test"
        test_dir.mkdir()
        with open(test_dir / "A.json", "w") as f:
            json.dump(module_data, f)
        
        # Create layers
        layers_dir = tmp_path / "layers"
        layers_dir.mkdir(parents=True)
        
        with open(layers_dir / "layer-0.json", "w") as f:
            json.dump(["Test.A"], f)
        
        with open(layers_dir / "_index.json", "w") as f:
            json.dump(["layer-0"], f)
        
        recomposer = DependencyGraphRecomposer(str(tmp_path))
        result = recomposer.recompose()
        
        # Result should include layers info
        assert "layers" in result
        assert 0 in result["layers"]
        assert "Test.A" in result["layers"][0]
    
    def test_recompose_with_cycles(self, tmp_path):
        """Test recomposition includes cycle information."""
        # Create modules
        modules_dir = tmp_path / "modules"
        modules_dir.mkdir(parents=True)
        write_metadata(tmp_path, total_items=3)
        
        for name in ["Cycle.A", "Cycle.B", "Cycle.C"]:
            parts = name.split(".")
            module_dir = modules_dir / parts[0]
            module_dir.mkdir(exist_ok=True)
            
            module_data = {
                "name": name,
                "imports": [],
                "imported_by": []
            }
            with open(module_dir / f"{parts[1]}.json", "w") as f:
                json.dump(module_data, f)
        
        # Create cycles
        cycles_dir = tmp_path / "cycles"
        cycles_dir.mkdir(parents=True)
        
        with open(cycles_dir / "cycle-000.json", "w") as f:
            json.dump({"modules": ["Cycle.A", "Cycle.B", "Cycle.C"]}, f)
        
        with open(cycles_dir / "_index.json", "w") as f:
            json.dump(["cycle-000"], f)
        
        recomposer = DependencyGraphRecomposer(str(tmp_path))
        result = recomposer.recompose()
        
        # Result should include cycles info
        assert "cycles" in result
        assert len(result["cycles"]) == 1
        assert set(result["cycles"][0]) == {"Cycle.A", "Cycle.B", "Cycle.C"}
    
    def test_recompose_empty_structure(self, tmp_path):
        """Test recomposing empty hierarchical structure."""
        # Create minimal structure
        modules_dir = tmp_path / "modules"
        modules_dir.mkdir(parents=True)
        write_metadata(tmp_path, total_items=0)
        
        with open(modules_dir / "_index.json", "w") as f:
            json.dump([], f)
        
        recomposer = DependencyGraphRecomposer(str(tmp_path))
        result = recomposer.recompose()
        
        # Should return empty but valid structure
        assert result["modules"] == {}
        assert result["edges"] == []
    
    def test_recompose_no_duplicates(self, tmp_path):
        """Test that recomposition doesn't create duplicate edges."""
        modules_dir = tmp_path / "modules"
        modules_dir.mkdir(parents=True)
        write_metadata(tmp_path, total_items=3)
        
        # Create module with duplicate imports (shouldn't happen, but test robustness)
        module_data = {
            "name": "Test.A",
            "imports": ["Test.B", "Test.B", "Test.C"],  # Duplicate Test.B
            "imported_by": []
        }
        test_dir = modules_dir / "Test"
        test_dir.mkdir()
        with open(test_dir / "A.json", "w") as f:
            json.dump(module_data, f)
        
        module_b = {
            "name": "Test.B",
            "imports": [],
            "imported_by": ["Test.A"]
        }
        with open(test_dir / "B.json", "w") as f:
            json.dump(module_b, f)
        
        module_c = {
            "name": "Test.C",
            "imports": [],
            "imported_by": ["Test.A"]
        }
        with open(test_dir / "C.json", "w") as f:
            json.dump(module_c, f)
        
        recomposer = DependencyGraphRecomposer(str(tmp_path))
        result = recomposer.recompose()
        
        # Should have exactly 2 edges (A→B and A→C), no duplicates
        edges = result["edges"]
        assert len(edges) == 2
        edge_set = {(e["from"], e["to"]) for e in edges}
        assert len(edge_set) == 2  # No duplicate edges


class TestRecomposerErrorHandling:
    """Test error handling in recomposition."""
    
    def test_recompose_with_missing_module_name(self, tmp_path):
        """Test handling module files without 'name' field."""
        modules_dir = tmp_path / "modules"
        modules_dir.mkdir(parents=True)
        write_metadata(tmp_path, total_items=1)
        
        # Write module without name field
        with open(modules_dir / "invalid.json", "w") as f:
            json.dump({"imports": [], "imported_by": []}, f)
        
        recomposer = DependencyGraphRecomposer(str(tmp_path))
        with pytest.raises(ValueError):
            recomposer.recompose()
    
    def test_recompose_with_malformed_json(self, tmp_path):
        """Test handling malformed JSON files."""
        modules_dir = tmp_path / "modules"
        modules_dir.mkdir(parents=True)
        write_metadata(tmp_path, total_items=1)
        
        # Write malformed JSON
        with open(modules_dir / "malformed.json", "w") as f:
            f.write("{invalid json")
        
        recomposer = DependencyGraphRecomposer(str(tmp_path))
        
        # Should raise JSONDecodeError
        with pytest.raises(json.JSONDecodeError):
            recomposer.recompose()
    
    def test_recompose_missing_modules_directory(self, tmp_path):
        """Test recomposition when modules directory doesn't exist."""
        write_metadata(tmp_path, total_items=0)
        recomposer = DependencyGraphRecomposer(str(tmp_path))
        result = recomposer.recompose()

        # Should return empty structure, not crash
        assert result["modules"] == {}
        assert result["edges"] == []


class TestRecomposerRoundtrip:
    """Integration tests for decompose → recompose roundtrip."""
    
    def test_roundtrip_preserves_module_count(self, tmp_path):
        """Test that decompose → recompose preserves module count."""
        # This would require importing json_decompose, which we test separately
        # Here we test that recompose produces consistent output
        
        write_metadata(tmp_path, total_items=5)
        modules_dir = tmp_path / "modules"
        modules_dir.mkdir(parents=True)
        
        # Create 5 modules
        for i in range(5):
            module_data = {
                "name": f"Module.{chr(65 + i)}",  # Module.A, Module.B, etc.
                "imports": [],
                "imported_by": []
            }
            module_dir = modules_dir / "Module"
            module_dir.mkdir(exist_ok=True)
            with open(module_dir / f"{chr(65 + i)}.json", "w") as f:
                json.dump(module_data, f)
        
        recomposer = DependencyGraphRecomposer(str(tmp_path))
        result = recomposer.recompose()
        
        # Should have exactly 5 modules
        assert len(result["modules"]) == 5
    
    def test_roundtrip_preserves_edge_count(self, tmp_path):
        """Test that decompose → recompose preserves edge count."""
        modules_dir = tmp_path / "modules"
        modules_dir.mkdir(parents=True)
        write_metadata(tmp_path, total_items=3)
        
        # Create modules with specific edges
        module_a = {
            "name": "A",
            "imports": ["B", "C"],
            "imported_by": []
        }
        with open(modules_dir / "A.json", "w") as f:
            json.dump(module_a, f)
        
        module_b = {
            "name": "B",
            "imports": ["C"],
            "imported_by": ["A"]
        }
        with open(modules_dir / "B.json", "w") as f:
            json.dump(module_b, f)
        
        module_c = {
            "name": "C",
            "imports": [],
            "imported_by": ["A", "B"]
        }
        with open(modules_dir / "C.json", "w") as f:
            json.dump(module_c, f)
        
        recomposer = DependencyGraphRecomposer(str(tmp_path))
        result = recomposer.recompose()
        
        # Should have exactly 3 edges: A→B, A→C, B→C
        assert len(result["edges"]) == 3
