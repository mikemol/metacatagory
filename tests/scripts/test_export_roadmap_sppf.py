#!/usr/bin/env python3
"""
Tests for export_roadmap_sppf.py

Coverage targets:
- SPPF JSON generation from planning index
- Node structure creation
- File I/O operations
- CLI main function
"""

import pytest
import json
import tempfile
from pathlib import Path

# Import the module under test
import sys
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "scripts"))
from export_roadmap_sppf import main


class TestSPPFExport:
    """Test SPPF export functionality."""
    
    def test_main_creates_sppf_from_planning_index(self, tmp_path, monkeypatch, capsys):
        """Test main function reads planning index and creates SPPF."""
        # Change to tmp directory
        monkeypatch.chdir(tmp_path)
        
        # Create build directory and planning index
        build_dir = tmp_path / "build"
        build_dir.mkdir(parents=True)
        
        planning_index = [
            {
                "id": "TASK-001",
                "title": "Test Task",
                "status": "completed",
                "category": "development",
                "source": "github",
                "files": ["src/test.py"],
                "tags": ["python"]
            },
            {
                "id": "TASK-002",
                "title": "Another Task",
                "status": "planned",
                "category": "testing",
                "source": "roadmap",
                "files": [],
                "tags": []
            }
        ]
        
        with open(build_dir / "planning_index.json", "w") as f:
            json.dump(planning_index, f)
        
        # Run main
        main()
        
        # Verify output created
        output_file = build_dir / "gp_roadmap_sppf.json"
        assert output_file.exists()
        
        # Verify content
        with open(output_file) as f:
            sppf_data = json.load(f)
        
        assert "nodes" in sppf_data
        assert len(sppf_data["nodes"]) == 2
        
        # Check first node structure
        node1 = sppf_data["nodes"][0]
        assert node1["id"] == "TASK-001"
        assert node1["title"] == "Test Task"
        assert node1["status"] == "completed"
        assert node1["category"] == "development"
        assert node1["source"] == "github"
        assert node1["files"] == ["src/test.py"]
        assert node1["tags"] == ["python"]
        assert node1["parents"] == []
    
    def test_main_creates_build_directory(self, tmp_path, monkeypatch):
        """Test that main creates build directory if it doesn't exist."""
        monkeypatch.chdir(tmp_path)
        
        # Create planning_index.json in a build dir that will be created
        planning_file = tmp_path / "data" / "planning_index.json"
        planning_file.parent.mkdir(parents=True)
        
        with open(planning_file, "w") as f:
            json.dump([{"id": "T1", "title": "Test", "status": "planned", 
                       "category": "dev", "source": "test", "files": [], "tags": []}], f)
        
        # Remove output path if exists
        output_path = tmp_path / "build" / "gp_roadmap_sppf.json"
        if output_path.exists():
            output_path.unlink()
        
        # Run main
        main()
        
        # Verify build directory and output file exist
        assert output_path.exists()
    
    def test_main_handles_empty_planning_index(self, tmp_path, monkeypatch):
        """Test main with empty planning index."""
        monkeypatch.chdir(tmp_path)
        
        build_dir = tmp_path / "build"
        build_dir.mkdir(parents=True)
        
        with open(build_dir / "planning_index.json", "w") as f:
            json.dump([], f)
        
        # Run main
        main()
        
        # Verify output
        with open(build_dir / "gp_roadmap_sppf.json") as f:
            sppf_data = json.load(f)
        
        assert sppf_data["nodes"] == []
    
    def test_main_handles_missing_fields(self, tmp_path, monkeypatch):
        """Test main handles items with missing optional fields."""
        monkeypatch.chdir(tmp_path)
        
        build_dir = tmp_path / "build"
        build_dir.mkdir(parents=True)
        
        # Item with minimal fields
        planning_index = [
            {
                "id": "T1"
                # Missing: title, status, category, source, files, tags
            }
        ]
        
        with open(build_dir / "planning_index.json", "w") as f:
            json.dump(planning_index, f)
        
        # Run main
        main()
        
        # Verify output handles missing fields gracefully
        with open(build_dir / "gp_roadmap_sppf.json") as f:
            sppf_data = json.load(f)
        
        node = sppf_data["nodes"][0]
        assert node["id"] == "T1"
        assert node["title"] == ""
        assert node["status"] == ""
        assert node["files"] == []
        assert node["tags"] == []
        assert node["parents"] == []
    
    def test_main_preserves_data_types(self, tmp_path, monkeypatch):
        """Test that main preserves correct data types."""
        monkeypatch.chdir(tmp_path)
        
        build_dir = tmp_path / "build"
        build_dir.mkdir(parents=True)
        
        planning_index = [
            {
                "id": "TASK-001",
                "title": "Test",
                "status": "planned",
                "category": "dev",
                "source": "github",
                "files": ["file1.py", "file2.py"],
                "tags": ["tag1", "tag2", "tag3"]
            }
        ]
        
        with open(build_dir / "planning_index.json", "w") as f:
            json.dump(planning_index, f)
        
        main()
        
        with open(build_dir / "gp_roadmap_sppf.json") as f:
            sppf_data = json.load(f)
        
        node = sppf_data["nodes"][0]
        
        # Verify types
        assert isinstance(node["id"], str)
        assert isinstance(node["title"], str)
        assert isinstance(node["files"], list)
        assert isinstance(node["tags"], list)
        assert isinstance(node["parents"], list)
        
        # Verify list contents
        assert len(node["files"]) == 2
        assert len(node["tags"]) == 3
        assert node["parents"] == []

    def test_main_guard_executes(self, tmp_path, monkeypatch):
        """Run __main__ guard to cover direct execution."""
        monkeypatch.chdir(tmp_path)
        build_dir = tmp_path / "build"
        build_dir.mkdir(parents=True)
        (build_dir / "planning_index.json").write_text("[]")

        script_path = Path(__file__).parents[2] / "scripts" / "export_roadmap_sppf.py"
        fake_file = tmp_path / "scripts" / "export_roadmap_sppf.py"
        fake_file.parent.mkdir(parents=True)

        code = script_path.read_text()
        exec_globals = {
            "__name__": "__main__",
            "__file__": str(fake_file),
            "__builtins__": __builtins__,
        }
        exec(compile(code, str(script_path), "exec"), exec_globals)

        assert (build_dir / "gp_roadmap_sppf.json").exists()


class TestIntegration:
    """Integration tests for complete SPPF export workflow."""
    
    def test_full_export_workflow(self, tmp_path, monkeypatch):
        """Test complete export workflow with realistic data."""
        monkeypatch.chdir(tmp_path)
        
        build_dir = tmp_path / "build"
        build_dir.mkdir(parents=True)
        
        # Create realistic planning index
        planning_index = [
            {
                "id": "PHASE1-001",
                "title": "Set up infrastructure",
                "status": "completed",
                "category": "infrastructure",
                "source": "ROADMAP.md",
                "files": ["src/agda/Infrastructure/Base.agda"],
                "tags": ["phase1", "agda"]
            },
            {
                "id": "PHASE2-001",
                "title": "Implement core types",
                "status": "in-progress",
                "category": "development",
                "source": "github-tasks",
                "files": [
                    "src/agda/Core/Types.agda",
                    "src/agda/Core/Operations.agda"
                ],
                "tags": ["phase2", "agda", "core"]
            },
            {
                "id": "PHASE2-002",
                "title": "Add validation tests",
                "status": "planned",
                "category": "testing",
                "source": "github-tasks",
                "files": ["tests/test_core.py"],
                "tags": ["phase2", "testing", "python"]
            }
        ]
        
        with open(build_dir / "planning_index.json", "w") as f:
            json.dump(planning_index, f)
        
        # Run export
        main()
        
        # Verify output
        output_file = build_dir / "gp_roadmap_sppf.json"
        assert output_file.exists()
        
        with open(output_file) as f:
            sppf_data = json.load(f)
        
        # Verify all items converted
        assert len(sppf_data["nodes"]) == 3
        
        # Verify structure
        assert all("id" in node for node in sppf_data["nodes"])
        assert all("title" in node for node in sppf_data["nodes"])
        assert all("parents" in node for node in sppf_data["nodes"])
        
        # Verify specific data
        phase1_node = next(n for n in sppf_data["nodes"] if n["id"] == "PHASE1-001")
        assert phase1_node["status"] == "completed"
        assert "agda" in phase1_node["tags"]
        
        phase2_node = next(n for n in sppf_data["nodes"] if n["id"] == "PHASE2-001")
        assert len(phase2_node["files"]) == 2
        assert phase2_node["category"] == "development"
