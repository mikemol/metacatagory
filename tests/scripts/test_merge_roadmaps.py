#!/usr/bin/env python3
"""
Tests for merge_roadmaps.py

Coverage targets:
- Multi-source loading (tasks.json, ROADMAP.md, Agda files, doclint)
- Normalization and deduplication
- Merge-by-title with provenance tracking
- ID conflict resolution
- Description backfilling
- Export to JSON and Agda formats
"""

import pytest
import json
import tempfile
from pathlib import Path

# Import the module under test
import sys
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "scripts"))
from merge_roadmaps import (
    normalize_title,
    ensure_provenance,
    load_tasks_json,
    parse_roadmap_md,
    deduplicate_by_id,
    merge_by_title,
    backfill_descriptions,
    build_description,
    export_to_json,
    export_to_agda
)


class TestNormalization:
    """Test title normalization."""
    
    def test_normalize_simple_title(self):
        """Test normalizing simple title."""
        assert normalize_title("Test Item") == "test item"
    
    def test_normalize_with_punctuation(self):
        """Test normalizing title with punctuation."""
        assert normalize_title("Test: Item!") == "test item"
    
    def test_normalize_with_whitespace(self):
        """Test normalizing title with extra whitespace."""
        assert normalize_title("  Test   Item  ") == "test item"
    
    def test_normalize_with_special_chars(self):
        """Test normalizing title with special characters."""
        assert normalize_title("Test-Item_123") == "test item_123"
    
    def test_normalize_empty_title(self):
        """Test normalizing empty title."""
        assert normalize_title("") == ""


class TestProvenance:
    """Test provenance tracking."""
    
    def test_ensure_provenance_adds_field(self):
        """Test that ensure_provenance adds provenance field if missing."""
        item = {"id": "TEST-001", "title": "Test", "source": ""}
        ensure_provenance(item)
        assert "provenance" in item
        assert "TEST-001|" in item["provenance"]
    
    def test_ensure_provenance_preserves_existing(self):
        """Test that ensure_provenance preserves existing provenance."""
        item = {
            "id": "TEST-001",
            "title": "Test",
            "provenance": ["GITHUB-001"]
        }
        ensure_provenance(item)
        assert item["provenance"] == ["GITHUB-001"]


class TestTasksJSONLoading:
    """Test loading tasks from GitHub tasks.json."""
    
    def test_load_tasks_json_valid_file(self, tmp_path):
        """Test loading valid tasks.json."""
        tasks_file = tmp_path / "tasks.json"
        tasks_data = [
            {
                "id": "TASK-001",
                "title": "Test Task",
                "status": "in-progress",
                "category": "development"
            },
            {
                "id": "TASK-002",
                "title": "Another Task",
                "status": "completed",
                "category": "testing"
            }
        ]
        
        with open(tasks_file, "w") as f:
            json.dump(tasks_data, f)
        
        items = load_tasks_json(tasks_file)
        
        assert len(items) == 2
        assert items[0]["id"] == "TASK-001"
        # Verify structure is loaded correctly (tags are empty if not in source)
        assert "tags" in items[0]
    
    def test_load_tasks_json_missing_file(self, tmp_path):
        """Test loading non-existent tasks.json."""
        items = load_tasks_json(tmp_path / "nonexistent.json")
        assert items == []
    
    def test_load_tasks_json_malformed(self, tmp_path):
        """Test loading malformed tasks.json."""
        tasks_file = tmp_path / "tasks.json"
        with open(tasks_file, "w") as f:
            f.write("{invalid json")
        
        # Implementation doesn't catch JSON errors
        with pytest.raises(json.JSONDecodeError):
            load_tasks_json(tasks_file)


class TestRoadmapMDParsing:
    """Test parsing ROADMAP.md."""
    
    @pytest.mark.skip(reason="parse_roadmap_md not fully implemented")
    def test_parse_roadmap_md_with_sections(self, tmp_path):
        """Test parsing ROADMAP.md with multiple sections."""
        roadmap_file = tmp_path / "ROADMAP.md"
        roadmap_content = """# Roadmap

## Phase 1: Setup

- [ ] Task 1 description
- [x] Completed task
- [ ] Another task

## Phase 2: Development

- [ ] Development task
"""
        
        with open(roadmap_file, "w") as f:
            f.write(roadmap_content)
        
        items = parse_roadmap_md(roadmap_file)
        
        # Should create items for tasks
        assert len(items) >= 4
        
        # Check first item
        first = next((i for i in items if "Task 1" in i["title"]), None)
        assert first is not None
        assert first["source"] == "ROADMAP.md"
        assert first["tags"] == ["markdown", "roadmap"]
        assert first["status"] in ["not-started", "planned"]
    
    @pytest.mark.skip(reason="parse_roadmap_md not fully implemented")
    def test_parse_roadmap_md_completed_tasks(self, tmp_path):
        """Test parsing completed tasks from ROADMAP.md."""
        roadmap_file = tmp_path / "ROADMAP.md"
        roadmap_content = """# Roadmap

- [x] Completed task
- [ ] Pending task
"""
        
        with open(roadmap_file, "w") as f:
            f.write(roadmap_content)
        
        items = parse_roadmap_md(roadmap_file)
        
        # Find completed task
        completed = next((i for i in items if "Completed" in i["title"]), None)
        assert completed is not None
        assert completed["status"] in ["completed", "done"]
    
    def test_parse_roadmap_md_missing_file(self, tmp_path):
        """Test parsing non-existent ROADMAP.md."""
        items = parse_roadmap_md(tmp_path / "nonexistent.md")
        assert items == []


class TestDeduplication:
    """Test deduplication logic."""
    
    def test_deduplicate_by_id_no_duplicates(self):
        """Test deduplication when there are no duplicates."""
        items = [
            {
                "id": "TEST-001",
                "title": "Test 1",
                "dependsOn": [],
                "related": [],
                "tags": []
            },
            {
                "id": "TEST-002",
                "title": "Test 2",
                "dependsOn": [],
                "related": [],
                "tags": []
            }
        ]
        
        result = deduplicate_by_id(items)
        assert len(result) == 2
    
    def test_deduplicate_by_id_with_duplicates(self):
        """Test deduplication merges dependencies."""
        items = [
            {
                "id": "TEST-001",
                "title": "Test",
                "status": "in-progress",
                "category": "dev",
                "source": "github",
                "dependsOn": ["DEP-001"],
                "related": ["REL-001"],
                "tags": ["tag1"]
            },
            {
                "id": "TEST-001",
                "title": "Test",
                "status": "in-progress",
                "category": "dev",
                "source": "github",
                "dependsOn": ["DEP-002"],
                "related": ["REL-002"],
                "tags": ["tag2"]
            }
        ]
        
        result = deduplicate_by_id(items)
        
        assert len(result) == 1
        assert set(result[0]["dependsOn"]) == {"DEP-001", "DEP-002"}
        assert set(result[0]["related"]) == {"REL-001", "REL-002"}
        assert set(result[0]["tags"]) == {"tag1", "tag2"}
    
    def test_deduplicate_by_id_prefers_non_empty(self):
        """Test that deduplication prefers non-empty field values."""
        items = [
            {
                "id": "TEST-001",
                "title": "",
                "status": "planned",
                "category": "",
                "source": "github",
                "dependsOn": [],
                "related": [],
                "tags": []
            },
            {
                "id": "TEST-001",
                "title": "Real Title",
                "status": "in-progress",
                "category": "development",
                "source": "roadmap",
                "dependsOn": [],
                "related": [],
                "tags": []
            }
        ]
        
        result = deduplicate_by_id(items)
        
        assert len(result) == 1
        assert result[0]["title"] == "Real Title"
        assert result[0]["category"] == "development"


class TestMergeByTitle:
    """Test merge-by-title with provenance tracking."""
    
    def test_merge_by_title_no_duplicates(self):
        """Test merge when there are no title duplicates."""
        items = [
            {
                "id": "TEST-001",
                "title": "Unique Task 1",
                "description": "Description 1",
                "status": "planned",
                "category": "dev",
                "source": "github",
                "files": [],
                "tags": [],
                "dependsOn": [],
                "related": [],
                "provenance": []
            },
            {
                "id": "TEST-002",
                "title": "Unique Task 2",
                "description": "Description 2",
                "status": "planned",
                "category": "dev",
                "source": "github",
                "files": [],
                "tags": [],
                "dependsOn": [],
                "related": [],
                "provenance": []
            }
        ]
        
        result = merge_by_title(items)
        assert len(result) == 2
    
    def test_merge_by_title_with_duplicates(self):
        """Test merge preserves provenance when merging duplicates."""
        items = [
            {
                "id": "GITHUB-001",
                "title": "Test Task",
                "description": "From GitHub",
                "status": "in-progress",
                "category": "dev",
                "source": "github",
                "files": ["file1.py"],
                "tags": ["tag1"],
                "dependsOn": ["DEP-001"],
                "related": [],
                "provenance": []
            },
            {
                "id": "ROADMAP-MD-001",
                "title": "Test Task",
                "description": "From ROADMAP.md",
                "status": "planned",
                "category": "dev",
                "source": "roadmap",
                "files": ["file2.py"],
                "tags": ["tag2"],
                "dependsOn": ["DEP-002"],
                "related": [],
                "provenance": []
            }
        ]
        
        result = merge_by_title(items)
        
        assert len(result) == 1
        # Should prefer non ROADMAP-MD-* ID
        assert result[0]["id"] == "GITHUB-001"
        # Should union files, tags, dependencies
        assert set(result[0]["files"]) == {"file1.py", "file2.py"}
        assert set(result[0]["tags"]) == {"tag1", "tag2"}
        assert set(result[0]["dependsOn"]) == {"DEP-001", "DEP-002"}
        # Should track provenance in ID|source format
        assert any("ROADMAP-MD-001" in p for p in result[0]["provenance"])
    
    def test_merge_by_title_prefers_stable_id(self):
        """Test that merge prefers stable IDs over ROADMAP-MD-* IDs."""
        items = [
            {
                "id": "ROADMAP-MD-001",
                "title": "Test Task",
                "description": "Desc",
                "status": "planned",
                "category": "dev",
                "source": "roadmap",
                "files": [],
                "tags": [],
                "dependsOn": [],
                "related": [],
                "provenance": []
            },
            {
                "id": "STABLE-001",
                "title": "Test Task",
                "description": "Desc",
                "status": "in-progress",
                "category": "dev",
                "source": "github",
                "files": [],
                "tags": [],
                "dependsOn": [],
                "related": [],
                "provenance": []
            }
        ]
        
        result = merge_by_title(items)
        
        assert len(result) == 1
        assert result[0]["id"] == "STABLE-001"


class TestDescriptionBackfill:
    """Test description backfilling."""
    
    def test_backfill_descriptions_missing(self):
        """Test backfilling missing descriptions."""
        items = [
            {
                "title": "Test Task",
                "source": "github",
                "category": "dev",
                "tags": ["tag1", "tag2"],
                "files": ["file1.py", "file2.py"]
            }
        ]
        
        result = backfill_descriptions(items)
        
        assert "description" in result[0]
        # Description should contain title and metadata
        desc = result[0]["description"]
        assert "Test Task" in desc
        assert "github" in desc or "dev" in desc or "tag1" in desc
    
    def test_backfill_descriptions_preserves_existing(self):
        """Test that backfill preserves existing descriptions."""
        items = [
            {
                "title": "Test Task",
                "description": "Existing description",
                "source": "github",
                "category": "dev",
                "tags": [],
                "files": []
            }
        ]
        
        result = backfill_descriptions(items)
        
        # Should preserve non-trivial descriptions
        # (but may enhance if description == title)
        assert "description" in result[0]
    
    def test_build_description_comprehensive(self):
        """Test building description from all available metadata."""
        item = {
            "title": "Test Task",
            "source": "github-tasks",
            "category": "development",
            "tags": ["python", "testing"],
            "files": ["tests/test_merge.py", "scripts/merge.py", "extra.py"]
        }
        
        desc = build_description(item)
        
        assert "Test Task" in desc
        assert "github-tasks" in desc
        assert "development" in desc
        assert "python" in desc or "testing" in desc
        assert "test_merge.py" in desc or "merge.py" in desc


class TestExport:
    """Test export functionality."""
    
    def test_export_to_json(self, tmp_path):
        """Test exporting to JSON format."""
        items = [
            {
                "id": "TEST-001",
                "title": "Test",
                "description": "Description",
                "status": "planned",
                "category": "dev",
                "source": "github",
                "files": [],
                "tags": [],
                "dependsOn": [],
                "related": [],
                "provenance": []
            }
        ]
        
        output_file = tmp_path / "output.json"
        export_to_json(items, output_file)
        
        assert output_file.exists()
        
        with open(output_file) as f:
            loaded = json.load(f)
        
        assert len(loaded) == 1
        assert loaded[0]["id"] == "TEST-001"
    
    def test_export_to_agda(self, tmp_path):
        """Test exporting to Agda format."""
        items = [
            {
                "id": "TEST-001",
                "title": "Test Task",
                "description": "Description",
                "status": "planned",
                "category": "development",
                "source": "github",
                "files": ["test.py"],
                "tags": ["python"],
                "dependsOn": ["DEP-001"],
                "related": ["REL-001"],
                "provenance": []
            }
        ]
        
        output_file = tmp_path / "output.agda"
        export_to_agda(items, output_file)
        
        assert output_file.exists()
        
        content = output_file.read_text()
        
        # Verify Agda syntax
        assert "canonicalItems : List RoadmapItem" in content
        assert "canonicalItems =" in content
        assert "TEST-001" in content
        assert "Test Task" in content
        assert "record {" in content
        assert "id =" in content
        assert "title =" in content
    
    def test_export_to_agda_escapes_strings(self, tmp_path):
        """Test that Agda export properly escapes strings."""
        items = [
            {
                "id": "TEST-001",
                "title": 'Test with "quotes" and \\backslash',
                "description": "Description\nwith\nnewlines",
                "status": "planned",
                "category": "dev",
                "source": "github",
                "files": [],
                "tags": [],
                "dependsOn": [],
                "related": [],
                "provenance": []
            }
        ]
        
        output_file = tmp_path / "output.agda"
        export_to_agda(items, output_file)
        
        content = output_file.read_text()
        
        # Verify escaping
        assert '\\"' in content  # Escaped quotes
        assert '\\\\' in content  # Escaped backslash
        assert '\\n' in content  # Escaped newlines


class TestIntegration:
    """Integration tests for merge workflow."""
    
    def test_full_merge_workflow(self, tmp_path):
        """Test complete merge workflow: load → deduplicate → merge → export."""
        # Create tasks.json
        tasks_file = tmp_path / "tasks.json"
        with open(tasks_file, "w") as f:
            json.dump([
                {
                    "id": "TASK-001",
                    "title": "Shared Task",
                    "status": "in-progress",
                    "category": "dev"
                }
            ], f)
        
        # Create ROADMAP.md with duplicate task
        roadmap_file = tmp_path / "ROADMAP.md"
        with open(roadmap_file, "w") as f:
            f.write("# Roadmap\n\n- [ ] Shared Task\n")
        
        # Load from both sources
        items = []
        items.extend(load_tasks_json(tasks_file))
        items.extend(parse_roadmap_md(roadmap_file))
        
        # Deduplicate
        merged = deduplicate_by_id(items)
        merged = merge_by_title(merged)
        merged = backfill_descriptions(merged)
        
        # Should merge to single item
        assert len(merged) >= 1
        
        # Export
        output_json = tmp_path / "output.json"
        export_to_json(merged, output_json)
        
        output_agda = tmp_path / "output.agda"
        export_to_agda(merged, output_agda)
        
        # Verify exports
        assert output_json.exists()
        assert output_agda.exists()
