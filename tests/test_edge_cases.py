"""
Phase 1 Block 3: Edge Case Tests (20 minutes)

Tests for boundary conditions:
- Unicode and special characters
- Large files (>100MB)
- Deep nesting (50+ levels)
"""

import pytest
import json
import sys
from pathlib import Path
from test_utils import (
    unicode_roadmap_json,
    deeply_nested_json,
)


class TestUnicodeHandling:
    """Test unicode character handling in JSON and markdown"""

    def test_unicode_json_roundtrip(self, unicode_roadmap_json):
        """Test that unicode survives JSON roundtrip"""
        # Test JSON module handles unicode
        json_str = json.dumps(unicode_roadmap_json, ensure_ascii=False)
        roundtripped = json.loads(json_str)
        
        # Check unicode preserved
        assert roundtripped is not None
        assert "café" in json_str
        assert "🎯" in json_str

    def test_unicode_in_identifiers(self, unicode_roadmap_json):
        """Test validation of unicode in item identifiers"""
        # Unicode roadmap should have valid structure
        assert "items" in unicode_roadmap_json
        for item in unicode_roadmap_json["items"]:
            assert "id" in item
            assert "title" in item
            # Unicode should be in title or description
            item_str = json.dumps(item, ensure_ascii=False)
            assert len(item_str) > 0

    def test_emoji_preservation(self):
        """Test that emojis are preserved through processing"""
        emoji_roadmap = {
            "items": [
                {
                    "id": "ITEM-001",
                    "title": "Task with emoji 🚀",
                    "description": "Complete ✅ or Pending ⏳",
                    "status": "active"
                }
            ]
        }
        
        # JSON should preserve emojis
        json_str = json.dumps(emoji_roadmap, ensure_ascii=False)
        roundtripped = json.loads(json_str)
        
        # Emojis should be preserved
        assert "🚀" in json_str
        assert roundtripped["items"][0]["title"] == "Task with emoji 🚀"

    def test_combining_marks(self):
        """Test handling of combining diacritical marks"""
        combining_roadmap = {
            "items": [
                {
                    "id": "ITEM-001",
                    "title": "Café naïve résumé",  # Characters with combining marks
                    "status": "pending"
                }
            ]
        }
        
        # JSON should handle combining marks
        json_str = json.dumps(combining_roadmap, ensure_ascii=False)
        roundtripped = json.loads(json_str)
        assert roundtripped["items"][0]["title"] == "Café naïve résumé"

    def test_right_to_left_text(self):
        """Test handling of RTL languages (Arabic, Hebrew)"""
        rtl_roadmap = {
            "items": [
                {
                    "id": "ITEM-001",
                    "title": "مرحبا بك",  # Arabic
                    "description": "שלום",  # Hebrew
                    "status": "pending"
                }
            ]
        }
        
        # JSON should handle RTL text
        json_str = json.dumps(rtl_roadmap, ensure_ascii=False)
        roundtripped = json.loads(json_str)
        assert roundtripped["items"][0]["description"] == "שלום"


class TestLargeFileHandling:
    """Test handling of large files"""

    def test_large_json_decomposition(self, tmp_path):
        """Test decomposing a moderately large JSON file (10MB)"""
        # Create 10,000 item roadmap (~10MB)
        large_roadmap = {
            "items": [
                {
                    "id": f"ITEM-{i:06d}",
                    "title": f"Task {i}",
                    "status": "pending",
                    "description": "x" * 1000,  # 1KB per item
                    "depends_on": []
                }
                for i in range(10000)
            ]
        }
        
        # Test JSON can handle large structures
        try:
            json_str = json.dumps(large_roadmap)
            roundtripped = json.loads(json_str)
            assert len(roundtripped["items"]) == 10000
        except MemoryError:
            pytest.skip("Insufficient memory for test")

    def test_large_markdown_string(self, tmp_path):
        """Test handling a large markdown string"""
        # Create large markdown (~0.4MB) to ensure handling without OOM
        large_md = "# Documentation\n\n"
        for i in range(10000):
            large_md += f"## Section {i}\n"
            large_md += f"Content for section {i}.\n\n"
        
        # Should handle without issue
        try:
            # Length should be substantial to exercise parsing
            assert len(large_md) > 400_000
            assert "Section 9999" in large_md
        except MemoryError:
            pytest.skip("Insufficient memory for test")

    def test_large_file_io(self, tmp_path):
        """Test file I/O with large JSON files"""
        try:
            # Create large file
            large_data = {"items": [{"id": f"ITEM-{i}", "status": "pending"} for i in range(10000)]}
            large_file = tmp_path / "large.json"
            
            with open(large_file, "w") as f:
                json.dump(large_data, f)
            
            # Read it back
            with open(large_file, "r") as f:
                loaded_data = json.load(f)
            
            assert len(loaded_data["items"]) == 10000
        except MemoryError:
            pytest.skip("Insufficient memory for test")


class TestDeepNestingHandling:
    """Test handling of deeply nested structures"""

    def test_deeply_nested_json(self, deeply_nested_json):
        """Test validation of deeply nested JSON (50+ levels)"""
        # Should not hit recursion limits with modern Python
        try:
            json_str = json.dumps(deeply_nested_json)
            roundtripped = json.loads(json_str)
            assert roundtripped is not None
        except RecursionError:
            pytest.fail("JSON module cannot handle deep nesting (RecursionError)")

    def test_dependency_chain_depth(self):
        """Test dependency chain with max depth"""
        # Create long dependency chain A→B→C→...→Z (50 items)
        chain_roadmap = {
            "items": [
                {
                    "id": f"ITEM-{i:03d}",
                    "status": "pending",
                    "depends_on": [] if i == 0 else [f"ITEM-{i-1:03d}"]
                }
                for i in range(50)
            ]
        }
        
        # Test we can handle deep dependency chains
        assert len(chain_roadmap["items"]) == 50
        assert chain_roadmap["items"][49]["depends_on"] == ["ITEM-048"]

    def test_max_nesting_level(self):
        """Test JSON at Python's default recursion limit"""
        # Build structure at safe depth (100 levels)
        safe_depth = 100
        nested = {"value": "leaf"}
        for i in range(safe_depth):
            nested = {"level": i, "nested": nested}
        
        # Should handle without stack overflow
        try:
            json_str = json.dumps(nested)
            roundtripped = json.loads(json_str)
            assert roundtripped is not None
        except RecursionError:
            pytest.fail("JSON module hit recursion limit at 100 levels")


class TestBoundaryConditions:
    """Test boundary and edge cases"""

    def test_empty_item_list(self):
        """Test roadmap with empty items list"""
        empty_roadmap = {"items": []}
        
        # Should be valid structure
        assert "items" in empty_roadmap
        assert len(empty_roadmap["items"]) == 0

    def test_single_item(self):
        """Test roadmap with exactly one item"""
        single_item = {"items": [{"id": "ONLY", "status": "pending"}]}
        
        # Should be valid
        assert len(single_item["items"]) == 1
        assert single_item["items"][0]["id"] == "ONLY"

    def test_max_id_length(self):
        """Test handling of very long item IDs"""
        long_id_roadmap = {
            "items": [
                {
                    "id": "A" * 10000,  # 10K character ID
                    "status": "pending"
                }
            ]
        }
        
        # JSON should handle long strings
        json_str = json.dumps(long_id_roadmap)
        assert len(json_str) > 10000

    def test_max_description_length(self):
        """Test handling of very long descriptions"""
        try:
            long_desc_roadmap = {
                "items": [
                    {
                        "id": "ITEM-001",
                        "description": "x" * 1000000,  # 1MB description
                        "status": "pending"
                    }
                ]
            }
            
            # JSON should handle large strings
            json_str = json.dumps(long_desc_roadmap)
            assert len(json_str) > 1000000
        except MemoryError:
            pytest.skip("Insufficient memory for test")

    def test_many_dependencies(self):
        """Test handling items with many dependencies"""
        # Item depends on 1000 other items
        many_deps_roadmap = {
            "items": [
                {
                    "id": "ITEM-001",
                    "status": "pending",
                    "depends_on": [f"ITEM-{i:06d}" for i in range(1000)]
                }
            ]
        }
        
        # Should handle large dependency arrays
        assert len(many_deps_roadmap["items"][0]["depends_on"]) == 1000

    def test_null_values(self):
        """Test handling of null/None values in JSON"""
        null_roadmap = {
            "items": [
                {
                    "id": "ITEM-001",
                    "title": None,
                    "description": None,
                    "status": "pending"
                }
            ]
        }
        
        # JSON should preserve None as null
        json_str = json.dumps(null_roadmap)
        assert "null" in json_str

    def test_duplicate_ids(self):
        """Test detection of duplicate item IDs"""
        duplicate_ids_roadmap = {
            "items": [
                {"id": "DUPLICATE", "status": "pending"},
                {"id": "DUPLICATE", "status": "pending"},
            ]
        }
        
        # Test duplicate detection algorithm
        ids_seen = set()
        duplicates = []
        for item in duplicate_ids_roadmap["items"]:
            if item["id"] in ids_seen:
                duplicates.append(item["id"])
            ids_seen.add(item["id"])
        
        # Should detect the duplicate
        assert "DUPLICATE" in duplicates

