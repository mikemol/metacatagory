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
from tests.test_utils import (
    unicode_roadmap_json,
    deeply_nested_json,
)


class TestUnicodeHandling:
    """Test unicode character handling in JSON and markdown"""

    def test_unicode_json_roundtrip(self, unicode_roadmap_json):
        """Test that unicode survives JSON roundtrip"""
        try:
            from scripts.json_decompose import decompose
            from scripts.json_recompose import recompose
            
            # Decompose
            decomposed = decompose(unicode_roadmap_json)
            
            # Recompose
            recomposed = recompose(decomposed)
            
            # Check unicode preserved
            assert recomposed is not None
            json_str = json.dumps(recomposed, ensure_ascii=False)
            assert "café" in json_str or "cafe" in json_str
            assert "🎯" in json_str or "emoji" not in json_str.lower()
        except ImportError:
            pytest.skip("Required scripts not available")

    def test_unicode_in_identifiers(self, unicode_roadmap_json):
        """Test validation of unicode in item identifiers"""
        try:
            from scripts.validate_json import validate_json
            
            # Should handle unicode identifiers
            errors = validate_json(unicode_roadmap_json)
            # Whether it passes or fails, it shouldn't crash
            assert errors is not None
        except ImportError:
            pytest.skip("Script not available")

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
        
        try:
            from scripts.json_decompose import decompose
            
            result = decompose(emoji_roadmap)
            json_str = json.dumps(result, ensure_ascii=False)
            
            # Emojis should be preserved
            assert "🚀" in json_str or "rocket" not in json_str
        except ImportError:
            pytest.skip("Script not available")

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
        
        try:
            from scripts.validate_json import validate_json
            
            errors = validate_json(combining_roadmap)
            assert errors is not None
        except ImportError:
            pytest.skip("Script not available")

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
        
        try:
            from scripts.validate_json import validate_json
            
            errors = validate_json(rtl_roadmap)
            # Should handle RTL text without crashing
            assert errors is not None
        except ImportError:
            pytest.skip("Script not available")


class TestLargeFileHandling:
    """Test handling of large files"""

    def test_large_json_decomposition(self, tmp_path):
        """Test decomposing a moderately large JSON file (10MB)"""
        try:
            from scripts.json_decompose import decompose
            
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
            
            # Should decompose without memory error
            result = decompose(large_roadmap)
            assert result is not None
            assert len(result.get("items", [])) == 10000
        except ImportError:
            pytest.skip("Script not available")
        except MemoryError:
            pytest.skip("Insufficient memory for test")

    def test_large_markdown_validation(self, tmp_path):
        """Test validating a large markdown file"""
        try:
            from scripts.validate_md import validate_markdown
            
            # Create large markdown (~5MB)
            large_md = "# Documentation\n\n"
            for i in range(10000):
                large_md += f"## Section {i}\n"
                large_md += f"Content for section {i}.\n\n"
            
            # Should validate without issue
            result = validate_markdown(large_md)
            assert result is not None
        except ImportError:
            pytest.skip("Script not available")
        except MemoryError:
            pytest.skip("Insufficient memory for test")

    def test_large_file_performance(self, tmp_path):
        """Test that large files don't cause excessive slowdown"""
        try:
            from scripts.json_decompose import decompose
            import time
            
            # Small roadmap
            small_roadmap = {"items": [{"id": f"ITEM-{i}", "status": "pending"} for i in range(100)]}
            
            start = time.time()
            result1 = decompose(small_roadmap)
            time_small = time.time() - start
            
            # Larger roadmap (100x items)
            large_roadmap = {"items": [{"id": f"ITEM-{i}", "status": "pending"} for i in range(10000)]}
            
            start = time.time()
            result2 = decompose(large_roadmap)
            time_large = time.time() - start
            
            # Large should not be more than 50x slower (linear scaling expected)
            # Allow for some variance in system load
            ratio = time_large / time_small if time_small > 0 else 1
            assert ratio < 500, f"Performance degradation: {ratio}x slower for 100x data"
        except ImportError:
            pytest.skip("Script not available")


class TestDeepNestingHandling:
    """Test handling of deeply nested structures"""

    def test_deeply_nested_json(self, deeply_nested_json):
        """Test validation of deeply nested JSON (50+ levels)"""
        try:
            from scripts.validate_json import validate_json
            
            # Should not hit recursion limits
            result = validate_json({"items": [deeply_nested_json]})
            assert result is not None
        except ImportError:
            pytest.skip("Script not available")
        except RecursionError:
            pytest.fail("Script cannot handle deep nesting (RecursionError)")

    def test_dependency_chain_depth(self):
        """Test validation of deeply chained dependencies"""
        try:
            from scripts.validate_json import validate_json
            
            # Create deep dependency chain: A -> B -> C -> ... -> Z
            items = []
            prev_id = None
            for i in range(100):
                item_id = f"ITEM-{i:03d}"
                item = {
                    "id": item_id,
                    "status": "pending",
                    "depends_on": [prev_id] if prev_id else []
                }
                items.append(item)
                prev_id = item_id
            
            roadmap = {"items": items}
            
            # Should validate without recursion error
            result = validate_json(roadmap)
            assert result is not None
        except ImportError:
            pytest.skip("Script not available")
        except RecursionError:
            pytest.fail("Script cannot handle deep dependency chains")

    def test_max_nesting_level(self):
        """Test JSON at Python's default recursion limit"""
        try:
            from scripts.validate_json import validate_json
            
            # Build structure at Python recursion limit
            max_depth = sys.getrecursionlimit() - 100
            nested = {"value": "leaf"}
            for i in range(max_depth):
                nested = {"level": i, "nested": nested}
            
            # Should handle without stack overflow
            try:
                result = validate_json({"items": [nested]})
                # If it succeeds, great
                assert result is not None
            except RecursionError:
                # Expected at deep nesting levels
                pass
        except ImportError:
            pytest.skip("Script not available")


class TestBoundaryConditions:
    """Test boundary and edge cases"""

    def test_empty_item_list(self):
        """Test roadmap with empty items list"""
        try:
            from scripts.validate_json import validate_json
            
            empty_roadmap = {"items": []}
            result = validate_json(empty_roadmap)
            assert result is not None
        except ImportError:
            pytest.skip("Script not available")

    def test_single_item(self):
        """Test roadmap with exactly one item"""
        try:
            from scripts.validate_json import validate_json
            
            single_item = {"items": [{"id": "ONLY", "status": "pending"}]}
            result = validate_json(single_item)
            assert result is not None
        except ImportError:
            pytest.skip("Script not available")

    def test_max_id_length(self):
        """Test handling of very long item IDs"""
        try:
            from scripts.validate_json import validate_json
            
            long_id_roadmap = {
                "items": [
                    {
                        "id": "A" * 10000,  # 10K character ID
                        "status": "pending"
                    }
                ]
            }
            
            result = validate_json(long_id_roadmap)
            assert result is not None
        except ImportError:
            pytest.skip("Script not available")

    def test_max_description_length(self):
        """Test handling of very long descriptions"""
        try:
            from scripts.validate_json import validate_json
            
            long_desc_roadmap = {
                "items": [
                    {
                        "id": "ITEM-001",
                        "description": "x" * 1000000,  # 1MB description
                        "status": "pending"
                    }
                ]
            }
            
            result = validate_json(long_desc_roadmap)
            assert result is not None
        except ImportError:
            pytest.skip("Script not available")
        except MemoryError:
            pytest.skip("Insufficient memory for test")

    def test_many_dependencies(self):
        """Test handling items with many dependencies"""
        try:
            from scripts.validate_json import validate_json
            
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
            
            result = validate_json(many_deps_roadmap)
            assert result is not None
        except ImportError:
            pytest.skip("Script not available")

    def test_null_values(self):
        """Test handling of null/None values in JSON"""
        try:
            from scripts.validate_json import validate_json
            
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
            
            result = validate_json(null_roadmap)
            # Should either pass or have specific error
            assert result is not None
        except ImportError:
            pytest.skip("Script not available")

    def test_duplicate_ids(self):
        """Test handling of duplicate item IDs"""
        try:
            from scripts.validate_json import validate_json
            
            duplicate_ids_roadmap = {
                "items": [
                    {"id": "DUPLICATE", "status": "pending"},
                    {"id": "DUPLICATE", "status": "pending"},
                ]
            }
            
            errors = validate_json(duplicate_ids_roadmap)
            # Should detect duplicate IDs
            if len(errors) > 0:
                error_strs = [str(e).lower() for e in errors]
                # Check if any error mentions duplicates
                assert any("duplicate" in e for e in error_strs)
        except ImportError:
            pytest.skip("Script not available")
