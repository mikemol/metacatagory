#!/usr/bin/env python3
"""Tests for scripts/shared/normalization.py module."""

import pytest
import sys
from pathlib import Path

# Import the module under test
sys.path.insert(0, str(Path(__file__).parent.parent.parent))
from scripts.shared.normalization import (
    normalize_title,
    normalize_id,
    ensure_item_fields,
    ensure_provenance,
    deduplicate_items_by_id,
    merge_dependencies,
    validate_item_structure,
    clean_empty_strings
)


class TestNormalizeTitle:
    """Test normalize_title function."""
    
    def test_removes_special_characters(self):
        """Should remove special characters."""
        result = normalize_title("Task-001: Fix! Issues?")
        assert result == "task 001 fix issues"
    
    def test_normalizes_whitespace(self):
        """Should normalize multiple spaces."""
        result = normalize_title("Multiple    Spaces   Here")
        assert result == "multiple spaces here"
    
    def test_converts_to_lowercase(self):
        """Should convert to lowercase."""
        result = normalize_title("UPPERCASE Title")
        assert result == "uppercase title"


class TestNormalizeId:
    """Test normalize_id function."""
    
    def test_removes_whitespace(self):
        """Should strip whitespace."""
        result = normalize_id("  TASK-001  ")
        assert result == "task-001"
    
    def test_converts_to_lowercase(self):
        """Should convert to lowercase."""
        result = normalize_id("TASK-001")
        assert result == "task-001"


class TestEnsureItemFields:
    """Test ensure_item_fields function."""
    
    def test_adds_missing_fields(self):
        """Should add missing fields with defaults."""
        item = {"id": "test"}
        
        ensure_item_fields(item)
        
        assert "title" in item
        assert "status" in item
        assert "category" in item
        assert item["dependsOn"] == []
    
    def test_preserves_existing_fields(self):
        """Should not overwrite existing fields."""
        item = {"id": "test", "title": "Original", "status": "completed"}
        
        ensure_item_fields(item)
        
        assert item["title"] == "Original"
        assert item["status"] == "completed"
    
    def test_custom_defaults(self):
        """Should use custom defaults."""
        item = {"id": "test"}
        defaults = {"priority": "high"}
        
        ensure_item_fields(item, defaults=defaults)
        
        assert item["priority"] == "high"


class TestEnsureProvenance:
    """Test ensure_provenance function."""
    
    def test_adds_provenance_field(self):
        """Should add provenance field."""
        item = {"id": "TASK-001"}
        
        ensure_provenance(item, source="roadmap")
        
        assert "provenance" in item
        assert item["provenance"] == ["TASK-001|roadmap"]
    
    def test_preserves_existing_provenance(self):
        """Should not overwrite existing provenance."""
        item = {"id": "TASK-001", "provenance": ["existing"]}
        
        ensure_provenance(item, source="roadmap")
        
        assert item["provenance"] == ["existing"]


class TestDeduplicateItemsById:
    """Test deduplicate_items_by_id function."""
    
    def test_deduplicates_by_id(self):
        """Should merge items with same ID."""
        items = [
            {"id": "T1", "title": "First"},
            {"id": "T1", "title": "Second"}
        ]
        
        result = deduplicate_items_by_id(items)
        
        assert len(result) == 1
        assert result[0]["id"] == "T1"
    
    def test_merges_list_fields(self):
        """Should merge list fields."""
        items = [
            {"id": "T1", "dependsOn": ["A"], "tags": ["tag1"]},
            {"id": "T1", "dependsOn": ["B"], "tags": ["tag2"]}
        ]
        
        result = deduplicate_items_by_id(items)
        
        assert len(result) == 1
        assert set(result[0]["dependsOn"]) == {"A", "B"}
        assert set(result[0]["tags"]) == {"tag1", "tag2"}
    
    def test_skips_items_without_id(self):
        """Should skip items missing ID."""
        items = [
            {"id": "T1", "title": "Has ID"},
            {"title": "No ID"}
        ]
        
        result = deduplicate_items_by_id(items)
        
        assert len(result) == 1
        assert result[0]["id"] == "T1"


class TestMergeDependencies:
    """Test merge_dependencies function."""
    
    def test_merges_multiple_lists(self):
        """Should merge and deduplicate dependencies."""
        result = merge_dependencies(
            ["A", "B"],
            ["B", "C"],
            ["A", "D"]
        )
        
        assert set(result) == {"A", "B", "C", "D"}
        assert result == sorted(result)  # Should be sorted
    
    def test_handles_empty_lists(self):
        """Should handle empty lists."""
        result = merge_dependencies(["A"], [], ["B"])
        
        assert set(result) == {"A", "B"}
    
    def test_handles_none_values(self):
        """Should handle None values."""
        result = merge_dependencies(["A"], None, ["B"])
        
        assert set(result) == {"A", "B"}


class TestValidateItemStructure:
    """Test validate_item_structure function."""
    
    def test_valid_item_returns_empty(self):
        """Should return empty list for valid item."""
        item = {"id": "T1", "title": "Test", "status": "planned"}
        
        errors = validate_item_structure(item)
        
        assert errors == []
    
    def test_missing_required_field(self):
        """Should report missing required fields."""
        item = {"id": "T1", "title": "Test"}  # Missing status
        
        errors = validate_item_structure(item)
        
        assert len(errors) == 1
        assert "status" in errors[0]
    
    def test_empty_required_field(self):
        """Should report empty required fields."""
        item = {"id": "", "title": "Test", "status": "planned"}
        
        errors = validate_item_structure(item)
        
        assert len(errors) == 1
        assert "id" in errors[0]
    
    def test_invalid_list_field_type(self):
        """Should report type errors for list fields."""
        item = {"id": "T1", "title": "Test", "status": "planned", "tags": "not-a-list"}
        
        errors = validate_item_structure(item)
        
        assert len(errors) == 1
        assert "tags" in errors[0]
        assert "must be a list" in errors[0]


class TestCleanEmptyStrings:
    """Test clean_empty_strings function."""
    
    def test_removes_empty_strings_from_dict(self):
        """Should remove empty string values from dict."""
        data = {"a": "value", "b": "", "c": "other"}
        
        result = clean_empty_strings(data)
        
        assert result == {"a": "value", "c": "other"}
    
    def test_removes_empty_strings_from_list(self):
        """Should remove empty strings from list."""
        data = ["item1", "", "item2", ""]
        
        result = clean_empty_strings(data)
        
        assert result == ["item1", "item2"]
    
    def test_recursive_cleaning(self):
        """Should clean nested structures."""
        data = {
            "key": {"nested": "", "value": "ok"},
            "list": ["", "item", ""]
        }
        
        result = clean_empty_strings(data)
        
        assert result == {
            "key": {"value": "ok"},
            "list": ["item"]
        }
