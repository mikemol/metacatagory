#!/usr/bin/env python3
"""Expanded test suite for validation.py - First-Order Implications.

Tests focus on:
1. Type Safety in Validators
2. Error Accumulation & Causality
3. Serialization for Provenance

Source: validation.py audit, First-Order Implications
"""

import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

import pytest
from scripts.shared.validation import (
    ValidationFailure,
    ValidationResult,
    type_validator,
    string_validator,
    list_validator,
    dict_validator,
    optional_validator,
    one_of_validator,
    custom_validator,
    combine_validators,
    roadmap_item_validator,
)
from scripts.shared.errors import ValidationError


class TestValidationFailureSerialization:
    """First-Order: ValidationFailure serialization and causality"""
    
    def test_validation_failure_to_dict(self):
        """Test ValidationFailure serializes to dict."""
        failure = ValidationFailure(
            path="items[0].status",
            constraint="must be one of: pending, done",
            value="invalid",
            hint="Use 'pending' or 'done'"
        )
        
        d = failure.to_dict()
        
        assert d['path'] == "items[0].status"
        assert d['constraint'] == "must be one of: pending, done"
        assert d['value'] == "invalid"
        assert d['hint'] == "Use 'pending' or 'done'"
        assert d['caused_by'] is None
    
    def test_validation_failure_causality_chain(self):
        """Test ValidationFailure supports causality chains."""
        cause = ValidationFailure(
            path="config.port",
            constraint="must be integer",
            value="not_a_number"
        )
        
        effect = ValidationFailure(
            path="config",
            constraint="invalid configuration",
            value=None,
            caused_by=cause
        )
        
        assert effect.caused_by is cause
        d = effect.to_dict()
        assert d['caused_by'] is not None
        assert d['caused_by']['path'] == "config.port"


class TestValidationResultSerialization:
    """First-Order: ValidationResult accumulation and serialization"""
    
    def test_validation_result_to_dict(self):
        """Test ValidationResult serializes to dict."""
        result = ValidationResult()
        result.add_error("field1", "required", hint="Add field1")
        result.add_error("field2", "type", value=123)
        
        d = result.to_dict()
        
        assert d['is_valid'] is False
        assert d['error_count'] == 2
        assert len(d['errors']) == 2
        assert d['errors'][0]['path'] == "field1"
        assert d['errors'][1]['path'] == "field2"
    
    def test_validation_result_empty(self):
        """Test ValidationResult with no errors."""
        result = ValidationResult()
        
        assert result.is_valid()
        d = result.to_dict()
        assert d['is_valid'] is True
        assert d['error_count'] == 0
        assert d['errors'] == []
    
    def test_raise_if_invalid_uses_errors_module(self):
        """Test raise_if_invalid uses errors.ValidationError."""
        result = ValidationResult()
        result.add_error("field", "required")
        
        with pytest.raises(ValidationError) as exc_info:
            result.raise_if_invalid("Test validation")
        
        assert "Test validation" in str(exc_info.value)
        # ValidationError should have context with constraint count in hint
        assert exc_info.value.context['hint'] == "1 constraint(s) violated"


class TestTypeValidator:
    """First-Order: Type safety validation"""
    
    def test_type_validator_accepts_correct_type(self):
        """Test type_validator accepts correct type."""
        validator = type_validator(str)
        result = validator("hello", "field")
        
        assert result.is_valid()
    
    def test_type_validator_rejects_wrong_type(self):
        """Test type_validator rejects wrong type."""
        validator = type_validator(str)
        result = validator(123, "field")
        
        assert not result.is_valid()
        assert len(result.errors) == 1
        assert result.errors[0].path == "field"
        assert result.errors[0].constraint == "type"
    
    def test_type_validator_accepts_tuple_of_types(self):
        """Test type_validator accepts multiple types."""
        validator = type_validator((str, int))
        
        assert validator("hello", "field").is_valid()
        assert validator(123, "field").is_valid()
        assert not validator([], "field").is_valid()


class TestStringValidator:
    """First-Order: String constraint validation"""
    
    def test_string_validator_min_length(self):
        """Test string_validator enforces min_length."""
        validator = string_validator(min_length=5)
        
        assert validator("hello", "field").is_valid()
        assert not validator("hi", "field").is_valid()
    
    def test_string_validator_max_length(self):
        """Test string_validator enforces max_length."""
        validator = string_validator(max_length=10)
        
        assert validator("short", "field").is_valid()
        assert not validator("this is too long", "field").is_valid()
    
    def test_string_validator_non_empty(self):
        """Test string_validator enforces non_empty."""
        validator = string_validator(non_empty=True)
        
        assert validator("text", "field").is_valid()
        assert not validator("", "field").is_valid()
    
    def test_string_validator_pattern(self):
        """Test string_validator enforces regex pattern."""
        validator = string_validator(pattern=r'^[a-z]+$')
        
        assert validator("abc", "field").is_valid()
        assert not validator("ABC", "field").is_valid()
        assert not validator("abc123", "field").is_valid()


class TestListValidator:
    """First-Order: List constraint validation"""
    
    def test_list_validator_min_items(self):
        """Test list_validator enforces min_items."""
        validator = list_validator(min_items=2)
        
        assert validator([1, 2], "field").is_valid()
        assert not validator([1], "field").is_valid()
    
    def test_list_validator_max_items(self):
        """Test list_validator enforces max_items."""
        validator = list_validator(max_items=3)
        
        assert validator([1, 2, 3], "field").is_valid()
        assert not validator([1, 2, 3, 4], "field").is_valid()
    
    def test_list_validator_unique(self):
        """Test list_validator enforces unique items."""
        validator = list_validator(unique=True)
        
        assert validator([1, 2, 3], "field").is_valid()
        assert not validator([1, 2, 2], "field").is_valid()
    
    def test_list_validator_item_validator(self):
        """Test list_validator validates each item."""
        validator = list_validator(item_validator=type_validator(int))
        
        assert validator([1, 2, 3], "field").is_valid()
        result = validator([1, "bad", 3], "field")
        assert not result.is_valid()
        assert "field[1]" in result.errors[0].path


class TestDictValidator:
    """First-Order: Dict/object constraint validation"""
    
    def test_dict_validator_required_fields(self):
        """Test dict_validator enforces required fields."""
        validator = dict_validator(required_fields=["name", "age"])
        
        assert validator({"name": "Alice", "age": 30}, "obj").is_valid()
        result = validator({"name": "Bob"}, "obj")
        assert not result.is_valid()
        assert any("age" in e.path for e in result.errors)
    
    def test_dict_validator_field_validators(self):
        """Test dict_validator validates each field."""
        validator = dict_validator(
            field_validators={
                "name": string_validator(non_empty=True),
                "age": type_validator(int)
            }
        )
        
        assert validator({"name": "Alice", "age": 30}, "obj").is_valid()
        result = validator({"name": "", "age": "not_int"}, "obj")
        assert not result.is_valid()
        assert len(result.errors) >= 2
    
    def test_dict_validator_allow_extra(self):
        """Test dict_validator allows extra fields when configured."""
        validator = dict_validator(
            field_validators={"name": string_validator()},
            allow_extra=True
        )
        
        assert validator({"name": "Alice", "extra": "data"}, "obj").is_valid()
    
    def test_dict_validator_reject_extra(self):
        """Test dict_validator rejects extra fields when configured."""
        validator = dict_validator(
            field_validators={"name": string_validator()},
            allow_extra=False
        )
        
        result = validator({"name": "Alice", "extra": "data"}, "obj")
        assert not result.is_valid()
        assert any("unexpected fields" in e.constraint for e in result.errors)


class TestOptionalValidator:
    """First-Order: Optional field handling"""
    
    def test_optional_validator_accepts_none(self):
        """Test optional_validator accepts None."""
        validator = optional_validator(string_validator(non_empty=True))
        
        assert validator(None, "field").is_valid()
    
    def test_optional_validator_validates_non_none(self):
        """Test optional_validator validates non-None values."""
        validator = optional_validator(string_validator(non_empty=True))
        
        assert validator("text", "field").is_valid()
        assert not validator("", "field").is_valid()


class TestOneOfValidator:
    """First-Order: Enum-like constraint validation"""
    
    def test_one_of_validator_accepts_allowed_values(self):
        """Test one_of_validator accepts allowed values."""
        validator = one_of_validator("pending", "done", "blocked")
        
        assert validator("pending", "status").is_valid()
        assert validator("done", "status").is_valid()
        assert not validator("invalid", "status").is_valid()
    
    def test_one_of_validator_with_list(self):
        """Test one_of_validator accepts list of values."""
        validator = one_of_validator(["pending", "done", "blocked"])
        
        assert validator("pending", "status").is_valid()
        assert not validator("invalid", "status").is_valid()


class TestCustomValidator:
    """First-Order: Custom predicate validation"""
    
    def test_custom_validator_predicate(self):
        """Test custom_validator uses predicate function."""
        validator = custom_validator(
            lambda x: x > 0,
            "must be positive"
        )
        
        assert validator(10, "field").is_valid()
        assert not validator(-5, "field").is_valid()
    
    def test_custom_validator_error_message(self):
        """Test custom_validator includes error message."""
        validator = custom_validator(
            lambda x: x % 2 == 0,
            "must be even"
        )
        
        result = validator(3, "field")
        assert not result.is_valid()
        assert "must be even" in result.errors[0].constraint


class TestCombineValidators:
    """Second-Order: Validator composition"""
    
    def test_combine_validators_all_must_pass(self):
        """Test combine_validators requires all validators to pass."""
        validator = combine_validators(
            type_validator(str),
            string_validator(min_length=3, max_length=10)
        )
        
        assert validator("hello", "field").is_valid()
        assert not validator("hi", "field").is_valid()  # Too short
        assert not validator(123, "field").is_valid()  # Wrong type
    
    def test_combine_validators_accumulates_errors(self):
        """Test combine_validators accumulates all errors."""
        validator = combine_validators(
            string_validator(min_length=5),
            string_validator(pattern=r'^[a-z]+$')
        )
        
        result = validator("AB", "field")
        # Should have 2 errors: too short + pattern mismatch
        assert len(result.errors) >= 2


class TestRoadmapItemValidator:
    """Domain-specific: Roadmap item schema validation"""
    
    def test_roadmap_item_validator_valid_item(self):
        """Test roadmap_item_validator accepts valid item."""
        item = {
            "id": "task-001",
            "title": "Implement feature",
            "description": "Add new functionality",
            "status": "in-progress",
            "category": "development",
            "source": "requirements.md",
            "files": ["src/feature.py"],
            "tags": ["priority-high"],
            "dependencies": ["task-000"],
            "related": [],
            "provenance": []
        }
        
        result = roadmap_item_validator(item, "item")
        assert result.is_valid()
    
    def test_roadmap_item_validator_missing_required(self):
        """Test roadmap_item_validator rejects missing required fields."""
        item = {
            "description": "Missing id and title"
        }
        
        result = roadmap_item_validator(item, "item")
        assert not result.is_valid()
        # Should have errors for missing 'id' and 'title'
        paths = {e.path for e in result.errors}
        assert "item.id" in paths or "id" in paths
        assert "item.title" in paths or "title" in paths
    
    def test_roadmap_item_validator_invalid_status(self):
        """Test roadmap_item_validator rejects invalid status."""
        item = {
            "id": "task-001",
            "title": "Task",
            "status": "invalid-status"
        }
        
        result = roadmap_item_validator(item, "item")
        assert not result.is_valid()
        assert any("status" in e.path for e in result.errors)


class TestErrorAccumulation:
    """Second-Order: Error accumulation and merging"""
    
    def test_validation_result_merge(self):
        """Test ValidationResult.merge combines errors."""
        result1 = ValidationResult()
        result1.add_error("field1", "required")
        
        result2 = ValidationResult()
        result2.add_error("field2", "type")
        
        result1.merge(result2)
        
        assert len(result1.errors) == 2
        assert result1.errors[0].path == "field1"
        assert result1.errors[1].path == "field2"
    
    def test_validation_result_merge_with_prefix(self):
        """Test ValidationResult.merge adds path prefix."""
        result1 = ValidationResult()
        
        result2 = ValidationResult()
        result2.add_error("name", "required")
        
        result1.merge(result2, path_prefix="user")
        
        assert len(result1.errors) == 1
        assert result1.errors[0].path == "user.name"
