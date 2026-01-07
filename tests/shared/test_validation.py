"""Tests for shared.validation module."""

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


class TestValidationError:
    """Test ValidationError dataclass."""
    
    def test_basic_construction(self):
        """Test basic error construction."""
        error = ValidationFailure(
            path="items[0].title",
            constraint="required",
            value=None
        )
        
        assert error.path == "items[0].title"
        assert error.constraint == "required"
        assert error.value is None
        assert error.hint is None
    
    def test_with_hint(self):
        """Test error with hint."""
        error = ValidationFailure(
            path="status",
            constraint="one_of",
            value="invalid",
            hint="Must be one of: not-started, in-progress, completed"
        )
        
        assert error.hint == "Must be one of: not-started, in-progress, completed"
    
    def test_str_representation(self):
        """Test string representation."""
        error = ValidationFailure(
            path="title",
            constraint="non_empty",
            value=""
        )
        
        str_repr = str(error)
        assert "title" in str_repr
        assert "non_empty" in str_repr


class TestValidationResult:
    """Test ValidationResult class."""
    
    def test_valid_result(self):
        """Test valid validation result."""
        result = ValidationResult()
        
        assert result.is_valid()
        assert len(result.errors) == 0
    
    def test_invalid_result(self):
        """Test invalid validation result."""
        result = ValidationResult()
        result.add_error("path", "constraint", "value")
        
        assert not result.is_valid()
        assert len(result.errors) == 1
    
    def test_add_error(self):
        """Test adding errors."""
        result = ValidationResult()
        result.add_error("field1", "required", None)
        result.add_error("field2", "type", "wrong", hint="Expected int")
        
        assert len(result.errors) == 2
        assert result.errors[0].path == "field1"
        assert result.errors[1].hint == "Expected int"
    
    def test_merge_results(self):
        """Test merging validation results."""
        result1 = ValidationResult()
        result1.add_error("field1", "error1", None)
        
        result2 = ValidationResult()
        result2.add_error("field2", "error2", None)
        
        result1.merge(result2)
        
        assert len(result1.errors) == 2
        assert result1.errors[0].path == "field1"
        assert result1.errors[1].path == "field2"
    
    def test_raise_if_invalid_success(self):
        """Test raise_if_invalid on valid result."""
        result = ValidationResult()
        
        # Should not raise
        result.raise_if_invalid()
    
    def test_raise_if_invalid_failure(self):
        """Test raise_if_invalid on invalid result."""
        from scripts.shared.errors import ValidationError as VError
        
        result = ValidationResult()
        result.add_error("field", "constraint", "value")
        
        with pytest.raises(VError):
            result.raise_if_invalid()


class TestTypeValidator:
    """Test type_validator function."""
    
    def test_valid_type(self):
        """Test validation with correct type."""
        validator = type_validator(int)
        result = validator(42, "value")
        
        assert result.is_valid()
    
    def test_invalid_type(self):
        """Test validation with incorrect type."""
        validator = type_validator(int)
        result = validator("not an int", "value")
        
        assert not result.is_valid()
        assert len(result.errors) == 1
        assert result.errors[0].constraint == "type"
    
    def test_multiple_types(self):
        """Test validation with multiple allowed types."""
        validator = type_validator((int, str))
        
        result1 = validator(42, "value")
        assert result1.is_valid()
        
        result2 = validator("text", "value")
        assert result2.is_valid()
        
        result3 = validator(3.14, "value")
        assert not result3.is_valid()


class TestStringValidator:
    """Test string_validator function."""
    
    def test_valid_string(self):
        """Test validation of valid string."""
        validator = string_validator()
        result = validator("test", "field")
        
        assert result.is_valid()
    
    def test_non_empty_constraint(self):
        """Test non_empty constraint."""
        validator = string_validator(non_empty=True)
        
        result1 = validator("test", "field")
        assert result1.is_valid()
        
        result2 = validator("", "field")
        assert not result2.is_valid()
    
    def test_min_length_constraint(self):
        """Test min_length constraint."""
        validator = string_validator(min_length=3)
        
        result1 = validator("abc", "field")
        assert result1.is_valid()
        
        result2 = validator("ab", "field")
        assert not result2.is_valid()
    
    def test_max_length_constraint(self):
        """Test max_length constraint."""
        validator = string_validator(max_length=5)
        
        result1 = validator("test", "field")
        assert result1.is_valid()
        
        result2 = validator("too long", "field")
        assert not result2.is_valid()
    
    def test_pattern_constraint(self):
        """Test pattern (regex) constraint."""
        validator = string_validator(pattern=r"^[a-z]+$")
        
        result1 = validator("abc", "field")
        assert result1.is_valid()
        
        result2 = validator("ABC123", "field")
        assert not result2.is_valid()
    
    def test_combined_constraints(self):
        """Test multiple constraints together."""
        validator = string_validator(
            non_empty=True,
            min_length=3,
            max_length=10,
            pattern=r"^[a-z]+$"
        )
        
        result1 = validator("hello", "field")
        assert result1.is_valid()
        
        result2 = validator("", "field")
        assert not result2.is_valid()  # Empty
        
        result3 = validator("ab", "field")
        assert not result3.is_valid()  # Too short
        
        result4 = validator("verylongstring", "field")
        assert not result4.is_valid()  # Too long
        
        result5 = validator("ABC", "field")
        assert not result5.is_valid()  # Pattern mismatch


class TestListValidator:
    """Test list_validator function."""
    
    def test_valid_list(self):
        """Test validation of valid list."""
        validator = list_validator()
        result = validator([1, 2, 3], "items")
        
        assert result.is_valid()
    
    def test_item_validation(self):
        """Test validation of list items."""
        item_val = type_validator(int)
        validator = list_validator(item_validator=item_val)
        
        result1 = validator([1, 2, 3], "items")
        assert result1.is_valid()
        
        result2 = validator([1, "two", 3], "items")
        assert not result2.is_valid()
    
    def test_min_items_constraint(self):
        """Test min_items constraint."""
        validator = list_validator(min_items=2)
        
        result1 = validator([1, 2], "items")
        assert result1.is_valid()
        
        result2 = validator([1], "items")
        assert not result2.is_valid()
    
    def test_max_items_constraint(self):
        """Test max_items constraint."""
        validator = list_validator(max_items=3)
        
        result1 = validator([1, 2, 3], "items")
        assert result1.is_valid()
        
        result2 = validator([1, 2, 3, 4], "items")
        assert not result2.is_valid()
    
    def test_unique_constraint(self):
        """Test unique items constraint."""
        validator = list_validator(unique=True)
        
        result1 = validator([1, 2, 3], "items")
        assert result1.is_valid()
        
        result2 = validator([1, 2, 2, 3], "items")
        assert not result2.is_valid()


class TestDictValidator:
    """Test dict_validator function."""
    
    def test_valid_dict(self):
        """Test validation of valid dict."""
        validator = dict_validator()
        result = validator({"key": "value"}, "data")
        
        assert result.is_valid()
    
    def test_field_validators(self):
        """Test validation of specific fields."""
        validators = {
            "name": string_validator(non_empty=True),
            "age": type_validator(int)
        }
        validator = dict_validator(field_validators=validators)
        
        result1 = validator({"name": "Alice", "age": 30}, "person")
        assert result1.is_valid()
        
        result2 = validator({"name": "", "age": 30}, "person")
        assert not result2.is_valid()  # Empty name
        
        result3 = validator({"name": "Bob", "age": "thirty"}, "person")
        assert not result3.is_valid()  # Invalid age type
    
    def test_required_fields(self):
        """Test required fields constraint."""
        validator = dict_validator(required_fields=["name", "id"])
        
        result1 = validator({"name": "Test", "id": 123}, "data")
        assert result1.is_valid()
        
        result2 = validator({"name": "Test"}, "data")
        assert not result2.is_valid()  # Missing id
    
    def test_allow_extra_false(self):
        """Test disallowing extra fields."""
        validators = {"name": string_validator()}
        validator = dict_validator(
            field_validators=validators,
            allow_extra=False
        )
        
        result1 = validator({"name": "Test"}, "data")
        assert result1.is_valid()
        
        result2 = validator({"name": "Test", "extra": "field"}, "data")
        assert not result2.is_valid()
    
    def test_allow_extra_true(self):
        """Test allowing extra fields."""
        validators = {"name": string_validator()}
        validator = dict_validator(
            field_validators=validators,
            allow_extra=True
        )
        
        result = validator({"name": "Test", "extra": "field"}, "data")
        assert result.is_valid()


class TestOptionalValidator:
    """Test optional_validator function."""
    
    def test_none_value(self):
        """Test validation of None value."""
        inner = type_validator(int)
        validator = optional_validator(inner)
        
        result = validator(None, "field")
        assert result.is_valid()
    
    def test_valid_value(self):
        """Test validation of valid non-None value."""
        inner = type_validator(int)
        validator = optional_validator(inner)
        
        result = validator(42, "field")
        assert result.is_valid()
    
    def test_invalid_value(self):
        """Test validation of invalid non-None value."""
        inner = type_validator(int)
        validator = optional_validator(inner)
        
        result = validator("not int", "field")
        assert not result.is_valid()


class TestOneOfValidator:
    """Test one_of_validator function."""
    
    def test_valid_choice(self):
        """Test validation with valid choice."""
        validator = one_of_validator(["red", "green", "blue"])
        
        result = validator("red", "color")
        assert result.is_valid()
    
    def test_invalid_choice(self):
        """Test validation with invalid choice."""
        validator = one_of_validator(["red", "green", "blue"])
        
        result = validator("yellow", "color")
        assert not result.is_valid()
    
    def test_numeric_choices(self):
        """Test with numeric choices."""
        validator = one_of_validator([1, 2, 3])
        
        result1 = validator(2, "number")
        assert result1.is_valid()
        
        result2 = validator(4, "number")
        assert not result2.is_valid()


class TestCustomValidator:
    """Test custom_validator function."""
    
    def test_valid_predicate(self):
        """Test validation with valid predicate."""
        validator = custom_validator(
            lambda x: x > 0,
            "must be positive"
        )
        
        result = validator(5, "value")
        assert result.is_valid()
    
    def test_invalid_predicate(self):
        """Test validation with invalid predicate."""
        validator = custom_validator(
            lambda x: x > 0,
            "must be positive"
        )
        
        result = validator(-5, "value")
        assert not result.is_valid()
        assert "must be positive" in result.errors[0].constraint
    
    def test_complex_predicate(self):
        """Test with complex predicate."""
        validator = custom_validator(
            lambda x: isinstance(x, str) and len(x.split()) > 2,
            "must be multi-word string"
        )
        
        result1 = validator("hello world test", "text")
        assert result1.is_valid()
        
        result2 = validator("hello", "text")
        assert not result2.is_valid()


class TestCombineValidators:
    """Test combine_validators function."""
    
    def test_all_validators_pass(self):
        """Test when all validators pass."""
        validators = [
            type_validator(str),
            string_validator(min_length=3),
            string_validator(pattern=r"^[a-z]+$")
        ]
        combined = combine_validators(validators)
        
        result = combined("hello", "field")
        assert result.is_valid()
    
    def test_one_validator_fails(self):
        """Test when one validator fails."""
        validators = [
            type_validator(str),
            string_validator(min_length=10)
        ]
        combined = combine_validators(validators)
        
        result = combined("short", "field")
        assert not result.is_valid()
    
    def test_multiple_validators_fail(self):
        """Test when multiple validators fail."""
        validators = [
            string_validator(min_length=10),
            string_validator(pattern=r"^[0-9]+$")
        ]
        combined = combine_validators(validators)
        
        result = combined("abc", "field")
        assert not result.is_valid()
        assert len(result.errors) == 2


class TestRoadmapItemValidator:
    """Test roadmap_item_validator function."""
    
    def test_valid_roadmap_item(self):
        """Test validation of valid roadmap item."""
        item = {
            "id": "task-1",
            "title": "Implement feature",
            "status": "in-progress",
            "dependencies": ["task-0"],
            "rationale": "Required for next phase"
        }
        
        result = roadmap_item_validator(item, "items[0]")
        assert result.is_valid()
    
    def test_missing_required_field(self):
        """Test validation with missing required field."""
        item = {
            "id": "task-1",
            # Missing title
            "status": "not-started"
        }
        
        result = roadmap_item_validator(item, "items[0]")
        assert not result.is_valid()
    
    def test_invalid_status(self):
        """Test validation with invalid status."""
        item = {
            "id": "task-1",
            "title": "Test",
            "status": "invalid-status"
        }
        
        result = roadmap_item_validator(item, "items[0]")
        assert not result.is_valid()
    
    def test_empty_title(self):
        """Test validation with empty title."""
        item = {
            "id": "task-1",
            "title": "",
            "status": "not-started"
        }
        
        result = roadmap_item_validator(item, "items[0]")
        assert not result.is_valid()
    
    def test_optional_fields(self):
        """Test that optional fields are accepted."""
        item = {
            "id": "task-1",
            "title": "Minimal item",
            "status": "not-started"
            # No dependencies or rationale
        }
        
        result = roadmap_item_validator(item, "items[0]")
        assert result.is_valid()
    
    def test_invalid_dependencies_type(self):
        """Test validation with invalid dependencies type."""
        item = {
            "id": "task-1",
            "title": "Test",
            "status": "not-started",
            "dependencies": "not-a-list"
        }
        
        result = roadmap_item_validator(item, "items[0]")
        assert not result.is_valid()


class TestValidationIntegration:
    """Test validation integration patterns."""
    
    def test_nested_dict_validation(self):
        """Test validation of nested dictionaries."""
        address_validator = dict_validator(
            field_validators={
                "street": string_validator(non_empty=True),
                "city": string_validator(non_empty=True)
            },
            required_fields=["street", "city"]
        )
        
        person_validator = dict_validator(
            field_validators={
                "name": string_validator(non_empty=True),
                "address": address_validator
            },
            required_fields=["name", "address"]
        )
        
        valid_person = {
            "name": "Alice",
            "address": {
                "street": "123 Main St",
                "city": "Springfield"
            }
        }
        
        invalid_person = {
            "name": "Bob",
            "address": {
                "street": "456 Oak Ave"
                # Missing city
            }
        }
        
        result1 = person_validator(valid_person, "person")
        assert result1.is_valid()
        
        result2 = person_validator(invalid_person, "person")
        assert not result2.is_valid()
    
    def test_list_of_validated_items(self):
        """Test validation of list with complex items."""
        item_validator = dict_validator(
            field_validators={
                "name": string_validator(non_empty=True),
                "value": type_validator(int)
            },
            required_fields=["name", "value"]
        )
        
        list_val = list_validator(item_validator=item_validator, min_items=1)
        
        valid_list = [
            {"name": "item1", "value": 10},
            {"name": "item2", "value": 20}
        ]
        
        invalid_list = [
            {"name": "item1", "value": 10},
            {"name": "", "value": 20}  # Empty name
        ]
        
        result1 = list_val(valid_list, "items")
        assert result1.is_valid()
        
        result2 = list_val(invalid_list, "items")
        assert not result2.is_valid()
    
    def test_complex_roadmap_validation(self):
        """Test validation of complete roadmap structure."""
        roadmap_validator = dict_validator(
            field_validators={
                "version": string_validator(pattern=r"^\d+\.\d+$"),
                "items": list_validator(
                    item_validator=roadmap_item_validator,
                    min_items=1
                )
            },
            required_fields=["version", "items"]
        )
        
        valid_roadmap = {
            "version": "1.0",
            "items": [
                {
                    "id": "task-1",
                    "title": "First task",
                    "status": "completed"
                },
                {
                    "id": "task-2",
                    "title": "Second task",
                    "status": "in-progress",
                    "dependencies": ["task-1"]
                }
            ]
        }
        
        result = roadmap_validator(valid_roadmap, "roadmap")
        assert result.is_valid()
