#!/usr/bin/env python3
"""Schema validation for metacatagory scripts.

Source: Third-order implications 1.4.1.1.1 through 1.4.3.3.3 from behavioral taxonomy
Purpose: Centralize data validation with schema-driven approach

Design Principles:
- Static + Runtime typing (mypy + runtime checks)
- Error accumulation (collect all errors before failing)
- Pluggable validators (custom validation functions)
- Schema-as-documentation (auto-generate examples)
"""

from typing import Any, Callable, Protocol, TypeVar, Union
from dataclasses import dataclass, field
from enum import Enum

T = TypeVar('T')


@dataclass
class ValidationFailure:
    """Represents a single validation failure.
    
    Source: Implications 3.2.3 (error context), 3.3.3 (recovery hints)
    Note: Named ValidationFailure to avoid collision with errors.ValidationError
    """
    path: str  # JSON path to invalid field (e.g., "items[2].status")
    constraint: str  # Which constraint was violated
    value: Any  # The invalid value
    hint: str | None = None  # Suggested correction
    caused_by: 'ValidationFailure | None' = None  # Causality chain
    
    def __str__(self) -> str:
        """Format validation error for display."""
        parts = [f"Validation failed at '{self.path}': {self.constraint}"]
        if self.value is not None:
            parts.append(f"(got: {repr(self.value)})")
        if self.hint:
            parts.append(f"Hint: {self.hint}")
        return " ".join(parts)
    
    def to_dict(self) -> dict[str, Any]:
        """Serialize to dictionary.
        
        Source: Implication 3.3.1 (provenance integration)
        """
        return {
            'path': self.path,
            'constraint': self.constraint,
            'value': str(self.value) if self.value is not None else None,
            'hint': self.hint,
            'caused_by': self.caused_by.to_dict() if self.caused_by else None
        }


class ValidationResult:
    """Result of validation operation.
    
    Source: Implication 2.2.2 (error accumulation)
    """
    
    def __init__(self, errors: list[ValidationFailure] | None = None):
        self.errors = errors or []
    
    def is_valid(self) -> bool:
        """Check if validation passed."""
        return len(self.errors) == 0
    
    def add_error(self, path: str, constraint: str, value: Any = None, hint: str = ""):
        """Add validation error."""
        self.errors.append(ValidationFailure(path, constraint, value, hint))
    
    def merge(self, other: 'ValidationResult', path_prefix: str = ""):
        """Merge another validation result.
        
        Source: Implication 1.3.2 (nested validation)
        """
        for error in other.errors:
            new_path = f"{path_prefix}.{error.path}" if path_prefix else error.path
            self.errors.append(ValidationFailure(new_path, error.constraint, error.value, error.hint))
    
    def raise_if_invalid(self, message: str = "Validation failed"):
        """Raise exception if validation failed.
        
        Source: Implication 2.1.1 (fail-fast pattern)
        """
        if not self.is_valid():
            from .errors import ValidationError
            error_details = "\n".join(str(e) for e in self.errors)
            raise ValidationError(
                f"{message}:\n{error_details}",
                field="validation",
                hint=f"{len(self.errors)} constraint(s) violated"
            )
    
    def to_dict(self) -> dict[str, Any]:
        """Serialize validation result.
        
        Source: Implication 3.3.1 (provenance integration)
        """
        return {
            'is_valid': self.is_valid(),
            'error_count': len(self.errors),
            'errors': [e.to_dict() for e in self.errors]
        }
    
    def __str__(self) -> str:
        """Format all errors."""
        if self.is_valid():
            return "Validation passed"
        return f"Validation failed with {len(self.errors)} error(s):\n" + \
               "\n".join(f"  - {e}" for e in self.errors)


# Type constraint validators
def type_validator(expected_type: type | tuple[type, ...]) -> Callable[[Any, str], ValidationResult]:
    """Create validator for type constraint.
    
    Source: Implication 1.1.2 (runtime type checking)
    
    Args:
        expected_type: Required type (str, int, list, dict, etc.)
    
    Returns:
        Validator function
    """
    def validate(value: Any, path: str = "") -> ValidationResult:
        result = ValidationResult()
        if not isinstance(value, expected_type):
            result.add_error(
                path,
                "type",
                value
            )
        return result
    return validate


def string_validator(
    min_length: int | None = None,
    max_length: int | None = None,
    pattern: str | None = None,
    non_empty: bool = False
) -> Callable[[Any, str], ValidationResult]:
    """Create validator for string constraints.
    
    Source: Implications 1.2.1-1.2.3 (value constraints)
    
    Args:
        min_length: Minimum string length
        max_length: Maximum string length
        pattern: Regex pattern the string must match
        non_empty: Require non-empty string
    
    Returns:
        Validator function
    """
    import re
    
    def validate(value: Any, path: str = "") -> ValidationResult:
        result = ValidationResult()
        
        # Check type first
        if not isinstance(value, str):
            result.add_error(path, "must be string", value)
            return result
        
        # Check non-empty
        if non_empty and not value:
            result.add_error(path, "must not be empty", value, "Provide a non-empty string")
        
        # Check length constraints
        if min_length is not None and len(value) < min_length:
            result.add_error(
                path,
                f"must be at least {min_length} characters",
                value,
                f"Current length: {len(value)}"
            )
        
        if max_length is not None and len(value) > max_length:
            result.add_error(
                path,
                f"must be at most {max_length} characters",
                value,
                f"Current length: {len(value)}"
            )
        
        # Check pattern
        if pattern and not re.match(pattern, value):
            result.add_error(
                path,
                f"must match pattern: {pattern}",
                value
            )
        
        return result
    
    return validate


def list_validator(
    item_validator: Callable[[Any, str], ValidationResult] | None = None,
    min_items: int | None = None,
    max_items: int | None = None,
    unique: bool = False
) -> Callable[[Any, str], ValidationResult]:
    """Create validator for list constraints.
    
    Source: Implications 1.2.1-1.2.3 (value constraints), 1.3.2 (nested validation)
    
    Args:
        item_validator: Validator for each list item
        min_items: Minimum number of items
        max_items: Maximum number of items
        unique: Require all items to be unique
    
    Returns:
        Validator function
    """
    def validate(value: Any, path: str = "") -> ValidationResult:
        result = ValidationResult()
        
        # Check type first
        if not isinstance(value, list):
            result.add_error(path, "must be list", value)
            return result
        
        # Check length constraints
        if min_items is not None and len(value) < min_items:
            result.add_error(
                path,
                f"must have at least {min_items} items",
                value,
                f"Current count: {len(value)}"
            )
        
        if max_items is not None and len(value) > max_items:
            result.add_error(
                path,
                f"must have at most {max_items} items",
                value,
                f"Current count: {len(value)}"
            )
        
        # Check uniqueness
        if unique and len(value) != len(set(str(v) for v in value)):
            result.add_error(path, "must have unique items", value)
        
        # Validate each item
        if item_validator:
            for i, item in enumerate(value):
                item_path = f"{path}[{i}]"
                item_result = item_validator(item, item_path)
                result.merge(item_result)
        
        return result
    
    return validate


def dict_validator(
    field_validators: dict[str, Callable[[Any, str], ValidationResult]] | None = None,
    required_fields: list[str] | None = None,
    allow_extra: bool = True
) -> Callable[[Any, str], ValidationResult]:
    """Create validator for dictionary/object constraints.
    
    Source: Implications 1.3.1-1.3.3 (structural constraints)
    
    Args:
        field_validators: Validators for each field
        required_fields: List of required field names
        allow_extra: Allow fields not in field_validators
    
    Returns:
        Validator function
    """
    def validate(value: Any, path: str = "") -> ValidationResult:
        result = ValidationResult()
        
        # Check type first
        if not isinstance(value, dict):
            result.add_error(path, "must be dict", value)
            return result
        
        # Check required fields
        if required_fields:
            for field in required_fields:
                if field not in value:
                    field_path = f"{path}.{field}" if path else field
                    result.add_error(
                        field_path,
                        "required field missing",
                        None,
                        f"Add '{field}' field"
                    )
        
        # Check for extra fields
        if not allow_extra:
            fv_keys = set((field_validators or {}).keys())
            extra_fields = set(value.keys()) - fv_keys
            if extra_fields:
                result.add_error(
                    path,
                    f"unexpected fields: {', '.join(extra_fields)}",
                    list(extra_fields)
                )
        
        # Validate each field
        for field, validator in (field_validators or {}).items():
            if field in value:
                field_path = f"{path}.{field}" if path else field
                field_result = validator(value[field], field_path)
                result.merge(field_result)
        
        return result
    
    return validate


def optional_validator(
    validator: Callable[[Any, str], ValidationResult],
    default: Any = None
) -> Callable[[Any, str], ValidationResult]:
    """Make a validator optional (None is allowed).
    
    Source: Implication 1.3.1 (required vs. optional fields)
    
    Args:
        validator: Base validator
        default: Default value if None
    
    Returns:
        Validator that accepts None
    """
    def validate(value: Any, path: str = "") -> ValidationResult:
        if value is None:
            return ValidationResult()  # Valid
        return validator(value, path)
    
    return validate


def one_of_validator(*allowed_values: Any) -> Callable[[Any, str], ValidationResult]:
    """Create validator for enum-like constraints.
    
    Source: Implication 1.2.1 (range validation)
    
    Args:
        *allowed_values: Allowed values
    
    Returns:
        Validator function
    """
    def validate(value: Any, path: str = "") -> ValidationResult:
        result = ValidationResult()
        # Support passing a single list/tuple of values
        vals = allowed_values
        if len(vals) == 1 and isinstance(vals[0], (list, tuple, set)):
            vals = tuple(vals[0])
        if value not in vals:
            result.add_error(
                path,
                f"must be one of: {', '.join(repr(v) for v in vals)}",
                value,
                f"Choose from allowed values"
            )
        return result
    
    return validate


def custom_validator(
    predicate: Callable[[Any], bool],
    error_message: str
) -> Callable[[Any, str], ValidationResult]:
    """Create validator from custom predicate function.
    
    Source: Implication 1.2.2 (custom validation functions)
    
    Args:
        predicate: Function that returns True if valid
        error_message: Error message if predicate returns False
    
    Returns:
        Validator function
    """
    def validate(value: Any, path: str = "") -> ValidationResult:
        result = ValidationResult()
        if not predicate(value):
            result.add_error(path, error_message, value)
        return result
    
    return validate


def combine_validators(
    *validators: Callable[[Any, str], ValidationResult] | list[Callable[[Any, str], ValidationResult]]
) -> Callable[[Any, str], ValidationResult]:
    """Combine multiple validators (all must pass).
    
    Source: Implication 3.1.1 (validation composition)
    
    Args:
        *validators: Validators to combine
    
    Returns:
        Combined validator
    """
    def validate(value: Any, path: str = "") -> ValidationResult:
        result = ValidationResult()
        # Flatten if a single list/tuple was provided
        flat_validators: list[Callable[[Any, str], ValidationResult]]
        if len(validators) == 1 and isinstance(validators[0], (list, tuple)):
            flat_validators = list(validators[0])  # type: ignore[arg-type]
        else:
            flat_validators = list(validators)  # type: ignore[list-item]
        for validator in flat_validators:
            validator_result = validator(value, path)
            result.merge(validator_result)
        return result
    
    return validate


# Common schema validator for roadmap items (callable directly)
def roadmap_item_validator(value: Any, path: str = "") -> ValidationResult:
    """Validate RoadmapItem structure directly.
    
    Source: Domain-specific schema from merge_roadmaps.py
    """
    schema = dict_validator(
        field_validators={
            "id": string_validator(non_empty=True),
            "title": string_validator(non_empty=True),
            "description": string_validator(),
            "status": one_of_validator(["not-started", "in-progress", "completed", "blocked", "deferred", "done"]),
            "category": string_validator(),
            "source": string_validator(),
            "files": list_validator(item_validator=string_validator()),
            "tags": list_validator(item_validator=string_validator()),
            "dependencies": list_validator(item_validator=string_validator()),
            "related": list_validator(item_validator=string_validator()),
            "provenance": list_validator(item_validator=string_validator())
        },
        required_fields=["id", "title"],
        allow_extra=True
    )
    return schema(value, path)


def ingested_metadata_validator(value: Any, path: str = "") -> ValidationResult:
    """Validate ingested_metadata.json structure."""
    file_schema = dict_validator(
        field_validators={
            "title": string_validator(non_empty=True),
            "summary": string_validator(),
            "keywords": list_validator(item_validator=string_validator()),
            "insight": string_validator(),
            "gap": string_validator(),
            "fix": string_validator(),
            "target_module": string_validator(non_empty=True),
        },
        required_fields=["title", "summary", "target_module"],
        allow_extra=True,
    )

    def file_map_validator(value: Any, path: str = "") -> ValidationResult:
        result = ValidationResult()
        if not isinstance(value, dict):
            result.add_error(path, "must be dict", value)
            return result
        for key, item in value.items():
            item_path = f"{path}.{key}" if path else str(key)
            result.merge(file_schema(item, item_path))
        return result
    schema = dict_validator(
        field_validators={
            "total_files": type_validator(int),
            "files": file_map_validator,
        },
        required_fields=["total_files", "files"],
        allow_extra=True,
    )
    return schema(value, path)
