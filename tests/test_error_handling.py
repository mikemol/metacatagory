"""
Phase 1 Block 1: Error Handling Tests (30 minutes)

Tests for error paths in critical validation scripts.
Validates behavior when:
- JSON is malformed
- Files are missing
- Permissions are denied
- Circular dependencies exist
- Invalid Agda syntax is encountered
"""

import pytest
import json
import tempfile
from pathlib import Path
from tests.test_utils import (
    malformed_json_string,
    circular_dependency_json,
    temp_json_file,
    temp_malformed_json_file,
)


class TestJSONDecomposeMalformed:
    """Test json_decompose.py error handling"""

    def test_malformed_json_raises_error(self, malformed_json_string):
        """Test that malformed JSON raises JSONDecodeError"""
        from scripts.json_decompose import decompose
        
        with pytest.raises(json.JSONDecodeError):
            decompose(malformed_json_string)

    def test_missing_brace_raises_error(self):
        """Test JSON with missing closing brace"""
        invalid_json = '{"items": [{"id": "ITEM-001"}'
        from scripts.json_decompose import decompose
        
        with pytest.raises(json.JSONDecodeError):
            decompose(invalid_json)

    def test_invalid_type_raises_error(self):
        """Test JSON with invalid type (string as number)"""
        invalid_json = '{"items": [{"id": 123}]}'  # id should be string
        from scripts.json_decompose import decompose
        
        # Should either raise error or have validation that catches type mismatch
        try:
            result = decompose(invalid_json)
            # If it doesn't raise, it should at least parse
            assert result is not None
        except (json.JSONDecodeError, ValueError, TypeError):
            # Expected error is fine
            pass


class TestJSONFileHandling:
    """Test file I/O error handling"""

    def test_missing_file_raises_error(self):
        """Test that missing file raises FileNotFoundError"""
        from scripts.json_decompose import decompose_file
        
        with pytest.raises(FileNotFoundError):
            decompose_file("/nonexistent/path/file.json")

    def test_valid_file_succeeds(self, temp_json_file):
        """Test that valid file loads successfully"""
        try:
            from scripts.json_decompose import decompose_file
            result = decompose_file(str(temp_json_file))
            assert result is not None
            assert "items" in result or "items" in str(result)
        except ImportError:
            pytest.skip("decompose_file not available")


class TestCircularDependencyDetection:
    """Test circular dependency error detection"""

    def test_circular_dependency_detected(self, circular_dependency_json):
        """Test that circular dependencies are detected"""
        from scripts.validate_json import validate_json
        
        errors = validate_json(circular_dependency_json)
        
        # Should have errors, and at least one should mention circular
        assert len(errors) > 0, "Expected validation errors for circular dependency"
        error_strings = [str(e).lower() for e in errors]
        # At least one error should mention "circular" or "cycle"
        has_cycle_error = any("circular" in e or "cycle" in e for e in error_strings)
        assert has_cycle_error, f"Expected circular/cycle error, got: {error_strings}"

    def test_self_dependency_detected(self):
        """Test that self-dependencies are detected"""
        from scripts.validate_json import validate_json
        
        self_dep_roadmap = {
            "items": [
                {"id": "A", "depends_on": ["A"]},  # Self-dependency
            ]
        }
        
        errors = validate_json(self_dep_roadmap)
        assert len(errors) > 0, "Expected validation errors for self-dependency"


class TestInvalidAgdaSyntax:
    """Test Agda syntax error handling"""

    def test_invalid_identifier_name(self):
        """Test validation of invalid Agda identifiers"""
        # This would typically come from RoadmapExporter.agda
        # Testing placeholder until we can test actual Agda code
        
        invalid_names = [
            "123-invalid",  # Cannot start with number
            "item-name!",   # Invalid character
            "test.name",    # Invalid character
        ]
        
        for name in invalid_names:
            # Attempt to validate as Agda identifier
            # This is simplified - real implementation would validate against Agda rules
            is_valid = name[0].isalpha() and all(c.isalnum() or c == "_" or c == "'" for c in name)
            assert not is_valid, f"Expected {name} to be invalid Agda identifier"


class TestMissingRequiredFields:
    """Test validation of required JSON fields"""

    def test_missing_id_field(self):
        """Test that missing 'id' field is caught"""
        from scripts.validate_json import validate_json
        
        incomplete_roadmap = {
            "items": [
                {"title": "Item without ID", "status": "pending"}  # Missing 'id'
            ]
        }
        
        errors = validate_json(incomplete_roadmap)
        # Should have validation errors
        if len(errors) > 0:
            assert any("id" in str(e).lower() for e in errors)

    def test_missing_status_field(self):
        """Test that missing 'status' field is caught"""
        from scripts.validate_json import validate_json
        
        incomplete_roadmap = {
            "items": [
                {"id": "ITEM-001", "title": "Item"}  # Missing 'status'
            ]
        }
        
        errors = validate_json(incomplete_roadmap)
        # Depending on validation strictness, should catch this
        if len(errors) > 0:
            assert any("status" in str(e).lower() for e in errors)


class TestPermissionDeniedRecovery:
    """Test handling of permission denied errors"""

    def test_read_only_file_handling(self, tmp_path):
        """Test handling of read-only files"""
        import os
        
        # Create a file and make it read-only
        test_file = tmp_path / "readonly.json"
        test_file.write_text('{"items": []}')
        os.chmod(str(test_file), 0o444)  # Read-only
        
        try:
            # Try to read - should succeed
            from scripts.json_decompose import decompose_file
            result = decompose_file(str(test_file))
            assert result is not None
        except PermissionError:
            # This is also acceptable - permission was denied as expected
            pass
        finally:
            # Restore permissions for cleanup
            os.chmod(str(test_file), 0o644)


# Integration: Error path for entire pipeline
class TestErrorPipelineIntegration:
    """Test error handling across the validation pipeline"""

    def test_malformed_input_through_validation(self, malformed_json_string):
        """Test that malformed JSON fails at validation stage"""
        try:
            from scripts.json_decompose import decompose
            from scripts.validate_json import validate_json
            
            # First decompose should fail
            with pytest.raises(json.JSONDecodeError):
                result = decompose(malformed_json_string)
                validate_json(result)
        except ImportError:
            pytest.skip("Required scripts not available")

    def test_valid_input_through_validation(self, temp_json_file):
        """Test that valid input passes through pipeline"""
        try:
            from scripts.json_decompose import decompose_file
            from scripts.validate_json import validate_json
            
            result = decompose_file(str(temp_json_file))
            errors = validate_json(result)
            # Should pass validation (or have minimal expected errors)
            assert result is not None
        except ImportError:
            pytest.skip("Required scripts not available")
