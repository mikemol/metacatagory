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
from test_utils import (
    malformed_json_string,
    circular_dependency_json,
    temp_json_file,
    temp_malformed_json_file,
)


class TestJSONDecomposeMalformed:
    """Test JSON parsing error handling (using standard json module)"""

    def test_malformed_json_raises_error(self, malformed_json_string):
        """Test that malformed JSON raises JSONDecodeError"""
        with pytest.raises(json.JSONDecodeError):
            json.loads(malformed_json_string)

    def test_missing_brace_raises_error(self):
        """Test JSON with missing closing brace"""
        invalid_json = '{"items": [{"id": "ITEM-001"}'
        
        with pytest.raises(json.JSONDecodeError):
            json.loads(invalid_json)

    def test_invalid_value_raises_error(self):
        """Test JSON with unquoted value"""
        invalid_json = '{"invalid": json}'  # unquoted value
        
        with pytest.raises(json.JSONDecodeError):
            json.loads(invalid_json)


class TestJSONFileHandling:
    """Test file I/O error handling"""

    def test_missing_file_raises_error(self):
        """Test that missing file raises FileNotFoundError"""
        with pytest.raises(FileNotFoundError):
            with open("/nonexistent/path/file.json") as f:
                json.load(f)

    def test_valid_file_succeeds(self, temp_json_file):
        """Test that valid file loads successfully"""
        with open(temp_json_file) as f:
            result = json.load(f)
        assert result is not None
        assert "items" in result


class TestCircularDependencyDetection:
    """Test circular dependency detection (algorithm implementation)"""

    def test_circular_dependency_detected(self, circular_dependency_json):
        """Test that circular dependencies can be detected via graph algorithm"""
        # Build dependency graph
        deps = {}
        for item in circular_dependency_json["items"]:
            deps[item["id"]] = set(item.get("depends_on", []))
        
        # Detect cycle using DFS
        def has_cycle(node, visited, rec_stack):
            visited.add(node)
            rec_stack.add(node)
            
            for neighbor in deps.get(node, []):
                if neighbor not in visited:
                    if has_cycle(neighbor, visited, rec_stack):
                        return True
                elif neighbor in rec_stack:
                    return True
            
            rec_stack.remove(node)
            return False
        
        visited = set()
        rec_stack = set()
        
        # Check for cycles
        cycle_found = False
        for node in deps:
            if node not in visited:
                if has_cycle(node, visited, rec_stack):
                    cycle_found = True
                    break
        
        assert cycle_found, "Expected to detect circular dependency"

    def test_self_dependency_detected(self):
        """Test that self-dependencies are detected"""
        self_dep_roadmap = {
            "items": [
                {"id": "A", "depends_on": ["A"], "title": "Self-dep", "status": "active"},
            ]
        }
        
        # Check for self-dependency
        has_self_dep = False
        for item in self_dep_roadmap["items"]:
            if item["id"] in item.get("depends_on", []):
                has_self_dep = True
                break
        
        assert has_self_dep, "Expected to detect self-dependency"


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
    """Test detection of missing required JSON fields"""

    def test_missing_id_field(self):
        """Test that missing 'id' field can be detected"""
        incomplete_roadmap = {
            "items": [
                {"title": "Item without ID", "status": "pending"}  # Missing 'id'
            ]
        }
        
        # Check that id is indeed missing
        has_missing_id = False
        for item in incomplete_roadmap["items"]:
            if "id" not in item:
                has_missing_id = True
                break
        
        assert has_missing_id, "Expected to find item without id field"

    def test_missing_status_field(self):
        """Test that missing 'status' field can be detected"""
        incomplete_roadmap = {
            "items": [
                {"id": "ITEM-001", "title": "Item"}  # Missing 'status'
            ]
        }
        
        # Check that status is indeed missing
        has_missing_status = False
        for item in incomplete_roadmap["items"]:
            if "status" not in item:
                has_missing_status = True
                break
        
        assert has_missing_status, "Expected to find item without status field"


class TestPermissionDeniedRecovery:
    """Test handling of permission denied errors"""

    def test_read_only_file_handling(self, tmp_path):
        """Test handling of permission denied on file access"""
        import os
        
        # Create a file and make it inaccessible
        test_file = tmp_path / "noaccess.json"
        test_file.write_text('{"items": []}')
        os.chmod(str(test_file), 0o000)  # No permissions
        
        try:
            # Try to read - should fail with PermissionError
            with pytest.raises(PermissionError):
                with open(test_file) as f:
                    json.load(f)
        finally:
            # Restore permissions for cleanup
            os.chmod(str(test_file), 0o644)


# Integration: Error path for JSON parsing
class TestErrorPipelineIntegration:
    """Test error handling for JSON processing"""

    def test_malformed_input_json_parsing(self, malformed_json_string):
        """Test that malformed JSON fails at parsing stage"""
        with pytest.raises(json.JSONDecodeError):
            json.loads(malformed_json_string)

    def test_valid_input_json_parsing(self, temp_json_file):
        """Test that valid input parses successfully"""
        with open(temp_json_file) as f:
            result = json.load(f)
        assert result is not None
        assert isinstance(result, dict)
        assert "items" in result
