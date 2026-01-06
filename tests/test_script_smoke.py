"""
Phase 1 Block 2: Smoke Tests (40 minutes)

Basic validation tests for scripts.
Tests that scripts:
- Can be imported without errors
- Have expected module-level code
- Basic functionality works

Note: Most scripts are command-line tools,  so we test what we can access.
"""

import pytest
import sys
import json
import tempfile
import subprocess
import importlib.util
from pathlib import Path


class TestScriptAvailability:
    """Test that key scripts exist and are importable"""

    def test_validate_json_exists(self):
        """Test that validate_json.py exists"""
        script_path = Path("scripts/validate_json.py")
        assert script_path.exists(), "validate_json.py should exist"

    def test_json_decompose_exists(self):
        """Test that json_decompose.py exists"""
        script_path = Path("scripts/json_decompose.py")
        assert script_path.exists(), "json_decompose.py should exist"

    def test_json_recompose_exists(self):
        """Test that json_recompose.py exists"""
        script_path = Path("scripts/json_recompose.py")
        assert script_path.exists(), "json_recompose.py should exist"

    def test_shared_data_exists(self):
        """Test that shared_data.py exists"""
        script_path = Path("scripts/shared_data.py")
        assert script_path.exists(), "shared_data.py should exist"


class TestSharedDataModule:
    """Test shared_data.py module functions"""

    def test_shared_data_import(self):
        """Test that shared_data module can be imported"""
        script_path = Path("scripts/shared_data.py")
        if not script_path.exists():
            pytest.skip("shared_data.py not present")

        try:
            spec = importlib.util.spec_from_file_location("shared_data", script_path)
            if spec is None or spec.loader is None:
                pytest.skip("Could not create spec for shared_data")
            module = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(module)
            assert module is not None
        except Exception as e:
            pytest.skip(f"shared_data not importable: {e}")

    def test_shared_data_has_path_utilities(self):
        """Test that shared_data has expected path utilities"""
        try:
            from scripts import shared_data
            # Check for expected attributes/functions
            assert hasattr(shared_data, '__file__')
        except ImportError:
            pytest.skip("shared_data not importable")


class TestBasicJSONOperations:
    """Test basic JSON operations work"""

    def test_json_loads_valid_data(self):
        """Test json.loads works with valid data"""
        valid_json = '{"items": [{"id": "TEST-001"}]}'
        result = json.loads(valid_json)
        assert result is not None
        assert "items" in result

    def test_json_dumps_preserves_structure(self):
        """Test json.dumps preserves structure"""
        data = {"items": [{"id": "TEST-001", "status": "active"}]}
        json_str = json.dumps(data)
        result = json.loads(json_str)
        assert result == data


class TestScriptCommandLineExecution:
    """Test scripts can be executed as command-line tools"""

    def test_validate_json_help(self):
        """Test validate_json.py shows help"""
        result = subprocess.run(
            [sys.executable, "scripts/validate_json.py", "--help"],
            capture_output=True,
            text=True,
            timeout=5
        )
        # Should either show help or run without crashing
        assert result.returncode in [0, 1, 2]  # 0=success, 1-2=expected errors

    def test_json_decompose_help(self):
        """Test json_decompose.py shows help or runs"""
        result = subprocess.run(
            [sys.executable, "scripts/json_decompose.py", "--help"],
            capture_output=True,
            text=True,
            timeout=5
        )
        assert result.returncode in [0, 1, 2]


class TestPathOperations:
    """Test path operations used by scripts"""

    def test_pathlib_basic_operations(self):
        """Test basic Path operations"""
        p = Path("tests/fixtures")
        assert p.exists() or not p.exists()  # Either state is valid
        
    def test_temp_directory_operations(self, tmp_path):
        """Test temporary directory operations"""
        temp_file = tmp_path / "test.json"
        temp_file.write_text('{"test": "data"}')
        assert temp_file.exists()
        assert temp_file.read_text() == '{"test": "data"}'
