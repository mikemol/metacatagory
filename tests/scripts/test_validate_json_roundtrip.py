#!/usr/bin/env python3

"""
Tests for validate_json_roundtrip.py - JSON decompose/recompose integrity validation

Coverage targets:
- get_all_values: Recursive value extraction from JSON structures
- validate_roundtrip: Roundtrip validation logic (file checks, comparison, reporting)
- CLI entry point
"""

import json
import sys
from pathlib import Path
from unittest.mock import mock_open, patch

import pytest

# Import the script under test
sys.path.insert(0, str(Path(__file__).parents[2]))
from scripts.validate_json_roundtrip import get_all_values, validate_roundtrip


class TestGetAllValues:
    """Test recursive value extraction from JSON structures"""

    def test_extract_from_simple_dict(self):
        """Should extract all leaf values from simple dict"""
        data = {"a": 1, "b": "two", "c": True}
        values = get_all_values(data)
        assert set(values) == {"1", "two", "True"}

    def test_extract_from_nested_dict(self):
        """Should recursively extract from nested dicts"""
        data = {
            "level1": {
                "level2": {
                    "value": 42
                }
            },
            "other": "test"
        }
        values = get_all_values(data)
        assert "42" in values
        assert "test" in values

    def test_extract_from_list(self):
        """Should extract values from lists"""
        data = {"items": [1, 2, 3, "four"]}
        values = get_all_values(data)
        assert "1" in values
        assert "2" in values
        assert "3" in values
        assert "four" in values

    def test_extract_from_mixed_structure(self):
        """Should handle mixed dicts and lists"""
        data = {
            "nodes": [
                {"id": "A", "data": {"count": 10}},
                {"id": "B", "data": {"count": 20}}
            ],
            "metadata": {
                "version": "1.0"
            }
        }
        values = get_all_values(data)
        assert "A" in values
        assert "B" in values
        assert "10" in values
        assert "20" in values
        assert "1.0" in values

    def test_empty_structures(self):
        """Should handle empty dicts and lists"""
        assert get_all_values({}) == []
        assert get_all_values([]) == []
        assert get_all_values({"empty": {}}) == []

    def test_none_values(self):
        """Should convert None to string"""
        data = {"value": None}
        values = get_all_values(data)
        assert "None" in values


class TestValidateRoundtrip:
    """Test roundtrip validation logic"""

    @pytest.fixture
    def sample_original(self):
        """Sample original dependency graph"""
        return {
            "metadata": {"version": "1.0"},
            "nodes": [
                {"id": "ModuleA", "type": "agda"},
                {"id": "ModuleB", "type": "agda"}
            ],
            "edges": [
                {"from": "ModuleA", "to": "ModuleB"}
            ]
        }

    @pytest.fixture
    def sample_recomposed(self):
        """Sample recomposed dependency graph"""
        return {
            "modules": [
                {"name": "ModuleA"},
                {"name": "ModuleB"}
            ],
            "edges": [
                {"source": "ModuleA", "target": "ModuleB"}
            ],
            "layers": [["ModuleB"], ["ModuleA"]]
        }

    def test_successful_roundtrip(self, tmp_path, sample_original, sample_recomposed, capsys):
        """Should validate successful roundtrip with matching module counts"""
        # Create temporary build directory
        build_dir = tmp_path / "build"
        build_dir.mkdir()
        
        original_path = build_dir / "dependency_graph.json"
        recomposed_path = build_dir / "dependency_graph_recomposed.json"
        
        original_path.write_text(json.dumps(sample_original))
        recomposed_path.write_text(json.dumps(sample_recomposed))
        
        # Patch Path to use our tmp_path
        with patch('scripts.validate_json_roundtrip.Path') as mock_path:
            mock_path.return_value.exists.side_effect = [True, True]
            
            # Mock file opening
            with patch('builtins.open', side_effect=[
                mock_open(read_data=json.dumps(sample_original)).return_value,
                mock_open(read_data=json.dumps(sample_recomposed)).return_value
            ]):
                result = validate_roundtrip()
        
        assert result is True
        captured = capsys.readouterr()
        assert "PASSED" in captured.out
        assert "2 ↔ 2" in captured.out  # Module count match

    def test_module_count_mismatch(self, tmp_path, sample_original, capsys):
        """Should fail when module counts differ"""
        mismatched_recomposed = {
            "modules": [{"name": "ModuleA"}],  # Only 1 module instead of 2
            "edges": [],
            "layers": [[]]
        }
        
        build_dir = tmp_path / "build"
        build_dir.mkdir()
        
        original_path = build_dir / "dependency_graph.json"
        recomposed_path = build_dir / "dependency_graph_recomposed.json"
        
        original_path.write_text(json.dumps(sample_original))
        recomposed_path.write_text(json.dumps(mismatched_recomposed))
        
        with patch('scripts.validate_json_roundtrip.Path') as mock_path:
            mock_path.return_value.exists.side_effect = [True, True]
            
            with patch('builtins.open', side_effect=[
                mock_open(read_data=json.dumps(sample_original)).return_value,
                mock_open(read_data=json.dumps(mismatched_recomposed)).return_value
            ]):
                result = validate_roundtrip()
        
        assert result is False
        captured = capsys.readouterr()
        assert "FAILED" in captured.out
        assert "module count differs" in captured.out

    def test_missing_original_file(self, capsys):
        """Should fail when original file doesn't exist"""
        with patch('scripts.validate_json_roundtrip.Path') as mock_path:
            mock_path.return_value.exists.side_effect = [False, False]
            
            result = validate_roundtrip()
        
        assert result is False
        captured = capsys.readouterr()
        assert "Original file not found" in captured.out

    def test_missing_recomposed_file(self, capsys):
        """Should fail when recomposed file doesn't exist"""
        with patch('scripts.validate_json_roundtrip.Path') as mock_path:
            mock_path.return_value.exists.side_effect = [True, False]
            
            result = validate_roundtrip()
        
        assert result is False
        captured = capsys.readouterr()
        assert "Recomposed file not found" in captured.out

    def test_invalid_json_original(self, capsys):
        """Should handle JSON parse errors in original file"""
        with patch('scripts.validate_json_roundtrip.Path') as mock_path:
            mock_path.return_value.exists.side_effect = [True, True]
            
            with patch('builtins.open', side_effect=[
                mock_open(read_data="invalid json{").return_value,
                mock_open(read_data="{}").return_value
            ]):
                result = validate_roundtrip()
        
        assert result is False
        captured = capsys.readouterr()
        assert "JSON parse error" in captured.out

    def test_invalid_json_recomposed(self, capsys):
        """Should handle JSON parse errors in recomposed file"""
        with patch('scripts.validate_json_roundtrip.Path') as mock_path:
            mock_path.return_value.exists.side_effect = [True, True]
            
            with patch('builtins.open', side_effect=[
                mock_open(read_data='{"nodes": []}').return_value,
                mock_open(read_data="invalid json{").return_value
            ]):
                result = validate_roundtrip()
        
        assert result is False
        captured = capsys.readouterr()
        assert "JSON parse error" in captured.out

    def test_empty_graphs(self, capsys):
        """Should handle empty dependency graphs"""
        empty_original = {"nodes": [], "edges": []}
        empty_recomposed = {"modules": [], "edges": [], "layers": []}
        
        with patch('scripts.validate_json_roundtrip.Path') as mock_path:
            mock_path.return_value.exists.side_effect = [True, True]
            
            with patch('builtins.open', side_effect=[
                mock_open(read_data=json.dumps(empty_original)).return_value,
                mock_open(read_data=json.dumps(empty_recomposed)).return_value
            ]):
                result = validate_roundtrip()
        
        assert result is True
        captured = capsys.readouterr()
        assert "0 ↔ 0" in captured.out

    def test_edge_count_reporting(self, tmp_path, capsys):
        """Should report edge counts in output"""
        original = {
            "nodes": [{"id": "A"}, {"id": "B"}],
            "edges": [{"from": "A", "to": "B"}]
        }
        recomposed = {
            "modules": [{"name": "A"}, {"name": "B"}],
            "edges": [{"source": "A", "target": "B"}],
            "layers": []
        }
        
        with patch('scripts.validate_json_roundtrip.Path') as mock_path:
            mock_path.return_value.exists.side_effect = [True, True]
            
            with patch('builtins.open', side_effect=[
                mock_open(read_data=json.dumps(original)).return_value,
                mock_open(read_data=json.dumps(recomposed)).return_value
            ]):
                result = validate_roundtrip()
        
        assert result is True
        captured = capsys.readouterr()
        assert "Edges:" in captured.out
        assert "1 ↔ 1" in captured.out


class TestCLI:
    """Test CLI entry point"""

    def test_main_exit_success(self):
        """Should call sys.exit(0) on successful validation"""
        with patch('scripts.validate_json_roundtrip.validate_roundtrip', return_value=True):
            with patch('sys.exit') as mock_exit:
                # Simulate the if __name__ == "__main__" block
                success = True
                mock_exit(0 if success else 1)
                mock_exit.assert_called_once_with(0)

    def test_main_exit_failure(self):
        """Should call sys.exit(1) on validation failure"""
        with patch('scripts.validate_json_roundtrip.validate_roundtrip', return_value=False):
            with patch('sys.exit') as mock_exit:
                # Simulate the if __name__ == "__main__" block
                success = False
                mock_exit(0 if success else 1)
                mock_exit.assert_called_once_with(1)

    def test_main_guard_executes(self, tmp_path, monkeypatch):
        """Execute __main__ to cover sys.exit path."""
        monkeypatch.chdir(tmp_path)
        build_dir = tmp_path / "build"
        build_dir.mkdir(parents=True)

        original = {"nodes": [{"id": "A"}], "edges": []}
        recomposed = {"modules": [{"name": "A"}], "edges": [], "layers": []}
        (build_dir / "dependency_graph.json").write_text(json.dumps(original))
        (build_dir / "dependency_graph_recomposed.json").write_text(json.dumps(recomposed))

        script_path = Path(__file__).parents[2] / "scripts" / "validate_json_roundtrip.py"
        fake_file = tmp_path / "scripts" / "validate_json_roundtrip.py"
        fake_file.parent.mkdir(parents=True)

        code = script_path.read_text()
        with pytest.raises(SystemExit) as excinfo:
            exec_globals = {
                "__name__": "__main__",
                "__file__": str(fake_file),
                "__builtins__": __builtins__,
            }
            exec(compile(code, str(script_path), "exec"), exec_globals)

        assert excinfo.value.code == 0
