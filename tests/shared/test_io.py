#!/usr/bin/env python3
"""Tests for scripts/shared/io.py module."""

import json
import pytest
import sys
from pathlib import Path

# Import the module under test
sys.path.insert(0, str(Path(__file__).parent.parent.parent))
from scripts.shared.io import (
    load_json,
    save_json,
    load_markdown,
    save_markdown,
    ensure_file_exists,
    create_directory
)


class TestLoadJSON:
    """Test load_json function."""
    
    def test_load_existing_json(self, tmp_path):
        """Should load valid JSON file."""
        json_file = tmp_path / "test.json"
        data = {"key": "value", "number": 42}
        json_file.write_text(json.dumps(data))
        
        result = load_json(json_file)
        assert result == data
    
    def test_load_nonexistent_with_default(self, tmp_path):
        """Should return default for missing file."""
        result = load_json(tmp_path / "missing.json", default=[])
        assert result == []
    
    def test_load_nonexistent_raises(self, tmp_path):
        """Should raise FileNotFoundError for missing file without default."""
        with pytest.raises(FileNotFoundError):
            load_json(tmp_path / "missing.json")
    
    def test_load_malformed_json_raises(self, tmp_path):
        """Should raise JSONDecodeError for malformed JSON."""
        json_file = tmp_path / "bad.json"
        json_file.write_text("{invalid json")
        
        with pytest.raises(json.JSONDecodeError):
            load_json(json_file)
    
    def test_load_required_missing_exits(self, tmp_path, capsys):
        """Should exit with error for required missing file."""
        with pytest.raises(SystemExit) as excinfo:
            load_json(tmp_path / "missing.json", required=True)
        assert excinfo.value.code == 1
        captured = capsys.readouterr()
        assert "Required file not found" in captured.err


class TestSaveJSON:
    """Test save_json function."""
    
    def test_save_basic_json(self, tmp_path):
        """Should save JSON with default formatting."""
        output = tmp_path / "output.json"
        data = {"test": "data", "array": [1, 2, 3]}
        
        save_json(output, data)
        
        assert output.exists()
        with open(output) as f:
            loaded = json.load(f)
        assert loaded == data
    
    def test_save_creates_parent_dirs(self, tmp_path):
        """Should create parent directories automatically."""
        output = tmp_path / "nested" / "deep" / "output.json"
        data = {"created": True}
        
        save_json(output, data)
        
        assert output.exists()
        assert output.parent.exists()

    def test_save_handles_parent_file_conflict(self, tmp_path):
        """Should replace a parent file when a directory is required."""
        conflict = tmp_path / "nested"
        conflict.write_text("not a dir")
        output = conflict / "output.json"
        data = {"fixed": True}

        save_json(output, data)

        assert output.exists()
        with open(output) as f:
            loaded = json.load(f)
        assert loaded == data
    
    def test_save_custom_indent(self, tmp_path):
        """Should respect custom indentation."""
        output = tmp_path / "indented.json"
        data = {"key": "value"}
        
        save_json(output, data, indent=4)
        
        content = output.read_text()
        assert '    "key"' in content  # 4 spaces


class TestLoadMarkdown:
    """Test load_markdown function."""
    
    def test_load_existing_markdown(self, tmp_path):
        """Should load Markdown file."""
        md_file = tmp_path / "test.md"
        content = "# Title\n\nContent here"
        md_file.write_text(content)
        
        result = load_markdown(md_file)
        assert result == content
    
    def test_load_nonexistent_with_default(self, tmp_path):
        """Should return default for missing file."""
        result = load_markdown(tmp_path / "missing.md", default="")
        assert result == ""
    
    def test_load_preserves_utf8(self, tmp_path):
        """Should handle UTF-8 content correctly."""
        md_file = tmp_path / "unicode.md"
        content = "# Título\n\nCafé résumé 完成"
        md_file.write_text(content, encoding='utf-8')
        
        result = load_markdown(md_file)
        assert result == content


class TestSaveMarkdown:
    """Test save_markdown function."""
    
    def test_save_string_content(self, tmp_path):
        """Should save string content."""
        output = tmp_path / "output.md"
        content = "# Header\n\nParagraph"
        
        save_markdown(output, content)
        
        assert output.exists()
        assert output.read_text() == content
    
    def test_save_list_content(self, tmp_path):
        """Should join list of lines."""
        output = tmp_path / "output.md"
        lines = ["# Header", "", "Paragraph"]
        
        save_markdown(output, lines)
        
        assert output.read_text() == "# Header\n\nParagraph"
    
    def test_save_creates_parent_dirs(self, tmp_path):
        """Should create parent directories."""
        output = tmp_path / "nested" / "output.md"
        
        save_markdown(output, "content")
        
        assert output.exists()


class TestEnsureFileExists:
    """Test ensure_file_exists function."""
    
    def test_existing_file_returns_true(self, tmp_path):
        """Should return True for existing file."""
        test_file = tmp_path / "exists.txt"
        test_file.write_text("content")
        
        result = ensure_file_exists(test_file)
        assert result is True
    
    def test_missing_file_returns_false(self, tmp_path, capsys):
        """Should return False and print error for missing file."""
        result = ensure_file_exists(tmp_path / "missing.txt")
        
        assert result is False
        captured = capsys.readouterr()
        assert "File not found" in captured.err
    
    def test_custom_error_message(self, tmp_path, capsys):
        """Should use custom error message."""
        result = ensure_file_exists(
            tmp_path / "missing.txt",
            error_msg="Custom error",
            suggestion="Run make build"
        )
        
        assert result is False
        captured = capsys.readouterr()
        assert "Custom error" in captured.err
        assert "Run make build" in captured.err


class TestCreateDirectory:
    """Test create_directory function."""
    
    def test_create_simple_directory(self, tmp_path):
        """Should create directory."""
        new_dir = tmp_path / "newdir"
        
        result = create_directory(new_dir)
        
        assert new_dir.exists()
        assert new_dir.is_dir()
        assert result == new_dir
    
    def test_create_nested_directories(self, tmp_path):
        """Should create nested directories."""
        new_dir = tmp_path / "a" / "b" / "c"
        
        create_directory(new_dir)
        
        assert new_dir.exists()
    
    def test_existing_directory_ok(self, tmp_path):
        """Should not error on existing directory."""
        new_dir = tmp_path / "exists"
        new_dir.mkdir()
        
        create_directory(new_dir)  # Should not raise
        assert new_dir.exists()
