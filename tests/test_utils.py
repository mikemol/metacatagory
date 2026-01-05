"""
Test utilities and fixtures for code coverage audit Phase 1

Provides common fixtures and helper functions for error handling,
smoke, and edge case tests.
"""

import pytest
import json
import tempfile
from pathlib import Path


def valid_roadmap_json_data():
    """Valid roadmap JSON for testing (function, not fixture)"""
    return {
        "items": [
            {
                "id": "ITEM-001",
                "title": "Test Item",
                "status": "pending",
                "depends_on": []
            },
            {
                "id": "ITEM-002",
                "title": "Dependent Item",
                "status": "pending",
                "depends_on": ["ITEM-001"]
            }
        ]
    }


@pytest.fixture
def valid_roadmap_json():
    """Valid roadmap JSON for testing"""
    return valid_roadmap_json_data()


@pytest.fixture
def malformed_json_string():
    """Malformed JSON string (missing closing brace)"""
    return '{"items": [{"id": "ITEM-001"}'


@pytest.fixture
def circular_dependency_json():
    """Roadmap JSON with circular dependencies"""
    return {
        "items": [
            {"id": "A", "depends_on": ["B"]},
            {"id": "B", "depends_on": ["C"]},
            {"id": "C", "depends_on": ["A"]},  # Creates cycle
        ]
    }


@pytest.fixture
def unicode_roadmap_json():
    """Roadmap JSON with unicode characters"""
    return {
        "items": [
            {
                "id": "ITEM-café",
                "title": "Test with émojis 🎯",
                "description": "Chinese: 完成, Japanese: 完了",
                "status": "active"
            }
        ]
    }


@pytest.fixture
def temp_json_file(tmp_path, valid_roadmap_json):
    """Create a temporary JSON file with valid content"""
    file_path = tmp_path / "test_roadmap.json"
    file_path.write_text(json.dumps(valid_roadmap_json, indent=2))
    return file_path


@pytest.fixture
def temp_malformed_json_file(tmp_path, malformed_json_string):
    """Create a temporary JSON file with malformed content"""
    file_path = tmp_path / "malformed.json"
    file_path.write_text(malformed_json_string)
    return file_path


@pytest.fixture
def large_json_file(tmp_path):
    """Create a large JSON file for testing memory/performance"""
    large_roadmap = {
        "items": [
            {
                "id": f"ITEM-{i:06d}",
                "title": f"Task {i}",
                "status": "pending",
                "description": "x" * 1000,  # 1KB per item
                "depends_on": [f"ITEM-{max(0, i-1):06d}"] if i > 0 else []
            }
            for i in range(10000)  # 10MB file
        ]
    }
    file_path = tmp_path / "large_roadmap.json"
    file_path.write_text(json.dumps(large_roadmap, indent=2))
    return file_path


@pytest.fixture
def deeply_nested_json():
    """Create deeply nested JSON structure"""
    nested = {"value": "leaf"}
    for i in range(50):
        nested = {"level": i, "nested": nested}
    return nested


def load_fixture(name):
    """Load a fixture file from tests/fixtures directory"""
    fixture_path = Path(__file__).parent / "fixtures" / name
    if not fixture_path.exists():
        raise FileNotFoundError(f"Fixture {name} not found at {fixture_path}")
    return json.loads(fixture_path.read_text())


def create_mock_markdown_file(tmp_path, content):
    """Create a temporary markdown file"""
    file_path = tmp_path / "test_doc.md"
    file_path.write_text(content)
    return file_path


def create_mock_makefile(tmp_path, content):
    """Create a temporary Makefile for testing"""
    file_path = tmp_path / "Makefile"
    file_path.write_text(content)
    return file_path
