"""
Test utilities and common fixtures for code coverage tests.
"""
import pytest
import json
import tempfile
from pathlib import Path

# Fixture directory
FIXTURE_DIR = Path(__file__).parent / "fixtures"


@pytest.fixture
def valid_roadmap():
    """Load valid roadmap JSON for testing."""
    return load_fixture("valid_roadmap.json")


@pytest.fixture
def roadmap_with_cycles():
    """Load roadmap with circular dependencies."""
    return load_fixture("roadmap_with_cycles.json")


@pytest.fixture
def roadmap_with_unicode():
    """Load roadmap with unicode characters."""
    return load_fixture("roadmap_with_unicode.json")


@pytest.fixture
def temp_file():
    """Create a temporary file for testing."""
    with tempfile.NamedTemporaryFile(delete=False, mode='w') as f:
        yield f.name
    Path(f.name).unlink(missing_ok=True)


@pytest.fixture
def temp_dir():
    """Create a temporary directory for testing."""
    with tempfile.TemporaryDirectory() as d:
        yield Path(d)


def load_fixture(name):
    """Load a fixture file by name."""
    path = FIXTURE_DIR / name
    if path.suffix == '.json':
        return json.loads(path.read_text())
    return path.read_text()


def fixture_path(name):
    """Get the absolute path to a fixture file."""
    return str(FIXTURE_DIR / name)


@pytest.fixture
def malformed_json_string():
    """Return a malformed JSON string for error testing."""
    return '{"invalid": json, "missing": "quotes"}'


@pytest.fixture
def circular_dependency_json():
    """Return a roadmap with circular dependencies."""
    return {
        "items": [
            {"id": "A", "depends_on": ["B"], "title": "Item A", "status": "active"},
            {"id": "B", "depends_on": ["C"], "title": "Item B", "status": "active"},
            {"id": "C", "depends_on": ["A"], "title": "Item C", "status": "active"}
        ]
    }


@pytest.fixture
def temp_json_file(tmp_path):
    """Create a temporary JSON file with valid content."""
    temp_file = tmp_path / "test.json"
    content = {"items": [{"id": "TEST-001", "title": "Test", "status": "active"}]}
    temp_file.write_text(json.dumps(content))
    return temp_file


@pytest.fixture
def temp_malformed_json_file(tmp_path):
    """Create a temporary JSON file with malformed content."""
    temp_file = tmp_path / "malformed.json"
    temp_file.write_text('{"invalid": json}')
    return temp_file


@pytest.fixture
def unicode_roadmap_json():
    """Return a roadmap with unicode characters."""
    return {
        "items": [
            {
                "id": "UNICODE-001",
                "title": "café implementation 🎯",
                "description": "Test émojis and spécial çharacters",
                "status": "active"
            }
        ]
    }


@pytest.fixture
def deeply_nested_json():
    """Generate deeply nested JSON structure."""
    current = {"value": "leaf"}
    for i in range(50):
        current = {"level": i, "nested": current}
    return {"items": [{"id": "NESTED-001", "data": current}]}
