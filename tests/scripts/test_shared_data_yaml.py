#!/usr/bin/env python3
"""Tests for shared_data.py and shared_yaml.py"""

import builtins
import importlib
import json
import sys
import types
from pathlib import Path

import pytest

import scripts.shared_data as shared_data
import scripts.shared_yaml as shared_yaml


def test_load_planning_index_variants(tmp_path, monkeypatch):
    monkeypatch.setattr(shared_data, "REPO_ROOT", tmp_path)

    # Missing file raises
    with pytest.raises(FileNotFoundError):
        shared_data.load_planning_index()

    # List form
    json_path = tmp_path / "data" / "planning_index.json"
    json_path.parent.mkdir(parents=True, exist_ok=True)
    json_path.write_text(json.dumps([{"id": "GP-1"}]), encoding="utf-8")
    assert shared_data.load_planning_index()[0]["id"] == "GP-1"

    # Dict form
    json_path.write_text(json.dumps({"items": [{"id": "GP-2"}]}), encoding="utf-8")
    assert shared_data.load_planning_index()[0]["id"] == "GP-2"

    # Unexpected shape
    json_path.write_text(json.dumps("oops"), encoding="utf-8")
    with pytest.raises(ValueError):
        shared_data.load_planning_index()


def test_load_planning_index_from_path(tmp_path, monkeypatch):
    monkeypatch.setattr(shared_data, "REPO_ROOT", tmp_path)
    json_path = tmp_path / "data" / "planning_index.json"
    json_path.parent.mkdir(parents=True, exist_ok=True)
    json_path.write_text(json.dumps([{"id": "GP-1"}]), encoding="utf-8")

    items = shared_data.load_planning_index_from(json_path)
    assert items[0]["id"] == "GP-1"

def test_load_planning_index_filter_legacy(tmp_path, monkeypatch):
    monkeypatch.setattr(shared_data, "REPO_ROOT", tmp_path)
    json_path = tmp_path / "data" / "planning_index.json"
    json_path.parent.mkdir(parents=True, exist_ok=True)
    json_path.write_text(
        json.dumps([{"id": "GP-1"}, {"id": "LEGACY-1"}]),
        encoding="utf-8",
    )

    items = shared_data.load_planning_index(filter_legacy=True)
    assert [item["id"] for item in items] == ["GP-1"]


def test_load_planning_index_validated(tmp_path, monkeypatch):
    monkeypatch.setattr(shared_data, "REPO_ROOT", tmp_path)
    json_path = tmp_path / "data" / "planning_index.json"
    json_path.parent.mkdir(parents=True, exist_ok=True)

    assert shared_data.load_planning_index_validated(allow_missing=True) == []

    json_path.write_text(json.dumps([{"id": "GP-1", "title": "Title"}]), encoding="utf-8")
    items = shared_data.load_planning_index_validated()
    assert items[0]["id"] == "GP-1"

    json_path.write_text(json.dumps([{"id": "GP-2"}]), encoding="utf-8")
    from scripts.shared.errors import ValidationError

    with pytest.raises(ValidationError):
        shared_data.load_planning_index_validated()


def test_load_planning_index_validated_from(tmp_path, monkeypatch):
    monkeypatch.setattr(shared_data, "REPO_ROOT", tmp_path)
    json_path = tmp_path / "data" / "planning_index.json"
    json_path.parent.mkdir(parents=True, exist_ok=True)

    assert shared_data.load_planning_index_validated_from(json_path, allow_missing=True) == []

    json_path.write_text(json.dumps([{"id": "GP-1", "title": "Title"}]), encoding="utf-8")
    items = shared_data.load_planning_index_validated_from(json_path)
    assert items[0]["id"] == "GP-1"

    json_path.write_text(json.dumps([{"id": "GP-2"}]), encoding="utf-8")
    from scripts.shared.errors import ValidationError

    with pytest.raises(ValidationError):
        shared_data.load_planning_index_validated_from(json_path)


def test_resolve_planning_path_prefers_data(tmp_path, monkeypatch):
    monkeypatch.setattr(shared_data, "REPO_ROOT", tmp_path)
    data_path = tmp_path / "data" / "planning_index.json"
    data_path.parent.mkdir(parents=True, exist_ok=True)
    data_path.write_text(json.dumps([{"id": "GP-1"}]), encoding="utf-8")
    build_path = tmp_path / "build" / "planning_index.json"
    build_path.parent.mkdir(parents=True, exist_ok=True)
    build_path.write_text(json.dumps([{"id": "GP-2"}]), encoding="utf-8")

    resolved = shared_data.resolve_planning_path()
    assert resolved == data_path


def test_resolve_planning_path_falls_back_to_build(tmp_path, monkeypatch):
    monkeypatch.setattr(shared_data, "REPO_ROOT", tmp_path)
    build_path = tmp_path / "build" / "planning_index.json"
    build_path.parent.mkdir(parents=True, exist_ok=True)
    build_path.write_text(json.dumps([{"id": "GP-2"}]), encoding="utf-8")

    resolved = shared_data.resolve_planning_path()
    assert resolved == build_path


def test_resolve_planning_path_defaults_to_data_when_missing(tmp_path, monkeypatch):
    monkeypatch.setattr(shared_data, "REPO_ROOT", tmp_path)
    resolved = shared_data.resolve_planning_path()
    assert resolved == tmp_path / "data" / "planning_index.json"


def test_load_roadmap_markdown_parses_yaml(tmp_path, monkeypatch):
    monkeypatch.setattr(shared_data, "REPO_ROOT", tmp_path)
    md = tmp_path / "ROADMAP.md"
    md.write_text(
        """
```yaml
id: GP-1
status: in-progress
```
- **Title** — Desc [status: in-progress]
[GP-2]
""",
        encoding="utf-8",
    )

    ids, items = shared_data.load_roadmap_markdown()
    assert ids == ["GP-1", "GP-2"]
    assert items[0]["id"] == "GP-1"

    ids_from_path, items_from_path = shared_data.load_roadmap_markdown_from(md)
    assert ids_from_path == ["GP-1", "GP-2"]
    assert items_from_path[0]["id"] == "GP-1"


def test_load_roadmap_markdown_fallback_without_yaml(tmp_path, monkeypatch):
    monkeypatch.setattr(shared_data, "REPO_ROOT", tmp_path)
    md = tmp_path / "ROADMAP.md"
    md.write_text(
        """
```yaml
id: GP-3
status: not-started
```
""",
        encoding="utf-8",
    )

    real_import = builtins.__import__

    def raising_import(name, *args, **kwargs):
        if name == "yaml":
            raise ImportError("no yaml")
        return real_import(name, *args, **kwargs)

    monkeypatch.setattr(builtins, "__import__", raising_import)

    ids, items = shared_data.load_roadmap_markdown()
    assert ids == ["GP-3"]
    assert items[0]["id"] == "GP-3"


def test_load_roadmap_markdown_missing_file(tmp_path, monkeypatch):
    monkeypatch.setattr(shared_data, "REPO_ROOT", tmp_path)
    with pytest.raises(FileNotFoundError):
        shared_data.load_roadmap_markdown()


def test_load_tasks_json_variants(tmp_path, monkeypatch):
    monkeypatch.setattr(shared_data, "REPO_ROOT", tmp_path)
    tasks_path = tmp_path / ".github" / "roadmap" / "tasks.json"

    with pytest.raises(FileNotFoundError):
        shared_data.load_tasks_json()

    assert shared_data.load_tasks_json(required=False) == []

    tasks_path.parent.mkdir(parents=True, exist_ok=True)
    tasks_path.write_text(json.dumps([{"id": "TASK-1"}]), encoding="utf-8")
    assert shared_data.load_tasks_json()[0]["id"] == "TASK-1"

    tasks_path.write_text(json.dumps({"items": [{"id": "TASK-2"}]}), encoding="utf-8")
    assert shared_data.load_tasks_json()[0]["id"] == "TASK-2"

    tasks_path.write_text(json.dumps("oops"), encoding="utf-8")
    with pytest.raises(ValueError):
        shared_data.load_tasks_json()


def test_validate_roadmap_frontmatter():
    valid = [{"id": "GP-1", "title": "Alpha", "status": "in-progress"}]
    result = shared_data.validate_roadmap_frontmatter(valid)
    assert result.is_valid()

    invalid = [{"title": "Missing ID"}]
    result = shared_data.validate_roadmap_frontmatter(invalid)
    assert not result.is_valid()


def test_load_roadmap_markdown_yaml_error(tmp_path, monkeypatch):
    real_yaml = pytest.importorskip("yaml")
    monkeypatch.setattr(shared_data, "REPO_ROOT", tmp_path)

    md = tmp_path / "ROADMAP.md"
    md.write_text("""```yaml
bad: [
```""", encoding="utf-8")

    # Force YAML loader to raise YAMLError so the exception path is covered
    fake_yaml = types.SimpleNamespace(
        safe_load=lambda _: (_ for _ in ()).throw(real_yaml.YAMLError("boom")),
        YAMLError=real_yaml.YAMLError,
    )
    monkeypatch.setitem(sys.modules, "yaml", fake_yaml)

    ids, items = shared_data.load_roadmap_markdown()
    assert ids == []
    assert items == []


def test_dump_yaml_with_pyyaml():
    data = {"id": "GP-1", "tags": ["a", "b"]}
    out = shared_yaml.dump_yaml(data)
    assert "GP-1" in out
    assert "tags" in out


def test_dump_yaml_fallback(monkeypatch):
    monkeypatch.setattr(shared_yaml, "yaml", None)
    data = {"id": "GP-2", "items": ["a", "b"]}
    out = shared_yaml.dump_yaml(data)
    assert "id: GP-2" in out
    assert "  - a" in out


def test_import_fallback_sets_yaml_none(monkeypatch):
    real_import = builtins.__import__

    def raising_import(name, *args, **kwargs):
        if name == "yaml":
            raise ImportError("no yaml")
        return real_import(name, *args, **kwargs)

    monkeypatch.setattr(builtins, "__import__", raising_import)
    reloaded = importlib.reload(shared_yaml)
    assert reloaded.yaml is None

    # Restore import hook and module state
    monkeypatch.setattr(builtins, "__import__", real_import)
    importlib.reload(shared_yaml)


def test_normalization_helpers():
    assert shared_yaml.normalize_unicode("x") == "x"
    fm, js = shared_yaml.normalize_field_comparison(1, 2)
    assert fm == "1" and js == "2"
    fm_deps, js_deps = shared_yaml.normalize_dependencies(["a"], ["a", "b"])
    assert fm_deps == {"a"}
    assert js_deps == {"a", "b"}
