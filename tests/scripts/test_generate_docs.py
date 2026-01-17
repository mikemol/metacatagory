#!/usr/bin/env python3
"""Tests for generate_docs.py"""

import json
import runpy
from pathlib import Path

import pytest

import scripts.generate_docs as mod

ORIGINAL_FILE = mod.__file__


def test_load_planning_index_missing(tmp_path, capsys):
    mod.__file__ = str(tmp_path / "scripts" / "generate_docs.py")
    result = mod.load_planning_index()
    captured = capsys.readouterr()
    assert result == []
    assert "not found" in captured.err
    mod.__file__ = ORIGINAL_FILE


def test_load_planning_index_present(tmp_path):
    mod.__file__ = str(tmp_path / "scripts" / "generate_docs.py")
    plan = tmp_path / "data" / "planning_index.json"
    plan.parent.mkdir(parents=True, exist_ok=True)
    data = [
        {"id": "GP-2", "title": "Done", "status": "done", "description": "B"},
        {"id": "GP-1", "title": "In Progress", "status": "in-progress", "description": "Longer desc"},
    ]
    plan.write_text(json.dumps(data), encoding="utf-8")

    items = mod.load_planning_index()
    assert items[0]["id"] == "GP-1"  # in-progress prioritized
    assert len(items) == 2
    mod.__file__ = ORIGINAL_FILE


def test_load_planning_index_invalid_json(tmp_path, capsys):
    mod.__file__ = str(tmp_path / "scripts" / "generate_docs.py")
    plan = tmp_path / "data" / "planning_index.json"
    plan.parent.mkdir(parents=True, exist_ok=True)
    plan.write_text("oops", encoding="utf-8")

    result = mod.load_planning_index()
    captured = capsys.readouterr()
    assert result == []
    assert "Warning:" in captured.err
    assert "using empty roadmap list" in captured.err
    mod.__file__ = ORIGINAL_FILE


def test_generate_readme_and_main(tmp_path, monkeypatch):
    sample = [
        {"id": "GP-1", "title": "Alpha", "description": "Desc", "status": "in-progress", "category": "Cat", "files": ["a"], "tags": ["t"]},
    ]
    mod.ROADMAPS = [mod.format_roadmap_for_readme(item) for item in sample]

    readme = mod.generate_readme()
    assert "Alpha" in readme
    assert "GP-1" in readme

    monkeypatch.chdir(tmp_path)
    mod.ROADMAPS = [mod.format_roadmap_for_readme(item) for item in sample]
    rc = mod.main()
    assert rc == 0
    assert (tmp_path / "README.md").exists()
    assert (tmp_path / "CONTRIBUTING.md").exists()
    assert (tmp_path / "NAVIGATION.md").exists()


def test_format_roadmap_for_readme_defaults():
    out = mod.format_roadmap_for_readme({})
    assert out["id"] == "unknown"
    assert out["status"] == "not-started"


def test_main_guard_runs(tmp_path, monkeypatch):
    monkeypatch.chdir(tmp_path)

    mod.__file__ = ORIGINAL_FILE

    with pytest.raises(SystemExit) as exc:
        runpy.run_path(mod.__file__, run_name="__main__")

    assert exc.value.code == 0
    assert (tmp_path / "README.md").exists()
    assert (tmp_path / "CONTRIBUTING.md").exists()
    assert (tmp_path / "NAVIGATION.md").exists()
