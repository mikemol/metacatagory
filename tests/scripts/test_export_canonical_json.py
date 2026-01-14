#!/usr/bin/env python3
"""Tests for export_canonical_json.py"""

import builtins
import json
import runpy
from pathlib import Path

import scripts.export_canonical_json as mod


def test_export_tasks_json_filters_legacy(tmp_path):
    source = tmp_path / "planning_index.json"
    output = tmp_path / "tasks.json"
    data = [
        {"id": "GP-1", "title": "Keep"},
        {"id": "LEGACY-1", "title": "Drop"},
    ]
    source.write_text(json.dumps(data), encoding="utf-8")

    mod.export_tasks_json(source, output)

    tasks = json.loads(output.read_text())
    assert len(tasks) == 1
    assert tasks[0]["id"] == "GP-1"


def test_main_guard_executes(tmp_path):
    script_copy = tmp_path / "scripts" / "export_canonical_json.py"
    script_copy.parent.mkdir(parents=True, exist_ok=True)
    script_copy.write_text(Path(mod.__file__).read_text(), encoding="utf-8")

    planning = tmp_path / "data" / "planning_index.json"
    planning.parent.mkdir(parents=True, exist_ok=True)
    planning.write_text(json.dumps([{"id": "GP-1"}]), encoding="utf-8")

    output = tmp_path / ".github" / "roadmap" / "tasks.json"
    output.parent.mkdir(parents=True, exist_ok=True)

    runpy.run_path(str(script_copy), run_name="__main__")

    tasks = json.loads(output.read_text())
    assert tasks[0]["id"] == "GP-1"


def test_main_guard_original_module(tmp_path, monkeypatch):
    planning = tmp_path / "planning_index.json"
    planning.write_text(json.dumps([{"id": "GP-1"}]), encoding="utf-8")
    output = tmp_path / "tasks.json"

    mapping = {
        str(mod.PLANNING_PATH): planning,
        str(mod.OUTPUT_PATH): output,
    }
    real_open = builtins.open

    def fake_open(path, *args, **kwargs):
        target = mapping.get(str(path))
        if target:
            return real_open(target, *args, **kwargs)
        return real_open(path, *args, **kwargs)

    monkeypatch.setattr(builtins, "open", fake_open)

    # Execute __main__ of the original module to cover the guard
    runpy.run_path(mod.__file__, run_name="__main__")

    tasks = json.loads(output.read_text())
    planning_file = Path(mod.resolve_planning_path())
    canonical = json.loads(planning_file.read_text(encoding="utf-8"))
    expected = next(item["id"] for item in canonical if not item["id"].startswith("LEGACY-"))
    assert tasks[0]["id"] == expected
