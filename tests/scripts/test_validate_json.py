#!/usr/bin/env python3
"""Tests for validate_json.py canonical loading."""

import json
from pathlib import Path

import scripts.validate_json as mod


def test_load_json_phase_handles_items_dict(tmp_path):
    canonical = {"items": [{"id": "GP-1", "title": "One", "status": "done", "category": "Cat"}]}
    tasks = [{"id": "GP-1", "title": "One", "status": "done", "category": "Cat"}]

    canonical_path = tmp_path / "planning_index.json"
    tasks_path = tmp_path / "tasks.json"

    canonical_path.write_text(json.dumps(canonical), encoding="utf-8")
    tasks_path.write_text(json.dumps(tasks), encoding="utf-8")

    logger = mod.configure_logging("test_validate_json", structured=False)
    phase = mod.LoadJSONFilesPhase(tasks_path, logger)

    output = phase.transform(canonical_path, {})
    assert len(output["canonical"]) == 1
    assert len(output["tasks"]) == 1
