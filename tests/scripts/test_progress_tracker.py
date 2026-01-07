#!/usr/bin/env python3
"""Tests for progress_tracker.py (Phase 5c)"""

import json
from pathlib import Path

from scripts import progress_tracker as mod


def test_initial_state_created_when_missing(tmp_path):
    state_path = tmp_path / "state.json"
    tracker = mod.ProgressTracker(str(state_path))

    assert tracker.state["statistics"]["total"] == 0
    assert tracker.state["steps"] == {}


def test_update_and_summary(tmp_path):
    state_path = tmp_path / "state.json"
    tracker = mod.ProgressTracker(str(state_path))

    tracker.update_step("GP-1", "in_progress", "note", 2, 5)

    assert state_path.exists()
    step = tracker.state["steps"]["GP-1"]
    assert step["status"] == "in_progress"
    assert step["subtasks_completed"] == 2
    assert step["subtasks_total"] == 5
    assert step["notes"]

    summary = tracker.get_progress_summary()
    assert summary["total_steps"] == 1
    assert summary["in_progress"] == 1
    assert summary["completed"] == 0
    assert summary["completion_percentage"] == 0


def test_generate_progress_report(tmp_path):
    state_path = tmp_path / "state.json"
    tracker = mod.ProgressTracker(str(state_path))
    tracker.update_step("GP-2", "completed", "done", 5, 5)

    report_path = tmp_path / "report.json"
    report = tracker.generate_progress_report(str(report_path))

    assert report_path.exists()
    loaded = json.loads(report_path.read_text())
    assert loaded["summary"]["completed"] == 1
    assert loaded["summary"]["total_steps"] == 1
    assert report["summary"]["completion_percentage"] == 100


def test_main_uses_workdir_and_writes_outputs(tmp_path, capsys, monkeypatch):
    monkeypatch.chdir(tmp_path)

    mod.main()

    state_file = Path("build/roadmap_progress.json")
    report_file = Path("build/roadmap_progress_report.json")

    assert state_file.exists()
    assert report_file.exists()

    out = capsys.readouterr().out
    assert "Progress Summary" in out
    assert "Completion" in out
