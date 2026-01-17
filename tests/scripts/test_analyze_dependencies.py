#!/usr/bin/env python3
"""
Tests for analyze_dependencies.py (Phase 5b)
"""

import json
import sys
from pathlib import Path
from unittest.mock import patch

import scripts.analyze_dependencies as mod


def _write_json(path: Path, data) -> None:
    path.write_text(json.dumps(data), encoding="utf-8")


class TestAnalyzeSuggestions:
    def test_missing_enriched_file(self, capsys, monkeypatch, tmp_path):
        monkeypatch.setattr(mod, "ENRICHED_JSON", tmp_path / "missing.json")
        mod.analyze_suggestions()
        out = capsys.readouterr().out
        assert "not found" in out

    def test_counts_and_lists_tasks(self, capsys, monkeypatch, tmp_path):
        enriched_path = tmp_path / "canonical_enriched.json"
        monkeypatch.setattr(mod, "ENRICHED_JSON", enriched_path)
        data = [
            {"id": "T1", "title": "Task One", "suggestedDependencies": ["A", "B", "C"]},
            {"id": "T2", "title": "Task Two", "suggestedDependencies": ["Z"]},
            {"id": "T3", "title": "Task Three", "suggestedDependencies": []},
        ]
        _write_json(enriched_path, data)

        mod.analyze_suggestions()
        out = capsys.readouterr().out
        assert "Total tasks with suggestions: 2" in out
        assert "Total suggested dependencies: 4" in out
        assert "Average per task: 2.0" in out
        assert "T1" in out and "Task One" in out
        assert "T2" in out and "Task Two" in out


class TestPromoteSuggestions:
    def test_missing_inputs(self, capsys, monkeypatch, tmp_path):
        monkeypatch.setattr(mod, "ENRICHED_JSON", tmp_path / "missing.json")
        monkeypatch.setattr(mod, "CANONICAL_JSON", tmp_path / "missing2.json")
        mod.promote_suggestions()
        out = capsys.readouterr().out
        assert "Missing enriched or canonical" in out

    def test_promotes_and_avoids_duplicates(self, capsys, monkeypatch, tmp_path):
        enriched_path = tmp_path / "canonical_enriched.json"
        canonical_path = tmp_path / "planning_index.json"
        monkeypatch.setattr(mod, "ENRICHED_JSON", enriched_path)
        monkeypatch.setattr(mod, "CANONICAL_JSON", canonical_path)

        enriched = [
            {"id": "T1", "suggestedDependencies": ["A", "B"]},
            {"id": "T2", "suggestedDependencies": ["B"]},
        ]
        canonical = [
            {"id": "T1", "title": "Task One", "status": "not-started", "dependsOn": ["A"]},
            {"id": "T2", "title": "Task Two", "status": "not-started", "dependsOn": []},
        ]
        _write_json(enriched_path, enriched)
        _write_json(canonical_path, canonical)

        mod.promote_suggestions()
        out = capsys.readouterr().out
        assert "Promoted" in out

        updated = json.loads(canonical_path.read_text())
        t1 = next(item for item in updated if item["id"] == "T1")
        t2 = next(item for item in updated if item["id"] == "T2")
        # T1 already had A, should gain B only once
        assert sorted(t1.get("dependsOn", [])) == ["A", "B"]
        # T2 gains B
        assert t2.get("dependsOn", []) == ["B"]


class TestShowTaskSuggestions:
    def test_missing_enriched_file(self, capsys, monkeypatch, tmp_path):
        monkeypatch.setattr(mod, "ENRICHED_JSON", tmp_path / "missing.json")
        mod.show_task_suggestions("X")
        out = capsys.readouterr().out
        assert "not found" in out

    def test_task_not_found(self, capsys, monkeypatch, tmp_path):
        enriched_path = tmp_path / "canonical_enriched.json"
        monkeypatch.setattr(mod, "ENRICHED_JSON", enriched_path)
        _write_json(enriched_path, [])
        mod.show_task_suggestions("T1")
        out = capsys.readouterr().out
        assert "not found" in out

    def test_show_details(self, capsys, monkeypatch, tmp_path):
        enriched_path = tmp_path / "canonical_enriched.json"
        monkeypatch.setattr(mod, "ENRICHED_JSON", enriched_path)
        data = [
            {
                "id": "T1",
                "title": "Task One",
                "status": "open",
                "category": "cat",
                "moduleAnchors": ["M1", "M2"],
                "suggestedDependencies": ["A", "B"],
                "dependsOn": ["C"],
            }
        ]
        _write_json(enriched_path, data)

        mod.show_task_suggestions("T1")
        out = capsys.readouterr().out
        assert "Task One" in out
        assert "Status: open" in out
        assert "Suggested dependencies (2)" in out
        assert "A" in out and "B" in out
        assert "Existing dependencies (1)" in out
        assert "C" in out


class TestMainDispatch:
    def test_usage_message_for_unknown_flag(self, capsys, monkeypatch):
        with patch("sys.argv", ["analyze_dependencies.py", "--unknown"]):
            import runpy

            # Clear cached module so run_module executes a fresh __main__
            sys.modules.pop("scripts.analyze_dependencies", None)

            runpy.run_module("scripts.analyze_dependencies", run_name="__main__", alter_sys=True)

        out = capsys.readouterr().out
        assert "Usage" in out
