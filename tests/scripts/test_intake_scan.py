#!/usr/bin/env python3
"""Tests for intake_scan.py (Phase 5c)"""

import json
from pathlib import Path

import scripts.intake_scan as mod


def test_load_canonical_ids_creates_empty(tmp_path, monkeypatch):
    canonical_path = tmp_path / "planning_index.json"
    monkeypatch.setattr(mod, "CANONICAL_PATH", canonical_path)

    ids = mod.load_canonical_ids()

    assert ids == set()
    assert canonical_path.exists()
    assert canonical_path.read_text() == "[]"


def test_build_coverage_classifies_and_tracks(tmp_path, monkeypatch):
    root = tmp_path
    intake_dir = root / "intake"
    intake_dir.mkdir()

    shard = intake_dir / "__.md"
    shard.write_text("GP-1")
    candidate = intake_dir / "note_candidate.md"
    candidate.write_text("GP-2 GP-X")
    substrate = intake_dir / "context.md"
    substrate.write_text("")
    formal = intake_dir / "formal.md"
    formal.write_text("GP-1")

    monkeypatch.setattr(mod, "ROOT", root)
    monkeypatch.setattr(mod, "INTAKE_DIR", intake_dir)

    canonical_ids = {"GP-1", "GP-2"}
    files = [shard, candidate, substrate, formal]

    payload = mod.build_coverage(canonical_ids, files)

    assert payload["coverage"]["GP-1"] == ["intake/__.md", "intake/formal.md"]
    assert payload["coverage"]["GP-2"] == ["intake/note_candidate.md"]
    assert payload["unknown_hits"] == {"intake/note_candidate.md": ["GP-X"]}
    assert "intake/context.md" in payload["files_without_ids"]
    assert payload["shards"] == ["intake/__.md"]
    assert payload["candidates"] == ["intake/note_candidate.md"]
    assert payload["substrates"] == ["intake/context.md"]
    assert payload["formalized"] == ["intake/formal.md"]


def test_main_writes_reports(tmp_path, monkeypatch, capsys):
    root = tmp_path
    intake_dir = root / "intake"
    intake_dir.mkdir()
    (intake_dir / "__.md").write_text("GP-1 GP-unknown")
    (intake_dir / "draft.md").write_text("GP-2")

    canonical_path = root / "build" / "planning_index.json"
    canonical_path.parent.mkdir(parents=True, exist_ok=True)
    canonical_path.write_text(json.dumps([{"id": "GP-1"}, {"id": "GP-2"}]))

    report_dir = root / "build" / "reports"
    monkeypatch.setattr(mod, "ROOT", root)
    monkeypatch.setattr(mod, "INTAKE_DIR", intake_dir)
    monkeypatch.setattr(mod, "CANONICAL_PATH", canonical_path)
    monkeypatch.setattr(mod, "REPORT_DIR", report_dir)
    monkeypatch.setattr(mod, "REPORT_JSON", report_dir / "intake_coverage.json")
    monkeypatch.setattr(mod, "REPORT_MD", report_dir / "intake_coverage.md")

    mod.main()

    json_report = report_dir / "intake_coverage.json"
    md_report = report_dir / "intake_coverage.md"
    assert json_report.exists()
    assert md_report.exists()

    payload = json.loads(json_report.read_text())
    assert payload["summary"]["files_with_ids"] == 2
    assert payload["summary"]["canonical_ids_seen"] == 2
    assert "GP-unknown" in json_report.read_text()

    md_text = md_report.read_text()
    assert "Intake Coverage" in md_text
    assert "Unknown IDs" in md_text

    out = capsys.readouterr().out
    assert "Intake scan complete" in out
    assert "Raw Shards" in out
