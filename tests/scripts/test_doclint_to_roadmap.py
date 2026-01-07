#!/usr/bin/env python3
"""Tests for doclint_to_roadmap.py"""

import json
import runpy
from pathlib import Path

import pytest

import scripts.doclint_to_roadmap as mod


def test_main_missing_report(monkeypatch, tmp_path, capsys):
    monkeypatch.setattr(mod, "REPORT", tmp_path / "missing.json")
    monkeypatch.setattr(mod, "OUT", tmp_path / "out.json")
    result = mod.main()
    captured = capsys.readouterr()
    assert result == 1
    assert "docs-lint report not found" in captured.out


def test_main_generates_items(tmp_path, monkeypatch):
    report = tmp_path / "docs-lint.json"
    report.parent.mkdir(parents=True, exist_ok=True)
    report.write_text(
        json.dumps({
            "missing_module": ["a.md"],
            "missing_decls": {"b.md": [1, 2]},
        }),
        encoding="utf-8",
    )

    out = tmp_path / "doclint_roadmap.json"
    monkeypatch.setattr(mod, "REPORT", report)
    monkeypatch.setattr(mod, "OUT", out)

    result = mod.main()
    assert result == 0

    items = json.loads(out.read_text())
    assert len(items) == 2
    assert any("a.md" in item["id"] for item in items)
    assert any("b.md" in item["id"] for item in items)


def test_main_guard_runs(tmp_path, monkeypatch):
    # Run a copy so __main__ writes within the temp root
    script_copy = tmp_path / "scripts" / "doclint_to_roadmap.py"
    script_copy.parent.mkdir(parents=True, exist_ok=True)
    script_copy.write_text(Path(mod.__file__).read_text(), encoding="utf-8")

    report = tmp_path / "build" / "reports" / "docs-lint.json"
    report.parent.mkdir(parents=True, exist_ok=True)
    report.write_text(json.dumps({"missing_module": ["src/A.agda"]}), encoding="utf-8")

    out = tmp_path / "build" / "doclint_roadmap.json"

    with pytest.raises(SystemExit) as exc:
        runpy.run_path(str(script_copy), run_name="__main__")

    assert exc.value.code == 0
    items = json.loads(out.read_text())
    assert items[0]["id"].startswith("DOC-LINT::src/A.agda")


def test_main_guard_original_paths(tmp_path):
    report = mod.REPORT
    out = mod.OUT

    # Backup any existing files
    report_backup = report.read_text() if report.exists() else None
    out_backup = out.read_text() if out.exists() else None

    try:
        report.parent.mkdir(parents=True, exist_ok=True)
        report.write_text(json.dumps({"missing_module": ["src/B.agda"]}), encoding="utf-8")

        with pytest.raises(SystemExit) as exc:
            runpy.run_path(mod.__file__, run_name="__main__")

        assert exc.value.code == 0
        data = json.loads(out.read_text())
        assert data[0]["id"].startswith("DOC-LINT::src/B.agda")
    finally:
        if report_backup is None:
            report.unlink(missing_ok=True)
        else:
            report.write_text(report_backup, encoding="utf-8")

        if out_backup is None:
            out.unlink(missing_ok=True)
        else:
            out.write_text(out_backup, encoding="utf-8")
