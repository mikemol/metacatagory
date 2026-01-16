#!/usr/bin/env python3
"""Tests for generate-badges.py (Phase 5e)"""

import builtins
import json
import importlib.util
from datetime import datetime, timedelta, timezone
from pathlib import Path


def _load_module(path: Path):
    spec = importlib.util.spec_from_file_location("generate_badges", path)
    module = importlib.util.module_from_spec(spec)
    assert spec and spec.loader
    spec.loader.exec_module(module)  # type: ignore[arg-type]
    return module


SCRIPT_PATH = Path(__file__).resolve().parent.parent.parent / "scripts" / "generate-badges.py"


def test_color_for_thresholds():
    mod = _load_module(SCRIPT_PATH)
    assert mod.color_for(10, [(30, "red"), (70, "yellow")]) == "red"
    assert mod.color_for(50, [(30, "red"), (70, "yellow")]) == "yellow"
    assert mod.color_for(100, [(30, "red"), (70, "yellow")], default="blue") == "blue"


def test_load_json_file_missing(tmp_path, capsys):
    mod = _load_module(SCRIPT_PATH)
    missing = tmp_path / "missing.json"
    result = mod.load_json_file(missing)
    captured = capsys.readouterr()
    assert result == {}
    assert "not found" in captured.err


def test_load_weights_profiles_and_errors(tmp_path):
    mod = _load_module(SCRIPT_PATH)
    weights_file = tmp_path / "weights.json"

    # Invalid JSON triggers error path and default write
    weights_file.write_text("{bad json")
    weights_after_error = mod.load_weights(tmp_path)
    assert weights_after_error == mod.DEFAULT_WEIGHTS
    assert json.loads(weights_file.read_text()) == mod.DEFAULT_WEIGHTS

    # Plain dict returns directly
    weights_file.write_text(json.dumps({"postulate": 9.0, "todo": 1.0, "fixme": 1.5, "deviation": 3.0}))
    direct_weights = mod.load_weights(tmp_path)
    assert direct_weights["postulate"] == 9.0

    # Profile selection honors active profile
    weights_file.write_text(json.dumps({
        "profiles": {
            "alt": {"postulate": 4.0, "todo": 1.0, "fixme": 1.5, "deviation": 2.5}
        },
        "active": "alt",
    }))
    prof_weights = mod.load_weights(tmp_path)
    assert prof_weights["postulate"] == 4.0


def test_generate_roadmap_badges_counts():
    mod = _load_module(SCRIPT_PATH)
    tasks = [
        {"status": "completed"},
        {"status": "in-progress"},
        {"status": "not-started"},
        {"status": "deferred"},
        {"status": "planned"},
    ]
    badges = mod.generate_roadmap_badges(tasks)
    assert badges["roadmap-progress"]["message"] == "20% (1/5)"
    assert badges["roadmap-active"]["message"] == "1"
    assert badges["roadmap-todo"]["message"] == "1"
    assert badges["roadmap-deferred"]["message"] == "1"
    assert badges["roadmap-planned"]["message"] == "1"


def test_generate_roadmap_badges_empty_tasks():
    mod = _load_module(SCRIPT_PATH)
    badges = mod.generate_roadmap_badges([])
    assert badges["roadmap-progress"]["message"] == "0% (0/0)"
    assert badges["roadmap-progress"]["color"] == "lightgrey"


def test_generate_deferred_badges_trend_and_weekly():
    mod = _load_module(SCRIPT_PATH)
    summary = {
        "total": 5,
        "weighted_total": 120,
        "deviation_log": 1,
        "postulates": 2,
        "todo": 3,
        "fixme": 4,
        "planned": 1,
        "trend_delta": -3,
        "weekly_avg_delta": 6.0,
    }
    badges = mod.generate_deferred_badges(summary)
    assert badges["deferred-total"]["color"] == "yellow"
    assert badges["deferred-weighted"]["message"] == "120"
    assert badges["deferred-trend"]["message"] == "-3"
    assert badges["deferred-trend-weekly"]["message"] == "+6.0"
    assert badges["deferred-deviations"]["message"] == "1"
    assert badges["deferred-postulates"]["message"] == "2"
    assert badges["deferred-todo"]["message"] == "3"
    assert badges["deferred-fixme"]["message"] == "4"
    assert badges["deferred-planned"]["message"] == "1"


def test_generate_deferred_badges_zero_and_colors():
    mod = _load_module(SCRIPT_PATH)

    zero_summary = {
        "total": 0,
        "weighted_total": 0,
        "deviation_log": 0,
        "postulates": 0,
        "todo": 0,
        "fixme": 0,
        "planned": 0,
        "trend_delta": 0,
        "weekly_avg_delta": -1.0,
    }
    badges_zero = mod.generate_deferred_badges(zero_summary)
    assert badges_zero["deferred-total"]["color"] == "brightgreen"
    assert badges_zero["deferred-trend"]["color"] == "lightgrey"
    assert badges_zero["deferred-trend-weekly"]["color"] == "brightgreen"

    small_trend = {
        "total": 10,
        "weighted_total": 12,
        "deviation_log": 1,
        "postulates": 0,
        "todo": 1,
        "fixme": 0,
        "planned": 0,
        "trend_delta": 2,
        "weekly_avg_delta": 1.0,
    }
    badges_small = mod.generate_deferred_badges(small_trend)
    assert badges_small["deferred-trend"]["color"] == "red"
    assert badges_small["deferred-trend-weekly"]["color"] == "lightgrey"


def test_scan_repository_for_deferred_counts(tmp_path):
    mod = _load_module(SCRIPT_PATH)
    # Files to scan
    agda = tmp_path / "A.agda"
    agda.write_text("postulate x\n-- postulate in comment\n")

    md = tmp_path / "note.md"
    md.write_text("TODO item\nFIXME here\n")

    py = tmp_path / "code.py"
    py.write_text("# DeviationLog present\n")

    excluded = tmp_path / ".git" / "skip.md"
    excluded.parent.mkdir()
    excluded.write_text("TODO should be ignored")

    tasks_json = tmp_path / ".github" / "roadmap" / "tasks.json"
    tasks_json.parent.mkdir(parents=True, exist_ok=True)
    tasks_json.write_text(json.dumps([{"status": "planned"}]))

    weights = {"postulate": 2.0, "todo": 1.0, "fixme": 1.5, "deviation": 3.0}

    summary = mod.scan_repository_for_deferred(tmp_path, weights)

    assert summary["postulates"] == 1
    assert summary["todo"] == 1
    assert summary["fixme"] == 1
    assert summary["deviation_log"] == 1
    assert summary["planned"] == 1
    assert summary["weighted_total"] == 2.0 + 1.0 + 1.5 + 3.0
    assert "A.agda" in summary["files"]
    assert "note.md" in summary["files"]
    # excluded directory not counted
    assert all(".git" not in name for name in summary["files"].keys())


def test_scan_repository_skips_and_recovers(tmp_path, monkeypatch):
    mod = _load_module(SCRIPT_PATH)
    ignored = tmp_path / "ignored.bin"
    ignored.write_text("TODO but wrong ext")

    generated_report = tmp_path / "docs" / "status" / "deferred-items.md"
    generated_report.parent.mkdir(parents=True, exist_ok=True)
    generated_report.write_text("TODO to skip")

    bad_file = tmp_path / "bad.md"
    bad_file.write_text("TODO boom")

    good_file = tmp_path / "good.md"
    good_file.write_text("FIXME here")

    tasks_json = tmp_path / ".github" / "roadmap" / "tasks.json"
    tasks_json.parent.mkdir(parents=True, exist_ok=True)
    tasks_json.write_text("{not json")

    real_open = builtins.open

    def raising_open(path, *args, **kwargs):
        if Path(path) == bad_file:
            raise IOError("boom")
        return real_open(path, *args, **kwargs)

    monkeypatch.setattr("builtins.open", raising_open)

    weights = {"postulate": 1.0, "todo": 1.0, "fixme": 1.5, "deviation": 3.0}
    summary = mod.scan_repository_for_deferred(tmp_path, weights)

    assert summary["fixme"] == 1
    assert summary["planned"] == 0  # tasks.json failed to parse
    assert "good.md" in summary["files"]
    assert "bad.md" not in summary["files"]


def test_load_weights_writes_default(tmp_path):
    mod = _load_module(SCRIPT_PATH)
    weights = mod.load_weights(tmp_path)
    loaded = json.loads((tmp_path / "weights.json").read_text())
    assert weights == loaded == mod.DEFAULT_WEIGHTS


def test_main_writes_badges_and_history(tmp_path, monkeypatch):
    mod = _load_module(SCRIPT_PATH)
    monkeypatch.setenv("CI_REPORT_DIR", "build/reports")

    repo_root = tmp_path
    scripts_dir = repo_root / "scripts"
    scripts_dir.mkdir()

    # Rebase module __file__ so Path(__file__).parent.parent resolves to tmp repo
    mod.__file__ = str(scripts_dir / "generate-badges.py")

    output_dir = repo_root / ".github" / "badges"
    output_dir.mkdir(parents=True, exist_ok=True)
    report_dir = repo_root / "build" / "reports"
    report_dir.mkdir(parents=True, exist_ok=True)

    # Tasks data
    tasks_file = repo_root / ".github" / "roadmap" / "tasks.json"
    tasks_file.parent.mkdir(parents=True, exist_ok=True)
    tasks_file.write_text(json.dumps([
        {"status": "completed"},
        {"status": "in-progress"},
    ]))

    # Deferred summary with files to skip scan
    deferred_summary = report_dir / "deferred-summary.json"
    deferred_summary.write_text(json.dumps({
        "total": 2,
        "weighted_total": 10,
        "deviation_log": 0,
        "postulates": 1,
        "todo": 1,
        "fixme": 0,
        "planned": 0,
        "files": {"file1": {"postulates": 1, "todo": 1, "fixme": 0, "deviation": 0, "total": 2, "weighted_total": 3.0}},
    }))

    # Existing history to trigger trend
    history_file = output_dir / "deferred-history.json"
    history_file.write_text(json.dumps([
        {"date": "2000-01-01", "total": 5, "weighted_total": 15}
    ]))

    # Run main
    mod.main()

    manifest = json.loads((output_dir / "manifest.json").read_text())
    badges_list = manifest["badges"]
    # Ensure key badges present
    assert "roadmap-progress" in badges_list
    assert "deferred-weighted" in badges_list
    assert "last-updated" in badges_list

    detailed = json.loads((output_dir / "deferred-files.json").read_text())
    assert "file1" in detailed

    history = json.loads((output_dir / "deferred-history.json").read_text())
    assert len(history) >= 1

    refreshed = json.loads((report_dir / "deferred-summary.json").read_text())
    assert "files" not in refreshed  # stripped when refreshed


def test_main_scans_trims_and_updates_history(tmp_path, monkeypatch):
    mod = _load_module(SCRIPT_PATH)
    monkeypatch.setenv("CI_REPORT_DIR", "build/reports")

    repo_root = tmp_path
    scripts_dir = repo_root / "scripts"
    scripts_dir.mkdir()
    mod.__file__ = str(scripts_dir / "generate-badges.py")

    # Minimal source files to scan
    (repo_root / "source.agda").write_text("postulate y")
    (repo_root / "code.py").write_text("# TODO\nDeviationLog\n")

    tasks_file = repo_root / ".github" / "roadmap" / "tasks.json"
    tasks_file.parent.mkdir(parents=True, exist_ok=True)
    tasks_file.write_text("{}")  # triggers tasks dict fallback

    output_dir = repo_root / ".github" / "badges"
    output_dir.mkdir(parents=True, exist_ok=True)
    history_file = output_dir / "deferred-history.json"

    today = datetime.now(timezone.utc).strftime("%Y-%m-%d")
    history_entries = []
    span = mod.MAX_HISTORY_ENTRIES + 5
    for i in range(span):
        day = (datetime.now(timezone.utc) - timedelta(days=span - i)).strftime("%Y-%m-%d")
        history_entries.append({"date": day, "total": i, "weighted_total": i})
    history_entries[-1]["date"] = today
    history_file.write_text(json.dumps(history_entries))

    mod.main()

    history = json.loads(history_file.read_text())
    assert len(history) == mod.MAX_HISTORY_ENTRIES
    assert history[-1]["date"] == today

    report_dir = repo_root / "build" / "reports"
    refreshed = json.loads((report_dir / "deferred-summary.json").read_text())
    assert refreshed.get("weekly_avg_delta") is not None
    assert refreshed.get("trend_delta") is not None

    manifest = json.loads((output_dir / "manifest.json").read_text())
    assert "roadmap-progress" in manifest["badges"]
    assert (output_dir / "top-offenders.md").exists()


def test_main_recovers_from_corrupt_history(tmp_path, monkeypatch):
    mod = _load_module(SCRIPT_PATH)
    monkeypatch.setenv("CI_REPORT_DIR", "build/reports")

    repo_root = tmp_path
    scripts_dir = repo_root / "scripts"
    scripts_dir.mkdir()
    mod.__file__ = str(scripts_dir / "generate-badges.py")

    tasks_file = repo_root / ".github" / "roadmap" / "tasks.json"
    tasks_file.parent.mkdir(parents=True, exist_ok=True)
    tasks_file.write_text(json.dumps([]))

    report_dir = repo_root / "build" / "reports"
    report_dir.mkdir(parents=True, exist_ok=True)
    deferred_summary = report_dir / "deferred-summary.json"
    deferred_summary.write_text(json.dumps({
        "total": 1,
        "weighted_total": 2,
        "deviation_log": 0,
        "postulates": 0,
        "todo": 1,
        "fixme": 0,
        "planned": 0,
        "files": {"file1": {"postulates": 0, "todo": 1, "fixme": 0, "deviation": 0, "total": 1, "weighted_total": 1}},
    }))

    output_dir = repo_root / ".github" / "badges"
    output_dir.mkdir(parents=True, exist_ok=True)
    history_file = output_dir / "deferred-history.json"
    history_file.write_text("{not json")

    mod.main()

    history = json.loads(history_file.read_text())
    assert isinstance(history, list)
    assert len(history) >= 1


def test_main_guard_runs_as_script(tmp_path, monkeypatch):
    script_source = SCRIPT_PATH.read_text()
    monkeypatch.setenv("CI_REPORT_DIR", "build/reports")
    repo_root = tmp_path

    tasks_file = repo_root / ".github" / "roadmap" / "tasks.json"
    tasks_file.parent.mkdir(parents=True, exist_ok=True)
    tasks_file.write_text(json.dumps([]))

    report_dir = repo_root / "build" / "reports"
    report_dir.mkdir(parents=True, exist_ok=True)
    deferred_summary = report_dir / "deferred-summary.json"
    deferred_summary.write_text(json.dumps({
        "total": 1,
        "weighted_total": 1,
        "deviation_log": 0,
        "postulates": 0,
        "todo": 1,
        "fixme": 0,
        "planned": 0,
        "files": {"file1": {"postulates": 0, "todo": 1, "fixme": 0, "deviation": 0, "total": 1, "weighted_total": 1}},
    }))

    fake_script_path = repo_root / "scripts" / "generate-badges.py"
    fake_script_path.parent.mkdir(parents=True, exist_ok=True)

    exec_globals = {
        "__name__": "__main__",
        "__file__": str(fake_script_path),
        "__package__": None,
        "__cached__": None,
        "__builtins__": builtins.__dict__,
    }

    exec(compile(script_source, str(SCRIPT_PATH), "exec"), exec_globals)

    output_dir = repo_root / ".github" / "badges"
    manifest = json.loads((output_dir / "manifest.json").read_text())
    assert "deferred-weighted" in manifest["badges"]


def test_scan_repository_for_deferred_reads_tasks(tmp_path):
    mod = _load_module(SCRIPT_PATH)
    (tmp_path / "src").mkdir()
    (tmp_path / "src" / "note.md").write_text("TODO: add more docs", encoding="utf-8")

    tasks_path = tmp_path / ".github" / "roadmap" / "tasks.json"
    tasks_path.parent.mkdir(parents=True, exist_ok=True)
    tasks_path.write_text(
        json.dumps(
            [
                {"id": "TASK-1", "status": "planned"},
                {"id": "TASK-2", "status": "completed"},
            ]
        ),
        encoding="utf-8",
    )

    summary = mod.scan_repository_for_deferred(tmp_path, mod.DEFAULT_WEIGHTS)
    assert summary["planned"] == 1


def test_scan_repository_for_deferred_missing_tasks(tmp_path):
    mod = _load_module(SCRIPT_PATH)
    (tmp_path / "src").mkdir()
    (tmp_path / "src" / "note.md").write_text("TODO: add more docs", encoding="utf-8")

    summary = mod.scan_repository_for_deferred(tmp_path, mod.DEFAULT_WEIGHTS)
    assert summary["planned"] == 0
