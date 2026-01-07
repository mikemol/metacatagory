#!/usr/bin/env python3
"""Tests for export_canonical_md.py (Phase 5e)"""

import runpy
import sys
import types
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent.parent
SCRIPTS_DIR = ROOT / "scripts"
if str(SCRIPTS_DIR) not in sys.path:
    sys.path.insert(0, str(SCRIPTS_DIR))

import export_canonical_md as mod

SCRIPT_PATH = SCRIPTS_DIR / "export_canonical_md.py"


def test_export_markdown_renders_categories(tmp_path, monkeypatch):
    sample = [
        {
            "id": "GP-1",
            "title": "Alpha",
            "description": "Desc A",
            "status": "in-progress",
            "category": "CatA",
            "dependsOn": ["GP-0"],
            "tags": ["t1", "t2"],
            "files": ["src/A.agda"],
            "source": "src",
        },
        {
            "id": "GP-2",
            "title": "Beta",
            "description": "Desc B",
            "status": "not-started",
            "category": "CatB",
            "dependsOn": [],
            "tags": [],
            "files": [],
            "source": "src",
        },
    ]

    out = tmp_path / "ROADMAP.md"

    # Patch REPO_ROOT so __main__ path not used; patch loader and yaml
    monkeypatch.setattr(mod, "REPO_ROOT", tmp_path)
    monkeypatch.setattr(mod, "load_planning_index", lambda: sample)
    def fake_dump(data):
        parts = [
            f"id: {data['id']}",
            f"title: {data['title']}",
            f"description: {data['description']}",
            f"status: {data['status']}",
            f"category: {data['category']}",
        ]
        if data.get("dependencies"):
            parts.append("dependencies:")
            parts.append("- GP-0")
        if data.get("tags"):
            parts.append("tags:")
            parts.append("- t1")
            parts.append("- t2")
        if data.get("files"):
            parts.append("files:")
            parts.append("- src/A.agda")
        return "\n".join(parts) + "\n"

    monkeypatch.setattr(mod, "dump_yaml", fake_dump)

    mod.export_markdown(out)

    text = out.read_text()
    assert "## CatA" in text and "## CatB" in text
    assert "id: GP-1" in text and "id: GP-2" in text
    assert "Depends on: `GP-0`" in text
    assert "Tags: t1, t2" in text
    assert "Target: `src/A.agda`" in text
    assert "[status: in-progress]" in text
    assert "[status: not-started]" in text


def test_main_guard_executes(tmp_path, monkeypatch):
    sample = [
        {
            "id": "GP-1",
            "title": "Alpha",
            "description": "Desc A",
            "status": "in-progress",
            "category": "CatA",
            "dependsOn": [],
            "tags": [],
            "files": [],
            "source": "src",
        }
    ]

    stub_shared_data = types.SimpleNamespace(
        REPO_ROOT=tmp_path, load_planning_index=lambda: sample
    )
    stub_shared_yaml = types.SimpleNamespace(
        dump_yaml=lambda data: f"id: {data['id']}\n"
    )

    monkeypatch.setitem(sys.modules, "shared_data", stub_shared_data)
    monkeypatch.setitem(sys.modules, "shared_yaml", stub_shared_yaml)

    runpy.run_path(str(SCRIPT_PATH), run_name="__main__")

    output = tmp_path / "ROADMAP.md"
    assert output.exists()
    assert "id: GP-1" in output.read_text()
