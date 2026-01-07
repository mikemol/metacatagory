#!/usr/bin/env python3
"""Tests for export_meta_index.py"""

import runpy
import sys
from pathlib import Path

import pytest

import scripts.export_meta_index as mod


def test_render_meta_index_formats_sections():
    mi = {
        "title": "Meta-Index: Demo",
        "sections": [
            {"name": "Scope", "bullets": ["Item A", "Item B"]},
            {"name": "Empty", "bullets": []},
        ],
    }
    out = mod.render_meta_index(mi)
    assert "# Meta-Index: Demo" in out
    assert "## Scope" in out
    assert "* Item A" in out
    assert "## Empty" in out


def test_export_file_writes_markdown(tmp_path):
    yaml_path = tmp_path / "demo.yaml"
    yaml_path.write_text(
        """title: Demo\nsections:\n  - name: Scope\n    bullets:\n      - Bullet""",
        encoding="utf-8",
    )

    mod.export_file(yaml_path)

    md_path = yaml_path.with_suffix(".md")
    text = md_path.read_text()
    assert "# Demo" in text
    assert "* Bullet" in text


def test_main_usage_and_directory(tmp_path, monkeypatch, capsys):
    # Usage branch
    monkeypatch.setattr(sys, "argv", ["prog"])
    with pytest.raises(SystemExit) as exc:
        mod.main()
    assert exc.value.code == 1

    # Directory branch with YAML discovery
    yaml_dir = tmp_path / "meta"
    yaml_dir.mkdir()
    yfile = yaml_dir / "item.yaml"
    yfile.write_text("title: Demo\nsections: []", encoding="utf-8")

    monkeypatch.setattr(sys, "argv", ["prog", str(yaml_dir)])
    mod.main()

    md_path = yfile.with_suffix(".md")
    assert md_path.exists()

    captured = capsys.readouterr()
    assert "No YAML files found" not in captured.out

    # No-yaml branch
    empty_dir = tmp_path / "empty"
    empty_dir.mkdir()
    monkeypatch.setattr(sys, "argv", ["prog", str(empty_dir)])
    with pytest.raises(SystemExit) as exc3:
        mod.main()
    assert exc3.value.code == 0
    captured2 = capsys.readouterr()
    assert "No YAML files found" in captured2.out


def test_main_single_yaml_file(tmp_path, monkeypatch):
    yaml_path = tmp_path / "item.yaml"
    yaml_path.write_text("title: Demo\nsections: []", encoding="utf-8")

    monkeypatch.setattr(sys, "argv", ["prog", str(yaml_path)])
    mod.main()

    md_path = yaml_path.with_suffix(".md")
    assert md_path.exists()


def test_main_guard_original_module(tmp_path, monkeypatch):
    yaml_path = tmp_path / "item.yaml"
    yaml_path.write_text("title: Demo\nsections: []", encoding="utf-8")

    monkeypatch.setattr(sys, "argv", ["prog", str(yaml_path)])

    runpy.run_path(mod.__file__, run_name="__main__")

    md_path = yaml_path.with_suffix(".md")
    assert md_path.exists()
