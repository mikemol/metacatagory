#!/usr/bin/env python3
"""Tests for validate_md.py and shared roadmap validation."""

import json
from pathlib import Path

from scripts.shared.composition import run_validate_roadmap_md


def test_validate_roadmap_md_accepts_items_dict(tmp_path):
    base = tmp_path
    data_dir = base / "data"
    data_dir.mkdir(parents=True, exist_ok=True)
    planning_path = data_dir / "planning_index.json"
    planning_path.write_text(
        json.dumps({"items": [{"title": "Alpha"}]}),
        encoding="utf-8",
    )

    roadmap = base / "ROADMAP.md"
    roadmap.write_text(
        "- **Alpha** — Desc [status: done]\n",
        encoding="utf-8",
    )

    exit_code, ctx = run_validate_roadmap_md(base, strict=True)
    assert exit_code == 0
    report = ctx.get("validation_report", {})
    assert report.get("missing_count") == 0
    assert report.get("extra_count") == 0

