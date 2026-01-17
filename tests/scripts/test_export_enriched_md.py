#!/usr/bin/env python3
"""Tests for export_enriched_md.py (Phase 5d)"""

import json
from pathlib import Path

import scripts.export_enriched_md as mod


def test_format_helpers_empty_cases():
    assert mod.format_list([]).strip() == "*None*"
    assert mod.format_evidence([]).strip() == "*No evidence extracted*"
    assert mod.format_scope({}).strip() == "Not specified."


def test_missing_enriched_file(tmp_path, capsys, monkeypatch):
    monkeypatch.setattr(mod, "ENRICHED_JSON", tmp_path / "missing.json")
    monkeypatch.setattr(mod, "OUTPUT_MD", tmp_path / "out.md")

    mod.export_enriched_markdown()

    out = capsys.readouterr().out
    assert "not found" in out


def test_exports_markdown_structure(tmp_path, monkeypatch):
    enriched = [
        {
            "id": "T1",
            "title": "Alpha",
            "status": "in-progress",
            "category": "CatA",
            "complexity": "high",
            "dependsOn": ["T0"],
            "derivedTags": ["x", "y"],
            "intent": "Intent text",
            "deliverable": "Deliverable text",
            "scope": {"in": ["scope-in"], "out": ["scope-out"]},
            "inputs": ["i1"],
            "outputs": ["o1", "o2"],
            "acceptance": ["accept"],
            "moduleAnchors": ["Mod.A"],
            "definitions": ["Def1"],
            "related": ["T2"],
            "suggestedDependencies": ["S1", "S2", "S3", "S4", "S5", "S6"],
            "evidence": [{"source": "md", "loc": "L1", "text": "evidence line"}],
            "provenance": ["p1", "p2", "p3", "p4"],
        },
        {
            "id": "T2",
            "title": "Beta",
            "status": "not-started",
            "category": "CatB",
            "complexity": "medium",
            "derivedTags": ["solo"],
        },
    ]

    enriched_path = tmp_path / "canonical_enriched.json"
    output_md = tmp_path / "tasks_enriched.md"
    enriched_path.write_text(json.dumps(enriched), encoding="utf-8")

    monkeypatch.setattr(mod, "ENRICHED_JSON", enriched_path)
    monkeypatch.setattr(mod, "OUTPUT_MD", output_md)

    mod.export_enriched_markdown()

    md = output_md.read_text()

    # Headers and TOC
    assert "# Enriched Roadmap Tasks" in md
    assert "Semantic details extracted" in md
    assert "[CatA](#category-cata)" in md
    assert "[CatB](#category-catb)" in md

    # Task sections and frontmatter
    assert "### Task 1: T1 — Alpha" in md
    assert "\"id\": \"T1\"" in md
    assert "\"status\": \"in-progress\"" in md
    assert "\"dependencies\":\n- \"T0\"" in md
    assert "\"tags\":\n- \"x\"\n- \"y\"" in md
    assert "Agda Modules" in md
    assert "**Suggested dependencies (from imports):** S1, S2, S3, S4, S5 (+ 1 more)" in md
    assert "Evidence — T1" in md and "**md** (L1)" in md and "> evidence line" in md
    assert "Provenance — T1" in md and "`p1` → `p2` → `p3` (+ 1 more)" in md

    # Category B task present
    assert "### Task 2: T2 — Beta" in md

    # Status and complexity breakdowns
    assert "Status Breakdown" in md
    assert "**in-progress**: 1 (50.0%)" in md
    assert "**not-started**: 1 (50.0%)" in md
    assert "Complexity Breakdown" in md
    assert "**high**: 1 (50.0%)" in md
    assert "**medium**: 1 (50.0%)" in md
