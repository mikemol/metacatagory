#!/usr/bin/env python3
"""Tests for phase_diagram_new.py."""

from __future__ import annotations

from pathlib import Path

from scripts.phase_diagram_new import PhaseDiagramGenerator


def _write_checklist(path: Path) -> None:
    content = "\n".join(
        [
            "module Chapter1Checklist where",
            "-----",
            "-- Level1sub2",
            "foo-adapter : A.KernelPair",
            "foo-bar-link : A.KernelPairAdapter foo ≡ Target.Ref",
        ]
    )
    path.write_text(content, encoding="utf-8")


def test_parse_test_files_builds_nodes_and_edges(tmp_path):
    test_dir = tmp_path / "src" / "agda" / "Tests"
    test_dir.mkdir(parents=True)
    _write_checklist(test_dir / "Chapter1Checklist.agda")

    generator = PhaseDiagramGenerator(test_dir)
    generator.parse_test_files()

    assert "Chapter1" in generator.nodes
    assert "Chapter1.2" in generator.nodes
    assert "Chapter1.2.foo" in generator.nodes

    link_edges = [
        edge
        for edge in generator.edges
        if edge.edge_type == "links_to" and edge.target == "witness.Target.Ref"
    ]
    assert link_edges


def test_export_json_writes_output(tmp_path):
    test_dir = tmp_path / "src" / "agda" / "Tests"
    test_dir.mkdir(parents=True)
    _write_checklist(test_dir / "Chapter1Checklist.agda")

    generator = PhaseDiagramGenerator(test_dir)
    generator.parse_test_files()
    out_path = tmp_path / "phase_structure.json"

    generator.export_json(out_path)

    assert out_path.exists()
