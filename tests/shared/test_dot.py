#!/usr/bin/env python3
"""Tests for scripts.shared.dot helpers."""

from pathlib import Path

from scripts.shared.dot import parse_dependency_graph


def test_parse_dependency_graph_filters_stdlib(tmp_path: Path) -> None:
    dot_path = tmp_path / "deps.dot"
    dot_path.write_text(
        """digraph dependencies {
  m1[label="Core.Types"]
  m2[label="Agda.Builtin.Nat"]
  m3[label="Data.List"]

  m1 -> m2
  m1 -> m3
}
""",
        encoding="utf-8",
    )

    deps = parse_dependency_graph(dot_path)
    assert isinstance(deps, dict)
    assert "Core.Types" not in deps or deps["Core.Types"] == []


def test_parse_dependency_graph_extracts_edges(tmp_path: Path) -> None:
    dot_path = tmp_path / "deps.dot"
    dot_path.write_text(
        """digraph dependencies {
  m1[label="Core.Types"]
  m2[label="Core.Functions"]
  m3[label="Utils.Helper"]

  m2 -> m1
  m3 -> m1
}
""",
        encoding="utf-8",
    )

    deps = parse_dependency_graph(dot_path)
    assert "Core.Functions" in deps
    assert "Core.Types" in deps["Core.Functions"]
    assert "Utils.Helper" in deps
    assert "Core.Types" in deps["Utils.Helper"]
