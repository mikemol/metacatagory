#!/usr/bin/env python3

from pathlib import Path

from scripts.search_algo import index_paths, query


def test_index_paths_collects_declarations(tmp_path: Path) -> None:
    agda_dir = tmp_path / "src"
    agda_dir.mkdir()
    sample = agda_dir / "Sample.agda"
    sample.write_text(
        "\n".join(
            [
                "module Foo.Bar where",
                "",
                "record MyRecord : Set where",
                "constructor mkMyRecord",
                "",
                "data MyData : Set where",
                "constructor MyDataCtor",
            ]
        ),
        encoding="utf-8",
    )

    entries = index_paths(agda_dir)
    texts = {e["text"] for e in entries}
    assert any("module Foo.Bar" in t for t in texts)
    assert any("record MyRecord" in t for t in texts)
    assert any("data MyData" in t for t in texts)


def test_query_matches_by_name(tmp_path: Path) -> None:
    agda_dir = tmp_path / "src"
    agda_dir.mkdir()
    sample = agda_dir / "Sample.agda"
    sample.write_text(
        "\n".join(
            [
                "record SearchTarget : Set where",
                "constructor mkSearchTarget",
            ]
        ),
        encoding="utf-8",
    )

    entries = index_paths(agda_dir)
    matches = query(entries, "SearchTarget")
    assert matches
