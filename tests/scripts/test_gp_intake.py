#!/usr/bin/env python3
"""Tests for shared GP intake parsing."""

from scripts.shared.gp_intake import (
    build_gp_metadata,
    categorize_gp_phase,
    extract_question,
    extract_metadata_from_text,
    extract_gp_number,
    infer_target_module,
    load_concept_config,
)


def test_extract_metadata_from_text_prefers_structured_sections():
    content = """# Title

* **The Insight:** The insight goes here.
* **The Gap:** The gap goes here.
* **The Fix:** The fix goes here.
"""
    meta = extract_metadata_from_text(content, "Fallback")
    assert meta["title"] == "Title"
    assert "Insight:" in meta["summary"]
    assert "insight goes here" in meta["insight"]
    assert "gap goes here" in meta["gap"]
    assert "fix goes here" in meta["fix"]


def test_extract_metadata_from_text_filters_prompt_lines():
    content = """# Title

Would you like me to proceed?
Here is a declarative line.
Second declarative line.
"""
    meta = extract_metadata_from_text(content, "Fallback")
    assert "Would you like" not in meta["summary"]
    assert "Here is a declarative line." in meta["summary"]


def test_extract_question_splits_lines():
    content = (
        "Preface line\r\n"
        "Would you like me to proceed?\r\n"
        "Second line.\n"
        "Third line.\n"
    )
    assert extract_question(content) == "Would you like me to proceed?"


def test_extract_gp_number_parses_id():
    assert extract_gp_number("GP01") == 1
    assert extract_gp_number("GP830") == 830
    assert extract_gp_number("UNKNOWN") == 0


def test_categorize_gp_phase_buckets():
    assert categorize_gp_phase(1) == "foundational"
    assert categorize_gp_phase(150) == "structural"
    assert categorize_gp_phase(250) == "geometric"
    assert categorize_gp_phase(350) == "topological"
    assert categorize_gp_phase(450) == "homological"
    assert categorize_gp_phase(550) == "polytope"
    assert categorize_gp_phase(650) == "coherence"
    assert categorize_gp_phase(750) == "analysis"
    assert categorize_gp_phase(850) == "unified"


def test_infer_target_module_defaults(tmp_path):
    config = load_concept_config(tmp_path / "missing.json")
    module = infer_target_module("no matches", "none", [], config)
    assert module == config["default_target_module"]


def test_infer_target_module_category_routing(tmp_path):
    config = load_concept_config(tmp_path / "missing.json")
    module = infer_target_module("functor morphism", "category", [], config)
    assert module == "src/agda/Core/CategoricalAdapter.agda"


def test_build_gp_metadata_includes_target_module(tmp_path):
    config = load_concept_config(tmp_path / "missing.json")
    content = "# Title\n\nSome text about functor.\n"

    metadata = build_gp_metadata(content, "GP01", config)

    assert metadata["title"] == "Title"
    assert metadata["category"] == "Foundation"
    assert metadata["target_module"] == "src/agda/Core/CategoricalAdapter.agda"


def test_ingest_metadata_includes_target_module(tmp_path):
    from scripts import ingest_gp_files

    gp_dir = tmp_path / "intake" / "GP"
    gp_dir.mkdir(parents=True, exist_ok=True)
    gp_file = gp_dir / "GP01.md"
    gp_file.write_text("# Title\n\nSome text about functor.", encoding="utf-8")

    records, metadata = ingest_gp_files.process_gp_directory(str(tmp_path / "intake"))
    assert records
    assert metadata["GP01"]["target_module"] == "src/agda/Core/CategoricalAdapter.agda"
