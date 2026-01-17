#!/usr/bin/env python3
"""Tests for extract_roadmaps.py"""

from pathlib import Path

import scripts.extract_roadmaps as mod


def test_parse_gp_file_uses_shared_metadata(tmp_path: Path):
    gp_file = tmp_path / "GP01.md"
    gp_file.write_text(
        """# Custom Title

* **The Insight:** Insight line.
* **The Gap:** Gap line.
* **The Fix:** Fix line.
""",
        encoding="utf-8",
    )

    entry = mod.parse_gp_file(gp_file)
    assert entry.title == "Custom Title"


def test_generate_agda_module_uses_shared_renderer(tmp_path: Path):
    entry = mod.RoadmapEntry(
        gp_number="GP01",
        title="Title",
        category="Foundation",
        question="Do thing",
        formal_correction="Correction",
        key_concepts=["Alpha"],
        related_gps=[],
        manifest_version=None,
        target_modules=[],
        target_module="src/agda/Plan/CIM/Implementation.agda",
    )

    module_text = mod.generate_agda_module("Foundation", [entry])
    assert "roadmapGp01" in module_text
    assert "Concepts: Alpha" in module_text
    assert "targetModule = \"src/agda/Plan/CIM/Implementation.agda\"" in module_text
