#!/usr/bin/env python3
"""Tests for shared roadmap Agda parsing helpers."""

from pathlib import Path

from scripts.shared.roadmap_agda import parse_ingested_agda, parse_legacy_agda


def test_parse_ingested_agda(tmp_path: Path):
    ingested_dir = tmp_path / "src/agda/Plan/CIM/IngestedRoadmaps"
    ingested_dir.mkdir(parents=True, exist_ok=True)
    agda_file = ingested_dir / "RoadmapGp1.agda"
    agda_file.write_text(
        """
roadmapGp1 : RoadmapStep
roadmapGp1 = record
  { provenance = "GP1"
  ; step = "Do the thing"
  ; status = "in-progress"
  ; targetModule = "src/agda/Plan/CIM/Utility.agda"
  }
""",
        encoding="utf-8",
    )

    items = parse_ingested_agda(tmp_path)
    assert len(items) == 1
    item = items[0]
    assert item["id"] == "GP-Gp1"
    assert item["title"] == "GP1"
    assert item["description"] == "Do the thing"
    assert item["status"] == "in-progress"
    assert item["files"] == ["src/agda/Plan/CIM/Utility.agda"]


def test_parse_legacy_agda(tmp_path: Path):
    legacy = tmp_path / "roadmap-legacy.agda"
    legacy.write_text(
        """
data LegacyThing : Set where
  legacy : LegacyThing
""",
        encoding="utf-8",
    )

    items = parse_legacy_agda(tmp_path)
    assert len(items) == 1
    item = items[0]
    assert item["id"].startswith("LEGACY-roadmap-legacy-")
    assert "LegacyThing" in item["title"]
