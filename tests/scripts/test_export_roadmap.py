#!/usr/bin/env python3
"""
Tests for export_roadmap.py (Phase 5b)
"""

import json
import os
from pathlib import Path
from unittest.mock import patch

import pytest

import scripts.export_roadmap as mod


class TestGenerateMarkdownSection:
    def test_groups_by_category_and_sorts(self):
        metadata = {
            "total_files": 3,
            "files": {
                "GP01": {"title": "Found", "summary": "F", "keywords": ["k1"]},
                "GP300": {"title": "Topo", "summary": "T", "keywords": []},
                "GP830": {"title": "Unified", "summary": "U"},
            },
        }
        md = mod.generate_markdown_section(metadata)
        # Category headings
        assert "### Foundational" in md
        assert "### Topological" in md
        assert "### Unified" in md
        # Entries include title and summary
        assert "**GP01**: Found" in md
        assert "> F" in md
        # Keywords line present only when non-empty
        assert "Keywords" in md
        # GP830 without keywords should not emit keywords line
        assert "Unified" in md
        assert "Keywords" not in md.split("Unified")[-1]

    def test_other_category_and_zero_pad(self):
        metadata = {
            "total_files": 1,
            "files": {
                "GP999": {"title": "Other", "summary": "X"},
            },
        }
        md = mod.generate_markdown_section(metadata)
        assert "### Other" in md
        assert "GP999" in md


class TestMain:
    def test_missing_metadata_file(self, capsys, tmp_path, monkeypatch):
        monkeypatch.setattr(mod, "ROOT", tmp_path)
        # No build/ingested_metadata.json
        mod.main()
        out = capsys.readouterr().out
        assert "Metadata file not found" in out

    def test_inserts_before_status(self, tmp_path, monkeypatch):
        root = tmp_path
        monkeypatch.setattr(mod, "ROOT", root)
        build_dir = root / "build"
        build_dir.mkdir()
        # Metadata file
        (build_dir / "ingested_metadata.json").write_text(
            json.dumps(
                {
                    "total_files": 1,
                    "files": {
                        "GP01": {"title": "Found", "summary": "F"}
                    },
                }
            )
        )
        roadmap_path = root / "ROADMAP.md"
        roadmap_path.write_text("Header\n## Implementation Status\nBody\n")

        mod.main()

        content = roadmap_path.read_text()
        # New section should appear before Implementation Status
        assert content.index("## Ingested Roadmap") < content.index("## Implementation Status")
        assert "GP01" in content

    def test_creates_new_roadmap_file(self, tmp_path, monkeypatch):
        root = tmp_path
        monkeypatch.setattr(mod, "ROOT", root)
        build_dir = root / "build"
        build_dir.mkdir()
        (build_dir / "ingested_metadata.json").write_text(
            json.dumps(
                {
                    "total_files": 1,
                    "files": {
                        "GP300": {"title": "Topo", "summary": "S", "keywords": ["k"]}
                    },
                }
            )
        )

        mod.main()

        roadmap_path = root / "ROADMAP.md"
        assert roadmap_path.exists()
        content = roadmap_path.read_text()
        assert "GP300" in content
        assert "Topological" in content
        assert "Keywords" in content

    def test_uses_fallback_insertion_point(self, tmp_path, monkeypatch):
        root = tmp_path
        monkeypatch.setattr(mod, "ROOT", root)
        build_dir = root / "build"
        build_dir.mkdir()
        (build_dir / "ingested_metadata.json").write_text(
            json.dumps(
                {
                    "total_files": 1,
                    "files": {
                        "GP700": {"title": "Analysis", "summary": "A"}
                    },
                }
            )
        )
        roadmap_path = root / "ROADMAP.md"
        # No Implementation Status; has See Also
        roadmap_path.write_text("Intro\n## See Also\nRefs\n")

        mod.main()

        content = roadmap_path.read_text()
        assert content.index("## Ingested Roadmap") < content.index("## See Also")
        assert "Analysis" in content

    def test_overwrites_existing_content_correctly(self, tmp_path, monkeypatch):
        root = tmp_path
        monkeypatch.setattr(mod, "ROOT", root)
        build_dir = root / "build"
        build_dir.mkdir()
        (build_dir / "ingested_metadata.json").write_text(
            json.dumps(
                {
                    "total_files": 2,
                    "files": {
                        "GP200": {"title": "Geom", "summary": "G"},
                        "GP201": {"title": "Geom2", "summary": "G2"},
                    },
                }
            )
        )
        roadmap_path = root / "ROADMAP.md"
        roadmap_path.write_text("Intro\n")

        mod.main()

        content = roadmap_path.read_text()
        # Both items present and original intro retained
        assert content.startswith("Intro")
        assert "GP200" in content and "GP201" in content