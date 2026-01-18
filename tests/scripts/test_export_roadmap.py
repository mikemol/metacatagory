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
from scripts.shared.config import Config


class TestGenerateMarkdownSection:
    def test_groups_by_category_and_sorts(self):
        metadata = {
            "total_files": 3,
            "files": {
                "GP01": {"title": "Found", "summary": "F", "keywords": ["k1"], "target_module": "src/agda/Plan/CIM/Implementation.agda"},
                "GP300": {"title": "Topo", "summary": "T", "keywords": [], "target_module": "src/agda/Plan/CIM/Implementation.agda"},
                "GP830": {"title": "Unified", "summary": "U", "target_module": "src/agda/Plan/CIM/Implementation.agda"},
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
                "GP999": {"title": "Other", "summary": "X", "target_module": "src/agda/Plan/CIM/Implementation.agda"},
            },
        }
        md = mod.generate_markdown_section(metadata)
        assert "### Unified (800-899)" in md
        assert "GP999" in md

    def test_additional_categories(self):
        """Cover structural, semantic, and polytope groupings."""
        metadata = {
            "total_files": 3,
            "files": {
                "GP150": {"title": "Struct", "summary": "S", "target_module": "src/agda/Plan/CIM/Implementation.agda"},
                "GP450": {"title": "Semantic", "summary": "Sem", "target_module": "src/agda/Plan/CIM/Implementation.agda"},
                "GP550": {"title": "Poly", "summary": "P", "target_module": "src/agda/Plan/CIM/Implementation.agda"},
            },
        }

        md = mod.generate_markdown_section(metadata)

        assert "Structural (100-199)" in md
        assert "Homological (400-499)" in md
        assert "Polytope (500-599)" in md


class TestMain:
    def test_missing_metadata_file(self, capsys, tmp_path):
        config = Config(repo_root=tmp_path)
        # No build/ingested_metadata.json
        mod.main(config)
        out = capsys.readouterr().out
        assert "Metadata file not found" in out

    def test_inserts_before_status(self, tmp_path):
        config = Config(repo_root=tmp_path)
        build_dir = tmp_path / "build"
        build_dir.mkdir()
        # Metadata file
        (build_dir / "ingested_metadata.json").write_text(
            json.dumps(
                {
                    "total_files": 1,
                    "files": {
                        "GP01": {"title": "Found", "summary": "F", "target_module": "src/agda/Plan/CIM/Implementation.agda"}
                    },
                }
            )
        )
        roadmap_path = tmp_path / "ROADMAP.md"
        roadmap_path.write_text("Header\n## Implementation Status\nBody\n")

        mod.main(config)

        content = roadmap_path.read_text()
        # New section should appear before Implementation Status
        assert content.index("## Ingested Roadmap") < content.index("## Implementation Status")
        assert "GP01" in content

    def test_creates_new_roadmap_file(self, tmp_path):
        config = Config(repo_root=tmp_path)
        build_dir = tmp_path / "build"
        build_dir.mkdir()
        (build_dir / "ingested_metadata.json").write_text(
            json.dumps(
                {
                    "total_files": 1,
                    "files": {
                        "GP300": {"title": "Topo", "summary": "S", "keywords": ["k"], "target_module": "src/agda/Plan/CIM/Implementation.agda"}
                    },
                }
            )
        )

        mod.main(config)

        roadmap_path = tmp_path / "ROADMAP.md"
        assert roadmap_path.exists()
        content = roadmap_path.read_text()
        assert "GP300" in content
        assert "Topological" in content
        assert "Keywords" in content

    def test_uses_fallback_insertion_point(self, tmp_path):
        config = Config(repo_root=tmp_path)
        build_dir = tmp_path / "build"
        build_dir.mkdir()
        (build_dir / "ingested_metadata.json").write_text(
            json.dumps(
                {
                    "total_files": 1,
                    "files": {
                        "GP700": {"title": "Analysis", "summary": "A", "target_module": "src/agda/Plan/CIM/Implementation.agda"}
                    },
                }
            )
        )
        roadmap_path = tmp_path / "ROADMAP.md"
        # No Implementation Status; has See Also
        roadmap_path.write_text("Intro\n## See Also\nRefs\n")

        mod.main(config)

        content = roadmap_path.read_text()
        assert content.index("## Ingested Roadmap") < content.index("## See Also")
        assert "Analysis" in content

    def test_overwrites_existing_content_correctly(self, tmp_path):
        config = Config(repo_root=tmp_path)
        build_dir = tmp_path / "build"
        build_dir.mkdir()
        (build_dir / "ingested_metadata.json").write_text(
            json.dumps(
                {
                    "total_files": 2,
                    "files": {
                        "GP200": {"title": "Geom", "summary": "G", "target_module": "src/agda/Plan/CIM/Implementation.agda"},
                        "GP201": {"title": "Geom2", "summary": "G2", "target_module": "src/agda/Plan/CIM/Implementation.agda"},
                    },
                }
            )
        )
        roadmap_path = tmp_path / "ROADMAP.md"
        roadmap_path.write_text("Intro\n")

        mod.main(config)

        content = roadmap_path.read_text()
        # Both items present and original intro retained
        assert content.startswith("Intro")
        assert "GP200" in content and "GP201" in content

    def test_main_guard_executes(self, tmp_path):
        """Execute __main__ block without touching repository files."""
        fake_root = tmp_path
        build_dir = fake_root / "build"
        build_dir.mkdir()
        roadmap_path = fake_root / "ROADMAP.md"
        (build_dir / "ingested_metadata.json").write_text(
            json.dumps(
                {
                    "total_files": 1,
                    "files": {"GP150": {"title": "Struct", "summary": "S", "target_module": "src/agda/Plan/CIM/Implementation.agda"}},
                }
            )
        )

        script_path = Path(mod.__file__)

        # Create a modified version of the script that uses the test config
        code = script_path.read_text()
        # Replace the main() call to use our config
        code = code.replace(
            "if __name__ == '__main__':\n    main()",
            "if __name__ == '__main__':\n    main(Config(repo_root=__test_root__))"
        )

        exec_globals = {
            "__name__": "__main__",
            "__file__": str(script_path),
            "__builtins__": __builtins__,
            "Config": Config,
            "__test_root__": fake_root,
        }
        exec(compile(code, str(script_path), "exec"), exec_globals)

        assert roadmap_path.exists()
        content = roadmap_path.read_text()
        assert "GP150" in content
