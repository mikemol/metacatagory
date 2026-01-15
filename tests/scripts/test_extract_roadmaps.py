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
