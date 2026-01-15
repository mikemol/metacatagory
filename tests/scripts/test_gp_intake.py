#!/usr/bin/env python3
"""Tests for shared GP intake parsing."""

from scripts.shared.gp_intake import extract_metadata_from_text


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
