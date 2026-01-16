#!/usr/bin/env python3
"""Tests for scripts.shared.intake helpers."""

from scripts.shared.intake import (
    classify_intake_filename,
    find_roadmap_ids,
    is_shard_filename,
)


def test_find_roadmap_ids() -> None:
    text = "GP-1 PHASE-2 ROADMAP-MD-3 and ignore GP-."
    ids = find_roadmap_ids(text)
    assert ids == {"GP-1", "PHASE-2", "ROADMAP-MD-3"}


def test_is_shard_filename() -> None:
    assert is_shard_filename("__.md")
    assert is_shard_filename("__(1).md")
    assert not is_shard_filename("note.md")


def test_classify_intake_filename() -> None:
    assert classify_intake_filename("__.md") == "shard"
    assert classify_intake_filename("draft_notes.md") == "candidate"
    assert classify_intake_filename("context_summary.md") == "substrate"
    assert classify_intake_filename("gp123.md") == "formalized"
