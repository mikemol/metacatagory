#!/usr/bin/env python3
"""Tests for normalize_generated_markdown.py."""

from pathlib import Path

import scripts.normalize_generated_markdown as mod


def test_parse_error_short_circuits(monkeypatch, tmp_path):
    target = tmp_path / "bad.md"
    target.write_text(
        "```yaml\nkey: [unterminated\n```\n\n# Title\n",
        encoding="utf-8",
    )

    called = {"run": False}

    def fake_run(*args, **kwargs):
        called["run"] = True
        raise AssertionError("markdownlint should not run on parse errors")

    monkeypatch.setattr(mod.subprocess, "run", fake_run)

    result = mod.normalize_markdown(Path(target))
    assert result is False
    assert called["run"] is False
