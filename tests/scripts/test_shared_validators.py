#!/usr/bin/env python3
"""Tests for shared_validators.py"""

import json
from pathlib import Path

from scripts.shared_validators import run_all_validations


def test_run_all_validations_uses_base_dir(tmp_path: Path):
    data_dir = tmp_path / "data"
    data_dir.mkdir(parents=True, exist_ok=True)
    planning_path = data_dir / "planning_index.json"
    planning_path.write_text(
        json.dumps(
            [
                {
                    "id": "GP-1",
                    "title": "Alpha",
                    "description": "Alpha desc",
                    "status": "in-progress",
                    "category": "Build",
                    "dependsOn": [],
                }
            ]
        ),
        encoding="utf-8",
    )

    roadmap_path = tmp_path / "ROADMAP.md"
    roadmap_path.write_text(
        """```yaml
id: GP-1
title: Alpha
description: Alpha desc
status: in-progress
category: Build
dependencies: []
```
""",
        encoding="utf-8",
    )

    assert run_all_validations(base_dir=tmp_path) is True
