#!/usr/bin/env python3
"""Tests for ingested metadata schema validation."""

from scripts.shared.validation import ingested_metadata_validator


def test_ingested_metadata_validator_accepts_valid_payload():
    payload = {
        "total_files": 1,
        "files": {
            "GP01": {
                "title": "Title",
                "summary": "Summary",
                "keywords": [],
                "insight": "",
                "gap": "",
                "fix": "",
                "target_module": "src/agda/Plan/CIM/Utility.agda",
            }
        },
    }
    result = ingested_metadata_validator(payload)
    assert result.is_valid()


def test_ingested_metadata_validator_requires_target_module():
    payload = {
        "total_files": 1,
        "files": {
            "GP01": {
                "title": "Title",
                "summary": "Summary",
            }
        },
    }
    result = ingested_metadata_validator(payload)
    assert not result.is_valid()
