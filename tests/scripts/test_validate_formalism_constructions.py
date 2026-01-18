"""Tests for validate_formalism_constructions.py."""

from __future__ import annotations

from scripts.validate_formalism_constructions import validate_payload


def _payload(constructions):
    return {
        "formalism_id": "test-formalism",
        "version": "0.1",
        "constructions": constructions,
        "metadata": {},
    }


def test_validate_payload_accepts_valid_constructions():
    payload = _payload(
        [
            {
                "construction_id": "product",
                "kind": "limit",
                "objects": ["A", "B"],
                "morphisms": ["p1", "p2"],
                "property_id": "universal-property",
                "notes": "",
                "sources": [],
            },
            {
                "construction_id": "coproduct",
                "kind": "colimit",
                "objects": ["A", "B"],
                "morphisms": ["i1", "i2"],
                "property_id": "universal-property",
                "notes": "",
                "sources": [],
            },
        ]
    )

    assert validate_payload(payload, "payload") is True


def test_validate_payload_rejects_missing_constructions():
    payload = _payload([])

    assert validate_payload(payload, "payload") is False


def test_validate_payload_rejects_duplicate_ids():
    payload = _payload(
        [
            {
                "construction_id": "product",
                "kind": "limit",
                "objects": [],
                "morphisms": [],
                "property_id": "universal-property",
            },
            {
                "construction_id": "product",
                "kind": "limit",
                "objects": [],
                "morphisms": [],
                "property_id": "universal-property",
            },
        ]
    )

    assert validate_payload(payload, "payload") is False


def test_validate_payload_rejects_missing_formalism_id():
    payload = _payload(
        [
            {
                "construction_id": "product",
                "kind": "limit",
                "objects": [],
                "morphisms": [],
                "property_id": "universal-property",
            }
        ]
    )
    payload.pop("formalism_id")

    assert validate_payload(payload, "payload") is False
