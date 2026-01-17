import json

from scripts.shared.formalism_adapter import (
    formalism_adapter_validator,
    load_formalism_adapter,
)


def test_formalism_adapter_validator_accepts_valid_payload():
    payload = {
        "formalism_id": "example-formalism",
        "version": "1.0",
        "constructions": [
            {
                "construction_id": "Product",
                "kind": "limit",
                "objects": ["A", "B"],
                "morphisms": [],
                "property_id": "ProductProperty",
            }
        ],
    }

    result = formalism_adapter_validator(payload, path="adapter")
    assert result.is_valid()


def test_formalism_adapter_validator_rejects_missing_fields():
    payload = {"formalism_id": "example"}

    result = formalism_adapter_validator(payload, path="adapter")
    assert not result.is_valid()
    assert result.errors


def test_load_formalism_adapter_reads_and_validates(tmp_path):
    payload = {
        "formalism_id": "example-formalism",
        "version": "1.0",
        "constructions": [
            {
                "construction_id": "Adjunction",
                "kind": "adjunction",
                "objects": ["C", "D"],
                "morphisms": [],
                "property_id": "AdjunctionProperty",
                "notes": "Sketch",
                "sources": ["docs/example.md"],
            }
        ],
        "metadata": {"owner": "tests"},
    }

    path = tmp_path / "adapter.json"
    path.write_text(json.dumps(payload))

    adapter = load_formalism_adapter(path)
    assert adapter.formalism_id == "example-formalism"
    assert adapter.constructions[0].construction_id == "Adjunction"
