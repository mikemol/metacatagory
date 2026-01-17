"""Formalism adapter spec for universal construction mapping."""

from __future__ import annotations

import json
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Dict, List, Optional

from scripts.shared.validation import ValidationResult
from scripts.shared.validation import (
    dict_validator,
    list_validator,
    string_validator,
)


@dataclass
class ConstructionSignature:
    """Canonical signature for a universal construction."""

    construction_id: str
    kind: str
    objects: List[str]
    morphisms: List[str]
    property_id: str
    notes: str = ""
    sources: List[str] = field(default_factory=list)

    @staticmethod
    def from_dict(payload: Dict[str, Any]) -> "ConstructionSignature":
        return ConstructionSignature(
            construction_id=payload["construction_id"],
            kind=payload["kind"],
            objects=payload.get("objects", []),
            morphisms=payload.get("morphisms", []),
            property_id=payload["property_id"],
            notes=payload.get("notes", ""),
            sources=payload.get("sources", []),
        )


@dataclass
class FormalismAdapter:
    """Adapter describing how a formalism maps to canonical constructions."""

    formalism_id: str
    version: str
    constructions: List[ConstructionSignature]
    metadata: Dict[str, Any] = field(default_factory=dict)

    @staticmethod
    def from_dict(payload: Dict[str, Any]) -> "FormalismAdapter":
        constructions = [
            ConstructionSignature.from_dict(item)
            for item in payload.get("constructions", [])
        ]
        return FormalismAdapter(
            formalism_id=payload["formalism_id"],
            version=payload["version"],
            constructions=constructions,
            metadata=payload.get("metadata", {}),
        )


def construction_signature_validator(value: Any, path: str = "") -> ValidationResult:
    result = ValidationResult()
    result.merge(
        dict_validator(
            {
                "construction_id": string_validator(),
                "kind": string_validator(),
                "objects": list_validator(string_validator()),
                "morphisms": list_validator(string_validator()),
                "property_id": string_validator(),
                "notes": string_validator(),
                "sources": list_validator(string_validator()),
            },
            required_fields=["construction_id", "kind", "property_id"],
            allow_extra=True,
        )(value, path)
    )
    return result


def formalism_adapter_validator(value: Any, path: str = "") -> ValidationResult:
    result = ValidationResult()
    result.merge(
        dict_validator(
            {
                "formalism_id": string_validator(),
                "version": string_validator(),
                "constructions": list_validator(construction_signature_validator),
                "metadata": dict_validator({}, allow_extra=True),
            },
            required_fields=["formalism_id", "version", "constructions"],
            allow_extra=True,
        )(value, path)
    )
    return result


def load_formalism_adapter(path: Path) -> FormalismAdapter:
    payload = json.loads(path.read_text())
    result = formalism_adapter_validator(payload, path=str(path))
    result.raise_if_invalid("Formalism adapter schema validation failed")
    return FormalismAdapter.from_dict(payload)
