#!/usr/bin/env python3
"""ValidatedProvenance: Provenance tracking with schema and lineage checks.

Composite module integrating:
- validation: Schema validation and error accumulation
- provenance: Tracker and trail management
- errors: Deterministic ValidationError semantics

CHIP-N+1 Protocol applied:
- 1.1.1: Interface-contract specification for record validation
- 2.1.1: Temporal coordination via lineage consistency checks
- 3.1.1: Contextual adaptation with structured error context
"""

from __future__ import annotations
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Callable, Dict, Optional, Tuple, List

from .errors import ValidationError, ScriptError
from .validation import (
    ValidationResult,
    dict_validator,
    string_validator,
    optional_validator,
    one_of_validator,
)
from .provenance import (
    ProvenanceTracker,
    ProvenanceTrail,
    ProvenanceRecord,
    ProvenanceSource,
)
from .logging import StructuredLogger


ValidatorFn = Callable[[Any, str], ValidationResult]


def default_record_validator() -> ValidatorFn:
    """Validator for ProvenanceRecord-like dictionaries.
    
    Validates the shape expected by ProvenanceRecord.to_dict().
    """
    return dict_validator(
        field_validators={
            "source_type": one_of_validator([s.value for s in ProvenanceSource]),
            "source_id": string_validator(non_empty=True),
            "source_location": optional_validator(string_validator(non_empty=False)),
            # timestamp is optional ISO string; accept any string if provided
            "timestamp": optional_validator(string_validator()),
            "metadata": optional_validator(dict_validator()),
        },
        required_fields=["source_type", "source_id"],
        allow_extra=False,
    )


@dataclass
class ValidationPolicy:
    """Policy controlling validation behavior."""
    strict: bool = True  # raise on invalid
    validate_records: bool = True
    validate_lineage: bool = True


class ValidatedProvenance:
    """Provenance wrapper with schema and lineage validation.

    - add_validated_record(): validates record structure before adding
    - validate_lineage_consistency(): checks related links and cycles
    - generate_validated_report(): validates and returns report + summary
    """

    def __init__(
        self,
        system_id: str,
        tracker: Optional[ProvenanceTracker] = None,
        record_validator: Optional[ValidatorFn] = None,
        policy: Optional[ValidationPolicy] = None,
        logger: Optional[StructuredLogger] = None,
    ):
        self.tracker = tracker or ProvenanceTracker(system_id)
        self.record_validator = record_validator or default_record_validator()
        self.policy = policy or ValidationPolicy()
        self.logger = logger or StructuredLogger("ValidatedProvenance")

    # --- Record operations ---
    def add_validated_record(
        self,
        artifact_id: str,
        record: Dict[str, Any] | ProvenanceRecord,
    ) -> ProvenanceRecord:
        """Validate and add a record to artifact's trail.

        Args:
            artifact_id: Target artifact id
            record: Dict (ProvenanceRecord.to_dict() shape) or ProvenanceRecord

        Returns:
            The ProvenanceRecord added

        Raises:
            ValidationError: on invalid structure (strict policy)
        """
        rec_obj: ProvenanceRecord
        if isinstance(record, ProvenanceRecord):
            rec_obj = record
        else:
            # Validate structure first
            if self.policy.validate_records:
                vr = self.record_validator(record, path="record")
                if not vr.is_valid():
                    msg = "Provenance record does not conform to schema"
                    if self.policy.strict:
                        raise ValidationError(msg, field="record", context=vr.to_dict())
                    # non-strict: log and continue by attempting best-effort parsing
                    self.logger.warning(msg, validation=vr.to_dict())
            # Parse to ProvenanceRecord (will raise ValueError if invalid enums)
            rec_obj = ProvenanceRecord.from_dict(record)

        trail = self.tracker.create_trail(artifact_id)
        trail.add_record(rec_obj)
        return rec_obj

    # --- Lineage consistency ---
    def validate_lineage_consistency(self, artifact_id: str) -> ValidationResult:
        """Validate lineage graph consistency for the artifact.

        Checks:
        - All related artifacts exist in the tracker
        - No immediate self-link
        - Detect simple cycles via DFS
        """
        result = ValidationResult()

        trail = self.tracker.get_trail(artifact_id)
        if not trail:
            # No trail -> valid but note absence
            return result

        # Check existence of related artifacts
        for related in trail.related_artifacts:
            if related == artifact_id:
                result.add_error("related", "artifact cannot relate to itself", related)
            if related not in self.tracker.trails:
                result.add_error("related", "missing related artifact", related, "Create related trail first")

        # Detect cycles using DFS
        visited: set[str] = set()
        stack: set[str] = set()

        def dfs(aid: str) -> None:
            if aid in stack:
                result.add_error("lineage", "cycle detected", aid)
                return
            if aid in visited:
                return
            visited.add(aid)
            stack.add(aid)
            t = self.tracker.get_trail(aid)
            if t:
                for rel in t.related_artifacts:
                    dfs(rel)
            stack.remove(aid)

        dfs(artifact_id)
        return result

    # --- Reporting ---
    def generate_validated_report(self, output_path: Optional[Path] = None) -> Dict[str, Any]:
        """Validate tracker state, then generate provenance report.

        Returns dictionary with keys:
        - report: tracker JSON report
        - validation: summary dict with error_count and errors
        """
        # Validate all artifacts
        aggregate = ValidationResult()
        for aid in sorted(self.tracker.trails.keys()):
            lr = self.validate_lineage_consistency(aid)
            aggregate.merge(lr, path_prefix=aid)

        if not aggregate.is_valid() and self.policy.strict and self.policy.validate_lineage:
            raise ValidationError(
                "Lineage consistency validation failed",
                field="lineage",
                context=aggregate.to_dict(),
            )

        report = self.tracker.generate_report(output_path=None)

        # Optionally write a Markdown report for human consumption
        if output_path is not None:
            # Write both JSON and Markdown side-by-side using stem
            json_path = output_path.with_suffix(".json") if output_path.suffix != ".json" else output_path
            md_path = output_path.with_suffix(".md")
            from pathlib import Path as _P
            # JSON
            try:
                json_path.write_text(__import__("json").dumps(report, indent=2, default=str))
            except Exception as e:  # Re-raise as ValidationError to keep composite semantics
                raise ValidationError("Failed to write validated report JSON", field="output", value=str(json_path), context={"error": str(e)})
            # Markdown
            try:
                md_content = self.tracker.generate_markdown_report()
                md_path.write_text(md_content)
            except Exception as e:
                raise ValidationError("Failed to write validated report Markdown", field="output", value=str(md_path), context={"error": str(e)})

        return {
            "report": report,
            "validation": aggregate.to_dict(),
        }

    # --- Serialization ---
    def to_dict(self) -> Dict[str, Any]:
        return {
            "tracker": self.tracker.generate_report(),
            "policy": {
                "strict": self.policy.strict,
                "validate_records": self.policy.validate_records,
                "validate_lineage": self.policy.validate_lineage,
            },
        }

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "ValidatedProvenance":
        tracker = ProvenanceTracker.from_dict(data.get("tracker", {"system_id": "unknown"}))
        pol = data.get("policy", {})
        policy = ValidationPolicy(
            strict=pol.get("strict", True),
            validate_records=pol.get("validate_records", True),
            validate_lineage=pol.get("validate_lineage", True),
        )
        return cls(system_id=tracker.system_id, tracker=tracker, policy=policy)
