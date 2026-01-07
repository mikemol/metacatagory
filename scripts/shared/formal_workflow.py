#!/usr/bin/env python3
"""FormalWorkflow: Agda-aware workflow with validation and provenance.

Composite module integrating:
- agda.AgdaParser, DependencyAnalyzer, ModuleCoverage
- validation.ValidationResult for consistency checks
- provenance.ProvenanceTracker for lineage of formal artifacts

Initial scope focuses on workspace analysis, cycle detection, coverage stats,
and report generation with provenance hooks.
"""

from __future__ import annotations
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, List, Optional

from .validation import ValidationResult
from .errors import ValidationError
from .agda import AgdaParser, DependencyAnalyzer, ModuleCoverage, AgdaModule
from .provenance import ProvenanceTracker, ProvenanceSource
from .logging import StructuredLogger


@dataclass
class WorkflowPolicy:
    strict: bool = True  # raise on invalid (e.g., cycles)


class FormalWorkflow:
    """Workspace-level formal analysis and reporting.

    APIs:
    - analyze_workspace: parse Agda modules under a directory
    - verify_no_cycles: validate dependency graph is acyclic
    - generate_verified_report: aggregate stats + optional file outputs
    - to_dict/from_dict: serialization symmetry
    """

    def __init__(
        self,
        src_dir: Path,
        policy: Optional[WorkflowPolicy] = None,
        logger: Optional[StructuredLogger] = None,
        tracker: Optional[ProvenanceTracker] = None,
    ):
        self.src_dir = Path(src_dir)
        self.policy = policy or WorkflowPolicy()
        self.logger = logger or StructuredLogger("FormalWorkflow")
        self.tracker = tracker or ProvenanceTracker(system_id="formal-workflow")
        self.parser = AgdaParser(agda_src_dir=self.src_dir)
        self.deps: Optional[DependencyAnalyzer] = None
        self.coverage: Optional[ModuleCoverage] = None

    def analyze_workspace(self) -> Dict[str, Any]:
        modules = self.parser.parse_directory(self.src_dir)
        self.deps = DependencyAnalyzer(parser=self.parser)
        self.coverage = ModuleCoverage(parser=self.parser)

        # Record provenance for discovered modules
        for m in modules:
            self.tracker.create_trail(m.name).add_source(
                ProvenanceSource.INGESTION, str(m.file_path), metadata={"description": "parsed agda module"}
            )

        stats = {
            "total_modules": len(modules),
            "internal_modules": len(self.parser.get_internal_modules()),
            "undocumented_modules": len(self.parser.get_undocumented_modules()),
        }
        return stats

    def verify_no_cycles(self) -> ValidationResult:
        if not self.deps:
            self.deps = DependencyAnalyzer(parser=self.parser)
        cycles = self.deps.find_cycles()
        result = ValidationResult()
        for cycle in cycles:
            result.add_error("dependencies", "cycle detected", value=" -> ".join(cycle))
        if not result.is_valid() and self.policy.strict:
            raise ValidationError(
                "Agda dependency cycles detected",
                field="dependencies",
                context=result.to_dict(),
            )
        return result

    def generate_verified_report(self, output_path: Optional[Path] = None) -> Dict[str, Any]:
        # Ensure analysis performed
        if not self.deps or not self.coverage:
            self.analyze_workspace()

        cycles = self.deps.find_cycles() if self.deps else []
        coverage_report = self.coverage.get_coverage_report() if self.coverage else {}

        report = {
            "workspace": str(self.src_dir),
            "cycles": cycles,
            "coverage": coverage_report,
            "provenance": self.tracker.generate_report(),
        }

        if output_path is not None:
            json_path = output_path.with_suffix(".json") if output_path.suffix != ".json" else output_path
            try:
                import json as _json
                json_path.write_text(_json.dumps(report, indent=2, default=str))
            except Exception as e:
                raise ValidationError("Failed to write formal report", field="output", value=str(json_path), context={"error": str(e)})

        return report

    def to_dict(self) -> Dict[str, Any]:
        return {
            "src_dir": str(self.src_dir),
            "policy": {"strict": self.policy.strict},
            "tracker": self.tracker.generate_report(),
        }

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "FormalWorkflow":
        src = Path(data.get("src_dir", "."))
        policy = WorkflowPolicy(strict=data.get("policy", {}).get("strict", True))
        tracker = ProvenanceTracker.from_dict(data.get("tracker", {"system_id": "formal-workflow"}))
        return cls(src_dir=src, policy=policy, tracker=tracker)
