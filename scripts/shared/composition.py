#!/usr/bin/env python3
"""Reusable composition phases and small pipelines.

Provides general-purpose phases that compose shared SPPF nodes into
concrete workflows. Intended to help "gut" legacy scripts and re-express
their behavior as pipelines.
"""

from __future__ import annotations
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, Generic, Optional, Set, Tuple, TypeVar, Callable
from concurrent.futures import ThreadPoolExecutor
import json
import re

from .pipelines import Phase, PhaseResult, PhaseStatus, PipelineContext
from .logging import StructuredLogger
from .errors import ValidationError
from .recovery_pipeline import RecoveryPipeline, RecoveryStrategy
from .parallel import get_parallel_settings

A = TypeVar('A')
B = TypeVar('B')


class CallablePhase(Phase[A, B]):
    """Adapter phase that calls a provided callable.
    The callable signature: (input_data, context) -> output
    """

    def __init__(self, name: str, fn: Callable[[A, Dict[str, Any]], B], description: str = ""):
        super().__init__(name=name, description=description)
        self._fn = fn

    def transform(self, input_data: A, context: Dict[str, Any]) -> B:
        return self._fn(input_data, context)


class ReadJSONPhase(Phase[Path, Any]):
    def __init__(self, name: str = "read_json"):
        super().__init__(name)

    def transform(self, input_data: Path, context: Dict[str, Any]) -> Any:
        data = json.loads(Path(input_data).read_text(encoding='utf-8'))
        return data


class ReadMarkdownRoadmapTitlesPhase(Phase[Path, Set[str]]):
    """Extract titles from ROADMAP.md in bullet lists with bold titles."""

    def __init__(self, name: str = "read_md_titles"):
        super().__init__(name)
        self._pattern = re.compile(r'[-*]\s+\*\*(.+?)\*\*')

    def transform(self, input_data: Path, context: Dict[str, Any]) -> Set[str]:
        content = Path(input_data).read_text(encoding='utf-8')
        titles = {m.group(1).strip() for m in self._pattern.finditer(content)}
        return titles


class StoreInContextPhase(Phase[Any, Any]):
    """Store the incoming value into context under a key and forward None."""

    def __init__(self, key: str, name: str = "store_context"):
        super().__init__(name)
        self.key = key

    def transform(self, input_data: Any, context: Dict[str, Any]) -> Any:
        context[self.key] = input_data
        return None


class CompareSetTitlesPhase(Phase[Any, int]):
    """Compare canonical titles vs md titles from context; return exit code.
    Expects context keys: 'canonical_titles', 'md_titles'.
    Raises ValidationError in strict mode when missing/extra.
    """

    def __init__(self, strict: bool = True, name: str = "compare_titles"):
        super().__init__(name)
        self.strict = strict

    def transform(self, input_data: Any, context: Dict[str, Any]) -> int:
        canonical: Set[str] = set(context.get('canonical_titles', set()))
        md_titles: Set[str] = set(context.get('md_titles', set()))

        missing = canonical - md_titles
        extra = md_titles - canonical
        overlap = len(canonical & md_titles)

        report = {
            "missing_count": len(missing),
            "extra_count": len(extra),
            "overlap": overlap,
            "missing_sample": sorted(list(missing))[:10],
            "extra_sample": sorted(list(extra))[:10],
        }
        context['validation_report'] = report

        if (missing or extra) and self.strict:
            raise ValidationError(
                "ROADMAP.md mismatch with canonical planning index",
                field="roadmap_titles",
                context=report,
            )
        return 0 if not (missing or extra) else 1


def run_validate_roadmap_md(base_dir: Path, logger: Optional[StructuredLogger] = None, strict: bool = True) -> Tuple[int, Dict[str, Any]]:
    """Run ROADMAP.md vs canonical_planning comparison via RecoveryPipeline.

    Returns (exit_code, context) where context includes 'validation_report'.
    """
    logger = logger or StructuredLogger("validate_roadmap_md")
    strategy = RecoveryStrategy(max_retries=1, backoff_factor=1.0, respect_recoverable=True)
    pipeline = RecoveryPipeline(logger=logger, strategy=strategy, name="ValidateRoadmapMD")

    # Phase 1: read planning_index.json and store canonical_titles
    def extract_canonical_titles(data: Any, ctx: Dict[str, Any]) -> Set[str]:
        return {str(item.get("title", "")).strip() for item in (data or [])}

    pipeline.add_phase(ReadJSONPhase())
    pipeline.add_phase(CallablePhase("extract_canonical_titles", extract_canonical_titles))
    pipeline.add_phase(StoreInContextPhase("canonical_titles"))

    # Phase 2: read ROADMAP.md titles and store
    pipeline.add_phase(ReadMarkdownRoadmapTitlesPhase())
    pipeline.add_phase(StoreInContextPhase("md_titles"))

    # Phase 3: compare
    pipeline.add_phase(CompareSetTitlesPhase(strict=strict))

    context: Dict[str, Any] = {}
    # Seed inputs: planning_index.json then ROADMAP.md then None
    planning_path = base_dir / "data" / "planning_index.json"
    roadmap_path = base_dir / "ROADMAP.md"

    parallel, workers = get_parallel_settings()

    if parallel and workers > 1:
        with ThreadPoolExecutor(max_workers=2) as executor:
            json_future = executor.submit(
                pipeline.execute_phase_with_recovery, pipeline.phases[0], planning_path, {}
            )
            md_future = executor.submit(
                pipeline.execute_phase_with_recovery, pipeline.phases[3], roadmap_path, {}
            )
            r1 = json_future.result()
            r2 = md_future.result()

        if not r1.is_success():
            return 1, context
        if not r2.is_success():
            return 1, context

        # Phase 1.5 extract titles
        r1b = pipeline.execute_phase_with_recovery(pipeline.phases[1], r1.output, context)
        if not r1b.is_success():
            return 1, context
        # Store canonical
        r1c = pipeline.execute_phase_with_recovery(pipeline.phases[2], r1b.output, context)
        if not r1c.is_success():
            return 1, context
        # Store md titles
        r2b = pipeline.execute_phase_with_recovery(pipeline.phases[4], r2.output, context)
        if not r2b.is_success():
            return 1, context
    else:
        # Execute manually to feed different inputs across phases
        # Phase 1
        r1 = pipeline.execute_phase_with_recovery(pipeline.phases[0], planning_path, context)
        if not r1.is_success():
            return 1, context
        # Phase 1.5 extract titles
        r1b = pipeline.execute_phase_with_recovery(pipeline.phases[1], r1.output, context)
        if not r1b.is_success():
            return 1, context
        # Store canonical
        r1c = pipeline.execute_phase_with_recovery(pipeline.phases[2], r1b.output, context)
        if not r1c.is_success():
            return 1, context
        # Phase 2: read MD
        r2 = pipeline.execute_phase_with_recovery(pipeline.phases[3], roadmap_path, context)
        if not r2.is_success():
            return 1, context
        # Store md titles
        r2b = pipeline.execute_phase_with_recovery(pipeline.phases[4], r2.output, context)
        if not r2b.is_success():
            return 1, context
    # Compare
    r3 = pipeline.execute_phase_with_recovery(pipeline.phases[5], None, context)
    exit_code = 0 if r3.is_success() and r3.output == 0 else 1
    return exit_code, context
