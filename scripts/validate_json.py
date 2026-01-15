#!/usr/bin/env python3
"""Validate tasks.json against canonical index (composition-based).

Showcases full integration of shared components:
- RecoveryPipeline for retry semantics
- ValidatedProvenance for lineage tracking
- StructuredLogger for progress tracking
- Validation for schema and content comparison
"""

import json
from pathlib import Path
import sys
import os
from typing import Any, Dict, List, Set, Tuple
from concurrent.futures import ThreadPoolExecutor

# Ensure repository root is importable as a package (scripts.*)
_REPO_ROOT = Path(__file__).resolve().parent.parent
if str(_REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(_REPO_ROOT))

from scripts.shared.parallel import get_parallel_settings
from scripts.shared.paths import REPO_ROOT, BUILD_DIR, REPORTS_DIR
from scripts.shared.io import load_json
from scripts.shared.logging import configure_logging, StructuredLogger
from scripts.shared.validation import ValidationResult, dict_validator
from scripts.shared.validated_provenance import ValidatedProvenance
from scripts.shared.recovery_pipeline import RecoveryPipeline, RecoveryStrategy
from scripts.shared.config import get_config
from scripts import shared_data


def allow_report_write() -> bool:
    return os.environ.get("MUTATE_OK") == "1" and get_config().report_mode == "write"
from scripts.shared.pipelines import Phase, PhaseResult


class LoadJSONFilesPhase(Phase[Path, Dict[str, List[Dict[str, Any]]]]):
    """Phase: Load canonical and tasks.json files."""
    
    def __init__(self, tasks_path: Path, logger: StructuredLogger):
        super().__init__("load_json", "Load JSON files")
        self.tasks_path = tasks_path
        self.logger = logger

    def transform(self, input_data: Path, context: Dict[str, Any]) -> Dict[str, List[Dict[str, Any]]]:
        parallel, workers = get_parallel_settings()
        context["planning_source"] = str(input_data)

        def load_tasks(path: Path) -> Any:
            return load_json(path, required=True)

        def load_canonical(path: Path) -> Any:
            return shared_data.load_planning_index_from(path)

        if parallel and workers > 1:
            self.logger.info("Loading canonical planning index", file=str(input_data), mode="parallel")
            self.logger.info("Loading tasks.json", file=str(self.tasks_path), mode="parallel")
            with ThreadPoolExecutor(max_workers=2) as executor:
                canonical_future = executor.submit(load_canonical, input_data)
                tasks_future = executor.submit(load_tasks, self.tasks_path)
                canonical = canonical_future.result()
                tasks = tasks_future.result()
        else:
            self.logger.info("Loading canonical planning index", file=str(input_data))
            canonical = load_canonical(input_data)
            
            self.logger.info("Loading tasks.json", file=str(self.tasks_path))
            tasks = load_tasks(self.tasks_path)
        
        if isinstance(tasks, dict):
            tasks = tasks.get('items', [])
        
        self.logger.info("Files loaded", 
                        canonical_count=len(canonical),
                        tasks_count=len(tasks))
        
        context['canonical_count'] = len(canonical)
        context['tasks_count'] = len(tasks)
        
        return {'canonical': canonical, 'tasks': tasks}


class NormalizeItemsPhase(Phase[Dict[str, List[Dict[str, Any]]], Dict[str, Dict[str, Dict[str, Any]]]]):
    """Phase: Normalize items for comparison."""
    
    def __init__(self, logger: StructuredLogger, provenance: ValidatedProvenance):
        super().__init__("normalize", "Normalize items")
        self.logger = logger
        self.provenance = provenance

    def _normalize_item(self, item: Dict[str, Any]) -> Dict[str, Any]:
        """Normalize item for comparison."""
        return {
            "id": item.get("id", ""),
            "title": item.get("title", "").strip(),
            "status": item.get("status", ""),
            "category": item.get("category", ""),
            "files": sorted(item.get("files", [])),
            "tags": sorted(item.get("tags", [])),
            "dependsOn": sorted(item.get("dependsOn", [])),
            "provenance": sorted(item.get("provenance", [])),
        }

    def transform(self, input_data: Dict[str, List[Dict[str, Any]]], 
                  context: Dict[str, Any]) -> Dict[str, Dict[str, Dict[str, Any]]]:
        canonical_items = input_data['canonical']
        tasks_items = input_data['tasks']
        
        # Normalize canonical (exclude legacy items)
        parallel, workers = get_parallel_settings()
        canonical_dict: Dict[str, Dict[str, Any]] = {}

        def normalize_canonical(item: Dict[str, Any]) -> Tuple[str, Dict[str, Any]] | None:
            item_id = item.get("id", "")
            if item_id.startswith("LEGACY-"):
                return None
            return item_id, self._normalize_item(item)

        if parallel and workers > 1 and canonical_items:
            with ThreadPoolExecutor(max_workers=workers) as executor:
                results = list(executor.map(normalize_canonical, canonical_items))
        else:
            results = [normalize_canonical(item) for item in canonical_items]

        for result in results:
            if result is None:
                continue
            item_id, normalized = result
            canonical_dict[item_id] = normalized

        for item_id in canonical_dict.keys():
            # Track provenance
            self.provenance.add_validated_record(
                artifact_id=f"canonical_{item_id}",
                record={
                    'source_type': 'ingestion',
                    'source_id': context.get('planning_source', 'planning_index.json'),
                    'source_location': f'item[{item_id}]',
                    'metadata': {'normalized': True}
                }
            )
        
        # Normalize tasks
        tasks_dict: Dict[str, Dict[str, Any]] = {}

        def normalize_task(item: Dict[str, Any]) -> Tuple[str, Dict[str, Any]]:
            item_id = item.get("id", "")
            return item_id, self._normalize_item(item)

        if parallel and workers > 1 and tasks_items:
            with ThreadPoolExecutor(max_workers=workers) as executor:
                task_results = list(executor.map(normalize_task, tasks_items))
        else:
            task_results = [normalize_task(item) for item in tasks_items]

        for item_id, normalized in task_results:
            tasks_dict[item_id] = normalized

        for item_id in tasks_dict.keys():
            # Track provenance
            self.provenance.add_validated_record(
                artifact_id=f"tasks_{item_id}",
                record={
                    'source_type': 'ingestion',
                    'source_id': 'tasks.json',
                    'source_location': f'item[{item_id}]',
                    'metadata': {'normalized': True}
                }
            )
        
        self.logger.progress(
            "Normalized items",
            current=len(canonical_dict) + len(tasks_dict),
            total=context.get('canonical_count', 0) + context.get('tasks_count', 0),
            succeeded=len(canonical_dict) + len(tasks_dict)
        )
        
        context['canonical_normalized'] = len(canonical_dict)
        context['tasks_normalized'] = len(tasks_dict)
        
        return {'canonical': canonical_dict, 'tasks': tasks_dict}


class CompareItemsPhase(Phase[Dict[str, Dict[str, Dict[str, Any]]], Dict[str, Any]]):
    """Phase: Compare canonical and tasks items."""
    
    def __init__(self, logger: StructuredLogger):
        super().__init__("compare", "Compare items")
        self.logger = logger

    def transform(self, input_data: Dict[str, Dict[str, Dict[str, Any]]], 
                  context: Dict[str, Any]) -> Dict[str, Any]:
        canonical_dict = input_data['canonical']
        tasks_dict = input_data['tasks']
        
        canonical_ids = set(canonical_dict.keys())
        tasks_ids = set(tasks_dict.keys())
        
        # Find differences
        only_canonical = canonical_ids - tasks_ids
        only_tasks = tasks_ids - canonical_ids
        common = canonical_ids & tasks_ids
        
        # Check for content drift in common items
        drifted = []
        drift_details: Dict[str, Dict[str, Dict[str, Any]]] = {}

        parallel, workers = get_parallel_settings()

        def compare_one(item_id: str) -> Tuple[str, Dict[str, Dict[str, Any]] | None]:
            if canonical_dict[item_id] == tasks_dict[item_id]:
                return item_id, None
            canonical_item = canonical_dict[item_id]
            tasks_item = tasks_dict[item_id]
            differences: Dict[str, Dict[str, Any]] = {}
            for key in canonical_item.keys():
                if canonical_item.get(key) != tasks_item.get(key):
                    differences[key] = {
                        'canonical': canonical_item.get(key),
                        'tasks': tasks_item.get(key)
                    }
            return item_id, differences

        common_ids = sorted(common)
        if parallel and workers > 1 and common_ids:
            with ThreadPoolExecutor(max_workers=workers) as executor:
                results = list(executor.map(compare_one, common_ids))
        else:
            results = [compare_one(item_id) for item_id in common_ids]

        for item_id, differences in results:
            if differences:
                drifted.append(item_id)
                drift_details[item_id] = differences
        
        report = {
            'only_canonical': sorted(only_canonical),
            'only_tasks': sorted(only_tasks),
            'drifted': sorted(drifted),
            'drift_details': drift_details,
            'common_count': len(common),
            'is_valid': not (only_canonical or only_tasks or drifted)
        }
        
        self.logger.info(
            "Comparison complete",
            only_canonical=len(only_canonical),
            only_tasks=len(only_tasks),
            drifted=len(drifted),
            common=len(common)
        )
        
        context['comparison'] = report
        return report


class GenerateReportPhase(Phase[Dict[str, Any], int]):
    """Phase: Generate validation report and return exit code."""
    
    def __init__(self, logger: StructuredLogger, provenance: ValidatedProvenance):
        super().__init__("report", "Generate report")
        self.logger = logger
        self.provenance = provenance

    def transform(self, input_data: Dict[str, Any], context: Dict[str, Any]) -> int:
        report = input_data
        
        # Track validation result in provenance
        self.provenance.add_validated_record(
            artifact_id="tasks_json_validation",
            record={
                'source_type': 'transformation',
                'source_id': 'validation_comparison',
                'source_location': 'tasks.json_vs_canonical',
                'metadata': {
                    'is_valid': report['is_valid'],
                    'common_count': report['common_count'],
                    'only_canonical': len(report['only_canonical']),
                    'only_tasks': len(report['only_tasks']),
                    'drifted': len(report['drifted'])
                }
            }
        )
        
        # Print results
        if report['only_canonical']:
            print(f"❌ {len(report['only_canonical'])} items only in canonical:")
            for item_id in report['only_canonical'][:10]:
                print(f"  - {item_id}")
            if len(report['only_canonical']) > 10:
                print(f"  ... and {len(report['only_canonical']) - 10} more")
        
        if report['only_tasks']:
            print(f"❌ {len(report['only_tasks'])} items only in tasks.json:")
            for item_id in report['only_tasks'][:10]:
                print(f"  - {item_id}")
            if len(report['only_tasks']) > 10:
                print(f"  ... and {len(report['only_tasks']) - 10} more")
        
        if report['drifted']:
            print(f"❌ {len(report['drifted'])} items drifted:")
            for item_id in report['drifted'][:10]:
                details = report['drift_details'].get(item_id, {})
                print(f"  - {item_id}: {', '.join(details.keys())}")
            if len(report['drifted']) > 10:
                print(f"  ... and {len(report['drifted']) - 10} more")
        
        if report['is_valid']:
            print(f"✓ tasks.json matches canonical ({report['common_count']} items)")
            return 0
        else:
            print(f"\n❌ Validation failed: run 'make .github/roadmap/tasks.json' to sync")
            return 1


def validate():
    """Validate tasks.json against canonical using composition pipeline."""
    logger = configure_logging("validate_json", structured=False)
    
    # Initialize validated provenance tracker
    provenance = ValidatedProvenance(system_id="validate_json", logger=logger)
    
    # Configure recovery strategy
    strategy = RecoveryStrategy(max_retries=2, backoff_factor=1.0, respect_recoverable=True)
    pipeline = RecoveryPipeline(logger=logger, strategy=strategy, name="ValidateJSON")
    
    canonical_path = shared_data.resolve_planning_path(repo_root=REPO_ROOT)
    tasks_path = REPO_ROOT / ".github" / "roadmap" / "tasks.json"
    
    # Build pipeline phases
    pipeline.add_phase(LoadJSONFilesPhase(tasks_path, logger))
    pipeline.add_phase(NormalizeItemsPhase(logger, provenance))
    pipeline.add_phase(CompareItemsPhase(logger))
    pipeline.add_phase(GenerateReportPhase(logger, provenance))
    
    # Execute pipeline
    result = pipeline.execute(canonical_path)
    
    if result.is_success():
        # Generate provenance report only when explicitly enabled.
        prov_report_path = REPORTS_DIR / "validate_json_provenance.json"
        if allow_report_write():
            provenance.generate_validated_report(prov_report_path)
            logger.info("Generated provenance report", report_path=str(prov_report_path))
        else:
            provenance.generate_validated_report(None)
            logger.info("Provenance report suppressed (report writing disabled).")

        return result.output
    else:
        logger.error("Validation pipeline failed", exception=result.error)
        print(f"❌ Validation pipeline failed: {result.error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(validate())
