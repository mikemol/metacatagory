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
from typing import Any, Dict, List, Set

# Ensure repository root is importable as a package (scripts.*)
_REPO_ROOT = Path(__file__).resolve().parent.parent
if str(_REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(_REPO_ROOT))

from scripts.shared.paths import REPO_ROOT, BUILD_DIR
from scripts.shared.io import load_json
from scripts.shared.logging import configure_logging, StructuredLogger
from scripts.shared.validation import ValidationResult, dict_validator
from scripts.shared.validated_provenance import ValidatedProvenance
from scripts.shared.recovery_pipeline import RecoveryPipeline, RecoveryStrategy
from scripts.shared.pipelines import Phase, PhaseResult


class LoadJSONFilesPhase(Phase[Path, Dict[str, List[Dict[str, Any]]]]):
    """Phase: Load canonical and tasks.json files."""
    
    def __init__(self, tasks_path: Path, logger: StructuredLogger):
        super().__init__("load_json", "Load JSON files")
        self.tasks_path = tasks_path
        self.logger = logger

    def transform(self, input_data: Path, context: Dict[str, Any]) -> Dict[str, List[Dict[str, Any]]]:
        self.logger.info("Loading canonical planning index", file=str(input_data))
        canonical = load_json(input_data, required=True)
        
        self.logger.info("Loading tasks.json", file=str(self.tasks_path))
        tasks = load_json(self.tasks_path, required=True)
        
        # Ensure we have lists
        if isinstance(canonical, dict):
            canonical = canonical.get('items', [])
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
        canonical_dict = {}
        for item in canonical_items:
            item_id = item.get("id", "")
            if not item_id.startswith("LEGACY-"):
                normalized = self._normalize_item(item)
                canonical_dict[item_id] = normalized
                
                # Track provenance
                self.provenance.add_validated_record(
                    artifact_id=f"canonical_{item_id}",
                    record={
                        'source_type': 'ingestion',
                        'source_id': 'planning_index.json',
                        'source_location': f'item[{item_id}]',
                        'metadata': {'normalized': True}
                    }
                )
        
        # Normalize tasks
        tasks_dict = {}
        for item in tasks_items:
            item_id = item.get("id", "")
            normalized = self._normalize_item(item)
            tasks_dict[item_id] = normalized
            
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
        drift_details = {}
        for item_id in common:
            if canonical_dict[item_id] != tasks_dict[item_id]:
                drifted.append(item_id)
                # Track what drifted
                canonical_item = canonical_dict[item_id]
                tasks_item = tasks_dict[item_id]
                differences = {}
                for key in canonical_item.keys():
                    if canonical_item.get(key) != tasks_item.get(key):
                        differences[key] = {
                            'canonical': canonical_item.get(key),
                            'tasks': tasks_item.get(key)
                        }
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
            print(f"\n❌ Validation failed: run 'make roadmap-export-json' to sync")
            return 1


def validate():
    """Validate tasks.json against canonical using composition pipeline."""
    logger = configure_logging("validate_json", structured=False)
    
    # Initialize validated provenance tracker
    provenance = ValidatedProvenance(system_id="validate_json", logger=logger)
    
    # Configure recovery strategy
    strategy = RecoveryStrategy(max_retries=2, backoff_factor=1.0, respect_recoverable=True)
    pipeline = RecoveryPipeline(logger=logger, strategy=strategy, name="ValidateJSON")
    
    canonical_path = BUILD_DIR / "planning_index.json"
    tasks_path = REPO_ROOT / ".github" / "roadmap" / "tasks.json"
    
    # Build pipeline phases
    pipeline.add_phase(LoadJSONFilesPhase(tasks_path, logger))
    pipeline.add_phase(NormalizeItemsPhase(logger, provenance))
    pipeline.add_phase(CompareItemsPhase(logger))
    pipeline.add_phase(GenerateReportPhase(logger, provenance))
    
    # Execute pipeline
    result = pipeline.execute(canonical_path)
    
    if result.is_success():
        # Generate provenance report
        prov_report_path = BUILD_DIR / "reports" / "validate_json_provenance.json"
        provenance.generate_validated_report(prov_report_path)
        logger.info("Generated provenance report", report_path=str(prov_report_path))
        
        return result.output
    else:
        logger.error("Validation pipeline failed", exception=result.error)
        print(f"❌ Validation pipeline failed: {result.error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(validate())
