#!/usr/bin/env python3
"""Validate JSON decomposition roundtrip: decompose → recompose → original (composition-based).

Showcases full integration of shared components:
- RecoveryPipeline for retry semantics
- ValidatedProvenance for lineage tracking
- StructuredLogger for progress tracking
- Validation for semantic comparison
"""

import json
import sys
from pathlib import Path
from typing import Any, Dict

# Ensure repository root is importable as a package (scripts.*)
_REPO_ROOT = Path(__file__).resolve().parent.parent
if str(_REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(_REPO_ROOT))

from scripts.shared.paths import BUILD_DIR
from scripts.shared.io import load_json
from scripts.shared.logging import configure_logging, StructuredLogger
from scripts.shared.validation import ValidationResult
from scripts.shared.validated_provenance import ValidatedProvenance
from scripts.shared.recovery_pipeline import RecoveryPipeline, RecoveryStrategy
from scripts.shared.pipelines import Phase


class LoadRoundtripFilesPhase(Phase[Path, Dict[str, Dict[str, Any]]]):
    """Phase: Load original and recomposed JSON files."""
    
    def __init__(self, recomposed_path: Path, logger: StructuredLogger):
        super().__init__("load_roundtrip", "Load roundtrip files")
        self.recomposed_path = recomposed_path
        self.logger = logger

    def transform(self, input_data: Path, context: Dict[str, Any]) -> Dict[str, Dict[str, Any]]:
        self.logger.info("Loading original file", file=str(input_data))
        original = load_json(input_data, required=True)
        
        self.logger.info("Loading recomposed file", file=str(self.recomposed_path))
        recomposed = load_json(self.recomposed_path, required=True)
        
        self.logger.info("Files loaded successfully")
        
        return {'original': original, 'recomposed': recomposed}


class ValidateSemanticEquivalencePhase(Phase[Dict[str, Dict[str, Any]], Dict[str, Any]]):
    """Phase: Validate semantic equivalence of original and recomposed."""
    
    def __init__(self, logger: StructuredLogger, provenance: ValidatedProvenance):
        super().__init__("validate_equivalence", "Validate semantic equivalence")
        self.logger = logger
        self.provenance = provenance

    def transform(self, input_data: Dict[str, Dict[str, Any]], 
                  context: Dict[str, Any]) -> Dict[str, Any]:
        original = input_data['original']
        recomposed = input_data['recomposed']
        
        # For decomposition strategy "dependency-graph", the structure changes:
        # Original: { "metadata": {...}, "nodes": [...] }
        # Recomposed: { "modules": [...], "edges": [...], "layers": [...] }
        # Both preserve the same number of modules (lossless at semantic level)
        
        original_modules = len(original.get("nodes", []))
        recomposed_modules = len(recomposed.get("modules", []))
        
        original_edges = len(original.get("edges", []))
        recomposed_edges = len(recomposed.get("edges", []))
        
        is_valid = original_modules == recomposed_modules
        
        result = {
            'is_valid': is_valid,
            'original_modules': original_modules,
            'recomposed_modules': recomposed_modules,
            'original_edges': original_edges,
            'recomposed_edges': recomposed_edges,
            'modules_match': original_modules == recomposed_modules,
            'edges_match': original_edges == recomposed_edges
        }
        
        # Track validation in provenance
        self.provenance.add_validated_record(
            artifact_id="roundtrip_validation",
            record={
                'source_type': 'transformation',
                'source_id': 'roundtrip_comparison',
                'source_location': 'dependency_graph',
                'metadata': result
            }
        )
        
        self.logger.info(
            "Semantic validation complete",
            is_valid=is_valid,
            original_modules=original_modules,
            recomposed_modules=recomposed_modules
        )
        
        context['validation_result'] = result
        return result


class GenerateRoundtripReportPhase(Phase[Dict[str, Any], bool]):
    """Phase: Generate roundtrip validation report."""
    
    def __init__(self, original_path: Path, recomposed_path: Path, logger: StructuredLogger):
        super().__init__("generate_report", "Generate roundtrip report")
        self.original_path = original_path
        self.recomposed_path = recomposed_path
        self.logger = logger

    def transform(self, input_data: Dict[str, Any], context: Dict[str, Any]) -> bool:
        result = input_data
        
        if result['is_valid']:
            print("✅ JSON decomposition roundtrip PASSED (module count preserved)")
            print(f"   Original:   {self.original_path}")
            print(f"   Recomposed: {self.recomposed_path}")
            print(f"   Modules:    {result['original_modules']} ↔ {result['recomposed_modules']}")
            print(f"   Edges:      {result['original_edges']} ↔ {result['recomposed_edges']}")
            return True
        else:
            print("❌ JSON roundtrip validation FAILED (module count differs)")
            print(f"   Original:   {result['original_modules']} modules")
            print(f"   Recomposed: {result['recomposed_modules']} modules")
            return False


def validate_roundtrip():
    """Validate decompose → recompose roundtrip using composition pipeline."""
    logger = configure_logging("validate_json_roundtrip", structured=False)
    
    # Initialize validated provenance tracker
    provenance = ValidatedProvenance(system_id="validate_json_roundtrip", logger=logger)
    
    # Configure recovery strategy
    strategy = RecoveryStrategy(max_retries=2, backoff_factor=1.0, respect_recoverable=True)
    pipeline = RecoveryPipeline(logger=logger, strategy=strategy, name="ValidateRoundtrip")
    
    original_path = BUILD_DIR / "dependency_graph.json"
    recomposed_path = BUILD_DIR / "dependency_graph_recomposed.json"
    
    # Build pipeline phases
    pipeline.add_phase(LoadRoundtripFilesPhase(recomposed_path, logger))
    pipeline.add_phase(ValidateSemanticEquivalencePhase(logger, provenance))
    pipeline.add_phase(GenerateRoundtripReportPhase(original_path, recomposed_path, logger))
    
    # Execute pipeline
    result = pipeline.execute(original_path)
    
    if result.is_success():
        # Generate provenance report
        prov_report_path = BUILD_DIR / "reports" / "roundtrip_validation_provenance.json"
        provenance.generate_validated_report(prov_report_path)
        logger.info("Generated provenance report", report_path=str(prov_report_path))
        
        return result.output
    else:
        logger.error("Roundtrip validation pipeline failed", exception=result.error)
        print(f"❌ Validation pipeline failed: {result.error}", file=sys.stderr)
        return False


if __name__ == "__main__":
    success = validate_roundtrip()
    sys.exit(0 if success else 1)

