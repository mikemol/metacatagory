# Composition Patterns for Python Pipelines

**Date**: January 6, 2026
**Scope**: Document the canonical composition pattern used in the refactored Python scripts so new or legacy scripts can align with shared components.

## Core Components
- Recovery: RecoveryPipeline + RecoveryStrategy in scripts/shared/recovery_pipeline.py (automatic retries, recovery metrics, analyze_recovery_patterns()).
- Phases: Phase and PhaseResult in scripts/shared/pipelines.py (context dict threading, metrics, status enum).
- Logging: StructuredLogger in scripts/shared/logging.py (progress tracking, structured fields) with configure_logging for CLI defaults.
- Provenance: ValidatedProvenance in scripts/shared/validated_provenance.py (schema-checked records, lineage validation, Markdown/JSON reports).
- Validation: Validators in scripts/shared/validation.py (string_validator, dict_validator, optional_validator, one_of_validator, ValidationResult aggregation).

## Pipeline Templates

### 4-Phase (load → normalize/validate → analyze/build → write/report)
```python
logger = configure_logging("example_pipeline", structured=False)
provenance = ValidatedProvenance(system_id="example_pipeline", logger=logger)
strategy = RecoveryStrategy(max_retries=2, backoff_factor=1.0, respect_recoverable=True)
pipeline = RecoveryPipeline(logger=logger, strategy=strategy, name="ExamplePipeline")

pipeline.add_phase(LoadPhase(logger))
pipeline.add_phase(NormalizeOrValidatePhase(logger, provenance))
pipeline.add_phase(AnalyzeOrBuildPhase(logger))
pipeline.add_phase(WriteOrReportPhase(output_path, logger, provenance))

result = pipeline.execute(initial_input)
if result.is_success():
    prov_path = REPO_ROOT / "build" / "reports" / "example_pipeline_provenance.json"
    provenance.generate_validated_report(prov_path)
```

### 3-Phase (load → validate/compare → report)
Use when no intermediate build step exists; same structure minus one phase.

## Provenance Conventions
- system_id per script (examples below) for tracker scoping; artifacts are item ids, input paths, or output paths.
- Add records at validation points and at final writes; keep source_type = ingestion or transformation; include metadata (counts, size_bytes, categories).
- Write reports to build/reports/<script>_provenance.json; Markdown sidecar is emitted automatically.

## Current Refactored Scripts
- [scripts/export_canonical_md.py](../../scripts/export_canonical_md.py): 4 phases, system_id=export_roadmap_md, report build/reports/roadmap_export_provenance.json, ~118 artifacts (117 items + export).
- [scripts/dependency_graph_builder.py](../../scripts/dependency_graph_builder.py): 4 phases, system_id=dependency_graph_builder, report build/reports/dependency_graph_provenance.json, ~256 artifacts (modules + report), Tarjan SCC for cycles.
- [scripts/validate_json.py](../../scripts/validate_json.py): 4 phases, system_id=validate_json, report build/reports/validate_json_provenance.json, ~235 artifacts (canonical + tasks + validation).
- [scripts/validate_json_roundtrip.py](../../scripts/validate_json_roundtrip.py): 3 phases, system_id=validate_json_roundtrip, report build/reports/roundtrip_validation_provenance.json, semantic equivalence checks (module/edge counts).

## Adoption Checklist (new refactors)
1. Bootstrap imports: ensure repo root is on sys.path for scripts.* packages.
2. Create logger (configure_logging), provenance (ValidatedProvenance(system_id=...)), and recovery strategy (max_retries=2, backoff_factor=1.0).
3. Define small Phase subclasses (or CallablePhase) for each step; thread shared context keys intentionally.
4. Add provenance records when items validate and when outputs are written; include counts and file paths.
5. Emit provenance report to build/reports/<name>_provenance.json; surface key metrics in CLI output (counts, retries, validation status).

## Notes
- Use string_validator/dict_validator for lightweight schema checks instead of bespoke validation logic.
- Prefer logger.progress(...) for long loops; include current, total, succeeded, failed.
- Analyze recovery patterns via pipeline.analyze_recovery_patterns() to report retry behavior when non-zero.
