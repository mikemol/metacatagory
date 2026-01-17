{-# OPTIONS --without-K #-}
-- | Roadmap export and validation targets.
module Examples.Makefile.Targets.RoadmapExports where

open import Agda.Builtin.List using (List; _∷_; [])
open import Examples.MakefileTargets using (MakefileTarget; generatorToTarget; generatorToFileTarget; mutateCert)

-- Roadmap ingestion/export/enrichment targets factored out of ExporterMakefile.agda
roadmapExportTargets : List MakefileTarget
roadmapExportTargets =
  generatorToFileTarget mutateCert "build/canonical_roadmap.json" "Merge ingestion streams into canonical roadmap JSON"
      (".github/roadmap/tasks.json" ∷ "ROADMAP.md" ∷ "src/agda/Plan/CIM/IngestedRoadmaps" ∷ "scripts/merge_roadmaps.py" ∷ [])
      ("python3 scripts/merge_roadmaps.py" ∷ [])
  ∷ generatorToTarget mutateCert "roadmap-merge" "Merge ingestion streams" ("build/canonical_roadmap.json" ∷ [])
      ("@echo \"roadmap merge complete\"" ∷ [])
  ∷ generatorToFileTarget mutateCert "build/ingested_metadata.json" "Ingest GP metadata"
      ("intake/GP" ∷ "scripts/ingest_gp_files.py" ∷ "scripts/shared/gp_intake.py" ∷ [])
      ("python3 scripts/ingest_gp_files.py" ∷ [])
  ∷ generatorToTarget mutateCert "ingested-metadata-validate" "Validate ingested metadata schema" ("build/ingested_metadata.json" ∷ [])
      ("python3 scripts/validate_ingested_metadata.py build/ingested_metadata.json" ∷ [])
  ∷ generatorToTarget mutateCert "canonical-roadmap-validate" "Validate canonical roadmap schema" ("build/canonical_roadmap.json" ∷ [])
      ("python3 scripts/validate_roadmap_items.py build/canonical_roadmap.json" ∷ [])
  ∷ generatorToFileTarget mutateCert "build/canonical_enriched.json" "Enrich canonical roadmap"
      ("data/planning_index.json" ∷ "build/diagrams/agda-deps-full.dot" ∷ "scripts/enrich_canonical.py" ∷ [])
      ("python3 scripts/enrich_canonical.py" ∷ [])
  ∷ generatorToTarget mutateCert "roadmap-enrich" "Enrich roadmap with graph data" ("build/canonical_enriched.json" ∷ [])
      ("@echo \"roadmap enrichment complete\"" ∷ [])
  ∷ generatorToFileTarget mutateCert ".github/roadmap/tasks.json" "Export canonical roadmap to JSON"
      ("data/planning_index.json" ∷ "scripts/export_canonical_json.py" ∷ [])
      ("python3 scripts/export_canonical_json.py" ∷ [])
  ∷ generatorToFileTarget mutateCert "ROADMAP.md" "Export canonical roadmap to Markdown"
      ("data/planning_index.json" ∷ "scripts/export_canonical_md.py" ∷ [])
      ("python3 scripts/export_canonical_md.py" ∷ [])
  ∷ generatorToFileTarget mutateCert "build/reports/tasks_enriched.md" "Export enriched roadmap"
      ("build/canonical_enriched.json" ∷ "scripts/export_enriched_md.py" ∷ [])
      ("python3 scripts/export_enriched_md.py" ∷ [])
  ∷ generatorToTarget mutateCert "roadmap-export-deps" "Export roadmap dependency graph"
      ("build/canonical_enriched.json" ∷ "scripts/export_dependency_graph.py" ∷ [])
      ("python3 scripts/export_dependency_graph.py" ∷ [])
  ∷ generatorToTarget mutateCert "roadmap-validate-json" "Validate canonical JSON" (".github/roadmap/tasks.json" ∷ [])
      ("python3 scripts/validate_json.py" ∷ [])
  ∷ generatorToTarget mutateCert "roadmap-validate-md" "Validate canonical Markdown" ("ROADMAP.md" ∷ [])
      ("python3 scripts/validate_md.py" ∷ [])
  ∷ generatorToTarget mutateCert "roadmap-validate-triangle" "Verify Triangle Identity (Agda <-> JSON <-> MD)"
      ("canonical-roadmap-validate" ∷ "ingested-metadata-validate" ∷ "roadmap-validate-json" ∷ "roadmap-validate-md" ∷ [])
      ("@echo \"✓ Triangle validation complete\"" ∷ [])
  ∷ generatorToFileTarget mutateCert "build/gp_roadmap_sppf.json" "Export SPPF structure"
      ("data/planning_index.json" ∷ "scripts/export_roadmap_sppf.py" ∷ [])
      ("python3 scripts/export_roadmap_sppf.py" ∷ [])
  ∷ generatorToTarget mutateCert "roadmap-all-enriched" "Build all enriched artifacts" ("build/reports/tasks_enriched.md" ∷ "roadmap-export-deps" ∷ [])
      ("@echo \"roadmap all enriched complete\"" ∷ [])
  ∷ []
