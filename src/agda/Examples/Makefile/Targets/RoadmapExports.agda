{-# OPTIONS --without-K #-}
-- | Roadmap export and validation targets.
module Examples.Makefile.Targets.RoadmapExports where

open import Agda.Builtin.List using (List; _∷_; [])
open import Examples.MakefileTargets using (MakefileTarget; generatorToTarget; generatorToFileTarget; mutateCert)

-- Roadmap ingestion/export/enrichment targets factored out of ExporterMakefile.agda
roadmapExportTargets : List MakefileTarget
roadmapExportTargets =
  generatorToTarget mutateCert "roadmap-merge" "Merge ingestion streams" ([]) ("python3 scripts/merge_roadmaps.py" ∷ [])
  ∷ generatorToFileTarget mutateCert "build/ingested_metadata.json" "Ingest GP metadata" ([]) ("python3 scripts/ingest_gp_files.py" ∷ [])
  ∷ generatorToFileTarget mutateCert "build/canonical_enriched.json" "Enrich canonical roadmap" ("data/planning_index.json" ∷ "build/diagrams/agda-deps-full.dot" ∷ [])
      ("python3 scripts/enrich_canonical.py" ∷ [])
  ∷ generatorToTarget mutateCert "roadmap-enrich" "Enrich roadmap with graph data" ("build/canonical_enriched.json" ∷ [])
      ("@echo \"roadmap enrichment complete\"" ∷ [])
  ∷ generatorToFileTarget mutateCert ".github/roadmap/tasks.json" "Export canonical roadmap to JSON" ("data/planning_index.json" ∷ [])
      ("python3 scripts/export_canonical_json.py" ∷ [])
  ∷ generatorToFileTarget mutateCert "ROADMAP.md" "Export canonical roadmap to Markdown" ("data/planning_index.json" ∷ [])
      ("python3 scripts/export_canonical_md.py" ∷ [])
  ∷ generatorToFileTarget mutateCert "build/reports/tasks_enriched.md" "Export enriched roadmap" ("build/canonical_enriched.json" ∷ [])
      ("python3 scripts/export_enriched_md.py" ∷ [])
  ∷ generatorToTarget mutateCert "roadmap-export-deps" "Export roadmap dependency graph" ("build/canonical_enriched.json" ∷ [])
      ("python3 scripts/export_dependency_graph.py" ∷ [])
  ∷ generatorToTarget mutateCert "roadmap-validate-json" "Validate canonical JSON" (".github/roadmap/tasks.json" ∷ [])
      ("python3 scripts/validate_json.py" ∷ [])
  ∷ generatorToTarget mutateCert "roadmap-validate-md" "Validate canonical Markdown" ("ROADMAP.md" ∷ [])
      ("python3 scripts/validate_md.py" ∷ [])
  ∷ generatorToTarget mutateCert "roadmap-validate-triangle" "Verify Triangle Identity (Agda <-> JSON <-> MD)" ("roadmap-validate-json" ∷ "roadmap-validate-md" ∷ "build/ingested_metadata.json" ∷ [])
      ("@echo \"✓ Triangle validation complete\"" ∷ [])
  ∷ generatorToFileTarget mutateCert "build/gp_roadmap_sppf.json" "Export SPPF structure" ("data/planning_index.json" ∷ [])
      ("python3 scripts/export_roadmap_sppf.py" ∷ [])
  ∷ generatorToTarget mutateCert "roadmap-all-enriched" "Build all enriched artifacts" ("build/reports/tasks_enriched.md" ∷ "roadmap-export-deps" ∷ [])
      ("@echo \"roadmap all enriched complete\"" ∷ [])
  ∷ []
