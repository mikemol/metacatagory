{-# OPTIONS --without-K #-}
module Examples.Makefile.Targets.RoadmapExports where

open import Agda.Builtin.List using (List; _∷_; [])
open import Examples.MakefileTargets using (MakefileTarget; generatorToTarget)

-- Roadmap ingestion/export/enrichment targets factored out of ExporterMakefile.agda
roadmapExportTargets : List MakefileTarget
roadmapExportTargets =
  generatorToTarget "roadmap-merge" "Merge ingestion streams" ([]) ("python3 scripts/merge_roadmaps.py" ∷ [])
  ∷ generatorToTarget "build/canonical_enriched.json" "Enrich canonical roadmap" ("planning-index-json" ∷ "build/diagrams/agda-deps-full.dot" ∷ [])
      ("python3 scripts/enrich_canonical.py" ∷ [])
  ∷ generatorToTarget "roadmap-enrich" "Enrich roadmap with graph data" ("build/canonical_enriched.json" ∷ [])
      ("@echo \"roadmap enrichment complete\"" ∷ [])
  ∷ generatorToTarget "roadmap-export-json" "Export canonical roadmap to JSON" ("planning-index-json" ∷ [])
      ("python3 scripts/export_canonical_json.py" ∷ [])
  ∷ generatorToTarget "roadmap-export-md" "Export canonical roadmap to Markdown" ("planning-index-json" ∷ [])
      ("python3 scripts/export_canonical_md.py" ∷ [])
  ∷ generatorToTarget "roadmap-export-enriched" "Export enriched roadmap" ("build/canonical_enriched.json" ∷ [])
      ("python3 scripts/export_enriched_md.py" ∷ [])
  ∷ generatorToTarget "roadmap-export-deps" "Export roadmap dependency graph" ("build/canonical_enriched.json" ∷ [])
      ("python3 scripts/export_dependency_graph.py" ∷ [])
  ∷ generatorToTarget "roadmap-validate-json" "Validate canonical JSON" ("roadmap-export-json" ∷ [])
      ("python3 scripts/validate_json.py" ∷ [])
  ∷ generatorToTarget "roadmap-validate-md" "Validate canonical Markdown" ("roadmap-export-md" ∷ [])
      ("python3 scripts/validate_md.py" ∷ [])
  ∷ generatorToTarget "roadmap-validate-triangle" "Verify Triangle Identity (Agda <-> JSON <-> MD)" ("roadmap-validate-json" ∷ "roadmap-validate-md" ∷ [])
      ("@echo \"✓ Triangle validation complete\"" ∷ [])
  ∷ generatorToTarget "roadmap-sppf-export" "Export SPPF structure" ("planning-index-json" ∷ [])
      ("python3 scripts/export_roadmap_sppf.py" ∷ [])
  ∷ generatorToTarget "roadmap-all-enriched" "Build all enriched artifacts" ("roadmap-export-enriched" ∷ "roadmap-export-deps" ∷ [])
      ("@echo \"roadmap all enriched complete\"" ∷ [])
  ∷ []
