{-# OPTIONS --without-K #-}
-- | Composite Makefile targets that orchestrate higher-level workflows.
module Examples.Makefile.Targets.Composite where

open import Agda.Builtin.List using (List; _∷_; [])
open import Examples.MakefileTargets using (MakefileTarget; generatorToTarget; mutateCert)

-- Composite/witness targets tying families together

compositeTargets : List MakefileTarget
compositeTargets =
  generatorToTarget mutateCert "regen-agda" "Regenerate Agda build outputs" ("agda-all" ∷ [])
    ("@echo \"regen-agda complete\"" ∷ [])
  ∷ generatorToTarget mutateCert "regen-exports" "Regenerate Agda export artifacts" (
        "data/planning_index.json" ∷ "build/diagrams/agda-deps-full.dot" ∷ "data/dependency_graph.json" ∷ "build/priority_profile.json" ∷ [])
    ("@echo \"regen-exports complete\"" ∷ [])
  ∷ generatorToTarget mutateCert "regen-roadmap" "Regenerate roadmap artifacts" (
        ".github/roadmap/tasks.json" ∷ "ROADMAP.md" ∷ "build/reports/tasks_enriched.md" ∷ "roadmap-export-deps" ∷
        "build/gp_roadmap_sppf.json" ∷ "roadmap-all-enriched" ∷ [])
    ("@echo \"regen-roadmap complete\"" ∷ [])
  ∷ generatorToTarget mutateCert "regen-docs" "Regenerate documentation exports" ("docs-all" ∷ [])
    ("@echo \"regen-docs complete\"" ∷ [])
  ∷ generatorToTarget mutateCert "regen-badges" "Regenerate badge artifacts" (".github/badges/weights.json" ∷ "badges" ∷ [])
    ("@echo \"regen-badges complete\"" ∷ [])
  ∷ generatorToTarget mutateCert "regen-intake" "Regenerate intake-derived artifacts" ("build/canonical_roadmap.json" ∷ "roadmap-merge" ∷ [])
    ("@echo \"regen-intake complete\"" ∷ [])
  ∷ generatorToTarget mutateCert "regen-all" "Regenerate all build artifacts" (
        "regen-agda" ∷ "regen-exports" ∷ "regen-roadmap" ∷ "regen-docs" ∷ "regen-badges" ∷ "regen-intake" ∷ [])
    ("@echo \"regen-all complete\"" ∷ [])
  ∷ generatorToTarget mutateCert "all" "Build all code and documentation" ("agda-all" ∷ "docs-all" ∷ [])
    ("@echo \"all complete\"" ∷ [])
  ∷ generatorToTarget mutateCert "debt-check" "Run debt tracking validation" ("deferred-items" ∷ "intake-scan" ∷ [])
    ("@echo \"✓ Debt tracking tools validated\"" ∷ [])
  ∷ generatorToTarget mutateCert "check-infra" "Validate graph + Makefile" ("graph-assert-ok" ∷ "makefile-validate" ∷ [])
    ("@echo \"check-infra complete\"" ∷ [])
  ∷ generatorToTarget mutateCert "check-docs" "Validate documentation outputs" ("md-lint" ∷ "docs-lint" ∷ "docs-validate" ∷ [])
    ("@echo \"check-docs complete\"" ∷ [])
  ∷ generatorToTarget mutateCert "check-roadmap" "Validate roadmap exports" ("roadmap-validate-triangle" ∷ [])
    ("@echo \"check-roadmap complete\"" ∷ [])
  ∷ generatorToTarget mutateCert "check-python" "Validate Python test suite" ("python-verified" ∷ [])
    ("@echo \"check-python complete\"" ∷ [])
  ∷ generatorToTarget mutateCert "check-json" "Validate JSON roundtrip outputs" (
        "json-roundtrip-validate" ∷ "json-roundtrip-validate-enriched" ∷ "json-roundtrip-validate-planning" ∷ [])
    ("@echo \"check-json complete\"" ∷ [])
  ∷ generatorToTarget mutateCert "check-debt" "Validate deferred items + intake scan" ("debt-check" ∷ [])
    ("@echo \"check-debt complete\"" ∷ [])
  ∷ generatorToTarget mutateCert "check-all" "Run full validation suite" (
        "check-infra" ∷ "check-docs" ∷ "check-roadmap" ∷ "check-python" ∷ "check-json" ∷ "check-debt" ∷ [])
    ("@echo \"check-all complete\"" ∷ [])
  ∷ generatorToTarget mutateCert "check" "Run all validation checks (alias)" ("check-all" ∷ [])
    ("@echo \"check complete\"" ∷ [])
  ∷ generatorToTarget mutateCert "validate-constructive" "Regenerate and validate all artifacts" ("regen-all" ∷ "check-all" ∷ [])
    ("@echo \"✓ Constructive validation complete\"" ∷ [])
  ∷ generatorToTarget mutateCert "ci-light" "Lightweight CI target (no GHC backend)" (
        "check-infra" ∷ "check-docs" ∷ "json-roundtrip-validate-light" ∷ "json-roundtrip-validate-enriched" ∷ "json-roundtrip-validate-planning" ∷ [])
    ("@echo \"ci-light complete\"" ∷ [])
  ∷ generatorToTarget mutateCert "ci-preflight" "Fast guard: graph + makefile docs" ("check-infra" ∷ [])
    ("@echo \"ci-preflight complete\"" ∷ [])
  ∷ []
