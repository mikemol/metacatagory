{-# OPTIONS --without-K #-}
module Examples.Makefile.Targets.Composite where

open import Agda.Builtin.List using (List; _∷_; [])
open import Examples.MakefileTargets using (MakefileTarget; generatorToTarget)

-- Composite/witness targets tying families together

compositeTargets : List MakefileTarget
compositeTargets =
  generatorToTarget "all" "Build all code and documentation" ("agda-all" ∷ "docs-all" ∷ [])
    ("@echo \"all complete\"" ∷ [])
  ∷ generatorToTarget "debt-check" "Run debt tracking validation" ("deferred-items" ∷ "intake-scan" ∷ [])
    ("@echo \"✓ Debt tracking tools validated\"" ∷ [])
  ∷ generatorToTarget "validate-constructive" "Run all constructive build targets" (
        "docs-all" ∷ "docs-generate" ∷ "docs-modules" ∷
        "roadmap-export-json" ∷ "roadmap-export-md" ∷ "roadmap-export-enriched" ∷ "roadmap-export-deps" ∷
        "roadmap-deps-graph" ∷ "roadmap-enrich" ∷ "roadmap-all-enriched" ∷
        "intake-scan" ∷ "md-normalize" ∷ "badges" ∷ [])
    ("@echo \"✓ Constructive validation complete\"" ∷ [])
  ∷ generatorToTarget "check" "Run all validation checks" ("graph-assert-ok" ∷ "makefile-validate" ∷ "md-lint" ∷ "roadmap-validate-triangle" ∷ "docs-validate" ∷ "python-verified" ∷ "debt-check" ∷ "json-roundtrip-validate" ∷ "json-roundtrip-validate-enriched" ∷ "json-roundtrip-validate-planning" ∷ "all" ∷ [])
    ("@echo \"check complete\"" ∷ [])
  ∷ generatorToTarget "ci-light" "Lightweight CI target (no GHC backend)" ("graph-assert-ok" ∷ "makefile-validate" ∷ "md-lint" ∷ "docs-validate" ∷ "json-roundtrip-validate-light" ∷ "json-roundtrip-validate-enriched" ∷ "json-roundtrip-validate-planning" ∷ [])
    ("@echo \"ci-light complete\"" ∷ [])
  ∷ generatorToTarget "ci-preflight" "Fast guard: graph + makefile docs" ("graph-assert-ok" ∷ "makefile-validate" ∷ [])
    ("@echo \"ci-preflight complete\"" ∷ [])
  ∷ []
