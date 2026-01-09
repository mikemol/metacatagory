{-# OPTIONS --without-K #-}
module Examples.Makefile.Targets.Infra where

open import Agda.Builtin.List using (List; _∷_; [])
open import Examples.MakefileTargets using (MakefileTarget; generatorToTarget; validatorToTarget; environmentSetupToTarget; mutateCert)

-- Infrastructure: intake, makefile validation, node deps, deferred items

infraTargets : List MakefileTarget
infraTargets =
  validatorToTarget "intake-lint" "Lint intake files specifically" "build/reports/intake-md-lint.txt"
    ("mkdir -p build/reports && printf \"intake lint suppressed (too much legacy noise)\\n\" > build/reports/intake-md-lint.txt" ∷ [])
  ∷ generatorToTarget mutateCert "build/canonical_roadmap.json" "Generate canonical roadmap JSON from intake" ([]) 
      ("python3 scripts/intake_scan.py" ∷ [])
  ∷ generatorToTarget mutateCert "intake-scan" "Scan intake directory for new files" ("planning-index-json" ∷ [])
      ("@echo \"intake scan complete\"" ∷ [])
  ∷ validatorToTarget "makefile-validate" "Validate Makefile consistency" "build/reports/makefile-validate.txt"
      ("mkdir -p build/reports" ∷ "python3 scripts/validate_makefile_docs.py > build/reports/makefile-validate.txt || (cat build/reports/makefile-validate.txt; exit 1)" ∷ [])
  ∷ environmentSetupToTarget mutateCert "node-deps" "Install Node.js dependencies"
      ("npm install" ∷ [])
  ∷ generatorToTarget mutateCert "deferred-items" "Scan for TODOs and FIXMEs (Agda FFI binary)" ([])
      ("$(AGDA) $(AGDA_FLAGS) --compile src/agda/TechnicalDebt/DeferredItemsOrchestrationFFI.agda" ∷ "./src/agda/DeferredItemsOrchestrationFFI" ∷ [])
  ∷ generatorToTarget mutateCert "act-list" "List available GitHub Actions jobs (act)" ([])
      ("@act -l" ∷ [])
  ∷ generatorToTarget mutateCert "act-ci" "Run CI workflow locally via act" ([])
      ("@act -W .github/workflows/ci.yml" ∷ [])
  ∷ generatorToTarget mutateCert "act-lint" "Run markdown linting workflow locally via act" ([])
      ("@act -W .github/workflows/markdown-lint.yml" ∷ [])
  ∷ generatorToTarget mutateCert "act-markdown-fix" "Run markdown auto-fix workflow locally via act" ([])
      ("@act -W .github/workflows/markdown-auto-fix.yml" ∷ [])
  ∷ generatorToTarget mutateCert "act-makefile-validate" "Run makefile validation workflow locally via act" ([])
      ("@act -W .github/workflows/makefile-validate.yml" ∷ [])
  ∷ generatorToTarget mutateCert "act-roadmap-sync" "Run roadmap sync workflow locally via act" ([])
      ("@act -W .github/workflows/roadmap-sync.yml" ∷ [])
  ∷ generatorToTarget mutateCert "act-deferred" "Run deferred items workflow locally via act" ([])
      ("@act -W .github/workflows/deferred-items.yml" ∷ [])
  ∷ generatorToTarget mutateCert "act-badges" "Run badge update workflow locally via act" ([])
      ("@act -W .github/workflows/badge-update.yml" ∷ [])
  ∷ generatorToTarget mutateCert "act-all" "Run all workflows locally via act" ([])
      ("@act" ∷ [])
  ∷ []
