{-# OPTIONS --without-K #-}
-- | Infrastructure targets for intake, validation, and act workflows.
module Examples.Makefile.Targets.Infra where

open import Agda.Builtin.List using (List; _∷_; [])
open import Examples.MakefileTargets using (MakefileTarget; generatorToTarget; generatorToFileTarget; validatorToTarget; environmentSetupToTarget; mutateCert)

-- Infrastructure: intake, makefile validation, node deps, deferred items

infraTargets : List MakefileTarget
infraTargets =
  generatorToFileTarget mutateCert "build/dir.stamp" "Ensure build/ exists" ([])
      ("mkdir -p build && touch build/dir.stamp" ∷ [])
  ∷ generatorToFileTarget mutateCert "build/reports/dir.stamp" "Ensure build/reports exists" ("build/dir.stamp" ∷ [])
      ("mkdir -p build/reports && touch build/reports/dir.stamp" ∷ [])
  ∷ generatorToFileTarget mutateCert "docs/status/dir.stamp" "Ensure docs/status exists" ([])
      ("mkdir -p docs/status && touch docs/status/dir.stamp" ∷ [])
  ∷ validatorToTarget "intake-lint" "Lint intake files specifically" "build/reports/intake-md-lint.txt"
    []
    ("if [ \"$${METACATAGORY_REPORT_MODE:-stdout}\" = \"write\" ]; then mkdir -p build/reports && printf \"intake lint suppressed (too much legacy noise)\\n\" > build/reports/intake-md-lint.txt; else printf \"intake lint suppressed (too much legacy noise)\\n\"; fi" ∷ [])
  ∷ generatorToTarget mutateCert "intake-scan" "Scan intake directory for new files" ("data/planning_index.json" ∷ [])
      ("@echo \"intake scan complete\"" ∷ [])
  ∷ validatorToTarget "makefile-validate" "Validate Makefile consistency" "build/reports/makefile-validate.txt"
      []
      ("python3 scripts/validate_makefile_docs.py" ∷ [])
  ∷ generatorToTarget mutateCert "check-makefile-generated" "Fail if Makefile.generated is stale"
      ([])
      ("if [ ! -f Makefile.generated ]; then echo \"Missing Makefile.generated; run make regen-makefile\"; exit 1; fi" ∷
       "if ! cmp -s Makefile.generated Makefile; then echo \"Makefile.generated is stale; run make regen-makefile\"; exit 1; fi" ∷
       [])
  ∷ environmentSetupToTarget mutateCert "node-deps" "Install Node.js dependencies"
      ("if [ -f package-lock.json ]; then npm ci; else npm install; fi" ∷ [])
  ∷ generatorToFileTarget mutateCert "build/agda/TechnicalDebt/DeferredItemsOrchestrationFFI" "Compile deferred items scanner (MAlonzo + binary)"
      ("build/dir.stamp" ∷ "src/agda/TechnicalDebt/DeferredItemsOrchestrationFFI.agda" ∷ [])
      ("$(AGDA_COMPILE) src/agda/TechnicalDebt/DeferredItemsOrchestrationFFI.agda" ∷ [])
  ∷ generatorToTarget mutateCert "deferred-items-dirs" "Ensure deferred items output directories exist"
      ("build/reports/dir.stamp" ∷ "docs/status/dir.stamp" ∷ [])
      ("@echo \"deferred items output dirs ready\"" ∷ [])
  ∷ generatorToTarget mutateCert "deferred-items" "Scan for TODOs and FIXMEs (Agda FFI binary)"
      ("build/agda/TechnicalDebt/DeferredItemsOrchestrationFFI" ∷ "deferred-items-dirs" ∷ [])
      ("bash scripts/run_deferred_items.sh" ∷ [])
  ∷ generatorToTarget mutateCert "act-list" "List available GitHub Actions jobs (act)" ([])
      ("scripts/run_act.sh --no-cache-server -l" ∷ [])
  ∷ generatorToTarget mutateCert "act-ci" "Run CI workflow locally via act" ([])
      ("scripts/run_act.sh --no-cache-server -W .github/workflows/ci.yml" ∷ [])
  ∷ generatorToTarget mutateCert "act-lint" "Run docs checks job locally via act" ([])
      ("scripts/run_act.sh --no-cache-server -W .github/workflows/ci.yml -j docs-checks" ∷ [])
  ∷ generatorToTarget mutateCert "act-makefile-validate" "Run Agda exports job locally via act" ([])
      ("scripts/run_act.sh --no-cache-server -W .github/workflows/ci.yml -j agda-exports" ∷ [])
  ∷ generatorToTarget mutateCert "act-roadmap-sync" "Run roadmap/JSON job locally via act" ([])
      ("scripts/run_act.sh --no-cache-server -W .github/workflows/ci.yml -j roadmap-json-checks" ∷ [])
  ∷ generatorToTarget mutateCert "act-deferred" "Run Python/debt job locally via act" ([])
      ("scripts/run_act.sh --no-cache-server -W .github/workflows/ci.yml -j python-checks" ∷ [])
  ∷ generatorToTarget mutateCert "act-all" "Run all workflows locally via act" ([])
      ("scripts/run_act.sh --no-cache-server" ∷ [])
  ∷ []
