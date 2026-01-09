{-# OPTIONS --without-K #-}
module Examples.Makefile.Targets.Priority where

open import Agda.Builtin.List using (List; _∷_; [])
open import Examples.MakefileTargets using (MakefileTarget; generatorToTarget; mutateCert)

-- Priority pipeline targets

priorityTargets : List MakefileTarget
priorityTargets =
  generatorToTarget mutateCert "badges" "Generate status badges" ("priority-badge-weights" ∷ []) 
    ("python3 scripts/generate-badges.py" ∷ [])
  ∷ generatorToTarget mutateCert "priority-strategy-profiles" "Compile and run Agda priority orchestration (generate strategy profiles)" ([]) 
    ("mkdir -p build" ∷ "$(AGDA) $(AGDA_FLAGS) --compile src/agda/TechnicalDebt/PriorityOrchestrationFFI.agda" ∷ "./src/agda/PriorityOrchestrationFFI" ∷ [])
  ∷ generatorToTarget mutateCert "priority-badge-weights" "Normalize Agda strategy profiles into badge weights" ("priority-strategy-profiles" ∷ []) 
    ("python3 scripts/adopt_priority_strategies.py --input data/priority_strategy_profiles.json --output .github/badges/weights.json" ∷ [])
  ∷ generatorToTarget mutateCert "priority-profile-json" "Export structured priority profile (lazy; derived from planning index)" ("planning-index-json" ∷ [])
    ("mkdir -p build" ∷ "$(AGDA) $(AGDA_FLAGS) --compile src/agda/Plan/CIM/PriorityProfileExport.agda && ./src/agda/PriorityProfileExport" ∷ [])
  ∷ generatorToTarget mutateCert "priority-refresh" "Re-run priority pipeline and refresh roadmap/badge outputs" ("planning-index-json" ∷ "roadmap-export-json" ∷ "priority-badge-weights" ∷ "badges" ∷ [])
    ("@echo \"priority pipeline refreshed (planning index, tasks, badge weights, roadmap badges)\"" ∷ [])
  ∷ []
