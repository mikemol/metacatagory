{-# OPTIONS --without-K #-}
-- | Priority pipeline targets for badges and profiles.
module Examples.Makefile.Targets.Priority where

open import Agda.Builtin.List using (List; _∷_; [])
open import Examples.MakefileTargets using (MakefileTarget; generatorToTarget; generatorToFileTarget; mutateCert)

-- Priority pipeline targets

priorityTargets : List MakefileTarget
priorityTargets =
  generatorToTarget mutateCert "badges" "Generate status badges" (".github/badges/weights.json" ∷ []) 
    ("python3 scripts/generate-badges.py" ∷ [])
  ∷ generatorToFileTarget mutateCert "build/agda/PriorityOrchestrationFFI" "Compile Agda priority orchestration (MAlonzo + binary)"
    ("build/dir.stamp" ∷ "src/agda/TechnicalDebt/PriorityOrchestrationFFI.agda" ∷ [])
    ("$(AGDA_COMPILE) src/agda/TechnicalDebt/PriorityOrchestrationFFI.agda" ∷ [])
  ∷ generatorToTarget mutateCert "priority-strategy-profiles" "Compile and run Agda priority orchestration (generate strategy profiles)" ("build/agda/PriorityOrchestrationFFI" ∷ [])
    ("$(AGDA_COMPILE_DIR)/PriorityOrchestrationFFI" ∷ [])
  ∷ generatorToFileTarget mutateCert ".github/badges/weights.json" "Normalize Agda strategy profiles into badge weights" ("priority-strategy-profiles" ∷ []) 
    ("python3 scripts/adopt_priority_strategies.py --input data/priority_strategy_profiles.json --output .github/badges/weights.json" ∷ [])
  ∷ generatorToFileTarget mutateCert "build/priority_profile_inputs.stamp" "Track priority profile inputs"
    ("data/planning_index.json" ∷ "build/reports/docs-lint.json" ∷ [])
    ("mkdir -p build && touch build/priority_profile_inputs.stamp" ∷ [])
  ∷ generatorToFileTarget mutateCert "build/agda/Plan/CIM/PriorityProfileExport" "Compile PriorityProfileExport (MAlonzo + binary)"
    ("build/dir.stamp" ∷ "src/agda/Plan/CIM/PriorityProfileExport.agda" ∷ [])
    ("$(AGDA_COMPILE) src/agda/Plan/CIM/PriorityProfileExport.agda" ∷ [])
  ∷ generatorToFileTarget mutateCert "build/priority_profile.json" "Export structured priority profile (lazy; derived from planning index)"
    ("build/priority_profile_inputs.stamp" ∷ "build/agda/Plan/CIM/PriorityProfileExport" ∷ [])
    ("$(AGDA_COMPILE_DIR)/PriorityProfileExport" ∷ [])
  ∷ generatorToTarget mutateCert "priority-refresh" "Re-run priority pipeline and refresh roadmap/badge outputs" ("data/planning_index.json" ∷ ".github/roadmap/tasks.json" ∷ ".github/badges/weights.json" ∷ "badges" ∷ [])
    ("@echo \"priority pipeline refreshed (planning index, tasks, badge weights, roadmap badges)\"" ∷ [])
  ∷ []
