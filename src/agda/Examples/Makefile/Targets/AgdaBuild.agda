{-# OPTIONS --without-K #-}
-- | Makefile targets for Agda compilation and exports.
module Examples.Makefile.Targets.AgdaBuild where

open import Agda.Builtin.List using (List; _∷_; [])
open import Examples.MakefileTargets using (MakefileTarget; generatorToTarget; generatorToFileTarget; mutateCert)

-- Agda build/export targets

agdaTargets : List MakefileTarget
agdaTargets =
  generatorToTarget mutateCert "roadmap-index" "Compile Roadmap Index" ("src/agda/Plan/CIM/RoadmapIndex.agdai" ∷ [])
    ("$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/RoadmapIndex.agda" ∷ [])
  ∷ generatorToFileTarget mutateCert "build/agda/Plan/CIM/PlanningExport" "Compile PlanningExport (MAlonzo + binary)" ("src/agda/Plan/CIM/PlanningExport.agda" ∷ [])
    ("if [ -n \"$${SKIP_GHC_BACKEND}\" ]; then echo \"skip planning-export-compile (SKIP_GHC_BACKEND)\"; else $(AGDA_COMPILE) src/agda/Plan/CIM/PlanningExport.agda; fi" ∷ [])
  ∷ generatorToFileTarget mutateCert "data/planning_index.json" "Export planning index to JSON" ("build/agda/Plan/CIM/PlanningExport" ∷ [])
    ("if [ -n \"$${SKIP_GHC_BACKEND}\" ]; then echo \"skip data/planning_index.json (SKIP_GHC_BACKEND)\"; else $(AGDA_COMPILE_DIR)/PlanningExport; fi" ∷ [])
  ∷ generatorToTarget mutateCert "planning-kernel" "Compile Planning Kernel" ("src/agda/Plan/CIM/PlanningKernel.agdai" ∷ [])
    ("$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/PlanningKernel.agda" ∷ [])
  ∷ generatorToTarget mutateCert "roadmap-sync" "Sync roadmap with external tracker" (".github/roadmap/tasks.json" ∷ "src/agda/Plan/CIM/RoadmapSync.agdai" ∷ [])
    ("$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/RoadmapSync.agda" ∷ [])
  ∷ generatorToTarget mutateCert "roadmap-sppf" "Compile Roadmap SPPF" ("src/agda/Plan/CIM/RoadmapSPPF.agdai" ∷ [])
    ("$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/RoadmapSPPF.agda" ∷ [])
  ∷ generatorToFileTarget mutateCert "build/diagrams/dir.stamp" "Ensure build/diagrams exists" ("build/dir.stamp" ∷ [])
    ("mkdir -p build/diagrams && touch build/diagrams/dir.stamp" ∷ [])
  ∷ generatorToFileTarget mutateCert "build/diagrams/agda-deps-full.dot" "Generate dependency graph" ("build/diagrams/dir.stamp" ∷ [])
    ("if [ -n \"$${SKIP_GHC_BACKEND}\" ]; then echo \"digraph G {}\" > build/diagrams/agda-deps-full.dot; else $(AGDA) --dependency-graph=build/diagrams/agda-deps-full.dot $(AGDA_FLAGS) src/agda/Tests/Index.agda 2>&1 | (grep -E \"(Checking|Error)\" || true) | awk 'NR<=20{print}'; fi" ∷ [])
  ∷ generatorToTarget mutateCert "roadmap-deps-graph" "Generate dependency graph" ("build/diagrams/agda-deps-full.dot" ∷ [])
    ("@echo \"agda dependency graph generated\"" ∷ [])
  ∷ generatorToFileTarget mutateCert "build/agda/Plan/CIM/DependencyGraphExport" "Compile DependencyGraphExport (MAlonzo + binary)" ("src/agda/Plan/CIM/DependencyGraphExport.agda" ∷ [])
    ("if [ -n \"$${SKIP_GHC_BACKEND}\" ]; then echo \"skip dependency-graph-export-compile (SKIP_GHC_BACKEND)\"; else $(AGDA_COMPILE) src/agda/Plan/CIM/DependencyGraphExport.agda; fi" ∷ [])
  ∷ generatorToFileTarget mutateCert "data/dependency_graph.json" "Export dependency graph JSON via Agda (from agda-deps-full.dot)" ("build/diagrams/agda-deps-full.dot" ∷ "build/agda/Plan/CIM/DependencyGraphExport" ∷ [])
    ("if [ -n \"$${SKIP_GHC_BACKEND}\" ]; then echo \"skip data/dependency_graph.json (SKIP_GHC_BACKEND)\"; else $(AGDA_COMPILE_DIR)/DependencyGraphExport; fi" ∷ [])
  ∷ []
