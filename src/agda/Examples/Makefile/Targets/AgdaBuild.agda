{-# OPTIONS --without-K #-}
module Examples.Makefile.Targets.AgdaBuild where

open import Agda.Builtin.List using (List; _∷_; [])
open import Examples.MakefileTargets using (MakefileTarget; generatorToTarget; mutateCert)

-- Agda build/export targets

agdaTargets : List MakefileTarget
agdaTargets =
  generatorToTarget mutateCert "roadmap-index" "Compile Roadmap Index" ("src/agda/Plan/CIM/RoadmapIndex.agdai" ∷ [])
    ("$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/RoadmapIndex.agda" ∷ [])
  ∷ generatorToTarget mutateCert "planning-index-json" "Export planning index to JSON" ("src/agda/Plan/CIM/PlanningExport.agdai" ∷ [])
    ("if [ -n \"$${SKIP_GHC_BACKEND}\" ]; then echo \"skip planning-index-json (SKIP_GHC_BACKEND)\"; else mkdir -p build && $(AGDA) $(AGDA_FLAGS) --compile src/agda/Plan/CIM/PlanningExport.agda && ./src/agda/PlanningExport; fi" ∷ [])
  ∷ generatorToTarget mutateCert "planning-kernel" "Compile Planning Kernel" ("src/agda/Plan/CIM/PlanningKernel.agdai" ∷ [])
    ("$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/PlanningKernel.agda" ∷ [])
  ∷ generatorToTarget mutateCert "roadmap-sync" "Sync roadmap with external tracker" ("roadmap-export-json" ∷ "src/agda/Plan/CIM/RoadmapSync.agdai" ∷ [])
    ("$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/RoadmapSync.agda" ∷ [])
  ∷ generatorToTarget mutateCert "roadmap-sppf" "Compile Roadmap SPPF" ("src/agda/Plan/CIM/RoadmapSPPF.agdai" ∷ [])
    ("$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/RoadmapSPPF.agda" ∷ [])
  ∷ generatorToTarget mutateCert "build/diagrams/agda-deps-full.dot" "Generate dependency graph" ([])
    ("if [ -n \"$${SKIP_GHC_BACKEND}\" ]; then mkdir -p build/diagrams && echo \"digraph G {}\" > build/diagrams/agda-deps-full.dot; else mkdir -p build/diagrams && $(AGDA) --dependency-graph=build/diagrams/agda-deps-full.dot $(AGDA_FLAGS) src/agda/Tests/Index.agda 2>&1 | (grep -E \"(Checking|Error)\" || true) | awk 'NR<=20{print}'; fi" ∷ [])
  ∷ generatorToTarget mutateCert "roadmap-deps-graph" "Generate dependency graph" ("build/diagrams/agda-deps-full.dot" ∷ [])
    ("@echo \"agda dependency graph generated\"" ∷ [])
  ∷ generatorToTarget mutateCert "dependency-graph-json" "Export dependency graph JSON via Agda (from agda-deps-full.dot)" ("build/diagrams/agda-deps-full.dot" ∷ [])
    ("if [ -n \"$${SKIP_GHC_BACKEND}\" ]; then echo \"skip dependency-graph-json (SKIP_GHC_BACKEND)\"; else mkdir -p build && $(AGDA) $(AGDA_FLAGS) --compile src/agda/Plan/CIM/DependencyGraphExport.agda && ./src/agda/DependencyGraphExport; fi" ∷ [])
  ∷ []
