{-# OPTIONS --without-K #-}
module Examples.Makefile.Targets.AgdaBuild where

open import Agda.Builtin.List using (List; _∷_; [])
open import Examples.MakefileTargets using (MakefileTarget; generatorToTarget)

-- Agda build/export targets

agdaTargets : List MakefileTarget
agdaTargets =
  generatorToTarget "roadmap-index" "Compile Roadmap Index" ("src/agda/Plan/CIM/RoadmapIndex.agdai" ∷ [])
    ("$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/RoadmapIndex.agda" ∷ [])
  ∷ generatorToTarget "planning-index-json" "Export planning index to JSON" ("src/agda/Plan/CIM/PlanningExport.agdai" ∷ [])
    ("if [ -n \"${SKIP_GHC_BACKEND:-}\" ]; then echo \"skip planning-index-json (SKIP_GHC_BACKEND)\"; else mkdir -p build && $(AGDA) $(AGDA_FLAGS) --compile src/agda/Plan/CIM/PlanningExport.agda && ./src/agda/PlanningExport; fi" ∷ [])
  ∷ generatorToTarget "planning-kernel" "Compile Planning Kernel" ("src/agda/Plan/CIM/PlanningKernel.agdai" ∷ [])
    ("$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/PlanningKernel.agda" ∷ [])
  ∷ generatorToTarget "roadmap-sync" "Sync roadmap with external tracker" ("roadmap-export-json" ∷ "src/agda/Plan/CIM/RoadmapSync.agdai" ∷ [])
    ("$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/RoadmapSync.agda" ∷ [])
  ∷ generatorToTarget "roadmap-sppf" "Compile Roadmap SPPF" ("src/agda/Plan/CIM/RoadmapSPPF.agdai" ∷ [])
    ("$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/RoadmapSPPF.agda" ∷ [])
  ∷ generatorToTarget "build/diagrams/agda-deps-full.dot" "Generate dependency graph" ([])
    ("if [ -n \"${SKIP_GHC_BACKEND:-}\" ]; then mkdir -p build/diagrams && echo \"digraph G {}\" > build/diagrams/agda-deps-full.dot; else mkdir -p build/diagrams && $(AGDA) --dependency-graph=build/diagrams/agda-deps-full.dot $(AGDA_FLAGS) src/agda/Tests/Index.agda 2>&1 | (grep -E \"(Checking|Error)\" || true) | head -20; fi" ∷ [])
  ∷ generatorToTarget "roadmap-deps-graph" "Generate dependency graph" ("build/diagrams/agda-deps-full.dot" ∷ [])
    ("@echo \"agda dependency graph generated\"" ∷ [])
  ∷ generatorToTarget "dependency-graph-json" "Export dependency graph JSON via Agda (from agda-deps-full.dot)" ("build/diagrams/agda-deps-full.dot" ∷ [])
    ("if [ -n \"${SKIP_GHC_BACKEND:-}\" ]; then echo \"skip dependency-graph-json (SKIP_GHC_BACKEND)\"; else mkdir -p build && $(AGDA) $(AGDA_FLAGS) --compile src/agda/Plan/CIM/DependencyGraphExport.agda && ./src/agda/DependencyGraphExport; fi" ∷ [])
  ∷ []
