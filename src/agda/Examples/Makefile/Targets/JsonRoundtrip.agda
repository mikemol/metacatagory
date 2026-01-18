{-# OPTIONS --without-K #-}
-- | JSON decomposition and roundtrip validation targets.
module Examples.Makefile.Targets.JsonRoundtrip where

open import Agda.Builtin.List using (List; _∷_; [])
open import Examples.MakefileTargets using (MakefileTarget; generatorToTarget; generatorToFileTarget; mutateCert)

-- JSON decomposition/recomposition/roundtrip targets for canonical, enriched, planning

jsonRoundtripTargets : List MakefileTarget
jsonRoundtripTargets =
  generatorToTarget mutateCert "json-decompose" "Decompose monolithic JSON to hierarchical structure" ("data/dependency_graph.json" ∷ [])
    ("python3 scripts/json_decompose.py data/dependency_graph.json $(DEPS_DIR) --strategy dependency-graph" ∷ [])
  ∷ generatorToTarget mutateCert "json-decompose-prebuilt" "Decompose monolithic JSON using prebuilt inputs" ("json-decompose" ∷ [])
    []
  ∷ generatorToFileTarget mutateCert "build/dependency_graph_recomposed.json" "Recompose hierarchical JSON back to monolithic form" ("json-decompose" ∷ "$(DEPS_METADATA)" ∷ [])
    ("python3 scripts/json_recompose.py $(DEPS_DIR) build/dependency_graph_recomposed.json" ∷ [])
  ∷ generatorToTarget mutateCert "json-roundtrip-validate" "Validate JSON decomposition roundtrip" ("json-decompose" ∷ "build/dependency_graph_recomposed.json" ∷ [])
    ("python3 scripts/validate_json_roundtrip.py" ∷ [])
  ∷ generatorToTarget mutateCert "json-roundtrip-validate-light" "Validate JSON decomposition roundtrip (light)" ("json-decompose-prebuilt" ∷ "build/dependency_graph_recomposed.json" ∷ [])
    ("python3 scripts/validate_json_roundtrip.py" ∷ [])

  ∷ generatorToTarget mutateCert "json-decompose-enriched" "Decompose canonical_enriched.json into item hierarchy" ("build/canonical_enriched.json" ∷ [])
      ("python3 scripts/json_decompose.py build/canonical_enriched.json data/enriched/ --strategy item-array" ∷ [])
  ∷ generatorToFileTarget mutateCert "build/canonical_enriched_recomposed.json" "Recompose enriched items into canonical_enriched.json" ("data/enriched/" ∷ [])
      ("python3 scripts/json_recompose.py data/enriched/ build/canonical_enriched_recomposed.json" ∷ [])
  ∷ generatorToTarget mutateCert "json-roundtrip-validate-enriched" "Validate enriched roundtrip" ("json-decompose-enriched" ∷ "build/canonical_enriched_recomposed.json" ∷ [])
      ("python3 scripts/validate_json_roundtrip.py build/canonical_enriched.json build/canonical_enriched_recomposed.json" ∷ [])

  ∷ generatorToTarget mutateCert "json-decompose-planning" "Decompose planning_index.json into plan hierarchy" ("data/planning_index.json" ∷ [])
      ("python3 scripts/json_decompose.py data/planning_index.json $(PLANNING_DIR) --strategy item-array" ∷ [])
    ∷ generatorToFileTarget mutateCert "build/planning_index_recomposed.json" "Recompose planning items into planning_index.json" ("json-decompose-planning" ∷ "$(PLANNING_METADATA)" ∷ [])
      ("python3 scripts/json_recompose.py $(PLANNING_DIR) build/planning_index_recomposed.json" ∷ [])
  ∷ generatorToTarget mutateCert "json-roundtrip-validate-planning" "Validate planning roundtrip" ("json-decompose-planning" ∷ "build/planning_index_recomposed.json" ∷ [])
      ("python3 scripts/validate_json_roundtrip.py data/planning_index.json build/planning_index_recomposed.json" ∷ [])

  ∷ generatorToTarget mutateCert "json-roundtrip-validate-all" "Validate JSON roundtrip (deps + planning)" ("json-roundtrip-validate" ∷ "json-roundtrip-validate-planning" ∷ [])
      []
  ∷ generatorToTarget mutateCert "json-contract-validate" "Validate JSON transformation contract (Agda)" ("src/agda/Plan/CIM/JSONTransformationTesting.agdai" ∷ [])
      ("$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/JSONTransformationTesting.agda" ∷ [])
  ∷ generatorToTarget mutateCert "json-real-validate" "Validate real JSON data (Agda contract + Python validators)" ("json-roundtrip-validate-all" ∷ "json-contract-validate" ∷ [])
      []

  ∷ []
