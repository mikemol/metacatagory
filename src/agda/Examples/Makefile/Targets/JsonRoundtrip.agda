{-# OPTIONS --without-K #-}
module Examples.Makefile.Targets.JsonRoundtrip where

open import Agda.Builtin.List using (List; _∷_; [])
open import Examples.MakefileTargets using (MakefileTarget; generatorToTarget; mutateCert)

-- JSON decomposition/recomposition/roundtrip targets for canonical, enriched, planning

jsonRoundtripTargets : List MakefileTarget
jsonRoundtripTargets =
  generatorToTarget mutateCert "json-decompose" "Decompose monolithic JSON to hierarchical structure" ("data/dependency_graph.json" ∷ [])
    ("python3 scripts/json_decompose.py data/dependency_graph.json $(DEPS_DIR) --strategy dependency-graph" ∷ [])
  ∷ generatorToTarget mutateCert "json-decompose-prebuilt" "Decompose monolithic JSON using prebuilt inputs" ("data/dependency_graph.json" ∷ [])
    ("python3 scripts/json_decompose.py data/dependency_graph.json $(DEPS_DIR) --strategy dependency-graph" ∷ [])
  ∷ generatorToTarget mutateCert "json-recompose" "Recompose hierarchical JSON back to monolithic form" ("$(DEPS_DIR)" ∷ [])
    ("python3 scripts/json_recompose.py $(DEPS_DIR) build/dependency_graph_recomposed.json" ∷ [])
  ∷ generatorToTarget mutateCert "json-recompose-light" "Recompose hierarchical JSON (prebuilt, fallback-safe)" ("json-decompose-prebuilt" ∷ [])
    ("python3 scripts/json_recompose.py $(DEPS_DIR) build/dependency_graph_recomposed.json" ∷ [])
  ∷ generatorToTarget mutateCert "json-roundtrip-validate" "Validate JSON decomposition roundtrip" ("json-decompose" ∷ "json-recompose" ∷ [])
    ("python3 scripts/validate_json_roundtrip.py" ∷ [])
  ∷ generatorToTarget mutateCert "json-roundtrip-validate-light" "Validate JSON decomposition roundtrip (light)" ("json-recompose-light" ∷ [])
    ("python3 scripts/validate_json_roundtrip.py" ∷ [])

  ∷ generatorToTarget mutateCert "json-decompose-enriched" "Decompose canonical_enriched.json into item hierarchy" ("build/canonical_enriched.json" ∷ [])
      ("python3 scripts/json_decompose.py build/canonical_enriched.json data/enriched/ --strategy item-array" ∷ [])
  ∷ generatorToTarget mutateCert "json-recompose-enriched" "Recompose enriched items into canonical_enriched.json" ("data/enriched/" ∷ [])
      ("python3 scripts/json_recompose.py data/enriched/ build/canonical_enriched_recomposed.json" ∷ [])
  ∷ generatorToTarget mutateCert "json-roundtrip-validate-enriched" "Validate enriched roundtrip" ("json-decompose-enriched" ∷ "json-recompose-enriched" ∷ [])
      ("python3 scripts/validate_json_roundtrip.py build/canonical_enriched.json build/canonical_enriched_recomposed.json" ∷ [])

  ∷ generatorToTarget mutateCert "json-decompose-planning" "Decompose planning_index.json into plan hierarchy" ("data/planning_index.json" ∷ [])
      ("python3 scripts/json_decompose.py data/planning_index.json $(PLANNING_DIR) --strategy item-array" ∷ [])
    ∷ generatorToTarget mutateCert "json-recompose-planning" "Recompose planning items into planning_index.json" ("$(PLANNING_DIR)" ∷ [])
      ("python3 scripts/json_recompose.py $(PLANNING_DIR) build/planning_index_recomposed.json" ∷ [])
  ∷ generatorToTarget mutateCert "json-roundtrip-validate-planning" "Validate planning roundtrip" ("json-decompose-planning" ∷ "json-recompose-planning" ∷ [])
      ("python3 scripts/validate_json_roundtrip.py data/planning_index.json build/planning_index_recomposed.json" ∷ [])

  ∷ []
