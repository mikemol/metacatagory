{-# OPTIONS --without-K #-}

-- | JSONTransformation: Natural transformation between monolithic and
-- hierarchical JSON representations.
--
-- This module formalizes the decomposition of large monolithic JSON artifacts
-- into hierarchical directory structures with index manifests. The transformation
-- is proven to be lossless via isomorphism properties.
--
-- Note: Uses postulates for unimplemented functions (to be implemented in Python).
-- The proof obligations serve as formal specifications.
--
-- Roadmap: BUILD-JSON-SCHEMA
-- Documentation: docs/process/JSON-DECOMPOSITION.md

module Plan.CIM.JSONTransformation where

open import Agda.Builtin.String using (String)
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.Maybe using (Maybe; just; nothing)
open import Agda.Builtin.Nat using (Nat)
open import Agda.Builtin.Equality using (_≡_; refl)

------------------------------------------------------------------------
-- Core JSON representation
------------------------------------------------------------------------

-- | Abstract JSON value type (we don't need to model full JSON structure,
-- just treat it as opaque data with equality)
postulate
  JSON : Set
  _≈ʲ_ : JSON → JSON → Bool  -- JSON equality (structural comparison)
  
-- | Filepath in hierarchical structure
Filepath : Set
Filepath = String

------------------------------------------------------------------------
-- Monolithic representation
------------------------------------------------------------------------

-- | Monolithic JSON: entire structure in a single file
record Monolithic : Set where
  constructor mkMonolithic
  field
    content : JSON

------------------------------------------------------------------------
-- Hierarchical representation
------------------------------------------------------------------------

-- | Manifest specification: rules for reconstructing monolithic from fragments
record ManifestSpec : Set where
  constructor mkManifest
  field
    -- Root metadata file path
    metadataPath : Filepath
    
    -- Index files for navigation (e.g., _index.json, _manifest.json)
    indexPaths : List Filepath
    
    -- Aggregation rule: how to merge fragments back into monolithic
    -- (encoded as filepath pattern, will be interpreted by recomposer)
    aggregationRule : String

-- | Fragment: single piece of the hierarchical decomposition
record Fragment : Set where
  constructor mkFragment
  field
    path : Filepath
    content : JSON

-- | Hierarchical JSON: structure decomposed into multiple files
record Hierarchical : Set where
  constructor mkHierarchical
  field
    -- Metadata file (e.g., build/deps/_metadata.json)
    metadata : JSON
    
    -- All fragments (data files + index files)
    fragments : List Fragment
    
    -- Reconstruction rules
    manifest : ManifestSpec

------------------------------------------------------------------------
-- Natural transformation
------------------------------------------------------------------------

-- | Forward transformation: monolithic → hierarchical
-- Decompose large JSON into fragments with indices
postulate
  forward : Monolithic → Hierarchical

-- | Backward transformation: hierarchical → monolithic
-- Reconstruct original JSON from fragments using manifest rules
postulate
  backward : Hierarchical → Monolithic

------------------------------------------------------------------------
-- Isomorphism properties (proof obligations)
------------------------------------------------------------------------

-- | Roundtrip law: backward ∘ forward = id
-- Ensures decomposition is lossless
postulate
  roundtrip-forward-backward : ∀ (m : Monolithic) →
    let h = forward m
        m′ = backward h
    in Monolithic.content m ≈ʲ Monolithic.content m′ ≡ true

-- | Roundtrip law (inverse): forward ∘ backward ≈ id
-- Ensures recomposition doesn't introduce spurious structure
-- (Note: not exact equality because fragment order may vary)
postulate
  roundtrip-backward-forward : ∀ (h : Hierarchical) →
    let m = backward h
        h′ = forward m
    in -- Metadata must match
       (Hierarchical.metadata h ≈ʲ Hierarchical.metadata h′ ≡ true) 
       -- Fragment contents must be equivalent (modulo reordering)
       -- TODO: formalize fragment equivalence predicate

-- | Determinism: forward always produces same output for same input
postulate
  forward-deterministic : ∀ (m : Monolithic) →
    let h₁ = forward m
        h₂ = forward m
    in Hierarchical.metadata h₁ ≈ʲ Hierarchical.metadata h₂ ≡ true

-- | Completeness: all data in monolithic appears somewhere in hierarchical
-- TODO: formalize "coverage" predicate that checks all JSON paths preserved
postulate
  completeness : ∀ (m : Monolithic) (h : Hierarchical) →
    h ≡ forward m →
    -- Every JSON path in m.content appears in some fragment in h
    -- (This requires a notion of JSON path and membership)
    Bool  -- Placeholder for actual coverage check

------------------------------------------------------------------------
-- Structure preservation
------------------------------------------------------------------------

-- | Composition in monolithic space
-- (e.g., merging two JSON objects)
postulate
  compose-monolithic : Monolithic → Monolithic → Monolithic

-- | Composition in hierarchical space
-- (e.g., merging two hierarchical structures)
postulate
  compose-hierarchical : Hierarchical → Hierarchical → Hierarchical

-- | Structure preservation: forward respects composition
-- This ensures that decomposing merged data equals merging decomposed data
postulate
  structure-preserving : ∀ (m₁ m₂ : Monolithic) →
    let h₁₂ = forward (compose-monolithic m₁ m₂)
        h₁ = forward m₁
        h₂ = forward m₂
        h₁₊₂ = compose-hierarchical h₁ h₂
    in Hierarchical.metadata h₁₂ ≈ʲ Hierarchical.metadata h₁₊₂ ≡ true

------------------------------------------------------------------------
-- Specialized schemas for target decompositions
------------------------------------------------------------------------

-- | Dependency graph schema
record DependencyGraphSchema : Set where
  constructor mkDepGraphSchema
  field
    -- Number of modules in the graph
    totalModules : Nat
    
    -- Number of dependency edges
    totalDependencies : Nat
    
    -- Target hierarchical structure:
    -- build/deps/_metadata.json
    -- build/deps/modules/{Package}/{Module}.json
    -- build/deps/layers/layer-{N}.json
    -- build/deps/cycles/cycle-{N}.json (if any)
    
    metadataPath : Filepath  -- "build/deps/_metadata.json"
    modulesPath : Filepath   -- "build/deps/modules/"
    layersPath : Filepath    -- "build/deps/layers/"
    cyclesPath : Filepath    -- "build/deps/cycles/"

-- | Roadmap enrichment schema
record EnrichmentSchema : Set where
  constructor mkEnrichmentSchema
  field
    -- Number of roadmap items
    totalItems : Nat
    
    -- Target hierarchical structure:
    -- build/enriched/_metadata.json
    -- build/enriched/items/{ID}-{slug}.json
    -- build/enriched/dependencies/{ID}.deps.json
    -- build/enriched/annotations/{ID}.source.json
    
    metadataPath : Filepath      -- "build/enriched/_metadata.json"
    itemsPath : Filepath         -- "build/enriched/items/"
    dependenciesPath : Filepath  -- "build/enriched/dependencies/"
    annotationsPath : Filepath   -- "build/enriched/annotations/"

-- | Planning index schema
record PlanningSchema : Set where
  constructor mkPlanningSchema
  field
    -- Number of planning items across all adapters
    totalItems : Nat
    
    -- Number of planning adapters contributing items
    totalAdapters : Nat
    
    -- Target hierarchical structure:
    -- build/planning/_metadata.json
    -- build/planning/items/{category}/{ID}.json
    -- build/planning/sources/{adapter}.json
    -- build/planning/artifacts/files.json
    
    metadataPath : Filepath   -- "build/planning/_metadata.json"
    itemsPath : Filepath      -- "build/planning/items/"
    sourcesPath : Filepath    -- "build/planning/sources/"
    artifactsPath : Filepath  -- "build/planning/artifacts/"

------------------------------------------------------------------------
-- Utility: Index manifest generation
------------------------------------------------------------------------

-- | Index manifest: lightweight file listing items for fast lookup
record IndexManifest : Set where
  constructor mkIndexManifest
  field
    -- List of item identifiers in this index
    items : List String
    
    -- Optional: categorization or metadata
    category : Maybe String
    
    -- Total count (redundant but useful for validation)
    count : Nat

-- | Generate index manifest from fragment list
postulate
  generateIndex : List Fragment → IndexManifest

------------------------------------------------------------------------
-- Validation predicates
------------------------------------------------------------------------

-- | Check if hierarchical structure is well-formed
-- - All index files reference valid fragments
-- - No dangling references
-- - Metadata is consistent with fragment count
postulate
  isWellFormed : Hierarchical → Bool

-- | Check if two JSON values are equivalent (accounting for key reordering)
postulate
  jsonEquivalent : JSON → JSON → Bool

-- | Validate roundtrip: decompose then recompose yields original
validateRoundtrip : Monolithic → Bool
validateRoundtrip m =
  let h = forward m
      m′ = backward h
  in Monolithic.content m ≈ʲ Monolithic.content m′

------------------------------------------------------------------------
-- Export
------------------------------------------------------------------------

-- Public API for transformation
record Transformation : Set where
  constructor mkTransformation
  field
    decompose : Monolithic → Hierarchical
    recompose : Hierarchical → Monolithic
    validate : Monolithic → Bool

-- Standard transformation using forward/backward
standardTransformation : Transformation
standardTransformation = mkTransformation forward backward validateRoundtrip
