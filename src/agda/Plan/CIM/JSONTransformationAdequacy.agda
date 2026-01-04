{-# OPTIONS --without-K #-}

-- | Adequacy kit for JSON transformations: synthesize forward/backward from primitives.
--
-- Instead of Python FFI, we construct the natural transformation from smaller
-- compositional operations (JSON navigation, fragment assembly, manifest generation).
-- The adequacy framework ensures these primitives are sufficient.
--
-- Roadmap: BUILD-JSON-SCHEMA (extended with adequacy)
-- Pattern: Infrastructure/Functor/Instances/*.agda (adequacy modules)

module Plan.CIM.JSONTransformationAdequacy where

open import Agda.Primitive using (Level; lzero; lsuc)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Agda.Builtin.String using (String)
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Maybe using (Maybe; just; nothing)
open import Agda.Builtin.Nat using (Nat)
open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.Sigma using (_,_; Σ)

open import Infrastructure.Axiom.Adequacy using (PathAlgebra)
open import Infrastructure.Axiom.Instance using (AxiomInstance; FramedFace)
open import Infrastructure.Axiom.Face using (Face)

open import Plan.CIM.JSONTransformation
  using (JSON; Filepath; Monolithic; Hierarchical; Fragment; ManifestSpec;
         mkMonolithic; mkHierarchical; mkFragment; mkManifest)

------------------------------------------------------------------------
-- Primitive operations: the "generators" for JSON transformations
------------------------------------------------------------------------

-- | Navigate JSON structure to extract a field
postulate
  json-get : JSON → String → Maybe JSON

-- | Navigate JSON array to get element at index
postulate
  json-at : JSON → Nat → Maybe JSON

-- | Create empty JSON object
postulate
  json-empty : JSON

-- | Add field to JSON object
postulate
  json-set : JSON → String → JSON → JSON

-- | Merge two JSON objects
postulate
  json-merge : JSON → JSON → JSON

-- | Convert JSON to string for writing
postulate
  json-serialize : JSON → String

-- | Parse string to JSON
postulate
  json-parse : String → Maybe JSON

-- | List all keys in JSON object
postulate
  json-keys : JSON → List String

-- | List all elements in JSON array
postulate
  json-array-items : JSON → List JSON

-- | Check if two JSONs are structurally equivalent
postulate
  json-equiv : JSON → JSON → Bool

------------------------------------------------------------------------
-- Derived operations: compose primitives into fragment operations
------------------------------------------------------------------------

-- | Extract metadata field from monolithic JSON
extractMetadata : JSON → JSON
extractMetadata j with json-get j "metadata"
... | just m  = m
... | nothing = json-empty

-- | Extract items array from monolithic JSON
extractItems : JSON → List JSON
extractItems j with json-get j "items"
... | just items = json-array-items items
... | nothing    = []

-- | Build fragment from item and path
mkFragmentFromItem : (path : Filepath) → JSON → Fragment
mkFragmentFromItem path item = mkFragment path item

-- | Generate index manifest from item list
postulate
  generateIndexManifest : List JSON → JSON

-- Helper for folding (will be used in implementations)
postulate
  foldl : ∀ {A B : Set} → (B → A → B) → B → List A → B

-- | Generate metadata JSON from stats  
postulate
  generateMetadata : (totalItems : Nat) → (timestamp : String) → JSON

------------------------------------------------------------------------
-- Adequacy Kit: primitives needed to solve one transformation
------------------------------------------------------------------------

record JSONTransformationKit : Set where
  field
    -- Input monolithic structure
    monolithic : Monolithic
    
    -- Target hierarchy specification
    targetRoot : Filepath
    
    -- Decomposition strategy (which fields → which fragments)
    strategy : JSON → List (Σ Filepath (λ _ → JSON))
    
    -- Metadata generation (stats from monolithic)
    metadataGen : JSON → JSON
    
    -- Index generation (from fragment list)
    indexGen : List Fragment → JSON
    
    -- Manifest spec (reconstruction rules)
    manifest : ManifestSpec

------------------------------------------------------------------------
-- Path algebra: transformations as paths in JSON-structure space
------------------------------------------------------------------------

-- | States in JSON transformation space (either monolithic or hierarchical)
data JSONState : Set where
  mono : Monolithic → JSONState
  hier : Hierarchical → JSONState

-- | Paths are transformation sequences
data JSONPath : JSONState → JSONState → Set where
  -- Identity: no transformation
  id-path : ∀ {s} → JSONPath s s
  
  -- Decompose: mono → hier
  decompose-step : ∀ {m} (strategy : JSON → List (Σ Filepath (λ _ → JSON))) →
    JSONPath (mono m) (hier (mkHierarchical {!!} {!!} {!!}))  -- to be filled by kit
  
  -- Recompose: hier → mono  
  recompose-step : ∀ {h} (rules : ManifestSpec) →
    JSONPath (hier h) (mono (mkMonolithic {!!}))  -- to be filled by kit
  
  -- Composition
  _⊙_ : ∀ {s₁ s₂ s₃} → JSONPath s₁ s₂ → JSONPath s₂ s₃ → JSONPath s₁ s₃

-- Path algebra instance
jsonTransformationAlgebra : PathAlgebra {ℓV = lzero} {ℓP = lzero} JSONState
PathAlgebra.Path jsonTransformationAlgebra = JSONPath
PathAlgebra._++_ jsonTransformationAlgebra = _⊙_
PathAlgebra.++-assoc jsonTransformationAlgebra id-path q r = refl
PathAlgebra.++-assoc jsonTransformationAlgebra (decompose-step strategy) q r = {!!}  -- provable
PathAlgebra.++-assoc jsonTransformationAlgebra (recompose-step rules) q r = {!!}     -- provable
PathAlgebra.++-assoc jsonTransformationAlgebra (p₁ ⊙ p₂) q r = {!!}                  -- provable by induction
PathAlgebra.id jsonTransformationAlgebra = id-path
PathAlgebra.id-left jsonTransformationAlgebra p = refl
PathAlgebra.id-right jsonTransformationAlgebra id-path = refl
PathAlgebra.id-right jsonTransformationAlgebra (decompose-step strategy) = {!!}  -- provable
PathAlgebra.id-right jsonTransformationAlgebra (recompose-step rules) = {!!}     -- provable
PathAlgebra.id-right jsonTransformationAlgebra (p₁ ⊙ p₂) = {!!}                  -- provable by induction

------------------------------------------------------------------------
-- Adequacy instance: kit → solvable roundtrip face
------------------------------------------------------------------------

-- | Build hierarchical from kit primitives
buildHierarchical : JSONTransformationKit → Hierarchical
buildHierarchical kit = 
  let open JSONTransformationKit kit
      content = Monolithic.content monolithic
      fragments = map (λ where (path , json) → mkFragment path json) 
                      (strategy content)
      metadata = metadataGen content
      idx = indexGen fragments
  in mkHierarchical metadata fragments manifest
  where
    postulate map : ∀ {A B : Set} → (A → B) → List A → List B

-- | Rebuild monolithic from hierarchical primitives
buildMonolithic : Hierarchical → Monolithic
buildMonolithic h =
  let open Hierarchical h
      -- Reconstruct by merging all fragments according to manifest rules
      merged = foldl (λ acc frag → 
        json-merge acc (Fragment.content frag)) 
        json-empty fragments
  in mkMonolithic merged

-- | Framed face: roundtrip must be identity
jsonTransformationFace : JSONTransformationKit → FramedFace jsonTransformationAlgebra
FramedFace.a (jsonTransformationFace kit) = 
  mono (JSONTransformationKit.monolithic kit)
FramedFace.b (jsonTransformationFace kit) = 
  mono (JSONTransformationKit.monolithic kit)
FramedFace.face (jsonTransformationFace kit) = record 
  { lhs = decompose-step (JSONTransformationKit.strategy kit) 
          ⊙ recompose-step (JSONTransformationKit.manifest kit)
  ; rhs = id-path
  }

-- | The adequacy witness: kit primitives suffice to prove roundtrip
jsonTransformationAxiomInstance : AxiomInstance jsonTransformationAlgebra
AxiomInstance.Kit jsonTransformationAxiomInstance = JSONTransformationKit
AxiomInstance.face jsonTransformationAxiomInstance = jsonTransformationFace
AxiomInstance.solve jsonTransformationAxiomInstance kit = {!!}  -- Provable given correct kit

------------------------------------------------------------------------
-- Concrete kits for each target decomposition
------------------------------------------------------------------------

-- | Kit for dependency_graph.json → build/deps/
postulate
  dependencyGraphKit : JSONTransformationKit

-- | Kit for canonical_enriched.json → build/enriched/  
postulate
  enrichedRoadmapKit : JSONTransformationKit

-- | Kit for planning_index.json → build/planning/
postulate
  planningIndexKit : JSONTransformationKit

------------------------------------------------------------------------
-- Adequacy claim: these kits solve their respective transformations
------------------------------------------------------------------------

-- The solve obligations for each kit witness that:
--   1. Forward transformation can be synthesized from primitives
--   2. Backward transformation can be synthesized from primitives  
--   3. Roundtrip backward ∘ forward = id follows from kit construction

-- Instead of Python FFI, we now have a formal proof obligation
-- that the kit's primitives are SUFFICIENT to construct the transformation.

-- Next steps:
--   - Implement the {!!} holes (associativity, identity laws)
--   - Provide concrete kit instances with actual strategies
--   - Extract verified Haskell/Python from proven kits
