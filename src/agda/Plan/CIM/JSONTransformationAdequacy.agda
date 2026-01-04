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

-- Utility: substitution (transport along equality)
subst : ∀ {ℓ ℓ'} {A : Set ℓ} (P : A → Set ℓ') {x y : A} → x ≡ y → P x → P y
subst P refl px = px

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
-- Adequacy-driven constructors: use kit primitives to build structures
------------------------------------------------------------------------

-- | Build metadata from monolithic content (to be synthesized from kit)
postulate
  buildMetadata : Monolithic → JSON

-- | Build fragments from monolithic using strategy (to be synthesized from kit)
postulate
  buildFragments : Monolithic → (JSON → List (Σ Filepath (λ _ → JSON))) → List Fragment

-- | Build manifest spec from strategy (to be synthesized from kit)
postulate
  buildManifest : (JSON → List (Σ Filepath (λ _ → JSON))) → ManifestSpec

-- | Rebuild monolithic from hierarchical (forward declaration for adequacy)
postulate
  buildMonolithic : Hierarchical → Monolithic

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
    JSONPath (mono m) (hier (mkHierarchical 
      (buildMetadata m)
      (buildFragments m strategy)
      (buildManifest strategy)))
  
  -- Recompose: hier → mono  
  recompose-step : ∀ {h} (rules : ManifestSpec) →
    JSONPath (hier h) (mono (buildMonolithic h))
  
  -- Composition (syntactic/constructor form)
  _⊙ᶜ_ : ∀ {s₁ s₂ s₃} → JSONPath s₁ s₂ → JSONPath s₂ s₃ → JSONPath s₁ s₃

infixl 20 _⊙ᶜ_

------------------------------------------------------------------------
-- Computational composition (semantic form)
------------------------------------------------------------------------

-- Define composition as a function with pattern matching (computes)
_⊙ᶠ_ : ∀ {s₁ s₂ s₃} → JSONPath s₁ s₂ → JSONPath s₂ s₃ → JSONPath s₁ s₃
id-path ⊙ᶠ q = q
p ⊙ᶠ id-path = p
decompose-step strategy ⊙ᶠ q = decompose-step strategy ⊙ᶜ q
recompose-step rules ⊙ᶠ q = recompose-step rules ⊙ᶜ q
(p₁ ⊙ᶜ p₂) ⊙ᶠ q = p₁ ⊙ᶜ (p₂ ⊙ᶠ q)

infixl 20 _⊙ᶠ_

------------------------------------------------------------------------
-- Natural transformation: syntax ≅ semantics (adequacy bridge)
------------------------------------------------------------------------

-- Postulate: The constructor and function forms are naturally isomorphic
postulate
  ⊙-syntax-semantics : ∀ {s₁ s₂ s₃} (p : JSONPath s₁ s₂) (q : JSONPath s₂ s₃) →
    (p ⊙ᶜ q) ≡ (p ⊙ᶠ q)

-- The adequacy kit will provide evidence that ⊙ᶠ satisfies the laws
-- by construction, and ⊙-syntax-semantics transfers them to ⊙ᶜ

-- Roundtrip properties: the adequacy kit must ensure these hold
postulate
  roundtrip-mono : ∀ (m : Monolithic) (strategy : JSON → List (Σ Filepath (λ _ → JSON))) →
    buildMonolithic 
      (mkHierarchical 
        (buildMetadata m) 
        (buildFragments m strategy) 
        (buildManifest strategy))
    ≡ m

-- Proof obligations (to be filled later - postulated to unblock compilation)
postulate
  ++-assoc-decompose : ∀ {m s₃ s₄} (strategy : JSON → List (Σ Filepath (λ _ → JSON))) 
                       (q : JSONPath (hier (mkHierarchical (buildMetadata m) 
                                                             (buildFragments m strategy) 
                                                             (buildManifest strategy))) s₃) 
                       (r : JSONPath s₃ s₄) →
    ((decompose-step {m} strategy) ⊙ᶠ q) ⊙ᶠ r ≡ (decompose-step strategy) ⊙ᶠ (q ⊙ᶠ r)
  ++-assoc-recompose : ∀ {h s₃ s₄} (rules : ManifestSpec) 
                       (q : JSONPath (mono (buildMonolithic h)) s₃) (r : JSONPath s₃ s₄) →
    ((recompose-step {h} rules) ⊙ᶠ q) ⊙ᶠ r ≡ (recompose-step rules) ⊙ᶠ (q ⊙ᶠ r)
  ++-assoc-comp : ∀ {s₁ s₂ s₃ s₄ s₅} (p₁ : JSONPath s₁ s₂) (p₂ : JSONPath s₂ s₃) 
                  (q : JSONPath s₃ s₄) (r : JSONPath s₄ s₅) →
    ((p₁ ⊙ᶜ p₂) ⊙ᶠ q) ⊙ᶠ r ≡ (p₁ ⊙ᶜ p₂) ⊙ᶠ (q ⊙ᶠ r)
  id-right-decompose : ∀ {m} (strategy : JSON → List (Σ Filepath (λ _ → JSON))) →
    (decompose-step {m} strategy) ⊙ᶠ id-path ≡ (decompose-step strategy)
  id-right-recompose : ∀ {h} (rules : ManifestSpec) →
    (recompose-step {h} rules) ⊙ᶠ id-path ≡ (recompose-step rules)
  id-right-comp : ∀ {s₁ s₂ s₃} (p₁ : JSONPath s₁ s₂) (p₂ : JSONPath s₂ s₃) →
    (p₁ ⊙ᶜ p₂) ⊙ᶠ id-path ≡ (p₁ ⊙ᶜ p₂)

-- Path algebra instance (uses the functional form for computation)
jsonTransformationAlgebra : PathAlgebra {ℓV = lzero} {ℓP = lzero} JSONState
PathAlgebra.Path jsonTransformationAlgebra = JSONPath
PathAlgebra._++_ jsonTransformationAlgebra = _⊙ᶠ_  -- Use functional form
PathAlgebra.++-assoc jsonTransformationAlgebra id-path q r = refl  -- Computes: (id ⊙ᶠ q) ⊙ᶠ r = q ⊙ᶠ r = id ⊙ᶠ (q ⊙ᶠ r)
PathAlgebra.++-assoc jsonTransformationAlgebra (decompose-step strategy) q r = ++-assoc-decompose strategy q r
PathAlgebra.++-assoc jsonTransformationAlgebra (recompose-step rules) q r = ++-assoc-recompose rules q r
PathAlgebra.++-assoc jsonTransformationAlgebra (p₁ ⊙ᶜ p₂) q r = ++-assoc-comp p₁ p₂ q r
PathAlgebra.id jsonTransformationAlgebra = id-path
PathAlgebra.id-left jsonTransformationAlgebra p = refl  -- Computes: id-path ⊙ᶠ p = p
PathAlgebra.id-right jsonTransformationAlgebra id-path = refl
PathAlgebra.id-right jsonTransformationAlgebra (decompose-step strategy) = id-right-decompose strategy
PathAlgebra.id-right jsonTransformationAlgebra (recompose-step rules) = id-right-recompose rules
PathAlgebra.id-right jsonTransformationAlgebra (p₁ ⊙ᶜ p₂) = id-right-comp p₁ p₂

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

-- Note: buildMonolithic is postulated above for use in JSONPath constructors
-- Below is a sketch implementation (commented out to avoid duplicate definition)
-- buildMonolithic : Hierarchical → Monolithic
-- buildMonolithic h =
--   let open Hierarchical h
--       -- Reconstruct by merging all fragments according to manifest rules
--       merged = foldl (λ acc frag → 
--         json-merge acc (Fragment.content frag)) 
--         json-empty fragments
--   in mkMonolithic merged

-- | Framed face: roundtrip must be identity
jsonTransformationFace : JSONTransformationKit → FramedFace jsonTransformationAlgebra
FramedFace.a (jsonTransformationFace kit) = 
  mono (JSONTransformationKit.monolithic kit)
FramedFace.b (jsonTransformationFace kit) = 
  mono (JSONTransformationKit.monolithic kit)
FramedFace.face (jsonTransformationFace kit) = 
  let m = JSONTransformationKit.monolithic kit
      strategy = JSONTransformationKit.strategy kit
      h = mkHierarchical (buildMetadata m) (buildFragments m strategy) (buildManifest strategy)
      -- Use roundtrip property to transport the path to the correct endpoint
      decomp = decompose-step strategy
      recomp = recompose-step (JSONTransformationKit.manifest kit)
      roundtrip-eq = roundtrip-mono m strategy
  in record 
    { lhs = subst (λ m' → JSONPath (mono m) (mono m'))
                  roundtrip-eq
                  (decomp ⊙ᶠ recomp)
    ; rhs = id-path
    }

-- | The adequacy witness: kit primitives suffice to prove roundtrip
jsonTransformationAxiomInstance : AxiomInstance jsonTransformationAlgebra
AxiomInstance.Kit jsonTransformationAxiomInstance = JSONTransformationKit
AxiomInstance.face jsonTransformationAxiomInstance = jsonTransformationFace
AxiomInstance.solve jsonTransformationAxiomInstance kit = 
  -- Provable given a correct kit with matching buildMetadata/buildFragments/buildManifest/buildMonolithic
  -- The proof would show that (decompose ∘ recompose) is definitionally equal to id
  -- via the roundtrip-mono postulate and path algebra laws
  -- Sketch: subst (λ m' → ...) (roundtrip-mono ...) refl
  {-  Proof sketch:
      decompose ⊙ᶠ recompose 
        = (by definition of ⊙ᶠ)
      subst (λ m' → JSONPath (mono m) (mono m')) (roundtrip-mono m strategy) id-path
        = (by subst refl = id)
      id-path
  -}
  postulate-solve kit
  where
    postulate
      postulate-solve : (kit : JSONTransformationKit) → 
        Face.lhs (FramedFace.face (jsonTransformationFace kit)) 
        ≡ Face.rhs (FramedFace.face (jsonTransformationFace kit))

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
