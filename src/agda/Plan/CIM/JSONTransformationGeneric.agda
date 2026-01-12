{-# OPTIONS --without-K #-}

-- | JSON Transformation using Generic Duality Framework
--
-- Before: JSONTransformationAdequacy.agda had 439 lines with manual mutual blocks,
--         cogenerators, natural transformations.
--
-- After:  Just instantiate GenericDualFramework with the interface.
--         Everything else is derived automatically.

module Plan.CIM.JSONTransformationGeneric where

open import Agda.Primitive using (Level; lzero; lsuc)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Agda.Builtin.String using (String)
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Maybe using (Maybe; just; nothing)
open import Agda.Builtin.Nat using (Nat)
open import Agda.Builtin.Sigma using (Σ; _,_)

open import Infrastructure.Adequacy.Polymorphic using 
  (_⊎_; inl; inr; DualityInterface; ProvidedDirection)
open Infrastructure.Adequacy.Polymorphic using 
  (module GenericDualPaths; module GenericDualAlgebra; module GenericDualAdequacy)
open ProvidedDirection using (Forward; Backward)
open import Plan.CIM.JSONTransformation

------------------------------------------------------------------------
-- State Spaces (from JSONTransformation.agda)
------------------------------------------------------------------------

-- Combined state for paths: either Monolithic or Hierarchical
data JSONState : Set where
  mono : Monolithic → JSONState
  hier : Hierarchical → JSONState

------------------------------------------------------------------------
-- Helper Functions (postulated for now)
------------------------------------------------------------------------

postulate
  buildMetadata : Monolithic → JSON
  buildFragments : Monolithic → List String → List Fragment
  buildManifest : List String → ManifestSpec
  buildMonolithic : Hierarchical → Monolithic

postulate
  defaultStrategy : List String

------------------------------------------------------------------------
-- The Core Interface: JSON Transformation as Duality
------------------------------------------------------------------------

json-duality-interface : DualityInterface lzero
json-duality-interface = record
  { StateA = Monolithic
  ; StateB = Hierarchical
  ; State = JSONState
  ; inj-A = mono
  ; inj-B = hier
  ; direction = Forward
  ; forward = λ m →
      mkHierarchical
        (buildMetadata m)
        (buildFragments m defaultStrategy)
        (buildManifest defaultStrategy)
  ; backward = buildMonolithic
  ; coverage-fwd-roundtrip = json-coverage-fwd
  ; coverage-bwd-roundtrip = json-coverage-bwd
  }
  where
    postulate
      json-coverage-fwd : ∀ (a : Monolithic) →
        buildMonolithic 
          (mkHierarchical
            (buildMetadata a)
            (buildFragments a defaultStrategy)
            (buildManifest defaultStrategy)) ≡ a
      json-coverage-bwd : ∀ (b : Hierarchical) →
        mkHierarchical
          (buildMetadata (buildMonolithic b))
          (buildFragments (buildMonolithic b) defaultStrategy)
          (buildManifest defaultStrategy) ≡ b

------------------------------------------------------------------------
-- Instantiate the Generic Framework
------------------------------------------------------------------------

-- Export the interface and its derived modules
open GenericDualPaths json-duality-interface public
open GenericDualAlgebra json-duality-interface public
open GenericDualAdequacy json-duality-interface public

-- Convenience aliases
JSONPath : JSONState → JSONState → Set
JSONPath = DualPath

json-decompose : Monolithic → Hierarchical
json-decompose = DualityInterface.forward json-duality-interface

-- Recompose a hierarchical JSON back to monolithic.
dependency_graph_recomposed_json : Hierarchical → Monolithic
dependency_graph_recomposed_json = DualityInterface.backward json-duality-interface
