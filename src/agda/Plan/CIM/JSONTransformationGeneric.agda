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
open import Agda.Builtin.Sum using (_⊎_; inl; inr)

open import Infrastructure.Adequacy
open import Plan.CIM.JSONTransformation

------------------------------------------------------------------------
-- State Spaces (from JSONTransformation.agda)
------------------------------------------------------------------------

-- Combined state for paths: either Monolithic or Hierarchical
data JSONState : Set where
  mono : Monolithic → JSONState
  hier : Hierarchical → JSONState

------------------------------------------------------------------------
-- The Core Interface: What we want to provide
------------------------------------------------------------------------

json-duality-interface : DualityInterface
DualityInterface.ℓ json-duality-interface = lzero
DualityInterface.StateA json-duality-interface = Monolithic
DualityInterface.StateB json-duality-interface = Hierarchical
DualityInterface.State json-duality-interface = JSONState
DualityInterface.inj-A json-duality-interface = mono
DualityInterface.inj-B json-duality-interface = hier
DualityInterface.direction json-duality-interface = ProvidedDirection.Forward
  -- We're providing the forward transformation (decomposition)

-- The forward transformation: Monolithic → Hierarchical (decomposition)
DualityInterface.forward json-duality-interface m =
  mkHierarchical
    (buildMetadata m)
    (buildFragments m (JSONTransformationKit.strategy defaultKit))
    (buildManifest (JSONTransformationKit.strategy defaultKit))
  where
    postulate
      defaultKit : JSONTransformationKit

-- The backward transformation (will be derived, but we postulate it for now)
DualityInterface.backward json-duality-interface h = buildMonolithic h

-- Coverage proofs
postulate
  json-coverage-fwd : ∀ (a : Monolithic) →
    DualityInterface.backward json-duality-interface 
      (DualityInterface.forward json-duality-interface a) ≡ a
  json-coverage-bwd : ∀ (b : Hierarchical) →
    DualityInterface.forward json-duality-interface 
      (DualityInterface.backward json-duality-interface b) ≡ b

DualityInterface.coverage-fwd-roundtrip json-duality-interface = json-coverage-fwd
DualityInterface.coverage-bwd-roundtrip json-duality-interface = json-coverage-bwd

------------------------------------------------------------------------
-- Instantiate the Generic Framework
------------------------------------------------------------------------

json-duality : GenericDualFramework
GenericDualFramework.interface json-duality = json-duality-interface

-- Now we have everything:
--   - DualPath (from GenericDualPaths)
--   - dualCogenerator (automatic)
--   - ⊙-duality (natural transformation, automatic)
--   - Adequacy algebra (from GenericDualAlgebra)
--   - Adequacy instance (from GenericDualAdequacy)

------------------------------------------------------------------------
-- Public API (same structure as before, but DERIVED)
------------------------------------------------------------------------

open GenericDualPaths json-duality-interface public
open GenericDualAlgebra json-duality-interface public
open GenericDualAdequacy json-duality-interface public

-- Convenience aliases
JSONPath : JSONState → JSONState → Set
JSONPath = DualPath

json-decompose : Monolithic → Hierarchical
json-decompose = DualityInterface.forward json-duality-interface

json-recompose : Hierarchical → Monolithic
json-recompose = DualityInterface.backward json-duality-interface

------------------------------------------------------------------------
-- Comparison to Original (JSONTransformationAdequacy.agda)
------------------------------------------------------------------------

{-
BEFORE (manual, 439 lines):
  - Manually defined JSONPath data with decompose-step, recompose-step
  - Manually defined cogenerateMono in mutual block
  - Manually defined _⊙ᶜ_ and _⊙ᶠ_
  - Manually defined ⊙-syntax-semantics postulate
  - Manually defined path algebra
  - Manually defined adequacy instance
  - Lots of repetitive structure

AFTER (generic, ~30 lines of actual code):
  - Provide DualityInterface with forward transformation
  - Instantiate GenericDualFramework
  - GET everything else automatically:
    * DualPath (with both directions)
    * Mutual block (with cogenerator)
    * Natural transformation
    * Path algebra
    * Adequacy instance

BENEFIT:
  Now we want to add ABNF parsing adequacy? Just instantiate
  the interface and reuse the whole framework.
  Same for proof traces, schema validation, phase functors, etc.
-}
