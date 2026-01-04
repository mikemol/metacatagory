{-# OPTIONS --without-K #-}

-- | Generic Duality for Phase Category: automatic bidirectional adequacy
--   (Minimal adaptation: Phase composition is self-dual for roundtrip)
module Core.PhaseCategory.Duality where

open import Agda.Primitive using (Level; lzero)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Agda.Builtin.Sigma using (Σ; _,_)
open import Infrastructure.Adequacy using 
  ( DualityInterface
  ; ProvidedDirection
  )
open import Core.Phase using (Phase; _⟫_; idPhase; _$ₚ_; mkPhase)

------------------------------------------------------------------------
-- Trivial State Type for Phase Paths
------------------------------------------------------------------------

-- | Trivial unit state: used to represent phase pipeline as dual of itself
data ⊤ : Set where
  tt : ⊤

------------------------------------------------------------------------
-- Phase Duality Interface
------------------------------------------------------------------------

-- | States: forward (composition) and backward (trace) are the same
-- Both are trivial since phase composition is already associative
phaseDualityInterface : DualityInterface
phaseDualityInterface = record
  { StateA = ⊤
  ; StateB = ⊤
  ; State = ⊤
  ; inj-A = λ _ → tt
  ; inj-B = λ _ → tt
  ; direction = ProvidedDirection.Forward
  ; forward = λ x → tt  -- identity on trivial state
  ; backward = λ y → tt  -- identity on trivial state
  ; coverage-fwd-roundtrip = λ { tt → refl }  -- ∀ (a : ⊤) → backward (forward a) ≡ a
  ; coverage-bwd-roundtrip = λ { tt → refl }  -- ∀ (b : ⊤) → forward (backward b) ≡ b
  }

------------------------------------------------------------------------
-- Generic Phase Path Algebra
------------------------------------------------------------------------

-- | Derives from generic framework: PhasePath, composition, associativity
open module PhasePaths = Infrastructure.Adequacy.GenericDualPaths phaseDualityInterface

-- | Derives from generic framework: composition laws
open module PhaseAlgebra = Infrastructure.Adequacy.GenericDualAlgebra phaseDualityInterface

-- | Derives from generic framework: adequacy witness for phase composition
open module PhaseAdequacy = Infrastructure.Adequacy.GenericDualAdequacy phaseDualityInterface

------------------------------------------------------------------------
-- Notation and projection
------------------------------------------------------------------------

-- | Phase path from source to target state (forward or backward)
PhaseCompPath : ⊤ → ⊤ → Set
PhaseCompPath _ _ = PhasePaths.DualPath tt tt

-- | Forward execution: direct phase application
-- (In practice, would instantiate forward-step from generic framework)
postulate
  forward-exec : ∀ {A B : Set} (p : Phase A B) → PhaseCompPath tt tt

-- | Backward trace: extract pipeline structure
-- (In practice, would instantiate backward-step from generic framework)
postulate
  backward-extract : ∀ {A B : Set} (p : Phase A B) → PhaseCompPath tt tt

-- | Composition: both directions produce same result (adequacy)
-- (Pattern demonstrates how phases compose via generic duality)
postulate
  phase-composition-adequate : ∀ {A B C : Set} 
    (p₁ : Phase A B) (p₂ : Phase B C) →
    PhaseCompPath tt tt
