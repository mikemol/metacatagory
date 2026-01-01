{-# OPTIONS --without-K #-}

-- Examples.PhaseCategoryExamples: Demonstrations for the category of phases

module Examples.PhaseCategoryExamples where

open import Agda.Primitive using (Level; lsuc)
open import Agda.Builtin.Equality using (_≡_; refl)

open import Core.Phase using
  ( Phase
  ; mkPhase
  ; idPhase
  ; _⟫_
  ; _$ₚ_
  ; _×_
  ; _,_
  ; fst
  ; snd
  ; _⊗_
  )

open import Core.PhaseCategory using
  ( phaseCategory
  )

-- Universe parameter (examples are generic over a level)
private
  variable ℓ : Level

-- ---------------------------------------------------------------------------
-- Identity and composition examples
-- ---------------------------------------------------------------------------

-- Identity behaves as expected
identityExample : ∀ {A : Set ℓ} (a : A) → idPhase $ₚ a ≡ a
identityExample a = refl

-- Composition uses categorical order: (f ∘ g) = g ⟫ f
compositionExample : ∀ {A B C : Set ℓ}
                   (f : Phase B C) (g : Phase A B) (a : A)
                 → (g ⟫ f) $ₚ a ≡ f $ₚ (g $ₚ a)
compositionExample f g a = refl

-- Associativity holds pointwise
associativityExample : ∀ {A B C D : Set ℓ}
                      (f : Phase A B) (g : Phase B C) (h : Phase C D)
                      (a : A)
                    → ((f ⟫ g) ⟫ h) $ₚ a ≡ (f ⟫ (g ⟫ h)) $ₚ a
associativityExample f g h a = refl

-- ---------------------------------------------------------------------------
-- Parallel composition examples
-- ---------------------------------------------------------------------------

-- Running two phases in parallel over a product; both results are preserved.
parallelExample : ∀ {A B C D : Set ℓ}
                (f : Phase A B) (g : Phase C D) (a : A) (c : C)
              → (f ⊗ g) $ₚ (a , c) ≡ (f $ₚ a , g $ₚ c)
parallelExample f g a c = refl
