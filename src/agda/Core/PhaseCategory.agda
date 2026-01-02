{-# OPTIONS --without-K #-}

-- | PhaseCategory: category over phases with lifted morphisms.
module Core.PhaseCategory where

open import Agda.Primitive using (Level; _⊔_; lsuc)

-- | Wrapper that lifts a phase morphism into a higher universe.
record PhaseLift {α β : Level} (A : Set α) : Set (α ⊔ β) where
  constructor lift
  field lower : A
open import Agda.Builtin.Equality using (_≡_; refl)

open import Infrastructure.Universe using (Setℓ)
open import Infrastructure.Functor.Interface using (CategoryLike; FunctorInstance)
open import Core.Phase using (Phase; _⟫_; idPhase; _$ₚ_; mkPhase; _×_; _,_; fst; snd; _⊗_)

------------------------------------------------------------------------
-- Category of phases re-expressed with CategoryLike
------------------------------------------------------------------------

phaseCategory : ∀ {ℓ} → CategoryLike {lsuc ℓ} (Setℓ ℓ)
phaseCategory {ℓ} = record
  { Hom      = λ A B → PhaseLift {α = ℓ} {β = lsuc ℓ} (Phase A B)
  ; id       = lift idPhase
  ; _∘_      = λ g f → lift (PhaseLift.lower f ⟫ PhaseLift.lower g)
  ; id-left  = λ f → refl
  ; id-right = λ f → refl
  ; assoc    = λ h g f → refl
  }

phaseIdentity : ∀ {ℓ} → FunctorInstance (phaseCategory {ℓ}) (phaseCategory {ℓ})
phaseIdentity = record
  { objMap      = λ A → A
  ; map         = λ f → f
  ; map-id      = refl
  ; map-compose = λ _ _ → refl
  }
