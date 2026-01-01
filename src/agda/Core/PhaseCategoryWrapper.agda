{-# OPTIONS --without-K #-}

-- | PhaseCategoryWrapper: expose Core.Phase as a CategoryLike so it can plug
-- into the generic functor interface (IdentityFunctor, etc.).

module Core.PhaseCategoryWrapper where

open import Agda.Primitive using (Level; lsuc; _⊔_)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Infrastructure.Functor.Interface
open import Infrastructure.Equality using (cong)
open import Core.Phase using (Phase; _⟫_; idPhase)

-- Lift to align Hom universe with CategoryLike expectation.
record PhaseLiftW {ℓ ℓ'} (A : Set ℓ) : Set (ℓ ⊔ ℓ') where
  constructor lift
  field lower : A

PhaseCategoryLike : ∀ {ℓ} → CategoryLike (Set ℓ)
CategoryLike.Hom      (PhaseCategoryLike {ℓ}) A B = PhaseLiftW {ℓ = ℓ} {ℓ' = lsuc ℓ} (Phase A B)
CategoryLike.id       (PhaseCategoryLike {ℓ}) = lift idPhase
CategoryLike._∘_      (PhaseCategoryLike {ℓ}) {A = A} {B = B} {C = C} f g = lift (PhaseLiftW.lower g ⟫ PhaseLiftW.lower f)
CategoryLike.id-left  (PhaseCategoryLike {ℓ}) {A = A} {B = B} f = cong lift refl
CategoryLike.id-right (PhaseCategoryLike {ℓ}) {A = A} {B = B} f = cong lift refl
CategoryLike.assoc    (PhaseCategoryLike {ℓ}) {A = A} {B = B} {C = C} {D = D} h g f = cong lift refl
