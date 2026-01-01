{-# OPTIONS --without-K #-}

-- | PhaseCategoryWrapper: expose Core.Phase as a CategoryLike so it can plug
-- into the generic functor interface (IdentityFunctor, etc.).

module Core.PhaseCategoryWrapper where

open import Agda.Primitive using (Level; lsuc; _⊔_)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Infrastructure.Functor.Interface
open import Core.Phase using (Phase; _⟫_; idPhase)

-- Lift to align Hom universe with CategoryLike expectation.
record Lift {ℓ ℓ'} (A : Set ℓ) : Set (ℓ ⊔ ℓ') where
  constructor lift
  field lower : A

cong : ∀ {ℓ₁ ℓ₂} {A : Set ℓ₁} {B : Set ℓ₂} {x y : A} (f : A → B) → x ≡ y → f x ≡ f y
cong f refl = refl

PhaseCategoryLike : ∀ {ℓ} → CategoryLike (Set ℓ)
CategoryLike.Hom      (PhaseCategoryLike {ℓ}) A B = Lift {ℓ = ℓ} {ℓ' = lsuc ℓ} (Phase A B)
CategoryLike.id       (PhaseCategoryLike {ℓ}) = lift idPhase
CategoryLike._∘_      (PhaseCategoryLike {ℓ}) {A = A} {B = B} {C = C} f g = lift (Lift.lower g ⟫ Lift.lower f)
CategoryLike.id-left  (PhaseCategoryLike {ℓ}) {A = A} {B = B} f = cong lift refl
CategoryLike.id-right (PhaseCategoryLike {ℓ}) {A = A} {B = B} f = cong lift refl
CategoryLike.assoc    (PhaseCategoryLike {ℓ}) {A = A} {B = B} {C = C} {D = D} h g f = cong lift refl
