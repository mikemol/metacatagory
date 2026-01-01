{-# OPTIONS --without-K #-}

-- | PhaseCategoryWrapper: expose Core.Phase as a CategoryLike so it can plug
-- into the generic functor interface (IdentityFunctor, etc.).

module Core.PhaseCategoryWrapper where

open import Agda.Primitive using (Level)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Infrastructure.Functor.Interface

open import Core.Phase using (Phase; _⟫_; idPhase)

PhaseCategoryLike : ∀ {ℓ} → CategoryLike (Set ℓ)
CategoryLike.Hom      PhaseCategoryLike A B = Phase A B
CategoryLike.id       PhaseCategoryLike = idPhase
CategoryLike._∘_      PhaseCategoryLike = _⟫_
CategoryLike.id-left  PhaseCategoryLike _ = refl
CategoryLike.id-right PhaseCategoryLike _ = refl
CategoryLike.assoc    PhaseCategoryLike _ _ _ = refl
