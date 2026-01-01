{-# OPTIONS --without-K #-}

-- | FunctionPathCategory: functions as morphisms with pointwise equality.
-- Provides a CategoryLike instance and pointwise identity/compose proofs.

module Infrastructure.Functor.Instances.FunctionPathCategory where

open import Agda.Primitive using (Level; _⊔_; lsuc)
open import Agda.Builtin.Equality using (_≡_; refl)

open import Infrastructure.Functor.Interface

-- Simple lift to adjust universe for Hom
record Lift {ℓ ℓ'} (A : Set ℓ) : Set (ℓ ⊔ ℓ') where
  constructor lift
  field lower : A

-- Category of functions with pointwise equality
FunctionPathCategory : ∀ {ℓ} → CategoryLike (Set ℓ)
CategoryLike.Hom      (FunctionPathCategory {ℓ}) A B = Lift {ℓ = ℓ} {ℓ' = lsuc ℓ} (A → B)
CategoryLike.id       (FunctionPathCategory {ℓ}) = lift (λ x → x)
CategoryLike._∘_      (FunctionPathCategory {ℓ}) f g = lift (comp (Lift.lower f) (Lift.lower g))
  where
    comp : ∀ {A B C : Set ℓ} → (B → C) → (A → B) → A → C
    comp h k x = h (k x)
CategoryLike.id-left  (FunctionPathCategory {ℓ}) f = refl
CategoryLike.id-right (FunctionPathCategory {ℓ}) f = refl
CategoryLike.assoc    (FunctionPathCategory {ℓ}) h g f = refl
