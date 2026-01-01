{-# OPTIONS --without-K #-}

-- | FunctionCategory: CategoryLike instance for sets with functions as morphisms,
-- plus the identity functor via the generic interface.

module Infrastructure.Functor.Instances.FunctionCategory where

open import Agda.Primitive using (Level; _⊔_; lsuc)
open import Agda.Builtin.Equality using (_≡_; refl)

open import Infrastructure.Functor.Interface
open import Infrastructure.Functor.Instances.Trivial

record Lift {ℓ ℓ'} (A : Set ℓ) : Set (ℓ ⊔ ℓ') where
  constructor lift
  field lower : A

FunctionCategory : ∀ {ℓ} → CategoryLike (Set ℓ)
CategoryLike.Hom      (FunctionCategory {ℓ}) A B = Lift {ℓ = ℓ} {ℓ' = lsuc ℓ} (A → B)
CategoryLike.id       (FunctionCategory {ℓ}) = lift (λ x → x)
CategoryLike._∘_      (FunctionCategory {ℓ}) f g = lift (comp (Lift.lower f) (Lift.lower g))
  where
    comp : ∀ {A B C : Set ℓ} → (B → C) → (A → B) → A → C
    comp h k x = h (k x)
CategoryLike.id-left  (FunctionCategory {ℓ}) _ = refl
CategoryLike.id-right (FunctionCategory {ℓ}) _ = refl
CategoryLike.assoc    (FunctionCategory {ℓ}) _ _ _ = refl

module FunctionIdentity {ℓ} where
  open IdentityFunctor (FunctionCategory {ℓ}) public
    using () renaming (identity to functionIdentityFunctor)
