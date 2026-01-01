{-# OPTIONS --without-K #-}

-- | Funext adapter: package function extensionality as a reusable kit.
-- This keeps functor instances that need pointwise→definitional equality
-- parameterized rather than postulated ad hoc.

module Infrastructure.Functor.Adapters.Funext where

open import Agda.Primitive using (Level; _⊔_)
open import Agda.Builtin.Equality using (_≡_)

-- | Bundle of function extensionality for a particular domain/codomain.
record Funext {ℓ₁ ℓ₂ : Level} (A : Set ℓ₁) (B : Set ℓ₂) : Set (ℓ₁ ⊔ ℓ₂) where
  field
    funext : ∀ {f g : A → B} → ((x : A) → f x ≡ g x) → f ≡ g

-- | Project the packaged extensionality principle.
pointwise→≡ : ∀ {ℓ₁ ℓ₂} {A : Set ℓ₁} {B : Set ℓ₂} → Funext A B →
  ∀ {f g : A → B} → ((x : A) → f x ≡ g x) → f ≡ g
pointwise→≡ kit = Funext.funext kit
