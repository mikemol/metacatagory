{-# OPTIONS --without-K #-}

-- | Examples demonstrating equality combinators.
module Examples.EqualityExamples where

open import Agda.Primitive using (Level)
open import Agda.Builtin.Equality using (_≡_; refl)

open import Infrastructure.Equality using (cong; sym; trans)

-- | Basic uses of the shared equality helpers.
congExample : ∀ {ℓ₁ ℓ₂} {A : Set ℓ₁} {B : Set ℓ₂} (f : A → B) {x : A} → cong f (refl {x = x}) ≡ refl
congExample f = refl

symExample : ∀ {ℓ} {A : Set ℓ} {x y : A} (p : x ≡ y) → sym (sym p) ≡ p
symExample refl = refl

transExample : ∀ {ℓ} {A : Set ℓ} {x y : A} (p : x ≡ y) → trans p refl ≡ p
transExample refl = refl
