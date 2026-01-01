{-# OPTIONS --without-K #-}
module Infrastructure.Equality where

open import Agda.Primitive using (Level)
open import Agda.Builtin.Equality using (_≡_; refl)

-- Basic equality utilities kept minimal to avoid depending on the stdlib.

cong : ∀ {ℓ₁ ℓ₂} {A : Set ℓ₁} {B : Set ℓ₂} (f : A → B) {x y : A} → x ≡ y → f x ≡ f y
cong f refl = refl

sym : ∀ {ℓ} {A : Set ℓ} {x y : A} → x ≡ y → y ≡ x
sym refl = refl

trans : ∀ {ℓ} {A : Set ℓ} {x y z : A} → x ≡ y → y ≡ z → x ≡ z
trans refl refl = refl
