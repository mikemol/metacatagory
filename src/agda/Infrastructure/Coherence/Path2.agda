{-# OPTIONS --without-K #-}

-- | Minimal 2-cell calculus: equality + whiskering.
module Infrastructure.Coherence.Path2 where

open import Agda.Primitive using (Level)
open import Agda.Builtin.Equality using (_≡_; refl)

-- Minimal 2-cell calculus: equality + whiskering (cong not exported in this Agda version)

whisker : ∀ {ℓA ℓB : Level} {A : Set ℓA} {B : Set ℓB}
        → (f : A → B) {x y : A} → x ≡ y → f x ≡ f y
whisker f refl = refl

-- Vertical composition is trans; we expose it so callers can stay proof-term small.
_∙₂_ : ∀ {ℓ : Level} {A : Set ℓ} {x y z : A}
     → x ≡ y → y ≡ z → x ≡ z
refl ∙₂ q = q

-- Re-export equality primitives for convenience in downstream modules.
-- This silences import warnings when modules open Path2 expecting _≡_ and refl.
open Agda.Builtin.Equality public using (_≡_; refl)
