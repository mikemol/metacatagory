{-# OPTIONS --without-K #-}
module Infrastructure.Universe where

open import Agda.Primitive using (Level; lzero; lsuc; _⊔_)

-- Universe-polymorphic alias
Setℓ : (ℓ : Level) → Set (lsuc ℓ)
Setℓ ℓ = Set ℓ
