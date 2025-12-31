module Infrastructure.Wrapper.With where

open import Agda.Primitive using (Level; _⊔_)

module With {ℓA ℓB : Level} (A : Set ℓA) (B : Set ℓB) where
  record With : Set (ℓA ⊔ ℓB) where
    constructor _▸_
    field
      base : A
      ann  : B
