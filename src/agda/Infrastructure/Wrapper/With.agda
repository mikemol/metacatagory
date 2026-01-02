-- | Simple wrapper to pair a base value with an annotation.
module Infrastructure.Wrapper.With where

open import Agda.Primitive using (Level; _⊔_)

module With {ℓA ℓB : Level} (A : Set ℓA) (B : Set ℓB) where
  -- | Annotated value pairing a base payload with auxiliary data.
  record With : Set (ℓA ⊔ ℓB) where
    constructor _▸_
    field
      base : A
      ann  : B
