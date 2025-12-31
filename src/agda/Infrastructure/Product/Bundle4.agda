module Infrastructure.Product.Bundle4
  {ℓ₁ ℓ₂ ℓ₃ ℓ₄ : Agda.Primitive.Level}
  (A : Set ℓ₁) (B : Set ℓ₂) (C : Set ℓ₃) (D : Set ℓ₄)
  where

open import Agda.Primitive using (_⊔_)

record Bundle₄ : Set (ℓ₁ ⊔ ℓ₂ ⊔ ℓ₃ ⊔ ℓ₄) where
  constructor ⟨_,_,_,_⟩
  field
    a : A
    b : B
    c : C
    d : D
