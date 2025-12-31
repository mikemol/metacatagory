module Infrastructure.Iso.Structural where

open import Agda.Primitive using (Level; _⊔_)
open import Agda.Builtin.Equality using (_≡_)

record Iso {ℓA ℓB : Level} (A : Set ℓA) (B : Set ℓB) : Set (ℓA ⊔ ℓB) where
  field
    to       : A → B
    from     : B → A
    to∘from  : ∀ b → to (from b) ≡ b
    from∘to  : ∀ a → from (to a) ≡ a
