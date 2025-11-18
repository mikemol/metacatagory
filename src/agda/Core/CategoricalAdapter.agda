-- Core.CategoricalAdapter: Universal categorical interface for adapters

module Core.CategoricalAdapter where

open import Agda.Primitive using (Level; lsuc)
open import Agda.Builtin.Unit using (⊤; tt)
open import Agda.Builtin.Equality using (_≡_; refl)

-- The categorical triad for any type T:
--   - object: T
--   - morphism: ⊤ → T (inhabitant)
--   - hom-set: Set of all morphisms ⊤ → T (i.e., T itself)

record CategoricalAdapter {ℓ : Level} (T : Set ℓ) : Set (lsuc ℓ) where
  field
    object : Set ℓ
    morphism : ⊤ → T
    homSet : Set ℓ
    isomorphism : homSet ≡ T

mkCategoricalAdapter : ∀ {ℓ} (T : Set ℓ) → (f : ⊤ → T) → CategoricalAdapter T
mkCategoricalAdapter T f =
  record { object = T
         ; morphism = f
         ; homSet = T
         ; isomorphism = refl }

-- Example usage:
-- For Bool:
--   mkCategoricalAdapter Bool (λ _ → true)
-- For ℕ:
--   mkCategoricalAdapter ℕ (λ _ → 0)
