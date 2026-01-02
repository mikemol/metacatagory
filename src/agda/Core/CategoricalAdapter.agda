{-# OPTIONS --without-K #-}

-- | Universal categorical interface for adapters.
module Core.CategoricalAdapter where

-- Core.CategoricalAdapter: Universal categorical interface for adapters


-- Infrastructure imports for universe polymorphism and equality
open import Infrastructure.Universe using (Setℓ)
open import Infrastructure.Coherence.Path2 using (_≡_; refl; whisker; _∙₂_)

open import Agda.Primitive using (Level; lsuc)
open import Agda.Builtin.Unit using (⊤; tt)
open import Agda.Builtin.Equality using (_≡_; refl)

-- The categorical triad for any type T:
--   - object: T
--   - morphism: ⊤ → T (inhabitant)
--   - hom-set: Set of all morphisms ⊤ → T (i.e., T itself)

-- | Package any carrier as a trivial category with a chosen morphism.
record CategoricalAdapter {ℓ : Level} (T : Set ℓ) : Set (lsuc ℓ) where
  -- | Object/morphism view of the carrier.
  field
    object : Set ℓ
    morphism : ⊤ → T
    homSet : Set ℓ
    isomorphism : homSet ≡ T

-- | Build the trivial adapter given a carrier and chosen inhabitant.
--   Useful when wrapping plain sets into CategoryLike slots.
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
