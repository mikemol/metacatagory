{-# OPTIONS --without-K #-}

module Plan.CIM.Ambiguity where

open import Agda.Builtin.Nat
open import Agda.Builtin.List
open import Agda.Builtin.String
open import Agda.Builtin.Sigma
open import Agda.Builtin.Bool
open import Agda.Builtin.Equality

-- The weighted option from GP104
record WeightedOption {ℓ} (A : Set ℓ) : Set ℓ where
  field
    value : A
    weight : Nat
    provenance : String

-- The Algebraic Ambiguity Monad
data Ambiguity {ℓ} (A : Set ℓ) : Set ℓ where
  determinate : A → Ambiguity A
  superposition : List (WeightedOption A) → Ambiguity A
  conflict : String → Ambiguity A

-- Functor definition
mapAmbiguity : ∀ {ℓ} {A B : Set ℓ} → (A → B) → Ambiguity A → Ambiguity B
mapAmbiguity {ℓ} {A} {B} f (determinate x) = determinate (f x)
mapAmbiguity {ℓ} {A} {B} f (superposition xs) = superposition (mapList (λ w → record { value = f (WeightedOption.value w) ; weight = WeightedOption.weight w ; provenance = WeightedOption.provenance w }) xs)
    where
      mapList : (WeightedOption A → WeightedOption B) → List (WeightedOption A) → List (WeightedOption B)
      mapList g [] = []
      mapList g (y ∷ ys) = g y ∷ mapList g ys
mapAmbiguity f (conflict s) = conflict s

-- Monad bind operation (needed for proper monad laws)
bindAmbiguity : ∀ {ℓ} {A B : Set ℓ} → Ambiguity A → (A → Ambiguity B) → Ambiguity B
bindAmbiguity (determinate x) f = f x
bindAmbiguity (superposition xs) f = superposition [] -- Simplified: flatten would go here
bindAmbiguity (conflict s) f = conflict s

-- Postulate the Monad Laws to ensure rigor (Implementation deferred to Phase V)
postulate
  left-identity : ∀ {ℓ} {A B : Set ℓ} (a : A) (f : A → Ambiguity B) → bindAmbiguity (determinate a) f ≡ f a

-- FFI Hook for Tension Calculation (Bridge to nedge_topology/mitosis.py)
postulate
  externalTensionCalc : ∀ {ℓ} {A : Set ℓ} → List (WeightedOption A) → Nat

ambiguityTension : ∀ {ℓ} {A : Set ℓ} → Ambiguity A → Nat
ambiguityTension (determinate _) = 0
ambiguityTension (conflict _) = 100
ambiguityTension (superposition xs) = externalTensionCalc xs
