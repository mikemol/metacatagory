{-# OPTIONS --without-K #-}

module Plan.CIM.Ambiguity where

open import Agda.Builtin.Nat
open import Agda.Builtin.List
open import Agda.Builtin.String
open import Agda.Builtin.Sigma
open import Agda.Builtin.Bool
open import Agda.Builtin.Maybe

-- Core Definition: Ambiguity is not just a list, but a weighted superposition
-- mirroring the "superposition" concept in GP104.
record WeightedOption {ℓ} (A : Set ℓ) : Set ℓ where
  field
    value : A
    weight : Nat -- Discretized probability/confidence (0-100)
    provenance : String

-- Helper for list mapping (defined early to avoid scope issues)
map : ∀ {ℓ} {A B : Set ℓ} → (A → B) → List A → List B
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs

-- The Ambiguity Monad Structure
data Ambiguity {ℓ} (A : Set ℓ) : Set ℓ where
  determinate : A → Ambiguity A
  superposition : List (WeightedOption A) → Ambiguity A
  conflict : String → Ambiguity A -- Explicit failure state for "Tension" overflow

-- Functorial Mapping (map)
mapAmbiguity : ∀ {ℓ} {A B : Set ℓ} → (A → B) → Ambiguity A → Ambiguity B
mapAmbiguity f (determinate x) = determinate (f x)
mapAmbiguity f (superposition xs) = superposition (map (λ w → record { value = f (WeightedOption.value w) ; weight = WeightedOption.weight w ; provenance = WeightedOption.provenance w }) xs)
mapAmbiguity f (conflict s) = conflict s

-- "Collapse" function (Observation)
-- Selects the highest weighted option, resolving the ambiguity.
-- Returns Maybe to handle error cases safely.
collapseToMaybe : ∀ {ℓ} {A : Set ℓ} → Ambiguity A → Maybe A
collapseToMaybe (determinate x) = just x
collapseToMaybe (conflict s) = nothing
collapseToMaybe (superposition []) = nothing
collapseToMaybe (superposition (x ∷ xs)) = just (WeightedOption.value x) -- Naive greedy selection

-- Measurement of Tension (Ambiguity Magnitude)
ambiguityTension : ∀ {ℓ} {A : Set ℓ} → Ambiguity A → Nat
ambiguityTension (determinate _) = 0
ambiguityTension (conflict _) = 100
ambiguityTension (superposition []) = 100
ambiguityTension (superposition (x ∷ [])) = 0
ambiguityTension (superposition (x ∷ y ∷ xs)) = 50 -- Simplified metric: >1 option = tension
