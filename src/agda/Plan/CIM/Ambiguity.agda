{-# OPTIONS --without-K #-}

module Plan.CIM.Ambiguity where

open import Agda.Builtin.Nat
open import Agda.Builtin.List
open import Agda.Builtin.String
open import Agda.Builtin.Sigma
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

-- Functorial Mapping
mapAmbiguity : ∀ {ℓ} {A B : Set ℓ} → (A → B) → Ambiguity A → Ambiguity B
mapAmbiguity {ℓ} f (determinate x) = determinate (f x)
mapAmbiguity {ℓ} f (superposition xs) = superposition (mapOption f xs)
  where
    mapOption : ∀ {A B : Set ℓ} → (A → B) → List (WeightedOption A) → List (WeightedOption B)
    mapOption g [] = []
    mapOption g (rec ∷ rs) = record { value = g (WeightedOption.value rec) ; weight = WeightedOption.weight rec ; provenance = WeightedOption.provenance rec } ∷ mapOption g rs
mapAmbiguity {ℓ} f (conflict s) = conflict s

-- [RIGOR] Postulated Functor Laws
postulate
  functor-identity : ∀ {ℓ} {A : Set ℓ} (m : Ambiguity A) → mapAmbiguity (λ x → x) m ≡ m
  functor-composition : ∀ {ℓ} {A B C : Set ℓ} (f : A → B) (g : B → C) (m : Ambiguity A) → 
                        mapAmbiguity (λ x → g (f x)) m ≡ mapAmbiguity g (mapAmbiguity f m)

-- [FFI] External hook for tension calculation (nedge_topology/mitosis.py)
postulate
  externalTensionCalc : ∀ {ℓ} {A : Set ℓ} → List (WeightedOption A) → Nat

ambiguityTension : ∀ {ℓ} {A : Set ℓ} → Ambiguity A → Nat
ambiguityTension (determinate _) = 0
ambiguityTension (conflict _) = 100
ambiguityTension (superposition xs) = externalTensionCalc xs
