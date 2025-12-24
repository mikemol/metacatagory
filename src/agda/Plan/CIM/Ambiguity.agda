{-# OPTIONS --without-K --safe #-}

module Plan.CIM.Ambiguity 
  -- [PARAMETER] FFI Oracle for Tension Calculation
  -- This forces the consumer to provide the connection to nedge_topology/mitosis.py
  (TensionOracle : ∀ {ℓ} {A : Set ℓ} → 
                   (options : List (Record {ℓ} {A})) → -- Defined below as WeightedOption
                   Nat)
  
  -- [PARAMETER] Rigorous Law Requirements
  -- The consumer must prove (or explicitly assume) the Monad laws
  (LeftIdentityProof : ∀ {ℓ} {A B : Set ℓ} (a : A) (f : A → Data {ℓ} B) → 
                       Map {ℓ} {A} {B} f (determinate a) ≡ f a)
  where

open import Agda.Builtin.Nat
open import Agda.Builtin.List
open import Agda.Builtin.String
open import Agda.Builtin.Sigma
open import Agda.Builtin.Equality

-- Definition moved inside (or we could parameterize the type itself, 
-- but the type definition is usually static).

record WeightedOption {ℓ} (A : Set ℓ) : Set ℓ where
  field
    value : A
    weight : Nat
    provenance : String

-- Helpers for parameter signature matching
Record = WeightedOption

data Ambiguity {ℓ} (A : Set ℓ) : Set ℓ where
  determinate : A → Ambiguity A
  superposition : List (WeightedOption A) → Ambiguity A
  conflict : String → Ambiguity A

-- Helper for signature matching
Data = Ambiguity

-- Functor Implementation
mapAmbiguity : ∀ {ℓ} {A B : Set ℓ} → (A → B) → Ambiguity A → Ambiguity B
mapAmbiguity f (determinate x) = determinate (f x)
mapAmbiguity f (superposition xs) = superposition (mapOption f xs)
  where
    mapOption : (A → B) → List (WeightedOption A) → List (WeightedOption B)
    mapOption g [] = []
    mapOption g (rec ∷ rs) = record { value = g (WeightedOption.value rec) ; weight = WeightedOption.weight rec ; provenance = WeightedOption.provenance rec } ∷ mapOption g rs
mapAmbiguity f (conflict s) = conflict s

-- Helper for signature matching
Map = mapAmbiguity

-- [USAGE] using the Parameterized Oracle
-- We no longer postulate `externalTensionCalc`; we use the module parameter.
ambiguityTension : ∀ {ℓ} {A : Set ℓ} → Ambiguity A → Nat
ambiguityTension (determinate _) = 0
ambiguityTension (conflict _) = 100
ambiguityTension (superposition xs) = TensionOracle xs

-- [USAGE] The Law is now a parameter `LeftIdentityProof` available in the context
-- No code change needed here, but proofs downstream can use `LeftIdentityProof`.
