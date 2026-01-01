{-# OPTIONS --without-K #-}
module Examples.AmbiguityExamples where

open import Agda.Primitive using (Level; lzero)
open import Agda.Builtin.Nat using (Nat; _+_; zero)
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Equality using (_≡_; refl)

open import Plan.CIM.Ambiguity using (Ambiguity; WeightedOption; determinate; superposition; mapAmbiguity)

-- Simple determinate mapping preserves the underlying value.
mapDeterminate : (n : Nat) → mapAmbiguity (λ x → x + 1) (determinate n) ≡ determinate (n + 1)
mapDeterminate n = refl

-- Mapping over a superposition preserves weights and provenance.
sampleOption : WeightedOption Nat
sampleOption = record { value = 1 ; weight = 2 ; provenance = "example" }

mapSuperposition : mapAmbiguity (λ x → x + 1) (superposition (sampleOption ∷ []))
                 ≡ superposition (record { value = 2 ; weight = 2 ; provenance = "example" } ∷ [])
mapSuperposition = refl
