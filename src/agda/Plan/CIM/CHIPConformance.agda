{-# OPTIONS --without-K --cubical --guardedness #-}

-- | CHIP conformance checks and scaffolding.
module Plan.CIM.CHIPConformance where

open import Agda.Builtin.String
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Unit
open import Agda.Builtin.Nat using (Nat; _+_)
open import Agda.Builtin.Equality using (_≡_)
open import Agda.Primitive using (Level; lzero; lsuc)
open import Agda.Builtin.Sigma using (Σ; _,_)

-- [UPDATED] Imported PhaseAmbiguity and BraidedSPPF
open import Plan.CIM.Utility using (PhaseAmbiguity; TransformationSystem; EmergentMetric; Path; CoherenceWitness; BraidedInheritanceFunctor; BraidedSPPF; map; _×_; packed-node)

-- | Graded vector space with per-grade dimensions and an emergent metric.
record GradedVectorSpace (n : Nat) : Set₁ where
  field
    dimensions : List String
    metric : EmergentMetric

open import Agda.Builtin.Nat using (Nat)

-- | Map string labels within a graded vector space, preserving metric.
mapGVS : ∀ {n} {A : Set} → (String → String) → GradedVectorSpace n → GradedVectorSpace n
mapGVS f gvs = record
  { dimensions = map f (GradedVectorSpace.dimensions gvs)
  ; metric = GradedVectorSpace.metric gvs
  }

-- | Compose two braided inheritance functors, summing coherence costs.
composeBraids : ∀ {ℓ} {A B C : Set ℓ} → BraidedInheritanceFunctor A B → BraidedInheritanceFunctor B C → BraidedInheritanceFunctor A C
composeBraids bif1 bif2 = record
  { inheritanceBraid = λ { (a , c) → c , a }
  ; coherenceCost = record { magnitude = EmergentMetric.magnitude (BraidedInheritanceFunctor.coherenceCost bif1) + EmergentMetric.magnitude (BraidedInheritanceFunctor.coherenceCost bif2) }
  ; fromValue = BraidedInheritanceFunctor.fromValue bif1
  ; toValue   = BraidedInheritanceFunctor.toValue bif2
  ; description = "Composed braid"
  }

-- [IMPLEMENTED] SPPF node construction
-- | Construct a braided SPPF node for a given path and inheritance functor.
makeSPPFNode : ∀ {ℓ} {N : Set ℓ} {Sys : TransformationSystem N N}
             → Path Sys
             → BraidedInheritanceFunctor N N
             → BraidedSPPF N N Sys
makeSPPFNode p bif = packed-node p p bif

-- | Abstract adjunction-like coherence between two endofunctors.
record CoherenceMapUniversalProperty {ℓ} (F : Set ℓ → Set ℓ) (G : Set ℓ → Set ℓ) : Set (lsuc ℓ) where
  field
    adjunction : (A : Set ℓ) → (F (G A)) ≡ (G (F A))
