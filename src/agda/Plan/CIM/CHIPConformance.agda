{-# OPTIONS --without-K #-}

-- | CHIP conformance checks and scaffolding.
module Plan.CIM.CHIPConformance where

open import Agda.Builtin.String
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Unit
open import Agda.Builtin.Nat using (Nat; _+_)
open import Agda.Builtin.Equality using (_≡_)
open import Agda.Primitive using (Level; lzero; lsuc)
open import Agda.Builtin.Sigma using (Σ; _,_)

open import Plan.CIM.Utility using (PhaseAmbiguity; TransformationSystem; Path; map; _×_; _++_)
open import Plan.CIM.FunctorialConstructs using (EmergentMetric; mkMetric; CoherenceWitness; BraidedInheritanceFunctor; BraidedSPPF; packed-node)

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

-- | Combine metrics by summing magnitude and complexity.
sumMetric : EmergentMetric → EmergentMetric → EmergentMetric
sumMetric m1 m2 =
  mkMetric (EmergentMetric.magnitude m1 + EmergentMetric.magnitude m2)
           (EmergentMetric.complexity m1 + EmergentMetric.complexity m2)

-- | Compose two braided inheritance functors, summing coherence costs.
composeBraids : ∀ {ℓ} {A B C : Set ℓ} → BraidedInheritanceFunctor A B → BraidedInheritanceFunctor B C → BraidedInheritanceFunctor A C
composeBraids bif1 bif2 = record
  { inheritanceBraid = λ { (a , c) →
      let
        b , a' = BraidedInheritanceFunctor.inheritanceBraid bif1 (a , BraidedInheritanceFunctor.toValue bif1)
        c' , _ = BraidedInheritanceFunctor.inheritanceBraid bif2 (b , c)
      in
        c' , a'
    }
  ; coherenceCost = sumMetric (BraidedInheritanceFunctor.coherenceCost bif1)
                              (BraidedInheritanceFunctor.coherenceCost bif2)
  ; fromValue = BraidedInheritanceFunctor.fromValue bif1
  ; toValue   = BraidedInheritanceFunctor.toValue bif2
  ; description =
      BraidedInheritanceFunctor.description bif1 ++ " ∘ " ++
      BraidedInheritanceFunctor.description bif2
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
