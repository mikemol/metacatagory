{-# OPTIONS --without-K #-}

-- Tests.CHIPConformanceChecklist: sanity checks for braid composition.
module Tests.CHIPConformanceChecklist where

open import Agda.Builtin.Nat using (Nat; _+_)
open import Agda.Builtin.String using (String)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Agda.Builtin.Sigma using (Σ; _,_)

open import Plan.CIM.Utility using (_×_; _++_; mkMetric)
open import Plan.CIM.Metricization using (EmergentMetric)
open import Plan.CIM.FunctorialConstructs using (BraidedInheritanceFunctor)
open import Plan.CIM.CHIPConformance using (composeBraids)

open EmergentMetric

swapNat : Nat × Nat → Nat × Nat
swapNat (a , b) = b , a

-- Simple braided functors for test composition.
bif1 : BraidedInheritanceFunctor Nat Nat
bif1 = record
  { inheritanceBraid = swapNat
  ; coherenceCost = mkMetric 1 2
  ; fromValue = 0
  ; toValue = 1
  ; description = "f1"
  }

bif2 : BraidedInheritanceFunctor Nat Nat
bif2 = record
  { inheritanceBraid = swapNat
  ; coherenceCost = mkMetric 2 3
  ; fromValue = 1
  ; toValue = 2
  ; description = "f2"
  }

composed : BraidedInheritanceFunctor Nat Nat
composed = composeBraids bif1 bif2

-- Metric aggregation should sum magnitude and complexity.
test-magnitude : magnitude (BraidedInheritanceFunctor.coherenceCost composed) ≡ 3
test-magnitude = refl

test-complexity : complexity (BraidedInheritanceFunctor.coherenceCost composed) ≡ 5
test-complexity = refl

-- Description should record composition order.
test-description : BraidedInheritanceFunctor.description composed ≡ "f1 ∘ f2"
test-description = refl
