{-# OPTIONS --without-K --cubical-compatible #-}

-- | Metricization primitives factored out of Utility.
module Plan.CIM.Metricization where

open import Agda.Builtin.Nat using (Nat; zero; suc; _+_)
open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.Maybe using (Maybe; just; nothing)

-- | Evidence that a natural number is non-zero.
data NonZero : Nat → Set where
  nz : ∀ {n} → NonZero (suc n)

nonZero? : Nat → Bool
nonZero? zero    = false
nonZero? (suc _) = true

-- | Simple metric wrapper for coherence costs.
--   The validity field captures the obligation that magnitude is non-zero.
--   Systolic area tracks the divergence between the two boundary paths.
--   When magnitude is zero, validity is left as nothing to surface debt.
record EmergentMetric : Set where
  constructor mkEmergentMetric
  field
    magnitude    : Nat
    complexity   : Nat
    systolicArea : Nat
    validity     : Maybe (NonZero magnitude)

-- | Absolute difference between two path lengths (divergence).
pathDivergence : Nat → Nat → Nat
pathDivergence zero    n = n
pathDivergence (suc m) zero    = suc m
pathDivergence (suc m) (suc n) = pathDivergence m n

-- | Systolic area as the sum of both paths plus their divergence.
systolicArea : Nat → Nat → Nat
systolicArea l r = (l + r) + pathDivergence l r

-- | Helper to build a metric while automatically threading validity and systolic area.
mkMetric : Nat → Nat → EmergentMetric
mkMetric m c with m
... | zero    = mkEmergentMetric m c (systolicArea m c) nothing
... | suc n   = mkEmergentMetric (suc n) c (systolicArea (suc n) c) (just nz)

-- | Cost function with a minimality predicate.
record CostFunction : Set where
  field
    cost : Nat → Nat
    minimal : Nat → Bool

-- | Check whether a metric value satisfies the minimality predicate.
metricMinimality : CostFunction → Nat → Bool
metricMinimality cf m = CostFunction.minimal cf m
