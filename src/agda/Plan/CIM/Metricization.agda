{-# OPTIONS --without-K --cubical-compatible #-}

-- | Metricization primitives factored out of Utility.
module Plan.CIM.Metricization where

open import Agda.Builtin.Nat using (Nat; zero; suc)
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
--   When magnitude is zero, validity is left as nothing to surface debt.
record EmergentMetric : Set where
  constructor mkEmergentMetric
  field
    magnitude  : Nat
    complexity : Nat
    validity   : Maybe (NonZero magnitude)

-- | Helper to build a metric while automatically threading validity.
mkMetric : Nat → Nat → EmergentMetric
mkMetric m c with m
... | zero    = mkEmergentMetric m c nothing
... | suc n   = mkEmergentMetric (suc n) c (just nz)

-- | Cost function with a minimality predicate.
record CostFunction : Set where
  field
    cost : Nat → Nat
    minimal : Nat → Bool

-- | Check whether a metric value satisfies the minimality predicate.
metricMinimality : CostFunction → Nat → Bool
metricMinimality cf m = CostFunction.minimal cf m
