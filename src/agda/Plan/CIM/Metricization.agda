{-# OPTIONS --without-K --cubical-compatible #-}

-- | Metricization primitives factored out of Utility.
module Plan.CIM.Metricization where

open import Agda.Builtin.Nat using (Nat)
open import Agda.Builtin.Bool using (Bool)

-- | Simple metric wrapper for coherence costs.
record EmergentMetric : Set where
  field
    magnitude : Nat

-- | Cost function with a minimality predicate.
record CostFunction : Set where
  field
    cost : Nat → Nat
    minimal : Nat → Bool

-- | Check whether a metric value satisfies the minimality predicate.
metricMinimality : CostFunction → Nat → Bool
metricMinimality cf m = CostFunction.minimal cf m
