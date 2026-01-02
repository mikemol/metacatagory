{-# OPTIONS --without-K --cubical-compatible #-}

-- | Elasticity: separate tension (internal stress) from resonance (external alignment)
--   and classify edges into the four elasticity quadrants described in GP400.
module Plan.CIM.Elasticity where

open import Agda.Builtin.Nat using (Nat; zero; suc)
open import Agda.Builtin.Bool using (Bool; true; false)

-- | Addition on natural numbers.
_+_ : Nat → Nat → Nat
zero + n = n
(suc m) + n = suc (m + n)

-- | Multiplication on natural numbers.
_∗_ : Nat → Nat → Nat
zero ∗ _ = zero
(suc m) ∗ n = n + (m ∗ n)

-- | Simple comparison on natural numbers (t ≤? u).
leq : Nat → Nat → Bool
leq zero _ = true
leq (suc _) zero = false
leq (suc m) (suc n) = leq m n

-- | Signal carrying both tension (internal stress) and resonance (external alignment).
record ElasticSignal : Set where
  constructor elastic
  field
    tension   : Nat
    resonance : Nat

-- | Quadrant labels from the GP400 elasticity plane.
data ElasticQuadrant : Set where
  NonSequitur   : ElasticQuadrant  -- low tension, low resonance
  Cliche        : ElasticQuadrant  -- low tension, high resonance
  Hallucination : ElasticQuadrant  -- high tension, low resonance
  Insight       : ElasticQuadrant  -- high tension, high resonance

-- | Elastic gate configuration and classifier.
record ElasticGate : Set where
  field
    tensionThreshold   : Nat     -- boundary between low/high tension
    resonanceThreshold : Nat     -- base resonance requirement
    tensionScale       : Nat     -- how much required resonance grows with tension

  -- | Compute the required resonance given a signal's tension.
  requiredResonance : ElasticSignal → Nat
  requiredResonance sig = resonanceThreshold + (tensionScale ∗ ElasticSignal.tension sig)

  -- | Classify a signal into one of the four GP400 quadrants.
  classify : ElasticSignal → ElasticQuadrant
  classify sig with leq tensionThreshold (ElasticSignal.tension sig)
                     | leq (requiredResonance sig) (ElasticSignal.resonance sig)
  ... | false | false = NonSequitur
  ... | false | true  = Cliche
  ... | true  | false = Hallucination
  ... | true  | true  = Insight

  -- | Accept if the signal lands in an allowed quadrant (Cliché or Insight).
  accept : ElasticSignal → Bool
  accept sig with classify sig
  ... | Cliche  = true
  ... | Insight = true
  ... | _       = false

------------------------------------------------------------------------
-- Derived helpers
------------------------------------------------------------------------

-- | Convenience constructor for a default gate (unit scales).
defaultGate : ElasticGate
ElasticGate.tensionThreshold defaultGate = suc zero        -- 1
ElasticGate.resonanceThreshold defaultGate = suc zero      -- 1
ElasticGate.tensionScale defaultGate = suc zero            -- 1
