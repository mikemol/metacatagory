{-# OPTIONS --without-K --cubical-compatible #-}

-- | Transformation system primitives factored out of Utility.
module Plan.CIM.TransformationSystem where

open import Agda.Primitive using (Level; lsuc)
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.String using (String)
open import Agda.Builtin.Nat using (Nat)
open import Agda.Builtin.Sigma using (Σ; _,_)
open import Plan.CIM.Metricization using (EmergentMetric)

_×_ : ∀ {ℓ} → Set ℓ → Set ℓ → Set ℓ
A × B = Σ A (λ _ → B)

-- | Pair of values with an associated phase indicating ambiguity.
record PhaseAmbiguity {ℓ} (A B : Set ℓ) : Set ℓ where
    field
        valA : A
        valB : B
        phase : Nat

-- | System describing steps and costs between phases.
record TransformationSystem {ℓ} (A B : Set ℓ) : Set (lsuc ℓ) where
    field
        Step : Set ℓ
        cost : Step → Nat

-- | Path of transformation steps.
data Path {ℓ} {A B : Set ℓ} (Sys : TransformationSystem {ℓ} A B) : Set ℓ where
    refl-path : Path Sys
    trans-step : (s : TransformationSystem.Step Sys) → (rest : Path Sys) → Path Sys

-- | Witness of coherence with an explicit proof path and metric.
record CoherenceWitness {ℓ} {A B : Set ℓ} (amb : PhaseAmbiguity {ℓ} A B) (Sys : TransformationSystem {ℓ} A B) : Set (lsuc ℓ) where
    field
        proofPath : Path Sys
        metric    : EmergentMetric

------------------------------------------------------------------------
-- AST-Dependent Types
------------------------------------------------------------------------

module ASTDependent (Block MdBlock BraidStep : Set) where
  -- | Trace of braid steps with summary.
  record BraidTrace : Set where
    field
        steps : List BraidStep
        summary : String

  -- | Protocol specialized to block-level transformations.
  record BlockProtocol : Set₁ where
    field
        ambiguity : PhaseAmbiguity Block MdBlock
        transSys  : TransformationSystem Block MdBlock
        coherence : CoherenceWitness ambiguity transSys
        metric    : EmergentMetric

  -- | Protocol specialized to document-level transformations.
  record DocProtocol : Set₁ where
    field
        ambiguity : PhaseAmbiguity String String
        transSys  : TransformationSystem String String
        coherence : CoherenceWitness ambiguity transSys
        metric    : EmergentMetric
