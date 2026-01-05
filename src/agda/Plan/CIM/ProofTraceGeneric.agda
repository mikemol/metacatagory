{-# OPTIONS --without-K #-}

-- | Generic Duality for Proof Traces: elaboration ↔ reconstruction
module Plan.CIM.ProofTraceGeneric where

open import Agda.Primitive using (Level; _⊔_)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Infrastructure.Universe using (Setℓ)
open import Infrastructure.Adequacy using 
  ( DualityInterface
  ; ProvidedDirection
  )

private
  variable
    ℓ : Level

------------------------------------------------------------------------
-- Proof Traces as Elaboration Duality
------------------------------------------------------------------------

-- | High-level proof term (source of elaboration)
postulate ProofTerm : Set

-- | Low-level trace steps (target of elaboration)
postulate TraceStep : Set

-- | Combined trace state
data TraceState : Set where
  term : ProofTerm → TraceState
  steps : TraceStep → TraceState

------------------------------------------------------------------------
-- Proof Trace Duality Interface
------------------------------------------------------------------------

-- | Forward: elaborate proof term into trace steps
postulate
  proof-forward : ProofTerm → TraceStep

-- | Backward: reconstruct proof term from trace steps
postulate
  proof-backward : TraceStep → ProofTerm

-- | Coverage: term ≡ reconstruct (elaborate term)
postulate
  proof-fwd-coverage : ∀ (pt : ProofTerm) → proof-backward (proof-forward pt) ≡ pt

-- | Coverage: trace ≡ elaborate (reconstruct trace)
postulate
  proof-bwd-coverage : ∀ (ts : TraceStep) → proof-forward (proof-backward ts) ≡ ts

------------------------------------------------------------------------
-- Duality Interface Instantiation
------------------------------------------------------------------------

proof-trace-duality-interface : DualityInterface
proof-trace-duality-interface = record
  { StateA = ProofTerm
  ; StateB = TraceStep
  ; State = TraceState
  ; inj-A = term
  ; inj-B = steps
  ; direction = ProvidedDirection.Forward
  ; forward = proof-forward
  ; backward = proof-backward
  ; coverage-fwd-roundtrip = proof-fwd-coverage
  ; coverage-bwd-roundtrip = proof-bwd-coverage
  }

------------------------------------------------------------------------
-- Generic Proof Trace Path Algebra (derived from framework)
------------------------------------------------------------------------

open module ProofTracePaths = Infrastructure.Adequacy.GenericDualPaths proof-trace-duality-interface
open module ProofTraceAlgebra = Infrastructure.Adequacy.GenericDualAlgebra proof-trace-duality-interface
open module ProofTraceAdequacy = Infrastructure.Adequacy.GenericDualAdequacy proof-trace-duality-interface

------------------------------------------------------------------------
-- Exported Adequacy Witness
------------------------------------------------------------------------

-- | Proof trace path: term ↔ steps
ProofTracePath : TraceState → TraceState → Set
ProofTracePath s₁ s₂ = ProofTracePaths.DualPath s₁ s₂

-- | Elaboration adequacy: proof term and trace are dual
postulate
  proof-trace-adequate : ∀ (pt : ProofTerm) →
    let ts = proof-forward pt in
    let pt' = proof-backward ts in
    pt ≡ pt'

-- | Reconstruction adequacy: trace steps and proof term are dual
postulate
  trace-reconstruction-adequate : ∀ (ts : TraceStep) →
    let pt = proof-backward ts in
    let ts' = proof-forward pt in
    ts ≡ ts'
