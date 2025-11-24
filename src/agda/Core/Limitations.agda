module Core.Limitations where

open import Metamodel as M
open import Agda.Builtin.String using (String)
open import Agda.Builtin.List   using (List; []; _∷_)
open import Core.Phase using (Bool; true; false)
open import Agda.Builtin.Unit   using (⊤; tt)
open import Agda.Builtin.Maybe  using (Maybe; just; nothing)

-- Error-as-Specification: capture constructive limitations tied to an algorithm identifier
record LimitationEvidence : Set where
  constructor mkLimit
  field
    algorithmId : M.Identifier
    message     : String
    assumptions : List String
    acknowledged : Bool

-- Minimal outcome wrapper to model success vs limitation-as-specification
data Outcome : Set where
  ok    : Outcome
  limit : LimitationEvidence → Outcome

-- Helper to acknowledge a limitation (treat as intentionally specified boundary)
acknowledge : LimitationEvidence → LimitationEvidence
acknowledge le = record le { acknowledged = true }

-- Convenience builder
mkLimitation : M.Identifier → String → List String → LimitationEvidence
mkLimitation alg msg asm = mkLimit alg msg asm false

-- Algorithm result wrapper with optional limitation evidence
record AlgorithmResult (A : Set) : Set where
  constructor mkResult
  field
    value      : A
    limitation : Maybe LimitationEvidence

-- Convenience constructors
okResult : ∀ {A} → A → AlgorithmResult A
okResult v = mkResult v nothing

limitedResult : ∀ {A} → A → LimitationEvidence → AlgorithmResult A
limitedResult v lim = mkResult v (just lim)

-- Extract outcome from result
toOutcome : ∀ {A} → AlgorithmResult A → Outcome
toOutcome (mkResult _ nothing) = ok
toOutcome (mkResult _ (just lim)) = limit lim
