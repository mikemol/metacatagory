{-# OPTIONS --without-K #-}

-- TechnicalDebt.PriorityMapping: Pure logic for mapping PriorityStrategy to simplified weights
-- PRIO-ADOPT-1: Domain logic only (no formatting) - Python handles JSON serialization

module TechnicalDebt.PriorityMapping where

open import TechnicalDebt.Priorities
open import Core.TechnicalDebt using (Priority; weight)
open import Core using (_×_; _,_)
open _×_ public
open import Agda.Builtin.Int using (Int; pos; negsuc)

-- ============================================================================
-- Extract numeric weight from Priority (first term's value)
-- ============================================================================

-- Extract numeric weight from Priority using the Core.TechnicalDebt weight function
extractPriorityWeight : Priority → Int
extractPriorityWeight p = pos (weight p)

-- ============================================================================
-- Map PriorityStrategy to category weights
-- ============================================================================

-- The Python badge system uses simplified categories. We map as follows:
-- - postulate: represents code scaffolding → use proof obligations priority
-- - todo: represents planned work → use documentation priority
-- - fixme: represents bugs/issues → use safety priority
-- - deviation: represents spec deviations → use critical priority

record CategoryWeights : Set where
  field
    postulateWeight : Int    -- from proof field
    todoWeight : Int         -- from documentation field
    fixmeWeight : Int        -- from safety field
    deviationWeight : Int    -- from critical field

-- Pure mapping function: extract and remap priorities
strategyToWeights : PriorityStrategy → CategoryWeights
strategyToWeights strat = record
  { postulateWeight = extractPriorityWeight (PriorityStrategy.proof strat)
  ; todoWeight = extractPriorityWeight (PriorityStrategy.documentation strat)
  ; fixmeWeight = extractPriorityWeight (PriorityStrategy.safety strat)
  ; deviationWeight = extractPriorityWeight (PriorityStrategy.critical strat)
  }

-- ============================================================================
-- Validation Examples (demonstrate pure logic)
-- ============================================================================

module Examples where
  open TechnicalDebt.Priorities

  -- All strategies' mapped weights are computable at compile time
  defaultWeights : CategoryWeights
  defaultWeights = strategyToWeights defaultStrategy

  ffiWeights : CategoryWeights
  ffiWeights = strategyToWeights ffiSafetyStrategy

  proofWeights : CategoryWeights
  proofWeights = strategyToWeights proofCompletenessStrategy

  rapidWeights : CategoryWeights
  rapidWeights = strategyToWeights rapidDevelopmentStrategy

  prodWeights : CategoryWeights
  prodWeights = strategyToWeights productionStrategy

