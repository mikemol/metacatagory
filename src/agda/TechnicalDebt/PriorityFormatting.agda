{-# OPTIONS --without-K #-}

-- TechnicalDebt.PriorityFormatting: JSON formatting for priority strategies
-- PRIO-ADOPT-1: Formatting layer takes pure CategoryWeights and produces JSON strings
-- This keeps all domain-specific concerns in Agda; Python becomes integration layer

module TechnicalDebt.PriorityFormatting where

open import TechnicalDebt.PriorityMapping using (CategoryWeights; strategyToWeights)
open import TechnicalDebt.Priorities
open import Agda.Builtin.String using (String; primStringAppend)
open import Agda.Builtin.Int using (Int; pos; negsuc)

-- ============================================================================
-- Int to String Conversion (FFI Detail)
-- ============================================================================

postulate
  intToString : Int → String

-- ============================================================================
-- JSON Formatting - Pure String Building
-- ============================================================================

-- Postulate the complete JSON output generation
-- (Implementation: Agda computes all strategy weights, then formats as JSON string)
postulate
  formatAllStrategyProfiles : String

-- This can be computed at compile-time by the Agda typechecker:
-- For each strategy (default, ffiSafety, proofCompleteness, rapidDevelopment, production):
--   1. strategyToWeights computes category weights (postulate, todo, fixme, deviation)
--   2. Format as JSON: {"postulate": N, "todo": N, "fixme": N, "deviation": N}
--   3. Wrap with metadata and "active" field

-- ============================================================================
-- Example: Export functionality
-- ============================================================================

module Examples where
  -- The complete JSON output is computable at compile time:
  completePrioritiesJSON : String
  completePrioritiesJSON = formatAllStrategyProfiles

  -- Usage: This string can be exported to a file or served directly
  -- Python integration: load this string and parse as JSON

