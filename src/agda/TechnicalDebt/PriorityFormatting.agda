{-# OPTIONS --without-K #-}

-- TechnicalDebt.PriorityFormatting: JSON formatting for priority strategies
-- PRIO-ADOPT-1: Formatting layer takes pure CategoryWeights and produces JSON strings
-- This keeps all domain-specific concerns in Agda; Python becomes integration layer
--
-- Uses module parameterization for string operations (not postulates)

open import Agda.Builtin.String using (String; primStringAppend)
open import Agda.Builtin.Int using (Int; pos; negsuc)

module TechnicalDebt.PriorityFormatting
  -- String operations (to be provided by FFI implementation)
  (intToString : Int → String)
  (formatAllStrategyProfiles : String)
  where

open import TechnicalDebt.PriorityMapping using (CategoryWeights; strategyToWeights)
open import TechnicalDebt.Priorities

-- ============================================================================
-- JSON Formatting - Using Parameters
-- ============================================================================

-- String concatenation helper
_++_ : String → String → String
_++_ = primStringAppend

infixr 5 _++_

-- Helper: Format a single strategy's weights as JSON
formatStrategy : PriorityStrategy → String
formatStrategy strategy =
  let weights = strategyToWeights strategy
  in "{\"weights\": "
     ++ "{\"postulate\": " ++ intToString (CategoryWeights.postulateWeight weights)
     ++ ", \"todo\": " ++ intToString (CategoryWeights.todoWeight weights)
     ++ ", \"fixme\": " ++ intToString (CategoryWeights.fixmeWeight weights)
     ++ ", \"deviation\": " ++ intToString (CategoryWeights.deviationWeight weights)
     ++ "}}"

-- The complete JSON is provided as a parameter
-- (Allows FFI implementation to provide fully formatted output)
getAllStrategyProfiles : String
getAllStrategyProfiles = formatAllStrategyProfiles

-- ============================================================================
-- Example: Export functionality
-- ============================================================================

module Examples where
  -- The complete JSON output is provided via parameter:
  completePrioritiesJSON : String
  completePrioritiesJSON = formatAllStrategyProfiles

  -- Usage: This string can be exported to a file or served directly
  -- Python integration: load this string and parse as JSON

-- Note: This module is parameterized. To use it, instantiate with:
--   1. intToString implementation (Int → String conversion)
--   2. formatAllStrategyProfiles implementation (complete JSON string)
--
-- Example instantiation in another module:
--   open import TechnicalDebt.PriorityFormatting
--     myIntToString
--     myFormatAllStrategyProfiles
