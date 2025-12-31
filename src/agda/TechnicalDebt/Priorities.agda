{-# OPTIONS --without-K #-}

-- TechnicalDebt.Priorities: Parameterized priority weighting strategies
-- PHASE-II.2: Extracted from Core.TechnicalDebt (lines 30-33)
--
-- This module enables different projects to define "criticality" differently
-- (e.g., prioritizing FFI safety over proof completeness).

module TechnicalDebt.Priorities where

open import Core.TechnicalDebt using (Priority; mkPriority)
open import Core using (_×_; _,_)
open _×_ public
open import Agda.Builtin.String using (String)
open import Agda.Builtin.Int using (Int; pos)
open import Agda.Builtin.List using (List; _∷_; [])

-- ============================================================================
-- Priority Strategy Interface
-- ============================================================================

record PriorityStrategy : Set where
  field
    -- Core priority levels
    minimal : Priority        -- Lowest priority (e.g., test fixtures, documentation TODOs)
    low : Priority           -- Low priority (e.g., optimization opportunities)
    medium : Priority        -- Medium priority (e.g., missing tests)
    high : Priority          -- High priority (e.g., incomplete proofs)
    critical : Priority      -- Critical priority (e.g., FFI safety issues)
    
    -- Specialized priorities
    testFixture : Priority   -- Test scaffolding
    documentation : Priority -- Documentation gaps
    performance : Priority   -- Performance issues
    safety : Priority        -- Safety-critical (FFI, runtime errors)
    proof : Priority         -- Proof obligations

open PriorityStrategy public

-- ============================================================================
-- Default Priority Strategy
-- ============================================================================
--
-- Balanced weighting suitable for general mathematical development.

defaultStrategy : PriorityStrategy
defaultStrategy = record
  { minimal = mkPriority (("minimal", pos 1) ∷ []) []
  ; low = mkPriority (("low", pos 10) ∷ []) []
  ; medium = mkPriority (("medium", pos 50) ∷ []) []
  ; high = mkPriority (("high", pos 100) ∷ []) []
  ; critical = mkPriority (("critical", pos 500) ∷ []) []
  ; testFixture = mkPriority (("test-fixture", pos 1) ∷ []) []
  ; documentation = mkPriority (("docs", pos 25) ∷ []) []
  ; performance = mkPriority (("perf", pos 75) ∷ []) []
  ; safety = mkPriority (("safety", pos 200) ∷ []) []
  ; proof = mkPriority (("proof", pos 150) ∷ []) []
  }

-- ============================================================================
-- FFI Safety-Focused Strategy
-- ============================================================================
--
-- Prioritizes foreign function interface safety over proof completeness.
-- Use case: Projects with heavy external bindings where runtime correctness
-- is more critical than mathematical completeness.

ffiSafetyStrategy : PriorityStrategy
ffiSafetyStrategy = record
  { minimal = mkPriority (("minimal", pos 1) ∷ []) []
  ; low = mkPriority (("low", pos 5) ∷ []) []
  ; medium = mkPriority (("medium", pos 25) ∷ []) []
  ; high = mkPriority (("high", pos 50) ∷ []) []
  ; critical = mkPriority (("critical", pos 1000) ∷ []) []
  ; testFixture = mkPriority (("test-fixture", pos 1) ∷ []) []
  ; documentation = mkPriority (("docs", pos 10) ∷ []) []
  ; performance = mkPriority (("perf", pos 100) ∷ []) []
  ; safety = mkPriority (("safety", pos 800) ∷ []) []  -- Higher than default
  ; proof = mkPriority (("proof", pos 30) ∷ []) []     -- Lower than default
  }

-- ============================================================================
-- Proof Completeness-Focused Strategy
-- ============================================================================
--
-- Prioritizes mathematical proof obligations over engineering concerns.
-- Use case: Formalization projects where completeness is paramount.

proofCompletenessStrategy : PriorityStrategy
proofCompletenessStrategy = record
  { minimal = mkPriority (("minimal", pos 1) ∷ []) []
  ; low = mkPriority (("low", pos 10) ∷ []) []
  ; medium = mkPriority (("medium", pos 50) ∷ []) []
  ; high = mkPriority (("high", pos 200) ∷ []) []
  ; critical = mkPriority (("critical", pos 500) ∷ []) []
  ; testFixture = mkPriority (("test-fixture", pos 1) ∷ []) []
  ; documentation = mkPriority (("docs", pos 20) ∷ []) []
  ; performance = mkPriority (("perf", pos 30) ∷ []) []  -- Lower than default
  ; safety = mkPriority (("safety", pos 150) ∷ []) []
  ; proof = mkPriority (("proof", pos 300) ∷ []) []      -- Higher than default
  }

-- ============================================================================
-- Rapid Development Strategy
-- ============================================================================
--
-- Lower overall weights for faster iteration, accepting more technical debt.
-- Use case: Prototyping, proof-of-concept work.

rapidDevelopmentStrategy : PriorityStrategy
rapidDevelopmentStrategy = record
  { minimal = mkPriority (("minimal", pos 1) ∷ []) []
  ; low = mkPriority (("low", pos 5) ∷ []) []
  ; medium = mkPriority (("medium", pos 20) ∷ []) []
  ; high = mkPriority (("high", pos 50) ∷ []) []
  ; critical = mkPriority (("critical", pos 100) ∷ []) []
  ; testFixture = mkPriority (("test-fixture", pos 1) ∷ []) []
  ; documentation = mkPriority (("docs", pos 5) ∷ []) []
  ; performance = mkPriority (("perf", pos 10) ∷ []) []
  ; safety = mkPriority (("safety", pos 75) ∷ []) []
  ; proof = mkPriority (("proof", pos 25) ∷ []) []
  }

-- ============================================================================
-- Production Hardening Strategy
-- ============================================================================
--
-- Higher weights across the board for production-ready code.
-- Use case: Final release preparation, hardening phase.

productionStrategy : PriorityStrategy
productionStrategy = record
  { minimal = mkPriority (("minimal", pos 10) ∷ []) []
  ; low = mkPriority (("low", pos 50) ∷ []) []
  ; medium = mkPriority (("medium", pos 150) ∷ []) []
  ; high = mkPriority (("high", pos 300) ∷ []) []
  ; critical = mkPriority (("critical", pos 1000) ∷ []) []
  ; testFixture = mkPriority (("test-fixture", pos 5) ∷ []) []
  ; documentation = mkPriority (("docs", pos 100) ∷ []) []
  ; performance = mkPriority (("perf", pos 200) ∷ []) []
  ; safety = mkPriority (("safety", pos 500) ∷ []) []
  ; proof = mkPriority (("proof", pos 400) ∷ []) []
  }

-- ============================================================================
-- Backward Compatibility Exports
-- ============================================================================
--
-- For code that expects the old hardcoded priorities, provide aliases
-- using default strategy values.

lowPriority : Priority
lowPriority = PriorityStrategy.testFixture defaultStrategy

highPriority : Priority
highPriority = PriorityStrategy.critical defaultStrategy
