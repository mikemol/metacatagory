{-# OPTIONS --without-K #-}

-- GrowthAnalysis: Parameterized solution space growth analysis
-- PHASE-II.2: Extracted from Core.GrowthMetrics (lines 233-260)
--
-- This module analyzes coordinate allocation patterns for different development
-- branches without hardcoding the history data.

module GrowthAnalysis where

open import Core.GrowthMetrics using
  ( CoordinateAllocation
  ; PhaseDensity
  ; YCoordinateDistribution
  ; GrowthRate
  ; ExpansionPattern
  ; GrowthSnapshot
  ; calculatePhaseDensity
  ; analyzeYDistribution
  ; computeGrowthRate
  ; classifyExpansionPattern
  ; captureGrowthSnapshot
  )
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.Nat using (Nat; zero; suc)
open import Core.Utils using (length; map)
open import Metamodel as M

-- ============================================================================
-- Parameterized Growth Analysis Module
-- ============================================================================
--
-- Instead of hardcoding metacatagoryGrowthHistory, this module takes
-- the allocation history as a parameter, enabling analysis of:
--   * Different development branches (Categorical vs Classical)
--   * Synthetic growth patterns for testing
--   * Historical snapshots at different points in time

module Analysis
  (history : List CoordinateAllocation)
  where

  -- Helper: Extract unique phase numbers from history
  -- Simplified: just list all phases (may have duplicates for analysis purposes)
  allPhases : List CoordinateAllocation → List Nat
  allPhases [] = []
  allPhases (a ∷ as) = M.Coordinate.x (CoordinateAllocation.coordinate a) ∷ allPhases as

  -- Overall growth snapshot
  growthSnapshot : GrowthSnapshot
  growthSnapshot = captureGrowthSnapshot (length history) history

  -- Growth rate metrics
  growthRate : GrowthRate
  growthRate = computeGrowthRate history

  -- Expansion pattern classification
  expansionPattern : ExpansionPattern
  expansionPattern = classifyExpansionPattern growthRate

  -- Per-phase density analysis
  phaseDensities : List Nat → List PhaseDensity
  phaseDensities phases = map (λ phase → calculatePhaseDensity phase history) phases

  -- Per-phase Y-coordinate distribution
  yDistributions : List Nat → List YCoordinateDistribution
  yDistributions phases = map (λ phase → analyzeYDistribution phase history) phases

  -- All active phases in history
  activePhases : List Nat
  activePhases = allPhases history

  -- Comprehensive phase analysis
  allPhaseDensities : List PhaseDensity
  allPhaseDensities = phaseDensities activePhases

  allYDistributions : List YCoordinateDistribution
  allYDistributions = yDistributions activePhases

  -- Query specific phase
  phase13Density : PhaseDensity
  phase13Density = calculatePhaseDensity 13 history

  phase13YDistribution : YCoordinateDistribution
  phase13YDistribution = analyzeYDistribution 13 history

-- ============================================================================
-- Concrete Growth Histories
-- ============================================================================

-- Main metacategory development history
-- (Previously hardcoded in Core.GrowthMetrics)
metacatagoryHistory : List CoordinateAllocation
metacatagoryHistory =
  record { coordinate = M.mkCoord 0 0 ; timestamp = 0 ; context = "dispatch-root" } ∷
  record { coordinate = M.mkCoord 1 0 ; timestamp = 1 ; context = "field-basic" } ∷
  record { coordinate = M.mkCoord 1 1 ; timestamp = 2 ; context = "field-extension" } ∷
  record { coordinate = M.mkCoord 2 0 ; timestamp = 3 ; context = "galois-group" } ∷
  record { coordinate = M.mkCoord 4 0 ; timestamp = 4 ; context = "yoneda-embedding" } ∷
  record { coordinate = M.mkCoord 4 1 ; timestamp = 5 ; context = "yoneda-iso" } ∷
  record { coordinate = M.mkCoord 13 0 ; timestamp = 6 ; context = "godel-sentence" } ∷
  record { coordinate = M.mkCoord 13 1 ; timestamp = 7 ; context = "incompleteness-first" } ∷
  record { coordinate = M.mkCoord 13 2 ; timestamp = 8 ; context = "incompleteness-second" } ∷
  []

-- Categorical development branch (focuses on categorical structures)
categoricalBranchHistory : List CoordinateAllocation
categoricalBranchHistory =
  record { coordinate = M.mkCoord 0 0 ; timestamp = 0 ; context = "category-init" } ∷
  record { coordinate = M.mkCoord 4 0 ; timestamp = 1 ; context = "yoneda-embedding" } ∷
  record { coordinate = M.mkCoord 4 1 ; timestamp = 2 ; context = "yoneda-iso" } ∷
  record { coordinate = M.mkCoord 4 2 ; timestamp = 3 ; context = "yoneda-lemma" } ∷
  record { coordinate = M.mkCoord 5 0 ; timestamp = 4 ; context = "adjunction-def" } ∷
  record { coordinate = M.mkCoord 5 1 ; timestamp = 5 ; context = "adjunction-unit" } ∷
  record { coordinate = M.mkCoord 6 0 ; timestamp = 6 ; context = "limits" } ∷
  record { coordinate = M.mkCoord 6 1 ; timestamp = 7 ; context = "colimits" } ∷
  []

-- Classical algebra branch (focuses on concrete structures)
classicalBranchHistory : List CoordinateAllocation
classicalBranchHistory =
  record { coordinate = M.mkCoord 0 0 ; timestamp = 0 ; context = "algebra-init" } ∷
  record { coordinate = M.mkCoord 1 0 ; timestamp = 1 ; context = "field-basic" } ∷
  record { coordinate = M.mkCoord 1 1 ; timestamp = 2 ; context = "field-extension" } ∷
  record { coordinate = M.mkCoord 1 2 ; timestamp = 3 ; context = "field-splitting" } ∷
  record { coordinate = M.mkCoord 2 0 ; timestamp = 4 ; context = "galois-group" } ∷
  record { coordinate = M.mkCoord 2 1 ; timestamp = 5 ; context = "galois-correspondence" } ∷
  record { coordinate = M.mkCoord 3 0 ; timestamp = 6 ; context = "solvability" } ∷
  record { coordinate = M.mkCoord 3 1 ; timestamp = 7 ; context = "radical-extensions" } ∷
  []

-- ============================================================================
-- Instantiated Analyses
-- ============================================================================

module Metacatagory where
  open Analysis metacatagoryHistory public

module Categorical where
  open Analysis categoricalBranchHistory public

module Classical where
  open Analysis classicalBranchHistory public

-- ============================================================================
-- Backward Compatibility Exports
-- ============================================================================
--
-- For code that expects the old module structure, re-export the main
-- metacategory analysis under the old names.

open Metacatagory public using ()
  renaming
    ( growthSnapshot to metacatagoryGrowthSnapshot
    ; phase13Density to phase13Density
    ; phase13YDistribution to phase13YDistribution
    ; growthRate to metacatagoryGrowthRate
    ; expansionPattern to metacatagoryExpansionPattern
    )
