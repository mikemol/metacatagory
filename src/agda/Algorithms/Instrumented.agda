{-# OPTIONS --without-K #-}

-- Algorithms.Instrumented: Growth-tracked algorithm bundles
-- PARAM-GROWTH-1: Extend GrowthAnalysis usage to production algorithm bundles
--
-- This module wraps algorithm bundles with optional coordinate allocation tracking,
-- enabling runtime complexity visibility without modifying bundle APIs.

module Algorithms.Instrumented where

open import Algebra.Rings.Basic using (FieldDeclaration)
open import Core.Algorithms.Bundle using (AlgorithmBundle)
open import Core.GrowthMetrics using
  ( CoordinateAllocation
  ; GrowthSnapshot
  ; captureGrowthSnapshot
  ; PhaseDensity
  ; calculatePhaseDensity
  ; GrowthRate
  ; computeGrowthRate
  )
open import Metamodel as M
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Nat using (Nat; zero; suc)
open import Agda.Builtin.String using (String)
open import Core.AlgebraicAlgorithms using
  ( MinimalPolynomialAlgorithm
  ; GaloisGroupAlgorithm
  ; SplittingFieldAlgorithm
  ; FieldExtensionDegreeAlgorithm
  ; SubfieldEnumerationAlgorithm
  ; SubgroupEnumerationAlgorithm
  ; AlgebraicityDecisionAlgorithm
  ; PrimitiveElementAlgorithm
  ; NormalityDecisionAlgorithm
  ; SeparabilityDecisionAlgorithm
  ; NormalClosureAlgorithm
  ; GaloisClosureAlgorithm
  )
open import Core.Limitations using (LimitationEvidence)
open import Algebra.Fields.Types using
  ( GaloisGroup
  ; FieldAutomorphism
  ; AlgebraicElement
  ; TranscendentalElement
  ; SplittingField
  ; ExtensionDegree
  ; Subfield
  ; NormalExtension
  ; SeparableExtension
  )
open import Algebra.Foundation using (GroupDeclaration)
open import Core.Phase using (Maybe; nothing; just)
open import Core.Utils using (length)

-- ============================================================================
-- Allocation Logger
-- ============================================================================

-- Mutable state for tracking allocations (abstract; actual implementation via IO or effect system)
postulate
  AllocationLog : Set
  emptyLog : AllocationLog
  appendAllocation : AllocationLog → CoordinateAllocation → AllocationLog
  getLog : AllocationLog → List CoordinateAllocation

-- ============================================================================
-- Instrumented Algorithm Records
-- ============================================================================

-- Each algorithm invocation logs a coordinate allocation event
-- Phase assignment maps algorithm categories to phase numbers

-- | Minimal-polynomial algorithm augmented with phase+allocation logging.
record InstrumentedMinimalPolynomialAlgorithm (F E : FieldDeclaration) : Set₁ where
  field
    base : MinimalPolynomialAlgorithm F E
    phase : Nat
    logger : AllocationLog
    
  minimalPolynomial : M.Identifier → M.Identifier
  minimalPolynomial α =
    let result = MinimalPolynomialAlgorithm.minimalPolynomial base α
        alloc = record
          { coordinate = M.mkCoord phase zero
          ; timestamp = length (getLog logger)
          ; context = "minpoly-computation"
          }
        _ = appendAllocation logger alloc
    in result

record InstrumentedGaloisGroupAlgorithm (F E : FieldDeclaration) : Set₁ where
  -- | Galois group algorithm augmented with allocation/phase logging.
  field
    base : GaloisGroupAlgorithm F E
    phase : Nat
    logger : AllocationLog
    
  galoisGroup : M.Identifier → GaloisGroup F E
  galoisGroup f =
    let result = GaloisGroupAlgorithm.galoisGroup base f
        alloc = record
          { coordinate = M.mkCoord phase (suc zero)
          ; timestamp = length (getLog logger)
          ; context = "galois-group-computation"
          }
        _ = appendAllocation logger alloc
    in result

  automorphisms : M.Identifier → List (FieldAutomorphism F E)
  automorphisms = GaloisGroupAlgorithm.automorphisms base

  isSolvable : M.Identifier → M.Identifier
  isSolvable = GaloisGroupAlgorithm.isSolvable base

  limitation : Maybe LimitationEvidence
  limitation = GaloisGroupAlgorithm.limitation base

-- ============================================================================
-- Phase Mapping Strategy
-- ============================================================================

-- Maps algorithm categories to coordinate phases
record AlgorithmPhaseMap : Set where
  field
    minPolyPhase : Nat
    galoisPhase : Nat
    splittingPhase : Nat
    degreePhase : Nat
    subfieldPhase : Nat
    subgroupPhase : Nat
    algebraicityPhase : Nat
    primitivePhase : Nat
    normalityPhase : Nat
    separabilityPhase : Nat
    normalClosurePhase : Nat
    galoisClosurePhase : Nat

-- ============================================================================
-- Instrumented Bundle
-- ============================================================================

record InstrumentedAlgorithmBundle (F E : FieldDeclaration) : Set₁ where
  field
    baseBundle : AlgorithmBundle F E
    logger : AllocationLog
    phaseMap : AlgorithmPhaseMap
    
  -- Instrumented algorithm access
  minimalPolynomialAlg : InstrumentedMinimalPolynomialAlgorithm F E
  minimalPolynomialAlg = record
    { base = AlgorithmBundle.minimalPolynomialAlg baseBundle
    ; phase = AlgorithmPhaseMap.minPolyPhase phaseMap
    ; logger = logger
    }
  
  galoisGroupAlg : InstrumentedGaloisGroupAlgorithm F E
  galoisGroupAlg = record
    { base = AlgorithmBundle.galoisGroupAlg baseBundle
    ; phase = AlgorithmPhaseMap.galoisPhase phaseMap
    ; logger = logger
    }
  
  -- Growth analysis
  allocationHistory : List CoordinateAllocation
  allocationHistory = getLog logger
  
  growthSnapshot : GrowthSnapshot
  growthSnapshot = captureGrowthSnapshot (length allocationHistory) allocationHistory
  
  growthRate : GrowthRate
  growthRate = computeGrowthRate allocationHistory

-- ============================================================================
-- Default Phase Maps
-- ============================================================================

-- Default phase map (aligns with metacategory coordinate system)
defaultPhaseMap : AlgorithmPhaseMap
defaultPhaseMap = record
  { minPolyPhase = 1         -- Field extension phase
  ; galoisPhase = 2           -- Galois theory phase
  ; splittingPhase = 1
  ; degreePhase = 1
  ; subfieldPhase = 1
  ; subgroupPhase = 2
  ; algebraicityPhase = 1
  ; primitivePhase = 1
  ; normalityPhase = 3        -- Advanced field theory
  ; separabilityPhase = 3
  ; normalClosurePhase = 3
  ; galoisClosurePhase = 3
  }

-- Categorical emphasis (focus on universal properties)
categoricalPhaseMap : AlgorithmPhaseMap
categoricalPhaseMap = record
  { minPolyPhase = 4          -- Yoneda embedding phase
  ; galoisPhase = 5            -- Adjunction phase
  ; splittingPhase = 6         -- Limits/colimits phase
  ; degreePhase = 4
  ; subfieldPhase = 6
  ; subgroupPhase = 5
  ; algebraicityPhase = 4
  ; primitivePhase = 4
  ; normalityPhase = 6
  ; separabilityPhase = 6
  ; normalClosurePhase = 6
  ; galoisClosurePhase = 6
  }

-- ============================================================================
-- Bundle Instrumentation
-- ============================================================================

-- Wrap an existing bundle with growth tracking
instrumentBundle : {F E : FieldDeclaration}
                 → AlgorithmBundle F E
                 → AlgorithmPhaseMap
                 → InstrumentedAlgorithmBundle F E
instrumentBundle {F} {E} bundle phaseMap = record
  { baseBundle = bundle
  ; logger = emptyLog
  ; phaseMap = phaseMap
  }

-- Convenience: instrument with default phase map
instrumentBundleDefault : {F E : FieldDeclaration}
                        → AlgorithmBundle F E
                        → InstrumentedAlgorithmBundle F E
instrumentBundleDefault bundle = instrumentBundle bundle defaultPhaseMap

-- ============================================================================
-- Growth Analysis Integration
-- ============================================================================

-- Run analysis on instrumented bundle's allocation history
module BundleAnalysis
  {F E : FieldDeclaration}
  (instrumented : InstrumentedAlgorithmBundle F E)
  where
  
  -- Open parameterized GrowthAnalysis.Analysis module with bundle's history
  open import GrowthAnalysis as GA
  open GA.Analysis (InstrumentedAlgorithmBundle.allocationHistory instrumented) public

-- ============================================================================
-- Usage Pattern
-- ============================================================================

{-
Example usage:

1. Start with a standard bundle:
   bundle = lookupAlgorithmBundle F E

2. Instrument it:
   instrumented = instrumentBundleDefault bundle

3. Use algorithms as normal:
   minPolyAlg = InstrumentedAlgorithmBundle.minimalPolynomialAlg instrumented
   result = InstrumentedMinimalPolynomialAlgorithm.minimalPolynomial minPolyAlg α

4. Analyze growth:
   module MyAnalysis = BundleAnalysis instrumented
   snapshot = MyAnalysis.growthSnapshot
   rate = MyAnalysis.growthRate

Implementation notes:
- AllocationLog is postulated; actual implementation needs mutable state or IO monad
- For pure analysis, log can be accumulated in State monad or passed explicitly
- Phase map enables different coordinate space strategies
-}
