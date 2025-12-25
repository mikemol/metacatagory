{-# OPTIONS --without-K #-}

-- Examples.InstrumentedAlgorithmDemo: Growth-tracked algorithm usage
-- PARAM-GROWTH-1: Demonstrates production growth analysis via instrumented bundles

module Examples.InstrumentedAlgorithmDemo where

open import Algorithms.Instrumented
open import Core.Algorithms.Registry using (lookupAlgorithmBundle)
open import Core.Algorithms.Bundle using (AlgorithmBundle)
open import Algebra.Rings.Basic using (FieldDeclaration)
open import Metamodel as M
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Nat using (Nat; zero; suc)
open import Core.GrowthMetrics using (GrowthSnapshot; GrowthRate)
open import Algebra.Fields.Types using (GaloisGroup)

-- Example field declarations
postulate
  ℚ : FieldDeclaration
  GF8 : FieldDeclaration

-- ============================================================================
-- Example 1: Instrumented bundle with default phase mapping
-- ============================================================================

-- Get base algorithm bundle from registry
baseBundle : AlgorithmBundle ℚ GF8
baseBundle = lookupAlgorithmBundle ℚ GF8

-- Instrument it with growth tracking
instrumentedBundle : InstrumentedAlgorithmBundle ℚ GF8
instrumentedBundle = instrumentBundleDefault baseBundle

-- Access instrumented algorithms
minPolyAlg : InstrumentedMinimalPolynomialAlgorithm ℚ GF8
minPolyAlg = InstrumentedAlgorithmBundle.minimalPolynomialAlg instrumentedBundle

galoisAlg : InstrumentedGaloisGroupAlgorithm ℚ GF8
galoisAlg = InstrumentedAlgorithmBundle.galoisGroupAlg instrumentedBundle

-- Example: Compute minimal polynomial (logs allocation)
exampleMinPoly : M.Identifier
exampleMinPoly = InstrumentedMinimalPolynomialAlgorithm.minimalPolynomial minPolyAlg (M.mkId "alpha")

-- Example: Compute Galois group (logs allocation)
exampleGaloisGroup : GaloisGroup ℚ GF8
exampleGaloisGroup = InstrumentedGaloisGroupAlgorithm.galoisGroup galoisAlg (M.mkId "f")

-- ============================================================================
-- Example 2: Growth analysis on instrumented bundle
-- ============================================================================

-- Open bundle-specific analysis module
module AnalysisDemo where
  open BundleAnalysis instrumentedBundle public

-- Access growth metrics
demoGrowthSnapshot : GrowthSnapshot
demoGrowthSnapshot = AnalysisDemo.growthSnapshot

demoGrowthRate : GrowthRate
demoGrowthRate = AnalysisDemo.growthRate

-- ============================================================================
-- Example 3: Categorical phase mapping
-- ============================================================================

-- Instrument bundle with categorical emphasis
categoricalInstrumented : InstrumentedAlgorithmBundle ℚ GF8
categoricalInstrumented = instrumentBundle baseBundle categoricalPhaseMap

-- Categorical analysis module
module CategoricalAnalysis where
  open BundleAnalysis categoricalInstrumented public

categoricalSnapshot : GrowthSnapshot
categoricalSnapshot = CategoricalAnalysis.growthSnapshot

-- ============================================================================
-- Example 4: Custom phase mapping
-- ============================================================================

-- Define custom phase map for domain-specific organization
customPhaseMap : AlgorithmPhaseMap
customPhaseMap = record
  { minPolyPhase = 10
  ; galoisPhase = 11
  ; splittingPhase = 12
  ; degreePhase = 10
  ; subfieldPhase = 13
  ; subgroupPhase = 11
  ; algebraicityPhase = 10
  ; primitivePhase = 10
  ; normalityPhase = 14
  ; separabilityPhase = 14
  ; normalClosurePhase = 15
  ; galoisClosurePhase = 15
  }

customInstrumented : InstrumentedAlgorithmBundle ℚ GF8
customInstrumented = instrumentBundle baseBundle customPhaseMap

-- ============================================================================
-- Usage Pattern Summary
-- ============================================================================

{-
Growth-tracked algorithm execution pattern:

1. Obtain standard bundle:
   bundle = lookupAlgorithmBundle F E

2. Instrument with phase mapping:
   instrumented = instrumentBundleDefault bundle
   -- or --
   instrumented = instrumentBundle bundle categoricalPhaseMap

3. Use algorithms normally:
   alg = InstrumentedAlgorithmBundle.minimalPolynomialAlg instrumented
   result = InstrumentedMinimalPolynomialAlgorithm.minimalPolynomial alg input

4. Analyze growth:
   module MyAnalysis = BundleAnalysis instrumented
   snapshot = MyAnalysis.growthSnapshot
   rate = MyAnalysis.growthRate

Benefits:
- Runtime complexity visibility
- Coordinate allocation tracking
- No API changes to existing algorithms
- Flexible phase mapping strategies
- Parameterized analysis via GrowthAnalysis module

Implementation status:
- AllocationLog is postulated (requires mutable state/IO)
- Pure analysis possible via State monad threading
- Phase maps enable different organizational strategies
-}
