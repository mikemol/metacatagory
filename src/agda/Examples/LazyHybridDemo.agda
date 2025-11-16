-- Example: Using the Lazy Hybrid Instance Approach
-- Demonstrates how explicit instance construction breaks cycles while maintaining ergonomics

module Examples.LazyHybridDemo where

open import Core
open import Algebra.Foundation
open import Algebra.Rings.Basic using (FieldDeclaration)
open import Algebra.Fields.Basic
open import Core.AlgebraicAlgorithms
open import Core.Algorithms.Bundle
open import Core.Algorithms.Registry
open import Core.Algorithms.FiniteFields
open import Core.Algorithms.NumberFields

-- ============================================================================
-- Example 1: Explicit Instance Construction (The Lazy Hybrid)
-- ============================================================================

module ExplicitInstanceExample where
  
  -- Suppose we have finite field evidence
  postulate
    GF8 : FieldDeclaration
    GF8-finite : IsFiniteField GF8
  
  -- Usage: Explicitly construct the Classifiable instance
  -- The instance declaration converts evidence → Classifiable lazily
  galoisGroupAlgo : GaloisGroupAlgorithm GF8 GF8
  galoisGroupAlgo = lookupGaloisGroupAuto GF8 GF8 
    ⦃ finiteFieldClassifiable GF8-finite ⦄  -- Explicit instance
    ⦃ finiteFieldClassifiable GF8-finite ⦄  -- Explicit instance
  
  -- This works because:
  -- 1. Instance search finds the Auto function signature needs Classifiable instances
  -- 2. We provide those instances explicitly in braces ⦃ ... ⦄
  -- 3. The instances are constructed from explicit evidence (GF8-finite)
  -- 4. No circular instance search occurs!

-- ============================================================================
-- Example 2: Using Explicit Classification (When You Want to See the Pair)
-- ============================================================================

module ExplicitClassificationExample where
  
  postulate
    Q : FieldDeclaration
    QSqrt2 : FieldDeclaration
    Q-number : IsNumberField Q
    QSqrt2-number : IsNumberField QSqrt2
  
  -- Build classifications explicitly
  Q-class : FieldClassification Q
  Q-class = classifyAsNumberField Q Q-number
  
  QSqrt2-class : FieldClassification QSqrt2
  QSqrt2-class = classifyAsNumberField QSqrt2 QSqrt2-number
  
  -- Usage: Provide classifications explicitly
  minPolyAlgo : MinimalPolynomialAlgorithm Q QSqrt2
  minPolyAlgo = lookupMinimalPolynomialWithClassification Q QSqrt2 Q-class QSqrt2-class

-- ============================================================================
-- Example 3: Mixing Field Types (Demonstrates Dispatch)
-- ============================================================================

module MixedFieldExample where
  
  postulate
    Q : FieldDeclaration
    GF8 : FieldDeclaration
    Q-number : IsNumberField Q
    GF8-finite : IsFiniteField GF8
  
  -- Mixing number field and finite field
  -- The dispatch will fall back to genericAlgorithmBundle
  mixedAlgo : GaloisGroupAlgorithm Q GF8
  mixedAlgo = lookupGaloisGroupAuto Q GF8
    ⦃ numberFieldClassifiable Q-number ⦄
    ⦃ finiteFieldClassifiable GF8-finite ⦄
  
  -- Alternatively with explicit classification:
  mixedAlgo' : GaloisGroupAlgorithm Q GF8
  mixedAlgo' = lookupGaloisGroupWithClassification Q GF8
    (classifyAsNumberField Q Q-number)
    (classifyAsFiniteField GF8 GF8-finite)

-- ============================================================================
-- Example 4: Direct Evidence-Based Lookup (Most Explicit)
-- ============================================================================

module DirectEvidenceExample where
  
  postulate
    GF4 : FieldDeclaration
    GF16 : FieldDeclaration
    GF4-finite : IsFiniteField GF4
    GF16-finite : IsFiniteField GF16
  
  -- Direct lookup without classification machinery
  bundleAlgo : AlgorithmBundle GF4 GF16
  bundleAlgo = lookupWithFiniteFieldEvidence GF4 GF16 GF4-finite GF16-finite
  
  -- Extract specific algorithm
  extensionDeg : FieldExtensionDegreeAlgorithm GF4 GF16
  extensionDeg = AlgorithmBundle.extensionDegreeAlg bundleAlgo

-- ============================================================================
-- Summary: Three Patterns, Choose Your Preference
-- ============================================================================

-- Pattern A: Lazy instance construction (hybrid)
--   lookupGaloisGroupAuto F E ⦃ finiteFieldClassifiable evF ⦄ ⦃ finiteFieldClassifiable evE ⦄
--   ✓ Uses instance arguments
--   ✓ Explicit evidence construction
--   ✓ No circular instance search
--   ✓ Best for: library code where evidence flow should be visible

-- Pattern B: Explicit classification
--   lookupGaloisGroupWithClassification F E (classifyAsFiniteField F evF) (classifyAsFiniteField E evE)
--   ✓ Shows the dependent pair explicitly
--   ✓ Easy to understand what's happening
--   ✓ Best for: examples, teaching, debugging

-- Pattern C: Direct evidence-based
--   lookupWithFiniteFieldEvidence F E evF evE
--   ✓ Most direct, no machinery
--   ✓ Type-safe (evidence types match)
--   ✓ Best for: when you already have typed evidence and don't need dispatch
