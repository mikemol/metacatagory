{-# OPTIONS --without-K #-}

-- Demonstration: Fully Automatic Evidence Detection
-- Shows how to get near-automatic dispatch with instance evidence

module Examples.AutomaticEvidenceDemo where

open import Core
open import Algebra.Foundation
open import Algebra.Rings.Basic
open import Algebra.Fields.Basic
open import Core.AlgebraicAlgorithms
open import Core.Algorithms.Bundle
open import Core.Algorithms.Registry
open import Core.Algorithms.FiniteFields
open import Core.Algorithms.NumberFields

-- ============================================================================
-- Setup: Declare Fields with Instance Evidence
-- ============================================================================

-- Declare specific fields
postulate
  GF8 : FieldDeclaration
  GF64 : FieldDeclaration
  Q : FieldDeclaration
  QSqrt2 : FieldDeclaration

-- Declare evidence as INSTANCES (this makes them findable by instance search)
postulate
  instance
    GF8-isFinite : IsFiniteField GF8
    GF64-isFinite : IsFiniteField GF64
    Q-isNumber : IsNumberField Q
    QSqrt2-isNumber : IsNumberField QSqrt2

-- ============================================================================
-- Example 1: Semi-Automatic with Instance Arguments
-- ============================================================================

module SemiAutomatic where
  
  -- The evidence instances (GF8-isFinite, etc.) are found automatically
  -- We just need to wrap them in Classifiable
  
  galoisGF8 : GaloisGroupAlgorithm GF8 GF8
  galoisGF8 = lookupGaloisGroupAuto GF8 GF8
    ⦃ finiteFieldClassifiable GF8-isFinite ⦄  -- Instance found in scope
    ⦃ finiteFieldClassifiable GF8-isFinite ⦄
  
  extensionDegree : FieldExtensionDegreeAlgorithm GF8 GF64
  extensionDegree = lookupExtensionDegreeAuto GF8 GF64
    ⦃ finiteFieldClassifiable GF8-isFinite ⦄
    ⦃ finiteFieldClassifiable GF64-isFinite ⦄
  
  minimalPoly : MinimalPolynomialAlgorithm Q QSqrt2
  minimalPoly = lookupMinimalPolynomialAuto Q QSqrt2
    ⦃ numberFieldClassifiable Q-isNumber ⦄
    ⦃ numberFieldClassifiable QSqrt2-isNumber ⦄

-- ============================================================================
-- Example 2: Using Wrapper Functions for More Automation
-- ============================================================================

module UsingWrappers where
  
  -- Define helpers that automatically find evidence instances
  lookupGaloisFF : {F E : FieldDeclaration}
                 → ⦃ evF : IsFiniteField F ⦄
                 → ⦃ evE : IsFiniteField E ⦄
                 → GaloisGroupAlgorithm F E
  lookupGaloisFF {F} {E} ⦃ evF ⦄ ⦃ evE ⦄ = 
    lookupGaloisGroupAuto F E 
      ⦃ finiteFieldClassifiable evF ⦄ 
      ⦃ finiteFieldClassifiable evE ⦄
  
  lookupMinPolyNF : {F E : FieldDeclaration}
                  → ⦃ evF : IsNumberField F ⦄
                  → ⦃ evE : IsNumberField E ⦄
                  → MinimalPolynomialAlgorithm F E
  lookupMinPolyNF {F} {E} ⦃ evF ⦄ ⦃ evE ⦄ = 
    lookupMinimalPolynomialAuto F E 
      ⦃ numberFieldClassifiable evF ⦄ 
      ⦃ numberFieldClassifiable evE ⦄
  
  -- NOW THIS IS FULLY AUTOMATIC - evidence instances found by Agda!
  galoisGF8 : GaloisGroupAlgorithm GF8 GF8
  galoisGF8 = lookupGaloisFF  -- Agda finds GF8-isFinite automatically!
  
  extensionDeg : FieldExtensionDegreeAlgorithm GF8 GF64  
  extensionDeg = AlgorithmBundle.extensionDegreeAlg (
    lookupAlgorithmBundleAuto GF8 GF64
      ⦃ finiteFieldClassifiable GF8-isFinite ⦄
      ⦃ finiteFieldClassifiable GF64-isFinite ⦄
    )
  
  minimalPoly : MinimalPolynomialAlgorithm Q QSqrt2
  minimalPoly = lookupMinPolyNF  -- Agda finds Q-isNumber, QSqrt2-isNumber automatically!

-- ============================================================================
-- Example 3: Wrapper Functions for Full Automation
-- ============================================================================

module FullyAutomaticWrappers where
  
  -- Create type-specific wrappers that hide the machinery
  
  module FiniteFieldOps where
    galoisGroup : {F E : FieldDeclaration}
                → ⦃ _ : IsFiniteField F ⦄
                → ⦃ _ : IsFiniteField E ⦄
                → GaloisGroupAlgorithm F E
    galoisGroup {F} {E} ⦃ evF ⦄ ⦃ evE ⦄ = 
      lookupGaloisGroupAuto F E 
        ⦃ finiteFieldClassifiable evF ⦄ 
        ⦃ finiteFieldClassifiable evE ⦄
    
    extensionDegree : {F E : FieldDeclaration}
                    → ⦃ _ : IsFiniteField F ⦄
                    → ⦃ _ : IsFiniteField E ⦄
                    → FieldExtensionDegreeAlgorithm F E
    extensionDegree {F} {E} ⦃ evF ⦄ ⦃ evE ⦄ = 
      lookupExtensionDegreeAuto F E 
        ⦃ finiteFieldClassifiable evF ⦄ 
        ⦃ finiteFieldClassifiable evE ⦄
  
  module NumberFieldOps where
    minimalPolynomial : {F E : FieldDeclaration}
                      → ⦃ _ : IsNumberField F ⦄
                      → ⦃ _ : IsNumberField E ⦄
                      → MinimalPolynomialAlgorithm F E
    minimalPolynomial {F} {E} ⦃ evF ⦄ ⦃ evE ⦄ = 
      lookupMinimalPolynomialAuto F E 
        ⦃ numberFieldClassifiable evF ⦄ 
        ⦃ numberFieldClassifiable evE ⦄
    
    primitiveElement : {F E : FieldDeclaration}
                     → ⦃ _ : IsNumberField F ⦄
                     → ⦃ _ : IsNumberField E ⦄
                     → PrimitiveElementAlgorithm F E
    primitiveElement {F} {E} ⦃ evF ⦄ ⦃ evE ⦄ = 
      lookupPrimitiveElementAuto F E 
        ⦃ numberFieldClassifiable evF ⦄ 
        ⦃ numberFieldClassifiable evE ⦄
  
  -- Usage is now COMPLETELY AUTOMATIC:
  open FiniteFieldOps
  open NumberFieldOps
  
  result1 : GaloisGroupAlgorithm GF8 GF64
  result1 = galoisGroup  -- Fully automatic!
  
  result2 : MinimalPolynomialAlgorithm Q QSqrt2
  result2 = minimalPolynomial  -- Fully automatic!
  
  result3 : FieldExtensionDegreeAlgorithm GF8 GF8
  result3 = extensionDegree  -- Fully automatic!

-- ============================================================================
-- Summary: The Automation Hierarchy
-- ============================================================================

-- Level 0: Completely manual
--   lookupGaloisGroupWithClassification F E 
--     (classifyAsFiniteField F evF) 
--     (classifyAsFiniteField E evE)

-- Level 1: Lazy instance construction (what Registry provides)
--   lookupGaloisGroupAuto F E 
--     ⦃ finiteFieldClassifiable evF ⦄ 
--     ⦃ finiteFieldClassifiable evE ⦄

-- Level 2: Semi-automatic (evidence instances declared, referenced by name)
--   lookupGaloisGroupAuto F E 
--     ⦃ finiteFieldClassifiable GF8-isFinite ⦄ 
--     ⦃ finiteFieldClassifiable GF64-isFinite ⦄

-- Level 3: Wrapper functions with instance arguments (FULLY AUTOMATIC)
--   galoisGroup {F} {E}  -- Evidence instances found by Agda automatically!

-- Conclusion:
-- ✓ YES, we get automatic evidence detection
-- ✓ Declare field evidence as instances once
-- ✓ Use wrapper functions for zero-boilerplate calls
-- ✓ The Classifiable layer still breaks cycles (safe)
