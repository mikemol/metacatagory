-- Example: Function field over F₂, symbolic demo of registry dispatch
-- Demonstrates classification-based, lazy instance, and fully automatic dispatch

module Examples.FunctionField.F2x where

open import Core
open import Algebra.Foundation
open import Algebra.Rings.Basic using (FieldDeclaration)
open import Algebra.Fields.Basic
open import Metamodel as M
open import Core.Algorithms.Registry
open import Core.Algorithms.FunctionFields
open import Core.AlgebraicAlgorithms
open import Agda.Builtin.List using (List)

-- Postulate a base field F₂(x) and an extension E = F₂(x)(t)
postulate
  F2x  : FieldDeclaration
  Eext : FieldDeclaration

-- Declare evidence as instances (makes them findable by instance search)
postulate
  instance
    F2x-isFunc  : IsFunctionField F2x
    Eext-isFunc : IsFunctionField Eext

-- ============================================================================
-- Example 1: Manual Classification-Based Dispatch
-- ============================================================================

module ManualClassification where
  -- Build classifications explicitly
  cF : FieldClassification F2x
  cF = classifyAsFunctionField F2x F2x-isFunc

  cE : FieldClassification Eext
  cE = classifyAsFunctionField Eext Eext-isFunc

  -- Obtain a minimal polynomial algorithm via classification-based dispatch
  minpolyAlg : MinimalPolynomialAlgorithm F2x Eext
  minpolyAlg = lookupMinimalPolynomialWithClassification F2x Eext cF cE

  -- Use the algorithm on a symbolic element α ∈ Eext
  α : M.Identifier
  α = M.mkId "t"

  minpoly-α : M.Identifier
  minpoly-α = MinimalPolynomialAlgorithm.minimalPolynomial minpolyAlg α

  -- Try other algorithms via classification
  galoisAlg : GaloisGroupAlgorithm F2x Eext
  galoisAlg = lookupGaloisGroupWithClassification F2x Eext cF cE

  extDegAlg : FieldExtensionDegreeAlgorithm F2x Eext
  extDegAlg = lookupExtensionDegreeWithClassification F2x Eext cF cE

  splittingAlg : SplittingFieldAlgorithm F2x
  splittingAlg = lookupSplittingFieldWithClassification F2x cF

-- ============================================================================
-- Example 2: Lazy Instance-Based Dispatch
-- ============================================================================

module LazyInstanceDispatch where
  -- Auto-dispatch using lazy instances constructed from evidence
  minpolyAlg : MinimalPolynomialAlgorithm F2x Eext
  minpolyAlg = lookupMinimalPolynomialAuto F2x Eext
    ⦃ functionFieldClassifiable F2x-isFunc ⦄
    ⦃ functionFieldClassifiable Eext-isFunc ⦄

  galoisAlg : GaloisGroupAlgorithm F2x Eext
  galoisAlg = lookupGaloisGroupAuto F2x Eext
    ⦃ functionFieldClassifiable F2x-isFunc ⦄
    ⦃ functionFieldClassifiable Eext-isFunc ⦄

  extDegAlg : FieldExtensionDegreeAlgorithm F2x Eext
  extDegAlg = lookupExtensionDegreeAuto F2x Eext
    ⦃ functionFieldClassifiable F2x-isFunc ⦄
    ⦃ functionFieldClassifiable Eext-isFunc ⦄

-- ============================================================================
-- Example 3: Fully Automatic Wrappers
-- ============================================================================

module FullyAutomaticWrappers where
  
  -- Define helpers that automatically find evidence instances
  module FunctionFieldOps where
    minimalPolynomial : {F E : FieldDeclaration}
                      → ⦃ evF : IsFunctionField F ⦄
                      → ⦃ evE : IsFunctionField E ⦄
                      → MinimalPolynomialAlgorithm F E
    minimalPolynomial {F} {E} ⦃ evF ⦄ ⦃ evE ⦄ = 
      lookupMinimalPolynomialAuto F E 
        ⦃ functionFieldClassifiable evF ⦄ 
        ⦃ functionFieldClassifiable evE ⦄
    
    galoisGroup : {F E : FieldDeclaration}
                → ⦃ evF : IsFunctionField F ⦄
                → ⦃ evE : IsFunctionField E ⦄
                → GaloisGroupAlgorithm F E
    galoisGroup {F} {E} ⦃ evF ⦄ ⦃ evE ⦄ = 
      lookupGaloisGroupAuto F E 
        ⦃ functionFieldClassifiable evF ⦄ 
        ⦃ functionFieldClassifiable evE ⦄
    
    extensionDegree : {F E : FieldDeclaration}
                    → ⦃ evF : IsFunctionField F ⦄
                    → ⦃ evE : IsFunctionField E ⦄
                    → FieldExtensionDegreeAlgorithm F E
    extensionDegree {F} {E} ⦃ evF ⦄ ⦃ evE ⦄ = 
      lookupExtensionDegreeAuto F E 
        ⦃ functionFieldClassifiable evF ⦄ 
        ⦃ functionFieldClassifiable evE ⦄
    
    splittingField : {F : FieldDeclaration}
                   → ⦃ evF : IsFunctionField F ⦄
                   → SplittingFieldAlgorithm F
    splittingField {F} ⦃ evF ⦄ = 
      lookupSplittingFieldAuto F 
        ⦃ functionFieldClassifiable evF ⦄
    
    algebraicityDecision : {F E : FieldDeclaration}
                         → ⦃ evF : IsFunctionField F ⦄
                         → ⦃ evE : IsFunctionField E ⦄
                         → AlgebraicityDecisionAlgorithm F E
    algebraicityDecision {F} {E} ⦃ evF ⦄ ⦃ evE ⦄ = 
      lookupAlgebraicityDecisionAuto F E 
        ⦃ functionFieldClassifiable evF ⦄ 
        ⦃ functionFieldClassifiable evE ⦄
  
  open FunctionFieldOps
  
  -- Usage is now COMPLETELY AUTOMATIC - evidence instances found by Agda!
  result-minpoly : MinimalPolynomialAlgorithm F2x Eext
  result-minpoly = minimalPolynomial  -- Fully automatic!
  
  result-galois : GaloisGroupAlgorithm F2x Eext
  result-galois = galoisGroup  -- Fully automatic!
  
  result-degree : FieldExtensionDegreeAlgorithm F2x Eext
  result-degree = extensionDegree  -- Fully automatic!
  
  result-splitting : SplittingFieldAlgorithm F2x
  result-splitting = splittingField  -- Fully automatic!
  
  -- Use these algorithms on concrete elements
  α : M.Identifier
  α = M.mkId "t"
  
  minpoly-of-t : M.Identifier
  minpoly-of-t = MinimalPolynomialAlgorithm.minimalPolynomial result-minpoly α
  
  is-algebraic : Dec (AlgebraicElement F2x Eext α)
  is-algebraic = AlgebraicityDecisionAlgorithm.isAlgebraic algebraicityDecision α

-- ============================================================================
-- Summary: Three Usage Tiers
-- ============================================================================

-- Tier 1: Manual classification (complete control)
--   lookupMinimalPolynomialWithClassification F2x Eext 
--     (classifyAsFunctionField F2x F2x-isFunc) 
--     (classifyAsFunctionField Eext Eext-isFunc)

-- Tier 2: Lazy instance construction (explicit evidence, lazy Classifiable)
--   lookupMinimalPolynomialAuto F2x Eext 
--     ⦃ functionFieldClassifiable F2x-isFunc ⦄ 
--     ⦃ functionFieldClassifiable Eext-isFunc ⦄

-- Tier 3: Wrapper functions with instance arguments (FULLY AUTOMATIC)
--   minimalPolynomial {F2x} {Eext}  -- Evidence instances found automatically!
