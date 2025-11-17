-- Tests.AdvancedPhaseExamples: Demonstrating advanced Phase features
--
-- This module shows the extended capabilities of the Phase abstraction:
-- - Dependent phases (type-indexed transformations)
-- - Phase invariants (property preservation)
-- - Combinators (retry, fallback, monadic chaining)
-- - Profiling (execution metadata and hooks)

module Tests.AdvancedPhaseExamples where

open import Core.Phase
open import Core
open import Algebra.Rings.Basic using (FieldDeclaration)
open import Algebra.Fields.Basic
open import Core.AlgebraicAlgorithms
open import Core.Algorithms.Bundle
open import Core.Algorithms.Registry
open import Core.Algorithms.FiniteFields
open import Metamodel as M
open import Agda.Builtin.Equality using (_≡_; refl)
open import Agda.Builtin.String using (String)

-- ============================================================================
-- Example 1: Dependent Phases for Type-Indexed Dispatch
-- ============================================================================

module DependentPhaseExample where

  postulate
    F E : FieldDeclaration
    
  -- The bundle type depends on which classification we have
  -- In reality, AlgorithmBundle is parameterized by F E, so this is simplified
  
  -- A dependent phase where the result type varies by input
  postulate
    TypeIndexedResult : FieldClassification F → Set₁
    
  -- Example: dispatch returns different types based on classification
  postulate
    dispatchDependent : DependentPhase (FieldClassification F) TypeIndexedResult
  
  -- Dependent composition example
  postulate
    SecondStage : (c : FieldClassification F) → TypeIndexedResult c → Set₁
    processResult : (c : FieldClassification F) → DependentPhase (TypeIndexedResult c) (SecondStage c)
  
  -- Compose dependent phases
  fullDepPipeline : DependentPhase (FieldClassification F) 
                                   (λ c → SecondStage c (dispatchDependent $ᵈ c))
  fullDepPipeline = dispatchDependent ⟫ᵈ processResult

-- ============================================================================
-- Example 2: Phase Invariants for Property Preservation
-- ============================================================================

module InvariantExample where

  postulate
    F E : FieldDeclaration
    
  -- Define an invariant: field degree is preserved
  degreeInvariant : Invariant (AlgorithmBundle F E)
  degreeInvariant = record
    { property = λ bundle → ExtensionDegree F E  -- Simplified: actual property would be richer
    }
  
  -- A phase that preserves field degree
  postulate
    transformBundle : Phase (AlgorithmBundle F E) (AlgorithmBundle F E)
    proof-preserves-degree : (b : AlgorithmBundle F E) 
                           → ExtensionDegree F E 
                           → ExtensionDegree F E
  
  bundlePhaseWithInvariant : PhaseWithInvariant (AlgorithmBundle F E) (AlgorithmBundle F E)
  bundlePhaseWithInvariant = record
    { phase = transformBundle
    ; invariantA = degreeInvariant
    ; invariantB = degreeInvariant
    ; preserves = proof-preserves-degree
    }
  
  -- Use the invariant-preserving phase
  test-with-invariant : AlgorithmBundle F E → AlgorithmBundle F E
  test-with-invariant = PhaseWithInvariant.phase bundlePhaseWithInvariant $ₚ_

-- ============================================================================
-- Example 3: Retry and Fallback Combinators
-- ============================================================================

module CombinatorExample where

  postulate
    F E : FieldDeclaration
  
  -- A phase that might fail (returns Maybe)
  postulate
    tryComputeMinPoly : Phase M.Identifier (Maybe M.Identifier)
  
  -- Retry up to 3 times
  minPolyWithRetry : Phase M.Identifier (Maybe M.Identifier)
  minPolyWithRetry = retry (suc (suc (suc zero))) tryComputeMinPoly
  
  -- Fallback to default if computation fails
  postulate
    defaultMinPoly : Phase M.Identifier M.Identifier
  
  minPolyWithFallback : Phase M.Identifier M.Identifier
  minPolyWithFallback = fallback tryComputeMinPoly defaultMinPoly
  
  -- Monadic chaining: compute minimal polynomial, then splitting field
  postulate
    trySplittingField : M.Identifier → Phase M.Identifier (Maybe M.Identifier)
  
  minPolyThenSplitting : Phase M.Identifier (Maybe M.Identifier)
  minPolyThenSplitting = tryComputeMinPoly >>=ₘ trySplittingField
  
  -- Test the combinators
  test-retry : M.Identifier → Maybe M.Identifier
  test-retry = minPolyWithRetry $ₚ_
  
  test-fallback : M.Identifier → M.Identifier
  test-fallback = minPolyWithFallback $ₚ_

-- ============================================================================
-- Example 4: Profiled Phases for Performance Analysis
-- ============================================================================

module ProfilingExample where

  postulate
    F E : FieldDeclaration
    F-ev : IsFiniteField F
    E-ev : IsFiniteField E
  
  -- Create an annotated phase
  classifyAnnotated : AnnotatedPhase (IsFiniteField F) (FieldClassification F)
  classifyAnnotated = annotate
    "Finite Field Classification"
    "Convert evidence to classification for finite field types"
    (mkPhase (classifyAsFiniteField F))
  
  -- Add profiling to track execution
  classifyProfiled : ProfiledPhase (IsFiniteField F) (FieldClassification F)
  classifyProfiled = profile classifyAnnotated
  
  -- Execute with profiling hooks
  test-profiled : IsFiniteField F → FieldClassification F
  test-profiled = ProfiledPhase.execute classifyProfiled
  
  -- The metadata is accessible
  test-metadata : String
  test-metadata = ExecutionMetadata.phaseName (ProfiledPhase.metadata classifyProfiled)

-- ============================================================================
-- Example 5: Lifting Regular Phases to Dependent Context
-- ============================================================================

module LiftingExample where

  postulate
    F E : FieldDeclaration
    regularPhase : Phase (FieldClassification F) (AlgorithmBundle F E)
  
  -- Lift to dependent phase (constant family)
  liftedPhase : DependentPhase (FieldClassification F) (λ _ → AlgorithmBundle F E)
  liftedPhase = liftPhase regularPhase
  
  -- Can now compose with other dependent phases
  postulate
    dependentContinuation : (c : FieldClassification F) 
                          → DependentPhase (AlgorithmBundle F E) (λ _ → M.Identifier)
  
  composedLifted : DependentPhase (FieldClassification F) 
                                  (λ c → M.Identifier)
  composedLifted = liftedPhase ⟫ᵈ dependentContinuation

-- ============================================================================
-- Example 6: Practical Use Case - Robust Algorithm Pipeline
-- ============================================================================

module RobustPipelineExample where

  postulate
    F E : FieldDeclaration
    α : M.Identifier
  
  -- Step 1: Try to compute minimal polynomial (might fail)
  postulate
    tryMinPoly : Phase M.Identifier (Maybe M.Identifier)
    defaultPoly : M.Identifier
  
  step1 : Phase M.Identifier M.Identifier
  step1 = fallback tryMinPoly (constPhase defaultPoly)
  
  -- Step 2: Compute next identifier (annotated for debugging)
  postulate
    computeNext : Phase M.Identifier M.Identifier
  
  step2 : AnnotatedPhase M.Identifier M.Identifier
  step2 = annotate
    "Next Computation"
    "Build next result from minimal polynomial"
    computeNext
  
  -- Step 3: Extract final result (profiled for performance analysis)
  postulate
    extractFinal : Phase M.Identifier M.Identifier
  
  step3 : ProfiledPhase M.Identifier M.Identifier
  step3 = profile (annotate "Final Extraction" 
                            "Extract final result"
                            extractFinal)
  
  -- Complete robust pipeline
  robustPipeline : Phase M.Identifier M.Identifier
  robustPipeline = step1 ⟫ (AnnotatedPhase.phase step2) ⟫ (ProfiledPhase.phase step3)
  
  -- Execute the pipeline
  test-robust : M.Identifier → M.Identifier
  test-robust = robustPipeline $ₚ_

-- ============================================================================
-- Summary
-- ============================================================================

-- Advanced Phase features provide:
-- 1. **Dependent phases**: Type-level dispatch and indexed transformations
-- 2. **Invariants**: Explicit property preservation with proofs
-- 3. **Combinators**: Retry, fallback, monadic composition for robust pipelines
-- 4. **Profiling**: Execution metadata and hooks for performance analysis
-- 5. **Lifting**: Convert regular phases to dependent context when needed
--
-- These enable building sophisticated, type-safe transformation pipelines
-- with error handling, property preservation, and performance tracking.

