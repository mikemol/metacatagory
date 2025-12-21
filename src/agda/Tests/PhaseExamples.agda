{-# OPTIONS --without-K #-}

-- Tests.PhaseExamples: Demonstrating formal Phase abstraction in tests
--
-- This module shows how the Phase type formalizes behavioral boundaries
-- that were previously implicit in our test organization.

module Tests.PhaseExamples where

open import Core.Phase
open import Core
open import Algebra.Rings.Basic using (FieldDeclaration)
open import Algebra.Fields.Basic
open import Core.AlgebraicAlgorithms
open import Core.Algorithms.Bundle
open import Core.Algorithms.Registry
open import Core.Algorithms.FiniteFields
open import Core.Algorithms.NumberFields
open import Metamodel as M
open import Agda.Builtin.Equality using (_≡_; refl)

-- ============================================================================
-- Example 1: Dispatch Pipeline as Formal Phases
-- ============================================================================

module DispatchPipelinePhases where

  postulate
    F E : FieldDeclaration
    F-ev : IsFiniteField F
    E-ev : IsFiniteField E

  -- Phase 1: Evidence → Classification
  evidenceToClassification : Phase (IsFiniteField F) (FieldClassification F)
  evidenceToClassification = mkPhase (classifyAsFiniteField F)

  -- Phase 2: Classification → Bundle (fix E via a captured classification)
  classificationToBundle : Phase (FieldClassification F) (AlgorithmBundle F E)
  classificationToBundle = mkPhase (λ cF →
    let cE = classifyAsFiniteField E E-ev in dispatchBundle F E cF cE)

  -- Complete pipeline: Evidence → Bundle (two phases composed)
  evidenceToBundle : Phase (IsFiniteField F) (AlgorithmBundle F E)
  evidenceToBundle = evidenceToClassification ⟫ classificationToBundle

  -- Test the pipeline
  test-pipeline : AlgorithmBundle F E
  test-pipeline = evidenceToBundle $ₚ F-ev

  -- Verify associativity: add a third phase (bundle → some result)
  postulate
    extractAlgorithm : AlgorithmBundle F E → M.Identifier

  extractPhase : Phase (AlgorithmBundle F E) M.Identifier
  extractPhase = mkPhase extractAlgorithm

  -- Three-phase pipeline
  fullPipeline : Phase (IsFiniteField F) M.Identifier
  fullPipeline = evidenceToClassification ⟫ classificationToBundle ⟫ extractPhase

  -- Equivalent via pipeline helper
  fullPipeline' : Phase (IsFiniteField F) M.Identifier
  fullPipeline' = pipeline₃ evidenceToClassification classificationToBundle extractPhase

-- ============================================================================
-- Example 2: Annotated Phases for Documentation
-- ============================================================================

module AnnotatedPhaseExample where

  postulate
    F E : FieldDeclaration
    F-ev : IsFiniteField F
    E-ev : IsFiniteField E

  -- Same phases as Example 1, but with annotations
  evidencePhase : AnnotatedPhase (IsFiniteField F) (FieldClassification F)
  evidencePhase = annotate
    "Evidence Classification"
    "Converts IsFiniteField evidence to FieldClassification tagged union"
    (mkPhase (classifyAsFiniteField F))

  dispatchPhase : AnnotatedPhase (FieldClassification F) (AlgorithmBundle F E)
  dispatchPhase = annotate
    "Bundle Dispatch"
    "Routes classification to appropriate algorithm bundle"
    (mkPhase (λ cF → let cE = classifyAsFiniteField E E-ev in dispatchBundle F E cF cE))

  -- Use annotated phases
  test-annotated-evidence : FieldClassification F
  test-annotated-evidence = AnnotatedPhase.apply evidencePhase F-ev

  test-annotated-dispatch : AlgorithmBundle F E
  test-annotated-dispatch =
    AnnotatedPhase.apply dispatchPhase (AnnotatedPhase.apply evidencePhase F-ev)

-- ============================================================================
-- Example 3: Identity and Constant Phases
-- ============================================================================

module IdentityPhaseExample where

  postulate
    F E : FieldDeclaration
    testClassification : FieldClassification F

  -- Identity phase: no transformation
  noTransform : Phase (FieldClassification F) (FieldClassification F)
  noTransform = idPhase

  -- Verify identity laws
  test-left-identity : FieldClassification F
  test-left-identity = (idPhase ⟫ idPhase) $ₚ testClassification

  test-right-identity : FieldClassification F
  test-right-identity = (noTransform ⟫ idPhase) $ₚ testClassification

  -- Constant phase: always returns same value
  postulate
    defaultBundle : AlgorithmBundle F E

  constantBundle : Phase (FieldClassification F) (AlgorithmBundle F E)
  constantBundle = constPhase defaultBundle

  test-constant : AlgorithmBundle F E
  test-constant = constantBundle $ₚ testClassification

-- ============================================================================
-- Example 4: Conditional Phases
-- ============================================================================

module ConditionalPhaseExample where

  postulate
    F E : FieldDeclaration

  -- Predicate: is classification for a finite field? (opaque for this example)
  postulate
    isFiniteFieldClass : FieldClassification F → Bool

  postulate
    fastAlgorithms : AlgorithmBundle F E
    slowAlgorithms : AlgorithmBundle F E

  -- Use fast algorithms for finite fields, slow otherwise
  optimizedDispatch : Phase (FieldClassification F) (AlgorithmBundle F E)
  optimizedDispatch = conditional
    isFiniteFieldClass
    (constPhase fastAlgorithms)
    (constPhase slowAlgorithms)

  -- Test conditional dispatch
  postulate
    finiteClass : FieldClassification F
    numberClass : FieldClassification F

  test-fast-path : AlgorithmBundle F E
  test-fast-path = optimizedDispatch $ₚ finiteClass

  test-slow-path : AlgorithmBundle F E
  test-slow-path = optimizedDispatch $ₚ numberClass

-- ============================================================================
-- Example 5: Phase Laws and Properties
-- ============================================================================

module PhaseLawsExample where

  postulate
    F E : FieldDeclaration
    ev : IsFiniteField F
    E-ev : IsFiniteField E

  -- Define three phases
  p1 : Phase (IsFiniteField F) (FieldClassification F)
  p1 = mkPhase (classifyAsFiniteField F)

  p2 : Phase (FieldClassification F) (AlgorithmBundle F E)
  p2 = mkPhase (λ cF → let cE = classifyAsFiniteField E E-ev in dispatchBundle F E cF cE)

  postulate
    extractId : AlgorithmBundle F E → M.Identifier

  p3 : Phase (AlgorithmBundle F E) M.Identifier
  p3 = mkPhase extractId

  -- Left identity: idPhase ⟫ p ≡ p
  test-left-id : (idPhase ⟫ p1) $ₚ ev ≡ p1 $ₚ ev
  test-left-id = left-identity p1 ev

  -- Right identity: p ⟫ idPhase ≡ p
  test-right-id : (p1 ⟫ idPhase) $ₚ ev ≡ p1 $ₚ ev
  test-right-id = right-identity p1 ev

  -- Associativity: (p1 ⟫ p2) ⟫ p3 ≡ p1 ⟫ (p2 ⟫ p3)
  test-assoc : ((p1 ⟫ p2) ⟫ p3) $ₚ ev ≡ (p1 ⟫ (p2 ⟫ p3)) $ₚ ev
  test-assoc = associativity p1 p2 p3 ev

-- ============================================================================
-- Summary
-- ============================================================================

-- The Phase abstraction provides:
-- 1. Type-safe composition of transformations
-- 2. Formal statement of identity and associativity laws
-- 3. Conditional branching
-- 4. Metadata/documentation via AnnotatedPhase
-- 5. Provable properties about pipelines
--
-- This formalizes what was previously implicit in our test suite organization.
-- Note: Parallel composition (⊗) exists but has product type conflicts with Core._×_
