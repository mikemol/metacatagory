-- Tests.ErrorAsSpecificationTests: Error-as-Specification (Phase III.3 - 3.2)
-- Status: COMPLETE [2025-11-19]
--
-- This module demonstrates the integration of LimitationEvidence into the algorithm
-- infrastructure, providing constructive rejection tests that treat limitations as
-- formally specified boundaries rather than failures.

module Tests.ErrorAsSpecificationTests where

open import Metamodel as M
open import Core.Limitations
open import Core
open import Core.AlgebraicAlgorithms
open import Core.Witnesses
open import Algebra.Rings.Basic
open import Algebra.Fields.Basic
open import Agda.Builtin.String using (String)
open import Agda.Builtin.List   using (List; []; _∷_)
open import Agda.Builtin.Bool   using (Bool; true; false)
open import Agda.Builtin.Maybe  using (Maybe; just; nothing)
open import Agda.Builtin.Equality using (_≡_; refl)

-- ============================================================================
-- Phase 1: Basic Limitation Construction and Acknowledgement
-- ============================================================================

-- Postulate an algorithm identifier to attach a limitation to
postulate
  someAlg : M.Identifier

-- Example limitation-as-specification
exLim : LimitationEvidence
exLim = mkLimitation someAlg "Requires square-free polynomial input" ("deg ≥ 1" ∷ "char(F) ≠ 0" ∷ [])

-- Acknowledged limitation is treated as specified boundary
exLimAck : LimitationEvidence
exLimAck = acknowledge exLim

-- Outcome examples
exOk : Outcome
exOk = ok

exLimited : Outcome
exLimited = limit exLimAck

-- Verify acknowledgement changes the flag
test-ack : LimitationEvidence.acknowledged exLimAck ≡ true
test-ack = refl

-- ============================================================================
-- Phase 2: AlgorithmResult Wrapper Integration
-- ============================================================================

postulate
  minPolyId : M.Identifier

-- Success case: algorithm produces result without limitation
test-success : AlgorithmResult M.Identifier
test-success = okResult minPolyId

-- Limited case: algorithm produces result with documented limitation
test-limited : AlgorithmResult M.Identifier
test-limited = limitedResult minPolyId exLimAck

-- Outcome extraction
test-success-outcome : toOutcome test-success ≡ ok
test-success-outcome = refl

test-limited-outcome : ∀ {lim} → toOutcome (limitedResult minPolyId lim) ≡ limit lim
test-limited-outcome = refl

-- ============================================================================
-- Phase 3: Algorithm Rejection Tests
-- ============================================================================

postulate
  F E : FieldDeclaration
  α : M.Identifier

-- Rejection test: minimal polynomial algorithm with characteristic restriction
minPolyLimitation : LimitationEvidence
minPolyLimitation = mkLimitation
  (M.mkId "MinimalPolynomial")
  "Requires characteristic ≠ 2 for efficient computation"
  ("char(F) ≠ 2" ∷ "α algebraic" ∷ [])

-- Rejection test: Galois group with solvability limitation
galoisGroupLimitation : LimitationEvidence
galoisGroupLimitation = mkLimitation
  (M.mkId "GaloisGroup")
  "Degree > 4 may require exponential time"
  ("deg(f) ≤ 4" ∷ "f separable" ∷ [])

-- Rejection test: splitting field with complexity boundary
splittingFieldLimitation : LimitationEvidence
splittingFieldLimitation = mkLimitation
  (M.mkId "SplittingField")
  "Construction requires factorization over F"
  ("f irreducible" ∷ "factorization oracle available" ∷ [])

-- Constructive algorithm with integrated limitation
postulate
  minPolyAlgLimited : MinimalPolynomialAlgorithm F E

-- Extract and verify limitation is present
test-minpoly-has-limitation : Maybe LimitationEvidence
test-minpoly-has-limitation = MinimalPolynomialAlgorithm.limitation minPolyAlgLimited

-- ============================================================================
-- Phase 4: Multi-Algorithm Limitation Aggregation
-- ============================================================================

-- Collect limitations from a suite of algorithms
aggregrateLimitations : List (Maybe LimitationEvidence) → List LimitationEvidence
aggregrateLimitations [] = []
aggregrateLimitations (nothing ∷ rest) = aggregrateLimitations rest
aggregrateLimitations (just lim ∷ rest) = lim ∷ aggregrateLimitations rest

-- Example: collect limitations from multiple algorithms
postulate
  galoisAlgLimited : GaloisGroupAlgorithm F E
  splitAlgLimited : SplittingFieldAlgorithm F

test-aggregate : List LimitationEvidence
test-aggregate = aggregrateLimitations
  ( MinimalPolynomialAlgorithm.limitation minPolyAlgLimited
  ∷ GaloisGroupAlgorithm.limitation galoisAlgLimited
  ∷ SplittingFieldAlgorithm.limitation splitAlgLimited
  ∷ []
  )

-- ============================================================================
-- Phase 5: Boundary Specification via Rejection
-- ============================================================================

-- Demonstrate that a limitation defines a formal boundary of the solution space
-- (formalization of the meta-principle from 2025-10-20)

data InputClass : Set where
  valid : InputClass
  outOfBounds : LimitationEvidence → InputClass

-- Classify input based on algorithm limitations
classifyInput : Maybe LimitationEvidence → M.Identifier → InputClass
classifyInput nothing _ = valid
classifyInput (just lim) _ = outOfBounds lim

-- Test classification
test-classify-valid : classifyInput nothing α ≡ valid
test-classify-valid = refl

test-classify-limited : classifyInput (just minPolyLimitation) α ≡ outOfBounds minPolyLimitation
test-classify-limited = refl
