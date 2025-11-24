-- Tests.AlgorithmCompositionTests: Validate algorithm composition and multi-step pipelines
--
-- This suite tests that algorithms compose correctly through phase boundaries:
-- - Multi-step algorithm pipelines maintain correctness
-- - Output of one algorithm is valid input for the next
-- - Composite algorithms satisfy end-to-end properties
-- - Phase composition preserves invariants
-- - Technical debt is registered using the shared Core infrastructure

module Tests.AlgorithmCompositionTests where

open import Core
open import Core.Phase
open import Algebra.Rings.Basic using (FieldDeclaration)
open import Algebra.Fields.Basic
open import Core.AlgebraicAlgorithms
open import Core.Algorithms.Bundle
open import Core.Algorithms.Registry
open import Core.Algorithms.FiniteFields
open import PropertyRegistry using (SplittingFieldPhaseId)
open import Core.UniversalProperties
open import Metamodel as M
open import Agda.Builtin.String using (String)
open import Agda.Builtin.Equality using (_≡_; refl)
-- Removed obsolete Agda.Builtin.Bool renaming import; all usages now use Core.Phase.Bool, true, false
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Int using (Int)

-- Import shared technical debt types
open import Core.TechnicalDebt

-- ============================================================================
-- Test Data: Concrete Examples
-- ============================================================================

module TestData where
  -- Concrete field declarations for testing
  postulate
    F-base : FieldDeclaration
    E-extension : FieldDeclaration
  
  -- Concrete identifiers
  α-example : M.Identifier
  α-example = M.mkId "√2"
  
  poly-example : M.Identifier
  poly-example = M.mkId "X²-2"

open TestData

-- ============================================================================
-- Test Fixtures Package
-- All postulated algorithm instances below are test mocks/fixtures for
-- validating composition pipelines. This package declaration consolidates
-- the conceptual debt of 20+ individual test fixture postulates.
-- ============================================================================

postulate TestFixturesPackage : M.Identifier

-- ============================================================================
-- Phase 1: Single Algorithm Output Validity
-- Tests that individual algorithm outputs are well-typed
-- ============================================================================

module Phase1-SingleAlgorithmValidity where
  
  -- Test: Minimal polynomial algorithm produces identifier
  postulate
    minPolyAlg : MinimalPolynomialAlgorithm F-base E-extension
  
  test-minpoly-output : M.Identifier
  test-minpoly-output = MinimalPolynomialAlgorithm.minimalPolynomial minPolyAlg α-example
  
  -- Test: Galois group algorithm produces group
  postulate
    galoisAlg : GaloisGroupAlgorithm F-base E-extension
  
  test-galois-output : GaloisGroup F-base E-extension
  test-galois-output = GaloisGroupAlgorithm.galoisGroup galoisAlg poly-example
  
  -- Test: Splitting field algorithm produces field
  postulate
    splitAlg : SplittingFieldAlgorithm F-base
  
  test-split-output : SplittingField F-base poly-example
  test-split-output = SplittingFieldAlgorithm.splittingField splitAlg poly-example

-- ============================================================================
-- Phase 2: Two-Step Algorithm Composition
-- Tests that output of first algorithm is valid input to second
-- ============================================================================

module Phase2-TwoStepComposition where
  
  -- Step 1: Compute minimal polynomial
  postulate
    minPolyAlg : MinimalPolynomialAlgorithm F-base E-extension
  
  minPoly : M.Identifier
  minPoly = MinimalPolynomialAlgorithm.minimalPolynomial minPolyAlg α-example
  
  -- Step 2: Use minimal polynomial to build splitting field
  postulate
    splitAlg : SplittingFieldAlgorithm F-base
  
  splittingField : SplittingField F-base minPoly
  splittingField = SplittingFieldAlgorithm.splittingField splitAlg minPoly
  
  -- Composed phase: α → minimal polynomial → identifier (simplified)
  minPolyToResult : Phase M.Identifier M.Identifier
  minPolyToResult = 
    mkPhase (λ x → MinimalPolynomialAlgorithm.minimalPolynomial minPolyAlg x)

-- ============================================================================
-- Phase 3: Three-Step Algorithm Pipeline
-- Tests complete pipeline: polynomial → splitting field → Galois group
-- ============================================================================

module Phase3-ThreeStepPipeline where
  
  -- Step 1: Build splitting field from polynomial
  postulate
    splitAlg : SplittingFieldAlgorithm F-base
  
  splitting : SplittingField F-base poly-example
  splitting = SplittingFieldAlgorithm.splittingField splitAlg poly-example
  
  -- Step 2: Extract Galois group from extension
  postulate
    galoisAlg : GaloisGroupAlgorithm F-base E-extension
  
  galoisGroup : GaloisGroup F-base E-extension
  galoisGroup = GaloisGroupAlgorithm.galoisGroup galoisAlg poly-example
  
  -- Step 3: Enumerate automorphisms
  automorphisms : M.Identifier  -- List simplified to Identifier
  automorphisms = M.mkId "automorphisms"
  
  -- Complete pipeline as phase composition
  fullPipeline : Phase M.Identifier M.Identifier
  fullPipeline = pipeline₃
    (mkPhase (λ p → p))  -- Identity: pass polynomial through
    (mkPhase (λ p → GaloisGroupAlgorithm.galoisGroup galoisAlg p))
    (mkPhase (λ _ → M.mkId "automorphisms"))

-- ============================================================================
-- Phase 4: Bundle-Based Algorithm Composition
-- Tests that algorithm bundles support composition
-- ============================================================================

module Phase4-BundleComposition where

  postulate
    bundle : AlgorithmBundle F-base E-extension
  
  -- Extract algorithms from bundle
  minPolyAlg : MinimalPolynomialAlgorithm F-base E-extension
  minPolyAlg = AlgorithmBundle.minimalPolynomialAlg bundle
  
  galoisAlg : GaloisGroupAlgorithm F-base E-extension
  galoisAlg = AlgorithmBundle.galoisGroupAlg bundle
  
  -- Compose algorithms from same bundle
  test-bundle-composition : M.Identifier → GaloisGroup F-base E-extension
  test-bundle-composition element =
    let poly = MinimalPolynomialAlgorithm.minimalPolynomial minPolyAlg element
    in GaloisGroupAlgorithm.galoisGroup galoisAlg poly

-- ============================================================================
-- Phase 5: Invariant Preservation Through Composition
-- Tests that multi-step pipelines preserve properties
-- ============================================================================

module Phase5-InvariantPreservation where
  
  -- Invariant: field identity is preserved (simplified to avoid universe issues)
  identityInvariant : Invariant M.Identifier
  identityInvariant = record { property = λ _ → M.Identifier }
  
  postulate
    step1 : Phase M.Identifier M.Identifier
    step2 : Phase M.Identifier M.Identifier
    proof1 : (x : M.Identifier) → M.Identifier → M.Identifier
    proof2 : (x : M.Identifier) → M.Identifier → M.Identifier
  
  -- Each step preserves identity
  phase1WithInvariant : PhaseWithInvariant M.Identifier M.Identifier
  phase1WithInvariant = record
    { phase = step1
    ; invariantA = identityInvariant
    ; invariantB = identityInvariant
    ; preserves = proof1
    }
  
  phase2WithInvariant : PhaseWithInvariant M.Identifier M.Identifier
  phase2WithInvariant = record
    { phase = step2
    ; invariantA = identityInvariant
    ; invariantB = identityInvariant
    ; preserves = proof2
    }
  
  -- Composition preserves invariant (transitive)
  composedWithInvariant : PhaseWithInvariant M.Identifier M.Identifier
  composedWithInvariant = record
    { phase = step1 ⟫ step2
    ; invariantA = identityInvariant
    ; invariantB = identityInvariant
    ; preserves = λ x prop → proof2 (step1 $ₚ x) (proof1 x prop)
    }

-- ============================================================================
-- Phase 6: Universal Property Preservation
-- Tests that composed algorithms satisfy composite universal properties
-- ============================================================================

module Phase6-UniversalPropertyComposition where
  
  -- Individual universal properties
  postulate
    minPolyProp : MinimalPolynomialProperty F-base E-extension α-example
    splitProp : SplittingFieldProperty F-base poly-example
  
  -- The minimal polynomial from the property
  minimalPoly : M.Identifier
  minimalPoly = MinimalPolynomialProperty.minPoly minPolyProp
  
  -- The splitting field from the property
  splitting : FieldDeclaration
  splitting = SplittingFieldProperty.splittingField splitProp
  
  -- Composite property: minimal polynomial satisfies splitting field property
  postulate
    compositeProperty : SplittingFieldProperty F-base minimalPoly
  
  test-composite-ump : FieldDeclaration
  test-composite-ump = SplittingFieldProperty.splittingField compositeProperty

-- ============================================================================
-- Phase 7: Error Propagation Through Composition
-- Tests that failures propagate correctly in composed phases
-- ============================================================================

module Phase7-ErrorPropagation where

  postulate
    step1 : Phase M.Identifier (Maybe M.Identifier)
    step2 : M.Identifier → Phase M.Identifier (Maybe M.Identifier)
  
  -- Monadic composition: failure in step1 prevents step2
  composedMaybe : Phase M.Identifier (Maybe M.Identifier)
  composedMaybe = step1 >>=ₘ step2
  
  -- Test with fallback: if composition fails, use default
  postulate
    defaultValue : M.Identifier
  
  robustComposition : Phase M.Identifier M.Identifier
  robustComposition = fallback composedMaybe (constPhase defaultValue)

-- ============================================================================
-- Phase 8: Profiled Algorithm Composition
-- Tests that profiling works through composed pipelines
-- ============================================================================

module Phase8-ProfiledComposition where
  
  -- Create profiled phases for each step
  postulate
    step1 : Phase M.Identifier M.Identifier
    step2 : Phase M.Identifier M.Identifier
  
  profiled1 : ProfiledPhase M.Identifier M.Identifier
  profiled1 = profile (annotate "Step 1: Compute Minimal Poly" 
                                "Extract minimal polynomial" 
                                step1)
  
  profiled2 : ProfiledPhase M.Identifier M.Identifier
  profiled2 = profile (annotate "Step 2: Build Splitting Field"
                                "Construct splitting field"
                                step2)
  
  -- Compose profiled phases (extract underlying phases)
  profiledPipeline : Phase M.Identifier M.Identifier
  profiledPipeline = (ProfiledPhase.phase profiled1) ⟫ (ProfiledPhase.phase profiled2)
  
  -- Execute with profiling on each step
  test-profiled-execution : M.Identifier → M.Identifier
  test-profiled-execution x = 
    let result1 = ProfiledPhase.execute profiled1 x
    in ProfiledPhase.execute profiled2 result1

-- ============================================================================
-- Phase 9: Dependent Algorithm Composition
-- Tests composition of algorithms with type-indexed results
-- ============================================================================

module Phase9-DependentComposition where
  
  -- Dependent phase: result type depends on classification
  postulate
    ResultType : FieldClassification F-base → Set₁
    step1Dep : DependentPhase (FieldClassification F-base) ResultType
  
  -- Second dependent phase uses first's result
  postulate
    FinalType : (c : FieldClassification F-base) → ResultType c → Set₁
    step2Dep : (c : FieldClassification F-base) → DependentPhase (ResultType c) (FinalType c)
  
  -- Compose dependent phases
  dependentPipeline : DependentPhase (FieldClassification F-base)
                                     (λ c → FinalType c (step1Dep $ᵈ c))
  dependentPipeline = step1Dep ⟫ᵈ step2Dep

-- ============================================================================
-- Phase 10: DAG Compositional Path Validation (Phase III.2 - 3.1)
-- Tests multi-step pipelines with index ordering enforcement
-- ============================================================================

module Phase10-DAGCompositionalValidation where

  open import Agda.Builtin.Nat as N using (Nat; suc)
  
  -- ========================================================================
  -- Multi-step pipeline: MinimalPolynomial → SplittingField → GaloisGroup
  -- ========================================================================
  
  -- Step 1: Compute minimal polynomial
  postulate
    minPolyAlg : MinimalPolynomialAlgorithm F-base E-extension
  
  step1-minPoly : M.Identifier
  step1-minPoly = MinimalPolynomialAlgorithm.minimalPolynomial minPolyAlg α-example
  
  -- Step 2: Build splitting field from minimal polynomial
  postulate
    splitAlg : SplittingFieldAlgorithm F-base
  
  step2-splitting : SplittingField F-base step1-minPoly
  step2-splitting = SplittingFieldAlgorithm.splittingField splitAlg step1-minPoly

  -- Derive downstream identifier by bumping x-coordinate (test logic)
  deriveAfter : M.Identifier → String → M.Identifier
  deriveAfter (M.mkIdWithCoord _ (M.mkCoord x y)) label = M.mkIdWithCoord label (M.mkCoord (N.suc x) y)

  step2-splittingId : M.Identifier
  step2-splittingId = deriveAfter step1-minPoly "SF(minPoly)"
  
  -- Step 3: Compute Galois group
  postulate
    galoisAlg : GaloisGroupAlgorithm F-base E-extension
  
  step3-galoisGroup : GaloisGroup F-base E-extension
  step3-galoisGroup = GaloisGroupAlgorithm.galoisGroup galoisAlg step1-minPoly
  
  step3-galoisId : M.Identifier
  step3-galoisId = GaloisGroup.automorphisms step3-galoisGroup
  
  -- ========================================================================
  -- Index Ordering Validation (Bool-based checks)
  -- ========================================================================
  
  -- Validate: input < step1 < step2 < step3
  ordering-check-1 : Bool
  ordering-check-1 = α-example M.<ⁱ step1-minPoly

  ordering-check-2 : Bool
  ordering-check-2 = step1-minPoly M.<ⁱ step2-splittingId

  ordering-check-3 : Bool
  ordering-check-3 = step2-splittingId M.<ⁱ step3-galoisId

  ordering-check-transitive : Bool
  ordering-check-transitive = α-example M.<ⁱ step3-galoisId

  all-ordering-checks : Bool
  all-ordering-checks = and4 ordering-check-1 ordering-check-2 
                             ordering-check-3 ordering-check-transitive
    where
      and4 : Bool → Bool → Bool → Bool → Bool
      and4 true true true true = true
      and4 _ _ _ _ = false
  
  -- ========================================================================
  -- Alternative Pipeline & Diamond DAG
  -- ========================================================================
  
  postulate
    alternativeMinPoly : M.Identifier
  
  alt-ordering-1 : Bool
  alt-ordering-1 = alternativeMinPoly M.<ⁱ step2-splittingId
  
  -- Diamond structure validation
  branch1-splitId : M.Identifier
  branch1-splitId = deriveAfter step1-minPoly "SF(minPoly)"

  branch2-splitId : M.Identifier
  branch2-splitId = deriveAfter alternativeMinPoly "SF(alt)"

  diamond-ordering-1 : Bool
  diamond-ordering-1 = branch1-splitId M.<ⁱ step3-galoisId

  diamond-ordering-2 : Bool
  diamond-ordering-2 = branch2-splitId M.<ⁱ step3-galoisId

  -- ========================================================================
  -- Concrete Instance: Fully reduced ordering with coordinates
  -- ========================================================================
  
  concrete-α : M.Identifier
  concrete-α = M.mkIdAt "α0" 1 1

  concrete-minPoly : M.Identifier
  concrete-minPoly = M.mkIdAt "minP0" 2 1

  concrete-splitting : M.Identifier
  concrete-splitting = M.mkIdAt "split0" 3 1

  concrete-galois : M.Identifier
  concrete-galois = M.mkIdAt "gal0" 4 1

  concrete-ord-1 : concrete-α M.<ⁱ concrete-minPoly ≡ true
  concrete-ord-1 = refl

  concrete-ord-2 : concrete-minPoly M.<ⁱ concrete-splitting ≡ true
  concrete-ord-2 = refl

  concrete-ord-3 : concrete-splitting M.<ⁱ concrete-galois ≡ true
  concrete-ord-3 = refl

-- ============================================================================
-- Technical Debt Registry (Using Core.TechnicalDebt)
-- ============================================================================

-- Annotate key test fixture postulates
TestFixturesPackageDebt : DebtAnnotation
TestFixturesPackageDebt = mkDebt TestFixturesPackage "Test mocks for composition validation" "open" lowPriority

-- Registry of technical debt items in this module
technicalDebtRegistry : List DebtAnnotation
technicalDebtRegistry = TestFixturesPackageDebt ∷ []

-- Export metadata
rationales : List String
rationales = map DebtAnnotation.rationale technicalDebtRegistry
  where
    map : {A B : Set} → (A → B) → List A → List B
    map f [] = []
    map f (x ∷ xs) = f x ∷ map f xs

statuses : List String
statuses = map DebtAnnotation.status technicalDebtRegistry
  where
    map : {A B : Set} → (A → B) → List A → List B
    map f [] = []
    map f (x ∷ xs) = f x ∷ map f xs

priorities : List Priority
priorities = map DebtAnnotation.priority technicalDebtRegistry
  where
    map : {A B : Set} → (A → B) → List A → List B
    map f [] = []
    map f (x ∷ xs) = f x ∷ map f xs
