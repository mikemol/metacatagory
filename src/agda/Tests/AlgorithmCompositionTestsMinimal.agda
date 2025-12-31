{-# OPTIONS --without-K --allow-unsolved-metas #-}

module Tests.AlgorithmCompositionTestsMinimal where

  open import Metamodel as M
  open import Core.Phase
  open import Core.AlgebraicAlgorithms
  open import Core.UniversalProperties
  open import Core.Algorithms.Bundle
  open import Core.Algorithms.Registry using (FieldClassification)
  open import Core.TechnicalDebt
  open import Algebra.Rings.Basic using (FieldDeclaration)
  open import Algebra.Fields.Basic using (GaloisGroup; SplittingField)
  open import Agda.Builtin.List using (List; []; _∷_)
  open import Agda.Builtin.String using (String)
  open import Agda.Builtin.Equality using (_≡_; refl)

  postulate
    F-base : FieldDeclaration
    E-extension : FieldDeclaration
    poly-example : M.Identifier
    α-example : M.Identifier
    TestFixturesPackage : M.Identifier

  -- Phase 1: simple splitting field
  postulate
    splitAlg : SplittingFieldAlgorithm F-base

  test-split-output : SplittingField F-base poly-example
  test-split-output = SplittingFieldAlgorithm.splittingField splitAlg poly-example

  -- Phase 2: Two-Step Algorithm Composition
  postulate
    minPolyAlg : MinimalPolynomialAlgorithm F-base E-extension

  minPoly : M.Identifier
  minPoly = MinimalPolynomialAlgorithm.minimalPolynomial minPolyAlg α-example

  postulate
    splitAlg-phase2 : SplittingFieldAlgorithm F-base

  splittingField : SplittingField F-base minPoly
  splittingField = SplittingFieldAlgorithm.splittingField splitAlg-phase2 minPoly

  minPolyToResult : Phase M.Identifier M.Identifier
  minPolyToResult = mkPhase (λ x → MinimalPolynomialAlgorithm.minimalPolynomial minPolyAlg x)

  -- Phase 3: Three-Step Algorithm Pipeline
  postulate
    splitAlg-phase3 : SplittingFieldAlgorithm F-base

  splitting : SplittingField F-base poly-example
  splitting = SplittingFieldAlgorithm.splittingField splitAlg-phase3 poly-example

  postulate
    galoisAlg : GaloisGroupAlgorithm F-base E-extension

  galoisGroup : GaloisGroup F-base E-extension
  galoisGroup = GaloisGroupAlgorithm.galoisGroup galoisAlg poly-example

  automorphisms : M.Identifier
  automorphisms = M.mkId "automorphisms"

  fullPipeline : Phase M.Identifier M.Identifier
  fullPipeline = pipeline₃
    (mkPhase (λ p → p))
    (mkPhase (λ p → GaloisGroupAlgorithm.galoisGroup galoisAlg p))
    (mkPhase (λ _ → M.mkId "automorphisms"))

  -- Phase 4: Bundle-Based Algorithm Composition
  postulate
    bundle : AlgorithmBundle F-base E-extension

  minPolyAlg-bundle : MinimalPolynomialAlgorithm F-base E-extension
  minPolyAlg-bundle = AlgorithmBundle.minimalPolynomialAlg bundle

  galoisAlg-bundle : GaloisGroupAlgorithm F-base E-extension
  galoisAlg-bundle = AlgorithmBundle.galoisGroupAlg bundle

  test-bundle-composition : M.Identifier → GaloisGroup F-base E-extension
  test-bundle-composition element =
    let poly = MinimalPolynomialAlgorithm.minimalPolynomial minPolyAlg-bundle element
    in GaloisGroupAlgorithm.galoisGroup galoisAlg-bundle poly

  -- Phase 5: Invariant Preservation Through Composition
  identityInvariant : Invariant M.Identifier
  identityInvariant = record { property = λ _ → M.Identifier }

  postulate
    step1 : Phase M.Identifier M.Identifier
    step2 : Phase M.Identifier M.Identifier
    proof1 : (x : M.Identifier) → M.Identifier → M.Identifier
    proof2 : (x : M.Identifier) → M.Identifier → M.Identifier

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

  composedWithInvariant : PhaseWithInvariant M.Identifier M.Identifier
  composedWithInvariant = record
    { phase = step1 ⟫ step2
    ; invariantA = identityInvariant
    ; invariantB = identityInvariant
    ; preserves = λ x prop → proof2 (step1 $ₚ x) (proof1 x prop)
    }

  -- Phase 6: Universal Property Preservation
  postulate
    minPolyProp : MinimalPolynomialProperty F-base E-extension α-example
    splitProp : SplittingFieldProperty F-base poly-example

  minimalPoly : M.Identifier
  minimalPoly = MinimalPolynomialProperty.minPoly minPolyProp

  splittingFromProp : FieldDeclaration
  splittingFromProp = SplittingFieldProperty.splittingField splitProp

  postulate
    compositeProperty : SplittingFieldProperty F-base minimalPoly

  test-composite-ump : FieldDeclaration
  test-composite-ump = SplittingFieldProperty.splittingField compositeProperty

  -- Phase 7: Error Propagation Through Composition
  postulate
    step1Maybe : Phase M.Identifier (Maybe M.Identifier)
    step2Maybe : M.Identifier → Phase M.Identifier (Maybe M.Identifier)
    defaultValue : M.Identifier

  composedMaybe : Phase M.Identifier (Maybe M.Identifier)
  composedMaybe = step1Maybe >>=ₘ step2Maybe

  robustComposition : Phase M.Identifier M.Identifier
  robustComposition = fallback composedMaybe (constPhase defaultValue)

  -- Phase 8: Profiled Algorithm Composition
  postulate
    step1Profiled : Phase M.Identifier M.Identifier
    step2Profiled : Phase M.Identifier M.Identifier

  profiled1 : ProfiledPhase M.Identifier M.Identifier
  profiled1 = profile (annotate "Step 1: Compute Minimal Poly"
                                "Extract minimal polynomial"
                                step1Profiled)

  profiled2 : ProfiledPhase M.Identifier M.Identifier
  profiled2 = profile (annotate "Step 2: Build Splitting Field"
                                "Construct splitting field"
                                step2Profiled)

  profiledPipeline : Phase M.Identifier M.Identifier
  profiledPipeline = ProfiledPhase.phase profiled1 ⟫ ProfiledPhase.phase profiled2

  test-profiled-execution : M.Identifier → M.Identifier
  test-profiled-execution x =
    let result1 = ProfiledPhase.execute profiled1 x
    in ProfiledPhase.execute profiled2 result1

  -- Phase 9: Dependent Algorithm Composition
  postulate
    ResultType : FieldClassification F-base → Set₁
    step1Dep : DependentPhase (FieldClassification F-base) ResultType
    FinalType : (c : FieldClassification F-base) → ResultType c → Set₁
    step2Dep : (c : FieldClassification F-base) → DependentPhase (ResultType c) (FinalType c)

  dependentPipeline : DependentPhase (FieldClassification F-base)
                                     (λ c → FinalType c (step1Dep $ᵈ c))
  dependentPipeline = step1Dep ⟫ᵈ step2Dep

  -- Phase 10: DAG Compositional Path Validation
  open import Agda.Builtin.Nat as N using (Nat; suc)

  postulate
    minPolyAlg-10 : MinimalPolynomialAlgorithm F-base E-extension
    splitAlg-10 : SplittingFieldAlgorithm F-base
    galoisAlg-10 : GaloisGroupAlgorithm F-base E-extension
    alternativeMinPoly : M.Identifier

  step1-minPoly : M.Identifier
  step1-minPoly = MinimalPolynomialAlgorithm.minimalPolynomial minPolyAlg-10 α-example

  step2-splitting : SplittingField F-base step1-minPoly
  step2-splitting = SplittingFieldAlgorithm.splittingField splitAlg-10 step1-minPoly

  deriveAfter : M.Identifier → String → M.Identifier
  deriveAfter (M.mkIdWithCoord _ (M.mkCoord x y)) label =
    M.mkIdWithCoord label (M.mkCoord (N.suc x) y)

  step2-splittingId : M.Identifier
  step2-splittingId = deriveAfter step1-minPoly "SF(minPoly)"

  step3-galoisGroup : GaloisGroup F-base E-extension
  step3-galoisGroup = GaloisGroupAlgorithm.galoisGroup galoisAlg-10 step1-minPoly

  step3-galoisId : M.Identifier
  step3-galoisId = GaloisGroup.automorphisms step3-galoisGroup

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

  branch1-splitId : M.Identifier
  branch1-splitId = deriveAfter step1-minPoly "SF(minPoly)"

  branch2-splitId : M.Identifier
  branch2-splitId = deriveAfter alternativeMinPoly "SF(alt)"

  diamond-ordering-1 : Bool
  diamond-ordering-1 = branch1-splitId M.<ⁱ step3-galoisId

  diamond-ordering-2 : Bool
  diamond-ordering-2 = branch2-splitId M.<ⁱ step3-galoisId

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

  -- Technical Debt Registry
  TestFixturesPackageDebt : DebtAnnotation
  TestFixturesPackageDebt = mkDebt TestFixturesPackage "Test mocks for composition validation" "open" lowPriority

  technicalDebtRegistry : List DebtAnnotation
  technicalDebtRegistry = TestFixturesPackageDebt ∷ []

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
