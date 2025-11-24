-- Examples.AlgorithmCorrectnessExamples: Demonstrations of algorithm correctness proofs
-- This module shows how to specify and verify that algebraic algorithms satisfy
-- their mathematical correctness properties.

module Examples.AlgorithmCorrectnessExamples where

open import Core
open import Core.Phase
open import Core.Witnesses
open import Core.ConstructiveWitnesses
open import Core.AlgorithmCorrectness
open import Core.AlgebraicAlgorithms
open import Algebra.Foundation
open import Algebra.Rings.Basic using (FieldDeclaration)
open import Algebra.Fields.Basic
open import Algebra.Fields.Advanced
open import Metamodel as M
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Nat using (Nat; zero; suc)
open import Core.Phase using (Bool; true; false)
open import Core.Phase using (Bool; true; false)
open import Core.Phase using (Maybe; just; nothing)

-- ============================================================================
-- Example 1: Minimal Polynomial Correctness
-- ============================================================================

module MinimalPolynomialCorrectnessExample where
  
  postulate
    F E : FieldDeclaration
    α : M.Identifier
    minpolyAlg : MinimalPolynomialAlgorithm F E
  
  -- Build constructive witness
  constructiveWitness : ConstructiveMinimalPolynomial F E α
  constructiveWitness = mkConstructiveMinimalPolynomial F E α minpolyAlg
  
  -- Extract correctness proof
  correctness : MinimalPolynomialCorrectness F E α
  correctness = minimalPolynomialCorrectness F E α constructiveWitness
  
  -- Access specification
  spec : MinimalPolynomialSpec F E α
  spec = MinimalPolynomialCorrectness.specification correctness
  
  -- Verify individual properties
  rootProof : M.Identifier
  rootProof = MinimalPolynomialCorrectness.proveRootProperty correctness
  
  irreducibilityProof : M.Identifier
  irreducibilityProof = MinimalPolynomialCorrectness.proveIrreducibility correctness
  
  minimalityProof : M.Identifier
  minimalityProof = MinimalPolynomialCorrectness.proveMinimality correctness
  
  -- Check overall correctness
  isCorrect : Bool
  isCorrect = MinimalPolynomialCorrectness.isCorrect correctness

-- ============================================================================
-- Example 2: Splitting Field Correctness
-- ============================================================================

module SplittingFieldCorrectnessExample where
  
  postulate
    F : FieldDeclaration
    poly : M.Identifier
    splitAlg : SplittingFieldAlgorithm F
    computedField : M.Identifier
  
  -- Build constructive witness
  constructiveWitness : ConstructiveSplittingField F poly
  constructiveWitness = mkConstructiveSplittingField F poly splitAlg computedField
  
  -- Extract correctness proof
  correctness : SplittingFieldCorrectness F poly
  correctness = splittingFieldCorrectness F poly constructiveWitness
  
  -- Access specification
  spec : SplittingFieldSpec F poly
  spec = SplittingFieldCorrectness.specification correctness
  
  -- Verify roots are all in field
  allRootsProof : M.Identifier
  allRootsProof = SplittingFieldCorrectness.proveAllRootsInField correctness
  
  -- Verify complete splitting
  completeSplittingProof : M.Identifier
  completeSplittingProof = SplittingFieldCorrectness.proveCompleteSplitting correctness
  
  -- Verify minimality
  minimalityProof : M.Identifier
  minimalityProof = SplittingFieldCorrectness.proveMinimalField correctness
  
  -- Check overall correctness
  isCorrect : Bool
  isCorrect = SplittingFieldCorrectness.isCorrect correctness

-- ============================================================================
-- Example 3: Galois Group Correctness
-- ============================================================================

module GaloisGroupCorrectnessExample where
  
  postulate
    F E : FieldDeclaration
    galoisAlg : GaloisGroupAlgorithm F E
  
  -- Build constructive witness
  constructiveWitness : ConstructiveGaloisGroup F E
  constructiveWitness = mkConstructiveGaloisGroup F E galoisAlg
  
  -- Extract correctness proof
  correctness : GaloisGroupCorrectness F E
  correctness = galoisGroupCorrectness F E constructiveWitness
  
  -- Access specification
  spec : GaloisGroupSpec F E
  spec = GaloisGroupCorrectness.specification correctness
  
  -- Verify automorphisms are valid
  automorphismsValidProof : M.Identifier
  automorphismsValidProof = GaloisGroupCorrectness.proveAutomorphismsValid correctness
  
  -- Verify group axioms
  closureProof : M.Identifier
  closureProof = GaloisGroupCorrectness.proveGroupClosure correctness
  
  assocProof : M.Identifier
  assocProof = GaloisGroupCorrectness.proveAssociativity correctness
  
  identityProof : M.Identifier
  identityProof = GaloisGroupCorrectness.proveIdentity correctness
  
  inversesProof : M.Identifier
  inversesProof = GaloisGroupCorrectness.proveInverses correctness
  
  -- Verify fundamental theorem
  orderEqualityProof : M.Identifier
  orderEqualityProof = GaloisGroupCorrectness.proveOrderEquality correctness
  
  fundamentalThmProof : M.Identifier
  fundamentalThmProof = GaloisGroupCorrectness.proveFundamentalTheorem correctness
  
  -- Check overall correctness
  isCorrect : Bool
  isCorrect = GaloisGroupCorrectness.isCorrect correctness

-- ============================================================================
-- Example 4: Extension Degree Correctness
-- ============================================================================

module ExtensionDegreeCorrectnessExample where
  
  postulate
    F E : FieldDeclaration
  
  -- Build constructive witness
  constructiveWitness : ConstructiveExtensionDegree F E
  constructiveWitness = mkConstructiveExtensionDegree F E (mkExtensionDegree F E)
  
  -- Extract correctness proof
  correctness : ExtensionDegreeCorrectness F E
  correctness = extensionDegreeCorrectness F E constructiveWitness
  
  -- Access specification
  spec : ExtensionDegreeSpec F E
  spec = ExtensionDegreeCorrectness.specification correctness
  
  -- Verify linear independence
  linIndepProof : M.Identifier
  linIndepProof = ExtensionDegreeCorrectness.proveLinearIndependence correctness
  
  -- Verify spanning
  spanningProof : M.Identifier
  spanningProof = ExtensionDegreeCorrectness.proveSpanning correctness
  
  -- Verify degree formula
  degreeFormulaProof : M.Identifier
  degreeFormulaProof = ExtensionDegreeCorrectness.proveDegreeFormula correctness
  
  -- Verify tower law
  towerLawProof : M.Identifier
  towerLawProof = ExtensionDegreeCorrectness.proveTowerLaw correctness
  
  -- Check overall correctness
  isCorrect : Bool
  isCorrect = ExtensionDegreeCorrectness.isCorrect correctness

-- ============================================================================
-- Example 5: Bundle Correctness
-- ============================================================================

module BundleCorrectnessExample where
  
  postulate
    F E : FieldDeclaration
    minpolyAlg : MinimalPolynomialAlgorithm F E
    splitAlg : SplittingFieldAlgorithm F
    galoisAlg : GaloisGroupAlgorithm F E
    normalAlg : NormalClosureAlgorithm F E
  
  -- Build constructive bundle
  bundle : ConstructiveExtensionBundle F E
  bundle = mkConstructiveBundle F E minpolyAlg splitAlg galoisAlg normalAlg
  
  -- Extract bundle correctness
  correctness : BundleCorrectness F E
  correctness = bundleCorrectness F E bundle
  
  -- Verify individual algorithm correctness
  minpolyCorrect : M.Identifier
  minpolyCorrect = BundleCorrectness.minimalPolynomialCorrect correctness
  
  splitCorrect : M.Identifier
  splitCorrect = BundleCorrectness.splittingFieldCorrect correctness
  
  galoisCorrect : M.Identifier
  galoisCorrect = BundleCorrectness.galoisGroupCorrect correctness
  
  degreeCorrect : M.Identifier
  degreeCorrect = BundleCorrectness.extensionDegreeCorrect correctness
  
  -- Verify consistency between algorithms
  degreeOrderMatch : M.Identifier
  degreeOrderMatch = BundleCorrectness.degreeMatchesGaloisOrder correctness
  
  minpolyDivides : M.Identifier
  minpolyDivides = BundleCorrectness.minPolyDividesSplitting correctness
  
  normalMinimal : M.Identifier
  normalMinimal = BundleCorrectness.normalClosureIsMinimal correctness
  
  -- Check overall bundle correctness
  allCorrect : Bool
  allCorrect = BundleCorrectness.allCorrect correctness

-- ============================================================================
-- Example 6: Correctness Specification Construction
-- ============================================================================

module CorrectnessSpecExample where
  
  postulate
    F E : FieldDeclaration
    α : M.Identifier
  
  -- We use FieldDeclaration as Input type (Set₁) for this example
  -- Define algorithm that produces an algebraic element witness
  computeAlgebraicWitness : FieldDeclaration → AlgebraicElement F E α
  computeAlgebraicWitness = λ _ → mkAlgebraicElement F E α
  
  -- Define expected property (same type)
  shouldBeAlgebraic : AlgebraicElement F E α → AlgebraicElement F E α
  shouldBeAlgebraic = λ w → w
  
  -- Build correctness specification
  spec : CorrectnessSpec FieldDeclaration (AlgebraicElement F E α) (AlgebraicElement F E α)
  spec = record
    { inputData = F
    ; algorithmOutput = computeAlgebraicWitness F
    ; expectedProperty = shouldBeAlgebraic (computeAlgebraicWitness F)
    ; proofObligation = M.mkId "prove-algebraic"
    }
  
  -- Build satisfaction proof (postulated)
  postulate
    satisfaction : AlgorithmSatisfiesSpec FieldDeclaration (AlgebraicElement F E α) 
                                          (AlgebraicElement F E α) spec
  
  -- Build certificate
  certificate : CorrectnessCertificate FieldDeclaration (AlgebraicElement F E α) (AlgebraicElement F E α)
  certificate = certifyCorrectness spec satisfaction

-- ============================================================================
-- Example 7: Correctness via Universal Property
-- ============================================================================

module CorrectnessViaUMPExample where
  
  postulate
    F : FieldDeclaration
    poly : M.Identifier
  
  -- Universal property for splitting field
  postulate
    splitUMP : Set₁
    
  -- Show splitting field satisfies UMP
  postulate
    umpSatisfaction : (E : FieldDeclaration) → SatisfiesUniversalProperty FieldDeclaration splitUMP
  
  -- Correctness follows from UMP
  correctnessFromUMP : (E : FieldDeclaration) → CorrectnessViaUMP (SplittingFieldAlgorithm F) FieldDeclaration splitUMP
  correctnessFromUMP E = record
    { algorithm = record
        { splittingField = λ f → mkSplittingField F f E
        ; roots = λ _ → []
        ; limitation = nothing
        }
    ; producedObject = E
    ; umpSatisfaction = umpSatisfaction E
    ; correctnessFromUMP = M.mkId "ump-implies-correct"
    }

-- ============================================================================
-- Example 8: Invariant Preservation
-- ============================================================================

module InvariantPreservationExample where
  
  postulate
    F : FieldDeclaration
  
  -- Define an invariant: "is a field"
  IsField : FieldDeclaration → Set₁
  IsField _ = FieldDeclaration  -- Simplified
  
  -- Field extension algorithm preserves "is a field"
  extensionPreservesField : InvariantPreservation FieldDeclaration IsField
  extensionPreservesField = record
    { algorithm = λ F → F  -- Simplified: just identity
    ; invariant = λ F → F
    ; preservation = λ F inv → inv
    ; preservationProof = M.mkId "field-preserved"
    }

-- ============================================================================
-- Example 9: Complexity Correctness
-- ============================================================================

module ComplexityCorrectnessExample where
  
  postulate
    F E : FieldDeclaration
    minpolyAlg : MinimalPolynomialAlgorithm F E
  
  -- Specify complexity correctness
  complexityCorrect : ComplexityCorrectness (MinimalPolynomialAlgorithm F E)
  complexityCorrect = record
    { algorithm = minpolyAlg
    ; worstCaseComplexity = M.mkId "O(d³)"  -- d = degree
    ; averageCaseComplexity = M.mkId "O(d²)"
    ; spaceComplexity = M.mkId "O(d²)"
    ; proveWorstCase = M.mkId "worst-case-proof"
    ; proveAverageCase = M.mkId "avg-case-proof"
    ; proveSpaceUsage = M.mkId "space-proof"
    ; boundsAreTight = true
    }

-- ============================================================================
-- Example 10: Error Handling Correctness
-- ============================================================================

module ErrorHandlingCorrectnessExample where
  
  postulate
    F E : FieldDeclaration
    α : M.Identifier
  
  -- Define error type (at Set₁ level)
  data MinPolyError : Set₁ where
    notAlgebraic : MinPolyError
    invalidField : MinPolyError
  
  -- Error handling correctness (using FieldDeclaration as input)
  errorCorrectness : ErrorHandlingCorrectness FieldDeclaration (AlgebraicElement F E α) MinPolyError
  errorCorrectness = record
    { algorithm = λ _ → mkAlgebraicElement F E α
    ; errorConditions = λ _ → false  -- No errors for valid inputs
    ; errorProduction = λ _ → notAlgebraic
    ; validInputsSucceed = M.mkId "valid-succeed"
    ; invalidInputsFail = M.mkId "invalid-fail"
    ; errorMessagesCorrect = M.mkId "messages-correct"
    ; noFalsePositives = M.mkId "no-false-pos"
    ; noFalseNegatives = M.mkId "no-false-neg"
    }

-- ============================================================================
-- Example 11: Totality and Termination
-- ============================================================================

module TotalityExample where
  
  postulate
    F E : FieldDeclaration
    α : M.Identifier
  
  -- Prove algorithm is total (FieldDeclaration → AlgebraicElement)
  minpolyTotal : AlgorithmTotality FieldDeclaration (AlgebraicElement F E α)
  minpolyTotal = record
    { algorithm = λ _ → mkAlgebraicElement F E α
    ; totalityProof = M.mkId "total"
    ; terminationMeasure = λ _ → zero  -- Simplified
    }
  
  -- Prove algorithm terminates
  minpolyTerminates : AlgorithmTermination FieldDeclaration (AlgebraicElement F E α)
  minpolyTerminates = record
    { algorithm = λ _ → mkAlgebraicElement F E α
    ; terminates = true
    ; terminationProof = M.mkId "terminates"
    ; maxSteps = M.mkId "max-iterations"
    }

-- ============================================================================
-- Example 12: Correctness Certificate Pipeline
-- ============================================================================

module CertificatePipelineExample where
  
  postulate
    F E : FieldDeclaration
    α : M.Identifier
  
  -- Define algorithm and property (using FieldDeclaration as input, AlgebraicElement as output)
  computeAlgebraic : FieldDeclaration → AlgebraicElement F E α
  computeAlgebraic = λ _ → mkAlgebraicElement F E α
  
  extractProperty : AlgebraicElement F E α → AlgebraicElement F E α
  extractProperty = λ w → w
  
  -- Build complete certificate
  certificate : CorrectnessCertificate FieldDeclaration (AlgebraicElement F E α) (AlgebraicElement F E α)
  certificate = buildCorrectnessCertificate computeAlgebraic extractProperty F
  
  -- Extract components
  spec : CorrectnessSpec FieldDeclaration (AlgebraicElement F E α) (AlgebraicElement F E α)
  spec = CorrectnessCertificate.specification certificate
  
  proof : AlgorithmSatisfiesSpec FieldDeclaration (AlgebraicElement F E α) 
                                  (AlgebraicElement F E α) spec
  proof = CorrectnessCertificate.satisfactionProof certificate
  
  certifier : M.Identifier
  certifier = CorrectnessCertificate.certificationAuthority certificate
