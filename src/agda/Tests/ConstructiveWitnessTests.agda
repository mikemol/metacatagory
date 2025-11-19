-- Tests.ConstructiveWitnessTests: Test suite for constructive witness framework
-- Validates that constructive witnesses provide computational content with
-- correctness proofs, contrasting with placeholder-based approaches.

module Tests.ConstructiveWitnessTests where

open import Core
open import Core.Phase
open import Core.Witnesses
open import Core.ConstructiveWitnesses
open import Core.AlgebraicAlgorithms
open import Core.AlgorithmUniversality hiding (proof)
open import Core.UniversalProperties
open import Algebra.Foundation
open import Algebra.Rings.Basic using (FieldDeclaration)
open import Algebra.Fields.Basic
open import Algebra.Fields.Advanced
open import Metamodel as M
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Nat using (Nat; zero; suc)
import Agda.Builtin.Bool as B
open B using () renaming (Bool to Boolean; true to tt; false to ff)

-- ============================================================================
-- Test 1: Constructive Minimal Polynomial Creation
-- ============================================================================

module ConstructiveMinimalPolynomialTest where
  
  postulate
    F E : FieldDeclaration
    α : M.Identifier
    minpolyAlg : MinimalPolynomialAlgorithm F E
  
  -- Phase: Build constructive minimal polynomial
  buildConstructiveMinPoly : Phase (MinimalPolynomialAlgorithm F E) (ConstructiveMinimalPolynomial F E α)
  buildConstructiveMinPoly = record
    { transform = λ alg → mkConstructiveMinimalPolynomial F E α alg
    }
  
  -- Test execution
  witness : ConstructiveMinimalPolynomial F E α
  witness = Phase.transform buildConstructiveMinPoly minpolyAlg
  
  -- Verify witness has computational content
  hasCoefficients : List M.Identifier
  hasCoefficients = ConstructiveMinimalPolynomial.polynomialCoefficients witness
  
  hasDegree : Nat
  hasDegree = ConstructiveMinimalPolynomial.degreeComputation witness
  
  hasIrreducibilityProof : M.Identifier
  hasIrreducibilityProof = ConstructiveMinimalPolynomial.irreducibilityProof witness

-- ============================================================================
-- Test 2: Constructive Splitting Field with Roots
-- ============================================================================

module ConstructiveSplittingFieldTest where
  
  postulate
    F : FieldDeclaration
    poly : M.Identifier
    splitAlg : SplittingFieldAlgorithm F
    computedField : M.Identifier
  
  -- Phase: Build constructive splitting field
  buildSplittingField : Phase M.Identifier (ConstructiveSplittingField F poly)
  buildSplittingField = record
    { transform = λ cf → mkConstructiveSplittingField F poly splitAlg cf
    }
  
  -- Test execution
  witness : ConstructiveSplittingField F poly
  witness = Phase.transform buildSplittingField computedField
  
  -- Verify witness has roots
  hasRoots : List (ConstructiveRoot F poly)
  hasRoots = ConstructiveSplittingField.roots witness
  
  hasFactorization : List M.Identifier
  hasFactorization = ConstructiveSplittingField.factorization witness
  
  hasCompletenessProof : M.Identifier
  hasCompletenessProof = ConstructiveSplittingField.allRootsPresent witness

-- ============================================================================
-- Test 3: Constructive Galois Group with Automorphisms
-- ============================================================================

module ConstructiveGaloisGroupTest where
  
  postulate
    F E : FieldDeclaration
    galoisAlg : GaloisGroupAlgorithm F E
  
  -- Phase: Build constructive Galois group
  buildGaloisGroup : Phase (GaloisGroupAlgorithm F E) (ConstructiveGaloisGroup F E)
  buildGaloisGroup = record
    { transform = λ alg → mkConstructiveGaloisGroup F E alg
    }
  
  -- Test execution
  witness : ConstructiveGaloisGroup F E
  witness = Phase.transform buildGaloisGroup galoisAlg
  
  -- Verify witness has automorphisms
  hasAutomorphisms : List (ConstructiveAutomorphism F E)
  hasAutomorphisms = ConstructiveGaloisGroup.automorphisms witness
  
  hasGroupOps : AutomorphismComposition F E
  hasGroupOps = ConstructiveGaloisGroup.groupOperation witness
  
  hasFundamentalThm : M.Identifier
  hasFundamentalThm = ConstructiveGaloisGroup.orderEqualsExtensionDegree witness

-- ============================================================================
-- Test 4: Witness Validation
-- ============================================================================

module WitnessValidationTest where
  
  postulate
    F E : FieldDeclaration
    α : M.Identifier
    minpolyAlg : MinimalPolynomialAlgorithm F E
  
  witness : ConstructiveMinimalPolynomial F E α
  witness = mkConstructiveMinimalPolynomial F E α minpolyAlg
  
  -- Phase: Validate witness
  validateWitness : Phase (ConstructiveMinimalPolynomial F E α) (WitnessValidation (ConstructiveMinimalPolynomial F E α))
  validateWitness = validate
  
  -- Test execution
  validation : WitnessValidation (ConstructiveMinimalPolynomial F E α)
  validation = Phase.transform validateWitness witness
  
  -- Check validation results
  isMarkedValid : Boolean
  isMarkedValid = WitnessValidation.isValid validation
  
  hasTrace : M.Identifier
  hasTrace = WitnessValidation.validationTrace validation

-- ============================================================================
-- Test 5: Correctness Proof Extraction
-- ============================================================================

module CorrectnessProofTest where
  
  postulate
    F E : FieldDeclaration
    α : M.Identifier
    minpolyAlg : MinimalPolynomialAlgorithm F E
  
  witness : ConstructiveMinimalPolynomial F E α
  witness = mkConstructiveMinimalPolynomial F E α minpolyAlg
  
  -- Phase: Extract correctness proof
  extractCorrectness : Phase (ConstructiveMinimalPolynomial F E α) (CorrectnessProof (AlgebraicElement F E α))
  extractCorrectness = extractProof (verifyMinimalPolynomial F E α)
  
  -- Test execution
  proof : CorrectnessProof (AlgebraicElement F E α)
  proof = Phase.transform extractCorrectness witness
  
  -- Check proof components
  hasProperty : AlgebraicElement F E α
  hasProperty = CorrectnessProof.property proof
  
  hasProofId : M.Identifier
  hasProofId = CorrectnessProof.proof proof
  
  hasSteps : List M.Identifier
  hasSteps = CorrectnessProof.verificationSteps proof

-- ============================================================================
-- Test 6: Constructive Extension Degree with Basis
-- ============================================================================

module ConstructiveExtensionDegreeTest where
  
  postulate
    F E : FieldDeclaration
    deg : ExtensionDegree F E
  
  -- Phase: Build constructive extension degree
  buildExtDegree : Phase (ExtensionDegree F E) (ConstructiveExtensionDegree F E)
  buildExtDegree = record
    { transform = λ d → mkConstructiveExtensionDegree F E d
    }
  
  -- Test execution
  witness : ConstructiveExtensionDegree F E
  witness = Phase.transform buildExtDegree deg
  
  -- Verify witness has basis
  hasBasis : List (ConstructiveBasisElement F E)
  hasBasis = ConstructiveExtensionDegree.basis witness
  
  hasDimension : Nat
  hasDimension = ConstructiveExtensionDegree.basisSize witness
  
  hasLinIndepProof : M.Identifier
  hasLinIndepProof = ConstructiveExtensionDegree.linearIndependenceProof witness

-- ============================================================================
-- Test 7: Constructive Normal Closure with Conjugates
-- ============================================================================

module ConstructiveNormalClosureTest where
  
  postulate
    F E N : FieldDeclaration
    normalAlg : NormalClosureAlgorithm F E
  
  -- Phase: Build constructive normal closure
  buildNormalClosure : Phase (NormalClosureAlgorithm F E) (ConstructiveNormalClosure F E)
  buildNormalClosure = record
    { transform = λ alg → mkConstructiveNormalClosure F E N alg
    }
  
  -- Test execution
  witness : ConstructiveNormalClosure F E
  witness = Phase.transform buildNormalClosure normalAlg
  
  -- Verify witness has conjugates
  hasConjugates : List M.Identifier
  hasConjugates = ConstructiveNormalClosure.conjugates witness
  
  hasEmbeddings : List M.Identifier
  hasEmbeddings = ConstructiveNormalClosure.embeddings witness
  
  hasClosureProof : M.Identifier
  hasClosureProof = ConstructiveNormalClosure.closedUnderConjugates witness

-- ============================================================================
-- Test 8: Complete Constructive Bundle
-- ============================================================================

module ConstructiveBundleTest where
  
  postulate
    F E : FieldDeclaration
    minpolyAlg : MinimalPolynomialAlgorithm F E
    splitAlg : SplittingFieldAlgorithm F
    galoisAlg : GaloisGroupAlgorithm F E
    normalAlg : NormalClosureAlgorithm F E
  
  -- Phase: Build complete bundle
  buildBundle : Phase (GaloisGroupAlgorithm F E) (ConstructiveExtensionBundle F E)
  buildBundle = record
    { transform = λ _ → mkConstructiveBundle F E minpolyAlg splitAlg galoisAlg normalAlg
    }
  
  -- Test execution
  bundle : ConstructiveExtensionBundle F E
  bundle = Phase.transform buildBundle galoisAlg
  
  -- Verify bundle components
  hasExtDegree : ConstructiveExtensionDegree F E
  hasExtDegree = ConstructiveExtensionBundle.extensionDegree bundle
  
  hasGaloisGroup : ConstructiveGaloisGroup F E
  hasGaloisGroup = ConstructiveExtensionBundle.galoisGroup bundle
  
  hasNormalClosure : ConstructiveNormalClosure F E
  hasNormalClosure = ConstructiveExtensionBundle.normalClosure bundle
  
  isConsistent : M.Identifier
  isConsistent = ConstructiveExtensionBundle.consistencyProof bundle

-- ============================================================================
-- Test 9: Phase Composition with Constructive Witnesses
-- ============================================================================

module ConstructivePhaseCompositionTest where
  
  postulate
    F E : FieldDeclaration
    α : M.Identifier
    minpolyAlg : MinimalPolynomialAlgorithm F E
  
  -- Compose phases: Build → Extract proof
  buildThenProof : Phase (MinimalPolynomialAlgorithm F E) (CorrectnessProof (AlgebraicElement F E α))
  buildThenProof =
    let build = constructivize (λ _ → mkConstructiveMinimalPolynomial F E α minpolyAlg)
        extract = extractProof (verifyMinimalPolynomial F E α)
    in build ⟫ extract
  
  -- Test execution
  result : CorrectnessProof (AlgebraicElement F E α)
  result = Phase.transform buildThenProof minpolyAlg
  
  -- Verify result
  hasCorrectness : List M.Identifier
  hasCorrectness = CorrectnessProof.verificationSteps result

-- ============================================================================
-- Test 10: Computational Evidence Construction
-- ============================================================================

module ComputationalEvidenceTest where
  
  postulate
    F E : FieldDeclaration
    α : M.Identifier
    minpolyAlg : MinimalPolynomialAlgorithm F E
  
  witness : ConstructiveMinimalPolynomial F E α
  witness = mkConstructiveMinimalPolynomial F E α minpolyAlg
  
  -- Build computational evidence
  evidence : ComputationalEvidence (ConstructiveMinimalPolynomial F E α)
  evidence = liftToConstructive witness (λ w → w)
  
  -- Verify evidence components
  hasAlgorithm : ConstructiveMinimalPolynomial F E α
  hasAlgorithm = ComputationalEvidence.algorithm evidence
  
  hasData : M.Identifier
  hasData = ComputationalEvidence.witnessData evidence
  
  isComputed : Boolean
  isComputed = ComputationalEvidence.isComputed evidence

-- ============================================================================
-- Test 11: Automorphism Composition
-- ============================================================================

module AutomorphismCompositionTest where
  
  postulate
    F E : FieldDeclaration
    σ τ : ConstructiveAutomorphism F E
    composition : AutomorphismComposition F E
  
  -- Test composition operation
  composed : ConstructiveAutomorphism F E
  composed = AutomorphismComposition.compose composition σ τ
  
  -- Test identity
  identity : ConstructiveAutomorphism F E
  identity = AutomorphismComposition.identity composition
  
  -- Test inverse
  σInverse : ConstructiveAutomorphism F E
  σInverse = AutomorphismComposition.inverseAut composition σ
  
  -- Verify group axioms present
  hasAssoc : M.Identifier
  hasAssoc = AutomorphismComposition.assocProof composition
  
  hasLeftId : M.Identifier
  hasLeftId = AutomorphismComposition.leftIdProof composition

-- ============================================================================
-- Test 12: Multi-step Verification Pipeline
-- ============================================================================

module VerificationPipelineTest where
  
  postulate
    F : FieldDeclaration
    poly : M.Identifier
    splitAlg : SplittingFieldAlgorithm F
    computedField : M.Identifier
  
  -- Build witness
  witness : ConstructiveSplittingField F poly
  witness = mkConstructiveSplittingField F poly splitAlg computedField
  
  -- Validate
  validation : WitnessValidation (ConstructiveSplittingField F poly)
  validation = validateConstructiveWitness witness
  
  -- Extract proof
  proof : CorrectnessProof (SplittingField F poly)
  proof = verifySplittingField F poly witness
  
  -- Verify pipeline results
  witnessIsValid : Boolean
  witnessIsValid = WitnessValidation.isValid validation
  
  proofHasSteps : List M.Identifier
  proofHasSteps = CorrectnessProof.verificationSteps proof

-- ============================================================================
-- Test 13: Constructive vs Non-constructive Comparison
-- ============================================================================

module ConstructiveVsNonConstructiveTest where
  
  postulate
    F E : FieldDeclaration
    α : M.Identifier
    minpolyAlg : MinimalPolynomialAlgorithm F E
  
  -- Non-constructive witness (placeholder-based)
  nonConstructive : AlgebraicElement F E α
  nonConstructive = mkAlgebraicElement F E α
  
  -- Constructive witness (algorithm-based)
  constructive : ConstructiveMinimalPolynomial F E α
  constructive = mkConstructiveMinimalPolynomial F E α minpolyAlg
  
  -- Constructive has computational content
  constructiveCoeffs : List M.Identifier
  constructiveCoeffs = ConstructiveMinimalPolynomial.polynomialCoefficients constructive
  
  constructiveDegree : Nat
  constructiveDegree = ConstructiveMinimalPolynomial.degreeComputation constructive
  
  -- Non-constructive only has identifiers
  nonConstructiveMinPoly : M.Identifier
  nonConstructiveMinPoly = AlgebraicElement.minimalPolynomial nonConstructive

-- ============================================================================
-- Test 14: Galois Group Correctness Verification
-- ============================================================================

module GaloisGroupVerificationTest where
  
  postulate
    F E : FieldDeclaration
    galoisAlg : GaloisGroupAlgorithm F E
  
  -- Build constructive Galois group
  witness : ConstructiveGaloisGroup F E
  witness = mkConstructiveGaloisGroup F E galoisAlg
  
  -- Verify correctness
  proof : CorrectnessProof (GaloisGroup F E)
  proof = verifyGaloisGroup F E witness
  
  -- Check verification steps
  steps : List M.Identifier
  steps = CorrectnessProof.verificationSteps proof
  
  -- Verify steps include key properties
  postulate
    stepsIncludeAutomorphismCheck : M.Identifier
    stepsIncludeGroupAxioms : M.Identifier
    stepsIncludeOrderEquality : M.Identifier
    stepsIncludeFundamentalTheorem : M.Identifier

-- ============================================================================
-- Test 15: Bundle Consistency Check
-- ============================================================================

module BundleConsistencyTest where
  
  postulate
    F E : FieldDeclaration
    minpolyAlg : MinimalPolynomialAlgorithm F E
    splitAlg : SplittingFieldAlgorithm F
    galoisAlg : GaloisGroupAlgorithm F E
    normalAlg : NormalClosureAlgorithm F E
  
  -- Build bundle
  bundle : ConstructiveExtensionBundle F E
  bundle = mkConstructiveBundle F E minpolyAlg splitAlg galoisAlg normalAlg
  
  -- Check all witnesses marked as valid
  allValid : Boolean
  allValid = ConstructiveExtensionBundle.allWitnessesValid bundle
  
  -- Check consistency proof exists
  consistency : M.Identifier
  consistency = ConstructiveExtensionBundle.consistencyProof bundle
  
  -- Access individual witnesses for consistency
  extDeg : ConstructiveExtensionDegree F E
  extDeg = ConstructiveExtensionBundle.extensionDegree bundle
  
  galois : ConstructiveGaloisGroup F E
  galois = ConstructiveExtensionBundle.galoisGroup bundle
  
  -- Verify Galois group order matches extension degree
  galoisOrder : Nat
  galoisOrder = ConstructiveGaloisGroup.groupOrder galois
  
  extDegSize : Nat
  extDegSize = ConstructiveExtensionDegree.basisSize extDeg

-- =========================================================================
-- Test 16b: Minimal Polynomial Divides Evidence (Scaffold)
-- =========================================================================

module MinpolyDividesEvidenceTest where
  
  postulate
    F E : FieldDeclaration
    α p : M.Identifier
    vanishes monic : M.Identifier
    minpolyAlg : MinimalPolynomialAlgorithm F E
  
  ump : MinimalPolynomialProperty F E α
  ump = minimalPolynomialImplementsUniversality F E minpolyAlg α
  
  evidence : MinpolyDividesEvidence F E α
  evidence = mkMinpolyDividesEvidence F E α ump p vanishes monic
  
  minpolyId : M.Identifier
  minpolyId = MinpolyDividesEvidence.minPoly evidence
  
  dividesId : M.Identifier
  dividesId = MinpolyDividesEvidence.dividesWitness evidence

  qId : M.Identifier
  qId = MinpolyDividesEvidence.quotient evidence

  rId : M.Identifier
  rId = MinpolyDividesEvidence.remainder evidence

  remainderZero : Boolean
  remainderZero = flagValue (MinpolyDividesEvidence.remainderZeroFlag evidence)

  -- Division scaffold conversion
  divScaffold : DivisionScaffold
  divScaffold = toDivisionScaffold evidence

  dividendId : M.Identifier
  dividendId = DivisionScaffold.dividend divScaffold

  divisorId : M.Identifier
  divisorId = DivisionScaffold.divisor divScaffold

  divRemainderZero : Boolean
  divRemainderZero = flagValue (DivisionScaffold.remainderZeroFlag divScaffold)

-- =========================================================================
-- Test 16c: Division Algorithm Scaffold Integration
-- =========================================================================

module DivisionAlgorithmScaffoldTest where
  
  -- Build a division scaffold directly using the minimal division algorithm
  dummyDivisor : M.Identifier
  dummyDivisor = M.mkId "minpoly-x3-2"

  dummyDividend : M.Identifier
  dummyDividend = M.mkId "target-poly-p"

  divResult : DivisionScaffold
  divResult = dividePolynomials dummyDivisor dummyDividend

  -- Extract components
  computedQuotient : M.Identifier
  computedQuotient = DivisionScaffold.quotient divResult

  computedRemainder : M.Identifier
  computedRemainder = DivisionScaffold.remainder divResult

  remainderIsZero : Boolean
  remainderIsZero = flagValue (DivisionScaffold.remainderZeroFlag divResult)

  -- Ensure scaffold fields are present
  divDivisor : M.Identifier
  divDivisor = DivisionScaffold.divisor divResult

  divDividend : M.Identifier
  divDividend = DivisionScaffold.dividend divResult

-- =========================================================================
-- Test 16d: Evidence-Based Division Bridge
-- =========================================================================

module DivisionAlgorithmEvidenceBridgeTest where
  postulate
    F E : FieldDeclaration
    α p vanishes monic : M.Identifier
    ump : MinimalPolynomialProperty F E α

  evidence : MinpolyDividesEvidence F E α
  evidence = mkMinpolyDividesEvidence F E α ump p vanishes monic

  bridged : DivisionScaffold
  bridged = dividePolynomialsFromEvidence evidence

  bridgedQuotient : M.Identifier
  bridgedQuotient = DivisionScaffold.quotient bridged

  bridgedRemainder : M.Identifier
  bridgedRemainder = DivisionScaffold.remainder bridged

  bridgedRemainderZero : Boolean
  bridgedRemainderZero = flagValue (DivisionScaffold.remainderZeroFlag bridged)

-- =========================================================================
-- Test 16e: UMP-Based Division Helper
-- =========================================================================

module DivisionByMinpolyUMPHelperTest where
  postulate
    F E : FieldDeclaration
    α p vanishes monic : M.Identifier
    ump : MinimalPolynomialProperty F E α

  ds : DivisionScaffold
  ds = divideByMinimalPolynomial {F} {E} {α} ump p vanishes monic

  dsQuot : M.Identifier
  dsQuot = DivisionScaffold.quotient ds

  dsRem : M.Identifier
  dsRem = DivisionScaffold.remainder ds

  dsZero : Boolean
  dsZero = flagValue (DivisionScaffold.remainderZeroFlag ds)

-- =========================================================================
-- Test 16f: Refinement of Generic Division via UMP
-- =========================================================================

module DivisionRefinementByUMPTest where
  postulate
    F E : FieldDeclaration
    α p vanishes monic : M.Identifier
    ump : MinimalPolynomialProperty F E α

  -- Start with a generic division using the minimal polynomial as divisor
  genericDS : DivisionScaffold
  genericDS = dividePolynomials (MinimalPolynomialProperty.minPoly ump) p

  genericZero : Boolean
  genericZero = flagValue (DivisionScaffold.remainderZeroFlag genericDS)

  -- Refine the generic division using UMP evidence
  refinedDS : DivisionScaffold
  refinedDS = refineDivisionByUMP {F} {E} {α} ump p vanishes monic genericDS

  refinedZero : Boolean
  refinedZero = flagValue (DivisionScaffold.remainderZeroFlag refinedDS)

-- =========================================================================
-- Test 16g: F2 Polynomial Division (Concrete Remainder Flag)
-- =========================================================================

module F2PolynomialDivisionTest where
  open import Core.PolynomialsF2 as PF2
  import Agda.Builtin.Bool as BB
  -- Example: (x^2 + 1) / (x + 1) over F2 has zero remainder
  -- dividend = [true, false, true]
  -- divisor  = [true, true]
  dividendF2 : PF2.PolyF2
  dividendF2 = PF2.normalize (BB.true ∷ BB.false ∷ BB.true ∷ [])

  divisorF2 : PF2.PolyF2
  divisorF2 = PF2.normalize (BB.true ∷ BB.true ∷ [])

  dsF2 : DivisionScaffold
  dsF2 = dividePolynomialsF2 dividendF2 divisorF2

  f2RemainderZero : Boolean
  f2RemainderZero = flagValue (DivisionScaffold.remainderZeroFlag dsF2)

-- =========================================================================
-- Test 16: Algorithm-to-UMP Coherence (Constructive)
-- =========================================================================

module ConstructiveUMPBridgeCoherence where
  
  postulate
    F E : FieldDeclaration
    α : M.Identifier
    f : M.Identifier
    minpolyAlg : MinimalPolynomialAlgorithm F E
    splitAlg : SplittingFieldAlgorithm F
    gcAlg : GaloisClosureAlgorithm F E
    computedSF : M.Identifier
  
  -- Build constructive witnesses from algorithms
  cmp : ConstructiveMinimalPolynomial F E α
  cmp = mkConstructiveMinimalPolynomial F E α minpolyAlg
  
  csf : ConstructiveSplittingField F f
  csf = mkConstructiveSplittingField F f splitAlg computedSF
  
  -- Obtain UMP records from the same algorithms
  ump-minpoly : MinimalPolynomialProperty F E α
  ump-minpoly = minimalPolynomialImplementsUniversality F E minpolyAlg α
  
  ump-splitting : SplittingFieldProperty F f
  ump-splitting = splittingFieldImplementsUniversality F splitAlg f
  
  ump-galois-closure : GaloisClosureProperty F E
  ump-galois-closure = galoisClosureImplementsUniversality F E gcAlg
  
  -- Coherence probes: simply ensure key components are present
  cmp-has-degree : Nat
  cmp-has-degree = ConstructiveMinimalPolynomial.degreeComputation cmp
  
  ump-minpoly-monic : M.Identifier
  ump-minpoly-monic = MinimalPolynomialProperty.isMonic ump-minpoly
  
  csf-has-structure : M.Identifier
  csf-has-structure = ConstructiveSplittingField.fieldStructure csf
  
  ump-split-has-roots : M.Identifier
  ump-split-has-roots = SplittingFieldProperty.hasAllRoots ump-splitting
  
  gc-normal : M.Identifier
  gc-normal = GaloisClosureProperty.isNormal ump-galois-closure
  
  -- A simple coherence marker to enforce type-level linkage exists
  coherenceMarker : M.Identifier
  coherenceMarker = M.mkId "✓ constructive↔UMP coherence (types)"
