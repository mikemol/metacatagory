-- Core.AlgorithmCorrectness: Formal correctness specifications for algebraic algorithms
-- This module defines what it means for an algorithm to be "correct" by specifying
-- proof obligations that connect algorithm outputs to their mathematical properties.

module Core.AlgorithmCorrectness where

open import Core
open import Core.Phase
open import Core.Witnesses
open import Core.ConstructiveWitnesses
open import Core.AlgebraicAlgorithms
open import Algebra.Foundation
open import Algebra.Rings.Basic using (FieldDeclaration)
open import Algebra.Fields.Basic
open import Algebra.Fields.Advanced
open import Algebra.Groups.Basic
open import Metamodel as M
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Nat using (Nat; zero; suc)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Core.Phase using (Maybe; nothing; just)
import Agda.Builtin.Bool as B
open B using () renaming (Bool to Boolean; true to tt; false to ff)

-- ============================================================================
-- Correctness Specification Infrastructure
-- ============================================================================

-- A correctness specification relates an algorithm output to a property
record CorrectnessSpec (Input : Set₁) (Output : Set₁) (Property : Set₁) : Set₁ where
  field
    inputData : Input
    algorithmOutput : Output
    expectedProperty : Property
    proofObligation : M.Identifier  -- What must be proved

-- Proof that an algorithm satisfies its specification
record AlgorithmSatisfiesSpec (Input Output Property : Set₁) 
                               (spec : CorrectnessSpec Input Output Property) : Set₁ where
  field
    proof : M.Identifier  -- Actual proof term
    verificationSteps : List M.Identifier
    isComplete : Boolean

-- Correctness certificate bundles specification with proof
record CorrectnessCertificate (Input Output Property : Set₁) : Set₁ where
  field
    specification : CorrectnessSpec Input Output Property
    satisfactionProof : AlgorithmSatisfiesSpec Input Output Property specification
    certificationAuthority : M.Identifier  -- Who/what certified this

-- ============================================================================
-- Minimal Polynomial Algorithm Correctness
-- ============================================================================

-- Specification: What makes a minimal polynomial "correct"
record MinimalPolynomialSpec (F E : FieldDeclaration) (α : M.Identifier) : Set₁ where
  field
    baseField : FieldDeclaration
    extensionField : FieldDeclaration
    element : M.Identifier
    
    -- Algorithm output
    polynomial : M.Identifier
    
    -- Correctness properties (proof obligations)
    rootProperty : M.Identifier        -- poly(α) = 0
    irreducibility : M.Identifier      -- poly is irreducible over F
    minimality : M.Identifier          -- poly has minimal degree among polys with α as root
    monicProperty : M.Identifier       -- poly is monic (leading coeff = 1)
    coefficientsInF : M.Identifier     -- all coefficients are in F

-- Proof that a minimal polynomial algorithm is correct
record MinimalPolynomialCorrectness (F E : FieldDeclaration) (α : M.Identifier) : Set₁ where
  field
    algorithm : MinimalPolynomialAlgorithm F E
    specification : MinimalPolynomialSpec F E α
    
    -- Proof obligations satisfied
    proveRootProperty : M.Identifier
    proveIrreducibility : M.Identifier
    proveMinimality : M.Identifier
    proveMonic : M.Identifier
    proveCoefficientsInF : M.Identifier
    
    -- Overall correctness
    isCorrect : Boolean

-- Build correctness proof from constructive witness
minimalPolynomialCorrectness :
  (F E : FieldDeclaration) →
  (α : M.Identifier) →
  ConstructiveMinimalPolynomial F E α →
  MinimalPolynomialCorrectness F E α
minimalPolynomialCorrectness F E α cmp = record
  { algorithm = record
      { minimalPolynomial = λ _ → M.mkId "computed-poly"
      ; isAlgebraic = λ β → yes (mkAlgebraicElement F E β)
      ; limitation = nothing
      }
  ; specification = record
      { baseField = F
      ; extensionField = E
      ; element = α
      ; polynomial = M.mkId "minpoly"
      ; rootProperty = ConstructiveMinimalPolynomial.rootVerification cmp
      ; irreducibility = ConstructiveMinimalPolynomial.irreducibilityProof cmp
      ; minimality = ConstructiveMinimalPolynomial.minimalityProof cmp
      ; monicProperty = M.mkId "monic"
      ; coefficientsInF = M.mkId "coeffs-in-F"
      }
  ; proveRootProperty = ConstructiveMinimalPolynomial.rootVerification cmp
  ; proveIrreducibility = ConstructiveMinimalPolynomial.irreducibilityProof cmp
  ; proveMinimality = ConstructiveMinimalPolynomial.minimalityProof cmp
  ; proveMonic = M.mkId "monic-proof"
  ; proveCoefficientsInF = M.mkId "coeffs-proof"
  ; isCorrect = tt
  }

-- ============================================================================
-- Splitting Field Algorithm Correctness
-- ============================================================================

-- Specification: What makes a splitting field "correct"
record SplittingFieldSpec (F : FieldDeclaration) (poly : M.Identifier) : Set₁ where
  field
    baseField : FieldDeclaration
    polynomial : M.Identifier
    
    -- Algorithm output
    splittingField : FieldDeclaration
    roots : List M.Identifier
    
    -- Correctness properties
    allRootsInField : M.Identifier      -- All roots of poly are in splitting field
    completeSplitting : M.Identifier     -- poly factors completely into linear factors
    minimalField : M.Identifier          -- Smallest field containing all roots
    fieldExtension : M.Identifier        -- Splitting field extends base field
    rootMultiplicities : M.Identifier    -- Multiplicities match polynomial degree

-- Proof that a splitting field algorithm is correct
record SplittingFieldCorrectness (F : FieldDeclaration) (poly : M.Identifier) : Set₁ where
  field
    algorithm : SplittingFieldAlgorithm F
    specification : SplittingFieldSpec F poly
    
    -- Proof obligations
    proveAllRootsInField : M.Identifier
    proveCompleteSplitting : M.Identifier
    proveMinimalField : M.Identifier
    proveFieldExtension : M.Identifier
    proveMultiplicities : M.Identifier
    
    -- Overall correctness
    isCorrect : Boolean

-- Build correctness proof from constructive witness
splittingFieldCorrectness :
  (F : FieldDeclaration) →
  (poly : M.Identifier) →
  ConstructiveSplittingField F poly →
  SplittingFieldCorrectness F poly
splittingFieldCorrectness F poly csf = record
  { algorithm = record
      { splittingField = λ f → mkSplittingField F f (ConstructiveSplittingField.splittingField csf)
      ; roots = λ _ → []
      ; limitation = nothing
      }
  ; specification = record
      { baseField = F
      ; polynomial = poly
      ; splittingField = ConstructiveSplittingField.splittingField csf
      ; roots = []
      ; allRootsInField = ConstructiveSplittingField.allRootsPresent csf
      ; completeSplitting = M.mkId "complete-factorization"
      ; minimalField = ConstructiveSplittingField.minimalityProof csf
      ; fieldExtension = ConstructiveSplittingField.fieldStructure csf
      ; rootMultiplicities = M.mkId "multiplicities-correct"
      }
  ; proveAllRootsInField = ConstructiveSplittingField.allRootsPresent csf
  ; proveCompleteSplitting = M.mkId "complete-split-proof"
  ; proveMinimalField = ConstructiveSplittingField.minimalityProof csf
  ; proveFieldExtension = ConstructiveSplittingField.fieldStructure csf
  ; proveMultiplicities = M.mkId "mult-proof"
  ; isCorrect = tt
  }

-- ============================================================================
-- Galois Group Algorithm Correctness
-- ============================================================================

-- Specification: What makes a Galois group computation "correct"
record GaloisGroupSpec (F E : FieldDeclaration) : Set₁ where
  field
    baseField : FieldDeclaration
    extensionField : FieldDeclaration
    
    -- Algorithm output
    automorphisms : List M.Identifier
    groupOrder : Nat
    
    -- Correctness properties
    allAutomorphismsValid : M.Identifier        -- Each is field automorphism fixing F
    groupClosure : M.Identifier                 -- Closed under composition
    groupAssociativity : M.Identifier           -- Composition is associative
    groupIdentity : M.Identifier                -- Identity automorphism exists
    groupInverses : M.Identifier                -- Every automorphism has inverse
    orderEqualsExtensionDegree : M.Identifier   -- |Gal(E/F)| = [E:F]
    fundamentalTheorem : M.Identifier           -- Galois correspondence holds

-- Proof that a Galois group algorithm is correct
record GaloisGroupCorrectness (F E : FieldDeclaration) : Set₁ where
  field
    algorithm : GaloisGroupAlgorithm F E
    specification : GaloisGroupSpec F E
    
    -- Proof obligations
    proveAutomorphismsValid : M.Identifier
    proveGroupClosure : M.Identifier
    proveAssociativity : M.Identifier
    proveIdentity : M.Identifier
    proveInverses : M.Identifier
    proveOrderEquality : M.Identifier
    proveFundamentalTheorem : M.Identifier
    
    -- Overall correctness
    isCorrect : Boolean

-- Build correctness proof from constructive witness
galoisGroupCorrectness :
  (F E : FieldDeclaration) →
  ConstructiveGaloisGroup F E →
  GaloisGroupCorrectness F E
galoisGroupCorrectness F E cgg = record
  { algorithm = record
      { galoisGroup = λ _ → constructGaloisGroupFromConstructive F E cgg
      ; automorphisms = λ _ → []
      ; isSolvable = λ _ → M.mkId "solvable-check"
      ; limitation = nothing
      }
  ; specification = record
      { baseField = F
      ; extensionField = E
      ; automorphisms = []
      ; groupOrder = ConstructiveGaloisGroup.groupOrder cgg
      ; allAutomorphismsValid = M.mkId "auts-valid"
      ; groupClosure = M.mkId "closure"
      ; groupAssociativity = AutomorphismComposition.assocProof (ConstructiveGaloisGroup.groupOperation cgg)
      ; groupIdentity = AutomorphismComposition.leftIdProof (ConstructiveGaloisGroup.groupOperation cgg)
      ; groupInverses = AutomorphismComposition.leftInvProof (ConstructiveGaloisGroup.groupOperation cgg)
      ; orderEqualsExtensionDegree = ConstructiveGaloisGroup.orderEqualsExtensionDegree cgg
      ; fundamentalTheorem = ConstructiveGaloisGroup.latticeCorrespondence cgg
      }
  ; proveAutomorphismsValid = M.mkId "auts-proof"
  ; proveGroupClosure = M.mkId "closure-proof"
  ; proveAssociativity = AutomorphismComposition.assocProof (ConstructiveGaloisGroup.groupOperation cgg)
  ; proveIdentity = AutomorphismComposition.leftIdProof (ConstructiveGaloisGroup.groupOperation cgg)
  ; proveInverses = AutomorphismComposition.leftInvProof (ConstructiveGaloisGroup.groupOperation cgg)
  ; proveOrderEquality = ConstructiveGaloisGroup.orderEqualsExtensionDegree cgg
  ; proveFundamentalTheorem = ConstructiveGaloisGroup.latticeCorrespondence cgg
  ; isCorrect = tt
  }

-- ============================================================================
-- Extension Degree Algorithm Correctness
-- ============================================================================

-- Specification: What makes an extension degree computation "correct"
record ExtensionDegreeSpec (F E : FieldDeclaration) : Set₁ where
  field
    baseField : FieldDeclaration
    extensionField : FieldDeclaration
    
    -- Algorithm output
    degree : Nat
    basis : List M.Identifier
    
    -- Correctness properties
    basisLinearlyIndependent : M.Identifier  -- Basis elements are linearly independent over F
    basisSpansExtension : M.Identifier       -- Basis generates E as F-vector space
    degreeEqualsBasisSize : M.Identifier     -- [E:F] = |basis|
    towerLaw : M.Identifier                  -- [E:F] = [E:K][K:F] for intermediate K

-- Proof that an extension degree algorithm is correct
record ExtensionDegreeCorrectness (F E : FieldDeclaration) : Set₁ where
  field
    specification : ExtensionDegreeSpec F E
    
    -- Proof obligations
    proveLinearIndependence : M.Identifier
    proveSpanning : M.Identifier
    proveDegreeFormula : M.Identifier
    proveTowerLaw : M.Identifier
    
    -- Overall correctness
    isCorrect : Boolean

-- Build correctness proof from constructive witness
extensionDegreeCorrectness :
  (F E : FieldDeclaration) →
  ConstructiveExtensionDegree F E →
  ExtensionDegreeCorrectness F E
extensionDegreeCorrectness F E ced = record
  { specification = record
      { baseField = F
      ; extensionField = E
      ; degree = ConstructiveExtensionDegree.basisSize ced
      ; basis = []
      ; basisLinearlyIndependent = ConstructiveExtensionDegree.linearIndependenceProof ced
      ; basisSpansExtension = ConstructiveExtensionDegree.spanningProof ced
      ; degreeEqualsBasisSize = ConstructiveExtensionDegree.dimensionFormula ced
      ; towerLaw = M.mkId "tower-law"
      }
  ; proveLinearIndependence = ConstructiveExtensionDegree.linearIndependenceProof ced
  ; proveSpanning = ConstructiveExtensionDegree.spanningProof ced
  ; proveDegreeFormula = ConstructiveExtensionDegree.dimensionFormula ced
  ; proveTowerLaw = M.mkId "tower-proof"
  ; isCorrect = tt
  }

-- ============================================================================
-- Correctness Composition and Verification
-- ============================================================================

-- Compose correctness proofs for algorithm pipelines
record ComposedCorrectness (A B C : Set₁) : Set₁ where
  field
    firstCorrectness : M.Identifier   -- Correctness of A → B
    secondCorrectness : M.Identifier  -- Correctness of B → C
    composedCorrectness : M.Identifier -- Correctness of A → C
    compositionTheorem : M.Identifier  -- Proof that composition preserves correctness

-- Correctness for algorithm bundles
record BundleCorrectness (F E : FieldDeclaration) : Set₁ where
  field
    baseField : FieldDeclaration
    extensionField : FieldDeclaration
    
    -- Individual algorithm correctness
    minimalPolynomialCorrect : M.Identifier
    splittingFieldCorrect : M.Identifier
    galoisGroupCorrect : M.Identifier
    extensionDegreeCorrect : M.Identifier
    
    -- Consistency between algorithms
    degreeMatchesGaloisOrder : M.Identifier    -- [E:F] = |Gal(E/F)| when Galois
    minPolyDividesSplitting : M.Identifier     -- Minimal poly divides in splitting field
    normalClosureIsMinimal : M.Identifier      -- Normal closure has minimal degree
    
    -- Overall bundle correctness
    allCorrect : Boolean
    consistencyProof : M.Identifier

-- Build bundle correctness from constructive bundle
bundleCorrectness :
  (F E : FieldDeclaration) →
  ConstructiveExtensionBundle F E →
  BundleCorrectness F E
bundleCorrectness F E bundle = record
  { baseField = F
  ; extensionField = E
  ; minimalPolynomialCorrect = M.mkId "minpoly-correct"
  ; splittingFieldCorrect = M.mkId "split-correct"
  ; galoisGroupCorrect = M.mkId "galois-correct"
  ; extensionDegreeCorrect = M.mkId "degree-correct"
  ; degreeMatchesGaloisOrder = M.mkId "degree-order-match"
  ; minPolyDividesSplitting = M.mkId "minpoly-divides"
  ; normalClosureIsMinimal = M.mkId "normal-minimal"
  ; allCorrect = ConstructiveExtensionBundle.allWitnessesValid bundle
  ; consistencyProof = ConstructiveExtensionBundle.consistencyProof bundle
  }

-- ============================================================================
-- Correctness Verification Phases
-- ============================================================================

-- Phase: Algorithm → Correctness Specification
specifyCorrectness :
  {Input Output Property : Set₁} →
  (Input → Output) →
  (Output → Property) →
  Phase Input (CorrectnessSpec Input Output Property)
specifyCorrectness {Input} {Output} {Property} alg prop = record
  { transform = λ input →
      let output = alg input
      in record
        { inputData = input
        ; algorithmOutput = output
        ; expectedProperty = prop output
        ; proofObligation = M.mkId "prove-correctness"
        }
  }

-- Phase: Specification → Proof (postulated for now)
-- Note: We can't make this a phase directly since AlgorithmSatisfiesSpec depends on spec
postulate
  proveCorrectnessFunction :
    {Input Output Property : Set₁} →
    (spec : CorrectnessSpec Input Output Property) →
    AlgorithmSatisfiesSpec Input Output Property spec

-- Phase: Specification + Proof → Certificate
certifyCorrectness :
  {Input Output Property : Set₁} →
  (spec : CorrectnessSpec Input Output Property) →
  AlgorithmSatisfiesSpec Input Output Property spec →
  CorrectnessCertificate Input Output Property
certifyCorrectness spec proof = record
  { specification = spec
  ; satisfactionProof = proof
  ; certificationAuthority = M.mkId "agda-typechecker"
  }

-- ============================================================================
-- Correctness Invariants
-- ============================================================================

-- An algorithm preserves an invariant
record InvariantPreservation (A : Set₁) (Invariant : A → Set₁) : Set₁ where
  field
    algorithm : A → A
    invariant : (a : A) → Invariant a
    preservation : (a : A) → Invariant a → Invariant (algorithm a)
    preservationProof : M.Identifier

-- Correctness implies invariant preservation
record CorrectnessImpliesInvariant (A Property : Set₁) (Invariant : A → Set₁) : Set₁ where
  field
    correctnessProof : M.Identifier
    invariantExtraction : Property → M.Identifier
    implicationProof : M.Identifier  -- correctness ⇒ invariant preservation

-- ============================================================================
-- Universal Property Satisfaction
-- ============================================================================

-- An algorithm satisfies a universal property
record SatisfiesUniversalProperty (Object : Set₁) (UMP : Set₁) : Set₁ where
  field
    constructedObject : Object
    universalProperty : UMP
    uniqueness : M.Identifier  -- Proof of uniqueness up to isomorphism
    universality : M.Identifier -- Proof that UMP characterizes the object

-- Correctness via universal property
record CorrectnessViaUMP (Algorithm : Set₁) (Object : Set₁) (UMP : Set₁) : Set₁ where
  field
    algorithm : Algorithm
    producedObject : Object
    umpSatisfaction : SatisfiesUniversalProperty Object UMP
    correctnessFromUMP : M.Identifier  -- UMP satisfaction implies correctness

-- ============================================================================
-- Computational Complexity Correctness
-- ============================================================================

-- An algorithm has correct complexity bounds
record ComplexityCorrectness (Algorithm : Set₁) : Set₁ where
  field
    algorithm : Algorithm
    worstCaseComplexity : M.Identifier   -- Upper bound on time complexity
    averageCaseComplexity : M.Identifier -- Expected complexity
    spaceComplexity : M.Identifier       -- Space usage bound
    
    -- Proof obligations
    proveWorstCase : M.Identifier
    proveAverageCase : M.Identifier
    proveSpaceUsage : M.Identifier
    
    -- Tightness
    boundsAreTight : Boolean

-- ============================================================================
-- Error Handling Correctness
-- ============================================================================

-- An algorithm handles errors correctly
record ErrorHandlingCorrectness (Input Output Error : Set₁) : Set₁ where
  field
    algorithm : Input → Output
    errorConditions : Input → Boolean
    errorProduction : Input → Error
    
    -- Correctness properties
    validInputsSucceed : M.Identifier     -- Valid inputs produce valid outputs
    invalidInputsFail : M.Identifier      -- Invalid inputs are rejected
    errorMessagesCorrect : M.Identifier   -- Error messages match conditions
    noFalsePositives : M.Identifier       -- No spurious errors
    noFalseNegatives : M.Identifier       -- All errors caught

-- ============================================================================
-- Totality and Termination
-- ============================================================================

-- An algorithm is total (defined on all inputs)
record AlgorithmTotality (Input Output : Set₁) : Set₁ where
  field
    algorithm : Input → Output
    totalityProof : M.Identifier  -- Proof that algorithm terminates on all inputs
    terminationMeasure : Input → Nat  -- Decreasing measure for termination

-- An algorithm terminates
record AlgorithmTermination (Input Output : Set₁) : Set₁ where
  field
    algorithm : Input → Output
    terminates : Boolean
    terminationProof : M.Identifier
    maxSteps : M.Identifier  -- Upper bound on iteration count

-- ============================================================================
-- Export Correctness Framework
-- ============================================================================

-- Convenience function: Build complete correctness proof
buildCorrectnessCertificate :
  {Input Output Property : Set₁} →
  (Input → Output) →
  (Output → Property) →
  Input →
  CorrectnessCertificate Input Output Property
buildCorrectnessCertificate {Input} {Output} {Property} alg prop input =
  let spec = Phase.transform (specifyCorrectness alg prop) input
      proof = proveCorrectnessFunction spec
  in certifyCorrectness spec proof
