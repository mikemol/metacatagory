-- Core.AlgebraicAlgorithms: Generic interfaces for computational algebraic algorithms
-- These records provide extensible, type-safe infrastructure for algebraic computation in Agda.

module Core.AlgebraicAlgorithms where

open import Core
open import Algebra.Rings.Basic
open import Algebra.Fields.Basic
open import Algebra.Fields.Advanced
open import Algebra.Groups.Basic
open import Algebra.Foundation
open import Metamodel as M

-- Minimal local decision type to avoid stdlib dependency (target Set₁ in this codebase)
data Dec (A : Set₁) : Set₁ where
  yes : A → Dec A
  no  : Dec A

-- Lists from builtins (avoid stdlib dependency)
open import Agda.Builtin.List using (List; []; _∷_)

-- ============================================================================
-- Minimal Polynomial Computation
-- ============================================================================

record MinimalPolynomialAlgorithm (F : FieldDeclaration) (E : FieldDeclaration) : Set₁ where
  field
    -- Given α ∈ E, compute its minimal polynomial over F
    minimalPolynomial : (α : M.Identifier) → M.Identifier
    -- Decision procedure: is α algebraic?
    isAlgebraic : (α : M.Identifier) → Dec (AlgebraicElement F E α)

-- ============================================================================
-- Galois Group Computation
-- ============================================================================

record GaloisGroupAlgorithm (F : FieldDeclaration) (E : FieldDeclaration) : Set₁ where
  field
    -- Given a polynomial f over F, compute Gal(E/F)
    galoisGroup : (f : M.Identifier) → GaloisGroup F E
    -- Enumerate automorphisms
    automorphisms : (f : M.Identifier) → List (FieldAutomorphism F E)
    -- Decision: is Gal(E/F) solvable?
    isSolvable : (f : M.Identifier) → M.Identifier

-- ============================================================================
-- Splitting Field Construction
-- ============================================================================

record SplittingFieldAlgorithm (F : FieldDeclaration) : Set₁ where
  field
    -- Given a polynomial f over F, construct its splitting field
    splittingField : (f : M.Identifier) → SplittingField F f
    -- Enumerate roots in splitting field
    roots : (f : M.Identifier) → List M.Identifier

-- ============================================================================
-- Field Extension Degree Calculation
-- ============================================================================

record FieldExtensionDegreeAlgorithm (F E : FieldDeclaration) : Set₁ where
  field
    -- Compute [E : F] (dimension of E as F-vector space)
    extensionDegree : ExtensionDegree F E
    -- Find basis of E over F
    basis : List M.Identifier

-- ============================================================================
-- Subfield and Subgroup Enumeration
-- ============================================================================

record SubfieldEnumerationAlgorithm (F E : FieldDeclaration) : Set₁ where
  field
    -- Enumerate all intermediate fields F ⊆ K ⊆ E
    subfields : List (Subfield E)

record SubgroupEnumerationAlgorithm (F E : FieldDeclaration) : Set₁ where
  field
    -- Enumerate all subgroups of Gal(E/F)
    subgroups : List (GroupDeclaration)

-- ============================================================================
-- Algebraicity and Transcendence Decision
-- ============================================================================

record AlgebraicityDecisionAlgorithm (F E : FieldDeclaration) : Set₁ where
  field
    -- Decide if α ∈ E is algebraic over F
    isAlgebraic : (α : M.Identifier) → Dec (AlgebraicElement F E α)
    -- Decide if α is transcendental
    isTranscendental : (α : M.Identifier) → Dec (TranscendentalElement F E α)

-- ==========================================================================
-- Primitive Element (simple extension witness)
-- ==========================================================================

record PrimitiveElementAlgorithm (F E : FieldDeclaration) : Set₁ where
  field
    -- Produce a primitive element α such that E = F(α)
    primitiveElement : M.Identifier
    witnessSimpleExtension : SimpleExtension F E primitiveElement

-- ==========================================================================
-- Normality and Separability Decision Algorithms (Chapter VI)
-- ==========================================================================

record NormalityDecisionAlgorithm (F E : FieldDeclaration) : Set₁ where
  field
    -- Decide if E/F is normal
    isNormal : Dec (NormalExtension F E)

record SeparabilityDecisionAlgorithm (F E : FieldDeclaration) : Set₁ where
  field
    -- Decide if E/F is separable
    isSeparable : Dec (SeparableExtension F E)
    -- Decide if E/F is purely inseparable (using an identifier placeholder)
    isPurelyInseparable : M.Identifier

-- Normal closure construction
record NormalClosureAlgorithm (F E : FieldDeclaration) : Set₁ where
  field
    -- Construct the normal closure of E/F
    normalClosure : M.Identifier
    witnessNormalClosure : M.Identifier

-- Galois closure construction
record GaloisClosureAlgorithm (F E : FieldDeclaration) : Set₁ where
  field
    -- Construct the Galois closure of E/F
    galoisClosure : M.Identifier
    witnessGaloisClosure : M.Identifier

-- Perfect field decision
record PerfectFieldDecisionAlgorithm (F : FieldDeclaration) : Set₁ where
  field
    -- Decide if F is perfect (using identifier placeholder)
    isPerfect : M.Identifier
    -- Decide if F is algebraically closed (using identifier placeholder)
    isAlgebraicallyClosed : M.Identifier

-- ============================================================================
-- Extensibility: External/Verified Computation
-- ============================================================================


-- ==========================================================================
-- Generic scaffolds for all algorithm interfaces
-- ==========================================================================

-- Parametric defaults (postulated) to avoid unsolved metas while enabling instantiation.
postulate
  defaultMinimalPolynomial : (F E : FieldDeclaration) → (α : M.Identifier) → M.Identifier
  defaultIsAlgebraic : (F E : FieldDeclaration) → (α : M.Identifier) → Dec (AlgebraicElement F E α)
  defaultGaloisGroup : (F E : FieldDeclaration) → (f : M.Identifier) → GaloisGroup F E
  defaultAutomorphisms : (F E : FieldDeclaration) → (f : M.Identifier) → List (FieldAutomorphism F E)
  defaultIsSolvable : (F E : FieldDeclaration) → (f : M.Identifier) → M.Identifier
  defaultSplittingField : (F : FieldDeclaration) → (f : M.Identifier) → SplittingField F f
  defaultRoots : (F : FieldDeclaration) → (f : M.Identifier) → List M.Identifier
  defaultExtensionDegree : (F E : FieldDeclaration) → ExtensionDegree F E
  defaultBasis : (F E : FieldDeclaration) → List M.Identifier
  defaultSubfields : (F E : FieldDeclaration) → List (Subfield E)
  defaultSubgroups : (F E : FieldDeclaration) → List (GroupDeclaration)
  defaultDecAlg : (F E : FieldDeclaration) → (α : M.Identifier) → Dec (AlgebraicElement F E α)
  defaultDecTrans : (F E : FieldDeclaration) → (α : M.Identifier) → Dec (TranscendentalElement F E α)
  defaultPrimitiveElement : (F E : FieldDeclaration) → PrimitiveElementAlgorithm F E
  -- Chapter VI defaults
  defaultIsNormal : (F E : FieldDeclaration) → Dec (NormalExtension F E)
  defaultIsSeparable : (F E : FieldDeclaration) → Dec (SeparableExtension F E)
  defaultIsPurelyInseparable : (F E : FieldDeclaration) → M.Identifier
  defaultNormalClosure : (F E : FieldDeclaration) → M.Identifier
  defaultNormalClosureWitness : (F E : FieldDeclaration) → M.Identifier
  defaultGaloisClosure : (F E : FieldDeclaration) → M.Identifier
  defaultGaloisClosureWitness : (F E : FieldDeclaration) → M.Identifier
  defaultIsPerfect : (F : FieldDeclaration) → M.Identifier
  defaultIsAlgebraicallyClosed : (F : FieldDeclaration) → M.Identifier

MinimalPolynomialAlgorithm-generic : ∀ {F E} → MinimalPolynomialAlgorithm F E
MinimalPolynomialAlgorithm-generic {F} {E} = record
  { minimalPolynomial = λ α → defaultMinimalPolynomial F E α
  ; isAlgebraic = λ α → defaultIsAlgebraic F E α
  }

GaloisGroupAlgorithm-generic : ∀ {F E} → GaloisGroupAlgorithm F E
GaloisGroupAlgorithm-generic {F} {E} = record
  { galoisGroup = λ f → defaultGaloisGroup F E f
  ; automorphisms = λ f → defaultAutomorphisms F E f
  ; isSolvable = λ f → defaultIsSolvable F E f
  }

SplittingFieldAlgorithm-generic : ∀ {F} → SplittingFieldAlgorithm F
SplittingFieldAlgorithm-generic {F} = record
  { splittingField = λ f → defaultSplittingField F f
  ; roots = λ f → defaultRoots F f
  }

FieldExtensionDegreeAlgorithm-generic : ∀ {F E} → FieldExtensionDegreeAlgorithm F E
FieldExtensionDegreeAlgorithm-generic {F} {E} = record
  { extensionDegree = defaultExtensionDegree F E
  ; basis = defaultBasis F E
  }

SubfieldEnumerationAlgorithm-generic : ∀ {F E} → SubfieldEnumerationAlgorithm F E
SubfieldEnumerationAlgorithm-generic {F} {E} = record
  { subfields = defaultSubfields F E
  }

SubgroupEnumerationAlgorithm-generic : ∀ {F E} → SubgroupEnumerationAlgorithm F E
SubgroupEnumerationAlgorithm-generic {F} {E} = record
  { subgroups = defaultSubgroups F E
  }

AlgebraicityDecisionAlgorithm-generic : ∀ {F E} → AlgebraicityDecisionAlgorithm F E
AlgebraicityDecisionAlgorithm-generic {F} {E} = record
  { isAlgebraic = λ α → defaultDecAlg F E α
  ; isTranscendental = λ α → defaultDecTrans F E α
  }

PrimitiveElementAlgorithm-generic : ∀ {F E} → PrimitiveElementAlgorithm F E
PrimitiveElementAlgorithm-generic {F} {E} = defaultPrimitiveElement F E

NormalityDecisionAlgorithm-generic : ∀ {F E} → NormalityDecisionAlgorithm F E
NormalityDecisionAlgorithm-generic {F} {E} = record
  { isNormal = defaultIsNormal F E
  }

SeparabilityDecisionAlgorithm-generic : ∀ {F E} → SeparabilityDecisionAlgorithm F E
SeparabilityDecisionAlgorithm-generic {F} {E} = record
  { isSeparable = defaultIsSeparable F E
  ; isPurelyInseparable = defaultIsPurelyInseparable F E
  }

NormalClosureAlgorithm-generic : ∀ {F E} → NormalClosureAlgorithm F E
NormalClosureAlgorithm-generic {F} {E} = record
  { normalClosure = defaultNormalClosure F E
  ; witnessNormalClosure = defaultNormalClosureWitness F E
  }

GaloisClosureAlgorithm-generic : ∀ {F E} → GaloisClosureAlgorithm F E
GaloisClosureAlgorithm-generic {F} {E} = record
  { galoisClosure = defaultGaloisClosure F E
  ; witnessGaloisClosure = defaultGaloisClosureWitness F E
  }

PerfectFieldDecisionAlgorithm-generic : ∀ {F} → PerfectFieldDecisionAlgorithm F
PerfectFieldDecisionAlgorithm-generic {F} = record
  { isPerfect = defaultIsPerfect F
  ; isAlgebraicallyClosed = defaultIsAlgebraicallyClosed F
  }
