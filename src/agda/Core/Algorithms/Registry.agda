-- Core.Algorithms.Registry: Centralized algorithm discovery and dispatch
-- This module provides a unified interface for finding and invoking algebraic algorithms
-- based on field types and problem categories, enabling systematic extension and reuse.

module Core.Algorithms.Registry where

open import Core
open import Algebra.Foundation
open import Algebra.Rings.Basic
open import Algebra.Fields.Basic
open import Algebra.Fields.Advanced
open import Core.AlgebraicAlgorithms
open import Core.Algorithms.FiniteFields
open import Metamodel as M
open import Agda.Builtin.List using (List; []; _∷_)

-- ============================================================================
-- Algorithm Categories
-- ============================================================================

-- Enumeration of available algorithm problem types
data AlgorithmCategory : Set where
  MinimalPolynomialComputation : AlgorithmCategory
  GaloisGroupComputation       : AlgorithmCategory
  SplittingFieldConstruction   : AlgorithmCategory
  ExtensionDegreeCalculation   : AlgorithmCategory
  SubfieldEnumeration          : AlgorithmCategory
  SubgroupEnumeration          : AlgorithmCategory
  AlgebraicityDecision         : AlgorithmCategory
  PrimitiveElementConstruction : AlgorithmCategory
  NormalityDecision            : AlgorithmCategory
  SeparabilityDecision         : AlgorithmCategory
  NormalClosureConstruction    : AlgorithmCategory
  GaloisClosureConstruction    : AlgorithmCategory
  PerfectFieldDecision         : AlgorithmCategory

-- ============================================================================
-- Field Type Classification
-- ============================================================================

-- Field type tags for dispatch
data FieldType : Set where
  FiniteFieldType   : FieldType
  NumberFieldType   : FieldType
  FunctionFieldType : FieldType
  GenericFieldType  : FieldType

-- Predicate to classify a field (extensible)
-- In practice, this would inspect field properties or carry explicit evidence
postulate
  classifyField : (F : FieldDeclaration) → FieldType

-- ============================================================================
-- Algorithm Bundle Registry
-- ============================================================================

-- Full suite of algorithms for a field extension E/F
record AlgorithmBundle (F E : FieldDeclaration) : Set₁ where
  field
    minimalPolynomialAlg : MinimalPolynomialAlgorithm F E
    galoisGroupAlg       : GaloisGroupAlgorithm F E
    splittingFieldAlg    : SplittingFieldAlgorithm F
    extensionDegreeAlg   : FieldExtensionDegreeAlgorithm F E
    subfieldEnumAlg      : SubfieldEnumerationAlgorithm F E
    subgroupEnumAlg      : SubgroupEnumerationAlgorithm F E
    algebraicityAlg      : AlgebraicityDecisionAlgorithm F E
    primitiveElementAlg  : PrimitiveElementAlgorithm F E
    normalityAlg         : NormalityDecisionAlgorithm F E
    separabilityAlg      : SeparabilityDecisionAlgorithm F E
    normalClosureAlg     : NormalClosureAlgorithm F E
    galoisClosureAlg     : GaloisClosureAlgorithm F E

open AlgorithmBundle public

-- Generic fallback bundle using all defaults
genericAlgorithmBundle : (F E : FieldDeclaration) → AlgorithmBundle F E
genericAlgorithmBundle F E = record
  { minimalPolynomialAlg = MinimalPolynomialAlgorithm-generic {F} {E}
  ; galoisGroupAlg       = GaloisGroupAlgorithm-generic {F} {E}
  ; splittingFieldAlg    = SplittingFieldAlgorithm-generic {F}
  ; extensionDegreeAlg   = FieldExtensionDegreeAlgorithm-generic {F} {E}
  ; subfieldEnumAlg      = SubfieldEnumerationAlgorithm-generic {F} {E}
  ; subgroupEnumAlg      = SubgroupEnumerationAlgorithm-generic {F} {E}
  ; algebraicityAlg      = AlgebraicityDecisionAlgorithm-generic {F} {E}
  ; primitiveElementAlg  = PrimitiveElementAlgorithm-generic {F} {E}
  ; normalityAlg         = NormalityDecisionAlgorithm-generic {F} {E}
  ; separabilityAlg      = SeparabilityDecisionAlgorithm-generic {F} {E}
  ; normalClosureAlg     = NormalClosureAlgorithm-generic {F} {E}
  ; galoisClosureAlg     = GaloisClosureAlgorithm-generic {F} {E}
  }

-- ============================================================================
-- Specialized Bundles (Registry Entries)
-- ============================================================================

-- Finite field bundle (requires explicit IsFiniteField evidence)
finiteFieldBundle : (F E : FieldDeclaration) → IsFiniteField F → IsFiniteField E → AlgorithmBundle F E
finiteFieldBundle F E Ffin Efin =
  let ffAlgs = finiteFieldAlgorithms Ffin Efin
  in record
    { minimalPolynomialAlg = FiniteFieldAlgorithms.minimalPolynomialAlg ffAlgs
    ; galoisGroupAlg       = FiniteFieldAlgorithms.galoisGroupAlg ffAlgs
    ; splittingFieldAlg    = FiniteFieldAlgorithms.splittingFieldAlg ffAlgs
    ; extensionDegreeAlg   = FiniteFieldAlgorithms.extensionDegreeAlg ffAlgs
    ; subfieldEnumAlg      = FiniteFieldAlgorithms.subfieldEnumAlg ffAlgs
    ; subgroupEnumAlg      = FiniteFieldAlgorithms.subgroupEnumAlg ffAlgs
    ; algebraicityAlg      = FiniteFieldAlgorithms.algebraicityAlg ffAlgs
    ; primitiveElementAlg  = FiniteFieldAlgorithms.primitiveElementAlg ffAlgs
    ; normalityAlg         = NormalityDecisionAlgorithm-generic {F} {E}
    ; separabilityAlg      = SeparabilityDecisionAlgorithm-generic {F} {E}
    ; normalClosureAlg     = NormalClosureAlgorithm-generic {F} {E}
    ; galoisClosureAlg     = GaloisClosureAlgorithm-generic {F} {E}
    }

-- ============================================================================
-- Central Dispatch (Algorithm Lookup)
-- ============================================================================

-- Lookup a full algorithm bundle based on field type classification
-- This is the main entry point for algorithm discovery
lookupAlgorithmBundle : (F E : FieldDeclaration) → AlgorithmBundle F E
lookupAlgorithmBundle F E = genericAlgorithmBundle F E
  -- Future enhancement: pattern match on classifyField F and classifyField E
  -- to dispatch to specialized bundles (finite field, number field, etc.)

-- Single-algorithm lookup by category
lookupMinimalPolynomial : (F E : FieldDeclaration) → MinimalPolynomialAlgorithm F E
lookupMinimalPolynomial F E = AlgorithmBundle.minimalPolynomialAlg (lookupAlgorithmBundle F E)

lookupGaloisGroup : (F E : FieldDeclaration) → GaloisGroupAlgorithm F E
lookupGaloisGroup F E = AlgorithmBundle.galoisGroupAlg (lookupAlgorithmBundle F E)

lookupSplittingField : (F : FieldDeclaration) → SplittingFieldAlgorithm F
lookupSplittingField F = AlgorithmBundle.splittingFieldAlg (lookupAlgorithmBundle F F)

lookupExtensionDegree : (F E : FieldDeclaration) → FieldExtensionDegreeAlgorithm F E
lookupExtensionDegree F E = AlgorithmBundle.extensionDegreeAlg (lookupAlgorithmBundle F E)

lookupSubfieldEnumeration : (F E : FieldDeclaration) → SubfieldEnumerationAlgorithm F E
lookupSubfieldEnumeration F E = AlgorithmBundle.subfieldEnumAlg (lookupAlgorithmBundle F E)

lookupSubgroupEnumeration : (F E : FieldDeclaration) → SubgroupEnumerationAlgorithm F E
lookupSubgroupEnumeration F E = AlgorithmBundle.subgroupEnumAlg (lookupAlgorithmBundle F E)

lookupAlgebraicityDecision : (F E : FieldDeclaration) → AlgebraicityDecisionAlgorithm F E
lookupAlgebraicityDecision F E = AlgorithmBundle.algebraicityAlg (lookupAlgorithmBundle F E)

lookupPrimitiveElement : (F E : FieldDeclaration) → PrimitiveElementAlgorithm F E
lookupPrimitiveElement F E = AlgorithmBundle.primitiveElementAlg (lookupAlgorithmBundle F E)

lookupNormalityDecision : (F E : FieldDeclaration) → NormalityDecisionAlgorithm F E
lookupNormalityDecision F E = AlgorithmBundle.normalityAlg (lookupAlgorithmBundle F E)

lookupSeparabilityDecision : (F E : FieldDeclaration) → SeparabilityDecisionAlgorithm F E
lookupSeparabilityDecision F E = AlgorithmBundle.separabilityAlg (lookupAlgorithmBundle F E)

lookupNormalClosure : (F E : FieldDeclaration) → NormalClosureAlgorithm F E
lookupNormalClosure F E = AlgorithmBundle.normalClosureAlg (lookupAlgorithmBundle F E)

lookupGaloisClosure : (F E : FieldDeclaration) → GaloisClosureAlgorithm F E
lookupGaloisClosure F E = AlgorithmBundle.galoisClosureAlg (lookupAlgorithmBundle F E)

-- ============================================================================
-- Registration Helpers (for extensibility)
-- ============================================================================

-- To add a new specialized bundle:
-- 1. Define your bundle constructor (like finiteFieldBundle above)
-- 2. Extend lookupAlgorithmBundle to dispatch to it based on classifyField
-- 3. Optionally add convenience lookups for common cases

-- Example: register a number field bundle (future)
-- numberFieldBundle : (F E : FieldDeclaration) → IsNumberField F → IsNumberField E → AlgorithmBundle F E
-- numberFieldBundle F E Fnf Enf = ...
