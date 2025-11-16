-- Tests.AlgorithmSmokeTests: Smoke tests for algorithm interfaces
-- This module instantiates all algorithm interfaces with minimal dummy structures
-- to verify that the interfaces are well-formed and can be instantiated generically.

module Tests.AlgorithmSmokeTests where

open import Core
open import Algebra.Foundation
open import Algebra.Groups.Abelian
open import Algebra.Rings.Basic
open import Algebra.Fields.Basic
open import Algebra.Fields.Advanced
open import Core.AlgebraicAlgorithms
open import Core.Witnesses
open import Metamodel as M
open import Agda.Builtin.List using (List; []; _∷_)

-- ============================================================================
-- Dummy Field Structures for Testing
-- ============================================================================

-- Build Magma → Semigroup → Monoid → Group → AbelianGroup for (+)
dummyMagma : MagmaDeclaration
dummyMagma = record
  { underlyingSet = M.mkId "DummyField"
  ; binaryOp = M.mkId "dummy-add"
  }

dummySemigroup : SemigroupDeclaration
dummySemigroup = record
  { underlyingMagma = dummyMagma
  ; associativity = record { over = M.mkId "dummy-add-assoc" }
  }

dummyMonoid : MonoidDeclaration
dummyMonoid = record
  { underlyingSemigroup = dummySemigroup
  ; identityElement = M.mkId "dummy-zero"
  ; identityAxiom = record { over = M.mkId "dummy-add-id" }
  }

dummyInverse : InverseOperation
dummyInverse = record
  { forMonoid = dummyMonoid
  ; inverseMap = M.mkId "dummy-neg"
  ; inverseAxiom = M.mkId "dummy-inv-ax"
  }

dummyGroup : GroupDeclaration
dummyGroup = record
  { underlyingMonoid = dummyMonoid
  ; inverseOperation = dummyInverse
  }

dummyAdditiveGroup : AbelianGroupDeclaration
dummyAdditiveGroup = record
  { underlyingGroup = dummyGroup
  ; commutativity = record { forGroup = dummyGroup ; axiom = M.mkId "dummy-add-comm" }
  }

dummyRing : RingDeclaration
dummyRing = record
  { identifier = M.mkId "DummyRing"
  ; additiveGroup = dummyAdditiveGroup
  ; multiplication = M.mkId "dummy-mul"
  ; multAssociative = M.mkId "dummy-mul-assoc"
  ; leftDistributive = M.mkId "dummy-left-dist"
  ; rightDistributive = M.mkId "dummy-right-dist"
  }

dummyUnitalRing : UnitalRingDeclaration
dummyUnitalRing = record
  { underlyingRing = dummyRing
  ; multiplicativeIdentity = M.mkId "dummy-one"
  ; leftIdentity = M.mkId "dummy-left-id"
  ; rightIdentity = M.mkId "dummy-right-id"
  }

dummyCommRing : CommutativeRingDeclaration
dummyCommRing = record
  { underlyingRing = dummyUnitalRing
  ; commutativity = M.mkId "dummy-mul-comm"
  }

dummyField : FieldDeclaration
dummyField = record
  { underlyingRing = dummyCommRing
  ; inverses = M.mkId "dummy-inverses"
  }

-- Extension field
dummyExtMagma : MagmaDeclaration
dummyExtMagma = record
  { underlyingSet = M.mkId "DummyExtension"
  ; binaryOp = M.mkId "ext-add"
  }

dummyExtSemigroup : SemigroupDeclaration
dummyExtSemigroup = record
  { underlyingMagma = dummyExtMagma
  ; associativity = record { over = M.mkId "ext-add-assoc" }
  }

dummyExtMonoid : MonoidDeclaration
dummyExtMonoid = record
  { underlyingSemigroup = dummyExtSemigroup
  ; identityElement = M.mkId "ext-zero"
  ; identityAxiom = record { over = M.mkId "ext-add-id" }
  }

dummyExtInverse : InverseOperation
dummyExtInverse = record
  { forMonoid = dummyExtMonoid
  ; inverseMap = M.mkId "ext-neg"
  ; inverseAxiom = M.mkId "ext-inv-ax"
  }

dummyExtGroup : GroupDeclaration
dummyExtGroup = record
  { underlyingMonoid = dummyExtMonoid
  ; inverseOperation = dummyExtInverse
  }

dummyExtAdditiveGroup : AbelianGroupDeclaration
dummyExtAdditiveGroup = record
  { underlyingGroup = dummyExtGroup
  ; commutativity = record { forGroup = dummyExtGroup ; axiom = M.mkId "ext-add-comm" }
  }

dummyExtRing : RingDeclaration
dummyExtRing = record
  { identifier = M.mkId "DummyExtensionRing"
  ; additiveGroup = dummyExtAdditiveGroup
  ; multiplication = M.mkId "ext-mul"
  ; multAssociative = M.mkId "ext-mul-assoc"
  ; leftDistributive = M.mkId "ext-left-dist"
  ; rightDistributive = M.mkId "ext-right-dist"
  }

dummyExtUnitalRing : UnitalRingDeclaration
dummyExtUnitalRing = record
  { underlyingRing = dummyExtRing
  ; multiplicativeIdentity = M.mkId "ext-one"
  ; leftIdentity = M.mkId "ext-left-id"
  ; rightIdentity = M.mkId "ext-right-id"
  }

dummyExtCommRing : CommutativeRingDeclaration
dummyExtCommRing = record
  { underlyingRing = dummyExtUnitalRing
  ; commutativity = M.mkId "ext-mul-comm"
  }

dummyExtension : FieldDeclaration
dummyExtension = record
  { underlyingRing = dummyExtCommRing
  ; inverses = M.mkId "ext-inverses"
  }

-- ============================================================================
-- Smoke Test 1: MinimalPolynomialAlgorithm
-- ============================================================================

testMinimalPolynomialAlgorithm : MinimalPolynomialAlgorithm dummyField dummyExtension
testMinimalPolynomialAlgorithm = MinimalPolynomialAlgorithm-generic {dummyField} {dummyExtension}

testMinimalPolynomial : M.Identifier
testMinimalPolynomial = MinimalPolynomialAlgorithm.minimalPolynomial testMinimalPolynomialAlgorithm (M.mkId "α")

-- ============================================================================
-- Smoke Test 2: GaloisGroupAlgorithm
-- ============================================================================

testGaloisGroupAlgorithm : GaloisGroupAlgorithm dummyField dummyExtension
testGaloisGroupAlgorithm = GaloisGroupAlgorithm-generic {dummyField} {dummyExtension}

testGaloisGroup : GaloisGroup dummyField dummyExtension
testGaloisGroup = GaloisGroupAlgorithm.galoisGroup testGaloisGroupAlgorithm (M.mkId "f")

-- ============================================================================
-- Smoke Test 3: SplittingFieldAlgorithm
-- ============================================================================

testSplittingFieldAlgorithm : SplittingFieldAlgorithm dummyField
testSplittingFieldAlgorithm = SplittingFieldAlgorithm-generic {dummyField}

testSplittingField : SplittingField dummyField (M.mkId "f")
testSplittingField = SplittingFieldAlgorithm.splittingField testSplittingFieldAlgorithm (M.mkId "f")

-- =========================================================================
-- Smoke Test 4: FieldExtensionDegreeAlgorithm
-- =========================================================================

testExtensionDegreeAlgorithm : FieldExtensionDegreeAlgorithm dummyField dummyExtension
testExtensionDegreeAlgorithm = FieldExtensionDegreeAlgorithm-generic {dummyField} {dummyExtension}

testExtensionDegree : ExtensionDegree dummyField dummyExtension
testExtensionDegree = FieldExtensionDegreeAlgorithm.extensionDegree testExtensionDegreeAlgorithm

testBasis : List M.Identifier
testBasis = FieldExtensionDegreeAlgorithm.basis testExtensionDegreeAlgorithm

-- =========================================================================
-- Smoke Test 5: SubfieldEnumerationAlgorithm
-- =========================================================================

testSubfieldEnumerationAlgorithm : SubfieldEnumerationAlgorithm dummyField dummyExtension
testSubfieldEnumerationAlgorithm = SubfieldEnumerationAlgorithm-generic {dummyField} {dummyExtension}

testSubfields : List (Subfield dummyExtension)
testSubfields = SubfieldEnumerationAlgorithm.subfields testSubfieldEnumerationAlgorithm

-- =========================================================================
-- Smoke Test 6: SubgroupEnumerationAlgorithm
-- =========================================================================

testSubgroupEnumerationAlgorithm : SubgroupEnumerationAlgorithm dummyField dummyExtension
testSubgroupEnumerationAlgorithm = SubgroupEnumerationAlgorithm-generic {dummyField} {dummyExtension}

testSubgroups : List GroupDeclaration
testSubgroups = SubgroupEnumerationAlgorithm.subgroups testSubgroupEnumerationAlgorithm

-- =========================================================================
-- Smoke Test 7: AlgebraicityDecisionAlgorithm
-- =========================================================================

testAlgebraicityDecisionAlgorithm : AlgebraicityDecisionAlgorithm dummyField dummyExtension
testAlgebraicityDecisionAlgorithm = AlgebraicityDecisionAlgorithm-generic {dummyField} {dummyExtension}

testIsAlgebraic : Dec (AlgebraicElement dummyField dummyExtension (M.mkId "α"))
testIsAlgebraic = AlgebraicityDecisionAlgorithm.isAlgebraic testAlgebraicityDecisionAlgorithm (M.mkId "α")

testIsTranscendental : Dec (TranscendentalElement dummyField dummyExtension (M.mkId "α"))
testIsTranscendental = AlgebraicityDecisionAlgorithm.isTranscendental testAlgebraicityDecisionAlgorithm (M.mkId "α")

-- =========================================================================
-- Smoke Test 8: PrimitiveElementAlgorithm
-- =========================================================================

testPrimitiveElementAlgorithm : PrimitiveElementAlgorithm dummyField dummyExtension
testPrimitiveElementAlgorithm = PrimitiveElementAlgorithm-generic {dummyField} {dummyExtension}

testPrimitiveElement : M.Identifier
testPrimitiveElement = PrimitiveElementAlgorithm.primitiveElement testPrimitiveElementAlgorithm

testWitnessSimpleExtension : SimpleExtension dummyField dummyExtension (PrimitiveElementAlgorithm.primitiveElement testPrimitiveElementAlgorithm)
testWitnessSimpleExtension = PrimitiveElementAlgorithm.witnessSimpleExtension testPrimitiveElementAlgorithm

-- =========================================================================
-- Smoke Test 9: NormalityDecisionAlgorithm
-- =========================================================================

testNormalityDecisionAlgorithm : NormalityDecisionAlgorithm dummyField dummyExtension
testNormalityDecisionAlgorithm = NormalityDecisionAlgorithm-generic {dummyField} {dummyExtension}

testDecideNormal : Dec (NormalExtension dummyField dummyExtension)
testDecideNormal = NormalityDecisionAlgorithm.isNormal testNormalityDecisionAlgorithm

-- =========================================================================
-- Smoke Test 10: SeparabilityDecisionAlgorithm
-- =========================================================================

testSeparabilityDecisionAlgorithm : SeparabilityDecisionAlgorithm dummyField dummyExtension
testSeparabilityDecisionAlgorithm = SeparabilityDecisionAlgorithm-generic {dummyField} {dummyExtension}

testDecideSeparable : Dec (SeparableExtension dummyField dummyExtension)
testDecideSeparable = SeparabilityDecisionAlgorithm.isSeparable testSeparabilityDecisionAlgorithm

testPurelyInseparableMarker : M.Identifier
testPurelyInseparableMarker = SeparabilityDecisionAlgorithm.isPurelyInseparable testSeparabilityDecisionAlgorithm

-- =========================================================================
-- Smoke Test 11: NormalClosureAlgorithm
-- =========================================================================

testNormalClosureAlgorithm : NormalClosureAlgorithm dummyField dummyExtension
testNormalClosureAlgorithm = NormalClosureAlgorithm-generic {dummyField} {dummyExtension}

testNormalClosure : M.Identifier
testNormalClosure = NormalClosureAlgorithm.normalClosure testNormalClosureAlgorithm

testNormalClosureWitness : M.Identifier
testNormalClosureWitness = NormalClosureAlgorithm.witnessNormalClosure testNormalClosureAlgorithm

-- =========================================================================
-- Smoke Test 12: GaloisClosureAlgorithm
-- =========================================================================

testGaloisClosureAlgorithm : GaloisClosureAlgorithm dummyField dummyExtension
testGaloisClosureAlgorithm = GaloisClosureAlgorithm-generic {dummyField} {dummyExtension}

testGaloisClosure : M.Identifier
testGaloisClosure = GaloisClosureAlgorithm.galoisClosure testGaloisClosureAlgorithm

testGaloisClosureWitness : M.Identifier
testGaloisClosureWitness = GaloisClosureAlgorithm.witnessGaloisClosure testGaloisClosureAlgorithm

-- =========================================================================
-- Smoke Test 13: PerfectFieldDecisionAlgorithm
-- =========================================================================

testPerfectFieldDecisionAlgorithm : PerfectFieldDecisionAlgorithm dummyField
testPerfectFieldDecisionAlgorithm = PerfectFieldDecisionAlgorithm-generic {dummyField}

testDecidePerfect : M.Identifier
testDecidePerfect = PerfectFieldDecisionAlgorithm.isPerfect testPerfectFieldDecisionAlgorithm

testDecideAlgebraicallyClosed : M.Identifier
testDecideAlgebraicallyClosed = PerfectFieldDecisionAlgorithm.isAlgebraicallyClosed testPerfectFieldDecisionAlgorithm

-- ============================================================================
-- Smoke Test 14: All Algorithm Interfaces Marker
-- ============================================================================

-- If this typechecks, all algorithm interfaces are well-formed
smokeTestsPass : M.Identifier
smokeTestsPass = M.mkId "✓ All algorithm interfaces typecheck successfully"
