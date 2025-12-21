{-# OPTIONS --without-K #-}

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

-- Reuse packed base chain from Core.AlgebraicAlgorithms
dummyMagma : MagmaDeclaration
dummyMagma = packedMagmaBase

dummySemigroup : SemigroupDeclaration
dummySemigroup = packedSemigroupBase

dummyMonoid : MonoidDeclaration
dummyMonoid = packedMonoidBase

dummyInverse : InverseOperation
dummyInverse = packedInverseBase

dummyGroup : GroupDeclaration
dummyGroup = packedGroupBase

dummyAdditiveGroup : AbelianGroupDeclaration
dummyAdditiveGroup = packedAbelianGroupBase

dummyRing : RingDeclaration
dummyRing = packedRingBase

dummyUnitalRing : UnitalRingDeclaration
dummyUnitalRing = packedUnitalRingBase

dummyCommRing : CommutativeRingDeclaration
dummyCommRing = packedCommRingBase

dummyField : FieldDeclaration
dummyField = packedFieldBase

-- Extension field: reuse packed extension chain
dummyExtMagma : MagmaDeclaration
dummyExtMagma = packedMagmaExt

dummyExtSemigroup : SemigroupDeclaration
dummyExtSemigroup = packedSemigroupExt

dummyExtMonoid : MonoidDeclaration
dummyExtMonoid = packedMonoidExt

dummyExtInverse : InverseOperation
dummyExtInverse = packedInverseExt

dummyExtGroup : GroupDeclaration
dummyExtGroup = packedGroupExt

dummyExtAdditiveGroup : AbelianGroupDeclaration
dummyExtAdditiveGroup = packedAbelianGroupExt

dummyExtRing : RingDeclaration
dummyExtRing = packedRingExt

dummyExtUnitalRing : UnitalRingDeclaration
dummyExtUnitalRing = packedUnitalRingExt

dummyExtCommRing : CommutativeRingDeclaration
dummyExtCommRing = packedCommRingExt

dummyExtension : FieldDeclaration
dummyExtension = packedFieldExt

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
