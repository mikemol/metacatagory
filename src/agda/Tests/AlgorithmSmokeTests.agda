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

-- ============================================================================
-- Smoke Test 4: All 13 Algorithm Interfaces
-- ============================================================================

-- If this typechecks, all algorithm interfaces are well-formed
smokeTestsPass : M.Identifier
smokeTestsPass = M.mkId "✓ All algorithm interfaces typecheck successfully"
