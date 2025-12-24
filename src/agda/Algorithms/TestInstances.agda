{-# OPTIONS --without-K #-}

-- Algorithms.TestInstances: Reusable dummy algebraic structures for testing
-- PHASE-II.2: Extracted from Core.AlgebraicAlgorithms (lines 29-177)
--
-- These packed structures serve as smoke test fixtures for algorithm development.
-- Instead of hardcoding these in the core module, they're isolated for testing.

module Algorithms.TestInstances where

open import Core
open import Algebra.Rings.Basic
open import Algebra.Fields.Basic
open import Algebra.Groups.Basic
open import Algebra.Foundation
open import Metamodel as M

-- ============================================================================
-- Base Test Structures (DummyField)
-- ============================================================================

packedMagmaBase : MagmaDeclaration
packedMagmaBase = record
  { underlyingSet = M.mkId "DummyField"
  ; binaryOp = M.mkId "dummy-add"
  ; index = magmaIndex
  }

packedSemigroupBase : SemigroupDeclaration
packedSemigroupBase = record
  { underlyingMagma = packedMagmaBase
  ; associativity = record { over = M.mkId "dummy-add-assoc" }
  ; index = semigroupIndex
  }

packedMonoidBase : MonoidDeclaration
packedMonoidBase = record
  { underlyingSemigroup = packedSemigroupBase
  ; identityElement = M.mkId "dummy-zero"
  ; identityAxiom = record { over = M.mkId "dummy-add-id" }
  ; index = monoidIndex
  }

packedInverseBase : InverseOperation
packedInverseBase = record
  { forMonoid = packedMonoidBase
  ; inverseMap = M.mkId "dummy-neg"
  ; inverseAxiom = M.mkId "dummy-inv-ax"
  }

packedGroupBase : GroupDeclaration
packedGroupBase = record
  { underlyingMonoid = packedMonoidBase
  ; inverseOperation = packedInverseBase
  ; index = groupIndex
  }

packedAbelianGroupBase : AbelianGroupDeclaration
packedAbelianGroupBase = record
  { underlyingGroup = packedGroupBase
  ; commutativity = record { forGroup = packedGroupBase ; axiom = M.mkId "dummy-add-comm" }
  ; index = abelianGroupIndex
  }

packedRingBase : RingDeclaration
packedRingBase = record
  { identifier = M.mkId "DummyRing"
  ; additiveGroup = packedAbelianGroupBase
  ; multiplication = M.mkId "dummy-mul"
  ; multAssociative = M.mkId "dummy-mul-assoc"
  ; leftDistributive = M.mkId "dummy-left-dist"
  ; rightDistributive = M.mkId "dummy-right-dist"
  }

packedUnitalRingBase : UnitalRingDeclaration
packedUnitalRingBase = record
  { underlyingRing = packedRingBase
  ; multiplicativeIdentity = M.mkId "dummy-one"
  ; leftIdentity = M.mkId "dummy-left-id"
  ; rightIdentity = M.mkId "dummy-right-id"
  }

packedCommRingBase : CommutativeRingDeclaration
packedCommRingBase = record
  { underlyingRing = packedUnitalRingBase
  ; commutativity = M.mkId "dummy-mul-comm"
  }

packedFieldBase : FieldDeclaration
packedFieldBase = record
  { underlyingRing = packedCommRingBase
  ; inverses = M.mkId "dummy-inverses"
  }

-- ============================================================================
-- Extension Test Structures (DummyExtension)
-- ============================================================================

packedMagmaExt : MagmaDeclaration
packedMagmaExt = record
  { underlyingSet = M.mkId "DummyExtension"
  ; binaryOp = M.mkId "ext-add"
  ; index = magmaIndex
  }

packedSemigroupExt : SemigroupDeclaration
packedSemigroupExt = record
  { underlyingMagma = packedMagmaExt
  ; associativity = record { over = M.mkId "ext-add-assoc" }
  ; index = semigroupIndex
  }

packedMonoidExt : MonoidDeclaration
packedMonoidExt = record
  { underlyingSemigroup = packedSemigroupExt
  ; identityElement = M.mkId "ext-zero"
  ; identityAxiom = record { over = M.mkId "ext-add-id" }
  ; index = monoidIndex
  }

packedInverseExt : InverseOperation
packedInverseExt = record
  { forMonoid = packedMonoidExt
  ; inverseMap = M.mkId "ext-neg"
  ; inverseAxiom = M.mkId "ext-inv-ax"
  }

packedGroupExt : GroupDeclaration
packedGroupExt = record
  { underlyingMonoid = packedMonoidExt
  ; inverseOperation = packedInverseExt
  ; index = groupIndex
  }

packedAbelianGroupExt : AbelianGroupDeclaration
packedAbelianGroupExt = record
  { underlyingGroup = packedGroupExt
  ; commutativity = record { forGroup = packedGroupExt ; axiom = M.mkId "ext-add-comm" }
  ; index = abelianGroupIndex
  }

packedRingExt : RingDeclaration
packedRingExt = record
  { identifier = M.mkId "DummyExtensionRing"
  ; additiveGroup = packedAbelianGroupExt
  ; multiplication = M.mkId "ext-mul"
  ; multAssociative = M.mkId "ext-mul-assoc"
  ; leftDistributive = M.mkId "ext-left-dist"
  ; rightDistributive = M.mkId "ext-right-dist"
  }

packedUnitalRingExt : UnitalRingDeclaration
packedUnitalRingExt = record
  { underlyingRing = packedRingExt
  ; multiplicativeIdentity = M.mkId "ext-one"
  ; leftIdentity = M.mkId "ext-left-id"
  ; rightIdentity = M.mkId "ext-right-id"
  }

packedCommRingExt : CommutativeRingDeclaration
packedCommRingExt = record
  { underlyingRing = packedUnitalRingExt
  ; commutativity = M.mkId "ext-mul-comm"
  }

packedFieldExt : FieldDeclaration
packedFieldExt = record
  { underlyingRing = packedCommRingExt
  ; inverses = M.mkId "ext-inverses"
  }

-- ============================================================================
-- Convenience Exports
-- ============================================================================

-- Common test fixtures
dummyBaseField : FieldDeclaration
dummyBaseField = packedFieldBase

dummyExtField : FieldDeclaration
dummyExtField = packedFieldExt

dummyRing : RingDeclaration
dummyRing = packedRingBase

dummyAbelianGroup : AbelianGroupDeclaration
dummyAbelianGroup = packedAbelianGroupBase

dummyExtensionAbelianGroup : AbelianGroupDeclaration
dummyExtensionAbelianGroup = packedAbelianGroupExt
