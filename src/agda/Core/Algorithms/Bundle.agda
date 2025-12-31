{-# OPTIONS --without-K #-}

-- Core.Algorithms.Bundle: Shared AlgorithmBundle type for registry infrastructure
-- This module defines the unified bundle interface that all specialized algorithm
-- modules (FiniteFields, NumberFields, etc.) can implement and Registry can dispatch.

module Core.Algorithms.Bundle where

open import Core.AlgebraicAlgorithms
open import Algebra.Rings.Basic
open import Algebra.Fields.Basic

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
