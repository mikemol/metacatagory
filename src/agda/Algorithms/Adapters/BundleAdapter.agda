{-# OPTIONS --without-K #-}

-- Algorithms.Adapters.BundleAdapter: Bridge between parameterized algorithms
-- (Algorithms.Basic) and Core.Algorithms.Bundle record used by Registry.
--
-- This adapter constructs an AlgorithmBundle using the Basic module's
-- record constructors, instantiated with Defaults for backward compatibility.

module Algorithms.Adapters.BundleAdapter where

open import Algebra.Rings.Basic using (FieldDeclaration)
open import Core.Algorithms.Bundle using (AlgorithmBundle)
open import Algorithms.Basic
open Algorithms.Basic.Defaults public

-- Default bundle built from Algorithms.Basic (Defaults)
-- Provides a backward-compatible AlgorithmBundle while moving construction
-- away from postulated generic functions toward explicit constructors.

defaultAlgorithmBundle : (F E : FieldDeclaration) → AlgorithmBundle F E
defaultAlgorithmBundle F E = record
  { minimalPolynomialAlg = mkMinimalPolynomialAlgorithm {F} {E}
  ; galoisGroupAlg       = mkGaloisGroupAlgorithm {F} {E}
  ; splittingFieldAlg    = mkSplittingFieldAlgorithm {F}
  ; extensionDegreeAlg   = mkFieldExtensionDegreeAlgorithm {F} {E}
  ; subfieldEnumAlg      = mkSubfieldEnumerationAlgorithm {F} {E}
  ; subgroupEnumAlg      = mkSubgroupEnumerationAlgorithm {F} {E}
  ; algebraicityAlg      = mkAlgebraicityDecisionAlgorithm {F} {E}
  ; primitiveElementAlg  = mkPrimitiveElementAlgorithm {F} {E}
  ; normalityAlg         = mkNormalityDecisionAlgorithm {F} {E}
  ; separabilityAlg      = mkSeparabilityDecisionAlgorithm {F} {E}
  ; normalClosureAlg     = mkNormalClosureAlgorithm {F} {E}
  ; galoisClosureAlg     = mkGaloisClosureAlgorithm {F} {E}
  }
