{-# OPTIONS --without-K #-}

-- | Algorithms and witnesses over number fields.
module Core.Algorithms.NumberFields where

open import Core
open import Algebra.Foundation
open import Algebra.Rings.Basic
open import Algebra.Fields.Basic
open import Algebra.Fields.Advanced
open import Core.AlgebraicAlgorithms
open import Core.Algorithms.Bundle
open import Core.Witnesses
open import Metamodel as M
open import Agda.Builtin.List using (List; []; _∷_)
-- Import External Oracle capability
open import Core.Algorithms.External
open import Algorithms.Basic
open Algorithms.Basic.Defaults

-- Evidence that a field is a number field
postulate
  IsNumberField : FieldDeclaration → Set

-- Configuration for Number Field computations (e.g., using PARI/GP or Sage)
numberFieldExternalConfig : ExternalConfig
numberFieldExternalConfig = record
  { system = PariBackend  -- Number theory specialized system
  ; endpoint = "gp"
  ; timeout = M.mkId "60s"
  }

-- Minimal specialization bundle for number fields E/F
record NumberFieldAlgorithms (F E : FieldDeclaration)
                             (Fnf : IsNumberField F)
                             (Enf : IsNumberField E) : Set₁ where
  field
    minimalPolynomialAlg : MinimalPolynomialAlgorithm F E
    galoisGroupAlg       : GaloisGroupAlgorithm F E
    splittingFieldAlg    : SplittingFieldAlgorithm F
    extensionDegreeAlg   : FieldExtensionDegreeAlgorithm F E
    subfieldEnumAlg      : SubfieldEnumerationAlgorithm F E
    algebraicityAlg      : AlgebraicityDecisionAlgorithm F E
    primitiveElementAlg  : PrimitiveElementAlgorithm F E

open NumberFieldAlgorithms public

-- Instantiation using External Oracles for hard problems
numberFieldAlgorithms : ∀ {F E} → (Fnf : IsNumberField F) → (Enf : IsNumberField E)
                       → NumberFieldAlgorithms F E Fnf Enf
numberFieldAlgorithms {F} {E} Fnf Enf = record
  -- CORRECTED: Use external oracle for MinPoly
  { minimalPolynomialAlg = externalMinimalPolynomialAlgorithm F E numberFieldExternalConfig
  -- CORRECTED: Use external oracle for Galois Group
  ; galoisGroupAlg       = externalGaloisGroupAlgorithm F E numberFieldExternalConfig
  -- CORRECTED: Use external oracle for Splitting Field
  ; splittingFieldAlg    = externalSplittingFieldAlgorithm F numberFieldExternalConfig
  -- CORRECTED: Use external oracle for Extension Degree
  ; extensionDegreeAlg   = externalExtensionDegreeAlgorithm F E numberFieldExternalConfig
  -- CORRECTED: Use external oracle for Subfields
  ; subfieldEnumAlg      = externalSubfieldEnumerationAlgorithm F E numberFieldExternalConfig
  -- Fallback to generics where external isn't critical yet
  ; algebraicityAlg      = mkAlgebraicityDecisionAlgorithm {F} {E}
  ; primitiveElementAlg  = mkPrimitiveElementAlgorithm {F} {E}
  }

-- Convenience: construct full registry bundle for number fields
numberFieldBundle : (F E : FieldDeclaration) → IsNumberField F → IsNumberField E → AlgorithmBundle F E
numberFieldBundle F E Fnf Enf =
  let nf = numberFieldAlgorithms {F} {E} Fnf Enf in
  record
    { minimalPolynomialAlg = NumberFieldAlgorithms.minimalPolynomialAlg nf
    ; galoisGroupAlg       = NumberFieldAlgorithms.galoisGroupAlg nf
    ; splittingFieldAlg    = NumberFieldAlgorithms.splittingFieldAlg nf
    ; extensionDegreeAlg   = NumberFieldAlgorithms.extensionDegreeAlg nf
    ; subfieldEnumAlg      = NumberFieldAlgorithms.subfieldEnumAlg nf
    -- CORRECTED: Use external oracle for Subgroups
    ; subgroupEnumAlg      = externalSubgroupEnumerationAlgorithm F E numberFieldExternalConfig
    ; algebraicityAlg      = NumberFieldAlgorithms.algebraicityAlg nf
    ; primitiveElementAlg  = NumberFieldAlgorithms.primitiveElementAlg nf
    ; normalityAlg         = mkNormalityDecisionAlgorithm {F} {E}
    ; separabilityAlg      = mkSeparabilityDecisionAlgorithm {F} {E}
    ; normalClosureAlg     = mkNormalClosureAlgorithm {F} {E}
    ; galoisClosureAlg     = mkGaloisClosureAlgorithm {F} {E}
    }
