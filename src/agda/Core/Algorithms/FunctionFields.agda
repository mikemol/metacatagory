{-# OPTIONS --without-K #-}

-- Core.Algorithms.FunctionFields: Specialization of algorithms for function fields K(t)/F

module Core.Algorithms.FunctionFields where

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
open import Algorithms.Basic
open Algorithms.Basic.Defaults

-- Evidence that a field is a (rational) function field over some base
-- Kept abstract; concrete instances can provide structure as needed.
postulate
  IsFunctionField : FieldDeclaration → Set

-- Minimal bundle for function fields E/F
record FunctionFieldAlgorithms (F E : FieldDeclaration)
                               (Fff : IsFunctionField F)
                               (Eff : IsFunctionField E) : Set₁ where
  field
    minimalPolynomialAlg : MinimalPolynomialAlgorithm F E
    galoisGroupAlg       : GaloisGroupAlgorithm F E
    splittingFieldAlg    : SplittingFieldAlgorithm F
    extensionDegreeAlg   : FieldExtensionDegreeAlgorithm F E
    subfieldEnumAlg      : SubfieldEnumerationAlgorithm F E
    algebraicityAlg      : AlgebraicityDecisionAlgorithm F E
    primitiveElementAlg  : PrimitiveElementAlgorithm F E

open FunctionFieldAlgorithms public

functionFieldAlgorithms : ∀ {F E} → (Fff : IsFunctionField F) → (Eff : IsFunctionField E)
                        → FunctionFieldAlgorithms F E Fff Eff
-- DeviationLog [2025-11-18]: Stubbing function field algorithms to generic defaults
-- (similar rationale as finite & number fields) to unblock Phase II 2.5.
functionFieldAlgorithms {F} {E} Fff Eff = record
  { minimalPolynomialAlg = mkMinimalPolynomialAlgorithm {F} {E}
  ; galoisGroupAlg       = mkGaloisGroupAlgorithm {F} {E}
  ; splittingFieldAlg    = mkSplittingFieldAlgorithm {F}
  ; extensionDegreeAlg   = mkFieldExtensionDegreeAlgorithm {F} {E}
  ; subfieldEnumAlg      = mkSubfieldEnumerationAlgorithm {F} {E}
  ; algebraicityAlg      = mkAlgebraicityDecisionAlgorithm {F} {E}
  ; primitiveElementAlg  = mkPrimitiveElementAlgorithm {F} {E}
  }

-- Convenience: construct full registry bundle for function fields
functionFieldBundle : (F E : FieldDeclaration)
                    → IsFunctionField F → IsFunctionField E
                    → AlgorithmBundle F E
functionFieldBundle F E Fff Eff =
  let ff = functionFieldAlgorithms {F} {E} Fff Eff in
  record
    { minimalPolynomialAlg = FunctionFieldAlgorithms.minimalPolynomialAlg ff
    ; galoisGroupAlg       = FunctionFieldAlgorithms.galoisGroupAlg ff
    ; splittingFieldAlg    = FunctionFieldAlgorithms.splittingFieldAlg ff
    ; extensionDegreeAlg   = FunctionFieldAlgorithms.extensionDegreeAlg ff
    ; subfieldEnumAlg      = FunctionFieldAlgorithms.subfieldEnumAlg ff
    ; subgroupEnumAlg      = mkSubgroupEnumerationAlgorithm {F} {E}
    ; algebraicityAlg      = FunctionFieldAlgorithms.algebraicityAlg ff
    ; primitiveElementAlg  = FunctionFieldAlgorithms.primitiveElementAlg ff
    ; normalityAlg         = mkNormalityDecisionAlgorithm {F} {E}
    ; separabilityAlg      = mkSeparabilityDecisionAlgorithm {F} {E}
    ; normalClosureAlg     = mkNormalClosureAlgorithm {F} {E}
    ; galoisClosureAlg     = mkGaloisClosureAlgorithm {F} {E}
    }
