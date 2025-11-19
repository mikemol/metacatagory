``` Agda
-- Core.Algorithms.NumberFields: Specialization of algebraic algorithms for number fields

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

-- Evidence that a field is a number field (finite extension of Q)
postulate
  IsNumberField : FieldDeclaration → Set

-- Minimal specialization bundle for number fields E/F
-- We provide concrete implementations for common tasks and
-- fall back to generic defaults where not worth specializing yet.
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

-- A simple, symbolic number field bundle (placeholders wired via witnesses)
numberFieldAlgorithms : ∀ {F E} → (Fnf : IsNumberField F) → (Enf : IsNumberField E)
                       → NumberFieldAlgorithms F E Fnf Enf
-- DeviationLog [2025-11-18]: Stubbing number field algorithms to generic defaults
-- to unblock full test index (Phase II 2.5). Restore concrete symbolic
-- constructions once related metas are resolved.
numberFieldAlgorithms {F} {E} Fnf Enf = record
  { minimalPolynomialAlg = MinimalPolynomialAlgorithm-generic {F} {E}
  ; galoisGroupAlg       = GaloisGroupAlgorithm-generic {F} {E}
  ; splittingFieldAlg    = SplittingFieldAlgorithm-generic {F}
  ; extensionDegreeAlg   = FieldExtensionDegreeAlgorithm-generic {F} {E}
  ; subfieldEnumAlg      = SubfieldEnumerationAlgorithm-generic {F} {E}
  ; algebraicityAlg      = AlgebraicityDecisionAlgorithm-generic {F} {E}
  ; primitiveElementAlg  = PrimitiveElementAlgorithm-generic {F} {E}
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
    ; subgroupEnumAlg      = SubgroupEnumerationAlgorithm-generic {F} {E}  -- Already generic
    ; algebraicityAlg      = NumberFieldAlgorithms.algebraicityAlg nf
    ; primitiveElementAlg  = NumberFieldAlgorithms.primitiveElementAlg nf
    ; normalityAlg         = NormalityDecisionAlgorithm-generic {F} {E}
    ; separabilityAlg      = SeparabilityDecisionAlgorithm-generic {F} {E}
    ; normalClosureAlg     = NormalClosureAlgorithm-generic {F} {E}
    ; galoisClosureAlg     = GaloisClosureAlgorithm-generic {F} {E}
    }
```
