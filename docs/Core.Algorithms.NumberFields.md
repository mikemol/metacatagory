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
numberFieldAlgorithms {F} {E} Fnf Enf = record
  { minimalPolynomialAlg = record
      { minimalPolynomial = λ α → M.mkId "minpoly-ℚ"
      ; isAlgebraic       = λ α → yes (mkAlgebraicElement F E α)
      }
  ; galoisGroupAlg = record
      { galoisGroup   = λ f → record
          { baseField      = F
          ; extensionField = E
          ; group          = record
              { underlyingMonoid = record
                  { underlyingSemigroup = record
                      { underlyingMagma = record
                          { underlyingSet = M.mkId "Gal(E/F)"
                          ; binaryOp = M.mkId "∘"
                          }
                      ; associativity = record { over = M.mkId "Gal(E/F)-assoc" }
                      }
                  ; identityElement = M.mkId "id"
                  ; identityAxiom = record { over = M.mkId "Gal(E/F)-id" }
                  }
              ; inverseOperation = record
                  { forMonoid = record
                      { underlyingSemigroup = record
                          { underlyingMagma = record
                              { underlyingSet = M.mkId "Gal(E/F)"
                              ; binaryOp = M.mkId "∘"
                              }
                          ; associativity = record { over = M.mkId "Gal(E/F)-assoc" }
                          }
                      ; identityElement = M.mkId "id"
                      ; identityAxiom = record { over = M.mkId "Gal(E/F)-id" }
                      }
                  ; inverseMap = M.mkId "inv"
                  ; inverseAxiom = M.mkId "inverse-ax"
                  }
              }
          ; automorphisms  = M.mkId "symbolic-auts"
          }
      ; automorphisms = λ f → []
      ; isSolvable    = λ f → M.mkId "unknown-solvable"
      }
  ; splittingFieldAlg = record
      { splittingField = λ f → mkSplittingField F f E
      ; roots          = λ f → []
      }
  ; extensionDegreeAlg = record
      { extensionDegree = mkExtensionDegree F E
      ; basis           = defaultBasis F E
      }
  ; subfieldEnumAlg = record
      { subfields = trivialSubfield F E
      }
  ; algebraicityAlg = record
      { isAlgebraic      = λ α → yes (mkAlgebraicElement F E α)
      ; isTranscendental = λ α → no
      }
  ; primitiveElementAlg = record
      { primitiveElement = M.mkId "primitive-ℚ"
      ; witnessSimpleExtension = mkSimpleExtension F E (M.mkId "primitive-ℚ")
      }
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
    ; subgroupEnumAlg      = SubgroupEnumerationAlgorithm-generic {F} {E}
    ; algebraicityAlg      = NumberFieldAlgorithms.algebraicityAlg nf
    ; primitiveElementAlg  = NumberFieldAlgorithms.primitiveElementAlg nf
    ; normalityAlg         = NormalityDecisionAlgorithm-generic {F} {E}
    ; separabilityAlg      = SeparabilityDecisionAlgorithm-generic {F} {E}
    ; normalClosureAlg     = NormalClosureAlgorithm-generic {F} {E}
    ; galoisClosureAlg     = GaloisClosureAlgorithm-generic {F} {E}
    }
```
