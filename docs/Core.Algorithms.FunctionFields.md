``` Agda
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
functionFieldAlgorithms {F} {E} Fff Eff = record
  { minimalPolynomialAlg = record
      { minimalPolynomial = λ α → M.mkId "minpoly-F(t)"
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
                          ; binaryOp      = M.mkId "∘"
                          }
                      ; associativity = record { over = M.mkId "Gal(E/F)-assoc" }
                      }
                  ; identityElement = M.mkId "id"
                  ; identityAxiom   = record { over = M.mkId "Gal(E/F)-id" }
                  }
              ; inverseOperation = record
                  { forMonoid = record
                      { underlyingSemigroup = record
                          { underlyingMagma = record
                              { underlyingSet = M.mkId "Gal(E/F)"
                              ; binaryOp      = M.mkId "∘"
                              }
                          ; associativity = record { over = M.mkId "Gal(E/F)-assoc" }
                          }
                      ; identityElement = M.mkId "id"
                      ; identityAxiom   = record { over = M.mkId "Gal(E/F)-id" }
                      }
                  ; inverseMap  = M.mkId "inv"
                  ; inverseAxiom = M.mkId "inverse-ax"
                  }
              }
          ; automorphisms  = M.mkId "symbolic-auts-F(t)"
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
      { primitiveElement       = M.mkId "primitive-F(t)"
      ; witnessSimpleExtension = mkSimpleExtension F E (M.mkId "primitive-F(t)")
      }
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
    ; subgroupEnumAlg      = SubgroupEnumerationAlgorithm-generic {F} {E}
    ; algebraicityAlg      = FunctionFieldAlgorithms.algebraicityAlg ff
    ; primitiveElementAlg  = FunctionFieldAlgorithms.primitiveElementAlg ff
    ; normalityAlg         = NormalityDecisionAlgorithm-generic {F} {E}
    ; separabilityAlg      = SeparabilityDecisionAlgorithm-generic {F} {E}
    ; normalClosureAlg     = NormalClosureAlgorithm-generic {F} {E}
    ; galoisClosureAlg     = GaloisClosureAlgorithm-generic {F} {E}
    }
```
