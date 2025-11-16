-- Core.Algorithms.FiniteFields: Instances of algebraic algorithms for finite fields

module Core.Algorithms.FiniteFields where

open import Core
open import Algebra.Rings.Basic
open import Algebra.Fields.Basic
open import Algebra.Groups.Basic
open import Algebra.Foundation
open import Core.AlgebraicAlgorithms
open import Metamodel as M
open import Agda.Builtin.List using (List; []; _∷_)

-- Finite field predicate (kept abstract);
-- all concrete algorithm content below avoids extra postulates.
postulate
  IsFiniteField : FieldDeclaration → Set

-- Bundle of algorithms specialized to finite fields E/F
record FiniteFieldAlgorithms (F E : FieldDeclaration)
                           (Ffin : IsFiniteField F)
                           (Efin : IsFiniteField E) : Set₁ where
  field
    minimalPolynomialAlg : MinimalPolynomialAlgorithm F E
    galoisGroupAlg       : GaloisGroupAlgorithm F E
    splittingFieldAlg    : SplittingFieldAlgorithm F
    extensionDegreeAlg   : FieldExtensionDegreeAlgorithm F E
    subfieldEnumAlg      : SubfieldEnumerationAlgorithm F E
    subgroupEnumAlg      : SubgroupEnumerationAlgorithm F E
    algebraicityAlg      : AlgebraicityDecisionAlgorithm F E
    primitiveElementAlg  : PrimitiveElementAlgorithm F E

open FiniteFieldAlgorithms public

finiteFieldAlgorithms : ∀ {F E} → (Ffin : IsFiniteField F) → (Efin : IsFiniteField E)
                       → FiniteFieldAlgorithms F E Ffin Efin
finiteFieldAlgorithms {F} {E} Ffin Efin = record
  { minimalPolynomialAlg = record
    { minimalPolynomial = λ α → defaultMinimalPolynomial F E α
    ; isAlgebraic       = λ α → yes (record
      { baseField        = F
      ; extensionField   = E
      ; element          = α
      ; minimalPolynomial = M.mkId "minpoly"
      ; isAlgebraic      = M.mkId "algebraic"
      })
      }
  ; galoisGroupAlg = record
    { galoisGroup   = λ f → defaultGaloisGroup F E f
    ; automorphisms = λ f → defaultAutomorphisms F E f
    ; isSolvable    = λ f → defaultIsSolvable F E f
      }
  ; splittingFieldAlg = record
    { splittingField = λ f → defaultSplittingField F f
    ; roots          = λ f → defaultRoots F f
      }
  ; extensionDegreeAlg = record
    { extensionDegree = defaultExtensionDegree F E
    ; basis           = defaultBasis F E
      }
  ; subfieldEnumAlg = record
    { subfields = defaultSubfields F E
      }
  ; subgroupEnumAlg = record
    { subgroups = defaultSubgroups F E
      }
  ; algebraicityAlg = record
    { isAlgebraic      = λ α → yes (record
      { baseField        = F
      ; extensionField   = E
      ; element          = α
      ; minimalPolynomial = M.mkId "minpoly"
      ; isAlgebraic      = M.mkId "algebraic"
      })
    ; isTranscendental = λ α → no
      }
  ; primitiveElementAlg = record
    { primitiveElement = M.mkId "primitive"
    ; witnessSimpleExtension = record
      { baseField         = F
      ; extensionField    = E
      ; adjoinedElement   = M.mkId "primitive"
      ; minimalPolynomial = M.mkId "minpoly"
      ; isSimpleExtension = M.mkId "witness"
      }
    }
  }
