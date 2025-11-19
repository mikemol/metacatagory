{-# OPTIONS --allow-unsolved-metas #-}

module Tests.PolynomialFieldExtensionsChecklist where

open import Agda.Builtin.Equality using (_≡_; refl)
open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.Unit using (⊤)
import Agda.Builtin.Nat as N
import Agda.Builtin.String as S
import Metamodel as M
import Chapter1.Level1 as C1L
import Algebra.Foundation as AF
import Algebra.Rings.Basic as AR
import Algebra.Fields.Basic as AFB
import Algebra.Fields.Advanced as AFA
import Tests.ObligationAdapters as A
import Core.CategoricalAdapter

-- ============================================================================
-- Setup: Base field and extensions for polynomial/function field testing
-- ============================================================================

-- Base field ℚ (use modern API from Rings.Basic for FieldDeclaration)
baseFieldId : M.Identifier
baseFieldId = M.mkId "ℚ"

baseFieldDecl : AR.FieldDeclaration
baseFieldDecl =
  let
    -- For brevity, assume underlyingRing is a commutative ring with inverses already built
    dummyUnitalRing : AR.UnitalRingDeclaration
    dummyUnitalRing = record
      { underlyingRing = record
          { identifier = baseFieldId
          ; additiveGroup = record
              { underlyingGroup = record
                  { underlyingMonoid = record
                      { underlyingSemigroup = record
                          { underlyingMagma = record { underlyingSet = baseFieldId ; binaryOp = M.mkId "+" }
                          ; associativity = C1L.AXIOM_Associativity (M.mkId "+-assoc")
                          }
                      ; identityElement = M.mkId "0"
                      ; identityAxiom = C1L.AXIOM_Identity (M.mkId "+-id")
                      }
                  ; inverseOperation = record { forMonoid = record { underlyingSemigroup = record { underlyingMagma = record { underlyingSet = baseFieldId ; binaryOp = M.mkId "+" } ; associativity = C1L.AXIOM_Associativity (M.mkId "+-assoc") } ; identityElement = M.mkId "0" ; identityAxiom = C1L.AXIOM_Identity (M.mkId "+-id") } ; inverseMap = M.mkId "neg" ; inverseAxiom = M.mkId "+-inv" }
                  }
              ; commutativity = record { forGroup = record { underlyingMonoid = record { underlyingSemigroup = record { underlyingMagma = record { underlyingSet = baseFieldId ; binaryOp = M.mkId "+" } ; associativity = C1L.AXIOM_Associativity (M.mkId "+-assoc") } ; identityElement = M.mkId "0" ; identityAxiom = C1L.AXIOM_Identity (M.mkId "+-id") } ; inverseOperation = record { forMonoid = record { underlyingSemigroup = record { underlyingMagma = record { underlyingSet = baseFieldId ; binaryOp = M.mkId "+" } ; associativity = C1L.AXIOM_Associativity (M.mkId "+-assoc") } ; identityElement = M.mkId "0" ; identityAxiom = C1L.AXIOM_Identity (M.mkId "+-id") } ; inverseMap = M.mkId "neg" ; inverseAxiom = M.mkId "+-inv" } } ; axiom = M.mkId "+-comm" }
              }
          ; multiplication = M.mkId "*"
          ; multAssociative = M.mkId "*-assoc"
          ; leftDistributive = M.mkId "left-dist"
          ; rightDistributive = M.mkId "right-dist"
          }
      ; multiplicativeIdentity = M.mkId "1"
      ; leftIdentity = M.mkId "*-left-id"
      ; rightIdentity = M.mkId "*-right-id"
      }

    commRing : AR.CommutativeRingDeclaration
    commRing = record { underlyingRing = dummyUnitalRing ; commutativity = M.mkId "*-comm" }
  in
  record
    { underlyingRing = commRing
    ; inverses = M.mkId "field-inv"
    }

-- For simplicity in test, reuse baseFieldDecl structure for extension fields
-- (In a full implementation, each would have distinct underlying structures)

-- Simple algebraic extension ℚ(√2)
sqrt2Id : M.Identifier
sqrt2Id = M.mkId "√2"

simpleExtFieldId : M.Identifier
simpleExtFieldId = M.mkId "ℚ(√2)"

simpleExtFieldDecl : AR.FieldDeclaration
simpleExtFieldDecl = baseFieldDecl  -- stub: reuse base field structure

-- Transcendental extension ℚ(X) - rational function field
transcFieldId : M.Identifier
transcFieldId = M.mkId "ℚ(X)"

transcFieldDecl : AR.FieldDeclaration
transcFieldDecl = baseFieldDecl  -- stub

transcElementId : M.Identifier
transcElementId = M.mkId "X"

-- Mixed extension ℚ(X, √2)
mixedExtFieldId : M.Identifier
mixedExtFieldId = M.mkId "ℚ(X,√2)"

mixedExtFieldDecl : AR.FieldDeclaration
mixedExtFieldDecl = baseFieldDecl  -- stub

-- Inseparable extension (characteristic p example conceptually)
insepFieldId : M.Identifier
insepFieldId = M.mkId "F_insep"

insepFieldDecl : AR.FieldDeclaration
insepFieldDecl = baseFieldDecl  -- stub

-- ============================================================================
-- 1. Extension Degree [E : F]
-- ============================================================================

extensionDegree : AFB.ExtensionDegree baseFieldDecl simpleExtFieldDecl
extensionDegree = record
  { baseField = baseFieldDecl
  ; extensionField = simpleExtFieldDecl
  ; degree = M.mkId "2"  -- [ℚ(√2) : ℚ] = 2
  }

extensionDegreeAdapt : A.ExtensionDegreeAdapter
extensionDegreeAdapt =
  A.mkExtensionDegreeAdapter
    baseFieldDecl
    simpleExtFieldDecl
    extensionDegree
    baseFieldDecl
    simpleExtFieldDecl
    refl
    refl

_ : A.isFilledExtensionDegree extensionDegreeAdapt ≡ true
_ = refl

-- ============================================================================
-- 2. Inseparable Degree [E : F]ᵢ
-- ============================================================================

inseparableDegree : AFA.InseparableDegree baseFieldDecl insepFieldDecl
inseparableDegree = record
  { baseField = baseFieldDecl
  ; extensionField = insepFieldDecl
  ; inseparableDegree = M.mkId "p"  -- Inseparable part in char p
  }

inseparableDegreeAdapt : A.InseparableDegreeAdapter
inseparableDegreeAdapt =
  A.mkInseparableDegreeAdapter
    baseFieldDecl
    insepFieldDecl
    inseparableDegree
    baseFieldDecl
    insepFieldDecl
    refl
    refl

_ : A.isFilledInseparableDegree inseparableDegreeAdapt ≡ true
_ = refl

-- ============================================================================
-- 3. Separable Degree [E : F]ₛ
-- ============================================================================

separableDegree : AFA.SeparableDegree baseFieldDecl simpleExtFieldDecl
separableDegree = record
  { baseField = baseFieldDecl
  ; extensionField = simpleExtFieldDecl
  ; separableDegree = M.mkId "2"  -- [ℚ(√2) : ℚ]ₛ = 2 (all separable in char 0)
  }

separableDegreeAdapt : A.SeparableDegreeAdapter
separableDegreeAdapt =
  A.mkSeparableDegreeAdapter
    baseFieldDecl
    simpleExtFieldDecl
    separableDegree
    baseFieldDecl
    simpleExtFieldDecl
    refl
    refl

_ : A.isFilledSeparableDegree separableDegreeAdapt ≡ true
_ = refl

-- ============================================================================
-- 4. Simple Extension F(α)
-- ============================================================================

simpleExtension : AFB.SimpleExtension baseFieldDecl simpleExtFieldDecl sqrt2Id
simpleExtension = record
  { baseField = baseFieldDecl
  ; extensionField = simpleExtFieldDecl
  ; adjoinedElement = sqrt2Id
  ; isSimpleExtension = M.mkId "sqrt2-simple-ext"
  ; minimalPolynomial = M.mkId "x²-2"  -- Minimal polynomial of √2
  }

simpleExtensionAdapt : A.SimpleExtensionAdapter
simpleExtensionAdapt =
  A.mkSimpleExtensionAdapter
    baseFieldDecl
    simpleExtFieldDecl
    sqrt2Id
    simpleExtension
    baseFieldDecl
    simpleExtFieldDecl
    refl
    refl

_ : A.isFilledSimpleExtension simpleExtensionAdapt ≡ true
_ = refl

-- ============================================================================
-- 5. Transcendental Element
-- ============================================================================

transcendentalElement : AFB.TranscendentalElement baseFieldDecl transcFieldDecl transcElementId
transcendentalElement = record
  { baseField = baseFieldDecl
  ; extensionField = transcFieldDecl
  ; element = transcElementId
  ; isTranscendental = M.mkId "X-transcendental"  -- X has no polynomial relation over ℚ
  }

transcendentalElementAdapt : A.TranscendentalElementAdapter
transcendentalElementAdapt =
  A.mkTranscendentalElementAdapter
    baseFieldDecl
    transcFieldDecl
    transcElementId
    transcendentalElement
    baseFieldDecl
    transcFieldDecl
    refl
    refl

_ : A.isFilledTranscendentalElement transcendentalElementAdapt ≡ true
_ = refl

-- ============================================================================
-- 6. Transcendence Basis
-- ============================================================================

transcendenceBasis : AFB.TranscendenceBasis baseFieldDecl mixedExtFieldDecl
transcendenceBasis = record
  { baseField = baseFieldDecl
  ; extensionField = mixedExtFieldDecl
  ; basis = M.mkId "{X}"
  ; isTranscendenceBasis = M.mkId "X-basis"
  }

transcendenceBasisAdapt : A.TranscendenceBasisAdapter
transcendenceBasisAdapt =
  A.mkTranscendenceBasisAdapter
    baseFieldDecl
    mixedExtFieldDecl
    transcendenceBasis
    baseFieldDecl
    mixedExtFieldDecl
    refl
    refl

_ : A.isFilledTranscendenceBasis transcendenceBasisAdapt ≡ true
_ = refl

-- Categorical assertions (omitted; adapter wiring smoke-tested via isFilledX)

