{-# OPTIONS --allow-unsolved-metas #-}

module Tests.PolynomialFieldExtensionsChecklist where

open import Agda.Builtin.Equality using (_≡_; refl)
open import Agda.Builtin.Unit using (⊤)
import Agda.Builtin.Nat as N
import Agda.Builtin.String as S
import Metamodel as M
import Algebra.Fields.Basic as AFB
import Algebra.Fields.Advanced as AFA
import Tests.ObligationAdapters as A
import Core.CategoricalAdapter

-- ============================================================================
-- Setup: Base field and extensions for polynomial/function field testing
-- ============================================================================

-- Base field ℚ
baseFieldId : M.Identifier
baseFieldId = M.mkId "ℚ"

baseFieldDecl : AFB.FieldDeclaration
baseFieldDecl = record
  { fieldId = baseFieldId
  ; characteristic = M.mkId "0"
  ; additiveGroup = M.mkId "ℚ⁺"
  ; multiplicativeGroup = M.mkId "ℚ*"
  }

-- Simple algebraic extension ℚ(√2)
sqrt2Id : M.Identifier
sqrt2Id = M.mkId "√2"

simpleExtFieldId : M.Identifier
simpleExtFieldId = M.mkId "ℚ(√2)"

simpleExtFieldDecl : AFB.FieldDeclaration
simpleExtFieldDecl = record
  { fieldId = simpleExtFieldId
  ; characteristic = M.mkId "0"
  ; additiveGroup = M.mkId "ℚ(√2)⁺"
  ; multiplicativeGroup = M.mkId "ℚ(√2)*"
  }

-- Transcendental extension ℚ(X) - rational function field
transcFieldId : M.Identifier
transcFieldId = M.mkId "ℚ(X)"

transcFieldDecl : AFB.FieldDeclaration
transcFieldDecl = record
  { fieldId = transcFieldId
  ; characteristic = M.mkId "0"
  ; additiveGroup = M.mkId "ℚ(X)⁺"
  ; multiplicativeGroup = M.mkId "ℚ(X)*"
  }

transcElementId : M.Identifier
transcElementId = M.mkId "X"

-- Mixed extension ℚ(X, √2)
mixedExtFieldId : M.Identifier
mixedExtFieldId = M.mkId "ℚ(X,√2)"

mixedExtFieldDecl : AFB.FieldDeclaration
mixedExtFieldDecl = record
  { fieldId = mixedExtFieldId
  ; characteristic = M.mkId "0"
  ; additiveGroup = M.mkId "ℚ(X,√2)⁺"
  ; multiplicativeGroup = M.mkId "ℚ(X,√2)*"
  }

-- Inseparable extension (characteristic p example conceptually)
-- In reality we'd use 𝔽ₚ(t^p)/𝔽ₚ(t), but we model it abstractly
insepFieldId : M.Identifier
insepFieldId = M.mkId "F_insep"

insepFieldDecl : AFB.FieldDeclaration
insepFieldDecl = record
  { fieldId = insepFieldId
  ; characteristic = M.mkId "p"
  ; additiveGroup = M.mkId "F_insep⁺"
  ; multiplicativeGroup = M.mkId "F_insep*"
  }

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
  ; generator = sqrt2Id
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
  ; basis = M.mkId "{X}"  -- {X} is a transcendence basis for ℚ(X,√2)/ℚ
  ; transcendenceDegree = M.mkId "1"
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

-- Categorical assertions
_ : Core.CategoricalAdapter.morphism (A.extensionDegreeCategorical extensionDegreeAdapt) ⊤ ⊤ ≡ Core.CategoricalAdapter.object (A.extensionDegreeCategorical extensionDegreeAdapt) ⊤
_ = refl

_ : Core.CategoricalAdapter.isomorphism (A.extensionDegreeCategorical extensionDegreeAdapt) = refl
_ = refl

_ : Core.CategoricalAdapter.morphism (A.inseparableDegreeCategorical inseparableDegreeAdapt) ⊤ ⊤ ≡ Core.CategoricalAdapter.object (A.inseparableDegreeCategorical inseparableDegreeAdapt) ⊤
_ = refl

_ : Core.CategoricalAdapter.isomorphism (A.inseparableDegreeCategorical inseparableDegreeAdapt) = refl
_ = refl

_ : Core.CategoricalAdapter.morphism (A.separableDegreeCategorical separableDegreeAdapt) ⊤ ⊤ ≡ Core.CategoricalAdapter.object (A.separableDegreeCategorical separableDegreeAdapt) ⊤
_ = refl

_ : Core.CategoricalAdapter.isomorphism (A.separableDegreeCategorical separableDegreeAdapt) = refl
_ = refl

_ : Core.CategoricalAdapter.morphism (A.simpleExtensionCategorical simpleExtensionAdapt) ⊤ ⊤ ≡ Core.CategoricalAdapter.object (A.simpleExtensionCategorical simpleExtensionAdapt) ⊤
_ = refl

_ : Core.CategoricalAdapter.isomorphism (A.simpleExtensionCategorical simpleExtensionAdapt) = refl
_ = refl

_ : Core.CategoricalAdapter.morphism (A.transcendentalElementCategorical transcendentalElementAdapt) ⊤ ⊤ ≡ Core.CategoricalAdapter.object (A.transcendentalElementCategorical transcendentalElementAdapt) ⊤
_ = refl

_ : Core.CategoricalAdapter.isomorphism (A.transcendentalElementCategorical transcendentalElementAdapt) = refl
_ = refl

_ : Core.CategoricalAdapter.morphism (A.transcendenceBasisCategorical transcendenceBasisAdapt) ⊤ ⊤ ≡ Core.CategoricalAdapter.object (A.transcendenceBasisCategorical transcendenceBasisAdapt) ⊤
_ = refl

_ : Core.CategoricalAdapter.isomorphism (A.transcendenceBasisCategorical transcendenceBasisAdapt) = refl
_ = refl

