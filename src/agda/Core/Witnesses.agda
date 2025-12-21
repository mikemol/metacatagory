{-# OPTIONS --without-K #-}

-- Core.Witnesses: Generic witness construction utilities
-- This module provides reusable builders for common algebraic witness types,
-- reducing boilerplate and ensuring consistency across specializations.

module Core.Witnesses where

open import Core
open import Algebra.Foundation
open import Algebra.Rings.Basic
open import Algebra.Fields.Basic
open import Algebra.Fields.Advanced
open import Metamodel as M
open import Agda.Builtin.List using (List; []; _∷_)

-- ============================================================================
-- Simple Extension Witnesses
-- ============================================================================

-- Build a SimpleExtension witness from F, E, and adjoined element α
mkSimpleExtension : (F E : FieldDeclaration) → (α : M.Identifier) → SimpleExtension F E α
mkSimpleExtension F E α = record
  { baseField         = F
  ; extensionField    = E
  ; adjoinedElement   = α
  ; minimalPolynomial = M.mkId "minpoly"
  ; isSimpleExtension = M.mkId "simple-ext-witness"
  }

-- Build a SimpleExtension with a specific minimal polynomial
mkSimpleExtensionWithPoly : (F E : FieldDeclaration) → (α : M.Identifier) → (poly : M.Identifier) → SimpleExtension F E α
mkSimpleExtensionWithPoly F E α poly = record
  { baseField         = F
  ; extensionField    = E
  ; adjoinedElement   = α
  ; minimalPolynomial = poly
  ; isSimpleExtension = M.mkId "simple-ext-witness"
  }

-- ============================================================================
-- Algebraic Element Witnesses
-- ============================================================================

-- Build an AlgebraicElement witness
mkAlgebraicElement : (F E : FieldDeclaration) → (α : M.Identifier) → AlgebraicElement F E α
mkAlgebraicElement F E α = record
  { baseField        = F
  ; extensionField   = E
  ; element          = α
  ; minimalPolynomial = M.mkId "minpoly"
  ; isAlgebraic      = M.mkId "algebraic-witness"
  }

-- Build an AlgebraicElement with specific minimal polynomial
mkAlgebraicElementWithPoly : (F E : FieldDeclaration) → (α : M.Identifier) → (poly : M.Identifier) → AlgebraicElement F E α
mkAlgebraicElementWithPoly F E α poly = record
  { baseField        = F
  ; extensionField   = E
  ; element          = α
  ; minimalPolynomial = poly
  ; isAlgebraic      = M.mkId "algebraic-witness"
  }

-- ============================================================================
-- Field Extension Witnesses
-- ============================================================================

-- Build a FieldExtension witness
mkFieldExtension : (F E : FieldDeclaration) → FieldExtension F E
mkFieldExtension F E = record
  { baseField      = F
  ; extensionField = E
  ; inclusion      = M.mkId "inclusion"
  ; vectorSpaceStructure = M.mkId "vec-space"
  }

-- Build an ExtensionDegree witness
mkExtensionDegree : (F E : FieldDeclaration) → ExtensionDegree F E
mkExtensionDegree F E = record
  { baseField      = F
  ; extensionField = E
  ; degree         = M.mkId "degree"
  }

-- Build an ExtensionDegree with specific degree
mkExtensionDegreeWith : (F E : FieldDeclaration) → (deg : M.Identifier) → ExtensionDegree F E
mkExtensionDegreeWith F E deg = record
  { baseField      = F
  ; extensionField = E
  ; degree         = deg
  }

-- ============================================================================
-- Galois Theory Witnesses
-- ============================================================================

-- Build a GaloisExtension witness
mkGaloisExtension : (F E : FieldDeclaration) → (G : GaloisGroup F E) → GaloisExtension F E
mkGaloisExtension F E G = record
  { baseField    = F
  ; extensionField = E
  ; galoisGroup  = G
  ; isGalois     = M.mkId "galois-witness"
  }

-- Build a NormalExtension witness
mkNormalExtension : (F E : FieldDeclaration) → NormalExtension F E
mkNormalExtension F E = record
  { baseField      = F
  ; extensionField = E
  ; isNormal       = M.mkId "normal-witness"
  }

-- Build a SeparableExtension witness
mkSeparableExtension : (F E : FieldDeclaration) → SeparableExtension F E
mkSeparableExtension F E = record
  { baseField      = F
  ; extensionField = E
  ; isSeparable    = M.mkId "separable-witness"
  }

-- ============================================================================
-- Splitting Field and Roots
-- ============================================================================

-- Build a SplittingField witness
mkSplittingField : (F : FieldDeclaration) → (f : M.Identifier) → (E : FieldDeclaration) → SplittingField F f
mkSplittingField F f E = record
  { baseField      = F
  ; polynomial     = f
  ; splittingField = E
  ; definition     = M.mkId "splits"
  }

-- ============================================================================
-- Subfield Witnesses
-- ============================================================================

-- Build a Subfield witness
mkSubfield : (E : FieldDeclaration) → (K : FieldDeclaration) → Subfield E
mkSubfield E K = record
  { subfield   = K
  ; subset     = M.mkId "subset"
  ; inclusion  = M.mkId "incl"
  ; isSubfield = M.mkId "subfield-witness"
  }

-- Build a list containing just the base field as a trivial subfield
trivialSubfield : (F E : FieldDeclaration) → List (Subfield E)
trivialSubfield F E = mkSubfield E F ∷ []

-- ============================================================================
-- Chapter VI Advanced Witnesses
-- ============================================================================

-- Build an InseparableExtension witness
mkInseparableExtension : (F E : FieldDeclaration) → InseparableExtension F E
mkInseparableExtension F E = record
  { baseField      = F
  ; extensionField = E
  ; isInseparable  = M.mkId "inseparable-witness"
  }

-- Build a PurelyInseparableExtension witness
mkPurelyInseparableExtension : (F E : FieldDeclaration) → PurelyInseparableExtension F E
mkPurelyInseparableExtension F E = record
  { baseField           = F
  ; extensionField      = E
  ; isPurelyInseparable = M.mkId "purely-insep-witness"
  }

-- Build a PerfectField witness
mkPerfectField : (F : FieldDeclaration) → PerfectField F
mkPerfectField F = record
  { baseField = F
  ; isPerfect = M.mkId "perfect-witness"
  }

-- Build an AlgebraicallyClosedField witness
mkAlgebraicallyClosedField : (F : FieldDeclaration) → AlgebraicallyClosedField F
mkAlgebraicallyClosedField F = record
  { baseField             = F
  ; isAlgebraicallyClosed = M.mkId "alg-closed-witness"
  }

-- Build a NormalClosure witness
mkNormalClosure : (F E N : FieldDeclaration) → NormalClosure F E
mkNormalClosure F E N = record
  { baseField       = F
  ; extensionField  = E
  ; normalClosure   = N
  ; isNormalClosure = M.mkId "normal-closure-witness"
  }

-- Build a GaloisClosure witness
mkGaloisClosure : (F E G : FieldDeclaration) → GaloisClosure F E
mkGaloisClosure F E G = record
  { baseField       = F
  ; extensionField  = E
  ; galoisClosure   = G
  ; isGaloisClosure = M.mkId "galois-closure-witness"
  }
