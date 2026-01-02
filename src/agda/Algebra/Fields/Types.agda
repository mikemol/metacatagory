{-# OPTIONS --without-K #-}

-- | Core field-theory types (extensions, closures, automorphisms).
module Algebra.Fields.Types where

open import Core
open import Algebra.Foundation
open import Algebra.Rings.Types
open import Metamodel as M

-- ============================================================================
-- Field-Specific Type Definitions (Hungerford Ch V)
-- ============================================================================

-- Subfield of a given field E
record Subfield (E : FieldDeclaration) : Set₁ where
  field
    subfield   : FieldDeclaration
    subset     : M.Identifier
    inclusion  : M.Identifier
    isSubfield : M.Identifier

-- Field extension E/F
record FieldExtension (F E : FieldDeclaration) : Set₁ where
  field
    baseField           : FieldDeclaration
    extensionField      : FieldDeclaration
    inclusion           : M.Identifier
    vectorSpaceStructure : M.Identifier

-- Extension degree [E : F]
record ExtensionDegree (F E : FieldDeclaration) : Set₁ where
  field
    baseField      : FieldDeclaration
    extensionField : FieldDeclaration
    degree         : M.Identifier

-- Simple extension (E = F(α))
record SimpleExtension (F E : FieldDeclaration) (α : M.Identifier) : Set₁ where
  field
    baseField         : FieldDeclaration
    extensionField    : FieldDeclaration
    adjoinedElement   : M.Identifier
    minimalPolynomial : M.Identifier
    isSimpleExtension : M.Identifier

-- Algebraic element (with minimal polynomial)
record AlgebraicElement (F E : FieldDeclaration) (α : M.Identifier) : Set₁ where
  field
    baseField         : FieldDeclaration
    extensionField    : FieldDeclaration
    element           : M.Identifier
    minimalPolynomial : M.Identifier
    isAlgebraic       : M.Identifier

-- Degree of extension
record DegreeOfExtension (F E : FieldDeclaration) : Set₁ where
  field
    baseField : FieldDeclaration
    extensionField : FieldDeclaration
    degree : M.Identifier

-- Algebraic extension
record AlgebraicExtension (F E : FieldDeclaration) : Set₁ where
  field
    baseField : FieldDeclaration
    extensionField : FieldDeclaration
    isAlgebraicExtension : M.Identifier

-- Transcendental element
record TranscendentalElement (F E : FieldDeclaration) (α : M.Identifier) : Set₁ where
  field
    baseField : FieldDeclaration
    extensionField : FieldDeclaration
    element : M.Identifier
    isTranscendental : M.Identifier

-- Transcendence basis
record TranscendenceBasis (F E : FieldDeclaration) : Set₁ where
  field
    baseField : FieldDeclaration
    extensionField : FieldDeclaration
    basis : M.Identifier
    isTranscendenceBasis : M.Identifier

-- Field automorphism (σ : E → E with σ|_F = id)
record FieldAutomorphism (F E : FieldDeclaration) : Set₁ where
  field
    baseField : FieldDeclaration
    extensionField : FieldDeclaration
    automorphism : M.Identifier
    isAutomorphism : M.Identifier

-- Normal extension
record NormalExtension (F E : FieldDeclaration) : Set₁ where
  field
    baseField : FieldDeclaration
    extensionField : FieldDeclaration
    isNormal : M.Identifier

-- | Extension E/F that is separable (all elements have distinct conjugates).
record SeparableExtension (F E : FieldDeclaration) : Set₁ where
  field
    baseField : FieldDeclaration
    extensionField : FieldDeclaration
    isSeparable : M.Identifier

-- Galois group Gal(E/F)
record GaloisGroup (F E : FieldDeclaration) : Set₁ where
  field
    baseField      : FieldDeclaration
    extensionField : FieldDeclaration
    group          : GroupDeclaration
    automorphisms  : M.Identifier

-- Galois extension
record GaloisExtension (F E : FieldDeclaration) : Set₁ where
  field
    baseField      : FieldDeclaration
    extensionField : FieldDeclaration
    galoisGroup    : GaloisGroup F E
    isGalois       : M.Identifier

-- Fixed field of a subgroup H ≤ Gal(E/F)
record FixedField (F E : FieldDeclaration) (H : M.Identifier) : Set₁ where
  field
    baseField : FieldDeclaration
    extensionField : FieldDeclaration
    subgroup : M.Identifier
    fixedField : FieldDeclaration

-- Splitting field of polynomial f over F
record SplittingField (F : FieldDeclaration) (f : M.Identifier) : Set₁ where
  field
    baseField      : FieldDeclaration
    polynomial     : M.Identifier
    splittingField : FieldDeclaration
    definition     : M.Identifier

-- ============================================================================
-- Algebraic Closure and Related
-- ============================================================================

-- Algebraic closure
record AlgebraicClosure (F : FieldDeclaration) : Set₁ where
  field
    baseField : FieldDeclaration
    closureField : FieldDeclaration
    isAlgebraicClosure : M.Identifier
