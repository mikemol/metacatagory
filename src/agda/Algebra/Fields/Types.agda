{-# OPTIONS --without-K #-}

module Algebra.Fields.Types where

open import Core
open import Algebra.Foundation
open import Algebra.Rings.Types
open import Metamodel as M

-- ============================================================================
-- Field-Specific Type Definitions (Hungerford Ch V)
-- ============================================================================

-- Field extension
record FieldExtension (F : FieldDeclaration) : Set₁ where
  field
    baseField : FieldDeclaration
    extensionField : FieldDeclaration
    embedding : M.Identifier

-- Algebraic element
record AlgebraicElement (F E : FieldDeclaration) (α : M.Identifier) : Set₁ where
  field
    baseField : FieldDeclaration
    extensionField : FieldDeclaration
    element : M.Identifier
    isAlgebraic : M.Identifier

-- Degree of extension
record DegreeOfExtension (F E : FieldDeclaration) : Set₁ where
  field
    baseField : FieldDeclaration
    extensionField : FieldDeclaration
    degree : M.Identifier

-- Normal extension
record NormalExtension (F E : FieldDeclaration) : Set₁ where
  field
    baseField : FieldDeclaration
    extensionField : FieldDeclaration
    isNormal : M.Identifier

-- Separable extension
record SeparableExtension (F E : FieldDeclaration) : Set₁ where
  field
    baseField : FieldDeclaration
    extensionField : FieldDeclaration
    isSeparable : M.Identifier

-- Galois extension
record GaloisExtension (F E : FieldDeclaration) : Set₁ where
  field
    baseField : FieldDeclaration
    extensionField : FieldDeclaration
    isGalois : M.Identifier

-- Galois group
record GaloisGroup (F E : FieldDeclaration) : Set₁ where
  field
    baseField : FieldDeclaration
    extensionField : FieldDeclaration
    group : M.Identifier

-- Fixed field
record FixedField (F E : FieldDeclaration) (H : M.Identifier) : Set₁ where
  field
    baseField : FieldDeclaration
    extensionField : FieldDeclaration
    subgroup : M.Identifier
    fixedField : FieldDeclaration

-- Splitting field
record SplittingField (F : FieldDeclaration) (f : M.Identifier) : Set₁ where
  field
    baseField : FieldDeclaration
    polynomial : M.Identifier
    splittingField : FieldDeclaration
    splitsCompletely : M.Identifier

-- ============================================================================
-- Algebraic Closure and Related
-- ============================================================================

-- Algebraic closure
record AlgebraicClosure (F : FieldDeclaration) : Set₁ where
  field
    baseField : FieldDeclaration
    closure : FieldDeclaration
    isAlgebraicClosure : M.Identifier

-- Vector space over field
record VectorSpace (F : FieldDeclaration) : Set₁ where
  field
    baseField : FieldDeclaration
    underlyingModule : M.Identifier

-- Basis of vector space
record BasisOfVectorSpace (F : FieldDeclaration) (V : VectorSpace F) : Set₁ where
  field
    field' : FieldDeclaration
    vectorSpace : VectorSpace field'
    basis : M.Identifier
