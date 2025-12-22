{-# OPTIONS --without-K #-}

-- Algebra.Fields.BasicWithTheorems: Fields with explicit theorem sourcing
--
-- Refactored version making theorem dependencies explicit.

module Algebra.Fields.BasicWithTheorems where

open import Core
open import Chapter1.Level1Index
open import Algebra.Foundation
open import Algebra.Rings.Basic
open import Algebra.Modules.Basic
open import PropertyRegistry
open import Metamodel as M
open import Algebra.Fields.Theorems.Classical

-- ============================================================================
-- V.1: Field Extensions
-- ============================================================================

open import Algebra.Rings.Basic using (FieldDeclaration)

-- Subfield
record Subfield (F : FieldDeclaration) : Set₁ where
  field
    subfield : FieldDeclaration
    subset : M.Identifier
    inclusion : M.Identifier
    isSubfield : M.Identifier

-- Field extension E/F
record FieldExtension (F E : FieldDeclaration) : Set₁ where
  field
    baseField : FieldDeclaration
    extensionField : FieldDeclaration
    inclusion : M.Identifier
    vectorSpaceStructure : M.Identifier

-- Degree of extension [E : F]
record ExtensionDegree (F E : FieldDeclaration) : Set₁ where
  field
    baseField : FieldDeclaration
    extensionField : FieldDeclaration
    degree : M.Identifier

-- Simple extension F(α)
record SimpleExtension (F : FieldDeclaration) (E : FieldDeclaration) (α : M.Identifier) : Set₁ where
  field
    baseField : FieldDeclaration
    extensionField : FieldDeclaration
    adjoinedElement : M.Identifier
    minimalPolynomial : M.Identifier
    isSimpleExtension : M.Identifier

-- Algebraic element
record AlgebraicElement (F : FieldDeclaration) (E : FieldDeclaration) (α : M.Identifier) : Set₁ where
  field
    baseField : FieldDeclaration
    extensionField : FieldDeclaration
    element : M.Identifier
    minimalPolynomial : M.Identifier
    isAlgebraic : M.Identifier

-- Algebraic extension
record AlgebraicExtension (F E : FieldDeclaration) : Set₁ where
  field
    baseField : FieldDeclaration
    extensionField : FieldDeclaration
    isAlgebraicExtension : M.Identifier

-- Transcendental element
record TranscendentalElement (F : FieldDeclaration) (E : FieldDeclaration) (α : M.Identifier) : Set₁ where
  field
    baseField : FieldDeclaration
    extensionField : FieldDeclaration
    element : M.Identifier
    isTranscendental : M.Identifier

-- Transcendence basis
record TranscendenceBasis (F : FieldDeclaration) (E : FieldDeclaration) : Set₁ where
  field
    baseField : FieldDeclaration
    extensionField : FieldDeclaration
    basis : M.Identifier
    isTranscendenceBasis : M.Identifier

-- ============================================================================
-- V.2: Galois Theory
-- ============================================================================

-- Automorphism of field extension
record FieldAutomorphism (F E : FieldDeclaration) : Set₁ where
  field
    baseField : FieldDeclaration
    extensionField : FieldDeclaration
    automorphism : M.Identifier
    isAutomorphism : M.Identifier

-- Galois group Gal(E/F)
record GaloisGroup (F E : FieldDeclaration) : Set₁ where
  field
    baseField : FieldDeclaration
    extensionField : FieldDeclaration
    group : GroupDeclaration
    automorphisms : M.Identifier

-- Fixed field of automorphism group
record FixedField (F E : FieldDeclaration) (G : GaloisGroup F E) : Set₁ where
  field
    fixedField : FieldDeclaration
    definition : M.Identifier

-- Galois extension
record GaloisExtension (F E : FieldDeclaration) : Set₁ where
  field
    baseField : FieldDeclaration
    extensionField : FieldDeclaration
    galoisGroup : GaloisGroup baseField extensionField
    isGalois : M.Identifier

-- Fundamental Theorem of Galois Theory
-- Sourced from module: fundamentalTheoremGalois

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

-- Galois iff normal and separable
-- Sourced from module: galoisIffNormalSeparable

-- ============================================================================
-- V.3: Advanced Topics and Integration
-- ============================================================================

-- Splitting field of polynomial
record SplittingField (F : FieldDeclaration) (f : M.Identifier) : Set₁ where
  field
    baseField : FieldDeclaration
    polynomial : M.Identifier
    splittingField : FieldDeclaration
    definition : M.Identifier

-- Algebraic closure
record AlgebraicClosure (F : FieldDeclaration) : Set₁ where
  field
    baseField : FieldDeclaration
    closureField : FieldDeclaration
    isAlgebraicClosure : M.Identifier

-- Fundamental Theorem of Algebra
-- Sourced from module: fundamentalTheoremAlgebra

-- Connection to modules: vector spaces over fields
-- Sourced from module: vectorSpacesOverFields

-- Connection to rings: fields are simple commutative rings
-- Sourced from module: fieldsAreSimpleCommutativeRings

-- Connection to algebraic geometry: function fields, Galois groups
-- Sourced from module: functionFieldsAndGalois
