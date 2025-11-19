``` Agda
-- Algebra.Fields.Basic: Field theory and Galois theory (Hungerford Ch V-VI)
-- This module covers field extensions, Galois theory, and connections to rings and modules.

module Algebra.Fields.Basic where

open import Core
open import Chapter1.Level1Index
open import Algebra.Foundation
open import Algebra.Rings.Basic
open import Algebra.Modules.Basic
open import PropertyRegistry
open import Metamodel as M

-- ============================================================================
-- V.1: Field Extensions
-- ============================================================================

-- Field (from Rings.Basic)
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
    inclusion : M.Identifier  -- F ↪ E
    -- E is a vector space over F
    vectorSpaceStructure : M.Identifier

-- Degree of extension [E : F]
record ExtensionDegree (F E : FieldDeclaration) : Set₁ where
  field
    baseField : FieldDeclaration
    extensionField : FieldDeclaration
    degree : M.Identifier  -- dim_F(E)

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
    -- Every element of E is algebraic over F
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
    basis : M.Identifier  -- maximal algebraically independent set
    isTranscendenceBasis : M.Identifier

-- ============================================================================
-- V.2: Galois Theory
-- ============================================================================

-- Automorphism of field extension
record FieldAutomorphism (F E : FieldDeclaration) : Set₁ where
  field
    baseField : FieldDeclaration
    extensionField : FieldDeclaration
    automorphism : M.Identifier  -- σ : E → E, σ|_F = id
    isAutomorphism : M.Identifier

-- Galois group Gal(E/F)
record GaloisGroup (F E : FieldDeclaration) : Set₁ where
  field
    baseField : FieldDeclaration
    extensionField : FieldDeclaration
    group : GroupDeclaration
    -- Gal(E/F) = group of field automorphisms of E fixing F
    automorphisms : M.Identifier

-- Fixed field of automorphism group
record FixedField (F E : FieldDeclaration) (G : GaloisGroup F E) : Set₁ where
  field
    fixedField : FieldDeclaration
    -- F = {x ∈ E | σ(x) = x ∀ σ ∈ G}
    definition : M.Identifier

-- Galois extension
record GaloisExtension (F E : FieldDeclaration) : Set₁ where
  field
    baseField : FieldDeclaration
    extensionField : FieldDeclaration
    galoisGroup : GaloisGroup baseField extensionField
    isGalois : M.Identifier  -- normal and separable

-- Fundamental Theorem of Galois Theory
postulate
  Fundamental-Theorem-Galois :
    (F E : FieldDeclaration) →
    (G : GaloisGroup F E) →
    M.Identifier  -- Subgroups of G ↔ intermediate fields F ⊆ K ⊆ E

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
postulate
  Galois-Iff-Normal-Separable :
    (F E : FieldDeclaration) →
    M.Identifier  -- E/F is Galois iff normal and separable

-- ============================================================================
-- V.3: Advanced Topics and Integration
-- ============================================================================

-- Splitting field of polynomial
record SplittingField (F : FieldDeclaration) (f : M.Identifier) : Set₁ where
  field
    baseField : FieldDeclaration
    polynomial : M.Identifier
    splittingField : FieldDeclaration
    -- f splits completely in splittingField
    definition : M.Identifier

-- Algebraic closure
record AlgebraicClosure (F : FieldDeclaration) : Set₁ where
  field
    baseField : FieldDeclaration
    closureField : FieldDeclaration
    isAlgebraicClosure : M.Identifier

-- Fundamental Theorem of Algebra
postulate
  Fundamental-Theorem-Algebra :
    (F : FieldDeclaration) →
    M.Identifier  -- Every nonconstant polynomial has root in algebraic closure

-- Connection to modules: vector spaces over fields
postulate
  VectorSpaces-Over-Fields :
    (F : FieldDeclaration) →
    M.Identifier  -- Vect(F) is abelian, complete, cocomplete

-- Connection to rings: fields are simple commutative rings
postulate
  Fields-Are-Simple-Commutative-Rings :
    (F : FieldDeclaration) →
    M.Identifier  -- No nontrivial ideals

-- Connection to algebraic geometry: function fields, Galois groups
postulate
  FunctionFields-And-Galois :
    (F : FieldDeclaration) →
    M.Identifier  -- Function fields and Galois theory link to geometry
```
