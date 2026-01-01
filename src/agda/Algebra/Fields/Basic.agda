{-# OPTIONS --without-K #-}

-- | Field theory and Galois theory (parameterized over classic theorems).
open import Core
open import Chapter1.Level1Index
open import Algebra.Foundation
open import Algebra.Rings.Types
open import Algebra.Fields.Types
open import PropertyRegistry
open import Metamodel as M

module Algebra.Fields.Basic
  (degreeOfExtensionFormula : ∀ (F K E : Set₁) → Set₁)
  (fundamentalTheoremGalois : ∀ (F E G : Set₁) → Set₁)
  (galoisIffNormalSeparable : ∀ (F E : Set₁) → Set₁)
  (fundamentalTheoremAlgebra : ∀ (F : Set₁) → Set₁)
  (vectorSpacesOverFields : ∀ (F : Set₁) → Set₁)
  (fieldsAreSimpleCommutativeRings : ∀ (F : Set₁) → Set₁)
  (functionFieldsAndGalois : ∀ (F : Set₁) → Set₁)
  where

-- Re-export all type definitions from Fields.Types
open import Algebra.Fields.Types
  using (
    Subfield; FieldExtension; ExtensionDegree; SimpleExtension;
    AlgebraicElement; AlgebraicExtension; TranscendentalElement;
    TranscendenceBasis; FieldAutomorphism;
    NormalExtension; SeparableExtension; GaloisExtension;
    GaloisGroup; FixedField; SplittingField;
    AlgebraicClosure
  )
  public

-- Re-export VectorSpace from Modules.Basic (where it's now defined to avoid cycles)
open import Algebra.Modules.Basic
  using (VectorSpace; BasisOfVectorSpace)
  public

-- The theorems are available as module parameters:
-- 1. degreeOfExtensionFormula: Degree of Extension Formula
-- 2. fundamentalTheoremGalois: Fundamental Theorem of Galois Theory
-- 3. galoisIffNormalSeparable: Galois iff Normal and Separable
-- 4. fundamentalTheoremAlgebra: Fundamental Theorem of Algebra
-- 5. vectorSpacesOverFields: Vector Spaces Over Fields
-- 6. fieldsAreSimpleCommutativeRings: Fields Are Simple Commutative Rings
-- 7. functionFieldsAndGalois: Function Fields and Galois Theory
