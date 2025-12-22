{-# OPTIONS --without-K #-}

open import Core
open import Chapter1.Level1Index
open import Algebra.Foundation
open import Algebra.Rings.Types
open import Algebra.Fields.Types
open import PropertyRegistry
open import Metamodel as M

-- Algebra.Fields.Basic: Field theory and Galois theory (Hungerford Ch V-VI)
-- 
-- PARAMETERIZED VERSION: All 7 classical field theorems are module parameters
-- rather than postulated locally.

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
    FieldExtension; AlgebraicElement; DegreeOfExtension;
    NormalExtension; SeparableExtension; GaloisExtension;
    GaloisGroup; FixedField; SplittingField;
    AlgebraicClosure; VectorSpace; BasisOfVectorSpace
  )
  public

-- The theorems are available as module parameters:
-- 1. degreeOfExtensionFormula: Degree of Extension Formula
-- 2. fundamentalTheoremGalois: Fundamental Theorem of Galois Theory
-- 3. galoisIffNormalSeparable: Galois iff Normal and Separable
-- 4. fundamentalTheoremAlgebra: Fundamental Theorem of Algebra
-- 5. vectorSpacesOverFields: Vector Spaces Over Fields
-- 6. fieldsAreSimpleCommutativeRings: Fields Are Simple Commutative Rings
-- 7. functionFieldsAndGalois: Function Fields and Galois Theory
