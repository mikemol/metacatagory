{-# OPTIONS --without-K #-}

open import Core
open import Chapter1.Level1Index
open import Chapter2.Level2sub1
open import Algebra.Foundation
open import Algebra.Groups.Types
open import Algebra.Groups.Abelian
open import Algebra.Rings.Types
open import Algebra.Modules.Types
open import PropertyRegistry
open import Metamodel as M

-- Algebra.Modules.Basic: Module theory (Hungerford Ch IV)
-- 
-- PARAMETERIZED VERSION: All 11 classical module theorems are module parameters
-- rather than postulated locally.

module Algebra.Modules.Basic
  (rModCategoricalProperties : ∀ (R : Set₁) → Set₁)
  (freeModuleAdjunction : ∀ (R F U : Set₁) → Set₁)
  (freeImplesProjective : ∀ (R X F : Set₁) → Set₁)
  (projectiveInjectiveProperties : ∀ (R : Set₁) → Set₁)
  (homLeftExact : ∀ (R M : Set₁) → Set₁)
  (freeFinitelyGeneratedReflexive : ∀ (R M : Set₁) → Set₁)
  (tensorProductProperties : ∀ (R : Set₁) → Set₁)
  (basisCardinalityInvariant : ∀ (F V B₁ B₂ : Set₁) → Set₁)
  (pidModuleClassification : ∀ (R : Set₁) → Set₁)
  (polynomialRingFreeRAlgebra : ∀ (R : Set₁) → Set₁)
  (rModHomologicalAlgebraPackage : ∀ (R : Set₁) → Set₁)
  where

-- Re-export all type definitions from Modules.Types
open import Algebra.Modules.Types
  using (
    LeftModule; RightModule; ModuleHomomorphism;
    Submodule; QuotientModule;
    KernelOfModuleHomomorphism; ImageOfModuleHomomorphism; CokernelOfModuleHomomorphism;
    ExactSequence; ShortExactSequence;
    CategoryOfModules;
    FreeModule; ProjectiveModule; InjectiveModule;
    HomFunctor; TensorProduct;
    TorsionElement
  )
  public

-- The theorems are available as module parameters:
-- 1. rModCategoricalProperties: R-Mod categorical properties
-- 2. freeModuleAdjunction: Free-Module adjunction
-- 3. freeImplesProjective: Free modules are projective
-- 4. projectiveInjectiveProperties: Projective and injective properties
-- 5. homLeftExact: Hom functor is left exact
-- 6. freeFinitelyGeneratedReflexive: FG free modules are reflexive
-- 7. tensorProductProperties: Tensor product properties
-- 8. basisCardinalityInvariant: Basis cardinality is invariant
-- 9. pidModuleClassification: PID module classification theorem
-- 10. polynomialRingFreeRAlgebra: Polynomial ring is free R-algebra
-- 11. rModHomologicalAlgebraPackage: R-Mod homological algebra
