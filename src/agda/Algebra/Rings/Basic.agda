{-# OPTIONS --without-K #-}

-- | Ring theory (Hungerford Ch III), parameterized over classical theorems.
open import Core
open import Chapter1.Level1Index
open import Chapter2.Level2sub3
open import Algebra.Foundation
open import Algebra.Groups.Types
open import Algebra.Rings.Types
open import PropertyRegistry
open import Metamodel as M

module Algebra.Rings.Basic
  (ringFirstIsomorphismTheorem : ∀ (f : Set₁) → Set₁)
  (maximalImpliesPrime : ∀ (R M : Set₁) → Set₁)
  (primeImplesIrreducible : ∀ (R p P : Set₁) → Set₁)
  (ufdIrreducibleIffPrime : ∀ (R p : Set₁) → Set₁)
  (pidImplesUfd : ∀ (R : Set₁) → Set₁)
  (euclideanImpliesPid : ∀ (R : Set₁) → Set₁)
  (factorizationHierarchy : Set₁)
  (polynomialPreservesIntegralDomain : ∀ (R : Set₁) → Set₁)
  (polynomialPreservesUfd : ∀ (R : Set₁) → Set₁)
  (gaussLemma : ∀ (R f g : Set₁) → Set₁)
  (eisensteinCriterion : ∀ (R f P : Set₁) → Set₁)
  (ringCategory : Set₁)
  (commutativeRingsAsLawvereTheory : Set₁)
  (polynomialRingFreeAlgebra : ∀ (R : Set₁) → Set₁)
  (localizationUniversalProperty : ∀ (R S : Set₁) → Set₁)
  (specFunctor : Set₁)
  (quotientRingIsCokernel : ∀ (R I : Set₁) → Set₁)
  (ringsAndModuleCategories : ∀ (R : Set₁) → Set₁)
  where

-- Re-export all type definitions from Rings.Types
open import Algebra.Rings.Types
  using (
    RingDeclaration; UnitalRingDeclaration; CommutativeRingDeclaration;
    DivisionRingDeclaration; FieldDeclaration;
    RingHomomorphism; UnitalRingHomomorphism;
    LeftIdeal; RightIdeal; Ideal; PrincipalIdeal; QuotientRing;
    PrimeIdeal; MaximalIdeal;
    IntegralDomain; UniqueFactorizationDomain; UFD; PrincipalIdealDomain; EuclideanDomain;
    Unit; IrreducibleElement; PrimeElement;
    LocalizationOfRing; MultiplicativeSystem; Localization; FieldOfFractions; PolynomialRing; MultivariatePolynomialRing;
    ContentOfPolynomial; PrimitivePolynomial; PrimeSpectrum;
    CategoryOfRings; CategoryOfCommutativeRings
  )
  public

-- The theorems are available as module parameters:
-- 1. ringFirstIsomorphismTheorem: First Isomorphism Theorem for Rings
-- 2. maximalImpliesPrime: Maximal implies Prime
-- 3. primeImplesIrreducible: Prime implies Irreducible
-- 4. ufdIrreducibleIffPrime: In UFD, Irreducible iff Prime
-- 5. pidImplesUfd: PID implies UFD
-- 6. euclideanImpliesPid: Euclidean Domain implies PID
-- 7. factorizationHierarchy: Euclidean ⊂ PID ⊂ UFD ⊂ ID
-- 8. polynomialPreservesIntegralDomain: Polynomials preserve ID
-- 9. polynomialPreservesUfd: Gauss's Lemma - Polynomials preserve UFD
-- 10. gaussLemma: Product of primitive polynomials is primitive
-- 11. eisensteinCriterion: Eisenstein's Criterion for irreducibility
-- 12. ringCategory: Ring category is complete and cocomplete
-- 13. commutativeRingsAsLawvereTheory: CommRing as Lawvere theory
-- 14. polynomialRingFreeAlgebra: R[x] as free R-algebra
-- 15. localizationUniversalProperty: Localization universal property
-- 16. specFunctor: Spec functor (prime ideal spectrum)
-- 17. quotientRingIsCokernel: Quotient as cokernel
-- 18. ringsAndModuleCategories: R-Mod abelian category
