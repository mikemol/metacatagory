{-# OPTIONS --without-K #-}

-- | Instantiate ring theory parameters with classical theorems.
module Algebra.Rings.ClassicalInstance where

open import Algebra.Rings.Theorems.Classical as Classical
open import Algebra.Rings.Basic
  Classical.ringFirstIsomorphismTheorem
  Classical.maximalImpliesPrime
  Classical.primeImplesIrreducible
  Classical.ufdIrreducibleIffPrime
  Classical.pidImplesUfd
  Classical.euclideanImpliesPid
  Classical.factorizationHierarchy
  Classical.polynomialPreservesIntegralDomain
  Classical.polynomialPreservesUfd
  Classical.gaussLemma
  Classical.eisensteinCriterion
  Classical.ringCategory
  Classical.commutativeRingsAsLawvereTheory
  Classical.polynomialRingFreeAlgebra
  Classical.localizationUniversalProperty
  Classical.specFunctor
  Classical.quotientRingIsCokernel
  Classical.ringsAndModuleCategories
  public
