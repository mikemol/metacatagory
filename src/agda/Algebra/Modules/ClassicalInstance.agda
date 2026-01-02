{-# OPTIONS --without-K #-}

-- | Instantiate module theory with classical theorems.
module Algebra.Modules.ClassicalInstance where

open import Algebra.Modules.Theorems.Classical as Classical
open import Algebra.Modules.Basic
  Classical.rModCategoricalProperties
  Classical.freeModuleAdjunction
  Classical.freeImplesProjective
  Classical.projectiveInjectiveProperties
  Classical.homLeftExact
  Classical.freeFinitelyGeneratedReflexive
  Classical.tensorProductProperties
  Classical.basisCardinalityInvariant
  Classical.pidModuleClassification
  Classical.polynomialRingFreeRAlgebra
  Classical.rModHomologicalAlgebraPackage
  public
