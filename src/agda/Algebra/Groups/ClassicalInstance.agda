{-# OPTIONS --without-K #-}

-- Algebra.Groups.ClassicalInstance: Groups with classical theorem proofs
-- This module instantiates the parameterized Algebra.Groups.Basic with theorems
-- from Algebra.Groups.Theorems.Classical, providing the "standard" groups interface.

module Algebra.Groups.ClassicalInstance where

open import Algebra.Groups.Theorems.Classical as Classical
open import Algebra.Groups.Basic
  Classical.cyclicGroupClassification
  Classical.lagrangeTheorem
  Classical.firstIsomorphismTheorem
  Classical.secondIsomorphismTheorem
  Classical.thirdIsomorphismTheorem
  Classical.alternatingIsSimple
  Classical.normalSubgroupIsKernel
  Classical.quotientGroupIsCokernelInAb
  Classical.groupsAsLawvereModels
  Classical.freeForgetfulAdjunctionGrp
  public
