{-# OPTIONS --without-K #-}

-- Algebra.Groups.Theorems.Classical
-- Provides classical proofs of group theorems.
--
-- This module exports theorem identifiers that can be passed as parameters to
-- Algebra.Groups.Basic, instantiating it with a classical proof approach.
--
-- The theorems here are marked as postulates (asserting they have been proven
-- in classical texts), but they are localized to this theory module rather than
-- scattered throughout the algebra structure. This enforces that:
-- 1. All classical proofs are in one place (modular)
-- 2. Alternative proof approaches can coexist (e.g., Categorical, Constructive)
-- 3. Consumers explicitly choose which theorem package to use
-- 4. The metatheory can explore comparative formalizations

module Algebra.Groups.Theorems.Classical where

open import Core
open import Algebra.Foundation
open import Algebra.Groups.Types
open import Metamodel as M

-- ============================================================================
-- THEOREM IMPLEMENTATIONS (Classical Approach)
-- ============================================================================
-- These are presented as postulates within THIS theorem module, making it clear
-- they are part of the classical proof package. Consumers of this package are
-- explicitly accepting classical mathematics.

-- Cyclic Group Classification (Hungerford I.3.5)
-- Every cyclic group is isomorphic to ℤ or ℤ/nℤ for some n ∈ ℕ
postulate
  cyclicGroupClassification : 
    ∀ (G : CyclicGroup) → M.Identifier

-- Lagrange's Theorem (Hungerford I.4.3)
-- If H is a subgroup of G, then |H| divides |G|, i.e., |G| = |H| · [G:H]
postulate
  lagrangeTheorem :
    ∀ (G : GroupDeclaration) (H : Subgroup G) → M.Identifier

-- First Isomorphism Theorem (Hungerford I.5.4)
-- If f : G → H is a group homomorphism, then G/ker(f) ≅ im(f)
postulate
  firstIsomorphismTheorem :
    ∀ (f : M.Identifier) → M.Identifier

-- Second Isomorphism Theorem (Hungerford I.5.5)
-- Diamond isomorphism: H/(H ∩ N) ≅ HN/N for subgroups H, N of G
postulate
  secondIsomorphismTheorem :
    ∀ (G : GroupDeclaration) (H : Subgroup G) (N : NormalSubgroup G) → M.Identifier

-- Third Isomorphism Theorem (Hungerford I.5.6)
-- Quotients compose: (G/H)/(N/H) ≅ G/N when H ⊆ N are normal subgroups
postulate
  thirdIsomorphismTheorem :
    ∀ (G : GroupDeclaration) (H N : NormalSubgroup G) → M.Identifier

-- Alternating Group Simplicity (Hungerford I.6.7)
-- The alternating group Aₙ is simple (has no proper normal subgroups) for n ≥ 5
postulate
  alternatingIsSimple :
    ∀ (n : M.Identifier) → M.Identifier

-- Normal Subgroup as Kernel (Hungerford I.5.3, categorical)
-- Every normal subgroup is the kernel of some group homomorphism
postulate
  normalSubgroupIsKernel :
    ∀ (G : GroupDeclaration) (N : NormalSubgroup G) → M.Identifier

-- Quotient as Cokernel in Abelian Categories (Categorical perspective)
-- In the category Ab of abelian groups, G/N is the cokernel of the inclusion N ↪ G
postulate
  quotientGroupIsCokernelInAb :
    ∀ (G : AbelianGroupDeclaration) 
      (N : NormalSubgroup (AbelianGroupDeclaration.underlyingGroup G)) → M.Identifier

-- Groups as Lawvere Algebra (Lawvere theories, categorical)
-- Groups are exactly the algebras for the Lawvere theory of groups
postulate
  groupsAsLawvereModels : M.Identifier

-- Free-Forgetful Adjunction (Categorical)
-- The free group functor Free : Set → Grp is left adjoint to the forgetful functor Forget : Grp → Set
postulate
  freeForgetfulAdjunctionGrp : M.Identifier

-- ============================================================================
-- INSTANTIATION SIGNATURE
-- ============================================================================
-- To use these classical theorems with Algebra.Groups.Basic, instantiate as:
--
--   module GroupsClassical = Algebra.Groups.Basic
--     Classical.cyclicGroupClassification
--     Classical.lagrangeTheorem
--     Classical.firstIsomorphismTheorem
--     Classical.secondIsomorphismTheorem
--     Classical.thirdIsomorphismTheorem
--     Classical.alternatingIsSimple
--     Classical.normalSubgroupIsKernel
--     Classical.quotientGroupIsCokernelInAb
--     Classical.groupsAsLawvereModels
--     Classical.freeForgetfulAdjunctionGrp
--
-- Or more concisely, if using module-level opens:
--
--   open Algebra.Groups.Theorems.Classical
--   open Algebra.Groups.Basic
--     cyclicGroupClassification
--     lagrangeTheorem
--     firstIsomorphismTheorem
--     secondIsomorphismTheorem
--     thirdIsomorphismTheorem
--     alternatingIsSimple
--     normalSubgroupIsKernel
--     quotientGroupIsCokernelInAb
--     groupsAsLawvereModels
--     freeForgetfulAdjunctionGrp

-- Import type definitions from Groups.Types
open import Algebra.Groups.Types
  using (
    Subgroup; ImageOfHomomorphism; KernelOfHomomorphism;
    CyclicGroup; ElementOrder;
    LeftCoset; IndexOfSubgroup;
    NormalSubgroup; QuotientGroup;
    SymmetricGroup; Cycle; SignOfPermutation;
    AlternatingGroup; DihedralGroup
  )
  public
