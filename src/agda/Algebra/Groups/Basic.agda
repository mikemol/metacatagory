{-# OPTIONS --without-K #-}

-- | Fundamental group theory (parameterized over classic theorems).
open import Core
open import Chapter1.Level1Index
open import Algebra.Foundation
open import PropertyRegistry
open import Metamodel as M
open import Algebra.Groups.Types

module Algebra.Groups.Basic
  (cyclicGroupClassification : ∀ (G : CyclicGroup) → M.Identifier)
  (lagrangeTheorem : ∀ (G : GroupDeclaration) (H : Subgroup G) → M.Identifier)
  (firstIsomorphismTheorem : ∀ (f : M.Identifier) → M.Identifier)
  (secondIsomorphismTheorem : ∀ (G : GroupDeclaration) (H : Subgroup G) (N : NormalSubgroup G) → M.Identifier)
  (thirdIsomorphismTheorem : ∀ (G : GroupDeclaration) (H N : NormalSubgroup G) → M.Identifier)
  (alternatingIsSimple : ∀ (n : M.Identifier) → M.Identifier)
  (normalSubgroupIsKernel : ∀ (G : GroupDeclaration) (N : NormalSubgroup G) → M.Identifier)
  (quotientGroupIsCokernelInAb : ∀ (G : AbelianGroupDeclaration) (N : NormalSubgroup (AbelianGroupDeclaration.underlyingGroup G)) → M.Identifier)
  (groupsAsLawvereModels : M.Identifier)
  (freeForgetfulAdjunctionGrp : M.Identifier)
  where

-- Re-export the types for users of this module
open import Algebra.Groups.Types
  using (
    Subgroup; CyclicGroup; NormalSubgroup;
    ImageOfHomomorphism; KernelOfHomomorphism;
    ElementOrder; LeftCoset; IndexOfSubgroup;
    QuotientGroup;
    SymmetricGroup; Cycle; SignOfPermutation;
    AlternatingGroup; DihedralGroup
  )
  public

-- ============================================================================
-- Integration Points with Category Theory
-- ============================================================================

-- Groups form a category (from Foundation)
-- Grp has:
-- - Objects: groups
-- - Morphisms: group homomorphisms
-- - Composition: function composition
-- - Identity: identity map

-- The theorems are available as module parameters:
-- - cyclicGroupClassification: Cyclic Group Classification theorem
-- - lagrangeTheorem: Lagrange's Theorem
-- - firstIsomorphismTheorem: First Isomorphism Theorem
-- - secondIsomorphismTheorem: Second Isomorphism Theorem
-- - thirdIsomorphismTheorem: Third Isomorphism Theorem
-- - alternatingIsSimple: Alternating Group Simplicity (A_n for n ≥ 5)
-- - normalSubgroupIsKernel: Normal Subgroup as Kernel
-- - quotientGroupIsCokernelInAb: Quotient as Cokernel in Ab
-- - groupsAsLawvereModels: Groups as Lawvere Models
-- - freeForgetfulAdjunctionGrp: Free-Forgetful Adjunction

-- To instantiate this parameterized module with classical theorems, use:
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
-- Or use Algebra.Groups.ClassicalInstance (provided below).
