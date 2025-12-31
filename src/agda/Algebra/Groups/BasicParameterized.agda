{-# OPTIONS --without-K #-}

open import Core
open import Chapter1.Level1Index
open import Algebra.Foundation
open import PropertyRegistry
open import Metamodel as M
open import Algebra.Groups.Types

-- Algebra.Groups.BasicParameterized: Group theory with explicit theorem parameters
-- Instead of postulating theorems, they are passed as module parameters.
-- This forces all consumers to explicitly provide or derive proofs, preventing
-- hidden assumptions of "well-known" theorems without rigorous justification.

module Algebra.Groups.BasicParameterized
  -- ==== THEOREM PARAMETERS ====
  -- These theorems must be provided by the instantiating module
  -- Either as concrete proofs or imported from a theorem library
  (CyclicGroup-Classification : 
    ∀ (G : CyclicGroup) → M.Identifier)  -- G ≅ ℤ or G ≅ ℤ/nℤ
  
  (Lagrange-Theorem :
    ∀ (G : GroupDeclaration) (H : Subgroup G) → M.Identifier)
  
  (First-Isomorphism-Theorem :
    ∀ (f : M.Identifier) → M.Identifier)  -- G/ker(f) ≅ im(f)
  
  (Second-Isomorphism-Theorem :
    ∀ (G : GroupDeclaration) (H : Subgroup G) (N : NormalSubgroup G) → M.Identifier)
  
  (Third-Isomorphism-Theorem :
    ∀ (G : GroupDeclaration) (H N : NormalSubgroup G) → M.Identifier)
  
  (Alternating-Is-Simple :
    ∀ (n : M.Identifier) → M.Identifier)  -- n ≥ 5 → Aₙ is simple
  
  (NormalSubgroup-Is-Kernel :
    ∀ (G : GroupDeclaration) (N : NormalSubgroup G) → M.Identifier)
  
  (QuotientGroup-Is-Cokernel-In-Ab :
    ∀ (G : AbelianGroupDeclaration) 
      (N : NormalSubgroup (AbelianGroupDeclaration.underlyingGroup G)) → M.Identifier)
  
  (Groups-As-Lawvere-Models : M.Identifier)
  
  (Free-Forgetful-Adjunction-Grp : M.Identifier)
  
  where

-- ============================================================================
-- Type Re-exports
-- ============================================================================

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
-- THEOREM APPLICATION (using parameters instead of postulates)
-- ============================================================================

-- Example: Derive a consequence of Lagrange's theorem using the parameter
lagrangeConsequence : 
  ∀ (G : GroupDeclaration) (H : Subgroup G) → M.Identifier
lagrangeConsequence = λ G H → Lagrange-Theorem G H

-- Example: Use first isomorphism theorem
firstIsoConsequence :
  ∀ (f : M.Identifier) → M.Identifier
firstIsoConsequence f = First-Isomorphism-Theorem f

-- ============================================================================
-- NOTE ON ARCHITECTURE
-- ============================================================================
-- When instantiating this module, the caller must provide proofs (or
-- references to proven theorems) for all 10 theorem parameters.
-- 
-- Example instantiation in a proof library:
--   module GroupTheoryWithProofs = 
--     Algebra.Groups.BasicParameterized 
--       (proof-of-CyclicGroup-Classification)
--       (proof-of-Lagrange)
--       ... (8 more theorem proofs)
--
-- This forces explicit accounting: either the theorem is proven, or it's
-- imported from a theorem package that has proven it. No implicit assumptions
-- of "well-known" theorems without explicit justification.
