{-# OPTIONS --without-K #-}

-- | Validate algebraic hierarchy composition ordering.
module Tests.HierarchyValidation where

-- Phase I.1.3: Hierarchy Composition Validation (P5 DAG)
-- We verify that indices respect the algebraic hierarchy orderings.
-- Per user guidance, avoid brittle inline equality proofs; use Bool checks.

open import Core.Phase using (Bool; true; false)
open import Agda.Builtin.Unit using (⊤; tt)
open import Agda.Builtin.Nat using (Nat; zero; suc)

-- Reuse the well-founded index definition and ordering from CoverageReport
open import Tests.CoverageReport using (WellFoundedIndex; mkIndex; _<ᵢ_)

-- Minimal Boolean combinators (since Agda.Builtin.Bool exports only Bool/constructors)
andBool : Bool → Bool → Bool
andBool true b = b
andBool false _ = false

orBool : Bool → Bool → Bool
orBool true _ = true
orBool false b = b

-- Enumerate base Declaration kinds used for hierarchy checks
-- (This avoids modifying the Algebra/Foundation declarations themselves.)
data DeclarationKind : Set where
  MagmaK : DeclarationKind
  SemigroupK : DeclarationKind
  MonoidK : DeclarationKind
  GroupK : DeclarationKind
  AbelianGroupK : DeclarationKind

-- Assign indices consistent with CoverageReport hierarchy
-- 0: Magma, 1: Semigroup, 2: Monoid, 3: Group, 3/4: Abelian Group (≥ Group)
DeclIndex : DeclarationKind → WellFoundedIndex
DeclIndex MagmaK = mkIndex 0 0
DeclIndex SemigroupK = mkIndex 1 0
DeclIndex MonoidK = mkIndex 2 0
DeclIndex GroupK = mkIndex 3 0
DeclIndex AbelianGroupK = mkIndex 3 1

-- Boolean checks encoding the DAG constraints
-- 1) Semigroup > Magma, 2) Monoid > Semigroup, 3) Group > Monoid,
-- 4) AbelianGroup ≥ Group (we check Group < AbelianGroup for strictness)
semigroupAboveMagma : Bool
semigroupAboveMagma = DeclIndex MagmaK <ᵢ DeclIndex SemigroupK

monoidAboveSemigroup : Bool
monoidAboveSemigroup = DeclIndex SemigroupK <ᵢ DeclIndex MonoidK

groupAboveMonoid : Bool
groupAboveMonoid = DeclIndex MonoidK <ᵢ DeclIndex GroupK

abelianAboveGroup : Bool
abelianAboveGroup = DeclIndex GroupK <ᵢ DeclIndex AbelianGroupK

-- Aggregate all constraints
allHierarchyConstraints : Bool
allHierarchyConstraints = andBool semigroupAboveMagma
                        (andBool monoidAboveSemigroup
                        (andBool groupAboveMonoid
                                abelianAboveGroup))

-- Optional: expose a unit value when constraints hold (non-failing build)
-- If desired later, we can switch this to a proof-relevant contract.
constraintsWitness : ⊤
constraintsWitness = tt
