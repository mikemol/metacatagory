{-# OPTIONS --allow-unsolved-metas --without-K #-}

-- | Checklist for subobject theory obligations.
module Tests.SubobjectTheoryChecklist where

open import Tests.ObligationAdapters as A
open import Core.Phase using (Bool; true; false)
open import Agda.Builtin.Equality using (refl; _≡_)
import Metamodel as M

-- Subobject Theory coverage assertions
-- Total: 11 adapters for subobject lattices, factorization, generators, projectives/injectives

-- Placeholder adapter (status = false). Assertion now reflects reality.
-- TODO: Replace with constructor-based adapter via mkSubobjectLatticeAdapter once a concrete lattice example is added.
emptySubobjectLatticeAdapter : A.SubobjectLatticeAdapter
emptySubobjectLatticeAdapter =
  record { decl = record { X = M.mkId "" } ; expectedX = M.mkId "" ; link = refl ; status = false }

_ : A.isFilledSubobjectLattice emptySubobjectLatticeAdapter ≡ false
_ = refl

-- TODO: Provide real well-powered category (e.g., Set) and switch to mkWellPoweredCategoryAdapter.
emptyWellPoweredCategoryAdapter : A.WellPoweredCategoryAdapter
emptyWellPoweredCategoryAdapter =
  record { decl = record { C = M.mkId "" } ; expectedC = M.mkId "" ; link = refl ; status = false }

_ : A.isFilledWellPoweredCategory emptyWellPoweredCategoryAdapter ≡ false
_ = refl

-- TODO: Populate with actual completeness proof of a subobject lattice.
emptySubobjectLatticeIsCompleteAdapter : A.SubobjectLatticeIsCompleteAdapter
emptySubobjectLatticeIsCompleteAdapter = record { decl = record { unit = _ } ; status = false }

_ : A.isFilledSubobjectLatticeIsComplete emptySubobjectLatticeIsCompleteAdapter ≡ false
_ = refl

-- TODO: Provide concrete strong epi (e.g., quotient map in Set) via mkStrongEpimorphismAdapter.
emptyStrongEpimorphismAdapter : A.StrongEpimorphismAdapter
emptyStrongEpimorphismAdapter = record { decl = record { e = M.mkId "" } ; expectedE = M.mkId "" ; link = refl ; status = false }

_ : A.isFilledStrongEpimorphism emptyStrongEpimorphismAdapter ≡ false
_ = refl

-- TODO: Replace with (E,M) factorization system witness using constructor.
emptyCanonicalFactorizationSystemAdapter : A.CanonicalFactorizationSystemAdapter
emptyCanonicalFactorizationSystemAdapter = record { decl = record { unit = _ } ; status = false }

_ : A.isFilledCanonicalFactorizationSystem emptyCanonicalFactorizationSystemAdapter ≡ false
_ = refl

-- TODO: Provide actual factorization f = m ∘ e with witness identifiers.
emptyMorphismFactorizationAdapter : A.MorphismFactorizationAdapter
emptyMorphismFactorizationAdapter = record { decl = record { f = M.mkId "" ; e = M.mkId "" ; m = M.mkId "" ; I = M.mkId "" } ; expectedF = M.mkId "" ; expectedE = M.mkId "" ; expectedM = M.mkId "" ; link1 = refl ; link2 = refl ; link3 = refl ; status = false }

_ : A.isFilledMorphismFactorization emptyMorphismFactorizationAdapter ≡ false
_ = refl

-- TODO: Provide generator object for a concrete category (e.g., singleton set for Set).
emptyHasGeneratorObjectAdapter : A.HasGeneratorObjectAdapter
emptyHasGeneratorObjectAdapter = record { decl = record { C = M.mkId "" ; G = M.mkId "" } ; expectedC = M.mkId "" ; expectedG = M.mkId "" ; link1 = refl ; link2 = refl ; status = false }

_ : A.isFilledHasGeneratorObject emptyHasGeneratorObjectAdapter ≡ false
_ = refl

-- TODO: Use free module or free object example for projective witness.
emptyProjectiveObjectAdapter : A.ProjectiveObjectAdapter
emptyProjectiveObjectAdapter = record { decl = record { P = M.mkId "" } ; expectedP = M.mkId "" ; link = refl ; status = false }

_ : A.isFilledProjectiveObject emptyProjectiveObjectAdapter ≡ false
_ = refl

-- TODO: Provide injective object (e.g., divisible group) using constructor adapter.
emptyInjectiveObjectAdapter : A.InjectiveObjectAdapter
emptyInjectiveObjectAdapter = record { decl = record { I = M.mkId "" } ; expectedI = M.mkId "" ; link = refl ; status = false }

_ : A.isFilledInjectiveObject emptyInjectiveObjectAdapter ≡ false
_ = refl

-- TODO: Replace with category having enough projectives proof.
emptyHasEnoughProjectivesAdapter : A.HasEnoughProjectivesAdapter
emptyHasEnoughProjectivesAdapter = record { decl = record { C = M.mkId "" } ; expectedC = M.mkId "" ; link = refl ; status = false }

_ : A.isFilledHasEnoughProjectives emptyHasEnoughProjectivesAdapter ≡ false
_ = refl

-- TODO: Replace with category having enough injectives proof.
emptyHasEnoughInjectivesAdapter : A.HasEnoughInjectivesAdapter
emptyHasEnoughInjectivesAdapter = record { decl = record { C = M.mkId "" } ; expectedC = M.mkId "" ; link = refl ; status = false }

_ : A.isFilledHasEnoughInjectives emptyHasEnoughInjectivesAdapter ≡ false
_ = refl
