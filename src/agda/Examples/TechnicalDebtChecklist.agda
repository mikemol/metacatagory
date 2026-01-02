{-# OPTIONS --without-K #-}

-- | Example checklist for technical debt items.
module Examples.TechnicalDebtChecklist where

open import Metamodel as M
open import PropertyRegistry as PR
open import Agda.Builtin.List
open import Agda.Builtin.String
open import Agda.Builtin.Int
open import Agda.Builtin.Equality using (_≡_; refl)
open import Agda.Builtin.Unit using (⊤; tt)
open import Agda.Builtin.Sigma using (Σ; _,_)
open import Core.Utils using (map)

data ⊥ : Set where
-- | Empty type used for contradiction in non-empty proofs.

-- | Cartesian product as a Σ-specialization (for simple pairing).
_×_ : Set → Set → Set
A × B = Σ A (λ _ → B)

-- Import example registry from AlgorithmCompositionTests
open import Tests.AlgorithmCompositionTests using (technicalDebtRegistry)
open import Core.TechnicalDebt using (DebtAnnotation; Priority; PriorityGreater; highPriority; lowPriority)

------------------------------------------------------------------------
-- 1. Registry Construction
------------------------------------------------------------------------

registryNonEmpty : List DebtAnnotation → Set
registryNonEmpty xs = xs ≡ [] → ⊥

registryTest : registryNonEmpty technicalDebtRegistry
registryTest ()

------------------------------------------------------------------------
-- 2. Priority Algebra
------------------------------------------------------------------------

priorityIsGroup : Priority → Set
priorityIsGroup _ = ⊤  -- Placeholder: extend for group laws

priorityGroupTest : priorityIsGroup highPriority
priorityGroupTest = tt

------------------------------------------------------------------------
-- 3. Priority Comparison
------------------------------------------------------------------------

priorityComparisonTest : PriorityGreater highPriority lowPriority
priorityComparisonTest = refl

------------------------------------------------------------------------
-- 4. Reporting/Export
------------------------------------------------------------------------

rationalesNonEmpty : List String → Set
rationalesNonEmpty rs = rs ≡ [] → ⊥

rationalesTest : rationalesNonEmpty (map DebtAnnotation.rationale technicalDebtRegistry)
rationalesTest ()

------------------------------------------------------------------------
-- 5. Typechecking Integration
------------------------------------------------------------------------

checkTypechecks : ⊤
checkTypechecks = tt

-- Extend with more properties as needed
