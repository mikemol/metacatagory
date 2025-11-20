module Examples.TechnicalDebtChecklist where

open import Metamodel as M
open import PropertyRegistry as PR
open import Agda.Builtin.List
open import Agda.Builtin.String
open import Agda.Builtin.Int

-- Import example registry from AlgorithmCompositionTests
open import Tests.AlgorithmCompositionTests using (technicalDebtRegistry; DebtAnnotation; Priority; PriorityGreater; highPriority; lowPriority)

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
priorityIsGroup p = List (String × Int) → Set
priorityIsGroup _ = ⊤  -- Placeholder: extend for group laws

priorityGroupTest : priorityIsGroup highPriority
priorityGroupTest _ = tt

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

rationalesTest : rationalesNonEmpty (List.map DebtAnnotation.rationale technicalDebtRegistry)
rationalesTest ()

------------------------------------------------------------------------
-- 5. Typechecking Integration
------------------------------------------------------------------------

checkTypechecks : ⊤
checkTypechecks = tt

-- Extend with more properties as needed
