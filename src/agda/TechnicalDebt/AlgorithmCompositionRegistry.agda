{-# OPTIONS --without-K #-}

-- | Registry of algorithm composition components for reuse in TD reports.
module TechnicalDebt.AlgorithmCompositionRegistry where

open import Core.TechnicalDebt
open import Metamodel as M
open import Agda.Builtin.List using (List; []; _∷_)

-- | Identifier for test fixture package used in composition registry.
postulate TestFixturesPackage : M.Identifier

TestFixturesPackageDebt : DebtAnnotation
TestFixturesPackageDebt = mkDebt TestFixturesPackage "Test mocks for composition validation" "open" lowPriority

technicalDebtRegistry : List DebtAnnotation
technicalDebtRegistry = TestFixturesPackageDebt ∷ []
