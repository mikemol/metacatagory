{-# OPTIONS --allow-unsolved-metas #-}

-- Tests.LimitsColimitsChecklist: Test instances for limits/colimits in algebra categories

module Tests.LimitsColimitsChecklist where

open import Core.Phase using (Bool; true; false)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Agda.Builtin.Unit using (⊤)
open import Tests.ObligationAdapters as A
open import Core.CategoricalAdapter


-- Limits and colimits in algebra categories (6 assertions)

forgetfulFunctorFromAlgebrasCreatesLimitsTheoremAdapt : A.ForgetfulFunctorFromAlgebrasCreatesLimitsTheoremAdapter
forgetfulFunctorFromAlgebrasCreatesLimitsTheoremAdapt = A.mkForgetfulFunctorFromAlgebrasCreatesLimitsTheoremAdapter _ _ _ refl refl

_ : A.isFilledForgetfulFunctorFromAlgebrasCreatesLimitsTheorem forgetfulFunctorFromAlgebrasCreatesLimitsTheoremAdapt ≡ true
_ = refl

completenessOfAlgebraCategoriesCorollaryAdapt : A.CompletenessOfAlgebraCategoriesCorollaryAdapter
completenessOfAlgebraCategoriesCorollaryAdapt = A.mkCompletenessOfAlgebraCategoriesCorollaryAdapter _ _ _ refl refl

_ : A.isFilledCompletenessOfAlgebraCategoriesCorollary completenessOfAlgebraCategoriesCorollaryAdapt ≡ true
_ = refl

reflexivePairAdapt : A.ReflexivePairAdapter
reflexivePairAdapt = A.mkReflexivePairAdapter _ _ _ refl refl

_ : A.isFilledReflexivePair reflexivePairAdapt ≡ true
_ = refl

forgetfulFunctorPreservesCertainCoequalizersTheoremAdapt : A.ForgetfulFunctorPreservesCertainCoequalizersTheoremAdapter
forgetfulFunctorPreservesCertainCoequalizersTheoremAdapt = A.mkForgetfulFunctorPreservesCertainCoequalizersTheoremAdapter _ _ _ _ refl refl refl

_ : A.isFilledForgetfulFunctorPreservesCertainCoequalizersTheorem forgetfulFunctorPreservesCertainCoequalizersTheoremAdapt ≡ true
_ = refl

reflectsIsomorphismsPropertyAdapt : A.ReflectsIsomorphismsPropertyAdapter
reflectsIsomorphismsPropertyAdapt = A.mkReflectsIsomorphismsPropertyAdapter _ _ refl

_ : A.isFilledReflectsIsomorphismsProperty reflectsIsomorphismsPropertyAdapt ≡ true
_ = refl

uSplitPairAdapt : A.USplitPairAdapter
uSplitPairAdapt = A.mkUSplitPairAdapter _ _ refl

_ : A.isFilledUSplitPair uSplitPairAdapt ≡ true
_ = refl

