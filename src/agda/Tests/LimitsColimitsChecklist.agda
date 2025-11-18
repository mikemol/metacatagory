-- Tests.LimitsColimitsChecklist: Test instances for limits/colimits in algebra categories

module Tests.LimitsColimitsChecklist where

open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.Equality using (refl)
open import Agda.Builtin.Unit using (⊤)
open import Tests.ObligationAdapters as A
open import Core.CategoricalAdapter


-- Limits and colimits in algebra categories (6 assertions)

forgetfulFunctorFromAlgebrasCreatesLimitsTheoremAdapt : A.ForgetfulFunctorFromAlgebrasCreatesLimitsTheoremAdapter
forgetfulFunctorFromAlgebrasCreatesLimitsTheoremAdapt = A.mkForgetfulFunctorFromAlgebrasCreatesLimitsTheoremAdapter _ _ _ refl refl

_ : A.isFilledForgetfulFunctorFromAlgebrasCreatesLimitsTheorem forgetfulFunctorFromAlgebrasCreatesLimitsTheoremAdapt ≡ true
_ = refl

-- Categorical assertions for ForgetfulFunctorFromAlgebrasCreatesLimitsTheorem
_ : CategoricalAdapter.morphism (A.forgetfulFunctorFromAlgebrasCreatesLimitsTheoremCategorical forgetfulFunctorFromAlgebrasCreatesLimitsTheoremAdapt) ⊤ ⊤ ≡
    CategoricalAdapter.object (A.forgetfulFunctorFromAlgebrasCreatesLimitsTheoremCategorical forgetfulFunctorFromAlgebrasCreatesLimitsTheoremAdapt) ⊤
_ = refl

_ : CategoricalAdapter.isomorphism (A.forgetfulFunctorFromAlgebrasCreatesLimitsTheoremCategorical forgetfulFunctorFromAlgebrasCreatesLimitsTheoremAdapt) ⊤ ⊤ ≡ refl
_ = refl

completenessOfAlgebraCategoriesCorollaryAdapt : A.CompletenessOfAlgebraCategoriesCorollaryAdapter
completenessOfAlgebraCategoriesCorollaryAdapt = A.mkCompletenessOfAlgebraCategoriesCorollaryAdapter _ _ _ refl refl

_ : A.isFilledCompletenessOfAlgebraCategoriesCorollary completenessOfAlgebraCategoriesCorollaryAdapt ≡ true
_ = refl

-- Categorical assertions for CompletenessOfAlgebraCategoriesCorollary
_ : CategoricalAdapter.morphism (A.completenessOfAlgebraCategoriesCorollaryCategorical completenessOfAlgebraCategoriesCorollaryAdapt) ⊤ ⊤ ≡
    CategoricalAdapter.object (A.completenessOfAlgebraCategoriesCorollaryCategorical completenessOfAlgebraCategoriesCorollaryAdapt) ⊤
_ = refl

_ : CategoricalAdapter.isomorphism (A.completenessOfAlgebraCategoriesCorollaryCategorical completenessOfAlgebraCategoriesCorollaryAdapt) ⊤ ⊤ ≡ refl
_ = refl

reflexivePairAdapt : A.ReflexivePairAdapter
reflexivePairAdapt = A.mkReflexivePairAdapter _ _ _ refl refl

_ : A.isFilledReflexivePair reflexivePairAdapt ≡ true
_ = refl

-- Categorical assertions for ReflexivePair
_ : CategoricalAdapter.morphism (A.reflexivePairCategorical reflexivePairAdapt) ⊤ ⊤ ≡
    CategoricalAdapter.object (A.reflexivePairCategorical reflexivePairAdapt) ⊤
_ = refl

_ : CategoricalAdapter.isomorphism (A.reflexivePairCategorical reflexivePairAdapt) ⊤ ⊤ ≡ refl
_ = refl

forgetfulFunctorPreservesCertainCoequalizersTheoremAdapt : A.ForgetfulFunctorPreservesCertainCoequalizersTheoremAdapter
forgetfulFunctorPreservesCertainCoequalizersTheoremAdapt = A.mkForgetfulFunctorPreservesCertainCoequalizersTheoremAdapter _ _ _ _ refl refl refl

_ : A.isFilledForgetfulFunctorPreservesCertainCoequalizersTheorem forgetfulFunctorPreservesCertainCoequalizersTheoremAdapt ≡ true
_ = refl

-- Categorical assertions for ForgetfulFunctorPreservesCertainCoequalizersTheorem
_ : CategoricalAdapter.morphism (A.forgetfulFunctorPreservesCertainCoequalizersTheoremCategorical forgetfulFunctorPreservesCertainCoequalizersTheoremAdapt) ⊤ ⊤ ≡
    CategoricalAdapter.object (A.forgetfulFunctorPreservesCertainCoequalizersTheoremCategorical forgetfulFunctorPreservesCertainCoequalizersTheoremAdapt) ⊤
_ = refl

_ : CategoricalAdapter.isomorphism (A.forgetfulFunctorPreservesCertainCoequalizersTheoremCategorical forgetfulFunctorPreservesCertainCoequalizersTheoremAdapt) ⊤ ⊤ ≡ refl
_ = refl

reflectsIsomorphismsPropertyAdapt : A.ReflectsIsomorphismsPropertyAdapter
reflectsIsomorphismsPropertyAdapt = A.mkReflectsIsomorphismsPropertyAdapter _ _ refl

_ : A.isFilledReflectsIsomorphismsProperty reflectsIsomorphismsPropertyAdapt ≡ true
_ = refl

-- Categorical assertions for ReflectsIsomorphismsProperty
_ : CategoricalAdapter.morphism (A.reflectsIsomorphismsPropertyCategorical reflectsIsomorphismsPropertyAdapt) ⊤ ⊤ ≡
    CategoricalAdapter.object (A.reflectsIsomorphismsPropertyCategorical reflectsIsomorphismsPropertyAdapt) ⊤
_ = refl

_ : CategoricalAdapter.isomorphism (A.reflectsIsomorphismsPropertyCategorical reflectsIsomorphismsPropertyAdapt) ⊤ ⊤ ≡ refl
_ = refl

uSplitPairAdapt : A.USplitPairAdapter
uSplitPairAdapt = A.mkUSplitPairAdapter _ _ refl

_ : A.isFilledUSplitPair uSplitPairAdapt ≡ true
_ = refl

-- Categorical assertions for USplitPair
_ : CategoricalAdapter.morphism (A.uSplitPairCategorical uSplitPairAdapt) ⊤ ⊤ ≡
    CategoricalAdapter.object (A.uSplitPairCategorical uSplitPairAdapt) ⊤
_ = refl

_ : CategoricalAdapter.isomorphism (A.uSplitPairCategorical uSplitPairAdapt) ⊤ ⊤ ≡ refl
_ = refl
