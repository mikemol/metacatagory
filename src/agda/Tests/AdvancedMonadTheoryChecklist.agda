-- Tests.AdvancedMonadTheoryChecklist: Test instances for advanced monad theory

module Tests.AdvancedMonadTheoryChecklist where

open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.Equality using (refl)
open import Agda.Builtin.Unit using (⊤)
open import Tests.ObligationAdapters as A
open import Core.CategoricalAdapter


-- Advanced monad theory (3 assertions)

monadWithRankAdapt : A.MonadWithRankAdapter
monadWithRankAdapt = A.mkMonadWithRankAdapter _ _ _ refl refl

_ : A.isFilledMonadWithRank monadWithRankAdapt ≡ true
_ = refl

-- Categorical assertions for MonadWithRank
_ : CategoricalAdapter.morphism (A.monadWithRankCategorical monadWithRankAdapt) ⊤ ⊤ ≡
    CategoricalAdapter.object (A.monadWithRankCategorical monadWithRankAdapt) ⊤
_ = refl

_ : CategoricalAdapter.isomorphism (A.monadWithRankCategorical monadWithRankAdapt) ⊤ ⊤ ≡ refl
_ = refl

locallyPresentableCategoryAdapt : A.LocallyPresentableCategoryAdapter
locallyPresentableCategoryAdapt = A.mkLocallyPresentableCategoryAdapter _ _ refl

_ : A.isFilledLocallyPresentableCategory locallyPresentableCategoryAdapt ≡ true
_ = refl

-- Categorical assertions for LocallyPresentableCategory
_ : CategoricalAdapter.morphism (A.locallyPresentableCategoryCategorical locallyPresentableCategoryAdapt) ⊤ ⊤ ≡
    CategoricalAdapter.object (A.locallyPresentableCategoryCategorical locallyPresentableCategoryAdapt) ⊤
_ = refl

_ : CategoricalAdapter.isomorphism (A.locallyPresentableCategoryCategorical locallyPresentableCategoryAdapt) ⊤ ⊤ ≡ refl
_ = refl

rankTheoremForMonadicCategoriesTheoremAdapt : A.RankTheoremForMonadicCategoriesTheoremAdapter
rankTheoremForMonadicCategoriesTheoremAdapt = A.mkRankTheoremForMonadicCategoriesTheoremAdapter _ _ _ _ refl refl refl

_ : A.isFilledRankTheoremForMonadicCategoriesTheorem rankTheoremForMonadicCategoriesTheoremAdapt ≡ true
_ = refl

-- Categorical assertions for RankTheoremForMonadicCategoriesTheorem
_ : CategoricalAdapter.morphism (A.rankTheoremForMonadicCategoriesTheoremCategorical rankTheoremForMonadicCategoriesTheoremAdapt) ⊤ ⊤ ≡
    CategoricalAdapter.object (A.rankTheoremForMonadicCategoriesTheoremCategorical rankTheoremForMonadicCategoriesTheoremAdapt) ⊤
_ = refl

_ : CategoricalAdapter.isomorphism (A.rankTheoremForMonadicCategoriesTheoremCategorical rankTheoremForMonadicCategoriesTheoremAdapt) ⊤ ⊤ ≡ refl
_ = refl
