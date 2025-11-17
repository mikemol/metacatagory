-- Tests.AdvancedMonadTheoryChecklist: Test instances for advanced monad theory

module Tests.AdvancedMonadTheoryChecklist where

open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.Equality using (refl)
open import Tests.ObligationAdapters as A

-- Advanced monad theory (3 assertions)

monadWithRankAdapt : A.MonadWithRankAdapter
monadWithRankAdapt = A.mkMonadWithRankAdapter _ _ _ refl refl

_ : A.isFilledMonadWithRank monadWithRankAdapt ≡ true
_ = refl

locallyPresentableCategoryAdapt : A.LocallyPresentableCategoryAdapter
locallyPresentableCategoryAdapt = A.mkLocallyPresentableCategoryAdapter _ _ refl

_ : A.isFilledLocallyPresentableCategory locallyPresentableCategoryAdapt ≡ true
_ = refl

rankTheoremForMonadicCategoriesTheoremAdapt : A.RankTheoremForMonadicCategoriesTheoremAdapter
rankTheoremForMonadicCategoriesTheoremAdapt = A.mkRankTheoremForMonadicCategoriesTheoremAdapter _ _ _ _ refl refl refl

_ : A.isFilledRankTheoremForMonadicCategoriesTheorem rankTheoremForMonadicCategoriesTheoremAdapt ≡ true
_ = refl
