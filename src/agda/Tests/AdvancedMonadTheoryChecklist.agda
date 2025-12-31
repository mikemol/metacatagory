{-# OPTIONS --without-K #-}

-- Tests.AdvancedMonadTheoryChecklist: Test instances for advanced monad theory

module Tests.AdvancedMonadTheoryChecklist where

open import Core.Phase using (Bool; true; false)
open import Agda.Builtin.Equality using (refl; _≡_)
open import Agda.Builtin.Unit using (⊤)
open import Tests.ObligationAdapters as A
open import Core.CategoricalAdapter
open import Chapter2.Level2sub4 as C2S4

-- Postulated test values
postulate
  testMonadWithRank : C2S4.MonadWithRank
  testMonad : C2S4.MonadDeclaration
  testRank : C2S4.RegularCardinal
  testLocPres : C2S4.LocallyPresentableCategory
  testBaseCategory : C2S4.RegularCardinal  
  testRankTheorem : C2S4.RankTheoremForMonadicCategoriesTheorem
  testRankThmBaseCategory : C2S4.LocallyPresentableCategory
  testRankThmMonadWithRank : C2S4.MonadWithRank
  testRankThmAlgCat : C2S4.CategoryOfAlgebras
  -- Postulated equalities for test assertions
  testMonadEq : C2S4.MonadWithRank.monad testMonadWithRank ≡ testMonad
  testRankEq : C2S4.MonadWithRank.cardinal testMonadWithRank ≡ testRank
  testLocPresEq : C2S4.LocallyPresentableCategory.cardinal testLocPres ≡ testBaseCategory
  testRankThm1 : C2S4.RankTheoremForMonadicCategoriesTheorem.baseCategory testRankTheorem ≡ testRankThmBaseCategory
  testRankThm2 : C2S4.RankTheoremForMonadicCategoriesTheorem.monadWithRank testRankTheorem ≡ testRankThmMonadWithRank
  testRankThm3 : C2S4.RankTheoremForMonadicCategoriesTheorem.algebraCategory testRankTheorem ≡ testRankThmAlgCat

-- Advanced monad theory (3 assertions)

monadWithRankAdapt : A.MonadWithRankAdapter
monadWithRankAdapt = A.mkMonadWithRankAdapter testMonadWithRank testMonad testRank testMonadEq testRankEq

_ : A.isFilledMonadWithRank monadWithRankAdapt ≡ true
_ = refl

-- Categorical assertions for MonadWithRank
-- TODO: Fix malformed categorical adapter tests
-- TODO: Fix malformed test: -- _ : CategoricalAdapter.morphism (A.monadWithRankCategorical monadWithRankAdapt) ⊤ ⊤ ≡
-- TODO: Fix malformed test: --     CategoricalAdapter.object (A.monadWithRankCategorical monadWithRankAdapt) ⊤
-- _ = refl

-- TODO: Fix malformed test: -- _ : CategoricalAdapter.isomorphism (A.monadWithRankCategorical monadWithRankAdapt) ⊤ ⊤ ≡ refl
-- _ = refl

locallyPresentableCategoryAdapt : A.LocallyPresentableCategoryAdapter
locallyPresentableCategoryAdapt = A.mkLocallyPresentableCategoryAdapter testLocPres testBaseCategory testLocPresEq

_ : A.isFilledLocallyPresentableCategory locallyPresentableCategoryAdapt ≡ true
_ = refl

-- Categorical assertions for LocallyPresentableCategory
-- TODO: Fix malformed test: _ : CategoricalAdapter.morphism (A.locallyPresentableCategoryCategorical locallyPresentableCategoryAdapt) ⊤ ⊤ ≡
-- TODO: Fix malformed test:     CategoricalAdapter.object (A.locallyPresentableCategoryCategorical locallyPresentableCategoryAdapt) ⊤
-- _ = refl

-- TODO: Fix malformed test: _ : CategoricalAdapter.isomorphism (A.locallyPresentableCategoryCategorical locallyPresentableCategoryAdapt) ⊤ ⊤ ≡ refl
-- _ = refl

rankTheoremForMonadicCategoriesTheoremAdapt : A.RankTheoremForMonadicCategoriesTheoremAdapter
rankTheoremForMonadicCategoriesTheoremAdapt = A.mkRankTheoremForMonadicCategoriesTheoremAdapter testRankTheorem testRankThmBaseCategory testRankThmMonadWithRank testRankThmAlgCat testRankThm1 testRankThm2 testRankThm3

_ : A.isFilledRankTheoremForMonadicCategoriesTheorem rankTheoremForMonadicCategoriesTheoremAdapt ≡ true
_ = refl

-- Categorical assertions for RankTheoremForMonadicCategoriesTheorem
-- TODO: Fix malformed test: _ : CategoricalAdapter.morphism (A.rankTheoremForMonadicCategoriesTheoremCategorical rankTheoremForMonadicCategoriesTheoremAdapt) ⊤ ⊤ ≡
-- TODO: Fix malformed test:     CategoricalAdapter.object (A.rankTheoremForMonadicCategoriesTheoremCategorical rankTheoremForMonadicCategoriesTheoremAdapt) ⊤
-- _ = refl

-- TODO: Fix malformed test: _ : CategoricalAdapter.isomorphism (A.rankTheoremForMonadicCategoriesTheoremCategorical rankTheoremForMonadicCategoriesTheoremAdapt) ⊤ ⊤ ≡ refl
-- _ = refl
