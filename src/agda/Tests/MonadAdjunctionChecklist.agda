-- Tests.MonadAdjunctionChecklist: Test instances for monad-adjunction theory

module Tests.MonadAdjunctionChecklist where

open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.Equality using (refl)
open import Agda.Builtin.Unit using (⊤)
open import Tests.ObligationAdapters as A
open import Core.CategoricalAdapter


-- Monad-adjunction correspondence (7 assertions)

categoryOfAlgebrasAdapt : A.CategoryOfAlgebrasAdapter
categoryOfAlgebrasAdapt = A.mkCategoryOfAlgebrasAdapter _ _ refl

_ : A.isFilledCategoryOfAlgebras categoryOfAlgebrasAdapt ≡ true
_ = refl

-- Categorical assertions for CategoryOfAlgebras
_ : CategoricalAdapter.morphism (A.categoryOfAlgebrasCategorical categoryOfAlgebrasAdapt) ⊤ ⊤ ≡
    CategoricalAdapter.object (A.categoryOfAlgebrasCategorical categoryOfAlgebrasAdapt) ⊤
_ = refl

_ : CategoricalAdapter.isomorphism (A.categoryOfAlgebrasCategorical categoryOfAlgebrasAdapt) ⊤ ⊤ ≡ refl
_ = refl

adjunctionInducesMonadTheoremAdapt : A.AdjunctionInducesMonadTheoremAdapter
adjunctionInducesMonadTheoremAdapt = A.mkAdjunctionInducesMonadTheoremAdapter _ _ refl

_ : A.isFilledAdjunctionInducesMonadTheorem adjunctionInducesMonadTheoremAdapt ≡ true
_ = refl

-- Categorical assertions for AdjunctionInducesMonadTheorem
_ : CategoricalAdapter.morphism (A.adjunctionInducesMonadTheoremCategorical adjunctionInducesMonadTheoremAdapt) ⊤ ⊤ ≡
    CategoricalAdapter.object (A.adjunctionInducesMonadTheoremCategorical adjunctionInducesMonadTheoremAdapt) ⊤
_ = refl

_ : CategoricalAdapter.isomorphism (A.adjunctionInducesMonadTheoremCategorical adjunctionInducesMonadTheoremAdapt) ⊤ ⊤ ≡ refl
_ = refl

eilenbergMooreAdjunctionAdapt : A.EilenbergMooreAdjunctionAdapter
eilenbergMooreAdjunctionAdapt = A.mkEilenbergMooreAdjunctionAdapter _ _ _ refl refl

_ : A.isFilledEilenbergMooreAdjunction eilenbergMooreAdjunctionAdapt ≡ true
_ = refl

-- Categorical assertions for EilenbergMooreAdjunction
_ : CategoricalAdapter.morphism (A.eilenbergMooreAdjunctionCategorical eilenbergMooreAdjunctionAdapt) ⊤ ⊤ ≡
    CategoricalAdapter.object (A.eilenbergMooreAdjunctionCategorical eilenbergMooreAdjunctionAdapt) ⊤
_ = refl

_ : CategoricalAdapter.isomorphism (A.eilenbergMooreAdjunctionCategorical eilenbergMooreAdjunctionAdapt) ⊤ ⊤ ≡ refl
_ = refl

monadAdjunctionCorrespondenceTheoremAdapt : A.MonadAdjunctionCorrespondenceTheoremAdapter
monadAdjunctionCorrespondenceTheoremAdapt = A.mkMonadAdjunctionCorrespondenceTheoremAdapter _ _ _ refl refl

_ : A.isFilledMonadAdjunctionCorrespondenceTheorem monadAdjunctionCorrespondenceTheoremAdapt ≡ true
_ = refl

-- Categorical assertions for MonadAdjunctionCorrespondenceTheorem
_ : CategoricalAdapter.morphism (A.monadAdjunctionCorrespondenceTheoremCategorical monadAdjunctionCorrespondenceTheoremAdapt) ⊤ ⊤ ≡
    CategoricalAdapter.object (A.monadAdjunctionCorrespondenceTheoremCategorical monadAdjunctionCorrespondenceTheoremAdapt) ⊤
_ = refl

_ : CategoricalAdapter.isomorphism (A.monadAdjunctionCorrespondenceTheoremCategorical monadAdjunctionCorrespondenceTheoremAdapt) ⊤ ⊤ ≡ refl
_ = refl

beckMonadicityTheoremAdapt : A.BeckMonadicityTheoremAdapter
beckMonadicityTheoremAdapt = A.mkBeckMonadicityTheoremAdapter _ _ refl

_ : A.isFilledBeckMonadicityTheorem beckMonadicityTheoremAdapt ≡ true
_ = refl

-- Categorical assertions for BeckMonadicityTheorem
_ : CategoricalAdapter.morphism (A.beckMonadicityTheoremCategorical beckMonadicityTheoremAdapt) ⊤ ⊤ ≡
    CategoricalAdapter.object (A.beckMonadicityTheoremCategorical beckMonadicityTheoremAdapt) ⊤
_ = refl

_ : CategoricalAdapter.isomorphism (A.beckMonadicityTheoremCategorical beckMonadicityTheoremAdapt) ⊤ ⊤ ≡ refl
_ = refl

monadicFunctorPropertyAdapt : A.MonadicFunctorPropertyAdapter
monadicFunctorPropertyAdapt = A.mkMonadicFunctorPropertyAdapter _ _ refl

_ : A.isFilledMonadicFunctorProperty monadicFunctorPropertyAdapt ≡ true
_ = refl

-- Categorical assertions for MonadicFunctorProperty
_ : CategoricalAdapter.morphism (A.monadicFunctorPropertyCategorical monadicFunctorPropertyAdapt) ⊤ ⊤ ≡
    CategoricalAdapter.object (A.monadicFunctorPropertyCategorical monadicFunctorPropertyAdapt) ⊤
_ = refl

_ : CategoricalAdapter.isomorphism (A.monadicFunctorPropertyCategorical monadicFunctorPropertyAdapt) ⊤ ⊤ ≡ refl
_ = refl

comonadFromAdjunctionAdapt : A.ComonadFromAdjunctionAdapter
comonadFromAdjunctionAdapt = A.mkComonadFromAdjunctionAdapter _ _ refl

_ : A.isFilledComonadFromAdjunction comonadFromAdjunctionAdapt ≡ true
_ = refl

-- Categorical assertions for ComonadFromAdjunction
_ : CategoricalAdapter.morphism (A.comonadFromAdjunctionCategorical comonadFromAdjunctionAdapt) ⊤ ⊤ ≡
    CategoricalAdapter.object (A.comonadFromAdjunctionCategorical comonadFromAdjunctionAdapt) ⊤
_ = refl

_ : CategoricalAdapter.isomorphism (A.comonadFromAdjunctionCategorical comonadFromAdjunctionAdapt) ⊤ ⊤ ≡ refl
_ = refl
