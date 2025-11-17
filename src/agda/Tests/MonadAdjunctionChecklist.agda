-- Tests.MonadAdjunctionChecklist: Test instances for monad-adjunction theory

module Tests.MonadAdjunctionChecklist where

open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.Equality using (refl)
open import Tests.ObligationAdapters as A

-- Monad-adjunction correspondence (7 assertions)

categoryOfAlgebrasAdapt : A.CategoryOfAlgebrasAdapter
categoryOfAlgebrasAdapt = A.mkCategoryOfAlgebrasAdapter _ _ refl

_ : A.isFilledCategoryOfAlgebras categoryOfAlgebrasAdapt ≡ true
_ = refl

adjunctionInducesMonadTheoremAdapt : A.AdjunctionInducesMonadTheoremAdapter
adjunctionInducesMonadTheoremAdapt = A.mkAdjunctionInducesMonadTheoremAdapter _ _ refl

_ : A.isFilledAdjunctionInducesMonadTheorem adjunctionInducesMonadTheoremAdapt ≡ true
_ = refl

eilenbergMooreAdjunctionAdapt : A.EilenbergMooreAdjunctionAdapter
eilenbergMooreAdjunctionAdapt = A.mkEilenbergMooreAdjunctionAdapter _ _ _ refl refl

_ : A.isFilledEilenbergMooreAdjunction eilenbergMooreAdjunctionAdapt ≡ true
_ = refl

monadAdjunctionCorrespondenceTheoremAdapt : A.MonadAdjunctionCorrespondenceTheoremAdapter
monadAdjunctionCorrespondenceTheoremAdapt = A.mkMonadAdjunctionCorrespondenceTheoremAdapter _ _ _ refl refl

_ : A.isFilledMonadAdjunctionCorrespondenceTheorem monadAdjunctionCorrespondenceTheoremAdapt ≡ true
_ = refl

beckMonadicityTheoremAdapt : A.BeckMonadicityTheoremAdapter
beckMonadicityTheoremAdapt = A.mkBeckMonadicityTheoremAdapter _ _ refl

_ : A.isFilledBeckMonadicityTheorem beckMonadicityTheoremAdapt ≡ true
_ = refl

monadicFunctorPropertyAdapt : A.MonadicFunctorPropertyAdapter
monadicFunctorPropertyAdapt = A.mkMonadicFunctorPropertyAdapter _ _ refl

_ : A.isFilledMonadicFunctorProperty monadicFunctorPropertyAdapt ≡ true
_ = refl

comonadFromAdjunctionAdapt : A.ComonadFromAdjunctionAdapter
comonadFromAdjunctionAdapt = A.mkComonadFromAdjunctionAdapter _ _ refl

_ : A.isFilledComonadFromAdjunction comonadFromAdjunctionAdapt ≡ true
_ = refl
