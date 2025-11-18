-- Tests.KanExtensionsChecklist: Test instances for Kan extensions

module Tests.KanExtensionsChecklist where

open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.Equality using (refl)
open import Agda.Builtin.Unit using (⊤)
open import Tests.ObligationAdapters as A
open import Core.CategoricalAdapter


-- Kan extensions (8 assertions: 7 Kan + 1 adjoint functor theorem)

kanExtensionContextAdapt : A.KanExtensionContextAdapter
kanExtensionContextAdapt = A.mkKanExtensionContextAdapter _ _ _ refl refl

_ : A.isFilledKanExtensionContext kanExtensionContextAdapt ≡ true
_ = refl

-- Categorical assertions for KanExtensionContext
_ : CategoricalAdapter.morphism (A.kanExtensionContextCategorical kanExtensionContextAdapt) ⊤ ⊤ ≡
    CategoricalAdapter.object (A.kanExtensionContextCategorical kanExtensionContextAdapt) ⊤
_ = refl

_ : CategoricalAdapter.isomorphism (A.kanExtensionContextCategorical kanExtensionContextAdapt) ⊤ ⊤ ≡ refl
_ = refl

leftKanCandidateAdapt : A.LeftKanCandidateAdapter
leftKanCandidateAdapt = A.mkLeftKanCandidateAdapter _ _ refl

_ : A.isFilledLeftKanCandidate leftKanCandidateAdapt ≡ true
_ = refl

-- Categorical assertions for LeftKanCandidate
_ : CategoricalAdapter.morphism (A.leftKanCandidateCategorical leftKanCandidateAdapt) ⊤ ⊤ ≡
    CategoricalAdapter.object (A.leftKanCandidateCategorical leftKanCandidateAdapt) ⊤
_ = refl

_ : CategoricalAdapter.isomorphism (A.leftKanCandidateCategorical leftKanCandidateAdapt) ⊤ ⊤ ≡ refl
_ = refl

rightKanCandidateAdapt : A.RightKanCandidateAdapter
rightKanCandidateAdapt = A.mkRightKanCandidateAdapter _ _ refl

_ : A.isFilledRightKanCandidate rightKanCandidateAdapt ≡ true
_ = refl

-- Categorical assertions for RightKanCandidate
_ : CategoricalAdapter.morphism (A.rightKanCandidateCategorical rightKanCandidateAdapt) ⊤ ⊤ ≡
    CategoricalAdapter.object (A.rightKanCandidateCategorical rightKanCandidateAdapt) ⊤
_ = refl

_ : CategoricalAdapter.isomorphism (A.rightKanCandidateCategorical rightKanCandidateAdapt) ⊤ ⊤ ≡ refl
_ = refl

leftKanExtensionIsInitialObjectAdapt : A.LeftKanExtensionIsInitialObjectAdapter
leftKanExtensionIsInitialObjectAdapt = A.mkLeftKanExtensionIsInitialObjectAdapter _ _ _ refl refl

_ : A.isFilledLeftKanExtensionIsInitialObject leftKanExtensionIsInitialObjectAdapt ≡ true
_ = refl

-- Categorical assertions for LeftKanExtensionIsInitialObject
_ : CategoricalAdapter.morphism (A.leftKanExtensionIsInitialObjectCategorical leftKanExtensionIsInitialObjectAdapt) ⊤ ⊤ ≡
    CategoricalAdapter.object (A.leftKanExtensionIsInitialObjectCategorical leftKanExtensionIsInitialObjectAdapt) ⊤
_ = refl

_ : CategoricalAdapter.isomorphism (A.leftKanExtensionIsInitialObjectCategorical leftKanExtensionIsInitialObjectAdapt) ⊤ ⊤ ≡ refl
_ = refl

rightKanExtensionIsTerminalObjectAdapt : A.RightKanExtensionIsTerminalObjectAdapter
rightKanExtensionIsTerminalObjectAdapt = A.mkRightKanExtensionIsTerminalObjectAdapter _ _ _ refl refl

_ : A.isFilledRightKanExtensionIsTerminalObject rightKanExtensionIsTerminalObjectAdapt ≡ true
_ = refl

-- Categorical assertions for RightKanExtensionIsTerminalObject
_ : CategoricalAdapter.morphism (A.rightKanExtensionIsTerminalObjectCategorical rightKanExtensionIsTerminalObjectAdapt) ⊤ ⊤ ≡
    CategoricalAdapter.object (A.rightKanExtensionIsTerminalObjectCategorical rightKanExtensionIsTerminalObjectAdapt) ⊤
_ = refl

_ : CategoricalAdapter.isomorphism (A.rightKanExtensionIsTerminalObjectCategorical rightKanExtensionIsTerminalObjectAdapt) ⊤ ⊤ ≡ refl
_ = refl

pointwiseKanFormulaTheoremAdapt : A.PointwiseKanFormulaTheoremAdapter
pointwiseKanFormulaTheoremAdapt = A.mkPointwiseKanFormulaTheoremAdapter _ _ _ refl refl

_ : A.isFilledPointwiseKanFormulaTheorem pointwiseKanFormulaTheoremAdapt ≡ true
_ = refl

-- Categorical assertions for PointwiseKanFormulaTheorem
_ : CategoricalAdapter.morphism (A.pointwiseKanFormulaTheoremCategorical pointwiseKanFormulaTheoremAdapt) ⊤ ⊤ ≡
    CategoricalAdapter.object (A.pointwiseKanFormulaTheoremCategorical pointwiseKanFormulaTheoremAdapt) ⊤
_ = refl

_ : CategoricalAdapter.isomorphism (A.pointwiseKanFormulaTheoremCategorical pointwiseKanFormulaTheoremAdapt) ⊤ ⊤ ≡ refl
_ = refl

adjointsAsKanExtensionsAdapt : A.AdjointsAsKanExtensionsAdapter
adjointsAsKanExtensionsAdapt = A.mkAdjointsAsKanExtensionsAdapter _ _ _ refl refl

_ : A.isFilledAdjointsAsKanExtensions adjointsAsKanExtensionsAdapt ≡ true
_ = refl

-- Categorical assertions for AdjointsAsKanExtensions
_ : CategoricalAdapter.morphism (A.adjointsAsKanExtensionsCategorical adjointsAsKanExtensionsAdapt) ⊤ ⊤ ≡
    CategoricalAdapter.object (A.adjointsAsKanExtensionsCategorical adjointsAsKanExtensionsAdapt) ⊤
_ = refl

_ : CategoricalAdapter.isomorphism (A.adjointsAsKanExtensionsCategorical adjointsAsKanExtensionsAdapt) ⊤ ⊤ ≡ refl
_ = refl

adjointFunctorTheoremRightAdapt : A.AdjointFunctorTheoremRightAdapter
adjointFunctorTheoremRightAdapt = A.mkAdjointFunctorTheoremRightAdapter _

_ : A.isFilledAdjointFunctorTheoremRight adjointFunctorTheoremRightAdapt ≡ true
_ = refl

-- Categorical assertions for AdjointFunctorTheoremRight
_ : CategoricalAdapter.morphism (A.adjointFunctorTheoremRightCategorical adjointFunctorTheoremRightAdapt) ⊤ ⊤ ≡
    CategoricalAdapter.object (A.adjointFunctorTheoremRightCategorical adjointFunctorTheoremRightAdapt) ⊤
_ = refl

_ : CategoricalAdapter.isomorphism (A.adjointFunctorTheoremRightCategorical adjointFunctorTheoremRightAdapt) ⊤ ⊤ ≡ refl
_ = refl
