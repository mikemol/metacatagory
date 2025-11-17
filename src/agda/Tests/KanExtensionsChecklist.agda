-- Tests.KanExtensionsChecklist: Test instances for Kan extensions

module Tests.KanExtensionsChecklist where

open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.Equality using (refl)
open import Tests.ObligationAdapters as A

-- Kan extensions (8 assertions: 7 Kan + 1 adjoint functor theorem)

kanExtensionContextAdapt : A.KanExtensionContextAdapter
kanExtensionContextAdapt = A.mkKanExtensionContextAdapter _ _ _ refl refl

_ : A.isFilledKanExtensionContext kanExtensionContextAdapt ≡ true
_ = refl

leftKanCandidateAdapt : A.LeftKanCandidateAdapter
leftKanCandidateAdapt = A.mkLeftKanCandidateAdapter _ _ refl

_ : A.isFilledLeftKanCandidate leftKanCandidateAdapt ≡ true
_ = refl

rightKanCandidateAdapt : A.RightKanCandidateAdapter
rightKanCandidateAdapt = A.mkRightKanCandidateAdapter _ _ refl

_ : A.isFilledRightKanCandidate rightKanCandidateAdapt ≡ true
_ = refl

leftKanExtensionIsInitialObjectAdapt : A.LeftKanExtensionIsInitialObjectAdapter
leftKanExtensionIsInitialObjectAdapt = A.mkLeftKanExtensionIsInitialObjectAdapter _ _ _ refl refl

_ : A.isFilledLeftKanExtensionIsInitialObject leftKanExtensionIsInitialObjectAdapt ≡ true
_ = refl

rightKanExtensionIsTerminalObjectAdapt : A.RightKanExtensionIsTerminalObjectAdapter
rightKanExtensionIsTerminalObjectAdapt = A.mkRightKanExtensionIsTerminalObjectAdapter _ _ _ refl refl

_ : A.isFilledRightKanExtensionIsTerminalObject rightKanExtensionIsTerminalObjectAdapt ≡ true
_ = refl

pointwiseKanFormulaTheoremAdapt : A.PointwiseKanFormulaTheoremAdapter
pointwiseKanFormulaTheoremAdapt = A.mkPointwiseKanFormulaTheoremAdapter _ _ _ refl refl

_ : A.isFilledPointwiseKanFormulaTheorem pointwiseKanFormulaTheoremAdapt ≡ true
_ = refl

adjointsAsKanExtensionsAdapt : A.AdjointsAsKanExtensionsAdapter
adjointsAsKanExtensionsAdapt = A.mkAdjointsAsKanExtensionsAdapter _ _ _ refl refl

_ : A.isFilledAdjointsAsKanExtensions adjointsAsKanExtensionsAdapt ≡ true
_ = refl

adjointFunctorTheoremRightAdapt : A.AdjointFunctorTheoremRightAdapter
adjointFunctorTheoremRightAdapt = A.mkAdjointFunctorTheoremRightAdapter _

_ : A.isFilledAdjointFunctorTheoremRight adjointFunctorTheoremRightAdapt ≡ true
_ = refl
