-- Tests/Chapter2Checklist.agda
-- 1–2 trivial inhabitants per Level2subN module to broaden smoke coverage.

module Tests.Chapter2Checklist where

open import Agda.Builtin.Unit using (⊤; tt)
open import Agda.Builtin.Bool using (Bool; true; false)
open import Metamodel as M

-- Submodule imports
import Chapter2.Level2sub1 as S1
import Chapter2.Level2sub2 as S2
import Chapter2.Level2sub3 as S3
import Chapter2.Level2sub4 as S4
import Chapter2.Level2sub5 as S5
import Chapter2.Level2sub6 as S6
import Chapter2.Level2sub7 as S7
import Chapter2.Level2sub8 as S8
import Chapter1.Level1sub3 as C1S3
import Chapter1.Level1sub6 as C1S6
import Chapter1.Level1 as C1L

-- TODO: These are smoke placeholders. Replace each with a constructed witness
--       once concrete examples/bridges land:
--       - Examples/ConstructiveWitnessExamples.agda (algebraic structures)
--       - Examples/AlgorithmCorrectnessExamples.agda (specs/certificates)
--       - Core/AlgorithmCorrectness.agda, Core/ConstructiveWitnesses.agda
--       - Chapter2-specific proof bridges (e.g., algebraic categories, monads)

------------------------------------------------------------------------
-- Level2sub1
------------------------------------------------------------------------

chk2s1A : S1.AdditivityEquivalenceTheorem
-- TODO(Ch2 §2.1): Replace with an actual additive-category instance when ready.
chk2s1A = S1.THEOREM_AdditivityEquivalence (M.mkId "C") true true (M.mkId "iso")

chk2s1B : S1.HomFunctorIsAdditiveTheorem
-- TODO(Ch2 §2.1): Replace with Hom functor built from a concrete category.
chk2s1B = S1.THEOREM_HomFunctorIsAdditive (M.mkId "C") (M.mkId "A") (M.mkId "HomCA-") (M.mkId "bilinear")

------------------------------------------------------------------------
-- Level2sub2
------------------------------------------------------------------------

chk2s2A : S2.RegularEpimorphismProperty
-- TODO(Ch2 §2.2): Use a concrete coequalizer presentation when examples exist.
chk2s2A = S2._is_REGULAR_EPIMORPHISM (M.mkId "e") (M.mkId "A") (M.mkId "B")
                                      (M.mkId "X") (M.mkId "f") (M.mkId "g") (M.mkId "coeq")
chk2s2B : S2.KernelPairDeclaration
-- TODO(Ch2 §2.2): Populate from an explicit pullback example later.
chk2s2B = S2.KernelPair_of (M.mkId "f") (M.mkId "K") (M.mkId "k1") (M.mkId "k2") (M.mkId "pb")

------------------------------------------------------------------------
-- Level2sub3
------------------------------------------------------------------------

lawvere : S3.LawvereTheoryDeclaration
lawvere = S3.LAWVERE_THEORY_WITH_base_object (M.mkId "T") (M.mkId "X") (M.mkId "fin-prod") (M.mkId "arity")

algCat : S3.AlgebraicCategoryDeclaration
algCat = S3._is_ALGEBRAIC_CATEGORY (M.mkId "C") lawvere (M.mkId "equiv")

chk2s3A : S3.AlgebraicCategoriesAreRegularTheorem
-- TODO(Ch2 §2.3): Replace with regularity witness derived from Mod(T,Set).
chk2s3A = S3.THEOREM_AlgebraicCategoriesAreRegular algCat (M.mkId "regular")

chk2s3B : S3.ExistenceOfFreeFunctorAdjunctionTheorem
-- TODO(Ch2 §2.3): Replace with actual F ⊣ U from a Lawvere theory example.
chk2s3B = S3.THEOREM_ExistenceOfFreeFunctorAdjunction algCat
             (S3.F_TO algCat (M.mkId "F")) (S3.U_OF algCat (M.mkId "U")) (M.mkId "F⊣U")

------------------------------------------------------------------------
-- Level2sub4
------------------------------------------------------------------------

monadData : S4.MonadData
monadData = S4.MONAD_DATA (M.mkId "C") (M.mkId "T") (M.mkId "η") (M.mkId "μ")

monadDecl : S4.MonadDeclaration
monadDecl = S4.MONAD_on (M.mkId "T") monadData (S4.AXIOM_MonadAssociativity monadData (M.mkId "assoc"))
                        (S4.AXIOM_MonadUnitality monadData (M.mkId "unit"))

algData : S4.TAlgebraData
algData = S4.T_ALGEBRA_DATA monadDecl (M.mkId "A") (M.mkId "h")

chk2s4A : S4.ListAlgebrasAreMonoidsTheorem
-- TODO(Ch2 §2.4): Replace with (A,h) obtained from a concrete list-algebra.
chk2s4A = S4.THEOREM_ListAlgebrasAreMonoids algData (M.mkId "A") (M.mkId "op") (M.mkId "e") (M.mkId "≅")

chk2s4B : S4.AdjunctionInducesMonadTheorem
-- TODO(Ch2 §2.4): Replace with monad induced by a concrete adjunction example.
chk2s4B = S4.THEOREM_AdjunctionInducesMonad (M.mkId "adj") (M.mkId "F") (M.mkId "G") (M.mkId "η") (M.mkId "ε") monadDecl

------------------------------------------------------------------------
-- Level2sub5 (choose a simple theorem record)
------------------------------------------------------------------------

catDeclC : C1S3.CategoryDeclaration
catDeclC = C1S3.CATEGORY (M.mkId "C")

alpha1 : C1S6.RegularCardinal
alpha1 = C1S6.REGULAR_CARDINAL (M.mkId "ℵ₀")

locallyPresentableC : S5.LocallyPresentableCategoryDeclaration
locallyPresentableC = record
    { category = catDeclC
        ; rank = alpha1
    ; isCocompleteCond = ⊤
    ; generatingSet = ⊤
    ; generatingSetIsSmall = ⊤
    ; generatorsArePresentable = ⊤
    ; generatesUnderColimits = ⊤
    }

chk2s5A : S5.LocallyPresentableAreWellBehavedTheorem
-- TODO(Ch2 §2.5): Replace with LP-category declaration built from small generators.
chk2s5A = record
    { locallyPresentableCategory = locallyPresentableC
    ; isWellPowered = ⊤
    ; isWellCopowered = ⊤
    ; hasStrongEpiMonoFactorization = ⊤
    }

------------------------------------------------------------------------
-- Level2sub6 (enriched category theorems)
------------------------------------------------------------------------

chk2s6A : S6.HomObjectDeclaration
-- TODO(Ch2 §2.6): Build from an enriched category example once available.
chk2s6A = record { sourceObject = M.mkId "A" ; targetObject = M.mkId "B" ; homObjectInV = M.mkId "HomAB" }

idMor : C1L.MorphismDeclaration
idMor = C1L.mor (M.mkId "id") (M.mkId "X") (M.mkId "X")

chk2s6B : S6.IdentityMorphismDeclaration_Enriched
-- TODO(Ch2 §2.6): Replace with identity in the enriching category of the example.
chk2s6B = record { object = M.mkId "X" ; identityMorphismInV = idMor }

------------------------------------------------------------------------
-- Level2sub7 (Topological theorems)
------------------------------------------------------------------------

topCat : S7.TopologicalSpacesCategory
topCat = record
    { underlyingCategory = catDeclC
    ; spaces = ⊤
    ; continuousMaps = ⊤
    }

chk2s7A : S7.TopIsNotCartesianClosedTheorem
-- TODO(Ch2 §2.7): Replace with explicit counterexample space when added.
chk2s7A = record
    { topCategory = topCat
    ; hasProducts = ⊤
    ; counterexample = M.mkId "Q"
    ; counterexampleNotExponentiable = ⊤
    ; conclusion = ⊤
    }

------------------------------------------------------------------------
-- Level2sub8 (Fibrations/cofibrations theorems)
------------------------------------------------------------------------

baseCat : C1S3.CategoryDeclaration
baseCat = C1S3.CATEGORY (M.mkId "B")

fibCat : S8.CategoryOfFibrations
fibCat = record
    { baseCategory = baseCat
    ; fibrations = ⊤
    ; cartesianFunctors = ⊤
    ; categoryStructure = baseCat
    }

chk2s8A : S8.GrothendieckEquivalenceTheorem
-- TODO(Ch2 §2.8): Replace with Grothendieck construction on a concrete fibration.
chk2s8A = record
    { baseCategory = baseCat
    ; fibrationsOver = fibCat
    ; pseudofunctors = ⊤
    ; equivalence = ⊤
    }
