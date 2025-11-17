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

------------------------------------------------------------------------
-- Level2sub1
------------------------------------------------------------------------

chk2s1A : S1.AdditivityEquivalenceTheorem
chk2s1A = S1.THEOREM_AdditivityEquivalence (M.mkId "C") true true (M.mkId "iso")

chk2s1B : S1.HomFunctorIsAdditiveTheorem
chk2s1B = S1.THEOREM_HomFunctorIsAdditive (M.mkId "C") (M.mkId "A") (M.mkId "HomCA-") (M.mkId "bilinear")

------------------------------------------------------------------------
-- Level2sub2
------------------------------------------------------------------------

chk2s2A : S2.RegularEpimorphismProperty
chk2s2A = S2._is_REGULAR_EPIMORPHISM (M.mkId "e") (M.mkId "A") (M.mkId "B")
                                      (M.mkId "X") (M.mkId "f") (M.mkId "g") (M.mkId "coeq")
chk2s2B : S2.KernelPairDeclaration
chk2s2B = S2.KernelPair_of (M.mkId "f") (M.mkId "K") (M.mkId "k1") (M.mkId "k2") (M.mkId "pb")

------------------------------------------------------------------------
-- Level2sub3
------------------------------------------------------------------------

lawvere : S3.LawvereTheoryDeclaration
lawvere = S3.LAWVERE_THEORY_WITH_base_object (M.mkId "T") (M.mkId "X") (M.mkId "fin-prod") (M.mkId "arity")

algCat : S3.AlgebraicCategoryDeclaration
algCat = S3._is_ALGEBRAIC_CATEGORY (M.mkId "C") lawvere (M.mkId "equiv")

chk2s3A : S3.AlgebraicCategoriesAreRegularTheorem
chk2s3A = S3.THEOREM_AlgebraicCategoriesAreRegular algCat (M.mkId "regular")

chk2s3B : S3.ExistenceOfFreeFunctorAdjunctionTheorem
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
chk2s4A = S4.THEOREM_ListAlgebrasAreMonoids algData (M.mkId "A") (M.mkId "op") (M.mkId "e") (M.mkId "≅")

chk2s4B : S4.AdjunctionInducesMonadTheorem
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
chk2s6A = record { sourceObject = M.mkId "A" ; targetObject = M.mkId "B" ; homObjectInV = M.mkId "HomAB" }

idMor : C1L.MorphismDeclaration
idMor = C1L.mor (M.mkId "id") (M.mkId "X") (M.mkId "X")

chk2s6B : S6.IdentityMorphismDeclaration_Enriched
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
chk2s8A = record
    { baseCategory = baseCat
    ; fibrationsOver = fibCat
    ; pseudofunctors = ⊤
    ; equivalence = ⊤
    }
