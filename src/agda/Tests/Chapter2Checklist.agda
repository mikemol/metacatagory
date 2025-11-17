-- Tests/Chapter2Checklist.agda
-- 1–2 trivial inhabitants per Level2subN module to broaden smoke coverage.

module Tests.Chapter2Checklist where

open import Agda.Builtin.Unit using (⊤; tt)
import Agda.Builtin.Bool as B
open B using () renaming (Bool to Boolean; true to True; false to False)
open import Agda.Builtin.Equality using (_≡_; refl)
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
import Tests.ObligationAdapters as A

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
chk2s1A = S1.THEOREM_AdditivityEquivalence (M.mkId "C") B.true B.true (M.mkId "iso")

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

chk2s2C : S2.InternalEquivalenceRelationDeclaration
-- TODO(Ch2 §2.3): Internal equivalence relation induced by (r1,r2) on A.
chk2s2C = S2.INTERNAL_EQUIV_RELATION_on (M.mkId "R") (M.mkId "A") (M.mkId "r1") (M.mkId "r2")
                                         (M.mkId "mono<r1,r2>") (M.mkId "refl") (M.mkId "sym") (M.mkId "trans")

chk2s2D : S2.RegularExactSequenceDeclaration
-- TODO(Ch2 §2.3): Regular exact sequence K --(k1,k2)--> A --e--> Q.
chk2s2D = S2.REGULAR_EXACT_SEQUENCE chk2s2B chk2s2A (M.mkId "compat")

-- Regular category declaration tying (RegEpi,Mono) factorization to a coequalizer witness
finLim : S2.FiniteLimitsProperty
finLim = S2._has_FINITE_LIMITS (M.mkId "C") (M.mkId "term") (M.mkId "pullbacks")

stab : S2.StabilityUnderPullbackProperty
stab = S2._is_STABLE_UNDER_PULLBACK (M.mkId "RegEpi") (M.mkId "stable")

regCatDecl : S2.RegularCategoryDeclaration
regCatDecl = S2.REGULAR_CATEGORY (M.mkId "C") finLim (M.mkId "coeq") stab

-- Adapter linking factorization witness to the coequalizer used above
reg-fact-link : S2.RegularCategoryDeclaration.regularEpiMonoFactorizationWitness regCatDecl ≡
                S2.RegularEpimorphismProperty.coequalizerWitness chk2s2A
reg-fact-link = refl

reg-fact-adapter : A.RegularFactorizationAdapter
reg-fact-adapter = A.mkRegularFactorizationAdapter regCatDecl
                     (S2.RegularEpimorphismProperty.coequalizerWitness chk2s2A)
                     reg-fact-link

reg-fact-status-is-filled : A.isFilledRegularFactorization reg-fact-adapter ≡ B.true
reg-fact-status-is-filled = refl

-- Link KernelPairDeclaration fields to chosen identifiers and assert status
kp-adapter : A.KernelPairAdapter
kp-adapter =
    A.mkKernelPairAdapter chk2s2B (M.mkId "f") (M.mkId "k1") (M.mkId "k2") (M.mkId "pb")
        refl refl refl refl

kp-status-is-filled : A.isFilledKernelPair kp-adapter ≡ B.true
kp-status-is-filled = refl

-- Link internal equivalence relation (r1,r2) and mono-into-product witness
ier-adapter : A.InternalEquivalenceRelationAdapter
ier-adapter =
    A.mkInternalEquivalenceRelationAdapter chk2s2C (M.mkId "r1") (M.mkId "r2") (M.mkId "mono<r1,r2>")
        refl refl refl

ier-status-is-filled : A.isFilledInternalEquiv ier-adapter ≡ B.true
ier-status-is-filled = refl

-- Link regular exact sequence’s kernel pair morphism and quotient morphism
res-adapter : A.RegularExactSequenceAdapter
res-adapter =
    A.mkRegularExactSequenceAdapter chk2s2D (M.mkId "f") (M.mkId "e")
        refl refl

res-status-is-filled : A.isFilledRegularExact res-adapter ≡ B.true
res-status-is-filled = refl

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

-- Link assertion and standardized status via adapter
hom-link : S6.HomObjectDeclaration.homObjectInV chk2s6A ≡ M.mkId "HomAB"
hom-link = refl

hom-adapter : A.HomObjectAdapter
hom-adapter = A.mkHomObjectAdapter chk2s6A (M.mkId "HomAB") hom-link

hom-status-is-filled : A.isFilledHom hom-adapter ≡ B.true
hom-status-is-filled = refl

idMor : C1L.MorphismDeclaration
idMor = C1L.mor (M.mkId "id") (M.mkId "X") (M.mkId "X")

chk2s6B : S6.IdentityMorphismDeclaration_Enriched
-- TODO(Ch2 §2.6): Replace with identity in the enriching category of the example.
chk2s6B = record { object = M.mkId "X" ; identityMorphismInV = idMor }

-- Link assertion and standardized status via adapter
id-link : S6.IdentityMorphismDeclaration_Enriched.identityMorphismInV chk2s6B ≡ idMor
id-link = refl

id-adapter : A.IdEnrichedAdapter
id-adapter = A.mkIdEnrichedAdapter chk2s6B idMor id-link

id-status-is-filled : A.isFilledId id-adapter ≡ B.true
id-status-is-filled = refl

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
