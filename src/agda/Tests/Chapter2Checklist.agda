-- Tests/Chapter2Checklist.agda
-- 1–2 trivial inhabitants per Level2subN module to broaden smoke coverage.

module Tests.Chapter2Checklist where

open import Agda.Builtin.Unit using (⊤; tt)
open import Core.Phase using (Bool; true; false)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Agda.Builtin.List using (List; []; _∷_)
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
open import Core.CategoricalAdapter

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

-- Additive category declaration
zeroObj : S1.HasZeroObjectProperty
zeroObj = S1._has_a_ZERO_OBJECT (M.mkId "AddCat") (M.mkId "0")

enrichment : S1.EnrichedOverProperty
enrichment = S1._is_ENRICHED_OVER_ (M.mkId "AddCat") (M.mkId "Ab")

addCatDecl : S1.AdditiveCategoryDeclaration
addCatDecl = S1.ADDITIVE_CATEGORY (M.mkId "AddCat") zeroObj enrichment []

-- Adapter for additive category
add-cat-adapter : A.AdditiveCategoryAdapter
add-cat-adapter = A.mkAdditiveCategoryAdapter addCatDecl (M.mkId "AddCat") zeroObj refl refl (λ _ → addCatDecl)

add-cat-status-is-filled : A.isFilledAdditive add-cat-adapter ≡ true
add-cat-status-is-filled = refl
_ : (CategoricalAdapter.morphism (A.additiveCategoryCategorical add-cat-adapter) tt) ≡ A.AdditiveCategoryAdapter.decl add-cat-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.additiveCategoryCategorical add-cat-adapter) ≡ refl
_ = refl

-- Abelian category declaration
abelianDecl : S1.AbelianCategoryDeclaration
abelianDecl = S1.ABELIAN_CATEGORY (M.mkId "Ab") addCatDecl true true true true

-- Adapter for abelian category
ab-cat-adapter : A.AbelianCategoryAdapter
ab-cat-adapter = A.mkAbelianCategoryAdapter abelianDecl (M.mkId "Ab") addCatDecl refl refl (λ _ → abelianDecl)

ab-cat-status-is-filled : A.isFilledAbelian ab-cat-adapter ≡ true
ab-cat-status-is-filled = refl
_ : (CategoricalAdapter.morphism (A.abelianCategoryCategorical ab-cat-adapter) tt) ≡ A.AbelianCategoryAdapter.decl ab-cat-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.abelianCategoryCategorical ab-cat-adapter) ≡ refl
_ = refl

-- Biproduct
biproductDecl : S1.BiproductObject
biproductDecl = record
  { left = M.mkId "A"
  ; right = M.mkId "B"
  ; object = M.mkId "A⊕B"
  ; projectionLeft = M.mkId "π₁"
  ; projectionRight = M.mkId "π₂"
  ; injectionLeft = M.mkId "ι₁"
  ; injectionRight = M.mkId "ι₂"
  }

-- Adapter for biproduct
biprod-adapter : A.BiproductAdapter
biprod-adapter = A.mkBiproductAdapter biproductDecl (M.mkId "A") (M.mkId "B") (M.mkId "A⊕B") refl refl refl (λ _ → biproductDecl)

biprod-status-is-filled : A.isFilledBiproduct biprod-adapter ≡ true
biprod-status-is-filled = refl
_ : (CategoricalAdapter.morphism (A.biproductCategorical biprod-adapter) tt) ≡ A.BiproductAdapter.decl biprod-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.biproductCategorical biprod-adapter) ≡ refl
_ = refl

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
                     reg-fact-link (λ _ → regCatDecl)

reg-fact-status-is-filled : A.isFilledRegularFactorization reg-fact-adapter ≡ true
reg-fact-status-is-filled = refl
_ : (CategoricalAdapter.morphism (A.regularFactorizationCategorical reg-fact-adapter) tt) ≡ A.RegularFactorizationAdapter.decl reg-fact-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.regularFactorizationCategorical reg-fact-adapter) ≡ refl
_ = refl

-- Link KernelPairDeclaration fields to chosen identifiers and assert status
kp-adapter : A.KernelPairAdapter
kp-adapter =
  A.mkKernelPairAdapter chk2s2B (M.mkId "f") (M.mkId "k1") (M.mkId "k2") (M.mkId "pb")
    refl refl refl refl (λ _ → chk2s2B)

kp-status-is-filled : A.isFilledKernelPair kp-adapter ≡ true
kp-status-is-filled = refl
_ : (CategoricalAdapter.morphism (A.kernelPairCategorical kp-adapter) tt) ≡ A.KernelPairAdapter.decl kp-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.kernelPairCategorical kp-adapter) ≡ refl
_ = refl

-- Link internal equivalence relation (r1,r2) and mono-into-product witness
ier-adapter : A.InternalEquivalenceRelationAdapter
ier-adapter =
  A.mkInternalEquivalenceRelationAdapter chk2s2C (M.mkId "r1") (M.mkId "r2") (M.mkId "mono<r1,r2>")
    refl refl refl (λ _ → chk2s2C)

ier-status-is-filled : A.isFilledInternalEquiv ier-adapter ≡ true
ier-status-is-filled = refl
_ : (CategoricalAdapter.morphism (A.internalEquivalenceRelationCategorical ier-adapter) tt) ≡ A.InternalEquivalenceRelationAdapter.decl ier-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.internalEquivalenceRelationCategorical ier-adapter) ≡ refl
_ = refl

-- Link regular exact sequence’s kernel pair morphism and quotient morphism
res-adapter : A.RegularExactSequenceAdapter
res-adapter =
  A.mkRegularExactSequenceAdapter chk2s2D (M.mkId "f") (M.mkId "e")
    refl refl (λ _ → chk2s2D)

res-status-is-filled : A.isFilledRegularExact res-adapter ≡ true
res-status-is-filled = refl
_ : (CategoricalAdapter.morphism (A.regularExactSequenceCategorical res-adapter) tt) ≡ A.RegularExactSequenceAdapter.decl res-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.regularExactSequenceCategorical res-adapter) ≡ refl
_ = refl

------------------------------------------------------------------------
-- Level2sub3
------------------------------------------------------------------------

lawvere : S3.LawvereTheoryDeclaration
lawvere = S3.LAWVERE_THEORY_WITH_base_object (M.mkId "T") (M.mkId "X") (M.mkId "fin-prod") (M.mkId "arity")

-- Adapter for Lawvere theory
lawvere-adapter : A.LawvereTheoryAdapter
lawvere-adapter = A.mkLawvereTheoryAdapter lawvere (M.mkId "T") (M.mkId "X") refl refl (λ _ → lawvere)

lawvere-status-is-filled : A.isFilledLawvereTheory lawvere-adapter ≡ true
lawvere-status-is-filled = refl
_ : (CategoricalAdapter.morphism (A.lawvereTheoryCategorical lawvere-adapter) tt) ≡ A.LawvereTheoryAdapter.decl lawvere-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.lawvereTheoryCategorical lawvere-adapter) ≡ refl
_ = refl

algCat : S3.AlgebraicCategoryDeclaration
algCat = S3._is_ALGEBRAIC_CATEGORY (M.mkId "C") lawvere (M.mkId "equiv")

-- Adapter for algebraic category
alg-cat-adapter : A.AlgebraicCategoryAdapter
alg-cat-adapter = A.mkAlgebraicCategoryAdapter algCat (M.mkId "C") lawvere refl refl (λ _ → algCat)

alg-cat-status-is-filled : A.isFilledAlgebraicCategory alg-cat-adapter ≡ true
alg-cat-status-is-filled = refl
_ : (CategoricalAdapter.morphism (A.algebraicCategoryCategorical alg-cat-adapter) tt) ≡ A.AlgebraicCategoryAdapter.decl alg-cat-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.algebraicCategoryCategorical alg-cat-adapter) ≡ refl
_ = refl

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

-- Adapter for monad
monad-adapter : A.MonadAdapter
monad-adapter = A.mkMonadAdapter monadDecl (M.mkId "T") monadData refl refl

monad-status-is-filled : A.isFilledMonad monad-adapter ≡ true
monad-status-is-filled = refl
_ : (CategoricalAdapter.morphism (A.monadCategorical monad-adapter) tt) ≡ A.MonadAdapter.decl monad-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.monadCategorical monad-adapter) ≡ refl
_ = refl

algData : S4.TAlgebraData
algData = S4.T_ALGEBRA_DATA monadDecl (M.mkId "A") (M.mkId "h")

-- Adapter for T-algebra
talg-adapter : A.TAlgebraAdapter
talg-adapter = A.mkTAlgebraAdapter algData (M.mkId "A") monadDecl refl refl

talg-status-is-filled : A.isFilledTAlgebra talg-adapter ≡ true
talg-status-is-filled = refl
_ : (CategoricalAdapter.morphism (A.talgebraCategorical talg-adapter) tt) ≡ A.TAlgebraAdapter.decl talg-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.talgebraCategorical talg-adapter) ≡ refl
_ = refl

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

-- Adapter for locally presentable category
lp-cat-adapter : A.LocallyPresentableAdapter
lp-cat-adapter = A.mkLocallyPresentableAdapter locallyPresentableC catDeclC alpha1 refl refl

lp-cat-status-is-filled : A.isFilledLocallyPresentable lp-cat-adapter ≡ true
lp-cat-status-is-filled = refl
_ : (CategoricalAdapter.morphism (A.locallyPresentableCategorical lp-cat-adapter) tt) ≡ A.LocallyPresentableAdapter.decl lp-cat-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.locallyPresentableCategorical lp-cat-adapter) ≡ refl
_ = refl

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

hom-status-is-filled : A.isFilledHom hom-adapter ≡ true
hom-status-is-filled = refl
_ : (CategoricalAdapter.morphism (A.homObjectCategorical hom-adapter) tt) ≡ A.HomObjectAdapter.decl hom-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.homObjectCategorical hom-adapter) ≡ refl
_ = refl

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

id-status-is-filled : A.isFilledId id-adapter ≡ true
id-status-is-filled = refl
_ : (CategoricalAdapter.morphism (A.idEnrichedCategorical id-adapter) tt) ≡ A.IdEnrichedAdapter.decl id-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.idEnrichedCategorical id-adapter) ≡ refl
_ = refl

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

------------------------------------------------------------------------
-- Additional adapter instances for comprehensive Chapter 2 coverage
------------------------------------------------------------------------

-- Level 2.1 additional adapters
sesDecl : S1.ShortExactSequenceDeclaration
sesDecl = S1.SHORT_EXACT_SEQUENCE (M.mkId "0") (M.mkId "A") (M.mkId "B") (M.mkId "C")
            (M.mkId "f") (M.mkId "g") true true true

ses-adapter : A.ShortExactSequenceAdapter
ses-adapter = A.mkShortExactSequenceAdapter sesDecl (M.mkId "A") (M.mkId "B") (M.mkId "C") refl refl refl

ses-status-is-filled : A.isFilledShortExactSequence ses-adapter ≡ true
ses-status-is-filled = refl
_ : (CategoricalAdapter.morphism (A.shortExactSequenceCategorical ses-adapter) tt) ≡ A.ShortExactSequenceAdapter.decl ses-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.shortExactSequenceCategorical ses-adapter) ≡ refl
_ = refl

zeroMorphDecl : S1.ZeroMorphismDeclaration
zeroMorphDecl = S1.zero_mor (M.mkId "A") (M.mkId "B") (M.mkId "0") (M.mkId "f") (M.mkId "g")

zero-morph-adapter : A.ZeroMorphismAdapter
zero-morph-adapter = A.mkZeroMorphismAdapter zeroMorphDecl (M.mkId "A") (M.mkId "B") (M.mkId "0") refl refl refl

zero-morph-status-is-filled : A.isFilledZeroMorphism zero-morph-adapter ≡ true
zero-morph-status-is-filled = refl
_ : (CategoricalAdapter.morphism (A.zeroMorphismCategorical zero-morph-adapter) tt) ≡ A.ZeroMorphismAdapter.decl zero-morph-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.zeroMorphismCategorical zero-morph-adapter) ≡ refl
_ = refl

torsionTheoryDecl : S1.TorsionTheoryDeclaration
torsionTheoryDecl = S1.TORSION_THEORY (M.mkId "C") (M.mkId "T") (M.mkId "F")
  (S1._⊥_ (M.mkId "T") (M.mkId "F") (M.mkId "orth"))
  [] (M.mkId "quot-closed") (M.mkId "subobj-closed") (M.mkId "corefl") (M.mkId "refl")

torsion-adapter : A.TorsionTheoryAdapter
torsion-adapter = A.mkTorsionTheoryAdapter torsionTheoryDecl (M.mkId "C") (M.mkId "T") (M.mkId "F") refl refl refl

torsion-status-is-filled : A.isFilledTorsionTheory torsion-adapter ≡ true
torsion-status-is-filled = refl
_ : (CategoricalAdapter.morphism (A.torsionTheoryCategorical torsion-adapter) tt) ≡ A.TorsionTheoryAdapter.decl torsion-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.torsionTheoryCategorical torsion-adapter) ≡ refl
_ = refl

-- Level 2.3 additional adapters
bialgebraDecl : S3.BialgebraDeclaration
bialgebraDecl = S3._is_T1_T2_Bialgebra
  (S3.LAWVERE_THEORY_WITH_base_object (M.mkId "T1") (M.mkId "X1") (M.mkId "fp1") (M.mkId "arity1"))
  (S3.LAWVERE_THEORY_WITH_base_object (M.mkId "T2") (M.mkId "X2") (M.mkId "fp2") (M.mkId "arity2"))
  (M.mkId "S") (M.mkId "M1") (M.mkId "M2") (M.mkId "compat")

bialgebra-adapter : A.BialgebraAdapter
bialgebra-adapter = A.mkBialgebraAdapter bialgebraDecl (M.mkId "S") (M.mkId "M1") (M.mkId "M2") refl refl refl

bialgebra-status-is-filled : A.isFilledBialgebra bialgebra-adapter ≡ true
bialgebra-status-is-filled = refl
_ : (CategoricalAdapter.morphism (A.bialgebraCategorical bialgebra-adapter) tt) ≡ A.BialgebraAdapter.decl bialgebra-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.bialgebraCategorical bialgebra-adapter) ≡ refl
_ = refl

-- Level 2.4 additional adapters
comonadData : S4.ComonadData
comonadData = S4.COMONAD_DATA (M.mkId "C") (M.mkId "G") (M.mkId "ε") (M.mkId "δ")

comonadAxioms : S4.ComonadAxioms
comonadAxioms = S4.COMONAD_AXIOMS (M.mkId "coassoc") (M.mkId "counit")

comonadDecl : S4.ComonadDeclaration
comonadDecl = S4.COMONAD_on (M.mkId "G") comonadData comonadAxioms

comonad-adapter : A.ComonadAdapter
comonad-adapter = A.mkComonadAdapter comonadDecl (M.mkId "G") comonadData refl refl

comonad-status-is-filled : A.isFilledComonad comonad-adapter ≡ true
comonad-status-is-filled = refl
_ : (CategoricalAdapter.morphism (A.comonadCategorical comonad-adapter) tt) ≡ A.ComonadAdapter.decl comonad-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.comonadCategorical comonad-adapter) ≡ refl
_ = refl

-- Level 2.5 additional adapters
accessibleCatDecl : S5.AccessibleCategoryDeclaration
accessibleCatDecl = record
  { category = C1S3.CATEGORY (M.mkId "C")
  ; rank = C1S6.REGULAR_CARDINAL (M.mkId "ℵ₀")
  ; hasFilteredColimits = ⊤
  ; generatingSet = ⊤
  ; generatingSetIsSmall = ⊤
  ; generatorsArePresentable = ⊤
  ; generatesViaFilteredColimits = ⊤
  }

accessible-adapter : A.AccessibleCategoryAdapter
accessible-adapter = A.mkAccessibleCategoryAdapter accessibleCatDecl
  (C1S3.CATEGORY (M.mkId "C")) (C1S6.REGULAR_CARDINAL (M.mkId "ℵ₀")) refl refl

accessible-status-is-filled : A.isFilledAccessibleCategory accessible-adapter ≡ true
accessible-status-is-filled = refl
_ : (CategoricalAdapter.morphism (A.accessibleCategoryCategorical accessible-adapter) tt) ≡ A.AccessibleCategoryAdapter.decl accessible-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.accessibleCategoryCategorical accessible-adapter) ≡ refl
_ = refl

sketchDecl : S5.SketchDeclaration
sketchDecl = record
  { underlyingCategory = C1S3.CATEGORY (M.mkId "S")
  ; categoryIsSmall = ⊤
  ; limitCones = ⊤
  ; colimitCocones = ⊤
  }

sketch-adapter : A.SketchAdapter
sketch-adapter = A.mkSketchAdapter sketchDecl (C1S3.CATEGORY (M.mkId "S")) refl

sketch-status-is-filled : A.isFilledSketch sketch-adapter ≡ true
sketch-status-is-filled = refl
_ : (CategoricalAdapter.morphism (A.sketchCategorical sketch-adapter) tt) ≡ A.SketchAdapter.decl sketch-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.sketchCategorical sketch-adapter) ≡ refl
_ = refl

-- Level 2.6 adapters
monoidalData : S6.MonoidalCategoryData
monoidalData = record
  { underlyingCategory = C1S3.CATEGORY (M.mkId "M")
  ; tensorProduct = ⊤
  ; unitObject = M.mkId "I"
  }

associatorDecl : S6.AssociatorDeclaration
associatorDecl = record
  { monoidalCategory = monoidalData
  ; naturalIsomorphism = ⊤
  }

leftUnitorDecl : S6.LeftUnitorDeclaration
leftUnitorDecl = record
  { monoidalCategory = monoidalData
  ; naturalIsomorphism = ⊤
  }

rightUnitorDecl : S6.RightUnitorDeclaration
rightUnitorDecl = record
  { monoidalCategory = monoidalData
  ; naturalIsomorphism = ⊤
  }

pentagonAxiom : S6.PentagonAxiom
pentagonAxiom = record
  { monoidalCategory = monoidalData
  ; associator = associatorDecl
  ; diagramCommutes = ⊤
  }

triangleAxiom : S6.TriangleAxiom
triangleAxiom = record
  { monoidalCategory = monoidalData
  ; associator = associatorDecl
  ; leftUnitor = leftUnitorDecl
  ; rightUnitor = rightUnitorDecl
  ; diagramCommutes = ⊤
  }

monoidalDecl : S6.MonoidalCategoryDeclaration
monoidalDecl = record
  { datum = monoidalData
  ; associator = associatorDecl
  ; leftUnitor = leftUnitorDecl
  ; rightUnitor = rightUnitorDecl
  ; pentagonAxiom = pentagonAxiom
  ; triangleAxiom = triangleAxiom
  }

monoidal-adapter : A.MonoidalCategoryAdapter
monoidal-adapter = A.mkMonoidalCategoryAdapter monoidalDecl monoidalData associatorDecl refl refl

monoidal-status-is-filled : A.isFilledMonoidal monoidal-adapter ≡ true
monoidal-status-is-filled = refl
_ : (CategoricalAdapter.morphism (A.monoidalCategoryCategorical monoidal-adapter) tt) ≡ A.MonoidalCategoryAdapter.decl monoidal-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.monoidalCategoryCategorical monoidal-adapter) ≡ refl
_ = refl

braidingDecl : S6.BraidingDeclaration
braidingDecl = record
  { monoidalCategory = monoidalData
  ; naturalIsomorphism = ⊤
  ; symmetryCondition = ⊤
  }

hexagonAxiom : S6.HexagonAxiom
hexagonAxiom = record
  { monoidalCategory = monoidalData
  ; associator = associatorDecl
  ; braiding = braidingDecl
  ; diagramCommutes = ⊤
  }

symMonoidalDecl : S6.SymmetricMonoidalCategoryDeclaration
symMonoidalDecl = record
  { monoidalCategory = monoidalDecl
  ; braiding = braidingDecl
  ; hexagonAxiom = hexagonAxiom
  }

sym-monoidal-adapter : A.SymmetricMonoidalAdapter
sym-monoidal-adapter = A.mkSymmetricMonoidalAdapter symMonoidalDecl monoidalDecl braidingDecl refl refl

sym-monoidal-status-is-filled : A.isFilledSymmetricMonoidal sym-monoidal-adapter ≡ true
sym-monoidal-status-is-filled = refl
_ : (CategoricalAdapter.morphism (A.symmetricMonoidalCategoryCategorical sym-monoidal-adapter) tt) ≡ A.SymmetricMonoidalAdapter.decl sym-monoidal-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.symmetricMonoidalCategoryCategorical sym-monoidal-adapter) ≡ refl
_ = refl

internalHomDecl : S6.InternalHomObjectDeclaration
internalHomDecl = record
  { category = C1S3.CATEGORY (M.mkId "C")
  ; sourceObject = M.mkId "A"
  ; targetObject = M.mkId "B"
  ; internalHomObject = M.mkId "[A,B]"
  }

internal-hom-adapter : A.InternalHomAdapter
internal-hom-adapter = A.mkInternalHomAdapter internalHomDecl
  (C1S3.CATEGORY (M.mkId "C")) (M.mkId "A") (M.mkId "B") refl refl refl

internal-hom-status-is-filled : A.isFilledInternalHom internal-hom-adapter ≡ true
internal-hom-status-is-filled = refl
_ : (CategoricalAdapter.morphism (A.internalHomCategorical internal-hom-adapter) tt) ≡ A.InternalHomAdapter.decl internal-hom-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.internalHomCategorical internal-hom-adapter) ≡ refl
_ = refl
_ : (CategoricalAdapter.morphism (A.internalHomCategorical internal-hom-adapter) tt) ≡ A.InternalHomAdapter.decl internal-hom-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.internalHomCategorical internal-hom-adapter) ≡ refl
_ = refl

-- Level 2.7 adapters
topCatCGWH : S7.TopologicalSpacesCategory
topCatCGWH = record
  { underlyingCategory = C1S3.CATEGORY (M.mkId "Top")
  ; spaces = ⊤
  ; continuousMaps = ⊤
  }

cgwhDecl : S7.CGWH_CategoryDeclaration
cgwhDecl = record
  { topCategory = topCatCGWH
  ; underlyingCategory = C1S3.CATEGORY (M.mkId "CGWH")
  ; objectsAreCompactlyGenerated = ⊤
  ; objectsAreWeakHausdorff = ⊤
  }

cgwh-adapter : A.CGWH_CategoryAdapter
cgwh-adapter = A.mkCGWH_CategoryAdapter cgwhDecl topCatCGWH (C1S3.CATEGORY (M.mkId "CGWH")) refl refl

cgwh-status-is-filled : A.isFilledCGWH cgwh-adapter ≡ true
cgwh-status-is-filled = refl
_ : (CategoricalAdapter.morphism (A.cgwhCategoryCategorical cgwh-adapter) tt) ≡ A.CGWH_CategoryAdapter.decl cgwh-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.cgwhCategoryCategorical cgwh-adapter) ≡ refl
_ = refl
_ : (CategoricalAdapter.morphism (A.cgwhCategoryCategorical cgwh-adapter) tt) ≡ A.CGWH_CategoryAdapter.decl cgwh-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.cgwhCategoryCategorical cgwh-adapter) ≡ refl
_ = refl

topFunctorProp : S7.TopologicalFunctorProperty
topFunctorProp = record
  { functor = M.mkId "U"
  ; admitsInitialLifts = λ sink → record
      { sink = sink
      ; liftedObject = M.mkId "X"
      ; liftedMorphisms = ⊤
      ; preservesTargetObject = ⊤
      ; preservesSourceMaps = ⊤
      ; universalProperty = ⊤
      }
  ; admitsFinalLifts = λ source → record
      { source = source
      ; liftedObject = M.mkId "Y"
      ; liftedMorphisms = ⊤
      ; preservesSourceObject = ⊤
      ; preservesTargetMaps = ⊤
      ; universalProperty = ⊤
      }
  }

top-functor-adapter : A.TopologicalFunctorAdapter
top-functor-adapter = A.mkTopologicalFunctorAdapter topFunctorProp (M.mkId "U") refl

top-functor-status-is-filled : A.isFilledTopologicalFunctor top-functor-adapter ≡ true
top-functor-status-is-filled = refl
_ : (CategoricalAdapter.morphism (A.topologicalFunctorCategorical top-functor-adapter) tt) ≡ A.TopologicalFunctorAdapter.decl top-functor-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.topologicalFunctorCategorical top-functor-adapter) ≡ refl
_ = refl
_ : (CategoricalAdapter.morphism (A.topologicalFunctorCategorical top-functor-adapter) tt) ≡ A.TopologicalFunctorAdapter.decl top-functor-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.topologicalFunctorCategorical top-functor-adapter) ≡ refl
_ = refl

-- Level 2.8 adapters
totalCat : C1S3.CategoryDeclaration
totalCat = C1S3.CATEGORY (M.mkId "E")

fibProjection : S8.FibrationProjectionFunctor
fibProjection = record
  { totalCategory = totalCat
  ; baseCategory = baseCat
  ; projectionFunctor = M.mkId "p"
  }

fibrationDecl : S8.FibrationDeclaration
fibrationDecl = record
  { projectionFunctor = fibProjection
  ; cartesianLiftsExist = ⊤
  }

fibration-adapter : A.FibrationAdapter
fibration-adapter = A.mkFibrationAdapter fibrationDecl fibProjection refl (λ _ → fibrationDecl)

fibration-status-is-filled : A.isFilledFibration fibration-adapter ≡ true
fibration-status-is-filled = refl
_ : (CategoricalAdapter.morphism (A.fibrationCategorical fibration-adapter) tt) ≡ A.FibrationAdapter.decl fibration-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.fibrationCategorical fibration-adapter) ≡ refl
_ = refl
_ : (CategoricalAdapter.morphism (A.fibrationCategorical fibration-adapter) tt) ≡ A.FibrationAdapter.decl fibration-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.fibrationCategorical fibration-adapter) ≡ refl
_ = refl

opfibrationDecl : S8.OpfibrationDeclaration
opfibrationDecl = record
  { projectionFunctor = fibProjection
  ; coCartesianLiftsExist = ⊤
  }

opfibration-adapter : A.OpfibrationAdapter
opfibration-adapter = A.mkOpfibrationAdapter opfibrationDecl fibProjection refl (λ _ → opfibrationDecl)

opfibration-status-is-filled : A.isFilledOpfibration opfibration-adapter ≡ true
opfibration-status-is-filled = refl
_ : (CategoricalAdapter.morphism (A.opfibrationCategorical opfibration-adapter) tt) ≡ A.OpfibrationAdapter.decl opfibration-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.opfibrationCategorical opfibration-adapter) ≡ refl
_ = refl
_ : (CategoricalAdapter.morphism (A.opfibrationCategorical opfibration-adapter) tt) ≡ A.OpfibrationAdapter.decl opfibration-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.opfibrationCategorical opfibration-adapter) ≡ refl
_ = refl
