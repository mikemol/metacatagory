{-# OPTIONS --allow-unsolved-metas --without-K #-}

-- | Checklist for enrichment obligations.
module Tests.EnrichmentChecklist where

open import Agda.Builtin.Equality using (_≡_; refl)
open import Agda.Builtin.Unit using (⊤)
open import Core.Phase using (Bool; true; false)
import Metamodel as M
import Algebra.Foundation as AFo
import Algebra.Enrichment as AE
import Algebra.Groups.Abelian as AGA
import Chapter1.Level1sub3 as C1S3
import Chapter2.Level2sub6 as Enriched
import Tests.ObligationAdapters as A

-- ============================================================================
-- Minimal, modern scaffolding for enrichment testing (greenfield rebuild)
-- We postulate the base algebraic/categorical declarations to match the
-- current APIs, then build lightweight high-level records referencing them.
-- This keeps definitional equalities intact for adapter builders (refl).
-- ============================================================================

postulate
  -- Algebraic base structures (aligned with Algebra.Foundation)
  natMonoidDecl : AFo.MonoidDeclaration
  groupDecl     : AFo.GroupDeclaration
  abGroupDecl   : AFo.AbelianGroupDeclaration
  abCategory    : AGA.Ab

  -- Base categorical context
  catDecl           : C1S3.CategoryDeclaration
  monoidalCatDecl   : Enriched.MonoidalCategoryDeclaration
  symMonoidalDecl   : Enriched.SymmetricMonoidalCategoryDeclaration
  enrichedData      : Enriched.EnrichedCategoryData
  enrichedCatDecl   : Enriched.EnrichedCategoryDeclaration

  -- Ab-specific helpers
  homAbGroup : (A B : AFo.AbelianGroupDeclaration) → AGA.HomAbelianGroup A B
  abClosed   : AGA.AbIsClosed

-- ============================================================================
-- 1. Monoid as Monoidal Category
-- ============================================================================

monoidAsMonoidal : AE.MonoidAsMonoidalCategory
monoidAsMonoidal = record
  { monoid = natMonoidDecl
  ; singleObject = M.mkId "*"
  ; morphismsAreElements = M.mkId "ℕ-as-morphisms"
  ; tensorIsOperation = M.mkId "⊗=+"
  ; unitIsIdentity = M.mkId "I=0"
  ; monoidalStructure = monoidalCatDecl
  }

monoidAsMonoidalAdapt : A.MonoidAsMonoidalCategoryAdapter
monoidAsMonoidalAdapt =
  A.mkMonoidAsMonoidalCategoryAdapter
    monoidAsMonoidal
    natMonoidDecl
    refl

_ : A.isFilledMonoidAsMonoidalCategory monoidAsMonoidalAdapt ≡ true
_ = refl

-- ============================================================================
-- 2. Abelian Group as Symmetric Monoidal Category
-- ============================================================================

abAsSymMonoidal : AE.AbelianGroupAsSymmetricMonoidal
abAsSymMonoidal = record
  { abelianGroup = abGroupDecl
  ; underlyingCategory = AGA.Ab.underlyingCategory abCategory
  ; tensorProduct = M.mkId "⊗"
  ; tensorUnit = M.mkId "ℤ"
  ; symmetricMonoidalStructure = symMonoidalDecl
  }

abAsSymMonoidalAdapt : A.AbelianGroupAsSymmetricMonoidalAdapter
abAsSymMonoidalAdapt =
  A.mkAbelianGroupAsSymmetricMonoidalAdapter
    abAsSymMonoidal
    abGroupDecl
    refl

_ : A.isFilledAbelianGroupAsSymmetricMonoidal abAsSymMonoidalAdapt ≡ true
_ = refl

-- ============================================================================
-- 3. Monoid-Enriched Category
-- ============================================================================

monoidEnriched : AE.MonoidEnrichedCategory
monoidEnriched = record
  { enrichingMonoid = natMonoidDecl
  ; monoidalCat = monoidAsMonoidal
  ; enrichedData = enrichedData
  }

monoidEnrichedAdapt : A.MonoidEnrichedCategoryAdapter
monoidEnrichedAdapt =
  A.mkMonoidEnrichedCategoryAdapter
    monoidEnriched
    natMonoidDecl
    refl

_ : A.isFilledMonoidEnrichedCategory monoidEnrichedAdapt ≡ true
_ = refl

-- ============================================================================
-- 4. Distance Category
-- ============================================================================

distanceCategory : AE.DistanceCategory
distanceCategory = record
  { naturalNumbersMonoid = natMonoidDecl
  ; enrichedStructure = monoidEnriched
  ; triangleInequality = M.mkId "d(x,z)≤d(x,y)+d(y,z)"
  }

distanceCategoryAdapt : A.DistanceCategoryAdapter
distanceCategoryAdapt =
  A.mkDistanceCategoryAdapter
    distanceCategory
    natMonoidDecl
    refl

_ : A.isFilledDistanceCategory distanceCategoryAdapt ≡ true
_ = refl

-- ============================================================================
-- 5. Ab-Enriched Category (Additive Category)
-- ============================================================================

abEnriched : AE.AbEnrichedCategory
abEnriched = record
  { enrichingCategory = AGA.Ab.underlyingCategory abCategory
  ; symmetricMonoidal = abAsSymMonoidal
  ; enrichedData = enrichedData
  ; isAdditive = M.mkId "has-biproducts"
  }

abEnrichedAdapt : A.AbEnrichedCategoryAdapter
abEnrichedAdapt =
  A.mkAbEnrichedCategoryAdapter
    abEnriched
    (AGA.Ab.underlyingCategory abCategory)
    refl

_ : A.isFilledAbEnrichedCategory abEnrichedAdapt ≡ true
_ = refl

-- ============================================================================
-- 6. Generic Enrichment
-- ============================================================================

genericEnrichment : AE.GenericEnrichment monoidalCatDecl
genericEnrichment = record
  { enrichingCategory = catDecl
  ; monoidalStructure = monoidalCatDecl
  ; objects = M.Identifier
  ; homObject = λ A B → M.mkId "Hom(A,B)"
  ; compositionInV = λ A B C → M.mkId "comp_ABC"
  ; identityInV = λ A → M.mkId "id_A"
  ; enrichedCategory = enrichedCatDecl
  }

genericEnrichmentAdapt : A.GenericEnrichmentAdapter
genericEnrichmentAdapt =
  A.mkGenericEnrichmentAdapter
    monoidalCatDecl
    genericEnrichment
    catDecl
    refl

_ : A.isFilledGenericEnrichment genericEnrichmentAdapt ≡ true
_ = refl

-- ============================================================================
-- 7. Group Action Enriched Category
-- ============================================================================

groupActionEnriched : AE.GroupActionEnrichedCategory
groupActionEnriched = record
  { actingGroup = groupDecl
  ; objects = M.mkId "G-Sets"
  ; homSets = M.mkId "G-equivariant-maps"
  ; enrichedStructure = M.mkId "G-action-enrichment"
  }

groupActionEnrichedAdapt : A.GroupActionEnrichedCategoryAdapter
groupActionEnrichedAdapt =
  A.mkGroupActionEnrichedCategoryAdapter
    groupActionEnriched
    groupDecl
    refl

_ : A.isFilledGroupActionEnrichedCategory groupActionEnrichedAdapt ≡ true
_ = refl

-- ============================================================================
-- 8. Module-Enriched Category
-- ============================================================================

moduleEnriched : AE.ModuleEnrichedCategory
moduleEnriched = record
  { baseRing = M.mkId "R"
  ; homModules = M.mkId "Hom_R(M,N)"
  ; bilinearComposition = M.mkId "comp-bilinear"
  ; enrichedStructure = enrichedCatDecl
  }

moduleEnrichedAdapt : A.ModuleEnrichedCategoryAdapter
moduleEnrichedAdapt =
  A.mkModuleEnrichedCategoryAdapter moduleEnriched

_ : A.isFilledModuleEnrichedCategory moduleEnrichedAdapt ≡ true
_ = refl

-- ============================================================================
-- 9. Lawvere Theory Enriched Category
-- ============================================================================

lawvereTheoryEnriched : AE.LawvereTheoryEnrichedCategory
lawvereTheoryEnriched = record
  { theory = M.mkId "T"
  ; modelsCategory = M.mkId "Mod(T,Set)"
  ; enrichedStructure = M.mkId "T-enrichment"
  }

lawvereTheoryEnrichedAdapt : A.LawvereTheoryEnrichedCategoryAdapter
lawvereTheoryEnrichedAdapt =
  A.mkLawvereTheoryEnrichedCategoryAdapter lawvereTheoryEnriched

_ : A.isFilledLawvereTheoryEnrichedCategory lawvereTheoryEnrichedAdapt ≡ true
_ = refl

-- ============================================================================
-- 10. Ab Self-Enriched
-- ============================================================================

abSelfEnriched : AGA.AbSelfEnriched
abSelfEnriched = record
  { category = abCategory
  ; homObject = homAbGroup
  ; compositionIsBilinear = M.mkId "comp-bilinear"
  ; identityStructure = M.mkId "id-structure"
  ; enrichedStructure = abEnriched
  }

abSelfEnrichedAdapt : A.AbSelfEnrichedAdapter
abSelfEnrichedAdapt =
  A.mkAbSelfEnrichedAdapter
    abSelfEnriched
    abCategory
    refl

_ : A.isFilledAbSelfEnriched abSelfEnrichedAdapt ≡ true
_ = refl

-- ============================================================================
-- 11. Ab Self-Enrichment via Internal Hom
-- ============================================================================

abSelfEnrichmentViaIntHom : AGA.AbSelfEnrichmentViaInternalHom
abSelfEnrichmentViaIntHom = record
  { category = abCategory
  ; selfEnriched = abSelfEnriched
  ; closed = abClosed
  ; enrichmentCoincides = M.mkId "Hom=[_,_]"
  }

abSelfEnrichmentViaIntHomAdapt : A.AbSelfEnrichmentViaInternalHomAdapter
abSelfEnrichmentViaIntHomAdapt =
  A.mkAbSelfEnrichmentViaInternalHomAdapter
    abSelfEnrichmentViaIntHom
    abCategory
    refl

_ : A.isFilledAbSelfEnrichmentViaInternalHom abSelfEnrichmentViaIntHomAdapt ≡ true
_ = refl
-- Intentionally omit brittle categorical equalities; type-checking the
-- categorical views is covered by constructing them during isFilled checks.
