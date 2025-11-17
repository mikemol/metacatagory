{-# OPTIONS --allow-unsolved-metas #-}

module Tests.EnrichmentChecklist where

open import Agda.Builtin.Equality using (_≡_; refl)
import Agda.Builtin.Nat as N
import Agda.Builtin.String as S
import Metamodel as M
import Algebra.Foundation as AFo
import Algebra.Enrichment as AE
import Algebra.Groups.Basic as AGB
import Algebra.Groups.Abelian as AGA
import Chapter1.Level1 as C
import Chapter2.Level2sub6 as Enriched
import Tests.ObligationAdapters as A

-- ============================================================================
-- Setup: Basic algebraic structures for enrichment testing
-- ============================================================================

-- Natural numbers monoid (ℕ, +, 0)
natMonoidId : M.Identifier
natMonoidId = M.mkId "ℕ⁺"

natMagmaDecl : AFo.MagmaDeclaration
natMagmaDecl = record
  { magmaId = M.mkId "ℕ-magma"
  ; operation = M.mkId "+"
  }

natSemigroupDecl : AFo.SemigroupDeclaration
natSemigroupDecl = record
  { semigroupId = M.mkId "ℕ-semigroup"
  ; underlyingMagma = natMagmaDecl
  ; isAssociative = M.mkId "+-assoc"
  }

natMonoidDecl : AFo.MonoidDeclaration
natMonoidDecl = record
  { monoidId = natMonoidId
  ; underlyingSemigroup = natSemigroupDecl
  ; identity = M.mkId "0"
  ; hasIdentity = M.mkId "0-identity"
  }

-- Generic group for examples
groupId : M.Identifier
groupId = M.mkId "G"

groupMagmaDecl : AFo.MagmaDeclaration
groupMagmaDecl = record
  { magmaId = M.mkId "G-magma"
  ; operation = M.mkId "·"
  }

groupSemigroupDecl : AFo.SemigroupDeclaration
groupSemigroupDecl = record
  { semigroupId = M.mkId "G-semigroup"
  ; underlyingMagma = groupMagmaDecl
  ; isAssociative = M.mkId "·-assoc"
  }

groupMonoidDecl : AFo.MonoidDeclaration
groupMonoidDecl = record
  { monoidId = M.mkId "G-monoid"
  ; underlyingSemigroup = groupSemigroupDecl
  ; identity = M.mkId "e"
  ; hasIdentity = M.mkId "e-identity"
  }

groupDecl : AGB.GroupDeclaration
groupDecl = record
  { groupId = groupId
  ; underlyingMonoid = groupMonoidDecl
  ; inverse = M.mkId "inv"
  ; hasInverse = M.mkId "inv-property"
  }

-- Abelian group ℤ
abGroupId : M.Identifier
abGroupId = M.mkId "ℤ"

abGroupMagmaDecl : AFo.MagmaDeclaration
abGroupMagmaDecl = record
  { magmaId = M.mkId "ℤ-magma"
  ; operation = M.mkId "+"
  }

abGroupSemigroupDecl : AFo.SemigroupDeclaration
abGroupSemigroupDecl = record
  { semigroupId = M.mkId "ℤ-semigroup"
  ; underlyingMagma = abGroupMagmaDecl
  ; isAssociative = M.mkId "+-assoc"
  }

abGroupMonoidDecl : AFo.MonoidDeclaration
abGroupMonoidDecl = record
  { monoidId = M.mkId "ℤ-monoid"
  ; underlyingSemigroup = abGroupSemigroupDecl
  ; identity = M.mkId "0"
  ; hasIdentity = M.mkId "0-identity"
  }

abGroupGroupDecl : AGB.GroupDeclaration
abGroupGroupDecl = record
  { groupId = M.mkId "ℤ-group"
  ; underlyingMonoid = abGroupMonoidDecl
  ; inverse = M.mkId "neg"
  ; hasInverse = M.mkId "neg-property"
  }

abGroupDecl : AGA.AbelianGroupDeclaration
abGroupDecl = record
  { abelianGroupId = abGroupId
  ; underlyingGroup = abGroupGroupDecl
  ; isCommutative = M.mkId "+-comm"
  }

-- Category of abelian groups
abCategoryId : M.Identifier
abCategoryId = M.mkId "Ab"

abCategory : AGA.CategoryOfAbelianGroups
abCategory = record
  { categoryId = abCategoryId
  ; objects = M.mkId "AbelianGroups"
  ; morphisms = M.mkId "GroupHomomorphisms"
  ; hasLimits = M.mkId "Ab-limits"
  ; hasColimits = M.mkId "Ab-colimits"
  }

-- Generic category declaration for enrichment
catId : M.Identifier
catId = M.mkId "C"

catDecl : C.CategoryDeclaration
catDecl = record
  { categoryId = catId
  ; objects = M.mkId "Obj(C)"
  ; morphisms = M.mkId "Mor(C)"
  }

-- Monoidal category declaration (placeholder)
monoidalCatId : M.Identifier
monoidalCatId = M.mkId "V"

monoidalCatDecl : Enriched.MonoidalCategoryDeclaration
monoidalCatDecl = record
  { monoidalCategoryId = monoidalCatId
  ; underlyingCategory = catDecl
  ; tensorProduct = M.mkId "⊗"
  ; tensorUnit = M.mkId "I"
  ; associator = M.mkId "α"
  ; leftUnitor = M.mkId "λ"
  ; rightUnitor = M.mkId "ρ"
  }

-- Symmetric monoidal structure
symMonoidalDecl : Enriched.SymmetricMonoidalCategoryDeclaration
symMonoidalDecl = record
  { symmetricMonoidalCategoryId = M.mkId "V-sym"
  ; underlyingMonoidalCategory = monoidalCatDecl
  ; braiding = M.mkId "β"
  ; symmetry = M.mkId "β⁻¹=β"
  }

-- Enriched category data
enrichedDataId : M.Identifier
enrichedDataId = M.mkId "EnrichedData"

enrichedData : Enriched.EnrichedCategoryData
enrichedData = record
  { dataId = enrichedDataId
  ; enrichingCategory = catDecl
  ; objects = M.mkId "Obj"
  ; homObjects = M.mkId "Hom"
  ; composition = M.mkId "comp"
  ; identity = M.mkId "id"
  }

-- Enriched category declaration
enrichedCatDecl : Enriched.EnrichedCategoryDeclaration
enrichedCatDecl = record
  { enrichedCategoryId = M.mkId "C-enriched"
  ; enrichingCategory = catDecl
  ; enrichedData = enrichedData
  }

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
  ; underlyingCategory = abCategory
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
  { enrichingCategory = abCategory
  ; symmetricMonoidal = abAsSymMonoidal
  ; enrichedData = enrichedData
  ; isAdditive = M.mkId "has-biproducts"
  }

abEnrichedAdapt : A.AbEnrichedCategoryAdapter
abEnrichedAdapt =
  A.mkAbEnrichedCategoryAdapter
    abEnriched
    abCategory
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

-- Hom of abelian groups
homAbGroup : (A B : AGA.AbelianGroupDeclaration) → AGA.HomAbelianGroup A B
homAbGroup A B = record
  { source = A
  ; target = B
  ; homGroup = record
    { abelianGroupId = M.mkId "Hom(A,B)"
    ; underlyingGroup = record
      { groupId = M.mkId "Hom(A,B)-group"
      ; underlyingMonoid = record
        { monoidId = M.mkId "Hom(A,B)-monoid"
        ; underlyingSemigroup = record
          { semigroupId = M.mkId "Hom(A,B)-semigroup"
          ; underlyingMagma = record
            { magmaId = M.mkId "Hom(A,B)-magma"
            ; operation = M.mkId "+"
            }
          ; isAssociative = M.mkId "hom-assoc"
          }
        ; identity = M.mkId "0"
        ; hasIdentity = M.mkId "hom-identity"
        }
      ; inverse = M.mkId "neg"
      ; hasInverse = M.mkId "hom-inverse"
      }
    ; isCommutative = M.mkId "hom-comm"
    }
  }

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

-- Closed structure for Ab
abClosed : AGA.AbIsClosed
abClosed = record
  { category = abCategory
  ; internalHom = M.mkId "[A,B]"
  ; evaluation = M.mkId "eval"
  ; closedStructure = M.mkId "Ab-closed"
  }

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
