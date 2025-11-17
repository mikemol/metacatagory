``` Agda
-- Level2_5: Accessible Categories (Chapter 2, Section 5)
-- This module encodes the structural content of Section 5 from the EBNF grammar.
-- It covers locally presentable categories, accessible categories, raising degrees,
-- functors with rank, and sketches.

module Chapter2.Level2sub5 where

open import Core
open import Chapter1.Level1Index  -- Not public to avoid conflicts
open import Metamodel as M
open import Chapter2.Level2sub3 as L23

-- ============================================================================
-- Section 5.2: Locally Presentable Categories
-- ============================================================================

-- The master definition: a category built from small generators via colimits
record LocallyPresentableCategoryDeclaration : Set₁ where
  field
    category : CategoryDeclaration
    rank : RegularCardinal
    -- Condition 1: C is cocomplete
    isCocompleteCond : Set
    -- Condition 2: There exists a small set S of λ-presentable objects
    -- that generates C under colimits
    generatingSet : Set
    generatingSetIsSmall : Set
    generatorsArePresentable : Set
    generatesUnderColimits : Set

-- Theorem: Every locally presentable category is complete
record LocallyPresentableImpliesCompleteTheorem : Set₁ where
  field
    locallyPresentableCategory : LocallyPresentableCategoryDeclaration
    -- Conclusion: C is also complete
    categoryIsComplete : Set
    -- This is a form of the Adjoint Functor Theorem

-- Theorem: Locally presentable categories are well-behaved
record LocallyPresentableAreWellBehavedTheorem : Set₁ where
  field
    locallyPresentableCategory : LocallyPresentableCategoryDeclaration
    -- Conclusion: C is well-powered, well-copowered, and has
    -- (StrongEpi, Mono) factorization system
    isWellPowered : Set
    isWellCopowered : Set
    hasStrongEpiMonoFactorization : Set

-- ============================================================================
-- Section 5.3: Accessible Categories
-- ============================================================================

-- A weakening of locally presentable: only requires λ-filtered colimits
record AccessibleCategoryDeclaration : Set₁ where
  field
    category : CategoryDeclaration
    rank : RegularCardinal
    -- Condition 1: C has all λ-filtered colimits
    hasFilteredColimits : Set
    -- Condition 2: There exists a small set S of λ-presentable objects
    -- such that every object is a λ-filtered colimit of objects from S
    generatingSet : Set
    generatingSetIsSmall : Set
    generatorsArePresentable : Set
    generatesViaFilteredColimits : Set

-- Theorem: Every locally presentable category is accessible
record LocallyPresentableImpliesAccessibleTheorem : Set₁ where
  field
    locallyPresentableCategory : LocallyPresentableCategoryDeclaration
    -- Conclusion: C is λ-accessible
    categoryIsAccessible : AccessibleCategoryDeclaration
    -- The rank is the same

-- ============================================================================
-- Section 5.4: Raising the Degree of Accessibility
-- ============================================================================

-- Lemma: Presentability is stable upwards
record PresentabilityIsStableUpwardsTheorem : Set₁ where
  field
    smallerRank : RegularCardinal
    largerRank : RegularCardinal
    rankOrdering : Set  -- λ < κ
    -- Premise: Object A is λ-presentable
    objectA : M.Identifier
    isLambdaPresentable : Set
    -- Conclusion: A is also κ-presentable
    isKappaPresentable : Set
    -- Proof sketch: Hom(A,-) preserves λ-filtered colimits,
    -- κ-filtered colimits are also λ-filtered colimits,
    -- therefore Hom(A,-) preserves κ-filtered colimits

-- Theorem: Accessibility is stable upwards
record RaisingTheDegreeOfAccessibilityTheorem : Set₁ where
  field
    category : CategoryDeclaration
    smallerRank : RegularCardinal
    -- Premise: C is λ-accessible
    isLambdaAccessible : AccessibleCategoryDeclaration
    -- Conclusion: For all κ > λ, C is κ-accessible
    stableUpwards : (largerRank : RegularCardinal) → Set

-- Theorem: Local presentability is stable upwards
record RaisingTheDegreeOfPresentabilityTheorem : Set₁ where
  field
    category : CategoryDeclaration
    smallerRank : RegularCardinal
    -- Premise: C is locally λ-presentable
    isLambdaPresentable : LocallyPresentableCategoryDeclaration
    -- Conclusion: For all κ > λ, C is locally κ-presentable
    stableUpwards : (largerRank : RegularCardinal) → Set

-- ============================================================================
-- Section 5.5: Functors with Rank
-- ============================================================================

-- Definition: A functor has rank λ if it preserves λ-filtered colimits
record FunctorHasRankProperty : Set₁ where
  field
    functor : M.Identifier
    rank : RegularCardinal
    -- F has rank λ ⟺ F preserves λ-filtered colimits
    preservesFilteredColimits : Set

-- Theorem: Right adjoints between locally presentable categories have rank
record RightAdjointsBetweenLPCatsHaveRankTheorem : Set₁ where
  field
    sourceCategory : CategoryDeclaration
    targetCategory : CategoryDeclaration
    rank : RegularCardinal
    -- Premise: C and D are locally λ-presentable
    sourceIsLocallyPresentable : LocallyPresentableCategoryDeclaration
    targetIsLocallyPresentable : LocallyPresentableCategoryDeclaration
    -- Premise: F ⊣ G is an adjunction
    leftAdjoint : M.Identifier
    rightAdjoint : M.Identifier
    adjunction : M.Identifier
    -- Conclusion: G has rank λ
    rightAdjointHasRank : FunctorHasRankProperty

-- ============================================================================
-- Section 5.6: Sketches
-- ============================================================================

-- A cone in a category (for sketch specification)
record LPConeDeclaration : Set₁ where
  field
    indexCategory : CategoryDeclaration
    baseCategory : CategoryDeclaration
    diagram : M.Identifier
    apex : M.Identifier
    legs : Set  -- Collection of morphisms from apex to diagram objects

-- A cocone in a category (for sketch specification)
record LPCoconeDeclaration : Set₁ where
  field
    indexCategory : CategoryDeclaration
    baseCategory : CategoryDeclaration
    diagram : M.Identifier
    apex : M.Identifier
    legs : Set  -- Collection of morphisms from diagram objects to apex

-- A sketch: syntactic specification of a categorical theory
record SketchDeclaration : Set₁ where
  field
    underlyingCategory : CategoryDeclaration
    -- The underlying category is small
    categoryIsSmall : Set
    -- Designated cones (to become limits in models)
    limitCones : Set  -- Collection of ConeDeclaration
    -- Designated cocones (to become colimits in models)
    colimitCocones : Set  -- Collection of CoconeDeclaration

-- A model of a sketch: a functor that realizes the specification
record ModelOfSketch : Set₁ where
  field
    sketch : SketchDeclaration
    targetCategory : CategoryDeclaration
    -- The functor from sketch's underlying category to target
    interpretationFunctor : M.Identifier
    -- Condition: Every designated cone becomes a limit cone
    preservesLimitCones : Set
    -- Condition: Every designated cocone becomes a colimit cone
    preservesColimitCocones : Set

-- The category of all models of a sketch
record CategoryOfSketchModels : Set₁ where
  field
    sketch : SketchDeclaration
    targetCategory : CategoryDeclaration
    -- Objects: models of S in C
    modelsAsObjects : Set
    -- Morphisms: natural transformations between models
    naturalTransformationsAsMorphisms : Set

-- Gabriel-Ulmer Duality: The fundamental characterization theorem
record GabrielUlmerDualityTheorem : Set₁ where
  field
    category : CategoryDeclaration
    -- The equivalence: C is locally presentable ⟺ C ≅ Mod(S, Set) for some sketch S
    -- Direction 1: Locally presentable implies models of a sketch
    locallyPresentableImpliesSketchModels : 
      LocallyPresentableCategoryDeclaration → 
      (sketch : SketchDeclaration) → 
      Set  -- C ≅ Mod(sketch, Set)
    -- Direction 2: Models of a sketch implies locally presentable
    sketchModelsImpliesLocallyPresentable :
      (sketch : SketchDeclaration) →
      Set  -- Mod(sketch, Set) is locally presentable

-- ============================================================================
-- Instances and Examples
-- ============================================================================

-- Example: Categories of algebraic models are locally finitely presentable
record AlgebraicModelsAreLocallyFinitelyPresentableInstance : Set₁ where
  field
    theory : L23.LawvereTheoryDeclaration  -- From Level2_3
    modelCategory : CategoryDeclaration
    -- The category of models is locally ℵ₀-presentable
    isLocallyFinitelyPresentable : LocallyPresentableCategoryDeclaration

-- ============================================================================
-- End of Level2_5
-- ============================================================================
```
