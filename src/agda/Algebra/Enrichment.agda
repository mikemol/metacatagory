{-# OPTIONS --without-K #-}

-- Algebra.Enrichment: Bridging algebraic structures with enriched category theory
-- This module shows how categories can be enriched over algebraic structures,
-- demonstrating the constructive connection between abstract algebra and enriched categories.

module Algebra.Enrichment where

open import Core
open import Chapter1.Level1Index
open import Chapter2.Level2sub6 as Enriched
open import Algebra.Foundation
open import PropertyRegistry
open import Metamodel as M

-- ============================================================================
-- Algebraic Structures as Monoidal Categories
-- ============================================================================

-- Any monoid gives rise to a (discrete) monoidal category
record MonoidAsMonoidalCategory : Set₁ where
  field
    monoid : MonoidDeclaration
    -- The category has one object (the monoid itself)
    singleObject : M.Identifier
    -- Morphisms are elements of the monoid
    morphismsAreElements : M.Identifier
    -- Tensor is the monoid operation
    tensorIsOperation : M.Identifier
    -- Unit is the identity element
    unitIsIdentity : M.Identifier
    -- This gives a monoidal category structure
    monoidalStructure : Enriched.MonoidalCategoryDeclaration

-- Abelian groups give symmetric monoidal categories (via tensor product)
record AbelianGroupAsSymmetricMonoidal : Set₁ where
  field
    abelianGroup : AbelianGroupDeclaration
    -- Ab has a symmetric monoidal structure via ⊗
    underlyingCategory : CategoryOfAbelianGroups
    tensorProduct : M.Identifier  -- A ⊗ B
    tensorUnit : M.Identifier     -- ℤ (integers)
    symmetricMonoidalStructure : Enriched.SymmetricMonoidalCategoryDeclaration

-- ============================================================================
-- Categories Enriched Over Algebraic Structures
-- ============================================================================

-- A category enriched over a monoid (preordered category)
-- This is the simplest case: hom-objects are truth values (0 or 1 in monoid)
record MonoidEnrichedCategory : Set₁ where
  field
    enrichingMonoid : MonoidDeclaration
    monoidalCat : MonoidAsMonoidalCategory
    -- The enriched category structure
    enrichedData : Enriched.EnrichedCategoryData

-- Example: Categories enriched over (ℕ, +, 0) are "distance categories"
-- where hom(A,B) is a natural number (the "distance" from A to B)
record DistanceCategory : Set₁ where
  field
    -- Enriching over (ℕ, +, 0)
    naturalNumbersMonoid : MonoidDeclaration
    -- Objects and "distances" between them
    enrichedStructure : MonoidEnrichedCategory
    -- Satisfies triangle inequality (via enriched composition)
    triangleInequality : M.Identifier

-- Ab-enriched categories (additive categories)
-- This is fundamental for homological algebra
record AbEnrichedCategory : Set₁ where
  field
    enrichingCategory : CategoryOfAbelianGroups
    symmetricMonoidal : AbelianGroupAsSymmetricMonoidal
    -- Hom-objects are abelian groups
    enrichedData : Enriched.EnrichedCategoryData
    -- This makes it an additive category
    isAdditive : M.Identifier

-- ============================================================================
-- Constructive Enrichment: Generic Pattern
-- ============================================================================

-- Given ANY monoidal category V, we can construct V-enriched categories
-- This is the fully general construction
record GenericEnrichment (V : Enriched.MonoidalCategoryDeclaration) : Set₁ where
  field
    -- The enriching category (could be from algebra or anywhere)
    enrichingCategory : CategoryDeclaration
    monoidalStructure : Enriched.MonoidalCategoryDeclaration
    
    -- A V-enriched category C has:
    -- 1. A set of objects
    objects : Set
    
    -- 2. For each pair (A,B), a hom-object C(A,B) in V
    homObject : M.Identifier → M.Identifier → M.Identifier
    
    -- 3. Composition morphisms in V
    compositionInV : (A B C : M.Identifier) → M.Identifier
    
    -- 4. Identity morphisms in V
    identityInV : (A : M.Identifier) → M.Identifier
    
    -- 5. Axioms (associativity and unit laws in V)
    enrichedCategory : Enriched.EnrichedCategoryDeclaration

-- ============================================================================
-- Examples from Algebra Tree
-- ============================================================================

-- Groups act on sets → categories enriched over Set with group actions
record GroupActionEnrichedCategory : Set₁ where
  field
    actingGroup : GroupDeclaration
    -- Objects are sets with G-actions
    objects : M.Identifier
    -- Morphisms are G-equivariant maps
    homSets : M.Identifier
    -- This gives enrichment over Set (or a suitable category)
    enrichedStructure : M.Identifier

-- Rings → categories enriched over R-modules
-- (this is the foundation of derived categories)
record ModuleEnrichedCategory : Set₁ where
  field
    baseRing : M.Identifier  -- Will be defined in Algebra.Rings
    -- Hom-objects are R-modules
    homModules : M.Identifier
    -- Composition is R-bilinear
    bilinearComposition : M.Identifier
    enrichedStructure : Enriched.EnrichedCategoryDeclaration

-- ============================================================================
-- Key Theorems (Placeholders for Future Development)
-- ============================================================================

-- An Ab-enriched category with biproducts is an additive category
postulate
  AbEnriched-WithBiproducts-IsAdditive :
    (C : AbEnrichedCategory) →
    M.Identifier  -- Evidence of biproducts
    → M.Identifier  -- Proof that C is additive

-- An additive category with kernels and cokernels is abelian
-- (This connects to Chapter2.Level2sub1)
postulate
  Additive-WithKernels-IsAbelian :
    (C : AbEnrichedCategory) →
    M.Identifier  -- Evidence of kernels
    → M.Identifier  -- Evidence of cokernels
    → M.Identifier  -- Proof that C is abelian

-- Change of enrichment: if V → W is lax monoidal, 
-- then V-categories can be seen as W-categories
postulate
  ChangeOfBase :
    (V W : Enriched.MonoidalCategoryDeclaration) →
    M.Identifier  -- Lax monoidal functor V → W
    → GenericEnrichment V
    → GenericEnrichment W

-- ============================================================================
-- Connection to Lawvere Theories
-- ============================================================================

-- Enrichment over algebraic theories (connects to Chapter2.Level2sub3)
-- A category enriched over Mod(T) for a Lawvere theory T
record LawvereTheoryEnrichedCategory : Set₁ where
  field
    theory : M.Identifier  -- Reference to a Lawvere theory
    modelsCategory : M.Identifier  -- Mod(T, Set)
    -- Categories enriched over models of the theory
    enrichedStructure : M.Identifier

-- ============================================================================
-- Summary and Design Philosophy
-- ============================================================================

-- This module demonstrates that:
--
-- 1. ANY algebraic structure that forms a monoidal category can enrich categories
-- 2. The construction is fully generic via GenericEnrichment
-- 3. Specific examples (monoids, abelian groups, rings, modules) are instances
-- 4. This bridges pure algebra ↔ category theory ↔ enriched category theory
-- 5. The framework is constructive: given V and data, we can build V-categories
--
-- Key instances from the algebra tree:
-- - Monoids → preorders/metric spaces
-- - Abelian groups → additive categories → abelian categories
-- - Rings → module categories → derived categories
-- - General algebraic theories → theory-enriched categories
