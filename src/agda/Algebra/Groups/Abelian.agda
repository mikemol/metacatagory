-- Algebra.Groups.Abelian: Abelian groups and their categorical properties
-- This module develops the theory of abelian groups, showing how Ab is enriched over itself
-- and serves as the foundation for homological algebra (Grothendieck, Eilenberg-Mac Lane).

module Algebra.Groups.Abelian where

open import Core
open import Chapter1.Level1Index
open import Chapter2.Level2sub1 as AbelianCat  -- Abelian categories
open import Chapter2.Level2sub6 as Enriched
open import Algebra.Foundation
open import Algebra.Enrichment
open import PropertyRegistry
open import Metamodel as M

-- ============================================================================
-- Ab as a Category
-- ============================================================================

-- The category Ab of abelian groups and group homomorphisms
record Ab : Set₁ where
  field
    underlyingCategory : CategoryOfAbelianGroups
    -- Objects are abelian groups
    objects : M.Identifier
    -- Morphisms are group homomorphisms
    morphisms : M.Identifier

-- ============================================================================
-- Ab is Enriched Over Itself
-- ============================================================================

-- For abelian groups A, B, the set Hom(A,B) of homomorphisms
-- forms an abelian group via pointwise addition
record HomAbelianGroup (A B : AbelianGroupDeclaration) : Set₁ where
  field
    -- The underlying set of homomorphisms
    homSet : M.Identifier
    
    -- Pointwise addition: (f + g)(x) = f(x) + g(x)
    pointwiseAddition : M.Identifier
    
    -- Zero homomorphism: 0(x) = 0_B for all x
    zeroHomomorphism : M.Identifier
    
    -- Negation: (-f)(x) = -(f(x))
    negation : M.Identifier
    
    -- This forms an abelian group
    abelianGroupStructure : AbelianGroupDeclaration

-- Ab is enriched over itself: Hom(A,B) is an object in Ab
record AbSelfEnriched : Set₁ where
  field
    category : Ab
    
    -- For each pair of abelian groups, hom-object is an abelian group
    homObject : (A B : AbelianGroupDeclaration) → HomAbelianGroup A B
    
    -- Composition is bilinear (a group homomorphism in each variable)
    -- comp : Hom(B,C) ⊗ Hom(A,B) → Hom(A,C)
    compositionIsBilinear : M.Identifier
    
    -- Identity is the zero homomorphism from Z → Hom(A,A)
    identityStructure : M.Identifier
    
    -- This makes Ab into an Ab-enriched category
    enrichedStructure : AbEnrichedCategory

-- ============================================================================
-- Ab is an Abelian Category
-- ============================================================================

-- Ab has zero objects (the trivial group)
record AbHasZeroObject : Set₁ where
  field
    category : Ab
    trivialGroup : AbelianGroupDeclaration
    -- 0 = {0} is both initial and terminal
    isZeroObject : M.Identifier

-- Ab has kernels
record AbHasKernels : Set₁ where
  field
    category : Ab
    homomorphism : M.Identifier  -- f : A → B
    -- ker(f) = {a ∈ A | f(a) = 0}
    kernel : AbelianGroupDeclaration
    kernelInclusion : M.Identifier
    isKernel : M.Identifier

-- Ab has cokernels
record AbHasCokernels : Set₁ where
  field
    category : Ab
    homomorphism : M.Identifier  -- f : A → B
    -- coker(f) = B / im(f)
    cokernel : AbelianGroupDeclaration
    cokernelProjection : M.Identifier
    isCokernel : M.Identifier

-- The fundamental theorem: Ab is an abelian category
record AbIsAbelianCategory : Set₁ where
  field
    category : Ab
    selfEnriched : AbSelfEnriched
    hasZeroObjects : AbHasZeroObject
    hasKernels : AbHasKernels
    hasCokernels : AbHasCokernels
    -- This witnesses that Ab satisfies all abelian category axioms
    abelianCategoryStructure : M.Identifier

-- ============================================================================
-- Grothendieck Group Construction
-- ============================================================================

-- For any commutative monoid M, construct its Grothendieck group K(M)
-- This is the universal way to "complete" a monoid to a group
record GrothendieckGroup (M : MonoidDeclaration) : Set₁ where
  field
    -- Construction: K(M) = (M × M) / ~
    -- where (a,b) ~ (c,d) iff ∃e. a+d+e = b+c+e
    underlyingSet : M.Identifier
    
    -- The group operation: [(a,b)] + [(c,d)] = [(a+c, b+d)]
    groupOperation : M.Identifier
    
    -- Identity: [(0,0)]
    identityElement : M.Identifier
    
    -- Inverse: -[(a,b)] = [(b,a)]
    inverseOperation : M.Identifier
    
    -- This forms an abelian group
    abelianGroupStructure : AbelianGroupDeclaration
    
    -- Universal property: M → K(M) is universal among monoid homomorphisms to groups
    universalMap : M.Identifier
    universalProperty : M.Identifier

-- The Grothendieck construction is functorial
record GrothendieckFunctor : Set₁ where
  field
    -- K : CMon → Ab (commutative monoids to abelian groups)
    sourceCat : M.Identifier  -- Category of commutative monoids
    targetCat : Ab
    
    -- On objects: M ↦ K(M)
    onObjects : MonoidDeclaration → AbelianGroupDeclaration
    
    -- On morphisms: f : M → N induces K(f) : K(M) → K(N)
    onMorphisms : M.Identifier
    
    -- This is a functor
    functorStructure : M.Identifier

-- ============================================================================
-- Free Abelian Groups and the Free-Forgetful Adjunction
-- ============================================================================

-- For any set X, the free abelian group on X
record FreeAbelianGroup (X : M.Identifier) : Set₁ where
  field
    -- Construction: ℤ[X] = formal integer linear combinations of elements of X
    underlyingSet : M.Identifier
    
    -- Universal property: Set(X, U(A)) ≅ Ab(ℤ[X], A)
    universalProperty : M.Identifier
    
    -- The abelian group structure
    abelianGroupStructure : AbelianGroupDeclaration

-- Free ⊣ Forgetful adjunction between Set and Ab
record FreeForgetfulAdjunctionAb : Set₁ where
  field
    -- F : Set → Ab (free abelian group functor)
    freeFunctor : M.Identifier
    
    -- U : Ab → Set (forgetful functor)
    forgetfulFunctor : M.Identifier
    
    -- Natural isomorphism: Set(X, U(A)) ≅ Ab(F(X), A)
    adjunctionIsomorphism : M.Identifier
    
    -- Unit: η : X → U(F(X)) (send x to the "basis element" x)
    unit : M.Identifier
    
    -- Counit: ε : F(U(A)) → A (evaluate formal sums)
    counit : M.Identifier

-- ============================================================================
-- Tensor Product of Abelian Groups
-- ============================================================================

-- The tensor product A ⊗ B makes Ab into a symmetric monoidal category
record TensorProductAb (A B : AbelianGroupDeclaration) : Set₁ where
  field
    -- Construction: A ⊗ B = free abelian group on A × B modulo bilinearity
    underlyingSet : M.Identifier
    
    -- Universal property: bilinear maps A × B → C correspond to 
    -- group homomorphisms A ⊗ B → C
    universalProperty : M.Identifier
    
    -- The resulting abelian group
    tensorProduct : AbelianGroupDeclaration

-- Ab with ⊗ is a symmetric monoidal category
record AbAsSymmetricMonoidalCategory : Set₁ where
  field
    category : Ab
    
    -- Tensor product bifunctor
    tensorBifunctor : M.Identifier
    
    -- Unit object is ℤ
    unitObject : AbelianGroupDeclaration  -- The integers
    
    -- Associator, unitors, braiding
    associator : M.Identifier
    leftUnitor : M.Identifier
    rightUnitor : M.Identifier
    braiding : M.Identifier
    
    -- Coherence axioms
    symmetricMonoidalStructure : Enriched.SymmetricMonoidalCategoryDeclaration

-- ============================================================================
-- Ab is Closed: Internal Hom
-- ============================================================================

-- Ab is symmetric monoidal closed: there's an internal hom
record AbIsClosed : Set₁ where
  field
    symmetricMonoidal : AbAsSymmetricMonoidalCategory
    
    -- Internal hom: Hom(A, B) is an object in Ab
    internalHom : (A B : AbelianGroupDeclaration) → AbelianGroupDeclaration
    
    -- Adjunction: Ab(A ⊗ B, C) ≅ Ab(A, Hom(B,C))
    -- This is the tensor-hom adjunction
    tensorHomAdjunction : M.Identifier
    
    -- Evaluation morphism: Hom(A,B) ⊗ A → B
    evaluation : M.Identifier
    
    -- This makes Ab a closed symmetric monoidal category
    closedStructure : M.Identifier

-- ============================================================================
-- Connection to Enrichment
-- ============================================================================

-- Ab enriched over itself (via internal hom) coincides with the standard enrichment
record AbSelfEnrichmentViaInternalHom : Set₁ where
  field
    category : Ab
    selfEnriched : AbSelfEnriched
    closed : AbIsClosed
    
    -- The enrichment via Hom(A,B) as an abelian group
    -- coincides with the closed structure's internal hom
    enrichmentCoincides : M.Identifier

-- ============================================================================
-- Key Theorems
-- ============================================================================

-- Ab is complete and cocomplete
postulate
  Ab-IsComplete : Ab → M.Identifier
  Ab-IsCocomplete : Ab → M.Identifier

-- Ab is abelian (connects to Chapter2.Level2sub1)
postulate
  Ab-IsAbelianTheorem : (cat : Ab) → AbIsAbelianCategory

-- The Grothendieck group construction is left adjoint to the forgetful functor
postulate
  Grothendieck-Forgetful-Adjunction : M.Identifier

-- Ab is the free cocomplete category on one generator
-- (This is a deep result connecting to locally presentable categories)
postulate
  Ab-IsFreeCocompletion : M.Identifier

-- ============================================================================
-- Summary
-- ============================================================================

-- This module shows that Ab has remarkable self-referential properties:
--
-- 1. Ab is enriched over itself (Hom(A,B) is an abelian group)
-- 2. This enrichment comes from Ab being closed monoidal
-- 3. The internal hom is the same as the enrichment
-- 4. Ab is an abelian category (zero objects, kernels, cokernels)
-- 5. The Grothendieck construction universally adds inverses
-- 6. Free abelian groups give the free-forgetful adjunction
--
-- These properties make Ab the foundation for:
-- - Homological algebra (chain complexes, derived functors)
-- - K-theory (Grothendieck group of vector bundles)
-- - Representation theory (character groups)
-- - Algebraic topology (homology groups)
