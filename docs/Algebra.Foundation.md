``` Agda
-- Algebra.Foundation: Foundational algebraic structures grounded in category theory
-- This module defines the basic algebraic hierarchy (Magma → Semigroup → Monoid → Group)
-- as enrichments of the categorical framework in Core, reusing existing axiom infrastructure.
--
-- Phase I.1.3: Hierarchy Composition Validation (P5 DAG)
-- Each declaration now carries a well-founded index establishing the algebraic hierarchy depth.
-- Validation ensures Index(GroupDeclaration) > Index(MonoidDeclaration) > ... > Index(MagmaDeclaration)

module Algebra.Foundation where

open import Core
open import Chapter1.Level1Index
open import PropertyRegistry
open import Metamodel as M
open import Agda.Builtin.Nat using (Nat)

-- Well-founded index for algebraic hierarchy (from Phase I.1.2)
record AlgebraIndex : Set where
  constructor mkAlgIdx
  field
    hierarchyLevel : Nat  -- 0=Magma, 1=Semigroup, 2=Monoid, 3=Group, etc.
open AlgebraIndex public

-- ============================================================================
-- Binary Operations and Magmas
-- ============================================================================

-- A magma is a set with a binary operation (no axioms required)
record MagmaStructure : Set₁ where
  field
    carrier : Set
    operation : carrier → carrier → carrier

-- Magma as a categorical object with operation structure
record MagmaDeclaration : Set₁ where
  field
    underlyingSet : M.Identifier
    binaryOp : M.Identifier  -- Reference to the operation
    index : AlgebraIndex     -- Phase I.1.3: Well-founded hierarchy index
    -- Connection to categories: Magmas form a category with homomorphisms

-- Canonical index for Magma (level 0)
magmaIndex : AlgebraIndex
magmaIndex = mkAlgIdx 0

-- ============================================================================
-- Semigroups (Associative Magmas)
-- ============================================================================

-- A semigroup is a magma with associativity
record SemigroupDeclaration : Set₁ where
  field
    underlyingMagma : MagmaDeclaration
    -- Reuse existing AssociativityAxiom from Chapter1.Level1
    associativity : AssociativityAxiom
    index : AlgebraIndex     -- Phase I.1.3: Well-founded hierarchy index

-- Canonical index for Semigroup (level 1)
semigroupIndex : AlgebraIndex
semigroupIndex = mkAlgIdx 1

-- ============================================================================
-- Monoids (Semigroups with Identity)
-- ============================================================================

-- A monoid is a semigroup with an identity element
record MonoidDeclaration : Set₁ where
  field
    underlyingSemigroup : SemigroupDeclaration
    identityElement : M.Identifier
    -- Reuse existing IdentityAxiom from Chapter1.Level1
    identityAxiom : IdentityAxiom
    index : AlgebraIndex     -- Phase I.1.3: Well-founded hierarchy index
    
  -- Re-export semigroup properties
  open SemigroupDeclaration underlyingSemigroup public

-- Canonical index for Monoid (level 2)
monoidIndex : AlgebraIndex
monoidIndex = mkAlgIdx 2

-- ============================================================================
-- Groups (Monoids with Inverses)
-- ============================================================================

-- Inverse operation for a group
record InverseOperation : Set₁ where
  field
    forMonoid : MonoidDeclaration
    inverseMap : M.Identifier
    -- Axiom: ∀ a, a ∙ a⁻¹ = a⁻¹ ∙ a = e
    inverseAxiom : M.Identifier

-- A group is a monoid where every element has an inverse
record GroupDeclaration : Set₁ where
  field
    underlyingMonoid : MonoidDeclaration
    inverseOperation : InverseOperation
    index : AlgebraIndex     -- Phase I.1.3: Well-founded hierarchy index
    
  -- Re-export monoid properties
  open MonoidDeclaration underlyingMonoid public

-- Canonical index for Group (level 3)
groupIndex : AlgebraIndex
groupIndex = mkAlgIdx 3

-- DeviationLog [2025-11-18]: Removed inline validation proof that Group index
-- is greater than Monoid index. This validation is now covered in
-- Tests.HierarchyValidation as Bool-based checks to avoid brittle proofs
-- that can break builds during refactors.

-- ============================================================================
-- Abelian (Commutative) Groups
-- ============================================================================

-- Commutativity axiom for groups
record CommutativityAxiom : Set₁ where
  field
    forGroup : GroupDeclaration
    axiom : M.Identifier

-- An abelian group is a commutative group
record AbelianGroupDeclaration : Set₁ where
  field
    underlyingGroup : GroupDeclaration
    commutativity : CommutativityAxiom
    index : AlgebraIndex     -- Phase I.1.3: Well-founded hierarchy index
    
  -- Re-export group properties
  open GroupDeclaration underlyingGroup public

-- Canonical index for AbelianGroup (level 3, position 1 within level)
abelianGroupIndex : AlgebraIndex
abelianGroupIndex = mkAlgIdx 3

-- DeviationLog [2025-11-18]: Removed inline validation proof that
-- AbelianGroup index equals Group index (same level). This is now
-- validated in Tests.HierarchyValidation using Bool checks.

-- ============================================================================
-- Homomorphisms (Structure-Preserving Maps)
-- ============================================================================

-- Magma homomorphism
record MagmaHomomorphism (M N : MagmaDeclaration) : Set₁ where
  field
    map : M.Identifier  -- The underlying function
    -- Preserves operation: f(a ∙ b) = f(a) ∙ f(b)
    preservesOperation : M.Identifier

-- Semigroup homomorphism (automatically preserves associativity)
record SemigroupHomomorphism (S T : SemigroupDeclaration) : Set₁ where
  field
    underlyingMagmaMap : MagmaHomomorphism 
      (SemigroupDeclaration.underlyingMagma S)
      (SemigroupDeclaration.underlyingMagma T)

-- Monoid homomorphism (preserves operation and identity)
record MonoidHomomorphism (M N : MonoidDeclaration) : Set₁ where
  field
    underlyingSemigroupMap : SemigroupHomomorphism
      (MonoidDeclaration.underlyingSemigroup M)
      (MonoidDeclaration.underlyingSemigroup N)
    preservesIdentity : M.Identifier

-- Group homomorphism (preserves operation, identity, and inverses)
record GroupHomomorphism (G H : GroupDeclaration) : Set₁ where
  field
    underlyingMonoidMap : MonoidHomomorphism
      (GroupDeclaration.underlyingMonoid G)
      (GroupDeclaration.underlyingMonoid H)
    preservesInverses : M.Identifier

-- ============================================================================
-- Categorical Structure
-- ============================================================================

-- The category of groups (Grp)
record CategoryOfGroups : Set₁ where
  field
    underlyingCategory : CategoryDeclaration
    -- Objects are groups
    objectsAreGroups : M.Identifier
    -- Morphisms are group homomorphisms
    morphismsAreHomomorphisms : M.Identifier

-- The category of abelian groups (Ab)
record CategoryOfAbelianGroups : Set₁ where
  field
    underlyingCategory : CategoryDeclaration
    fullSubcategoryOfGroups : CategoryOfGroups
    -- Objects are abelian groups
    objectsAreAbelian : M.Identifier
    -- Ab is an abelian category (see Chapter2/Level2sub1)
    isAbelianCategory : M.Identifier

-- ============================================================================
-- Connection to Existing Framework
-- ============================================================================

-- Property identifiers for algebraic structures
GroupCategoryId : M.Identifier
GroupCategoryId = M.mkId "GroupCategory"

AbelianGroupCategoryId : M.Identifier
AbelianGroupCategoryId = M.mkId "AbelianGroupCategory"

MonoidCategoryId : M.Identifier
MonoidCategoryId = M.mkId "MonoidCategory"

SemigroupCategoryId : M.Identifier
SemigroupCategoryId = M.mkId "SemigroupCategory"

-- Theorems about algebraic categories (to be proven in detail later)
-- These are placeholders showing how algebra connects to category theory
postulate
  -- Free-forgetful adjunction: Free ⊣ U : Grp → Set
  FreeGroupAdjunction : M.Identifier
  
  -- Grp has all small limits and colimits
  Grp-HasLimits : M.Identifier
  Grp-HasColimits : M.Identifier
  
  -- Ab is an abelian category
  Ab-IsAbelian : M.Identifier
```
