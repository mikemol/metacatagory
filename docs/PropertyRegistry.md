```Agda
module PropertyRegistry where

open import Metamodel as M

-- Stable identifiers for commonly used properties/classes/operations
-- These are plain Identifiers to be used with the generic scaffolding subjects.

-- Category-level properties
RegularCategoryId : M.Identifier
RegularCategoryId = M.mkId "RegularCategory"

ExactCategoryId : M.Identifier
ExactCategoryId = M.mkId "ExactCategory"

BarrExactCategoryId : M.Identifier
BarrExactCategoryId = M.mkId "BarrExactCategory"

FiniteLimitsId : M.Identifier
FiniteLimitsId = M.mkId "FiniteLimits"

-- Relation effectiveness property
EffectiveEquivalenceRelationsId : M.Identifier
EffectiveEquivalenceRelationsId = M.mkId "EffectiveEquivalenceRelations"

-- Morphism classes and closure notions
RegularEpiClassId : M.Identifier
RegularEpiClassId = M.mkId "RegularEpi"

-- Operations / contexts for closure and stability
CompositionOpId : M.Identifier
CompositionOpId = M.mkId "Composition"

PullbackContextId : M.Identifier
PullbackContextId = M.mkId "Pullback"

-- Chapter 3: Algebraic categories and related properties
AlgebraicCategoryId : M.Identifier
AlgebraicCategoryId = M.mkId "AlgebraicCategory"

AbelianCategoryId : M.Identifier
AbelianCategoryId = M.mkId "AbelianCategory"

CompleteCategoryId : M.Identifier
CompleteCategoryId = M.mkId "Complete"

CocompleteCategoryId : M.Identifier
CocompleteCategoryId = M.mkId "Cocomplete"

SymmetricMonoidalCategoryId : M.Identifier
SymmetricMonoidalCategoryId = M.mkId "SymmetricMonoidalCategory"

-- Free/forgetful adjunction existence (algebraic categories)
HasFreeForgetfulAdjunctionId : M.Identifier
HasFreeForgetfulAdjunctionId = M.mkId "HasFreeForgetfulAdjunction"

-- Lattice-level properties (used for Subobject lattices etc.)
AlgebraicLatticeId : M.Identifier
AlgebraicLatticeId = M.mkId "AlgebraicLattice"

-- Beck characterization: generator properties
HasRegularProjectiveGeneratorId : M.Identifier
HasRegularProjectiveGeneratorId = M.mkId "HasRegularProjectiveGenerator"

RegularlyCoveredByGeneratorId : M.Identifier
RegularlyCoveredByGeneratorId = M.mkId "RegularlyCoveredByGenerator"

-- Category has a (possibly non-regular-projective) generator
HasGeneratorId : M.Identifier
HasGeneratorId = M.mkId "HasGenerator"

-- Functor-level properties
AlgebraicFunctorId : M.Identifier
AlgebraicFunctorId = M.mkId "AlgebraicFunctor"
```
