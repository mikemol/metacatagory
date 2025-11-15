module PropertyRegistry where

open import Metamodel as M

-- Stable identifiers for commonly used properties/classes/operations
-- These are plain Identifiers to be used with the generic scaffolding subjects.

-- Category-level properties
RegularCategoryId : M.Identifier
RegularCategoryId = "RegularCategory"

ExactCategoryId : M.Identifier
ExactCategoryId = "ExactCategory"

BarrExactCategoryId : M.Identifier
BarrExactCategoryId = "BarrExactCategory"

FiniteLimitsId : M.Identifier
FiniteLimitsId = "FiniteLimits"

-- Relation effectiveness property
EffectiveEquivalenceRelationsId : M.Identifier
EffectiveEquivalenceRelationsId = "EffectiveEquivalenceRelations"

-- Morphism classes and closure notions
RegularEpiClassId : M.Identifier
RegularEpiClassId = "RegularEpi"

-- Operations / contexts for closure and stability
CompositionOpId : M.Identifier
CompositionOpId = "Composition"

PullbackContextId : M.Identifier
PullbackContextId = "Pullback"

-- Chapter 3: Algebraic categories and related properties
AlgebraicCategoryId : M.Identifier
AlgebraicCategoryId = "AlgebraicCategory"

CompleteCategoryId : M.Identifier
CompleteCategoryId = "Complete"

CocompleteCategoryId : M.Identifier
CocompleteCategoryId = "Cocomplete"

SymmetricMonoidalCategoryId : M.Identifier
SymmetricMonoidalCategoryId = "SymmetricMonoidalCategory"

-- Free/forgetful adjunction existence (algebraic categories)
HasFreeForgetfulAdjunctionId : M.Identifier
HasFreeForgetfulAdjunctionId = "HasFreeForgetfulAdjunction"

-- Lattice-level properties (used for Subobject lattices etc.)
AlgebraicLatticeId : M.Identifier
AlgebraicLatticeId = "AlgebraicLattice"

-- Beck characterization: generator properties
HasRegularProjectiveGeneratorId : M.Identifier
HasRegularProjectiveGeneratorId = "HasRegularProjectiveGenerator"

RegularlyCoveredByGeneratorId : M.Identifier
RegularlyCoveredByGeneratorId = "RegularlyCoveredByGenerator"

-- Category has a (possibly non-regular-projective) generator
HasGeneratorId : M.Identifier
HasGeneratorId = "HasGenerator"

-- Functor-level properties
AlgebraicFunctorId : M.Identifier
AlgebraicFunctorId = "AlgebraicFunctor"
