-- Tests.PropertyRegistryTests: Validate property registry identifiers and usage
--
-- This suite ensures that stable identifiers in PropertyRegistry are
-- correctly typed, usable across the codebase where generic identifiers
-- are expected, and remain consistent through simple compositions.

module Tests.PropertyRegistryTests where

open import Metamodel as M
open import PropertyRegistry

-- ============================================================================
-- Phase 1: Identifier Existence and Typing
-- All registry entries should be plain Identifiers
-- ============================================================================

module Phase1-Typing where

  -- Category-level properties
  regCatId : M.Identifier
  regCatId = RegularCategoryId

  exactCatId : M.Identifier
  exactCatId = ExactCategoryId

  barrExactCatId : M.Identifier
  barrExactCatId = BarrExactCategoryId

  finiteLimitsId : M.Identifier
  finiteLimitsId = FiniteLimitsId

  -- Relation effectiveness
  effEqRelId : M.Identifier
  effEqRelId = EffectiveEquivalenceRelationsId

  -- Morphism classes and closure contexts
  regEpiClassId : M.Identifier
  regEpiClassId = RegularEpiClassId

  compositionCtxId : M.Identifier
  compositionCtxId = CompositionOpId

  pullbackCtxId : M.Identifier
  pullbackCtxId = PullbackContextId

  -- Algebraic/categorical properties
  algebraicCatId : M.Identifier
  algebraicCatId = AlgebraicCategoryId

  abelianCatId : M.Identifier
  abelianCatId = AbelianCategoryId

  completeCatId : M.Identifier
  completeCatId = CompleteCategoryId

  cocompleteCatId : M.Identifier
  cocompleteCatId = CocompleteCategoryId

  symMonoidalCatId : M.Identifier
  symMonoidalCatId = SymmetricMonoidalCategoryId

  freeForgetfulAdjId : M.Identifier
  freeForgetfulAdjId = HasFreeForgetfulAdjunctionId

  -- Lattice/generator properties
  algebraicLatticeId' : M.Identifier
  algebraicLatticeId' = AlgebraicLatticeId

  hasRegProjGenId : M.Identifier
  hasRegProjGenId = HasRegularProjectiveGeneratorId

  regularlyCoveredByGenId : M.Identifier
  regularlyCoveredByGenId = RegularlyCoveredByGeneratorId

  hasGeneratorId : M.Identifier
  hasGeneratorId = HasGeneratorId

  -- Functor-level
  algebraicFunctorId' : M.Identifier
  algebraicFunctorId' = AlgebraicFunctorId

-- ============================================================================
-- Phase 2: Generic Consumption
-- Registry identifiers should be consumable by APIs that accept Identifiers
-- ============================================================================

module Phase2-GenericConsumption where

  -- A generic consumer of identifiers (e.g., tagging, lookup keys)
  postulate
    consumeId : M.Identifier → M.Identifier

  -- Using several registry identifiers with the consumer
  use-regular-category : M.Identifier
  use-regular-category = consumeId RegularCategoryId

  use-exact-category : M.Identifier
  use-exact-category = consumeId ExactCategoryId

  use-abelian-category : M.Identifier
  use-abelian-category = consumeId AbelianCategoryId

  use-algebraic-functor : M.Identifier
  use-algebraic-functor = consumeId AlgebraicFunctorId

-- ============================================================================
-- Phase 3: Stability Under Simple Composition
-- Identifiers should remain well-typed when combined structurally
-- (we don’t assert equality; we assert type safety across compositions)
-- ============================================================================

module Phase3-Composition where

  -- Pairing two identifiers to model composite tags
  record Pair : Set where
    constructor _,_
    field
      fst : M.Identifier
      snd : M.Identifier

  open Pair

  pair-regular-exact : Pair
  pair-regular-exact = RegularCategoryId , ExactCategoryId

  pair-abelian-limits : Pair
  pair-abelian-limits = AbelianCategoryId , FiniteLimitsId

  -- A simple projector-based consumer
  postulate
    consumePair : Pair → M.Identifier

  use-pair : M.Identifier
  use-pair = consumePair pair-regular-exact

-- ============================================================================
-- Summary
-- ============================================================================

-- This suite is intentionally lightweight: it validates that the
-- property registry provides stable, well-typed identifiers that can be
-- used uniformly wherever a generic Identifier is required.
