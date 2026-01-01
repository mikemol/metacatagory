{-# OPTIONS --without-K #-}

-- | Group structure utilities (decompositions, fundamental theorems).
module Algebra.Groups.Structure where

open import Core
open import Algebra.Foundation
open import Algebra.Groups.Basic
open import Algebra.Groups.Free
open import Metamodel as M

-- Phase 1: finitely generated abelian decomposition records
record InvariantFactorDecomposition (A : FinitelyGeneratedAbelianGroup) : Set₁ where
  field
    freeRank         : M.Identifier
    invariantFactors : M.Identifier
    isomorphism      : M.Identifier

record ElementaryDivisorDecomposition (A : FinitelyGeneratedAbelianGroup) : Set₁ where
  field
    freeRank          : M.Identifier
    primePowerFactors : M.Identifier
    isomorphism       : M.Identifier

record TorsionSubgroup (A : AbelianGroupDeclaration) : Set₁ where
  field
    abelianGroup    : AbelianGroupDeclaration
    torsionElements : M.Identifier
    isSubgroup      : Subgroup (AbelianGroupDeclaration.underlyingGroup abelianGroup)

postulate FinitelyGeneratedAbelianStructurePackage : (A : FinitelyGeneratedAbelianGroup) → M.Identifier

-- Indecomposable groups
record IndecomposableGroup (G : GroupDeclaration) : Set₁ where
  field
    underlyingGroup  : GroupDeclaration
    isIndecomposable : M.Identifier

-- Basic group action records
record GroupAction (G : GroupDeclaration) (X : M.Identifier) : Set₁ where
    field
      group               : GroupDeclaration
      set                 : M.Identifier
      action              : M.Identifier
      identityAction      : M.Identifier
      compatibilityAction : M.Identifier

postulate KrullSchmidtPackage : (G : GroupDeclaration) → M.Identifier
postulate GroupActionCoreTheorems : (G : GroupDeclaration) → M.Identifier

-- Orbit of an element under an action
record Orbit (G : GroupDeclaration) (X : M.Identifier)
               (act : GroupAction G X) (x : M.Identifier) : Set₁ where
  field
    groupAction : GroupAction G X
    element     : M.Identifier
    orbitSet    : M.Identifier

-- Stabilizer subgroup of an element
record Stabilizer (G : GroupDeclaration) (X : M.Identifier)
                    (act : GroupAction G X) (x : M.Identifier) : Set₁ where
  field
    groupAction : GroupAction G X
    element     : M.Identifier
    stabilizer  : Subgroup (GroupAction.group groupAction)

-- View of a group action as a functor from delooping BG to Set
record GroupActionAsFunctor (G : GroupDeclaration) : Set₁ where
  field
    group     : GroupDeclaration
    delooping : M.Identifier
    toSet     : M.Identifier

postulate OrbitStabilizerTheorem : (G : GroupDeclaration) → M.Identifier

-- Sylow-related structure
record PGroup (p : M.Identifier) (G : GroupDeclaration) : Set₁ where
  field
    prime    : M.Identifier
    group    : GroupDeclaration
    isPGroup : M.Identifier

record SylowPSubgroup (p : M.Identifier) (G : GroupDeclaration) : Set₁ where
  field
    prime           : M.Identifier
    group           : GroupDeclaration
    subgroup        : Subgroup group
    subgroupAsGroup : GroupDeclaration
    isPGroup        : PGroup prime subgroupAsGroup
    isMaximal       : M.Identifier

postulate SylowTheoremsPackage : (G : GroupDeclaration) → (p : M.Identifier) → M.Identifier

-- Simple groups and composition series
record SimpleGroup (G : GroupDeclaration) : Set₁ where
  field
    group    : GroupDeclaration
    isSimple : M.Identifier

record CompositionSeries (G : GroupDeclaration) : Set₁ where
  field
    group            : GroupDeclaration
    series           : M.Identifier
    factorsAreSimple : M.Identifier

postulate FiniteSimpleGroupsClassification : M.Identifier
postulate CompositionSeriesPackage : (G : GroupDeclaration) → M.Identifier

-- Commutator and derived series
record CommutatorSubgroup (G : GroupDeclaration) : Set₁ where
  field
    group              : GroupDeclaration
    commutatorSubgroup : NormalSubgroup group

record DerivedSeries (G : GroupDeclaration) : Set₁ where
  field
    group  : GroupDeclaration
    series : M.Identifier

-- Solvable groups
record SolvableGroup (G : GroupDeclaration) : Set₁ where
  field
    group         : GroupDeclaration
    derivedSeries : DerivedSeries group
    isSolvable    : M.Identifier

-- Nilpotent groups
record LowerCentralSeries (G : GroupDeclaration) : Set₁ where
  field
    group  : GroupDeclaration
    series : M.Identifier

record NilpotentGroup (G : GroupDeclaration) : Set₁ where
  field
    group              : GroupDeclaration
    lowerCentralSeries : LowerCentralSeries group
    isNilpotent        : M.Identifier

postulate NilpotentSolvablePackage : (G : GroupDeclaration) → (p : M.Identifier) → M.Identifier

-- Normal and subnormal series
record NormalSeries (G : GroupDeclaration) : Set₁ where
  field
    group        : GroupDeclaration
    series       : M.Identifier
    eachIsNormal : M.Identifier

record SubnormalSeries (G : GroupDeclaration) : Set₁ where
  field
    group               : GroupDeclaration
    series              : M.Identifier
    successiveNormality : M.Identifier

record SeriesRefinement (G : GroupDeclaration) (S : NormalSeries G) : Set₁ where
  field
    originalSeries : NormalSeries G
    refinedSeries  : NormalSeries (NormalSeries.group originalSeries)
    isRefinement   : M.Identifier

postulate SchreierRefinementTheorem : (G : GroupDeclaration) → (S₁ S₂ : NormalSeries G) → M.Identifier

-- Additional structure theorems
postulate JordanHolderTheorem : (G : GroupDeclaration) → (S₁ S₂ : CompositionSeries G) → M.Identifier
postulate NilpotentImpliesSolvable : (G : GroupDeclaration) → (N : NilpotentGroup G) → M.Identifier
postulate PGroupsAreNilpotent : (p : M.Identifier) → (G : GroupDeclaration) → (P : PGroup p G) → M.Identifier
postulate SolvableViaAbelianQuotients : (G : GroupDeclaration) → M.Identifier

-- Categorical and homological perspectives
postulate FGAbelianAsLawvereModels : M.Identifier
postulate GroupActionFunctorCorrespondence : (G : GroupDeclaration) → M.Identifier
postulate SylowCategoricalPerspective : M.Identifier
postulate CompositionSeriesAsFiltration : (G : GroupDeclaration) → M.Identifier
postulate SolvableAsIteratedExtension : (G : GroupDeclaration) → M.Identifier
postulate NilpotentAsCentralExtension : (G : GroupDeclaration) → M.Identifier
postulate GroupExtensionsAndCohomology : M.Identifier
