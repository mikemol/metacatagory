{-# OPTIONS --without-K #-}

-- | Group structure utilities (decompositions, fundamental theorems).
module Algebra.Groups.Structure where

open import Core
open import Algebra.Foundation
open import Algebra.Groups.Basic
open import Algebra.Groups.Free
open import Metamodel as M

-- Phase 1: finitely generated abelian decomposition records
-- | Invariant factor decomposition of a finitely generated abelian group.
record InvariantFactorDecomposition (A : FinitelyGeneratedAbelianGroup) : Set₁ where
  field
    freeRank         : M.Identifier
    invariantFactors : M.Identifier
    isomorphism      : M.Identifier

-- | Elementary divisor decomposition of a finitely generated abelian group.
record ElementaryDivisorDecomposition (A : FinitelyGeneratedAbelianGroup) : Set₁ where
  field
    freeRank          : M.Identifier
    primePowerFactors : M.Identifier
    isomorphism       : M.Identifier

-- | The torsion subgroup of an abelian group.
record TorsionSubgroup (A : AbelianGroupDeclaration) : Set₁ where
  field
    abelianGroup    : AbelianGroupDeclaration
    torsionElements : M.Identifier
    isSubgroup      : Subgroup (AbelianGroupDeclaration.underlyingGroup abelianGroup)

-- | Package of structure theorems for finitely generated abelian groups.
postulate FinitelyGeneratedAbelianStructurePackage : (A : FinitelyGeneratedAbelianGroup) → M.Identifier

-- Indecomposable groups
-- | Witness that a group is indecomposable.
record IndecomposableGroup (G : GroupDeclaration) : Set₁ where
  field
    underlyingGroup  : GroupDeclaration
    isIndecomposable : M.Identifier

-- Basic group action records
-- | Group action of G on a set X with coherence proofs.
record GroupAction (G : GroupDeclaration) (X : M.Identifier) : Set₁ where
    field
      group               : GroupDeclaration
      set                 : M.Identifier
      action              : M.Identifier
      identityAction      : M.Identifier
      compatibilityAction : M.Identifier

-- | Krull–Schmidt decomposition package.
postulate KrullSchmidtPackage : (G : GroupDeclaration) → M.Identifier
-- | Core action theorems (orbits, stabilizers, etc.).
postulate GroupActionCoreTheorems : (G : GroupDeclaration) → M.Identifier

-- Orbit of an element under an action
-- | Orbit of x under a group action.
record Orbit (G : GroupDeclaration) (X : M.Identifier)
               (act : GroupAction G X) (x : M.Identifier) : Set₁ where
  field
    groupAction : GroupAction G X
    element     : M.Identifier
    orbitSet    : M.Identifier

-- Stabilizer subgroup of an element
-- | Stabilizer subgroup of x under the action.
record Stabilizer (G : GroupDeclaration) (X : M.Identifier)
                    (act : GroupAction G X) (x : M.Identifier) : Set₁ where
  field
    groupAction : GroupAction G X
    element     : M.Identifier
    stabilizer  : Subgroup (GroupAction.group groupAction)

-- View of a group action as a functor from delooping BG to Set
-- | Functorial view of a group action from BG to Set.
record GroupActionAsFunctor (G : GroupDeclaration) : Set₁ where
  field
    group     : GroupDeclaration
    delooping : M.Identifier
    toSet     : M.Identifier

-- | Orbit–stabilizer theorem witness.
postulate OrbitStabilizerTheorem : (G : GroupDeclaration) → M.Identifier

-- Sylow-related structure
-- | A p-group declaration with prime p.
record PGroup (p : M.Identifier) (G : GroupDeclaration) : Set₁ where
  field
    prime    : M.Identifier
    group    : GroupDeclaration
    isPGroup : M.Identifier

-- | Sylow p-subgroup with maximality proof.
record SylowPSubgroup (p : M.Identifier) (G : GroupDeclaration) : Set₁ where
  field
    prime           : M.Identifier
    group           : GroupDeclaration
    subgroup        : Subgroup group
    subgroupAsGroup : GroupDeclaration
    isPGroup        : PGroup prime subgroupAsGroup
    isMaximal       : M.Identifier

-- | Package of Sylow theorems for a group and prime.
postulate SylowTheoremsPackage : (G : GroupDeclaration) → (p : M.Identifier) → M.Identifier

-- Simple groups and composition series
-- | Simple group witness.
record SimpleGroup (G : GroupDeclaration) : Set₁ where
  field
    group    : GroupDeclaration
    isSimple : M.Identifier

-- | Composition series with simple factors.
record CompositionSeries (G : GroupDeclaration) : Set₁ where
  field
    group            : GroupDeclaration
    series           : M.Identifier
    factorsAreSimple : M.Identifier

-- | Pointer to the finite simple groups classification.
postulate FiniteSimpleGroupsClassification : M.Identifier
-- | Package of composition series theorems for a group.
postulate CompositionSeriesPackage : (G : GroupDeclaration) → M.Identifier

-- Commutator and derived series
-- | Commutator subgroup of a group.
record CommutatorSubgroup (G : GroupDeclaration) : Set₁ where
  field
    group              : GroupDeclaration
    commutatorSubgroup : NormalSubgroup group

-- | Derived series of a group.
record DerivedSeries (G : GroupDeclaration) : Set₁ where
  field
    group  : GroupDeclaration
    series : M.Identifier

-- Solvable groups
-- | Solvable group via derived series.
record SolvableGroup (G : GroupDeclaration) : Set₁ where
  field
    group         : GroupDeclaration
    derivedSeries : DerivedSeries group
    isSolvable    : M.Identifier

-- Nilpotent groups
-- | Lower central series of a group.
record LowerCentralSeries (G : GroupDeclaration) : Set₁ where
  field
    group  : GroupDeclaration
    series : M.Identifier

-- | Nilpotent group with lower central series witness.
record NilpotentGroup (G : GroupDeclaration) : Set₁ where
  field
    group              : GroupDeclaration
    lowerCentralSeries : LowerCentralSeries group
    isNilpotent        : M.Identifier

-- | Package linking nilpotent/solvable properties.
postulate NilpotentSolvablePackage : (G : GroupDeclaration) → (p : M.Identifier) → M.Identifier

-- Normal and subnormal series
-- | Normal series of a group.
record NormalSeries (G : GroupDeclaration) : Set₁ where
  field
    group        : GroupDeclaration
    series       : M.Identifier
    eachIsNormal : M.Identifier

-- | Subnormal series with successive normality proofs.
record SubnormalSeries (G : GroupDeclaration) : Set₁ where
  field
    group               : GroupDeclaration
    series              : M.Identifier
    successiveNormality : M.Identifier

-- | Refinement of a normal series.
record SeriesRefinement (G : GroupDeclaration) (S : NormalSeries G) : Set₁ where
  field
    originalSeries : NormalSeries G
    refinedSeries  : NormalSeries (NormalSeries.group originalSeries)
    isRefinement   : M.Identifier

-- | Schreier refinement theorem witness.
postulate SchreierRefinementTheorem : (G : GroupDeclaration) → (S₁ S₂ : NormalSeries G) → M.Identifier

-- Additional structure theorems
-- | Jordan–Hölder theorem witness.
postulate JordanHolderTheorem : (G : GroupDeclaration) → (S₁ S₂ : CompositionSeries G) → M.Identifier
-- | Proof that nilpotent groups are solvable.
postulate NilpotentImpliesSolvable : (G : GroupDeclaration) → (N : NilpotentGroup G) → M.Identifier
-- | Proof that p-groups are nilpotent.
postulate PGroupsAreNilpotent : (p : M.Identifier) → (G : GroupDeclaration) → (P : PGroup p G) → M.Identifier
-- | Solvable characterization via abelian quotients.
postulate SolvableViaAbelianQuotients : (G : GroupDeclaration) → M.Identifier

-- Categorical and homological perspectives
-- | Treat finitely generated abelian groups as Lawvere theory models.
postulate FGAbelianAsLawvereModels : M.Identifier
-- | Correspondence between actions and functors from BG.
postulate GroupActionFunctorCorrespondence : (G : GroupDeclaration) → M.Identifier
-- | Categorical view on Sylow theory.
postulate SylowCategoricalPerspective : M.Identifier
-- | View composition series as a filtration.
postulate CompositionSeriesAsFiltration : (G : GroupDeclaration) → M.Identifier
-- | Solvable groups as iterated extensions.
postulate SolvableAsIteratedExtension : (G : GroupDeclaration) → M.Identifier
-- | Nilpotent groups as central extensions.
postulate NilpotentAsCentralExtension : (G : GroupDeclaration) → M.Identifier
-- | Group extensions connected to cohomology.
postulate GroupExtensionsAndCohomology : M.Identifier
