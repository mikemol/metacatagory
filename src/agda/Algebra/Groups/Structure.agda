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
      carrier             : M.Identifier
      action              : M.Identifier
      identityAction      : M.Identifier
      compatibilityAction : M.Identifier

postulate KrullSchmidtPackage : (G : GroupDeclaration) → M.Identifier
postulate GroupActionCoreTheorems : (G : GroupDeclaration) → M.Identifier

-- Orbit of an element under an action
record Orbit (G : GroupDeclaration) (X : M.Identifier)
               (act : GroupAction G X) (x : M.Identifier) : Set₁ where
  field
    actionRecord : GroupAction G X
    element      : M.Identifier
    orbitSet     : M.Identifier

-- Stabilizer subgroup of an element
record Stabilizer (G : GroupDeclaration) (X : M.Identifier)
                    (act : GroupAction G X) (x : M.Identifier) : Set₁ where
  field
    actionRecord : GroupAction G X
    element      : M.Identifier
    stabilizer   : Subgroup (GroupAction.group actionRecord)

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
    carrier  : GroupDeclaration
    witness  : M.Identifier

record SylowPSubgroup (p : M.Identifier) (G : GroupDeclaration) : Set₁ where
  field
    prime            : M.Identifier
    ambient          : GroupDeclaration
    subgroup         : Subgroup ambient
    inducedGroup     : GroupDeclaration
    pGroupStructure  : PGroup prime inducedGroup
    maximalProperty  : M.Identifier

postulate SylowTheoremsPackage : (G : GroupDeclaration) → (p : M.Identifier) → M.Identifier
