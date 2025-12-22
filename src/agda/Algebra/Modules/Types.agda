{-# OPTIONS --without-K #-}

module Algebra.Modules.Types where

open import Core
open import Algebra.Foundation
open import Algebra.Rings.Types
open import Metamodel as M

-- ============================================================================
-- Module-Specific Type Definitions (Hungerford Ch IV)
-- ============================================================================

-- Left R-module
record LeftModule (R : RingDeclaration) : Set₁ where
  field
    ring : RingDeclaration
    underlyingAbelianGroup : AbelianGroupDeclaration
    scalarMultiplication : M.Identifier
    distributiveOverAddition : M.Identifier
    distributiveOverRingAddition : M.Identifier
    associativeScalar : M.Identifier
    unitalAction : M.Identifier

-- Right R-module
record RightModule (R : RingDeclaration) : Set₁ where
  field
    ring : RingDeclaration
    underlyingAbelianGroup : AbelianGroupDeclaration
    scalarMultiplication : M.Identifier
    distributiveOverAddition : M.Identifier
    distributiveOverRingAddition : M.Identifier
    associativeScalar : M.Identifier
    unitalAction : M.Identifier

-- Module homomorphism
record ModuleHomomorphism (R : RingDeclaration) (M N : LeftModule R) : Set₁ where
  field
    ring : RingDeclaration
    sourceModule : LeftModule ring
    targetModule : LeftModule ring
    morphism : M.Identifier
    preservesAddition : M.Identifier
    preservesScalarMultiplication : M.Identifier

-- Submodule
record Submodule (R : RingDeclaration) (M : LeftModule R) : Set₁ where
  field
    ring : RingDeclaration
    module' : LeftModule ring
    subgroupStructure : M.Identifier
    closedUnderScalars : M.Identifier

-- Quotient module
record QuotientModule (R : RingDeclaration) (M : LeftModule R) (N : Submodule R M) : Set₁ where
  field
    ring : RingDeclaration
    module' : LeftModule ring
    submodule : Submodule ring module'
    quotientModule : LeftModule ring
    canonicalProjection : M.Identifier

-- Kernel and Image of homomorphism
record KernelOfModuleHomomorphism (R : RingDeclaration) (f : M.Identifier) : Set₁ where
  field
    ring : RingDeclaration
    sourceModule : LeftModule ring
    targetModule : LeftModule ring
    kernel : Submodule ring sourceModule

record ImageOfModuleHomomorphism (R : RingDeclaration) (f : M.Identifier) : Set₁ where
  field
    ring : RingDeclaration
    sourceModule : LeftModule ring
    targetModule : LeftModule ring
    image : Submodule ring targetModule

-- Cokernel
record CokernelOfModuleHomomorphism (R : RingDeclaration) (f : M.Identifier) : Set₁ where
  field
    ring : RingDeclaration
    sourceModule : LeftModule ring
    targetModule : LeftModule ring
    cokernel : LeftModule ring

-- Exact sequence
record ExactSequence (R : RingDeclaration) : Set₁ where
  field
    ring : RingDeclaration
    modules : M.Identifier
    morphisms : M.Identifier
    exactnessCondition : M.Identifier

-- Short exact sequence
record ShortExactSequence (R : RingDeclaration) : Set₁ where
  field
    ring : RingDeclaration
    leftModule : LeftModule ring
    middleModule : LeftModule ring
    rightModule : LeftModule ring
    leftMap : M.Identifier
    rightMap : M.Identifier
    exactness : M.Identifier

-- Category of modules
record CategoryOfModules (R : RingDeclaration) : Set₁ where
  field
    ring : RingDeclaration
    category : M.Identifier
    isAbelian : M.Identifier

-- Free module
record FreeModule (R : RingDeclaration) (X : M.Identifier) : Set₁ where
  field
    ring : RingDeclaration
    basis : M.Identifier
    freeModule : LeftModule ring
    universalProperty : M.Identifier

-- Projective module
record ProjectiveModule (R : RingDeclaration) (P : LeftModule R) : Set₁ where
  field
    ring : RingDeclaration
    module' : LeftModule ring
    liftingProperty : M.Identifier

-- Injective module
record InjectiveModule (R : RingDeclaration) (I : LeftModule R) : Set₁ where
  field
    ring : RingDeclaration
    module' : LeftModule ring
    extensionProperty : M.Identifier

-- Hom functor
record HomFunctor (R : RingDeclaration) (M : LeftModule R) : Set₁ where
  field
    ring : RingDeclaration
    fixedModule : LeftModule ring
    homAsAbelianGroup : M.Identifier
    homAsModule : M.Identifier
    functoriality : M.Identifier

-- Tensor product
record TensorProduct (R : CommutativeRingDeclaration) (M N : LeftModule (UnitalRingDeclaration.underlyingRing (CommutativeRingDeclaration.underlyingRing R))) : Set₁ where
  field
    ring : CommutativeRingDeclaration
    leftModule : LeftModule (UnitalRingDeclaration.underlyingRing (CommutativeRingDeclaration.underlyingRing ring))
    rightModule : LeftModule (UnitalRingDeclaration.underlyingRing (CommutativeRingDeclaration.underlyingRing ring))
    tensorProduct : LeftModule (UnitalRingDeclaration.underlyingRing (CommutativeRingDeclaration.underlyingRing ring))
    universalBilinearMap : M.Identifier
    universalProperty : M.Identifier

-- Torsion element
record TorsionElement (R : IntegralDomain) (M : LeftModule (UnitalRingDeclaration.underlyingRing (CommutativeRingDeclaration.underlyingRing (IntegralDomain.underlyingRing R)))) (a : M.Identifier) : Set₁ where
  field
    domain : IntegralDomain
    module' : LeftModule (UnitalRingDeclaration.underlyingRing (CommutativeRingDeclaration.underlyingRing (IntegralDomain.underlyingRing domain)))
    element : M.Identifier
    isTorsion : M.Identifier
