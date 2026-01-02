{-# OPTIONS --without-K #-}

-- | Module-theory types (modules, submodules, bases) used across algebra.
module Algebra.Modules.Types where

open import Core
open import Algebra.Foundation
open import Algebra.Rings.Types
open import Algebra.Fields.Types
open import Algebra.Groups.Types
open import Metamodel as M

-- ============================================================================
-- Module-Specific Type Definitions (Hungerford Ch IV)
-- ============================================================================

-- Left R-module
-- | Structure of a left module over a ring R.
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
-- | Structure of a right module over a ring R.
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
    subgroup : Subgroup (AbelianGroupDeclaration.underlyingGroup (LeftModule.underlyingAbelianGroup module'))
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
-- Vector space over field (module over a field)
record VectorSpace (F : FieldDeclaration) : Set₁ where
  field
    field' : FieldDeclaration
    underlyingModule : LeftModule (UnitalRingDeclaration.underlyingRing (CommutativeRingDeclaration.underlyingRing (FieldDeclaration.underlyingRing field')))

-- Basis of vector space
record BasisOfVectorSpace (F : FieldDeclaration) (V : VectorSpace F) : Set₁ where
  field
    field' : FieldDeclaration
    vectorSpace : VectorSpace field'
    basisSet : M.Identifier
    linearIndependence : M.Identifier
    spanning : M.Identifier

-- Dimension of vector space (basis cardinality)
record Dimension (F : FieldDeclaration) (V : VectorSpace F) : Set₁ where
  field
    field' : FieldDeclaration
    vectorSpace : VectorSpace field'
    dimension : M.Identifier
-- Torsion submodule
record TorsionSubmodule (R : IntegralDomain) (M : LeftModule (UnitalRingDeclaration.underlyingRing (CommutativeRingDeclaration.underlyingRing (IntegralDomain.underlyingRing R)))) : Set₁ where
  field
    domain : IntegralDomain
    module' : LeftModule (UnitalRingDeclaration.underlyingRing (CommutativeRingDeclaration.underlyingRing (IntegralDomain.underlyingRing domain)))
    torsionSubmodule : Submodule (UnitalRingDeclaration.underlyingRing (CommutativeRingDeclaration.underlyingRing (IntegralDomain.underlyingRing domain))) module'

-- Torsion-free module
record TorsionFreeModule (R : IntegralDomain) (M : LeftModule (UnitalRingDeclaration.underlyingRing (CommutativeRingDeclaration.underlyingRing (IntegralDomain.underlyingRing R)))) : Set₁ where
  field
    domain : IntegralDomain
    module' : LeftModule (UnitalRingDeclaration.underlyingRing (CommutativeRingDeclaration.underlyingRing (IntegralDomain.underlyingRing domain)))
    isTorsionFree : M.Identifier

-- Structure theorem for finitely generated modules over PID
record StructureTheoremPID (R : PrincipalIdealDomain) (M : LeftModule (UnitalRingDeclaration.underlyingRing (CommutativeRingDeclaration.underlyingRing (IntegralDomain.underlyingRing (PrincipalIdealDomain.domain R))))) : Set₁ where
  field
    pid : PrincipalIdealDomain
    module' : LeftModule (UnitalRingDeclaration.underlyingRing (CommutativeRingDeclaration.underlyingRing (IntegralDomain.underlyingRing (PrincipalIdealDomain.domain pid))))
    decomposition : M.Identifier

-- Dual module M* = Hom_R(M, R)
record DualModule (R : RingDeclaration) (M : LeftModule R) : Set₁ where
  field
    ring : RingDeclaration
    module' : LeftModule ring
    dualModule : LeftModule ring

-- Reflexive module
record ReflexiveModule (R : RingDeclaration) (M : LeftModule R) : Set₁ where
  field
    ring : RingDeclaration
    module' : LeftModule ring
    isReflexive : M.Identifier

-- Free module functor (F : Set → R-Mod)
record FreeModuleFunctor (R : RingDeclaration) : Set₁ where
  field
    ring : RingDeclaration
    onObjects : M.Identifier
    onMorphisms : M.Identifier
    preservesIdentity : M.Identifier
    preservesComposition : M.Identifier

-- Forgetful module functor (U : R-Mod → Set)
record ForgetfulModuleFunctor (R : RingDeclaration) : Set₁ where
  field
    ring : RingDeclaration
    onObjects : M.Identifier
    onMorphisms : M.Identifier
    preservesIdentity : M.Identifier
    preservesComposition : M.Identifier

-- R-algebra (ring with compatible R-module structure)
record RAlgebra (R : CommutativeRingDeclaration) : Set₁ where
  field
    coefficientRing : CommutativeRingDeclaration
    underlyingRing : RingDeclaration
    moduleStructure : LeftModule (UnitalRingDeclaration.underlyingRing (CommutativeRingDeclaration.underlyingRing coefficientRing))
    compatibility : M.Identifier

-- Algebra homomorphism
record AlgebraHomomorphism (R : CommutativeRingDeclaration) (A B : RAlgebra R) : Set₁ where
  field
    coefficientRing : CommutativeRingDeclaration
    sourceAlgebra : RAlgebra coefficientRing
    targetAlgebra : RAlgebra coefficientRing
    ringHomomorphism : M.Identifier
    moduleHomomorphism : M.Identifier
