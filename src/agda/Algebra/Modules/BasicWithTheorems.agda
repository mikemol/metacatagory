{-# OPTIONS --without-K #-}

-- Algebra.Modules.BasicWithTheorems: Modules with explicit theorem sourcing
--
-- Refactored version making theorem dependencies explicit.

module Algebra.Modules.BasicWithTheorems where

open import Core
open import Chapter1.Level1Index
open import Chapter2.Level2sub1
open import Algebra.Foundation
open import Algebra.Groups.Basic
open import Algebra.Groups.Abelian
open import Algebra.Rings.Basic
open import PropertyRegistry
open import Metamodel as M
open import Algebra.Modules.Theorems.Classical

-- ============================================================================
-- IV.1: Modules, Homomorphisms and Exact Sequences
-- ============================================================================

-- Concrete Examples (for testing and verification)
module ConcreteExamples where
  -- | Integers viewed as a ℤ-module over itself.
  ZAsZModule : M.Identifier
  ZAsZModule = M.mkId "ℤ-as-ℤ-module"
  
  -- | ℤ/nℤ as a module over ℤ, parameterized by n.
  postulate ZnZAsZModule : M.Identifier → M.Identifier
  -- | R^n module constructor.
  postulate RPowerN : M.Identifier → M.Identifier → M.Identifier
  -- | F as a vector space over itself.
  postulate FAsFVectorspace : M.Identifier → M.Identifier

open ConcreteExamples public

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

-- Module homomorphism (R-linear map)
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

-- R-Mod categorical properties
-- Sourced from module: rModCategoricalProperties

-- ============================================================================
-- IV.2: Free Modules and Vector Spaces
-- ============================================================================

-- Free module
record FreeModule (R : RingDeclaration) (X : M.Identifier) : Set₁ where
  field
    ring : RingDeclaration
    basis : M.Identifier
    freeModule : LeftModule ring
    universalProperty : M.Identifier

-- Free functor F : Set → R-Mod
record FreeModuleFunctor (R : RingDeclaration) : Set₁ where
  field
    ring : RingDeclaration
    onObjects : M.Identifier
    onMorphisms : M.Identifier
    preservesIdentity : M.Identifier
    preservesComposition : M.Identifier

-- Forgetful functor U : R-Mod → Set
record ForgetfulModuleFunctor (R : RingDeclaration) : Set₁ where
  field
    ring : RingDeclaration
    onObjects : M.Identifier
    onMorphisms : M.Identifier
    preservesIdentity : M.Identifier
    preservesComposition : M.Identifier

-- F ⊣ U adjunction
-- Sourced from module: freeModuleAdjunction

-- Vector space (module over a field)
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

-- Dimension of vector space
record Dimension (F : FieldDeclaration) (V : VectorSpace F) : Set₁ where
  field
    field' : FieldDeclaration
    vectorSpace : VectorSpace field'
    dimension : M.Identifier

-- Basis cardinality invariant (dimension well-definedness)
-- Sourced from module: basisCardinalityInvariant

-- ============================================================================
-- IV.3: Projective and Injective Modules
-- ============================================================================

-- Projective module (categorical definition)
record ProjectiveModule (R : RingDeclaration) (P : LeftModule R) : Set₁ where
  field
    ring : RingDeclaration
    module' : LeftModule ring
    liftingProperty : M.Identifier

-- Free modules are projective
-- Sourced from module: freeImplesProjective

-- Injective module (categorical definition)
record InjectiveModule (R : RingDeclaration) (I : LeftModule R) : Set₁ where
  field
    ring : RingDeclaration
    module' : LeftModule ring
    extensionProperty : M.Identifier

-- Projective and injective properties
-- Sourced from module: projectiveInjectiveProperties

-- ============================================================================
-- IV.4: Hom and Duality
-- ============================================================================

-- Hom functor Hom_R(M, -)
record HomFunctor (R : RingDeclaration) (M : LeftModule R) : Set₁ where
  field
    ring : RingDeclaration
    fixedModule : LeftModule ring
    homAsAbelianGroup : M.Identifier
    homAsModule : M.Identifier
    functoriality : M.Identifier

-- Hom is left exact
-- Sourced from module: homLeftExact

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

-- Finitely generated free modules are reflexive
-- Sourced from module: freeFinitelyGeneratedReflexive

-- ============================================================================
-- IV.5: Tensor Products
-- ============================================================================

-- Tensor product of modules
record TensorProduct (R : CommutativeRingDeclaration) (M N : LeftModule (UnitalRingDeclaration.underlyingRing (CommutativeRingDeclaration.underlyingRing R))) : Set₁ where
  field
    ring : CommutativeRingDeclaration
    leftModule : LeftModule (UnitalRingDeclaration.underlyingRing (CommutativeRingDeclaration.underlyingRing ring))
    rightModule : LeftModule (UnitalRingDeclaration.underlyingRing (CommutativeRingDeclaration.underlyingRing ring))
    tensorProduct : LeftModule (UnitalRingDeclaration.underlyingRing (CommutativeRingDeclaration.underlyingRing ring))
    universalBilinearMap : M.Identifier
    universalProperty : M.Identifier

-- Tensor product properties
-- Sourced from module: tensorProductProperties

-- ============================================================================
-- IV.6: Modules over Principal Ideal Domains
-- ============================================================================

-- Torsion element in module
record TorsionElement (R : IntegralDomain) (M : LeftModule (UnitalRingDeclaration.underlyingRing (CommutativeRingDeclaration.underlyingRing (IntegralDomain.underlyingRing R)))) (m : M.Identifier) : Set₁ where
  field
    domain : IntegralDomain
    module' : LeftModule (UnitalRingDeclaration.underlyingRing (CommutativeRingDeclaration.underlyingRing (IntegralDomain.underlyingRing domain)))
    element : M.Identifier
    isTorsion : M.Identifier

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

-- PID module classification
-- Sourced from module: pidModuleClassification

-- ============================================================================
-- IV.7: Algebras
-- ============================================================================

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

-- Polynomial ring as free algebra
-- Sourced from module: polynomialRingFreeRAlgebra

-- ============================================================================
-- Integration with Abelian Categories (Chapter2.Level2sub1)
-- ============================================================================

-- Integration with Abelian Categories and Homological Algebra
-- Sourced from module: rModHomologicalAlgebraPackage
