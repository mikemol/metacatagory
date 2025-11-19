``` Agda
-- Algebra.Modules.Basic: Module theory (Hungerford Ch IV)
-- This module covers modules, exact sequences, and tensor products.
-- KEY INTEGRATION: Connects deeply with Chapter2.Level2sub1 (abelian categories).

module Algebra.Modules.Basic where

open import Core
open import Chapter1.Level1Index
open import Chapter2.Level2sub1  -- Abelian categories!
open import Algebra.Foundation
open import Algebra.Groups.Basic
open import Algebra.Groups.Abelian
open import Algebra.Rings.Basic
open import PropertyRegistry
open import Metamodel as M

-- ============================================================================
-- IV.1: Modules, Homomorphisms and Exact Sequences
-- ============================================================================

-- Left R-module
record LeftModule (R : RingDeclaration) : Set₁ where
  field
    ring : RingDeclaration
    -- M is an abelian group
    underlyingAbelianGroup : AbelianGroupDeclaration
    -- Scalar multiplication R × M → M
    scalarMultiplication : M.Identifier
    -- Axioms
    distributiveOverAddition : M.Identifier  -- r(m + n) = rm + rn
    distributiveOverRingAddition : M.Identifier  -- (r + s)m = rm + sm
    associativeScalar : M.Identifier  -- (rs)m = r(sm)
    -- If R is unital: 1·m = m
    unitalAction : M.Identifier

-- Right R-module
record RightModule (R : RingDeclaration) : Set₁ where
  field
    ring : RingDeclaration
    underlyingAbelianGroup : AbelianGroupDeclaration
    scalarMultiplication : M.Identifier  -- M × R → M
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
    -- f : M → N
    morphism : M.Identifier
    -- f is group homomorphism
    preservesAddition : M.Identifier
    -- f(rm) = rf(m)
    preservesScalarMultiplication : M.Identifier

-- Submodule
record Submodule (R : RingDeclaration) (M : LeftModule R) : Set₁ where
  field
    ring : RingDeclaration
    module' : LeftModule ring
    -- N ⊆ M is subgroup
    subgroup : Subgroup (AbelianGroupDeclaration.underlyingGroup (LeftModule.underlyingAbelianGroup module'))
    -- Closed under scalar multiplication
    closedUnderScalars : M.Identifier

-- Quotient module
record QuotientModule (R : RingDeclaration) (M : LeftModule R) (N : Submodule R M) : Set₁ where
  field
    ring : RingDeclaration
    module' : LeftModule ring
    submodule : Submodule ring module'
    -- M/N with operations (m + N) + (m' + N) = (m + m') + N, r(m + N) = rm + N
    quotientModule : LeftModule ring
    canonicalProjection : M.Identifier

-- Kernel of module homomorphism
record KernelOfModuleHomomorphism (R : RingDeclaration) (f : M.Identifier) : Set₁ where
  field
    ring : RingDeclaration
    sourceModule : LeftModule ring
    targetModule : LeftModule ring
    homomorphism : ModuleHomomorphism ring sourceModule targetModule
    -- ker(f) = {m ∈ M | f(m) = 0}
    kernel : Submodule ring sourceModule

-- Image of module homomorphism
record ImageOfModuleHomomorphism (R : RingDeclaration) (f : M.Identifier) : Set₁ where
  field
    ring : RingDeclaration
    sourceModule : LeftModule ring
    targetModule : LeftModule ring
    homomorphism : ModuleHomomorphism ring sourceModule targetModule
    -- im(f) = {f(m) | m ∈ M}
    image : Submodule ring targetModule

-- Cokernel of module homomorphism
record CokernelOfModuleHomomorphism (R : RingDeclaration) (f : M.Identifier) : Set₁ where
  field
    ring : RingDeclaration
    sourceModule : LeftModule ring
    targetModule : LeftModule ring
    homomorphism : ModuleHomomorphism ring sourceModule targetModule
    -- coker(f) = N / im(f)
    cokernel : LeftModule ring

-- EXACT SEQUENCE (crucial for abelian categories!)
record ExactSequence (R : RingDeclaration) : Set₁ where
  field
    ring : RingDeclaration
    -- ... → Mᵢ₋₁ → Mᵢ → Mᵢ₊₁ → ...
    modules : M.Identifier
    morphisms : M.Identifier
    -- im(fᵢ₋₁) = ker(fᵢ) for all i
    exactnessCondition : M.Identifier

-- Short exact sequence: 0 → A → B → C → 0
record ShortExactSequence (R : RingDeclaration) : Set₁ where
  field
    ring : RingDeclaration
    leftModule : LeftModule ring
    middleModule : LeftModule ring
    rightModule : LeftModule ring
    -- 0 → A →^f B →^g C → 0
    leftMap : M.Identifier  -- f : A → B (injective)
    rightMap : M.Identifier  -- g : B → C (surjective)
    -- ker(g) = im(f)
    exactness : M.Identifier

-- ============================================================================
-- CATEGORY OF MODULES (Key Integration Point!)
-- ============================================================================

-- R-Mod is an abelian category
record CategoryOfModules (R : RingDeclaration) : Set₁ where
  field
    ring : RingDeclaration
    category : M.Identifier
    -- Objects: R-modules
    -- Morphisms: R-linear maps
    -- R-Mod is abelian
    isAbelian : M.Identifier

-- R-Mod has kernels and cokernels (from abelian category structure)
postulate
  RMod-Has-Kernels-Cokernels :
    (R : RingDeclaration) →
    M.Identifier  -- Every morphism has kernel and cokernel

-- R-Mod has all limits and colimits
postulate
  RMod-Is-Complete-Cocomplete :
    (R : RingDeclaration) →
    M.Identifier  -- R-Mod has all (small) limits and colimits

-- First Isomorphism Theorem (categorical: image factorization)
postulate
  Module-First-Isomorphism :
    (R : RingDeclaration) →
    (f : M.Identifier) →
    M.Identifier  -- M/ker(f) ≅ im(f)

-- Connection to Chapter2.Level2sub1: Exact sequences
postulate
  RMod-Exact-Sequences-Categorical :
    (R : RingDeclaration) →
    M.Identifier  -- Exact sequences in R-Mod reflect categorical ker/coker

-- ============================================================================
-- IV.2: Free Modules and Vector Spaces
-- ============================================================================

-- Free module
record FreeModule (R : RingDeclaration) (X : M.Identifier) : Set₁ where
  field
    ring : RingDeclaration
    basis : M.Identifier  -- Set X
    -- F(X) = ⊕_{x∈X} R (direct sum of copies of R)
    freeModule : LeftModule ring
    -- Universal property
    universalProperty : M.Identifier

-- Free functor F : Set → R-Mod
record FreeModuleFunctor (R : RingDeclaration) : Set₁ where
  field
    ring : RingDeclaration
    onObjects : M.Identifier  -- X ↦ F(X)
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
postulate
  Free-Module-Adjunction :
    (R : RingDeclaration) →
    (F : FreeModuleFunctor R) →
    (U : ForgetfulModuleFunctor R) →
    M.Identifier  -- F ⊣ U

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
    -- Every vector is unique linear combination of basis vectors
    linearIndependence : M.Identifier
    spanning : M.Identifier

-- Dimension of vector space
record Dimension (F : FieldDeclaration) (V : VectorSpace F) : Set₁ where
  field
    field' : FieldDeclaration
    vectorSpace : VectorSpace field'
    dimension : M.Identifier  -- |basis| (well-defined)

-- All bases have same cardinality
postulate
  Basis-Cardinality-Invariant :
    (F : FieldDeclaration) →
    (V : VectorSpace F) →
    (B₁ B₂ : BasisOfVectorSpace F V) →
    M.Identifier  -- |B₁| = |B₂|

-- ============================================================================
-- IV.3: Projective and Injective Modules
-- ============================================================================

-- Projective module (categorical definition)
record ProjectiveModule (R : RingDeclaration) (P : LeftModule R) : Set₁ where
  field
    ring : RingDeclaration
    module' : LeftModule ring
    -- Lifting property: for every surjection g : B → C and map f : P → C,
    -- ∃ lift h : P → B with g ∘ h = f
    liftingProperty : M.Identifier

-- Free modules are projective
postulate
  Free-Implies-Projective :
    (R : RingDeclaration) →
    (X : M.Identifier) →
    (F : FreeModule R X) →
    M.Identifier  -- F is projective

-- Injective module (categorical definition)
record InjectiveModule (R : RingDeclaration) (I : LeftModule R) : Set₁ where
  field
    ring : RingDeclaration
    module' : LeftModule ring
    -- Extension property: for every injection f : A → B and map g : A → I,
    -- ∃ extension h : B → I with h ∘ f = g
    extensionProperty : M.Identifier

-- Projective modules and split exact sequences
postulate
  Projective-Split-SES :
    (R : RingDeclaration) →
    (P : LeftModule R) →
    M.Identifier  -- P projective iff every SES 0 → A → B → P → 0 splits

-- Injective modules and split exact sequences
postulate
  Injective-Split-SES :
    (R : RingDeclaration) →
    (I : LeftModule R) →
    M.Identifier  -- I injective iff every SES 0 → I → B → C → 0 splits

-- Connection to homological algebra
postulate
  Projective-Injective-Resolutions :
    M.Identifier  -- Every module has projective and injective resolutions

-- ============================================================================
-- IV.4: Hom and Duality
-- ============================================================================

-- Hom functor Hom_R(M, -)
record HomFunctor (R : RingDeclaration) (M : LeftModule R) : Set₁ where
  field
    ring : RingDeclaration
    fixedModule : LeftModule ring
    -- Hom_R(M, N) is abelian group
    homAsAbelianGroup : M.Identifier
    -- When R commutative, Hom_R(M, N) is R-module
    homAsModule : M.Identifier
    -- Contravariant in first argument, covariant in second
    functoriality : M.Identifier

-- Hom is left exact
postulate
  Hom-Left-Exact :
    (R : RingDeclaration) →
    (M : LeftModule R) →
    M.Identifier  -- 0 → A → B → C exact implies 0 → Hom(M,A) → Hom(M,B) → Hom(M,C) exact

-- Dual module M* = Hom_R(M, R)
record DualModule (R : RingDeclaration) (M : LeftModule R) : Set₁ where
  field
    ring : RingDeclaration
    module' : LeftModule ring
    -- M* = Hom_R(M, R)
    dualModule : LeftModule ring

-- Double dual and reflexivity
record ReflexiveModule (R : RingDeclaration) (M : LeftModule R) : Set₁ where
  field
    ring : RingDeclaration
    module' : LeftModule ring
    -- M ≅ M** (canonical isomorphism)
    isReflexive : M.Identifier

-- Finitely generated free modules are reflexive
postulate
  Free-Finitely-Generated-Reflexive :
    (R : RingDeclaration) →
    (M : LeftModule R) →
    M.Identifier  -- M free and finitely generated → M ≅ M**

-- ============================================================================
-- IV.5: Tensor Products
-- ============================================================================

-- Tensor product of modules
record TensorProduct (R : CommutativeRingDeclaration) (M N : LeftModule (UnitalRingDeclaration.underlyingRing (CommutativeRingDeclaration.underlyingRing R))) : Set₁ where
  field
    ring : CommutativeRingDeclaration
    leftModule : LeftModule (UnitalRingDeclaration.underlyingRing (CommutativeRingDeclaration.underlyingRing ring))
    rightModule : LeftModule (UnitalRingDeclaration.underlyingRing (CommutativeRingDeclaration.underlyingRing ring))
    -- M ⊗_R N
    tensorProduct : LeftModule (UnitalRingDeclaration.underlyingRing (CommutativeRingDeclaration.underlyingRing ring))
    -- Bilinear map M × N → M ⊗ N
    universalBilinearMap : M.Identifier
    -- Universal property
    universalProperty : M.Identifier

-- Tensor product is right exact
postulate
  Tensor-Right-Exact :
    (R : CommutativeRingDeclaration) →
    (M : LeftModule (UnitalRingDeclaration.underlyingRing (CommutativeRingDeclaration.underlyingRing R))) →
    M.Identifier  -- A → B → C → 0 exact implies M⊗A → M⊗B → M⊗C → 0 exact

-- Tensor-Hom adjunction (when R commutative)
postulate
  Tensor-Hom-Adjunction :
    (R : CommutativeRingDeclaration) →
    (M N P : LeftModule (UnitalRingDeclaration.underlyingRing (CommutativeRingDeclaration.underlyingRing R))) →
    M.Identifier  -- Hom_R(M ⊗_R N, P) ≅ Hom_R(M, Hom_R(N, P))

-- Tensor product over ℤ (for abelian groups)
postulate
  Tensor-Over-Z :
    (A B : AbelianGroupDeclaration) →
    M.Identifier  -- A ⊗_ℤ B exists (from Ab self-enrichment)

-- Symmetric monoidal structure on R-Mod
postulate
  RMod-Symmetric-Monoidal :
    (R : CommutativeRingDeclaration) →
    M.Identifier  -- (R-Mod, ⊗_R, R) is symmetric monoidal

-- ============================================================================
-- IV.6: Modules over Principal Ideal Domains
-- ============================================================================

-- Torsion element in module
record TorsionElement (R : IntegralDomain) (M : LeftModule (UnitalRingDeclaration.underlyingRing (CommutativeRingDeclaration.underlyingRing (IntegralDomain.underlyingRing R)))) (m : M.Identifier) : Set₁ where
  field
    domain : IntegralDomain
    module' : LeftModule (UnitalRingDeclaration.underlyingRing (CommutativeRingDeclaration.underlyingRing (IntegralDomain.underlyingRing domain)))
    element : M.Identifier
    -- ∃ r ≠ 0 such that rm = 0
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
    -- Only 0 is torsion
    isTorsionFree : M.Identifier

-- Structure theorem for finitely generated modules over PID
record StructureTheoremPID (R : PrincipalIdealDomain) (M : LeftModule (UnitalRingDeclaration.underlyingRing (CommutativeRingDeclaration.underlyingRing (IntegralDomain.underlyingRing (PrincipalIdealDomain.domain R))))) : Set₁ where
  field
    pid : PrincipalIdealDomain
    module' : LeftModule (UnitalRingDeclaration.underlyingRing (CommutativeRingDeclaration.underlyingRing (IntegralDomain.underlyingRing (PrincipalIdealDomain.domain pid))))
    -- M ≅ R^r ⊕ R/(a₁) ⊕ ... ⊕ R/(aₙ) where aᵢ | aᵢ₊₁
    decomposition : M.Identifier

-- Classification over PID generalizes fundamental theorem for abelian groups
postulate
  PID-Module-Classification :
    (R : PrincipalIdealDomain) →
    M.Identifier  -- Finitely generated R-modules classified by invariant factors

-- ============================================================================
-- IV.7: Algebras
-- ============================================================================

-- R-algebra (ring with compatible R-module structure)
record RAlgebra (R : CommutativeRingDeclaration) : Set₁ where
  field
    coefficientRing : CommutativeRingDeclaration
    -- A is a ring
    underlyingRing : RingDeclaration
    -- A is an R-module
    moduleStructure : LeftModule (UnitalRingDeclaration.underlyingRing (CommutativeRingDeclaration.underlyingRing coefficientRing))
    -- Compatibility: r(ab) = (ra)b = a(rb)
    compatibility : M.Identifier

-- Algebra homomorphism
record AlgebraHomomorphism (R : CommutativeRingDeclaration) (A B : RAlgebra R) : Set₁ where
  field
    coefficientRing : CommutativeRingDeclaration
    sourceAlgebra : RAlgebra coefficientRing
    targetAlgebra : RAlgebra coefficientRing
    -- Ring homomorphism that is also R-linear
    ringHomomorphism : M.Identifier
    moduleHomomorphism : M.Identifier

-- Polynomial ring as free algebra
postulate
  Polynomial-Ring-Free-RAlgebra :
    (R : CommutativeRingDeclaration) →
    M.Identifier  -- R[x] is free R-algebra on one generator

-- ============================================================================
-- Integration with Abelian Categories (Chapter2.Level2sub1)
-- ============================================================================

-- R-Mod IS the prototypical abelian category
postulate
  RMod-Is-Abelian-Category :
    (R : RingDeclaration) →
    M.Identifier  -- R-Mod satisfies all axioms of abelian category

-- Exact sequences in R-Mod = exact sequences in abelian category sense
postulate
  Module-Exact-Equals-Categorical-Exact :
    (R : RingDeclaration) →
    M.Identifier  -- Module-theoretic exactness ≡ categorical exactness

-- Snake Lemma (diagram chasing in abelian categories)
postulate
  Snake-Lemma :
    (R : RingDeclaration) →
    M.Identifier  -- Commutative diagram with exact rows gives long exact sequence

-- Five Lemma (from abelian categories)
postulate
  Five-Lemma :
    (R : RingDeclaration) →
    M.Identifier  -- Diagram with exact rows: outer 4 isomorphisms → middle is isomorphism

-- Connection to derived functors and homological algebra
postulate
  Module-Homological-Algebra :
    (R : RingDeclaration) →
    M.Identifier  -- Ext and Tor functors via projective/injective resolutions

-- Every abelian category embeds in a category of modules (Freyd-Mitchell)
postulate
  Freyd-Mitchell-Embedding :
    M.Identifier  -- Every small abelian category embeds fully faithfully in R-Mod
```
