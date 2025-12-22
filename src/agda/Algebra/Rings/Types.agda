{-# OPTIONS --without-K #-}

module Algebra.Rings.Types where

open import Core
open import Algebra.Foundation
open import Algebra.Groups.Types
open import Metamodel as M

-- ============================================================================
-- Basic Ring Structures (Hungerford Ch III)
-- ============================================================================

-- Ring (from Foundation, but expanded here)
record RingDeclaration : Set₁ where
  field
    identifier : M.Identifier
    additiveGroup : AbelianGroupDeclaration
    multiplication : M.Identifier
    multAssociative : M.Identifier
    leftDistributive : M.Identifier
    rightDistributive : M.Identifier

-- Ring with unity (unital ring)
record UnitalRingDeclaration : Set₁ where
  field
    underlyingRing : RingDeclaration
    multiplicativeIdentity : M.Identifier
    leftIdentity : M.Identifier
    rightIdentity : M.Identifier

-- Commutative ring
record CommutativeRingDeclaration : Set₁ where
  field
    underlyingRing : UnitalRingDeclaration
    commutativity : M.Identifier

-- Division ring (skew field)
record DivisionRingDeclaration : Set₁ where
  field
    underlyingRing : UnitalRingDeclaration
    inverses : M.Identifier

-- Field (commutative division ring)
record FieldDeclaration : Set₁ where
  field
    underlyingRing : CommutativeRingDeclaration
    inverses : M.Identifier

-- ============================================================================
-- Homomorphisms and Ideals
-- ============================================================================

-- Ring homomorphism
record RingHomomorphism (R S : RingDeclaration) : Set₁ where
  field
    sourceRing : RingDeclaration
    targetRing : RingDeclaration
    morphism : M.Identifier
    preservesAddition : M.Identifier
    preservesMultiplication : M.Identifier

-- Unital ring homomorphism
record UnitalRingHomomorphism (R S : UnitalRingDeclaration) : Set₁ where
  field
    sourceRing : UnitalRingDeclaration
    targetRing : UnitalRingDeclaration
    underlyingHomomorphism : RingHomomorphism (UnitalRingDeclaration.underlyingRing sourceRing) (UnitalRingDeclaration.underlyingRing targetRing)
    preservesUnity : M.Identifier

-- Left ideal
record LeftIdeal (R : RingDeclaration) : Set₁ where
  field
    ring : RingDeclaration
    additiveSubgroup : Subgroup (AbelianGroupDeclaration.underlyingGroup (RingDeclaration.additiveGroup ring))
    closedUnderLeftMultiplication : M.Identifier

-- Right ideal
record RightIdeal (R : RingDeclaration) : Set₁ where
  field
    ring : RingDeclaration
    additiveSubgroup : Subgroup (AbelianGroupDeclaration.underlyingGroup (RingDeclaration.additiveGroup ring))
    closedUnderRightMultiplication : M.Identifier

-- Two-sided ideal
record Ideal (R : RingDeclaration) : Set₁ where
  field
    ring : RingDeclaration
    leftIdeal : LeftIdeal ring
    rightIdeal : RightIdeal ring

-- Principal ideal
record PrincipalIdeal (R : RingDeclaration) (a : M.Identifier) : Set₁ where
  field
    ring : RingDeclaration
    generator : M.Identifier
    ideal : Ideal ring

-- Quotient ring
record QuotientRing (R : RingDeclaration) (I : Ideal R) : Set₁ where
  field
    ring : RingDeclaration
    ideal : Ideal ring
    quotientRing : RingDeclaration
    canonicalProjection : M.Identifier

-- ============================================================================
-- Prime and Maximal Ideals
-- ============================================================================

-- Prime ideal
record PrimeIdeal (R : CommutativeRingDeclaration) : Set₁ where
  field
    ring : CommutativeRingDeclaration
    ideal : Ideal (UnitalRingDeclaration.underlyingRing (CommutativeRingDeclaration.underlyingRing ring))
    isPrime : M.Identifier

-- Maximal ideal
record MaximalIdeal (R : CommutativeRingDeclaration) : Set₁ where
  field
    ring : CommutativeRingDeclaration
    ideal : Ideal (UnitalRingDeclaration.underlyingRing (CommutativeRingDeclaration.underlyingRing ring))
    isMaximal : M.Identifier

-- ============================================================================
-- Factorization Domains (Hungerford III.3)
-- ============================================================================

-- Integral domain
record IntegralDomain : Set₁ where
  field
    underlyingRing : CommutativeRingDeclaration
    noZeroDivisors : M.Identifier

-- Unique factorization domain
record UniqueFactorizationDomain : Set₁ where
  field
    underlyingDomain : IntegralDomain
    uniqueFactorization : M.Identifier

-- Principal ideal domain
record PrincipalIdealDomain : Set₁ where
  field
    domain : IntegralDomain
    allIdealsPrincipal : M.Identifier

-- Euclidean domain
record EuclideanDomain : Set₁ where
  field
    underlyingDomain : IntegralDomain
    euclideanFunction : M.Identifier
    quotientRemainder : M.Identifier

-- ============================================================================
-- Special Elements and Properties
-- ============================================================================

-- Unit (invertible element)
record Unit (R : UnitalRingDeclaration) (u : M.Identifier) : Set₁ where
  field
    ring : UnitalRingDeclaration
    element : M.Identifier
    inverseElement : M.Identifier
    leftInverse : M.Identifier
    rightInverse : M.Identifier

-- Irreducible element
record IrreducibleElement (R : IntegralDomain) (p : M.Identifier) : Set₁ where
  field
    domain : IntegralDomain
    element : M.Identifier
    isIrreducible : M.Identifier

-- Prime element
record PrimeElement (R : IntegralDomain) (p : M.Identifier) : Set₁ where
  field
    domain : IntegralDomain
    element : M.Identifier
    isPrime : M.Identifier

-- ============================================================================
-- Ring Extensions and Localization
-- ============================================================================

-- Localization of a ring at multiplicative set
record LocalizationOfRing (R : CommutativeRingDeclaration) (S : M.Identifier) : Set₁ where
  field
    ring : CommutativeRingDeclaration
    multiplicativeSet : M.Identifier
    localizedRing : UnitalRingDeclaration
    universalProperty : M.Identifier

-- Field of fractions
record FieldOfFractions (R : IntegralDomain) : Set₁ where
  field
    domain : IntegralDomain
    fractionField : FieldDeclaration
    embedding : M.Identifier
    universalProperty : M.Identifier

-- Polynomial ring
record PolynomialRing (R : CommutativeRingDeclaration) : Set₁ where
  field
    coefficientRing : CommutativeRingDeclaration
    polynomialRing : CommutativeRingDeclaration
    universalProperty : M.Identifier

-- ============================================================================
-- Categories of Rings
-- ============================================================================

-- Category of rings
record CategoryOfRings : Set₁ where
  field
    category : M.Identifier
    hasProducts : M.Identifier
    hasCoproducts : M.Identifier

-- Category of commutative rings
record CategoryOfCommutativeRings : Set₁ where
  field
    category : M.Identifier
    categoryOfRings : CategoryOfRings
