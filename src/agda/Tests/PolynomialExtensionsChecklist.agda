{-# OPTIONS --without-K #-}

-- Tests.PolynomialExtensionsChecklist
-- Minimal instances for polynomial-related adapters

module Tests.PolynomialExtensionsChecklist where

open import Agda.Builtin.Equality
open import Core.Phase using (Bool; true; false)
open import Agda.Builtin.Unit using (⊤)

import Metamodel as M
import Algebra.Foundation as AF
import Algebra.Rings.Basic as AR
import Tests.ObligationAdapters as A
import Core.CategoricalAdapter
import Chapter1.Level1 as C1L

-- Minimal commutative ring
commRingDecl : AR.CommutativeRingDeclaration
commRingDecl =
  let
    plusSemigroup : AF.SemigroupDeclaration
    plusSemigroup = record
      { underlyingMagma = record { underlyingSet = M.mkId "Z" ; binaryOp = M.mkId "+" ; index = AF.magmaIndex }
      ; associativity = C1L.AXIOM_Associativity (M.mkId "+-assoc")
      ; index = AF.semigroupIndex
      }

    plusMonoid : AF.MonoidDeclaration
    plusMonoid = record
      { underlyingSemigroup = plusSemigroup
      ; identityElement = M.mkId "0"
      ; identityAxiom = C1L.AXIOM_Identity (M.mkId "+-id")
      ; index = AF.monoidIndex
      }

    plusGroup : AF.GroupDeclaration
    plusGroup = record
      { underlyingMonoid = plusMonoid
      ; inverseOperation =
          record
            { forMonoid = plusMonoid
            ; inverseMap = M.mkId "neg"
            ; inverseAxiom = M.mkId "+-left-inv"
            }
      ; index = AF.groupIndex
      }

    addAbelian : AF.AbelianGroupDeclaration
    addAbelian = record
      { underlyingGroup = plusGroup
      ; commutativity = record { forGroup = plusGroup ; axiom = M.mkId "+-comm" }
      ; index = AF.abelianGroupIndex
      }

    ringDecl : AR.RingDeclaration
    ringDecl = record
      { identifier = M.mkId "Z"
      ; additiveGroup = addAbelian
      ; multiplication = M.mkId "*"
      ; multAssociative = M.mkId "*-assoc"
      ; leftDistributive = M.mkId "left-dist"
      ; rightDistributive = M.mkId "right-dist"
      }

    unitalRing : AR.UnitalRingDeclaration
    unitalRing = record
      { underlyingRing = ringDecl
      ; multiplicativeIdentity = M.mkId "1"
      ; leftIdentity = M.mkId "*-left-id"
      ; rightIdentity = M.mkId "*-right-id"
      }
  in
  record { underlyingRing = unitalRing ; commutativity = M.mkId "*-comm" }

-- Integral domain (for UFD-dependent constructs)
-- | Integral domain used for UFD-dependent constructs.
integralDomainDecl : AR.IntegralDomain
integralDomainDecl = record
  { underlyingRing = commRingDecl
  ; noZeroDivisors = M.mkId "no-zero-div"
  }

-- | Unique factorization domain.
ufdDecl : AR.UFD
ufdDecl = record
  { domain = integralDomainDecl
  ; uniqueFactorization = M.mkId "unique-fact"
  }

-- Multivariate polynomial ring (R[x1,...,xn])
multivariatePolyRingDecl : AR.MultivariatePolynomialRing commRingDecl (M.mkId "n")
multivariatePolyRingDecl =
  let
    pSemigroup : AF.SemigroupDeclaration
    pSemigroup = record
      { underlyingMagma = record { underlyingSet = M.mkId "R[x1,...,xn]" ; binaryOp = M.mkId "+P" ; index = AF.magmaIndex }
      ; associativity = C1L.AXIOM_Associativity (M.mkId "P-assoc")
      ; index = AF.semigroupIndex
      }

    pMonoid : AF.MonoidDeclaration
    pMonoid = record
      { underlyingSemigroup = pSemigroup
      ; identityElement = M.mkId "0P"
      ; identityAxiom = C1L.AXIOM_Identity (M.mkId "P-id")
      ; index = AF.monoidIndex
      }

    pGroup : AF.GroupDeclaration
    pGroup = record
      { underlyingMonoid = pMonoid
      ; inverseOperation =
          record
            { forMonoid = pMonoid
            ; inverseMap = M.mkId "negP"
            ; inverseAxiom = M.mkId "P-left-inv"
            }
      ; index = AF.groupIndex
      }

    pAbelian : AF.AbelianGroupDeclaration
    pAbelian = record
      { underlyingGroup = pGroup
      ; commutativity = record { forGroup = pGroup ; axiom = M.mkId "P-comm" }
      ; index = AF.abelianGroupIndex
      }

    ringP : AR.RingDeclaration
    ringP = record
      { identifier = M.mkId "R[x1,...,xn]"
      ; additiveGroup = pAbelian
      ; multiplication = M.mkId "*P"
      ; multAssociative = M.mkId "P-mult-assoc"
      ; leftDistributive = M.mkId "P-left-dist"
      ; rightDistributive = M.mkId "P-right-dist"
      }

    unitalRP : AR.UnitalRingDeclaration
    unitalRP = record
      { underlyingRing = ringP
      ; multiplicativeIdentity = M.mkId "1P"
      ; leftIdentity = M.mkId "P-left-id"
      ; rightIdentity = M.mkId "P-right-id"
      }
  in
  record
    { coefficientRing = commRingDecl
    ; numberOfVariables = M.mkId "n"
    ; polynomialRing = record { underlyingRing = unitalRP ; commutativity = M.mkId "P-mult-comm" }
    }

multivariatePolyRing : AR.CommutativeRingDeclaration
multivariatePolyRing = AR.MultivariatePolynomialRing.polynomialRing multivariatePolyRingDecl

-- Content of polynomial
contentDecl : AR.ContentOfPolynomial ufdDecl (M.mkId "f")
contentDecl = record
  { ufd = ufdDecl
  ; polynomial = M.mkId "f"
  ; content = M.mkId "cont(f)"
  }

-- Primitive polynomial
primPolyDecl : AR.PrimitivePolynomial ufdDecl (M.mkId "p")
primPolyDecl = record
  { ufd = ufdDecl
  ; polynomial = M.mkId "p"
  ; isPrimitive = M.mkId "prim"
  }

-- Prime spectrum
primeSpectrumDecl : AR.PrimeSpectrum commRingDecl
primeSpectrumDecl = record
  { ring = commRingDecl
  ; spectrum = M.mkId "Spec(R)"
  ; topology = M.mkId "zariski-top"
  }

-- Adapter instances
multivariatePolyAdapt : A.MultivariatePolynomialRingAdapter
multivariatePolyAdapt = A.mkMultivariatePolynomialRingAdapter commRingDecl (M.mkId "n") multivariatePolyRingDecl multivariatePolyRing refl

contentAdapt : A.ContentOfPolynomialAdapter
contentAdapt = A.mkContentOfPolynomialAdapter ufdDecl (M.mkId "f") contentDecl (M.mkId "cont(f)") refl

primPolyAdapt : A.PrimitivePolynomialAdapter
primPolyAdapt = A.mkPrimitivePolynomialAdapter ufdDecl (M.mkId "p") primPolyDecl ufdDecl refl

primeSpectrumAdapt : A.PrimeSpectrumAdapter
primeSpectrumAdapt = A.mkPrimeSpectrumAdapter commRingDecl primeSpectrumDecl (M.mkId "zariski-top") refl

-- Status assertions
_ : A.isFilledMultivariatePolynomialRing multivariatePolyAdapt ≡ true
_ = refl

-- Categorical assertions for MultivariatePolynomialRing (omitted; smoke-tested via adapter wiring)

_ : A.isFilledContentOfPolynomial contentAdapt ≡ true
_ = refl

-- Categorical assertions for ContentOfPolynomial (omitted)

_ : A.isFilledPrimitivePolynomial primPolyAdapt ≡ true
_ = refl

-- Categorical assertions for PrimitivePolynomial (omitted)

_ : A.isFilledPrimeSpectrum primeSpectrumAdapt ≡ true
_ = refl

-- Categorical assertions for PrimeSpectrum (omitted)
