-- Tests.PolynomialExtensionsChecklist
-- Minimal instances for polynomial-related adapters

module Tests.PolynomialExtensionsChecklist where

open import Agda.Builtin.Equality
open import Agda.Builtin.Bool as B

import Metamodel as M
import Algebra.Foundation as AF
import Algebra.Rings.Basic as AR
import Tests.ObligationAdapters as A

-- Minimal commutative ring
commRingDecl : AR.CommutativeRingDeclaration
commRingDecl = record
  { underlyingRing = record
    { additiveGroup = record
      { underlyingGroup = record
        { underlyingMonoid = record
          { underlyingSemigroup = record
            { underlyingMagma = record
              { carrier = M.mkId "Z"
              ; operation = M.mkId "+"
              }
            ; associativity = M.mkId "+-assoc"
            }
          ; identityElement = M.mkId "0"
          ; leftIdentity = M.mkId "+-left-id"
          ; rightIdentity = M.mkId "+-right-id"
          }
        ; inverseOp = record
          { inverse = M.mkId "neg"
          ; leftInverse = M.mkId "+-left-inv"
          ; rightInverse = M.mkId "+-right-inv"
          }
        }
      ; commutativityAxiom = record
        { commutativity = M.mkId "+-comm"
        }
      }
    ; multiplicativeSemigroup = record
      { underlyingMagma = record
        { carrier = M.mkId "Z"
        ; operation = M.mkId "*"
        }
      ; associativity = M.mkId "*-assoc"
      }
    ; leftDistributivity = M.mkId "left-dist"
    ; rightDistributivity = M.mkId "right-dist"
    }
  ; commutativity = M.mkId "*-comm"
  }

-- Integral domain (for UFD-dependent constructs)
integralDomainDecl : AR.IntegralDomain
integralDomainDecl = record
  { underlyingRing = commRingDecl
  ; nontrivial = M.mkId "nontrivial"
  ; noZeroDivisors = M.mkId "no-zero-div"
  }

-- UFD
ufdDecl : AR.UFD
ufdDecl = record
  { domain = integralDomainDecl
  ; uniqueFactorization = M.mkId "unique-fact"
  }

-- Multivariate polynomial ring (R[x1,...,xn])
multivariatePolyRingDecl : AR.MultivariatePolynomialRing commRingDecl (M.mkId "n")
multivariatePolyRingDecl = record
  { baseRing = commRingDecl
  ; numberOfVariables = M.mkId "n"
  ; polynomialRing = record
    { underlyingRing = record
      { additiveGroup = record
        { underlyingGroup = record
          { underlyingMonoid = record
            { underlyingSemigroup = record
              { underlyingMagma = record
                { carrier = M.mkId "R[x1,...,xn]"
                ; operation = M.mkId "+P"
                }
              ; associativity = M.mkId "P-assoc"
              }
            ; identityElement = M.mkId "0P"
            ; leftIdentity = M.mkId "P-left-id"
            ; rightIdentity = M.mkId "P-right-id"
            }
          ; inverseOp = record
            { inverse = M.mkId "negP"
            ; leftInverse = M.mkId "P-left-inv"
            ; rightInverse = M.mkId "P-right-inv"
            }
          }
        ; commutativityAxiom = record
          { commutativity = M.mkId "P-comm"
          }
        }
      ; multiplicativeSemigroup = record
        { underlyingMagma = record
          { carrier = M.mkId "R[x1,...,xn]"
          ; operation = M.mkId "*P"
          }
        ; associativity = M.mkId "P-mult-assoc"
        }
      ; leftDistributivity = M.mkId "P-left-dist"
      ; rightDistributivity = M.mkId "P-right-dist"
      }
    ; commutativity = M.mkId "P-mult-comm"
    }
  ; universalProperty = M.mkId "poly-univ"
  }

multivariatePolyRing : AR.CommutativeRingDeclaration
multivariatePolyRing = AR.MultivariatePolynomialRing.polynomialRing multivariatePolyRingDecl

-- Content of polynomial
contentDecl : AR.ContentOfPolynomial ufdDecl (M.mkId "f")
contentDecl = record
  { polynomial = M.mkId "f"
  ; content = M.mkId "cont(f)"
  ; contentIsGCD = M.mkId "content-gcd"
  }

-- Primitive polynomial
primPolyDecl : AR.PrimitivePolynomial ufdDecl (M.mkId "p")
primPolyDecl = record
  { domain = ufdDecl
  ; polynomial = M.mkId "p"
  ; contentIsUnit = M.mkId "prim-content-unit"
  }

-- Prime spectrum
primeSpectrumDecl : AR.PrimeSpectrum commRingDecl
primeSpectrumDecl = record
  { ring = commRingDecl
  ; topologicalSpace = M.mkId "Spec(R)"
  ; zariski = M.mkId "zariski-top"
  }

-- Adapter instances
multivariatePolyAdapt : A.MultivariatePolynomialRingAdapter
multivariatePolyAdapt = A.mkMultivariatePolynomialRingAdapter commRingDecl (M.mkId "n") multivariatePolyRingDecl multivariatePolyRing refl

contentAdapt : A.ContentOfPolynomialAdapter
contentAdapt = A.mkContentOfPolynomialAdapter ufdDecl (M.mkId "f") contentDecl (M.mkId "cont(f)") refl

primPolyAdapt : A.PrimitivePolynomialAdapter
primPolyAdapt = A.mkPrimitivePolynomialAdapter ufdDecl (M.mkId "p") primPolyDecl ufdDecl refl

primeSpectrumAdapt : A.PrimeSpectrumAdapter
primeSpectrumAdapt = A.mkPrimeSpectrumAdapter commRingDecl primeSpectrumDecl (M.mkId "Spec(R)") refl

-- Status assertions
_ : A.isFilledMultivariatePolynomialRing multivariatePolyAdapt ≡ B.true
_ = refl

_ : A.isFilledContentOfPolynomial contentAdapt ≡ B.true
_ = refl

_ : A.isFilledPrimitivePolynomial primPolyAdapt ≡ B.true
_ = refl

_ : A.isFilledPrimeSpectrum primeSpectrumAdapt ≡ B.true
_ = refl
