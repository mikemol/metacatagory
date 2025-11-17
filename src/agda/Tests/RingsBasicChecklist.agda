-- Tests.RingsBasicChecklist: Coverage for Algebra.Rings.Basic (Ring Theory & Ideal Theory)

module Tests.RingsBasicChecklist where

import Agda.Builtin.Bool as B
open import Agda.Builtin.Equality using (_≡_; refl)
open import Metamodel as M

-- Imports
import Chapter1.Level1 as C1L
import Algebra.Foundation as AF
import Algebra.Rings.Basic as AR
import Algebra.Groups.Basic as AGB
import Tests.ObligationAdapters as A

-- Build minimal algebra scaffolding
magmaDecl : AF.MagmaDeclaration
magmaDecl = record { binaryOp = M.mkId "ringOp" }

semigroupDecl : AF.SemigroupDeclaration
semigroupDecl = record
  { underlyingMagma = magmaDecl
  ; associativity = M.mkId "ringAssoc"
  }

monoidDecl : AF.MonoidDeclaration
monoidDecl = record
  { underlyingSemigroup = semigroupDecl
  ; identityElement = record
    { forSemigroup = semigroupDecl
    ; element = M.mkId "ringId"
    ; leftIdentity = M.mkId "ringLId"
    ; rightIdentity = M.mkId "ringRId"
    }
  }

groupDecl : AF.GroupDeclaration
groupDecl = record
  { underlyingMonoid = monoidDecl
  ; inverseOperation = record
    { forMonoid = monoidDecl
    ; operation = M.mkId "ringInv"
    ; inverseAxiom = M.mkId "ringInvAx"
    }
  }

abelianGroupDecl : AF.AbelianGroupDeclaration
abelianGroupDecl = record
  { underlyingGroup = groupDecl
  ; commutativity = record
    { forGroup = groupDecl
    ; axiom = M.mkId "ringComm"
    }
  }

ringDecl : AR.RingDeclaration
ringDecl = record
  { identifier = M.mkId "ring"
  ; additiveGroup = abelianGroupDecl
  ; multiplication = M.mkId "ringMult"
  ; multAssociative = M.mkId "ringMultAssoc"
  ; leftDistributive = M.mkId "ringLDist"
  ; rightDistributive = M.mkId "ringRDist"
  }

unitalRingDecl : AR.UnitalRingDeclaration
unitalRingDecl = record
  { underlyingRing = ringDecl
  ; multiplicativeIdentity = M.mkId "ringOne"
  ; leftIdentity = M.mkId "ringOneLId"
  ; rightIdentity = M.mkId "ringOneRId"
  }

commRingDecl : AR.CommutativeRingDeclaration
commRingDecl = record
  { underlyingRing = unitalRingDecl
  ; commutativity = M.mkId "ringMultComm"
  }

subgroupDecl : AGB.Subgroup (AF.AbelianGroupDeclaration.underlyingGroup abelianGroupDecl)
subgroupDecl = record
  { group = AF.AbelianGroupDeclaration.underlyingGroup abelianGroupDecl
  ; subset = M.mkId "subgroupSubset"
  ; closedUnderOp = M.mkId "subgroupClosed"
  ; containsIdentity = M.mkId "subgroupId"
  ; closedUnderInverse = M.mkId "subgroupInv"
  }

-- Ideal
leftIdealDecl : AR.LeftIdeal ringDecl
leftIdealDecl = record
  { ring = ringDecl
  ; additiveSubgroup = subgroupDecl
  ; closedUnderLeftMultiplication = M.mkId "leftIdealClosed"
  }

rightIdealDecl : AR.RightIdeal ringDecl
rightIdealDecl = record
  { ring = ringDecl
  ; additiveSubgroup = subgroupDecl
  ; closedUnderRightMultiplication = M.mkId "rightIdealClosed"
  }

idealDecl : AR.Ideal ringDecl
idealDecl = record
  { ring = ringDecl
  ; leftIdeal = leftIdealDecl
  ; rightIdeal = rightIdealDecl
  }

idealAdapt : A.IdealAdapter
idealAdapt = A.mkIdealAdapter ringDecl idealDecl ringDecl refl

idealStatus : A.isFilledIdeal idealAdapt ≡ B.true
idealStatus = refl

-- Prime ideal
primeIdealDecl : AR.PrimeIdeal commRingDecl
primeIdealDecl = record
  { ring = commRingDecl
  ; ideal = idealDecl
  ; isPrime = M.mkId "isPrime"
  }

primeIdealAdapt : A.PrimeIdealAdapter
primeIdealAdapt = A.mkPrimeIdealAdapter commRingDecl primeIdealDecl commRingDecl refl

primeIdealStatus : A.isFilledPrimeIdeal primeIdealAdapt ≡ B.true
primeIdealStatus = refl

-- Maximal ideal
maxIdealDecl : AR.MaximalIdeal commRingDecl
maxIdealDecl = record
  { ring = commRingDecl
  ; ideal = idealDecl
  ; isMaximal = M.mkId "isMaximal"
  }

maxIdealAdapt : A.MaximalIdealAdapter
maxIdealAdapt = A.mkMaximalIdealAdapter commRingDecl maxIdealDecl commRingDecl refl

maxIdealStatus : A.isFilledMaximalIdeal maxIdealAdapt ≡ B.true
maxIdealStatus = refl

-- Integral domain
integralDomainDecl : AR.IntegralDomain
integralDomainDecl = record
  { underlyingRing = commRingDecl
  ; noZeroDivisors = M.mkId "noZeroDiv"
  }

integralDomainAdapt : A.IntegralDomainAdapter
integralDomainAdapt = A.mkIntegralDomainAdapter integralDomainDecl commRingDecl refl

integralDomainStatus : A.isFilledIntegralDomain integralDomainAdapt ≡ B.true
integralDomainStatus = refl

-- Irreducible element
p : M.Identifier
p = M.mkId "p"

irreducibleDecl : AR.IrreducibleElement integralDomainDecl p
irreducibleDecl = record
  { domain = integralDomainDecl
  ; element = p
  ; isIrreducible = M.mkId "isIrreducible"
  }

irreducibleAdapt : A.IrreducibleElementAdapter
irreducibleAdapt = A.mkIrreducibleElementAdapter integralDomainDecl p irreducibleDecl integralDomainDecl refl

irreducibleStatus : A.isFilledIrreducibleElement irreducibleAdapt ≡ B.true
irreducibleStatus = refl

-- Prime element
primeElemDecl : AR.PrimeElement integralDomainDecl p
primeElemDecl = record
  { domain = integralDomainDecl
  ; element = p
  ; isPrime = M.mkId "isPrimeElem"
  }

primeElemAdapt : A.PrimeElementAdapter
primeElemAdapt = A.mkPrimeElementAdapter integralDomainDecl p primeElemDecl integralDomainDecl refl

primeElemStatus : A.isFilledPrimeElement primeElemAdapt ≡ B.true
primeElemStatus = refl

-- UFD
ufdDecl : AR.UFD
ufdDecl = record
  { domain = integralDomainDecl
  ; uniqueFactorization = M.mkId "uniqueFact"
  }

ufdAdapt : A.UFDAdapter
ufdAdapt = A.mkUFDAdapter ufdDecl integralDomainDecl refl

ufdStatus : A.isFilledUFD ufdAdapt ≡ B.true
ufdStatus = refl

-- PID
pidDecl : AR.PrincipalIdealDomain
pidDecl = record
  { domain = integralDomainDecl
  ; allIdealsPrincipal = M.mkId "allPrincipal"
  }

pidAdapt : A.PrincipalIdealDomainAdapter
pidAdapt = A.mkPrincipalIdealDomainAdapter pidDecl integralDomainDecl refl

pidStatus : A.isFilledPrincipalIdealDomain pidAdapt ≡ B.true
pidStatus = refl

-- Euclidean domain
euclideanDecl : AR.EuclideanDomain
euclideanDecl = record
  { domain = integralDomainDecl
  ; euclideanFunction = M.mkId "euclideanFunc"
  ; divisionAlgorithm = M.mkId "divAlg"
  }

euclideanAdapt : A.EuclideanDomainAdapter
euclideanAdapt = A.mkEuclideanDomainAdapter euclideanDecl integralDomainDecl refl

euclideanStatus : A.isFilledEuclideanDomain euclideanAdapt ≡ B.true
euclideanStatus = refl

-- Multiplicative system
multSysDecl : AR.MultiplicativeSystem commRingDecl
multSysDecl = record
  { ring = commRingDecl
  ; subset = M.mkId "multSysSubset"
  ; containsOne = M.mkId "multSysOne"
  ; closedUnderMultiplication = M.mkId "multSysClosed"
  }

multSysAdapt : A.MultiplicativeSystemAdapter
multSysAdapt = A.mkMultiplicativeSystemAdapter commRingDecl multSysDecl commRingDecl refl

multSysStatus : A.isFilledMultiplicativeSystem multSysAdapt ≡ B.true
multSysStatus = refl

-- Localization
localizationDecl : AR.Localization commRingDecl multSysDecl
localizationDecl = record
  { ring = commRingDecl
  ; multiplicativeSystem = multSysDecl
  ; localization = commRingDecl
  ; universalMap = M.mkId "locUniversal"
  }

localizationAdapt : A.LocalizationAdapter
localizationAdapt = A.mkLocalizationAdapter commRingDecl multSysDecl localizationDecl commRingDecl refl

localizationStatus : A.isFilledLocalization localizationAdapt ≡ B.true
localizationStatus = refl

-- Field of fractions
fieldOfFracDecl : AR.FieldOfFractions integralDomainDecl
fieldOfFracDecl = record
  { domain = integralDomainDecl
  ; fieldOfFractions = record
    { underlyingRing = commRingDecl
    ; inverses = M.mkId "fracInverses"
    }
  ; universalProperty = M.mkId "fracUniversal"
  }

fieldOfFracAdapt : A.FieldOfFractionsAdapter
fieldOfFracAdapt = A.mkFieldOfFractionsAdapter integralDomainDecl fieldOfFracDecl integralDomainDecl refl

fieldOfFracStatus : A.isFilledFieldOfFractions fieldOfFracAdapt ≡ B.true
fieldOfFracStatus = refl

-- Polynomial ring
polyRingDecl : AR.PolynomialRing commRingDecl
polyRingDecl = record
  { coefficientRing = commRingDecl
  ; polynomialRing = commRingDecl
  ; universalProperty = M.mkId "polyUniversal"
  }

polyRingAdapt : A.PolynomialRingAdapter
polyRingAdapt = A.mkPolynomialRingAdapter commRingDecl polyRingDecl commRingDecl refl

polyRingStatus : A.isFilledPolynomialRing polyRingAdapt ≡ B.true
polyRingStatus = refl

-- Quotient ring
quotRingDecl : AR.QuotientRing ringDecl idealDecl
quotRingDecl = record
  { ring = ringDecl
  ; ideal = idealDecl
  ; quotientRing = ringDecl
  ; canonicalProjection = M.mkId "quotProj"
  }

quotRingAdapt : A.QuotientRingAdapter
quotRingAdapt = A.mkQuotientRingAdapter ringDecl idealDecl quotRingDecl ringDecl refl

quotRingStatus : A.isFilledQuotientRing quotRingAdapt ≡ B.true
quotRingStatus = refl
