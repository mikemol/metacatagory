-- Tests.RingsBasicChecklist: Coverage for Algebra.Rings.Basic (Ring Theory & Ideal Theory)

module Tests.RingsBasicChecklist where

import Agda.Builtin.Bool as B
open import Agda.Builtin.Equality using (_≡_; refl)
open import Agda.Builtin.Unit using (⊤; tt)
open import Metamodel as M
open import Core.CategoricalAdapter

-- Imports
import Chapter1.Level1 as C1L
import Algebra.Foundation as AF
import Algebra.Rings.Basic as AR
import Algebra.Groups.Basic as AGB
import Tests.ObligationAdapters as A

-- Build minimal algebra scaffolding
magmaDecl : AF.MagmaDeclaration
magmaDecl = record { underlyingSet = M.mkId "ringCarrier" ; binaryOp = M.mkId "ringOp" ; index = AF.magmaIndex }

semigroupDecl : AF.SemigroupDeclaration
semigroupDecl = record
  { underlyingMagma = magmaDecl
  ; associativity = C1L.AXIOM_Associativity (M.mkId "ringAssoc")
  ; index = AF.semigroupIndex
  }

monoidDecl : AF.MonoidDeclaration
monoidDecl = record
  { underlyingSemigroup = semigroupDecl
  ; identityElement = M.mkId "ringId"
  ; identityAxiom = C1L.AXIOM_Identity (M.mkId "ringIdAx")
  ; index = AF.monoidIndex
  }

groupDecl : AF.GroupDeclaration
groupDecl = record
  { underlyingMonoid = monoidDecl
  ; inverseOperation = record
    { forMonoid = monoidDecl
    ; inverseMap = M.mkId "ringInv"
    ; inverseAxiom = M.mkId "ringInvAx"
    }
  ; index = AF.groupIndex
  }

abelianGroupDecl : AF.AbelianGroupDeclaration
abelianGroupDecl = record
  { underlyingGroup = groupDecl
  ; commutativity = record
    { forGroup = groupDecl
    ; axiom = M.mkId "ringComm"
    }
  ; index = AF.abelianGroupIndex
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
  { subset = M.mkId "subgroupSubset"
  ; inclusion = M.mkId "subgroupIncl"
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

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.idealCategorical idealAdapt) tt) ≡ A.IdealAdapter.decl idealAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.idealCategorical idealAdapt) ≡ refl
_ = refl

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

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.primeIdealCategorical primeIdealAdapt) tt) ≡ A.PrimeIdealAdapter.decl primeIdealAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.primeIdealCategorical primeIdealAdapt) ≡ refl
_ = refl

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

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.maximalIdealCategorical maxIdealAdapt) tt) ≡ A.MaximalIdealAdapter.decl maxIdealAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.maximalIdealCategorical maxIdealAdapt) ≡ refl
_ = refl

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

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.integralDomainCategorical integralDomainAdapt) tt) ≡ A.IntegralDomainAdapter.decl integralDomainAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.integralDomainCategorical integralDomainAdapt) ≡ refl
_ = refl

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

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.irreducibleElementCategorical irreducibleAdapt) tt) ≡ A.IrreducibleElementAdapter.decl irreducibleAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.irreducibleElementCategorical irreducibleAdapt) ≡ refl
_ = refl

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

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.primeElementCategorical primeElemAdapt) tt) ≡ A.PrimeElementAdapter.decl primeElemAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.primeElementCategorical primeElemAdapt) ≡ refl
_ = refl

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

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.ufdCategorical ufdAdapt) tt) ≡ A.UFDAdapter.decl ufdAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.ufdCategorical ufdAdapt) ≡ refl
_ = refl

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

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.principalIdealDomainCategorical pidAdapt) tt) ≡ A.PrincipalIdealDomainAdapter.decl pidAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.principalIdealDomainCategorical pidAdapt) ≡ refl
_ = refl

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

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.euclideanDomainCategorical euclideanAdapt) tt) ≡ A.EuclideanDomainAdapter.decl euclideanAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.euclideanDomainCategorical euclideanAdapt) ≡ refl
_ = refl

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

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.multiplicativeSystemCategorical multSysAdapt) tt) ≡ A.MultiplicativeSystemAdapter.decl multSysAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.multiplicativeSystemCategorical multSysAdapt) ≡ refl
_ = refl

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

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.localizationCategorical localizationAdapt) tt) ≡ A.LocalizationAdapter.decl localizationAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.localizationCategorical localizationAdapt) ≡ refl
_ = refl

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

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.fieldOfFractionsCategorical fieldOfFracAdapt) tt) ≡ A.FieldOfFractionsAdapter.decl fieldOfFracAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.fieldOfFractionsCategorical fieldOfFracAdapt) ≡ refl
_ = refl

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

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.polynomialRingCategorical polyRingAdapt) tt) ≡ A.PolynomialRingAdapter.decl polyRingAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.polynomialRingCategorical polyRingAdapt) ≡ refl
_ = refl

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

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.quotientRingCategorical quotRingAdapt) tt) ≡ A.QuotientRingAdapter.decl quotRingAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.quotientRingCategorical quotRingAdapt) ≡ refl
_ = refl
