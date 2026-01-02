{-# OPTIONS --without-K #-}

-- | Minimal instances for module structure theorem and classification adapters.
module Tests.ModuleStructureChecklist where

open import Agda.Builtin.Equality
open import Core.Phase using (Bool; true; false)
open import Agda.Builtin.Unit using (⊤; tt)

import Metamodel as M
import Algebra.Foundation as AF
import Algebra.Rings.Basic as AR
import Algebra.Modules.Basic as AM
import Tests.ObligationAdapters as A
import Core.CategoricalAdapter
import Chapter1.Level1 as C1L

-- | Minimal ring declaration (modern API).
ringDecl : AR.RingDeclaration
ringDecl =
  let
    -- Addition semigroup
    plusSemigroup : AF.SemigroupDeclaration
    plusSemigroup = record
      { underlyingMagma = record { underlyingSet = M.mkId "Z" ; binaryOp = M.mkId "+" ; index = AF.magmaIndex }
      ; associativity = C1L.AXIOM_Associativity (M.mkId "+-assoc")
      ; index = AF.semigroupIndex
      }

    -- Addition monoid
    plusMonoid : AF.MonoidDeclaration
    plusMonoid = record
      { underlyingSemigroup = plusSemigroup
      ; identityElement = M.mkId "0"
      ; identityAxiom = C1L.AXIOM_Identity (M.mkId "+-id")
      ; index = AF.monoidIndex
      }

    -- Addition group
    plusGroup : AF.GroupDeclaration
    plusGroup = record
      { underlyingMonoid = plusMonoid
      ; inverseOperation =
          record
            { forMonoid = plusMonoid
            ; inverseMap = M.mkId "neg"
            ; inverseAxiom = M.mkId "+-inv"
            }
      ; index = AF.groupIndex
      }

    -- Abelian additive group
    addAbelian : AF.AbelianGroupDeclaration
    addAbelian = record
      { underlyingGroup = plusGroup
      ; commutativity = record { forGroup = plusGroup ; axiom = M.mkId "+-comm" }
      ; index = AF.abelianGroupIndex
      }
  in
  record
    { identifier = M.mkId "Z"
    ; additiveGroup = addAbelian
    ; multiplication = M.mkId "*"
    ; multAssociative = M.mkId "*-assoc"
    ; leftDistributive = M.mkId "left-dist"
    ; rightDistributive = M.mkId "right-dist"
    }

-- | Commutative ring (modern API).
commRingDecl : AR.CommutativeRingDeclaration
commRingDecl =
  let
    unitalRing : AR.UnitalRingDeclaration
    unitalRing = record
      { underlyingRing = ringDecl
      ; multiplicativeIdentity = M.mkId "1"
      ; leftIdentity = M.mkId "*-left-id"
      ; rightIdentity = M.mkId "*-right-id"
      }
  in
  -- | Commutative ring (modern API) built from the unital ring.
  record { underlyingRing = unitalRing ; commutativity = M.mkId "*-comm" }

-- | Integral domain (modern API).
integralDomainDecl : AR.IntegralDomain
integralDomainDecl = record
  { underlyingRing = commRingDecl
  ; noZeroDivisors = M.mkId "no-zero-div"
  }

-- | Principal ideal domain (modern API).
pidDecl : AR.PrincipalIdealDomain
pidDecl = record
  { domain = integralDomainDecl
  ; allIdealsPrincipal = M.mkId "principal"
  }

-- | Minimal left module (modern API).
leftModuleDecl : AM.LeftModule ringDecl
leftModuleDecl =
  let
    -- Module addition semigroup
    mSemigroup : AF.SemigroupDeclaration
    mSemigroup = record
      { underlyingMagma = record { underlyingSet = M.mkId "M" ; binaryOp = M.mkId "+M" ; index = AF.magmaIndex }
      ; associativity = C1L.AXIOM_Associativity (M.mkId "M-assoc")
      ; index = AF.semigroupIndex
      }

    -- Module addition monoid
    mMonoid : AF.MonoidDeclaration
    mMonoid = record
      { underlyingSemigroup = mSemigroup
      ; identityElement = M.mkId "0M"
      ; identityAxiom = C1L.AXIOM_Identity (M.mkId "M-id")
      ; index = AF.monoidIndex
      }

    -- Module addition group
    mGroup : AF.GroupDeclaration
    mGroup = record
      { underlyingMonoid = mMonoid
      ; inverseOperation =
          record
            { forMonoid = mMonoid
            ; inverseMap = M.mkId "negM"
            ; inverseAxiom = M.mkId "M-inv"
            }
      ; index = AF.groupIndex
      }

    mAbelian : AF.AbelianGroupDeclaration
    mAbelian = record
      { underlyingGroup = mGroup
      ; commutativity = record { forGroup = mGroup ; axiom = M.mkId "M-comm" }
      ; index = AF.abelianGroupIndex
      }
  in
  record
    { ring = ringDecl
    ; underlyingAbelianGroup = mAbelian
    ; scalarMultiplication = M.mkId "·"
    ; distributiveOverAddition = M.mkId "scalar-dist-M"
    ; distributiveOverRingAddition = M.mkId "scalar-dist-R"
    ; associativeScalar = M.mkId "scalar-assoc"
    ; unitalAction = M.mkId "scalar-id"
    }

-- Module over integral domain (for torsion elements)
leftModuleOverDomainDecl : AM.LeftModule (AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing (AR.IntegralDomain.underlyingRing integralDomainDecl)))
leftModuleOverDomainDecl =
  let
    mSemigroup : AF.SemigroupDeclaration
    mSemigroup = record
      { underlyingMagma = record { underlyingSet = M.mkId "M" ; binaryOp = M.mkId "+M" ; index = AF.magmaIndex }
      ; associativity = C1L.AXIOM_Associativity (M.mkId "M-assoc")
      ; index = AF.semigroupIndex
      }

    mMonoid : AF.MonoidDeclaration
    mMonoid = record
      { underlyingSemigroup = mSemigroup
      ; identityElement = M.mkId "0M"
      ; identityAxiom = C1L.AXIOM_Identity (M.mkId "M-id")
      ; index = AF.monoidIndex
      }

    mGroup : AF.GroupDeclaration
    mGroup = record
      { underlyingMonoid = mMonoid
      ; inverseOperation =
          record
            { forMonoid = mMonoid
            ; inverseMap = M.mkId "negM"
            ; inverseAxiom = M.mkId "M-inv"
            }
      ; index = AF.groupIndex
      }

    mAbelian : AF.AbelianGroupDeclaration
    mAbelian = record
      { underlyingGroup = mGroup
      ; commutativity = record { forGroup = mGroup ; axiom = M.mkId "M-comm" }
      ; index = AF.abelianGroupIndex
      }

    baseRing : AR.RingDeclaration
    baseRing = AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing (AR.IntegralDomain.underlyingRing integralDomainDecl))
  in
  record
    { ring = baseRing
    ; underlyingAbelianGroup = mAbelian
    ; scalarMultiplication = M.mkId "·"
    ; distributiveOverAddition = M.mkId "scalar-dist-M"
    ; distributiveOverRingAddition = M.mkId "scalar-dist-R"
    ; associativeScalar = M.mkId "scalar-assoc"
    ; unitalAction = M.mkId "scalar-id"
    }

-- Module over PID (for structure theorem)
leftModuleOverPIDDecl : AM.LeftModule (AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing (AR.IntegralDomain.underlyingRing (AR.PrincipalIdealDomain.domain pidDecl))))
leftModuleOverPIDDecl =
  let
    mSemigroup : AF.SemigroupDeclaration
    mSemigroup = record
      { underlyingMagma = record { underlyingSet = M.mkId "M" ; binaryOp = M.mkId "+M" ; index = AF.magmaIndex }
      ; associativity = C1L.AXIOM_Associativity (M.mkId "M-assoc")
      ; index = AF.semigroupIndex
      }

    mMonoid : AF.MonoidDeclaration
    mMonoid = record
      { underlyingSemigroup = mSemigroup
      ; identityElement = M.mkId "0M"
      ; identityAxiom = C1L.AXIOM_Identity (M.mkId "M-id")
      ; index = AF.monoidIndex
      }

    mGroup : AF.GroupDeclaration
    mGroup = record
      { underlyingMonoid = mMonoid
      ; inverseOperation =
          record
            { forMonoid = mMonoid
            ; inverseMap = M.mkId "negM"
            ; inverseAxiom = M.mkId "M-inv"
            }
      ; index = AF.groupIndex
      }

    mAbelian : AF.AbelianGroupDeclaration
    mAbelian = record
      { underlyingGroup = mGroup
      ; commutativity = record { forGroup = mGroup ; axiom = M.mkId "M-comm" }
      ; index = AF.abelianGroupIndex
      }

    baseRing : AR.RingDeclaration
    baseRing = AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing (AR.IntegralDomain.underlyingRing (AR.PrincipalIdealDomain.domain pidDecl)))
  in
  record
    { ring = baseRing
    ; underlyingAbelianGroup = mAbelian
    ; scalarMultiplication = M.mkId "·"
    ; distributiveOverAddition = M.mkId "scalar-dist-M"
    ; distributiveOverRingAddition = M.mkId "scalar-dist-R"
    ; associativeScalar = M.mkId "scalar-assoc"
    ; unitalAction = M.mkId "scalar-id"
    }

-- Projective module
projectiveModuleDecl : AM.ProjectiveModule ringDecl leftModuleDecl
projectiveModuleDecl = record
  { ring = ringDecl
  ; module' = leftModuleDecl
  ; liftingProperty = M.mkId "lift-prop"
  }

-- Injective module
injectiveModuleDecl : AM.InjectiveModule ringDecl leftModuleDecl
injectiveModuleDecl = record
  { ring = ringDecl
  ; module' = leftModuleDecl
  ; extensionProperty = M.mkId "ext-prop"
  }

-- Torsion element
torsionElementDecl : AM.TorsionElement integralDomainDecl leftModuleOverDomainDecl (M.mkId "m")
torsionElementDecl = record
  { domain = integralDomainDecl
  ; module' = leftModuleOverDomainDecl
  ; element = M.mkId "m"
  ; isTorsion = M.mkId "is-torsion"
  }

-- Torsion submodule
torsionSubmoduleDecl : AM.TorsionSubmodule integralDomainDecl leftModuleOverDomainDecl
torsionSubmoduleDecl =
  let
    baseRing : AR.RingDeclaration
    baseRing = AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing (AR.IntegralDomain.underlyingRing integralDomainDecl))

    grp : AF.GroupDeclaration
    grp = AF.AbelianGroupDeclaration.underlyingGroup (AM.LeftModule.underlyingAbelianGroup leftModuleOverDomainDecl)
  in
  record
    { domain = integralDomainDecl
    ; module' = leftModuleOverDomainDecl
    ; torsionSubmodule = record
        { ring = baseRing
        ; module' = leftModuleOverDomainDecl
        ; subgroup = record
            { subset = M.mkId "T(M)"
            ; inclusion = M.mkId "incl"
            ; closedUnderOp = M.mkId "closed-add"
            ; containsIdentity = M.mkId "contains-0"
            ; closedUnderInverse = M.mkId "closed-inv"
            }
        ; closedUnderScalars = M.mkId "closed-scalar"
        }
    }

-- Torsion-free module
torsionFreeModuleDecl : AM.TorsionFreeModule integralDomainDecl leftModuleOverDomainDecl
torsionFreeModuleDecl = record
  { domain = integralDomainDecl
  ; module' = leftModuleOverDomainDecl
  ; isTorsionFree = M.mkId "torsion-free"
  }

-- Structure theorem for PID
structureTheoremPIDDecl : AM.StructureTheoremPID pidDecl leftModuleOverPIDDecl
structureTheoremPIDDecl = record
  { pid = pidDecl
  ; module' = leftModuleOverPIDDecl
  ; decomposition = M.mkId "M≅R^r⊕R/(a₁)⊕...⊕R/(aₙ)"
  }

-- Adapter instances
projectiveModuleAdapt : A.ProjectiveModuleAdapter
projectiveModuleAdapt = A.mkProjectiveModuleAdapter ringDecl leftModuleDecl projectiveModuleDecl ringDecl refl

injectiveModuleAdapt : A.InjectiveModuleAdapter
injectiveModuleAdapt = A.mkInjectiveModuleAdapter ringDecl leftModuleDecl injectiveModuleDecl ringDecl refl

torsionElementAdapt : A.TorsionElementAdapter
torsionElementAdapt = A.mkTorsionElementAdapter integralDomainDecl leftModuleOverDomainDecl (M.mkId "m") torsionElementDecl integralDomainDecl refl

torsionSubmoduleAdapt : A.TorsionSubmoduleAdapter
torsionSubmoduleAdapt = A.mkTorsionSubmoduleAdapter integralDomainDecl leftModuleOverDomainDecl torsionSubmoduleDecl integralDomainDecl refl

torsionFreeModuleAdapt : A.TorsionFreeModuleAdapter
torsionFreeModuleAdapt = A.mkTorsionFreeModuleAdapter integralDomainDecl leftModuleOverDomainDecl torsionFreeModuleDecl integralDomainDecl refl

structureTheoremPIDAdapt : A.StructureTheoremPIDAdapter
structureTheoremPIDAdapt = A.mkStructureTheoremPIDAdapter pidDecl leftModuleOverPIDDecl structureTheoremPIDDecl pidDecl refl

-- Status assertions
_ : A.isFilledProjectiveModule projectiveModuleAdapt ≡ true
_ = refl

_ : A.isFilledInjectiveModule injectiveModuleAdapt ≡ true
_ = refl

_ : A.isFilledTorsionElement torsionElementAdapt ≡ true
_ = refl

_ : A.isFilledTorsionSubmodule torsionSubmoduleAdapt ≡ true
_ = refl

_ : A.isFilledTorsionFreeModule torsionFreeModuleAdapt ≡ true
_ = refl

_ : A.isFilledStructureTheoremPID structureTheoremPIDAdapt ≡ true
_ = refl

-- Categorical assertions (omitted; covered by adapter wiring)
