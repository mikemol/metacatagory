-- Tests.ModuleStructureChecklist
-- Minimal instances for module structure theorem and classification adapters

module Tests.ModuleStructureChecklist where

open import Agda.Builtin.Equality
open import Agda.Builtin.Bool as B

import Metamodel as M
import Algebra.Foundation as AF
import Algebra.Rings.Basic as AR
import Algebra.Modules.Basic as AM
import Tests.ObligationAdapters as A

-- Minimal ring declaration
ringDecl : AR.RingDeclaration
ringDecl = record
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

-- Commutative ring
commRingDecl : AR.CommutativeRingDeclaration
commRingDecl = record
  { underlyingRing = ringDecl
  ; commutativity = M.mkId "*-comm"
  }

-- Integral domain
integralDomainDecl : AR.IntegralDomain
integralDomainDecl = record
  { underlyingRing = commRingDecl
  ; nontrivial = M.mkId "nontrivial"
  ; noZeroDivisors = M.mkId "no-zero-div"
  }

-- PID
pidDecl : AR.PrincipalIdealDomain
pidDecl = record
  { domain = integralDomainDecl
  ; principalIdeals = M.mkId "principal"
  }

-- Minimal left module
leftModuleDecl : AM.LeftModule ringDecl
leftModuleDecl = record
  { underlyingAbelianGroup = record
    { underlyingGroup = record
      { underlyingMonoid = record
        { underlyingSemigroup = record
          { underlyingMagma = record
            { carrier = M.mkId "M"
            ; operation = M.mkId "+M"
            }
          ; associativity = M.mkId "M-assoc"
          }
        ; identityElement = M.mkId "0M"
        ; leftIdentity = M.mkId "M-left-id"
        ; rightIdentity = M.mkId "M-right-id"
        }
      ; inverseOp = record
        { inverse = M.mkId "negM"
        ; leftInverse = M.mkId "M-left-inv"
        ; rightInverse = M.mkId "M-right-inv"
        }
      }
    ; commutativityAxiom = record
      { commutativity = M.mkId "M-comm"
      }
    }
  ; scalarMultiplication = M.mkId "·"
  ; scalarIdentity = M.mkId "scalar-id"
  ; scalarAssociativity = M.mkId "scalar-assoc"
  ; scalarDistributivityOverVectorAddition = M.mkId "scalar-dist-M"
  ; scalarDistributivityOverScalarAddition = M.mkId "scalar-dist-R"
  }

-- Module over integral domain (for torsion elements)
leftModuleOverDomainDecl : AM.LeftModule (AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing (AR.IntegralDomain.underlyingRing integralDomainDecl)))
leftModuleOverDomainDecl = record
  { underlyingAbelianGroup = record
    { underlyingGroup = record
      { underlyingMonoid = record
        { underlyingSemigroup = record
          { underlyingMagma = record
            { carrier = M.mkId "M"
            ; operation = M.mkId "+M"
            }
          ; associativity = M.mkId "M-assoc"
          }
        ; identityElement = M.mkId "0M"
        ; leftIdentity = M.mkId "M-left-id"
        ; rightIdentity = M.mkId "M-right-id"
        }
      ; inverseOp = record
        { inverse = M.mkId "negM"
        ; leftInverse = M.mkId "M-left-inv"
        ; rightInverse = M.mkId "M-right-inv"
        }
      }
    ; commutativityAxiom = record
      { commutativity = M.mkId "M-comm"
      }
    }
  ; scalarMultiplication = M.mkId "·"
  ; scalarIdentity = M.mkId "scalar-id"
  ; scalarAssociativity = M.mkId "scalar-assoc"
  ; scalarDistributivityOverVectorAddition = M.mkId "scalar-dist-M"
  ; scalarDistributivityOverScalarAddition = M.mkId "scalar-dist-R"
  }

-- Module over PID (for structure theorem)
leftModuleOverPIDDecl : AM.LeftModule (AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing (AR.IntegralDomain.underlyingRing (AR.PrincipalIdealDomain.domain pidDecl))))
leftModuleOverPIDDecl = record
  { underlyingAbelianGroup = record
    { underlyingGroup = record
      { underlyingMonoid = record
        { underlyingSemigroup = record
          { underlyingMagma = record
            { carrier = M.mkId "M"
            ; operation = M.mkId "+M"
            }
          ; associativity = M.mkId "M-assoc"
          }
        ; identityElement = M.mkId "0M"
        ; leftIdentity = M.mkId "M-left-id"
        ; rightIdentity = M.mkId "M-right-id"
        }
      ; inverseOp = record
        { inverse = M.mkId "negM"
        ; leftInverse = M.mkId "M-left-inv"
        ; rightInverse = M.mkId "M-right-inv"
        }
      }
    ; commutativityAxiom = record
      { commutativity = M.mkId "M-comm"
      }
    }
  ; scalarMultiplication = M.mkId "·"
  ; scalarIdentity = M.mkId "scalar-id"
  ; scalarAssociativity = M.mkId "scalar-assoc"
  ; scalarDistributivityOverVectorAddition = M.mkId "scalar-dist-M"
  ; scalarDistributivityOverScalarAddition = M.mkId "scalar-dist-R"
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
torsionSubmoduleDecl = record
  { domain = integralDomainDecl
  ; module' = leftModuleOverDomainDecl
  ; torsionSubmodule = record
    { submoduleCarrier = M.mkId "T(M)"
    ; submoduleInclusion = M.mkId "incl"
    ; closedUnderAddition = M.mkId "closed-add"
    ; closedUnderScalarMultiplication = M.mkId "closed-scalar"
    ; containsZero = M.mkId "contains-0"
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
_ : A.isFilledProjectiveModule projectiveModuleAdapt ≡ B.true
_ = refl

_ : A.isFilledInjectiveModule injectiveModuleAdapt ≡ B.true
_ = refl

_ : A.isFilledTorsionElement torsionElementAdapt ≡ B.true
_ = refl

_ : A.isFilledTorsionSubmodule torsionSubmoduleAdapt ≡ B.true
_ = refl

_ : A.isFilledTorsionFreeModule torsionFreeModuleAdapt ≡ B.true
_ = refl

_ : A.isFilledStructureTheoremPID structureTheoremPIDAdapt ≡ B.true
_ = refl
