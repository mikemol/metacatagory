{-# OPTIONS --allow-unsolved-metas #-}

module Tests.AlgebraicCompletionChecklist where

open import Core.Phase using (Bool; true; false)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Agda.Builtin.Unit using (⊤)
import Agda.Builtin.Nat as N
import Agda.Builtin.String as S
import Metamodel as M
-- Imports
import Chapter1.Level1 as C1L
import Algebra.Foundation as AFo
import Algebra.Rings.Basic as AR
import Algebra.Fields.Basic as AFB
import Algebra.Groups.Basic as AGB
import Algebra.Groups.Abelian as AGA
import Algebra.Modules.Basic as AM
import Tests.ObligationAdapters as A
import Core.CategoricalAdapter as Core

-- ============================================================================
-- Setup: Basic structures for module category theory and R-algebras
-- ============================================================================

-- Ring ℤ
ringId : M.Identifier
ringId = M.mkId "ℤ"

abGroupMagmaDecl : AFo.MagmaDeclaration
abGroupMagmaDecl = record
  { underlyingSet = M.mkId "ℤ"
  ; binaryOp = M.mkId "+"
  }

abGroupSemigroupDecl : AFo.SemigroupDeclaration
abGroupSemigroupDecl = record
  { underlyingMagma = abGroupMagmaDecl
  ; associativity = C1L.AXIOM_Associativity (M.mkId "+-assoc")
  }

abGroupMonoidDecl : AFo.MonoidDeclaration
abGroupMonoidDecl = record
  { underlyingSemigroup = abGroupSemigroupDecl
  ; identityElement = M.mkId "0"
  ; identityAxiom = C1L.AXIOM_Identity (M.mkId "0-identity")
  }

abGroupGroupDecl : AFo.GroupDeclaration
abGroupGroupDecl = record
  { underlyingMonoid = abGroupMonoidDecl
  ; inverseOperation = record
    { forMonoid = abGroupMonoidDecl
    ; inverseMap = M.mkId "neg"
    ; inverseAxiom = M.mkId "neg-property"
    }
  }

abGroupDecl : AFo.AbelianGroupDeclaration
abGroupDecl = record
  { underlyingGroup = abGroupGroupDecl
  ; commutativity = record
    { forGroup = abGroupGroupDecl
    ; axiom = M.mkId "+-comm"
    }
  }

ringDecl : AR.RingDeclaration
ringDecl = record
  { identifier = ringId
  ; additiveGroup = abGroupDecl
  ; multiplication = M.mkId "·"
  ; multAssociative = M.mkId "·-assoc"
  ; leftDistributive = M.mkId "left-dist"
  ; rightDistributive = M.mkId "right-dist"
  }

-- Commutative ring (also ℤ)
commRingDecl : AR.CommutativeRingDeclaration
commRingDecl = record
  { underlyingRing = record
    { underlyingRing = ringDecl
    ; multiplicativeIdentity = M.mkId "1"
    ; leftIdentity = M.mkId "1-left-id"
    ; rightIdentity = M.mkId "1-right-id"
    }
  ; commutativity = M.mkId "·-comm"
  }

-- Field ℚ
fieldId : M.Identifier
fieldId = M.mkId "ℚ"

private
  ℚ-magma : AFo.MagmaDeclaration
  ℚ-magma = record { underlyingSet = M.mkId "ℚ⁺" ; binaryOp = M.mkId "+" }

  ℚ-semigroup : AFo.SemigroupDeclaration
  ℚ-semigroup = record { underlyingMagma = ℚ-magma ; associativity = C1L.AXIOM_Associativity (M.mkId "+-assoc") }

  ℚ-monoid : AFo.MonoidDeclaration
  ℚ-monoid = record { underlyingSemigroup = ℚ-semigroup ; identityElement = M.mkId "0" ; identityAxiom = C1L.AXIOM_Identity (M.mkId "0-identity") }

  ℚ-group : AFo.GroupDeclaration
  ℚ-group = record { underlyingMonoid = ℚ-monoid ; inverseOperation = record { forMonoid = ℚ-monoid ; inverseMap = M.mkId "neg" ; inverseAxiom = M.mkId "neg-property" } }

fieldAbGroupDecl : AFo.AbelianGroupDeclaration
fieldAbGroupDecl = record { underlyingGroup = ℚ-group ; commutativity = record { forGroup = ℚ-group ; axiom = M.mkId "+-comm" } }

fieldRingDecl : AR.RingDeclaration
fieldRingDecl = record
  { identifier = M.mkId "ℚ-ring"
  ; additiveGroup = fieldAbGroupDecl
  ; multiplication = M.mkId "·"
  ; multAssociative = M.mkId "·-assoc"
  ; leftDistributive = M.mkId "left-dist"
  ; rightDistributive = M.mkId "right-dist"
  }

fieldDecl : AR.FieldDeclaration
fieldDecl = record
  { underlyingRing = record
    { underlyingRing = record
      { underlyingRing = fieldRingDecl
      ; multiplicativeIdentity = M.mkId "1"
      ; leftIdentity = M.mkId "1-left-id"
      ; rightIdentity = M.mkId "1-right-id"
      }
    ; commutativity = M.mkId "·-comm"
    }
  ; inverses = M.mkId "ℚ*"
  }

-- Modules
moduleId1 : M.Identifier
moduleId1 = M.mkId "M"

private
  M-magma : AFo.MagmaDeclaration
  M-magma = record { underlyingSet = M.mkId "M-group" ; binaryOp = M.mkId "+" }

  M-semigroup : AFo.SemigroupDeclaration
  M-semigroup = record { underlyingMagma = M-magma ; associativity = C1L.AXIOM_Associativity (M.mkId "+-assoc") }

  M-monoid : AFo.MonoidDeclaration
  M-monoid = record { underlyingSemigroup = M-semigroup ; identityElement = M.mkId "0" ; identityAxiom = C1L.AXIOM_Identity (M.mkId "0-identity") }

  M-group : AFo.GroupDeclaration
  M-group = record { underlyingMonoid = M-monoid ; inverseOperation = record { forMonoid = M-monoid ; inverseMap = M.mkId "neg" ; inverseAxiom = M.mkId "neg-property" } }

moduleAbGroup1 : AFo.AbelianGroupDeclaration
moduleAbGroup1 = record { underlyingGroup = M-group ; commutativity = record { forGroup = M-group ; axiom = M.mkId "+-comm" } }

unitalRingDecl : AR.UnitalRingDeclaration
unitalRingDecl = record
  { underlyingRing = ringDecl
  ; multiplicativeIdentity = M.mkId "1"
  ; leftIdentity = M.mkId "1-left-id"
  ; rightIdentity = M.mkId "1-right-id"
  }

module1 : AM.LeftModule ringDecl
module1 = record
  { ring = ringDecl
  ; underlyingAbelianGroup = moduleAbGroup1
  ; scalarMultiplication = M.mkId "·"
  ; distributiveOverAddition = M.mkId "r(m+n)=rm+rn"
  ; distributiveOverRingAddition = M.mkId "(r+s)m=rm+sm"
  ; associativeScalar = M.mkId "(rs)m=r(sm)"
  ; unitalAction = M.mkId "1m=m"
  }

module2 : AM.LeftModule ringDecl
module2 = record
  { ring = ringDecl
  ; underlyingAbelianGroup = record
    { underlyingGroup = record
      { underlyingMonoid = record
        { underlyingSemigroup = record
          { underlyingMagma = record { underlyingSet = M.mkId "N" ; binaryOp = M.mkId "+" }
          ; associativity = C1L.AXIOM_Associativity (M.mkId "+-assoc")
          }
        ; identityElement = M.mkId "0"
        ; identityAxiom = C1L.AXIOM_Identity (M.mkId "0-identity")
        }
      ; inverseOperation = record
        { forMonoid = record
          { underlyingSemigroup = record
            { underlyingMagma = record { underlyingSet = M.mkId "N" ; binaryOp = M.mkId "+" }
            ; associativity = C1L.AXIOM_Associativity (M.mkId "+-assoc")
            }
          ; identityElement = M.mkId "0"
          ; identityAxiom = C1L.AXIOM_Identity (M.mkId "0-identity")
          }
        ; inverseMap = M.mkId "neg"
        ; inverseAxiom = M.mkId "neg-property"
        }
      }
    ; commutativity = record
      { forGroup = record
        { underlyingMonoid = record
          { underlyingSemigroup = record
            { underlyingMagma = record { underlyingSet = M.mkId "N" ; binaryOp = M.mkId "+" }
            ; associativity = C1L.AXIOM_Associativity (M.mkId "+-assoc")
            }
          ; identityElement = M.mkId "0"
          ; identityAxiom = C1L.AXIOM_Identity (M.mkId "0-identity")
          }
        ; inverseOperation = record
          { forMonoid = record
            { underlyingSemigroup = record
              { underlyingMagma = record { underlyingSet = M.mkId "N" ; binaryOp = M.mkId "+" }
              ; associativity = C1L.AXIOM_Associativity (M.mkId "+-assoc")
              }
            ; identityElement = M.mkId "0"
            ; identityAxiom = C1L.AXIOM_Identity (M.mkId "0-identity")
            }
          ; inverseMap = M.mkId "neg"
          ; inverseAxiom = M.mkId "neg-property"
          }
        }
      ; axiom = M.mkId "+-comm"
      }
    }
  ; scalarMultiplication = M.mkId "·"
  ; distributiveOverAddition = M.mkId "r(m+n)=rm+rn"
  ; distributiveOverRingAddition = M.mkId "(r+s)m=rm+sm"
  ; associativeScalar = M.mkId "(rs)m=r(sm)"
  ; unitalAction = M.mkId "1m=m"
  }

module3 : AM.LeftModule ringDecl
module3 = record
  { ring = ringDecl
  ; underlyingAbelianGroup = record
    { underlyingGroup = record
      { underlyingMonoid = record
        { underlyingSemigroup = record
          { underlyingMagma = record { underlyingSet = M.mkId "P" ; binaryOp = M.mkId "+" }
          ; associativity = C1L.AXIOM_Associativity (M.mkId "+-assoc")
          }
        ; identityElement = M.mkId "0"
        ; identityAxiom = C1L.AXIOM_Identity (M.mkId "0-identity")
        }
      ; inverseOperation = record
        { forMonoid = record
          { underlyingSemigroup = record
            { underlyingMagma = record { underlyingSet = M.mkId "P" ; binaryOp = M.mkId "+" }
            ; associativity = C1L.AXIOM_Associativity (M.mkId "+-assoc")
            }
          ; identityElement = M.mkId "0"
          ; identityAxiom = C1L.AXIOM_Identity (M.mkId "0-identity")
          }
        ; inverseMap = M.mkId "neg"
        ; inverseAxiom = M.mkId "neg-property"
        }
      }
    ; commutativity = record
      { forGroup = record
        { underlyingMonoid = record
          { underlyingSemigroup = record
            { underlyingMagma = record { underlyingSet = M.mkId "P" ; binaryOp = M.mkId "+" }
            ; associativity = C1L.AXIOM_Associativity (M.mkId "+-assoc")
            }
          ; identityElement = M.mkId "0"
          ; identityAxiom = C1L.AXIOM_Identity (M.mkId "0-identity")
          }
        ; inverseOperation = record
          { forMonoid = record
            { underlyingSemigroup = record
              { underlyingMagma = record { underlyingSet = M.mkId "P" ; binaryOp = M.mkId "+" }
              ; associativity = C1L.AXIOM_Associativity (M.mkId "+-assoc")
              }
            ; identityElement = M.mkId "0"
            ; identityAxiom = C1L.AXIOM_Identity (M.mkId "0-identity")
            }
          ; inverseMap = M.mkId "neg"
          ; inverseAxiom = M.mkId "neg-property"
          }
        }
      ; axiom = M.mkId "+-comm"
      }
    }
  ; scalarMultiplication = M.mkId "·"
  ; distributiveOverAddition = M.mkId "r(m+n)=rm+rn"
  ; distributiveOverRingAddition = M.mkId "(r+s)m=rm+sm"
  ; associativeScalar = M.mkId "(rs)m=r(sm)"
  ; unitalAction = M.mkId "1m=m"
  }

-- ============================================================================
-- 1. Exact Sequence
-- ============================================================================

exactSequence : AM.ExactSequence ringDecl
exactSequence = record
  { ring = ringDecl
  ; modules = M.mkId "M₀→M₁→M₂→..."
  ; morphisms = M.mkId "f₀,f₁,f₂,..."
  ; exactnessCondition = M.mkId "im(fᵢ)=ker(fᵢ₊₁)"
  }

exactSequenceAdapt : A.ExactSequenceAdapter
exactSequenceAdapt =
  A.mkExactSequenceAdapter
    ringDecl
    exactSequence
    ringDecl
    refl

_ : A.isFilledExactSequence exactSequenceAdapt ≡ true
_ = refl

-- ============================================================================
-- 2. Category of Modules
-- ============================================================================

categoryOfModules : AM.CategoryOfModules ringDecl
categoryOfModules = record
  { ring = ringDecl
  ; category = M.mkId "ℤ-Mod"
  ; isAbelian = M.mkId "ℤ-Mod-is-abelian"
  }

categoryOfModulesAdapt : A.CategoryOfModulesAdapter
categoryOfModulesAdapt =
  A.mkCategoryOfModulesAdapter
    ringDecl
    categoryOfModules
    ringDecl
    refl

_ : A.isFilledCategoryOfModules categoryOfModulesAdapt ≡ true
_ = refl

-- ============================================================================
-- 3. Vector Space
-- ============================================================================

-- Extract underlying ring from field
fieldUnitalRing : AR.UnitalRingDeclaration
fieldUnitalRing = record
  { underlyingRing = fieldRingDecl
  ; multiplicativeIdentity = M.mkId "1"
  ; leftIdentity = M.mkId "1-left-id"
  ; rightIdentity = M.mkId "1-right-id"
  }

fieldCommRing : AR.CommutativeRingDeclaration
fieldCommRing = record
  { underlyingRing = fieldUnitalRing
  ; commutativity = M.mkId "·-comm"
  }

fieldAsRing : AR.RingDeclaration
fieldAsRing = AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing fieldCommRing)

vectorSpaceModule : AM.LeftModule fieldAsRing
vectorSpaceModule = record
  { ring = fieldAsRing
  ; underlyingAbelianGroup = record
    { underlyingGroup = record
      { underlyingMonoid = record
        { underlyingSemigroup = record
          { underlyingMagma = record { underlyingSet = M.mkId "V" ; binaryOp = M.mkId "+" }
          ; associativity = C1L.AXIOM_Associativity (M.mkId "+-assoc")
          }
        ; identityElement = M.mkId "0"
        ; identityAxiom = C1L.AXIOM_Identity (M.mkId "0-identity")
        }
      ; inverseOperation = record
        { forMonoid = record
          { underlyingSemigroup = record
            { underlyingMagma = record { underlyingSet = M.mkId "V" ; binaryOp = M.mkId "+" }
            ; associativity = C1L.AXIOM_Associativity (M.mkId "+-assoc")
            }
          ; identityElement = M.mkId "0"
          ; identityAxiom = C1L.AXIOM_Identity (M.mkId "0-identity")
          }
        ; inverseMap = M.mkId "neg"
        ; inverseAxiom = M.mkId "neg-property"
        }
      }
    ; commutativity = record
      { forGroup = record
        { underlyingMonoid = record
          { underlyingSemigroup = record
            { underlyingMagma = record { underlyingSet = M.mkId "V" ; binaryOp = M.mkId "+" }
            ; associativity = C1L.AXIOM_Associativity (M.mkId "+-assoc")
            }
          ; identityElement = M.mkId "0"
          ; identityAxiom = C1L.AXIOM_Identity (M.mkId "0-identity")
          }
        ; inverseOperation = record
          { forMonoid = record
            { underlyingSemigroup = record
              { underlyingMagma = record { underlyingSet = M.mkId "V" ; binaryOp = M.mkId "+" }
              ; associativity = C1L.AXIOM_Associativity (M.mkId "+-assoc")
              }
            ; identityElement = M.mkId "0"
            ; identityAxiom = C1L.AXIOM_Identity (M.mkId "0-identity")
            }
          ; inverseMap = M.mkId "neg"
          ; inverseAxiom = M.mkId "neg-property"
          }
        }
      ; axiom = M.mkId "+-comm"
      }
    }
  ; scalarMultiplication = M.mkId "·"
  ; distributiveOverAddition = M.mkId "r(v+w)=rv+rw"
  ; distributiveOverRingAddition = M.mkId "(r+s)v=rv+sv"
  ; associativeScalar = M.mkId "(rs)v=r(sv)"
  ; unitalAction = M.mkId "1v=v"
  }

vectorSpace : AM.VectorSpace fieldDecl
vectorSpace = record
  { field' = fieldDecl
  ; underlyingModule = vectorSpaceModule
  }

vectorSpaceAdapt : A.VectorSpaceAdapter
vectorSpaceAdapt =
  A.mkVectorSpaceAdapter
    fieldDecl
    vectorSpace
    fieldDecl
    refl

_ : A.isFilledVectorSpace vectorSpaceAdapt ≡ true
_ = refl

-- ============================================================================
-- 4. R-Algebra
-- ============================================================================

-- Polynomial ring ℤ[X] as an algebra over ℤ
polyRingDecl : AR.RingDeclaration
polyRingDecl = record
  { identifier = M.mkId "ℤ[X]"
  ; additiveGroup = record
    { underlyingGroup = record
      { underlyingMonoid = record
        { underlyingSemigroup = record
          { underlyingMagma = record { underlyingSet = M.mkId "ℤ[X]⁺" ; binaryOp = M.mkId "+" }
          ; associativity = C1L.AXIOM_Associativity (M.mkId "+-assoc")
          }
        ; identityElement = M.mkId "0"
        ; identityAxiom = C1L.AXIOM_Identity (M.mkId "0-identity")
        }
      ; inverseOperation = record
        { forMonoid = record
          { underlyingSemigroup = record
            { underlyingMagma = record { underlyingSet = M.mkId "ℤ[X]⁺" ; binaryOp = M.mkId "+" }
            ; associativity = C1L.AXIOM_Associativity (M.mkId "+-assoc")
            }
          ; identityElement = M.mkId "0"
          ; identityAxiom = C1L.AXIOM_Identity (M.mkId "0-identity")
          }
        ; inverseMap = M.mkId "neg"
        ; inverseAxiom = M.mkId "neg-property"
        }
      }
    ; commutativity = record
      { forGroup = record
        { underlyingMonoid = record
          { underlyingSemigroup = record
            { underlyingMagma = record { underlyingSet = M.mkId "ℤ[X]⁺" ; binaryOp = M.mkId "+" }
            ; associativity = C1L.AXIOM_Associativity (M.mkId "+-assoc")
            }
          ; identityElement = M.mkId "0"
          ; identityAxiom = C1L.AXIOM_Identity (M.mkId "0-identity")
          }
        ; inverseOperation = record
          { forMonoid = record
            { underlyingSemigroup = record
              { underlyingMagma = record { underlyingSet = M.mkId "ℤ[X]⁺" ; binaryOp = M.mkId "+" }
              ; associativity = C1L.AXIOM_Associativity (M.mkId "+-assoc")
              }
            ; identityElement = M.mkId "0"
            ; identityAxiom = C1L.AXIOM_Identity (M.mkId "0-identity")
            }
          ; inverseMap = M.mkId "neg"
          ; inverseAxiom = M.mkId "neg-property"
          }
        }
      ; axiom = M.mkId "+-comm"
      }
    }
  ; multiplication = M.mkId "·"
  ; multAssociative = M.mkId "·-assoc"
  ; leftDistributive = M.mkId "left-dist"
  ; rightDistributive = M.mkId "right-dist"
  }

polyModuleStructure : AM.LeftModule (AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing commRingDecl))
polyModuleStructure = record
  { ring = AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing commRingDecl)
  ; underlyingAbelianGroup = AR.RingDeclaration.additiveGroup polyRingDecl
  ; scalarMultiplication = M.mkId "r·p"
  ; distributiveOverAddition = M.mkId "r(p+q)=rp+rq"
  ; distributiveOverRingAddition = M.mkId "(r+s)p=rp+sp"
  ; associativeScalar = M.mkId "(rs)p=r(sp)"
  ; unitalAction = M.mkId "1·p=p"
  }

rAlgebra : AM.RAlgebra commRingDecl
rAlgebra = record
  { coefficientRing = commRingDecl
  ; underlyingRing = polyRingDecl
  ; moduleStructure = polyModuleStructure
  ; compatibility = M.mkId "r(pq)=(rp)q=p(rq)"
  }

rAlgebraAdapt : A.RAlgebraAdapter
rAlgebraAdapt =
  A.mkRAlgebraAdapter
    commRingDecl
    rAlgebra
    commRingDecl
    refl

_ : A.isFilledRAlgebra rAlgebraAdapt ≡ true
_ = refl

-- ============================================================================
-- 5. Algebra Homomorphism
-- ============================================================================

-- Second algebra (ℤ[Y])
polyRingDecl2 : AR.RingDeclaration
polyRingDecl2 = record
  { identifier = M.mkId "ℤ[Y]"
  ; additiveGroup = record
    { underlyingGroup = record
      { underlyingMonoid = record
        { underlyingSemigroup = record
          { underlyingMagma = record { underlyingSet = M.mkId "ℤ[Y]⁺" ; binaryOp = M.mkId "+" }
          ; associativity = C1L.AXIOM_Associativity (M.mkId "+-assoc")
          }
        ; identityElement = M.mkId "0"
        ; identityAxiom = C1L.AXIOM_Identity (M.mkId "0-identity")
        }
      ; inverseOperation = record
        { forMonoid = record
          { underlyingSemigroup = record
            { underlyingMagma = record { underlyingSet = M.mkId "ℤ[Y]⁺" ; binaryOp = M.mkId "+" }
            ; associativity = C1L.AXIOM_Associativity (M.mkId "+-assoc")
            }
          ; identityElement = M.mkId "0"
          ; identityAxiom = C1L.AXIOM_Identity (M.mkId "0-identity")
          }
        ; inverseMap = M.mkId "neg"
        ; inverseAxiom = M.mkId "neg-property"
        }
      }
    ; commutativity = record
      { forGroup = record
        { underlyingMonoid = record
          { underlyingSemigroup = record
            { underlyingMagma = record { underlyingSet = M.mkId "ℤ[Y]⁺" ; binaryOp = M.mkId "+" }
            ; associativity = C1L.AXIOM_Associativity (M.mkId "+-assoc")
            }
          ; identityElement = M.mkId "0"
          ; identityAxiom = C1L.AXIOM_Identity (M.mkId "0-identity")
          }
        ; inverseOperation = record
          { forMonoid = record
            { underlyingSemigroup = record
              { underlyingMagma = record { underlyingSet = M.mkId "ℤ[Y]⁺" ; binaryOp = M.mkId "+" }
              ; associativity = C1L.AXIOM_Associativity (M.mkId "+-assoc")
              }
            ; identityElement = M.mkId "0"
            ; identityAxiom = C1L.AXIOM_Identity (M.mkId "0-identity")
            }
          ; inverseMap = M.mkId "neg"
          ; inverseAxiom = M.mkId "neg-property"
          }
        }
      ; axiom = M.mkId "+-comm"
      }
    }
  ; multiplication = M.mkId "·"
  ; multAssociative = M.mkId "·-assoc"
  ; leftDistributive = M.mkId "left-dist"
  ; rightDistributive = M.mkId "right-dist"
  }

polyModuleStructure2 : AM.LeftModule (AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing commRingDecl))
polyModuleStructure2 = record
  { ring = AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing commRingDecl)
  ; underlyingAbelianGroup = AR.RingDeclaration.additiveGroup polyRingDecl2
  ; scalarMultiplication = M.mkId "r·p"
  ; distributiveOverAddition = M.mkId "r(p+q)=rp+rq"
  ; distributiveOverRingAddition = M.mkId "(r+s)p=rp+sp"
  ; associativeScalar = M.mkId "(rs)p=r(sp)"
  ; unitalAction = M.mkId "1·p=p"
  }

rAlgebra2 : AM.RAlgebra commRingDecl
rAlgebra2 = record
  { coefficientRing = commRingDecl
  ; underlyingRing = polyRingDecl2
  ; moduleStructure = polyModuleStructure2
  ; compatibility = M.mkId "r(pq)=(rp)q=p(rq)"
  }

algebraHomomorphism : AM.AlgebraHomomorphism commRingDecl rAlgebra rAlgebra2
algebraHomomorphism = record
  { coefficientRing = commRingDecl
  ; sourceAlgebra = rAlgebra
  ; targetAlgebra = rAlgebra2
  ; ringHomomorphism = M.mkId "φ:ℤ[X]→ℤ[Y]"
  ; moduleHomomorphism = M.mkId "φ-linear"
  }

algebraHomomorphismAdapt : A.AlgebraHomomorphismAdapter
algebraHomomorphismAdapt =
  A.mkAlgebraHomomorphismAdapter
    commRingDecl
    rAlgebra
    rAlgebra2
    algebraHomomorphism
    commRingDecl
    refl

_ : A.isFilledAlgebraHomomorphism algebraHomomorphismAdapt ≡ true
_ = refl
