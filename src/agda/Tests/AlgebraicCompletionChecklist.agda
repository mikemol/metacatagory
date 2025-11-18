{-# OPTIONS --allow-unsolved-metas #-}

module Tests.AlgebraicCompletionChecklist where

open import Agda.Builtin.Equality using (_≡_; refl)
open import Agda.Builtin.Unit using (⊤)
import Agda.Builtin.Nat as N
import Agda.Builtin.String as S
import Metamodel as M
import Algebra.Foundation as AFo
import Algebra.Rings.Basic as AR
import Algebra.Fields.Basic as AFB
import Algebra.Groups.Basic as AGB
import Algebra.Groups.Abelian as AGA
import Algebra.Modules.Basic as AM
import Tests.ObligationAdapters as A
import Core.CategoricalAdapter

-- ============================================================================
-- Setup: Basic structures for module category theory and R-algebras
-- ============================================================================

-- Ring ℤ
ringId : M.Identifier
ringId = M.mkId "ℤ"

abGroupMagmaDecl : AFo.MagmaDeclaration
abGroupMagmaDecl = record
  { magmaId = M.mkId "ℤ-magma"
  ; operation = M.mkId "+"
  }

abGroupSemigroupDecl : AFo.SemigroupDeclaration
abGroupSemigroupDecl = record
  { semigroupId = M.mkId "ℤ-semigroup"
  ; underlyingMagma = abGroupMagmaDecl
  ; isAssociative = M.mkId "+-assoc"
  }

abGroupMonoidDecl : AFo.MonoidDeclaration
abGroupMonoidDecl = record
  { monoidId = M.mkId "ℤ-monoid"
  ; underlyingSemigroup = abGroupSemigroupDecl
  ; identity = M.mkId "0"
  ; hasIdentity = M.mkId "0-identity"
  }

abGroupGroupDecl : AGB.GroupDeclaration
abGroupGroupDecl = record
  { groupId = M.mkId "ℤ-group"
  ; underlyingMonoid = abGroupMonoidDecl
  ; inverse = M.mkId "neg"
  ; hasInverse = M.mkId "neg-property"
  }

abGroupDecl : AGA.AbelianGroupDeclaration
abGroupDecl = record
  { abelianGroupId = M.mkId "ℤ"
  ; underlyingGroup = abGroupGroupDecl
  ; isCommutative = M.mkId "+-comm"
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
  { commutativeRingId = M.mkId "ℤ-comm"
  ; underlyingRing = record
    { unitalRingId = M.mkId "ℤ-unital"
    ; underlyingRing = ringDecl
    ; multiplicativeIdentity = M.mkId "1"
    ; hasMultiplicativeIdentity = M.mkId "1-identity"
    }
  ; isCommutative = M.mkId "·-comm"
  }

-- Field ℚ
fieldId : M.Identifier
fieldId = M.mkId "ℚ"

fieldAbGroupDecl : AGA.AbelianGroupDeclaration
fieldAbGroupDecl = record
  { abelianGroupId = M.mkId "ℚ⁺"
  ; underlyingGroup = record
    { groupId = M.mkId "ℚ-add-group"
    ; underlyingMonoid = record
      { monoidId = M.mkId "ℚ-add-monoid"
      ; underlyingSemigroup = record
        { semigroupId = M.mkId "ℚ-add-semigroup"
        ; underlyingMagma = record
          { magmaId = M.mkId "ℚ-add-magma"
          ; operation = M.mkId "+"
          }
        ; isAssociative = M.mkId "+-assoc"
        }
      ; identity = M.mkId "0"
      ; hasIdentity = M.mkId "0-identity"
      }
    ; inverse = M.mkId "neg"
    ; hasInverse = M.mkId "neg-property"
    }
  ; isCommutative = M.mkId "+-comm"
  }

fieldRingDecl : AR.RingDeclaration
fieldRingDecl = record
  { identifier = M.mkId "ℚ-ring"
  ; additiveGroup = fieldAbGroupDecl
  ; multiplication = M.mkId "·"
  ; multAssociative = M.mkId "·-assoc"
  ; leftDistributive = M.mkId "left-dist"
  ; rightDistributive = M.mkId "right-dist"
  }

fieldDecl : AFB.FieldDeclaration
fieldDecl = record
  { fieldId = fieldId
  ; characteristic = M.mkId "0"
  ; additiveGroup = M.mkId "ℚ⁺"
  ; multiplicativeGroup = M.mkId "ℚ*"
  }

-- Modules
moduleId1 : M.Identifier
moduleId1 = M.mkId "M"

moduleAbGroup1 : AGA.AbelianGroupDeclaration
moduleAbGroup1 = record
  { abelianGroupId = M.mkId "M-group"
  ; underlyingGroup = record
    { groupId = M.mkId "M-underlying"
    ; underlyingMonoid = record
      { monoidId = M.mkId "M-monoid"
      ; underlyingSemigroup = record
        { semigroupId = M.mkId "M-semigroup"
        ; underlyingMagma = record
          { magmaId = M.mkId "M-magma"
          ; operation = M.mkId "+"
          }
        ; isAssociative = M.mkId "+-assoc"
        }
      ; identity = M.mkId "0"
      ; hasIdentity = M.mkId "0-identity"
      }
    ; inverse = M.mkId "neg"
    ; hasInverse = M.mkId "neg-property"
    }
  ; isCommutative = M.mkId "+-comm"
  }

unitalRingDecl : AR.UnitalRingDeclaration
unitalRingDecl = record
  { unitalRingId = M.mkId "ℤ-unital"
  ; underlyingRing = ringDecl
  ; multiplicativeIdentity = M.mkId "1"
  ; hasMultiplicativeIdentity = M.mkId "1-identity"
  }

module1 : AM.LeftModule ringDecl
module1 = record
  { ring = ringDecl
  ; underlyingAbelianGroup = moduleAbGroup1
  ; scalarMultiplication = M.mkId "·"
  ; compatibilityAssoc = M.mkId "r(sm)=(rs)m"
  ; compatibilityId = M.mkId "1m=m"
  ; distributivityScalar = M.mkId "r(m+n)=rm+rn"
  ; distributivityModule = M.mkId "(r+s)m=rm+sm"
  }

module2 : AM.LeftModule ringDecl
module2 = record
  { ring = ringDecl
  ; underlyingAbelianGroup = record
    { abelianGroupId = M.mkId "N"
    ; underlyingGroup = record
      { groupId = M.mkId "N-group"
      ; underlyingMonoid = record
        { monoidId = M.mkId "N-monoid"
        ; underlyingSemigroup = record
          { semigroupId = M.mkId "N-semigroup"
          ; underlyingMagma = record
            { magmaId = M.mkId "N-magma"
            ; operation = M.mkId "+"
            }
          ; isAssociative = M.mkId "+-assoc"
          }
        ; identity = M.mkId "0"
        ; hasIdentity = M.mkId "0-identity"
        }
      ; inverse = M.mkId "neg"
      ; hasInverse = M.mkId "neg-property"
      }
    ; isCommutative = M.mkId "+-comm"
    }
  ; scalarMultiplication = M.mkId "·"
  ; compatibilityAssoc = M.mkId "r(sm)=(rs)m"
  ; compatibilityId = M.mkId "1m=m"
  ; distributivityScalar = M.mkId "r(m+n)=rm+rn"
  ; distributivityModule = M.mkId "(r+s)m=rm+sm"
  }

module3 : AM.LeftModule ringDecl
module3 = record
  { ring = ringDecl
  ; underlyingAbelianGroup = record
    { abelianGroupId = M.mkId "P"
    ; underlyingGroup = record
      { groupId = M.mkId "P-group"
      ; underlyingMonoid = record
        { monoidId = M.mkId "P-monoid"
        ; underlyingSemigroup = record
          { semigroupId = M.mkId "P-semigroup"
          ; underlyingMagma = record
            { magmaId = M.mkId "P-magma"
            ; operation = M.mkId "+"
            }
          ; isAssociative = M.mkId "+-assoc"
          }
        ; identity = M.mkId "0"
        ; hasIdentity = M.mkId "0-identity"
        }
      ; inverse = M.mkId "neg"
      ; hasInverse = M.mkId "neg-property"
      }
    ; isCommutative = M.mkId "+-comm"
    }
  ; scalarMultiplication = M.mkId "·"
  ; compatibilityAssoc = M.mkId "r(sm)=(rs)m"
  ; compatibilityId = M.mkId "1m=m"
  ; distributivityScalar = M.mkId "r(m+n)=rm+rn"
  ; distributivityModule = M.mkId "(r+s)m=rm+sm"
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
  { unitalRingId = M.mkId "ℚ-unital"
  ; underlyingRing = fieldRingDecl
  ; multiplicativeIdentity = M.mkId "1"
  ; hasMultiplicativeIdentity = M.mkId "1-identity"
  }

fieldCommRing : AR.CommutativeRingDeclaration
fieldCommRing = record
  { commutativeRingId = M.mkId "ℚ-comm"
  ; underlyingRing = fieldUnitalRing
  ; isCommutative = M.mkId "·-comm"
  }

fieldAsRing : AR.RingDeclaration
fieldAsRing = AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing fieldCommRing)

vectorSpaceModule : AM.LeftModule fieldAsRing
vectorSpaceModule = record
  { ring = fieldAsRing
  ; underlyingAbelianGroup = record
    { abelianGroupId = M.mkId "V"
    ; underlyingGroup = record
      { groupId = M.mkId "V-group"
      ; underlyingMonoid = record
        { monoidId = M.mkId "V-monoid"
        ; underlyingSemigroup = record
          { semigroupId = M.mkId "V-semigroup"
          ; underlyingMagma = record
            { magmaId = M.mkId "V-magma"
            ; operation = M.mkId "+"
            }
          ; isAssociative = M.mkId "+-assoc"
          }
        ; identity = M.mkId "0"
        ; hasIdentity = M.mkId "0-identity"
        }
      ; inverse = M.mkId "neg"
      ; hasInverse = M.mkId "neg-property"
      }
    ; isCommutative = M.mkId "+-comm"
    }
  ; scalarMultiplication = M.mkId "·"
  ; compatibilityAssoc = M.mkId "r(sm)=(rs)m"
  ; compatibilityId = M.mkId "1v=v"
  ; distributivityScalar = M.mkId "r(v+w)=rv+rw"
  ; distributivityModule = M.mkId "(r+s)v=rv+sv"
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
    { abelianGroupId = M.mkId "ℤ[X]⁺"
    ; underlyingGroup = record
      { groupId = M.mkId "ℤ[X]-group"
      ; underlyingMonoid = record
        { monoidId = M.mkId "ℤ[X]-monoid"
        ; underlyingSemigroup = record
          { semigroupId = M.mkId "ℤ[X]-semigroup"
          ; underlyingMagma = record
            { magmaId = M.mkId "ℤ[X]-magma"
            ; operation = M.mkId "+"
            }
          ; isAssociative = M.mkId "+-assoc"
          }
        ; identity = M.mkId "0"
        ; hasIdentity = M.mkId "0-identity"
        }
      ; inverse = M.mkId "neg"
      ; hasInverse = M.mkId "neg-property"
      }
    ; isCommutative = M.mkId "+-comm"
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
  ; compatibilityAssoc = M.mkId "r(s·p)=(rs)·p"
  ; compatibilityId = M.mkId "1·p=p"
  ; distributivityScalar = M.mkId "r(p+q)=rp+rq"
  ; distributivityModule = M.mkId "(r+s)p=rp+sp"
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
    { abelianGroupId = M.mkId "ℤ[Y]⁺"
    ; underlyingGroup = record
      { groupId = M.mkId "ℤ[Y]-group"
      ; underlyingMonoid = record
        { monoidId = M.mkId "ℤ[Y]-monoid"
        ; underlyingSemigroup = record
          { semigroupId = M.mkId "ℤ[Y]-semigroup"
          ; underlyingMagma = record
            { magmaId = M.mkId "ℤ[Y]-magma"
            ; operation = M.mkId "+"
            }
          ; isAssociative = M.mkId "+-assoc"
          }
        ; identity = M.mkId "0"
        ; hasIdentity = M.mkId "0-identity"
        }
      ; inverse = M.mkId "neg"
      ; hasInverse = M.mkId "neg-property"
      }
    ; isCommutative = M.mkId "+-comm"
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
  ; compatibilityAssoc = M.mkId "r(s·p)=(rs)·p"
  ; compatibilityId = M.mkId "1·p=p"
  ; distributivityScalar = M.mkId "r(p+q)=rp+rq"
  ; distributivityModule = M.mkId "(r+s)p=rp+sp"
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

-- Categorical assertions
_ : Core.CategoricalAdapter.morphism (A.exactSequenceCategorical exactSequenceAdapt) ⊤ ⊤ ≡ Core.CategoricalAdapter.object (A.exactSequenceCategorical exactSequenceAdapt) ⊤
_ = refl

_ : Core.CategoricalAdapter.isomorphism (A.exactSequenceCategorical exactSequenceAdapt) = refl
_ = refl

_ : Core.CategoricalAdapter.morphism (A.categoryOfModulesCategorical categoryOfModulesAdapt) ⊤ ⊤ ≡ Core.CategoricalAdapter.object (A.categoryOfModulesCategorical categoryOfModulesAdapt) ⊤
_ = refl

_ : Core.CategoricalAdapter.isomorphism (A.categoryOfModulesCategorical categoryOfModulesAdapt) = refl
_ = refl

_ : Core.CategoricalAdapter.morphism (A.vectorSpaceCategorical vectorSpaceAdapt) ⊤ ⊤ ≡ Core.CategoricalAdapter.object (A.vectorSpaceCategorical vectorSpaceAdapt) ⊤
_ = refl

_ : Core.CategoricalAdapter.isomorphism (A.vectorSpaceCategorical vectorSpaceAdapt) = refl
_ = refl

_ : Core.CategoricalAdapter.morphism (A.rAlgebraCategorical rAlgebraAdapt) ⊤ ⊤ ≡ Core.CategoricalAdapter.object (A.rAlgebraCategorical rAlgebraAdapt) ⊤
_ = refl

_ : Core.CategoricalAdapter.isomorphism (A.rAlgebraCategorical rAlgebraAdapt) = refl
_ = refl

_ : Core.CategoricalAdapter.morphism (A.algebraHomomorphismCategorical algebraHomomorphismAdapt) ⊤ ⊤ ≡ Core.CategoricalAdapter.object (A.algebraHomomorphismCategorical algebraHomomorphismAdapt) ⊤
_ = refl

_ : Core.CategoricalAdapter.isomorphism (A.algebraHomomorphismCategorical algebraHomomorphismAdapt) = refl
_ = refl

