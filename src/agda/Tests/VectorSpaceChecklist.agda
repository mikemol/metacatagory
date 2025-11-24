-- Tests.VectorSpaceChecklist
-- Minimal instances for vector space structure adapters

module Tests.VectorSpaceChecklist where

open import Agda.Builtin.Equality
open import Core.Phase using (Bool; true; false)
open import Agda.Builtin.Unit using (⊤; tt)
open import Core.CategoricalAdapter

import Metamodel as M
import Algebra.Foundation as AF
import Algebra.Rings.Basic as AR
import Algebra.Modules.Basic as AM
import Tests.ObligationAdapters as A
import Chapter1.Level1 as C1L

-- Minimal field (reusing ring structure)
fieldDecl : AR.FieldDeclaration
fieldDecl =
  let
    plusSemigroup : AF.SemigroupDeclaration
    plusSemigroup = record
      { underlyingMagma = record { underlyingSet = M.mkId "Q" ; binaryOp = M.mkId "+" ; index = AF.magmaIndex }
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
      { identifier = M.mkId "Q"
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

    commRing : AR.CommutativeRingDeclaration
    commRing = record
      { underlyingRing = unitalRing
      ; commutativity = M.mkId "*-comm"
      }
  in
  record
    { underlyingRing = commRing
    ; inverses = M.mkId "mult-inv"
    }

-- Vector space
vectorSpaceDecl : AM.VectorSpace fieldDecl
vectorSpaceDecl =
  let
    vPlusSemigroup : AF.SemigroupDeclaration
    vPlusSemigroup = record
      { underlyingMagma = record { underlyingSet = M.mkId "V" ; binaryOp = M.mkId "+V" ; index = AF.magmaIndex }
      ; associativity = C1L.AXIOM_Associativity (M.mkId "V-assoc")
      ; index = AF.semigroupIndex
      }

    vPlusMonoid : AF.MonoidDeclaration
    vPlusMonoid = record
      { underlyingSemigroup = vPlusSemigroup
      ; identityElement = M.mkId "0V"
      ; identityAxiom = C1L.AXIOM_Identity (M.mkId "V-id")
      ; index = AF.monoidIndex
      }

    vPlusGroup : AF.GroupDeclaration
    vPlusGroup = record
      { underlyingMonoid = vPlusMonoid
      ; inverseOperation =
          record
            { forMonoid = vPlusMonoid
            ; inverseMap = M.mkId "negV"
            ; inverseAxiom = M.mkId "V-left-inv"
            }
      ; index = AF.groupIndex
      }

    vAbelian : AF.AbelianGroupDeclaration
    vAbelian = record
      { underlyingGroup = vPlusGroup
      ; commutativity = record { forGroup = vPlusGroup ; axiom = M.mkId "V-comm" }
      ; index = AF.abelianGroupIndex
      }

    baseRing : AR.RingDeclaration
    baseRing = AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing (AR.FieldDeclaration.underlyingRing fieldDecl))
  in
  record
    { field' = fieldDecl
    ; underlyingModule =
        record
          { ring = baseRing
          ; underlyingAbelianGroup = vAbelian
          ; scalarMultiplication = M.mkId "·"
          ; distributiveOverAddition = M.mkId "scalar-dist-V"
          ; distributiveOverRingAddition = M.mkId "scalar-dist-F"
          ; associativeScalar = M.mkId "scalar-assoc"
          ; unitalAction = M.mkId "scalar-id"
          }
    }

-- Basis of vector space
basisDecl : AM.BasisOfVectorSpace fieldDecl vectorSpaceDecl
basisDecl = record
  { field' = fieldDecl
  ; vectorSpace = vectorSpaceDecl
  ; basisSet = M.mkId "{e1,e2,e3}"
  ; linearIndependence = M.mkId "lin-indep"
  ; spanning = M.mkId "spans-V"
  }

-- Dimension
dimensionDecl : AM.Dimension fieldDecl vectorSpaceDecl
dimensionDecl = record
  { field' = fieldDecl
  ; vectorSpace = vectorSpaceDecl
  ; dimension = M.mkId "3"
  }

-- Adapter instances
basisAdapt : A.BasisOfVectorSpaceAdapter
basisAdapt = A.mkBasisOfVectorSpaceAdapter fieldDecl vectorSpaceDecl basisDecl (M.mkId "{e1,e2,e3}") refl

dimensionAdapt : A.DimensionAdapter
dimensionAdapt = A.mkDimensionAdapter fieldDecl vectorSpaceDecl dimensionDecl (M.mkId "3") refl

-- Status assertions
_ : A.isFilledBasisOfVectorSpace basisAdapt ≡ true
_ = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.basisOfVectorSpaceCategorical basisAdapt) tt) ≡ A.BasisOfVectorSpaceAdapter.decl basisAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.basisOfVectorSpaceCategorical basisAdapt) ≡ refl
_ = refl

_ : A.isFilledDimension dimensionAdapt ≡ true
_ = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.dimensionCategorical dimensionAdapt) tt) ≡ A.DimensionAdapter.decl dimensionAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.dimensionCategorical dimensionAdapt) ≡ refl
_ = refl
