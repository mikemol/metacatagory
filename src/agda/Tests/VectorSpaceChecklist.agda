-- Tests.VectorSpaceChecklist
-- Minimal instances for vector space structure adapters

module Tests.VectorSpaceChecklist where

open import Agda.Builtin.Equality
open import Agda.Builtin.Bool as B

import Metamodel as M
import Algebra.Foundation as AF
import Algebra.Rings.Basic as AR
import Algebra.Modules.Basic as AM
import Tests.ObligationAdapters as A

-- Minimal field (reusing ring structure)
fieldDecl : AR.FieldDeclaration
fieldDecl = record
  { underlyingDivisionRing = record
    { underlyingUnitalRing = record
      { underlyingRing = record
        { additiveGroup = record
          { underlyingGroup = record
            { underlyingMonoid = record
              { underlyingSemigroup = record
                { underlyingMagma = record
                  { carrier = M.mkId "Q"
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
            { carrier = M.mkId "Q"
            ; operation = M.mkId "*"
            }
          ; associativity = M.mkId "*-assoc"
          }
        ; leftDistributivity = M.mkId "left-dist"
        ; rightDistributivity = M.mkId "right-dist"
        }
      ; multiplicativeIdentity = M.mkId "1"
      ; leftUnital = M.mkId "*-left-id"
      ; rightUnital = M.mkId "*-right-id"
      }
    ; multiplicativeInverse = M.mkId "inv"
    ; inverseProperty = M.mkId "mult-inv"
    }
  ; commutativity = M.mkId "*-comm"
  }

-- Vector space
vectorSpaceDecl : AM.VectorSpace fieldDecl
vectorSpaceDecl = record
  { underlyingModule = record
    { underlyingAbelianGroup = record
      { underlyingGroup = record
        { underlyingMonoid = record
          { underlyingSemigroup = record
            { underlyingMagma = record
              { carrier = M.mkId "V"
              ; operation = M.mkId "+V"
              }
            ; associativity = M.mkId "V-assoc"
            }
          ; identityElement = M.mkId "0V"
          ; leftIdentity = M.mkId "V-left-id"
          ; rightIdentity = M.mkId "V-right-id"
          }
        ; inverseOp = record
          { inverse = M.mkId "negV"
          ; leftInverse = M.mkId "V-left-inv"
          ; rightInverse = M.mkId "V-right-inv"
          }
        }
      ; commutativityAxiom = record
        { commutativity = M.mkId "V-comm"
        }
      }
    ; scalarMultiplication = M.mkId "·"
    ; scalarIdentity = M.mkId "scalar-id"
    ; scalarAssociativity = M.mkId "scalar-assoc"
    ; scalarDistributivityOverVectorAddition = M.mkId "scalar-dist-V"
    ; scalarDistributivityOverScalarAddition = M.mkId "scalar-dist-F"
    }
  }

-- Basis of vector space
basisDecl : AM.BasisOfVectorSpace fieldDecl vectorSpaceDecl
basisDecl = record
  { basisSet = M.mkId "{e1,e2,e3}"
  ; linearlyIndependent = M.mkId "lin-indep"
  ; spansSpace = M.mkId "spans-V"
  }

-- Dimension
dimensionDecl : AM.Dimension fieldDecl vectorSpaceDecl
dimensionDecl = record
  { dimensionValue = M.mkId "3"
  ; wellDefined = M.mkId "dim-well-def"
  }

-- Adapter instances
basisAdapt : A.BasisOfVectorSpaceAdapter
basisAdapt = A.mkBasisOfVectorSpaceAdapter fieldDecl vectorSpaceDecl basisDecl (M.mkId "{e1,e2,e3}") refl

dimensionAdapt : A.DimensionAdapter
dimensionAdapt = A.mkDimensionAdapter fieldDecl vectorSpaceDecl dimensionDecl (M.mkId "3") refl

-- Status assertions
_ : A.isFilledBasisOfVectorSpace basisAdapt ≡ B.true
_ = refl

_ : A.isFilledDimension dimensionAdapt ≡ B.true
_ = refl
