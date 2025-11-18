-- Tests.TensorProductChecklist
-- Minimal instances for tensor product adapters

module Tests.TensorProductChecklist where

open import Agda.Builtin.Equality
open import Agda.Builtin.Bool as B
open import Agda.Builtin.Unit using (⊤)

import Metamodel as M
import Algebra.Foundation as AF
import Algebra.Groups.Abelian as AGA
import Tests.ObligationAdapters as A
import Core.CategoricalAdapter

-- Minimal abelian groups for tensor product
abelianGroupDeclA : AF.AbelianGroupDeclaration
abelianGroupDeclA = record
  { underlyingGroup = record
    { underlyingMonoid = record
      { underlyingSemigroup = record
        { underlyingMagma = record
          { carrier = M.mkId "Z"
          ; operation = M.mkId "+"
          }
        ; associativity = M.mkId "assoc-A"
        }
      ; identityElement = M.mkId "0"
      ; leftIdentity = M.mkId "left-id-A"
      ; rightIdentity = M.mkId "right-id-A"
      }
    ; inverseOp = record
      { inverse = M.mkId "neg"
      ; leftInverse = M.mkId "left-inv-A"
      ; rightInverse = M.mkId "right-inv-A"
      }
    }
  ; commutativityAxiom = record
    { commutativity = M.mkId "comm-A"
    }
  }

abelianGroupDeclB : AF.AbelianGroupDeclaration
abelianGroupDeclB = record
  { underlyingGroup = record
    { underlyingMonoid = record
      { underlyingSemigroup = record
        { underlyingMagma = record
          { carrier = M.mkId "Z"
          ; operation = M.mkId "+"
          }
        ; associativity = M.mkId "assoc-B"
        }
      ; identityElement = M.mkId "0"
      ; leftIdentity = M.mkId "left-id-B"
      ; rightIdentity = M.mkId "right-id-B"
      }
    ; inverseOp = record
      { inverse = M.mkId "neg"
      ; leftInverse = M.mkId "left-inv-B"
      ; rightInverse = M.mkId "right-inv-B"
      }
    }
  ; commutativityAxiom = record
    { commutativity = M.mkId "comm-B"
    }
  }

-- Tensor product result (another abelian group)
tensorProductDecl : AF.AbelianGroupDeclaration
tensorProductDecl = record
  { underlyingGroup = record
    { underlyingMonoid = record
      { underlyingSemigroup = record
        { underlyingMagma = record
          { carrier = M.mkId "Z⊗Z"
          ; operation = M.mkId "+⊗"
          }
        ; associativity = M.mkId "assoc-tensor"
        }
      ; identityElement = M.mkId "0⊗"
      ; leftIdentity = M.mkId "left-id-tensor"
      ; rightIdentity = M.mkId "right-id-tensor"
      }
    ; inverseOp = record
      { inverse = M.mkId "neg⊗"
      ; leftInverse = M.mkId "left-inv-tensor"
      ; rightInverse = M.mkId "right-inv-tensor"
      }
    }
  ; commutativityAxiom = record
    { commutativity = M.mkId "comm-tensor"
    }
  }

-- Tensor product declaration
tensorProdAbDecl : AGA.TensorProductAb abelianGroupDeclA abelianGroupDeclB
tensorProdAbDecl = record
  { A = abelianGroupDeclA
  ; B = abelianGroupDeclB
  ; tensorProduct = tensorProductDecl
  ; bilinearMap = M.mkId "bilin"
  ; universalProperty = M.mkId "univ-tensor"
  }

-- Adapter instance
tensorProdAbAdapt : A.TensorProductAbAdapter
tensorProdAbAdapt = A.mkTensorProductAbAdapter abelianGroupDeclA abelianGroupDeclB tensorProdAbDecl tensorProductDecl refl

-- Status assertion
_ : A.isFilledTensorProductAb tensorProdAbAdapt ≡ B.true
_ = refl

-- Categorical assertions for TensorProductAb
_ : Core.CategoricalAdapter.CategoricalAdapter.morphism (A.tensorProductAbCategorical tensorProdAbAdapt) ⊤ ⊤ ≡
    Core.CategoricalAdapter.CategoricalAdapter.object (A.tensorProductAbCategorical tensorProdAbAdapt) ⊤
_ = refl

_ : Core.CategoricalAdapter.CategoricalAdapter.isomorphism (A.tensorProductAbCategorical tensorProdAbAdapt) ⊤ ⊤ ≡ refl
_ = refl
