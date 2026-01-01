{-# OPTIONS --without-K #-}

-- | Checklist for module-theory adapters (hom, duals, tensors, free).
module Tests.ModuleTheoryChecklist where

open import Agda.Builtin.Equality
open import Core.Phase using (Bool; true; false)
open import Agda.Builtin.Unit using (⊤)

import Metamodel as M
import Algebra.Foundation as AF
import Algebra.Rings.Basic as AR
import Algebra.Modules.Basic as AM
import Tests.ObligationAdapters as A
import Core.CategoricalAdapter
import Chapter1.Level1 as C1L

-- Minimal ring declaration (modern API)
ringDecl : AR.RingDeclaration
ringDecl =
  let
    plusSemigroup : AF.SemigroupDeclaration
    plusSemigroup = record
      { underlyingMagma = record { underlyingSet = M.mkId "R" ; binaryOp = M.mkId "+" ; index = AF.magmaIndex }
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
            ; inverseAxiom = M.mkId "+-inv"
            }
      ; index = AF.groupIndex
      }

    addAbelian : AF.AbelianGroupDeclaration
    addAbelian = record
      { underlyingGroup = plusGroup
      ; commutativity = record { forGroup = plusGroup ; axiom = M.mkId "+-comm" }
      ; index = AF.abelianGroupIndex
      }
  in
  record
    { identifier = M.mkId "R"
    ; additiveGroup = addAbelian
    ; multiplication = M.mkId "*"
    ; multAssociative = M.mkId "*-assoc"
    ; leftDistributive = M.mkId "left-dist"
    ; rightDistributive = M.mkId "right-dist"
    }

-- Commutative ring for tensor products (modern API)
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
  record { underlyingRing = unitalRing ; commutativity = M.mkId "*-comm" }

-- Module M (modern API)
leftModuleDecl : AM.LeftModule ringDecl
leftModuleDecl =
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

-- Module N (for tensor products, modern API)
leftModuleNDecl : AM.LeftModule ringDecl
leftModuleNDecl =
  let
    nSemigroup : AF.SemigroupDeclaration
    nSemigroup = record
      { underlyingMagma = record { underlyingSet = M.mkId "N" ; binaryOp = M.mkId "+N" ; index = AF.magmaIndex }
      ; associativity = C1L.AXIOM_Associativity (M.mkId "N-assoc")
      ; index = AF.semigroupIndex
      }

    nMonoid : AF.MonoidDeclaration
    nMonoid = record
      { underlyingSemigroup = nSemigroup
      ; identityElement = M.mkId "0N"
      ; identityAxiom = C1L.AXIOM_Identity (M.mkId "N-id")
      ; index = AF.monoidIndex
      }

    nGroup : AF.GroupDeclaration
    nGroup = record
      { underlyingMonoid = nMonoid
      ; inverseOperation =
          record
            { forMonoid = nMonoid
            ; inverseMap = M.mkId "negN"
            ; inverseAxiom = M.mkId "N-inv"
            }
      ; index = AF.groupIndex
      }

    nAbelian : AF.AbelianGroupDeclaration
    nAbelian = record
      { underlyingGroup = nGroup
      ; commutativity = record { forGroup = nGroup ; axiom = M.mkId "N-comm" }
      ; index = AF.abelianGroupIndex
      }
  in
  record
    { ring = ringDecl
    ; underlyingAbelianGroup = nAbelian
    ; scalarMultiplication = M.mkId "·N"
    ; distributiveOverAddition = M.mkId "scalar-dist-N-vec"
    ; distributiveOverRingAddition = M.mkId "scalar-dist-N-scalar"
    ; associativeScalar = M.mkId "scalar-assoc-N"
    ; unitalAction = M.mkId "scalar-id-N"
    }

-- Module over commutative ring (modern API)
leftModuleCommDecl : AM.LeftModule (AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing commRingDecl))
leftModuleCommDecl =
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
    baseRing = AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing commRingDecl)
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

leftModuleNCommDecl : AM.LeftModule (AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing commRingDecl))
leftModuleNCommDecl =
  let
    nSemigroup : AF.SemigroupDeclaration
    nSemigroup = record
      { underlyingMagma = record { underlyingSet = M.mkId "N" ; binaryOp = M.mkId "+N" ; index = AF.magmaIndex }
      ; associativity = C1L.AXIOM_Associativity (M.mkId "N-assoc")
      ; index = AF.semigroupIndex
      }

    nMonoid : AF.MonoidDeclaration
    nMonoid = record
      { underlyingSemigroup = nSemigroup
      ; identityElement = M.mkId "0N"
      ; identityAxiom = C1L.AXIOM_Identity (M.mkId "N-id")
      ; index = AF.monoidIndex
      }

    nGroup : AF.GroupDeclaration
    nGroup = record
      { underlyingMonoid = nMonoid
      ; inverseOperation =
          record
            { forMonoid = nMonoid
            ; inverseMap = M.mkId "negN"
            ; inverseAxiom = M.mkId "N-inv"
            }
      ; index = AF.groupIndex
      }

    nAbelian : AF.AbelianGroupDeclaration
    nAbelian = record
      { underlyingGroup = nGroup
      ; commutativity = record { forGroup = nGroup ; axiom = M.mkId "N-comm" }
      ; index = AF.abelianGroupIndex
      }

    baseRing : AR.RingDeclaration
    baseRing = AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing commRingDecl)
  in
  record
    { ring = baseRing
    ; underlyingAbelianGroup = nAbelian
    ; scalarMultiplication = M.mkId "·N"
    ; distributiveOverAddition = M.mkId "scalar-dist-N-vec"
    ; distributiveOverRingAddition = M.mkId "scalar-dist-N-scalar"
    ; associativeScalar = M.mkId "scalar-assoc-N"
    ; unitalAction = M.mkId "scalar-id-N"
    }

-- Tensor product result (modern API)
tensorProdModuleDecl : AM.LeftModule (AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing commRingDecl))
tensorProdModuleDecl =
  let
    tSemigroup : AF.SemigroupDeclaration
    tSemigroup = record
      { underlyingMagma = record { underlyingSet = M.mkId "M⊗N" ; binaryOp = M.mkId "+⊗" ; index = AF.magmaIndex }
      ; associativity = C1L.AXIOM_Associativity (M.mkId "⊗-assoc")
      ; index = AF.semigroupIndex
      }

    tMonoid : AF.MonoidDeclaration
    tMonoid = record
      { underlyingSemigroup = tSemigroup
      ; identityElement = M.mkId "0⊗"
      ; identityAxiom = C1L.AXIOM_Identity (M.mkId "⊗-id")
      ; index = AF.monoidIndex
      }

    tGroup : AF.GroupDeclaration
    tGroup = record
      { underlyingMonoid = tMonoid
      ; inverseOperation =
          record
            { forMonoid = tMonoid
            ; inverseMap = M.mkId "neg⊗"
            ; inverseAxiom = M.mkId "⊗-inv"
            }
      ; index = AF.groupIndex
      }

    tAbelian : AF.AbelianGroupDeclaration
    tAbelian = record
      { underlyingGroup = tGroup
      ; commutativity = record { forGroup = tGroup ; axiom = M.mkId "⊗-comm" }
      ; index = AF.abelianGroupIndex
      }

    baseRing : AR.RingDeclaration
    baseRing = AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing commRingDecl)
  in
  record
    { ring = baseRing
    ; underlyingAbelianGroup = tAbelian
    ; scalarMultiplication = M.mkId "·⊗"
    ; distributiveOverAddition = M.mkId "scalar-dist-⊗-vec"
    ; distributiveOverRingAddition = M.mkId "scalar-dist-⊗-scalar"
    ; associativeScalar = M.mkId "scalar-assoc-⊗"
    ; unitalAction = M.mkId "scalar-id-⊗"
    }

-- Hom functor
homFunctorDecl : AM.HomFunctor ringDecl leftModuleDecl
homFunctorDecl = record
  { ring = ringDecl
  ; fixedModule = leftModuleDecl
  ; homAsAbelianGroup = M.mkId "Hom(M,-)"
  ; homAsModule = M.mkId "Hom-mod"
  ; functoriality = M.mkId "hom-funct"
  }

-- Dual module M* = Hom(M,R)
dualModuleDecl : AM.DualModule ringDecl leftModuleDecl
dualModuleDecl = record
  { ring = ringDecl
  ; module' = leftModuleDecl
  ; dualModule = leftModuleDecl  -- simplified: M* has same structure
  }

-- Reflexive module M ≅ M**
reflexiveModuleDecl : AM.ReflexiveModule ringDecl leftModuleDecl
reflexiveModuleDecl = record
  { ring = ringDecl
  ; module' = leftModuleDecl
  ; isReflexive = M.mkId "M≅M**"
  }

-- Tensor product of modules
tensorProductDecl : AM.TensorProduct commRingDecl leftModuleCommDecl leftModuleNCommDecl
tensorProductDecl = record
  { ring = commRingDecl
  ; leftModule = leftModuleCommDecl
  ; rightModule = leftModuleNCommDecl
  ; tensorProduct = tensorProdModuleDecl
  ; universalBilinearMap = M.mkId "bilin"
  ; universalProperty = M.mkId "tensor-univ"
  }

-- Free module F(X) over R
freeModuleDecl : AM.FreeModule ringDecl (M.mkId "X")
freeModuleDecl = record
  { ring = ringDecl
  ; basis = M.mkId "X"
  ; freeModule = leftModuleDecl
  ; universalProperty = M.mkId "free-univ"
  }

-- Free module functor
freeModuleFunctorDecl : AM.FreeModuleFunctor ringDecl
freeModuleFunctorDecl = record
  { ring = ringDecl
  ; onObjects = M.mkId "Free-obj"
  ; onMorphisms = M.mkId "Free-morph"
  ; preservesIdentity = M.mkId "Free-id"
  ; preservesComposition = M.mkId "Free-comp"
  }

-- Forgetful module functor
forgetfulModuleFunctorDecl : AM.ForgetfulModuleFunctor ringDecl
forgetfulModuleFunctorDecl = record
  { ring = ringDecl
  ; onObjects = M.mkId "Forget-obj"
  ; onMorphisms = M.mkId "Forget-morph"
  ; preservesIdentity = M.mkId "Forget-id"
  ; preservesComposition = M.mkId "Forget-comp"
  }

-- Right module (modern API)
rightModuleDecl : AM.RightModule ringDecl
rightModuleDecl =
  let
    rSemigroup : AF.SemigroupDeclaration
    rSemigroup = record
      { underlyingMagma = record { underlyingSet = M.mkId "M" ; binaryOp = M.mkId "+M" ; index = AF.magmaIndex }
      ; associativity = C1L.AXIOM_Associativity (M.mkId "M-assoc")
      ; index = AF.semigroupIndex
      }

    rMonoid : AF.MonoidDeclaration
    rMonoid = record
      { underlyingSemigroup = rSemigroup
      ; identityElement = M.mkId "0M"
      ; identityAxiom = C1L.AXIOM_Identity (M.mkId "M-id")
      ; index = AF.monoidIndex
      }

    rGroup : AF.GroupDeclaration
    rGroup = record
      { underlyingMonoid = rMonoid
      ; inverseOperation =
          record
            { forMonoid = rMonoid
            ; inverseMap = M.mkId "negM"
            ; inverseAxiom = M.mkId "M-inv"
            }
      ; index = AF.groupIndex
      }

    rAbelian : AF.AbelianGroupDeclaration
    rAbelian = record
      { underlyingGroup = rGroup
      ; commutativity = record { forGroup = rGroup ; axiom = M.mkId "M-comm" }
      ; index = AF.abelianGroupIndex
      }
  in
  record
    { ring = ringDecl
    ; underlyingAbelianGroup = rAbelian
    ; scalarMultiplication = M.mkId "·ʳ"
    ; distributiveOverAddition = M.mkId "scalar-dist-M-r"
    ; distributiveOverRingAddition = M.mkId "scalar-dist-R-r"
    ; associativeScalar = M.mkId "scalar-assoc-r"
    ; unitalAction = M.mkId "scalar-id-r"
    }

-- Adapter instances
homFunctorAdapt : A.HomFunctorAdapter
homFunctorAdapt = A.mkHomFunctorAdapter ringDecl leftModuleDecl homFunctorDecl ringDecl refl

dualModuleAdapt : A.DualModuleAdapter
dualModuleAdapt = A.mkDualModuleAdapter ringDecl leftModuleDecl dualModuleDecl ringDecl refl

reflexiveModuleAdapt : A.ReflexiveModuleAdapter
reflexiveModuleAdapt = A.mkReflexiveModuleAdapter ringDecl leftModuleDecl reflexiveModuleDecl ringDecl refl

tensorProductModuleAdapt : A.TensorProductModuleAdapter
tensorProductModuleAdapt = A.mkTensorProductModuleAdapter commRingDecl leftModuleCommDecl leftModuleNCommDecl tensorProductDecl

freeModuleAdapt : A.FreeModuleAdapter
freeModuleAdapt = A.mkFreeModuleAdapter ringDecl (M.mkId "X") freeModuleDecl ringDecl refl

freeModuleFunctorAdapt : A.FreeModuleFunctorAdapter
freeModuleFunctorAdapt = A.mkFreeModuleFunctorAdapter ringDecl freeModuleFunctorDecl ringDecl refl

forgetfulModuleFunctorAdapt : A.ForgetfulModuleFunctorAdapter
forgetfulModuleFunctorAdapt = A.mkForgetfulModuleFunctorAdapter ringDecl forgetfulModuleFunctorDecl ringDecl refl

rightModuleAdapt : A.RightModuleAdapter
rightModuleAdapt = A.mkRightModuleAdapter ringDecl rightModuleDecl ringDecl refl

-- Status assertions
_ : A.isFilledHomFunctor homFunctorAdapt ≡ true
_ = refl

_ : A.isFilledDualModule dualModuleAdapt ≡ true
_ = refl

_ : A.isFilledReflexiveModule reflexiveModuleAdapt ≡ true
_ = refl

_ : A.isFilledTensorProductModule tensorProductModuleAdapt ≡ true
_ = refl

_ : A.isFilledFreeModule freeModuleAdapt ≡ true
_ = refl

_ : A.isFilledFreeModuleFunctor freeModuleFunctorAdapt ≡ true
_ = refl

_ : A.isFilledForgetfulModuleFunctor forgetfulModuleFunctorAdapt ≡ true
_ = refl

_ : A.isFilledRightModule rightModuleAdapt ≡ true
_ = refl

-- Categorical assertions (omitted; adapter wiring smoke-tested via isFilledX)
