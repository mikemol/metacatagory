-- Tests.ModuleTheoryChecklist
-- Minimal instances for additional module-theory adapters
-- (Hom functors, duality, tensor products, free modules)

module Tests.ModuleTheoryChecklist where

open import Agda.Builtin.Equality
open import Agda.Builtin.Bool as B
open import Agda.Builtin.Unit using (⊤)

import Metamodel as M
import Algebra.Foundation as AF
import Algebra.Rings.Basic as AR
import Algebra.Modules.Basic as AM
import Tests.ObligationAdapters as A
import Core.CategoricalAdapter

-- Minimal ring declaration
ringDecl : AR.RingDeclaration
ringDecl = record
  { additiveGroup = record
    { underlyingGroup = record
      { underlyingMonoid = record
        { underlyingSemigroup = record
          { underlyingMagma = record
            { carrier = M.mkId "R"
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
      { carrier = M.mkId "R"
      ; operation = M.mkId "*"
      }
    ; associativity = M.mkId "*-assoc"
    }
  ; leftDistributivity = M.mkId "left-dist"
  ; rightDistributivity = M.mkId "right-dist"
  }

-- Commutative ring for tensor products
commRingDecl : AR.CommutativeRingDeclaration
commRingDecl = record
  { underlyingRing = ringDecl
  ; commutativity = M.mkId "*-comm"
  }

-- Unital ring (for modules)
unitalRingDecl : AR.UnitalRingDeclaration
unitalRingDecl = record
  { underlyingRing = ringDecl
  ; multiplicativeIdentity = M.mkId "1"
  ; leftUnital = M.mkId "*-left-id"
  ; rightUnital = M.mkId "*-right-id"
  }

-- Module M
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

-- Module N (for tensor products)
leftModuleNDecl : AM.LeftModule ringDecl
leftModuleNDecl = record
  { underlyingAbelianGroup = record
    { underlyingGroup = record
      { underlyingMonoid = record
        { underlyingSemigroup = record
          { underlyingMagma = record
            { carrier = M.mkId "N"
            ; operation = M.mkId "+N"
            }
          ; associativity = M.mkId "N-assoc"
          }
        ; identityElement = M.mkId "0N"
        ; leftIdentity = M.mkId "N-left-id"
        ; rightIdentity = M.mkId "N-right-id"
        }
      ; inverseOp = record
        { inverse = M.mkId "negN"
        ; leftInverse = M.mkId "N-left-inv"
        ; rightInverse = M.mkId "N-right-inv"
        }
      }
    ; commutativityAxiom = record
      { commutativity = M.mkId "N-comm"
      }
    }
  ; scalarMultiplication = M.mkId "·N"
  ; scalarIdentity = M.mkId "scalar-id-N"
  ; scalarAssociativity = M.mkId "scalar-assoc-N"
  ; scalarDistributivityOverVectorAddition = M.mkId "scalar-dist-N-vec"
  ; scalarDistributivityOverScalarAddition = M.mkId "scalar-dist-N-scalar"
  }

-- Module over commutative ring
leftModuleCommDecl : AM.LeftModule (AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing commRingDecl))
leftModuleCommDecl = record
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

leftModuleNCommDecl : AM.LeftModule (AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing commRingDecl))
leftModuleNCommDecl = record
  { underlyingAbelianGroup = record
    { underlyingGroup = record
      { underlyingMonoid = record
        { underlyingSemigroup = record
          { underlyingMagma = record
            { carrier = M.mkId "N"
            ; operation = M.mkId "+N"
            }
          ; associativity = M.mkId "N-assoc"
          }
        ; identityElement = M.mkId "0N"
        ; leftIdentity = M.mkId "N-left-id"
        ; rightIdentity = M.mkId "N-right-id"
        }
      ; inverseOp = record
        { inverse = M.mkId "negN"
        ; leftInverse = M.mkId "N-left-inv"
        ; rightInverse = M.mkId "N-right-inv"
        }
      }
    ; commutativityAxiom = record
      { commutativity = M.mkId "N-comm"
      }
    }
  ; scalarMultiplication = M.mkId "·N"
  ; scalarIdentity = M.mkId "scalar-id-N"
  ; scalarAssociativity = M.mkId "scalar-assoc-N"
  ; scalarDistributivityOverVectorAddition = M.mkId "scalar-dist-N-vec"
  ; scalarDistributivityOverScalarAddition = M.mkId "scalar-dist-N-scalar"
  }

-- Tensor product result
tensorProdModuleDecl : AM.LeftModule (AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing commRingDecl))
tensorProdModuleDecl = record
  { underlyingAbelianGroup = record
    { underlyingGroup = record
      { underlyingMonoid = record
        { underlyingSemigroup = record
          { underlyingMagma = record
            { carrier = M.mkId "M⊗N"
            ; operation = M.mkId "+⊗"
            }
          ; associativity = M.mkId "⊗-assoc"
          }
        ; identityElement = M.mkId "0⊗"
        ; leftIdentity = M.mkId "⊗-left-id"
        ; rightIdentity = M.mkId "⊗-right-id"
        }
      ; inverseOp = record
        { inverse = M.mkId "neg⊗"
        ; leftInverse = M.mkId "⊗-left-inv"
        ; rightInverse = M.mkId "⊗-right-inv"
        }
      }
    ; commutativityAxiom = record
      { commutativity = M.mkId "⊗-comm"
      }
    }
  ; scalarMultiplication = M.mkId "·⊗"
  ; scalarIdentity = M.mkId "scalar-id-⊗"
  ; scalarAssociativity = M.mkId "scalar-assoc-⊗"
  ; scalarDistributivityOverVectorAddition = M.mkId "scalar-dist-⊗-vec"
  ; scalarDistributivityOverScalarAddition = M.mkId "scalar-dist-⊗-scalar"
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
  ; bilinearMap = M.mkId "bilin"
  ; universalProperty = M.mkId "tensor-univ"
  }

-- Free module F(X) over R
freeModuleDecl : AM.FreeModule ringDecl (M.mkId "X")
freeModuleDecl = record
  { ring = ringDecl
  ; basisSet = M.mkId "X"
  ; freeModule = leftModuleDecl
  ; universalProperty = M.mkId "free-univ"
  }

-- Free module functor
freeModuleFunctorDecl : AM.FreeModuleFunctor ringDecl
freeModuleFunctorDecl = record
  { ring = ringDecl
  ; freeFunctor = M.mkId "Free"
  ; universalMapping = M.mkId "free-map"
  }

-- Forgetful module functor
forgetfulModuleFunctorDecl : AM.ForgetfulModuleFunctor ringDecl
forgetfulModuleFunctorDecl = record
  { ring = ringDecl
  ; forgetfulFunctor = M.mkId "Forget"
  ; forgetStructure = M.mkId "forget-scalar"
  }

-- Right module
rightModuleDecl : AM.RightModule ringDecl
rightModuleDecl = record
  { ring = ringDecl
  ; underlyingAbelianGroup = record
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
  ; scalarMultiplication = M.mkId "·ʳ"
  ; scalarIdentity = M.mkId "scalar-id-r"
  ; scalarAssociativity = M.mkId "scalar-assoc-r"
  ; scalarDistributivityOverVectorAddition = M.mkId "scalar-dist-M-r"
  ; scalarDistributivityOverScalarAddition = M.mkId "scalar-dist-R-r"
  }

-- Adapter instances
homFunctorAdapt : A.HomFunctorAdapter
homFunctorAdapt = A.mkHomFunctorAdapter ringDecl leftModuleDecl homFunctorDecl ringDecl refl

dualModuleAdapt : A.DualModuleAdapter
dualModuleAdapt = A.mkDualModuleAdapter ringDecl leftModuleDecl dualModuleDecl ringDecl refl

reflexiveModuleAdapt : A.ReflexiveModuleAdapter
reflexiveModuleAdapt = A.mkReflexiveModuleAdapter ringDecl leftModuleDecl reflexiveModuleDecl ringDecl refl

tensorProductModuleAdapt : A.TensorProductModuleAdapter
tensorProductModuleAdapt = A.mkTensorProductModuleAdapter commRingDecl leftModuleCommDecl leftModuleNCommDecl tensorProductDecl tensorProdModuleDecl refl

freeModuleAdapt : A.FreeModuleAdapter
freeModuleAdapt = A.mkFreeModuleAdapter ringDecl (M.mkId "X") freeModuleDecl ringDecl refl

freeModuleFunctorAdapt : A.FreeModuleFunctorAdapter
freeModuleFunctorAdapt = A.mkFreeModuleFunctorAdapter ringDecl freeModuleFunctorDecl ringDecl refl

forgetfulModuleFunctorAdapt : A.ForgetfulModuleFunctorAdapter
forgetfulModuleFunctorAdapt = A.mkForgetfulModuleFunctorAdapter ringDecl forgetfulModuleFunctorDecl ringDecl refl

rightModuleAdapt : A.RightModuleAdapter
rightModuleAdapt = A.mkRightModuleAdapter ringDecl rightModuleDecl ringDecl refl

-- Status assertions
_ : A.isFilledHomFunctor homFunctorAdapt ≡ B.true
_ = refl

_ : A.isFilledDualModule dualModuleAdapt ≡ B.true
_ = refl

_ : A.isFilledReflexiveModule reflexiveModuleAdapt ≡ B.true
_ = refl

_ : A.isFilledTensorProductModule tensorProductModuleAdapt ≡ B.true
_ = refl

_ : A.isFilledFreeModule freeModuleAdapt ≡ B.true
_ = refl

_ : A.isFilledFreeModuleFunctor freeModuleFunctorAdapt ≡ B.true
_ = refl

_ : A.isFilledForgetfulModuleFunctor forgetfulModuleFunctorAdapt ≡ B.true
_ = refl

_ : A.isFilledRightModule rightModuleAdapt ≡ B.true
_ = refl

-- Categorical assertions
_ : Core.CategoricalAdapter.morphism (A.homFunctorCategorical homFunctorAdapt) ⊤ ⊤ ≡ Core.CategoricalAdapter.object (A.homFunctorCategorical homFunctorAdapt) ⊤
_ = refl

_ : Core.CategoricalAdapter.isomorphism (A.homFunctorCategorical homFunctorAdapt) = refl
_ = refl

_ : Core.CategoricalAdapter.morphism (A.dualModuleCategorical dualModuleAdapt) ⊤ ⊤ ≡ Core.CategoricalAdapter.object (A.dualModuleCategorical dualModuleAdapt) ⊤
_ = refl

_ : Core.CategoricalAdapter.isomorphism (A.dualModuleCategorical dualModuleAdapt) = refl
_ = refl

_ : Core.CategoricalAdapter.morphism (A.reflexiveModuleCategorical reflexiveModuleAdapt) ⊤ ⊤ ≡ Core.CategoricalAdapter.object (A.reflexiveModuleCategorical reflexiveModuleAdapt) ⊤
_ = refl

_ : Core.CategoricalAdapter.isomorphism (A.reflexiveModuleCategorical reflexiveModuleAdapt) = refl
_ = refl

_ : Core.CategoricalAdapter.morphism (A.tensorProductModuleCategorical tensorProductModuleAdapt) ⊤ ⊤ ≡ Core.CategoricalAdapter.object (A.tensorProductModuleCategorical tensorProductModuleAdapt) ⊤
_ = refl

_ : Core.CategoricalAdapter.isomorphism (A.tensorProductModuleCategorical tensorProductModuleAdapt) = refl
_ = refl

_ : Core.CategoricalAdapter.morphism (A.freeModuleCategorical freeModuleAdapt) ⊤ ⊤ ≡ Core.CategoricalAdapter.object (A.freeModuleCategorical freeModuleAdapt) ⊤
_ = refl

_ : Core.CategoricalAdapter.isomorphism (A.freeModuleCategorical freeModuleAdapt) = refl
_ = refl

_ : Core.CategoricalAdapter.morphism (A.freeModuleFunctorCategorical freeModuleFunctorAdapt) ⊤ ⊤ ≡ Core.CategoricalAdapter.object (A.freeModuleFunctorCategorical freeModuleFunctorAdapt) ⊤
_ = refl

_ : Core.CategoricalAdapter.isomorphism (A.freeModuleFunctorCategorical freeModuleFunctorAdapt) = refl
_ = refl

_ : Core.CategoricalAdapter.morphism (A.forgetfulModuleFunctorCategorical forgetfulModuleFunctorAdapt) ⊤ ⊤ ≡ Core.CategoricalAdapter.object (A.forgetfulModuleFunctorCategorical forgetfulModuleFunctorAdapt) ⊤
_ = refl

_ : Core.CategoricalAdapter.isomorphism (A.forgetfulModuleFunctorCategorical forgetfulModuleFunctorAdapt) = refl
_ = refl

_ : Core.CategoricalAdapter.morphism (A.rightModuleCategorical rightModuleAdapt) ⊤ ⊤ ≡ Core.CategoricalAdapter.object (A.rightModuleCategorical rightModuleAdapt) ⊤
_ = refl

_ : Core.CategoricalAdapter.isomorphism (A.rightModuleCategorical rightModuleAdapt) = refl
_ = refl
