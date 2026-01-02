{-# OPTIONS --without-K #-}

-- Tests.TensorProductChecklist
-- Minimal instances for tensor product adapters

module Tests.TensorProductChecklist where

open import Agda.Builtin.Equality
open import Core.Phase using (Bool; true; false)
open import Agda.Builtin.Unit using (⊤; tt)

import Metamodel as M
import Algebra.Foundation as AF
import Chapter1.Level1 as C1L
import Algebra.Groups.Abelian as AGA
import Tests.ObligationAdapters as A
import Core.CategoricalAdapter

-- Minimal abelian groups for tensor product
-- | Minimal abelian group declaration for factor A.
abelianGroupDeclA : AF.AbelianGroupDeclaration
abelianGroupDeclA = record
      { underlyingGroup =
          record
            { underlyingMonoid =
                record
                  { underlyingSemigroup =
                      record
                        { underlyingMagma =
                            -- | Magma for abelian group A.
                            record { underlyingSet = M.mkId "Z" ; binaryOp = M.mkId "+" ; index = AF.magmaIndex }
                        ; associativity = C1L.AXIOM_Associativity (M.mkId "assoc-A")
                        ; index = AF.semigroupIndex
                        }
              ; identityElement = M.mkId "0"
              ; identityAxiom = C1L.AXIOM_Identity (M.mkId "id-A")
              ; index = AF.monoidIndex
              }
        ; inverseOperation =
            record
              { forMonoid =
                  record
                    { underlyingSemigroup =
                        record
                          { underlyingMagma =
                              -- | Magma for inverse operation of A.
                              record { underlyingSet = M.mkId "Z" ; binaryOp = M.mkId "+" ; index = AF.magmaIndex }
                          ; associativity = C1L.AXIOM_Associativity (M.mkId "assoc-A")
                          ; index = AF.semigroupIndex
                          }
                    ; identityElement = M.mkId "0"
                    ; identityAxiom = C1L.AXIOM_Identity (M.mkId "id-A")
                    ; index = AF.monoidIndex
                    }
              ; inverseMap = M.mkId "neg"
              ; inverseAxiom = M.mkId "left-inv-A"
              }
        ; index = AF.groupIndex
        }
  ; commutativity =
      record
        { forGroup =
            record
            { underlyingMonoid =
                record
                  { underlyingSemigroup =
                      record
                        { underlyingMagma =
                            -- | Magma for commutativity proof of A.
                            record { underlyingSet = M.mkId "Z" ; binaryOp = M.mkId "+" ; index = AF.magmaIndex }
                        ; associativity = C1L.AXIOM_Associativity (M.mkId "assoc-A")
                        ; index = AF.semigroupIndex
                        }
                    ; identityElement = M.mkId "0"
                    ; identityAxiom = C1L.AXIOM_Identity (M.mkId "id-A")
                    ; index = AF.monoidIndex
                    }
              ; inverseOperation =
                  record
              { forMonoid =
                  record
                    { underlyingSemigroup =
                        record
                          { underlyingMagma =
                              -- | Magma for inverse operation in commutativity proof of A.
                              record { underlyingSet = M.mkId "Z" ; binaryOp = M.mkId "+" ; index = AF.magmaIndex }
                          ; associativity = C1L.AXIOM_Associativity (M.mkId "assoc-A")
                          ; index = AF.semigroupIndex
                          }
                          ; identityElement = M.mkId "0"
                          ; identityAxiom = C1L.AXIOM_Identity (M.mkId "id-A")
                          ; index = AF.monoidIndex
                          }
                    ; inverseMap = M.mkId "neg"
                    ; inverseAxiom = M.mkId "left-inv-A"
                    }
              ; index = AF.groupIndex
              }
        ; axiom = M.mkId "comm-A"
        }
  ; index = AF.abelianGroupIndex
  }

-- | Minimal abelian group declaration for factor B.
abelianGroupDeclB : AF.AbelianGroupDeclaration
abelianGroupDeclB = record
  { underlyingGroup =
      record
        { underlyingMonoid =
            record
              { underlyingSemigroup =
                  record
                    { underlyingMagma =
                        -- | Magma for abelian group B.
                        record { underlyingSet = M.mkId "Z" ; binaryOp = M.mkId "+" ; index = AF.magmaIndex }
                    ; associativity = C1L.AXIOM_Associativity (M.mkId "assoc-B")
                    ; index = AF.semigroupIndex
                    }
              ; identityElement = M.mkId "0"
              ; identityAxiom = C1L.AXIOM_Identity (M.mkId "id-B")
              ; index = AF.monoidIndex
              }
        ; inverseOperation =
            record
          { forMonoid =
              record
                { underlyingSemigroup =
                    record
                      { underlyingMagma =
                          -- | Magma for inverse operation of B.
                          record { underlyingSet = M.mkId "Z" ; binaryOp = M.mkId "+" ; index = AF.magmaIndex }
                      ; associativity = C1L.AXIOM_Associativity (M.mkId "assoc-B")
                      ; index = AF.semigroupIndex
                      }
                    ; identityElement = M.mkId "0"
                    ; identityAxiom = C1L.AXIOM_Identity (M.mkId "id-B")
                    ; index = AF.monoidIndex
                    }
              ; inverseMap = M.mkId "neg"
              ; inverseAxiom = M.mkId "left-inv-B"
              }
        ; index = AF.groupIndex
        }
  ; commutativity =
      record
        { forGroup =
            record
        { underlyingMonoid =
            record
              { underlyingSemigroup =
                  record
                    { underlyingMagma =
                        -- | Magma for commutativity proof of B.
                        record { underlyingSet = M.mkId "Z" ; binaryOp = M.mkId "+" ; index = AF.magmaIndex }
                    ; associativity = C1L.AXIOM_Associativity (M.mkId "assoc-B")
                    ; index = AF.semigroupIndex
                    }
                    ; identityElement = M.mkId "0"
                    ; identityAxiom = C1L.AXIOM_Identity (M.mkId "id-B")
                    ; index = AF.monoidIndex
                    }
              ; inverseOperation =
                  record
            { forMonoid =
                record
                { underlyingSemigroup =
                    record
                      { underlyingMagma =
                          -- | Magma for inverse operation in commutativity proof of B.
                          record { underlyingSet = M.mkId "Z" ; binaryOp = M.mkId "+" ; index = AF.magmaIndex }
                        ; associativity = C1L.AXIOM_Associativity (M.mkId "assoc-B")
                        ; index = AF.semigroupIndex
                        }
                          ; identityElement = M.mkId "0"
                          ; identityAxiom = C1L.AXIOM_Identity (M.mkId "id-B")
                          ; index = AF.monoidIndex
                          }
                    ; inverseMap = M.mkId "neg"
                    ; inverseAxiom = M.mkId "left-inv-B"
                    }
              ; index = AF.groupIndex
              }
        ; axiom = M.mkId "comm-B"
        }
  ; index = AF.abelianGroupIndex
  }

-- Tensor product result (another abelian group)
-- | Abelian group structure on the tensor product Z⊗Z used in tests.
tensorProductDecl : AF.AbelianGroupDeclaration
tensorProductDecl = record
  { underlyingGroup =
      record
        { underlyingMonoid =
            record
              { underlyingSemigroup =
                  record
                    { underlyingMagma =
                        -- | Magma for the tensor product abelian group.
                        record { underlyingSet = M.mkId "Z⊗Z" ; binaryOp = M.mkId "+⊗" ; index = AF.magmaIndex }
                    ; associativity = C1L.AXIOM_Associativity (M.mkId "assoc-tensor")
                    ; index = AF.semigroupIndex
                    }
              ; identityElement = M.mkId "0⊗"
              ; identityAxiom = C1L.AXIOM_Identity (M.mkId "id-tensor")
              ; index = AF.monoidIndex
              }
        ; inverseOperation =
            record
          { forMonoid =
              record
                { underlyingSemigroup =
                    record
                      { underlyingMagma =
                          -- | Magma for inverse operation on the tensor product.
                          record { underlyingSet = M.mkId "Z⊗Z" ; binaryOp = M.mkId "+⊗" ; index = AF.magmaIndex }
                      ; associativity = C1L.AXIOM_Associativity (M.mkId "assoc-tensor")
                      ; index = AF.semigroupIndex
                      }
                    ; identityElement = M.mkId "0⊗"
                    ; identityAxiom = C1L.AXIOM_Identity (M.mkId "id-tensor")
                    ; index = AF.monoidIndex
                    }
              ; inverseMap = M.mkId "neg⊗"
              ; inverseAxiom = M.mkId "left-inv-tensor"
              }
        ; index = AF.groupIndex
        }
  ; commutativity =
      record
        { forGroup =
            record
        { underlyingMonoid =
            record
              { underlyingSemigroup =
                  record
                    { underlyingMagma =
                        -- | Magma for the commutativity witness of the tensor product.
                        record { underlyingSet = M.mkId "Z⊗Z" ; binaryOp = M.mkId "+⊗" ; index = AF.magmaIndex }
                    ; associativity = C1L.AXIOM_Associativity (M.mkId "assoc-tensor")
                    ; index = AF.semigroupIndex
                    }
                    ; identityElement = M.mkId "0⊗"
                    ; identityAxiom = C1L.AXIOM_Identity (M.mkId "id-tensor")
                    ; index = AF.monoidIndex
                    }
              ; inverseOperation =
                  record
            { forMonoid =
                record
                { underlyingSemigroup =
                    record
                      { underlyingMagma =
                          -- | Magma for inverse operation in tensor-product commutativity proof.
                          record { underlyingSet = M.mkId "Z⊗Z" ; binaryOp = M.mkId "+⊗" ; index = AF.magmaIndex }
                        ; associativity = C1L.AXIOM_Associativity (M.mkId "assoc-tensor")
                        ; index = AF.semigroupIndex
                        }
                          ; identityElement = M.mkId "0⊗"
                          ; identityAxiom = C1L.AXIOM_Identity (M.mkId "id-tensor")
                          ; index = AF.monoidIndex
                          }
                    ; inverseMap = M.mkId "neg⊗"
                    ; inverseAxiom = M.mkId "left-inv-tensor"
                    }
              ; index = AF.groupIndex
              }
        ; axiom = M.mkId "comm-tensor"
        }
  ; index = AF.abelianGroupIndex
  }

-- Tensor product declaration
-- | Declares the tensor product object and its universal property witness.
tensorProdAbDecl : AGA.TensorProductAb abelianGroupDeclA abelianGroupDeclB
tensorProdAbDecl = record
  { underlyingSet = M.mkId "A⊗B-underlying"
  ; universalProperty = M.mkId "univ-tensor"
  ; tensorProduct = tensorProductDecl
  }

-- Adapter instance
-- | Adapter instance wiring the tensor product declaration into the obligation framework.
tensorProdAbAdapt : A.TensorProductAbAdapter
tensorProdAbAdapt = A.mkTensorProductAbAdapter abelianGroupDeclA abelianGroupDeclB tensorProdAbDecl tensorProductDecl refl

-- Status assertion
_ : A.isFilledTensorProductAb tensorProdAbAdapt ≡ true
_ = refl

-- Categorical assertions for TensorProductAb (omitted here; smoke-tested via adapter wiring)
