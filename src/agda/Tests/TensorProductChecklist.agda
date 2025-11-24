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
abelianGroupDeclA : AF.AbelianGroupDeclaration
abelianGroupDeclA = record
  { underlyingGroup =
      record
        { underlyingMonoid =
            record
              { underlyingSemigroup =
                  record
                    { underlyingMagma =
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

abelianGroupDeclB : AF.AbelianGroupDeclaration
abelianGroupDeclB = record
  { underlyingGroup =
      record
        { underlyingMonoid =
            record
              { underlyingSemigroup =
                  record
                    { underlyingMagma =
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
tensorProductDecl : AF.AbelianGroupDeclaration
tensorProductDecl = record
  { underlyingGroup =
      record
        { underlyingMonoid =
            record
              { underlyingSemigroup =
                  record
                    { underlyingMagma =
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
tensorProdAbDecl : AGA.TensorProductAb abelianGroupDeclA abelianGroupDeclB
tensorProdAbDecl = record
  { underlyingSet = M.mkId "A⊗B-underlying"
  ; universalProperty = M.mkId "univ-tensor"
  ; tensorProduct = tensorProductDecl
  }

-- Adapter instance
tensorProdAbAdapt : A.TensorProductAbAdapter
tensorProdAbAdapt = A.mkTensorProductAbAdapter abelianGroupDeclA abelianGroupDeclB tensorProdAbDecl tensorProductDecl refl

-- Status assertion
_ : A.isFilledTensorProductAb tensorProdAbAdapt ≡ true
_ = refl

-- Categorical assertions for TensorProductAb (omitted here; smoke-tested via adapter wiring)
