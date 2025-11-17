-- Tests.GroupsFreeChecklist: Coverage for Algebra.Groups.Free (Free Groups and Categorical Constructions)

module Tests.GroupsFreeChecklist where

import Agda.Builtin.Bool as B
open import Agda.Builtin.Equality using (_≡_; refl)
open import Metamodel as M

-- Imports
import Chapter1.Level1 as C1L
import Algebra.Foundation as AF
import Algebra.Groups.Free as AGF
import Tests.ObligationAdapters as A

-- Build minimal algebra scaffolding
magmaDecl : AF.MagmaDeclaration
magmaDecl = record { binaryOp = M.mkId "grpOp" }

semigroupDecl : AF.SemigroupDeclaration
semigroupDecl = record
  { underlyingMagma = magmaDecl
  ; associativity = M.mkId "grpAssoc"
  }

monoidDecl : AF.MonoidDeclaration
monoidDecl = record
  { underlyingSemigroup = semigroupDecl
  ; identityElement = record
    { forSemigroup = semigroupDecl
    ; element = M.mkId "grpId"
    ; leftIdentity = M.mkId "grpLId"
    ; rightIdentity = M.mkId "grpRId"
    }
  }

groupDecl : AF.GroupDeclaration
groupDecl = record
  { underlyingMonoid = monoidDecl
  ; inverseOperation = record
    { forMonoid = monoidDecl
    ; operation = M.mkId "grpInv"
    ; inverseAxiom = M.mkId "grpInvAx"
    }
  }

abelianGroupDecl : AF.AbelianGroupDeclaration
abelianGroupDecl = record
  { underlyingGroup = groupDecl
  ; commutativity = record
    { forGroup = groupDecl
    ; axiom = M.mkId "grpComm"
    }
  }

G : AF.GroupDeclaration
G = groupDecl

H : AF.GroupDeclaration
H = groupDecl

X : M.Identifier
X = M.mkId "genSet"

-- Product in Grp
productInGrpDecl : AGF.ProductInGrp G H
productInGrpDecl = record
  { group1 = G
  ; group2 = H
  ; productGroup = groupDecl
  ; projection1 = M.mkId "proj1"
  ; projection2 = M.mkId "proj2"
  ; isProduct = M.mkId "isProd"
  }

productInGrpAdapt : A.ProductInGrpAdapter
productInGrpAdapt = A.mkProductInGrpAdapter G H productInGrpDecl G refl

productInGrpStatus : A.isFilledProductInGrp productInGrpAdapt ≡ B.true
productInGrpStatus = refl

-- Coproduct in Grp
coproductInGrpDecl : AGF.CoproductInGrp G H
coproductInGrpDecl = record
  { group1 = G
  ; group2 = H
  ; coproductGroup = groupDecl
  ; injection1 = M.mkId "inj1"
  ; injection2 = M.mkId "inj2"
  ; isCoproduct = M.mkId "isCoprod"
  }

coproductInGrpAdapt : A.CoproductInGrpAdapter
coproductInGrpAdapt = A.mkCoproductInGrpAdapter G H coproductInGrpDecl G refl

coproductInGrpStatus : A.isFilledCoproductInGrp coproductInGrpAdapt ≡ B.true
coproductInGrpStatus = refl

-- Free group object
freeGroupObjectDecl : AGF.FreeGroupObject X
freeGroupObjectDecl = record
  { generatingSet = X
  ; freeGroup = groupDecl
  ; universalProperty = M.mkId "freeObjUniversal"
  }

freeGroupObjectAdapt : A.FreeGroupObjectAdapter
freeGroupObjectAdapt = A.mkFreeGroupObjectAdapter X freeGroupObjectDecl X refl

freeGroupObjectStatus : A.isFilledFreeGroupObject freeGroupObjectAdapt ≡ B.true
freeGroupObjectStatus = refl

-- Free group
freeGroupDecl : AGF.FreeGroup X
freeGroupDecl = record
  { generatingSet = X
  ; underlyingGroup = groupDecl
  ; reducedWords = M.mkId "reducedWords"
  ; multiplication = M.mkId "freeGroupMult"
  ; universalExtension = M.mkId "freeGroupUniversal"
  }

freeGroupAdapt : A.FreeGroupAdapter
freeGroupAdapt = A.mkFreeGroupAdapter X freeGroupDecl X refl

freeGroupStatus : A.isFilledFreeGroup freeGroupAdapt ≡ B.true
freeGroupStatus = refl

-- Group presentation
groupPresentationDecl : AGF.GroupPresentation
groupPresentationDecl = record
  { generators = X
  ; relations = M.mkId "relations"
  ; presentedGroup = groupDecl
  }

groupPresentationAdapt : A.GroupPresentationAdapter
groupPresentationAdapt = A.mkGroupPresentationAdapter groupPresentationDecl X refl

groupPresentationStatus : A.isFilledGroupPresentation groupPresentationAdapt ≡ B.true
groupPresentationStatus = refl

-- Abelianization
abelianizationDecl : AGF.Abelianization G
abelianizationDecl = record
  { group = G
  ; abelianization = abelianGroupDecl
  ; universalMap = M.mkId "abelianizeMap"
  ; isLeftAdjoint = M.mkId "abelianizeAdj"
  }

abelianizationAdapt : A.AbelianizationAdapter
abelianizationAdapt = A.mkAbelianizationAdapter G abelianizationDecl G refl

abelianizationStatus : A.isFilledAbelianization abelianizationAdapt ≡ B.true
abelianizationStatus = refl

-- Finitely generated abelian group
fgAbelianGroupDecl : AGF.FinitelyGeneratedAbelianGroup
fgAbelianGroupDecl = record
  { underlyingGroup = abelianGroupDecl
  ; generators = X
  ; finitelyGenerated = M.mkId "isFinitelyGenerated"
  }

fgAbelianGroupAdapt : A.FinitelyGeneratedAbelianGroupAdapter
fgAbelianGroupAdapt = A.mkFinitelyGeneratedAbelianGroupAdapter fgAbelianGroupDecl abelianGroupDecl refl

fgAbelianGroupStatus : A.isFilledFinitelyGeneratedAbelianGroup fgAbelianGroupAdapt ≡ B.true
fgAbelianGroupStatus = refl
