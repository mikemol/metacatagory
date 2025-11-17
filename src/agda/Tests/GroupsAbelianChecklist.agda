-- Tests.GroupsAbelianChecklist: Coverage for Algebra.Groups.Abelian (Free Abelian Groups & Grothendieck)

module Tests.GroupsAbelianChecklist where

import Agda.Builtin.Bool as B
open import Agda.Builtin.Equality using (_≡_; refl)
open import Metamodel as M

-- Imports
import Chapter1.Level1 as C1L
import Algebra.Foundation as AF
import Algebra.Groups.Abelian as AGA
import Tests.ObligationAdapters as A

-- Build minimal algebra scaffolding
magmaDecl : AF.MagmaDeclaration
magmaDecl = record { binaryOp = M.mkId "abOp" }

semigroupDecl : AF.SemigroupDeclaration
semigroupDecl = record
  { underlyingMagma = magmaDecl
  ; associativity = M.mkId "abAssoc"
  }

monoidDecl : AF.MonoidDeclaration
monoidDecl = record
  { underlyingSemigroup = semigroupDecl
  ; identityElement = record
    { forSemigroup = semigroupDecl
    ; element = M.mkId "abId"
    ; leftIdentity = M.mkId "abLId"
    ; rightIdentity = M.mkId "abRId"
    }
  }

groupDecl : AF.GroupDeclaration
groupDecl = record
  { underlyingMonoid = monoidDecl
  ; inverseOperation = record
    { forMonoid = monoidDecl
    ; operation = M.mkId "abInv"
    ; inverseAxiom = M.mkId "abInvAx"
    }
  }

abelianGroupDecl : AF.AbelianGroupDeclaration
abelianGroupDecl = record
  { underlyingGroup = groupDecl
  ; commutativity = record
    { forGroup = groupDecl
    ; axiom = M.mkId "abComm"
    }
  }

X : M.Identifier
X = M.mkId "genSet"

-- Free abelian group
freeAbelianGroupDecl : AGA.FreeAbelianGroup X
freeAbelianGroupDecl = record
  { underlyingSet = M.mkId "freeAbSet"
  ; universalProperty = M.mkId "freeAbUniversal"
  ; abelianGroupStructure = abelianGroupDecl
  }

freeAbelianGroupAdapt : A.FreeAbelianGroupAdapter
freeAbelianGroupAdapt = A.mkFreeAbelianGroupAdapter X freeAbelianGroupDecl (M.mkId "freeAbSet") refl

freeAbelianGroupStatus : A.isFilledFreeAbelianGroup freeAbelianGroupAdapt ≡ B.true
freeAbelianGroupStatus = refl

-- Free-Forgetful adjunction Ab
freeForgetfulAdjAbDecl : AGA.FreeForgetfulAdjunctionAb
freeForgetfulAdjAbDecl = record
  { freeFunctor = M.mkId "freeAbFunctor"
  ; forgetfulFunctor = M.mkId "forgetAbFunctor"
  ; adjunctionIsomorphism = M.mkId "abAdjIso"
  ; unit = M.mkId "abAdjUnit"
  ; counit = M.mkId "abAdjCounit"
  }

freeForgetfulAdjAbAdapt : A.FreeForgetfulAdjunctionAbAdapter
freeForgetfulAdjAbAdapt = A.mkFreeForgetfulAdjunctionAbAdapter freeForgetfulAdjAbDecl (M.mkId "freeAbFunctor") refl

freeForgetfulAdjAbStatus : A.isFilledFreeForgetfulAdjunctionAb freeForgetfulAdjAbAdapt ≡ B.true
freeForgetfulAdjAbStatus = refl

-- Grothendieck group
grothendieckGroupDecl : AGA.GrothendieckGroup monoidDecl
grothendieckGroupDecl = record
  { underlyingSet = M.mkId "grothSet"
  ; groupOperation = M.mkId "grothOp"
  ; identityElement = M.mkId "grothId"
  ; inverseOperation = M.mkId "grothInv"
  ; abelianGroupStructure = abelianGroupDecl
  ; universalMap = M.mkId "grothUniversalMap"
  ; universalProperty = M.mkId "grothUniversal"
  }

grothendieckGroupAdapt : A.GrothendieckGroupAdapter
grothendieckGroupAdapt = A.mkGrothendieckGroupAdapter monoidDecl grothendieckGroupDecl (M.mkId "grothSet") refl

grothendieckGroupStatus : A.isFilledGrothendieckGroup grothendieckGroupAdapt ≡ B.true
grothendieckGroupStatus = refl
