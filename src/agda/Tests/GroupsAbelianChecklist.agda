-- Tests.GroupsAbelianChecklist: Coverage for Algebra.Groups.Abelian (Free Abelian Groups & Grothendieck)

module Tests.GroupsAbelianChecklist where

open import Core.Phase using (Bool; true; false)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Agda.Builtin.Unit using (⊤; tt)
open import Metamodel as M
open import Core.CategoricalAdapter

-- Imports
import Chapter1.Level1 as C1L
import Algebra.Foundation as AF
import Algebra.Groups.Abelian as AGA
import Tests.ObligationAdapters as A

-- Build minimal algebra scaffolding
magmaDecl : AF.MagmaDeclaration
magmaDecl = record { underlyingSet = M.mkId "abCarrier" ; binaryOp = M.mkId "abOp" ; index = AF.magmaIndex }

semigroupDecl : AF.SemigroupDeclaration
semigroupDecl = record
  { underlyingMagma = magmaDecl
  ; associativity = C1L.AXIOM_Associativity (M.mkId "abAssoc")
  ; index = AF.semigroupIndex
  }

monoidDecl : AF.MonoidDeclaration
monoidDecl = record
  { underlyingSemigroup = semigroupDecl
  ; identityElement = M.mkId "abId"
  ; identityAxiom = C1L.AXIOM_Identity (M.mkId "abIdAx")
  ; index = AF.monoidIndex
  }

groupDecl : AF.GroupDeclaration
groupDecl = record
  { underlyingMonoid = monoidDecl
  ; inverseOperation = record
    { forMonoid = monoidDecl
    ; inverseMap = M.mkId "abInv"
    ; inverseAxiom = M.mkId "abInvAx"
    }
  ; index = AF.groupIndex
  }

abelianGroupDecl : AF.AbelianGroupDeclaration
abelianGroupDecl = record
  { underlyingGroup = groupDecl
  ; commutativity = record
    { forGroup = groupDecl
    ; axiom = M.mkId "abComm"
    }
  ; index = AF.abelianGroupIndex
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

freeAbelianGroupStatus : A.isFilledFreeAbelianGroup freeAbelianGroupAdapt ≡ true
freeAbelianGroupStatus = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.freeAbelianGroupCategorical freeAbelianGroupAdapt) tt) ≡ A.FreeAbelianGroupAdapter.decl freeAbelianGroupAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.freeAbelianGroupCategorical freeAbelianGroupAdapt) ≡ refl
_ = refl

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

freeForgetfulAdjAbStatus : A.isFilledFreeForgetfulAdjunctionAb freeForgetfulAdjAbAdapt ≡ true
freeForgetfulAdjAbStatus = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.freeForgetfulAdjunctionAbCategorical freeForgetfulAdjAbAdapt) tt) ≡ A.FreeForgetfulAdjunctionAbAdapter.decl freeForgetfulAdjAbAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.freeForgetfulAdjunctionAbCategorical freeForgetfulAdjAbAdapt) ≡ refl
_ = refl

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

grothendieckGroupStatus : A.isFilledGrothendieckGroup grothendieckGroupAdapt ≡ true
grothendieckGroupStatus = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.grothendieckGroupCategorical grothendieckGroupAdapt) tt) ≡ A.GrothendieckGroupAdapter.decl grothendieckGroupAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.grothendieckGroupCategorical grothendieckGroupAdapt) ≡ refl
_ = refl
