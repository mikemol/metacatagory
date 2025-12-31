{-# OPTIONS --without-K #-}

-- Tests/ModulesChecklist.agda
-- Coverage for Algebra.Modules.Basic with concrete instances

module Tests.ModulesChecklist where

open import Agda.Builtin.Unit using (⊤; tt)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Core.Phase using (Bool; true; false)
open import Metamodel as M
open import Core.CategoricalAdapter
import Tests.ObligationAdapters as A

-- Algebra imports
import Algebra.Foundation as AF
import Algebra.Groups.Basic as GB
import Algebra.Groups.Abelian as GA
import Algebra.Rings.Basic as AR
import Algebra.Modules.Basic as AM
import Chapter1.Level1 as C1L

------------------------------------------------------------------------
-- Minimal algebra to build modules
------------------------------------------------------------------------

magmaDecl : AF.MagmaDeclaration
magmaDecl = record { underlyingSet = M.mkId "M" ; binaryOp = M.mkId "∙" ; index = AF.magmaIndex }

assocAxiom : C1L.AssociativityAxiom
assocAxiom = record { over = M.mkId "∙" }

semigroupDecl : AF.SemigroupDeclaration
semigroupDecl = record { underlyingMagma = magmaDecl ; associativity = assocAxiom ; index = AF.semigroupIndex }

identityAxiom : C1L.IdentityAxiom
identityAxiom = record { over = M.mkId "e" }

monoidDecl : AF.MonoidDeclaration
monoidDecl = record
  { underlyingSemigroup = semigroupDecl
  ; identityElement = M.mkId "e"
  ; identityAxiom = identityAxiom
  ; index = AF.monoidIndex
  }

groupDecl : AF.GroupDeclaration
groupDecl = record
  { underlyingMonoid = monoidDecl
  ; inverseOperation = record { forMonoid = monoidDecl ; inverseMap = M.mkId "inv" ; inverseAxiom = M.mkId "inv-proof" }
  ; index = AF.groupIndex
  }

abelianGroupDecl : AF.AbelianGroupDeclaration
abelianGroupDecl = record { underlyingGroup = groupDecl ; commutativity = record { forGroup = groupDecl ; axiom = M.mkId "comm" } ; index = AF.abelianGroupIndex }

ringDecl : AR.RingDeclaration
ringDecl = record
  { identifier = M.mkId "R"
  ; additiveGroup = abelianGroupDecl
  ; multiplication = M.mkId "·"
  ; multAssociative = M.mkId "mult-assoc"
  ; leftDistributive = M.mkId "left-dist"
  ; rightDistributive = M.mkId "right-dist"
  }

------------------------------------------------------------------------
-- Left modules and homomorphisms
------------------------------------------------------------------------

Mmod Nmod : AM.LeftModule ringDecl
Mmod = record
  { ring = ringDecl
  ; underlyingAbelianGroup = abelianGroupDecl
  ; scalarMultiplication = M.mkId "⋅"
  ; distributiveOverAddition = M.mkId "r(m+n)=rm+rn"
  ; distributiveOverRingAddition = M.mkId "(r+s)m=rm+sm"
  ; associativeScalar = M.mkId "(rs)m=r(sm)"
  ; unitalAction = M.mkId "1·m=m"
  }

Nmod = record
  { ring = ringDecl
  ; underlyingAbelianGroup = abelianGroupDecl
  ; scalarMultiplication = M.mkId "⋅"
  ; distributiveOverAddition = M.mkId "r(m+n)=rm+rn"
  ; distributiveOverRingAddition = M.mkId "(r+s)m=rm+sm"
  ; associativeScalar = M.mkId "(rs)m=r(sm)"
  ; unitalAction = M.mkId "1·m=m"
  }

leftModule-adapter : A.LeftModuleAdapter
leftModule-adapter = A.mkLeftModuleAdapter ringDecl Mmod (AM.LeftModule.ring Mmod) refl

leftModule-status : A.isFilledLeftModule leftModule-adapter ≡ true
leftModule-status = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.leftModuleCategorical leftModule-adapter) tt) ≡ A.LeftModuleAdapter.decl leftModule-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.leftModuleCategorical leftModule-adapter) ≡ refl
_ = refl

hom : AM.ModuleHomomorphism ringDecl Mmod Nmod
hom = record
  { ring = ringDecl
  ; sourceModule = Mmod
  ; targetModule = Nmod
  ; morphism = M.mkId "f"
  ; preservesAddition = M.mkId "+"
  ; preservesScalarMultiplication = M.mkId "scalar"
  }

hom-adapter : A.ModuleHomomorphismAdapter
hom-adapter = A.mkModuleHomomorphismAdapter ringDecl Mmod Nmod hom (AM.ModuleHomomorphism.ring hom) refl

hom-status : A.isFilledModuleHom hom-adapter ≡ true
hom-status = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.moduleHomomorphismCategorical hom-adapter) tt) ≡ A.ModuleHomomorphismAdapter.decl hom-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.moduleHomomorphismCategorical hom-adapter) ≡ refl
_ = refl

------------------------------------------------------------------------
-- Submodule, quotient, kernel, image, cokernel
------------------------------------------------------------------------

underG : AF.GroupDeclaration
underG = AF.AbelianGroupDeclaration.underlyingGroup abelianGroupDecl

subgroupVal : GB.Subgroup underG
subgroupVal = record
  { subset = M.mkId "H"
  ; inclusion = M.mkId "ι"
  ; closedUnderOp = M.mkId "closedOp"
  ; containsIdentity = M.mkId "e∈H"
  ; closedUnderInverse = M.mkId "invH"
  }

subM : AM.Submodule ringDecl Mmod
subM = record
  { ring = ringDecl
  ; module' = Mmod
  ; subgroup = subgroupVal
  ; closedUnderScalars = M.mkId "rH⊆H"
  }

submodule-adapter : A.SubmoduleAdapter
submodule-adapter = A.mkSubmoduleAdapter ringDecl Mmod subM (AM.Submodule.ring subM) refl

submodule-status : A.isFilledSubmodule submodule-adapter ≡ true
submodule-status = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.submoduleCategorical submodule-adapter) tt) ≡ A.SubmoduleAdapter.decl submodule-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.submoduleCategorical submodule-adapter) ≡ refl
_ = refl

quotM : AM.QuotientModule ringDecl Mmod subM
quotM = record
  { ring = ringDecl
  ; module' = Mmod
  ; submodule = subM
  ; quotientModule = Nmod
  ; canonicalProjection = M.mkId "π"
  }

quotient-adapter : A.QuotientModuleAdapter
quotient-adapter = A.mkQuotientModuleAdapter ringDecl Mmod subM quotM (AM.QuotientModule.ring quotM) refl

quotient-status : A.isFilledQuotientModule quotient-adapter ≡ true
quotient-status = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.quotientModuleCategorical quotient-adapter) tt) ≡ A.QuotientModuleAdapter.decl quotient-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.quotientModuleCategorical quotient-adapter) ≡ refl
_ = refl

ker : AM.KernelOfModuleHomomorphism ringDecl (AM.ModuleHomomorphism.morphism hom)
ker = record
  { ring = ringDecl
  ; sourceModule = Mmod
  ; targetModule = Nmod
  ; homomorphism = hom
  ; kernel = subM
  }

kernel-adapter : A.KernelOfModuleHomomorphismAdapter
kernel-adapter = A.mkKernelOfModuleHomomorphismAdapter ringDecl (AM.ModuleHomomorphism.morphism hom) ker (AM.KernelOfModuleHomomorphism.ring ker) refl

kernel-status : A.isFilledKernelModuleHom kernel-adapter ≡ true
kernel-status = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.kernelOfModuleHomomorphismCategorical kernel-adapter) tt) ≡ A.KernelOfModuleHomomorphismAdapter.decl kernel-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.kernelOfModuleHomomorphismCategorical kernel-adapter) ≡ refl
_ = refl

img : AM.ImageOfModuleHomomorphism ringDecl (AM.ModuleHomomorphism.morphism hom)
img = record
  { ring = ringDecl
  ; sourceModule = Mmod
  ; targetModule = Nmod
  ; homomorphism = hom
  ; image = subM
  }

image-adapter : A.ImageOfModuleHomomorphismAdapter
image-adapter = A.mkImageOfModuleHomomorphismAdapter ringDecl (AM.ModuleHomomorphism.morphism hom) img (AM.ImageOfModuleHomomorphism.ring img) refl

image-status : A.isFilledImageModuleHom image-adapter ≡ true
image-status = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.imageOfModuleHomomorphismCategorical image-adapter) tt) ≡ A.ImageOfModuleHomomorphismAdapter.decl image-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.imageOfModuleHomomorphismCategorical image-adapter) ≡ refl
_ = refl

cok : AM.CokernelOfModuleHomomorphism ringDecl (AM.ModuleHomomorphism.morphism hom)
cok = record
  { ring = ringDecl
  ; sourceModule = Mmod
  ; targetModule = Nmod
  ; homomorphism = hom
  ; cokernel = Nmod
  }

cokernel-adapter : A.CokernelOfModuleHomomorphismAdapter
cokernel-adapter = A.mkCokernelOfModuleHomomorphismAdapter ringDecl (AM.ModuleHomomorphism.morphism hom) cok (AM.CokernelOfModuleHomomorphism.ring cok) refl

cokernel-status : A.isFilledCokernelModuleHom cokernel-adapter ≡ true
cokernel-status = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.cokernelOfModuleHomomorphismCategorical cokernel-adapter) tt) ≡ A.CokernelOfModuleHomomorphismAdapter.decl cokernel-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.cokernelOfModuleHomomorphismCategorical cokernel-adapter) ≡ refl
_ = refl

------------------------------------------------------------------------
-- Exact sequences
------------------------------------------------------------------------

exseq : AM.ExactSequence ringDecl
exseq = record
  { ring = ringDecl
  ; modules = M.mkId "{M_i}"
  ; morphisms = M.mkId "{f_i}"
  ; exactnessCondition = M.mkId "im=ker"
  }

exseq-adapter : A.ModuleExactSequenceAdapter
exseq-adapter = A.mkModuleExactSequenceAdapter ringDecl exseq (AM.ExactSequence.ring exseq) refl

exseq-status : A.isFilledModuleExactSequence exseq-adapter ≡ true
exseq-status = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.moduleExactSequenceCategorical exseq-adapter) tt) ≡ A.ModuleExactSequenceAdapter.decl exseq-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.moduleExactSequenceCategorical exseq-adapter) ≡ refl
_ = refl

ses : AM.ShortExactSequence ringDecl
ses = record
  { ring = ringDecl
  ; leftModule = Mmod
  ; middleModule = Nmod
  ; rightModule = Mmod
  ; leftMap = M.mkId "f"
  ; rightMap = M.mkId "g"
  ; exactness = M.mkId "ker=g"
  }

ses-adapter : A.ModuleShortExactSequenceAdapter
ses-adapter = A.mkModuleShortExactSequenceAdapter ringDecl ses (AM.ShortExactSequence.ring ses) refl

ses-status : A.isFilledModuleShortExactSequence ses-adapter ≡ true
ses-status = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.moduleShortExactSequenceCategorical ses-adapter) tt) ≡ A.ModuleShortExactSequenceAdapter.decl ses-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.moduleShortExactSequenceCategorical ses-adapter) ≡ refl
_ = refl
