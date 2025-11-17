-- Tests/ModulesChecklist.agda
-- Coverage for Algebra.Modules.Basic with concrete instances

module Tests.ModulesChecklist where

open import Agda.Builtin.Unit using (⊤; tt)
open import Agda.Builtin.Equality using (_≡_; refl)
import Agda.Builtin.Bool as B
open import Metamodel as M
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
magmaDecl = record { underlyingSet = M.mkId "M" ; binaryOp = M.mkId "∙" }

assocAxiom : C1L.AssociativityAxiom
assocAxiom = record { over = M.mkId "∙" }

semigroupDecl : AF.SemigroupDeclaration
semigroupDecl = record { underlyingMagma = magmaDecl ; associativity = assocAxiom }

identityAxiom : C1L.IdentityAxiom
identityAxiom = record { over = M.mkId "e" }

monoidDecl : AF.MonoidDeclaration
monoidDecl = record
  { underlyingSemigroup = semigroupDecl
  ; identityElement = M.mkId "e"
  ; identityAxiom = identityAxiom
  }

groupDecl : AF.GroupDeclaration
groupDecl = record
  { underlyingMonoid = monoidDecl
  ; inverseOperation = record { forMonoid = monoidDecl ; inverseMap = M.mkId "inv" ; inverseAxiom = M.mkId "inv-proof" }
  }

abelianGroupDecl : AF.AbelianGroupDeclaration
abelianGroupDecl = record { underlyingGroup = groupDecl ; commutativity = record { forGroup = groupDecl ; axiom = M.mkId "comm" } }

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

leftModule-status : A.isFilledLeftModule leftModule-adapter ≡ B.true
leftModule-status = refl

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

hom-status : A.isFilledModuleHom hom-adapter ≡ B.true
hom-status = refl

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

submodule-status : A.isFilledSubmodule submodule-adapter ≡ B.true
submodule-status = refl

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

quotient-status : A.isFilledQuotientModule quotient-adapter ≡ B.true
quotient-status = refl

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

kernel-status : A.isFilledKernelModuleHom kernel-adapter ≡ B.true
kernel-status = refl

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

image-status : A.isFilledImageModuleHom image-adapter ≡ B.true
image-status = refl

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

cokernel-status : A.isFilledCokernelModuleHom cokernel-adapter ≡ B.true
cokernel-status = refl

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

exseq-status : A.isFilledModuleExactSequence exseq-adapter ≡ B.true
exseq-status = refl

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

ses-status : A.isFilledModuleShortExactSequence ses-adapter ≡ B.true
ses-status = refl
