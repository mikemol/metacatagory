-- Tests.FieldsBasicChecklist: Coverage for Algebra.Fields.Basic (Core Galois Theory)

module Tests.FieldsBasicChecklist where

import Agda.Builtin.Bool as B
open import Agda.Builtin.Equality using (_≡_; refl)
open import Metamodel as M
open import Core.CategoricalAdapter

-- Imports
import Chapter1.Level1 as C1L
import Algebra.Foundation as AF
import Algebra.Rings.Basic as AR
import Algebra.Fields.Basic as AFB
import Tests.ObligationAdapters as A

-- Build minimal algebra scaffolding for field theory
magmaDecl : AF.MagmaDeclaration
magmaDecl = record
  { binaryOp = M.mkId "fieldOp"
  }

semigroupDecl : AF.SemigroupDeclaration
semigroupDecl = record
  { underlyingMagma = magmaDecl
  ; associativity = M.mkId "fieldAssoc"
  }

monoidDecl : AF.MonoidDeclaration
monoidDecl = record
  { underlyingSemigroup = semigroupDecl
  ; identityElement = record
    { forSemigroup = semigroupDecl
    ; element = M.mkId "fieldId"
    ; leftIdentity = M.mkId "fieldLId"
    ; rightIdentity = M.mkId "fieldRId"
    }
  }

groupDecl : AF.GroupDeclaration
groupDecl = record
  { underlyingMonoid = monoidDecl
  ; inverseOperation = record
    { forMonoid = monoidDecl
    ; operation = M.mkId "fieldInv"
    ; inverseAxiom = M.mkId "fieldInvAx"
    }
  }

abelianGroupDecl : AF.AbelianGroupDeclaration
abelianGroupDecl = record
  { underlyingGroup = groupDecl
  ; commutativity = record
    { forGroup = groupDecl
    ; axiom = M.mkId "fieldComm"
    }
  }

ringDecl : AR.RingDeclaration
ringDecl = record
  { identifier = M.mkId "fieldRing"
  ; additiveGroup = abelianGroupDecl
  ; multiplication = M.mkId "fieldMult"
  ; multAssociative = M.mkId "fieldMultAssoc"
  ; leftDistributive = M.mkId "fieldLDist"
  ; rightDistributive = M.mkId "fieldRDist"
  }

unitalRingDecl : AR.UnitalRingDeclaration
unitalRingDecl = record
  { underlyingRing = ringDecl
  ; multiplicativeIdentity = M.mkId "fieldOne"
  ; leftIdentity = M.mkId "fieldOneLId"
  ; rightIdentity = M.mkId "fieldOneRId"
  }

commRingDecl : AR.CommutativeRingDeclaration
commRingDecl = record
  { underlyingRing = unitalRingDecl
  ; commutativity = M.mkId "fieldMultComm"
  }

fieldDecl : AR.FieldDeclaration
fieldDecl = record
  { underlyingRing = commRingDecl
  ; inverses = M.mkId "fieldInverses"
  }

-- Use F = E = K for simplicity
F : AR.FieldDeclaration
F = fieldDecl

E : AR.FieldDeclaration
E = fieldDecl

K : AR.FieldDeclaration
K = fieldDecl

α : M.Identifier
α = M.mkId "alpha"

f : M.Identifier
f = M.mkId "polynomial"

-- Subfield
subfieldDecl : AFB.Subfield F
subfieldDecl = record
  { subfield = F
  ; subset = M.mkId "subfieldSubset"
  ; inclusion = M.mkId "subfieldInc"
  ; isSubfield = M.mkId "subfieldProp"
  }

subfieldAdapt : A.SubfieldAdapter
subfieldAdapt = A.mkSubfieldAdapter F subfieldDecl F refl

subfieldStatus : A.isFilledSubfield subfieldAdapt ≡ B.true
subfieldStatus = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.subfieldCategorical subfieldAdapt) tt) ≡ A.SubfieldAdapter.decl subfieldAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.subfieldCategorical subfieldAdapt) ≡ refl
_ = refl

-- Field extension
fieldExtDecl : AFB.FieldExtension F E
fieldExtDecl = record
  { baseField = F
  ; extensionField = E
  ; inclusion = M.mkId "extInclusion"
  ; vectorSpaceStructure = M.mkId "extVecSpace"
  }

fieldExtAdapt : A.FieldExtensionAdapter
fieldExtAdapt = A.mkFieldExtensionAdapter F E fieldExtDecl F refl

fieldExtStatus : A.isFilledFieldExtension fieldExtAdapt ≡ B.true
fieldExtStatus = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.fieldExtensionCategorical fieldExtAdapt) tt) ≡ A.FieldExtensionAdapter.decl fieldExtAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.fieldExtensionCategorical fieldExtAdapt) ≡ refl
_ = refl

-- Algebraic element
algElemDecl : AFB.AlgebraicElement F E α
algElemDecl = record
  { baseField = F
  ; extensionField = E
  ; element = α
  ; minimalPolynomial = M.mkId "minPoly"
  ; isAlgebraic = M.mkId "isAlg"
  }

algElemAdapt : A.AlgebraicElementAdapter
algElemAdapt = A.mkAlgebraicElementAdapter F E α algElemDecl F refl

algElemStatus : A.isFilledAlgebraicElement algElemAdapt ≡ B.true
algElemStatus = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.algebraicElementCategorical algElemAdapt) tt) ≡ A.AlgebraicElementAdapter.decl algElemAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.algebraicElementCategorical algElemAdapt) ≡ refl
_ = refl

-- Algebraic extension
algExtDecl : AFB.AlgebraicExtension F E
algExtDecl = record
  { baseField = F
  ; extensionField = E
  ; isAlgebraicExtension = M.mkId "algExt"
  }

algExtAdapt : A.AlgebraicExtensionAdapter
algExtAdapt = A.mkAlgebraicExtensionAdapter F E algExtDecl F refl

algExtStatus : A.isFilledAlgebraicExtension algExtAdapt ≡ B.true
algExtStatus = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.algebraicExtensionCategorical algExtAdapt) tt) ≡ A.AlgebraicExtensionAdapter.decl algExtAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.algebraicExtensionCategorical algExtAdapt) ≡ refl
_ = refl

-- Field automorphism
fieldAutDecl : AFB.FieldAutomorphism F E
fieldAutDecl = record
  { baseField = F
  ; extensionField = E
  ; automorphism = M.mkId "fieldAuto"
  ; isAutomorphism = M.mkId "isAuto"
  }

fieldAutAdapt : A.FieldAutomorphismAdapter
fieldAutAdapt = A.mkFieldAutomorphismAdapter F E fieldAutDecl F refl

fieldAutStatus : A.isFilledFieldAutomorphism fieldAutAdapt ≡ B.true
fieldAutStatus = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.fieldAutomorphismCategorical fieldAutAdapt) tt) ≡ A.FieldAutomorphismAdapter.decl fieldAutAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.fieldAutomorphismCategorical fieldAutAdapt) ≡ refl
_ = refl

-- Galois group
galoisGrpDecl : AFB.GaloisGroup F E
galoisGrpDecl = record
  { baseField = F
  ; extensionField = E
  ; group = groupDecl
  ; automorphisms = M.mkId "galGrpAuts"
  }

galoisGrpAdapt : A.GaloisGroupAdapter
galoisGrpAdapt = A.mkGaloisGroupAdapter F E galoisGrpDecl F refl

galoisGrpStatus : A.isFilledGaloisGroup galoisGrpAdapt ≡ B.true
galoisGrpStatus = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.galoisGroupCategorical galoisGrpAdapt) tt) ≡ A.GaloisGroupAdapter.decl galoisGrpAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.galoisGroupCategorical galoisGrpAdapt) ≡ refl
_ = refl

-- Galois extension
galoisExtDecl : AFB.GaloisExtension F E
galoisExtDecl = record
  { baseField = F
  ; extensionField = E
  ; galoisGroup = galoisGrpDecl
  ; isGalois = M.mkId "isGalois"
  }

galoisExtAdapt : A.GaloisExtensionAdapter
galoisExtAdapt = A.mkGaloisExtensionAdapter F E galoisExtDecl F refl

galoisExtStatus : A.isFilledGaloisExtension galoisExtAdapt ≡ B.true
galoisExtStatus = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.galoisExtensionCategorical galoisExtAdapt) tt) ≡ A.GaloisExtensionAdapter.decl galoisExtAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.galoisExtensionCategorical galoisExtAdapt) ≡ refl
_ = refl

-- Normal extension
normalExtDecl : AFB.NormalExtension F E
normalExtDecl = record
  { baseField = F
  ; extensionField = E
  ; isNormal = M.mkId "isNormal"
  }

normalExtAdapt : A.NormalExtensionAdapter
normalExtAdapt = A.mkNormalExtensionAdapter F E normalExtDecl F refl

normalExtStatus : A.isFilledNormalExtension normalExtAdapt ≡ B.true
normalExtStatus = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.normalExtensionCategorical normalExtAdapt) tt) ≡ A.NormalExtensionAdapter.decl normalExtAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.normalExtensionCategorical normalExtAdapt) ≡ refl
_ = refl

-- Separable extension
sepExtDecl : AFB.SeparableExtension F E
sepExtDecl = record
  { baseField = F
  ; extensionField = E
  ; isSeparable = M.mkId "isSeparable"
  }

sepExtAdapt : A.SeparableExtensionAdapter
sepExtAdapt = A.mkSeparableExtensionAdapter F E sepExtDecl F refl

sepExtStatus : A.isFilledSeparableExtension sepExtAdapt ≡ B.true
sepExtStatus = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.separableExtensionCategorical sepExtAdapt) tt) ≡ A.SeparableExtensionAdapter.decl sepExtAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.separableExtensionCategorical sepExtAdapt) ≡ refl
_ = refl

-- Splitting field
splitFieldDecl : AFB.SplittingField F f
splitFieldDecl = record
  { baseField = F
  ; polynomial = f
  ; splittingField = E
  ; definition = M.mkId "splitDef"
  }

splitFieldAdapt : A.SplittingFieldAdapter
splitFieldAdapt = A.mkSplittingFieldAdapter F f splitFieldDecl F refl

splitFieldStatus : A.isFilledSplittingField splitFieldAdapt ≡ B.true
splitFieldStatus = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.splittingFieldCategorical splitFieldAdapt) tt) ≡ A.SplittingFieldAdapter.decl splitFieldAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.splittingFieldCategorical splitFieldAdapt) ≡ refl
_ = refl

-- Algebraic closure
algClosureDecl : AFB.AlgebraicClosure F
algClosureDecl = record
  { baseField = F
  ; closureField = E
  ; isAlgebraicClosure = M.mkId "algClosure"
  }

algClosureAdapt : A.AlgebraicClosureAdapter
algClosureAdapt = A.mkAlgebraicClosureAdapter F algClosureDecl F refl

algClosureStatus : A.isFilledAlgebraicClosure algClosureAdapt ≡ B.true
algClosureStatus = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.algebraicClosureCategorical algClosureAdapt) tt) ≡ A.AlgebraicClosureAdapter.decl algClosureAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.algebraicClosureCategorical algClosureAdapt) ≡ refl
_ = refl
