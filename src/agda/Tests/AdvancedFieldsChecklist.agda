-- Tests/AdvancedFieldsChecklist.agda
-- Coverage for Algebra.Fields.Advanced with concrete instances

module Tests.AdvancedFieldsChecklist where

open import Agda.Builtin.Unit using (⊤; tt)
open import Agda.Builtin.Equality using (_≡_; refl)
import Agda.Builtin.Bool as B
open import Metamodel as M
open import Core.CategoricalAdapter
import Tests.ObligationAdapters as A
import Algebra.Rings.Basic as AR
import Algebra.Fields.Advanced as AFA
import Algebra.Foundation as AF
import Chapter1.Level1 as C1L

------------------------------------------------------------------------
-- Base field and extension placeholders
------------------------------------------------------------------------

-- Minimal field scaffold
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

unitalRingDecl : AR.UnitalRingDeclaration
unitalRingDecl = record { underlyingRing = ringDecl ; multiplicativeIdentity = M.mkId "1" ; leftIdentity = M.mkId "1·a=a" ; rightIdentity = M.mkId "a·1=a" }

commRingDecl : AR.CommutativeRingDeclaration
commRingDecl = record { underlyingRing = unitalRingDecl ; commutativity = M.mkId "ab=ba" }

fieldDecl : AR.FieldDeclaration
fieldDecl = record { underlyingRing = commRingDecl ; inverses = M.mkId "field-inverses" }

F E K : AR.FieldDeclaration
F = fieldDecl
E = F
K = F

------------------------------------------------------------------------
-- Inseparable and purely inseparable extensions
------------------------------------------------------------------------

inseparable : AFA.InseparableExtension F E
inseparable = record { baseField = F ; extensionField = E ; isInseparable = M.mkId "inseparable" }

inseparable-adapter : A.InseparableExtensionAdapter
inseparable-adapter = A.mkInseparableExtensionAdapter F E inseparable (AFA.InseparableExtension.baseField inseparable) refl

inseparable-status : A.isFilledInseparableExtension inseparable-adapter ≡ B.true
inseparable-status = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.inseparableExtensionCategorical inseparable-adapter) tt) ≡ A.InseparableExtensionAdapter.decl inseparable-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.inseparableExtensionCategorical inseparable-adapter) ≡ refl
_ = refl

purely : AFA.PurelyInseparableExtension F E
purely = record { baseField = F ; extensionField = E ; isPurelyInseparable = M.mkId "purely" }

purely-adapter : A.PurelyInseparableExtensionAdapter
purely-adapter = A.mkPurelyInseparableExtensionAdapter F E purely (AFA.PurelyInseparableExtension.baseField purely) refl

purely-status : A.isFilledPurelyInseparableExtension purely-adapter ≡ B.true
purely-status = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.purelyInseparableExtensionCategorical purely-adapter) tt) ≡ A.PurelyInseparableExtensionAdapter.decl purely-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.purelyInseparableExtensionCategorical purely-adapter) ≡ refl
_ = refl

-- Perfect and algebraically closed fields

perfect : AFA.PerfectField F
perfect = record { baseField = F ; isPerfect = M.mkId "perfect" }

perfect-adapter : A.PerfectFieldAdapter
perfect-adapter = A.mkPerfectFieldAdapter F perfect (AFA.PerfectField.baseField perfect) refl

perfect-status : A.isFilledPerfectField perfect-adapter ≡ B.true
perfect-status = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.perfectFieldCategorical perfect-adapter) tt) ≡ A.PerfectFieldAdapter.decl perfect-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.perfectFieldCategorical perfect-adapter) ≡ refl
_ = refl

algClosed : AFA.AlgebraicallyClosedField F
algClosed = record { baseField = F ; isAlgebraicallyClosed = M.mkId "aclosed" }

algClosed-adapter : A.AlgebraicallyClosedFieldAdapter
algClosed-adapter = A.mkAlgebraicallyClosedFieldAdapter F algClosed (AFA.AlgebraicallyClosedField.baseField algClosed) refl

algClosed-status : A.isFilledAlgebraicallyClosedField algClosed-adapter ≡ B.true
algClosed-status = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.algebraicallyClosedFieldCategorical algClosed-adapter) tt) ≡ A.AlgebraicallyClosedFieldAdapter.decl algClosed-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.algebraicallyClosedFieldCategorical algClosed-adapter) ≡ refl
_ = refl

-- Closures and Frobenius

normalClosure : AFA.NormalClosure F E
normalClosure = record { baseField = F ; extensionField = E ; normalClosure = E ; isNormalClosure = M.mkId "normal" }

normalClosure-adapter : A.NormalClosureAdapter
normalClosure-adapter = A.mkNormalClosureAdapter F E normalClosure (AFA.NormalClosure.normalClosure normalClosure) refl

normalClosure-status : A.isFilledNormalClosure normalClosure-adapter ≡ B.true
normalClosure-status = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.normalClosureCategorical normalClosure-adapter) tt) ≡ A.NormalClosureAdapter.decl normalClosure-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.normalClosureCategorical normalClosure-adapter) ≡ refl
_ = refl

galoisClosure : AFA.GaloisClosure F E
galoisClosure = record { baseField = F ; extensionField = E ; galoisClosure = E ; isGaloisClosure = M.mkId "galois" }

galoisClosure-adapter : A.GaloisClosureAdapter
galoisClosure-adapter = A.mkGaloisClosureAdapter F E galoisClosure (AFA.GaloisClosure.galoisClosure galoisClosure) refl

galoisClosure-status : A.isFilledGaloisClosure galoisClosure-adapter ≡ B.true
galoisClosure-status = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.galoisClosureCategorical galoisClosure-adapter) tt) ≡ A.GaloisClosureAdapter.decl galoisClosure-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.galoisClosureCategorical galoisClosure-adapter) ≡ refl
_ = refl

frobenius : AFA.FrobeniusEndomorphism F
frobenius = record { baseField = F ; frobeniusMap = M.mkId "φ" ; isFrobenius = M.mkId "frobenius" }

frobenius-adapter : A.FrobeniusEndomorphismAdapter
frobenius-adapter = A.mkFrobeniusEndomorphismAdapter F frobenius (AFA.FrobeniusEndomorphism.baseField frobenius) refl

frobenius-status : A.isFilledFrobeniusEndomorphism frobenius-adapter ≡ B.true
frobenius-status = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.frobeniusEndomorphismCategorical frobenius-adapter) tt) ≡ A.FrobeniusEndomorphismAdapter.decl frobenius-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.frobeniusEndomorphismCategorical frobenius-adapter) ≡ refl
_ = refl

-- Function fields

ratFF : AFA.RationalFunctionField K
ratFF = record { baseField = K ; functionField = K ; isRationalFunctionField = M.mkId "ratff" }

ratFF-adapter : A.RationalFunctionFieldAdapter
ratFF-adapter = A.mkRationalFunctionFieldAdapter K ratFF (AFA.RationalFunctionField.functionField ratFF) refl

ratFF-status : A.isFilledRationalFunctionField ratFF-adapter ≡ B.true
ratFF-status = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.rationalFunctionFieldCategorical ratFF-adapter) tt) ≡ A.RationalFunctionFieldAdapter.decl ratFF-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.rationalFunctionFieldCategorical ratFF-adapter) ≡ refl
_ = refl

algFF : AFA.AlgebraicFunctionField K
algFF = record { baseField = K ; functionField = K ; isAlgebraicFunctionField = M.mkId "algff" }

algFF-adapter : A.AlgebraicFunctionFieldAdapter
algFF-adapter = A.mkAlgebraicFunctionFieldAdapter K algFF (AFA.AlgebraicFunctionField.functionField algFF) refl

algFF-status : A.isFilledAlgebraicFunctionField algFF-adapter ≡ B.true
algFF-status = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.algebraicFunctionFieldCategorical algFF-adapter) tt) ≡ A.AlgebraicFunctionFieldAdapter.decl algFF-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.algebraicFunctionFieldCategorical algFF-adapter) ≡ refl
_ = refl
