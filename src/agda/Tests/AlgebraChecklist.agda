{-# OPTIONS --without-K #-}

-- Tests/AlgebraChecklist.agda
-- Comprehensive coverage for Algebra subtree with concrete instances

module Tests.AlgebraChecklist where

open import Agda.Builtin.Unit using (⊤; tt)
open import Metamodel as M
open import Core.Phase using (Bool; true; false)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Core.CategoricalAdapter
import Tests.ObligationAdapters as A

-- Algebra imports
import Algebra.Foundation as AF
import Algebra.Rings.Basic as AR

-- Chapter 1 imports for axioms
import Chapter1.Level1 as C1L

------------------------------------------------------------------------
-- Algebra.Foundation: Basic algebraic hierarchy
------------------------------------------------------------------------

-- Magma: Set with binary operation (no axioms)
magmaDecl : AF.MagmaDeclaration
magmaDecl = record
  { underlyingSet = M.mkId "M"
  ; binaryOp = M.mkId "∙"
  ; index = AF.magmaIndex
  }

magma-adapter : A.MagmaAdapter
magma-adapter = A.mkMagmaAdapter magmaDecl

magma-status : A.isFilledMagma magma-adapter ≡ true
magma-status = refl
-- Categorical assertions for Magma
_ : (CategoricalAdapter.morphism (A.magmaCategorical magma-adapter) tt) ≡ A.MagmaAdapter.decl magma-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.magmaCategorical magma-adapter) ≡ refl
_ = refl

-- Semigroup: Magma with associativity
assocAxiom : C1L.AssociativityAxiom
assocAxiom = record
  { over = M.mkId "∙"
  }

semigroupDecl : AF.SemigroupDeclaration
semigroupDecl = record
  { underlyingMagma = magmaDecl
  ; associativity = assocAxiom
  ; index = AF.semigroupIndex
  }

semigroup-link : AF.SemigroupDeclaration.underlyingMagma semigroupDecl ≡ magmaDecl
semigroup-link = refl

semigroup-adapter : A.SemigroupAdapter
semigroup-adapter = A.mkSemigroupAdapter semigroupDecl magmaDecl semigroup-link

semigroup-status : A.isFilledSemigroup semigroup-adapter ≡ true
semigroup-status = refl
-- Categorical assertions for Semigroup
_ : (CategoricalAdapter.morphism (A.semigroupCategorical semigroup-adapter) tt) ≡ A.SemigroupAdapter.decl semigroup-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.semigroupCategorical semigroup-adapter) ≡ refl
_ = refl

-- Monoid: Semigroup with identity
identityAxiom : C1L.IdentityAxiom
identityAxiom = record
  { over = M.mkId "e"
  }

monoidDecl : AF.MonoidDeclaration
monoidDecl = record
  { underlyingSemigroup = semigroupDecl
  ; identityElement = M.mkId "e"
  ; identityAxiom = identityAxiom
  ; index = AF.monoidIndex
  }

monoid-link : AF.MonoidDeclaration.underlyingSemigroup monoidDecl ≡ semigroupDecl
monoid-link = refl

monoid-adapter : A.MonoidAdapter
monoid-adapter = A.mkMonoidAdapter monoidDecl semigroupDecl monoid-link

monoid-status : A.isFilledMonoid monoid-adapter ≡ true
monoid-status = refl
-- Categorical assertions for Monoid
_ : (CategoricalAdapter.morphism (A.monoidCategorical monoid-adapter) tt) ≡ A.MonoidAdapter.decl monoid-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.monoidCategorical monoid-adapter) ≡ refl
_ = refl

-- Group: Monoid with inverses
inverseOp : AF.InverseOperation
inverseOp = record
  { forMonoid = monoidDecl
  ; inverseMap = M.mkId "inv"
  ; inverseAxiom = M.mkId "inverse-proof"
  }

groupDecl : AF.GroupDeclaration
groupDecl = record
  { underlyingMonoid = monoidDecl
  ; inverseOperation = inverseOp
  ; index = AF.groupIndex
  }

group-link : AF.GroupDeclaration.underlyingMonoid groupDecl ≡ monoidDecl
group-link = refl

group-adapter : A.GroupAdapter
group-adapter = A.mkGroupAdapter groupDecl monoidDecl group-link

group-status : A.isFilledGroup group-adapter ≡ true
group-status = refl
-- Categorical assertions for Group
_ : (CategoricalAdapter.morphism (A.groupCategorical group-adapter) tt) ≡ A.GroupAdapter.decl group-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.groupCategorical group-adapter) ≡ refl
_ = refl

-- AbelianGroup: Commutative group
commutAxiom : AF.CommutativityAxiom
commutAxiom = record
  { forGroup = groupDecl
  ; axiom = M.mkId "commutative-proof"
  }

abelianGroupDecl : AF.AbelianGroupDeclaration
abelianGroupDecl = record
  { underlyingGroup = groupDecl
  ; commutativity = commutAxiom
  ; index = AF.abelianGroupIndex
  }

abelian-link : AF.AbelianGroupDeclaration.underlyingGroup abelianGroupDecl ≡ groupDecl
abelian-link = refl

abelian-adapter : A.AbelianGroupAdapter
abelian-adapter = A.mkAbelianGroupAdapter abelianGroupDecl groupDecl abelian-link

abelian-status : A.isFilledAbelianGroup abelian-adapter ≡ true
abelian-status = refl
-- Categorical assertions for Abelian Group
_ : (CategoricalAdapter.morphism (A.abelianGroupCategorical abelian-adapter) tt) ≡ A.AbelianGroupAdapter.decl abelian-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.abelianGroupCategorical abelian-adapter) ≡ refl
_ = refl

------------------------------------------------------------------------
-- Algebra.Rings.Basic: Ring hierarchy
------------------------------------------------------------------------

-- Ring: Abelian group with multiplication and distributivity
ringDecl : AR.RingDeclaration
ringDecl = record
  { identifier = M.mkId "R"
  ; additiveGroup = abelianGroupDecl
  ; multiplication = M.mkId "·"
  ; multAssociative = M.mkId "mult-assoc"
  ; leftDistributive = M.mkId "left-dist"
  ; rightDistributive = M.mkId "right-dist"
  }

ring-link : AR.RingDeclaration.additiveGroup ringDecl ≡ abelianGroupDecl
ring-link = refl

ring-adapter : A.RingAdapter
ring-adapter = A.mkRingAdapter ringDecl abelianGroupDecl ring-link

ring-status : A.isFilledRing ring-adapter ≡ true
ring-status = refl
-- Categorical assertions for Ring
_ : (CategoricalAdapter.morphism (A.ringCategorical ring-adapter) tt) ≡ A.RingAdapter.decl ring-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.ringCategorical ring-adapter) ≡ refl
_ = refl

-- UnitalRing: Ring with multiplicative identity
unitalRingDecl : AR.UnitalRingDeclaration
unitalRingDecl = record
  { underlyingRing = ringDecl
  ; multiplicativeIdentity = M.mkId "1"
  ; leftIdentity = M.mkId "1·a=a"
  ; rightIdentity = M.mkId "a·1=a"
  }

unital-link : AR.UnitalRingDeclaration.underlyingRing unitalRingDecl ≡ ringDecl
unital-link = refl

unital-adapter : A.UnitalRingAdapter
unital-adapter = A.mkUnitalRingAdapter unitalRingDecl ringDecl unital-link

unital-status : A.isFilledUnitalRing unital-adapter ≡ true
unital-status = refl
-- Categorical assertions for Unital Ring
_ : (CategoricalAdapter.morphism (A.unitalRingCategorical unital-adapter) tt) ≡ A.UnitalRingAdapter.decl unital-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.unitalRingCategorical unital-adapter) ≡ refl
_ = refl

-- CommutativeRing: Unital ring with ab = ba
commRingDecl : AR.CommutativeRingDeclaration
commRingDecl = record
  { underlyingRing = unitalRingDecl
  ; commutativity = M.mkId "ab=ba"
  }

comm-ring-link : AR.CommutativeRingDeclaration.underlyingRing commRingDecl ≡ unitalRingDecl
comm-ring-link = refl

comm-ring-adapter : A.CommutativeRingAdapter
comm-ring-adapter = A.mkCommutativeRingAdapter commRingDecl unitalRingDecl comm-ring-link

comm-ring-status : A.isFilledCommutativeRing comm-ring-adapter ≡ true
comm-ring-status = refl
-- Categorical assertions for Commutative Ring
_ : (CategoricalAdapter.morphism (A.commutativeRingCategorical comm-ring-adapter) tt) ≡ A.CommutativeRingAdapter.decl comm-ring-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.commutativeRingCategorical comm-ring-adapter) ≡ refl
_ = refl

-- DivisionRing: Unital ring with multiplicative inverses (skew field)
divRingDecl : AR.DivisionRingDeclaration
divRingDecl = record
  { underlyingRing = unitalRingDecl
  ; inverses = M.mkId "mult-inverses"
  }

div-ring-link : AR.DivisionRingDeclaration.underlyingRing divRingDecl ≡ unitalRingDecl
div-ring-link = refl

div-ring-adapter : A.DivisionRingAdapter
div-ring-adapter = A.mkDivisionRingAdapter divRingDecl unitalRingDecl div-ring-link

div-ring-status : A.isFilledDivisionRing div-ring-adapter ≡ true
div-ring-status = refl
-- Categorical assertions for Division Ring
_ : (CategoricalAdapter.morphism (A.divisionRingCategorical div-ring-adapter) tt) ≡ A.DivisionRingAdapter.decl div-ring-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.divisionRingCategorical div-ring-adapter) ≡ refl
_ = refl

-- Field: Commutative division ring
fieldDecl : AR.FieldDeclaration
fieldDecl = record
  { underlyingRing = commRingDecl
  ; inverses = M.mkId "field-inverses"
  }

field-link : AR.FieldDeclaration.underlyingRing fieldDecl ≡ commRingDecl
field-link = refl

field-adapter : A.FieldAdapter
field-adapter = A.mkFieldAdapter fieldDecl commRingDecl field-link

field-status : A.isFilledField field-adapter ≡ true
field-status = refl
-- Categorical assertions for Field
_ : (CategoricalAdapter.morphism (A.fieldCategorical field-adapter) tt) ≡ A.FieldAdapter.decl field-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.fieldCategorical field-adapter) ≡ refl
_ = refl
