-- Tests/AlgebraChecklist.agda
-- Comprehensive coverage for Algebra subtree with concrete instances

module Tests.AlgebraChecklist where

open import Agda.Builtin.Unit using (⊤; tt)
open import Metamodel as M
import Agda.Builtin.Bool as B
open import Agda.Builtin.Equality using (_≡_; refl)
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
  }

magma-adapter : A.MagmaAdapter
magma-adapter = A.mkMagmaAdapter magmaDecl

magma-status : A.isFilledMagma magma-adapter ≡ B.true
magma-status = refl

-- Semigroup: Magma with associativity
assocAxiom : C1L.AssociativityAxiom
assocAxiom = record
  { over = M.mkId "∙"
  }

semigroupDecl : AF.SemigroupDeclaration
semigroupDecl = record
  { underlyingMagma = magmaDecl
  ; associativity = assocAxiom
  }

semigroup-link : AF.SemigroupDeclaration.underlyingMagma semigroupDecl ≡ magmaDecl
semigroup-link = refl

semigroup-adapter : A.SemigroupAdapter
semigroup-adapter = A.mkSemigroupAdapter semigroupDecl magmaDecl semigroup-link

semigroup-status : A.isFilledSemigroup semigroup-adapter ≡ B.true
semigroup-status = refl

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
  }

monoid-link : AF.MonoidDeclaration.underlyingSemigroup monoidDecl ≡ semigroupDecl
monoid-link = refl

monoid-adapter : A.MonoidAdapter
monoid-adapter = A.mkMonoidAdapter monoidDecl semigroupDecl monoid-link

monoid-status : A.isFilledMonoid monoid-adapter ≡ B.true
monoid-status = refl

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
  }

group-link : AF.GroupDeclaration.underlyingMonoid groupDecl ≡ monoidDecl
group-link = refl

group-adapter : A.GroupAdapter
group-adapter = A.mkGroupAdapter groupDecl monoidDecl group-link

group-status : A.isFilledGroup group-adapter ≡ B.true
group-status = refl

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
  }

abelian-link : AF.AbelianGroupDeclaration.underlyingGroup abelianGroupDecl ≡ groupDecl
abelian-link = refl

abelian-adapter : A.AbelianGroupAdapter
abelian-adapter = A.mkAbelianGroupAdapter abelianGroupDecl groupDecl abelian-link

abelian-status : A.isFilledAbelianGroup abelian-adapter ≡ B.true
abelian-status = refl

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

ring-status : A.isFilledRing ring-adapter ≡ B.true
ring-status = refl

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

unital-status : A.isFilledUnitalRing unital-adapter ≡ B.true
unital-status = refl

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

comm-ring-status : A.isFilledCommutativeRing comm-ring-adapter ≡ B.true
comm-ring-status = refl

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

div-ring-status : A.isFilledDivisionRing div-ring-adapter ≡ B.true
div-ring-status = refl

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

field-status : A.isFilledField field-adapter ≡ B.true
field-status = refl
