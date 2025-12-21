{-# OPTIONS --without-K #-}

-- Tests.GroupsStructureChecklist: Coverage for Algebra.Groups.Structure (Group Structure Theory)

module Tests.GroupsStructureChecklist where

open import Core.Phase using (Bool; true; false)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Agda.Builtin.Unit using (⊤; tt)
open import Metamodel as M
open import Core.CategoricalAdapter

-- Imports
import Chapter1.Level1 as C1L
import Algebra.Foundation as AF
import Algebra.Groups.Free as AGF
import Algebra.Groups.Structure as AGS
import Algebra.Groups.Basic as AGB
import Tests.ObligationAdapters as A

-- Build minimal algebra scaffolding
magmaDecl : AF.MagmaDeclaration
magmaDecl = record { underlyingSet = M.mkId "structCarrier" ; binaryOp = M.mkId "structOp" ; index = AF.magmaIndex }

semigroupDecl : AF.SemigroupDeclaration
semigroupDecl = record
  { underlyingMagma = magmaDecl
  ; associativity = C1L.AXIOM_Associativity (M.mkId "structAssoc")
  ; index = AF.semigroupIndex
  }

monoidDecl : AF.MonoidDeclaration
monoidDecl = record
  { underlyingSemigroup = semigroupDecl
  ; identityElement = M.mkId "structId"
  ; identityAxiom = C1L.AXIOM_Identity (M.mkId "structIdAx")
  ; index = AF.monoidIndex
  }

groupDecl : AF.GroupDeclaration
groupDecl = record
  { underlyingMonoid = monoidDecl
  ; inverseOperation = record
    { forMonoid = monoidDecl
    ; inverseMap = M.mkId "structInv"
    ; inverseAxiom = M.mkId "structInvAx"
    }
  ; index = AF.groupIndex
  }

abelianGroupDecl : AF.AbelianGroupDeclaration
abelianGroupDecl = record
  { underlyingGroup = groupDecl
  ; commutativity = record
    { forGroup = groupDecl
    ; axiom = M.mkId "structComm"
    }
  ; index = AF.abelianGroupIndex
  }

fgAbelianGroupDecl : AGF.FinitelyGeneratedAbelianGroup
fgAbelianGroupDecl = record
  { underlyingGroup = abelianGroupDecl
  ; generators = M.mkId "fgGens"
  ; finitelyGenerated = M.mkId "isFG"
  }

G : AF.GroupDeclaration
G = groupDecl

X : M.Identifier
X = M.mkId "set"

p : M.Identifier
p = M.mkId "prime"

x : M.Identifier
x = M.mkId "element"

groupActionDecl : AGS.GroupAction G X
groupActionDecl = record
  { group = G
  ; set = X
  ; action = M.mkId "actionMap"
  ; identityAction = M.mkId "actionId"
  ; compatibilityAction = M.mkId "actionCompat"
  }

-- Invariant factor decomposition
invFactDecl : AGS.InvariantFactorDecomposition fgAbelianGroupDecl
invFactDecl = record
  { freeRank = M.mkId "freeRank"
  ; invariantFactors = M.mkId "invFactors"
  ; isomorphism = M.mkId "invFactIso"
  }

invFactAdapt : A.InvariantFactorDecompositionAdapter
invFactAdapt = A.mkInvariantFactorDecompositionAdapter fgAbelianGroupDecl invFactDecl (M.mkId "freeRank") refl

invFactStatus : A.isFilledInvariantFactorDecomposition invFactAdapt ≡ true
invFactStatus = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.invariantFactorDecompositionCategorical invFactAdapt) tt) ≡ A.InvariantFactorDecompositionAdapter.decl invFactAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.invariantFactorDecompositionCategorical invFactAdapt) ≡ refl
_ = refl

-- Torsion subgroup
torsionDecl : AGS.TorsionSubgroup abelianGroupDecl
torsionDecl = record
  { abelianGroup = abelianGroupDecl
  ; torsionElements = M.mkId "torsionElems"
  ; isSubgroup = record
    { subset = M.mkId "torsionSubset"
    ; inclusion = M.mkId "torsionIncl"
    ; closedUnderOp = M.mkId "torsionClosed"
    ; containsIdentity = M.mkId "torsionId"
    ; closedUnderInverse = M.mkId "torsionInv"
    }
  }

torsionAdapt : A.TorsionSubgroupAdapter
torsionAdapt = A.mkTorsionSubgroupAdapter abelianGroupDecl torsionDecl abelianGroupDecl refl

torsionStatus : A.isFilledTorsionSubgroup torsionAdapt ≡ true
torsionStatus = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.torsionSubgroupCategorical torsionAdapt) tt) ≡ A.TorsionSubgroupAdapter.decl torsionAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.torsionSubgroupCategorical torsionAdapt) ≡ refl
_ = refl

-- Group action
groupActionAdapt : A.GroupActionAdapter
groupActionAdapt = A.mkGroupActionAdapter G X groupActionDecl G refl

groupActionStatus : A.isFilledGroupAction groupActionAdapt ≡ true
groupActionStatus = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.groupActionCategorical groupActionAdapt) tt) ≡ A.GroupActionAdapter.decl groupActionAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.groupActionCategorical groupActionAdapt) ≡ refl
_ = refl

-- Orbit
orbitDecl : AGS.Orbit G X groupActionDecl x
orbitDecl = record
  { groupAction = groupActionDecl
  ; element = x
  ; orbitSet = M.mkId "orbitSet"
  }

orbitAdapt : A.OrbitAdapter
orbitAdapt = A.mkOrbitAdapter G X groupActionDecl x orbitDecl groupActionDecl refl

orbitStatus : A.isFilledOrbit orbitAdapt ≡ true
orbitStatus = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.orbitCategorical orbitAdapt) tt) ≡ A.OrbitAdapter.decl orbitAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.orbitCategorical orbitAdapt) ≡ refl
_ = refl

-- Stabilizer
stabilizerDecl : AGS.Stabilizer G X groupActionDecl x
stabilizerDecl = record
  { groupAction = groupActionDecl
  ; element = x
  ; stabilizer = record
    { subset = M.mkId "stabSubset"
    ; inclusion = M.mkId "stabIncl"
    ; closedUnderOp = M.mkId "stabClosed"
    ; containsIdentity = M.mkId "stabId"
    ; closedUnderInverse = M.mkId "stabInv"
    }
  }

stabilizerAdapt : A.StabilizerAdapter
stabilizerAdapt = A.mkStabilizerAdapter G X groupActionDecl x stabilizerDecl groupActionDecl refl

stabilizerStatus : A.isFilledStabilizer stabilizerAdapt ≡ true
stabilizerStatus = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.stabilizerCategorical stabilizerAdapt) tt) ≡ A.StabilizerAdapter.decl stabilizerAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.stabilizerCategorical stabilizerAdapt) ≡ refl
_ = refl

-- P-group
pGroupDecl : AGS.PGroup p G
pGroupDecl = record
  { prime = p
  ; group = G
  ; isPGroup = M.mkId "isPGroup"
  }

pGroupAdapt : A.PGroupAdapter
pGroupAdapt = A.mkPGroupAdapter p G pGroupDecl p refl

pGroupStatus : A.isFilledPGroup pGroupAdapt ≡ true
pGroupStatus = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.pGroupCategorical pGroupAdapt) tt) ≡ A.PGroupAdapter.decl pGroupAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.pGroupCategorical pGroupAdapt) ≡ refl
_ = refl

-- Sylow p-subgroup
sylowDecl : AGS.SylowPSubgroup p G
sylowDecl = record
  { prime = p
  ; group = G
  ; subgroup = record
    { subset = M.mkId "sylowSubset"
    ; inclusion = M.mkId "sylowIncl"
    ; closedUnderOp = M.mkId "sylowClosed"
    ; containsIdentity = M.mkId "sylowId"
    ; closedUnderInverse = M.mkId "sylowInv"
    }
  ; subgroupAsGroup = groupDecl
  ; isPGroup = pGroupDecl
  ; isMaximal = M.mkId "sylowMaximal"
  }

sylowAdapt : A.SylowPSubgroupAdapter
sylowAdapt = A.mkSylowPSubgroupAdapter p G sylowDecl p refl

sylowStatus : A.isFilledSylowPSubgroup sylowAdapt ≡ true
sylowStatus = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.sylowPSubgroupCategorical sylowAdapt) tt) ≡ A.SylowPSubgroupAdapter.decl sylowAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.sylowPSubgroupCategorical sylowAdapt) ≡ refl
_ = refl

-- Simple group
simpleGroupDecl : AGS.SimpleGroup G
simpleGroupDecl = record
  { group = G
  ; isSimple = M.mkId "isSimple"
  }

simpleGroupAdapt : A.SimpleGroupAdapter
simpleGroupAdapt = A.mkSimpleGroupAdapter G simpleGroupDecl G refl

simpleGroupStatus : A.isFilledSimpleGroup simpleGroupAdapt ≡ true
simpleGroupStatus = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.simpleGroupCategorical simpleGroupAdapt) tt) ≡ A.SimpleGroupAdapter.decl simpleGroupAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.simpleGroupCategorical simpleGroupAdapt) ≡ refl
_ = refl

-- Composition series
compositionSeriesDecl : AGS.CompositionSeries G
compositionSeriesDecl = record
  { group = G
  ; series = M.mkId "compSeries"
  ; factorsAreSimple = M.mkId "compSimple"
  }

compositionSeriesAdapt : A.CompositionSeriesAdapter
compositionSeriesAdapt = A.mkCompositionSeriesAdapter G compositionSeriesDecl G refl

compositionSeriesStatus : A.isFilledCompositionSeries compositionSeriesAdapt ≡ true
compositionSeriesStatus = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.compositionSeriesCategorical compositionSeriesAdapt) tt) ≡ A.CompositionSeriesAdapter.decl compositionSeriesAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.compositionSeriesCategorical compositionSeriesAdapt) ≡ refl
_ = refl

-- Solvable group
solvableGroupDecl : AGS.SolvableGroup G
solvableGroupDecl = record
  { group = G
  ; derivedSeries = record
    { group = G
    ; series = M.mkId "derivedSeries"
    }
  ; isSolvable = M.mkId "isSolvable"
  }

solvableGroupAdapt : A.SolvableGroupAdapter
solvableGroupAdapt = A.mkSolvableGroupAdapter G solvableGroupDecl G refl

solvableGroupStatus : A.isFilledSolvableGroup solvableGroupAdapt ≡ true
solvableGroupStatus = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.solvableGroupCategorical solvableGroupAdapt) tt) ≡ A.SolvableGroupAdapter.decl solvableGroupAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.solvableGroupCategorical solvableGroupAdapt) ≡ refl
_ = refl

-- Nilpotent group
nilpotentGroupDecl : AGS.NilpotentGroup G
nilpotentGroupDecl = record
  { group = G
  ; lowerCentralSeries = record
    { group = G
    ; series = M.mkId "lowerCentralSeries"
    }
  ; isNilpotent = M.mkId "isNilpotent"
  }

nilpotentGroupAdapt : A.NilpotentGroupAdapter
nilpotentGroupAdapt = A.mkNilpotentGroupAdapter G nilpotentGroupDecl G refl

nilpotentGroupStatus : A.isFilledNilpotentGroup nilpotentGroupAdapt ≡ true
nilpotentGroupStatus = refl

-- Categorical assertions
_ : (CategoricalAdapter.morphism (A.nilpotentGroupCategorical nilpotentGroupAdapt) tt) ≡ A.NilpotentGroupAdapter.decl nilpotentGroupAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.nilpotentGroupCategorical nilpotentGroupAdapt) ≡ refl
_ = refl
