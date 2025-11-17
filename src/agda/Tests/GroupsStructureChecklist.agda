-- Tests.GroupsStructureChecklist: Coverage for Algebra.Groups.Structure (Group Structure Theory)

module Tests.GroupsStructureChecklist where

import Agda.Builtin.Bool as B
open import Agda.Builtin.Equality using (_≡_; refl)
open import Metamodel as M

-- Imports
import Chapter1.Level1 as C1L
import Algebra.Foundation as AF
import Algebra.Groups.Free as AGF
import Algebra.Groups.Structure as AGS
import Algebra.Groups.Basic as AGB
import Tests.ObligationAdapters as A

-- Build minimal algebra scaffolding
magmaDecl : AF.MagmaDeclaration
magmaDecl = record { binaryOp = M.mkId "structOp" }

semigroupDecl : AF.SemigroupDeclaration
semigroupDecl = record
  { underlyingMagma = magmaDecl
  ; associativity = M.mkId "structAssoc"
  }

monoidDecl : AF.MonoidDeclaration
monoidDecl = record
  { underlyingSemigroup = semigroupDecl
  ; identityElement = record
    { forSemigroup = semigroupDecl
    ; element = M.mkId "structId"
    ; leftIdentity = M.mkId "structLId"
    ; rightIdentity = M.mkId "structRId"
    }
  }

groupDecl : AF.GroupDeclaration
groupDecl = record
  { underlyingMonoid = monoidDecl
  ; inverseOperation = record
    { forMonoid = monoidDecl
    ; operation = M.mkId "structInv"
    ; inverseAxiom = M.mkId "structInvAx"
    }
  }

abelianGroupDecl : AF.AbelianGroupDeclaration
abelianGroupDecl = record
  { underlyingGroup = groupDecl
  ; commutativity = record
    { forGroup = groupDecl
    ; axiom = M.mkId "structComm"
    }
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

invFactStatus : A.isFilledInvariantFactorDecomposition invFactAdapt ≡ B.true
invFactStatus = refl

-- Torsion subgroup
torsionDecl : AGS.TorsionSubgroup abelianGroupDecl
torsionDecl = record
  { abelianGroup = abelianGroupDecl
  ; torsionElements = M.mkId "torsionElems"
  ; isSubgroup = record
    { group = AF.AbelianGroupDeclaration.underlyingGroup abelianGroupDecl
    ; subset = M.mkId "torsionSubset"
    ; closedUnderOp = M.mkId "torsionClosed"
    ; containsIdentity = M.mkId "torsionId"
    ; closedUnderInverse = M.mkId "torsionInv"
    }
  }

torsionAdapt : A.TorsionSubgroupAdapter
torsionAdapt = A.mkTorsionSubgroupAdapter abelianGroupDecl torsionDecl abelianGroupDecl refl

torsionStatus : A.isFilledTorsionSubgroup torsionAdapt ≡ B.true
torsionStatus = refl

-- Group action
groupActionAdapt : A.GroupActionAdapter
groupActionAdapt = A.mkGroupActionAdapter G X groupActionDecl G refl

groupActionStatus : A.isFilledGroupAction groupActionAdapt ≡ B.true
groupActionStatus = refl

-- Orbit
orbitDecl : AGS.Orbit G X groupActionDecl x
orbitDecl = record
  { groupAction = groupActionDecl
  ; element = x
  ; orbitSet = M.mkId "orbitSet"
  }

orbitAdapt : A.OrbitAdapter
orbitAdapt = A.mkOrbitAdapter G X groupActionDecl x orbitDecl groupActionDecl refl

orbitStatus : A.isFilledOrbit orbitAdapt ≡ B.true
orbitStatus = refl

-- Stabilizer
stabilizerDecl : AGS.Stabilizer G X groupActionDecl x
stabilizerDecl = record
  { groupAction = groupActionDecl
  ; element = x
  ; stabilizer = record
    { group = G
    ; subset = M.mkId "stabSubset"
    ; closedUnderOp = M.mkId "stabClosed"
    ; containsIdentity = M.mkId "stabId"
    ; closedUnderInverse = M.mkId "stabInv"
    }
  }

stabilizerAdapt : A.StabilizerAdapter
stabilizerAdapt = A.mkStabilizerAdapter G X groupActionDecl x stabilizerDecl groupActionDecl refl

stabilizerStatus : A.isFilledStabilizer stabilizerAdapt ≡ B.true
stabilizerStatus = refl

-- P-group
pGroupDecl : AGS.PGroup p G
pGroupDecl = record
  { prime = p
  ; group = G
  ; isPGroup = M.mkId "isPGroup"
  }

pGroupAdapt : A.PGroupAdapter
pGroupAdapt = A.mkPGroupAdapter p G pGroupDecl p refl

pGroupStatus : A.isFilledPGroup pGroupAdapt ≡ B.true
pGroupStatus = refl

-- Sylow p-subgroup
sylowDecl : AGS.SylowPSubgroup p G
sylowDecl = record
  { prime = p
  ; group = G
  ; subgroup = record
    { group = G
    ; subset = M.mkId "sylowSubset"
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

sylowStatus : A.isFilledSylowPSubgroup sylowAdapt ≡ B.true
sylowStatus = refl

-- Simple group
simpleGroupDecl : AGS.SimpleGroup G
simpleGroupDecl = record
  { group = G
  ; isSimple = M.mkId "isSimple"
  }

simpleGroupAdapt : A.SimpleGroupAdapter
simpleGroupAdapt = A.mkSimpleGroupAdapter G simpleGroupDecl G refl

simpleGroupStatus : A.isFilledSimpleGroup simpleGroupAdapt ≡ B.true
simpleGroupStatus = refl

-- Composition series
compositionSeriesDecl : AGS.CompositionSeries G
compositionSeriesDecl = record
  { group = G
  ; series = M.mkId "compSeries"
  ; factorsAreSimple = M.mkId "compSimple"
  }

compositionSeriesAdapt : A.CompositionSeriesAdapter
compositionSeriesAdapt = A.mkCompositionSeriesAdapter G compositionSeriesDecl G refl

compositionSeriesStatus : A.isFilledCompositionSeries compositionSeriesAdapt ≡ B.true
compositionSeriesStatus = refl

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

solvableGroupStatus : A.isFilledSolvableGroup solvableGroupAdapt ≡ B.true
solvableGroupStatus = refl

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

nilpotentGroupStatus : A.isFilledNilpotentGroup nilpotentGroupAdapt ≡ B.true
nilpotentGroupStatus = refl
