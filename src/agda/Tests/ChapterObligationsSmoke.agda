-- Tests/ChapterObligationsSmoke.agda
-- Leverage proof-obligation records from the chapters as smoke tests by
-- constructing trivial inhabitants with dummy witnesses.

module Tests.ChapterObligationsSmoke where

open import Agda.Builtin.Unit using (⊤; tt)
open import Core.Phase using (Bool; true; false)
open import Metamodel as M

-- Chapter 1 samples
import Chapter1.Level1sub4 as C1S4
import Chapter1.Level1sub2 as C1S2

-- Chapter 2 samples
import Chapter2.Level2sub1 as C2S1
import Chapter2.Level2sub2 as C2S2

-- Chapter 3 samples
import Chapter3.Level3sub2 as C3S2

------------------------------------------------------------------------
-- Chapter 1: simple obligations using ⊤ and Identifiers
------------------------------------------------------------------------

subLatticeCompleteSmoke : C1S4.SubobjectLatticeIsComplete
subLatticeCompleteSmoke = C1S4.THEOREM_SubobjectLatticeIsComplete tt

canonicalFSsmoke : C1S4.CanonicalFactorizationSystem
canonicalFSsmoke = C1S4.THEOREM_CanonicalFactorizationSystem tt

compEqSmoke : C1S2.CompletenessEquivalenceTheorem
compEqSmoke = C1S2.THEOREM_CompletenessEquivalenceTheorem (M.mkId "C")

------------------------------------------------------------------------
-- Chapter 2: obligations with Boolean flags and Identifiers
------------------------------------------------------------------------

addEqSmoke : C2S1.AdditivityEquivalenceTheorem
addEqSmoke = C2S1.THEOREM_AdditivityEquivalence (M.mkId "C") true true (M.mkId "iso")

regEpiPropSmoke : C2S2.RegularEpimorphismProperty
regEpiPropSmoke = C2S2._is_REGULAR_EPIMORPHISM (M.mkId "e") (M.mkId "A") (M.mkId "B")
                                                    (M.mkId "X") (M.mkId "f") (M.mkId "g") (M.mkId "coeq")

regEpiStrongSmoke : C2S2.RegularEpisAreStrongTheorem
regEpiStrongSmoke = C2S2.THEOREM_RegularEpisAreStrong (M.mkId "C") regEpiPropSmoke (M.mkId "w")

------------------------------------------------------------------------
-- Chapter 3: obligations requiring basic Set-shaped placeholders
------------------------------------------------------------------------

etaleSmoke : C3S2.EtaleSpaceOver
etaleSmoke = record
  { totalSpace = ⊤
  ; baseSpace  = ⊤
  ; projection = M.mkId "p"
  ; isLocalHomeomorphism = record
      { morphism = M.mkId "p"
      ; sourceSpace = ⊤
      ; targetSpace = ⊤
      ; localHomeomorphismCondition = ⊤
      }
  }
