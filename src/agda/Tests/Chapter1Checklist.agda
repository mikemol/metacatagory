-- Tests/Chapter1Checklist.agda
-- 1–2 trivial inhabitants per Level1subN module to broaden smoke coverage.

module Tests.Chapter1Checklist where

open import Agda.Builtin.Unit using (⊤; tt)
open import Agda.Builtin.Bool using (Bool; true; false)
open import Metamodel as M

-- Submodule imports
import Chapter1.Level1sub2 as S2
import Chapter1.Level1sub3 as S3
import Chapter1.Level1sub4 as S4
import Chapter1.Level1sub5 as S5
import Chapter1.Level1sub6 as S6
import Chapter1.Level1sub7 as S7
import Chapter1.Level1sub8 as S8

------------------------------------------------------------------------
-- Level1sub2
------------------------------------------------------------------------

chk1s2A : S2.CompletenessEquivalenceTheorem
chk1s2A = S2.THEOREM_CompletenessEquivalenceTheorem (M.mkId "C")

chk1s2B : S2.GeneralLimitConstructor
chk1s2B = S2.CONSTRUCT_LIMIT_from_ProductsAndEqualizers (M.mkId "D")

------------------------------------------------------------------------
-- Level1sub3
------------------------------------------------------------------------

chk1s3A : S3.AdjunctionHomDecl
chk1s3A = S3.ADJUNCTION_HOM_on (M.mkId "F") (M.mkId "G") (M.mkId "C") (M.mkId "D")

chk1s3B : S3.RightAdjointsPreserveLimits
chk1s3B = S3.THEOREM_RightAdjointsPreserveLimits (M.mkId "G")

------------------------------------------------------------------------
-- Level1sub4
------------------------------------------------------------------------

chk1s4A : S4.SubobjectLatticeIsComplete
chk1s4A = S4.THEOREM_SubobjectLatticeIsComplete tt

chk1s4B : S4.StrongMonomorphism
chk1s4B = S4._is_STRONG_MONOMORPHISM (M.mkId "m")

------------------------------------------------------------------------
-- Level1sub5
------------------------------------------------------------------------

chk1s5A : S5.ReflectiveSubcategoryAsLocalizationTheorem
chk1s5A = S5.THEOREM_ReflectiveSubcategoryAsLocalization (M.mkId "R") (M.mkId "C") (M.mkId "L")

chk1s5B : S5.FactorizationSystemPair
chk1s5B = S5.FactSys (M.mkId "E") (M.mkId "M")

------------------------------------------------------------------------
-- Level1sub6
------------------------------------------------------------------------

chk1s6A : S6.LeftExactnessViaKernels
chk1s6A = S6.THEOREM_LeftExactnessViaKernels (M.mkId "AbCat") (M.mkId "F")

chk1s6B : S6.RepresentableFunctor
chk1s6B = S6.y (M.mkId "X")

------------------------------------------------------------------------
-- Level1sub7
------------------------------------------------------------------------

chk1s7A : S7.LimitHierarchyTheorem
chk1s7A = S7.THEOREM_LimitHierarchy tt

chk1s7B : S7.BicategoryHomCategory
chk1s7B = S7.HOM_CATEGORY_Bicat (M.mkId "A") (M.mkId "B")

------------------------------------------------------------------------
-- Level1sub8
------------------------------------------------------------------------

internalCatData : S8.InternalCategoryData
internalCatData = S8.INTERNAL_CAT_DATA_consists_of
  (M.mkId "E") (M.mkId "C0") (M.mkId "C1") (M.mkId "d0") (M.mkId "d1") (M.mkId "i") (M.mkId "m")

internalCatAxioms : S8.InternalCategoryAxioms
internalCatAxioms = S8.INTERNAL_CATEGORY_AXIOMS (M.mkId "assoc") (M.mkId "left") (M.mkId "right")

chk1s8A : S8.InternalCategory
chk1s8A = S8.INTERNAL_CATEGORY_verified_by (M.mkId "C") internalCatData internalCatAxioms

chk1s8B : S8.InternalPresheaf
chk1s8B = S8.INTERNAL_PRESHEAF_on (M.mkId "F") chk1s8A (M.mkId "action")
