-- Tests/Chapter3Checklist.agda
-- 1–2 trivial inhabitants per Level3subN module to broaden smoke coverage.

module Tests.Chapter3Checklist where

open import Agda.Builtin.Unit using (⊤; tt)
open import Metamodel as M

-- Submodule imports
import Chapter3.Level3sub1 as S1
import Chapter3.Level3sub2 as S2
import Chapter1.Level1sub3 as C1S3

------------------------------------------------------------------------
-- Level3sub1
------------------------------------------------------------------------

catDecl : C1S3.CategoryDeclaration
catDecl = C1S3.CATEGORY (M.mkId "C")

framesCat : S1.CategoryOfFrames
framesCat = record { frames = ⊤ ; frameHomomorphisms = ⊤ ; categoryStructure = catDecl }

localesCat : S1.CategoryOfLocales
localesCat = record { locales = ⊤ ; localeMorphisms = ⊤ ; categoryStructure = catDecl }

chk3s1A : S1.LocaleFrameDualityTheorem
chk3s1A = record { localeCategory = localesCat ; frameCategory = framesCat ; isOppositeCategory = ⊤ }

------------------------------------------------------------------------
-- Level3sub2
------------------------------------------------------------------------

chk3s2A : S2.MorphismPropertyAssertionLocalHomeomorphism
chk3s2A = record { morphism = M.mkId "p" ; sourceSpace = ⊤ ; targetSpace = ⊤ ; localHomeomorphismCondition = ⊤ }

chk3s2B : S2.EtaleSpaceOver
chk3s2B = record
  { totalSpace = ⊤
  ; baseSpace  = ⊤
  ; projection = M.mkId "p"
  ; isLocalHomeomorphism = chk3s2A
  }
