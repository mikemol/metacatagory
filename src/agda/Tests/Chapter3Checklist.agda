-- Tests/Chapter3Checklist.agda
-- 1–2 trivial inhabitants per Level3subN module to broaden smoke coverage.

module Tests.Chapter3Checklist where

open import Agda.Builtin.Unit using (⊤; tt)
open import Metamodel as M
import Agda.Builtin.Bool as B
open import Agda.Builtin.Equality using (_≡_; refl)
open import Core.CategoricalAdapter
import Tests.ObligationAdapters as A

-- TODO: These are smoke placeholders for Chapter 3. Replace with constructed
--       witnesses as concrete topology/locale/sheaf examples land:
--       - Examples/* (topology, locales, sheaves)
--       - Core/* (structures and proof bridges)
--       - Chapter3-specific bridges (locale–frame duality, étale maps)

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
-- TODO(Ch3 §3.1): Replace with duality built from a concrete locale/frame pair.
chk3s1A = record { localeCategory = localesCat ; frameCategory = framesCat ; isOppositeCategory = ⊤ }

-- Adapter-based link and status for duality
dual-link : S1.LocaleFrameDualityTheorem.isOppositeCategory chk3s1A ≡ ⊤
dual-link = refl

dual-adapter : A.LocaleFrameDualityAdapter
dual-adapter = A.mkLocaleFrameDualityAdapter chk3s1A ⊤ dual-link

dual-status-is-filled : A.isFilledDuality dual-adapter ≡ B.true
dual-status-is-filled = refl
-- Categorical assertions for Locale–Frame Duality
_ : (CategoricalAdapter.morphism (A.localeFrameDualityCategorical dual-adapter) tt) ≡ A.LocaleFrameDualityAdapter.decl dual-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.localeFrameDualityCategorical dual-adapter) ≡ refl
_ = refl

------------------------------------------------------------------------
-- Level3sub2
------------------------------------------------------------------------

chk3s2A : S2.MorphismPropertyAssertionLocalHomeomorphism
-- TODO(Ch3 §3.2): Replace with a specific local homeomorphism between spaces.
chk3s2A = record { morphism = M.mkId "p" ; sourceSpace = ⊤ ; targetSpace = ⊤ ; localHomeomorphismCondition = ⊤ }

-- Adapter-based link and status
loc-link : S2.MorphismPropertyAssertionLocalHomeomorphism.morphism chk3s2A ≡ M.mkId "p"
loc-link = refl

loc-adapter : A.LocalHomeomorphismAdapter
loc-adapter = A.mkLocalHomeomorphismAdapter chk3s2A (M.mkId "p") loc-link

loc-status-is-filled : A.isFilledLocalHomeo loc-adapter ≡ B.true
loc-status-is-filled = refl
-- Categorical assertions for Local Homeomorphism
_ : (CategoricalAdapter.morphism (A.localHomeomorphismCategorical loc-adapter) tt) ≡ A.LocalHomeomorphismAdapter.decl loc-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.localHomeomorphismCategorical loc-adapter) ≡ refl
_ = refl

chk3s2B : S2.EtaleSpaceOver
-- TODO(Ch3 §3.2): Replace with an étale space built from a sheaf example.
chk3s2B = record
  { totalSpace = ⊤
  ; baseSpace  = ⊤
  ; projection = M.mkId "p"
  ; isLocalHomeomorphism = chk3s2A
  }

-- Adapter-based links and status
etal-proj-link : S2.EtaleSpaceOver.projection chk3s2B ≡ M.mkId "p"
etal-proj-link = refl

etal-loc-link : S2.EtaleSpaceOver.isLocalHomeomorphism chk3s2B ≡ chk3s2A
etal-loc-link = refl

etal-adapter : A.EtaleSpaceAdapter
etal-adapter = A.mkEtaleSpaceAdapter chk3s2B (M.mkId "p") chk3s2A etal-proj-link etal-loc-link

etal-status-is-filled : A.isFilledEtale etal-adapter ≡ B.true
etal-status-is-filled = refl
-- Categorical assertions for Etale Space
_ : (CategoricalAdapter.morphism (A.etaleSpaceCategorical etal-adapter) tt) ≡ A.EtaleSpaceAdapter.decl etal-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.etaleSpaceCategorical etal-adapter) ≡ refl
_ = refl

------------------------------------------------------------------------
-- New adapters for comprehensive Chapter 3 coverage
------------------------------------------------------------------------

-- HeytingAlgebra
heytingDecl : S1.HeytingAlgebraDeclaration
heytingDecl = record
  { underlyingLattice = ⊤
  ; implicationOperation = ⊤
  ; adjointnessAxiom = ⊤
  }

heyting-adapter : A.HeytingAlgebraAdapter
heyting-adapter = A.mkHeytingAlgebraAdapter heytingDecl

heyting-status : A.isFilledHeytingAlgebra heyting-adapter ≡ B.true
heyting-status = refl
-- Categorical assertions for Heyting Algebra
_ : (CategoricalAdapter.morphism (A.heytingAlgebraCategorical heyting-adapter) tt) ≡ A.HeytingAlgebraAdapter.decl heyting-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.heytingAlgebraCategorical heyting-adapter) ≡ refl
_ = refl

-- Frame
frameDecl : S1.FrameDeclaration
frameDecl = record
  { underlyingHeytingAlgebra = heytingDecl
  ; isCompleteLattice = ⊤
  ; frameStructure = ⊤
  }

frame-link : S1.FrameDeclaration.underlyingHeytingAlgebra frameDecl ≡ heytingDecl
frame-link = refl

frame-adapter : A.FrameAdapter
frame-adapter = A.mkFrameAdapter frameDecl heytingDecl frame-link

frame-status : A.isFilledFrame frame-adapter ≡ B.true
frame-status = refl
-- Categorical assertions for Frame
_ : (CategoricalAdapter.morphism (A.frameCategorical frame-adapter) tt) ≡ A.FrameAdapter.decl frame-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.frameCategorical frame-adapter) ≡ refl
_ = refl

-- Locale
localeDecl : S1.LocaleDeclaration
localeDecl = record
  { associatedFrame = frameDecl
  }

locale-link : S1.LocaleDeclaration.associatedFrame localeDecl ≡ frameDecl
locale-link = refl

locale-adapter : A.LocaleAdapter
locale-adapter = A.mkLocaleAdapter localeDecl frameDecl locale-link

locale-status : A.isFilledLocale locale-adapter ≡ B.true
locale-status = refl
-- Categorical assertions for Locale
_ : (CategoricalAdapter.morphism (A.localeCategorical locale-adapter) tt) ≡ A.LocaleAdapter.decl locale-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.localeCategorical locale-adapter) ≡ refl
_ = refl

-- LocaleMorphism
srcLocale : S1.LocaleDeclaration
srcLocale = localeDecl

tgtLocale : S1.LocaleDeclaration
tgtLocale = localeDecl

localeMorphDecl : S1.LocaleMorphismDeclaration
localeMorphDecl = record
  { sourceLocale = srcLocale
  ; targetLocale = tgtLocale
  ; representingFrameHomomorphism = ⊤
  }

locale-morph-src-link : S1.LocaleMorphismDeclaration.sourceLocale localeMorphDecl ≡ srcLocale
locale-morph-src-link = refl

locale-morph-tgt-link : S1.LocaleMorphismDeclaration.targetLocale localeMorphDecl ≡ tgtLocale
locale-morph-tgt-link = refl

locale-morph-adapter : A.LocaleMorphismAdapter
locale-morph-adapter = A.mkLocaleMorphismAdapter localeMorphDecl srcLocale tgtLocale
                                                  locale-morph-src-link locale-morph-tgt-link

locale-morph-status : A.isFilledLocaleMorphism locale-morph-adapter ≡ B.true
locale-morph-status = refl
-- Categorical assertions for Locale Morphism
_ : (CategoricalAdapter.morphism (A.localeMorphismCategorical locale-morph-adapter) tt) ≡ A.LocaleMorphismAdapter.decl locale-morph-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.localeMorphismCategorical locale-morph-adapter) ≡ refl
_ = refl

-- Nucleus
nucleusDecl : S1.NucleusDeclaration
nucleusDecl = record
  { frame = frameDecl
  ; nucleusMap = ⊤
  ; inflationaryAxiom = ⊤
  ; monotoneAxiom = ⊤
  ; idempotentAxiom = ⊤
  ; preservesMeetsAxiom = ⊤
  }

nucleus-link : S1.NucleusDeclaration.frame nucleusDecl ≡ frameDecl
nucleus-link = refl

nucleus-adapter : A.NucleusAdapter
nucleus-adapter = A.mkNucleusAdapter nucleusDecl frameDecl nucleus-link

nucleus-status : A.isFilledNucleus nucleus-adapter ≡ B.true
nucleus-status = refl
-- Categorical assertions for Nucleus
_ : (CategoricalAdapter.morphism (A.nucleusCategorical nucleus-adapter) tt) ≡ A.NucleusAdapter.decl nucleus-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.nucleusCategorical nucleus-adapter) ≡ refl
_ = refl

-- Sublocale
sublocDecl : S1.SublocaleDeclaration
sublocDecl = record
  { sublocale = localeDecl
  ; parentLocale = localeDecl
  ; isMonomorphism = ⊤
  }

subloc-sub-link : S1.SublocaleDeclaration.sublocale sublocDecl ≡ localeDecl
subloc-sub-link = refl

subloc-par-link : S1.SublocaleDeclaration.parentLocale sublocDecl ≡ localeDecl
subloc-par-link = refl

subloc-adapter : A.SublocaleAdapter
subloc-adapter = A.mkSublocaleAdapter sublocDecl localeDecl localeDecl
                                      subloc-sub-link subloc-par-link

subloc-status : A.isFilledSublocale subloc-adapter ≡ B.true
subloc-status = refl
-- Categorical assertions for Sublocale
_ : (CategoricalAdapter.morphism (A.sublocaleCategorical subloc-adapter) tt) ≡ A.SublocaleAdapter.decl subloc-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.sublocaleCategorical subloc-adapter) ≡ refl
_ = refl

-- OpenLocaleMorphism
openMorphDecl : S1.OpenLocaleMorphismDeclaration
openMorphDecl = record
  { localeMorphism = localeMorphDecl
  ; hasLeftAdjoint = ⊤
  ; leftAdjointPreservesMeets = record { functor = M.mkId "f" ; preservesTop = ⊤ ; preservesBinaryMeets = ⊤ }
  }

open-morph-link : S1.OpenLocaleMorphismDeclaration.localeMorphism openMorphDecl ≡ localeMorphDecl
open-morph-link = refl

open-morph-adapter : A.OpenLocaleMorphismAdapter
open-morph-adapter = A.mkOpenLocaleMorphismAdapter openMorphDecl localeMorphDecl open-morph-link

open-morph-status : A.isFilledOpenLocaleMorphism open-morph-adapter ≡ B.true
open-morph-status = refl
-- Categorical assertions for Open Locale Morphism
_ : (CategoricalAdapter.morphism (A.openLocaleMorphismCategorical open-morph-adapter) tt) ≡ A.OpenLocaleMorphismAdapter.decl open-morph-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.openLocaleMorphismCategorical open-morph-adapter) ≡ refl
_ = refl

-- SoberSpace
soberDecl : S1.SoberSpaceDeclaration
soberDecl = record
  { space = ⊤
  ; unitComponent = ⊤
  ; unitIsIsomorphism = ⊤
  }

sober-adapter : A.SoberSpaceAdapter
sober-adapter = A.mkSoberSpaceAdapter soberDecl

sober-status : A.isFilledSoberSpace sober-adapter ≡ B.true
sober-status = refl
-- Categorical assertions for Sober Space
_ : (CategoricalAdapter.morphism (A.soberSpaceCategorical sober-adapter) tt) ≡ A.SoberSpaceAdapter.decl sober-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.soberSpaceCategorical sober-adapter) ≡ refl
_ = refl

-- SpatialLocale
spatialDecl : S1.SpatialLocaleDeclaration
spatialDecl = record
  { locale = localeDecl
  ; counitComponent = ⊤
  ; counitIsIsomorphism = ⊤
  }

spatial-link : S1.SpatialLocaleDeclaration.locale spatialDecl ≡ localeDecl
spatial-link = refl

spatial-adapter : A.SpatialLocaleAdapter
spatial-adapter = A.mkSpatialLocaleAdapter spatialDecl localeDecl spatial-link

spatial-status : A.isFilledSpatialLocale spatial-adapter ≡ B.true
spatial-status = refl
-- Categorical assertions for Spatial Locale
_ : (CategoricalAdapter.morphism (A.spatialLocaleCategorical spatial-adapter) tt) ≡ A.SpatialLocaleAdapter.decl spatial-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.spatialLocaleCategorical spatial-adapter) ≡ refl
_ = refl

-- SheafOnLocale
presheafDecl : S2.PresheafOnLocale
presheafDecl = record
  { locale = ⊤
  ; underlyingFunctor = ⊤
  }

sheafDecl : S2.SheafOnLocaleDeclaration
sheafDecl = record
  { underlyingPresheaf = presheafDecl
  ; satisfiesGluingAxiom = record { presheaf = presheafDecl ; uniqueAmalgamationProperty = ⊤ }
  }

sheaf-link : S2.SheafOnLocaleDeclaration.underlyingPresheaf sheafDecl ≡ presheafDecl
sheaf-link = refl

sheaf-adapter : A.SheafOnLocaleAdapter
sheaf-adapter = A.mkSheafOnLocaleAdapter sheafDecl presheafDecl sheaf-link

sheaf-status : A.isFilledSheafOnLocale sheaf-adapter ≡ B.true
sheaf-status = refl
-- Categorical assertions for Sheaf on Locale
_ : (CategoricalAdapter.morphism (A.sheafOnLocaleCategorical sheaf-adapter) tt) ≡ A.SheafOnLocaleAdapter.decl sheaf-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.sheafOnLocaleCategorical sheaf-adapter) ≡ refl
_ = refl

-- GrothendieckTopos
toposDecl : S2.GrothendieckToposDeclaration
toposDecl = record
  { category = catDecl
  ; giraudAxiom1CocompletenessAndGenerators = ⊤
  ; giraudAxiom2DisjointCoproductsStableUnderPullback = ⊤
  ; giraudAxiom3EffectiveEquivalenceRelations = ⊤
  }

topos-link : S2.GrothendieckToposDeclaration.category toposDecl ≡ catDecl
topos-link = refl

topos-adapter : A.GrothendieckToposAdapter
topos-adapter = A.mkGrothendieckToposAdapter toposDecl catDecl topos-link

topos-status : A.isFilledGrothendieckTopos topos-adapter ≡ B.true
topos-status = refl
-- Categorical assertions for Grothendieck Topos
_ : (CategoricalAdapter.morphism (A.grothendieckToposCategorical topos-adapter) tt) ≡ A.GrothendieckToposAdapter.decl topos-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.grothendieckToposCategorical topos-adapter) ≡ refl
_ = refl

-- OmegaSet
omegaData : S2.OmegaSetData
omegaData = record
  { frame = ⊤
  ; elementsSet = ⊤
  ; equalityPredicate = ⊤
  }

omegaSetDecl : S2.OmegaSetDeclarationVerified
omegaSetDecl = record
  { dataOmegaSet = omegaData
  ; reflexivityProof = record { omegaSetData = omegaData ; reflexivityCondition = ⊤ }
  ; symmetryProof = record { omegaSetData = omegaData ; symmetryCondition = ⊤ }
  ; transitivityProof = record { omegaSetData = omegaData ; transitivityCondition = ⊤ }
  }

omega-link : S2.OmegaSetDeclarationVerified.dataOmegaSet omegaSetDecl ≡ omegaData
omega-link = refl

omega-adapter : A.OmegaSetAdapter
omega-adapter = A.mkOmegaSetAdapter omegaSetDecl omegaData omega-link

omega-status : A.isFilledOmegaSet omega-adapter ≡ B.true
omega-status = refl
-- Categorical assertions for Omega Set
_ : (CategoricalAdapter.morphism (A.omegaSetCategorical omega-adapter) tt) ≡ A.OmegaSetAdapter.decl omega-adapter
_ = refl
_ : CategoricalAdapter.isomorphism (A.omegaSetCategorical omega-adapter) ≡ refl
_ = refl

