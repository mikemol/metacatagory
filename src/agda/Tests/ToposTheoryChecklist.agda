-- Tests.ToposTheoryChecklist: Test instances for topos theory concepts

module Tests.ToposTheoryChecklist where

open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.Equality using (refl; _≡_)
open import Agda.Builtin.Unit using (⊤; tt)
open import Tests.ToposObligationAdapters as A
open import Metamodel as M
import Chapter3.Level3sub2 as S2
import Chapter1.Level1sub3 as C1S3

------------------------------------------------------------------------
-- Test setup: Common definitions
------------------------------------------------------------------------

-- Dummy category for tests
catDecl : C1S3.CategoryDeclaration
catDecl = C1S3.CATEGORY (M.mkId "ToposTestCat")

-- Dummy frame identifier
frameId : M.Identifier
frameId = M.mkId "TestFrame"

-- Dummy locale
dummyLocale : Set
dummyLocale = ⊤

------------------------------------------------------------------------
-- Presheaves and Sheaves
------------------------------------------------------------------------

-- PresheafOnLocale
presheafDecl : S2.PresheafOnLocale
presheafDecl = record { locale = dummyLocale ; underlyingFunctor = ⊤ }

presheafAdapt : A.PresheafOnLocaleAdapter
presheafAdapt = A.mkPresheafOnLocaleAdapter presheafDecl (λ _ → presheafDecl)

_ : A.isFilledPresheafOnLocale presheafAdapt ≡ true
_ = refl

-- SheafGluingAxiom
gluingAxiomDecl : S2.SheafGluingAxiom
gluingAxiomDecl = record
  { presheaf = presheafDecl
  ; uniqueAmalgamationProperty = ⊤
  }

gluingLink : S2.SheafGluingAxiom.presheaf gluingAxiomDecl ≡ presheafDecl
gluingLink = refl

gluingAdapt : A.SheafGluingAxiomAdapter
gluingAdapt = A.mkSheafGluingAxiomAdapter gluingAxiomDecl presheafDecl gluingLink

_ : A.isFilledSheafGluingAxiom gluingAdapt ≡ true
_ = refl

-- SheafOnLocaleDeclaration (already tested elsewhere, but we include for completeness)
sheafDecl : S2.SheafOnLocaleDeclaration
sheafDecl = record
  { underlyingPresheaf = presheafDecl
  ; satisfiesGluingAxiom = gluingAxiomDecl
  }

sheafLink : S2.SheafOnLocaleDeclaration.underlyingPresheaf sheafDecl ≡ presheafDecl
sheafLink = refl

sheafAdapt : A.SheafOnLocaleAdapter
sheafAdapt = A.mkSheafOnLocaleAdapter sheafDecl presheafDecl sheafLink

_ : A.isFilledSheafOnLocale sheafAdapt ≡ true
_ = refl

------------------------------------------------------------------------
-- Category of Sheaves and Toposes
------------------------------------------------------------------------

-- CategoryOfSheaves
sheafCatDecl : S2.CategoryOfSheaves
sheafCatDecl = record
  { locale = dummyLocale
  ; underlyingCategory = catDecl
  ; sheaves = ⊤
  ; naturalTransformations = ⊤
  }

sheafCatLink : S2.CategoryOfSheaves.underlyingCategory sheafCatDecl ≡ catDecl
sheafCatLink = refl

sheafCatAdapt : A.CategoryOfSheavesAdapter
sheafCatAdapt = A.mkCategoryOfSheavesAdapter sheafCatDecl catDecl sheafCatLink

_ : A.isFilledCategoryOfSheaves sheafCatAdapt ≡ true
_ = refl

-- GrothendieckToposDeclaration (already tested in Chapter3Checklist)
toposDecl : S2.GrothendieckToposDeclaration
toposDecl = record
  { category = catDecl
  ; giraudAxiom1CocompletenessAndGenerators = ⊤
  ; giraudAxiom2DisjointCoproductsStableUnderPullback = ⊤
  ; giraudAxiom3EffectiveEquivalenceRelations = ⊤
  }

toposLink : S2.GrothendieckToposDeclaration.category toposDecl ≡ catDecl
toposLink = refl

toposAdapt : A.GrothendieckToposAdapter
toposAdapt = A.mkGrothendieckToposAdapter toposDecl catDecl toposLink

_ : A.isFilledGrothendieckTopos toposAdapt ≡ true
_ = refl

-- CategoryOfSheavesIsAToposTheorem
sheafToposTheoremDecl : S2.CategoryOfSheavesIsAToposTheorem
sheafToposTheoremDecl = record
  { locale = dummyLocale
  ; sheafCategory = sheafCatDecl
  ; isGrothendieckTopos = toposDecl
  }

sheafToposLink1 : S2.CategoryOfSheavesIsAToposTheorem.sheafCategory sheafToposTheoremDecl ≡ sheafCatDecl
sheafToposLink1 = refl

sheafToposLink2 : S2.CategoryOfSheavesIsAToposTheorem.isGrothendieckTopos sheafToposTheoremDecl ≡ toposDecl
sheafToposLink2 = refl

sheafToposTheoremAdapt : A.CategoryOfSheavesIsAToposTheoremAdapter
sheafToposTheoremAdapt = A.mkCategoryOfSheavesIsAToposTheoremAdapter
  sheafToposTheoremDecl sheafCatDecl toposDecl sheafToposLink1 sheafToposLink2

_ : A.isFilledCategoryOfSheavesIsAToposTheorem sheafToposTheoremAdapt ≡ true
_ = refl

------------------------------------------------------------------------
-- Exponentials and Subobject Classifier
------------------------------------------------------------------------

-- ExponentialObjectSheaf
exponentialSheafDecl : S2.ExponentialObjectSheaf
exponentialSheafDecl = record
  { baseSheaf = sheafDecl
  ; exponentSheaf = sheafDecl
  ; presheafExponential = record
      { sourcePresheaf = presheafDecl
      ; targetPresheaf = presheafDecl
      ; homPresheaf = presheafDecl
      }
  ; sheafifiedExponential = sheafDecl
  }

expSheafLink1 : S2.ExponentialObjectSheaf.baseSheaf exponentialSheafDecl ≡ sheafDecl
expSheafLink1 = refl

expSheafLink2 : S2.ExponentialObjectSheaf.exponentSheaf exponentialSheafDecl ≡ sheafDecl
expSheafLink2 = refl

exponentialSheafAdapt : A.ExponentialObjectSheafAdapter
exponentialSheafAdapt = A.mkExponentialObjectSheafAdapter
  exponentialSheafDecl sheafDecl sheafDecl expSheafLink1 expSheafLink2

_ : A.isFilledExponentialObjectSheaf exponentialSheafAdapt ≡ true
_ = refl

-- SubobjectClassifierAxiom
charMapDecl : S2.CharacteristicMapConstructor
charMapDecl = record
  { subsheafMono = ⊤
  ; parentSheaf = sheafDecl
  ; subsheaf = sheafDecl
  ; characteristicMap = ⊤
  ; measurementFunction = ⊤
  }

subobjectClassifierDecl : S2.SubobjectClassifierAxiom
subobjectClassifierDecl = record
  { monomorphism = ⊤
  ; pullbackProperty = ⊤
  ; characteristicMap = charMapDecl
  }

subobClassLink : S2.SubobjectClassifierAxiom.characteristicMap subobjectClassifierDecl ≡ charMapDecl
subobClassLink = refl

subobjectClassifierAdapt : A.SubobjectClassifierAxiomAdapter
subobjectClassifierAdapt = A.mkSubobjectClassifierAxiomAdapter
  subobjectClassifierDecl charMapDecl subobClassLink

_ : A.isFilledSubobjectClassifierAxiom subobjectClassifierAdapt ≡ true
_ = refl

------------------------------------------------------------------------
-- Étale Spaces and Stalks
------------------------------------------------------------------------

-- EtaleSpaceOver
projId : M.Identifier
projId = M.mkId "TestProjection"

etaleSpaceDecl : S2.EtaleSpaceOver
etaleSpaceDecl = record
  { totalSpace = ⊤
  ; baseSpace = ⊤
  ; projection = projId
  ; isLocalHomeomorphism = record
      { morphism = projId
      ; sourceSpace = ⊤
      ; targetSpace = ⊤
      ; localHomeomorphismCondition = ⊤
      }
  }

etaleLink : S2.EtaleSpaceOver.projection etaleSpaceDecl ≡ projId
etaleLink = refl

etaleSpaceAdapt : A.EtaleSpaceOverAdapter
etaleSpaceAdapt = A.mkEtaleSpaceOverAdapter etaleSpaceDecl projId etaleLink

_ : A.isFilledEtaleSpaceOver etaleSpaceAdapt ≡ true
_ = refl

-- CategoryOfEtaleSpaces
etaleCatDecl : S2.CategoryOfEtaleSpaces
etaleCatDecl = record
  { baseSpace = ⊤
  ; etaleSpaces = ⊤
  ; baseFibreMorphisms = ⊤
  ; categoryStructure = catDecl
  }

etaleCatLink : S2.CategoryOfEtaleSpaces.categoryStructure etaleCatDecl ≡ catDecl
etaleCatLink = refl

etaleCatAdapt : A.CategoryOfEtaleSpacesAdapter
etaleCatAdapt = A.mkCategoryOfEtaleSpacesAdapter etaleCatDecl catDecl etaleCatLink

_ : A.isFilledCategoryOfEtaleSpaces etaleCatAdapt ≡ true
_ = refl

-- StalkConstructor
stalkDecl : S2.StalkConstructor
stalkDecl = record
  { presheaf = presheafDecl
  ; point = ⊤
  ; stalkSet = ⊤
  }

stalkLink : S2.StalkConstructor.presheaf stalkDecl ≡ presheafDecl
stalkLink = refl

stalkAdapt : A.StalkConstructorAdapter
stalkAdapt = A.mkStalkConstructorAdapter stalkDecl presheafDecl stalkLink

_ : A.isFilledStalkConstructor stalkAdapt ≡ true
_ = refl

-- TotalSpaceOfStalks
totalSpaceDecl : S2.TotalSpaceOfStalks
totalSpaceDecl = record
  { presheaf = presheafDecl
  ; baseLocale = dummyLocale
  ; underlyingSet = ⊤
  ; topologyStructure = ⊤
  ; projectionMap = ⊤
  }

totalSpaceLink : S2.TotalSpaceOfStalks.presheaf totalSpaceDecl ≡ presheafDecl
totalSpaceLink = refl

totalSpaceAdapt : A.TotalSpaceOfStalksAdapter
totalSpaceAdapt = A.mkTotalSpaceOfStalksAdapter totalSpaceDecl presheafDecl totalSpaceLink

_ : A.isFilledTotalSpaceOfStalks totalSpaceAdapt ≡ true
_ = refl

-- SheafOfSectionsFunctor
sectionsFunctorDecl : S2.SheafOfSectionsFunctor
sectionsFunctorDecl = record
  { etaleSpace = etaleSpaceDecl
  ; underlyingPresheaf = presheafDecl
  ; isSheaf = sheafDecl
  }

sectionsLink1 : S2.SheafOfSectionsFunctor.etaleSpace sectionsFunctorDecl ≡ etaleSpaceDecl
sectionsLink1 = refl

sectionsLink2 : S2.SheafOfSectionsFunctor.isSheaf sectionsFunctorDecl ≡ sheafDecl
sectionsLink2 = refl

sectionsFunctorAdapt : A.SheafOfSectionsFunctorAdapter
sectionsFunctorAdapt = A.mkSheafOfSectionsFunctorAdapter
  sectionsFunctorDecl etaleSpaceDecl sheafDecl sectionsLink1 sectionsLink2

_ : A.isFilledSheafOfSectionsFunctor sectionsFunctorAdapt ≡ true
_ = refl

-- SheafEtaleEquivalenceTheorem
stalksF : M.Identifier
stalksF = M.mkId "StalksToEtaleFunctor"

sectionsF : M.Identifier
sectionsF = M.mkId "SectionsToSheafFunctor"

sheafEtaleEquivDecl : S2.SheafEtaleEquivalenceTheorem
sheafEtaleEquivDecl = record
  { soberSpace = ⊤
  ; sheafCategory = sheafCatDecl
  ; etaleCategory = etaleCatDecl
  ; stalksToEtaleFunctor = stalksF
  ; sectionsToSheafFunctor = sectionsF
  ; equivalenceOfCategories = ⊤
  }

sheafEtaleLink1 : S2.SheafEtaleEquivalenceTheorem.sheafCategory sheafEtaleEquivDecl ≡ sheafCatDecl
sheafEtaleLink1 = refl

sheafEtaleLink2 : S2.SheafEtaleEquivalenceTheorem.etaleCategory sheafEtaleEquivDecl ≡ etaleCatDecl
sheafEtaleLink2 = refl

sheafEtaleLink3 : S2.SheafEtaleEquivalenceTheorem.stalksToEtaleFunctor sheafEtaleEquivDecl ≡ stalksF
sheafEtaleLink3 = refl

sheafEtaleLink4 : S2.SheafEtaleEquivalenceTheorem.sectionsToSheafFunctor sheafEtaleEquivDecl ≡ sectionsF
sheafEtaleLink4 = refl

sheafEtaleEquivAdapt : A.SheafEtaleEquivalenceTheoremAdapter
sheafEtaleEquivAdapt = A.mkSheafEtaleEquivalenceTheoremAdapter
  sheafEtaleEquivDecl sheafCatDecl etaleCatDecl stalksF sectionsF
  sheafEtaleLink1 sheafEtaleLink2 sheafEtaleLink3 sheafEtaleLink4

_ : A.isFilledSheafEtaleEquivalenceTheorem sheafEtaleEquivAdapt ≡ true
_ = refl

------------------------------------------------------------------------
-- Change of Base
------------------------------------------------------------------------

-- DirectImageFunctorLocale
directF : M.Identifier
directF = M.mkId "DirectImageFunctor"

directImageDecl : S2.DirectImageFunctorLocale
directImageDecl = record
  { localeMorphism = ⊤
  ; sourceLocale = dummyLocale
  ; targetLocale = dummyLocale
  ; underlyingFunctor = directF
  ; actionOnSheaves = ⊤
  }

directImageLink : S2.DirectImageFunctorLocale.underlyingFunctor directImageDecl ≡ directF
directImageLink = refl

directImageAdapt : A.DirectImageFunctorLocaleAdapter
directImageAdapt = A.mkDirectImageFunctorLocaleAdapter directImageDecl directF directImageLink

_ : A.isFilledDirectImageFunctorLocale directImageAdapt ≡ true
_ = refl

-- InverseImageFunctorLocale
inverseF : M.Identifier
inverseF = M.mkId "InverseImageFunctor"

inverseImageDecl : S2.InverseImageFunctorLocale
inverseImageDecl = record
  { localeMorphism = ⊤
  ; sourceLocale = dummyLocale
  ; targetLocale = dummyLocale
  ; underlyingFunctor = inverseF
  ; sheafificationComponent = ⊤
  }

inverseImageLink : S2.InverseImageFunctorLocale.underlyingFunctor inverseImageDecl ≡ inverseF
inverseImageLink = refl

inverseImageAdapt : A.InverseImageFunctorLocaleAdapter
inverseImageAdapt = A.mkInverseImageFunctorLocaleAdapter inverseImageDecl inverseF inverseImageLink

_ : A.isFilledInverseImageFunctorLocale inverseImageAdapt ≡ true
_ = refl

-- LocaleChangeOfBaseAdjunctionTheorem
adjId : M.Identifier
adjId = M.mkId "ChangeOfBaseAdjunction"

changeOfBaseDecl : S2.LocaleChangeOfBaseAdjunctionTheorem
changeOfBaseDecl = record
  { localeMorphism = ⊤
  ; inverseImageFunctor = inverseImageDecl
  ; directImageFunctor = directImageDecl
  ; adjunction = adjId
  }

changeOfBaseLink1 : S2.LocaleChangeOfBaseAdjunctionTheorem.inverseImageFunctor changeOfBaseDecl ≡ inverseImageDecl
changeOfBaseLink1 = refl

changeOfBaseLink2 : S2.LocaleChangeOfBaseAdjunctionTheorem.directImageFunctor changeOfBaseDecl ≡ directImageDecl
changeOfBaseLink2 = refl

changeOfBaseLink3 : S2.LocaleChangeOfBaseAdjunctionTheorem.adjunction changeOfBaseDecl ≡ adjId
changeOfBaseLink3 = refl

changeOfBaseAdapt : A.LocaleChangeOfBaseAdjunctionTheoremAdapter
changeOfBaseAdapt = A.mkLocaleChangeOfBaseAdjunctionTheoremAdapter
  changeOfBaseDecl inverseImageDecl directImageDecl adjId
  changeOfBaseLink1 changeOfBaseLink2 changeOfBaseLink3

_ : A.isFilledLocaleChangeOfBaseAdjunctionTheorem changeOfBaseAdapt ≡ true
_ = refl

-- EtaleMorphismInducesSheafEquivalenceTheorem
etaleEquivDecl : S2.EtaleMorphismInducesSheafEquivalenceTheorem
etaleEquivDecl = record
  { etaleMorphism = ⊤
  ; sourceLocale = dummyLocale
  ; targetLocale = dummyLocale
  ; inverseImageFunctor = inverseImageDecl
  ; isEquivalenceOfCategories = ⊤
  }

etaleEquivLink : S2.EtaleMorphismInducesSheafEquivalenceTheorem.inverseImageFunctor etaleEquivDecl ≡ inverseImageDecl
etaleEquivLink = refl

etaleEquivAdapt : A.EtaleMorphismInducesSheafEquivalenceTheoremAdapter
etaleEquivAdapt = A.mkEtaleMorphismInducesSheafEquivalenceTheoremAdapter
  etaleEquivDecl inverseImageDecl etaleEquivLink

_ : A.isFilledEtaleMorphismInducesSheafEquivalenceTheorem etaleEquivAdapt ≡ true
_ = refl

------------------------------------------------------------------------
-- Ω-Sets
------------------------------------------------------------------------

-- OmegaSetData
omegaDataDecl : S2.OmegaSetData
omegaDataDecl = record
  { frame = ⊤
  ; elementsSet = ⊤
  ; equalityPredicate = ⊤
  }

-- OmegaSetDeclarationVerified (already tested, but include here)
omegaSetDecl : S2.OmegaSetDeclarationVerified
omegaSetDecl = record
  { dataOmegaSet = omegaDataDecl
  ; reflexivityProof = record { omegaSetData = omegaDataDecl ; reflexivityCondition = ⊤ }
  ; symmetryProof = record { omegaSetData = omegaDataDecl ; symmetryCondition = ⊤ }
  ; transitivityProof = record { omegaSetData = omegaDataDecl ; transitivityCondition = ⊤ }
  }

omegaSetLink : S2.OmegaSetDeclarationVerified.dataOmegaSet omegaSetDecl ≡ omegaDataDecl
omegaSetLink = refl

omegaSetAdapt : A.OmegaSetAdapter
omegaSetAdapt = A.mkOmegaSetAdapter omegaSetDecl omegaDataDecl omegaSetLink

_ : A.isFilledOmegaSet omegaSetAdapt ≡ true
_ = refl

-- CategoryOfOmegaSets
omegaCatDecl : S2.CategoryOfOmegaSets
omegaCatDecl = record
  { frame = ⊤
  ; omegaSets = ⊤
  ; omegaHomomorphisms = ⊤
  ; categoryStructure = catDecl
  }

-- FunctorSheafToOmegaSet
sheafToOmegaId : M.Identifier
sheafToOmegaId = M.mkId "SheafToOmegaSetFunctor"

sheafToOmegaDecl : S2.FunctorSheafToOmegaSet
sheafToOmegaDecl = record
  { locale = dummyLocale
  ; frame = ⊤
  ; actionOnSheaves = ⊤
  ; globalElementsExtraction = ⊤
  ; equalityFromAgreement = ⊤
  ; actionOnMorphisms = ⊤
  ; underlyingFunctor = sheafToOmegaId
  }

-- FunctorOmegaSetToSheaf
omegaToSheafId : M.Identifier
omegaToSheafId = M.mkId "OmegaSetToSheafFunctor"

omegaToSheafDecl : S2.FunctorOmegaSetToSheaf
omegaToSheafDecl = record
  { frame = ⊤
  ; locale = dummyLocale
  ; actionOnOmegaSets = ⊤
  ; uElementsConstruction = ⊤
  ; completenessImpliesSheaf = ⊤
  ; actionOnMorphisms = ⊤
  ; underlyingFunctor = omegaToSheafId
  }

-- SheavesAreCompleteOmegaSetsRefinedTheorem
omegaEquivDecl : S2.SheavesAreCompleteOmegaSetsRefinedTheorem
omegaEquivDecl = record
  { locale = dummyLocale
  ; frame = ⊤
  ; sheafCategory = sheafCatDecl
  ; completeOmegaSetCategory = omegaCatDecl
  ; functorA = sheafToOmegaDecl
  ; functorS = omegaToSheafDecl
  ; quasiInverses = ⊤
  }

omegaEquivLink1 : S2.SheavesAreCompleteOmegaSetsRefinedTheorem.sheafCategory omegaEquivDecl ≡ sheafCatDecl
omegaEquivLink1 = refl

omegaEquivLink2 : S2.SheavesAreCompleteOmegaSetsRefinedTheorem.completeOmegaSetCategory omegaEquivDecl ≡ omegaCatDecl
omegaEquivLink2 = refl

omegaEquivLink3 : S2.SheavesAreCompleteOmegaSetsRefinedTheorem.functorA omegaEquivDecl ≡ sheafToOmegaDecl
omegaEquivLink3 = refl

omegaEquivLink4 : S2.SheavesAreCompleteOmegaSetsRefinedTheorem.functorS omegaEquivDecl ≡ omegaToSheafDecl
omegaEquivLink4 = refl

omegaEquivAdapt : A.SheavesAreCompleteOmegaSetsRefinedTheoremAdapter
omegaEquivAdapt = A.mkSheavesAreCompleteOmegaSetsRefinedTheoremAdapter
  omegaEquivDecl sheafCatDecl omegaCatDecl sheafToOmegaDecl omegaToSheafDecl
  omegaEquivLink1 omegaEquivLink2 omegaEquivLink3 omegaEquivLink4

_ : A.isFilledSheavesAreCompleteOmegaSetsRefinedTheorem omegaEquivAdapt ≡ true
_ = refl

------------------------------------------------------------------------
-- Sheaves of Rings and Modules
------------------------------------------------------------------------

-- SheafOfRings
ringSheafDecl : S2.SheafOfRings
ringSheafDecl = record
  { locale = dummyLocale
  ; underlyingSheaf = sheafDecl
  ; ringStructureOnSections = ⊤
  ; restrictionsPreserveRingStructure = ⊤
  }

ringSheafLink : S2.SheafOfRings.underlyingSheaf ringSheafDecl ≡ sheafDecl
ringSheafLink = refl

ringSheafAdapt : A.SheafOfRingsAdapter
ringSheafAdapt = A.mkSheafOfRingsAdapter ringSheafDecl sheafDecl ringSheafLink

_ : A.isFilledSheafOfRings ringSheafAdapt ≡ true
_ = refl

-- SheafOfOModules
moduleSheafDecl : S2.SheafOfOModules
moduleSheafDecl = record
  { locale = dummyLocale
  ; sheafOfRings = ringSheafDecl
  ; underlyingSheaf = sheafDecl
  ; moduleStructureOnSections = ⊤
  ; restrictionsAreOLinear = ⊤
  }

moduleSheafLink1 : S2.SheafOfOModules.sheafOfRings moduleSheafDecl ≡ ringSheafDecl
moduleSheafLink1 = refl

moduleSheafLink2 : S2.SheafOfOModules.underlyingSheaf moduleSheafDecl ≡ sheafDecl
moduleSheafLink2 = refl

moduleSheafAdapt : A.SheafOfOModulesAdapter
moduleSheafAdapt = A.mkSheafOfOModulesAdapter
  moduleSheafDecl ringSheafDecl sheafDecl moduleSheafLink1 moduleSheafLink2

_ : A.isFilledSheafOfOModules moduleSheafAdapt ≡ true
_ = refl

-- CategoryOfOModulesIsAbelianCorollary
oModAbelianDecl : S2.CategoryOfOModulesIsAbelianCorollary
oModAbelianDecl = record
  { sheafOfRings = ringSheafDecl
  ; categoryOfOModules = catDecl
  ; isAbelianCategory = ⊤
  ; homologicalAlgebraAvailable = ⊤
  }

oModAbelianLink1 : S2.CategoryOfOModulesIsAbelianCorollary.sheafOfRings oModAbelianDecl ≡ ringSheafDecl
oModAbelianLink1 = refl

oModAbelianLink2 : S2.CategoryOfOModulesIsAbelianCorollary.categoryOfOModules oModAbelianDecl ≡ catDecl
oModAbelianLink2 = refl

oModAbelianAdapt : A.CategoryOfOModulesIsAbelianCorollaryAdapter
oModAbelianAdapt = A.mkCategoryOfOModulesIsAbelianCorollaryAdapter
  oModAbelianDecl ringSheafDecl catDecl oModAbelianLink1 oModAbelianLink2

_ : A.isFilledCategoryOfOModulesIsAbelianCorollary oModAbelianAdapt ≡ true
_ = refl

------------------------------------------------------------------------
-- Summary: 25 topos theory test assertions
------------------------------------------------------------------------
