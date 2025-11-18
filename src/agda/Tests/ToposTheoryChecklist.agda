-- Tests.ToposTheoryChecklist: Test instances for topos theory concepts

module Tests.ToposTheoryChecklist where

open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.Equality using (refl; _≡_)
open import Agda.Builtin.Unit using (⊤; tt)
open import Core.CategoricalAdapter
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
presheafAdapt = A.mkPresheafOnLocaleAdapter presheafDecl (λ _ → presheafDecl) (λ _ → presheafDecl)

_ : A.isFilledPresheafOnLocale presheafAdapt ≡ true
_ = refl
_ : (CategoricalAdapter.morphism (A.presheafCategorical presheafAdapt) tt) ≡ A.PresheafOnLocaleAdapter.decl presheafAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.presheafCategorical presheafAdapt) ≡ refl
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
gluingAdapt = A.mkSheafGluingAxiomAdapter gluingAxiomDecl presheafDecl gluingLink (λ _ → gluingAxiomDecl)

_ : A.isFilledSheafGluingAxiom gluingAdapt ≡ true
_ = refl
_ : (CategoricalAdapter.morphism (A.sheafGluingCategorical gluingAdapt) tt) ≡ A.SheafGluingAxiomAdapter.decl gluingAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.sheafGluingCategorical gluingAdapt) ≡ refl
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
sheafAdapt = A.mkSheafOnLocaleAdapter sheafDecl presheafDecl sheafLink (λ _ → sheafDecl)

_ : A.isFilledSheafOnLocale sheafAdapt ≡ true
_ = refl
_ : (CategoricalAdapter.morphism (A.sheafOnLocaleCategorical sheafAdapt) tt) ≡ A.SheafOnLocaleAdapter.decl sheafAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.sheafOnLocaleCategorical sheafAdapt) ≡ refl
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
sheafCatAdapt = A.mkCategoryOfSheavesAdapter sheafCatDecl catDecl sheafCatLink (λ _ → sheafCatDecl)

_ : A.isFilledCategoryOfSheaves sheafCatAdapt ≡ true
_ = refl
_ : (CategoricalAdapter.morphism (A.categoryOfSheavesCategorical sheafCatAdapt) tt) ≡ A.CategoryOfSheavesAdapter.decl sheafCatAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.categoryOfSheavesCategorical sheafCatAdapt) ≡ refl
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
toposAdapt = A.mkGrothendieckToposAdapter toposDecl catDecl toposLink (λ _ → toposDecl)

_ : A.isFilledGrothendieckTopos toposAdapt ≡ true
_ = refl
_ : (CategoricalAdapter.morphism (A.grothendieckToposCategorical toposAdapt) tt) ≡ A.GrothendieckToposAdapter.decl toposAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.grothendieckToposCategorical toposAdapt) ≡ refl
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
  sheafToposTheoremDecl sheafCatDecl toposDecl sheafToposLink1 sheafToposLink2 (λ _ → sheafToposTheoremDecl)

_ : A.isFilledCategoryOfSheavesIsAToposTheorem sheafToposTheoremAdapt ≡ true
_ = refl
_ : (CategoricalAdapter.morphism (A.categoryOfSheavesIsAToposTheoremCategorical sheafToposTheoremAdapt) tt) ≡ A.CategoryOfSheavesIsAToposTheoremAdapter.decl sheafToposTheoremAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.categoryOfSheavesIsAToposTheoremCategorical sheafToposTheoremAdapt) ≡ refl
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
  exponentialSheafDecl sheafDecl sheafDecl expSheafLink1 expSheafLink2 (λ _ → exponentialSheafDecl)

_ : A.isFilledExponentialObjectSheaf exponentialSheafAdapt ≡ true
_ = refl
_ : (CategoricalAdapter.morphism (A.exponentialObjectSheafCategorical exponentialSheafAdapt) tt) ≡ A.ExponentialObjectSheafAdapter.decl exponentialSheafAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.exponentialObjectSheafCategorical exponentialSheafAdapt) ≡ refl
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
  subobjectClassifierDecl charMapDecl subobClassLink (λ _ → subobjectClassifierDecl)

_ : A.isFilledSubobjectClassifierAxiom subobjectClassifierAdapt ≡ true
_ = refl
_ : (CategoricalAdapter.morphism (A.subobjectClassifierAxiomCategorical subobjectClassifierAdapt) tt) ≡ A.SubobjectClassifierAxiomAdapter.decl subobjectClassifierAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.subobjectClassifierAxiomCategorical subobjectClassifierAdapt) ≡ refl
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
etaleSpaceAdapt = A.mkEtaleSpaceOverAdapter etaleSpaceDecl projId etaleLink (λ _ → etaleSpaceDecl)

_ : A.isFilledEtaleSpaceOver etaleSpaceAdapt ≡ true
_ = refl
_ : (CategoricalAdapter.morphism (A.etaleSpaceOverCategorical etaleSpaceAdapt) tt) ≡ A.EtaleSpaceOverAdapter.decl etaleSpaceAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.etaleSpaceOverCategorical etaleSpaceAdapt) ≡ refl
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
etaleCatAdapt = A.mkCategoryOfEtaleSpacesAdapter etaleCatDecl catDecl etaleCatLink (λ _ → etaleCatDecl)

_ : A.isFilledCategoryOfEtaleSpaces etaleCatAdapt ≡ true
_ = refl
_ : (CategoricalAdapter.morphism (A.categoryOfEtaleSpacesCategorical etaleCatAdapt) tt) ≡ A.CategoryOfEtaleSpacesAdapter.decl etaleCatAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.categoryOfEtaleSpacesCategorical etaleCatAdapt) ≡ refl
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
stalkAdapt = A.mkStalkConstructorAdapter stalkDecl presheafDecl stalkLink (λ _ → stalkDecl)

_ : A.isFilledStalkConstructor stalkAdapt ≡ true
_ = refl
_ : (CategoricalAdapter.morphism (A.stalkConstructorCategorical stalkAdapt) tt) ≡ A.StalkConstructorAdapter.decl stalkAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.stalkConstructorCategorical stalkAdapt) ≡ refl
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
totalSpaceAdapt = A.mkTotalSpaceOfStalksAdapter totalSpaceDecl presheafDecl totalSpaceLink (λ _ → totalSpaceDecl)

_ : A.isFilledTotalSpaceOfStalks totalSpaceAdapt ≡ true
_ = refl
_ : (CategoricalAdapter.morphism (A.totalSpaceOfStalksCategorical totalSpaceAdapt) tt) ≡ A.TotalSpaceOfStalksAdapter.decl totalSpaceAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.totalSpaceOfStalksCategorical totalSpaceAdapt) ≡ refl
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
  sectionsFunctorDecl etaleSpaceDecl sheafDecl sectionsLink1 sectionsLink2 (λ _ → sectionsFunctorDecl)

_ : A.isFilledSheafOfSectionsFunctor sectionsFunctorAdapt ≡ true
_ = refl
_ : (CategoricalAdapter.morphism (A.sheafOfSectionsFunctorCategorical sectionsFunctorAdapt) tt) ≡ A.SheafOfSectionsFunctorAdapter.decl sectionsFunctorAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.sheafOfSectionsFunctorCategorical sectionsFunctorAdapt) ≡ refl
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
  sheafEtaleLink1 sheafEtaleLink2 sheafEtaleLink3 sheafEtaleLink4 (λ _ → sheafEtaleEquivDecl)

_ : A.isFilledSheafEtaleEquivalenceTheorem sheafEtaleEquivAdapt ≡ true
_ = refl
_ : (CategoricalAdapter.morphism (A.sheafEtaleEquivalenceTheoremCategorical sheafEtaleEquivAdapt) tt) ≡ A.SheafEtaleEquivalenceTheoremAdapter.decl sheafEtaleEquivAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.sheafEtaleEquivalenceTheoremCategorical sheafEtaleEquivAdapt) ≡ refl
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
directImageAdapt = A.mkDirectImageFunctorLocaleAdapter directImageDecl directF directImageLink (λ _ → directImageDecl)

_ : A.isFilledDirectImageFunctorLocale directImageAdapt ≡ true
_ = refl
_ : (CategoricalAdapter.morphism (A.directImageFunctorLocaleCategorical directImageAdapt) tt) ≡ A.DirectImageFunctorLocaleAdapter.decl directImageAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.directImageFunctorLocaleCategorical directImageAdapt) ≡ refl
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
inverseImageAdapt = A.mkInverseImageFunctorLocaleAdapter inverseImageDecl inverseF inverseImageLink (λ _ → inverseImageDecl)

_ : A.isFilledInverseImageFunctorLocale inverseImageAdapt ≡ true
_ = refl
_ : (CategoricalAdapter.morphism (A.inverseImageFunctorLocaleCategorical inverseImageAdapt) tt) ≡ A.InverseImageFunctorLocaleAdapter.decl inverseImageAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.inverseImageFunctorLocaleCategorical inverseImageAdapt) ≡ refl
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
  changeOfBaseLink1 changeOfBaseLink2 changeOfBaseLink3 (λ _ → changeOfBaseDecl)

_ : A.isFilledLocaleChangeOfBaseAdjunctionTheorem changeOfBaseAdapt ≡ true
_ = refl
_ : (CategoricalAdapter.morphism (A.localeChangeOfBaseAdjunctionTheoremCategorical changeOfBaseAdapt) tt) ≡ A.LocaleChangeOfBaseAdjunctionTheoremAdapter.decl changeOfBaseAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.localeChangeOfBaseAdjunctionTheoremCategorical changeOfBaseAdapt) ≡ refl
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
  etaleEquivDecl inverseImageDecl etaleEquivLink (λ _ → etaleEquivDecl)

_ : A.isFilledEtaleMorphismInducesSheafEquivalenceTheorem etaleEquivAdapt ≡ true
_ = refl
_ : (CategoricalAdapter.morphism (A.etaleMorphismInducesSheafEquivalenceTheoremCategorical etaleEquivAdapt) tt) ≡ A.EtaleMorphismInducesSheafEquivalenceTheoremAdapter.decl etaleEquivAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.etaleMorphismInducesSheafEquivalenceTheoremCategorical etaleEquivAdapt) ≡ refl
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
_ : (CategoricalAdapter.morphism (A.omegaSetCategorical omegaSetAdapt) tt) ≡ A.OmegaSetAdapter.decl omegaSetAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.omegaSetCategorical omegaSetAdapt) ≡ refl
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
  omegaEquivLink1 omegaEquivLink2 omegaEquivLink3 omegaEquivLink4 (λ _ → omegaEquivDecl)

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
ringSheafAdapt = A.mkSheafOfRingsAdapter ringSheafDecl sheafDecl ringSheafLink (λ _ → ringSheafDecl)

_ : A.isFilledSheafOfRings ringSheafAdapt ≡ true
_ = refl
_ : (CategoricalAdapter.morphism (A.sheafOfRingsCategorical ringSheafAdapt) tt) ≡ A.SheafOfRingsAdapter.decl ringSheafAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.sheafOfRingsCategorical ringSheafAdapt) ≡ refl
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
  moduleSheafDecl ringSheafDecl sheafDecl moduleSheafLink1 moduleSheafLink2 (λ _ → moduleSheafDecl)

_ : A.isFilledSheafOfOModules moduleSheafAdapt ≡ true
_ = refl
_ : (CategoricalAdapter.morphism (A.sheafOfOModulesCategorical moduleSheafAdapt) tt) ≡ A.SheafOfOModulesAdapter.decl moduleSheafAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.sheafOfOModulesCategorical moduleSheafAdapt) ≡ refl
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
  oModAbelianDecl ringSheafDecl catDecl oModAbelianLink1 oModAbelianLink2 (λ _ → oModAbelianDecl)

_ : A.isFilledCategoryOfOModulesIsAbelianCorollary oModAbelianAdapt ≡ true
_ = refl
_ : (CategoricalAdapter.morphism (A.categoryOfOModulesIsAbelianCorollaryCategorical oModAbelianAdapt) tt) ≡ A.CategoryOfOModulesIsAbelianCorollaryAdapter.decl oModAbelianAdapt
_ = refl
_ : CategoricalAdapter.isomorphism (A.categoryOfOModulesIsAbelianCorollaryCategorical oModAbelianAdapt) ≡ refl
_ = refl

------------------------------------------------------------------------
-- Summary: 25 topos theory test assertions
------------------------------------------------------------------------
