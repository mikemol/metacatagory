-- Tests.ToposObligationAdapters: Adapters specific to topos theory to decouple from global adapters

module Tests.ToposObligationAdapters where

import Agda.Builtin.Bool as B
open import Agda.Builtin.Equality using (_≡_)
open import Agda.Builtin.Unit using (⊤)
open import Agda.Primitive using (Level; lzero; lsuc)
open import Metamodel as M
import Chapter1.Level1sub3 as C1S3
import Chapter3.Level3sub2 as C3S2
open import Core.CategoricalAdapter

-- Presheaf on locale
record PresheafOnLocaleAdapter : Set₁ where
  field
    decl : C3S2.PresheafOnLocale
    status : B.Bool

mkPresheafOnLocaleAdapter : 
  C3S2.PresheafOnLocale → 
  (⊤ → C3S2.PresheafOnLocale) →
  PresheafOnLocaleAdapter
mkPresheafOnLocaleAdapter d f = record 
  { decl = d 
  ; status = B.true 
  }

-- Categorical view (separate from adapter record to avoid universe issues)
presheafCategorical : PresheafOnLocaleAdapter → CategoricalAdapter {lsuc lzero} C3S2.PresheafOnLocale
presheafCategorical adapt = mkCategoricalAdapter C3S2.PresheafOnLocale (λ _ → PresheafOnLocaleAdapter.decl adapt)

isFilledPresheafOnLocale : PresheafOnLocaleAdapter → B.Bool
isFilledPresheafOnLocale a = PresheafOnLocaleAdapter.status a

-- Sheaf gluing axiom
record SheafGluingAxiomAdapter : Set₁ where
  field
    decl : C3S2.SheafGluingAxiom
    expPresheaf : C3S2.PresheafOnLocale
    linkPresheaf : C3S2.SheafGluingAxiom.presheaf decl ≡ expPresheaf
    status : B.Bool

mkSheafGluingAxiomAdapter :
  (d : C3S2.SheafGluingAxiom) →
  (psh : C3S2.PresheafOnLocale) →
  (ppsh : C3S2.SheafGluingAxiom.presheaf d ≡ psh) →
  (f : ⊤ → C3S2.SheafGluingAxiom) →
  SheafGluingAxiomAdapter
mkSheafGluingAxiomAdapter d psh ppsh f =
  record { decl = d ; expPresheaf = psh ; linkPresheaf = ppsh ; status = B.true }

sheafGluingCategorical : SheafGluingAxiomAdapter → CategoricalAdapter {lsuc lzero} C3S2.SheafGluingAxiom
sheafGluingCategorical adapt = mkCategoricalAdapter C3S2.SheafGluingAxiom (λ _ → SheafGluingAxiomAdapter.decl adapt)

isFilledSheafGluingAxiom : SheafGluingAxiomAdapter → B.Bool
isFilledSheafGluingAxiom a = SheafGluingAxiomAdapter.status a

-- Sheaf on locale
record SheafOnLocaleAdapter : Set₁ where
  field
    decl : C3S2.SheafOnLocaleDeclaration
    expPresheaf : C3S2.PresheafOnLocale
    linkPresheaf : C3S2.SheafOnLocaleDeclaration.underlyingPresheaf decl ≡ expPresheaf
    status : B.Bool

mkSheafOnLocaleAdapter :
  (d : C3S2.SheafOnLocaleDeclaration) →
  (psh : C3S2.PresheafOnLocale) →
  (ppsh : C3S2.SheafOnLocaleDeclaration.underlyingPresheaf d ≡ psh) →
  (f : ⊤ → C3S2.SheafOnLocaleDeclaration) →
  SheafOnLocaleAdapter
mkSheafOnLocaleAdapter d psh ppsh f =
  record { decl = d ; expPresheaf = psh ; linkPresheaf = ppsh ; status = B.true }

sheafOnLocaleCategorical : SheafOnLocaleAdapter → CategoricalAdapter {lsuc lzero} C3S2.SheafOnLocaleDeclaration
sheafOnLocaleCategorical adapt = mkCategoricalAdapter C3S2.SheafOnLocaleDeclaration (λ _ → SheafOnLocaleAdapter.decl adapt)

isFilledSheafOnLocale : SheafOnLocaleAdapter → B.Bool
isFilledSheafOnLocale a = SheafOnLocaleAdapter.status a

-- Category of sheaves
record CategoryOfSheavesAdapter : Set₁ where
  field
    decl : C3S2.CategoryOfSheaves
    expCategory : C1S3.CategoryDeclaration
    linkCategory : C3S2.CategoryOfSheaves.underlyingCategory decl ≡ expCategory
    status : B.Bool

mkCategoryOfSheavesAdapter :
  (d : C3S2.CategoryOfSheaves) →
  (cat : C1S3.CategoryDeclaration) →
  (pcat : C3S2.CategoryOfSheaves.underlyingCategory d ≡ cat) →
  (f : ⊤ → C3S2.CategoryOfSheaves) →
  CategoryOfSheavesAdapter
mkCategoryOfSheavesAdapter d cat pcat f =
  record { decl = d ; expCategory = cat ; linkCategory = pcat ; status = B.true }

categoryOfSheavesCategorical : CategoryOfSheavesAdapter → CategoricalAdapter {lsuc lzero} C3S2.CategoryOfSheaves
categoryOfSheavesCategorical adapt = mkCategoricalAdapter C3S2.CategoryOfSheaves (λ _ → CategoryOfSheavesAdapter.decl adapt)

isFilledCategoryOfSheaves : CategoryOfSheavesAdapter → B.Bool
isFilledCategoryOfSheaves a = CategoryOfSheavesAdapter.status a

-- Grothendieck topos declaration
record GrothendieckToposAdapter : Set₁ where
  field
    decl : C3S2.GrothendieckToposDeclaration
    expCategory : C1S3.CategoryDeclaration
    linkCategory : C3S2.GrothendieckToposDeclaration.category decl ≡ expCategory
    status : B.Bool

mkGrothendieckToposAdapter :
  (d : C3S2.GrothendieckToposDeclaration) →
  (cat : C1S3.CategoryDeclaration) →
  (pcat : C3S2.GrothendieckToposDeclaration.category d ≡ cat) →
  (f : ⊤ → C3S2.GrothendieckToposDeclaration) →
  GrothendieckToposAdapter
mkGrothendieckToposAdapter d cat pcat f =
  record { decl = d ; expCategory = cat ; linkCategory = pcat ; status = B.true }

grothendieckToposCategorical : GrothendieckToposAdapter → CategoricalAdapter {lsuc lzero} C3S2.GrothendieckToposDeclaration
grothendieckToposCategorical adapt = mkCategoricalAdapter C3S2.GrothendieckToposDeclaration (λ _ → GrothendieckToposAdapter.decl adapt)

isFilledGrothendieckTopos : GrothendieckToposAdapter → B.Bool
isFilledGrothendieckTopos a = GrothendieckToposAdapter.status a

-- CategoryOfSheavesIsAToposTheorem
record CategoryOfSheavesIsAToposTheoremAdapter : Set₁ where
  field
    decl : C3S2.CategoryOfSheavesIsAToposTheorem
    expSheafCat : C3S2.CategoryOfSheaves
    expTopos : C3S2.GrothendieckToposDeclaration
    linkSheafCat : C3S2.CategoryOfSheavesIsAToposTheorem.sheafCategory decl ≡ expSheafCat
    linkTopos : C3S2.CategoryOfSheavesIsAToposTheorem.isGrothendieckTopos decl ≡ expTopos
    status : B.Bool

mkCategoryOfSheavesIsAToposTheoremAdapter :
  (d : C3S2.CategoryOfSheavesIsAToposTheorem) →
  (sc : C3S2.CategoryOfSheaves) →
  (tp : C3S2.GrothendieckToposDeclaration) →
  (psc : C3S2.CategoryOfSheavesIsAToposTheorem.sheafCategory d ≡ sc) →
  (ptp : C3S2.CategoryOfSheavesIsAToposTheorem.isGrothendieckTopos d ≡ tp) →
  (f : ⊤ → C3S2.CategoryOfSheavesIsAToposTheorem) →
  CategoryOfSheavesIsAToposTheoremAdapter
mkCategoryOfSheavesIsAToposTheoremAdapter d sc tp psc ptp f =
  record { decl = d ; expSheafCat = sc ; expTopos = tp
         ; linkSheafCat = psc ; linkTopos = ptp ; status = B.true }

categoryOfSheavesIsAToposTheoremCategorical : CategoryOfSheavesIsAToposTheoremAdapter → CategoricalAdapter {lsuc lzero} C3S2.CategoryOfSheavesIsAToposTheorem
categoryOfSheavesIsAToposTheoremCategorical adapt = mkCategoricalAdapter C3S2.CategoryOfSheavesIsAToposTheorem (λ _ → CategoryOfSheavesIsAToposTheoremAdapter.decl adapt)

isFilledCategoryOfSheavesIsAToposTheorem : CategoryOfSheavesIsAToposTheoremAdapter → B.Bool
isFilledCategoryOfSheavesIsAToposTheorem a = CategoryOfSheavesIsAToposTheoremAdapter.status a

-- Exponential object in sheaf category
record ExponentialObjectSheafAdapter : Set₁ where
  field
    decl : C3S2.ExponentialObjectSheaf
    expBase : C3S2.SheafOnLocaleDeclaration
    expExponent : C3S2.SheafOnLocaleDeclaration
    linkBase : C3S2.ExponentialObjectSheaf.baseSheaf decl ≡ expBase
    linkExponent : C3S2.ExponentialObjectSheaf.exponentSheaf decl ≡ expExponent
    status : B.Bool

mkExponentialObjectSheafAdapter :
  (d : C3S2.ExponentialObjectSheaf) →
  (b : C3S2.SheafOnLocaleDeclaration) →
  (e : C3S2.SheafOnLocaleDeclaration) →
  (pb : C3S2.ExponentialObjectSheaf.baseSheaf d ≡ b) →
  (pe : C3S2.ExponentialObjectSheaf.exponentSheaf d ≡ e) →
  (f : ⊤ → C3S2.ExponentialObjectSheaf) →
  ExponentialObjectSheafAdapter
mkExponentialObjectSheafAdapter d b e pb pe f =
  record { decl = d ; expBase = b ; expExponent = e
         ; linkBase = pb ; linkExponent = pe ; status = B.true }

exponentialObjectSheafCategorical : ExponentialObjectSheafAdapter → CategoricalAdapter {lsuc lzero} C3S2.ExponentialObjectSheaf
exponentialObjectSheafCategorical adapt = mkCategoricalAdapter C3S2.ExponentialObjectSheaf (λ _ → ExponentialObjectSheafAdapter.decl adapt)

isFilledExponentialObjectSheaf : ExponentialObjectSheafAdapter → B.Bool
isFilledExponentialObjectSheaf a = ExponentialObjectSheafAdapter.status a

-- Subobject classifier
record SubobjectClassifierAxiomAdapter : Set₁ where
  field
    decl : C3S2.SubobjectClassifierAxiom
    expCharMap : C3S2.CharacteristicMapConstructor
    linkCharMap : C3S2.SubobjectClassifierAxiom.characteristicMap decl ≡ expCharMap
    status : B.Bool

mkSubobjectClassifierAxiomAdapter :
  (d : C3S2.SubobjectClassifierAxiom) →
  (cm : C3S2.CharacteristicMapConstructor) →
  (pcm : C3S2.SubobjectClassifierAxiom.characteristicMap d ≡ cm) →
  (f : ⊤ → C3S2.SubobjectClassifierAxiom) →
  SubobjectClassifierAxiomAdapter
mkSubobjectClassifierAxiomAdapter d cm pcm f =
  record { decl = d ; expCharMap = cm ; linkCharMap = pcm ; status = B.true }

subobjectClassifierAxiomCategorical : SubobjectClassifierAxiomAdapter → CategoricalAdapter {lsuc lzero} C3S2.SubobjectClassifierAxiom
subobjectClassifierAxiomCategorical adapt = mkCategoricalAdapter C3S2.SubobjectClassifierAxiom (λ _ → SubobjectClassifierAxiomAdapter.decl adapt)

isFilledSubobjectClassifierAxiom : SubobjectClassifierAxiomAdapter → B.Bool
isFilledSubobjectClassifierAxiom a = SubobjectClassifierAxiomAdapter.status a

-- Étale space
record EtaleSpaceOverAdapter : Set₁ where
  field
    decl : C3S2.EtaleSpaceOver
    expProj : M.Identifier
    linkProj : C3S2.EtaleSpaceOver.projection decl ≡ expProj
    status : B.Bool

mkEtaleSpaceOverAdapter :
  (d : C3S2.EtaleSpaceOver) →
  (p : M.Identifier) →
  (pp : C3S2.EtaleSpaceOver.projection d ≡ p) →
  (f : ⊤ → C3S2.EtaleSpaceOver) →
  EtaleSpaceOverAdapter
mkEtaleSpaceOverAdapter d p pp f =
  record { decl = d ; expProj = p ; linkProj = pp ; status = B.true }

etaleSpaceOverCategorical : EtaleSpaceOverAdapter → CategoricalAdapter {lsuc lzero} C3S2.EtaleSpaceOver
etaleSpaceOverCategorical adapt = mkCategoricalAdapter C3S2.EtaleSpaceOver (λ _ → EtaleSpaceOverAdapter.decl adapt)

isFilledEtaleSpaceOver : EtaleSpaceOverAdapter → B.Bool
isFilledEtaleSpaceOver a = EtaleSpaceOverAdapter.status a

-- Category of étale spaces
record CategoryOfEtaleSpacesAdapter : Set₁ where
  field
    decl : C3S2.CategoryOfEtaleSpaces
    expCategory : C1S3.CategoryDeclaration
    linkCategory : C3S2.CategoryOfEtaleSpaces.categoryStructure decl ≡ expCategory
    status : B.Bool

mkCategoryOfEtaleSpacesAdapter :
  (d : C3S2.CategoryOfEtaleSpaces) →
  (cat : C1S3.CategoryDeclaration) →
  (pcat : C3S2.CategoryOfEtaleSpaces.categoryStructure d ≡ cat) →
  (f : ⊤ → C3S2.CategoryOfEtaleSpaces) →
  CategoryOfEtaleSpacesAdapter
mkCategoryOfEtaleSpacesAdapter d cat pcat f =
  record { decl = d ; expCategory = cat ; linkCategory = pcat ; status = B.true }

categoryOfEtaleSpacesCategorical : CategoryOfEtaleSpacesAdapter → CategoricalAdapter {lsuc lzero} C3S2.CategoryOfEtaleSpaces
categoryOfEtaleSpacesCategorical adapt = mkCategoricalAdapter C3S2.CategoryOfEtaleSpaces (λ _ → CategoryOfEtaleSpacesAdapter.decl adapt)

isFilledCategoryOfEtaleSpaces : CategoryOfEtaleSpacesAdapter → B.Bool
isFilledCategoryOfEtaleSpaces a = CategoryOfEtaleSpacesAdapter.status a

-- Stalk constructor
record StalkConstructorAdapter : Set₁ where
  field
    decl : C3S2.StalkConstructor
    expPresheaf : C3S2.PresheafOnLocale
    linkPresheaf : C3S2.StalkConstructor.presheaf decl ≡ expPresheaf
    status : B.Bool

mkStalkConstructorAdapter :
  (d : C3S2.StalkConstructor) →
  (psh : C3S2.PresheafOnLocale) →
  (ppsh : C3S2.StalkConstructor.presheaf d ≡ psh) →
  (f : ⊤ → C3S2.StalkConstructor) →
  StalkConstructorAdapter
mkStalkConstructorAdapter d psh ppsh f =
  record { decl = d ; expPresheaf = psh ; linkPresheaf = ppsh ; status = B.true }

stalkConstructorCategorical : StalkConstructorAdapter → CategoricalAdapter {lsuc lzero} C3S2.StalkConstructor
stalkConstructorCategorical adapt = mkCategoricalAdapter C3S2.StalkConstructor (λ _ → StalkConstructorAdapter.decl adapt)

isFilledStalkConstructor : StalkConstructorAdapter → B.Bool
isFilledStalkConstructor a = StalkConstructorAdapter.status a

-- Total space of stalks
record TotalSpaceOfStalksAdapter : Set₁ where
  field
    decl : C3S2.TotalSpaceOfStalks
    expPresheaf : C3S2.PresheafOnLocale
    linkPresheaf : C3S2.TotalSpaceOfStalks.presheaf decl ≡ expPresheaf
    status : B.Bool

mkTotalSpaceOfStalksAdapter :
  (d : C3S2.TotalSpaceOfStalks) →
  (psh : C3S2.PresheafOnLocale) →
  (ppsh : C3S2.TotalSpaceOfStalks.presheaf d ≡ psh) →
  (f : ⊤ → C3S2.TotalSpaceOfStalks) →
  TotalSpaceOfStalksAdapter
mkTotalSpaceOfStalksAdapter d psh ppsh f =
  record { decl = d ; expPresheaf = psh ; linkPresheaf = ppsh ; status = B.true }

totalSpaceOfStalksCategorical : TotalSpaceOfStalksAdapter → CategoricalAdapter {lsuc lzero} C3S2.TotalSpaceOfStalks
totalSpaceOfStalksCategorical adapt = mkCategoricalAdapter C3S2.TotalSpaceOfStalks (λ _ → TotalSpaceOfStalksAdapter.decl adapt)

isFilledTotalSpaceOfStalks : TotalSpaceOfStalksAdapter → B.Bool
isFilledTotalSpaceOfStalks a = TotalSpaceOfStalksAdapter.status a

-- Sheaf of sections functor
record SheafOfSectionsFunctorAdapter : Set₁ where
  field
    decl : C3S2.SheafOfSectionsFunctor
    expEtale : C3S2.EtaleSpaceOver
    expSheaf : C3S2.SheafOnLocaleDeclaration
    linkEtale : C3S2.SheafOfSectionsFunctor.etaleSpace decl ≡ expEtale
    linkSheaf : C3S2.SheafOfSectionsFunctor.isSheaf decl ≡ expSheaf
    status : B.Bool

mkSheafOfSectionsFunctorAdapter :
  (d : C3S2.SheafOfSectionsFunctor) →
  (et : C3S2.EtaleSpaceOver) →
  (sh : C3S2.SheafOnLocaleDeclaration) →
  (pet : C3S2.SheafOfSectionsFunctor.etaleSpace d ≡ et) →
  (psh : C3S2.SheafOfSectionsFunctor.isSheaf d ≡ sh) →
  (f : ⊤ → C3S2.SheafOfSectionsFunctor) →
  SheafOfSectionsFunctorAdapter
mkSheafOfSectionsFunctorAdapter d et sh pet psh f =
  record { decl = d ; expEtale = et ; expSheaf = sh
         ; linkEtale = pet ; linkSheaf = psh ; status = B.true }

sheafOfSectionsFunctorCategorical : SheafOfSectionsFunctorAdapter → CategoricalAdapter {lsuc lzero} C3S2.SheafOfSectionsFunctor
sheafOfSectionsFunctorCategorical adapt = mkCategoricalAdapter C3S2.SheafOfSectionsFunctor (λ _ → SheafOfSectionsFunctorAdapter.decl adapt)

isFilledSheafOfSectionsFunctor : SheafOfSectionsFunctorAdapter → B.Bool
isFilledSheafOfSectionsFunctor a = SheafOfSectionsFunctorAdapter.status a

-- Sheaf-étale equivalence theorem
record SheafEtaleEquivalenceTheoremAdapter : Set₁ where
  field
    decl : C3S2.SheafEtaleEquivalenceTheorem
    expSheafCat : C3S2.CategoryOfSheaves
    expEtaleCat : C3S2.CategoryOfEtaleSpaces
    expStalksF : M.Identifier
    expSectionsF : M.Identifier
    linkSheafCat : C3S2.SheafEtaleEquivalenceTheorem.sheafCategory decl ≡ expSheafCat
    linkEtaleCat : C3S2.SheafEtaleEquivalenceTheorem.etaleCategory decl ≡ expEtaleCat
    linkStalksF : C3S2.SheafEtaleEquivalenceTheorem.stalksToEtaleFunctor decl ≡ expStalksF
    linkSectionsF : C3S2.SheafEtaleEquivalenceTheorem.sectionsToSheafFunctor decl ≡ expSectionsF
    status : B.Bool

mkSheafEtaleEquivalenceTheoremAdapter :
  (d : C3S2.SheafEtaleEquivalenceTheorem) →
  (sc : C3S2.CategoryOfSheaves) →
  (ec : C3S2.CategoryOfEtaleSpaces) →
  (sf : M.Identifier) →
  (tf : M.Identifier) →
  (psc : C3S2.SheafEtaleEquivalenceTheorem.sheafCategory d ≡ sc) →
  (pec : C3S2.SheafEtaleEquivalenceTheorem.etaleCategory d ≡ ec) →
  (psf : C3S2.SheafEtaleEquivalenceTheorem.stalksToEtaleFunctor d ≡ sf) →
  (ptf : C3S2.SheafEtaleEquivalenceTheorem.sectionsToSheafFunctor d ≡ tf) →
  (f : ⊤ → C3S2.SheafEtaleEquivalenceTheorem) →
  SheafEtaleEquivalenceTheoremAdapter
mkSheafEtaleEquivalenceTheoremAdapter d sc ec sf tf psc pec psf ptf f =
  record { decl = d ; expSheafCat = sc ; expEtaleCat = ec
         ; expStalksF = sf ; expSectionsF = tf
         ; linkSheafCat = psc ; linkEtaleCat = pec
         ; linkStalksF = psf ; linkSectionsF = ptf ; status = B.true }

sheafEtaleEquivalenceTheoremCategorical : SheafEtaleEquivalenceTheoremAdapter → CategoricalAdapter {lsuc lzero} C3S2.SheafEtaleEquivalenceTheorem
sheafEtaleEquivalenceTheoremCategorical adapt = mkCategoricalAdapter C3S2.SheafEtaleEquivalenceTheorem (λ _ → SheafEtaleEquivalenceTheoremAdapter.decl adapt)

isFilledSheafEtaleEquivalenceTheorem : SheafEtaleEquivalenceTheoremAdapter → B.Bool
isFilledSheafEtaleEquivalenceTheorem a = SheafEtaleEquivalenceTheoremAdapter.status a

-- Direct image functor
record DirectImageFunctorLocaleAdapter : Set₁ where
  field
    decl : C3S2.DirectImageFunctorLocale
    expFunctor : M.Identifier
    linkFunctor : C3S2.DirectImageFunctorLocale.underlyingFunctor decl ≡ expFunctor
    status : B.Bool

mkDirectImageFunctorLocaleAdapter :
  (d : C3S2.DirectImageFunctorLocale) →
  (f : M.Identifier) →
  (pf : C3S2.DirectImageFunctorLocale.underlyingFunctor d ≡ f) →
  (morph : ⊤ → C3S2.DirectImageFunctorLocale) →
  DirectImageFunctorLocaleAdapter
mkDirectImageFunctorLocaleAdapter d f pf morph =
  record { decl = d ; expFunctor = f ; linkFunctor = pf ; status = B.true }

directImageFunctorLocaleCategorical : DirectImageFunctorLocaleAdapter → CategoricalAdapter {lsuc lzero} C3S2.DirectImageFunctorLocale
directImageFunctorLocaleCategorical adapt = mkCategoricalAdapter C3S2.DirectImageFunctorLocale (λ _ → DirectImageFunctorLocaleAdapter.decl adapt)

isFilledDirectImageFunctorLocale : DirectImageFunctorLocaleAdapter → B.Bool
isFilledDirectImageFunctorLocale a = DirectImageFunctorLocaleAdapter.status a

-- Inverse image functor
record InverseImageFunctorLocaleAdapter : Set₁ where
  field
    decl : C3S2.InverseImageFunctorLocale
    expFunctor : M.Identifier
    linkFunctor : C3S2.InverseImageFunctorLocale.underlyingFunctor decl ≡ expFunctor
    status : B.Bool

mkInverseImageFunctorLocaleAdapter :
  (d : C3S2.InverseImageFunctorLocale) →
  (f : M.Identifier) →
  (pf : C3S2.InverseImageFunctorLocale.underlyingFunctor d ≡ f) →
  (morph : ⊤ → C3S2.InverseImageFunctorLocale) →
  InverseImageFunctorLocaleAdapter
mkInverseImageFunctorLocaleAdapter d f pf morph =
  record { decl = d ; expFunctor = f ; linkFunctor = pf ; status = B.true }

inverseImageFunctorLocaleCategorical : InverseImageFunctorLocaleAdapter → CategoricalAdapter {lsuc lzero} C3S2.InverseImageFunctorLocale
inverseImageFunctorLocaleCategorical adapt = mkCategoricalAdapter C3S2.InverseImageFunctorLocale (λ _ → InverseImageFunctorLocaleAdapter.decl adapt)

isFilledInverseImageFunctorLocale : InverseImageFunctorLocaleAdapter → B.Bool
isFilledInverseImageFunctorLocale a = InverseImageFunctorLocaleAdapter.status a

-- Change of base adjunction theorem
record LocaleChangeOfBaseAdjunctionTheoremAdapter : Set₁ where
  field
    decl : C3S2.LocaleChangeOfBaseAdjunctionTheorem
    expInverse : C3S2.InverseImageFunctorLocale
    expDirect : C3S2.DirectImageFunctorLocale
    expAdj : M.Identifier
    linkInverse : C3S2.LocaleChangeOfBaseAdjunctionTheorem.inverseImageFunctor decl ≡ expInverse
    linkDirect : C3S2.LocaleChangeOfBaseAdjunctionTheorem.directImageFunctor decl ≡ expDirect
    linkAdj : C3S2.LocaleChangeOfBaseAdjunctionTheorem.adjunction decl ≡ expAdj
    status : B.Bool

mkLocaleChangeOfBaseAdjunctionTheoremAdapter :
  (d : C3S2.LocaleChangeOfBaseAdjunctionTheorem) →
  (inv : C3S2.InverseImageFunctorLocale) →
  (dir : C3S2.DirectImageFunctorLocale) →
  (adj : M.Identifier) →
  (pinv : C3S2.LocaleChangeOfBaseAdjunctionTheorem.inverseImageFunctor d ≡ inv) →
  (pdir : C3S2.LocaleChangeOfBaseAdjunctionTheorem.directImageFunctor d ≡ dir) →
  (padj : C3S2.LocaleChangeOfBaseAdjunctionTheorem.adjunction d ≡ adj) →
  (f : ⊤ → C3S2.LocaleChangeOfBaseAdjunctionTheorem) →
  LocaleChangeOfBaseAdjunctionTheoremAdapter
mkLocaleChangeOfBaseAdjunctionTheoremAdapter d inv dir adj pinv pdir padj f =
  record { decl = d ; expInverse = inv ; expDirect = dir ; expAdj = adj
         ; linkInverse = pinv ; linkDirect = pdir ; linkAdj = padj ; status = B.true }

localeChangeOfBaseAdjunctionTheoremCategorical : LocaleChangeOfBaseAdjunctionTheoremAdapter → CategoricalAdapter {lsuc lzero} C3S2.LocaleChangeOfBaseAdjunctionTheorem
localeChangeOfBaseAdjunctionTheoremCategorical adapt = mkCategoricalAdapter C3S2.LocaleChangeOfBaseAdjunctionTheorem (λ _ → LocaleChangeOfBaseAdjunctionTheoremAdapter.decl adapt)

isFilledLocaleChangeOfBaseAdjunctionTheorem : LocaleChangeOfBaseAdjunctionTheoremAdapter → B.Bool
isFilledLocaleChangeOfBaseAdjunctionTheorem a = LocaleChangeOfBaseAdjunctionTheoremAdapter.status a

-- Étale morphism induces sheaf equivalence theorem
record EtaleMorphismInducesSheafEquivalenceTheoremAdapter : Set₁ where
  field
    decl : C3S2.EtaleMorphismInducesSheafEquivalenceTheorem
    expInverse : C3S2.InverseImageFunctorLocale
    linkInverse : C3S2.EtaleMorphismInducesSheafEquivalenceTheorem.inverseImageFunctor decl ≡ expInverse
    status : B.Bool

mkEtaleMorphismInducesSheafEquivalenceTheoremAdapter :
  (d : C3S2.EtaleMorphismInducesSheafEquivalenceTheorem) →
  (inv : C3S2.InverseImageFunctorLocale) →
  (pinv : C3S2.EtaleMorphismInducesSheafEquivalenceTheorem.inverseImageFunctor d ≡ inv) →
  (f : ⊤ → C3S2.EtaleMorphismInducesSheafEquivalenceTheorem) →
  EtaleMorphismInducesSheafEquivalenceTheoremAdapter
mkEtaleMorphismInducesSheafEquivalenceTheoremAdapter d inv pinv f =
  record { decl = d ; expInverse = inv ; linkInverse = pinv ; status = B.true }

etaleMorphismInducesSheafEquivalenceTheoremCategorical : EtaleMorphismInducesSheafEquivalenceTheoremAdapter → CategoricalAdapter {lsuc lzero} C3S2.EtaleMorphismInducesSheafEquivalenceTheorem
etaleMorphismInducesSheafEquivalenceTheoremCategorical adapt = mkCategoricalAdapter C3S2.EtaleMorphismInducesSheafEquivalenceTheorem (λ _ → EtaleMorphismInducesSheafEquivalenceTheoremAdapter.decl adapt)

isFilledEtaleMorphismInducesSheafEquivalenceTheorem : EtaleMorphismInducesSheafEquivalenceTheoremAdapter → B.Bool
isFilledEtaleMorphismInducesSheafEquivalenceTheorem a = EtaleMorphismInducesSheafEquivalenceTheoremAdapter.status a

-- Omega set declaration (verified)
record OmegaSetAdapter : Set₁ where
  field
    decl : C3S2.OmegaSetDeclarationVerified
    expData : C3S2.OmegaSetData
    linkData : C3S2.OmegaSetDeclarationVerified.dataOmegaSet decl ≡ expData
    status : B.Bool

mkOmegaSetAdapter :
  (d : C3S2.OmegaSetDeclarationVerified) →
  (dat : C3S2.OmegaSetData) →
  (plink : C3S2.OmegaSetDeclarationVerified.dataOmegaSet d ≡ dat) →
  OmegaSetAdapter
mkOmegaSetAdapter d dat plink =
  record { decl = d ; expData = dat ; linkData = plink ; status = B.true }

isFilledOmegaSet : OmegaSetAdapter → B.Bool
isFilledOmegaSet a = OmegaSetAdapter.status a

-- Ω-sets are complete Ω-sets theorem
record SheavesAreCompleteOmegaSetsRefinedTheoremAdapter : Set₁ where
  field
    decl : C3S2.SheavesAreCompleteOmegaSetsRefinedTheorem
    expSheafCat : C3S2.CategoryOfSheaves
    expOmegaCat : C3S2.CategoryOfOmegaSets
    expFunctorA : C3S2.FunctorSheafToOmegaSet
    expFunctorS : C3S2.FunctorOmegaSetToSheaf
    linkSheafCat : C3S2.SheavesAreCompleteOmegaSetsRefinedTheorem.sheafCategory decl ≡ expSheafCat
    linkOmegaCat : C3S2.SheavesAreCompleteOmegaSetsRefinedTheorem.completeOmegaSetCategory decl ≡ expOmegaCat
    linkFunctorA : C3S2.SheavesAreCompleteOmegaSetsRefinedTheorem.functorA decl ≡ expFunctorA
    linkFunctorS : C3S2.SheavesAreCompleteOmegaSetsRefinedTheorem.functorS decl ≡ expFunctorS
    status : B.Bool

mkSheavesAreCompleteOmegaSetsRefinedTheoremAdapter :
  (d : C3S2.SheavesAreCompleteOmegaSetsRefinedTheorem) →
  (sc : C3S2.CategoryOfSheaves) →
  (oc : C3S2.CategoryOfOmegaSets) →
  (fa : C3S2.FunctorSheafToOmegaSet) →
  (fs : C3S2.FunctorOmegaSetToSheaf) →
  (psc : C3S2.SheavesAreCompleteOmegaSetsRefinedTheorem.sheafCategory d ≡ sc) →
  (poc : C3S2.SheavesAreCompleteOmegaSetsRefinedTheorem.completeOmegaSetCategory d ≡ oc) →
  (pfa : C3S2.SheavesAreCompleteOmegaSetsRefinedTheorem.functorA d ≡ fa) →
  (pfs : C3S2.SheavesAreCompleteOmegaSetsRefinedTheorem.functorS d ≡ fs) →
  (f : ⊤ → C3S2.SheavesAreCompleteOmegaSetsRefinedTheorem) →
  SheavesAreCompleteOmegaSetsRefinedTheoremAdapter
mkSheavesAreCompleteOmegaSetsRefinedTheoremAdapter d sc oc fa fs psc poc pfa pfs f =
  record { decl = d ; expSheafCat = sc ; expOmegaCat = oc
         ; expFunctorA = fa ; expFunctorS = fs
         ; linkSheafCat = psc ; linkOmegaCat = poc
         ; linkFunctorA = pfa ; linkFunctorS = pfs ; status = B.true }

sheavesAreCompleteOmegaSetsRefinedTheoremCategorical : SheavesAreCompleteOmegaSetsRefinedTheoremAdapter → CategoricalAdapter {lsuc lzero} C3S2.SheavesAreCompleteOmegaSetsRefinedTheorem
sheavesAreCompleteOmegaSetsRefinedTheoremCategorical adapt = mkCategoricalAdapter C3S2.SheavesAreCompleteOmegaSetsRefinedTheorem (λ _ → SheavesAreCompleteOmegaSetsRefinedTheoremAdapter.decl adapt)

isFilledSheavesAreCompleteOmegaSetsRefinedTheorem : SheavesAreCompleteOmegaSetsRefinedTheoremAdapter → B.Bool
isFilledSheavesAreCompleteOmegaSetsRefinedTheorem a = SheavesAreCompleteOmegaSetsRefinedTheoremAdapter.status a

-- Sheaf of rings
record SheafOfRingsAdapter : Set₁ where
  field
    decl : C3S2.SheafOfRings
    expSheaf : C3S2.SheafOnLocaleDeclaration
    linkSheaf : C3S2.SheafOfRings.underlyingSheaf decl ≡ expSheaf
    status : B.Bool

mkSheafOfRingsAdapter :
  (d : C3S2.SheafOfRings) →
  (sh : C3S2.SheafOnLocaleDeclaration) →
  (psh : C3S2.SheafOfRings.underlyingSheaf d ≡ sh) →
  (f : ⊤ → C3S2.SheafOfRings) →
  SheafOfRingsAdapter
mkSheafOfRingsAdapter d sh psh f =
  record { decl = d ; expSheaf = sh ; linkSheaf = psh ; status = B.true }

sheafOfRingsCategorical : SheafOfRingsAdapter → CategoricalAdapter {lsuc lzero} C3S2.SheafOfRings
sheafOfRingsCategorical adapt = mkCategoricalAdapter C3S2.SheafOfRings (λ _ → SheafOfRingsAdapter.decl adapt)

isFilledSheafOfRings : SheafOfRingsAdapter → B.Bool
isFilledSheafOfRings a = SheafOfRingsAdapter.status a

-- Sheaf of O-modules
record SheafOfOModulesAdapter : Set₁ where
  field
    decl : C3S2.SheafOfOModules
    expRingSheaf : C3S2.SheafOfRings
    expModSheaf : C3S2.SheafOnLocaleDeclaration
    linkRingSheaf : C3S2.SheafOfOModules.sheafOfRings decl ≡ expRingSheaf
    linkModSheaf : C3S2.SheafOfOModules.underlyingSheaf decl ≡ expModSheaf
    status : B.Bool

mkSheafOfOModulesAdapter :
  (d : C3S2.SheafOfOModules) →
  (rs : C3S2.SheafOfRings) →
  (ms : C3S2.SheafOnLocaleDeclaration) →
  (prs : C3S2.SheafOfOModules.sheafOfRings d ≡ rs) →
  (pms : C3S2.SheafOfOModules.underlyingSheaf d ≡ ms) →
  (f : ⊤ → C3S2.SheafOfOModules) →
  SheafOfOModulesAdapter
mkSheafOfOModulesAdapter d rs ms prs pms f =
  record { decl = d ; expRingSheaf = rs ; expModSheaf = ms
         ; linkRingSheaf = prs ; linkModSheaf = pms ; status = B.true }

sheafOfOModulesCategorical : SheafOfOModulesAdapter → CategoricalAdapter {lsuc lzero} C3S2.SheafOfOModules
sheafOfOModulesCategorical adapt = mkCategoricalAdapter C3S2.SheafOfOModules (λ _ → SheafOfOModulesAdapter.decl adapt)

isFilledSheafOfOModules : SheafOfOModulesAdapter → B.Bool
isFilledSheafOfOModules a = SheafOfOModulesAdapter.status a

-- Category of O-modules is abelian
record CategoryOfOModulesIsAbelianCorollaryAdapter : Set₁ where
  field
    decl : C3S2.CategoryOfOModulesIsAbelianCorollary
    expRingSheaf : C3S2.SheafOfRings
    expCategory : C1S3.CategoryDeclaration
    linkRingSheaf : C3S2.CategoryOfOModulesIsAbelianCorollary.sheafOfRings decl ≡ expRingSheaf
    linkCategory : C3S2.CategoryOfOModulesIsAbelianCorollary.categoryOfOModules decl ≡ expCategory
    status : B.Bool

mkCategoryOfOModulesIsAbelianCorollaryAdapter :
  (d : C3S2.CategoryOfOModulesIsAbelianCorollary) →
  (rs : C3S2.SheafOfRings) →
  (cat : C1S3.CategoryDeclaration) →
  (prs : C3S2.CategoryOfOModulesIsAbelianCorollary.sheafOfRings d ≡ rs) →
  (pcat : C3S2.CategoryOfOModulesIsAbelianCorollary.categoryOfOModules d ≡ cat) →
  (f : ⊤ → C3S2.CategoryOfOModulesIsAbelianCorollary) →
  CategoryOfOModulesIsAbelianCorollaryAdapter
mkCategoryOfOModulesIsAbelianCorollaryAdapter d rs cat prs pcat f =
  record { decl = d ; expRingSheaf = rs ; expCategory = cat
         ; linkRingSheaf = prs ; linkCategory = pcat ; status = B.true }

categoryOfOModulesIsAbelianCorollaryCategorical : CategoryOfOModulesIsAbelianCorollaryAdapter → CategoricalAdapter {lsuc lzero} C3S2.CategoryOfOModulesIsAbelianCorollary
categoryOfOModulesIsAbelianCorollaryCategorical adapt = mkCategoricalAdapter C3S2.CategoryOfOModulesIsAbelianCorollary (λ _ → CategoryOfOModulesIsAbelianCorollaryAdapter.decl adapt)

isFilledCategoryOfOModulesIsAbelianCorollary : CategoryOfOModulesIsAbelianCorollaryAdapter → B.Bool
isFilledCategoryOfOModulesIsAbelianCorollary a = CategoryOfOModulesIsAbelianCorollaryAdapter.status a
