{-# OPTIONS --without-K #-}

-- Tests.ToposObligationAdapters: Adapters specific to topos theory to decouple from global adapters

module Tests.ToposObligationAdapters where

open import Core.Phase using (Bool; true; false)
open import Agda.Builtin.Equality using (_≡_)
open import Agda.Builtin.Unit using (⊤)
open import Agda.Primitive using (Level; lzero; lsuc)
open import Metamodel as M
import Chapter1.Level1sub3 as C1S3
import Chapter3.Level3sub2 as C3S2
open import Core.CategoricalAdapter

-- Presheaf on locale
-- | Adapter capturing a presheaf on a locale and whether it is populated.
record PresheafOnLocaleAdapter : Set₁ where
  field
    decl : C3S2.PresheafOnLocale
    status : Bool

-- | Mark a presheaf on locale as satisfied.
mkPresheafOnLocaleAdapter : 
  C3S2.PresheafOnLocale → 
  (⊤ → C3S2.PresheafOnLocale) →
  PresheafOnLocaleAdapter
mkPresheafOnLocaleAdapter d f = record 
  { decl = d 
  ; status = true 
  }

-- Categorical view (separate from adapter record to avoid universe issues)
-- | Categorical adapter view for presheaves on a locale.
presheafCategorical : PresheafOnLocaleAdapter → CategoricalAdapter {lsuc lzero} C3S2.PresheafOnLocale
presheafCategorical adapt = mkCategoricalAdapter C3S2.PresheafOnLocale (λ _ → PresheafOnLocaleAdapter.decl adapt)

-- | Check if the presheaf adapter is populated.
isFilledPresheafOnLocale : PresheafOnLocaleAdapter → Bool
isFilledPresheafOnLocale a = PresheafOnLocaleAdapter.status a

-- Sheaf gluing axiom
-- | Adapter for the sheaf gluing axiom with explicit presheaf link.
record SheafGluingAxiomAdapter : Set₁ where
  field
    decl : C3S2.SheafGluingAxiom
    expPresheaf : C3S2.PresheafOnLocale
    linkPresheaf : C3S2.SheafGluingAxiom.presheaf decl ≡ expPresheaf
    status : Bool

-- | Mark the sheaf gluing axiom as satisfied with a concrete presheaf.
mkSheafGluingAxiomAdapter :
  (d : C3S2.SheafGluingAxiom) →
  (psh : C3S2.PresheafOnLocale) →
  (ppsh : C3S2.SheafGluingAxiom.presheaf d ≡ psh) →
  (f : ⊤ → C3S2.SheafGluingAxiom) →
  SheafGluingAxiomAdapter
mkSheafGluingAxiomAdapter d psh ppsh f =
  -- | Sheaf gluing adapter payload.
  record { decl = d ; expPresheaf = psh ; linkPresheaf = ppsh ; status = true }

-- | Categorical adapter for the sheaf gluing axiom.
sheafGluingCategorical : SheafGluingAxiomAdapter → CategoricalAdapter {lsuc lzero} C3S2.SheafGluingAxiom
sheafGluingCategorical adapt = mkCategoricalAdapter C3S2.SheafGluingAxiom (λ _ → SheafGluingAxiomAdapter.decl adapt)

-- | Check if the sheaf gluing adapter is populated.
isFilledSheafGluingAxiom : SheafGluingAxiomAdapter → Bool
isFilledSheafGluingAxiom a = SheafGluingAxiomAdapter.status a

-- Sheaf on locale
-- | Adapter connecting a sheaf declaration to its underlying presheaf.
record SheafOnLocaleAdapter : Set₁ where
  field
    decl : C3S2.SheafOnLocaleDeclaration
    expPresheaf : C3S2.PresheafOnLocale
    linkPresheaf : C3S2.SheafOnLocaleDeclaration.underlyingPresheaf decl ≡ expPresheaf
    status : Bool

-- | Mark a sheaf-on-locale declaration as filled.
mkSheafOnLocaleAdapter :
  (d : C3S2.SheafOnLocaleDeclaration) →
  (psh : C3S2.PresheafOnLocale) →
  (ppsh : C3S2.SheafOnLocaleDeclaration.underlyingPresheaf d ≡ psh) →
  (f : ⊤ → C3S2.SheafOnLocaleDeclaration) →
  SheafOnLocaleAdapter
mkSheafOnLocaleAdapter d psh ppsh f =
  -- | Sheaf-on-locale adapter payload.
  record { decl = d ; expPresheaf = psh ; linkPresheaf = ppsh ; status = true }

-- | Categorical adapter for sheaves on a locale.
sheafOnLocaleCategorical : SheafOnLocaleAdapter → CategoricalAdapter {lsuc lzero} C3S2.SheafOnLocaleDeclaration
sheafOnLocaleCategorical adapt = mkCategoricalAdapter C3S2.SheafOnLocaleDeclaration (λ _ → SheafOnLocaleAdapter.decl adapt)

-- | Check if the sheaf-on-locale adapter is populated.
isFilledSheafOnLocale : SheafOnLocaleAdapter → Bool
isFilledSheafOnLocale a = SheafOnLocaleAdapter.status a

-- Category of sheaves
-- | Adapter linking a category of sheaves to its underlying category.
record CategoryOfSheavesAdapter : Set₁ where
  field
    decl : C3S2.CategoryOfSheaves
    expCategory : C1S3.CategoryDeclaration
    linkCategory : C3S2.CategoryOfSheaves.underlyingCategory decl ≡ expCategory
    status : Bool

-- | Mark a category-of-sheaves declaration as satisfied.
mkCategoryOfSheavesAdapter :
  (d : C3S2.CategoryOfSheaves) →
  (cat : C1S3.CategoryDeclaration) →
  (pcat : C3S2.CategoryOfSheaves.underlyingCategory d ≡ cat) →
  (f : ⊤ → C3S2.CategoryOfSheaves) →
  CategoryOfSheavesAdapter
mkCategoryOfSheavesAdapter d cat pcat f =
  -- | Category-of-sheaves adapter payload.
  record { decl = d ; expCategory = cat ; linkCategory = pcat ; status = true }

-- | Categorical adapter for categories of sheaves.
categoryOfSheavesCategorical : CategoryOfSheavesAdapter → CategoricalAdapter {lsuc lzero} C3S2.CategoryOfSheaves
categoryOfSheavesCategorical adapt = mkCategoricalAdapter C3S2.CategoryOfSheaves (λ _ → CategoryOfSheavesAdapter.decl adapt)

-- | Check if the category-of-sheaves adapter is populated.
isFilledCategoryOfSheaves : CategoryOfSheavesAdapter → Bool
isFilledCategoryOfSheaves a = CategoryOfSheavesAdapter.status a

-- Grothendieck topos declaration
-- | Adapter linking a Grothendieck topos declaration to its base category.
record GrothendieckToposAdapter : Set₁ where
  field
    decl : C3S2.GrothendieckToposDeclaration
    expCategory : C1S3.CategoryDeclaration
    linkCategory : C3S2.GrothendieckToposDeclaration.category decl ≡ expCategory
    status : Bool

-- | Mark a Grothendieck topos declaration as satisfied.
mkGrothendieckToposAdapter :
  (d : C3S2.GrothendieckToposDeclaration) →
  (cat : C1S3.CategoryDeclaration) →
  (pcat : C3S2.GrothendieckToposDeclaration.category d ≡ cat) →
  (f : ⊤ → C3S2.GrothendieckToposDeclaration) →
  GrothendieckToposAdapter
mkGrothendieckToposAdapter d cat pcat f =
  -- | Grothendieck topos adapter payload.
  record { decl = d ; expCategory = cat ; linkCategory = pcat ; status = true }

-- | Categorical adapter for Grothendieck topos declarations.
grothendieckToposCategorical : GrothendieckToposAdapter → CategoricalAdapter {lsuc lzero} C3S2.GrothendieckToposDeclaration
grothendieckToposCategorical adapt = mkCategoricalAdapter C3S2.GrothendieckToposDeclaration (λ _ → GrothendieckToposAdapter.decl adapt)

-- | Check if the Grothendieck topos adapter is populated.
isFilledGrothendieckTopos : GrothendieckToposAdapter → Bool
isFilledGrothendieckTopos a = GrothendieckToposAdapter.status a

-- CategoryOfSheavesIsAToposTheorem
-- | Adapter for the theorem that a category of sheaves is a Grothendieck topos.
record CategoryOfSheavesIsAToposTheoremAdapter : Set₁ where
  field
    decl : C3S2.CategoryOfSheavesIsAToposTheorem
    expSheafCat : C3S2.CategoryOfSheaves
    expTopos : C3S2.GrothendieckToposDeclaration
    linkSheafCat : C3S2.CategoryOfSheavesIsAToposTheorem.sheafCategory decl ≡ expSheafCat
    linkTopos : C3S2.CategoryOfSheavesIsAToposTheorem.isGrothendieckTopos decl ≡ expTopos
    status : Bool

-- | Mark the sheaf-category/topos theorem adapter as satisfied.
mkCategoryOfSheavesIsAToposTheoremAdapter :
  (d : C3S2.CategoryOfSheavesIsAToposTheorem) →
  (sc : C3S2.CategoryOfSheaves) →
  (tp : C3S2.GrothendieckToposDeclaration) →
  (psc : C3S2.CategoryOfSheavesIsAToposTheorem.sheafCategory d ≡ sc) →
  (ptp : C3S2.CategoryOfSheavesIsAToposTheorem.isGrothendieckTopos d ≡ tp) →
  (f : ⊤ → C3S2.CategoryOfSheavesIsAToposTheorem) →
  CategoryOfSheavesIsAToposTheoremAdapter
mkCategoryOfSheavesIsAToposTheoremAdapter d sc tp psc ptp f =
  -- | Sheaf-category/topos theorem adapter payload.
  record { decl = d ; expSheafCat = sc ; expTopos = tp
         ; linkSheafCat = psc ; linkTopos = ptp ; status = true }

-- | Categorical adapter view for the sheaf/topos equivalence theorem.
categoryOfSheavesIsAToposTheoremCategorical : CategoryOfSheavesIsAToposTheoremAdapter → CategoricalAdapter {lsuc lzero} C3S2.CategoryOfSheavesIsAToposTheorem
categoryOfSheavesIsAToposTheoremCategorical adapt = mkCategoricalAdapter C3S2.CategoryOfSheavesIsAToposTheorem (λ _ → CategoryOfSheavesIsAToposTheoremAdapter.decl adapt)

-- | Check if the topos theorem adapter is populated.
isFilledCategoryOfSheavesIsAToposTheorem : CategoryOfSheavesIsAToposTheoremAdapter → Bool
isFilledCategoryOfSheavesIsAToposTheorem a = CategoryOfSheavesIsAToposTheoremAdapter.status a

-- Exponential object in sheaf category
-- | Adapter for exponential objects in a sheaf category.
record ExponentialObjectSheafAdapter : Set₁ where
  field
    decl : C3S2.ExponentialObjectSheaf
    expBase : C3S2.SheafOnLocaleDeclaration
    expExponent : C3S2.SheafOnLocaleDeclaration
    linkBase : C3S2.ExponentialObjectSheaf.baseSheaf decl ≡ expBase
    linkExponent : C3S2.ExponentialObjectSheaf.exponentSheaf decl ≡ expExponent
    status : Bool

-- | Mark an exponential-object construction as satisfied.
mkExponentialObjectSheafAdapter :
  (d : C3S2.ExponentialObjectSheaf) →
  (b : C3S2.SheafOnLocaleDeclaration) →
  (e : C3S2.SheafOnLocaleDeclaration) →
  (pb : C3S2.ExponentialObjectSheaf.baseSheaf d ≡ b) →
  (pe : C3S2.ExponentialObjectSheaf.exponentSheaf d ≡ e) →
  (f : ⊤ → C3S2.ExponentialObjectSheaf) →
  ExponentialObjectSheafAdapter
mkExponentialObjectSheafAdapter d b e pb pe f =
  -- | Exponential object adapter payload.
  record { decl = d ; expBase = b ; expExponent = e
         ; linkBase = pb ; linkExponent = pe ; status = true }

-- | Categorical adapter for exponential sheaf objects.
exponentialObjectSheafCategorical : ExponentialObjectSheafAdapter → CategoricalAdapter {lsuc lzero} C3S2.ExponentialObjectSheaf
exponentialObjectSheafCategorical adapt = mkCategoricalAdapter C3S2.ExponentialObjectSheaf (λ _ → ExponentialObjectSheafAdapter.decl adapt)

-- | Check if the exponential-object adapter is populated.
isFilledExponentialObjectSheaf : ExponentialObjectSheafAdapter → Bool
isFilledExponentialObjectSheaf a = ExponentialObjectSheafAdapter.status a

-- Subobject classifier
-- | Adapter for the subobject classifier axiom.
record SubobjectClassifierAxiomAdapter : Set₁ where
  field
    decl : C3S2.SubobjectClassifierAxiom
    expCharMap : C3S2.CharacteristicMapConstructor
    linkCharMap : C3S2.SubobjectClassifierAxiom.characteristicMap decl ≡ expCharMap
    status : Bool

-- | Mark a subobject classifier witness as satisfied.
mkSubobjectClassifierAxiomAdapter :
  (d : C3S2.SubobjectClassifierAxiom) →
  (cm : C3S2.CharacteristicMapConstructor) →
  (pcm : C3S2.SubobjectClassifierAxiom.characteristicMap d ≡ cm) →
  (f : ⊤ → C3S2.SubobjectClassifierAxiom) →
  SubobjectClassifierAxiomAdapter
mkSubobjectClassifierAxiomAdapter d cm pcm f =
  -- | Subobject classifier adapter payload.
  record { decl = d ; expCharMap = cm ; linkCharMap = pcm ; status = true }

subobjectClassifierAxiomCategorical : SubobjectClassifierAxiomAdapter → CategoricalAdapter {lsuc lzero} C3S2.SubobjectClassifierAxiom
subobjectClassifierAxiomCategorical adapt = mkCategoricalAdapter C3S2.SubobjectClassifierAxiom (λ _ → SubobjectClassifierAxiomAdapter.decl adapt)

isFilledSubobjectClassifierAxiom : SubobjectClassifierAxiomAdapter → Bool
isFilledSubobjectClassifierAxiom a = SubobjectClassifierAxiomAdapter.status a

-- Étale space
-- | Adapter for an étale space and its projection map.
record EtaleSpaceOverAdapter : Set₁ where
  field
    decl : C3S2.EtaleSpaceOver
    expProj : M.Identifier
    linkProj : C3S2.EtaleSpaceOver.projection decl ≡ expProj
    status : Bool

mkEtaleSpaceOverAdapter :
  (d : C3S2.EtaleSpaceOver) →
  (p : M.Identifier) →
  (pp : C3S2.EtaleSpaceOver.projection d ≡ p) →
  (f : ⊤ → C3S2.EtaleSpaceOver) →
  EtaleSpaceOverAdapter
mkEtaleSpaceOverAdapter d p pp f =
  -- | Étale-space adapter payload.
  record { decl = d ; expProj = p ; linkProj = pp ; status = true }

-- | Categorical adapter for étale spaces.
etaleSpaceOverCategorical : EtaleSpaceOverAdapter → CategoricalAdapter {lsuc lzero} C3S2.EtaleSpaceOver
etaleSpaceOverCategorical adapt = mkCategoricalAdapter C3S2.EtaleSpaceOver (λ _ → EtaleSpaceOverAdapter.decl adapt)

-- | Check if the étale-space adapter is populated.
isFilledEtaleSpaceOver : EtaleSpaceOverAdapter → Bool
isFilledEtaleSpaceOver a = EtaleSpaceOverAdapter.status a

-- Category of étale spaces
-- | Adapter linking a category of étale spaces to a category declaration.
record CategoryOfEtaleSpacesAdapter : Set₁ where
  field
    decl : C3S2.CategoryOfEtaleSpaces
    expCategory : C1S3.CategoryDeclaration
    linkCategory : C3S2.CategoryOfEtaleSpaces.categoryStructure decl ≡ expCategory
    status : Bool

mkCategoryOfEtaleSpacesAdapter :
  (d : C3S2.CategoryOfEtaleSpaces) →
  (cat : C1S3.CategoryDeclaration) →
  (pcat : C3S2.CategoryOfEtaleSpaces.categoryStructure d ≡ cat) →
  (f : ⊤ → C3S2.CategoryOfEtaleSpaces) →
  CategoryOfEtaleSpacesAdapter
mkCategoryOfEtaleSpacesAdapter d cat pcat f =
  -- | Category-of-étale-spaces adapter payload.
  record { decl = d ; expCategory = cat ; linkCategory = pcat ; status = true }

-- | Categorical adapter for categories of étale spaces.
categoryOfEtaleSpacesCategorical : CategoryOfEtaleSpacesAdapter → CategoricalAdapter {lsuc lzero} C3S2.CategoryOfEtaleSpaces
categoryOfEtaleSpacesCategorical adapt = mkCategoricalAdapter C3S2.CategoryOfEtaleSpaces (λ _ → CategoryOfEtaleSpacesAdapter.decl adapt)

-- | Check if the category-of-étale-spaces adapter is populated.
isFilledCategoryOfEtaleSpaces : CategoryOfEtaleSpacesAdapter → Bool
isFilledCategoryOfEtaleSpaces a = CategoryOfEtaleSpacesAdapter.status a

-- Stalk constructor
-- | Adapter for constructing stalks of a presheaf at a point.
record StalkConstructorAdapter : Set₁ where
  field
    decl : C3S2.StalkConstructor
    expPresheaf : C3S2.PresheafOnLocale
    linkPresheaf : C3S2.StalkConstructor.presheaf decl ≡ expPresheaf
    status : Bool

mkStalkConstructorAdapter :
  (d : C3S2.StalkConstructor) →
  (psh : C3S2.PresheafOnLocale) →
  (ppsh : C3S2.StalkConstructor.presheaf d ≡ psh) →
  (f : ⊤ → C3S2.StalkConstructor) →
  StalkConstructorAdapter
mkStalkConstructorAdapter d psh ppsh f =
  -- | Stalk constructor adapter payload.
  record { decl = d ; expPresheaf = psh ; linkPresheaf = ppsh ; status = true }

-- | Categorical adapter for stalk constructors.
stalkConstructorCategorical : StalkConstructorAdapter → CategoricalAdapter {lsuc lzero} C3S2.StalkConstructor
stalkConstructorCategorical adapt = mkCategoricalAdapter C3S2.StalkConstructor (λ _ → StalkConstructorAdapter.decl adapt)

-- | Check if the stalk-constructor adapter is populated.
isFilledStalkConstructor : StalkConstructorAdapter → Bool
isFilledStalkConstructor a = StalkConstructorAdapter.status a

-- Total space of stalks
-- | Adapter describing the total space assembled from stalks.
record TotalSpaceOfStalksAdapter : Set₁ where
  field
    decl : C3S2.TotalSpaceOfStalks
    expPresheaf : C3S2.PresheafOnLocale
    linkPresheaf : C3S2.TotalSpaceOfStalks.presheaf decl ≡ expPresheaf
    status : Bool

mkTotalSpaceOfStalksAdapter :
  (d : C3S2.TotalSpaceOfStalks) →
  (psh : C3S2.PresheafOnLocale) →
  (ppsh : C3S2.TotalSpaceOfStalks.presheaf d ≡ psh) →
  (f : ⊤ → C3S2.TotalSpaceOfStalks) →
  TotalSpaceOfStalksAdapter
mkTotalSpaceOfStalksAdapter d psh ppsh f =
  -- | Total-space-of-stalks adapter payload.
  record { decl = d ; expPresheaf = psh ; linkPresheaf = ppsh ; status = true }

-- | Categorical adapter for total spaces of stalks.
totalSpaceOfStalksCategorical : TotalSpaceOfStalksAdapter → CategoricalAdapter {lsuc lzero} C3S2.TotalSpaceOfStalks
totalSpaceOfStalksCategorical adapt = mkCategoricalAdapter C3S2.TotalSpaceOfStalks (λ _ → TotalSpaceOfStalksAdapter.decl adapt)

-- | Check if the total-space-of-stalks adapter is populated.
isFilledTotalSpaceOfStalks : TotalSpaceOfStalksAdapter → Bool
isFilledTotalSpaceOfStalks a = TotalSpaceOfStalksAdapter.status a

-- Sheaf of sections functor
-- | Adapter for the sheaf-of-sections functor derived from an étale space.
record SheafOfSectionsFunctorAdapter : Set₁ where
  field
    decl : C3S2.SheafOfSectionsFunctor
    expEtale : C3S2.EtaleSpaceOver
    expSheaf : C3S2.SheafOnLocaleDeclaration
    linkEtale : C3S2.SheafOfSectionsFunctor.etaleSpace decl ≡ expEtale
    linkSheaf : C3S2.SheafOfSectionsFunctor.isSheaf decl ≡ expSheaf
    status : Bool

mkSheafOfSectionsFunctorAdapter :
  (d : C3S2.SheafOfSectionsFunctor) →
  (et : C3S2.EtaleSpaceOver) →
  (sh : C3S2.SheafOnLocaleDeclaration) →
  (pet : C3S2.SheafOfSectionsFunctor.etaleSpace d ≡ et) →
  (psh : C3S2.SheafOfSectionsFunctor.isSheaf d ≡ sh) →
  (f : ⊤ → C3S2.SheafOfSectionsFunctor) →
  SheafOfSectionsFunctorAdapter
mkSheafOfSectionsFunctorAdapter d et sh pet psh f =
  -- | Sheaf-of-sections adapter payload.
  record { decl = d ; expEtale = et ; expSheaf = sh
         ; linkEtale = pet ; linkSheaf = psh ; status = true }

-- | Categorical adapter for sheaf-of-sections functors.
sheafOfSectionsFunctorCategorical : SheafOfSectionsFunctorAdapter → CategoricalAdapter {lsuc lzero} C3S2.SheafOfSectionsFunctor
sheafOfSectionsFunctorCategorical adapt = mkCategoricalAdapter C3S2.SheafOfSectionsFunctor (λ _ → SheafOfSectionsFunctorAdapter.decl adapt)

-- | Check if the sheaf-of-sections adapter is populated.
isFilledSheafOfSectionsFunctor : SheafOfSectionsFunctorAdapter → Bool
isFilledSheafOfSectionsFunctor a = SheafOfSectionsFunctorAdapter.status a

-- Sheaf-étale equivalence theorem
-- | Adapter for the equivalence between sheaf and étale categories.
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
    status : Bool

-- | Populate the sheaf/étale equivalence adapter with concrete data.
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
  -- | Sheaf/étale equivalence adapter payload.
  record { decl = d ; expSheafCat = sc ; expEtaleCat = ec
         ; expStalksF = sf ; expSectionsF = tf
         ; linkSheafCat = psc ; linkEtaleCat = pec
         ; linkStalksF = psf ; linkSectionsF = ptf ; status = true }

-- | Categorical adapter for the sheaf/étale equivalence theorem.
sheafEtaleEquivalenceTheoremCategorical : SheafEtaleEquivalenceTheoremAdapter → CategoricalAdapter {lsuc lzero} C3S2.SheafEtaleEquivalenceTheorem
sheafEtaleEquivalenceTheoremCategorical adapt = mkCategoricalAdapter C3S2.SheafEtaleEquivalenceTheorem (λ _ → SheafEtaleEquivalenceTheoremAdapter.decl adapt)

-- | Check if the sheaf/étale equivalence adapter is populated.
isFilledSheafEtaleEquivalenceTheorem : SheafEtaleEquivalenceTheoremAdapter → Bool
isFilledSheafEtaleEquivalenceTheorem a = SheafEtaleEquivalenceTheoremAdapter.status a

-- Direct image functor
-- | Adapter for a direct image functor between locales.
record DirectImageFunctorLocaleAdapter : Set₁ where
  field
    decl : C3S2.DirectImageFunctorLocale
    expFunctor : M.Identifier
    linkFunctor : C3S2.DirectImageFunctorLocale.underlyingFunctor decl ≡ expFunctor
    status : Bool

-- | Mark a direct-image functor witness as satisfied.
mkDirectImageFunctorLocaleAdapter :
  (d : C3S2.DirectImageFunctorLocale) →
  (f : M.Identifier) →
  (pf : C3S2.DirectImageFunctorLocale.underlyingFunctor d ≡ f) →
  (morph : ⊤ → C3S2.DirectImageFunctorLocale) →
  DirectImageFunctorLocaleAdapter
mkDirectImageFunctorLocaleAdapter d f pf morph =
  -- | Direct-image functor adapter payload.
  record { decl = d ; expFunctor = f ; linkFunctor = pf ; status = true }

-- | Categorical adapter for direct image functors.
directImageFunctorLocaleCategorical : DirectImageFunctorLocaleAdapter → CategoricalAdapter {lsuc lzero} C3S2.DirectImageFunctorLocale
directImageFunctorLocaleCategorical adapt = mkCategoricalAdapter C3S2.DirectImageFunctorLocale (λ _ → DirectImageFunctorLocaleAdapter.decl adapt)

-- | Check if the direct-image adapter is populated.
isFilledDirectImageFunctorLocale : DirectImageFunctorLocaleAdapter → Bool
isFilledDirectImageFunctorLocale a = DirectImageFunctorLocaleAdapter.status a

-- Inverse image functor
-- | Adapter for an inverse image functor between locales.
record InverseImageFunctorLocaleAdapter : Set₁ where
  field
    decl : C3S2.InverseImageFunctorLocale
    expFunctor : M.Identifier
    linkFunctor : C3S2.InverseImageFunctorLocale.underlyingFunctor decl ≡ expFunctor
    status : Bool

-- | Mark an inverse-image functor witness as satisfied.
mkInverseImageFunctorLocaleAdapter :
  (d : C3S2.InverseImageFunctorLocale) →
  (f : M.Identifier) →
  (pf : C3S2.InverseImageFunctorLocale.underlyingFunctor d ≡ f) →
  (morph : ⊤ → C3S2.InverseImageFunctorLocale) →
  InverseImageFunctorLocaleAdapter
mkInverseImageFunctorLocaleAdapter d f pf morph =
  -- | Inverse-image functor adapter payload.
  record { decl = d ; expFunctor = f ; linkFunctor = pf ; status = true }

-- | Categorical adapter for inverse image functors.
inverseImageFunctorLocaleCategorical : InverseImageFunctorLocaleAdapter → CategoricalAdapter {lsuc lzero} C3S2.InverseImageFunctorLocale
inverseImageFunctorLocaleCategorical adapt = mkCategoricalAdapter C3S2.InverseImageFunctorLocale (λ _ → InverseImageFunctorLocaleAdapter.decl adapt)

-- | Check if the inverse-image adapter is populated.
isFilledInverseImageFunctorLocale : InverseImageFunctorLocaleAdapter → Bool
isFilledInverseImageFunctorLocale a = InverseImageFunctorLocaleAdapter.status a

-- Change of base adjunction theorem
-- | Adapter for the locale change-of-base adjunction theorem.
record LocaleChangeOfBaseAdjunctionTheoremAdapter : Set₁ where
  field
    decl : C3S2.LocaleChangeOfBaseAdjunctionTheorem
    expInverse : C3S2.InverseImageFunctorLocale
    expDirect : C3S2.DirectImageFunctorLocale
    expAdj : M.Identifier
    linkInverse : C3S2.LocaleChangeOfBaseAdjunctionTheorem.inverseImageFunctor decl ≡ expInverse
    linkDirect : C3S2.LocaleChangeOfBaseAdjunctionTheorem.directImageFunctor decl ≡ expDirect
    linkAdj : C3S2.LocaleChangeOfBaseAdjunctionTheorem.adjunction decl ≡ expAdj
    status : Bool

-- | Populate the change-of-base adjunction adapter with concrete functors.
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
  -- | Change-of-base adjunction adapter payload.
  record { decl = d ; expInverse = inv ; expDirect = dir ; expAdj = adj
         ; linkInverse = pinv ; linkDirect = pdir ; linkAdj = padj ; status = true }

-- | Categorical adapter for change-of-base adjunctions.
localeChangeOfBaseAdjunctionTheoremCategorical : LocaleChangeOfBaseAdjunctionTheoremAdapter → CategoricalAdapter {lsuc lzero} C3S2.LocaleChangeOfBaseAdjunctionTheorem
localeChangeOfBaseAdjunctionTheoremCategorical adapt = mkCategoricalAdapter C3S2.LocaleChangeOfBaseAdjunctionTheorem (λ _ → LocaleChangeOfBaseAdjunctionTheoremAdapter.decl adapt)

-- | Check if the change-of-base adapter is populated.
isFilledLocaleChangeOfBaseAdjunctionTheorem : LocaleChangeOfBaseAdjunctionTheoremAdapter → Bool
isFilledLocaleChangeOfBaseAdjunctionTheorem a = LocaleChangeOfBaseAdjunctionTheoremAdapter.status a

-- Étale morphism induces sheaf equivalence theorem
-- | Adapter for the theorem that an étale morphism induces a sheaf equivalence.
record EtaleMorphismInducesSheafEquivalenceTheoremAdapter : Set₁ where
  field
    decl : C3S2.EtaleMorphismInducesSheafEquivalenceTheorem
    expInverse : C3S2.InverseImageFunctorLocale
    linkInverse : C3S2.EtaleMorphismInducesSheafEquivalenceTheorem.inverseImageFunctor decl ≡ expInverse
    status : Bool

-- | Mark the étale-morphism/sheaf-equivalence theorem as satisfied.
mkEtaleMorphismInducesSheafEquivalenceTheoremAdapter :
  (d : C3S2.EtaleMorphismInducesSheafEquivalenceTheorem) →
  (inv : C3S2.InverseImageFunctorLocale) →
  (pinv : C3S2.EtaleMorphismInducesSheafEquivalenceTheorem.inverseImageFunctor d ≡ inv) →
  (f : ⊤ → C3S2.EtaleMorphismInducesSheafEquivalenceTheorem) →
  EtaleMorphismInducesSheafEquivalenceTheoremAdapter
mkEtaleMorphismInducesSheafEquivalenceTheoremAdapter d inv pinv f =
  -- | Étale-morphism/sheaf-equivalence adapter payload.
  record { decl = d ; expInverse = inv ; linkInverse = pinv ; status = true }

-- | Categorical adapter for the étale-morphism equivalence theorem.
etaleMorphismInducesSheafEquivalenceTheoremCategorical : EtaleMorphismInducesSheafEquivalenceTheoremAdapter → CategoricalAdapter {lsuc lzero} C3S2.EtaleMorphismInducesSheafEquivalenceTheorem
etaleMorphismInducesSheafEquivalenceTheoremCategorical adapt = mkCategoricalAdapter C3S2.EtaleMorphismInducesSheafEquivalenceTheorem (λ _ → EtaleMorphismInducesSheafEquivalenceTheoremAdapter.decl adapt)

-- | Check if the étale-morphism equivalence adapter is populated.
isFilledEtaleMorphismInducesSheafEquivalenceTheorem : EtaleMorphismInducesSheafEquivalenceTheoremAdapter → Bool
isFilledEtaleMorphismInducesSheafEquivalenceTheorem a = EtaleMorphismInducesSheafEquivalenceTheoremAdapter.status a

-- Omega set declaration (verified)
-- | Adapter for an Ω-set declaration with attached data witness.
record OmegaSetAdapter : Set₁ where
  field
    decl : C3S2.OmegaSetDeclarationVerified
    expData : C3S2.OmegaSetData
    linkData : C3S2.OmegaSetDeclarationVerified.dataOmegaSet decl ≡ expData
    status : Bool

-- | Populate an Ω-set adapter with its data witness.
mkOmegaSetAdapter :
  (d : C3S2.OmegaSetDeclarationVerified) →
  (dat : C3S2.OmegaSetData) →
  (plink : C3S2.OmegaSetDeclarationVerified.dataOmegaSet d ≡ dat) →
  OmegaSetAdapter
mkOmegaSetAdapter d dat plink =
  -- | Ω-set adapter payload.
  record { decl = d ; expData = dat ; linkData = plink ; status = true }

-- | Check if the Ω-set adapter is populated.
isFilledOmegaSet : OmegaSetAdapter → Bool
isFilledOmegaSet a = OmegaSetAdapter.status a

-- Categorical view for OmegaSet (Topos-side)
-- | Categorical adapter view for Ω-set declarations.
omegaSetCategorical : OmegaSetAdapter → CategoricalAdapter {lsuc lzero} C3S2.OmegaSetDeclarationVerified
omegaSetCategorical adapt = mkCategoricalAdapter C3S2.OmegaSetDeclarationVerified (λ _ → OmegaSetAdapter.decl adapt)

-- Ω-sets are complete Ω-sets theorem
-- | Adapter for the refined completeness theorem relating sheaves and Ω-sets.
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
    status : Bool

-- | Populate the refined Ω-set completeness theorem adapter.
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
  -- | Ω-sets completeness adapter payload.
  record { decl = d ; expSheafCat = sc ; expOmegaCat = oc
         ; expFunctorA = fa ; expFunctorS = fs
         ; linkSheafCat = psc ; linkOmegaCat = poc
         ; linkFunctorA = pfa ; linkFunctorS = pfs ; status = true }

-- | Categorical adapter for the completeness of Ω-sets theorem.
sheavesAreCompleteOmegaSetsRefinedTheoremCategorical : SheavesAreCompleteOmegaSetsRefinedTheoremAdapter → CategoricalAdapter {lsuc lzero} C3S2.SheavesAreCompleteOmegaSetsRefinedTheorem
sheavesAreCompleteOmegaSetsRefinedTheoremCategorical adapt = mkCategoricalAdapter C3S2.SheavesAreCompleteOmegaSetsRefinedTheorem (λ _ → SheavesAreCompleteOmegaSetsRefinedTheoremAdapter.decl adapt)

-- | Check if the Ω-sets completeness adapter is populated.
isFilledSheavesAreCompleteOmegaSetsRefinedTheorem : SheavesAreCompleteOmegaSetsRefinedTheoremAdapter → Bool
isFilledSheavesAreCompleteOmegaSetsRefinedTheorem a = SheavesAreCompleteOmegaSetsRefinedTheoremAdapter.status a

-- Sheaf of rings
-- | Adapter for a sheaf of rings declaration.
record SheafOfRingsAdapter : Set₁ where
  field
    decl : C3S2.SheafOfRings
    expSheaf : C3S2.SheafOnLocaleDeclaration
    linkSheaf : C3S2.SheafOfRings.underlyingSheaf decl ≡ expSheaf
    status : Bool

-- | Mark a sheaf-of-rings declaration as satisfied.
mkSheafOfRingsAdapter :
  (d : C3S2.SheafOfRings) →
  (sh : C3S2.SheafOnLocaleDeclaration) →
  (psh : C3S2.SheafOfRings.underlyingSheaf d ≡ sh) →
  (f : ⊤ → C3S2.SheafOfRings) →
  SheafOfRingsAdapter
mkSheafOfRingsAdapter d sh psh f =
  -- | Sheaf-of-rings adapter payload.
  record { decl = d ; expSheaf = sh ; linkSheaf = psh ; status = true }

-- | Categorical adapter for sheaves of rings.
sheafOfRingsCategorical : SheafOfRingsAdapter → CategoricalAdapter {lsuc lzero} C3S2.SheafOfRings
sheafOfRingsCategorical adapt = mkCategoricalAdapter C3S2.SheafOfRings (λ _ → SheafOfRingsAdapter.decl adapt)

-- | Check if the sheaf-of-rings adapter is populated.
isFilledSheafOfRings : SheafOfRingsAdapter → Bool
isFilledSheafOfRings a = SheafOfRingsAdapter.status a

-- Sheaf of O-modules
-- | Adapter for a sheaf of O-modules and its underlying data.
record SheafOfOModulesAdapter : Set₁ where
  field
    decl : C3S2.SheafOfOModules
    expRingSheaf : C3S2.SheafOfRings
    expModSheaf : C3S2.SheafOnLocaleDeclaration
    linkRingSheaf : C3S2.SheafOfOModules.sheafOfRings decl ≡ expRingSheaf
    linkModSheaf : C3S2.SheafOfOModules.underlyingSheaf decl ≡ expModSheaf
    status : Bool

-- | Mark a sheaf-of-O-modules declaration as satisfied.
mkSheafOfOModulesAdapter :
  (d : C3S2.SheafOfOModules) →
  (rs : C3S2.SheafOfRings) →
  (ms : C3S2.SheafOnLocaleDeclaration) →
  (prs : C3S2.SheafOfOModules.sheafOfRings d ≡ rs) →
  (pms : C3S2.SheafOfOModules.underlyingSheaf d ≡ ms) →
  (f : ⊤ → C3S2.SheafOfOModules) →
  SheafOfOModulesAdapter
mkSheafOfOModulesAdapter d rs ms prs pms f =
  -- | Sheaf-of-O-modules adapter payload.
  record { decl = d ; expRingSheaf = rs ; expModSheaf = ms
         ; linkRingSheaf = prs ; linkModSheaf = pms ; status = true }

-- | Categorical adapter for sheaves of O-modules.
sheafOfOModulesCategorical : SheafOfOModulesAdapter → CategoricalAdapter {lsuc lzero} C3S2.SheafOfOModules
sheafOfOModulesCategorical adapt = mkCategoricalAdapter C3S2.SheafOfOModules (λ _ → SheafOfOModulesAdapter.decl adapt)

-- | Check if the sheaf-of-O-modules adapter is populated.
isFilledSheafOfOModules : SheafOfOModulesAdapter → Bool
isFilledSheafOfOModules a = SheafOfOModulesAdapter.status a

-- Category of O-modules is abelian
-- | Adapter for the corollary that the category of O-modules is abelian.
record CategoryOfOModulesIsAbelianCorollaryAdapter : Set₁ where
  field
    decl : C3S2.CategoryOfOModulesIsAbelianCorollary
    expRingSheaf : C3S2.SheafOfRings
    expCategory : C1S3.CategoryDeclaration
    linkRingSheaf : C3S2.CategoryOfOModulesIsAbelianCorollary.sheafOfRings decl ≡ expRingSheaf
    linkCategory : C3S2.CategoryOfOModulesIsAbelianCorollary.categoryOfOModules decl ≡ expCategory
    status : Bool

-- | Populate the corollary that O-modules form an abelian category.
mkCategoryOfOModulesIsAbelianCorollaryAdapter :
  (d : C3S2.CategoryOfOModulesIsAbelianCorollary) →
  (rs : C3S2.SheafOfRings) →
  (cat : C1S3.CategoryDeclaration) →
  (prs : C3S2.CategoryOfOModulesIsAbelianCorollary.sheafOfRings d ≡ rs) →
  (pcat : C3S2.CategoryOfOModulesIsAbelianCorollary.categoryOfOModules d ≡ cat) →
  (f : ⊤ → C3S2.CategoryOfOModulesIsAbelianCorollary) →
  CategoryOfOModulesIsAbelianCorollaryAdapter
mkCategoryOfOModulesIsAbelianCorollaryAdapter d rs cat prs pcat f =
  -- | Abelian O-modules corollary adapter payload.
  record { decl = d ; expRingSheaf = rs ; expCategory = cat
         ; linkRingSheaf = prs ; linkCategory = pcat ; status = true }

-- | Categorical adapter for the abelian O-modules corollary.
categoryOfOModulesIsAbelianCorollaryCategorical : CategoryOfOModulesIsAbelianCorollaryAdapter → CategoricalAdapter {lsuc lzero} C3S2.CategoryOfOModulesIsAbelianCorollary
categoryOfOModulesIsAbelianCorollaryCategorical adapt = mkCategoricalAdapter C3S2.CategoryOfOModulesIsAbelianCorollary (λ _ → CategoryOfOModulesIsAbelianCorollaryAdapter.decl adapt)

-- | Check if the abelian O-modules adapter is populated.
isFilledCategoryOfOModulesIsAbelianCorollary : CategoryOfOModulesIsAbelianCorollaryAdapter → Bool
isFilledCategoryOfOModulesIsAbelianCorollary a = CategoryOfOModulesIsAbelianCorollaryAdapter.status a
