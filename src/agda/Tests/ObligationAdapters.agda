{-# OPTIONS --without-K #-}

-- Tests.ObligationAdapters: Tiny adapters and a common status predicate

module Tests.ObligationAdapters where

open import Core.Phase using (Bool; true; false)
open import Agda.Builtin.Unit using (⊤; tt)
open import Agda.Builtin.Equality using (_≡_)
open import Metamodel as M

-- Chapter imports
import Chapter1.Level1 as C1L
import Chapter1.Level1sub2 as C1S2
import Chapter1.Level1sub3 as C1S3
import Chapter1.Level1sub4 as C1S4
import Chapter1.Level1sub5 as C1S5
import Chapter1.Level1sub6 as C1S6
import Chapter1.Level1sub8 as C1S8
import Chapter2.Level2sub1 as C2S1
import Chapter2.Level2sub2 as C2S2
import Chapter2.Level2sub3 as C2S3
import Chapter2.Level2sub4 as C2S4
import Chapter2.Level2sub5 as C2S5
import Chapter2.Level2sub6 as S6
import Chapter2.Level2sub7 as C2S7
import Chapter2.Level2sub8 as C2S8
import Chapter3.Level3sub1 as C3S1
import Chapter3.Level3sub2 as C3S2
open import Agda.Primitive using (Level; lzero; lsuc)
open import Core.CategoricalAdapter

-- Algebra imports
import Algebra.Foundation as AFo
import Algebra.Enrichment as AE
import Algebra.Rings.Basic as AR
import Algebra.Modules.Basic as AM
import Algebra.Fields.Basic as AFB
import Algebra.Fields.Advanced as AFA
import Algebra.Groups.Basic as AGB
import Algebra.Groups.Free as AGF
import Algebra.Groups.Structure as AGS
import Algebra.Groups.Abelian as AGA
import Core.UniversalProperties as CUP
import Chapter1.Level1 as C
import Chapter2.Level2sub6 as Enriched

-- Common status predicate wrapper
record ObligationStatus : Set₁ where
  field
    status : Bool

-- Adapter: Identity morphism in an enriched category
record IdEnrichedAdapter : Set₁ where
  field
    decl     : S6.IdentityMorphismDeclaration_Enriched
    expected : C1L.MorphismDeclaration
    link     : S6.IdentityMorphismDeclaration_Enriched.identityMorphismInV decl ≡ expected
    status   : Bool

mkIdEnrichedAdapter :
  (d : S6.IdentityMorphismDeclaration_Enriched) →
  (e : C1L.MorphismDeclaration) →
  (p : S6.IdentityMorphismDeclaration_Enriched.identityMorphismInV d ≡ e) →
  IdEnrichedAdapter
mkIdEnrichedAdapter d e p = record { decl = d ; expected = e ; link = p ; status = true }

isFilledId : IdEnrichedAdapter → Bool
isFilledId a = IdEnrichedAdapter.status a

idEnrichedCategorical : IdEnrichedAdapter →
  CategoricalAdapter {lsuc lzero} S6.IdentityMorphismDeclaration_Enriched
idEnrichedCategorical adapt =
  mkCategoricalAdapter S6.IdentityMorphismDeclaration_Enriched (λ _ → IdEnrichedAdapter.decl adapt)

-- Adapter: Hom-object declaration in an enriched setting
record HomObjectAdapter : Set₁ where
  field
    decl     : S6.HomObjectDeclaration
    expected : M.Identifier
    link     : S6.HomObjectDeclaration.homObjectInV decl ≡ expected
    status   : Bool

mkHomObjectAdapter :
  (d : S6.HomObjectDeclaration) →
  (e : M.Identifier) →
  (p : S6.HomObjectDeclaration.homObjectInV d ≡ e) →
  HomObjectAdapter
mkHomObjectAdapter d e p = record { decl = d ; expected = e ; link = p ; status = true }

isFilledHom : HomObjectAdapter → Bool
isFilledHom a = HomObjectAdapter.status a

homObjectCategorical : HomObjectAdapter →
  CategoricalAdapter {lsuc lzero} S6.HomObjectDeclaration
homObjectCategorical adapt =
  mkCategoricalAdapter S6.HomObjectDeclaration (λ _ → HomObjectAdapter.decl adapt)

-- ==========================================================
-- Chapter 3, Level 3.2 (local homeomorphisms and étale spaces)
-- ==========================================================

import Chapter3.Level3sub2 as C3S2
import Chapter3.Level3sub1 as C3S1
import Chapter1.Level1sub3 as C1S3

record LocalHomeomorphismAdapter : Set₁ where
  field
    decl     : C3S2.MorphismPropertyAssertionLocalHomeomorphism
    expected : M.Identifier
    link     : C3S2.MorphismPropertyAssertionLocalHomeomorphism.morphism decl ≡ expected
    status   : Bool

mkLocalHomeomorphismAdapter :
  (d : C3S2.MorphismPropertyAssertionLocalHomeomorphism) →
  (e : M.Identifier) →
  (p : C3S2.MorphismPropertyAssertionLocalHomeomorphism.morphism d ≡ e) →
  LocalHomeomorphismAdapter
mkLocalHomeomorphismAdapter d e p = record { decl = d ; expected = e ; link = p ; status = true }

isFilledLocalHomeo : LocalHomeomorphismAdapter → Bool
isFilledLocalHomeo a = LocalHomeomorphismAdapter.status a

-- Categorical view for Local Homeomorphism
localHomeomorphismCategorical : LocalHomeomorphismAdapter → CategoricalAdapter {lsuc lzero} C3S2.MorphismPropertyAssertionLocalHomeomorphism
localHomeomorphismCategorical adapt =
  mkCategoricalAdapter C3S2.MorphismPropertyAssertionLocalHomeomorphism (λ _ → LocalHomeomorphismAdapter.decl adapt)

record EtaleSpaceAdapter : Set₁ where
  field
    decl              : C3S2.EtaleSpaceOver
    expectedProjection : M.Identifier
    expectedLocalHomeo : C3S2.MorphismPropertyAssertionLocalHomeomorphism
    projLink          : C3S2.EtaleSpaceOver.projection decl ≡ expectedProjection
    locLink           : C3S2.EtaleSpaceOver.isLocalHomeomorphism decl ≡ expectedLocalHomeo
    status            : Bool

mkEtaleSpaceAdapter :
  (d : C3S2.EtaleSpaceOver) →
  (p : M.Identifier) →
  (h : C3S2.MorphismPropertyAssertionLocalHomeomorphism) →
  (pl : C3S2.EtaleSpaceOver.projection d ≡ p) →
  (hl : C3S2.EtaleSpaceOver.isLocalHomeomorphism d ≡ h) →
  EtaleSpaceAdapter
mkEtaleSpaceAdapter d p h pl hl = record
  { decl = d ; expectedProjection = p ; expectedLocalHomeo = h
  ; projLink = pl ; locLink = hl ; status = true }

isFilledEtale : EtaleSpaceAdapter → Bool
isFilledEtale a = EtaleSpaceAdapter.status a

-- Categorical view for Étale Space
etaleSpaceCategorical : EtaleSpaceAdapter → CategoricalAdapter {lsuc lzero} C3S2.EtaleSpaceOver
etaleSpaceCategorical adapt = mkCategoricalAdapter C3S2.EtaleSpaceOver (λ _ → EtaleSpaceAdapter.decl adapt)

-- ==========================================================
-- Chapter 3, Level 3.1 (locale–frame duality)
-- ==========================================================

record LocaleFrameDualityAdapter : Set₁ where
  field
    decl       : C3S1.LocaleFrameDualityTheorem
    expectedOP : Set
    link       : C3S1.LocaleFrameDualityTheorem.isOppositeCategory decl ≡ expectedOP
    status     : Bool

mkLocaleFrameDualityAdapter :
  (d : C3S1.LocaleFrameDualityTheorem) →
  (op : Set) →
  (p : C3S1.LocaleFrameDualityTheorem.isOppositeCategory d ≡ op) →
  LocaleFrameDualityAdapter
mkLocaleFrameDualityAdapter d op p = record { decl = d ; expectedOP = op ; link = p ; status = true }

isFilledDuality : LocaleFrameDualityAdapter → Bool
isFilledDuality a = LocaleFrameDualityAdapter.status a

-- Categorical view for Locale–Frame Duality Theorem
localeFrameDualityCategorical : LocaleFrameDualityAdapter → CategoricalAdapter {lsuc lzero} C3S1.LocaleFrameDualityTheorem
localeFrameDualityCategorical adapt = mkCategoricalAdapter C3S1.LocaleFrameDualityTheorem (λ _ → LocaleFrameDualityAdapter.decl adapt)

-- ==========================================================
-- Chapter 1, Level 1.3 (adjunction hom-set declaration)
-- ==========================================================

record AdjunctionHomAdapter : Set₁ where
  field
    decl : C1S3.AdjunctionHomDecl
    expF expG expC expD : M.Identifier
    linkF : C1S3.AdjunctionHomDecl.F decl ≡ expF
    linkG : C1S3.AdjunctionHomDecl.G decl ≡ expG
    linkC : C1S3.AdjunctionHomDecl.C decl ≡ expC
    linkD : C1S3.AdjunctionHomDecl.D decl ≡ expD
    status : Bool

mkAdjunctionHomAdapter :
  (d : C1S3.AdjunctionHomDecl) →
  (f g c d' : M.Identifier) →
  (pf : C1S3.AdjunctionHomDecl.F d ≡ f) →
  (pg : C1S3.AdjunctionHomDecl.G d ≡ g) →
  (pc : C1S3.AdjunctionHomDecl.C d ≡ c) →
  (pd : C1S3.AdjunctionHomDecl.D d ≡ d') →
  AdjunctionHomAdapter
mkAdjunctionHomAdapter d f g c d' pf pg pc pd =
  record { decl = d ; expF = f ; expG = g ; expC = c ; expD = d'
         ; linkF = pf ; linkG = pg ; linkC = pc ; linkD = pd ; status = true }

isFilledAdjunction : AdjunctionHomAdapter → Bool
isFilledAdjunction a = AdjunctionHomAdapter.status a

-- Categorical view for AdjunctionHom
adjunctionHomCategorical : AdjunctionHomAdapter →
  CategoricalAdapter {lzero} C1S3.AdjunctionHomDecl
adjunctionHomCategorical adapt =
  mkCategoricalAdapter C1S3.AdjunctionHomDecl (λ _ → AdjunctionHomAdapter.decl adapt)

-- ==========================================================
-- Chapter 1, Level 1.4 (canonical factorization system)
-- ==========================================================

record CanonicalFactorizationAdapter : Set₁ where
  field
    decl  : C1S4.CanonicalFactorizationSystem
    link  : C1S4.CanonicalFactorizationSystem.unit decl ≡ tt
    status : Bool

mkCanonicalFactorizationAdapter :
  (d : C1S4.CanonicalFactorizationSystem) →
  (p : C1S4.CanonicalFactorizationSystem.unit d ≡ tt) →
  (f : ⊤ → C1S4.CanonicalFactorizationSystem) →
  CanonicalFactorizationAdapter
mkCanonicalFactorizationAdapter d p f = record { decl = d ; link = p ; status = true }

canonicalFactorizationCategorical : CanonicalFactorizationAdapter → CategoricalAdapter {lzero} C1S4.CanonicalFactorizationSystem
canonicalFactorizationCategorical adapt = mkCategoricalAdapter C1S4.CanonicalFactorizationSystem (λ _ → CanonicalFactorizationAdapter.decl adapt)

isFilledCanonicalFactorization : CanonicalFactorizationAdapter → Bool
isFilledCanonicalFactorization a = CanonicalFactorizationAdapter.status a

-- ==========================================================
-- Chapter 2, Level 2.2 (regular epi/mono factorization in regular categories)
-- ==========================================================

record RegularFactorizationAdapter : Set₁ where
  field
    decl     : C2S2.RegularCategoryDeclaration
    expected : M.Identifier
    link     : C2S2.RegularCategoryDeclaration.regularEpiMonoFactorizationWitness decl ≡ expected
    status   : Bool

mkRegularFactorizationAdapter :
  (d : C2S2.RegularCategoryDeclaration) →
  (e : M.Identifier) →
  (p : C2S2.RegularCategoryDeclaration.regularEpiMonoFactorizationWitness d ≡ e) →
  (f : ⊤ → C2S2.RegularCategoryDeclaration) →
  RegularFactorizationAdapter
mkRegularFactorizationAdapter d e p f = record { decl = d ; expected = e ; link = p ; status = true }

regularFactorizationCategorical : RegularFactorizationAdapter → CategoricalAdapter {lzero} C2S2.RegularCategoryDeclaration
regularFactorizationCategorical adapt = mkCategoricalAdapter C2S2.RegularCategoryDeclaration (λ _ → RegularFactorizationAdapter.decl adapt)

isFilledRegularFactorization : RegularFactorizationAdapter → Bool
isFilledRegularFactorization a = RegularFactorizationAdapter.status a

-- ==========================================================
-- Chapter 1, Level 1.5 (reflective subcategory as localization)
-- ==========================================================

record ReflectiveLocalizationAdapter : Set₁ where
  field
    decl : C1S5.ReflectiveSubcategoryAsLocalizationTheorem
    expR expC expL : M.Identifier
    linkR : C1S5.ReflectiveSubcategoryAsLocalizationTheorem.reflectiveSubcat decl ≡ expR
    linkC : C1S5.ReflectiveSubcategoryAsLocalizationTheorem.ambientCategory decl ≡ expC
    linkL : C1S5.ReflectiveSubcategoryAsLocalizationTheorem.reflector decl ≡ expL
    status : Bool

mkReflectiveLocalizationAdapter :
  (d : C1S5.ReflectiveSubcategoryAsLocalizationTheorem) →
  (r c l : M.Identifier) →
  (pr : C1S5.ReflectiveSubcategoryAsLocalizationTheorem.reflectiveSubcat d ≡ r) →
  (pc : C1S5.ReflectiveSubcategoryAsLocalizationTheorem.ambientCategory d ≡ c) →
  (pl : C1S5.ReflectiveSubcategoryAsLocalizationTheorem.reflector d ≡ l) →
  (f : ⊤ → C1S5.ReflectiveSubcategoryAsLocalizationTheorem) →
  ReflectiveLocalizationAdapter
mkReflectiveLocalizationAdapter d r c l pr pc pl f =
  record { decl = d ; expR = r ; expC = c ; expL = l ; linkR = pr ; linkC = pc ; linkL = pl ; status = true }

reflectiveLocalizationCategorical : ReflectiveLocalizationAdapter → CategoricalAdapter {lzero} C1S5.ReflectiveSubcategoryAsLocalizationTheorem
reflectiveLocalizationCategorical adapt = mkCategoricalAdapter C1S5.ReflectiveSubcategoryAsLocalizationTheorem (λ _ → ReflectiveLocalizationAdapter.decl adapt)

isFilledReflectiveLocalization : ReflectiveLocalizationAdapter → Bool
isFilledReflectiveLocalization a = ReflectiveLocalizationAdapter.status a

-- ==========================================================
-- Chapter 2, Level 2.2 (kernel pair specifics)
-- ==========================================================

record KernelPairAdapter : Set₁ where
  field
    decl   : C2S2.KernelPairDeclaration
    expM   : M.Identifier
    expK1  : M.Identifier
    expK2  : M.Identifier
    expPB  : M.Identifier
    linkM  : C2S2.KernelPairDeclaration.morphism decl ≡ expM
    linkK1 : C2S2.KernelPairDeclaration.projection1 decl ≡ expK1
    linkK2 : C2S2.KernelPairDeclaration.projection2 decl ≡ expK2
    linkPB : C2S2.KernelPairDeclaration.pullbackSquareWitness decl ≡ expPB
    status : Bool

mkKernelPairAdapter :
  (d : C2S2.KernelPairDeclaration) →
  (m k1 k2 pb : M.Identifier) →
  (pm  : C2S2.KernelPairDeclaration.morphism d ≡ m) →
  (pk1 : C2S2.KernelPairDeclaration.projection1 d ≡ k1) →
  (pk2 : C2S2.KernelPairDeclaration.projection2 d ≡ k2) →
  (ppb : C2S2.KernelPairDeclaration.pullbackSquareWitness d ≡ pb) →
  (f : ⊤ → C2S2.KernelPairDeclaration) →
  KernelPairAdapter
mkKernelPairAdapter d m k1 k2 pb pm pk1 pk2 ppb f =
    record { decl = d ; expM = m ; expK1 = k1 ; expK2 = k2 ; expPB = pb
      ; linkM = pm ; linkK1 = pk1 ; linkK2 = pk2 ; linkPB = ppb ; status = true }

kernelPairCategorical : KernelPairAdapter → CategoricalAdapter {lzero} C2S2.KernelPairDeclaration
kernelPairCategorical adapt = mkCategoricalAdapter C2S2.KernelPairDeclaration (λ _ → KernelPairAdapter.decl adapt)

isFilledKernelPair : KernelPairAdapter → Bool
isFilledKernelPair a = KernelPairAdapter.status a

-- ==========================================================
-- Chapter 1, Level 1.4 (strong monomorphism)
-- ==========================================================

record StrongMonoAdapter : Set₁ where
  field
    decl     : C1S4.StrongMonomorphism
    expected : M.Identifier
    link     : C1S4.StrongMonomorphism.m decl ≡ expected
    status   : Bool

mkStrongMonoAdapter :
  (d : C1S4.StrongMonomorphism) →
  (e : M.Identifier) →
  (p : C1S4.StrongMonomorphism.m d ≡ e) →
  (f : ⊤ → C1S4.StrongMonomorphism) →
  StrongMonoAdapter
mkStrongMonoAdapter d e p f = record { decl = d ; expected = e ; link = p ; status = true }

strongMonoCategorical : StrongMonoAdapter → CategoricalAdapter {lzero} C1S4.StrongMonomorphism
strongMonoCategorical adapt = mkCategoricalAdapter C1S4.StrongMonomorphism (λ _ → StrongMonoAdapter.decl adapt)

isFilledStrongMono : StrongMonoAdapter → Bool
isFilledStrongMono a = StrongMonoAdapter.status a

-- ==========================================================
-- Chapter 2, Level 2.3 (internal equivalence relations)
-- ==========================================================

record InternalEquivalenceRelationAdapter : Set₁ where
  field
    decl    : C2S2.InternalEquivalenceRelationDeclaration
    expR1   : M.Identifier
    expR2   : M.Identifier
    expMono : M.Identifier
    linkR1  : C2S2.InternalEquivalenceRelationDeclaration.relLeft decl ≡ expR1
    linkR2  : C2S2.InternalEquivalenceRelationDeclaration.relRight decl ≡ expR2
    linkMono : C2S2.InternalEquivalenceRelationDeclaration.monoIntoProductWitness decl ≡ expMono
    status  : Bool

mkInternalEquivalenceRelationAdapter :
  (d : C2S2.InternalEquivalenceRelationDeclaration) →
  (r1 r2 mono : M.Identifier) →
  (pr1 : C2S2.InternalEquivalenceRelationDeclaration.relLeft d ≡ r1) →
  (pr2 : C2S2.InternalEquivalenceRelationDeclaration.relRight d ≡ r2) →
  (pmono : C2S2.InternalEquivalenceRelationDeclaration.monoIntoProductWitness d ≡ mono) →
  (f : ⊤ → C2S2.InternalEquivalenceRelationDeclaration) →
  InternalEquivalenceRelationAdapter
mkInternalEquivalenceRelationAdapter d r1 r2 mono pr1 pr2 pmono f =
    record { decl = d ; expR1 = r1 ; expR2 = r2 ; expMono = mono
      ; linkR1 = pr1 ; linkR2 = pr2 ; linkMono = pmono ; status = true }

internalEquivalenceRelationCategorical : InternalEquivalenceRelationAdapter → CategoricalAdapter {lzero} C2S2.InternalEquivalenceRelationDeclaration
internalEquivalenceRelationCategorical adapt = mkCategoricalAdapter C2S2.InternalEquivalenceRelationDeclaration (λ _ → InternalEquivalenceRelationAdapter.decl adapt)

isFilledInternalEquiv : InternalEquivalenceRelationAdapter → Bool
isFilledInternalEquiv a = InternalEquivalenceRelationAdapter.status a

-- ==========================================================
-- Chapter 2, Level 2.3 (regular exact sequences)
-- ==========================================================

record RegularExactSequenceAdapter : Set₁ where
  field
    decl : C2S2.RegularExactSequenceDeclaration
    expKernelMorphism   : M.Identifier
    expQuotientMorphism : M.Identifier
    linkKernel   : C2S2.KernelPairDeclaration.morphism (C2S2.RegularExactSequenceDeclaration.kernelPair decl) ≡ expKernelMorphism
    linkQuotient : C2S2.RegularEpimorphismProperty.morphism (C2S2.RegularExactSequenceDeclaration.quotient decl) ≡ expQuotientMorphism
    status : Bool

mkRegularExactSequenceAdapter :
  (d : C2S2.RegularExactSequenceDeclaration) →
  (k q : M.Identifier) →
  (pk : C2S2.KernelPairDeclaration.morphism (C2S2.RegularExactSequenceDeclaration.kernelPair d) ≡ k) →
  (pq : C2S2.RegularEpimorphismProperty.morphism (C2S2.RegularExactSequenceDeclaration.quotient d) ≡ q) →
  (f : ⊤ → C2S2.RegularExactSequenceDeclaration) →
  RegularExactSequenceAdapter
mkRegularExactSequenceAdapter d k q pk pq f =
    record { decl = d ; expKernelMorphism = k ; expQuotientMorphism = q
      ; linkKernel = pk ; linkQuotient = pq ; status = true }

regularExactSequenceCategorical : RegularExactSequenceAdapter → CategoricalAdapter {lzero} C2S2.RegularExactSequenceDeclaration
regularExactSequenceCategorical adapt = mkCategoricalAdapter C2S2.RegularExactSequenceDeclaration (λ _ → RegularExactSequenceAdapter.decl adapt)

isFilledRegularExact : RegularExactSequenceAdapter → Bool
isFilledRegularExact a = RegularExactSequenceAdapter.status a

-- ==========================================================
-- Chapter 2, Level 2.1 (Abelian categories)
-- ==========================================================

record AdditiveCategoryAdapter : Set₁ where
  field
    decl : C2S1.AdditiveCategoryDeclaration
    expCategory : M.Identifier
    expZero : C2S1.HasZeroObjectProperty
    linkCat : C2S1.AdditiveCategoryDeclaration.category decl ≡ expCategory
    linkZero : C2S1.AdditiveCategoryDeclaration.hasZeroObject decl ≡ expZero
    status : Bool

mkAdditiveCategoryAdapter :
  (d : C2S1.AdditiveCategoryDeclaration) →
  (cat : M.Identifier) →
  (zero : C2S1.HasZeroObjectProperty) →
  (pcat : C2S1.AdditiveCategoryDeclaration.category d ≡ cat) →
  (pzero : C2S1.AdditiveCategoryDeclaration.hasZeroObject d ≡ zero) →
  (f : ⊤ → C2S1.AdditiveCategoryDeclaration) →
  AdditiveCategoryAdapter
mkAdditiveCategoryAdapter d cat zero pcat pzero f =
    record { decl = d ; expCategory = cat ; expZero = zero
      ; linkCat = pcat ; linkZero = pzero ; status = true }

additiveCategoryCategorical : AdditiveCategoryAdapter → CategoricalAdapter {lzero} C2S1.AdditiveCategoryDeclaration
additiveCategoryCategorical adapt = mkCategoricalAdapter C2S1.AdditiveCategoryDeclaration (λ _ → AdditiveCategoryAdapter.decl adapt)

isFilledAdditive : AdditiveCategoryAdapter → Bool
isFilledAdditive a = AdditiveCategoryAdapter.status a

record AbelianCategoryAdapter : Set₁ where
  field
    decl : C2S1.AbelianCategoryDeclaration
    expCategory : M.Identifier
    expAdditive : C2S1.AdditiveCategoryDeclaration
    linkCat : C2S1.AbelianCategoryDeclaration.category decl ≡ expCategory
    linkAdd : C2S1.AbelianCategoryDeclaration.additive decl ≡ expAdditive
    status : Bool

mkAbelianCategoryAdapter :
  (d : C2S1.AbelianCategoryDeclaration) →
  (cat : M.Identifier) →
  (add : C2S1.AdditiveCategoryDeclaration) →
  (pcat : C2S1.AbelianCategoryDeclaration.category d ≡ cat) →
  (padd : C2S1.AbelianCategoryDeclaration.additive d ≡ add) →
  (f : ⊤ → C2S1.AbelianCategoryDeclaration) →
  AbelianCategoryAdapter
mkAbelianCategoryAdapter d cat add pcat padd f =
  record { decl = d ; expCategory = cat ; expAdditive = add
         ; linkCat = pcat ; linkAdd = padd ; status = true }

abelianCategoryCategorical : AbelianCategoryAdapter → CategoricalAdapter {lzero} C2S1.AbelianCategoryDeclaration
abelianCategoryCategorical adapt = mkCategoricalAdapter C2S1.AbelianCategoryDeclaration (λ _ → AbelianCategoryAdapter.decl adapt)

isFilledAbelian : AbelianCategoryAdapter → Bool
isFilledAbelian a = AbelianCategoryAdapter.status a

record BiproductAdapter : Set₁ where
  field
    decl : C2S1.BiproductObject
    expLeft : M.Identifier
    expRight : M.Identifier
    expObject : M.Identifier
    linkLeft : C2S1.BiproductObject.left decl ≡ expLeft
    linkRight : C2S1.BiproductObject.right decl ≡ expRight
    linkObject : C2S1.BiproductObject.object decl ≡ expObject
    status : Bool

mkBiproductAdapter :
  (d : C2S1.BiproductObject) →
  (l r obj : M.Identifier) →
  (pl : C2S1.BiproductObject.left d ≡ l) →
  (pr : C2S1.BiproductObject.right d ≡ r) →
  (pobj : C2S1.BiproductObject.object d ≡ obj) →
  (f : ⊤ → C2S1.BiproductObject) →
  BiproductAdapter
mkBiproductAdapter d l r obj pl pr pobj f =
  record { decl = d ; expLeft = l ; expRight = r ; expObject = obj
         ; linkLeft = pl ; linkRight = pr ; linkObject = pobj ; status = true }

biproductCategorical : BiproductAdapter → CategoricalAdapter {lzero} C2S1.BiproductObject
biproductCategorical adapt = mkCategoricalAdapter C2S1.BiproductObject (λ _ → BiproductAdapter.decl adapt)

isFilledBiproduct : BiproductAdapter → Bool
isFilledBiproduct a = BiproductAdapter.status a

-- ==========================================================
-- Chapter 2, Level 2.3 (Lawvere theories, algebraic categories)
-- ==========================================================

record LawvereTheoryAdapter : Set₁ where
  field
    decl : C2S3.LawvereTheoryDeclaration
    expTheory : M.Identifier
    expBase : M.Identifier
    linkTheory : C2S3.LawvereTheoryDeclaration.theoryCategory decl ≡ expTheory
    linkBase : C2S3.LawvereTheoryDeclaration.baseObject decl ≡ expBase
    status : Bool

mkLawvereTheoryAdapter :
  (d : C2S3.LawvereTheoryDeclaration) →
  (th base : M.Identifier) →
  (pth : C2S3.LawvereTheoryDeclaration.theoryCategory d ≡ th) →
  (pbase : C2S3.LawvereTheoryDeclaration.baseObject d ≡ base) →
  (morph : ⊤ → C2S3.LawvereTheoryDeclaration) →
  LawvereTheoryAdapter
mkLawvereTheoryAdapter d th base pth pbase morph =
  record { decl = d ; expTheory = th ; expBase = base
         ; linkTheory = pth ; linkBase = pbase ; status = true }

isFilledLawvereTheory : LawvereTheoryAdapter → Bool
isFilledLawvereTheory a = LawvereTheoryAdapter.status a

lawvereTheoryCategorical : LawvereTheoryAdapter → CategoricalAdapter {lzero} C2S3.LawvereTheoryDeclaration
lawvereTheoryCategorical adapt = mkCategoricalAdapter C2S3.LawvereTheoryDeclaration (λ _ → LawvereTheoryAdapter.decl adapt)

record AlgebraicCategoryAdapter : Set₁ where
  field
    decl : C2S3.AlgebraicCategoryDeclaration
    expCategory : M.Identifier
    expTheory : C2S3.LawvereTheoryDeclaration
    linkCat : C2S3.AlgebraicCategoryDeclaration.category decl ≡ expCategory
    linkTheory : C2S3.AlgebraicCategoryDeclaration.witnessTheory decl ≡ expTheory
    status : Bool

mkAlgebraicCategoryAdapter :
  (d : C2S3.AlgebraicCategoryDeclaration) →
  (cat : M.Identifier) →
  (th : C2S3.LawvereTheoryDeclaration) →
  (pcat : C2S3.AlgebraicCategoryDeclaration.category d ≡ cat) →
  (pth : C2S3.AlgebraicCategoryDeclaration.witnessTheory d ≡ th) →
  (morph : ⊤ → C2S3.AlgebraicCategoryDeclaration) →
  AlgebraicCategoryAdapter
mkAlgebraicCategoryAdapter d cat th pcat pth morph =
  record { decl = d ; expCategory = cat ; expTheory = th
         ; linkCat = pcat ; linkTheory = pth ; status = true }

isFilledAlgebraicCategory : AlgebraicCategoryAdapter → Bool
isFilledAlgebraicCategory a = AlgebraicCategoryAdapter.status a

algebraicCategoryCategorical : AlgebraicCategoryAdapter → CategoricalAdapter {lzero} C2S3.AlgebraicCategoryDeclaration
algebraicCategoryCategorical adapt = mkCategoricalAdapter C2S3.AlgebraicCategoryDeclaration (λ _ → AlgebraicCategoryAdapter.decl adapt)

-- ==========================================================
-- Chapter 2, Level 2.4 (Monads)
-- ==========================================================

record MonadAdapter : Set₁ where
  field
    decl : C2S4.MonadDeclaration
    expName : M.Identifier
    expDatum : C2S4.MonadData
    linkName : C2S4.MonadDeclaration.name decl ≡ expName
    linkDatum : C2S4.MonadDeclaration.datum decl ≡ expDatum
    status : Bool

mkMonadAdapter :
  (d : C2S4.MonadDeclaration) →
  (n : M.Identifier) →
  (dat : C2S4.MonadData) →
  (pn : C2S4.MonadDeclaration.name d ≡ n) →
  (pdat : C2S4.MonadDeclaration.datum d ≡ dat) →
  MonadAdapter
mkMonadAdapter d n dat pn pdat =
  record { decl = d ; expName = n ; expDatum = dat
         ; linkName = pn ; linkDatum = pdat ; status = true }

isFilledMonad : MonadAdapter → Bool
isFilledMonad a = MonadAdapter.status a

-- Categorical view for Monad
monadCategorical : MonadAdapter →
  CategoricalAdapter {lzero} C2S4.MonadDeclaration
monadCategorical adapt =
  mkCategoricalAdapter C2S4.MonadDeclaration (λ _ → MonadAdapter.decl adapt)

record TAlgebraAdapter : Set₁ where
  field
    decl : C2S4.TAlgebraData
    expCarrier : M.Identifier
    monad : C2S4.MonadDeclaration
    linkCarrier : C2S4.TAlgebraData.carrier decl ≡ expCarrier
    linkMonad : C2S4.TAlgebraData.monad decl ≡ monad
    status : Bool

mkTAlgebraAdapter :
  (d : C2S4.TAlgebraData) →
  (c : M.Identifier) →
  (m : C2S4.MonadDeclaration) →
  (pc : C2S4.TAlgebraData.carrier d ≡ c) →
  (pm : C2S4.TAlgebraData.monad d ≡ m) →
  TAlgebraAdapter
mkTAlgebraAdapter d c m pc pm =
  record { decl = d ; expCarrier = c ; monad = m
         ; linkCarrier = pc ; linkMonad = pm ; status = true }

isFilledTAlgebra : TAlgebraAdapter → Bool
isFilledTAlgebra a = TAlgebraAdapter.status a

-- Categorical view for TAlgebra
talgebraCategorical : TAlgebraAdapter →
  CategoricalAdapter {lzero} C2S4.TAlgebraData
talgebraCategorical adapt =
  mkCategoricalAdapter C2S4.TAlgebraData (λ _ → TAlgebraAdapter.decl adapt)

-- ==========================================================
-- Chapter 2, Level 2.5 (Locally presentable categories)
-- ==========================================================

record LocallyPresentableAdapter : Set₁ where
  field
    decl : C2S5.LocallyPresentableCategoryDeclaration
    expCat : C1S3.CategoryDeclaration
    expRank : C1S6.RegularCardinal
    linkCat : C2S5.LocallyPresentableCategoryDeclaration.category decl ≡ expCat
    linkRank : C2S5.LocallyPresentableCategoryDeclaration.rank decl ≡ expRank
    status : Bool

mkLocallyPresentableAdapter :
  (d : C2S5.LocallyPresentableCategoryDeclaration) →
  (cat : C1S3.CategoryDeclaration) →
  (rk : C1S6.RegularCardinal) →
  (pcat : C2S5.LocallyPresentableCategoryDeclaration.category d ≡ cat) →
  (prk : C2S5.LocallyPresentableCategoryDeclaration.rank d ≡ rk) →
  LocallyPresentableAdapter
mkLocallyPresentableAdapter d cat rk pcat prk =
  record { decl = d ; expCat = cat ; expRank = rk
         ; linkCat = pcat ; linkRank = prk ; status = true }

isFilledLocallyPresentable : LocallyPresentableAdapter → Bool
isFilledLocallyPresentable a = LocallyPresentableAdapter.status a

locallyPresentableCategorical : LocallyPresentableAdapter →
  CategoricalAdapter {lsuc lzero} C2S5.LocallyPresentableCategoryDeclaration
locallyPresentableCategorical adapt =
  mkCategoricalAdapter C2S5.LocallyPresentableCategoryDeclaration (λ _ → LocallyPresentableAdapter.decl adapt)

-- ==========================================================
-- Chapter 2, Level 2.6 (Monoidal/Enriched categories)
-- ==========================================================

record MonoidalCategoryAdapter : Set₁ where
  field
    decl : S6.MonoidalCategoryDeclaration
    expDatum : S6.MonoidalCategoryData
    expAssociator : S6.AssociatorDeclaration
    linkDatum : S6.MonoidalCategoryDeclaration.datum decl ≡ expDatum
    linkAssoc : S6.MonoidalCategoryDeclaration.associator decl ≡ expAssociator
    status : Bool

mkMonoidalCategoryAdapter :
  (d : S6.MonoidalCategoryDeclaration) →
  (dat : S6.MonoidalCategoryData) →
  (assoc : S6.AssociatorDeclaration) →
  (pdat : S6.MonoidalCategoryDeclaration.datum d ≡ dat) →
  (passoc : S6.MonoidalCategoryDeclaration.associator d ≡ assoc) →
  MonoidalCategoryAdapter
mkMonoidalCategoryAdapter d dat assoc pdat passoc =
  record { decl = d ; expDatum = dat ; expAssociator = assoc
         ; linkDatum = pdat ; linkAssoc = passoc ; status = true }

isFilledMonoidal : MonoidalCategoryAdapter → Bool
isFilledMonoidal a = MonoidalCategoryAdapter.status a

monoidalCategoryCategorical : MonoidalCategoryAdapter →
  CategoricalAdapter {lsuc lzero} S6.MonoidalCategoryDeclaration
monoidalCategoryCategorical adapt =
  mkCategoricalAdapter S6.MonoidalCategoryDeclaration (λ _ → MonoidalCategoryAdapter.decl adapt)

record SymmetricMonoidalAdapter : Set₁ where
  field
    decl : S6.SymmetricMonoidalCategoryDeclaration
    expMonoidal : S6.MonoidalCategoryDeclaration
    expBraiding : S6.BraidingDeclaration
    linkMonoidal : S6.SymmetricMonoidalCategoryDeclaration.monoidalCategory decl ≡ expMonoidal
    linkBraiding : S6.SymmetricMonoidalCategoryDeclaration.braiding decl ≡ expBraiding
    status : Bool

mkSymmetricMonoidalAdapter :
  (d : S6.SymmetricMonoidalCategoryDeclaration) →
  (mon : S6.MonoidalCategoryDeclaration) →
  (br : S6.BraidingDeclaration) →
  (pmon : S6.SymmetricMonoidalCategoryDeclaration.monoidalCategory d ≡ mon) →
  (pbr : S6.SymmetricMonoidalCategoryDeclaration.braiding d ≡ br) →
  SymmetricMonoidalAdapter
mkSymmetricMonoidalAdapter d mon br pmon pbr =
  record { decl = d ; expMonoidal = mon ; expBraiding = br
         ; linkMonoidal = pmon ; linkBraiding = pbr ; status = true }

isFilledSymmetricMonoidal : SymmetricMonoidalAdapter → Bool
isFilledSymmetricMonoidal a = SymmetricMonoidalAdapter.status a

symmetricMonoidalCategoryCategorical : SymmetricMonoidalAdapter →
  CategoricalAdapter {lsuc lzero} S6.SymmetricMonoidalCategoryDeclaration
symmetricMonoidalCategoryCategorical adapt =
  mkCategoricalAdapter S6.SymmetricMonoidalCategoryDeclaration (λ _ → SymmetricMonoidalAdapter.decl adapt)

record InternalHomAdapter : Set₁ where
  field
    decl : S6.InternalHomObjectDeclaration
    expCat : C1S3.CategoryDeclaration
    expSource : M.Identifier
    expTarget : M.Identifier
    linkCat : S6.InternalHomObjectDeclaration.category decl ≡ expCat
    linkSource : S6.InternalHomObjectDeclaration.sourceObject decl ≡ expSource
    linkTarget : S6.InternalHomObjectDeclaration.targetObject decl ≡ expTarget
    status : Bool

mkInternalHomAdapter :
  (d : S6.InternalHomObjectDeclaration) →
  (cat : C1S3.CategoryDeclaration) →
  (src tgt : M.Identifier) →
  (pcat : S6.InternalHomObjectDeclaration.category d ≡ cat) →
  (psrc : S6.InternalHomObjectDeclaration.sourceObject d ≡ src) →
  (ptgt : S6.InternalHomObjectDeclaration.targetObject d ≡ tgt) →
  InternalHomAdapter
mkInternalHomAdapter d cat src tgt pcat psrc ptgt =
  record { decl = d ; expCat = cat ; expSource = src ; expTarget = tgt
         ; linkCat = pcat ; linkSource = psrc ; linkTarget = ptgt ; status = true }

isFilledInternalHom : InternalHomAdapter → Bool
isFilledInternalHom a = InternalHomAdapter.status a

internalHomCategorical : InternalHomAdapter →
  CategoricalAdapter {lsuc lzero} S6.InternalHomObjectDeclaration
internalHomCategorical adapt =
  mkCategoricalAdapter S6.InternalHomObjectDeclaration (λ _ → InternalHomAdapter.decl adapt)

-- ==========================================================
-- Chapter 2, Level 2.7 (Topological categories)
-- ==========================================================

record CGWH_CategoryAdapter : Set₁ where
  field
    decl : C2S7.CGWH_CategoryDeclaration
    expTopCat : C2S7.TopologicalSpacesCategory
    expUnderlyingCat : C1S3.CategoryDeclaration
    linkTopCat : C2S7.CGWH_CategoryDeclaration.topCategory decl ≡ expTopCat
    linkUnderlyingCat : C2S7.CGWH_CategoryDeclaration.underlyingCategory decl ≡ expUnderlyingCat
    status : Bool

mkCGWH_CategoryAdapter :
  (d : C2S7.CGWH_CategoryDeclaration) →
  (topcat : C2S7.TopologicalSpacesCategory) →
  (cat : C1S3.CategoryDeclaration) →
  (ptop : C2S7.CGWH_CategoryDeclaration.topCategory d ≡ topcat) →
  (pcat : C2S7.CGWH_CategoryDeclaration.underlyingCategory d ≡ cat) →
  CGWH_CategoryAdapter
mkCGWH_CategoryAdapter d topcat cat ptop pcat =
  record { decl = d ; expTopCat = topcat ; expUnderlyingCat = cat
         ; linkTopCat = ptop ; linkUnderlyingCat = pcat ; status = true }

isFilledCGWH : CGWH_CategoryAdapter → Bool
isFilledCGWH a = CGWH_CategoryAdapter.status a

cgwhCategoryCategorical : CGWH_CategoryAdapter →
  CategoricalAdapter {lsuc lzero} C2S7.CGWH_CategoryDeclaration
cgwhCategoryCategorical adapt =
  mkCategoricalAdapter C2S7.CGWH_CategoryDeclaration (λ _ → CGWH_CategoryAdapter.decl adapt)

record TopologicalFunctorAdapter : Set₁ where
  field
    decl : C2S7.TopologicalFunctorProperty
    expFunctor : M.Identifier
    linkFunctor : C2S7.TopologicalFunctorProperty.functor decl ≡ expFunctor
    status : Bool

mkTopologicalFunctorAdapter :
  (d : C2S7.TopologicalFunctorProperty) →
  (f : M.Identifier) →
  (pf : C2S7.TopologicalFunctorProperty.functor d ≡ f) →
  TopologicalFunctorAdapter
mkTopologicalFunctorAdapter d f pf =
  record { decl = d ; expFunctor = f
         ; linkFunctor = pf ; status = true }

isFilledTopologicalFunctor : TopologicalFunctorAdapter → Bool
isFilledTopologicalFunctor a = TopologicalFunctorAdapter.status a

topologicalFunctorCategorical : TopologicalFunctorAdapter →
  CategoricalAdapter {lsuc lzero} C2S7.TopologicalFunctorProperty
topologicalFunctorCategorical adapt =
  mkCategoricalAdapter C2S7.TopologicalFunctorProperty (λ _ → TopologicalFunctorAdapter.decl adapt)

-- ==========================================================
-- Chapter 2, Level 2.8 (Fibrations)
-- ==========================================================

record FibrationAdapter : Set₁ where
  field
    decl : C2S8.FibrationDeclaration
    expProjection : C2S8.FibrationProjectionFunctor
    linkProjection : C2S8.FibrationDeclaration.projectionFunctor decl ≡ expProjection
    status : Bool

mkFibrationAdapter :
  (d : C2S8.FibrationDeclaration) →
  (proj : C2S8.FibrationProjectionFunctor) →
  (pproj : C2S8.FibrationDeclaration.projectionFunctor d ≡ proj) →
  (f : ⊤ → C2S8.FibrationDeclaration) →
  FibrationAdapter
mkFibrationAdapter d proj pproj f =
    record { decl = d ; expProjection = proj
      ; linkProjection = pproj ; status = true }

fibrationCategorical : FibrationAdapter → CategoricalAdapter {lsuc lzero} C2S8.FibrationDeclaration
fibrationCategorical adapt = mkCategoricalAdapter C2S8.FibrationDeclaration (λ _ → FibrationAdapter.decl adapt)

isFilledFibration : FibrationAdapter → Core.Phase.Bool
isFilledFibration a = FibrationAdapter.status a

record OpfibrationAdapter : Set₁ where
  field
    decl : C2S8.OpfibrationDeclaration
    expProjection : C2S8.FibrationProjectionFunctor
    linkProjection : C2S8.OpfibrationDeclaration.projectionFunctor decl ≡ expProjection
    status : Bool

mkOpfibrationAdapter :
  (d : C2S8.OpfibrationDeclaration) →
  (proj : C2S8.FibrationProjectionFunctor) →
  (pproj : C2S8.OpfibrationDeclaration.projectionFunctor d ≡ proj) →
  (f : ⊤ → C2S8.OpfibrationDeclaration) →
  OpfibrationAdapter
mkOpfibrationAdapter d proj pproj f =
  record { decl = d ; expProjection = proj
         ; linkProjection = pproj ; status = true }

opfibrationCategorical : OpfibrationAdapter → CategoricalAdapter {lsuc lzero} C2S8.OpfibrationDeclaration
opfibrationCategorical adapt = mkCategoricalAdapter C2S8.OpfibrationDeclaration (λ _ → OpfibrationAdapter.decl adapt)

isFilledOpfibration : OpfibrationAdapter → Bool
isFilledOpfibration a = OpfibrationAdapter.status a

-- ==========================================================
-- Additional Chapter 2, Level 2.1 adapters
-- ==========================================================

record ShortExactSequenceAdapter : Set₁ where
  field
    decl : C2S1.ShortExactSequenceDeclaration
    expA : M.Identifier
    expB : M.Identifier
    expC : M.Identifier
    linkA : C2S1.ShortExactSequenceDeclaration.A decl ≡ expA
    linkB : C2S1.ShortExactSequenceDeclaration.B decl ≡ expB
    linkC : C2S1.ShortExactSequenceDeclaration.C decl ≡ expC
    status : Bool

mkShortExactSequenceAdapter :
  (d : C2S1.ShortExactSequenceDeclaration) →
  (a b c : M.Identifier) →
  (pa : C2S1.ShortExactSequenceDeclaration.A d ≡ a) →
  (pb : C2S1.ShortExactSequenceDeclaration.B d ≡ b) →
  (pc : C2S1.ShortExactSequenceDeclaration.C d ≡ c) →
  ShortExactSequenceAdapter
mkShortExactSequenceAdapter d a b c pa pb pc =
  record { decl = d ; expA = a ; expB = b ; expC = c
         ; linkA = pa ; linkB = pb ; linkC = pc ; status = true }

isFilledShortExactSequence : ShortExactSequenceAdapter → Bool
isFilledShortExactSequence a = ShortExactSequenceAdapter.status a

shortExactSequenceCategorical : ShortExactSequenceAdapter →
  CategoricalAdapter {lzero} C2S1.ShortExactSequenceDeclaration
shortExactSequenceCategorical adapt =
  mkCategoricalAdapter C2S1.ShortExactSequenceDeclaration (λ _ → ShortExactSequenceAdapter.decl adapt)

record ZeroMorphismAdapter : Set₁ where
  field
    decl : C2S1.ZeroMorphismDeclaration
    expFrom : M.Identifier
    expTo : M.Identifier
    expViaZero : M.Identifier
    linkFrom : C2S1.ZeroMorphismDeclaration.from decl ≡ expFrom
    linkTo : C2S1.ZeroMorphismDeclaration.to decl ≡ expTo
    linkViaZero : C2S1.ZeroMorphismDeclaration.viaZeroObject decl ≡ expViaZero
    status : Bool

mkZeroMorphismAdapter :
  (d : C2S1.ZeroMorphismDeclaration) →
  (from to via : M.Identifier) →
  (pfrom : C2S1.ZeroMorphismDeclaration.from d ≡ from) →
  (pto : C2S1.ZeroMorphismDeclaration.to d ≡ to) →
  (pvia : C2S1.ZeroMorphismDeclaration.viaZeroObject d ≡ via) →
  ZeroMorphismAdapter
mkZeroMorphismAdapter d from to via pfrom pto pvia =
  record { decl = d ; expFrom = from ; expTo = to ; expViaZero = via
         ; linkFrom = pfrom ; linkTo = pto ; linkViaZero = pvia ; status = true }

isFilledZeroMorphism : ZeroMorphismAdapter → Bool
isFilledZeroMorphism a = ZeroMorphismAdapter.status a

zeroMorphismCategorical : ZeroMorphismAdapter →
  CategoricalAdapter {lzero} C2S1.ZeroMorphismDeclaration
zeroMorphismCategorical adapt =
  mkCategoricalAdapter C2S1.ZeroMorphismDeclaration (λ _ → ZeroMorphismAdapter.decl adapt)

record TorsionTheoryAdapter : Set₁ where
  field
    decl : C2S1.TorsionTheoryDeclaration
    expCategory : M.Identifier
    expTorsionClass : M.Identifier
    expTorsionFreeClass : M.Identifier
    linkCat : C2S1.TorsionTheoryDeclaration.category decl ≡ expCategory
    linkTorsion : C2S1.TorsionTheoryDeclaration.torsionClass decl ≡ expTorsionClass
    linkTorsionFree : C2S1.TorsionTheoryDeclaration.torsionFreeClass decl ≡ expTorsionFreeClass
    status : Bool

mkTorsionTheoryAdapter :
  (d : C2S1.TorsionTheoryDeclaration) →
  (cat tclass tfclass : M.Identifier) →
  (pcat : C2S1.TorsionTheoryDeclaration.category d ≡ cat) →
  (pt : C2S1.TorsionTheoryDeclaration.torsionClass d ≡ tclass) →
  (ptf : C2S1.TorsionTheoryDeclaration.torsionFreeClass d ≡ tfclass) →
  TorsionTheoryAdapter
mkTorsionTheoryAdapter d cat tclass tfclass pcat pt ptf =
  record { decl = d ; expCategory = cat ; expTorsionClass = tclass ; expTorsionFreeClass = tfclass
         ; linkCat = pcat ; linkTorsion = pt ; linkTorsionFree = ptf ; status = true }

isFilledTorsionTheory : TorsionTheoryAdapter → Bool
isFilledTorsionTheory a = TorsionTheoryAdapter.status a

torsionTheoryCategorical : TorsionTheoryAdapter →
  CategoricalAdapter {lzero} C2S1.TorsionTheoryDeclaration
torsionTheoryCategorical adapt =
  mkCategoricalAdapter C2S1.TorsionTheoryDeclaration (λ _ → TorsionTheoryAdapter.decl adapt)

-- ==========================================================
-- Additional Chapter 2, Level 2.3 adapters
-- ==========================================================

record BialgebraAdapter : Set₁ where
  field
    decl : C2S3.BialgebraDeclaration
    expCarrier : M.Identifier
    expModel1 : M.Identifier
    expModel2 : M.Identifier
    linkCarrier : C2S3.BialgebraDeclaration.carrier decl ≡ expCarrier
    linkModel1 : C2S3.BialgebraDeclaration.model1 decl ≡ expModel1
    linkModel2 : C2S3.BialgebraDeclaration.model2 decl ≡ expModel2
    status : Bool

mkBialgebraAdapter :
  (d : C2S3.BialgebraDeclaration) →
  (car m1 m2 : M.Identifier) →
  (pcar : C2S3.BialgebraDeclaration.carrier d ≡ car) →
  (pm1 : C2S3.BialgebraDeclaration.model1 d ≡ m1) →
  (pm2 : C2S3.BialgebraDeclaration.model2 d ≡ m2) →
  BialgebraAdapter
mkBialgebraAdapter d car m1 m2 pcar pm1 pm2 =
  record { decl = d ; expCarrier = car ; expModel1 = m1 ; expModel2 = m2
         ; linkCarrier = pcar ; linkModel1 = pm1 ; linkModel2 = pm2 ; status = true }

isFilledBialgebra : BialgebraAdapter → Bool
isFilledBialgebra a = BialgebraAdapter.status a

bialgebraCategorical : BialgebraAdapter →
  CategoricalAdapter {lzero} C2S3.BialgebraDeclaration
bialgebraCategorical adapt =
  mkCategoricalAdapter C2S3.BialgebraDeclaration (λ _ → BialgebraAdapter.decl adapt)

-- ==========================================================
-- Additional Chapter 2, Level 2.4 adapters
-- ==========================================================

record ComonadAdapter : Set₁ where
  field
    decl : C2S4.ComonadDeclaration
    expName : M.Identifier
    expDatum : C2S4.ComonadData
    linkName : C2S4.ComonadDeclaration.name decl ≡ expName
    linkDatum : C2S4.ComonadDeclaration.datum decl ≡ expDatum
    status : Bool

mkComonadAdapter :
  (d : C2S4.ComonadDeclaration) →
  (n : M.Identifier) →
  (dat : C2S4.ComonadData) →
  (pn : C2S4.ComonadDeclaration.name d ≡ n) →
  (pdat : C2S4.ComonadDeclaration.datum d ≡ dat) →
  ComonadAdapter
mkComonadAdapter d n dat pn pdat =
  record { decl = d ; expName = n ; expDatum = dat
         ; linkName = pn ; linkDatum = pdat ; status = true }

isFilledComonad : ComonadAdapter → Bool
isFilledComonad a = ComonadAdapter.status a

comonadCategorical : ComonadAdapter →
  CategoricalAdapter {lzero} C2S4.ComonadDeclaration
comonadCategorical adapt =
  mkCategoricalAdapter C2S4.ComonadDeclaration (λ _ → ComonadAdapter.decl adapt)

-- ==========================================================
-- Additional Chapter 2, Level 2.5 adapters
-- ==========================================================

record AccessibleCategoryAdapter : Set₁ where
  field
    decl : C2S5.AccessibleCategoryDeclaration
    expCat : C1S3.CategoryDeclaration
    expRank : C1S6.RegularCardinal
    linkCat : C2S5.AccessibleCategoryDeclaration.category decl ≡ expCat
    linkRank : C2S5.AccessibleCategoryDeclaration.rank decl ≡ expRank
    status : Bool

mkAccessibleCategoryAdapter :
  (d : C2S5.AccessibleCategoryDeclaration) →
  (cat : C1S3.CategoryDeclaration) →
  (rk : C1S6.RegularCardinal) →
  (pcat : C2S5.AccessibleCategoryDeclaration.category d ≡ cat) →
  (prk : C2S5.AccessibleCategoryDeclaration.rank d ≡ rk) →
  AccessibleCategoryAdapter
mkAccessibleCategoryAdapter d cat rk pcat prk =
  record { decl = d ; expCat = cat ; expRank = rk
         ; linkCat = pcat ; linkRank = prk ; status = true }

isFilledAccessibleCategory : AccessibleCategoryAdapter → Bool
isFilledAccessibleCategory a = AccessibleCategoryAdapter.status a

accessibleCategoryCategorical : AccessibleCategoryAdapter →
  CategoricalAdapter {lsuc lzero} C2S5.AccessibleCategoryDeclaration
accessibleCategoryCategorical adapt =
  mkCategoricalAdapter C2S5.AccessibleCategoryDeclaration (λ _ → AccessibleCategoryAdapter.decl adapt)

record SketchAdapter : Set₁ where
  field
    decl : C2S5.SketchDeclaration
    expUnderlyingCat : C1S3.CategoryDeclaration
    linkUnderlyingCat : C2S5.SketchDeclaration.underlyingCategory decl ≡ expUnderlyingCat
    status : Bool

mkSketchAdapter :
  (d : C2S5.SketchDeclaration) →
  (cat : C1S3.CategoryDeclaration) →
  (pcat : C2S5.SketchDeclaration.underlyingCategory d ≡ cat) →
  SketchAdapter
mkSketchAdapter d cat pcat =
  record { decl = d ; expUnderlyingCat = cat
         ; linkUnderlyingCat = pcat ; status = true }

isFilledSketch : SketchAdapter → Bool
isFilledSketch a = SketchAdapter.status a

sketchCategorical : SketchAdapter →
  CategoricalAdapter {lsuc lzero} C2S5.SketchDeclaration
sketchCategorical adapt =
  mkCategoricalAdapter C2S5.SketchDeclaration (λ _ → SketchAdapter.decl adapt)

-- ==========================================================
-- Chapter 3, Level 3.1 (Locales & Frames) - Additional
-- ==========================================================

record HeytingAlgebraAdapter : Set₁ where
  field
    decl : C3S1.HeytingAlgebraDeclaration
    status : Bool

mkHeytingAlgebraAdapter :
  (d : C3S1.HeytingAlgebraDeclaration) →
  HeytingAlgebraAdapter
mkHeytingAlgebraAdapter d =
  record { decl = d ; status = true }

isFilledHeytingAlgebra : HeytingAlgebraAdapter → Bool
isFilledHeytingAlgebra a = HeytingAlgebraAdapter.status a

-- Categorical view for Heyting Algebra
heytingAlgebraCategorical : HeytingAlgebraAdapter → CategoricalAdapter {lsuc lzero} C3S1.HeytingAlgebraDeclaration
heytingAlgebraCategorical adapt = mkCategoricalAdapter C3S1.HeytingAlgebraDeclaration (λ _ → HeytingAlgebraAdapter.decl adapt)

record FrameAdapter : Set₁ where
  field
    decl : C3S1.FrameDeclaration
    expHeyting : C3S1.HeytingAlgebraDeclaration
    linkHeyting : C3S1.FrameDeclaration.underlyingHeytingAlgebra decl ≡ expHeyting
    status : Bool

mkFrameAdapter :
  (d : C3S1.FrameDeclaration) →
  (h : C3S1.HeytingAlgebraDeclaration) →
  (ph : C3S1.FrameDeclaration.underlyingHeytingAlgebra d ≡ h) →
  FrameAdapter
mkFrameAdapter d h ph =
  record { decl = d ; expHeyting = h
         ; linkHeyting = ph ; status = true }

isFilledFrame : FrameAdapter → Bool
isFilledFrame a = FrameAdapter.status a

-- Categorical view for Frame
frameCategorical : FrameAdapter → CategoricalAdapter {lsuc lzero} C3S1.FrameDeclaration
frameCategorical adapt = mkCategoricalAdapter C3S1.FrameDeclaration (λ _ → FrameAdapter.decl adapt)

record LocaleAdapter : Set₁ where
  field
    decl : C3S1.LocaleDeclaration
    expFrame : C3S1.FrameDeclaration
    linkFrame : C3S1.LocaleDeclaration.associatedFrame decl ≡ expFrame
    status : Bool

mkLocaleAdapter :
  (d : C3S1.LocaleDeclaration) →
  (f : C3S1.FrameDeclaration) →
  (pf : C3S1.LocaleDeclaration.associatedFrame d ≡ f) →
  LocaleAdapter
mkLocaleAdapter d f pf =
  record { decl = d ; expFrame = f
         ; linkFrame = pf ; status = true }

isFilledLocale : LocaleAdapter → Bool
isFilledLocale a = LocaleAdapter.status a

-- Categorical view for Locale
localeCategorical : LocaleAdapter → CategoricalAdapter {lsuc lzero} C3S1.LocaleDeclaration
localeCategorical adapt = mkCategoricalAdapter C3S1.LocaleDeclaration (λ _ → LocaleAdapter.decl adapt)

record LocaleMorphismAdapter : Set₁ where
  field
    decl : C3S1.LocaleMorphismDeclaration
    expSource : C3S1.LocaleDeclaration
    expTarget : C3S1.LocaleDeclaration
    linkSource : C3S1.LocaleMorphismDeclaration.sourceLocale decl ≡ expSource
    linkTarget : C3S1.LocaleMorphismDeclaration.targetLocale decl ≡ expTarget
    status : Bool

mkLocaleMorphismAdapter :
  (d : C3S1.LocaleMorphismDeclaration) →
  (src tgt : C3S1.LocaleDeclaration) →
  (psrc : C3S1.LocaleMorphismDeclaration.sourceLocale d ≡ src) →
  (ptgt : C3S1.LocaleMorphismDeclaration.targetLocale d ≡ tgt) →
  LocaleMorphismAdapter
mkLocaleMorphismAdapter d src tgt psrc ptgt =
  record { decl = d ; expSource = src ; expTarget = tgt
         ; linkSource = psrc ; linkTarget = ptgt ; status = true }

isFilledLocaleMorphism : LocaleMorphismAdapter → Bool
isFilledLocaleMorphism a = LocaleMorphismAdapter.status a

-- Categorical view for Locale Morphism
localeMorphismCategorical : LocaleMorphismAdapter → CategoricalAdapter {lsuc lzero} C3S1.LocaleMorphismDeclaration
localeMorphismCategorical adapt = mkCategoricalAdapter C3S1.LocaleMorphismDeclaration (λ _ → LocaleMorphismAdapter.decl adapt)

record NucleusAdapter : Set₁ where
  field
    decl : C3S1.NucleusDeclaration
    expFrame : C3S1.FrameDeclaration
    linkFrame : C3S1.NucleusDeclaration.frame decl ≡ expFrame
    status : Bool

mkNucleusAdapter :
  (d : C3S1.NucleusDeclaration) →
  (f : C3S1.FrameDeclaration) →
  (pf : C3S1.NucleusDeclaration.frame d ≡ f) →
  NucleusAdapter
mkNucleusAdapter d f pf =
  record { decl = d ; expFrame = f
         ; linkFrame = pf ; status = true }

isFilledNucleus : NucleusAdapter → Bool
isFilledNucleus a = NucleusAdapter.status a

-- Categorical view for Nucleus
nucleusCategorical : NucleusAdapter → CategoricalAdapter {lsuc lzero} C3S1.NucleusDeclaration
nucleusCategorical adapt = mkCategoricalAdapter C3S1.NucleusDeclaration (λ _ → NucleusAdapter.decl adapt)

record SublocaleAdapter : Set₁ where
  field
    decl : C3S1.SublocaleDeclaration
    expSublocale : C3S1.LocaleDeclaration
    expParent : C3S1.LocaleDeclaration
    linkSublocale : C3S1.SublocaleDeclaration.sublocale decl ≡ expSublocale
    linkParent : C3S1.SublocaleDeclaration.parentLocale decl ≡ expParent
    status : Bool

mkSublocaleAdapter :
  (d : C3S1.SublocaleDeclaration) →
  (sub : C3S1.LocaleDeclaration) →
  (par : C3S1.LocaleDeclaration) →
  (psub : C3S1.SublocaleDeclaration.sublocale d ≡ sub) →
  (ppar : C3S1.SublocaleDeclaration.parentLocale d ≡ par) →
  SublocaleAdapter
mkSublocaleAdapter d sub par psub ppar =
  record { decl = d ; expSublocale = sub ; expParent = par
         ; linkSublocale = psub ; linkParent = ppar ; status = true }

isFilledSublocale : SublocaleAdapter → Bool
isFilledSublocale a = SublocaleAdapter.status a

-- Categorical view for Sublocale
sublocaleCategorical : SublocaleAdapter → CategoricalAdapter {lsuc lzero} C3S1.SublocaleDeclaration
sublocaleCategorical adapt = mkCategoricalAdapter C3S1.SublocaleDeclaration (λ _ → SublocaleAdapter.decl adapt)

record OpenLocaleMorphismAdapter : Set₁ where
  field
    decl : C3S1.OpenLocaleMorphismDeclaration
    expMorphism : C3S1.LocaleMorphismDeclaration
    linkMorphism : C3S1.OpenLocaleMorphismDeclaration.localeMorphism decl ≡ expMorphism
    status : Bool

mkOpenLocaleMorphismAdapter :
  (d : C3S1.OpenLocaleMorphismDeclaration) →
  (m : C3S1.LocaleMorphismDeclaration) →
  (pm : C3S1.OpenLocaleMorphismDeclaration.localeMorphism d ≡ m) →
  OpenLocaleMorphismAdapter
mkOpenLocaleMorphismAdapter d m pm =
  record { decl = d ; expMorphism = m
         ; linkMorphism = pm ; status = true }

isFilledOpenLocaleMorphism : OpenLocaleMorphismAdapter → Bool
isFilledOpenLocaleMorphism a = OpenLocaleMorphismAdapter.status a

-- Categorical view for Open Locale Morphism
openLocaleMorphismCategorical : OpenLocaleMorphismAdapter → CategoricalAdapter {lsuc lzero} C3S1.OpenLocaleMorphismDeclaration
openLocaleMorphismCategorical adapt = mkCategoricalAdapter C3S1.OpenLocaleMorphismDeclaration (λ _ → OpenLocaleMorphismAdapter.decl adapt)

record SoberSpaceAdapter : Set₁ where
  field
    decl : C3S1.SoberSpaceDeclaration
    status : Core.Phase.Bool

mkSoberSpaceAdapter :
  (d : C3S1.SoberSpaceDeclaration) →
  SoberSpaceAdapter
mkSoberSpaceAdapter d =
  record { decl = d ; status = true }

isFilledSoberSpace : SoberSpaceAdapter → Core.Phase.Bool
isFilledSoberSpace a = SoberSpaceAdapter.status a

-- Categorical view for Sober Space
soberSpaceCategorical : SoberSpaceAdapter → CategoricalAdapter {lsuc lzero} C3S1.SoberSpaceDeclaration
soberSpaceCategorical adapt = mkCategoricalAdapter C3S1.SoberSpaceDeclaration (λ _ → SoberSpaceAdapter.decl adapt)

record SpatialLocaleAdapter : Set₁ where
  field
    decl : C3S1.SpatialLocaleDeclaration
    expLocale : C3S1.LocaleDeclaration
    linkLocale : C3S1.SpatialLocaleDeclaration.locale decl ≡ expLocale
    status : Core.Phase.Bool

mkSpatialLocaleAdapter :
  (d : C3S1.SpatialLocaleDeclaration) →
  (loc : C3S1.LocaleDeclaration) →
  (ploc : C3S1.SpatialLocaleDeclaration.locale d ≡ loc) →
  SpatialLocaleAdapter
mkSpatialLocaleAdapter d loc ploc =
  record { decl = d ; expLocale = loc
         ; linkLocale = ploc ; status = true }

isFilledSpatialLocale : SpatialLocaleAdapter → Core.Phase.Bool
isFilledSpatialLocale a = SpatialLocaleAdapter.status a

-- Categorical view for Spatial Locale
spatialLocaleCategorical : SpatialLocaleAdapter → CategoricalAdapter {lsuc lzero} C3S1.SpatialLocaleDeclaration
spatialLocaleCategorical adapt = mkCategoricalAdapter C3S1.SpatialLocaleDeclaration (λ _ → SpatialLocaleAdapter.decl adapt)

-- ==========================================================
-- Chapter 3, Level 3.2 (Sheaves & Toposes) - Additional
-- ==========================================================

record SheafOnLocaleAdapter : Set₁ where
  field
    decl : C3S2.SheafOnLocaleDeclaration
    expPresheaf : C3S2.PresheafOnLocale
    linkPresheaf : C3S2.SheafOnLocaleDeclaration.underlyingPresheaf decl ≡ expPresheaf
    status : Core.Phase.Bool

mkSheafOnLocaleAdapter :
  (d : C3S2.SheafOnLocaleDeclaration) →
  (psh : C3S2.PresheafOnLocale) →
  (ppsh : C3S2.SheafOnLocaleDeclaration.underlyingPresheaf d ≡ psh) →
  SheafOnLocaleAdapter
mkSheafOnLocaleAdapter d psh ppsh =
  record { decl = d ; expPresheaf = psh
         ; linkPresheaf = ppsh ; status = true }

isFilledSheafOnLocale : SheafOnLocaleAdapter → Core.Phase.Bool
isFilledSheafOnLocale a = SheafOnLocaleAdapter.status a

-- Categorical view for Sheaf on Locale
sheafOnLocaleCategorical : SheafOnLocaleAdapter → CategoricalAdapter {lsuc lzero} C3S2.SheafOnLocaleDeclaration
sheafOnLocaleCategorical adapt = mkCategoricalAdapter C3S2.SheafOnLocaleDeclaration (λ _ → SheafOnLocaleAdapter.decl adapt)

record GrothendieckToposAdapter : Set₁ where
  field
    decl : C3S2.GrothendieckToposDeclaration
    expCategory : C1S3.CategoryDeclaration
    linkCategory : C3S2.GrothendieckToposDeclaration.category decl ≡ expCategory
    status : Core.Phase.Bool

mkGrothendieckToposAdapter :
  (d : C3S2.GrothendieckToposDeclaration) →
  (cat : C1S3.CategoryDeclaration) →
  (pcat : C3S2.GrothendieckToposDeclaration.category d ≡ cat) →
  GrothendieckToposAdapter
mkGrothendieckToposAdapter d cat pcat =
  record { decl = d ; expCategory = cat
         ; linkCategory = pcat ; status = true }

isFilledGrothendieckTopos : GrothendieckToposAdapter → Core.Phase.Bool
isFilledGrothendieckTopos a = GrothendieckToposAdapter.status a

-- Categorical view for Grothendieck Topos
grothendieckToposCategorical : GrothendieckToposAdapter → CategoricalAdapter {lsuc lzero} C3S2.GrothendieckToposDeclaration
grothendieckToposCategorical adapt = mkCategoricalAdapter C3S2.GrothendieckToposDeclaration (λ _ → GrothendieckToposAdapter.decl adapt)

record OmegaSetAdapter : Set₁ where
  field
    decl : C3S2.OmegaSetDeclarationVerified
    expData : C3S2.OmegaSetData
    linkData : C3S2.OmegaSetDeclarationVerified.dataOmegaSet decl ≡ expData
    status : Core.Phase.Bool

mkOmegaSetAdapter :
  (d : C3S2.OmegaSetDeclarationVerified) →
  (data' : C3S2.OmegaSetData) →
  (pdata : C3S2.OmegaSetDeclarationVerified.dataOmegaSet d ≡ data') →
  OmegaSetAdapter
mkOmegaSetAdapter d data' pdata =
  record { decl = d ; expData = data'
         ; linkData = pdata ; status = true }

isFilledOmegaSet : OmegaSetAdapter → Core.Phase.Bool
isFilledOmegaSet a = OmegaSetAdapter.status a

-- Categorical view for OmegaSet
omegaSetCategorical : OmegaSetAdapter → CategoricalAdapter {lsuc lzero} C3S2.OmegaSetDeclarationVerified
omegaSetCategorical adapt = mkCategoricalAdapter C3S2.OmegaSetDeclarationVerified (λ _ → OmegaSetAdapter.decl adapt)

-- Presheaf on locale
record PresheafOnLocaleAdapter : Set₁ where
  field
    decl : C3S2.PresheafOnLocale
    status : Core.Phase.Bool

mkPresheafOnLocaleAdapter : C3S2.PresheafOnLocale → PresheafOnLocaleAdapter
mkPresheafOnLocaleAdapter d = record { decl = d ; status = true }

isFilledPresheafOnLocale : PresheafOnLocaleAdapter → Core.Phase.Bool
isFilledPresheafOnLocale a = PresheafOnLocaleAdapter.status a

-- Sheaf gluing axiom
record SheafGluingAxiomAdapter : Set₁ where
  field
    decl : C3S2.SheafGluingAxiom
    expPresheaf : C3S2.PresheafOnLocale
    linkPresheaf : C3S2.SheafGluingAxiom.presheaf decl ≡ expPresheaf
    status : Core.Phase.Bool

mkSheafGluingAxiomAdapter :
  (d : C3S2.SheafGluingAxiom) →
  (psh : C3S2.PresheafOnLocale) →
  (ppsh : C3S2.SheafGluingAxiom.presheaf d ≡ psh) →
  SheafGluingAxiomAdapter
mkSheafGluingAxiomAdapter d psh ppsh =
  record { decl = d ; expPresheaf = psh ; linkPresheaf = ppsh ; status = true }

isFilledSheafGluingAxiom : SheafGluingAxiomAdapter → Core.Phase.Bool
isFilledSheafGluingAxiom a = SheafGluingAxiomAdapter.status a

-- Category of sheaves
record CategoryOfSheavesAdapter : Set₁ where
  field
    decl : C3S2.CategoryOfSheaves
    expCategory : C1S3.CategoryDeclaration
    linkCategory : C3S2.CategoryOfSheaves.underlyingCategory decl ≡ expCategory
    status : Core.Phase.Bool

mkCategoryOfSheavesAdapter :
  (d : C3S2.CategoryOfSheaves) →
  (cat : C1S3.CategoryDeclaration) →
  (pcat : C3S2.CategoryOfSheaves.underlyingCategory d ≡ cat) →
  CategoryOfSheavesAdapter
mkCategoryOfSheavesAdapter d cat pcat =
  record { decl = d ; expCategory = cat ; linkCategory = pcat ; status = true }

isFilledCategoryOfSheaves : CategoryOfSheavesAdapter → Core.Phase.Bool
isFilledCategoryOfSheaves a = CategoryOfSheavesAdapter.status a

-- Category of sheaves is a topos theorem
record CategoryOfSheavesIsAToposTheoremAdapter : Set₁ where
  field
    decl : C3S2.CategoryOfSheavesIsAToposTheorem
    expSheafCat : C3S2.CategoryOfSheaves
    expTopos : C3S2.GrothendieckToposDeclaration
    linkSheafCat : C3S2.CategoryOfSheavesIsAToposTheorem.sheafCategory decl ≡ expSheafCat
    linkTopos : C3S2.CategoryOfSheavesIsAToposTheorem.isGrothendieckTopos decl ≡ expTopos
    status : Core.Phase.Bool

mkCategoryOfSheavesIsAToposTheoremAdapter :
  (d : C3S2.CategoryOfSheavesIsAToposTheorem) →
  (sc : C3S2.CategoryOfSheaves) →
  (tp : C3S2.GrothendieckToposDeclaration) →
  (psc : C3S2.CategoryOfSheavesIsAToposTheorem.sheafCategory d ≡ sc) →
  (ptp : C3S2.CategoryOfSheavesIsAToposTheorem.isGrothendieckTopos d ≡ tp) →
  CategoryOfSheavesIsAToposTheoremAdapter
mkCategoryOfSheavesIsAToposTheoremAdapter d sc tp psc ptp =
  record { decl = d ; expSheafCat = sc ; expTopos = tp
         ; linkSheafCat = psc ; linkTopos = ptp ; status = true }

isFilledCategoryOfSheavesIsAToposTheorem : CategoryOfSheavesIsAToposTheoremAdapter → Core.Phase.Bool
isFilledCategoryOfSheavesIsAToposTheorem a = CategoryOfSheavesIsAToposTheoremAdapter.status a

-- Exponential object in sheaf category
record ExponentialObjectSheafAdapter : Set₁ where
  field
    decl : C3S2.ExponentialObjectSheaf
    expBase : C3S2.SheafOnLocaleDeclaration
    expExponent : C3S2.SheafOnLocaleDeclaration
    linkBase : C3S2.ExponentialObjectSheaf.baseSheaf decl ≡ expBase
    linkExponent : C3S2.ExponentialObjectSheaf.exponentSheaf decl ≡ expExponent
    status : Core.Phase.Bool

mkExponentialObjectSheafAdapter :
  (d : C3S2.ExponentialObjectSheaf) →
  (b : C3S2.SheafOnLocaleDeclaration) →
  (e : C3S2.SheafOnLocaleDeclaration) →
  (pb : C3S2.ExponentialObjectSheaf.baseSheaf d ≡ b) →
  (pe : C3S2.ExponentialObjectSheaf.exponentSheaf d ≡ e) →
  ExponentialObjectSheafAdapter
mkExponentialObjectSheafAdapter d b e pb pe =
  record { decl = d ; expBase = b ; expExponent = e
         ; linkBase = pb ; linkExponent = pe ; status = true }

isFilledExponentialObjectSheaf : ExponentialObjectSheafAdapter → Core.Phase.Bool
isFilledExponentialObjectSheaf a = ExponentialObjectSheafAdapter.status a

-- Subobject classifier
record SubobjectClassifierAxiomAdapter : Set₁ where
  field
    decl : C3S2.SubobjectClassifierAxiom
    expCharMap : C3S2.CharacteristicMapConstructor
    linkCharMap : C3S2.SubobjectClassifierAxiom.characteristicMap decl ≡ expCharMap
    status : Core.Phase.Bool

mkSubobjectClassifierAxiomAdapter :
  (d : C3S2.SubobjectClassifierAxiom) →
  (cm : C3S2.CharacteristicMapConstructor) →
  (pcm : C3S2.SubobjectClassifierAxiom.characteristicMap d ≡ cm) →
  SubobjectClassifierAxiomAdapter
mkSubobjectClassifierAxiomAdapter d cm pcm =
  record { decl = d ; expCharMap = cm ; linkCharMap = pcm ; status = true }

isFilledSubobjectClassifierAxiom : SubobjectClassifierAxiomAdapter → Core.Phase.Bool
isFilledSubobjectClassifierAxiom a = SubobjectClassifierAxiomAdapter.status a

-- Étale space
record EtaleSpaceOverAdapter : Set₁ where
  field
    decl : C3S2.EtaleSpaceOver
    expProj : M.Identifier
    linkProj : C3S2.EtaleSpaceOver.projection decl ≡ expProj
    status : Core.Phase.Bool

mkEtaleSpaceOverAdapter :
  (d : C3S2.EtaleSpaceOver) →
  (p : M.Identifier) →
  (pp : C3S2.EtaleSpaceOver.projection d ≡ p) →
  EtaleSpaceOverAdapter
mkEtaleSpaceOverAdapter d p pp =
  record { decl = d ; expProj = p ; linkProj = pp ; status = true }

isFilledEtaleSpaceOver : EtaleSpaceOverAdapter → Core.Phase.Bool
isFilledEtaleSpaceOver a = EtaleSpaceOverAdapter.status a

-- Category of étale spaces
record CategoryOfEtaleSpacesAdapter : Set₁ where
  field
    decl : C3S2.CategoryOfEtaleSpaces
    expCategory : C1S3.CategoryDeclaration
    linkCategory : C3S2.CategoryOfEtaleSpaces.categoryStructure decl ≡ expCategory
    status : Core.Phase.Bool

mkCategoryOfEtaleSpacesAdapter :
  (d : C3S2.CategoryOfEtaleSpaces) →
  (cat : C1S3.CategoryDeclaration) →
  (pcat : C3S2.CategoryOfEtaleSpaces.categoryStructure d ≡ cat) →
  CategoryOfEtaleSpacesAdapter
mkCategoryOfEtaleSpacesAdapter d cat pcat =
  record { decl = d ; expCategory = cat ; linkCategory = pcat ; status = true }

isFilledCategoryOfEtaleSpaces : CategoryOfEtaleSpacesAdapter → Core.Phase.Bool
isFilledCategoryOfEtaleSpaces a = CategoryOfEtaleSpacesAdapter.status a

-- Stalk constructor
record StalkConstructorAdapter : Set₁ where
  field
    decl : C3S2.StalkConstructor
    expPresheaf : C3S2.PresheafOnLocale
    linkPresheaf : C3S2.StalkConstructor.presheaf decl ≡ expPresheaf
    status : Core.Phase.Bool

mkStalkConstructorAdapter :
  (d : C3S2.StalkConstructor) →
  (psh : C3S2.PresheafOnLocale) →
  (ppsh : C3S2.StalkConstructor.presheaf d ≡ psh) →
  StalkConstructorAdapter
mkStalkConstructorAdapter d psh ppsh =
  record { decl = d ; expPresheaf = psh ; linkPresheaf = ppsh ; status = true }

isFilledStalkConstructor : StalkConstructorAdapter → Core.Phase.Bool
isFilledStalkConstructor a = StalkConstructorAdapter.status a

-- Total space of stalks
record TotalSpaceOfStalksAdapter : Set₁ where
  field
    decl : C3S2.TotalSpaceOfStalks
    expPresheaf : C3S2.PresheafOnLocale
    linkPresheaf : C3S2.TotalSpaceOfStalks.presheaf decl ≡ expPresheaf
    status : Core.Phase.Bool

mkTotalSpaceOfStalksAdapter :
  (d : C3S2.TotalSpaceOfStalks) →
  (psh : C3S2.PresheafOnLocale) →
  (ppsh : C3S2.TotalSpaceOfStalks.presheaf d ≡ psh) →
  TotalSpaceOfStalksAdapter
mkTotalSpaceOfStalksAdapter d psh ppsh =
  record { decl = d ; expPresheaf = psh ; linkPresheaf = ppsh ; status = true }

isFilledTotalSpaceOfStalks : TotalSpaceOfStalksAdapter → Core.Phase.Bool
isFilledTotalSpaceOfStalks a = TotalSpaceOfStalksAdapter.status a

-- Sheaf of sections functor
record SheafOfSectionsFunctorAdapter : Set₁ where
  field
    decl : C3S2.SheafOfSectionsFunctor
    expEtale : C3S2.EtaleSpaceOver
    expSheaf : C3S2.SheafOnLocaleDeclaration
    linkEtale : C3S2.SheafOfSectionsFunctor.etaleSpace decl ≡ expEtale
    linkSheaf : C3S2.SheafOfSectionsFunctor.isSheaf decl ≡ expSheaf
    status : Core.Phase.Bool

mkSheafOfSectionsFunctorAdapter :
  (d : C3S2.SheafOfSectionsFunctor) →
  (et : C3S2.EtaleSpaceOver) →
  (sh : C3S2.SheafOnLocaleDeclaration) →
  (pet : C3S2.SheafOfSectionsFunctor.etaleSpace d ≡ et) →
  (psh : C3S2.SheafOfSectionsFunctor.isSheaf d ≡ sh) →
  SheafOfSectionsFunctorAdapter
mkSheafOfSectionsFunctorAdapter d et sh pet psh =
  record { decl = d ; expEtale = et ; expSheaf = sh
         ; linkEtale = pet ; linkSheaf = psh ; status = true }

isFilledSheafOfSectionsFunctor : SheafOfSectionsFunctorAdapter → Core.Phase.Bool
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
    status : Core.Phase.Bool

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
  SheafEtaleEquivalenceTheoremAdapter
mkSheafEtaleEquivalenceTheoremAdapter d sc ec sf tf psc pec psf ptf =
  record { decl = d ; expSheafCat = sc ; expEtaleCat = ec
         ; expStalksF = sf ; expSectionsF = tf
         ; linkSheafCat = psc ; linkEtaleCat = pec
         ; linkStalksF = psf ; linkSectionsF = ptf ; status = true }

isFilledSheafEtaleEquivalenceTheorem : SheafEtaleEquivalenceTheoremAdapter → Core.Phase.Bool
isFilledSheafEtaleEquivalenceTheorem a = SheafEtaleEquivalenceTheoremAdapter.status a

-- Direct image functor
record DirectImageFunctorLocaleAdapter : Set₁ where
  field
    decl : C3S2.DirectImageFunctorLocale
    expFunctor : M.Identifier
    linkFunctor : C3S2.DirectImageFunctorLocale.underlyingFunctor decl ≡ expFunctor
    status : Core.Phase.Bool

mkDirectImageFunctorLocaleAdapter :
  (d : C3S2.DirectImageFunctorLocale) →
  (f : M.Identifier) →
  (pf : C3S2.DirectImageFunctorLocale.underlyingFunctor d ≡ f) →
  DirectImageFunctorLocaleAdapter
mkDirectImageFunctorLocaleAdapter d f pf =
  record { decl = d ; expFunctor = f ; linkFunctor = pf ; status = true }

isFilledDirectImageFunctorLocale : DirectImageFunctorLocaleAdapter → Core.Phase.Bool
isFilledDirectImageFunctorLocale a = DirectImageFunctorLocaleAdapter.status a

-- Inverse image functor
record InverseImageFunctorLocaleAdapter : Set₁ where
  field
    decl : C3S2.InverseImageFunctorLocale
    expFunctor : M.Identifier
    linkFunctor : C3S2.InverseImageFunctorLocale.underlyingFunctor decl ≡ expFunctor
    status : Core.Phase.Bool

mkInverseImageFunctorLocaleAdapter :
  (d : C3S2.InverseImageFunctorLocale) →
  (f : M.Identifier) →
  (pf : C3S2.InverseImageFunctorLocale.underlyingFunctor d ≡ f) →
  InverseImageFunctorLocaleAdapter
mkInverseImageFunctorLocaleAdapter d f pf =
  record { decl = d ; expFunctor = f ; linkFunctor = pf ; status = true }

isFilledInverseImageFunctorLocale : InverseImageFunctorLocaleAdapter → Core.Phase.Bool
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
    status : Core.Phase.Bool

mkLocaleChangeOfBaseAdjunctionTheoremAdapter :
  (d : C3S2.LocaleChangeOfBaseAdjunctionTheorem) →
  (inv : C3S2.InverseImageFunctorLocale) →
  (dir : C3S2.DirectImageFunctorLocale) →
  (adj : M.Identifier) →
  (pinv : C3S2.LocaleChangeOfBaseAdjunctionTheorem.inverseImageFunctor d ≡ inv) →
  (pdir : C3S2.LocaleChangeOfBaseAdjunctionTheorem.directImageFunctor d ≡ dir) →
  (padj : C3S2.LocaleChangeOfBaseAdjunctionTheorem.adjunction d ≡ adj) →
  LocaleChangeOfBaseAdjunctionTheoremAdapter
mkLocaleChangeOfBaseAdjunctionTheoremAdapter d inv dir adj pinv pdir padj =
  record { decl = d ; expInverse = inv ; expDirect = dir ; expAdj = adj
         ; linkInverse = pinv ; linkDirect = pdir ; linkAdj = padj ; status = true }

isFilledLocaleChangeOfBaseAdjunctionTheorem : LocaleChangeOfBaseAdjunctionTheoremAdapter → Core.Phase.Bool
isFilledLocaleChangeOfBaseAdjunctionTheorem a = LocaleChangeOfBaseAdjunctionTheoremAdapter.status a

-- Étale morphism induces sheaf equivalence theorem
record EtaleMorphismInducesSheafEquivalenceTheoremAdapter : Set₁ where
  field
    decl : C3S2.EtaleMorphismInducesSheafEquivalenceTheorem
    expInverse : C3S2.InverseImageFunctorLocale
    linkInverse : C3S2.EtaleMorphismInducesSheafEquivalenceTheorem.inverseImageFunctor decl ≡ expInverse
    status : Core.Phase.Bool

mkEtaleMorphismInducesSheafEquivalenceTheoremAdapter :
  (d : C3S2.EtaleMorphismInducesSheafEquivalenceTheorem) →
  (inv : C3S2.InverseImageFunctorLocale) →
  (pinv : C3S2.EtaleMorphismInducesSheafEquivalenceTheorem.inverseImageFunctor d ≡ inv) →
  EtaleMorphismInducesSheafEquivalenceTheoremAdapter
mkEtaleMorphismInducesSheafEquivalenceTheoremAdapter d inv pinv =
  record { decl = d ; expInverse = inv ; linkInverse = pinv ; status = true }

isFilledEtaleMorphismInducesSheafEquivalenceTheorem : EtaleMorphismInducesSheafEquivalenceTheoremAdapter → Core.Phase.Bool
isFilledEtaleMorphismInducesSheafEquivalenceTheorem a = EtaleMorphismInducesSheafEquivalenceTheoremAdapter.status a

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
    status : Core.Phase.Bool

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
  SheavesAreCompleteOmegaSetsRefinedTheoremAdapter
mkSheavesAreCompleteOmegaSetsRefinedTheoremAdapter d sc oc fa fs psc poc pfa pfs =
  record { decl = d ; expSheafCat = sc ; expOmegaCat = oc
         ; expFunctorA = fa ; expFunctorS = fs
         ; linkSheafCat = psc ; linkOmegaCat = poc
         ; linkFunctorA = pfa ; linkFunctorS = pfs ; status = true }

isFilledSheavesAreCompleteOmegaSetsRefinedTheorem : SheavesAreCompleteOmegaSetsRefinedTheoremAdapter → Core.Phase.Bool
isFilledSheavesAreCompleteOmegaSetsRefinedTheorem a = SheavesAreCompleteOmegaSetsRefinedTheoremAdapter.status a

-- Sheaf of rings
record SheafOfRingsAdapter : Set₁ where
  field
    decl : C3S2.SheafOfRings
    expSheaf : C3S2.SheafOnLocaleDeclaration
    linkSheaf : C3S2.SheafOfRings.underlyingSheaf decl ≡ expSheaf
    status : Core.Phase.Bool

mkSheafOfRingsAdapter :
  (d : C3S2.SheafOfRings) →
  (sh : C3S2.SheafOnLocaleDeclaration) →
  (psh : C3S2.SheafOfRings.underlyingSheaf d ≡ sh) →
  SheafOfRingsAdapter
mkSheafOfRingsAdapter d sh psh =
  record { decl = d ; expSheaf = sh ; linkSheaf = psh ; status = true }

isFilledSheafOfRings : SheafOfRingsAdapter → Core.Phase.Bool
isFilledSheafOfRings a = SheafOfRingsAdapter.status a

-- Sheaf of O-modules
record SheafOfOModulesAdapter : Set₁ where
  field
    decl : C3S2.SheafOfOModules
    expRingSheaf : C3S2.SheafOfRings
    expModSheaf : C3S2.SheafOnLocaleDeclaration
    linkRingSheaf : C3S2.SheafOfOModules.sheafOfRings decl ≡ expRingSheaf
    linkModSheaf : C3S2.SheafOfOModules.underlyingSheaf decl ≡ expModSheaf
    status : Core.Phase.Bool

mkSheafOfOModulesAdapter :
  (d : C3S2.SheafOfOModules) →
  (rs : C3S2.SheafOfRings) →
  (ms : C3S2.SheafOnLocaleDeclaration) →
  (prs : C3S2.SheafOfOModules.sheafOfRings d ≡ rs) →
  (pms : C3S2.SheafOfOModules.underlyingSheaf d ≡ ms) →
  SheafOfOModulesAdapter
mkSheafOfOModulesAdapter d rs ms prs pms =
  record { decl = d ; expRingSheaf = rs ; expModSheaf = ms
         ; linkRingSheaf = prs ; linkModSheaf = pms ; status = true }

isFilledSheafOfOModules : SheafOfOModulesAdapter → Core.Phase.Bool
isFilledSheafOfOModules a = SheafOfOModulesAdapter.status a

-- Category of O-modules is abelian
record CategoryOfOModulesIsAbelianCorollaryAdapter : Set₁ where
  field
    decl : C3S2.CategoryOfOModulesIsAbelianCorollary
    expRingSheaf : C3S2.SheafOfRings
    expCategory : C1S3.CategoryDeclaration
    linkRingSheaf : C3S2.CategoryOfOModulesIsAbelianCorollary.sheafOfRings decl ≡ expRingSheaf
    linkCategory : C3S2.CategoryOfOModulesIsAbelianCorollary.categoryOfOModules decl ≡ expCategory
    status : Core.Phase.Bool

mkCategoryOfOModulesIsAbelianCorollaryAdapter :
  (d : C3S2.CategoryOfOModulesIsAbelianCorollary) →
  (rs : C3S2.SheafOfRings) →
  (cat : C1S3.CategoryDeclaration) →
  (prs : C3S2.CategoryOfOModulesIsAbelianCorollary.sheafOfRings d ≡ rs) →
  (pcat : C3S2.CategoryOfOModulesIsAbelianCorollary.categoryOfOModules d ≡ cat) →
  CategoryOfOModulesIsAbelianCorollaryAdapter
mkCategoryOfOModulesIsAbelianCorollaryAdapter d rs cat prs pcat =
  record { decl = d ; expRingSheaf = rs ; expCategory = cat
         ; linkRingSheaf = prs ; linkCategory = pcat ; status = true }

isFilledCategoryOfOModulesIsAbelianCorollary : CategoryOfOModulesIsAbelianCorollaryAdapter → Core.Phase.Bool
isFilledCategoryOfOModulesIsAbelianCorollary a = CategoryOfOModulesIsAbelianCorollaryAdapter.status a

------------------------------------------------------------------------
-- Algebra Adapters
------------------------------------------------------------------------

-- Magma
record MagmaAdapter : Set₁ where
  field
    decl : AFo.MagmaDeclaration
    status : Core.Phase.Bool

mkMagmaAdapter : AFo.MagmaDeclaration → MagmaAdapter
mkMagmaAdapter d = record { decl = d ; status = true }

isFilledMagma : MagmaAdapter → Core.Phase.Bool
isFilledMagma a = MagmaAdapter.status a

-- Categorical view for Magma
magmaCategorical : MagmaAdapter → CategoricalAdapter {lsuc lzero} AFo.MagmaDeclaration
magmaCategorical adapt = mkCategoricalAdapter AFo.MagmaDeclaration (λ _ → MagmaAdapter.decl adapt)

-- Semigroup
record SemigroupAdapter : Set₁ where
  field
    decl : AFo.SemigroupDeclaration
    expMagma : AFo.MagmaDeclaration
    linkMagma : AFo.SemigroupDeclaration.underlyingMagma decl ≡ expMagma
    status : Core.Phase.Bool

mkSemigroupAdapter :
  (d : AFo.SemigroupDeclaration) →
  (m : AFo.MagmaDeclaration) →
  (pm : AFo.SemigroupDeclaration.underlyingMagma d ≡ m) →
  SemigroupAdapter
mkSemigroupAdapter d m pm =
  record { decl = d ; expMagma = m ; linkMagma = pm ; status = true }

isFilledSemigroup : SemigroupAdapter → Core.Phase.Bool
isFilledSemigroup a = SemigroupAdapter.status a

-- Categorical view for Semigroup
semigroupCategorical : SemigroupAdapter → CategoricalAdapter {lsuc lzero} AFo.SemigroupDeclaration
semigroupCategorical adapt = mkCategoricalAdapter AFo.SemigroupDeclaration (λ _ → SemigroupAdapter.decl adapt)

-- Monoid
record MonoidAdapter : Set₁ where
  field
    decl : AFo.MonoidDeclaration
    expSemigroup : AFo.SemigroupDeclaration
    linkSemigroup : AFo.MonoidDeclaration.underlyingSemigroup decl ≡ expSemigroup
    status : Core.Phase.Bool

mkMonoidAdapter :
  (d : AFo.MonoidDeclaration) →
  (s : AFo.SemigroupDeclaration) →
  (ps : AFo.MonoidDeclaration.underlyingSemigroup d ≡ s) →
  MonoidAdapter
mkMonoidAdapter d s ps =
  record { decl = d ; expSemigroup = s ; linkSemigroup = ps ; status = true }

isFilledMonoid : MonoidAdapter → Core.Phase.Bool
isFilledMonoid a = MonoidAdapter.status a

-- Categorical view for Monoid
monoidCategorical : MonoidAdapter → CategoricalAdapter {lsuc lzero} AFo.MonoidDeclaration
monoidCategorical adapt = mkCategoricalAdapter AFo.MonoidDeclaration (λ _ → MonoidAdapter.decl adapt)

-- Group
record GroupAdapter : Set₁ where
  field
    decl : AFo.GroupDeclaration
    expMonoid : AFo.MonoidDeclaration
    linkMonoid : AFo.GroupDeclaration.underlyingMonoid decl ≡ expMonoid
    status : Core.Phase.Bool

mkGroupAdapter :
  (d : AFo.GroupDeclaration) →
  (m : AFo.MonoidDeclaration) →
  (pm : AFo.GroupDeclaration.underlyingMonoid d ≡ m) →
  GroupAdapter
mkGroupAdapter d m pm =
  record { decl = d ; expMonoid = m ; linkMonoid = pm ; status = true }

isFilledGroup : GroupAdapter → Core.Phase.Bool
isFilledGroup a = GroupAdapter.status a

-- Categorical view for Group
groupCategorical : GroupAdapter → CategoricalAdapter {lsuc lzero} AFo.GroupDeclaration
groupCategorical adapt = mkCategoricalAdapter AFo.GroupDeclaration (λ _ → GroupAdapter.decl adapt)

-- AbelianGroup
record AbelianGroupAdapter : Set₁ where
  field
    decl : AFo.AbelianGroupDeclaration
    expGroup : AFo.GroupDeclaration
    linkGroup : AFo.AbelianGroupDeclaration.underlyingGroup decl ≡ expGroup
    status : Core.Phase.Bool

mkAbelianGroupAdapter :
  (d : AFo.AbelianGroupDeclaration) →
  (g : AFo.GroupDeclaration) →
  (pg : AFo.AbelianGroupDeclaration.underlyingGroup d ≡ g) →
  AbelianGroupAdapter
mkAbelianGroupAdapter d g pg =
  record { decl = d ; expGroup = g ; linkGroup = pg ; status = true }

isFilledAbelianGroup : AbelianGroupAdapter → Core.Phase.Bool
isFilledAbelianGroup a = AbelianGroupAdapter.status a

-- Categorical view for Abelian Group
abelianGroupCategorical : AbelianGroupAdapter → CategoricalAdapter {lsuc lzero} AFo.AbelianGroupDeclaration
abelianGroupCategorical adapt = mkCategoricalAdapter AFo.AbelianGroupDeclaration (λ _ → AbelianGroupAdapter.decl adapt)

-- Ring
record RingAdapter : Set₁ where
  field
    decl : AR.RingDeclaration
    expAdditiveGroup : AFo.AbelianGroupDeclaration
    linkAdditiveGroup : AR.RingDeclaration.additiveGroup decl ≡ expAdditiveGroup
    status : Core.Phase.Bool

mkRingAdapter :
  (d : AR.RingDeclaration) →
  (ag : AFo.AbelianGroupDeclaration) →
  (pag : AR.RingDeclaration.additiveGroup d ≡ ag) →
  RingAdapter
mkRingAdapter d ag pag =
  record { decl = d ; expAdditiveGroup = ag ; linkAdditiveGroup = pag ; status = true }

isFilledRing : RingAdapter → Core.Phase.Bool
isFilledRing a = RingAdapter.status a

-- Categorical view for Ring
ringCategorical : RingAdapter → CategoricalAdapter {lsuc lzero} AR.RingDeclaration
ringCategorical adapt = mkCategoricalAdapter AR.RingDeclaration (λ _ → RingAdapter.decl adapt)

-- UnitalRing
record UnitalRingAdapter : Set₁ where
  field
    decl : AR.UnitalRingDeclaration
    expRing : AR.RingDeclaration
    linkRing : AR.UnitalRingDeclaration.underlyingRing decl ≡ expRing
    status : Core.Phase.Bool

mkUnitalRingAdapter :
  (d : AR.UnitalRingDeclaration) →
  (r : AR.RingDeclaration) →
  (pr : AR.UnitalRingDeclaration.underlyingRing d ≡ r) →
  UnitalRingAdapter
mkUnitalRingAdapter d r pr =
  record { decl = d ; expRing = r ; linkRing = pr ; status = true }

isFilledUnitalRing : UnitalRingAdapter → Core.Phase.Bool
isFilledUnitalRing a = UnitalRingAdapter.status a

-- Categorical view for Unital Ring
unitalRingCategorical : UnitalRingAdapter → CategoricalAdapter {lsuc lzero} AR.UnitalRingDeclaration
unitalRingCategorical adapt = mkCategoricalAdapter AR.UnitalRingDeclaration (λ _ → UnitalRingAdapter.decl adapt)

-- CommutativeRing
record CommutativeRingAdapter : Set₁ where
  field
    decl : AR.CommutativeRingDeclaration
    expUnitalRing : AR.UnitalRingDeclaration
    linkUnitalRing : AR.CommutativeRingDeclaration.underlyingRing decl ≡ expUnitalRing
    status : Core.Phase.Bool

mkCommutativeRingAdapter :
  (d : AR.CommutativeRingDeclaration) →
  (ur : AR.UnitalRingDeclaration) →
  (pur : AR.CommutativeRingDeclaration.underlyingRing d ≡ ur) →
  CommutativeRingAdapter
mkCommutativeRingAdapter d ur pur =
  record { decl = d ; expUnitalRing = ur ; linkUnitalRing = pur ; status = true }

isFilledCommutativeRing : CommutativeRingAdapter → Core.Phase.Bool
isFilledCommutativeRing a = CommutativeRingAdapter.status a

-- Categorical view for Commutative Ring
commutativeRingCategorical : CommutativeRingAdapter → CategoricalAdapter {lsuc lzero} AR.CommutativeRingDeclaration
commutativeRingCategorical adapt = mkCategoricalAdapter AR.CommutativeRingDeclaration (λ _ → CommutativeRingAdapter.decl adapt)

-- DivisionRing
record DivisionRingAdapter : Set₁ where
  field
    decl : AR.DivisionRingDeclaration
    expUnitalRing : AR.UnitalRingDeclaration
    linkUnitalRing : AR.DivisionRingDeclaration.underlyingRing decl ≡ expUnitalRing
    status : Core.Phase.Bool

mkDivisionRingAdapter :
  (d : AR.DivisionRingDeclaration) →
  (ur : AR.UnitalRingDeclaration) →
  (pur : AR.DivisionRingDeclaration.underlyingRing d ≡ ur) →
  DivisionRingAdapter
mkDivisionRingAdapter d ur pur =
  record { decl = d ; expUnitalRing = ur ; linkUnitalRing = pur ; status = true }

isFilledDivisionRing : DivisionRingAdapter → Core.Phase.Bool
isFilledDivisionRing a = DivisionRingAdapter.status a

-- Categorical view for Division Ring
divisionRingCategorical : DivisionRingAdapter → CategoricalAdapter {lsuc lzero} AR.DivisionRingDeclaration
divisionRingCategorical adapt = mkCategoricalAdapter AR.DivisionRingDeclaration (λ _ → DivisionRingAdapter.decl adapt)

-- Field
record FieldAdapter : Set₁ where
  field
    decl : AR.FieldDeclaration
    expCommutativeRing : AR.CommutativeRingDeclaration
    linkCommutativeRing : AR.FieldDeclaration.underlyingRing decl ≡ expCommutativeRing
    status : Core.Phase.Bool

mkFieldAdapter :
  (d : AR.FieldDeclaration) →
  (cr : AR.CommutativeRingDeclaration) →
  (pcr : AR.FieldDeclaration.underlyingRing d ≡ cr) →
  FieldAdapter
mkFieldAdapter d cr pcr =
  record { decl = d ; expCommutativeRing = cr ; linkCommutativeRing = pcr ; status = true }

isFilledField : FieldAdapter → Core.Phase.Bool
isFilledField a = FieldAdapter.status a

-- Categorical view for Field
fieldCategorical : FieldAdapter → CategoricalAdapter {lsuc lzero} AR.FieldDeclaration
fieldCategorical adapt = mkCategoricalAdapter AR.FieldDeclaration (λ _ → FieldAdapter.decl adapt)

-- ==========================================================
-- Core.UniversalProperties: adapters for general UMPs
-- ==========================================================

-- Initial object
record InitialObjectAdapter : Set₁ where
  field
    decl : CUP.InitialObject
    expInitial : M.Identifier
    linkInitial : CUP.InitialObject.initial decl ≡ expInitial
    status : Core.Phase.Bool

mkInitialObjectAdapter :
  (d : CUP.InitialObject) →
  (i : M.Identifier) →
  (pi : CUP.InitialObject.initial d ≡ i) →
  InitialObjectAdapter
mkInitialObjectAdapter d i pi =
  record { decl = d ; expInitial = i ; linkInitial = pi ; status = true }

isFilledInitialObject : InitialObjectAdapter → Core.Phase.Bool
isFilledInitialObject a = InitialObjectAdapter.status a

-- Categorical view for InitialObject
initialObjectCategorical : InitialObjectAdapter →
  CategoricalAdapter {lsuc lzero} CUP.InitialObject
initialObjectCategorical adapt =
  mkCategoricalAdapter CUP.InitialObject (λ _ → InitialObjectAdapter.decl adapt)

-- Terminal object
record TerminalObjectAdapter : Set₁ where
  field
    decl : CUP.TerminalObject
    expTerminal : M.Identifier
    linkTerminal : CUP.TerminalObject.terminal decl ≡ expTerminal
    status : Core.Phase.Bool

mkTerminalObjectAdapter :
  (d : CUP.TerminalObject) →
  (t : M.Identifier) →
  (pt : CUP.TerminalObject.terminal d ≡ t) →
  TerminalObjectAdapter
mkTerminalObjectAdapter d t pt =
  record { decl = d ; expTerminal = t ; linkTerminal = pt ; status = true }

isFilledTerminalObject : TerminalObjectAdapter → Core.Phase.Bool
isFilledTerminalObject a = TerminalObjectAdapter.status a

-- Categorical view for TerminalObject
terminalObjectCategorical : TerminalObjectAdapter →
  CategoricalAdapter {lsuc lzero} CUP.TerminalObject
terminalObjectCategorical adapt =
  mkCategoricalAdapter CUP.TerminalObject (λ _ → TerminalObjectAdapter.decl adapt)

-- Product property
record ProductPropertyAdapter : Set₁ where
  field
    A B : M.Identifier
    decl : CUP.ProductProperty A B
    expProduct : M.Identifier
    linkProduct : CUP.ProductProperty.product decl ≡ expProduct
    status : Core.Phase.Bool

mkProductPropertyAdapter :
  (A B : M.Identifier) →
  (d : CUP.ProductProperty A B) →
  (p : M.Identifier) →
  (pp : CUP.ProductProperty.product d ≡ p) →
  ProductPropertyAdapter
mkProductPropertyAdapter A B d p pp =
  record { A = A ; B = B ; decl = d ; expProduct = p ; linkProduct = pp ; status = true }

isFilledProductProperty : ProductPropertyAdapter → Core.Phase.Bool
isFilledProductProperty a = ProductPropertyAdapter.status a

-- Categorical view for ProductProperty
productPropertyCategorical : (adapt : ProductPropertyAdapter) →
  CategoricalAdapter {lsuc lzero}
    (CUP.ProductProperty (ProductPropertyAdapter.A adapt) (ProductPropertyAdapter.B adapt))
productPropertyCategorical adapt =
  mkCategoricalAdapter (CUP.ProductProperty (ProductPropertyAdapter.A adapt) (ProductPropertyAdapter.B adapt))
    (λ _ → ProductPropertyAdapter.decl adapt)

-- Coproduct property
record CoproductPropertyAdapter : Set₁ where
  field
    A B : M.Identifier
    decl : CUP.CoproductProperty A B
    expCoproduct : M.Identifier
    linkCoproduct : CUP.CoproductProperty.coproduct decl ≡ expCoproduct
    status : Core.Phase.Bool

mkCoproductPropertyAdapter :
  (A B : M.Identifier) →
  (d : CUP.CoproductProperty A B) →
  (c : M.Identifier) →
  (pc : CUP.CoproductProperty.coproduct d ≡ c) →
  CoproductPropertyAdapter
mkCoproductPropertyAdapter A B d c pc =
  record { A = A ; B = B ; decl = d ; expCoproduct = c ; linkCoproduct = pc ; status = true }

isFilledCoproductProperty : CoproductPropertyAdapter → Core.Phase.Bool
isFilledCoproductProperty a = CoproductPropertyAdapter.status a

-- Categorical view for CoproductProperty
coproductPropertyCategorical : (adapt : CoproductPropertyAdapter) →
  CategoricalAdapter {lsuc lzero}
    (CUP.CoproductProperty (CoproductPropertyAdapter.A adapt) (CoproductPropertyAdapter.B adapt))
coproductPropertyCategorical adapt =
  mkCategoricalAdapter (CUP.CoproductProperty (CoproductPropertyAdapter.A adapt) (CoproductPropertyAdapter.B adapt))
    (λ _ → CoproductPropertyAdapter.decl adapt)

-- Equalizer property
record EqualizerPropertyAdapter : Set₁ where
  field
    A B f g : M.Identifier
    decl : CUP.EqualizerProperty A B f g
    expEqualizer : M.Identifier
    linkEqualizer : CUP.EqualizerProperty.equalizer decl ≡ expEqualizer
    status : Core.Phase.Bool

mkEqualizerPropertyAdapter :
  (A B f g : M.Identifier) →
  (d : CUP.EqualizerProperty A B f g) →
  (e : M.Identifier) →
  (pe : CUP.EqualizerProperty.equalizer d ≡ e) →
  EqualizerPropertyAdapter
mkEqualizerPropertyAdapter A B f g d e pe =
  record { A = A ; B = B ; f = f ; g = g ; decl = d ; expEqualizer = e ; linkEqualizer = pe ; status = true }

isFilledEqualizerProperty : EqualizerPropertyAdapter → Core.Phase.Bool
isFilledEqualizerProperty a = EqualizerPropertyAdapter.status a

-- Categorical view for EqualizerProperty
equalizerPropertyCategorical : (adapt : EqualizerPropertyAdapter) →
  CategoricalAdapter {lsuc lzero}
    (CUP.EqualizerProperty (EqualizerPropertyAdapter.A adapt)
                           (EqualizerPropertyAdapter.B adapt)
                           (EqualizerPropertyAdapter.f adapt)
                           (EqualizerPropertyAdapter.g adapt))
equalizerPropertyCategorical adapt =
  mkCategoricalAdapter (CUP.EqualizerProperty (EqualizerPropertyAdapter.A adapt)
                                              (EqualizerPropertyAdapter.B adapt)
                                              (EqualizerPropertyAdapter.f adapt)
                                              (EqualizerPropertyAdapter.g adapt))
    (λ _ → EqualizerPropertyAdapter.decl adapt)

-- Coequalizer property
record CoequalizerPropertyAdapter : Set₁ where
  field
    A B f g : M.Identifier
    decl : CUP.CoequalizerProperty A B f g
    expCoequalizer : M.Identifier
    linkCoequalizer : CUP.CoequalizerProperty.coequalizer decl ≡ expCoequalizer
    status : Core.Phase.Bool

mkCoequalizerPropertyAdapter :
  (A B f g : M.Identifier) →
  (d : CUP.CoequalizerProperty A B f g) →
  (e : M.Identifier) →
  (pe : CUP.CoequalizerProperty.coequalizer d ≡ e) →
  CoequalizerPropertyAdapter
mkCoequalizerPropertyAdapter A B f g d e pe =
  record { A = A ; B = B ; f = f ; g = g ; decl = d ; expCoequalizer = e ; linkCoequalizer = pe ; status = true }

isFilledCoequalizerProperty : CoequalizerPropertyAdapter → Core.Phase.Bool
isFilledCoequalizerProperty a = CoequalizerPropertyAdapter.status a

-- Categorical view for CoequalizerProperty
coequalizerPropertyCategorical : (adapt : CoequalizerPropertyAdapter) →
  CategoricalAdapter {lsuc lzero}
    (CUP.CoequalizerProperty (CoequalizerPropertyAdapter.A adapt)
                             (CoequalizerPropertyAdapter.B adapt)
                             (CoequalizerPropertyAdapter.f adapt)
                             (CoequalizerPropertyAdapter.g adapt))
coequalizerPropertyCategorical adapt =
  mkCategoricalAdapter (CUP.CoequalizerProperty (CoequalizerPropertyAdapter.A adapt)
                                                (CoequalizerPropertyAdapter.B adapt)
                                                (CoequalizerPropertyAdapter.f adapt)
                                                (CoequalizerPropertyAdapter.g adapt))
    (λ _ → CoequalizerPropertyAdapter.decl adapt)

-- Pullback property
record PullbackPropertyAdapter : Set₁ where
  field
    A B C f g : M.Identifier
    decl : CUP.PullbackProperty A B C f g
    expPullback : M.Identifier
    linkPullback : CUP.PullbackProperty.pullback decl ≡ expPullback
    status : Core.Phase.Bool

mkPullbackPropertyAdapter :
  (A B C f g : M.Identifier) →
  (d : CUP.PullbackProperty A B C f g) →
  (p : M.Identifier) →
  (pp : CUP.PullbackProperty.pullback d ≡ p) →
  PullbackPropertyAdapter
mkPullbackPropertyAdapter A B C f g d p pp =
  record { A = A ; B = B ; C = C ; f = f ; g = g ; decl = d ; expPullback = p ; linkPullback = pp ; status = true }

isFilledPullbackProperty : PullbackPropertyAdapter → Core.Phase.Bool
isFilledPullbackProperty a = PullbackPropertyAdapter.status a

-- Categorical view for PullbackProperty
pullbackPropertyCategorical : (adapt : PullbackPropertyAdapter) →
  CategoricalAdapter {lsuc lzero}
    (CUP.PullbackProperty (PullbackPropertyAdapter.A adapt)
                          (PullbackPropertyAdapter.B adapt)
                          (PullbackPropertyAdapter.C adapt)
                          (PullbackPropertyAdapter.f adapt)
                          (PullbackPropertyAdapter.g adapt))
pullbackPropertyCategorical adapt =
  mkCategoricalAdapter (CUP.PullbackProperty (PullbackPropertyAdapter.A adapt)
                                             (PullbackPropertyAdapter.B adapt)
                                             (PullbackPropertyAdapter.C adapt)
                                             (PullbackPropertyAdapter.f adapt)
                                             (PullbackPropertyAdapter.g adapt))
    (λ _ → PullbackPropertyAdapter.decl adapt)

-- Pushout property
record PushoutPropertyAdapter : Set₁ where
  field
    A B C f g : M.Identifier
    decl : CUP.PushoutProperty A B C f g
    expPushout : M.Identifier
    linkPushout : CUP.PushoutProperty.pushout decl ≡ expPushout
    status : Core.Phase.Bool

mkPushoutPropertyAdapter :
  (A B C f g : M.Identifier) →
  (d : CUP.PushoutProperty A B C f g) →
  (p : M.Identifier) →
  (pp : CUP.PushoutProperty.pushout d ≡ p) →
  PushoutPropertyAdapter
mkPushoutPropertyAdapter A B C f g d p pp =
  record { A = A ; B = B ; C = C ; f = f ; g = g ; decl = d ; expPushout = p ; linkPushout = pp ; status = true }

isFilledPushoutProperty : PushoutPropertyAdapter → Core.Phase.Bool
isFilledPushoutProperty a = PushoutPropertyAdapter.status a

-- Categorical view for PushoutProperty
pushoutPropertyCategorical : (adapt : PushoutPropertyAdapter) →
  CategoricalAdapter {lsuc lzero}
    (CUP.PushoutProperty (PushoutPropertyAdapter.A adapt)
                         (PushoutPropertyAdapter.B adapt)
                         (PushoutPropertyAdapter.C adapt)
                         (PushoutPropertyAdapter.f adapt)
                         (PushoutPropertyAdapter.g adapt))
pushoutPropertyCategorical adapt =
  mkCategoricalAdapter (CUP.PushoutProperty (PushoutPropertyAdapter.A adapt)
                                            (PushoutPropertyAdapter.B adapt)
                                            (PushoutPropertyAdapter.C adapt)
                                            (PushoutPropertyAdapter.f adapt)
                                            (PushoutPropertyAdapter.g adapt))
    (λ _ → PushoutPropertyAdapter.decl adapt)

-- Limit property
record LimitPropertyAdapter : Set₁ where
  field
    D : M.Identifier
    decl : CUP.LimitProperty D
    expLimit : M.Identifier
    linkLimit : CUP.LimitProperty.limit decl ≡ expLimit
    status : Core.Phase.Bool

mkLimitPropertyAdapter :
  (D : M.Identifier) →
  (d : CUP.LimitProperty D) →
  (l : M.Identifier) →
  (pl : CUP.LimitProperty.limit d ≡ l) →
  LimitPropertyAdapter
mkLimitPropertyAdapter D d l pl =
  record { D = D ; decl = d ; expLimit = l ; linkLimit = pl ; status = true }

isFilledLimitProperty : LimitPropertyAdapter → Core.Phase.Bool
isFilledLimitProperty a = LimitPropertyAdapter.status a

-- Categorical view for LimitProperty
limitPropertyCategorical : (adapt : LimitPropertyAdapter) →
  CategoricalAdapter {lsuc lzero} (CUP.LimitProperty (LimitPropertyAdapter.D adapt))
limitPropertyCategorical adapt =
  mkCategoricalAdapter (CUP.LimitProperty (LimitPropertyAdapter.D adapt))
    (λ _ → LimitPropertyAdapter.decl adapt)

-- Colimit property
record ColimitPropertyAdapter : Set₁ where
  field
    D : M.Identifier
    decl : CUP.ColimitProperty D
    expColimit : M.Identifier
    linkColimit : CUP.ColimitProperty.colimit decl ≡ expColimit
    status : Core.Phase.Bool

mkColimitPropertyAdapter :
  (D : M.Identifier) →
  (d : CUP.ColimitProperty D) →
  (c : M.Identifier) →
  (pc : CUP.ColimitProperty.colimit d ≡ c) →
  ColimitPropertyAdapter
mkColimitPropertyAdapter D d c pc =
  record { D = D ; decl = d ; expColimit = c ; linkColimit = pc ; status = true }

isFilledColimitProperty : ColimitPropertyAdapter → Core.Phase.Bool
isFilledColimitProperty a = ColimitPropertyAdapter.status a

-- Categorical view for ColimitProperty
colimitPropertyCategorical : (adapt : ColimitPropertyAdapter) →
  CategoricalAdapter {lsuc lzero} (CUP.ColimitProperty (ColimitPropertyAdapter.D adapt))
colimitPropertyCategorical adapt =
  mkCategoricalAdapter (CUP.ColimitProperty (ColimitPropertyAdapter.D adapt))
    (λ _ → ColimitPropertyAdapter.decl adapt)

-- ==========================================================
-- Algebra.Modules.Basic: adapters
-- ==========================================================

-- Left module
record LeftModuleAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    decl : AM.LeftModule R
    expRing : AR.RingDeclaration
    linkRing : AM.LeftModule.ring decl ≡ expRing
    status : Core.Phase.Bool

mkLeftModuleAdapter :
  (R : AR.RingDeclaration) →
  (d : AM.LeftModule R) →
  (er : AR.RingDeclaration) →
  (pr : AM.LeftModule.ring d ≡ er) →
  LeftModuleAdapter
mkLeftModuleAdapter R d er pr =
  record { R = R ; decl = d ; expRing = er ; linkRing = pr ; status = true }

isFilledLeftModule : LeftModuleAdapter → Core.Phase.Bool
isFilledLeftModule a = LeftModuleAdapter.status a

-- Categorical view for LeftModule
leftModuleCategorical : (adapt : LeftModuleAdapter) →
  CategoricalAdapter {lsuc lzero} (AM.LeftModule (LeftModuleAdapter.R adapt))
leftModuleCategorical adapt =
  mkCategoricalAdapter (AM.LeftModule (LeftModuleAdapter.R adapt))
    (λ _ → LeftModuleAdapter.decl adapt)

-- Module homomorphism
record ModuleHomomorphismAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    M N : AM.LeftModule R
    decl : AM.ModuleHomomorphism R M N
    expRing : AR.RingDeclaration
    linkRing : AM.ModuleHomomorphism.ring decl ≡ expRing
    status : Core.Phase.Bool

mkModuleHomomorphismAdapter :
  (R : AR.RingDeclaration) →
  (M N : AM.LeftModule R) →
  (d : AM.ModuleHomomorphism R M N) →
  (er : AR.RingDeclaration) →
  (pr : AM.ModuleHomomorphism.ring d ≡ er) →
  ModuleHomomorphismAdapter
mkModuleHomomorphismAdapter R M N d er pr =
  record { R = R ; M = M ; N = N ; decl = d ; expRing = er ; linkRing = pr ; status = true }

isFilledModuleHom : ModuleHomomorphismAdapter → Core.Phase.Bool
isFilledModuleHom a = ModuleHomomorphismAdapter.status a

-- Categorical view for ModuleHomomorphism
moduleHomomorphismCategorical : (adapt : ModuleHomomorphismAdapter) →
  CategoricalAdapter {lsuc lzero}
    (AM.ModuleHomomorphism (ModuleHomomorphismAdapter.R adapt)
                           (ModuleHomomorphismAdapter.M adapt)
                           (ModuleHomomorphismAdapter.N adapt))
moduleHomomorphismCategorical adapt =
  mkCategoricalAdapter (AM.ModuleHomomorphism (ModuleHomomorphismAdapter.R adapt)
                                              (ModuleHomomorphismAdapter.M adapt)
                                              (ModuleHomomorphismAdapter.N adapt))
    (λ _ → ModuleHomomorphismAdapter.decl adapt)

-- Submodule
record SubmoduleAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    M : AM.LeftModule R
    decl : AM.Submodule R M
    expRing : AR.RingDeclaration
    linkRing : AM.Submodule.ring decl ≡ expRing
    status : Core.Phase.Bool

mkSubmoduleAdapter :
  (R : AR.RingDeclaration) →
  (M : AM.LeftModule R) →
  (d : AM.Submodule R M) →
  (er : AR.RingDeclaration) →
  (pr : AM.Submodule.ring d ≡ er) →
  SubmoduleAdapter
mkSubmoduleAdapter R M d er pr =
  record { R = R ; M = M ; decl = d ; expRing = er ; linkRing = pr ; status = true }

isFilledSubmodule : SubmoduleAdapter → Core.Phase.Bool
isFilledSubmodule a = SubmoduleAdapter.status a

-- Categorical view for Submodule
submoduleCategorical : (adapt : SubmoduleAdapter) →
  CategoricalAdapter {lsuc lzero}
    (AM.Submodule (SubmoduleAdapter.R adapt) (SubmoduleAdapter.M adapt))
submoduleCategorical adapt =
  mkCategoricalAdapter (AM.Submodule (SubmoduleAdapter.R adapt) (SubmoduleAdapter.M adapt))
    (λ _ → SubmoduleAdapter.decl adapt)

-- Quotient module
record QuotientModuleAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    M : AM.LeftModule R
    N : AM.Submodule R M
    decl : AM.QuotientModule R M N
    expRing : AR.RingDeclaration
    linkRing : AM.QuotientModule.ring decl ≡ expRing
    status : Core.Phase.Bool

mkQuotientModuleAdapter :
  (R : AR.RingDeclaration) →
  (M : AM.LeftModule R) →
  (N : AM.Submodule R M) →
  (d : AM.QuotientModule R M N) →
  (er : AR.RingDeclaration) →
  (pr : AM.QuotientModule.ring d ≡ er) →
  QuotientModuleAdapter
mkQuotientModuleAdapter R M N d er pr =
  record { R = R ; M = M ; N = N ; decl = d ; expRing = er ; linkRing = pr ; status = true }

isFilledQuotientModule : QuotientModuleAdapter → Core.Phase.Bool
isFilledQuotientModule a = QuotientModuleAdapter.status a

-- Categorical view for QuotientModule
quotientModuleCategorical : (adapt : QuotientModuleAdapter) →
  CategoricalAdapter {lsuc lzero}
    (AM.QuotientModule (QuotientModuleAdapter.R adapt)
                       (QuotientModuleAdapter.M adapt)
                       (QuotientModuleAdapter.N adapt))
quotientModuleCategorical adapt =
  mkCategoricalAdapter (AM.QuotientModule (QuotientModuleAdapter.R adapt)
                                          (QuotientModuleAdapter.M adapt)
                                          (QuotientModuleAdapter.N adapt))
    (λ _ → QuotientModuleAdapter.decl adapt)

-- Kernel of module homomorphism
record KernelOfModuleHomomorphismAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    f : M.Identifier
    decl : AM.KernelOfModuleHomomorphism R f
    expRing : AR.RingDeclaration
    linkRing : AM.KernelOfModuleHomomorphism.ring decl ≡ expRing
    status : Core.Phase.Bool

mkKernelOfModuleHomomorphismAdapter :
  (R : AR.RingDeclaration) →
  (f : M.Identifier) →
  (d : AM.KernelOfModuleHomomorphism R f) →
  (er : AR.RingDeclaration) →
  (pr : AM.KernelOfModuleHomomorphism.ring d ≡ er) →
  KernelOfModuleHomomorphismAdapter
mkKernelOfModuleHomomorphismAdapter R f d er pr =
  record { R = R ; f = f ; decl = d ; expRing = er ; linkRing = pr ; status = true }

isFilledKernelModuleHom : KernelOfModuleHomomorphismAdapter → Core.Phase.Bool
isFilledKernelModuleHom a = KernelOfModuleHomomorphismAdapter.status a

-- Categorical view for KernelOfModuleHomomorphism
kernelOfModuleHomomorphismCategorical : (adapt : KernelOfModuleHomomorphismAdapter) →
  CategoricalAdapter {lsuc lzero}
    (AM.KernelOfModuleHomomorphism (KernelOfModuleHomomorphismAdapter.R adapt)
                                   (KernelOfModuleHomomorphismAdapter.f adapt))
kernelOfModuleHomomorphismCategorical adapt =
  mkCategoricalAdapter (AM.KernelOfModuleHomomorphism (KernelOfModuleHomomorphismAdapter.R adapt)
                                                       (KernelOfModuleHomomorphismAdapter.f adapt))
    (λ _ → KernelOfModuleHomomorphismAdapter.decl adapt)

-- Image of module homomorphism
record ImageOfModuleHomomorphismAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    f : M.Identifier
    decl : AM.ImageOfModuleHomomorphism R f
    expRing : AR.RingDeclaration
    linkRing : AM.ImageOfModuleHomomorphism.ring decl ≡ expRing
    status : Core.Phase.Bool

mkImageOfModuleHomomorphismAdapter :
  (R : AR.RingDeclaration) →
  (f : M.Identifier) →
  (d : AM.ImageOfModuleHomomorphism R f) →
  (er : AR.RingDeclaration) →
  (pr : AM.ImageOfModuleHomomorphism.ring d ≡ er) →
  ImageOfModuleHomomorphismAdapter
mkImageOfModuleHomomorphismAdapter R f d er pr =
  record { R = R ; f = f ; decl = d ; expRing = er ; linkRing = pr ; status = true }

isFilledImageModuleHom : ImageOfModuleHomomorphismAdapter → Core.Phase.Bool
isFilledImageModuleHom a = ImageOfModuleHomomorphismAdapter.status a

-- Categorical view for ImageOfModuleHomomorphism
imageOfModuleHomomorphismCategorical : (adapt : ImageOfModuleHomomorphismAdapter) →
  CategoricalAdapter {lsuc lzero}
    (AM.ImageOfModuleHomomorphism (ImageOfModuleHomomorphismAdapter.R adapt)
                                  (ImageOfModuleHomomorphismAdapter.f adapt))
imageOfModuleHomomorphismCategorical adapt =
  mkCategoricalAdapter (AM.ImageOfModuleHomomorphism (ImageOfModuleHomomorphismAdapter.R adapt)
                                                      (ImageOfModuleHomomorphismAdapter.f adapt))
    (λ _ → ImageOfModuleHomomorphismAdapter.decl adapt)

-- Cokernel of module homomorphism
record CokernelOfModuleHomomorphismAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    f : M.Identifier
    decl : AM.CokernelOfModuleHomomorphism R f
    expRing : AR.RingDeclaration
    linkRing : AM.CokernelOfModuleHomomorphism.ring decl ≡ expRing
    status : Core.Phase.Bool

mkCokernelOfModuleHomomorphismAdapter :
  (R : AR.RingDeclaration) →
  (f : M.Identifier) →
  (d : AM.CokernelOfModuleHomomorphism R f) →
  (er : AR.RingDeclaration) →
  (pr : AM.CokernelOfModuleHomomorphism.ring d ≡ er) →
  CokernelOfModuleHomomorphismAdapter
mkCokernelOfModuleHomomorphismAdapter R f d er pr =
  record { R = R ; f = f ; decl = d ; expRing = er ; linkRing = pr ; status = true }

isFilledCokernelModuleHom : CokernelOfModuleHomomorphismAdapter → Core.Phase.Bool
isFilledCokernelModuleHom a = CokernelOfModuleHomomorphismAdapter.status a

-- Categorical view for CokernelOfModuleHomomorphism
cokernelOfModuleHomomorphismCategorical : (adapt : CokernelOfModuleHomomorphismAdapter) →
  CategoricalAdapter {lsuc lzero}
    (AM.CokernelOfModuleHomomorphism (CokernelOfModuleHomomorphismAdapter.R adapt)
                                     (CokernelOfModuleHomomorphismAdapter.f adapt))
cokernelOfModuleHomomorphismCategorical adapt =
  mkCategoricalAdapter (AM.CokernelOfModuleHomomorphism (CokernelOfModuleHomomorphismAdapter.R adapt)
                                                         (CokernelOfModuleHomomorphismAdapter.f adapt))
    (λ _ → CokernelOfModuleHomomorphismAdapter.decl adapt)

-- Exact sequence (modules)
record ModuleExactSequenceAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    decl : AM.ExactSequence R
    expRing : AR.RingDeclaration
    linkRing : AM.ExactSequence.ring decl ≡ expRing
    status : Core.Phase.Bool

mkModuleExactSequenceAdapter :
  (R : AR.RingDeclaration) →
  (d : AM.ExactSequence R) →
  (er : AR.RingDeclaration) →
  (pr : AM.ExactSequence.ring d ≡ er) →
  ModuleExactSequenceAdapter
mkModuleExactSequenceAdapter R d er pr =
  record { R = R ; decl = d ; expRing = er ; linkRing = pr ; status = true }

isFilledModuleExactSequence : ModuleExactSequenceAdapter → Core.Phase.Bool
isFilledModuleExactSequence a = ModuleExactSequenceAdapter.status a

-- Categorical view for Module ExactSequence
moduleExactSequenceCategorical : (adapt : ModuleExactSequenceAdapter) →
  CategoricalAdapter {lsuc lzero} (AM.ExactSequence (ModuleExactSequenceAdapter.R adapt))
moduleExactSequenceCategorical adapt =
  mkCategoricalAdapter (AM.ExactSequence (ModuleExactSequenceAdapter.R adapt))
    (λ _ → ModuleExactSequenceAdapter.decl adapt)

-- Short exact sequence (modules)
record ModuleShortExactSequenceAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    decl : AM.ShortExactSequence R
    expRing : AR.RingDeclaration
    linkRing : AM.ShortExactSequence.ring decl ≡ expRing
    status : Core.Phase.Bool

mkModuleShortExactSequenceAdapter :
  (R : AR.RingDeclaration) →
  (d : AM.ShortExactSequence R) →
  (er : AR.RingDeclaration) →
  (pr : AM.ShortExactSequence.ring d ≡ er) →
  ModuleShortExactSequenceAdapter
mkModuleShortExactSequenceAdapter R d er pr =
  record { R = R ; decl = d ; expRing = er ; linkRing = pr ; status = true }

isFilledModuleShortExactSequence : ModuleShortExactSequenceAdapter → Core.Phase.Bool
isFilledModuleShortExactSequence a = ModuleShortExactSequenceAdapter.status a

-- Categorical view for Module ShortExactSequence
moduleShortExactSequenceCategorical : (adapt : ModuleShortExactSequenceAdapter) →
  CategoricalAdapter {lsuc lzero} (AM.ShortExactSequence (ModuleShortExactSequenceAdapter.R adapt))
moduleShortExactSequenceCategorical adapt =
  mkCategoricalAdapter (AM.ShortExactSequence (ModuleShortExactSequenceAdapter.R adapt))
    (λ _ → ModuleShortExactSequenceAdapter.decl adapt)

-- ==========================================================
-- Algebra.Fields.Advanced: adapters
-- ==========================================================

-- Inseparable extension
record InseparableExtensionAdapter : Set₁ where
  field
    F E : AR.FieldDeclaration
    decl : AFA.InseparableExtension F E
    expBase : AR.FieldDeclaration
    linkBase : AFA.InseparableExtension.baseField decl ≡ expBase
    status : Core.Phase.Bool

mkInseparableExtensionAdapter :
  (F E : AR.FieldDeclaration) →
  (d : AFA.InseparableExtension F E) →
  (eb : AR.FieldDeclaration) →
  (pb : AFA.InseparableExtension.baseField d ≡ eb) →
  InseparableExtensionAdapter
mkInseparableExtensionAdapter F E d eb pb =
  record { F = F ; E = E ; decl = d ; expBase = eb ; linkBase = pb ; status = true }

isFilledInseparableExtension : InseparableExtensionAdapter → Core.Phase.Bool
isFilledInseparableExtension a = InseparableExtensionAdapter.status a

-- Categorical view for InseparableExtension
inseparableExtensionCategorical : (adapt : InseparableExtensionAdapter) →
  CategoricalAdapter {lsuc lzero}
    (AFA.InseparableExtension (InseparableExtensionAdapter.F adapt)
                              (InseparableExtensionAdapter.E adapt))
inseparableExtensionCategorical adapt =
  mkCategoricalAdapter (AFA.InseparableExtension (InseparableExtensionAdapter.F adapt)
                                                 (InseparableExtensionAdapter.E adapt))
    (λ _ → InseparableExtensionAdapter.decl adapt)

-- Purely inseparable extension
record PurelyInseparableExtensionAdapter : Set₁ where
  field
    F E : AR.FieldDeclaration
    decl : AFA.PurelyInseparableExtension F E
    expBase : AR.FieldDeclaration
    linkBase : AFA.PurelyInseparableExtension.baseField decl ≡ expBase
    status : Core.Phase.Bool

mkPurelyInseparableExtensionAdapter :
  (F E : AR.FieldDeclaration) →
  (d : AFA.PurelyInseparableExtension F E) →
  (eb : AR.FieldDeclaration) →
  (pb : AFA.PurelyInseparableExtension.baseField d ≡ eb) →
  PurelyInseparableExtensionAdapter
mkPurelyInseparableExtensionAdapter F E d eb pb =
  record { F = F ; E = E ; decl = d ; expBase = eb ; linkBase = pb ; status = true }

isFilledPurelyInseparableExtension : PurelyInseparableExtensionAdapter → Core.Phase.Bool
isFilledPurelyInseparableExtension a = PurelyInseparableExtensionAdapter.status a

-- Categorical view for PurelyInseparableExtension
purelyInseparableExtensionCategorical : (adapt : PurelyInseparableExtensionAdapter) →
  CategoricalAdapter {lsuc lzero}
    (AFA.PurelyInseparableExtension (PurelyInseparableExtensionAdapter.F adapt)
                                    (PurelyInseparableExtensionAdapter.E adapt))
purelyInseparableExtensionCategorical adapt =
  mkCategoricalAdapter (AFA.PurelyInseparableExtension (PurelyInseparableExtensionAdapter.F adapt)
                                                        (PurelyInseparableExtensionAdapter.E adapt))
    (λ _ → PurelyInseparableExtensionAdapter.decl adapt)

-- Perfect field
record PerfectFieldAdapter : Set₁ where
  field
    F : AR.FieldDeclaration
    decl : AFA.PerfectField F
    expBase : AR.FieldDeclaration
    linkBase : AFA.PerfectField.baseField decl ≡ expBase
    status : Core.Phase.Bool

mkPerfectFieldAdapter :
  (F : AR.FieldDeclaration) →
  (d : AFA.PerfectField F) →
  (eb : AR.FieldDeclaration) →
  (pb : AFA.PerfectField.baseField d ≡ eb) →
  PerfectFieldAdapter
mkPerfectFieldAdapter F d eb pb =
  record { F = F ; decl = d ; expBase = eb ; linkBase = pb ; status = true }

isFilledPerfectField : PerfectFieldAdapter → Core.Phase.Bool
isFilledPerfectField a = PerfectFieldAdapter.status a

-- Categorical view for PerfectField
perfectFieldCategorical : (adapt : PerfectFieldAdapter) →
  CategoricalAdapter {lsuc lzero} (AFA.PerfectField (PerfectFieldAdapter.F adapt))
perfectFieldCategorical adapt =
  mkCategoricalAdapter (AFA.PerfectField (PerfectFieldAdapter.F adapt))
    (λ _ → PerfectFieldAdapter.decl adapt)

-- Algebraically closed field
record AlgebraicallyClosedFieldAdapter : Set₁ where
  field
    F : AR.FieldDeclaration
    decl : AFA.AlgebraicallyClosedField F
    expBase : AR.FieldDeclaration
    linkBase : AFA.AlgebraicallyClosedField.baseField decl ≡ expBase
    status : Core.Phase.Bool

mkAlgebraicallyClosedFieldAdapter :
  (F : AR.FieldDeclaration) →
  (d : AFA.AlgebraicallyClosedField F) →
  (eb : AR.FieldDeclaration) →
  (pb : AFA.AlgebraicallyClosedField.baseField d ≡ eb) →
  AlgebraicallyClosedFieldAdapter
mkAlgebraicallyClosedFieldAdapter F d eb pb =
  record { F = F ; decl = d ; expBase = eb ; linkBase = pb ; status = true }

isFilledAlgebraicallyClosedField : AlgebraicallyClosedFieldAdapter → Core.Phase.Bool
isFilledAlgebraicallyClosedField a = AlgebraicallyClosedFieldAdapter.status a

-- Categorical view for AlgebraicallyClosedField
algebraicallyClosedFieldCategorical : (adapt : AlgebraicallyClosedFieldAdapter) →
  CategoricalAdapter {lsuc lzero}
    (AFA.AlgebraicallyClosedField (AlgebraicallyClosedFieldAdapter.F adapt))
algebraicallyClosedFieldCategorical adapt =
  mkCategoricalAdapter (AFA.AlgebraicallyClosedField (AlgebraicallyClosedFieldAdapter.F adapt))
    (λ _ → AlgebraicallyClosedFieldAdapter.decl adapt)

-- Normal closure
record NormalClosureAdapter : Set₁ where
  field
    F E : AR.FieldDeclaration
    decl : AFA.NormalClosure F E
    expNormal : AR.FieldDeclaration
    linkNormal : AFA.NormalClosure.normalClosure decl ≡ expNormal
    status : Core.Phase.Bool

mkNormalClosureAdapter :
  (F E : AR.FieldDeclaration) →
  (d : AFA.NormalClosure F E) →
  (en : AR.FieldDeclaration) →
  (pn : AFA.NormalClosure.normalClosure d ≡ en) →
  NormalClosureAdapter
mkNormalClosureAdapter F E d en pn =
  record { F = F ; E = E ; decl = d ; expNormal = en ; linkNormal = pn ; status = true }

isFilledNormalClosure : NormalClosureAdapter → Core.Phase.Bool
isFilledNormalClosure a = NormalClosureAdapter.status a

-- Categorical view for NormalClosure
normalClosureCategorical : (adapt : NormalClosureAdapter) →
  CategoricalAdapter {lsuc lzero}
    (AFA.NormalClosure (NormalClosureAdapter.F adapt) (NormalClosureAdapter.E adapt))
normalClosureCategorical adapt =
  mkCategoricalAdapter (AFA.NormalClosure (NormalClosureAdapter.F adapt) (NormalClosureAdapter.E adapt))
    (λ _ → NormalClosureAdapter.decl adapt)

-- Galois closure
record GaloisClosureAdapter : Set₁ where
  field
    F E : AR.FieldDeclaration
    decl : AFA.GaloisClosure F E
    expGalois : AR.FieldDeclaration
    linkGalois : AFA.GaloisClosure.galoisClosure decl ≡ expGalois
    status : Core.Phase.Bool

mkGaloisClosureAdapter :
  (F E : AR.FieldDeclaration) →
  (d : AFA.GaloisClosure F E) →
  (eg : AR.FieldDeclaration) →
  (pg : AFA.GaloisClosure.galoisClosure d ≡ eg) →
  GaloisClosureAdapter
mkGaloisClosureAdapter F E d eg pg =
  record { F = F ; E = E ; decl = d ; expGalois = eg ; linkGalois = pg ; status = true }

isFilledGaloisClosure : GaloisClosureAdapter → Core.Phase.Bool
isFilledGaloisClosure a = GaloisClosureAdapter.status a

-- Categorical view for GaloisClosure
galoisClosureCategorical : (adapt : GaloisClosureAdapter) →
  CategoricalAdapter {lsuc lzero}
    (AFA.GaloisClosure (GaloisClosureAdapter.F adapt) (GaloisClosureAdapter.E adapt))
galoisClosureCategorical adapt =
  mkCategoricalAdapter (AFA.GaloisClosure (GaloisClosureAdapter.F adapt) (GaloisClosureAdapter.E adapt))
    (λ _ → GaloisClosureAdapter.decl adapt)

-- Frobenius endomorphism
record FrobeniusEndomorphismAdapter : Set₁ where
  field
    F : AR.FieldDeclaration
    decl : AFA.FrobeniusEndomorphism F
    expBase : AR.FieldDeclaration
    linkBase : AFA.FrobeniusEndomorphism.baseField decl ≡ expBase
    status : Core.Phase.Bool

mkFrobeniusEndomorphismAdapter :
  (F : AR.FieldDeclaration) →
  (d : AFA.FrobeniusEndomorphism F) →
  (eb : AR.FieldDeclaration) →
  (pb : AFA.FrobeniusEndomorphism.baseField d ≡ eb) →
  FrobeniusEndomorphismAdapter
mkFrobeniusEndomorphismAdapter F d eb pb =
  record { F = F ; decl = d ; expBase = eb ; linkBase = pb ; status = true }

isFilledFrobeniusEndomorphism : FrobeniusEndomorphismAdapter → Core.Phase.Bool
isFilledFrobeniusEndomorphism a = FrobeniusEndomorphismAdapter.status a

-- Categorical view for FrobeniusEndomorphism
frobeniusEndomorphismCategorical : (adapt : FrobeniusEndomorphismAdapter) →
  CategoricalAdapter {lsuc lzero} (AFA.FrobeniusEndomorphism (FrobeniusEndomorphismAdapter.F adapt))
frobeniusEndomorphismCategorical adapt =
  mkCategoricalAdapter (AFA.FrobeniusEndomorphism (FrobeniusEndomorphismAdapter.F adapt))
    (λ _ → FrobeniusEndomorphismAdapter.decl adapt)

-- Rational function field
record RationalFunctionFieldAdapter : Set₁ where
  field
    K : AR.FieldDeclaration
    decl : AFA.RationalFunctionField K
    expFunctionField : AR.FieldDeclaration
    linkFunctionField : AFA.RationalFunctionField.functionField decl ≡ expFunctionField
    status : Core.Phase.Bool

mkRationalFunctionFieldAdapter :
  (K : AR.FieldDeclaration) →
  (d : AFA.RationalFunctionField K) →
  (eff : AR.FieldDeclaration) →
  (pf : AFA.RationalFunctionField.functionField d ≡ eff) →
  RationalFunctionFieldAdapter
mkRationalFunctionFieldAdapter K d eff pf =
  record { K = K ; decl = d ; expFunctionField = eff ; linkFunctionField = pf ; status = true }

isFilledRationalFunctionField : RationalFunctionFieldAdapter → Core.Phase.Bool
isFilledRationalFunctionField a = RationalFunctionFieldAdapter.status a

-- Categorical view for RationalFunctionField
rationalFunctionFieldCategorical : (adapt : RationalFunctionFieldAdapter) →
  CategoricalAdapter {lsuc lzero} (AFA.RationalFunctionField (RationalFunctionFieldAdapter.K adapt))
rationalFunctionFieldCategorical adapt =
  mkCategoricalAdapter (AFA.RationalFunctionField (RationalFunctionFieldAdapter.K adapt))
    (λ _ → RationalFunctionFieldAdapter.decl adapt)

-- Algebraic function field
record AlgebraicFunctionFieldAdapter : Set₁ where
  field
    K : AR.FieldDeclaration
    decl : AFA.AlgebraicFunctionField K
    expFunctionField : AR.FieldDeclaration
    linkFunctionField : AFA.AlgebraicFunctionField.functionField decl ≡ expFunctionField
    status : Core.Phase.Bool

mkAlgebraicFunctionFieldAdapter :
  (K : AR.FieldDeclaration) →
  (d : AFA.AlgebraicFunctionField K) →
  (eff : AR.FieldDeclaration) →
  (pf : AFA.AlgebraicFunctionField.functionField d ≡ eff) →
  AlgebraicFunctionFieldAdapter
mkAlgebraicFunctionFieldAdapter K d eff pf =
  record { K = K ; decl = d ; expFunctionField = eff ; linkFunctionField = pf ; status = true }

isFilledAlgebraicFunctionField : AlgebraicFunctionFieldAdapter → Core.Phase.Bool
isFilledAlgebraicFunctionField a = AlgebraicFunctionFieldAdapter.status a

-- Categorical view for AlgebraicFunctionField
algebraicFunctionFieldCategorical : (adapt : AlgebraicFunctionFieldAdapter) →
  CategoricalAdapter {lsuc lzero} (AFA.AlgebraicFunctionField (AlgebraicFunctionFieldAdapter.K adapt))
algebraicFunctionFieldCategorical adapt =
  mkCategoricalAdapter (AFA.AlgebraicFunctionField (AlgebraicFunctionFieldAdapter.K adapt))
    (λ _ → AlgebraicFunctionFieldAdapter.decl adapt)

-- ==========================================================
-- Algebra.Fields.Basic - Core Galois Theory
-- ==========================================================

-- Subfield
record SubfieldAdapter : Set₁ where
  field
    F : AR.FieldDeclaration
    decl : AFB.Subfield F
    expSubfield : AR.FieldDeclaration
    linkSubfield : AFB.Subfield.subfield decl ≡ expSubfield
    status : Core.Phase.Bool

mkSubfieldAdapter :
  (F : AR.FieldDeclaration) →
  (d : AFB.Subfield F) →
  (es : AR.FieldDeclaration) →
  (ps : AFB.Subfield.subfield d ≡ es) →
  SubfieldAdapter
mkSubfieldAdapter F d es ps =
  record { F = F ; decl = d ; expSubfield = es ; linkSubfield = ps ; status = true }

isFilledSubfield : SubfieldAdapter → Core.Phase.Bool
isFilledSubfield a = SubfieldAdapter.status a

-- Categorical view for Subfield
subfieldCategorical : (adapt : SubfieldAdapter) →
  CategoricalAdapter {lsuc lzero} (AFB.Subfield (SubfieldAdapter.F adapt))
subfieldCategorical adapt =
  mkCategoricalAdapter (AFB.Subfield (SubfieldAdapter.F adapt))
    (λ _ → SubfieldAdapter.decl adapt)

-- Field extension
record FieldExtensionAdapter : Set₁ where
  field
    F E : AR.FieldDeclaration
    decl : AFB.FieldExtension F E
    expBase : AR.FieldDeclaration
    linkBase : AFB.FieldExtension.baseField decl ≡ expBase
    status : Core.Phase.Bool

mkFieldExtensionAdapter :
  (F E : AR.FieldDeclaration) →
  (d : AFB.FieldExtension F E) →
  (eb : AR.FieldDeclaration) →
  (pb : AFB.FieldExtension.baseField d ≡ eb) →
  FieldExtensionAdapter
mkFieldExtensionAdapter F E d eb pb =
  record { F = F ; E = E ; decl = d ; expBase = eb ; linkBase = pb ; status = true }

isFilledFieldExtension : FieldExtensionAdapter → Core.Phase.Bool
isFilledFieldExtension a = FieldExtensionAdapter.status a

-- Categorical view for FieldExtension
fieldExtensionCategorical : (adapt : FieldExtensionAdapter) →
  CategoricalAdapter {lsuc lzero} (AFB.FieldExtension (FieldExtensionAdapter.F adapt) (FieldExtensionAdapter.E adapt))
fieldExtensionCategorical adapt =
  mkCategoricalAdapter (AFB.FieldExtension (FieldExtensionAdapter.F adapt) (FieldExtensionAdapter.E adapt))
    (λ _ → FieldExtensionAdapter.decl adapt)

-- Algebraic element
record AlgebraicElementAdapter : Set₁ where
  field
    F E : AR.FieldDeclaration
    α : M.Identifier
    decl : AFB.AlgebraicElement F E α
    expBase : AR.FieldDeclaration
    linkBase : AFB.AlgebraicElement.baseField decl ≡ expBase
    status : Core.Phase.Bool

mkAlgebraicElementAdapter :
  (F E : AR.FieldDeclaration) →
  (α : M.Identifier) →
  (d : AFB.AlgebraicElement F E α) →
  (eb : AR.FieldDeclaration) →
  (pb : AFB.AlgebraicElement.baseField d ≡ eb) →
  AlgebraicElementAdapter
mkAlgebraicElementAdapter F E α d eb pb =
  record { F = F ; E = E ; α = α ; decl = d ; expBase = eb ; linkBase = pb ; status = true }

isFilledAlgebraicElement : AlgebraicElementAdapter → Core.Phase.Bool
isFilledAlgebraicElement a = AlgebraicElementAdapter.status a

-- Categorical view for AlgebraicElement
algebraicElementCategorical : (adapt : AlgebraicElementAdapter) →
  CategoricalAdapter {lsuc lzero}
    (AFB.AlgebraicElement (AlgebraicElementAdapter.F adapt)
                          (AlgebraicElementAdapter.E adapt)
                          (AlgebraicElementAdapter.α adapt))
algebraicElementCategorical adapt =
  mkCategoricalAdapter (AFB.AlgebraicElement (AlgebraicElementAdapter.F adapt)
                                            (AlgebraicElementAdapter.E adapt)
                                            (AlgebraicElementAdapter.α adapt))
    (λ _ → AlgebraicElementAdapter.decl adapt)

-- Algebraic extension
record AlgebraicExtensionAdapter : Set₁ where
  field
    F E : AR.FieldDeclaration
    decl : AFB.AlgebraicExtension F E
    expBase : AR.FieldDeclaration
    linkBase : AFB.AlgebraicExtension.baseField decl ≡ expBase
    status : Core.Phase.Bool

mkAlgebraicExtensionAdapter :
  (F E : AR.FieldDeclaration) →
  (d : AFB.AlgebraicExtension F E) →
  (eb : AR.FieldDeclaration) →
  (pb : AFB.AlgebraicExtension.baseField d ≡ eb) →
  AlgebraicExtensionAdapter
mkAlgebraicExtensionAdapter F E d eb pb =
  record { F = F ; E = E ; decl = d ; expBase = eb ; linkBase = pb ; status = true }

isFilledAlgebraicExtension : AlgebraicExtensionAdapter → Core.Phase.Bool
isFilledAlgebraicExtension a = AlgebraicExtensionAdapter.status a

-- Categorical view for AlgebraicExtension
algebraicExtensionCategorical : (adapt : AlgebraicExtensionAdapter) →
  CategoricalAdapter {lsuc lzero}
    (AFB.AlgebraicExtension (AlgebraicExtensionAdapter.F adapt)
                            (AlgebraicExtensionAdapter.E adapt))
algebraicExtensionCategorical adapt =
  mkCategoricalAdapter (AFB.AlgebraicExtension (AlgebraicExtensionAdapter.F adapt)
                                              (AlgebraicExtensionAdapter.E adapt))
    (λ _ → AlgebraicExtensionAdapter.decl adapt)

-- Field automorphism
record FieldAutomorphismAdapter : Set₁ where
  field
    F E : AR.FieldDeclaration
    decl : AFB.FieldAutomorphism F E
    expBase : AR.FieldDeclaration
    linkBase : AFB.FieldAutomorphism.baseField decl ≡ expBase
    status : Core.Phase.Bool

mkFieldAutomorphismAdapter :
  (F E : AR.FieldDeclaration) →
  (d : AFB.FieldAutomorphism F E) →
  (eb : AR.FieldDeclaration) →
  (pb : AFB.FieldAutomorphism.baseField d ≡ eb) →
  FieldAutomorphismAdapter
mkFieldAutomorphismAdapter F E d eb pb =
  record { F = F ; E = E ; decl = d ; expBase = eb ; linkBase = pb ; status = true }

isFilledFieldAutomorphism : FieldAutomorphismAdapter → Core.Phase.Bool
isFilledFieldAutomorphism a = FieldAutomorphismAdapter.status a

-- Categorical view for FieldAutomorphism
fieldAutomorphismCategorical : (adapt : FieldAutomorphismAdapter) →
  CategoricalAdapter {lsuc lzero}
    (AFB.FieldAutomorphism (FieldAutomorphismAdapter.F adapt)
                           (FieldAutomorphismAdapter.E adapt))
fieldAutomorphismCategorical adapt =
  mkCategoricalAdapter (AFB.FieldAutomorphism (FieldAutomorphismAdapter.F adapt)
                                              (FieldAutomorphismAdapter.E adapt))
    (λ _ → FieldAutomorphismAdapter.decl adapt)

-- Galois group
record GaloisGroupAdapter : Set₁ where
  field
    F E : AR.FieldDeclaration
    decl : AFB.GaloisGroup F E
    expBase : AR.FieldDeclaration
    linkBase : AFB.GaloisGroup.baseField decl ≡ expBase
    status : Core.Phase.Bool

mkGaloisGroupAdapter :
  (F E : AR.FieldDeclaration) →
  (d : AFB.GaloisGroup F E) →
  (eb : AR.FieldDeclaration) →
  (pb : AFB.GaloisGroup.baseField d ≡ eb) →
  GaloisGroupAdapter
mkGaloisGroupAdapter F E d eb pb =
  record { F = F ; E = E ; decl = d ; expBase = eb ; linkBase = pb ; status = true }

isFilledGaloisGroup : GaloisGroupAdapter → Core.Phase.Bool
isFilledGaloisGroup a = GaloisGroupAdapter.status a

-- Categorical view for GaloisGroup
galoisGroupCategorical : (adapt : GaloisGroupAdapter) →
  CategoricalAdapter {lsuc lzero}
    (AFB.GaloisGroup (GaloisGroupAdapter.F adapt)
                     (GaloisGroupAdapter.E adapt))
galoisGroupCategorical adapt =
  mkCategoricalAdapter (AFB.GaloisGroup (GaloisGroupAdapter.F adapt)
                                        (GaloisGroupAdapter.E adapt))
    (λ _ → GaloisGroupAdapter.decl adapt)

-- Galois extension
record GaloisExtensionAdapter : Set₁ where
  field
    F E : AR.FieldDeclaration
    decl : AFB.GaloisExtension F E
    expBase : AR.FieldDeclaration
    linkBase : AFB.GaloisExtension.baseField decl ≡ expBase
    status : Core.Phase.Bool

mkGaloisExtensionAdapter :
  (F E : AR.FieldDeclaration) →
  (d : AFB.GaloisExtension F E) →
  (eb : AR.FieldDeclaration) →
  (pb : AFB.GaloisExtension.baseField d ≡ eb) →
  GaloisExtensionAdapter
mkGaloisExtensionAdapter F E d eb pb =
  record { F = F ; E = E ; decl = d ; expBase = eb ; linkBase = pb ; status = true }

isFilledGaloisExtension : GaloisExtensionAdapter → Core.Phase.Bool
isFilledGaloisExtension a = GaloisExtensionAdapter.status a

-- Categorical view for GaloisExtension
galoisExtensionCategorical : (adapt : GaloisExtensionAdapter) →
  CategoricalAdapter {lsuc lzero}
    (AFB.GaloisExtension (GaloisExtensionAdapter.F adapt)
                         (GaloisExtensionAdapter.E adapt))
galoisExtensionCategorical adapt =
  mkCategoricalAdapter (AFB.GaloisExtension (GaloisExtensionAdapter.F adapt)
                                            (GaloisExtensionAdapter.E adapt))
    (λ _ → GaloisExtensionAdapter.decl adapt)

-- Normal extension
record NormalExtensionAdapter : Set₁ where
  field
    F E : AR.FieldDeclaration
    decl : AFB.NormalExtension F E
    expBase : AR.FieldDeclaration
    linkBase : AFB.NormalExtension.baseField decl ≡ expBase
    status : Core.Phase.Bool

mkNormalExtensionAdapter :
  (F E : AR.FieldDeclaration) →
  (d : AFB.NormalExtension F E) →
  (eb : AR.FieldDeclaration) →
  (pb : AFB.NormalExtension.baseField d ≡ eb) →
  NormalExtensionAdapter
mkNormalExtensionAdapter F E d eb pb =
  record { F = F ; E = E ; decl = d ; expBase = eb ; linkBase = pb ; status = true }

isFilledNormalExtension : NormalExtensionAdapter → Core.Phase.Bool
isFilledNormalExtension a = NormalExtensionAdapter.status a

-- Categorical view for NormalExtension
normalExtensionCategorical : (adapt : NormalExtensionAdapter) →
  CategoricalAdapter {lsuc lzero}
    (AFB.NormalExtension (NormalExtensionAdapter.F adapt)
                         (NormalExtensionAdapter.E adapt))
normalExtensionCategorical adapt =
  mkCategoricalAdapter (AFB.NormalExtension (NormalExtensionAdapter.F adapt)
                                            (NormalExtensionAdapter.E adapt))
    (λ _ → NormalExtensionAdapter.decl adapt)

-- Separable extension
record SeparableExtensionAdapter : Set₁ where
  field
    F E : AR.FieldDeclaration
    decl : AFB.SeparableExtension F E
    expBase : AR.FieldDeclaration
    linkBase : AFB.SeparableExtension.baseField decl ≡ expBase
    status : Core.Phase.Bool

mkSeparableExtensionAdapter :
  (F E : AR.FieldDeclaration) →
  (d : AFB.SeparableExtension F E) →
  (eb : AR.FieldDeclaration) →
  (pb : AFB.SeparableExtension.baseField d ≡ eb) →
  SeparableExtensionAdapter
mkSeparableExtensionAdapter F E d eb pb =
  record { F = F ; E = E ; decl = d ; expBase = eb ; linkBase = pb ; status = true }

isFilledSeparableExtension : SeparableExtensionAdapter → Core.Phase.Bool
isFilledSeparableExtension a = SeparableExtensionAdapter.status a

-- Categorical view for SeparableExtension
separableExtensionCategorical : (adapt : SeparableExtensionAdapter) →
  CategoricalAdapter {lsuc lzero}
    (AFB.SeparableExtension (SeparableExtensionAdapter.F adapt)
                            (SeparableExtensionAdapter.E adapt))
separableExtensionCategorical adapt =
  mkCategoricalAdapter (AFB.SeparableExtension (SeparableExtensionAdapter.F adapt)
                                               (SeparableExtensionAdapter.E adapt))
    (λ _ → SeparableExtensionAdapter.decl adapt)

-- Splitting field
record SplittingFieldAdapter : Set₁ where
  field
    F : AR.FieldDeclaration
    f : M.Identifier
    decl : AFB.SplittingField F f
    expBase : AR.FieldDeclaration
    linkBase : AFB.SplittingField.baseField decl ≡ expBase
    status : Core.Phase.Bool

mkSplittingFieldAdapter :
  (F : AR.FieldDeclaration) →
  (f : M.Identifier) →
  (d : AFB.SplittingField F f) →
  (eb : AR.FieldDeclaration) →
  (pb : AFB.SplittingField.baseField d ≡ eb) →
  SplittingFieldAdapter
mkSplittingFieldAdapter F f d eb pb =
  record { F = F ; f = f ; decl = d ; expBase = eb ; linkBase = pb ; status = true }

isFilledSplittingField : SplittingFieldAdapter → Core.Phase.Bool
isFilledSplittingField a = SplittingFieldAdapter.status a

-- Categorical view for SplittingField
splittingFieldCategorical : (adapt : SplittingFieldAdapter) →
  CategoricalAdapter {lsuc lzero}
    (AFB.SplittingField (SplittingFieldAdapter.F adapt)
                        (SplittingFieldAdapter.f adapt))
splittingFieldCategorical adapt =
  mkCategoricalAdapter (AFB.SplittingField (SplittingFieldAdapter.F adapt)
                                           (SplittingFieldAdapter.f adapt))
    (λ _ → SplittingFieldAdapter.decl adapt)

-- Algebraic closure
record AlgebraicClosureAdapter : Set₁ where
  field
    F : AR.FieldDeclaration
    decl : AFB.AlgebraicClosure F
    expBase : AR.FieldDeclaration
    linkBase : AFB.AlgebraicClosure.baseField decl ≡ expBase
    status : Core.Phase.Bool

mkAlgebraicClosureAdapter :
  (F : AR.FieldDeclaration) →
  (d : AFB.AlgebraicClosure F) →
  (eb : AR.FieldDeclaration) →
  (pb : AFB.AlgebraicClosure.baseField d ≡ eb) →
  AlgebraicClosureAdapter
mkAlgebraicClosureAdapter F d eb pb =
  record { F = F ; decl = d ; expBase = eb ; linkBase = pb ; status = true }

isFilledAlgebraicClosure : AlgebraicClosureAdapter → Core.Phase.Bool
isFilledAlgebraicClosure a = AlgebraicClosureAdapter.status a

-- Categorical view for AlgebraicClosure
algebraicClosureCategorical : (adapt : AlgebraicClosureAdapter) →
  CategoricalAdapter {lsuc lzero}
    (AFB.AlgebraicClosure (AlgebraicClosureAdapter.F adapt))
algebraicClosureCategorical adapt =
  mkCategoricalAdapter (AFB.AlgebraicClosure (AlgebraicClosureAdapter.F adapt))
    (λ _ → AlgebraicClosureAdapter.decl adapt)

-- ==========================================================
-- Algebra.Rings.Basic - Ring Theory & Ideal Theory
-- ==========================================================

-- Ideal
record IdealAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    decl : AR.Ideal R
    expRing : AR.RingDeclaration
    linkRing : AR.Ideal.ring decl ≡ expRing
    status : Core.Phase.Bool

mkIdealAdapter :
  (R : AR.RingDeclaration) →
  (d : AR.Ideal R) →
  (er : AR.RingDeclaration) →
  (pr : AR.Ideal.ring d ≡ er) →
  IdealAdapter
mkIdealAdapter R d er pr =
  record { R = R ; decl = d ; expRing = er ; linkRing = pr ; status = true }

isFilledIdeal : IdealAdapter → Core.Phase.Bool
isFilledIdeal a = IdealAdapter.status a

-- Categorical view for Ideal
idealCategorical : (adapt : IdealAdapter) →
  CategoricalAdapter {lsuc lzero} (AR.Ideal (IdealAdapter.R adapt))
idealCategorical adapt =
  mkCategoricalAdapter (AR.Ideal (IdealAdapter.R adapt))
    (λ _ → IdealAdapter.decl adapt)

-- Prime ideal
record PrimeIdealAdapter : Set₁ where
  field
    R : AR.CommutativeRingDeclaration
    decl : AR.PrimeIdeal R
    expRing : AR.CommutativeRingDeclaration
    linkRing : AR.PrimeIdeal.ring decl ≡ expRing
    status : Core.Phase.Bool

mkPrimeIdealAdapter :
  (R : AR.CommutativeRingDeclaration) →
  (d : AR.PrimeIdeal R) →
  (er : AR.CommutativeRingDeclaration) →
  (pr : AR.PrimeIdeal.ring d ≡ er) →
  PrimeIdealAdapter
mkPrimeIdealAdapter R d er pr =
  record { R = R ; decl = d ; expRing = er ; linkRing = pr ; status = true }

isFilledPrimeIdeal : PrimeIdealAdapter → Core.Phase.Bool
isFilledPrimeIdeal a = PrimeIdealAdapter.status a

-- Categorical view for PrimeIdeal
primeIdealCategorical : (adapt : PrimeIdealAdapter) →
  CategoricalAdapter {lsuc lzero} (AR.PrimeIdeal (PrimeIdealAdapter.R adapt))
primeIdealCategorical adapt =
  mkCategoricalAdapter (AR.PrimeIdeal (PrimeIdealAdapter.R adapt))
    (λ _ → PrimeIdealAdapter.decl adapt)

-- Maximal ideal
record MaximalIdealAdapter : Set₁ where
  field
    R : AR.CommutativeRingDeclaration
    decl : AR.MaximalIdeal R
    expRing : AR.CommutativeRingDeclaration
    linkRing : AR.MaximalIdeal.ring decl ≡ expRing
    status : Core.Phase.Bool

mkMaximalIdealAdapter :
  (R : AR.CommutativeRingDeclaration) →
  (d : AR.MaximalIdeal R) →
  (er : AR.CommutativeRingDeclaration) →
  (pr : AR.MaximalIdeal.ring d ≡ er) →
  MaximalIdealAdapter
mkMaximalIdealAdapter R d er pr =
  record { R = R ; decl = d ; expRing = er ; linkRing = pr ; status = true }

isFilledMaximalIdeal : MaximalIdealAdapter → Core.Phase.Bool
isFilledMaximalIdeal a = MaximalIdealAdapter.status a

-- Categorical view for MaximalIdeal
maximalIdealCategorical : (adapt : MaximalIdealAdapter) →
  CategoricalAdapter {lsuc lzero} (AR.MaximalIdeal (MaximalIdealAdapter.R adapt))
maximalIdealCategorical adapt =
  mkCategoricalAdapter (AR.MaximalIdeal (MaximalIdealAdapter.R adapt))
    (λ _ → MaximalIdealAdapter.decl adapt)

-- Integral domain
record IntegralDomainAdapter : Set₁ where
  field
    decl : AR.IntegralDomain
    expRing : AR.CommutativeRingDeclaration
    linkRing : AR.IntegralDomain.underlyingRing decl ≡ expRing
    status : Core.Phase.Bool

mkIntegralDomainAdapter :
  (d : AR.IntegralDomain) →
  (er : AR.CommutativeRingDeclaration) →
  (pr : AR.IntegralDomain.underlyingRing d ≡ er) →
  IntegralDomainAdapter
mkIntegralDomainAdapter d er pr =
  record { decl = d ; expRing = er ; linkRing = pr ; status = true }

isFilledIntegralDomain : IntegralDomainAdapter → Core.Phase.Bool
isFilledIntegralDomain a = IntegralDomainAdapter.status a

-- Categorical view for IntegralDomain
integralDomainCategorical : IntegralDomainAdapter →
  CategoricalAdapter {lsuc lzero} AR.IntegralDomain
integralDomainCategorical adapt =
  mkCategoricalAdapter AR.IntegralDomain (λ _ → IntegralDomainAdapter.decl adapt)

-- Irreducible element
record IrreducibleElementAdapter : Set₁ where
  field
    R : AR.IntegralDomain
    p : M.Identifier
    decl : AR.IrreducibleElement R p
    expDomain : AR.IntegralDomain
    linkDomain : AR.IrreducibleElement.domain decl ≡ expDomain
    status : Core.Phase.Bool

mkIrreducibleElementAdapter :
  (R : AR.IntegralDomain) →
  (p : M.Identifier) →
  (d : AR.IrreducibleElement R p) →
  (ed : AR.IntegralDomain) →
  (pd : AR.IrreducibleElement.domain d ≡ ed) →
  IrreducibleElementAdapter
mkIrreducibleElementAdapter R p d ed pd =
  record { R = R ; p = p ; decl = d ; expDomain = ed ; linkDomain = pd ; status = true }

isFilledIrreducibleElement : IrreducibleElementAdapter → Core.Phase.Bool
isFilledIrreducibleElement a = IrreducibleElementAdapter.status a

-- Categorical view for IrreducibleElement
irreducibleElementCategorical : (adapt : IrreducibleElementAdapter) →
  CategoricalAdapter {lsuc lzero}
    (AR.IrreducibleElement (IrreducibleElementAdapter.R adapt)
                           (IrreducibleElementAdapter.p adapt))
irreducibleElementCategorical adapt =
  mkCategoricalAdapter (AR.IrreducibleElement (IrreducibleElementAdapter.R adapt)
                                              (IrreducibleElementAdapter.p adapt))
    (λ _ → IrreducibleElementAdapter.decl adapt)

-- Prime element
record PrimeElementAdapter : Set₁ where
  field
    R : AR.IntegralDomain
    p : M.Identifier
    decl : AR.PrimeElement R p
    expDomain : AR.IntegralDomain
    linkDomain : AR.PrimeElement.domain decl ≡ expDomain
    status : Core.Phase.Bool

mkPrimeElementAdapter :
  (R : AR.IntegralDomain) →
  (p : M.Identifier) →
  (d : AR.PrimeElement R p) →
  (ed : AR.IntegralDomain) →
  (pd : AR.PrimeElement.domain d ≡ ed) →
  PrimeElementAdapter
mkPrimeElementAdapter R p d ed pd =
  record { R = R ; p = p ; decl = d ; expDomain = ed ; linkDomain = pd ; status = true }

isFilledPrimeElement : PrimeElementAdapter → Core.Phase.Bool
isFilledPrimeElement a = PrimeElementAdapter.status a

-- Categorical view for PrimeElement
primeElementCategorical : (adapt : PrimeElementAdapter) →
  CategoricalAdapter {lsuc lzero}
    (AR.PrimeElement (PrimeElementAdapter.R adapt)
                     (PrimeElementAdapter.p adapt))
primeElementCategorical adapt =
  mkCategoricalAdapter (AR.PrimeElement (PrimeElementAdapter.R adapt)
                                        (PrimeElementAdapter.p adapt))
    (λ _ → PrimeElementAdapter.decl adapt)

-- Unique factorization domain (UFD)
record UFDAdapter : Set₁ where
  field
    decl : AR.UFD
    expDomain : AR.IntegralDomain
    linkDomain : AR.UFD.domain decl ≡ expDomain
    status : Core.Phase.Bool

mkUFDAdapter :
  (d : AR.UFD) →
  (ed : AR.IntegralDomain) →
  (pd : AR.UFD.domain d ≡ ed) →
  UFDAdapter
mkUFDAdapter d ed pd =
  record { decl = d ; expDomain = ed ; linkDomain = pd ; status = true }

isFilledUFD : UFDAdapter → Core.Phase.Bool
isFilledUFD a = UFDAdapter.status a

-- Categorical view for UFD
ufdCategorical : UFDAdapter → CategoricalAdapter {lsuc lzero} AR.UFD
ufdCategorical adapt = mkCategoricalAdapter AR.UFD (λ _ → UFDAdapter.decl adapt)

-- Principal ideal domain (PID)
record PrincipalIdealDomainAdapter : Set₁ where
  field
    decl : AR.PrincipalIdealDomain
    expDomain : AR.IntegralDomain
    linkDomain : AR.PrincipalIdealDomain.domain decl ≡ expDomain
    status : Core.Phase.Bool

mkPrincipalIdealDomainAdapter :
  (d : AR.PrincipalIdealDomain) →
  (ed : AR.IntegralDomain) →
  (pd : AR.PrincipalIdealDomain.domain d ≡ ed) →
  PrincipalIdealDomainAdapter
mkPrincipalIdealDomainAdapter d ed pd =
  record { decl = d ; expDomain = ed ; linkDomain = pd ; status = true }

isFilledPrincipalIdealDomain : PrincipalIdealDomainAdapter → Core.Phase.Bool
isFilledPrincipalIdealDomain a = PrincipalIdealDomainAdapter.status a

-- Categorical view for PrincipalIdealDomain
principalIdealDomainCategorical : PrincipalIdealDomainAdapter →
  CategoricalAdapter {lsuc lzero} AR.PrincipalIdealDomain
principalIdealDomainCategorical adapt =
  mkCategoricalAdapter AR.PrincipalIdealDomain (λ _ → PrincipalIdealDomainAdapter.decl adapt)

-- Euclidean domain
record EuclideanDomainAdapter : Set₁ where
  field
    decl : AR.EuclideanDomain
    expDomain : AR.IntegralDomain
    linkDomain : AR.EuclideanDomain.domain decl ≡ expDomain
    status : Core.Phase.Bool

mkEuclideanDomainAdapter :
  (d : AR.EuclideanDomain) →
  (ed : AR.IntegralDomain) →
  (pd : AR.EuclideanDomain.domain d ≡ ed) →
  EuclideanDomainAdapter
mkEuclideanDomainAdapter d ed pd =
  record { decl = d ; expDomain = ed ; linkDomain = pd ; status = true }

isFilledEuclideanDomain : EuclideanDomainAdapter → Core.Phase.Bool
isFilledEuclideanDomain a = EuclideanDomainAdapter.status a

-- Categorical view for EuclideanDomain
euclideanDomainCategorical : EuclideanDomainAdapter →
  CategoricalAdapter {lsuc lzero} AR.EuclideanDomain
euclideanDomainCategorical adapt =
  mkCategoricalAdapter AR.EuclideanDomain (λ _ → EuclideanDomainAdapter.decl adapt)

-- Multiplicative system
record MultiplicativeSystemAdapter : Set₁ where
  field
    R : AR.CommutativeRingDeclaration
    decl : AR.MultiplicativeSystem R
    expRing : AR.CommutativeRingDeclaration
    linkRing : AR.MultiplicativeSystem.ring decl ≡ expRing
    status : Core.Phase.Bool

mkMultiplicativeSystemAdapter :
  (R : AR.CommutativeRingDeclaration) →
  (d : AR.MultiplicativeSystem R) →
  (er : AR.CommutativeRingDeclaration) →
  (pr : AR.MultiplicativeSystem.ring d ≡ er) →
  MultiplicativeSystemAdapter
mkMultiplicativeSystemAdapter R d er pr =
  record { R = R ; decl = d ; expRing = er ; linkRing = pr ; status = true }

isFilledMultiplicativeSystem : MultiplicativeSystemAdapter → Core.Phase.Bool
isFilledMultiplicativeSystem a = MultiplicativeSystemAdapter.status a

-- Categorical view for MultiplicativeSystem
multiplicativeSystemCategorical : (adapt : MultiplicativeSystemAdapter) →
  CategoricalAdapter {lsuc lzero} (AR.MultiplicativeSystem (MultiplicativeSystemAdapter.R adapt))
multiplicativeSystemCategorical adapt =
  mkCategoricalAdapter (AR.MultiplicativeSystem (MultiplicativeSystemAdapter.R adapt))
    (λ _ → MultiplicativeSystemAdapter.decl adapt)

-- Localization
record LocalizationAdapter : Set₁ where
  field
    R : AR.CommutativeRingDeclaration
    S : AR.MultiplicativeSystem R
    decl : AR.Localization R S
    expRing : AR.CommutativeRingDeclaration
    linkRing : AR.Localization.ring decl ≡ expRing
    status : Core.Phase.Bool

mkLocalizationAdapter :
  (R : AR.CommutativeRingDeclaration) →
  (S : AR.MultiplicativeSystem R) →
  (d : AR.Localization R S) →
  (er : AR.CommutativeRingDeclaration) →
  (pr : AR.Localization.ring d ≡ er) →
  LocalizationAdapter
mkLocalizationAdapter R S d er pr =
  record { R = R ; S = S ; decl = d ; expRing = er ; linkRing = pr ; status = true }

isFilledLocalization : LocalizationAdapter → Core.Phase.Bool
isFilledLocalization a = LocalizationAdapter.status a

-- Categorical view for Localization
localizationCategorical : (adapt : LocalizationAdapter) →
  CategoricalAdapter {lsuc lzero}
    (AR.Localization (LocalizationAdapter.R adapt) (LocalizationAdapter.S adapt))
localizationCategorical adapt =
  mkCategoricalAdapter (AR.Localization (LocalizationAdapter.R adapt) (LocalizationAdapter.S adapt))
    (λ _ → LocalizationAdapter.decl adapt)

-- Field of fractions
record FieldOfFractionsAdapter : Set₁ where
  field
    R : AR.IntegralDomain
    decl : AR.FieldOfFractions R
    expDomain : AR.IntegralDomain
    linkDomain : AR.FieldOfFractions.domain decl ≡ expDomain
    status : Core.Phase.Bool

mkFieldOfFractionsAdapter :
  (R : AR.IntegralDomain) →
  (d : AR.FieldOfFractions R) →
  (ed : AR.IntegralDomain) →
  (pd : AR.FieldOfFractions.domain d ≡ ed) →
  FieldOfFractionsAdapter
mkFieldOfFractionsAdapter R d ed pd =
  record { R = R ; decl = d ; expDomain = ed ; linkDomain = pd ; status = true }

isFilledFieldOfFractions : FieldOfFractionsAdapter → Core.Phase.Bool
isFilledFieldOfFractions a = FieldOfFractionsAdapter.status a

-- Categorical view for FieldOfFractions
fieldOfFractionsCategorical : (adapt : FieldOfFractionsAdapter) →
  CategoricalAdapter {lsuc lzero} (AR.FieldOfFractions (FieldOfFractionsAdapter.R adapt))
fieldOfFractionsCategorical adapt =
  mkCategoricalAdapter (AR.FieldOfFractions (FieldOfFractionsAdapter.R adapt))
    (λ _ → FieldOfFractionsAdapter.decl adapt)

-- Polynomial ring
record PolynomialRingAdapter : Set₁ where
  field
    R : AR.CommutativeRingDeclaration
    decl : AR.PolynomialRing R
    expCoeffRing : AR.CommutativeRingDeclaration
    linkCoeffRing : AR.PolynomialRing.coefficientRing decl ≡ expCoeffRing
    status : Core.Phase.Bool

mkPolynomialRingAdapter :
  (R : AR.CommutativeRingDeclaration) →
  (d : AR.PolynomialRing R) →
  (ec : AR.CommutativeRingDeclaration) →
  (pc : AR.PolynomialRing.coefficientRing d ≡ ec) →
  PolynomialRingAdapter
mkPolynomialRingAdapter R d ec pc =
  record { R = R ; decl = d ; expCoeffRing = ec ; linkCoeffRing = pc ; status = true }

isFilledPolynomialRing : PolynomialRingAdapter → Core.Phase.Bool
isFilledPolynomialRing a = PolynomialRingAdapter.status a

-- Categorical view for PolynomialRing
polynomialRingCategorical : (adapt : PolynomialRingAdapter) →
  CategoricalAdapter {lsuc lzero} (AR.PolynomialRing (PolynomialRingAdapter.R adapt))
polynomialRingCategorical adapt =
  mkCategoricalAdapter (AR.PolynomialRing (PolynomialRingAdapter.R adapt))
    (λ _ → PolynomialRingAdapter.decl adapt)

-- Quotient ring
record QuotientRingAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    I : AR.Ideal R
    decl : AR.QuotientRing R I
    expRing : AR.RingDeclaration
    linkRing : AR.QuotientRing.ring decl ≡ expRing
    status : Core.Phase.Bool

mkQuotientRingAdapter :
  (R : AR.RingDeclaration) →
  (I : AR.Ideal R) →
  (d : AR.QuotientRing R I) →
  (er : AR.RingDeclaration) →
  (pr : AR.QuotientRing.ring d ≡ er) →
  QuotientRingAdapter
mkQuotientRingAdapter R I d er pr =
  record { R = R ; I = I ; decl = d ; expRing = er ; linkRing = pr ; status = true }

isFilledQuotientRing : QuotientRingAdapter → Core.Phase.Bool
isFilledQuotientRing a = QuotientRingAdapter.status a

-- Categorical view for QuotientRing
quotientRingCategorical : (adapt : QuotientRingAdapter) →
  CategoricalAdapter {lsuc lzero}
    (AR.QuotientRing (QuotientRingAdapter.R adapt) (QuotientRingAdapter.I adapt))
quotientRingCategorical adapt =
  mkCategoricalAdapter (AR.QuotientRing (QuotientRingAdapter.R adapt) (QuotientRingAdapter.I adapt))
    (λ _ → QuotientRingAdapter.decl adapt)

-- ==========================================================
-- Algebra.Groups.Free - Free Groups and Categorical Constructions
-- ==========================================================

-- Product in Grp
record ProductInGrpAdapter : Set₁ where
  field
    G H : AFo.GroupDeclaration
    decl : AGF.ProductInGrp G H
    expGroup1 : AFo.GroupDeclaration
    linkGroup1 : AGF.ProductInGrp.group1 decl ≡ expGroup1
    status : Core.Phase.Bool

mkProductInGrpAdapter :
  (G H : AFo.GroupDeclaration) →
  (d : AGF.ProductInGrp G H) →
  (eg : AFo.GroupDeclaration) →
  (pg : AGF.ProductInGrp.group1 d ≡ eg) →
  ProductInGrpAdapter
mkProductInGrpAdapter G H d eg pg =
  record { G = G ; H = H ; decl = d ; expGroup1 = eg ; linkGroup1 = pg ; status = true }

isFilledProductInGrp : ProductInGrpAdapter → Core.Phase.Bool
isFilledProductInGrp a = ProductInGrpAdapter.status a

-- Categorical view for ProductInGrp
productInGrpCategorical : (adapt : ProductInGrpAdapter) →
  CategoricalAdapter {lsuc lzero} (AGF.ProductInGrp (ProductInGrpAdapter.G adapt) (ProductInGrpAdapter.H adapt))
productInGrpCategorical adapt =
  mkCategoricalAdapter (AGF.ProductInGrp (ProductInGrpAdapter.G adapt) (ProductInGrpAdapter.H adapt))
    (λ _ → ProductInGrpAdapter.decl adapt)

-- Coproduct in Grp
record CoproductInGrpAdapter : Set₁ where
  field
    G H : AFo.GroupDeclaration
    decl : AGF.CoproductInGrp G H
    expGroup1 : AFo.GroupDeclaration
    linkGroup1 : AGF.CoproductInGrp.group1 decl ≡ expGroup1
    status : Core.Phase.Bool

mkCoproductInGrpAdapter :
  (G H : AFo.GroupDeclaration) →
  (d : AGF.CoproductInGrp G H) →
  (eg : AFo.GroupDeclaration) →
  (pg : AGF.CoproductInGrp.group1 d ≡ eg) →
  CoproductInGrpAdapter
mkCoproductInGrpAdapter G H d eg pg =
  record { G = G ; H = H ; decl = d ; expGroup1 = eg ; linkGroup1 = pg ; status = true }

isFilledCoproductInGrp : CoproductInGrpAdapter → Core.Phase.Bool
isFilledCoproductInGrp a = CoproductInGrpAdapter.status a

-- Categorical view for CoproductInGrp
coproductInGrpCategorical : (adapt : CoproductInGrpAdapter) →
  CategoricalAdapter {lsuc lzero} (AGF.CoproductInGrp (CoproductInGrpAdapter.G adapt) (CoproductInGrpAdapter.H adapt))
coproductInGrpCategorical adapt =
  mkCategoricalAdapter (AGF.CoproductInGrp (CoproductInGrpAdapter.G adapt) (CoproductInGrpAdapter.H adapt))
    (λ _ → CoproductInGrpAdapter.decl adapt)

-- Free group object
record FreeGroupObjectAdapter : Set₁ where
  field
    X : M.Identifier
    decl : AGF.FreeGroupObject X
    expGenSet : M.Identifier
    linkGenSet : AGF.FreeGroupObject.generatingSet decl ≡ expGenSet
    status : Core.Phase.Bool

mkFreeGroupObjectAdapter :
  (X : M.Identifier) →
  (d : AGF.FreeGroupObject X) →
  (eg : M.Identifier) →
  (pg : AGF.FreeGroupObject.generatingSet d ≡ eg) →
  FreeGroupObjectAdapter
mkFreeGroupObjectAdapter X d eg pg =
  record { X = X ; decl = d ; expGenSet = eg ; linkGenSet = pg ; status = true }

isFilledFreeGroupObject : FreeGroupObjectAdapter → Core.Phase.Bool
isFilledFreeGroupObject a = FreeGroupObjectAdapter.status a

-- Categorical view for FreeGroupObject
freeGroupObjectCategorical : (adapt : FreeGroupObjectAdapter) →
  CategoricalAdapter {lsuc lzero} (AGF.FreeGroupObject (FreeGroupObjectAdapter.X adapt))
freeGroupObjectCategorical adapt =
  mkCategoricalAdapter (AGF.FreeGroupObject (FreeGroupObjectAdapter.X adapt))
    (λ _ → FreeGroupObjectAdapter.decl adapt)

-- Free group
record FreeGroupAdapter : Set₁ where
  field
    X : M.Identifier
    decl : AGF.FreeGroup X
    expGenSet : M.Identifier
    linkGenSet : AGF.FreeGroup.generatingSet decl ≡ expGenSet
    status : Core.Phase.Bool

mkFreeGroupAdapter :
  (X : M.Identifier) →
  (d : AGF.FreeGroup X) →
  (eg : M.Identifier) →
  (pg : AGF.FreeGroup.generatingSet d ≡ eg) →
  FreeGroupAdapter
mkFreeGroupAdapter X d eg pg =
  record { X = X ; decl = d ; expGenSet = eg ; linkGenSet = pg ; status = true }

isFilledFreeGroup : FreeGroupAdapter → Core.Phase.Bool
isFilledFreeGroup a = FreeGroupAdapter.status a

-- Categorical view for FreeGroup
freeGroupCategorical : (adapt : FreeGroupAdapter) →
  CategoricalAdapter {lsuc lzero} (AGF.FreeGroup (FreeGroupAdapter.X adapt))
freeGroupCategorical adapt =
  mkCategoricalAdapter (AGF.FreeGroup (FreeGroupAdapter.X adapt))
    (λ _ → FreeGroupAdapter.decl adapt)

-- Group presentation
record GroupPresentationAdapter : Set₁ where
  field
    decl : AGF.GroupPresentation
    expGenerators : M.Identifier
    linkGenerators : AGF.GroupPresentation.generators decl ≡ expGenerators
    status : Core.Phase.Bool

mkGroupPresentationAdapter :
  (d : AGF.GroupPresentation) →
  (eg : M.Identifier) →
  (pg : AGF.GroupPresentation.generators d ≡ eg) →
  GroupPresentationAdapter
mkGroupPresentationAdapter d eg pg =
  record { decl = d ; expGenerators = eg ; linkGenerators = pg ; status = true }

isFilledGroupPresentation : GroupPresentationAdapter → Core.Phase.Bool
isFilledGroupPresentation a = GroupPresentationAdapter.status a

-- Categorical view for GroupPresentation
groupPresentationCategorical : GroupPresentationAdapter →
  CategoricalAdapter {lsuc lzero} AGF.GroupPresentation
groupPresentationCategorical adapt =
  mkCategoricalAdapter AGF.GroupPresentation (λ _ → GroupPresentationAdapter.decl adapt)

-- Abelianization
record AbelianizationAdapter : Set₁ where
  field
    G : AFo.GroupDeclaration
    decl : AGF.Abelianization G
    expGroup : AFo.GroupDeclaration
    linkGroup : AGF.Abelianization.group decl ≡ expGroup
    status : Core.Phase.Bool

mkAbelianizationAdapter :
  (G : AFo.GroupDeclaration) →
  (d : AGF.Abelianization G) →
  (eg : AFo.GroupDeclaration) →
  (pg : AGF.Abelianization.group d ≡ eg) →
  AbelianizationAdapter
mkAbelianizationAdapter G d eg pg =
  record { G = G ; decl = d ; expGroup = eg ; linkGroup = pg ; status = true }

isFilledAbelianization : AbelianizationAdapter → Core.Phase.Bool
isFilledAbelianization a = AbelianizationAdapter.status a

-- Categorical view for Abelianization
abelianizationCategorical : (adapt : AbelianizationAdapter) →
  CategoricalAdapter {lsuc lzero} (AGF.Abelianization (AbelianizationAdapter.G adapt))
abelianizationCategorical adapt =
  mkCategoricalAdapter (AGF.Abelianization (AbelianizationAdapter.G adapt))
    (λ _ → AbelianizationAdapter.decl adapt)

-- Finitely generated abelian group
record FinitelyGeneratedAbelianGroupAdapter : Set₁ where
  field
    decl : AGF.FinitelyGeneratedAbelianGroup
    expGroup : AFo.AbelianGroupDeclaration
    linkGroup : AGF.FinitelyGeneratedAbelianGroup.underlyingGroup decl ≡ expGroup
    status : Core.Phase.Bool

mkFinitelyGeneratedAbelianGroupAdapter :
  (d : AGF.FinitelyGeneratedAbelianGroup) →
  (eg : AFo.AbelianGroupDeclaration) →
  (pg : AGF.FinitelyGeneratedAbelianGroup.underlyingGroup d ≡ eg) →
  FinitelyGeneratedAbelianGroupAdapter
mkFinitelyGeneratedAbelianGroupAdapter d eg pg =
  record { decl = d ; expGroup = eg ; linkGroup = pg ; status = true }

isFilledFinitelyGeneratedAbelianGroup : FinitelyGeneratedAbelianGroupAdapter → Core.Phase.Bool
isFilledFinitelyGeneratedAbelianGroup a = FinitelyGeneratedAbelianGroupAdapter.status a

-- ==========================================================
-- Algebra.Groups.Structure - Group Structure Theory
-- ==========================================================

-- Invariant factor decomposition
record InvariantFactorDecompositionAdapter : Set₁ where
  field
    A : AGF.FinitelyGeneratedAbelianGroup
    decl : AGS.InvariantFactorDecomposition A
    expFreeRank : M.Identifier
    linkFreeRank : AGS.InvariantFactorDecomposition.freeRank decl ≡ expFreeRank
    status : Core.Phase.Bool

mkInvariantFactorDecompositionAdapter :
  (A : AGF.FinitelyGeneratedAbelianGroup) →
  (d : AGS.InvariantFactorDecomposition A) →
  (ef : M.Identifier) →
  (pf : AGS.InvariantFactorDecomposition.freeRank d ≡ ef) →
  InvariantFactorDecompositionAdapter
mkInvariantFactorDecompositionAdapter A d ef pf =
  record { A = A ; decl = d ; expFreeRank = ef ; linkFreeRank = pf ; status = true }

isFilledInvariantFactorDecomposition : InvariantFactorDecompositionAdapter → Core.Phase.Bool
isFilledInvariantFactorDecomposition a = InvariantFactorDecompositionAdapter.status a

-- Categorical view for InvariantFactorDecomposition
invariantFactorDecompositionCategorical : (adapt : InvariantFactorDecompositionAdapter) →
  CategoricalAdapter {lsuc lzero} (AGS.InvariantFactorDecomposition (InvariantFactorDecompositionAdapter.A adapt))
invariantFactorDecompositionCategorical adapt =
  mkCategoricalAdapter (AGS.InvariantFactorDecomposition (InvariantFactorDecompositionAdapter.A adapt))
    (λ _ → InvariantFactorDecompositionAdapter.decl adapt)

-- Torsion subgroup
record TorsionSubgroupAdapter : Set₁ where
  field
    A : AFo.AbelianGroupDeclaration
    decl : AGS.TorsionSubgroup A
    expAbelianGroup : AFo.AbelianGroupDeclaration
    linkAbelianGroup : AGS.TorsionSubgroup.abelianGroup decl ≡ expAbelianGroup
    status : Core.Phase.Bool

mkTorsionSubgroupAdapter :
  (A : AFo.AbelianGroupDeclaration) →
  (d : AGS.TorsionSubgroup A) →
  (ea : AFo.AbelianGroupDeclaration) →
  (pa : AGS.TorsionSubgroup.abelianGroup d ≡ ea) →
  TorsionSubgroupAdapter
mkTorsionSubgroupAdapter A d ea pa =
  record { A = A ; decl = d ; expAbelianGroup = ea ; linkAbelianGroup = pa ; status = true }

isFilledTorsionSubgroup : TorsionSubgroupAdapter → Core.Phase.Bool
isFilledTorsionSubgroup a = TorsionSubgroupAdapter.status a

-- Categorical view for TorsionSubgroup
torsionSubgroupCategorical : (adapt : TorsionSubgroupAdapter) →
  CategoricalAdapter {lsuc lzero} (AGS.TorsionSubgroup (TorsionSubgroupAdapter.A adapt))
torsionSubgroupCategorical adapt =
  mkCategoricalAdapter (AGS.TorsionSubgroup (TorsionSubgroupAdapter.A adapt))
    (λ _ → TorsionSubgroupAdapter.decl adapt)

-- Group action
record GroupActionAdapter : Set₁ where
  field
    G : AFo.GroupDeclaration
    X : M.Identifier
    decl : AGS.GroupAction G X
    expGroup : AFo.GroupDeclaration
    linkGroup : AGS.GroupAction.group decl ≡ expGroup
    status : Core.Phase.Bool

mkGroupActionAdapter :
  (G : AFo.GroupDeclaration) →
  (X : M.Identifier) →
  (d : AGS.GroupAction G X) →
  (eg : AFo.GroupDeclaration) →
  (pg : AGS.GroupAction.group d ≡ eg) →
  GroupActionAdapter
mkGroupActionAdapter G X d eg pg =
  record { G = G ; X = X ; decl = d ; expGroup = eg ; linkGroup = pg ; status = true }

isFilledGroupAction : GroupActionAdapter → Core.Phase.Bool
isFilledGroupAction a = GroupActionAdapter.status a

-- Categorical view for GroupAction
groupActionCategorical : (adapt : GroupActionAdapter) →
  CategoricalAdapter {lsuc lzero}
    (AGS.GroupAction (GroupActionAdapter.G adapt) (GroupActionAdapter.X adapt))
groupActionCategorical adapt =
  mkCategoricalAdapter (AGS.GroupAction (GroupActionAdapter.G adapt) (GroupActionAdapter.X adapt))
    (λ _ → GroupActionAdapter.decl adapt)

-- Orbit
record OrbitAdapter : Set₁ where
  field
    G : AFo.GroupDeclaration
    X : M.Identifier
    act : AGS.GroupAction G X
    x : M.Identifier
    decl : AGS.Orbit G X act x
    expGroupAction : AGS.GroupAction G X
    linkGroupAction : AGS.Orbit.groupAction decl ≡ expGroupAction
    status : Core.Phase.Bool

mkOrbitAdapter :
  (G : AFo.GroupDeclaration) →
  (X : M.Identifier) →
  (act : AGS.GroupAction G X) →
  (x : M.Identifier) →
  (d : AGS.Orbit G X act x) →
  (ea : AGS.GroupAction G X) →
  (pa : AGS.Orbit.groupAction d ≡ ea) →
  OrbitAdapter
mkOrbitAdapter G X act x d ea pa =
  record { G = G ; X = X ; act = act ; x = x ; decl = d ; expGroupAction = ea ; linkGroupAction = pa ; status = true }

isFilledOrbit : OrbitAdapter → Core.Phase.Bool
isFilledOrbit a = OrbitAdapter.status a

-- Categorical view for Orbit
orbitCategorical : (adapt : OrbitAdapter) →
  CategoricalAdapter {lsuc lzero}
    (AGS.Orbit (OrbitAdapter.G adapt) (OrbitAdapter.X adapt) (OrbitAdapter.act adapt) (OrbitAdapter.x adapt))
orbitCategorical adapt =
  mkCategoricalAdapter (AGS.Orbit (OrbitAdapter.G adapt) (OrbitAdapter.X adapt) (OrbitAdapter.act adapt) (OrbitAdapter.x adapt))
    (λ _ → OrbitAdapter.decl adapt)

-- Stabilizer
record StabilizerAdapter : Set₁ where
  field
    G : AFo.GroupDeclaration
    X : M.Identifier
    act : AGS.GroupAction G X
    x : M.Identifier
    decl : AGS.Stabilizer G X act x
    expGroupAction : AGS.GroupAction G X
    linkGroupAction : AGS.Stabilizer.groupAction decl ≡ expGroupAction
    status : Core.Phase.Bool

mkStabilizerAdapter :
  (G : AFo.GroupDeclaration) →
  (X : M.Identifier) →
  (act : AGS.GroupAction G X) →
  (x : M.Identifier) →
  (d : AGS.Stabilizer G X act x) →
  (ea : AGS.GroupAction G X) →
  (pa : AGS.Stabilizer.groupAction d ≡ ea) →
  StabilizerAdapter
mkStabilizerAdapter G X act x d ea pa =
  record { G = G ; X = X ; act = act ; x = x ; decl = d ; expGroupAction = ea ; linkGroupAction = pa ; status = true }

isFilledStabilizer : StabilizerAdapter → Core.Phase.Bool
isFilledStabilizer a = StabilizerAdapter.status a

-- Categorical view for Stabilizer
stabilizerCategorical : (adapt : StabilizerAdapter) →
  CategoricalAdapter {lsuc lzero}
    (AGS.Stabilizer (StabilizerAdapter.G adapt) (StabilizerAdapter.X adapt) (StabilizerAdapter.act adapt) (StabilizerAdapter.x adapt))
stabilizerCategorical adapt =
  mkCategoricalAdapter (AGS.Stabilizer (StabilizerAdapter.G adapt) (StabilizerAdapter.X adapt) (StabilizerAdapter.act adapt) (StabilizerAdapter.x adapt))
    (λ _ → StabilizerAdapter.decl adapt)

-- P-group
record PGroupAdapter : Set₁ where
  field
    p : M.Identifier
    G : AFo.GroupDeclaration
    decl : AGS.PGroup p G
    expPrime : M.Identifier
    linkPrime : AGS.PGroup.prime decl ≡ expPrime
    status : Core.Phase.Bool

mkPGroupAdapter :
  (p : M.Identifier) →
  (G : AFo.GroupDeclaration) →
  (d : AGS.PGroup p G) →
  (ep : M.Identifier) →
  (pp : AGS.PGroup.prime d ≡ ep) →
  PGroupAdapter
mkPGroupAdapter p G d ep pp =
  record { p = p ; G = G ; decl = d ; expPrime = ep ; linkPrime = pp ; status = true }

isFilledPGroup : PGroupAdapter → Core.Phase.Bool
isFilledPGroup a = PGroupAdapter.status a

-- Categorical view for PGroup
pGroupCategorical : (adapt : PGroupAdapter) →
  CategoricalAdapter {lsuc lzero} (AGS.PGroup (PGroupAdapter.p adapt) (PGroupAdapter.G adapt))
pGroupCategorical adapt =
  mkCategoricalAdapter (AGS.PGroup (PGroupAdapter.p adapt) (PGroupAdapter.G adapt))
    (λ _ → PGroupAdapter.decl adapt)

-- Sylow p-subgroup
record SylowPSubgroupAdapter : Set₁ where
  field
    p : M.Identifier
    G : AFo.GroupDeclaration
    decl : AGS.SylowPSubgroup p G
    expPrime : M.Identifier
    linkPrime : AGS.SylowPSubgroup.prime decl ≡ expPrime
    status : Core.Phase.Bool

mkSylowPSubgroupAdapter :
  (p : M.Identifier) →
  (G : AFo.GroupDeclaration) →
  (d : AGS.SylowPSubgroup p G) →
  (ep : M.Identifier) →
  (pp : AGS.SylowPSubgroup.prime d ≡ ep) →
  SylowPSubgroupAdapter
mkSylowPSubgroupAdapter p G d ep pp =
  record { p = p ; G = G ; decl = d ; expPrime = ep ; linkPrime = pp ; status = true }

isFilledSylowPSubgroup : SylowPSubgroupAdapter → Core.Phase.Bool
isFilledSylowPSubgroup a = SylowPSubgroupAdapter.status a

-- Categorical view for SylowPSubgroup
sylowPSubgroupCategorical : (adapt : SylowPSubgroupAdapter) →
  CategoricalAdapter {lsuc lzero} (AGS.SylowPSubgroup (SylowPSubgroupAdapter.p adapt) (SylowPSubgroupAdapter.G adapt))
sylowPSubgroupCategorical adapt =
  mkCategoricalAdapter (AGS.SylowPSubgroup (SylowPSubgroupAdapter.p adapt) (SylowPSubgroupAdapter.G adapt))
    (λ _ → SylowPSubgroupAdapter.decl adapt)

-- Simple group
record SimpleGroupAdapter : Set₁ where
  field
    G : AFo.GroupDeclaration
    decl : AGS.SimpleGroup G
    expGroup : AFo.GroupDeclaration
    linkGroup : AGS.SimpleGroup.group decl ≡ expGroup
    status : Core.Phase.Bool

mkSimpleGroupAdapter :
  (G : AFo.GroupDeclaration) →
  (d : AGS.SimpleGroup G) →
  (eg : AFo.GroupDeclaration) →
  (pg : AGS.SimpleGroup.group d ≡ eg) →
  SimpleGroupAdapter
mkSimpleGroupAdapter G d eg pg =
  record { G = G ; decl = d ; expGroup = eg ; linkGroup = pg ; status = true }

isFilledSimpleGroup : SimpleGroupAdapter → Core.Phase.Bool
isFilledSimpleGroup a = SimpleGroupAdapter.status a

-- Categorical view for SimpleGroup
simpleGroupCategorical : (adapt : SimpleGroupAdapter) →
  CategoricalAdapter {lsuc lzero} (AGS.SimpleGroup (SimpleGroupAdapter.G adapt))
simpleGroupCategorical adapt =
  mkCategoricalAdapter (AGS.SimpleGroup (SimpleGroupAdapter.G adapt))
    (λ _ → SimpleGroupAdapter.decl adapt)

-- Composition series
record CompositionSeriesAdapter : Set₁ where
  field
    G : AFo.GroupDeclaration
    decl : AGS.CompositionSeries G
    expGroup : AFo.GroupDeclaration
    linkGroup : AGS.CompositionSeries.group decl ≡ expGroup
    status : Core.Phase.Bool

mkCompositionSeriesAdapter :
  (G : AFo.GroupDeclaration) →
  (d : AGS.CompositionSeries G) →
  (eg : AFo.GroupDeclaration) →
  (pg : AGS.CompositionSeries.group d ≡ eg) →
  CompositionSeriesAdapter
mkCompositionSeriesAdapter G d eg pg =
  record { G = G ; decl = d ; expGroup = eg ; linkGroup = pg ; status = true }

isFilledCompositionSeries : CompositionSeriesAdapter → Core.Phase.Bool
isFilledCompositionSeries a = CompositionSeriesAdapter.status a

-- Categorical view for CompositionSeries
compositionSeriesCategorical : (adapt : CompositionSeriesAdapter) →
  CategoricalAdapter {lsuc lzero} (AGS.CompositionSeries (CompositionSeriesAdapter.G adapt))
compositionSeriesCategorical adapt =
  mkCategoricalAdapter (AGS.CompositionSeries (CompositionSeriesAdapter.G adapt))
    (λ _ → CompositionSeriesAdapter.decl adapt)

-- Solvable group
record SolvableGroupAdapter : Set₁ where
  field
    G : AFo.GroupDeclaration
    decl : AGS.SolvableGroup G
    expGroup : AFo.GroupDeclaration
    linkGroup : AGS.SolvableGroup.group decl ≡ expGroup
    status : Core.Phase.Bool

mkSolvableGroupAdapter :
  (G : AFo.GroupDeclaration) →
  (d : AGS.SolvableGroup G) →
  (eg : AFo.GroupDeclaration) →
  (pg : AGS.SolvableGroup.group d ≡ eg) →
  SolvableGroupAdapter
mkSolvableGroupAdapter G d eg pg =
  record { G = G ; decl = d ; expGroup = eg ; linkGroup = pg ; status = true }

isFilledSolvableGroup : SolvableGroupAdapter → Core.Phase.Bool
isFilledSolvableGroup a = SolvableGroupAdapter.status a

-- Categorical view for SolvableGroup
solvableGroupCategorical : (adapt : SolvableGroupAdapter) →
  CategoricalAdapter {lsuc lzero} (AGS.SolvableGroup (SolvableGroupAdapter.G adapt))
solvableGroupCategorical adapt =
  mkCategoricalAdapter (AGS.SolvableGroup (SolvableGroupAdapter.G adapt))
    (λ _ → SolvableGroupAdapter.decl adapt)

-- Nilpotent group
record NilpotentGroupAdapter : Set₁ where
  field
    G : AFo.GroupDeclaration
    decl : AGS.NilpotentGroup G
    expGroup : AFo.GroupDeclaration
    linkGroup : AGS.NilpotentGroup.group decl ≡ expGroup
    status : Core.Phase.Bool

mkNilpotentGroupAdapter :
  (G : AFo.GroupDeclaration) →
  (d : AGS.NilpotentGroup G) →
  (eg : AFo.GroupDeclaration) →
  (pg : AGS.NilpotentGroup.group d ≡ eg) →
  NilpotentGroupAdapter
mkNilpotentGroupAdapter G d eg pg =
  record { G = G ; decl = d ; expGroup = eg ; linkGroup = pg ; status = true }

isFilledNilpotentGroup : NilpotentGroupAdapter → Core.Phase.Bool
isFilledNilpotentGroup a = NilpotentGroupAdapter.status a

-- Categorical view for NilpotentGroup
nilpotentGroupCategorical : (adapt : NilpotentGroupAdapter) →
  CategoricalAdapter {lsuc lzero} (AGS.NilpotentGroup (NilpotentGroupAdapter.G adapt))
nilpotentGroupCategorical adapt =
  mkCategoricalAdapter (AGS.NilpotentGroup (NilpotentGroupAdapter.G adapt))
    (λ _ → NilpotentGroupAdapter.decl adapt)

-- ==========================================================
-- Algebra.Groups.Abelian - Free Abelian Groups & Grothendieck
-- ==========================================================

-- Free abelian group
record FreeAbelianGroupAdapter : Set₁ where
  field
    X : M.Identifier
    decl : AGA.FreeAbelianGroup X
    expUnderlyingSet : M.Identifier
    linkUnderlyingSet : AGA.FreeAbelianGroup.underlyingSet decl ≡ expUnderlyingSet
    status : Core.Phase.Bool

mkFreeAbelianGroupAdapter :
  (X : M.Identifier) →
  (d : AGA.FreeAbelianGroup X) →
  (eu : M.Identifier) →
  (pu : AGA.FreeAbelianGroup.underlyingSet d ≡ eu) →
  FreeAbelianGroupAdapter
mkFreeAbelianGroupAdapter X d eu pu =
  record { X = X ; decl = d ; expUnderlyingSet = eu ; linkUnderlyingSet = pu ; status = true }

isFilledFreeAbelianGroup : FreeAbelianGroupAdapter → Core.Phase.Bool
isFilledFreeAbelianGroup a = FreeAbelianGroupAdapter.status a

-- Categorical view for FreeAbelianGroup
freeAbelianGroupCategorical : (adapt : FreeAbelianGroupAdapter) →
  CategoricalAdapter {lsuc lzero} (AGA.FreeAbelianGroup (FreeAbelianGroupAdapter.X adapt))
freeAbelianGroupCategorical adapt =
  mkCategoricalAdapter (AGA.FreeAbelianGroup (FreeAbelianGroupAdapter.X adapt))
    (λ _ → FreeAbelianGroupAdapter.decl adapt)

-- Free-Forgetful adjunction for Ab
record FreeForgetfulAdjunctionAbAdapter : Set₁ where
  field
    decl : AGA.FreeForgetfulAdjunctionAb
    expFreeFunctor : M.Identifier
    linkFreeFunctor : AGA.FreeForgetfulAdjunctionAb.freeFunctor decl ≡ expFreeFunctor
    status : Core.Phase.Bool

mkFreeForgetfulAdjunctionAbAdapter :
  (d : AGA.FreeForgetfulAdjunctionAb) →
  (ef : M.Identifier) →
  (pf : AGA.FreeForgetfulAdjunctionAb.freeFunctor d ≡ ef) →
  FreeForgetfulAdjunctionAbAdapter
mkFreeForgetfulAdjunctionAbAdapter d ef pf =
  record { decl = d ; expFreeFunctor = ef ; linkFreeFunctor = pf ; status = true }

isFilledFreeForgetfulAdjunctionAb : FreeForgetfulAdjunctionAbAdapter → Core.Phase.Bool
isFilledFreeForgetfulAdjunctionAb a = FreeForgetfulAdjunctionAbAdapter.status a

-- Categorical view for FreeForgetfulAdjunctionAb
freeForgetfulAdjunctionAbCategorical : FreeForgetfulAdjunctionAbAdapter →
  CategoricalAdapter {lsuc lzero} AGA.FreeForgetfulAdjunctionAb
freeForgetfulAdjunctionAbCategorical adapt =
  mkCategoricalAdapter AGA.FreeForgetfulAdjunctionAb (λ _ → FreeForgetfulAdjunctionAbAdapter.decl adapt)

-- Grothendieck group
record GrothendieckGroupAdapter : Set₁ where
  field
    M : AFo.MonoidDeclaration
    decl : AGA.GrothendieckGroup M
    expUnderlyingSet : M.Identifier
    linkUnderlyingSet : AGA.GrothendieckGroup.underlyingSet decl ≡ expUnderlyingSet
    status : Core.Phase.Bool

mkGrothendieckGroupAdapter :
  (M : AFo.MonoidDeclaration) →
  (d : AGA.GrothendieckGroup M) →
  (eu : M.Identifier) →
  (pu : AGA.GrothendieckGroup.underlyingSet d ≡ eu) →
  GrothendieckGroupAdapter
mkGrothendieckGroupAdapter M d eu pu =
  record { M = M ; decl = d ; expUnderlyingSet = eu ; linkUnderlyingSet = pu ; status = true }

isFilledGrothendieckGroup : GrothendieckGroupAdapter → Core.Phase.Bool
isFilledGrothendieckGroup a = GrothendieckGroupAdapter.status a

-- Categorical view for GrothendieckGroup
grothendieckGroupCategorical : (adapt : GrothendieckGroupAdapter) →
  CategoricalAdapter {lsuc lzero} (AGA.GrothendieckGroup (GrothendieckGroupAdapter.M adapt))
grothendieckGroupCategorical adapt =
  mkCategoricalAdapter (AGA.GrothendieckGroup (GrothendieckGroupAdapter.M adapt))
    (λ _ → GrothendieckGroupAdapter.decl adapt)

-- Tensor product of abelian groups
record TensorProductAbAdapter : Set₁ where
  field
    A : AFo.AbelianGroupDeclaration
    B : AFo.AbelianGroupDeclaration
    decl : AGA.TensorProductAb A B
    expTensorProduct : AFo.AbelianGroupDeclaration
    linkTensorProduct : AGA.TensorProductAb.tensorProduct decl ≡ expTensorProduct
    status : Core.Phase.Bool

mkTensorProductAbAdapter :
  (A B : AFo.AbelianGroupDeclaration) →
  (d : AGA.TensorProductAb A B) →
  (et : AFo.AbelianGroupDeclaration) →
  (pt : AGA.TensorProductAb.tensorProduct d ≡ et) →
  TensorProductAbAdapter
mkTensorProductAbAdapter A B d et pt =
  record { A = A ; B = B ; decl = d ; expTensorProduct = et ; linkTensorProduct = pt ; status = true }

isFilledTensorProductAb : TensorProductAbAdapter → Core.Phase.Bool
isFilledTensorProductAb a = TensorProductAbAdapter.status a

-- Categorical view for TensorProductAb
tensorProductAbCategorical : (adapt : TensorProductAbAdapter) →
  CategoricalAdapter {lsuc lzero} (AGA.TensorProductAb (TensorProductAbAdapter.A adapt) (TensorProductAbAdapter.B adapt))
tensorProductAbCategorical adapt =
  mkCategoricalAdapter (AGA.TensorProductAb (TensorProductAbAdapter.A adapt) (TensorProductAbAdapter.B adapt))
    (λ _ → TensorProductAbAdapter.decl adapt)

-- Basis of vector space
record BasisOfVectorSpaceAdapter : Set₁ where
  field
    F : AR.FieldDeclaration
    V : AM.VectorSpace F
    decl : AM.BasisOfVectorSpace F V
    expBasisSet : M.Identifier
    linkBasisSet : AM.BasisOfVectorSpace.basisSet decl ≡ expBasisSet
    status : Core.Phase.Bool

mkBasisOfVectorSpaceAdapter :
  (F : AR.FieldDeclaration) →
  (V : AM.VectorSpace F) →
  (d : AM.BasisOfVectorSpace F V) →
  (eb : M.Identifier) →
  (pb : AM.BasisOfVectorSpace.basisSet d ≡ eb) →
  BasisOfVectorSpaceAdapter
mkBasisOfVectorSpaceAdapter F V d eb pb =
  record { F = F ; V = V ; decl = d ; expBasisSet = eb ; linkBasisSet = pb ; status = true }

isFilledBasisOfVectorSpace : BasisOfVectorSpaceAdapter → Core.Phase.Bool
isFilledBasisOfVectorSpace a = BasisOfVectorSpaceAdapter.status a

-- Categorical view for BasisOfVectorSpace
basisOfVectorSpaceCategorical : (adapt : BasisOfVectorSpaceAdapter) →
  CategoricalAdapter {lsuc lzero}
    (AM.BasisOfVectorSpace (BasisOfVectorSpaceAdapter.F adapt)
                           (BasisOfVectorSpaceAdapter.V adapt))
basisOfVectorSpaceCategorical adapt =
  mkCategoricalAdapter (AM.BasisOfVectorSpace (BasisOfVectorSpaceAdapter.F adapt)
                                              (BasisOfVectorSpaceAdapter.V adapt))
    (λ _ → BasisOfVectorSpaceAdapter.decl adapt)

-- Dimension of vector space
record DimensionAdapter : Set₁ where
  field
    F : AR.FieldDeclaration
    V : AM.VectorSpace F
    decl : AM.Dimension F V
    expDimensionValue : M.Identifier
    linkDimensionValue : AM.Dimension.dimension decl ≡ expDimensionValue
    status : Core.Phase.Bool

mkDimensionAdapter :
  (F : AR.FieldDeclaration) →
  (V : AM.VectorSpace F) →
  (d : AM.Dimension F V) →
  (edv : M.Identifier) →
  (pdv : AM.Dimension.dimension d ≡ edv) →
  DimensionAdapter
mkDimensionAdapter F V d edv pdv =
  record { F = F ; V = V ; decl = d ; expDimensionValue = edv ; linkDimensionValue = pdv ; status = true }

isFilledDimension : DimensionAdapter → Core.Phase.Bool
isFilledDimension a = DimensionAdapter.status a

-- Categorical view for Dimension
dimensionCategorical : (adapt : DimensionAdapter) →
  CategoricalAdapter {lsuc lzero}
    (AM.Dimension (DimensionAdapter.F adapt)
                  (DimensionAdapter.V adapt))
dimensionCategorical adapt =
  mkCategoricalAdapter (AM.Dimension (DimensionAdapter.F adapt)
                                     (DimensionAdapter.V adapt))
    (λ _ → DimensionAdapter.decl adapt)

-- Multivariate polynomial ring
record MultivariatePolynomialRingAdapter : Set₁ where
  field
    R : AR.CommutativeRingDeclaration
    n : M.Identifier
    decl : AR.MultivariatePolynomialRing R n
    expRing : AR.CommutativeRingDeclaration
    linkRing : AR.MultivariatePolynomialRing.polynomialRing decl ≡ expRing
    status : Core.Phase.Bool

mkMultivariatePolynomialRingAdapter :
  (R : AR.CommutativeRingDeclaration) →
  (n : M.Identifier) →
  (d : AR.MultivariatePolynomialRing R n) →
  (er : AR.CommutativeRingDeclaration) →
  (pr : AR.MultivariatePolynomialRing.polynomialRing d ≡ er) →
  MultivariatePolynomialRingAdapter
mkMultivariatePolynomialRingAdapter R n d er pr =
  record { R = R ; n = n ; decl = d ; expRing = er ; linkRing = pr ; status = true }

isFilledMultivariatePolynomialRing : MultivariatePolynomialRingAdapter → Core.Phase.Bool
isFilledMultivariatePolynomialRing a = MultivariatePolynomialRingAdapter.status a

-- Categorical view for MultivariatePolynomialRing
multivariatePolynomialRingCategorical : (adapt : MultivariatePolynomialRingAdapter) →
  CategoricalAdapter {lsuc lzero} (AR.MultivariatePolynomialRing (MultivariatePolynomialRingAdapter.R adapt) (MultivariatePolynomialRingAdapter.n adapt))
multivariatePolynomialRingCategorical adapt =
  mkCategoricalAdapter (AR.MultivariatePolynomialRing (MultivariatePolynomialRingAdapter.R adapt) (MultivariatePolynomialRingAdapter.n adapt))
    (λ _ → MultivariatePolynomialRingAdapter.decl adapt)

-- Content of polynomial
record ContentOfPolynomialAdapter : Set₁ where
  field
    R : AR.UFD
    f : M.Identifier
    decl : AR.ContentOfPolynomial R f
    expContent : M.Identifier
    linkContent : AR.ContentOfPolynomial.content decl ≡ expContent
    status : Core.Phase.Bool

mkContentOfPolynomialAdapter :
  (R : AR.UFD) →
  (f : M.Identifier) →
  (d : AR.ContentOfPolynomial R f) →
  (ec : M.Identifier) →
  (pc : AR.ContentOfPolynomial.content d ≡ ec) →
  ContentOfPolynomialAdapter
mkContentOfPolynomialAdapter R f d ec pc =
  record { R = R ; f = f ; decl = d ; expContent = ec ; linkContent = pc ; status = true }

isFilledContentOfPolynomial : ContentOfPolynomialAdapter → Core.Phase.Bool
isFilledContentOfPolynomial a = ContentOfPolynomialAdapter.status a

-- Categorical view for ContentOfPolynomial
contentOfPolynomialCategorical : (adapt : ContentOfPolynomialAdapter) →
  CategoricalAdapter {lsuc lzero} (AR.ContentOfPolynomial (ContentOfPolynomialAdapter.R adapt) (ContentOfPolynomialAdapter.f adapt))
contentOfPolynomialCategorical adapt =
  mkCategoricalAdapter (AR.ContentOfPolynomial (ContentOfPolynomialAdapter.R adapt) (ContentOfPolynomialAdapter.f adapt))
    (λ _ → ContentOfPolynomialAdapter.decl adapt)

-- Primitive polynomial
record PrimitivePolynomialAdapter : Set₁ where
  field
    R : AR.UFD
    f : M.Identifier
    decl : AR.PrimitivePolynomial R f
    expUFD : AR.UFD
    linkUFD : AR.PrimitivePolynomial.ufd decl ≡ expUFD
    status : Core.Phase.Bool

mkPrimitivePolynomialAdapter :
  (R : AR.UFD) →
  (f : M.Identifier) →
  (d : AR.PrimitivePolynomial R f) →
  (eu : AR.UFD) →
  (pu : AR.PrimitivePolynomial.ufd d ≡ eu) →
  PrimitivePolynomialAdapter
mkPrimitivePolynomialAdapter R f d eu pu =
  record { R = R ; f = f ; decl = d ; expUFD = eu ; linkUFD = pu ; status = true }

isFilledPrimitivePolynomial : PrimitivePolynomialAdapter → Core.Phase.Bool
isFilledPrimitivePolynomial a = PrimitivePolynomialAdapter.status a

-- Categorical view for PrimitivePolynomial
primitivePolynomialCategorical : (adapt : PrimitivePolynomialAdapter) →
  CategoricalAdapter {lsuc lzero} (AR.PrimitivePolynomial (PrimitivePolynomialAdapter.R adapt) (PrimitivePolynomialAdapter.f adapt))
primitivePolynomialCategorical adapt =
  mkCategoricalAdapter (AR.PrimitivePolynomial (PrimitivePolynomialAdapter.R adapt) (PrimitivePolynomialAdapter.f adapt))
    (λ _ → PrimitivePolynomialAdapter.decl adapt)

-- Prime spectrum
record PrimeSpectrumAdapter : Set₁ where
  field
    R : AR.CommutativeRingDeclaration
    decl : AR.PrimeSpectrum R
    expTopologicalSpace : M.Identifier
    linkTopologicalSpace : AR.PrimeSpectrum.topology decl ≡ expTopologicalSpace
    status : Core.Phase.Bool

mkPrimeSpectrumAdapter :
  (R : AR.CommutativeRingDeclaration) →
  (d : AR.PrimeSpectrum R) →
  (ets : M.Identifier) →
  (pts : AR.PrimeSpectrum.topology d ≡ ets) →
  PrimeSpectrumAdapter
mkPrimeSpectrumAdapter R d ets pts =
  record { R = R ; decl = d ; expTopologicalSpace = ets ; linkTopologicalSpace = pts ; status = true }

isFilledPrimeSpectrum : PrimeSpectrumAdapter → Core.Phase.Bool
isFilledPrimeSpectrum a = PrimeSpectrumAdapter.status a

-- Categorical view for PrimeSpectrum
primeSpectrumCategorical : (adapt : PrimeSpectrumAdapter) →
  CategoricalAdapter {lsuc lzero} (AR.PrimeSpectrum (PrimeSpectrumAdapter.R adapt))
primeSpectrumCategorical adapt =
  mkCategoricalAdapter (AR.PrimeSpectrum (PrimeSpectrumAdapter.R adapt))
    (λ _ → PrimeSpectrumAdapter.decl adapt)

-- Projective module
record ProjectiveModuleAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    P : AM.LeftModule R
    decl : AM.ProjectiveModule R P
    expRing : AR.RingDeclaration
    linkRing : AM.ProjectiveModule.ring decl ≡ expRing
    status : Core.Phase.Bool

mkProjectiveModuleAdapter :
  (R : AR.RingDeclaration) →
  (P : AM.LeftModule R) →
  (d : AM.ProjectiveModule R P) →
  (er : AR.RingDeclaration) →
  (pr : AM.ProjectiveModule.ring d ≡ er) →
  ProjectiveModuleAdapter
mkProjectiveModuleAdapter R P d er pr =
  record { R = R ; P = P ; decl = d ; expRing = er ; linkRing = pr ; status = true }

isFilledProjectiveModule : ProjectiveModuleAdapter → Core.Phase.Bool
isFilledProjectiveModule a = ProjectiveModuleAdapter.status a

-- Injective module
record InjectiveModuleAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    I : AM.LeftModule R
    decl : AM.InjectiveModule R I
    expRing : AR.RingDeclaration
    linkRing : AM.InjectiveModule.ring decl ≡ expRing
    status : Core.Phase.Bool

mkInjectiveModuleAdapter :
  (R : AR.RingDeclaration) →
  (I : AM.LeftModule R) →
  (d : AM.InjectiveModule R I) →
  (er : AR.RingDeclaration) →
  (pr : AM.InjectiveModule.ring d ≡ er) →
  InjectiveModuleAdapter
mkInjectiveModuleAdapter R I d er pr =
  record { R = R ; I = I ; decl = d ; expRing = er ; linkRing = pr ; status = true }

isFilledInjectiveModule : InjectiveModuleAdapter → Core.Phase.Bool
isFilledInjectiveModule a = InjectiveModuleAdapter.status a

-- Torsion element
record TorsionElementAdapter : Set₁ where
  field
    R : AR.IntegralDomain
    M : AM.LeftModule (AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing (AR.IntegralDomain.underlyingRing R)))
    m : M.Identifier
    decl : AM.TorsionElement R M m
    expDomain : AR.IntegralDomain
    linkDomain : AM.TorsionElement.domain decl ≡ expDomain
    status : Core.Phase.Bool

mkTorsionElementAdapter :
  (R : AR.IntegralDomain) →
  (M : AM.LeftModule (AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing (AR.IntegralDomain.underlyingRing R)))) →
  (m : M.Identifier) →
  (d : AM.TorsionElement R M m) →
  (ed : AR.IntegralDomain) →
  (pd : AM.TorsionElement.domain d ≡ ed) →
  TorsionElementAdapter
mkTorsionElementAdapter R M m d ed pd =
  record { R = R ; M = M ; m = m ; decl = d ; expDomain = ed ; linkDomain = pd ; status = true }

isFilledTorsionElement : TorsionElementAdapter → Core.Phase.Bool
isFilledTorsionElement a = TorsionElementAdapter.status a

-- Torsion submodule
record TorsionSubmoduleAdapter : Set₁ where
  field
    R : AR.IntegralDomain
    M : AM.LeftModule (AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing (AR.IntegralDomain.underlyingRing R)))
    decl : AM.TorsionSubmodule R M
    expDomain : AR.IntegralDomain
    linkDomain : AM.TorsionSubmodule.domain decl ≡ expDomain
    status : Core.Phase.Bool

mkTorsionSubmoduleAdapter :
  (R : AR.IntegralDomain) →
  (M : AM.LeftModule (AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing (AR.IntegralDomain.underlyingRing R)))) →
  (d : AM.TorsionSubmodule R M) →
  (ed : AR.IntegralDomain) →
  (pd : AM.TorsionSubmodule.domain d ≡ ed) →
  TorsionSubmoduleAdapter
mkTorsionSubmoduleAdapter R M d ed pd =
  record { R = R ; M = M ; decl = d ; expDomain = ed ; linkDomain = pd ; status = true }

isFilledTorsionSubmodule : TorsionSubmoduleAdapter → Core.Phase.Bool
isFilledTorsionSubmodule a = TorsionSubmoduleAdapter.status a

-- Torsion-free module
record TorsionFreeModuleAdapter : Set₁ where
  field
    R : AR.IntegralDomain
    M : AM.LeftModule (AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing (AR.IntegralDomain.underlyingRing R)))
    decl : AM.TorsionFreeModule R M
    expDomain : AR.IntegralDomain
    linkDomain : AM.TorsionFreeModule.domain decl ≡ expDomain
    status : Core.Phase.Bool

mkTorsionFreeModuleAdapter :
  (R : AR.IntegralDomain) →
  (M : AM.LeftModule (AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing (AR.IntegralDomain.underlyingRing R)))) →
  (d : AM.TorsionFreeModule R M) →
  (ed : AR.IntegralDomain) →
  (pd : AM.TorsionFreeModule.domain d ≡ ed) →
  TorsionFreeModuleAdapter
mkTorsionFreeModuleAdapter R M d ed pd =
  record { R = R ; M = M ; decl = d ; expDomain = ed ; linkDomain = pd ; status = true }

isFilledTorsionFreeModule : TorsionFreeModuleAdapter → Core.Phase.Bool
isFilledTorsionFreeModule a = TorsionFreeModuleAdapter.status a

-- Structure theorem for finitely generated modules over PID
record StructureTheoremPIDAdapter : Set₁ where
  field
    R : AR.PrincipalIdealDomain
    M : AM.LeftModule (AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing (AR.IntegralDomain.underlyingRing (AR.PrincipalIdealDomain.domain R))))
    decl : AM.StructureTheoremPID R M
    expPID : AR.PrincipalIdealDomain
    linkPID : AM.StructureTheoremPID.pid decl ≡ expPID
    status : Core.Phase.Bool

mkStructureTheoremPIDAdapter :
  (R : AR.PrincipalIdealDomain) →
  (M : AM.LeftModule (AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing (AR.IntegralDomain.underlyingRing (AR.PrincipalIdealDomain.domain R))))) →
  (d : AM.StructureTheoremPID R M) →
  (ep : AR.PrincipalIdealDomain) →
  (pp : AM.StructureTheoremPID.pid d ≡ ep) →
  StructureTheoremPIDAdapter
mkStructureTheoremPIDAdapter R M d ep pp =
  record { R = R ; M = M ; decl = d ; expPID = ep ; linkPID = pp ; status = true }

isFilledStructureTheoremPID : StructureTheoremPIDAdapter → Core.Phase.Bool
isFilledStructureTheoremPID a = StructureTheoremPIDAdapter.status a

projectiveModuleCategorical : (adapt : ProjectiveModuleAdapter) →
  CategoricalAdapter {lsuc lzero} (AM.ProjectiveModule (ProjectiveModuleAdapter.R adapt) (ProjectiveModuleAdapter.P adapt))
projectiveModuleCategorical adapt = 
  mkCategoricalAdapter (AM.ProjectiveModule (ProjectiveModuleAdapter.R adapt) (ProjectiveModuleAdapter.P adapt))
    (λ _ → ProjectiveModuleAdapter.decl adapt)

injectiveModuleCategorical : (adapt : InjectiveModuleAdapter) →
  CategoricalAdapter {lsuc lzero} (AM.InjectiveModule (InjectiveModuleAdapter.R adapt) (InjectiveModuleAdapter.I adapt))
injectiveModuleCategorical adapt = 
  mkCategoricalAdapter (AM.InjectiveModule (InjectiveModuleAdapter.R adapt) (InjectiveModuleAdapter.I adapt))
    (λ _ → InjectiveModuleAdapter.decl adapt)

torsionElementCategorical : (adapt : TorsionElementAdapter) →
  CategoricalAdapter {lsuc lzero} (AM.TorsionElement (TorsionElementAdapter.R adapt) (TorsionElementAdapter.M adapt) (TorsionElementAdapter.m adapt))
torsionElementCategorical adapt = 
  mkCategoricalAdapter (AM.TorsionElement (TorsionElementAdapter.R adapt) (TorsionElementAdapter.M adapt) (TorsionElementAdapter.m adapt))
    (λ _ → TorsionElementAdapter.decl adapt)

torsionSubmoduleCategorical : (adapt : TorsionSubmoduleAdapter) →
  CategoricalAdapter {lsuc lzero} (AM.TorsionSubmodule (TorsionSubmoduleAdapter.R adapt) (TorsionSubmoduleAdapter.M adapt))
torsionSubmoduleCategorical adapt = 
  mkCategoricalAdapter (AM.TorsionSubmodule (TorsionSubmoduleAdapter.R adapt) (TorsionSubmoduleAdapter.M adapt))
    (λ _ → TorsionSubmoduleAdapter.decl adapt)

torsionFreeModuleCategorical : (adapt : TorsionFreeModuleAdapter) →
  CategoricalAdapter {lsuc lzero} (AM.TorsionFreeModule (TorsionFreeModuleAdapter.R adapt) (TorsionFreeModuleAdapter.M adapt))
torsionFreeModuleCategorical adapt = 
  mkCategoricalAdapter (AM.TorsionFreeModule (TorsionFreeModuleAdapter.R adapt) (TorsionFreeModuleAdapter.M adapt))
    (λ _ → TorsionFreeModuleAdapter.decl adapt)

structureTheoremPIDCategorical : (adapt : StructureTheoremPIDAdapter) →
  CategoricalAdapter {lsuc lzero} (AM.StructureTheoremPID (StructureTheoremPIDAdapter.R adapt) (StructureTheoremPIDAdapter.M adapt))
structureTheoremPIDCategorical adapt = 
  mkCategoricalAdapter (AM.StructureTheoremPID (StructureTheoremPIDAdapter.R adapt) (StructureTheoremPIDAdapter.M adapt))
    (λ _ → StructureTheoremPIDAdapter.decl adapt)

-- Hom functor
record HomFunctorAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    M : AM.LeftModule R
    decl : AM.HomFunctor R M
    expRing : AR.RingDeclaration
    linkRing : AM.HomFunctor.ring decl ≡ expRing
    status : Core.Phase.Bool

mkHomFunctorAdapter :
  (R : AR.RingDeclaration) →
  (M : AM.LeftModule R) →
  (d : AM.HomFunctor R M) →
  (er : AR.RingDeclaration) →
  (pr : AM.HomFunctor.ring d ≡ er) →
  HomFunctorAdapter
mkHomFunctorAdapter R M d er pr =
  record { R = R ; M = M ; decl = d ; expRing = er ; linkRing = pr ; status = true }

isFilledHomFunctor : HomFunctorAdapter → Core.Phase.Bool
isFilledHomFunctor a = HomFunctorAdapter.status a

-- Dual module
record DualModuleAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    M : AM.LeftModule R
    decl : AM.DualModule R M
    expRing : AR.RingDeclaration
    linkRing : AM.DualModule.ring decl ≡ expRing
    status : Core.Phase.Bool

mkDualModuleAdapter :
  (R : AR.RingDeclaration) →
  (M : AM.LeftModule R) →
  (d : AM.DualModule R M) →
  (er : AR.RingDeclaration) →
  (pr : AM.DualModule.ring d ≡ er) →
  DualModuleAdapter
mkDualModuleAdapter R M d er pr =
  record { R = R ; M = M ; decl = d ; expRing = er ; linkRing = pr ; status = true }

isFilledDualModule : DualModuleAdapter → Core.Phase.Bool
isFilledDualModule a = DualModuleAdapter.status a

-- Reflexive module
record ReflexiveModuleAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    M : AM.LeftModule R
    decl : AM.ReflexiveModule R M
    expRing : AR.RingDeclaration
    linkRing : AM.ReflexiveModule.ring decl ≡ expRing
    status : Core.Phase.Bool

mkReflexiveModuleAdapter :
  (R : AR.RingDeclaration) →
  (M : AM.LeftModule R) →
  (d : AM.ReflexiveModule R M) →
  (er : AR.RingDeclaration) →
  (pr : AM.ReflexiveModule.ring d ≡ er) →
  ReflexiveModuleAdapter
mkReflexiveModuleAdapter R M d er pr =
  record { R = R ; M = M ; decl = d ; expRing = er ; linkRing = pr ; status = true }

isFilledReflexiveModule : ReflexiveModuleAdapter → Core.Phase.Bool
isFilledReflexiveModule a = ReflexiveModuleAdapter.status a

-- Tensor product of modules
-- Redesigned: Simplified to avoid dependent type issues - extract expected values from decl fields
record TensorProductModuleAdapter : Set₁ where
  field
    R : AR.CommutativeRingDeclaration
    M : AM.LeftModule (AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing R))
    N : AM.LeftModule (AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing R))
    decl : AM.TensorProduct R M N
    status : Core.Phase.Bool

mkTensorProductModuleAdapter :
  (R : AR.CommutativeRingDeclaration) →
  (M N : AM.LeftModule (AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing R))) →
  (d : AM.TensorProduct R M N) →
  TensorProductModuleAdapter
mkTensorProductModuleAdapter R M N d =
  record { R = R ; M = M ; N = N ; decl = d ; status = true }

isFilledTensorProductModule : TensorProductModuleAdapter → Core.Phase.Bool
isFilledTensorProductModule a = TensorProductModuleAdapter.status a

-- Free module
record FreeModuleAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    X : M.Identifier
    decl : AM.FreeModule R X
    expRing : AR.RingDeclaration
    linkRing : AM.FreeModule.ring decl ≡ expRing
    status : Core.Phase.Bool

mkFreeModuleAdapter :
  (R : AR.RingDeclaration) →
  (X : M.Identifier) →
  (d : AM.FreeModule R X) →
  (er : AR.RingDeclaration) →
  (pr : AM.FreeModule.ring d ≡ er) →
  FreeModuleAdapter
mkFreeModuleAdapter R X d er pr =
  record { R = R ; X = X ; decl = d ; expRing = er ; linkRing = pr ; status = true }

isFilledFreeModule : FreeModuleAdapter → Core.Phase.Bool
isFilledFreeModule a = FreeModuleAdapter.status a

-- Free module functor
record FreeModuleFunctorAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    decl : AM.FreeModuleFunctor R
    expRing : AR.RingDeclaration
    linkRing : AM.FreeModuleFunctor.ring decl ≡ expRing
    status : Core.Phase.Bool

mkFreeModuleFunctorAdapter :
  (R : AR.RingDeclaration) →
  (d : AM.FreeModuleFunctor R) →
  (er : AR.RingDeclaration) →
  (pr : AM.FreeModuleFunctor.ring d ≡ er) →
  FreeModuleFunctorAdapter
mkFreeModuleFunctorAdapter R d er pr =
  record { R = R ; decl = d ; expRing = er ; linkRing = pr ; status = true }

isFilledFreeModuleFunctor : FreeModuleFunctorAdapter → Core.Phase.Bool
isFilledFreeModuleFunctor a = FreeModuleFunctorAdapter.status a

-- Forgetful module functor
record ForgetfulModuleFunctorAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    decl : AM.ForgetfulModuleFunctor R
    expRing : AR.RingDeclaration
    linkRing : AM.ForgetfulModuleFunctor.ring decl ≡ expRing
    status : Core.Phase.Bool

mkForgetfulModuleFunctorAdapter :
  (R : AR.RingDeclaration) →
  (d : AM.ForgetfulModuleFunctor R) →
  (er : AR.RingDeclaration) →
  (pr : AM.ForgetfulModuleFunctor.ring d ≡ er) →
  ForgetfulModuleFunctorAdapter
mkForgetfulModuleFunctorAdapter R d er pr =
  record { R = R ; decl = d ; expRing = er ; linkRing = pr ; status = true }

isFilledForgetfulModuleFunctor : ForgetfulModuleFunctorAdapter → Core.Phase.Bool
isFilledForgetfulModuleFunctor a = ForgetfulModuleFunctorAdapter.status a

-- Right module
record RightModuleAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    decl : AM.RightModule R
    expRing : AR.RingDeclaration
    linkRing : AM.RightModule.ring decl ≡ expRing
    status : Core.Phase.Bool

mkRightModuleAdapter :
  (R : AR.RingDeclaration) →
  (d : AM.RightModule R) →
  (er : AR.RingDeclaration) →
  (pr : AM.RightModule.ring d ≡ er) →
  RightModuleAdapter
mkRightModuleAdapter R d er pr =
  record { R = R ; decl = d ; expRing = er ; linkRing = pr ; status = true }

isFilledRightModule : RightModuleAdapter → Core.Phase.Bool
isFilledRightModule a = RightModuleAdapter.status a

homFunctorCategorical : (adapt : HomFunctorAdapter) →
  CategoricalAdapter {lsuc lzero} (AM.HomFunctor (HomFunctorAdapter.R adapt) (HomFunctorAdapter.M adapt))
homFunctorCategorical adapt = 
  mkCategoricalAdapter (AM.HomFunctor (HomFunctorAdapter.R adapt) (HomFunctorAdapter.M adapt))
    (λ _ → HomFunctorAdapter.decl adapt)

dualModuleCategorical : (adapt : DualModuleAdapter) →
  CategoricalAdapter {lsuc lzero} (AM.DualModule (DualModuleAdapter.R adapt) (DualModuleAdapter.M adapt))
dualModuleCategorical adapt = 
  mkCategoricalAdapter (AM.DualModule (DualModuleAdapter.R adapt) (DualModuleAdapter.M adapt))
    (λ _ → DualModuleAdapter.decl adapt)

reflexiveModuleCategorical : (adapt : ReflexiveModuleAdapter) →
  CategoricalAdapter {lsuc lzero} (AM.ReflexiveModule (ReflexiveModuleAdapter.R adapt) (ReflexiveModuleAdapter.M adapt))
reflexiveModuleCategorical adapt = 
  mkCategoricalAdapter (AM.ReflexiveModule (ReflexiveModuleAdapter.R adapt) (ReflexiveModuleAdapter.M adapt))
    (λ _ → ReflexiveModuleAdapter.decl adapt)

tensorProductModuleCategorical : (adapt : TensorProductModuleAdapter) →
  CategoricalAdapter {lsuc lzero} (AM.TensorProduct (TensorProductModuleAdapter.R adapt) (TensorProductModuleAdapter.M adapt) (TensorProductModuleAdapter.N adapt))
tensorProductModuleCategorical adapt = 
  mkCategoricalAdapter (AM.TensorProduct (TensorProductModuleAdapter.R adapt) (TensorProductModuleAdapter.M adapt) (TensorProductModuleAdapter.N adapt))
    (λ _ → TensorProductModuleAdapter.decl adapt)

freeModuleCategorical : (adapt : FreeModuleAdapter) →
  CategoricalAdapter {lsuc lzero} (AM.FreeModule (FreeModuleAdapter.R adapt) (FreeModuleAdapter.X adapt))
freeModuleCategorical adapt = 
  mkCategoricalAdapter (AM.FreeModule (FreeModuleAdapter.R adapt) (FreeModuleAdapter.X adapt))
    (λ _ → FreeModuleAdapter.decl adapt)

freeModuleFunctorCategorical : (adapt : FreeModuleFunctorAdapter) →
  CategoricalAdapter {lsuc lzero} (AM.FreeModuleFunctor (FreeModuleFunctorAdapter.R adapt))
freeModuleFunctorCategorical adapt = 
  mkCategoricalAdapter (AM.FreeModuleFunctor (FreeModuleFunctorAdapter.R adapt))
    (λ _ → FreeModuleFunctorAdapter.decl adapt)

forgetfulModuleFunctorCategorical : (adapt : ForgetfulModuleFunctorAdapter) →
  CategoricalAdapter {lsuc lzero} (AM.ForgetfulModuleFunctor (ForgetfulModuleFunctorAdapter.R adapt))
forgetfulModuleFunctorCategorical adapt = 
  mkCategoricalAdapter (AM.ForgetfulModuleFunctor (ForgetfulModuleFunctorAdapter.R adapt))
    (λ _ → ForgetfulModuleFunctorAdapter.decl adapt)

rightModuleCategorical : (adapt : RightModuleAdapter) →
  CategoricalAdapter {lsuc lzero} (AM.RightModule (RightModuleAdapter.R adapt))
rightModuleCategorical adapt = 
  mkCategoricalAdapter (AM.RightModule (RightModuleAdapter.R adapt))
    (λ _ → RightModuleAdapter.decl adapt)

-- ============================================================================
-- Extension Degree and Polynomial-Related Adapters
-- ============================================================================

-- Extension degree [E : F]
record ExtensionDegreeAdapter : Set₁ where
  field
    F E : AR.FieldDeclaration
    decl : AFB.ExtensionDegree F E
    expected : AR.FieldDeclaration
    expExt : AR.FieldDeclaration
    link : AFB.ExtensionDegree.baseField decl ≡ expected
    linkExt : AFB.ExtensionDegree.extensionField decl ≡ expExt
    status : Core.Phase.Bool

mkExtensionDegreeAdapter :
  (F E : AR.FieldDeclaration) →
  (d : AFB.ExtensionDegree F E) →
  (ef : AR.FieldDeclaration) →
  (ee : AR.FieldDeclaration) →
  (pf : AFB.ExtensionDegree.baseField d ≡ ef) →
  (pe : AFB.ExtensionDegree.extensionField d ≡ ee) →
  ExtensionDegreeAdapter
mkExtensionDegreeAdapter F E d ef ee pf pe =
  record { F = F ; E = E ; decl = d ; expected = ef ; expExt = ee ; link = pf ; linkExt = pe ; status = true }

isFilledExtensionDegree : ExtensionDegreeAdapter → Core.Phase.Bool
isFilledExtensionDegree a = ExtensionDegreeAdapter.status a

-- Inseparable degree [E : F]ᵢ
record InseparableDegreeAdapter : Set₁ where
  field
    F E : AR.FieldDeclaration
    decl : AFA.InseparableDegree F E
    expected : AR.FieldDeclaration
    expExt : AR.FieldDeclaration
    link : AFA.InseparableDegree.baseField decl ≡ expected
    linkExt : AFA.InseparableDegree.extensionField decl ≡ expExt
    status : Core.Phase.Bool

mkInseparableDegreeAdapter :
  (F E : AR.FieldDeclaration) →
  (d : AFA.InseparableDegree F E) →
  (ef : AR.FieldDeclaration) →
  (ee : AR.FieldDeclaration) →
  (pf : AFA.InseparableDegree.baseField d ≡ ef) →
  (pe : AFA.InseparableDegree.extensionField d ≡ ee) →
  InseparableDegreeAdapter
mkInseparableDegreeAdapter F E d ef ee pf pe =
  record { F = F ; E = E ; decl = d ; expected = ef ; expExt = ee ; link = pf ; linkExt = pe ; status = true }

isFilledInseparableDegree : InseparableDegreeAdapter → Core.Phase.Bool
isFilledInseparableDegree a = InseparableDegreeAdapter.status a

-- Separable degree [E : F]ₛ
record SeparableDegreeAdapter : Set₁ where
  field
    F E : AR.FieldDeclaration
    decl : AFA.SeparableDegree F E
    expected : AR.FieldDeclaration
    expExt : AR.FieldDeclaration
    link : AFA.SeparableDegree.baseField decl ≡ expected
    linkExt : AFA.SeparableDegree.extensionField decl ≡ expExt
    status : Core.Phase.Bool

mkSeparableDegreeAdapter :
  (F E : AR.FieldDeclaration) →
  (d : AFA.SeparableDegree F E) →
  (ef : AR.FieldDeclaration) →
  (ee : AR.FieldDeclaration) →
  (pf : AFA.SeparableDegree.baseField d ≡ ef) →
  (pe : AFA.SeparableDegree.extensionField d ≡ ee) →
  SeparableDegreeAdapter
mkSeparableDegreeAdapter F E d ef ee pf pe =
  record { F = F ; E = E ; decl = d ; expected = ef ; expExt = ee ; link = pf ; linkExt = pe ; status = true }

isFilledSeparableDegree : SeparableDegreeAdapter → Core.Phase.Bool
isFilledSeparableDegree a = SeparableDegreeAdapter.status a

-- Simple extension F(α)
record SimpleExtensionAdapter : Set₁ where
  field
    F E : AR.FieldDeclaration
    α : M.Identifier
    decl : AFB.SimpleExtension F E α
    expected : AR.FieldDeclaration
    expExt : AR.FieldDeclaration
    link : AFB.SimpleExtension.baseField decl ≡ expected
    linkExt : AFB.SimpleExtension.extensionField decl ≡ expExt
    status : Core.Phase.Bool

mkSimpleExtensionAdapter :
  (F E : AR.FieldDeclaration) →
  (α : M.Identifier) →
  (d : AFB.SimpleExtension F E α) →
  (ef : AR.FieldDeclaration) →
  (ee : AR.FieldDeclaration) →
  (pf : AFB.SimpleExtension.baseField d ≡ ef) →
  (pe : AFB.SimpleExtension.extensionField d ≡ ee) →
  SimpleExtensionAdapter
mkSimpleExtensionAdapter F E α d ef ee pf pe =
  record { F = F ; E = E ; α = α ; decl = d ; expected = ef ; expExt = ee ; link = pf ; linkExt = pe ; status = true }

isFilledSimpleExtension : SimpleExtensionAdapter → Core.Phase.Bool
isFilledSimpleExtension a = SimpleExtensionAdapter.status a

-- Transcendental element
record TranscendentalElementAdapter : Set₁ where
  field
    F E : AR.FieldDeclaration
    α : M.Identifier
    decl : AFB.TranscendentalElement F E α
    expected : AR.FieldDeclaration
    expExt : AR.FieldDeclaration
    link : AFB.TranscendentalElement.baseField decl ≡ expected
    linkExt : AFB.TranscendentalElement.extensionField decl ≡ expExt
    status : Core.Phase.Bool

mkTranscendentalElementAdapter :
  (F E : AR.FieldDeclaration) →
  (α : M.Identifier) →
  (d : AFB.TranscendentalElement F E α) →
  (ef : AR.FieldDeclaration) →
  (ee : AR.FieldDeclaration) →
  (pf : AFB.TranscendentalElement.baseField d ≡ ef) →
  (pe : AFB.TranscendentalElement.extensionField d ≡ ee) →
  TranscendentalElementAdapter
mkTranscendentalElementAdapter F E α d ef ee pf pe =
  record { F = F ; E = E ; α = α ; decl = d ; expected = ef ; expExt = ee ; link = pf ; linkExt = pe ; status = true }

isFilledTranscendentalElement : TranscendentalElementAdapter → Core.Phase.Bool
isFilledTranscendentalElement a = TranscendentalElementAdapter.status a

-- Transcendence basis
record TranscendenceBasisAdapter : Set₁ where
  field
    F E : AR.FieldDeclaration
    decl : AFB.TranscendenceBasis F E
    expected : AR.FieldDeclaration
    expExt : AR.FieldDeclaration
    link : AFB.TranscendenceBasis.baseField decl ≡ expected
    linkExt : AFB.TranscendenceBasis.extensionField decl ≡ expExt
    status : Core.Phase.Bool

mkTranscendenceBasisAdapter :
  (F E : AR.FieldDeclaration) →
  (d : AFB.TranscendenceBasis F E) →
  (ef : AR.FieldDeclaration) →
  (ee : AR.FieldDeclaration) →
  (pf : AFB.TranscendenceBasis.baseField d ≡ ef) →
  (pe : AFB.TranscendenceBasis.extensionField d ≡ ee) →
  TranscendenceBasisAdapter
mkTranscendenceBasisAdapter F E d ef ee pf pe =
  record { F = F ; E = E ; decl = d ; expected = ef ; expExt = ee ; link = pf ; linkExt = pe ; status = true }

isFilledTranscendenceBasis : TranscendenceBasisAdapter → Core.Phase.Bool
isFilledTranscendenceBasis a = TranscendenceBasisAdapter.status a

extensionDegreeCategorical : (adapt : ExtensionDegreeAdapter) →
  CategoricalAdapter {lsuc lzero} (AFB.ExtensionDegree (ExtensionDegreeAdapter.F adapt) (ExtensionDegreeAdapter.E adapt))
extensionDegreeCategorical adapt = 
  mkCategoricalAdapter (AFB.ExtensionDegree (ExtensionDegreeAdapter.F adapt) (ExtensionDegreeAdapter.E adapt))
    (λ _ → ExtensionDegreeAdapter.decl adapt)

inseparableDegreeCategorical : (adapt : InseparableDegreeAdapter) →
  CategoricalAdapter {lsuc lzero} (AFA.InseparableDegree (InseparableDegreeAdapter.F adapt) (InseparableDegreeAdapter.E adapt))
inseparableDegreeCategorical adapt = 
  mkCategoricalAdapter (AFA.InseparableDegree (InseparableDegreeAdapter.F adapt) (InseparableDegreeAdapter.E adapt))
    (λ _ → InseparableDegreeAdapter.decl adapt)

separableDegreeCategorical : (adapt : SeparableDegreeAdapter) →
  CategoricalAdapter {lsuc lzero} (AFA.SeparableDegree (SeparableDegreeAdapter.F adapt) (SeparableDegreeAdapter.E adapt))
separableDegreeCategorical adapt = 
  mkCategoricalAdapter (AFA.SeparableDegree (SeparableDegreeAdapter.F adapt) (SeparableDegreeAdapter.E adapt))
    (λ _ → SeparableDegreeAdapter.decl adapt)

simpleExtensionCategorical : (adapt : SimpleExtensionAdapter) →
  CategoricalAdapter {lsuc lzero} (AFB.SimpleExtension (SimpleExtensionAdapter.F adapt) (SimpleExtensionAdapter.E adapt) (SimpleExtensionAdapter.α adapt))
simpleExtensionCategorical adapt = 
  mkCategoricalAdapter (AFB.SimpleExtension (SimpleExtensionAdapter.F adapt) (SimpleExtensionAdapter.E adapt) (SimpleExtensionAdapter.α adapt))
    (λ _ → SimpleExtensionAdapter.decl adapt)

transcendentalElementCategorical : (adapt : TranscendentalElementAdapter) →
  CategoricalAdapter {lsuc lzero} (AFB.TranscendentalElement (TranscendentalElementAdapter.F adapt) (TranscendentalElementAdapter.E adapt) (TranscendentalElementAdapter.α adapt))
transcendentalElementCategorical adapt = 
  mkCategoricalAdapter (AFB.TranscendentalElement (TranscendentalElementAdapter.F adapt) (TranscendentalElementAdapter.E adapt) (TranscendentalElementAdapter.α adapt))
    (λ _ → TranscendentalElementAdapter.decl adapt)

transcendenceBasisCategorical : (adapt : TranscendenceBasisAdapter) →
  CategoricalAdapter {lsuc lzero} (AFB.TranscendenceBasis (TranscendenceBasisAdapter.F adapt) (TranscendenceBasisAdapter.E adapt))
transcendenceBasisCategorical adapt = 
  mkCategoricalAdapter (AFB.TranscendenceBasis (TranscendenceBasisAdapter.F adapt) (TranscendenceBasisAdapter.E adapt))
    (λ _ → TranscendenceBasisAdapter.decl adapt)

-- ============================================================================
-- Enrichment-Specific Adapters
-- ============================================================================

-- Monoid as monoidal category
record MonoidAsMonoidalCategoryAdapter : Set₁ where
  field
    decl : AE.MonoidAsMonoidalCategory
    expectedMonoid : AFo.MonoidDeclaration
    link : AE.MonoidAsMonoidalCategory.monoid decl ≡ expectedMonoid
    status : Core.Phase.Bool

mkMonoidAsMonoidalCategoryAdapter :
  (d : AE.MonoidAsMonoidalCategory) →
  (em : AFo.MonoidDeclaration) →
  (p : AE.MonoidAsMonoidalCategory.monoid d ≡ em) →
  MonoidAsMonoidalCategoryAdapter
mkMonoidAsMonoidalCategoryAdapter d em p =
  record { decl = d ; expectedMonoid = em ; link = p ; status = true }

isFilledMonoidAsMonoidalCategory : MonoidAsMonoidalCategoryAdapter → Core.Phase.Bool
isFilledMonoidAsMonoidalCategory a = MonoidAsMonoidalCategoryAdapter.status a

-- Abelian group as symmetric monoidal category
record AbelianGroupAsSymmetricMonoidalAdapter : Set₁ where
  field
    decl : AE.AbelianGroupAsSymmetricMonoidal
    expectedAbGroup : AFo.AbelianGroupDeclaration
    link : AE.AbelianGroupAsSymmetricMonoidal.abelianGroup decl ≡ expectedAbGroup
    status : Core.Phase.Bool

mkAbelianGroupAsSymmetricMonoidalAdapter :
  (d : AE.AbelianGroupAsSymmetricMonoidal) →
  (eab : AFo.AbelianGroupDeclaration) →
  (p : AE.AbelianGroupAsSymmetricMonoidal.abelianGroup d ≡ eab) →
  AbelianGroupAsSymmetricMonoidalAdapter
mkAbelianGroupAsSymmetricMonoidalAdapter d eab p =
  record { decl = d ; expectedAbGroup = eab ; link = p ; status = true }

isFilledAbelianGroupAsSymmetricMonoidal : AbelianGroupAsSymmetricMonoidalAdapter → Core.Phase.Bool
isFilledAbelianGroupAsSymmetricMonoidal a = AbelianGroupAsSymmetricMonoidalAdapter.status a

-- Monoid-enriched category
record MonoidEnrichedCategoryAdapter : Set₁ where
  field
    decl : AE.MonoidEnrichedCategory
    expectedMonoid : AFo.MonoidDeclaration
    link : AE.MonoidEnrichedCategory.enrichingMonoid decl ≡ expectedMonoid
    status : Core.Phase.Bool

mkMonoidEnrichedCategoryAdapter :
  (d : AE.MonoidEnrichedCategory) →
  (em : AFo.MonoidDeclaration) →
  (p : AE.MonoidEnrichedCategory.enrichingMonoid d ≡ em) →
  MonoidEnrichedCategoryAdapter
mkMonoidEnrichedCategoryAdapter d em p =
  record { decl = d ; expectedMonoid = em ; link = p ; status = true }

isFilledMonoidEnrichedCategory : MonoidEnrichedCategoryAdapter → Core.Phase.Bool
isFilledMonoidEnrichedCategory a = MonoidEnrichedCategoryAdapter.status a

-- Distance category (enriched over ℕ)
record DistanceCategoryAdapter : Set₁ where
  field
    decl : AE.DistanceCategory
    expectedMonoid : AFo.MonoidDeclaration
    link : AE.DistanceCategory.naturalNumbersMonoid decl ≡ expectedMonoid
    status : Core.Phase.Bool

mkDistanceCategoryAdapter :
  (d : AE.DistanceCategory) →
  (em : AFo.MonoidDeclaration) →
  (p : AE.DistanceCategory.naturalNumbersMonoid d ≡ em) →
  DistanceCategoryAdapter
mkDistanceCategoryAdapter d em p =
  record { decl = d ; expectedMonoid = em ; link = p ; status = true }

isFilledDistanceCategory : DistanceCategoryAdapter → Core.Phase.Bool
isFilledDistanceCategory a = DistanceCategoryAdapter.status a

-- Ab-enriched category (additive category)
record AbEnrichedCategoryAdapter : Set₁ where
  field
    decl : AE.AbEnrichedCategory
    expectedCat : AFo.CategoryOfAbelianGroups
    link : AE.AbEnrichedCategory.enrichingCategory decl ≡ expectedCat
    status : Core.Phase.Bool

mkAbEnrichedCategoryAdapter :
  (d : AE.AbEnrichedCategory) →
  (ec : AFo.CategoryOfAbelianGroups) →
  (p : AE.AbEnrichedCategory.enrichingCategory d ≡ ec) →
  AbEnrichedCategoryAdapter
mkAbEnrichedCategoryAdapter d ec p =
  record { decl = d ; expectedCat = ec ; link = p ; status = true }

isFilledAbEnrichedCategory : AbEnrichedCategoryAdapter → Core.Phase.Bool
isFilledAbEnrichedCategory a = AbEnrichedCategoryAdapter.status a

-- Generic enrichment over monoidal category V
record GenericEnrichmentAdapter : Set₁ where
  field
    V : Enriched.MonoidalCategoryDeclaration
    decl : AE.GenericEnrichment V
    expectedCat : C1S3.CategoryDeclaration
    link : AE.GenericEnrichment.enrichingCategory decl ≡ expectedCat
    status : Core.Phase.Bool

mkGenericEnrichmentAdapter :
  (V : Enriched.MonoidalCategoryDeclaration) →
  (d : AE.GenericEnrichment V) →
  (ec : C1S3.CategoryDeclaration) →
  (p : AE.GenericEnrichment.enrichingCategory d ≡ ec) →
  GenericEnrichmentAdapter
mkGenericEnrichmentAdapter V d ec p =
  record { V = V ; decl = d ; expectedCat = ec ; link = p ; status = true }

isFilledGenericEnrichment : GenericEnrichmentAdapter → Core.Phase.Bool
isFilledGenericEnrichment a = GenericEnrichmentAdapter.status a

-- Group action enriched category
record GroupActionEnrichedCategoryAdapter : Set₁ where
  field
    decl : AE.GroupActionEnrichedCategory
    expectedGroup : AFo.GroupDeclaration
    link : AE.GroupActionEnrichedCategory.actingGroup decl ≡ expectedGroup
    status : Core.Phase.Bool

mkGroupActionEnrichedCategoryAdapter :
  (d : AE.GroupActionEnrichedCategory) →
  (eg : AFo.GroupDeclaration) →
  (p : AE.GroupActionEnrichedCategory.actingGroup d ≡ eg) →
  GroupActionEnrichedCategoryAdapter
mkGroupActionEnrichedCategoryAdapter d eg p =
  record { decl = d ; expectedGroup = eg ; link = p ; status = true }

isFilledGroupActionEnrichedCategory : GroupActionEnrichedCategoryAdapter → Core.Phase.Bool
isFilledGroupActionEnrichedCategory a = GroupActionEnrichedCategoryAdapter.status a

-- Module-enriched category (over a ring)
record ModuleEnrichedCategoryAdapter : Set₁ where
  field
    decl : AE.ModuleEnrichedCategory
    status : Core.Phase.Bool

mkModuleEnrichedCategoryAdapter :
  (d : AE.ModuleEnrichedCategory) →
  ModuleEnrichedCategoryAdapter
mkModuleEnrichedCategoryAdapter d =
  record { decl = d ; status = true }

isFilledModuleEnrichedCategory : ModuleEnrichedCategoryAdapter → Core.Phase.Bool
isFilledModuleEnrichedCategory a = ModuleEnrichedCategoryAdapter.status a

-- Lawvere theory enriched category
record LawvereTheoryEnrichedCategoryAdapter : Set₁ where
  field
    decl : AE.LawvereTheoryEnrichedCategory
    status : Core.Phase.Bool

mkLawvereTheoryEnrichedCategoryAdapter :
  (d : AE.LawvereTheoryEnrichedCategory) →
  LawvereTheoryEnrichedCategoryAdapter
mkLawvereTheoryEnrichedCategoryAdapter d =
  record { decl = d ; status = true }

isFilledLawvereTheoryEnrichedCategory : LawvereTheoryEnrichedCategoryAdapter → Core.Phase.Bool
isFilledLawvereTheoryEnrichedCategory a = LawvereTheoryEnrichedCategoryAdapter.status a

-- Ab self-enriched
record AbSelfEnrichedAdapter : Set₁ where
  field
    decl : AGA.AbSelfEnriched
    expectedCat : AGA.Ab
    link : AGA.AbSelfEnriched.category decl ≡ expectedCat
    status : Core.Phase.Bool

mkAbSelfEnrichedAdapter :
  (d : AGA.AbSelfEnriched) →
  (ec : AGA.Ab) →
  (p : AGA.AbSelfEnriched.category d ≡ ec) →
  AbSelfEnrichedAdapter
mkAbSelfEnrichedAdapter d ec p =
  record { decl = d ; expectedCat = ec ; link = p ; status = true }

isFilledAbSelfEnriched : AbSelfEnrichedAdapter → Core.Phase.Bool
isFilledAbSelfEnriched a = AbSelfEnrichedAdapter.status a

-- Ab self-enrichment via internal hom
record AbSelfEnrichmentViaInternalHomAdapter : Set₁ where
  field
    decl : AGA.AbSelfEnrichmentViaInternalHom
    expectedCat : AGA.Ab
    link : AGA.AbSelfEnrichmentViaInternalHom.category decl ≡ expectedCat
    status : Core.Phase.Bool

mkAbSelfEnrichmentViaInternalHomAdapter :
  (d : AGA.AbSelfEnrichmentViaInternalHom) →
  (ec : AGA.Ab) →
  (p : AGA.AbSelfEnrichmentViaInternalHom.category d ≡ ec) →
  AbSelfEnrichmentViaInternalHomAdapter
mkAbSelfEnrichmentViaInternalHomAdapter d ec p =
  record { decl = d ; expectedCat = ec ; link = p ; status = true }

isFilledAbSelfEnrichmentViaInternalHom : AbSelfEnrichmentViaInternalHomAdapter → Core.Phase.Bool
isFilledAbSelfEnrichmentViaInternalHom a = AbSelfEnrichmentViaInternalHomAdapter.status a

monoidAsMonoidalCategorical : (adapt : MonoidAsMonoidalCategoryAdapter) →
  CategoricalAdapter {lsuc lzero} AE.MonoidAsMonoidalCategory
monoidAsMonoidalCategorical adapt = 
  mkCategoricalAdapter AE.MonoidAsMonoidalCategory (λ _ → MonoidAsMonoidalCategoryAdapter.decl adapt)

abelianGroupAsSymmetricMonoidalCategorical : (adapt : AbelianGroupAsSymmetricMonoidalAdapter) →
  CategoricalAdapter {lsuc lzero} AE.AbelianGroupAsSymmetricMonoidal
abelianGroupAsSymmetricMonoidalCategorical adapt = 
  mkCategoricalAdapter AE.AbelianGroupAsSymmetricMonoidal (λ _ → AbelianGroupAsSymmetricMonoidalAdapter.decl adapt)

monoidEnrichedCategoryCategorical : (adapt : MonoidEnrichedCategoryAdapter) →
  CategoricalAdapter {lsuc lzero} AE.MonoidEnrichedCategory
monoidEnrichedCategoryCategorical adapt = 
  mkCategoricalAdapter AE.MonoidEnrichedCategory (λ _ → MonoidEnrichedCategoryAdapter.decl adapt)

distanceCategoryCategorical : (adapt : DistanceCategoryAdapter) →
  CategoricalAdapter {lsuc lzero} AE.DistanceCategory
distanceCategoryCategorical adapt = 
  mkCategoricalAdapter AE.DistanceCategory (λ _ → DistanceCategoryAdapter.decl adapt)

abEnrichedCategoryCategorical : (adapt : AbEnrichedCategoryAdapter) →
  CategoricalAdapter {lsuc lzero} AE.AbEnrichedCategory
abEnrichedCategoryCategorical adapt = 
  mkCategoricalAdapter AE.AbEnrichedCategory (λ _ → AbEnrichedCategoryAdapter.decl adapt)

genericEnrichmentCategorical : (adapt : GenericEnrichmentAdapter) →
  CategoricalAdapter {lsuc lzero} (AE.GenericEnrichment (GenericEnrichmentAdapter.V adapt))
genericEnrichmentCategorical adapt = 
  mkCategoricalAdapter (AE.GenericEnrichment (GenericEnrichmentAdapter.V adapt))
    (λ _ → GenericEnrichmentAdapter.decl adapt)

groupActionEnrichedCategoryCategorical : (adapt : GroupActionEnrichedCategoryAdapter) →
  CategoricalAdapter {lsuc lzero} AE.GroupActionEnrichedCategory
groupActionEnrichedCategoryCategorical adapt = 
  mkCategoricalAdapter AE.GroupActionEnrichedCategory (λ _ → GroupActionEnrichedCategoryAdapter.decl adapt)

moduleEnrichedCategoryCategorical : (adapt : ModuleEnrichedCategoryAdapter) →
  CategoricalAdapter {lsuc lzero} AE.ModuleEnrichedCategory
moduleEnrichedCategoryCategorical adapt = 
  mkCategoricalAdapter AE.ModuleEnrichedCategory (λ _ → ModuleEnrichedCategoryAdapter.decl adapt)

lawvereTheoryEnrichedCategoryCategorical : (adapt : LawvereTheoryEnrichedCategoryAdapter) →
  CategoricalAdapter {lsuc lzero} AE.LawvereTheoryEnrichedCategory
lawvereTheoryEnrichedCategoryCategorical adapt = 
  mkCategoricalAdapter AE.LawvereTheoryEnrichedCategory (λ _ → LawvereTheoryEnrichedCategoryAdapter.decl adapt)

abSelfEnrichedCategorical : (adapt : AbSelfEnrichedAdapter) →
  CategoricalAdapter {lsuc lzero} AGA.AbSelfEnriched
abSelfEnrichedCategorical adapt = 
  mkCategoricalAdapter AGA.AbSelfEnriched (λ _ → AbSelfEnrichedAdapter.decl adapt)

abSelfEnrichmentViaInternalHomCategorical : (adapt : AbSelfEnrichmentViaInternalHomAdapter) →
  CategoricalAdapter {lsuc lzero} AGA.AbSelfEnrichmentViaInternalHom
abSelfEnrichmentViaInternalHomCategorical adapt = 
  mkCategoricalAdapter AGA.AbSelfEnrichmentViaInternalHom (λ _ → AbSelfEnrichmentViaInternalHomAdapter.decl adapt)

-- ============================================================================
-- Module Category Theory and R-Algebras
-- ============================================================================

-- General exact sequence
record ExactSequenceAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    decl : AM.ExactSequence R
    expectedRing : AR.RingDeclaration
    link : AM.ExactSequence.ring decl ≡ expectedRing
    status : Core.Phase.Bool

mkExactSequenceAdapter :
  (R : AR.RingDeclaration) →
  (d : AM.ExactSequence R) →
  (er : AR.RingDeclaration) →
  (p : AM.ExactSequence.ring d ≡ er) →
  ExactSequenceAdapter
mkExactSequenceAdapter R d er p =
  record { R = R ; decl = d ; expectedRing = er ; link = p ; status = true }

isFilledExactSequence : ExactSequenceAdapter → Core.Phase.Bool
isFilledExactSequence a = ExactSequenceAdapter.status a

-- Category of modules R-Mod
record CategoryOfModulesAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    decl : AM.CategoryOfModules R
    expectedRing : AR.RingDeclaration
    link : AM.CategoryOfModules.ring decl ≡ expectedRing
    status : Core.Phase.Bool

mkCategoryOfModulesAdapter :
  (R : AR.RingDeclaration) →
  (d : AM.CategoryOfModules R) →
  (er : AR.RingDeclaration) →
  (p : AM.CategoryOfModules.ring d ≡ er) →
  CategoryOfModulesAdapter
mkCategoryOfModulesAdapter R d er p =
  record { R = R ; decl = d ; expectedRing = er ; link = p ; status = true }

isFilledCategoryOfModules : CategoryOfModulesAdapter → Core.Phase.Bool
isFilledCategoryOfModules a = CategoryOfModulesAdapter.status a

-- Vector space over a field
record VectorSpaceAdapter : Set₁ where
  field
    F : AR.FieldDeclaration
    decl : AM.VectorSpace F
    expectedField : AR.FieldDeclaration
    link : AM.VectorSpace.field' decl ≡ expectedField
    status : Core.Phase.Bool

mkVectorSpaceAdapter :
  (F : AR.FieldDeclaration) →
  (d : AM.VectorSpace F) →
  (ef : AR.FieldDeclaration) →
  (p : AM.VectorSpace.field' d ≡ ef) →
  VectorSpaceAdapter
mkVectorSpaceAdapter F d ef p =
  record { F = F ; decl = d ; expectedField = ef ; link = p ; status = true }

isFilledVectorSpace : VectorSpaceAdapter → Core.Phase.Bool
isFilledVectorSpace a = VectorSpaceAdapter.status a

-- R-algebra (ring with compatible R-module structure)
record RAlgebraAdapter : Set₁ where
  field
    R : AR.CommutativeRingDeclaration
    decl : AM.RAlgebra R
    expectedRing : AR.CommutativeRingDeclaration
    link : AM.RAlgebra.coefficientRing decl ≡ expectedRing
    status : Core.Phase.Bool

mkRAlgebraAdapter :
  (R : AR.CommutativeRingDeclaration) →
  (d : AM.RAlgebra R) →
  (er : AR.CommutativeRingDeclaration) →
  (p : AM.RAlgebra.coefficientRing d ≡ er) →
  RAlgebraAdapter
mkRAlgebraAdapter R d er p =
  record { R = R ; decl = d ; expectedRing = er ; link = p ; status = true }

isFilledRAlgebra : RAlgebraAdapter → Core.Phase.Bool
isFilledRAlgebra a = RAlgebraAdapter.status a

-- Algebra homomorphism
record AlgebraHomomorphismAdapter : Set₁ where
  field
    R : AR.CommutativeRingDeclaration
    A B : AM.RAlgebra R
    decl : AM.AlgebraHomomorphism R A B
    expectedRing : AR.CommutativeRingDeclaration
    link : AM.AlgebraHomomorphism.coefficientRing decl ≡ expectedRing
    status : Core.Phase.Bool

mkAlgebraHomomorphismAdapter :
  (R : AR.CommutativeRingDeclaration) →
  (A B : AM.RAlgebra R) →
  (d : AM.AlgebraHomomorphism R A B) →
  (er : AR.CommutativeRingDeclaration) →
  (p : AM.AlgebraHomomorphism.coefficientRing d ≡ er) →
  AlgebraHomomorphismAdapter
mkAlgebraHomomorphismAdapter R A B d er p =
  record { R = R ; A = A ; B = B ; decl = d ; expectedRing = er ; link = p ; status = true }

isFilledAlgebraHomomorphism : AlgebraHomomorphismAdapter → Core.Phase.Bool
isFilledAlgebraHomomorphism a = AlgebraHomomorphismAdapter.status a

exactSequenceCategorical : (adapt : ExactSequenceAdapter) →
  CategoricalAdapter {lsuc lzero} (AM.ExactSequence (ExactSequenceAdapter.R adapt))
exactSequenceCategorical adapt = 
  mkCategoricalAdapter (AM.ExactSequence (ExactSequenceAdapter.R adapt))
    (λ _ → ExactSequenceAdapter.decl adapt)

categoryOfModulesCategorical : (adapt : CategoryOfModulesAdapter) →
  CategoricalAdapter {lsuc lzero} (AM.CategoryOfModules (CategoryOfModulesAdapter.R adapt))
categoryOfModulesCategorical adapt = 
  mkCategoricalAdapter (AM.CategoryOfModules (CategoryOfModulesAdapter.R adapt))
    (λ _ → CategoryOfModulesAdapter.decl adapt)

vectorSpaceCategorical : (adapt : VectorSpaceAdapter) →
  CategoricalAdapter {lsuc lzero} (AM.VectorSpace (VectorSpaceAdapter.F adapt))
vectorSpaceCategorical adapt = 
  mkCategoricalAdapter (AM.VectorSpace (VectorSpaceAdapter.F adapt))
    (λ _ → VectorSpaceAdapter.decl adapt)

rAlgebraCategorical : (adapt : RAlgebraAdapter) →
  CategoricalAdapter {lsuc lzero} (AM.RAlgebra (RAlgebraAdapter.R adapt))
rAlgebraCategorical adapt = 
  mkCategoricalAdapter (AM.RAlgebra (RAlgebraAdapter.R adapt))
    (λ _ → RAlgebraAdapter.decl adapt)

algebraHomomorphismCategorical : (adapt : AlgebraHomomorphismAdapter) →
  CategoricalAdapter {lsuc lzero} (AM.AlgebraHomomorphism (AlgebraHomomorphismAdapter.R adapt) (AlgebraHomomorphismAdapter.A adapt) (AlgebraHomomorphismAdapter.B adapt))
algebraHomomorphismCategorical adapt = 
  mkCategoricalAdapter (AM.AlgebraHomomorphism (AlgebraHomomorphismAdapter.R adapt) (AlgebraHomomorphismAdapter.A adapt) (AlgebraHomomorphismAdapter.B adapt))
    (λ _ → AlgebraHomomorphismAdapter.decl adapt)

------------------------------------------------------------------------
-- Monad-Adjunction Theory (Chapter2.Level2sub4)
------------------------------------------------------------------------

-- Category of T-algebras (Eilenberg-Moore category)
record CategoryOfAlgebrasAdapter : Set₁ where
  field
    decl : C2S4.CategoryOfAlgebras
    expectedMonad : C2S4.MonadDeclaration
    link : C2S4.CategoryOfAlgebras.monad decl ≡ expectedMonad
    status : Core.Phase.Bool

mkCategoryOfAlgebrasAdapter :
  (d : C2S4.CategoryOfAlgebras) →
  (em : C2S4.MonadDeclaration) →
  (p : C2S4.CategoryOfAlgebras.monad d ≡ em) →
  CategoryOfAlgebrasAdapter
mkCategoryOfAlgebrasAdapter d em p =
  record { decl = d ; expectedMonad = em ; link = p ; status = true }

isFilledCategoryOfAlgebras : CategoryOfAlgebrasAdapter → Core.Phase.Bool
isFilledCategoryOfAlgebras a = CategoryOfAlgebrasAdapter.status a

-- Categorical view for CategoryOfAlgebras
categoryOfAlgebrasCategorical : CategoryOfAlgebrasAdapter →
  CategoricalAdapter {lzero} C2S4.CategoryOfAlgebras
categoryOfAlgebrasCategorical adapt =
  mkCategoricalAdapter C2S4.CategoryOfAlgebras (λ _ → CategoryOfAlgebrasAdapter.decl adapt)

-- Theorem: Adjunction induces monad
record AdjunctionInducesMonadTheoremAdapter : Set₁ where
  field
    decl : C2S4.AdjunctionInducesMonadTheorem
    expectedMonad : C2S4.MonadDeclaration
    link : C2S4.AdjunctionInducesMonadTheorem.inducedMonad decl ≡ expectedMonad
    status : Core.Phase.Bool

mkAdjunctionInducesMonadTheoremAdapter :
  (d : C2S4.AdjunctionInducesMonadTheorem) →
  (em : C2S4.MonadDeclaration) →
  (p : C2S4.AdjunctionInducesMonadTheorem.inducedMonad d ≡ em) →
  AdjunctionInducesMonadTheoremAdapter
mkAdjunctionInducesMonadTheoremAdapter d em p =
  record { decl = d ; expectedMonad = em ; link = p ; status = true }

isFilledAdjunctionInducesMonadTheorem : AdjunctionInducesMonadTheoremAdapter → Core.Phase.Bool
isFilledAdjunctionInducesMonadTheorem a = AdjunctionInducesMonadTheoremAdapter.status a

-- Categorical view for AdjunctionInducesMonadTheorem
adjunctionInducesMonadTheoremCategorical : AdjunctionInducesMonadTheoremAdapter →
  CategoricalAdapter {lzero} C2S4.AdjunctionInducesMonadTheorem
adjunctionInducesMonadTheoremCategorical adapt =
  mkCategoricalAdapter C2S4.AdjunctionInducesMonadTheorem (λ _ → AdjunctionInducesMonadTheoremAdapter.decl adapt)

-- Eilenberg-Moore adjunction from monad
record EilenbergMooreAdjunctionAdapter : Set₁ where
  field
    decl : C2S4.EilenbergMooreAdjunction
    expectedMonad : C2S4.MonadDeclaration
    expectedAlgCat : C2S4.CategoryOfAlgebras
    link1 : C2S4.EilenbergMooreAdjunction.monad decl ≡ expectedMonad
    link2 : C2S4.EilenbergMooreAdjunction.algebraCategory decl ≡ expectedAlgCat
    status : Core.Phase.Bool

mkEilenbergMooreAdjunctionAdapter :
  (d : C2S4.EilenbergMooreAdjunction) →
  (em : C2S4.MonadDeclaration) →
  (eac : C2S4.CategoryOfAlgebras) →
  (p1 : C2S4.EilenbergMooreAdjunction.monad d ≡ em) →
  (p2 : C2S4.EilenbergMooreAdjunction.algebraCategory d ≡ eac) →
  EilenbergMooreAdjunctionAdapter
mkEilenbergMooreAdjunctionAdapter d em eac p1 p2 =
  record { decl = d ; expectedMonad = em ; expectedAlgCat = eac ; link1 = p1 ; link2 = p2 ; status = true }

isFilledEilenbergMooreAdjunction : EilenbergMooreAdjunctionAdapter → Core.Phase.Bool
isFilledEilenbergMooreAdjunction a = EilenbergMooreAdjunctionAdapter.status a

-- Categorical view for EilenbergMooreAdjunction
eilenbergMooreAdjunctionCategorical : EilenbergMooreAdjunctionAdapter →
  CategoricalAdapter {lzero} C2S4.EilenbergMooreAdjunction
eilenbergMooreAdjunctionCategorical adapt =
  mkCategoricalAdapter C2S4.EilenbergMooreAdjunction (λ _ → EilenbergMooreAdjunctionAdapter.decl adapt)

-- Monad-adjunction correspondence theorem
record MonadAdjunctionCorrespondenceTheoremAdapter : Set₁ where
  field
    decl : C2S4.MonadAdjunctionCorrespondenceTheorem
    expectedMonad : C2S4.MonadDeclaration
    expectedEM : C2S4.EilenbergMooreAdjunction
    link1 : C2S4.MonadAdjunctionCorrespondenceTheorem.monad decl ≡ expectedMonad
    link2 : C2S4.MonadAdjunctionCorrespondenceTheorem.emAdjunction decl ≡ expectedEM
    status : Core.Phase.Bool

mkMonadAdjunctionCorrespondenceTheoremAdapter :
  (d : C2S4.MonadAdjunctionCorrespondenceTheorem) →
  (em : C2S4.MonadDeclaration) →
  (eEM : C2S4.EilenbergMooreAdjunction) →
  (p1 : C2S4.MonadAdjunctionCorrespondenceTheorem.monad d ≡ em) →
  (p2 : C2S4.MonadAdjunctionCorrespondenceTheorem.emAdjunction d ≡ eEM) →
  MonadAdjunctionCorrespondenceTheoremAdapter
mkMonadAdjunctionCorrespondenceTheoremAdapter d em eEM p1 p2 =
  record { decl = d ; expectedMonad = em ; expectedEM = eEM ; link1 = p1 ; link2 = p2 ; status = true }

isFilledMonadAdjunctionCorrespondenceTheorem : MonadAdjunctionCorrespondenceTheoremAdapter → Core.Phase.Bool
isFilledMonadAdjunctionCorrespondenceTheorem a = MonadAdjunctionCorrespondenceTheoremAdapter.status a

-- Categorical view for MonadAdjunctionCorrespondenceTheorem
monadAdjunctionCorrespondenceTheoremCategorical : MonadAdjunctionCorrespondenceTheoremAdapter →
  CategoricalAdapter {lzero} C2S4.MonadAdjunctionCorrespondenceTheorem
monadAdjunctionCorrespondenceTheoremCategorical adapt =
  mkCategoricalAdapter C2S4.MonadAdjunctionCorrespondenceTheorem (λ _ → MonadAdjunctionCorrespondenceTheoremAdapter.decl adapt)

-- Beck monadicity theorem
record BeckMonadicityTheoremAdapter : Set₁ where
  field
    decl : C2S4.BeckMonadicityTheorem
    expectedReflects : C2S4.ReflectsIsomorphismsProperty
    link : C2S4.BeckMonadicityTheorem.reflectsIsomorphisms decl ≡ expectedReflects
    status : Core.Phase.Bool

mkBeckMonadicityTheoremAdapter :
  (d : C2S4.BeckMonadicityTheorem) →
  (er : C2S4.ReflectsIsomorphismsProperty) →
  (p : C2S4.BeckMonadicityTheorem.reflectsIsomorphisms d ≡ er) →
  BeckMonadicityTheoremAdapter
mkBeckMonadicityTheoremAdapter d er p =
  record { decl = d ; expectedReflects = er ; link = p ; status = true }

isFilledBeckMonadicityTheorem : BeckMonadicityTheoremAdapter → Core.Phase.Bool
isFilledBeckMonadicityTheorem a = BeckMonadicityTheoremAdapter.status a

-- Categorical view for BeckMonadicityTheorem
beckMonadicityTheoremCategorical : BeckMonadicityTheoremAdapter →
  CategoricalAdapter {lzero} C2S4.BeckMonadicityTheorem
beckMonadicityTheoremCategorical adapt =
  mkCategoricalAdapter C2S4.BeckMonadicityTheorem (λ _ → BeckMonadicityTheoremAdapter.decl adapt)

-- Functor property: is monadic
record MonadicFunctorPropertyAdapter : Set₁ where
  field
    decl : C2S4.MonadicFunctorProperty
    expectedFunctor : M.Identifier
    link : C2S4.MonadicFunctorProperty.functor decl ≡ expectedFunctor
    status : Core.Phase.Bool

mkMonadicFunctorPropertyAdapter :
  (d : C2S4.MonadicFunctorProperty) →
  (ef : M.Identifier) →
  (p : C2S4.MonadicFunctorProperty.functor d ≡ ef) →
  MonadicFunctorPropertyAdapter
mkMonadicFunctorPropertyAdapter d ef p =
  record { decl = d ; expectedFunctor = ef ; link = p ; status = true }

isFilledMonadicFunctorProperty : MonadicFunctorPropertyAdapter → Core.Phase.Bool
isFilledMonadicFunctorProperty a = MonadicFunctorPropertyAdapter.status a

-- Categorical view for MonadicFunctorProperty
monadicFunctorPropertyCategorical : MonadicFunctorPropertyAdapter →
  CategoricalAdapter {lzero} C2S4.MonadicFunctorProperty
monadicFunctorPropertyCategorical adapt =
  mkCategoricalAdapter C2S4.MonadicFunctorProperty (λ _ → MonadicFunctorPropertyAdapter.decl adapt)

-- Comonad from adjunction (for descent theory)
record ComonadFromAdjunctionAdapter : Set₁ where
  field
    decl : C2S4.ComonadFromAdjunction
    expectedComonad : C2S4.ComonadDeclaration
    link : C2S4.ComonadFromAdjunction.inducedComonad decl ≡ expectedComonad
    status : Core.Phase.Bool

mkComonadFromAdjunctionAdapter :
  (d : C2S4.ComonadFromAdjunction) →
  (ec : C2S4.ComonadDeclaration) →
  (p : C2S4.ComonadFromAdjunction.inducedComonad d ≡ ec) →
  ComonadFromAdjunctionAdapter
mkComonadFromAdjunctionAdapter d ec p =
  record { decl = d ; expectedComonad = ec ; link = p ; status = true }

isFilledComonadFromAdjunction : ComonadFromAdjunctionAdapter → Core.Phase.Bool
isFilledComonadFromAdjunction a = ComonadFromAdjunctionAdapter.status a

-- Categorical view for ComonadFromAdjunction
comonadFromAdjunctionCategorical : ComonadFromAdjunctionAdapter →
  CategoricalAdapter {lzero} C2S4.ComonadFromAdjunction
comonadFromAdjunctionCategorical adapt =
  mkCategoricalAdapter C2S4.ComonadFromAdjunction (λ _ → ComonadFromAdjunctionAdapter.decl adapt)

------------------------------------------------------------------------
-- Limits and Colimits in Algebra Categories (Chapter2.Level2sub4)
------------------------------------------------------------------------

-- Theorem: Forgetful functor from algebras creates limits
record ForgetfulFunctorFromAlgebrasCreatesLimitsTheoremAdapter : Set₁ where
  field
    decl : C2S4.ForgetfulFunctorFromAlgebrasCreatesLimitsTheorem
    expectedMonad : C2S4.MonadDeclaration
    expectedAlgCat : C2S4.CategoryOfAlgebras
    link1 : C2S4.ForgetfulFunctorFromAlgebrasCreatesLimitsTheorem.monad decl ≡ expectedMonad
    link2 : C2S4.ForgetfulFunctorFromAlgebrasCreatesLimitsTheorem.algebraCategory decl ≡ expectedAlgCat
    status : Core.Phase.Bool

mkForgetfulFunctorFromAlgebrasCreatesLimitsTheoremAdapter :
  (d : C2S4.ForgetfulFunctorFromAlgebrasCreatesLimitsTheorem) →
  (em : C2S4.MonadDeclaration) →
  (eac : C2S4.CategoryOfAlgebras) →
  (p1 : C2S4.ForgetfulFunctorFromAlgebrasCreatesLimitsTheorem.monad d ≡ em) →
  (p2 : C2S4.ForgetfulFunctorFromAlgebrasCreatesLimitsTheorem.algebraCategory d ≡ eac) →
  ForgetfulFunctorFromAlgebrasCreatesLimitsTheoremAdapter
mkForgetfulFunctorFromAlgebrasCreatesLimitsTheoremAdapter d em eac p1 p2 =
  record { decl = d ; expectedMonad = em ; expectedAlgCat = eac ; link1 = p1 ; link2 = p2 ; status = true }

isFilledForgetfulFunctorFromAlgebrasCreatesLimitsTheorem : ForgetfulFunctorFromAlgebrasCreatesLimitsTheoremAdapter → Core.Phase.Bool
isFilledForgetfulFunctorFromAlgebrasCreatesLimitsTheorem a = ForgetfulFunctorFromAlgebrasCreatesLimitsTheoremAdapter.status a

-- Categorical view for ForgetfulFunctorFromAlgebrasCreatesLimitsTheorem
forgetfulFunctorFromAlgebrasCreatesLimitsTheoremCategorical : ForgetfulFunctorFromAlgebrasCreatesLimitsTheoremAdapter →
  CategoricalAdapter {lzero} C2S4.ForgetfulFunctorFromAlgebrasCreatesLimitsTheorem
forgetfulFunctorFromAlgebrasCreatesLimitsTheoremCategorical adapt =
  mkCategoricalAdapter C2S4.ForgetfulFunctorFromAlgebrasCreatesLimitsTheorem (λ _ → ForgetfulFunctorFromAlgebrasCreatesLimitsTheoremAdapter.decl adapt)

-- Corollary: Completeness of algebra categories
record CompletenessOfAlgebraCategoriesCorollaryAdapter : Set₁ where
  field
    decl : C2S4.CompletenessOfAlgebraCategoriesCorollary
    expectedMonad : C2S4.MonadDeclaration
    expectedAlgCat : C2S4.CategoryOfAlgebras
    link1 : C2S4.CompletenessOfAlgebraCategoriesCorollary.monad decl ≡ expectedMonad
    link2 : C2S4.CompletenessOfAlgebraCategoriesCorollary.algebraCategory decl ≡ expectedAlgCat
    status : Core.Phase.Bool

mkCompletenessOfAlgebraCategoriesCorollaryAdapter :
  (d : C2S4.CompletenessOfAlgebraCategoriesCorollary) →
  (em : C2S4.MonadDeclaration) →
  (eac : C2S4.CategoryOfAlgebras) →
  (p1 : C2S4.CompletenessOfAlgebraCategoriesCorollary.monad d ≡ em) →
  (p2 : C2S4.CompletenessOfAlgebraCategoriesCorollary.algebraCategory d ≡ eac) →
  CompletenessOfAlgebraCategoriesCorollaryAdapter
mkCompletenessOfAlgebraCategoriesCorollaryAdapter d em eac p1 p2 =
  record { decl = d ; expectedMonad = em ; expectedAlgCat = eac ; link1 = p1 ; link2 = p2 ; status = true }

isFilledCompletenessOfAlgebraCategoriesCorollary : CompletenessOfAlgebraCategoriesCorollaryAdapter → Core.Phase.Bool
isFilledCompletenessOfAlgebraCategoriesCorollary a = CompletenessOfAlgebraCategoriesCorollaryAdapter.status a

-- Categorical view for CompletenessOfAlgebraCategoriesCorollary
completenessOfAlgebraCategoriesCorollaryCategorical : CompletenessOfAlgebraCategoriesCorollaryAdapter →
  CategoricalAdapter {lzero} C2S4.CompletenessOfAlgebraCategoriesCorollary
completenessOfAlgebraCategoriesCorollaryCategorical adapt =
  mkCategoricalAdapter C2S4.CompletenessOfAlgebraCategoriesCorollary (λ _ → CompletenessOfAlgebraCategoriesCorollaryAdapter.decl adapt)

-- Reflexive pair
record ReflexivePairAdapter : Set₁ where
  field
    decl : C2S4.ReflexivePair
    expectedDomain : M.Identifier
    expectedCodomain : M.Identifier
    link1 : C2S4.ReflexivePair.domain decl ≡ expectedDomain
    link2 : C2S4.ReflexivePair.codomain decl ≡ expectedCodomain
    status : Core.Phase.Bool

mkReflexivePairAdapter :
  (d : C2S4.ReflexivePair) →
  (edom : M.Identifier) →
  (ecod : M.Identifier) →
  (p1 : C2S4.ReflexivePair.domain d ≡ edom) →
  (p2 : C2S4.ReflexivePair.codomain d ≡ ecod) →
  ReflexivePairAdapter
mkReflexivePairAdapter d edom ecod p1 p2 =
  record { decl = d ; expectedDomain = edom ; expectedCodomain = ecod ; link1 = p1 ; link2 = p2 ; status = true }

isFilledReflexivePair : ReflexivePairAdapter → Core.Phase.Bool
isFilledReflexivePair a = ReflexivePairAdapter.status a

-- Categorical view for ReflexivePair
reflexivePairCategorical : ReflexivePairAdapter →
  CategoricalAdapter {lzero} C2S4.ReflexivePair
reflexivePairCategorical adapt =
  mkCategoricalAdapter C2S4.ReflexivePair (λ _ → ReflexivePairAdapter.decl adapt)

-- Theorem: Forgetful functor preserves coequalizers of reflexive pairs
record ForgetfulFunctorPreservesCertainCoequalizersTheoremAdapter : Set₁ where
  field
    decl : C2S4.ForgetfulFunctorPreservesCertainCoequalizersTheorem
    expectedMonad : C2S4.MonadDeclaration
    expectedAlgCat : C2S4.CategoryOfAlgebras
    expectedRefPair : C2S4.ReflexivePair
    link1 : C2S4.ForgetfulFunctorPreservesCertainCoequalizersTheorem.monad decl ≡ expectedMonad
    link2 : C2S4.ForgetfulFunctorPreservesCertainCoequalizersTheorem.algebraCategory decl ≡ expectedAlgCat
    link3 : C2S4.ForgetfulFunctorPreservesCertainCoequalizersTheorem.reflexivePair decl ≡ expectedRefPair
    status : Core.Phase.Bool

mkForgetfulFunctorPreservesCertainCoequalizersTheoremAdapter :
  (d : C2S4.ForgetfulFunctorPreservesCertainCoequalizersTheorem) →
  (em : C2S4.MonadDeclaration) →
  (eac : C2S4.CategoryOfAlgebras) →
  (erp : C2S4.ReflexivePair) →
  (p1 : C2S4.ForgetfulFunctorPreservesCertainCoequalizersTheorem.monad d ≡ em) →
  (p2 : C2S4.ForgetfulFunctorPreservesCertainCoequalizersTheorem.algebraCategory d ≡ eac) →
  (p3 : C2S4.ForgetfulFunctorPreservesCertainCoequalizersTheorem.reflexivePair d ≡ erp) →
  ForgetfulFunctorPreservesCertainCoequalizersTheoremAdapter
mkForgetfulFunctorPreservesCertainCoequalizersTheoremAdapter d em eac erp p1 p2 p3 =
  record { decl = d ; expectedMonad = em ; expectedAlgCat = eac ; expectedRefPair = erp ; link1 = p1 ; link2 = p2 ; link3 = p3 ; status = true }

isFilledForgetfulFunctorPreservesCertainCoequalizersTheorem : ForgetfulFunctorPreservesCertainCoequalizersTheoremAdapter → Core.Phase.Bool
isFilledForgetfulFunctorPreservesCertainCoequalizersTheorem a = ForgetfulFunctorPreservesCertainCoequalizersTheoremAdapter.status a

-- Categorical view for ForgetfulFunctorPreservesCertainCoequalizersTheorem
forgetfulFunctorPreservesCertainCoequalizersTheoremCategorical : ForgetfulFunctorPreservesCertainCoequalizersTheoremAdapter →
  CategoricalAdapter {lzero} C2S4.ForgetfulFunctorPreservesCertainCoequalizersTheorem
forgetfulFunctorPreservesCertainCoequalizersTheoremCategorical adapt =
  mkCategoricalAdapter C2S4.ForgetfulFunctorPreservesCertainCoequalizersTheorem (λ _ → ForgetfulFunctorPreservesCertainCoequalizersTheoremAdapter.decl adapt)

-- Functor property: reflects isomorphisms
record ReflectsIsomorphismsPropertyAdapter : Set₁ where
  field
    decl : C2S4.ReflectsIsomorphismsProperty
    expectedFunctor : M.Identifier
    link : C2S4.ReflectsIsomorphismsProperty.functor decl ≡ expectedFunctor
    status : Core.Phase.Bool

mkReflectsIsomorphismsPropertyAdapter :
  (d : C2S4.ReflectsIsomorphismsProperty) →
  (ef : M.Identifier) →
  (p : C2S4.ReflectsIsomorphismsProperty.functor d ≡ ef) →
  ReflectsIsomorphismsPropertyAdapter
mkReflectsIsomorphismsPropertyAdapter d ef p =
  record { decl = d ; expectedFunctor = ef ; link = p ; status = true }

isFilledReflectsIsomorphismsProperty : ReflectsIsomorphismsPropertyAdapter → Core.Phase.Bool
isFilledReflectsIsomorphismsProperty a = ReflectsIsomorphismsPropertyAdapter.status a

-- Categorical view for ReflectsIsomorphismsProperty
reflectsIsomorphismsPropertyCategorical : ReflectsIsomorphismsPropertyAdapter →
  CategoricalAdapter {lzero} C2S4.ReflectsIsomorphismsProperty
reflectsIsomorphismsPropertyCategorical adapt =
  mkCategoricalAdapter C2S4.ReflectsIsomorphismsProperty (λ _ → ReflectsIsomorphismsPropertyAdapter.decl adapt)

-- U-split pair
record USplitPairAdapter : Set₁ where
  field
    decl : C2S4.USplitPair
    expectedFunctor : M.Identifier
    link : C2S4.USplitPair.functor decl ≡ expectedFunctor
    status : Core.Phase.Bool

mkUSplitPairAdapter :
  (d : C2S4.USplitPair) →
  (ef : M.Identifier) →
  (p : C2S4.USplitPair.functor d ≡ ef) →
  USplitPairAdapter
mkUSplitPairAdapter d ef p =
  record { decl = d ; expectedFunctor = ef ; link = p ; status = true }

isFilledUSplitPair : USplitPairAdapter → Core.Phase.Bool
isFilledUSplitPair a = USplitPairAdapter.status a

-- Categorical view for USplitPair
uSplitPairCategorical : USplitPairAdapter →
  CategoricalAdapter {lzero} C2S4.USplitPair
uSplitPairCategorical adapt =
  mkCategoricalAdapter C2S4.USplitPair (λ _ → USplitPairAdapter.decl adapt)

------------------------------------------------------------------------
-- Regular Category Theory (Chapter2.Level2sub2)
------------------------------------------------------------------------

-- Regular category declaration
record RegularCategoryDeclarationAdapter : Set₁ where
  field
    decl : C2S2.RegularCategoryDeclaration
    expectedFiniteLimits : C2S2.FiniteLimitsProperty
    expectedStability : C2S2.StabilityUnderPullbackProperty
    link1 : C2S2.RegularCategoryDeclaration.finiteLimits decl ≡ expectedFiniteLimits
    link2 : C2S2.RegularCategoryDeclaration.regularEpiStability decl ≡ expectedStability
    status : Core.Phase.Bool

mkRegularCategoryDeclarationAdapter :
  (d : C2S2.RegularCategoryDeclaration) →
  (efl : C2S2.FiniteLimitsProperty) →
  (es : C2S2.StabilityUnderPullbackProperty) →
  (p1 : C2S2.RegularCategoryDeclaration.finiteLimits d ≡ efl) →
  (p2 : C2S2.RegularCategoryDeclaration.regularEpiStability d ≡ es) →
  RegularCategoryDeclarationAdapter
mkRegularCategoryDeclarationAdapter d efl es p1 p2 =
  record { decl = d ; expectedFiniteLimits = efl ; expectedStability = es ; link1 = p1 ; link2 = p2 ; status = true }

isFilledRegularCategoryDeclaration : RegularCategoryDeclarationAdapter → Core.Phase.Bool
isFilledRegularCategoryDeclaration a = RegularCategoryDeclarationAdapter.status a

-- Categorical view for RegularCategoryDeclaration
regularCategoryDeclarationCategorical : RegularCategoryDeclarationAdapter →
  CategoricalAdapter {lzero} C2S2.RegularCategoryDeclaration
regularCategoryDeclarationCategorical adapt =
  mkCategoricalAdapter C2S2.RegularCategoryDeclaration (λ _ → RegularCategoryDeclarationAdapter.decl adapt)

-- Kernel pair of a morphism
record KernelPairDeclarationAdapter : Set₁ where
  field
    decl : C2S2.KernelPairDeclaration
    expectedMorphism : M.Identifier
    link : C2S2.KernelPairDeclaration.morphism decl ≡ expectedMorphism
    status : Core.Phase.Bool

mkKernelPairDeclarationAdapter :
  (d : C2S2.KernelPairDeclaration) →
  (em : M.Identifier) →
  (p : C2S2.KernelPairDeclaration.morphism d ≡ em) →
  KernelPairDeclarationAdapter
mkKernelPairDeclarationAdapter d em p =
  record { decl = d ; expectedMorphism = em ; link = p ; status = true }

isFilledKernelPairDeclaration : KernelPairDeclarationAdapter → Core.Phase.Bool
isFilledKernelPairDeclaration a = KernelPairDeclarationAdapter.status a

-- Categorical view for KernelPairDeclaration
kernelPairDeclarationCategorical : KernelPairDeclarationAdapter →
  CategoricalAdapter {lzero} C2S2.KernelPairDeclaration
kernelPairDeclarationCategorical adapt =
  mkCategoricalAdapter C2S2.KernelPairDeclaration (λ _ → KernelPairDeclarationAdapter.decl adapt)

-- Internal equivalence relation
record InternalEquivalenceRelationDeclarationAdapter : Set₁ where
  field
    decl : C2S2.InternalEquivalenceRelationDeclaration
    expectedObjectA : M.Identifier
    link : C2S2.InternalEquivalenceRelationDeclaration.objectA decl ≡ expectedObjectA
    status : Core.Phase.Bool

mkInternalEquivalenceRelationDeclarationAdapter :
  (d : C2S2.InternalEquivalenceRelationDeclaration) →
  (eoa : M.Identifier) →
  (p : C2S2.InternalEquivalenceRelationDeclaration.objectA d ≡ eoa) →
  InternalEquivalenceRelationDeclarationAdapter
mkInternalEquivalenceRelationDeclarationAdapter d eoa p =
  record { decl = d ; expectedObjectA = eoa ; link = p ; status = true }

isFilledInternalEquivalenceRelationDeclaration : InternalEquivalenceRelationDeclarationAdapter → Core.Phase.Bool
isFilledInternalEquivalenceRelationDeclaration a = InternalEquivalenceRelationDeclarationAdapter.status a

-- Categorical view for InternalEquivalenceRelationDeclaration
internalEquivalenceRelationDeclarationCategorical : InternalEquivalenceRelationDeclarationAdapter →
  CategoricalAdapter {lzero} C2S2.InternalEquivalenceRelationDeclaration
internalEquivalenceRelationDeclarationCategorical adapt =
  mkCategoricalAdapter C2S2.InternalEquivalenceRelationDeclaration (λ _ → InternalEquivalenceRelationDeclarationAdapter.decl adapt)

-- Exact category (regular + effective relations)
record ExactCategoryDeclarationAdapter : Set₁ where
  field
    decl : C2S2.ExactCategoryDeclaration
    expectedRegular : C2S2.RegularCategoryDeclaration
    link : C2S2.ExactCategoryDeclaration.regular decl ≡ expectedRegular
    status : Core.Phase.Bool

mkExactCategoryDeclarationAdapter :
  (d : C2S2.ExactCategoryDeclaration) →
  (er : C2S2.RegularCategoryDeclaration) →
  (p : C2S2.ExactCategoryDeclaration.regular d ≡ er) →
  ExactCategoryDeclarationAdapter
mkExactCategoryDeclarationAdapter d er p =
  record { decl = d ; expectedRegular = er ; link = p ; status = true }

isFilledExactCategoryDeclaration : ExactCategoryDeclarationAdapter → Core.Phase.Bool
isFilledExactCategoryDeclaration a = ExactCategoryDeclarationAdapter.status a

-- Categorical view for ExactCategoryDeclaration
exactCategoryDeclarationCategorical : ExactCategoryDeclarationAdapter →
  CategoricalAdapter {lzero} C2S2.ExactCategoryDeclaration
exactCategoryDeclarationCategorical adapt =
  mkCategoricalAdapter C2S2.ExactCategoryDeclaration (λ _ → ExactCategoryDeclarationAdapter.decl adapt)

------------------------------------------------------------------------
-- Advanced Monad Theory (Chapter2.Level2sub4)
------------------------------------------------------------------------

-- Monad with rank α
record MonadWithRankAdapter : Set₁ where
  field
    decl : C2S4.MonadWithRank
    expectedMonad : C2S4.MonadDeclaration
    expectedCardinal : C2S4.RegularCardinal
    link1 : C2S4.MonadWithRank.monad decl ≡ expectedMonad
    link2 : C2S4.MonadWithRank.cardinal decl ≡ expectedCardinal
    status : Core.Phase.Bool

mkMonadWithRankAdapter :
  (d : C2S4.MonadWithRank) →
  (em : C2S4.MonadDeclaration) →
  (ec : C2S4.RegularCardinal) →
  (p1 : C2S4.MonadWithRank.monad d ≡ em) →
  (p2 : C2S4.MonadWithRank.cardinal d ≡ ec) →
  MonadWithRankAdapter
mkMonadWithRankAdapter d em ec p1 p2 =
  record { decl = d ; expectedMonad = em ; expectedCardinal = ec ; link1 = p1 ; link2 = p2 ; status = true }

open import Core.Phase using (Bool)
isFilledMonadWithRank : MonadWithRankAdapter → Bool
isFilledMonadWithRank a = MonadWithRankAdapter.status a

-- Categorical view for MonadWithRank
monadWithRankCategorical : MonadWithRankAdapter →
  CategoricalAdapter {lzero} C2S4.MonadWithRank
monadWithRankCategorical adapt =
  mkCategoricalAdapter C2S4.MonadWithRank (λ _ → MonadWithRankAdapter.decl adapt)

-- Locally α-presentable category
record LocallyPresentableCategoryAdapter : Set₁ where
  field
    decl : C2S4.LocallyPresentableCategory
    expectedCardinal : C2S4.RegularCardinal
    link : C2S4.LocallyPresentableCategory.cardinal decl ≡ expectedCardinal
    status : Core.Phase.Bool

mkLocallyPresentableCategoryAdapter :
  (d : C2S4.LocallyPresentableCategory) →
  (ec : C2S4.RegularCardinal) →
  (p : C2S4.LocallyPresentableCategory.cardinal d ≡ ec) →
  LocallyPresentableCategoryAdapter
mkLocallyPresentableCategoryAdapter d ec p =
  record { decl = d ; expectedCardinal = ec ; link = p ; status = true }

isFilledLocallyPresentableCategory : LocallyPresentableCategoryAdapter → Core.Phase.Bool
isFilledLocallyPresentableCategory a = LocallyPresentableCategoryAdapter.status a

-- Categorical view for LocallyPresentableCategory
locallyPresentableCategoryCategorical : LocallyPresentableCategoryAdapter →
  CategoricalAdapter {lzero} C2S4.LocallyPresentableCategory
locallyPresentableCategoryCategorical adapt =
  mkCategoricalAdapter C2S4.LocallyPresentableCategory (λ _ → LocallyPresentableCategoryAdapter.decl adapt)

-- Rank theorem for monadic categories
record RankTheoremForMonadicCategoriesTheoremAdapter : Set₁ where
  field
    decl : C2S4.RankTheoremForMonadicCategoriesTheorem
    expectedBaseCategory : C2S4.LocallyPresentableCategory
    expectedMonadWithRank : C2S4.MonadWithRank
    expectedAlgCat : C2S4.CategoryOfAlgebras
    link1 : C2S4.RankTheoremForMonadicCategoriesTheorem.baseCategory decl ≡ expectedBaseCategory
    link2 : C2S4.RankTheoremForMonadicCategoriesTheorem.monadWithRank decl ≡ expectedMonadWithRank
    link3 : C2S4.RankTheoremForMonadicCategoriesTheorem.algebraCategory decl ≡ expectedAlgCat
    status : Core.Phase.Bool

mkRankTheoremForMonadicCategoriesTheoremAdapter :
  (d : C2S4.RankTheoremForMonadicCategoriesTheorem) →
  (ebc : C2S4.LocallyPresentableCategory) →
  (emr : C2S4.MonadWithRank) →
  (eac : C2S4.CategoryOfAlgebras) →
  (p1 : C2S4.RankTheoremForMonadicCategoriesTheorem.baseCategory d ≡ ebc) →
  (p2 : C2S4.RankTheoremForMonadicCategoriesTheorem.monadWithRank d ≡ emr) →
  (p3 : C2S4.RankTheoremForMonadicCategoriesTheorem.algebraCategory d ≡ eac) →
  RankTheoremForMonadicCategoriesTheoremAdapter
mkRankTheoremForMonadicCategoriesTheoremAdapter d ebc emr eac p1 p2 p3 =
  record { decl = d ; expectedBaseCategory = ebc ; expectedMonadWithRank = emr ; expectedAlgCat = eac ; link1 = p1 ; link2 = p2 ; link3 = p3 ; status = true }

isFilledRankTheoremForMonadicCategoriesTheorem : RankTheoremForMonadicCategoriesTheoremAdapter → Core.Phase.Bool
isFilledRankTheoremForMonadicCategoriesTheorem a = RankTheoremForMonadicCategoriesTheoremAdapter.status a

-- Categorical view for RankTheoremForMonadicCategoriesTheorem
rankTheoremForMonadicCategoriesTheoremCategorical : RankTheoremForMonadicCategoriesTheoremAdapter →
  CategoricalAdapter {lzero} C2S4.RankTheoremForMonadicCategoriesTheorem
rankTheoremForMonadicCategoriesTheoremCategorical adapt =
  mkCategoricalAdapter C2S4.RankTheoremForMonadicCategoriesTheorem (λ _ → RankTheoremForMonadicCategoriesTheoremAdapter.decl adapt)

------------------------------------------------------------------------
-- Functor Properties: Preserve/Reflect/Create Limits (Chapter1.Level1sub2)
------------------------------------------------------------------------

-- Functor preserves limits
record FunctorPreservesLimitsAdapter : Set₁ where
  field
    decl : C1S2.FunctorPreservesLimits
    expectedFunctor : M.Identifier
    link : C1S2.FunctorPreservesLimits.F decl ≡ expectedFunctor
    status : Core.Phase.Bool

mkFunctorPreservesLimitsAdapter :
  (d : C1S2.FunctorPreservesLimits) →
  (ef : M.Identifier) →
  (p : C1S2.FunctorPreservesLimits.F d ≡ ef) →
  FunctorPreservesLimitsAdapter
mkFunctorPreservesLimitsAdapter d ef p =
  record { decl = d ; expectedFunctor = ef ; link = p ; status = true }

isFilledFunctorPreservesLimits : FunctorPreservesLimitsAdapter → Core.Phase.Bool
isFilledFunctorPreservesLimits a = FunctorPreservesLimitsAdapter.status a

-- Categorical view for FunctorPreservesLimits
functorPreservesLimitsCategorical : FunctorPreservesLimitsAdapter →
  CategoricalAdapter {lzero} C1S2.FunctorPreservesLimits
functorPreservesLimitsCategorical adapt =
  mkCategoricalAdapter C1S2.FunctorPreservesLimits (λ _ → FunctorPreservesLimitsAdapter.decl adapt)

-- Functor reflects limits
record FunctorReflectsLimitsAdapter : Set₁ where
  field
    decl : C1S2.FunctorReflectsLimits
    expectedFunctor : M.Identifier
    link : C1S2.FunctorReflectsLimits.F decl ≡ expectedFunctor
    status : Core.Phase.Bool

mkFunctorReflectsLimitsAdapter :
  (d : C1S2.FunctorReflectsLimits) →
  (ef : M.Identifier) →
  (p : C1S2.FunctorReflectsLimits.F d ≡ ef) →
  FunctorReflectsLimitsAdapter
mkFunctorReflectsLimitsAdapter d ef p =
  record { decl = d ; expectedFunctor = ef ; link = p ; status = true }

isFilledFunctorReflectsLimits : FunctorReflectsLimitsAdapter → Core.Phase.Bool
isFilledFunctorReflectsLimits a = FunctorReflectsLimitsAdapter.status a

-- Categorical view for FunctorReflectsLimits
functorReflectsLimitsCategorical : FunctorReflectsLimitsAdapter →
  CategoricalAdapter {lzero} C1S2.FunctorReflectsLimits
functorReflectsLimitsCategorical adapt =
  mkCategoricalAdapter C1S2.FunctorReflectsLimits (λ _ → FunctorReflectsLimitsAdapter.decl adapt)

-- Functor creates limits
record FunctorCreatesLimitsAdapter : Set₁ where
  field
    decl : C1S2.FunctorCreatesLimits
    expectedFunctor : M.Identifier
    link : C1S2.FunctorCreatesLimits.F decl ≡ expectedFunctor
    status : Core.Phase.Bool

mkFunctorCreatesLimitsAdapter :
  (d : C1S2.FunctorCreatesLimits) →
  (ef : M.Identifier) →
  (p : C1S2.FunctorCreatesLimits.F d ≡ ef) →
  FunctorCreatesLimitsAdapter
mkFunctorCreatesLimitsAdapter d ef p =
  record { decl = d ; expectedFunctor = ef ; link = p ; status = true }

isFilledFunctorCreatesLimits : FunctorCreatesLimitsAdapter → Core.Phase.Bool
isFilledFunctorCreatesLimits a = FunctorCreatesLimitsAdapter.status a

-- Categorical view for FunctorCreatesLimits
functorCreatesLimitsCategorical : FunctorCreatesLimitsAdapter →
  CategoricalAdapter {lzero} C1S2.FunctorCreatesLimits
functorCreatesLimitsCategorical adapt =
  mkCategoricalAdapter C1S2.FunctorCreatesLimits (λ _ → FunctorCreatesLimitsAdapter.decl adapt)

-- Theorem: Creation implies reflection
record CreationImpliesReflectionAdapter : Set₁ where
  field
    decl : C1S2.CreationImpliesReflection
    expectedFunctor : M.Identifier
    link : C1S2.CreationImpliesReflection.F decl ≡ expectedFunctor
    status : Core.Phase.Bool

mkCreationImpliesReflectionAdapter :
  (d : C1S2.CreationImpliesReflection) →
  (ef : M.Identifier) →
  (p : C1S2.CreationImpliesReflection.F d ≡ ef) →
  CreationImpliesReflectionAdapter
mkCreationImpliesReflectionAdapter d ef p =
  record { decl = d ; expectedFunctor = ef ; link = p ; status = true }

isFilledCreationImpliesReflection : CreationImpliesReflectionAdapter → Core.Phase.Bool
isFilledCreationImpliesReflection a = CreationImpliesReflectionAdapter.status a

-- Categorical view for CreationImpliesReflection
creationImpliesReflectionCategorical : CreationImpliesReflectionAdapter →
  CategoricalAdapter {lzero} C1S2.CreationImpliesReflection
creationImpliesReflectionCategorical adapt =
  mkCategoricalAdapter C1S2.CreationImpliesReflection (λ _ → CreationImpliesReflectionAdapter.decl adapt)

-- Theorem: Isomorphisms of categories reflect limits
record IsomorphismsOfCategoriesReflectLimitsAdapter : Set₁ where
  field
    decl : C1S2.IsomorphismsOfCategoriesReflectLimits
    expectedFunctor : M.Identifier
    link : C1S2.IsomorphismsOfCategoriesReflectLimits.F decl ≡ expectedFunctor
    status : Core.Phase.Bool

mkIsomorphismsOfCategoriesReflectLimitsAdapter :
  (d : C1S2.IsomorphismsOfCategoriesReflectLimits) →
  (ef : M.Identifier) →
  (p : C1S2.IsomorphismsOfCategoriesReflectLimits.F d ≡ ef) →
  IsomorphismsOfCategoriesReflectLimitsAdapter
mkIsomorphismsOfCategoriesReflectLimitsAdapter d ef p =
  record { decl = d ; expectedFunctor = ef ; link = p ; status = true }

isFilledIsomorphismsOfCategoriesReflectLimits : IsomorphismsOfCategoriesReflectLimitsAdapter → Core.Phase.Bool
isFilledIsomorphismsOfCategoriesReflectLimits a = IsomorphismsOfCategoriesReflectLimitsAdapter.status a

-- Categorical view for IsomorphismsOfCategoriesReflectLimits
isomorphismsOfCategoriesReflectLimitsCategorical : IsomorphismsOfCategoriesReflectLimitsAdapter →
  CategoricalAdapter {lzero} C1S2.IsomorphismsOfCategoriesReflectLimits
isomorphismsOfCategoriesReflectLimitsCategorical adapt =
  mkCategoricalAdapter C1S2.IsomorphismsOfCategoriesReflectLimits (λ _ → IsomorphismsOfCategoriesReflectLimitsAdapter.decl adapt)

-- Theorem: Right adjoints preserve limits
record RightAdjointsPreserveLimits_L2Adapter : Set₁ where
  field
    decl : C1S2.RightAdjointsPreserveLimits_L2
    expectedFunctor : M.Identifier
    link : C1S2.RightAdjointsPreserveLimits_L2.F decl ≡ expectedFunctor
    status : Core.Phase.Bool

mkRightAdjointsPreserveLimits_L2Adapter :
  (d : C1S2.RightAdjointsPreserveLimits_L2) →
  (ef : M.Identifier) →
  (p : C1S2.RightAdjointsPreserveLimits_L2.F d ≡ ef) →
  RightAdjointsPreserveLimits_L2Adapter
mkRightAdjointsPreserveLimits_L2Adapter d ef p =
  record { decl = d ; expectedFunctor = ef ; link = p ; status = true }

isFilledRightAdjointsPreserveLimits_L2 : RightAdjointsPreserveLimits_L2Adapter → Core.Phase.Bool
isFilledRightAdjointsPreserveLimits_L2 a = RightAdjointsPreserveLimits_L2Adapter.status a

-- Categorical view for RightAdjointsPreserveLimits_L2
rightAdjointsPreserveLimits_L2Categorical : RightAdjointsPreserveLimits_L2Adapter →
  CategoricalAdapter {lzero} C1S2.RightAdjointsPreserveLimits_L2
rightAdjointsPreserveLimits_L2Categorical adapt =
  mkCategoricalAdapter C1S2.RightAdjointsPreserveLimits_L2 (λ _ → RightAdjointsPreserveLimits_L2Adapter.decl adapt)

------------------------------------------------------------------------
-- Yoneda Lemma (Chapter1.Level1sub8)
------------------------------------------------------------------------

-- Internal Yoneda embedding
record InternalYonedaEmbeddingAdapter : Set₁ where
  field
    decl : C1S8.InternalYonedaEmbedding
    expectedInternalCategory : C1S8.InternalCategory
    link : C1S8.InternalYonedaEmbedding.internalCategory decl ≡ expectedInternalCategory
    status : Core.Phase.Bool

mkInternalYonedaEmbeddingAdapter :
  (d : C1S8.InternalYonedaEmbedding) →
  (eic : C1S8.InternalCategory) →
  (p : C1S8.InternalYonedaEmbedding.internalCategory d ≡ eic) →
  InternalYonedaEmbeddingAdapter
mkInternalYonedaEmbeddingAdapter d eic p =
  record { decl = d ; expectedInternalCategory = eic ; link = p ; status = true }

isFilledInternalYonedaEmbedding : InternalYonedaEmbeddingAdapter → Core.Phase.Bool
isFilledInternalYonedaEmbedding a = InternalYonedaEmbeddingAdapter.status a

-- Categorical view for InternalYonedaEmbedding
internalYonedaEmbeddingCategorical : InternalYonedaEmbeddingAdapter →
  CategoricalAdapter {lzero} C1S8.InternalYonedaEmbedding
internalYonedaEmbeddingCategorical adapt =
  mkCategoricalAdapter C1S8.InternalYonedaEmbedding (λ _ → InternalYonedaEmbeddingAdapter.decl adapt)

-- Internal Yoneda lemma theorem
record InternalYonedaLemmaAdapter : Set₁ where
  field
    decl : C1S8.InternalYonedaLemma
    expectedInternalCategory : C1S8.InternalCategory
    expectedPresheaf : C1S8.InternalPresheaf
    link1 : C1S8.InternalYonedaLemma.internalCategory decl ≡ expectedInternalCategory
    link2 : C1S8.InternalYonedaLemma.presheaf decl ≡ expectedPresheaf
    status : Core.Phase.Bool

mkInternalYonedaLemmaAdapter :
  (d : C1S8.InternalYonedaLemma) →
  (eic : C1S8.InternalCategory) →
  (ep : C1S8.InternalPresheaf) →
  (p1 : C1S8.InternalYonedaLemma.internalCategory d ≡ eic) →
  (p2 : C1S8.InternalYonedaLemma.presheaf d ≡ ep) →
  InternalYonedaLemmaAdapter
mkInternalYonedaLemmaAdapter d eic ep p1 p2 =
  record { decl = d ; expectedInternalCategory = eic ; expectedPresheaf = ep ; link1 = p1 ; link2 = p2 ; status = true }

isFilledInternalYonedaLemma : InternalYonedaLemmaAdapter → Core.Phase.Bool
isFilledInternalYonedaLemma a = InternalYonedaLemmaAdapter.status a

-- Categorical view for InternalYonedaLemma
internalYonedaLemmaCategorical : InternalYonedaLemmaAdapter →
  CategoricalAdapter {lzero} C1S8.InternalYonedaLemma
internalYonedaLemmaCategorical adapt =
  mkCategoricalAdapter C1S8.InternalYonedaLemma (λ _ → InternalYonedaLemmaAdapter.decl adapt)

------------------------------------------------------------------------
-- Kan Extensions (Chapter1.Level1sub3)
------------------------------------------------------------------------

-- Kan extension context
record KanExtensionContextAdapter : Set₁ where
  field
    decl : C1S3.KanExtensionContext
    expectedK : M.Identifier
    expectedT : M.Identifier
    link1 : C1S3.KanExtensionContext.K decl ≡ expectedK
    link2 : C1S3.KanExtensionContext.T decl ≡ expectedT
    status : Core.Phase.Bool

mkKanExtensionContextAdapter :
  (d : C1S3.KanExtensionContext) →
  (ek : M.Identifier) →
  (et : M.Identifier) →
  (p1 : C1S3.KanExtensionContext.K d ≡ ek) →
  (p2 : C1S3.KanExtensionContext.T d ≡ et) →
  KanExtensionContextAdapter
mkKanExtensionContextAdapter d ek et p1 p2 =
  record { decl = d ; expectedK = ek ; expectedT = et ; link1 = p1 ; link2 = p2 ; status = true }

isFilledKanExtensionContext : KanExtensionContextAdapter → Core.Phase.Bool
isFilledKanExtensionContext a = KanExtensionContextAdapter.status a

-- Categorical view for KanExtensionContext
kanExtensionContextCategorical : KanExtensionContextAdapter →
  CategoricalAdapter {lzero} C1S3.KanExtensionContext
kanExtensionContextCategorical adapt =
  mkCategoricalAdapter C1S3.KanExtensionContext (λ _ → KanExtensionContextAdapter.decl adapt)

-- Left Kan candidate
record LeftKanCandidateAdapter : Set₁ where
  field
    decl : C1S3.LeftKanCandidate
    expectedM : M.Identifier
    link : C1S3.LeftKanCandidate.M decl ≡ expectedM
    status : Core.Phase.Bool

mkLeftKanCandidateAdapter :
  (d : C1S3.LeftKanCandidate) →
  (em : M.Identifier) →
  (p : C1S3.LeftKanCandidate.M d ≡ em) →
  LeftKanCandidateAdapter
mkLeftKanCandidateAdapter d em p =
  record { decl = d ; expectedM = em ; link = p ; status = true }

isFilledLeftKanCandidate : LeftKanCandidateAdapter → Core.Phase.Bool
isFilledLeftKanCandidate a = LeftKanCandidateAdapter.status a

-- Categorical view for LeftKanCandidate
leftKanCandidateCategorical : LeftKanCandidateAdapter →
  CategoricalAdapter {lzero} C1S3.LeftKanCandidate
leftKanCandidateCategorical adapt =
  mkCategoricalAdapter C1S3.LeftKanCandidate (λ _ → LeftKanCandidateAdapter.decl adapt)

-- Right Kan candidate
record RightKanCandidateAdapter : Set₁ where
  field
    decl : C1S3.RightKanCandidate
    expectedM : M.Identifier
    link : C1S3.RightKanCandidate.M decl ≡ expectedM
    status : Core.Phase.Bool

mkRightKanCandidateAdapter :
  (d : C1S3.RightKanCandidate) →
  (em : M.Identifier) →
  (p : C1S3.RightKanCandidate.M d ≡ em) →
  RightKanCandidateAdapter
mkRightKanCandidateAdapter d em p =
  record { decl = d ; expectedM = em ; link = p ; status = true }

isFilledRightKanCandidate : RightKanCandidateAdapter → Core.Phase.Bool
isFilledRightKanCandidate a = RightKanCandidateAdapter.status a

-- Categorical view for RightKanCandidate
rightKanCandidateCategorical : RightKanCandidateAdapter →
  CategoricalAdapter {lzero} C1S3.RightKanCandidate
rightKanCandidateCategorical adapt =
  mkCategoricalAdapter C1S3.RightKanCandidate (λ _ → RightKanCandidateAdapter.decl adapt)

-- Theorem: Left Kan extension is initial object
record LeftKanExtensionIsInitialObjectAdapter : Set₁ where
  field
    decl : C1S3.LeftKanExtensionIsInitialObject
    expectedK : M.Identifier
    expectedT : M.Identifier
    link1 : C1S3.LeftKanExtensionIsInitialObject.K decl ≡ expectedK
    link2 : C1S3.LeftKanExtensionIsInitialObject.T decl ≡ expectedT
    status : Core.Phase.Bool

mkLeftKanExtensionIsInitialObjectAdapter :
  (d : C1S3.LeftKanExtensionIsInitialObject) →
  (ek : M.Identifier) →
  (et : M.Identifier) →
  (p1 : C1S3.LeftKanExtensionIsInitialObject.K d ≡ ek) →
  (p2 : C1S3.LeftKanExtensionIsInitialObject.T d ≡ et) →
  LeftKanExtensionIsInitialObjectAdapter
mkLeftKanExtensionIsInitialObjectAdapter d ek et p1 p2 =
  record { decl = d ; expectedK = ek ; expectedT = et ; link1 = p1 ; link2 = p2 ; status = true }

isFilledLeftKanExtensionIsInitialObject : LeftKanExtensionIsInitialObjectAdapter → Core.Phase.Bool
isFilledLeftKanExtensionIsInitialObject a = LeftKanExtensionIsInitialObjectAdapter.status a

-- Categorical view for LeftKanExtensionIsInitialObject
leftKanExtensionIsInitialObjectCategorical : LeftKanExtensionIsInitialObjectAdapter →
  CategoricalAdapter {lzero} C1S3.LeftKanExtensionIsInitialObject
leftKanExtensionIsInitialObjectCategorical adapt =
  mkCategoricalAdapter C1S3.LeftKanExtensionIsInitialObject (λ _ → LeftKanExtensionIsInitialObjectAdapter.decl adapt)

-- Theorem: Right Kan extension is terminal object
record RightKanExtensionIsTerminalObjectAdapter : Set₁ where
  field
    decl : C1S3.RightKanExtensionIsTerminalObject
    expectedK : M.Identifier
    expectedT : M.Identifier
    link1 : C1S3.RightKanExtensionIsTerminalObject.K decl ≡ expectedK
    link2 : C1S3.RightKanExtensionIsTerminalObject.T decl ≡ expectedT
    status : Core.Phase.Bool

mkRightKanExtensionIsTerminalObjectAdapter :
  (d : C1S3.RightKanExtensionIsTerminalObject) →
  (ek : M.Identifier) →
  (et : M.Identifier) →
  (p1 : C1S3.RightKanExtensionIsTerminalObject.K d ≡ ek) →
  (p2 : C1S3.RightKanExtensionIsTerminalObject.T d ≡ et) →
  RightKanExtensionIsTerminalObjectAdapter
mkRightKanExtensionIsTerminalObjectAdapter d ek et p1 p2 =
  record { decl = d ; expectedK = ek ; expectedT = et ; link1 = p1 ; link2 = p2 ; status = true }

isFilledRightKanExtensionIsTerminalObject : RightKanExtensionIsTerminalObjectAdapter → Core.Phase.Bool
isFilledRightKanExtensionIsTerminalObject a = RightKanExtensionIsTerminalObjectAdapter.status a

-- Categorical view for RightKanExtensionIsTerminalObject
rightKanExtensionIsTerminalObjectCategorical : RightKanExtensionIsTerminalObjectAdapter →
  CategoricalAdapter {lzero} C1S3.RightKanExtensionIsTerminalObject
rightKanExtensionIsTerminalObjectCategorical adapt =
  mkCategoricalAdapter C1S3.RightKanExtensionIsTerminalObject (λ _ → RightKanExtensionIsTerminalObjectAdapter.decl adapt)

-- Theorem: Pointwise Kan formula
record PointwiseKanFormulaTheoremAdapter : Set₁ where
  field
    decl : C1S3.PointwiseKanFormulaTheorem
    expectedK : M.Identifier
    expectedT : M.Identifier
    link1 : C1S3.PointwiseKanFormulaTheorem.K decl ≡ expectedK
    link2 : C1S3.PointwiseKanFormulaTheorem.T decl ≡ expectedT
    status : Core.Phase.Bool

mkPointwiseKanFormulaTheoremAdapter :
  (d : C1S3.PointwiseKanFormulaTheorem) →
  (ek : M.Identifier) →
  (et : M.Identifier) →
  (p1 : C1S3.PointwiseKanFormulaTheorem.K d ≡ ek) →
  (p2 : C1S3.PointwiseKanFormulaTheorem.T d ≡ et) →
  PointwiseKanFormulaTheoremAdapter
mkPointwiseKanFormulaTheoremAdapter d ek et p1 p2 =
  record { decl = d ; expectedK = ek ; expectedT = et ; link1 = p1 ; link2 = p2 ; status = true }

isFilledPointwiseKanFormulaTheorem : PointwiseKanFormulaTheoremAdapter → Core.Phase.Bool
isFilledPointwiseKanFormulaTheorem a = PointwiseKanFormulaTheoremAdapter.status a

-- Categorical view for PointwiseKanFormulaTheorem
pointwiseKanFormulaTheoremCategorical : PointwiseKanFormulaTheoremAdapter →
  CategoricalAdapter {lzero} C1S3.PointwiseKanFormulaTheorem
pointwiseKanFormulaTheoremCategorical adapt =
  mkCategoricalAdapter C1S3.PointwiseKanFormulaTheorem (λ _ → PointwiseKanFormulaTheoremAdapter.decl adapt)

-- Theorem: Adjoints as Kan extensions
record AdjointsAsKanExtensionsAdapter : Set₁ where
  field
    decl : C1S3.AdjointsAsKanExtensions
    expectedF : M.Identifier
    expectedG : M.Identifier
    link1 : C1S3.AdjointsAsKanExtensions.F decl ≡ expectedF
    link2 : C1S3.AdjointsAsKanExtensions.G decl ≡ expectedG
    status : Core.Phase.Bool

mkAdjointsAsKanExtensionsAdapter :
  (d : C1S3.AdjointsAsKanExtensions) →
  (ef : M.Identifier) →
  (eg : M.Identifier) →
  (p1 : C1S3.AdjointsAsKanExtensions.F d ≡ ef) →
  (p2 : C1S3.AdjointsAsKanExtensions.G d ≡ eg) →
  AdjointsAsKanExtensionsAdapter
mkAdjointsAsKanExtensionsAdapter d ef eg p1 p2 =
  record { decl = d ; expectedF = ef ; expectedG = eg ; link1 = p1 ; link2 = p2 ; status = true }

isFilledAdjointsAsKanExtensions : AdjointsAsKanExtensionsAdapter → Core.Phase.Bool
isFilledAdjointsAsKanExtensions a = AdjointsAsKanExtensionsAdapter.status a

-- Categorical view for AdjointsAsKanExtensions
adjointsAsKanExtensionsCategorical : AdjointsAsKanExtensionsAdapter →
  CategoricalAdapter {lzero} C1S3.AdjointsAsKanExtensions
adjointsAsKanExtensionsCategorical adapt =
  mkCategoricalAdapter C1S3.AdjointsAsKanExtensions (λ _ → AdjointsAsKanExtensionsAdapter.decl adapt)

------------------------------------------------------------------------
-- Adjoint Functor Theorems (Chapter1.Level1sub3)
------------------------------------------------------------------------

-- Adjoint functor theorem (right version - dual theorem)
record AdjointFunctorTheoremRightAdapter : Set₁ where
  field
    decl : C1S3.AdjointFunctorTheoremRight
    status : Core.Phase.Bool

mkAdjointFunctorTheoremRightAdapter :
  (d : C1S3.AdjointFunctorTheoremRight) →
  AdjointFunctorTheoremRightAdapter
mkAdjointFunctorTheoremRightAdapter d =
  record { decl = d ; status = true }

isFilledAdjointFunctorTheoremRight : AdjointFunctorTheoremRightAdapter → Core.Phase.Bool
isFilledAdjointFunctorTheoremRight a = AdjointFunctorTheoremRightAdapter.status a

-- Categorical view for AdjointFunctorTheoremRight
adjointFunctorTheoremRightCategorical : AdjointFunctorTheoremRightAdapter →
  CategoricalAdapter {lzero} C1S3.AdjointFunctorTheoremRight
adjointFunctorTheoremRightCategorical adapt =
  mkCategoricalAdapter C1S3.AdjointFunctorTheoremRight (λ _ → AdjointFunctorTheoremRightAdapter.decl adapt)

------------------------------------------------------------------------
-- Grothendieck Fibrations (Chapter2.Level2sub8)
------------------------------------------------------------------------

-- Fibration declaration (projection functor with Cartesian lifts)
record FibrationDeclarationAdapter : Set₁ where
  field
    decl : C2S8.FibrationDeclaration
    expectedProjection : M.Identifier
    link : C2S8.FibrationProjectionFunctor.projectionFunctor (C2S8.FibrationDeclaration.projectionFunctor decl) ≡ expectedProjection
    status : Core.Phase.Bool

mkFibrationDeclarationAdapter :
  (d : C2S8.FibrationDeclaration) →
  (ep : M.Identifier) →
  (p : C2S8.FibrationProjectionFunctor.projectionFunctor (C2S8.FibrationDeclaration.projectionFunctor d) ≡ ep) →
  (f : ⊤ → C2S8.FibrationDeclaration) →
  FibrationDeclarationAdapter
mkFibrationDeclarationAdapter d ep p f =
  record { decl = d ; expectedProjection = ep ; link = p ; status = true }

fibrationDeclarationCategorical : FibrationDeclarationAdapter → CategoricalAdapter {lsuc lzero} C2S8.FibrationDeclaration
fibrationDeclarationCategorical adapt = mkCategoricalAdapter C2S8.FibrationDeclaration (λ _ → FibrationDeclarationAdapter.decl adapt)

isFilledFibrationDeclaration : FibrationDeclarationAdapter → Core.Phase.Bool
isFilledFibrationDeclaration a = FibrationDeclarationAdapter.status a

-- Cartesian arrow (universal lifting property)
record CartesianArrowAdapter : Set₁ where
  field
    decl : C2S8.CartesianArrow
    expectedArrow : M.Identifier
    link : C2S8.CartesianArrow.arrow decl ≡ expectedArrow
    status : Core.Phase.Bool

mkCartesianArrowAdapter :
  (d : C2S8.CartesianArrow) →
  (ea : M.Identifier) →
  (p : C2S8.CartesianArrow.arrow d ≡ ea) →
  (f : ⊤ → C2S8.CartesianArrow) →
  CartesianArrowAdapter
mkCartesianArrowAdapter d ea p f =
  record { decl = d ; expectedArrow = ea ; link = p ; status = true }

cartesianArrowCategorical : CartesianArrowAdapter → CategoricalAdapter {lsuc lzero} C2S8.CartesianArrow
cartesianArrowCategorical adapt = mkCategoricalAdapter C2S8.CartesianArrow (λ _ → CartesianArrowAdapter.decl adapt)

isFilledCartesianArrow : CartesianArrowAdapter → Core.Phase.Bool
isFilledCartesianArrow a = CartesianArrowAdapter.status a

-- Cartesian functor between fibrations
record CartesianFunctorDeclarationAdapter : Set₁ where
  field
    decl : C2S8.CartesianFunctorDeclaration
    expectedFunctor : M.Identifier
    link : C2S8.CartesianFunctorDeclaration.underlyingFunctor decl ≡ expectedFunctor
    status : Core.Phase.Bool

mkCartesianFunctorDeclarationAdapter :
  (d : C2S8.CartesianFunctorDeclaration) →
  (ef : M.Identifier) →
  (p : C2S8.CartesianFunctorDeclaration.underlyingFunctor d ≡ ef) →
  (f : ⊤ → C2S8.CartesianFunctorDeclaration) →
  CartesianFunctorDeclarationAdapter
mkCartesianFunctorDeclarationAdapter d ef p f =
  record { decl = d ; expectedFunctor = ef ; link = p ; status = true }

cartesianFunctorDeclarationCategorical : CartesianFunctorDeclarationAdapter → CategoricalAdapter {lsuc lzero} C2S8.CartesianFunctorDeclaration
cartesianFunctorDeclarationCategorical adapt = mkCategoricalAdapter C2S8.CartesianFunctorDeclaration (λ _ → CartesianFunctorDeclarationAdapter.decl adapt)

isFilledCartesianFunctorDeclaration : CartesianFunctorDeclarationAdapter → Core.Phase.Bool
isFilledCartesianFunctorDeclaration a = CartesianFunctorDeclarationAdapter.status a

-- Category of fibrations over a base
record CategoryOfFibrationsAdapter : Set₁ where
  field
    decl : C2S8.CategoryOfFibrations
    expectedBase : C1S3.CategoryDeclaration
    link : C2S8.CategoryOfFibrations.baseCategory decl ≡ expectedBase
    status : Core.Phase.Bool

mkCategoryOfFibrationsAdapter :
  (d : C2S8.CategoryOfFibrations) →
  (eb : C1S3.CategoryDeclaration) →
  (p : C2S8.CategoryOfFibrations.baseCategory d ≡ eb) →
  (f : ⊤ → C2S8.CategoryOfFibrations) →
  CategoryOfFibrationsAdapter
mkCategoryOfFibrationsAdapter d eb p f =
  record { decl = d ; expectedBase = eb ; link = p ; status = true }

categoryOfFibrationsCategorical : CategoryOfFibrationsAdapter → CategoricalAdapter {lsuc lzero} C2S8.CategoryOfFibrations
categoryOfFibrationsCategorical adapt = mkCategoricalAdapter C2S8.CategoryOfFibrations (λ _ → CategoryOfFibrationsAdapter.decl adapt)

isFilledCategoryOfFibrations : CategoryOfFibrationsAdapter → Core.Phase.Bool
isFilledCategoryOfFibrations a = CategoryOfFibrationsAdapter.status a

-- Pseudofunctor from fibration (unpacking)
record PseudofunctorFromFibrationAdapter : Set₁ where
  field
    decl : C2S8.PseudofunctorFromFibration
    status : Core.Phase.Bool

mkPseudofunctorFromFibrationAdapter :
  (d : C2S8.PseudofunctorFromFibration) →
  (f : ⊤ → C2S8.PseudofunctorFromFibration) →
  PseudofunctorFromFibrationAdapter
mkPseudofunctorFromFibrationAdapter d f =
  record { decl = d ; status = true }

pseudofunctorFromFibrationCategorical : PseudofunctorFromFibrationAdapter → CategoricalAdapter {lsuc lzero} C2S8.PseudofunctorFromFibration
pseudofunctorFromFibrationCategorical adapt = mkCategoricalAdapter C2S8.PseudofunctorFromFibration (λ _ → PseudofunctorFromFibrationAdapter.decl adapt)

isFilledPseudofunctorFromFibration : PseudofunctorFromFibrationAdapter → Core.Phase.Bool
isFilledPseudofunctorFromFibration a = PseudofunctorFromFibrationAdapter.status a

-- Grothendieck construction (category of elements)
record GrothendieckConstructionAdapter : Set₁ where
  field
    decl : C2S8.GrothendieckConstruction
    expectedTotal : C1S3.CategoryDeclaration
    link : C2S8.GrothendieckConstruction.totalCategory decl ≡ expectedTotal
    status : Core.Phase.Bool

mkGrothendieckConstructionAdapter :
  (d : C2S8.GrothendieckConstruction) →
  (et : C1S3.CategoryDeclaration) →
  (p : C2S8.GrothendieckConstruction.totalCategory d ≡ et) →
  (f : ⊤ → C2S8.GrothendieckConstruction) →
  GrothendieckConstructionAdapter
mkGrothendieckConstructionAdapter d et p f =
  record { decl = d ; expectedTotal = et ; link = p ; status = true }

grothendieckConstructionCategorical : GrothendieckConstructionAdapter → CategoricalAdapter {lsuc lzero} C2S8.GrothendieckConstruction
grothendieckConstructionCategorical adapt = mkCategoricalAdapter C2S8.GrothendieckConstruction (λ _ → GrothendieckConstructionAdapter.decl adapt)

isFilledGrothendieckConstruction : GrothendieckConstructionAdapter → Core.Phase.Bool
isFilledGrothendieckConstruction a = GrothendieckConstructionAdapter.status a

-- Grothendieck equivalence theorem (2-equivalence)
record GrothendieckEquivalenceTheoremAdapter : Set₁ where
  field
    decl : C2S8.GrothendieckEquivalenceTheorem
    expectedBase : C1S3.CategoryDeclaration
    link : C2S8.GrothendieckEquivalenceTheorem.baseCategory decl ≡ expectedBase
    status : Core.Phase.Bool

mkGrothendieckEquivalenceTheoremAdapter :
  (d : C2S8.GrothendieckEquivalenceTheorem) →
  (eb : C1S3.CategoryDeclaration) →
  (p : C2S8.GrothendieckEquivalenceTheorem.baseCategory d ≡ eb) →
  (f : ⊤ → C2S8.GrothendieckEquivalenceTheorem) →
  GrothendieckEquivalenceTheoremAdapter
mkGrothendieckEquivalenceTheoremAdapter d eb p f =
  record { decl = d ; expectedBase = eb ; link = p ; status = true }

grothendieckEquivalenceTheoremCategorical : GrothendieckEquivalenceTheoremAdapter → CategoricalAdapter {lsuc lzero} C2S8.GrothendieckEquivalenceTheorem
grothendieckEquivalenceTheoremCategorical adapt = mkCategoricalAdapter C2S8.GrothendieckEquivalenceTheorem (λ _ → GrothendieckEquivalenceTheoremAdapter.decl adapt)

isFilledGrothendieckEquivalenceTheorem : GrothendieckEquivalenceTheoremAdapter → Core.Phase.Bool
isFilledGrothendieckEquivalenceTheorem a = GrothendieckEquivalenceTheoremAdapter.status a

-- Fibred adjunction (pointwise on fibres)
record FibredAdjunctionDeclarationAdapter : Set₁ where
  field
    decl : C2S8.FibredAdjunctionDeclaration
    expectedLeft : C2S8.CartesianFunctorDeclaration
    expectedRight : C2S8.CartesianFunctorDeclaration
    link1 : C2S8.FibredAdjunctionDeclaration.leftAdjoint decl ≡ expectedLeft
    link2 : C2S8.FibredAdjunctionDeclaration.rightAdjoint decl ≡ expectedRight
    status : Core.Phase.Bool

mkFibredAdjunctionDeclarationAdapter :
  (d : C2S8.FibredAdjunctionDeclaration) →
  (el : C2S8.CartesianFunctorDeclaration) →
  (er : C2S8.CartesianFunctorDeclaration) →
  (p1 : C2S8.FibredAdjunctionDeclaration.leftAdjoint d ≡ el) →
  (p2 : C2S8.FibredAdjunctionDeclaration.rightAdjoint d ≡ er) →
  (f : ⊤ → C2S8.FibredAdjunctionDeclaration) →
  FibredAdjunctionDeclarationAdapter
mkFibredAdjunctionDeclarationAdapter d el er p1 p2 f =
  record { decl = d ; expectedLeft = el ; expectedRight = er ; link1 = p1 ; link2 = p2 ; status = true }

fibredAdjunctionDeclarationCategorical : FibredAdjunctionDeclarationAdapter → CategoricalAdapter {lsuc lzero} C2S8.FibredAdjunctionDeclaration
fibredAdjunctionDeclarationCategorical adapt = mkCategoricalAdapter C2S8.FibredAdjunctionDeclaration (λ _ → FibredAdjunctionDeclarationAdapter.decl adapt)

isFilledFibredAdjunctionDeclaration : FibredAdjunctionDeclarationAdapter → Core.Phase.Bool
isFilledFibredAdjunctionDeclaration a = FibredAdjunctionDeclarationAdapter.status a

-- Beck-Chevalley condition (coherence for fibred adjunctions)
record BeckChevalleyConditionAdapter : Set₁ where
  field
    decl : C2S8.BeckChevalleyCondition
    status : Core.Phase.Bool

mkBeckChevalleyConditionAdapter :
  (d : C2S8.BeckChevalleyCondition) →
  (f : ⊤ → C2S8.BeckChevalleyCondition) →
  BeckChevalleyConditionAdapter
mkBeckChevalleyConditionAdapter d f =
  record { decl = d ; status = true }

beckChevalleyConditionCategorical : BeckChevalleyConditionAdapter → CategoricalAdapter {lsuc lzero} C2S8.BeckChevalleyCondition
beckChevalleyConditionCategorical adapt = mkCategoricalAdapter C2S8.BeckChevalleyCondition (λ _ → BeckChevalleyConditionAdapter.decl adapt)

isFilledBeckChevalleyCondition : BeckChevalleyConditionAdapter → Core.Phase.Bool
isFilledBeckChevalleyCondition a = BeckChevalleyConditionAdapter.status a

-- Fibration completeness criterion theorem
record FibrationCompletenessCriterionTheoremAdapter : Set₁ where
  field
    decl : C2S8.FibrationCompletenessCriterionTheorem
    status : Core.Phase.Bool

mkFibrationCompletenessCriterionTheoremAdapter :
  (d : C2S8.FibrationCompletenessCriterionTheorem) →
  (f : ⊤ → C2S8.FibrationCompletenessCriterionTheorem) →
  FibrationCompletenessCriterionTheoremAdapter
mkFibrationCompletenessCriterionTheoremAdapter d f =
  record { decl = d ; status = true }

fibrationCompletenessCriterionTheoremCategorical : FibrationCompletenessCriterionTheoremAdapter → CategoricalAdapter {lsuc lzero} C2S8.FibrationCompletenessCriterionTheorem
fibrationCompletenessCriterionTheoremCategorical adapt = mkCategoricalAdapter C2S8.FibrationCompletenessCriterionTheorem (λ _ → FibrationCompletenessCriterionTheoremAdapter.decl adapt)

isFilledFibrationCompletenessCriterionTheorem : FibrationCompletenessCriterionTheoremAdapter → Core.Phase.Bool
isFilledFibrationCompletenessCriterionTheorem a = FibrationCompletenessCriterionTheoremAdapter.status a

-- Locally small fibration
record LocallySmallFibrationAdapter : Set₁ where
  field
    decl : C2S8.LocallySmallFibration
    status : Core.Phase.Bool

mkLocallySmallFibrationAdapter :
  (d : C2S8.LocallySmallFibration) →
  (f : ⊤ → C2S8.LocallySmallFibration) →
  LocallySmallFibrationAdapter
mkLocallySmallFibrationAdapter d f =
  record { decl = d ; status = true }

locallySmallFibrationCategorical : LocallySmallFibrationAdapter → CategoricalAdapter {lsuc lzero} C2S8.LocallySmallFibration
locallySmallFibrationCategorical adapt = mkCategoricalAdapter C2S8.LocallySmallFibration (λ _ → LocallySmallFibrationAdapter.decl adapt)

isFilledLocallySmallFibration : LocallySmallFibrationAdapter → Core.Phase.Bool
isFilledLocallySmallFibration a = LocallySmallFibrationAdapter.status a

-- Refined Grothendieck equivalence (for locally small fibrations)
record RefinedGrothendieckEquivalenceTheoremAdapter : Set₁ where
  field
    decl : C2S8.RefinedGrothendieckEquivalenceTheorem
    expectedBase : C1S3.CategoryDeclaration
    link : C2S8.RefinedGrothendieckEquivalenceTheorem.baseCategory decl ≡ expectedBase
    status : Core.Phase.Bool

mkRefinedGrothendieckEquivalenceTheoremAdapter :
  (d : C2S8.RefinedGrothendieckEquivalenceTheorem) →
  (eb : C1S3.CategoryDeclaration) →
  (p : C2S8.RefinedGrothendieckEquivalenceTheorem.baseCategory d ≡ eb) →
  (f : ⊤ → C2S8.RefinedGrothendieckEquivalenceTheorem) →
  RefinedGrothendieckEquivalenceTheoremAdapter
mkRefinedGrothendieckEquivalenceTheoremAdapter d eb p f =
  record { decl = d ; expectedBase = eb ; link = p ; status = true }

refinedGrothendieckEquivalenceTheoremCategorical : RefinedGrothendieckEquivalenceTheoremAdapter → CategoricalAdapter {lsuc lzero} C2S8.RefinedGrothendieckEquivalenceTheorem
refinedGrothendieckEquivalenceTheoremCategorical adapt = mkCategoricalAdapter C2S8.RefinedGrothendieckEquivalenceTheorem (λ _ → RefinedGrothendieckEquivalenceTheoremAdapter.decl adapt)

isFilledRefinedGrothendieckEquivalenceTheorem : RefinedGrothendieckEquivalenceTheoremAdapter → Core.Phase.Bool
isFilledRefinedGrothendieckEquivalenceTheorem a = RefinedGrothendieckEquivalenceTheoremAdapter.status a

-- Codomain fibration (canonical example)
record CodomainFibrationAdapter : Set₁ where
  field
    decl : C2S8.CodomainFibration
    expectedBase : C1S3.CategoryDeclaration
    link : C2S8.CodomainFibration.baseCategory decl ≡ expectedBase
    status : Core.Phase.Bool

mkCodomainFibrationAdapter :
  (d : C2S8.CodomainFibration) →
  (eb : C1S3.CategoryDeclaration) →
  (p : C2S8.CodomainFibration.baseCategory d ≡ eb) →
  (f : ⊤ → C2S8.CodomainFibration) →
  CodomainFibrationAdapter
mkCodomainFibrationAdapter d eb p f =
  record { decl = d ; expectedBase = eb ; link = p ; status = true }

codomainFibrationCategorical : CodomainFibrationAdapter → CategoricalAdapter {lsuc lzero} C2S8.CodomainFibration
codomainFibrationCategorical adapt = mkCategoricalAdapter C2S8.CodomainFibration (λ _ → CodomainFibrationAdapter.decl adapt)

isFilledCodomainFibration : CodomainFibrationAdapter → Core.Phase.Bool
isFilledCodomainFibration a = CodomainFibrationAdapter.status a

-- Lindenbaum-Tarski fibration (logic connection)
record LindenbaumTarskiFibrationAdapter : Set₁ where
  field
    decl : C2S8.LindenbaumTarskiFibration
    status : Core.Phase.Bool

mkLindenbaumTarskiFibrationAdapter :
  (d : C2S8.LindenbaumTarskiFibration) →
  (f : ⊤ → C2S8.LindenbaumTarskiFibration) →
  LindenbaumTarskiFibrationAdapter
mkLindenbaumTarskiFibrationAdapter d f =
  record { decl = d ; status = true }

lindenbaumTarskiFibrationCategorical : LindenbaumTarskiFibrationAdapter → CategoricalAdapter {lsuc lzero} C2S8.LindenbaumTarskiFibration
lindenbaumTarskiFibrationCategorical adapt = mkCategoricalAdapter C2S8.LindenbaumTarskiFibration (λ _ → LindenbaumTarskiFibrationAdapter.decl adapt)

isFilledLindenbaumTarskiFibration : LindenbaumTarskiFibrationAdapter → Core.Phase.Bool
isFilledLindenbaumTarskiFibration a = LindenbaumTarskiFibrationAdapter.status a

-- Families fibration (indexed sets)
record FamiliesFibrationAdapter : Set₁ where
  field
    decl : C2S8.FamiliesFibration
    expectedBase : C1S3.CategoryDeclaration
    link : C2S8.FamiliesFibration.baseCategory decl ≡ expectedBase
    status : Core.Phase.Bool

mkFamiliesFibrationAdapter :
  (d : C2S8.FamiliesFibration) →
  (eb : C1S3.CategoryDeclaration) →
  (p : C2S8.FamiliesFibration.baseCategory d ≡ eb) →
  (f : ⊤ → C2S8.FamiliesFibration) →
  FamiliesFibrationAdapter
mkFamiliesFibrationAdapter d eb p f =
  record { decl = d ; expectedBase = eb ; link = p ; status = true }

familiesFibrationCategorical : FamiliesFibrationAdapter → CategoricalAdapter {lsuc lzero} C2S8.FamiliesFibration
familiesFibrationCategorical adapt = mkCategoricalAdapter C2S8.FamiliesFibration (λ _ → FamiliesFibrationAdapter.decl adapt)

isFilledFamiliesFibration : FamiliesFibrationAdapter → Core.Phase.Bool
isFilledFamiliesFibration a = FamiliesFibrationAdapter.status a

------------------------------------------------------------------------
-- Abelian Categories (Chapter2.Level2sub1)
------------------------------------------------------------------------

-- Zero object property (initial and terminal)
record HasZeroObjectPropertyAdapter : Set₁ where
  field
    decl : C2S1.HasZeroObjectProperty
    expectedCategory : M.Identifier
    expectedZero : M.Identifier
    link1 : C2S1.HasZeroObjectProperty.category decl ≡ expectedCategory
    link2 : C2S1.HasZeroObjectProperty.zeroObj decl ≡ expectedZero
    status : Core.Phase.Bool

mkHasZeroObjectPropertyAdapter :
  (d : C2S1.HasZeroObjectProperty) →
  (ec : M.Identifier) →
  (ez : M.Identifier) →
  (p1 : C2S1.HasZeroObjectProperty.category d ≡ ec) →
  (p2 : C2S1.HasZeroObjectProperty.zeroObj d ≡ ez) →
  HasZeroObjectPropertyAdapter
mkHasZeroObjectPropertyAdapter d ec ez p1 p2 =
  record { decl = d ; expectedCategory = ec ; expectedZero = ez ; link1 = p1 ; link2 = p2 ; status = true }

isFilledHasZeroObjectProperty : HasZeroObjectPropertyAdapter → Core.Phase.Bool
isFilledHasZeroObjectProperty a = HasZeroObjectPropertyAdapter.status a

-- Kernel as equalizer definition
record KernelAsEqualizerDefinitionAdapter : Set₁ where
  field
    decl : C2S1.KernelAsEqualizerDefinition
    expectedMorphism : M.Identifier
    expectedKernel : M.Identifier
    link1 : C2S1.KernelAsEqualizerDefinition.morphism decl ≡ expectedMorphism
    link2 : C2S1.KernelAsEqualizerDefinition.equalizerObject decl ≡ expectedKernel
    status : Core.Phase.Bool

mkKernelAsEqualizerDefinitionAdapter :
  (d : C2S1.KernelAsEqualizerDefinition) →
  (em : M.Identifier) →
  (ek : M.Identifier) →
  (p1 : C2S1.KernelAsEqualizerDefinition.morphism d ≡ em) →
  (p2 : C2S1.KernelAsEqualizerDefinition.equalizerObject d ≡ ek) →
  KernelAsEqualizerDefinitionAdapter
mkKernelAsEqualizerDefinitionAdapter d em ek p1 p2 =
  record { decl = d ; expectedMorphism = em ; expectedKernel = ek ; link1 = p1 ; link2 = p2 ; status = true }

isFilledKernelAsEqualizerDefinition : KernelAsEqualizerDefinitionAdapter → Core.Phase.Bool
isFilledKernelAsEqualizerDefinition a = KernelAsEqualizerDefinitionAdapter.status a

-- Biproduct object (simultaneous product and coproduct)
record BiproductObjectAdapter : Set₁ where
  field
    decl : C2S1.BiproductObject
    expectedLeft : M.Identifier
    expectedRight : M.Identifier
    expectedObject : M.Identifier
    link1 : C2S1.BiproductObject.left decl ≡ expectedLeft
    link2 : C2S1.BiproductObject.right decl ≡ expectedRight
    link3 : C2S1.BiproductObject.object decl ≡ expectedObject
    status : Core.Phase.Bool

mkBiproductObjectAdapter :
  (d : C2S1.BiproductObject) →
  (el : M.Identifier) →
  (er : M.Identifier) →
  (eo : M.Identifier) →
  (p1 : C2S1.BiproductObject.left d ≡ el) →
  (p2 : C2S1.BiproductObject.right d ≡ er) →
  (p3 : C2S1.BiproductObject.object d ≡ eo) →
  BiproductObjectAdapter
mkBiproductObjectAdapter d el er eo p1 p2 p3 =
  record { decl = d ; expectedLeft = el ; expectedRight = er ; expectedObject = eo ; link1 = p1 ; link2 = p2 ; link3 = p3 ; status = true }

isFilledBiproductObject : BiproductObjectAdapter → Core.Phase.Bool
isFilledBiproductObject a = BiproductObjectAdapter.status a

-- Additive category declaration
record AdditiveCategoryDeclarationAdapter : Set₁ where
  field
    decl : C2S1.AdditiveCategoryDeclaration
    expectedCategory : M.Identifier
    link : C2S1.AdditiveCategoryDeclaration.category decl ≡ expectedCategory
    status : Core.Phase.Bool

mkAdditiveCategoryDeclarationAdapter :
  (d : C2S1.AdditiveCategoryDeclaration) →
  (ec : M.Identifier) →
  (p : C2S1.AdditiveCategoryDeclaration.category d ≡ ec) →
  AdditiveCategoryDeclarationAdapter
mkAdditiveCategoryDeclarationAdapter d ec p =
  record { decl = d ; expectedCategory = ec ; link = p ; status = true }

isFilledAdditiveCategoryDeclaration : AdditiveCategoryDeclarationAdapter → Core.Phase.Bool
isFilledAdditiveCategoryDeclaration a = AdditiveCategoryDeclarationAdapter.status a

-- Abelian category declaration (main definition)
record AbelianCategoryDeclarationAdapter : Set₁ where
  field
    decl : C2S1.AbelianCategoryDeclaration
    expectedCategory : M.Identifier
    link : C2S1.AbelianCategoryDeclaration.category decl ≡ expectedCategory
    status : Core.Phase.Bool

mkAbelianCategoryDeclarationAdapter :
  (d : C2S1.AbelianCategoryDeclaration) →
  (ec : M.Identifier) →
  (p : C2S1.AbelianCategoryDeclaration.category d ≡ ec) →
  AbelianCategoryDeclarationAdapter
mkAbelianCategoryDeclarationAdapter d ec p =
  record { decl = d ; expectedCategory = ec ; link = p ; status = true }

isFilledAbelianCategoryDeclaration : AbelianCategoryDeclarationAdapter → Core.Phase.Bool
isFilledAbelianCategoryDeclaration a = AbelianCategoryDeclarationAdapter.status a

-- First isomorphism theorem for abelian categories
record FirstIsomorphismForAbelianCategoriesTheoremAdapter : Set₁ where
  field
    decl : C2S1.FirstIsomorphismForAbelianCategoriesTheorem
    expectedCategory : M.Identifier
    expectedMorphism : M.Identifier
    link1 : C2S1.FirstIsomorphismForAbelianCategoriesTheorem.category decl ≡ expectedCategory
    link2 : C2S1.FirstIsomorphismForAbelianCategoriesTheorem.morphism decl ≡ expectedMorphism
    status : Core.Phase.Bool

mkFirstIsomorphismForAbelianCategoriesTheoremAdapter :
  (d : C2S1.FirstIsomorphismForAbelianCategoriesTheorem) →
  (ec : M.Identifier) →
  (em : M.Identifier) →
  (p1 : C2S1.FirstIsomorphismForAbelianCategoriesTheorem.category d ≡ ec) →
  (p2 : C2S1.FirstIsomorphismForAbelianCategoriesTheorem.morphism d ≡ em) →
  FirstIsomorphismForAbelianCategoriesTheoremAdapter
mkFirstIsomorphismForAbelianCategoriesTheoremAdapter d ec em p1 p2 =
  record { decl = d ; expectedCategory = ec ; expectedMorphism = em ; link1 = p1 ; link2 = p2 ; status = true }

isFilledFirstIsomorphismForAbelianCategoriesTheorem : FirstIsomorphismForAbelianCategoriesTheoremAdapter → Core.Phase.Bool
isFilledFirstIsomorphismForAbelianCategoriesTheorem a = FirstIsomorphismForAbelianCategoriesTheoremAdapter.status a

-- Normal monomorphism property
record NormalMonomorphismPropertyAdapter : Set₁ where
  field
    decl : C2S1.NormalMonomorphismProperty
    expectedMono : M.Identifier
    link : C2S1.NormalMonomorphismProperty.mono decl ≡ expectedMono
    status : Core.Phase.Bool

mkNormalMonomorphismPropertyAdapter :
  (d : C2S1.NormalMonomorphismProperty) →
  (em : M.Identifier) →
  (p : C2S1.NormalMonomorphismProperty.mono d ≡ em) →
  NormalMonomorphismPropertyAdapter
mkNormalMonomorphismPropertyAdapter d em p =
  record { decl = d ; expectedMono = em ; link = p ; status = true }

isFilledNormalMonomorphismProperty : NormalMonomorphismPropertyAdapter → Core.Phase.Bool
isFilledNormalMonomorphismProperty a = NormalMonomorphismPropertyAdapter.status a

-- Abelian category example: Ab
record AbelianCategoryExampleAbAdapter : Set₁ where
  field
    decl : C2S1.AbelianCategoryExampleAb
    status : Core.Phase.Bool

mkAbelianCategoryExampleAbAdapter :
  (d : C2S1.AbelianCategoryExampleAb) →
  AbelianCategoryExampleAbAdapter
mkAbelianCategoryExampleAbAdapter d =
  record { decl = d ; status = true }

isFilledAbelianCategoryExampleAb : AbelianCategoryExampleAbAdapter → Core.Phase.Bool
isFilledAbelianCategoryExampleAb a = AbelianCategoryExampleAbAdapter.status a

-- Abelian category example: R-Mod
record AbelianCategoryExampleRModAdapter : Set₁ where
  field
    decl : C2S1.AbelianCategoryExampleRMod
    expectedRing : M.Identifier
    link : C2S1.AbelianCategoryExampleRMod.ring decl ≡ expectedRing
    status : Core.Phase.Bool

mkAbelianCategoryExampleRModAdapter :
  (d : C2S1.AbelianCategoryExampleRMod) →
  (er : M.Identifier) →
  (p : C2S1.AbelianCategoryExampleRMod.ring d ≡ er) →
  AbelianCategoryExampleRModAdapter
mkAbelianCategoryExampleRModAdapter d er p =
  record { decl = d ; expectedRing = er ; link = p ; status = true }

isFilledAbelianCategoryExampleRMod : AbelianCategoryExampleRModAdapter → Core.Phase.Bool
isFilledAbelianCategoryExampleRMod a = AbelianCategoryExampleRModAdapter.status a

-- Functor additive property
record FunctorAdditivePropertyAdapter : Set₁ where
  field
    decl : C2S1.FunctorAdditiveProperty
    expectedFunctor : M.Identifier
    link : C2S1.FunctorAdditiveProperty.functor decl ≡ expectedFunctor
    status : Core.Phase.Bool

mkFunctorAdditivePropertyAdapter :
  (d : C2S1.FunctorAdditiveProperty) →
  (ef : M.Identifier) →
  (p : C2S1.FunctorAdditiveProperty.functor d ≡ ef) →
  FunctorAdditivePropertyAdapter
mkFunctorAdditivePropertyAdapter d ef p =
  record { decl = d ; expectedFunctor = ef ; link = p ; status = true }

isFilledFunctorAdditiveProperty : FunctorAdditivePropertyAdapter → Core.Phase.Bool
isFilledFunctorAdditiveProperty a = FunctorAdditivePropertyAdapter.status a

-- Additivity via biproduct coincidence theorem
record AdditivityViaBiproductCoincidenceTheoremAdapter : Set₁ where
  field
    decl : C2S1.AdditivityViaBiproductCoincidenceTheorem
    expectedCategory : M.Identifier
    link : C2S1.AdditivityViaBiproductCoincidenceTheorem.category decl ≡ expectedCategory
    status : Core.Phase.Bool

mkAdditivityViaBiproductCoincidenceTheoremAdapter :
  (d : C2S1.AdditivityViaBiproductCoincidenceTheorem) →
  (ec : M.Identifier) →
  (p : C2S1.AdditivityViaBiproductCoincidenceTheorem.category d ≡ ec) →
  AdditivityViaBiproductCoincidenceTheoremAdapter
mkAdditivityViaBiproductCoincidenceTheoremAdapter d ec p =
  record { decl = d ; expectedCategory = ec ; link = p ; status = true }

isFilledAdditivityViaBiproductCoincidenceTheorem : AdditivityViaBiproductCoincidenceTheoremAdapter → Core.Phase.Bool
isFilledAdditivityViaBiproductCoincidenceTheorem a = AdditivityViaBiproductCoincidenceTheoremAdapter.status a

------------------------------------------------------------------------
-- Subobject Theory (Chapter1.Level1sub4)
------------------------------------------------------------------------

-- Subobject lattice
record SubobjectLatticeAdapter : Set₁ where
  field
    decl : C1S4.SubobjectLattice
    expectedX : M.Identifier
    link : C1S4.SubobjectLattice.X decl ≡ expectedX
    status : Core.Phase.Bool

mkSubobjectLatticeAdapter :
  (d : C1S4.SubobjectLattice) →
  (ex : M.Identifier) →
  (p : C1S4.SubobjectLattice.X d ≡ ex) →
  SubobjectLatticeAdapter
mkSubobjectLatticeAdapter d ex p =
  record { decl = d ; expectedX = ex ; link = p ; status = true }

isFilledSubobjectLattice : SubobjectLatticeAdapter → Core.Phase.Bool
isFilledSubobjectLattice a = SubobjectLatticeAdapter.status a

-- Well-powered category
record WellPoweredCategoryAdapter : Set₁ where
  field
    decl : C1S4.WellPoweredCategory
    expectedC : M.Identifier
    link : C1S4.WellPoweredCategory.C decl ≡ expectedC
    status : Core.Phase.Bool

mkWellPoweredCategoryAdapter :
  (d : C1S4.WellPoweredCategory) →
  (ec : M.Identifier) →
  (p : C1S4.WellPoweredCategory.C d ≡ ec) →
  WellPoweredCategoryAdapter
mkWellPoweredCategoryAdapter d ec p =
  record { decl = d ; expectedC = ec ; link = p ; status = true }

isFilledWellPoweredCategory : WellPoweredCategoryAdapter → Core.Phase.Bool
isFilledWellPoweredCategory a = WellPoweredCategoryAdapter.status a

-- Subobject lattice is complete theorem
record SubobjectLatticeIsCompleteAdapter : Set₁ where
  field
    decl : C1S4.SubobjectLatticeIsComplete
    status : Core.Phase.Bool

mkSubobjectLatticeIsCompleteAdapter :
  (d : C1S4.SubobjectLatticeIsComplete) →
  SubobjectLatticeIsCompleteAdapter
mkSubobjectLatticeIsCompleteAdapter d =
  record { decl = d ; status = true }

isFilledSubobjectLatticeIsComplete : SubobjectLatticeIsCompleteAdapter → Core.Phase.Bool
isFilledSubobjectLatticeIsComplete a = SubobjectLatticeIsCompleteAdapter.status a

-- Strong epimorphism (orthogonal to monomorphisms)
record StrongEpimorphismAdapter : Set₁ where
  field
    decl : C1S4.StrongEpimorphism
    expectedE : M.Identifier
    link : C1S4.StrongEpimorphism.e decl ≡ expectedE
    status : Core.Phase.Bool

mkStrongEpimorphismAdapter :
  (d : C1S4.StrongEpimorphism) →
  (ee : M.Identifier) →
  (p : C1S4.StrongEpimorphism.e d ≡ ee) →
  StrongEpimorphismAdapter
mkStrongEpimorphismAdapter d ee p =
  record { decl = d ; expectedE = ee ; link = p ; status = true }

isFilledStrongEpimorphism : StrongEpimorphismAdapter → Core.Phase.Bool
isFilledStrongEpimorphism a = StrongEpimorphismAdapter.status a

-- Canonical factorization system theorem
record CanonicalFactorizationSystemAdapter : Set₁ where
  field
    decl : C1S4.CanonicalFactorizationSystem
    status : Core.Phase.Bool

mkCanonicalFactorizationSystemAdapter :
  (d : C1S4.CanonicalFactorizationSystem) →
  CanonicalFactorizationSystemAdapter
mkCanonicalFactorizationSystemAdapter d =
  record { decl = d ; status = true }

isFilledCanonicalFactorizationSystem : CanonicalFactorizationSystemAdapter → Core.Phase.Bool
isFilledCanonicalFactorizationSystem a = CanonicalFactorizationSystemAdapter.status a

-- Morphism factorization (epi-mono)
record MorphismFactorizationAdapter : Set₁ where
  field
    decl : C1S4.MorphismFactorization
    expectedF : M.Identifier
    expectedE : M.Identifier
    expectedM : M.Identifier
    link1 : C1S4.MorphismFactorization.f decl ≡ expectedF
    link2 : C1S4.MorphismFactorization.e decl ≡ expectedE
    link3 : C1S4.MorphismFactorization.m decl ≡ expectedM
    status : Core.Phase.Bool

mkMorphismFactorizationAdapter :
  (d : C1S4.MorphismFactorization) →
  (ef : M.Identifier) →
  (ee : M.Identifier) →
  (em : M.Identifier) →
  (p1 : C1S4.MorphismFactorization.f d ≡ ef) →
  (p2 : C1S4.MorphismFactorization.e d ≡ ee) →
  (p3 : C1S4.MorphismFactorization.m d ≡ em) →
  MorphismFactorizationAdapter
mkMorphismFactorizationAdapter d ef ee em p1 p2 p3 =
  record { decl = d ; expectedF = ef ; expectedE = ee ; expectedM = em ; link1 = p1 ; link2 = p2 ; link3 = p3 ; status = true }

isFilledMorphismFactorization : MorphismFactorizationAdapter → Core.Phase.Bool
isFilledMorphismFactorization a = MorphismFactorizationAdapter.status a

-- Has generator object
record HasGeneratorObjectAdapter : Set₁ where
  field
    decl : C1S4.HasGeneratorObject
    expectedC : M.Identifier
    expectedG : M.Identifier
    link1 : C1S4.HasGeneratorObject.C decl ≡ expectedC
    link2 : C1S4.HasGeneratorObject.G decl ≡ expectedG
    status : Core.Phase.Bool

mkHasGeneratorObjectAdapter :
  (d : C1S4.HasGeneratorObject) →
  (ec : M.Identifier) →
  (eg : M.Identifier) →
  (p1 : C1S4.HasGeneratorObject.C d ≡ ec) →
  (p2 : C1S4.HasGeneratorObject.G d ≡ eg) →
  HasGeneratorObjectAdapter
mkHasGeneratorObjectAdapter d ec eg p1 p2 =
  record { decl = d ; expectedC = ec ; expectedG = eg ; link1 = p1 ; link2 = p2 ; status = true }

isFilledHasGeneratorObject : HasGeneratorObjectAdapter → Core.Phase.Bool
isFilledHasGeneratorObject a = HasGeneratorObjectAdapter.status a

-- Projective object
record ProjectiveObjectAdapter : Set₁ where
  field
    decl : C1S4.ProjectiveObject
    expectedP : M.Identifier
    link : C1S4.ProjectiveObject.P decl ≡ expectedP
    status : Core.Phase.Bool

mkProjectiveObjectAdapter :
  (d : C1S4.ProjectiveObject) →
  (ep : M.Identifier) →
  (p : C1S4.ProjectiveObject.P d ≡ ep) →
  ProjectiveObjectAdapter
mkProjectiveObjectAdapter d ep p =
  record { decl = d ; expectedP = ep ; link = p ; status = true }

isFilledProjectiveObject : ProjectiveObjectAdapter → Core.Phase.Bool
isFilledProjectiveObject a = ProjectiveObjectAdapter.status a

-- Injective object (dual to projective)
record InjectiveObjectAdapter : Set₁ where
  field
    decl : C1S4.InjectiveObject
    expectedI : M.Identifier
    link : C1S4.InjectiveObject.I decl ≡ expectedI
    status : Core.Phase.Bool

mkInjectiveObjectAdapter :
  (d : C1S4.InjectiveObject) →
  (ei : M.Identifier) →
  (p : C1S4.InjectiveObject.I d ≡ ei) →
  InjectiveObjectAdapter
mkInjectiveObjectAdapter d ei p =
  record { decl = d ; expectedI = ei ; link = p ; status = true }

isFilledInjectiveObject : InjectiveObjectAdapter → Core.Phase.Bool
isFilledInjectiveObject a = InjectiveObjectAdapter.status a

-- Has enough projectives
record HasEnoughProjectivesAdapter : Set₁ where
  field
    decl : C1S4.HasEnoughProjectives
    expectedC : M.Identifier
    link : C1S4.HasEnoughProjectives.C decl ≡ expectedC
    status : Core.Phase.Bool

mkHasEnoughProjectivesAdapter :
  (d : C1S4.HasEnoughProjectives) →
  (ec : M.Identifier) →
  (p : C1S4.HasEnoughProjectives.C d ≡ ec) →
  HasEnoughProjectivesAdapter
mkHasEnoughProjectivesAdapter d ec p =
  record { decl = d ; expectedC = ec ; link = p ; status = true }

isFilledHasEnoughProjectives : HasEnoughProjectivesAdapter → Core.Phase.Bool
isFilledHasEnoughProjectives a = HasEnoughProjectivesAdapter.status a

-- Has enough injectives
record HasEnoughInjectivesAdapter : Set₁ where
  field
    decl : C1S4.HasEnoughInjectives
    expectedC : M.Identifier
    link : C1S4.HasEnoughInjectives.C decl ≡ expectedC
    status : Core.Phase.Bool

mkHasEnoughInjectivesAdapter :
  (d : C1S4.HasEnoughInjectives) →
  (ec : M.Identifier) →
  (p : C1S4.HasEnoughInjectives.C d ≡ ec) →
  HasEnoughInjectivesAdapter
mkHasEnoughInjectivesAdapter d ec p =
  record { decl = d ; expectedC = ec ; link = p ; status = true }

isFilledHasEnoughInjectives : HasEnoughInjectivesAdapter → Core.Phase.Bool
isFilledHasEnoughInjectives a = HasEnoughInjectivesAdapter.status a
