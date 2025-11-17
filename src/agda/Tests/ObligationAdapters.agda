-- Tests.ObligationAdapters: Tiny adapters and a common status predicate

module Tests.ObligationAdapters where

import Agda.Builtin.Bool as B
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
    status : B.Bool

-- Adapter: Identity morphism in an enriched category
record IdEnrichedAdapter : Set₁ where
  field
    decl     : S6.IdentityMorphismDeclaration_Enriched
    expected : C1L.MorphismDeclaration
    link     : S6.IdentityMorphismDeclaration_Enriched.identityMorphismInV decl ≡ expected
    status   : B.Bool

mkIdEnrichedAdapter :
  (d : S6.IdentityMorphismDeclaration_Enriched) →
  (e : C1L.MorphismDeclaration) →
  (p : S6.IdentityMorphismDeclaration_Enriched.identityMorphismInV d ≡ e) →
  IdEnrichedAdapter
mkIdEnrichedAdapter d e p = record { decl = d ; expected = e ; link = p ; status = B.true }

isFilledId : IdEnrichedAdapter → B.Bool
isFilledId a = IdEnrichedAdapter.status a

-- Adapter: Hom-object declaration in an enriched setting
record HomObjectAdapter : Set₁ where
  field
    decl     : S6.HomObjectDeclaration
    expected : M.Identifier
    link     : S6.HomObjectDeclaration.homObjectInV decl ≡ expected
    status   : B.Bool

mkHomObjectAdapter :
  (d : S6.HomObjectDeclaration) →
  (e : M.Identifier) →
  (p : S6.HomObjectDeclaration.homObjectInV d ≡ e) →
  HomObjectAdapter
mkHomObjectAdapter d e p = record { decl = d ; expected = e ; link = p ; status = B.true }

isFilledHom : HomObjectAdapter → B.Bool
isFilledHom a = HomObjectAdapter.status a

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
    status   : B.Bool

mkLocalHomeomorphismAdapter :
  (d : C3S2.MorphismPropertyAssertionLocalHomeomorphism) →
  (e : M.Identifier) →
  (p : C3S2.MorphismPropertyAssertionLocalHomeomorphism.morphism d ≡ e) →
  LocalHomeomorphismAdapter
mkLocalHomeomorphismAdapter d e p = record { decl = d ; expected = e ; link = p ; status = B.true }

isFilledLocalHomeo : LocalHomeomorphismAdapter → B.Bool
isFilledLocalHomeo a = LocalHomeomorphismAdapter.status a

record EtaleSpaceAdapter : Set₁ where
  field
    decl              : C3S2.EtaleSpaceOver
    expectedProjection : M.Identifier
    expectedLocalHomeo : C3S2.MorphismPropertyAssertionLocalHomeomorphism
    projLink          : C3S2.EtaleSpaceOver.projection decl ≡ expectedProjection
    locLink           : C3S2.EtaleSpaceOver.isLocalHomeomorphism decl ≡ expectedLocalHomeo
    status            : B.Bool

mkEtaleSpaceAdapter :
  (d : C3S2.EtaleSpaceOver) →
  (p : M.Identifier) →
  (h : C3S2.MorphismPropertyAssertionLocalHomeomorphism) →
  (pl : C3S2.EtaleSpaceOver.projection d ≡ p) →
  (hl : C3S2.EtaleSpaceOver.isLocalHomeomorphism d ≡ h) →
  EtaleSpaceAdapter
mkEtaleSpaceAdapter d p h pl hl = record
  { decl = d ; expectedProjection = p ; expectedLocalHomeo = h
  ; projLink = pl ; locLink = hl ; status = B.true }

isFilledEtale : EtaleSpaceAdapter → B.Bool
isFilledEtale a = EtaleSpaceAdapter.status a

-- ==========================================================
-- Chapter 3, Level 3.1 (locale–frame duality)
-- ==========================================================

record LocaleFrameDualityAdapter : Set₁ where
  field
    decl       : C3S1.LocaleFrameDualityTheorem
    expectedOP : Set
    link       : C3S1.LocaleFrameDualityTheorem.isOppositeCategory decl ≡ expectedOP
    status     : B.Bool

mkLocaleFrameDualityAdapter :
  (d : C3S1.LocaleFrameDualityTheorem) →
  (op : Set) →
  (p : C3S1.LocaleFrameDualityTheorem.isOppositeCategory d ≡ op) →
  LocaleFrameDualityAdapter
mkLocaleFrameDualityAdapter d op p = record { decl = d ; expectedOP = op ; link = p ; status = B.true }

isFilledDuality : LocaleFrameDualityAdapter → B.Bool
isFilledDuality a = LocaleFrameDualityAdapter.status a

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
    status : B.Bool

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
         ; linkF = pf ; linkG = pg ; linkC = pc ; linkD = pd ; status = B.true }

isFilledAdjunction : AdjunctionHomAdapter → B.Bool
isFilledAdjunction a = AdjunctionHomAdapter.status a

-- ==========================================================
-- Chapter 1, Level 1.4 (canonical factorization system)
-- ==========================================================

record CanonicalFactorizationAdapter : Set₁ where
  field
    decl  : C1S4.CanonicalFactorizationSystem
    link  : C1S4.CanonicalFactorizationSystem.unit decl ≡ tt
    status : B.Bool

mkCanonicalFactorizationAdapter :
  (d : C1S4.CanonicalFactorizationSystem) →
  (p : C1S4.CanonicalFactorizationSystem.unit d ≡ tt) →
  CanonicalFactorizationAdapter
mkCanonicalFactorizationAdapter d p = record { decl = d ; link = p ; status = B.true }

isFilledCanonicalFactorization : CanonicalFactorizationAdapter → B.Bool
isFilledCanonicalFactorization a = CanonicalFactorizationAdapter.status a

-- ==========================================================
-- Chapter 2, Level 2.2 (regular epi/mono factorization in regular categories)
-- ==========================================================

record RegularFactorizationAdapter : Set₁ where
  field
    decl     : C2S2.RegularCategoryDeclaration
    expected : M.Identifier
    link     : C2S2.RegularCategoryDeclaration.regularEpiMonoFactorizationWitness decl ≡ expected
    status   : B.Bool

mkRegularFactorizationAdapter :
  (d : C2S2.RegularCategoryDeclaration) →
  (e : M.Identifier) →
  (p : C2S2.RegularCategoryDeclaration.regularEpiMonoFactorizationWitness d ≡ e) →
  RegularFactorizationAdapter
mkRegularFactorizationAdapter d e p = record { decl = d ; expected = e ; link = p ; status = B.true }

isFilledRegularFactorization : RegularFactorizationAdapter → B.Bool
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
    status : B.Bool

mkReflectiveLocalizationAdapter :
  (d : C1S5.ReflectiveSubcategoryAsLocalizationTheorem) →
  (r c l : M.Identifier) →
  (pr : C1S5.ReflectiveSubcategoryAsLocalizationTheorem.reflectiveSubcat d ≡ r) →
  (pc : C1S5.ReflectiveSubcategoryAsLocalizationTheorem.ambientCategory d ≡ c) →
  (pl : C1S5.ReflectiveSubcategoryAsLocalizationTheorem.reflector d ≡ l) →
  ReflectiveLocalizationAdapter
mkReflectiveLocalizationAdapter d r c l pr pc pl =
  record { decl = d ; expR = r ; expC = c ; expL = l ; linkR = pr ; linkC = pc ; linkL = pl ; status = B.true }

isFilledReflectiveLocalization : ReflectiveLocalizationAdapter → B.Bool
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
    status : B.Bool

mkKernelPairAdapter :
  (d : C2S2.KernelPairDeclaration) →
  (m k1 k2 pb : M.Identifier) →
  (pm  : C2S2.KernelPairDeclaration.morphism d ≡ m) →
  (pk1 : C2S2.KernelPairDeclaration.projection1 d ≡ k1) →
  (pk2 : C2S2.KernelPairDeclaration.projection2 d ≡ k2) →
  (ppb : C2S2.KernelPairDeclaration.pullbackSquareWitness d ≡ pb) →
  KernelPairAdapter
mkKernelPairAdapter d m k1 k2 pb pm pk1 pk2 ppb =
  record { decl = d ; expM = m ; expK1 = k1 ; expK2 = k2 ; expPB = pb
         ; linkM = pm ; linkK1 = pk1 ; linkK2 = pk2 ; linkPB = ppb ; status = B.true }

isFilledKernelPair : KernelPairAdapter → B.Bool
isFilledKernelPair a = KernelPairAdapter.status a

-- ==========================================================
-- Chapter 1, Level 1.4 (strong monomorphism)
-- ==========================================================

record StrongMonoAdapter : Set₁ where
  field
    decl     : C1S4.StrongMonomorphism
    expected : M.Identifier
    link     : C1S4.StrongMonomorphism.m decl ≡ expected
    status   : B.Bool

mkStrongMonoAdapter :
  (d : C1S4.StrongMonomorphism) →
  (e : M.Identifier) →
  (p : C1S4.StrongMonomorphism.m d ≡ e) →
  StrongMonoAdapter
mkStrongMonoAdapter d e p = record { decl = d ; expected = e ; link = p ; status = B.true }

isFilledStrongMono : StrongMonoAdapter → B.Bool
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
    status  : B.Bool

mkInternalEquivalenceRelationAdapter :
  (d : C2S2.InternalEquivalenceRelationDeclaration) →
  (r1 r2 mono : M.Identifier) →
  (pr1 : C2S2.InternalEquivalenceRelationDeclaration.relLeft d ≡ r1) →
  (pr2 : C2S2.InternalEquivalenceRelationDeclaration.relRight d ≡ r2) →
  (pmono : C2S2.InternalEquivalenceRelationDeclaration.monoIntoProductWitness d ≡ mono) →
  InternalEquivalenceRelationAdapter
mkInternalEquivalenceRelationAdapter d r1 r2 mono pr1 pr2 pmono =
  record { decl = d ; expR1 = r1 ; expR2 = r2 ; expMono = mono
         ; linkR1 = pr1 ; linkR2 = pr2 ; linkMono = pmono ; status = B.true }

isFilledInternalEquiv : InternalEquivalenceRelationAdapter → B.Bool
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
    status : B.Bool

mkRegularExactSequenceAdapter :
  (d : C2S2.RegularExactSequenceDeclaration) →
  (k q : M.Identifier) →
  (pk : C2S2.KernelPairDeclaration.morphism (C2S2.RegularExactSequenceDeclaration.kernelPair d) ≡ k) →
  (pq : C2S2.RegularEpimorphismProperty.morphism (C2S2.RegularExactSequenceDeclaration.quotient d) ≡ q) →
  RegularExactSequenceAdapter
mkRegularExactSequenceAdapter d k q pk pq =
  record { decl = d ; expKernelMorphism = k ; expQuotientMorphism = q
         ; linkKernel = pk ; linkQuotient = pq ; status = B.true }

isFilledRegularExact : RegularExactSequenceAdapter → B.Bool
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
    status : B.Bool

mkAdditiveCategoryAdapter :
  (d : C2S1.AdditiveCategoryDeclaration) →
  (cat : M.Identifier) →
  (zero : C2S1.HasZeroObjectProperty) →
  (pcat : C2S1.AdditiveCategoryDeclaration.category d ≡ cat) →
  (pzero : C2S1.AdditiveCategoryDeclaration.hasZeroObject d ≡ zero) →
  AdditiveCategoryAdapter
mkAdditiveCategoryAdapter d cat zero pcat pzero =
  record { decl = d ; expCategory = cat ; expZero = zero
         ; linkCat = pcat ; linkZero = pzero ; status = B.true }

isFilledAdditive : AdditiveCategoryAdapter → B.Bool
isFilledAdditive a = AdditiveCategoryAdapter.status a

record AbelianCategoryAdapter : Set₁ where
  field
    decl : C2S1.AbelianCategoryDeclaration
    expCategory : M.Identifier
    expAdditive : C2S1.AdditiveCategoryDeclaration
    linkCat : C2S1.AbelianCategoryDeclaration.category decl ≡ expCategory
    linkAdd : C2S1.AbelianCategoryDeclaration.additive decl ≡ expAdditive
    status : B.Bool

mkAbelianCategoryAdapter :
  (d : C2S1.AbelianCategoryDeclaration) →
  (cat : M.Identifier) →
  (add : C2S1.AdditiveCategoryDeclaration) →
  (pcat : C2S1.AbelianCategoryDeclaration.category d ≡ cat) →
  (padd : C2S1.AbelianCategoryDeclaration.additive d ≡ add) →
  AbelianCategoryAdapter
mkAbelianCategoryAdapter d cat add pcat padd =
  record { decl = d ; expCategory = cat ; expAdditive = add
         ; linkCat = pcat ; linkAdd = padd ; status = B.true }

isFilledAbelian : AbelianCategoryAdapter → B.Bool
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
    status : B.Bool

mkBiproductAdapter :
  (d : C2S1.BiproductObject) →
  (l r obj : M.Identifier) →
  (pl : C2S1.BiproductObject.left d ≡ l) →
  (pr : C2S1.BiproductObject.right d ≡ r) →
  (pobj : C2S1.BiproductObject.object d ≡ obj) →
  BiproductAdapter
mkBiproductAdapter d l r obj pl pr pobj =
  record { decl = d ; expLeft = l ; expRight = r ; expObject = obj
         ; linkLeft = pl ; linkRight = pr ; linkObject = pobj ; status = B.true }

isFilledBiproduct : BiproductAdapter → B.Bool
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
    status : B.Bool

mkLawvereTheoryAdapter :
  (d : C2S3.LawvereTheoryDeclaration) →
  (th base : M.Identifier) →
  (pth : C2S3.LawvereTheoryDeclaration.theoryCategory d ≡ th) →
  (pbase : C2S3.LawvereTheoryDeclaration.baseObject d ≡ base) →
  LawvereTheoryAdapter
mkLawvereTheoryAdapter d th base pth pbase =
  record { decl = d ; expTheory = th ; expBase = base
         ; linkTheory = pth ; linkBase = pbase ; status = B.true }

isFilledLawvereTheory : LawvereTheoryAdapter → B.Bool
isFilledLawvereTheory a = LawvereTheoryAdapter.status a

record AlgebraicCategoryAdapter : Set₁ where
  field
    decl : C2S3.AlgebraicCategoryDeclaration
    expCategory : M.Identifier
    expTheory : C2S3.LawvereTheoryDeclaration
    linkCat : C2S3.AlgebraicCategoryDeclaration.category decl ≡ expCategory
    linkTheory : C2S3.AlgebraicCategoryDeclaration.witnessTheory decl ≡ expTheory
    status : B.Bool

mkAlgebraicCategoryAdapter :
  (d : C2S3.AlgebraicCategoryDeclaration) →
  (cat : M.Identifier) →
  (th : C2S3.LawvereTheoryDeclaration) →
  (pcat : C2S3.AlgebraicCategoryDeclaration.category d ≡ cat) →
  (pth : C2S3.AlgebraicCategoryDeclaration.witnessTheory d ≡ th) →
  AlgebraicCategoryAdapter
mkAlgebraicCategoryAdapter d cat th pcat pth =
  record { decl = d ; expCategory = cat ; expTheory = th
         ; linkCat = pcat ; linkTheory = pth ; status = B.true }

isFilledAlgebraicCategory : AlgebraicCategoryAdapter → B.Bool
isFilledAlgebraicCategory a = AlgebraicCategoryAdapter.status a

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
    status : B.Bool

mkMonadAdapter :
  (d : C2S4.MonadDeclaration) →
  (n : M.Identifier) →
  (dat : C2S4.MonadData) →
  (pn : C2S4.MonadDeclaration.name d ≡ n) →
  (pdat : C2S4.MonadDeclaration.datum d ≡ dat) →
  MonadAdapter
mkMonadAdapter d n dat pn pdat =
  record { decl = d ; expName = n ; expDatum = dat
         ; linkName = pn ; linkDatum = pdat ; status = B.true }

isFilledMonad : MonadAdapter → B.Bool
isFilledMonad a = MonadAdapter.status a

record TAlgebraAdapter : Set₁ where
  field
    decl : C2S4.TAlgebraData
    expCarrier : M.Identifier
    monad : C2S4.MonadDeclaration
    linkCarrier : C2S4.TAlgebraData.carrier decl ≡ expCarrier
    linkMonad : C2S4.TAlgebraData.monad decl ≡ monad
    status : B.Bool

mkTAlgebraAdapter :
  (d : C2S4.TAlgebraData) →
  (c : M.Identifier) →
  (m : C2S4.MonadDeclaration) →
  (pc : C2S4.TAlgebraData.carrier d ≡ c) →
  (pm : C2S4.TAlgebraData.monad d ≡ m) →
  TAlgebraAdapter
mkTAlgebraAdapter d c m pc pm =
  record { decl = d ; expCarrier = c ; monad = m
         ; linkCarrier = pc ; linkMonad = pm ; status = B.true }

isFilledTAlgebra : TAlgebraAdapter → B.Bool
isFilledTAlgebra a = TAlgebraAdapter.status a

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
    status : B.Bool

mkLocallyPresentableAdapter :
  (d : C2S5.LocallyPresentableCategoryDeclaration) →
  (cat : C1S3.CategoryDeclaration) →
  (rk : C1S6.RegularCardinal) →
  (pcat : C2S5.LocallyPresentableCategoryDeclaration.category d ≡ cat) →
  (prk : C2S5.LocallyPresentableCategoryDeclaration.rank d ≡ rk) →
  LocallyPresentableAdapter
mkLocallyPresentableAdapter d cat rk pcat prk =
  record { decl = d ; expCat = cat ; expRank = rk
         ; linkCat = pcat ; linkRank = prk ; status = B.true }

isFilledLocallyPresentable : LocallyPresentableAdapter → B.Bool
isFilledLocallyPresentable a = LocallyPresentableAdapter.status a

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
    status : B.Bool

mkMonoidalCategoryAdapter :
  (d : S6.MonoidalCategoryDeclaration) →
  (dat : S6.MonoidalCategoryData) →
  (assoc : S6.AssociatorDeclaration) →
  (pdat : S6.MonoidalCategoryDeclaration.datum d ≡ dat) →
  (passoc : S6.MonoidalCategoryDeclaration.associator d ≡ assoc) →
  MonoidalCategoryAdapter
mkMonoidalCategoryAdapter d dat assoc pdat passoc =
  record { decl = d ; expDatum = dat ; expAssociator = assoc
         ; linkDatum = pdat ; linkAssoc = passoc ; status = B.true }

isFilledMonoidal : MonoidalCategoryAdapter → B.Bool
isFilledMonoidal a = MonoidalCategoryAdapter.status a

record SymmetricMonoidalAdapter : Set₁ where
  field
    decl : S6.SymmetricMonoidalCategoryDeclaration
    expMonoidal : S6.MonoidalCategoryDeclaration
    expBraiding : S6.BraidingDeclaration
    linkMonoidal : S6.SymmetricMonoidalCategoryDeclaration.monoidalCategory decl ≡ expMonoidal
    linkBraiding : S6.SymmetricMonoidalCategoryDeclaration.braiding decl ≡ expBraiding
    status : B.Bool

mkSymmetricMonoidalAdapter :
  (d : S6.SymmetricMonoidalCategoryDeclaration) →
  (mon : S6.MonoidalCategoryDeclaration) →
  (br : S6.BraidingDeclaration) →
  (pmon : S6.SymmetricMonoidalCategoryDeclaration.monoidalCategory d ≡ mon) →
  (pbr : S6.SymmetricMonoidalCategoryDeclaration.braiding d ≡ br) →
  SymmetricMonoidalAdapter
mkSymmetricMonoidalAdapter d mon br pmon pbr =
  record { decl = d ; expMonoidal = mon ; expBraiding = br
         ; linkMonoidal = pmon ; linkBraiding = pbr ; status = B.true }

isFilledSymmetricMonoidal : SymmetricMonoidalAdapter → B.Bool
isFilledSymmetricMonoidal a = SymmetricMonoidalAdapter.status a

record InternalHomAdapter : Set₁ where
  field
    decl : S6.InternalHomObjectDeclaration
    expCat : C1S3.CategoryDeclaration
    expSource : M.Identifier
    expTarget : M.Identifier
    linkCat : S6.InternalHomObjectDeclaration.category decl ≡ expCat
    linkSource : S6.InternalHomObjectDeclaration.sourceObject decl ≡ expSource
    linkTarget : S6.InternalHomObjectDeclaration.targetObject decl ≡ expTarget
    status : B.Bool

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
         ; linkCat = pcat ; linkSource = psrc ; linkTarget = ptgt ; status = B.true }

isFilledInternalHom : InternalHomAdapter → B.Bool
isFilledInternalHom a = InternalHomAdapter.status a

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
    status : B.Bool

mkCGWH_CategoryAdapter :
  (d : C2S7.CGWH_CategoryDeclaration) →
  (topcat : C2S7.TopologicalSpacesCategory) →
  (cat : C1S3.CategoryDeclaration) →
  (ptop : C2S7.CGWH_CategoryDeclaration.topCategory d ≡ topcat) →
  (pcat : C2S7.CGWH_CategoryDeclaration.underlyingCategory d ≡ cat) →
  CGWH_CategoryAdapter
mkCGWH_CategoryAdapter d topcat cat ptop pcat =
  record { decl = d ; expTopCat = topcat ; expUnderlyingCat = cat
         ; linkTopCat = ptop ; linkUnderlyingCat = pcat ; status = B.true }

isFilledCGWH : CGWH_CategoryAdapter → B.Bool
isFilledCGWH a = CGWH_CategoryAdapter.status a

record TopologicalFunctorAdapter : Set₁ where
  field
    decl : C2S7.TopologicalFunctorProperty
    expFunctor : M.Identifier
    linkFunctor : C2S7.TopologicalFunctorProperty.functor decl ≡ expFunctor
    status : B.Bool

mkTopologicalFunctorAdapter :
  (d : C2S7.TopologicalFunctorProperty) →
  (f : M.Identifier) →
  (pf : C2S7.TopologicalFunctorProperty.functor d ≡ f) →
  TopologicalFunctorAdapter
mkTopologicalFunctorAdapter d f pf =
  record { decl = d ; expFunctor = f
         ; linkFunctor = pf ; status = B.true }

isFilledTopologicalFunctor : TopologicalFunctorAdapter → B.Bool
isFilledTopologicalFunctor a = TopologicalFunctorAdapter.status a

-- ==========================================================
-- Chapter 2, Level 2.8 (Fibrations)
-- ==========================================================

record FibrationAdapter : Set₁ where
  field
    decl : C2S8.FibrationDeclaration
    expProjection : C2S8.FibrationProjectionFunctor
    linkProjection : C2S8.FibrationDeclaration.projectionFunctor decl ≡ expProjection
    status : B.Bool

mkFibrationAdapter :
  (d : C2S8.FibrationDeclaration) →
  (proj : C2S8.FibrationProjectionFunctor) →
  (pproj : C2S8.FibrationDeclaration.projectionFunctor d ≡ proj) →
  FibrationAdapter
mkFibrationAdapter d proj pproj =
  record { decl = d ; expProjection = proj
         ; linkProjection = pproj ; status = B.true }

isFilledFibration : FibrationAdapter → B.Bool
isFilledFibration a = FibrationAdapter.status a

record OpfibrationAdapter : Set₁ where
  field
    decl : C2S8.OpfibrationDeclaration
    expProjection : C2S8.FibrationProjectionFunctor
    linkProjection : C2S8.OpfibrationDeclaration.projectionFunctor decl ≡ expProjection
    status : B.Bool

mkOpfibrationAdapter :
  (d : C2S8.OpfibrationDeclaration) →
  (proj : C2S8.FibrationProjectionFunctor) →
  (pproj : C2S8.OpfibrationDeclaration.projectionFunctor d ≡ proj) →
  OpfibrationAdapter
mkOpfibrationAdapter d proj pproj =
  record { decl = d ; expProjection = proj
         ; linkProjection = pproj ; status = B.true }

isFilledOpfibration : OpfibrationAdapter → B.Bool
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
    status : B.Bool

mkShortExactSequenceAdapter :
  (d : C2S1.ShortExactSequenceDeclaration) →
  (a b c : M.Identifier) →
  (pa : C2S1.ShortExactSequenceDeclaration.A d ≡ a) →
  (pb : C2S1.ShortExactSequenceDeclaration.B d ≡ b) →
  (pc : C2S1.ShortExactSequenceDeclaration.C d ≡ c) →
  ShortExactSequenceAdapter
mkShortExactSequenceAdapter d a b c pa pb pc =
  record { decl = d ; expA = a ; expB = b ; expC = c
         ; linkA = pa ; linkB = pb ; linkC = pc ; status = B.true }

isFilledShortExactSequence : ShortExactSequenceAdapter → B.Bool
isFilledShortExactSequence a = ShortExactSequenceAdapter.status a

record ZeroMorphismAdapter : Set₁ where
  field
    decl : C2S1.ZeroMorphismDeclaration
    expFrom : M.Identifier
    expTo : M.Identifier
    expViaZero : M.Identifier
    linkFrom : C2S1.ZeroMorphismDeclaration.from decl ≡ expFrom
    linkTo : C2S1.ZeroMorphismDeclaration.to decl ≡ expTo
    linkViaZero : C2S1.ZeroMorphismDeclaration.viaZeroObject decl ≡ expViaZero
    status : B.Bool

mkZeroMorphismAdapter :
  (d : C2S1.ZeroMorphismDeclaration) →
  (from to via : M.Identifier) →
  (pfrom : C2S1.ZeroMorphismDeclaration.from d ≡ from) →
  (pto : C2S1.ZeroMorphismDeclaration.to d ≡ to) →
  (pvia : C2S1.ZeroMorphismDeclaration.viaZeroObject d ≡ via) →
  ZeroMorphismAdapter
mkZeroMorphismAdapter d from to via pfrom pto pvia =
  record { decl = d ; expFrom = from ; expTo = to ; expViaZero = via
         ; linkFrom = pfrom ; linkTo = pto ; linkViaZero = pvia ; status = B.true }

isFilledZeroMorphism : ZeroMorphismAdapter → B.Bool
isFilledZeroMorphism a = ZeroMorphismAdapter.status a

record TorsionTheoryAdapter : Set₁ where
  field
    decl : C2S1.TorsionTheoryDeclaration
    expCategory : M.Identifier
    expTorsionClass : M.Identifier
    expTorsionFreeClass : M.Identifier
    linkCat : C2S1.TorsionTheoryDeclaration.category decl ≡ expCategory
    linkTorsion : C2S1.TorsionTheoryDeclaration.torsionClass decl ≡ expTorsionClass
    linkTorsionFree : C2S1.TorsionTheoryDeclaration.torsionFreeClass decl ≡ expTorsionFreeClass
    status : B.Bool

mkTorsionTheoryAdapter :
  (d : C2S1.TorsionTheoryDeclaration) →
  (cat tclass tfclass : M.Identifier) →
  (pcat : C2S1.TorsionTheoryDeclaration.category d ≡ cat) →
  (pt : C2S1.TorsionTheoryDeclaration.torsionClass d ≡ tclass) →
  (ptf : C2S1.TorsionTheoryDeclaration.torsionFreeClass d ≡ tfclass) →
  TorsionTheoryAdapter
mkTorsionTheoryAdapter d cat tclass tfclass pcat pt ptf =
  record { decl = d ; expCategory = cat ; expTorsionClass = tclass ; expTorsionFreeClass = tfclass
         ; linkCat = pcat ; linkTorsion = pt ; linkTorsionFree = ptf ; status = B.true }

isFilledTorsionTheory : TorsionTheoryAdapter → B.Bool
isFilledTorsionTheory a = TorsionTheoryAdapter.status a

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
    status : B.Bool

mkBialgebraAdapter :
  (d : C2S3.BialgebraDeclaration) →
  (car m1 m2 : M.Identifier) →
  (pcar : C2S3.BialgebraDeclaration.carrier d ≡ car) →
  (pm1 : C2S3.BialgebraDeclaration.model1 d ≡ m1) →
  (pm2 : C2S3.BialgebraDeclaration.model2 d ≡ m2) →
  BialgebraAdapter
mkBialgebraAdapter d car m1 m2 pcar pm1 pm2 =
  record { decl = d ; expCarrier = car ; expModel1 = m1 ; expModel2 = m2
         ; linkCarrier = pcar ; linkModel1 = pm1 ; linkModel2 = pm2 ; status = B.true }

isFilledBialgebra : BialgebraAdapter → B.Bool
isFilledBialgebra a = BialgebraAdapter.status a

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
    status : B.Bool

mkComonadAdapter :
  (d : C2S4.ComonadDeclaration) →
  (n : M.Identifier) →
  (dat : C2S4.ComonadData) →
  (pn : C2S4.ComonadDeclaration.name d ≡ n) →
  (pdat : C2S4.ComonadDeclaration.datum d ≡ dat) →
  ComonadAdapter
mkComonadAdapter d n dat pn pdat =
  record { decl = d ; expName = n ; expDatum = dat
         ; linkName = pn ; linkDatum = pdat ; status = B.true }

isFilledComonad : ComonadAdapter → B.Bool
isFilledComonad a = ComonadAdapter.status a

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
    status : B.Bool

mkAccessibleCategoryAdapter :
  (d : C2S5.AccessibleCategoryDeclaration) →
  (cat : C1S3.CategoryDeclaration) →
  (rk : C1S6.RegularCardinal) →
  (pcat : C2S5.AccessibleCategoryDeclaration.category d ≡ cat) →
  (prk : C2S5.AccessibleCategoryDeclaration.rank d ≡ rk) →
  AccessibleCategoryAdapter
mkAccessibleCategoryAdapter d cat rk pcat prk =
  record { decl = d ; expCat = cat ; expRank = rk
         ; linkCat = pcat ; linkRank = prk ; status = B.true }

isFilledAccessibleCategory : AccessibleCategoryAdapter → B.Bool
isFilledAccessibleCategory a = AccessibleCategoryAdapter.status a

record SketchAdapter : Set₁ where
  field
    decl : C2S5.SketchDeclaration
    expUnderlyingCat : C1S3.CategoryDeclaration
    linkUnderlyingCat : C2S5.SketchDeclaration.underlyingCategory decl ≡ expUnderlyingCat
    status : B.Bool

mkSketchAdapter :
  (d : C2S5.SketchDeclaration) →
  (cat : C1S3.CategoryDeclaration) →
  (pcat : C2S5.SketchDeclaration.underlyingCategory d ≡ cat) →
  SketchAdapter
mkSketchAdapter d cat pcat =
  record { decl = d ; expUnderlyingCat = cat
         ; linkUnderlyingCat = pcat ; status = B.true }

isFilledSketch : SketchAdapter → B.Bool
isFilledSketch a = SketchAdapter.status a

-- ==========================================================
-- Chapter 3, Level 3.1 (Locales & Frames) - Additional
-- ==========================================================

record HeytingAlgebraAdapter : Set₁ where
  field
    decl : C3S1.HeytingAlgebraDeclaration
    status : B.Bool

mkHeytingAlgebraAdapter :
  (d : C3S1.HeytingAlgebraDeclaration) →
  HeytingAlgebraAdapter
mkHeytingAlgebraAdapter d =
  record { decl = d ; status = B.true }

isFilledHeytingAlgebra : HeytingAlgebraAdapter → B.Bool
isFilledHeytingAlgebra a = HeytingAlgebraAdapter.status a

record FrameAdapter : Set₁ where
  field
    decl : C3S1.FrameDeclaration
    expHeyting : C3S1.HeytingAlgebraDeclaration
    linkHeyting : C3S1.FrameDeclaration.underlyingHeytingAlgebra decl ≡ expHeyting
    status : B.Bool

mkFrameAdapter :
  (d : C3S1.FrameDeclaration) →
  (h : C3S1.HeytingAlgebraDeclaration) →
  (ph : C3S1.FrameDeclaration.underlyingHeytingAlgebra d ≡ h) →
  FrameAdapter
mkFrameAdapter d h ph =
  record { decl = d ; expHeyting = h
         ; linkHeyting = ph ; status = B.true }

isFilledFrame : FrameAdapter → B.Bool
isFilledFrame a = FrameAdapter.status a

record LocaleAdapter : Set₁ where
  field
    decl : C3S1.LocaleDeclaration
    expFrame : C3S1.FrameDeclaration
    linkFrame : C3S1.LocaleDeclaration.associatedFrame decl ≡ expFrame
    status : B.Bool

mkLocaleAdapter :
  (d : C3S1.LocaleDeclaration) →
  (f : C3S1.FrameDeclaration) →
  (pf : C3S1.LocaleDeclaration.associatedFrame d ≡ f) →
  LocaleAdapter
mkLocaleAdapter d f pf =
  record { decl = d ; expFrame = f
         ; linkFrame = pf ; status = B.true }

isFilledLocale : LocaleAdapter → B.Bool
isFilledLocale a = LocaleAdapter.status a

record LocaleMorphismAdapter : Set₁ where
  field
    decl : C3S1.LocaleMorphismDeclaration
    expSource : C3S1.LocaleDeclaration
    expTarget : C3S1.LocaleDeclaration
    linkSource : C3S1.LocaleMorphismDeclaration.sourceLocale decl ≡ expSource
    linkTarget : C3S1.LocaleMorphismDeclaration.targetLocale decl ≡ expTarget
    status : B.Bool

mkLocaleMorphismAdapter :
  (d : C3S1.LocaleMorphismDeclaration) →
  (src tgt : C3S1.LocaleDeclaration) →
  (psrc : C3S1.LocaleMorphismDeclaration.sourceLocale d ≡ src) →
  (ptgt : C3S1.LocaleMorphismDeclaration.targetLocale d ≡ tgt) →
  LocaleMorphismAdapter
mkLocaleMorphismAdapter d src tgt psrc ptgt =
  record { decl = d ; expSource = src ; expTarget = tgt
         ; linkSource = psrc ; linkTarget = ptgt ; status = B.true }

isFilledLocaleMorphism : LocaleMorphismAdapter → B.Bool
isFilledLocaleMorphism a = LocaleMorphismAdapter.status a

record NucleusAdapter : Set₁ where
  field
    decl : C3S1.NucleusDeclaration
    expFrame : C3S1.FrameDeclaration
    linkFrame : C3S1.NucleusDeclaration.frame decl ≡ expFrame
    status : B.Bool

mkNucleusAdapter :
  (d : C3S1.NucleusDeclaration) →
  (f : C3S1.FrameDeclaration) →
  (pf : C3S1.NucleusDeclaration.frame d ≡ f) →
  NucleusAdapter
mkNucleusAdapter d f pf =
  record { decl = d ; expFrame = f
         ; linkFrame = pf ; status = B.true }

isFilledNucleus : NucleusAdapter → B.Bool
isFilledNucleus a = NucleusAdapter.status a

record SublocaleAdapter : Set₁ where
  field
    decl : C3S1.SublocaleDeclaration
    expSublocale : C3S1.LocaleDeclaration
    expParent : C3S1.LocaleDeclaration
    linkSublocale : C3S1.SublocaleDeclaration.sublocale decl ≡ expSublocale
    linkParent : C3S1.SublocaleDeclaration.parentLocale decl ≡ expParent
    status : B.Bool

mkSublocaleAdapter :
  (d : C3S1.SublocaleDeclaration) →
  (sub : C3S1.LocaleDeclaration) →
  (par : C3S1.LocaleDeclaration) →
  (psub : C3S1.SublocaleDeclaration.sublocale d ≡ sub) →
  (ppar : C3S1.SublocaleDeclaration.parentLocale d ≡ par) →
  SublocaleAdapter
mkSublocaleAdapter d sub par psub ppar =
  record { decl = d ; expSublocale = sub ; expParent = par
         ; linkSublocale = psub ; linkParent = ppar ; status = B.true }

isFilledSublocale : SublocaleAdapter → B.Bool
isFilledSublocale a = SublocaleAdapter.status a

record OpenLocaleMorphismAdapter : Set₁ where
  field
    decl : C3S1.OpenLocaleMorphismDeclaration
    expMorphism : C3S1.LocaleMorphismDeclaration
    linkMorphism : C3S1.OpenLocaleMorphismDeclaration.localeMorphism decl ≡ expMorphism
    status : B.Bool

mkOpenLocaleMorphismAdapter :
  (d : C3S1.OpenLocaleMorphismDeclaration) →
  (m : C3S1.LocaleMorphismDeclaration) →
  (pm : C3S1.OpenLocaleMorphismDeclaration.localeMorphism d ≡ m) →
  OpenLocaleMorphismAdapter
mkOpenLocaleMorphismAdapter d m pm =
  record { decl = d ; expMorphism = m
         ; linkMorphism = pm ; status = B.true }

isFilledOpenLocaleMorphism : OpenLocaleMorphismAdapter → B.Bool
isFilledOpenLocaleMorphism a = OpenLocaleMorphismAdapter.status a

record SoberSpaceAdapter : Set₁ where
  field
    decl : C3S1.SoberSpaceDeclaration
    status : B.Bool

mkSoberSpaceAdapter :
  (d : C3S1.SoberSpaceDeclaration) →
  SoberSpaceAdapter
mkSoberSpaceAdapter d =
  record { decl = d ; status = B.true }

isFilledSoberSpace : SoberSpaceAdapter → B.Bool
isFilledSoberSpace a = SoberSpaceAdapter.status a

record SpatialLocaleAdapter : Set₁ where
  field
    decl : C3S1.SpatialLocaleDeclaration
    expLocale : C3S1.LocaleDeclaration
    linkLocale : C3S1.SpatialLocaleDeclaration.locale decl ≡ expLocale
    status : B.Bool

mkSpatialLocaleAdapter :
  (d : C3S1.SpatialLocaleDeclaration) →
  (loc : C3S1.LocaleDeclaration) →
  (ploc : C3S1.SpatialLocaleDeclaration.locale d ≡ loc) →
  SpatialLocaleAdapter
mkSpatialLocaleAdapter d loc ploc =
  record { decl = d ; expLocale = loc
         ; linkLocale = ploc ; status = B.true }

isFilledSpatialLocale : SpatialLocaleAdapter → B.Bool
isFilledSpatialLocale a = SpatialLocaleAdapter.status a

-- ==========================================================
-- Chapter 3, Level 3.2 (Sheaves & Toposes) - Additional
-- ==========================================================

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
  SheafOnLocaleAdapter
mkSheafOnLocaleAdapter d psh ppsh =
  record { decl = d ; expPresheaf = psh
         ; linkPresheaf = ppsh ; status = B.true }

isFilledSheafOnLocale : SheafOnLocaleAdapter → B.Bool
isFilledSheafOnLocale a = SheafOnLocaleAdapter.status a

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
  GrothendieckToposAdapter
mkGrothendieckToposAdapter d cat pcat =
  record { decl = d ; expCategory = cat
         ; linkCategory = pcat ; status = B.true }

isFilledGrothendieckTopos : GrothendieckToposAdapter → B.Bool
isFilledGrothendieckTopos a = GrothendieckToposAdapter.status a

record OmegaSetAdapter : Set₁ where
  field
    decl : C3S2.OmegaSetDeclarationVerified
    expData : C3S2.OmegaSetData
    linkData : C3S2.OmegaSetDeclarationVerified.dataOmegaSet decl ≡ expData
    status : B.Bool

mkOmegaSetAdapter :
  (d : C3S2.OmegaSetDeclarationVerified) →
  (data' : C3S2.OmegaSetData) →
  (pdata : C3S2.OmegaSetDeclarationVerified.dataOmegaSet d ≡ data') →
  OmegaSetAdapter
mkOmegaSetAdapter d data' pdata =
  record { decl = d ; expData = data'
         ; linkData = pdata ; status = B.true }

isFilledOmegaSet : OmegaSetAdapter → B.Bool
isFilledOmegaSet a = OmegaSetAdapter.status a

-- Presheaf on locale
record PresheafOnLocaleAdapter : Set₁ where
  field
    decl : C3S2.PresheafOnLocale
    status : B.Bool

mkPresheafOnLocaleAdapter : C3S2.PresheafOnLocale → PresheafOnLocaleAdapter
mkPresheafOnLocaleAdapter d = record { decl = d ; status = B.true }

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
  SheafGluingAxiomAdapter
mkSheafGluingAxiomAdapter d psh ppsh =
  record { decl = d ; expPresheaf = psh ; linkPresheaf = ppsh ; status = B.true }

isFilledSheafGluingAxiom : SheafGluingAxiomAdapter → B.Bool
isFilledSheafGluingAxiom a = SheafGluingAxiomAdapter.status a

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
  CategoryOfSheavesAdapter
mkCategoryOfSheavesAdapter d cat pcat =
  record { decl = d ; expCategory = cat ; linkCategory = pcat ; status = B.true }

isFilledCategoryOfSheaves : CategoryOfSheavesAdapter → B.Bool
isFilledCategoryOfSheaves a = CategoryOfSheavesAdapter.status a

-- Category of sheaves is a topos theorem
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
  CategoryOfSheavesIsAToposTheoremAdapter
mkCategoryOfSheavesIsAToposTheoremAdapter d sc tp psc ptp =
  record { decl = d ; expSheafCat = sc ; expTopos = tp
         ; linkSheafCat = psc ; linkTopos = ptp ; status = B.true }

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
  ExponentialObjectSheafAdapter
mkExponentialObjectSheafAdapter d b e pb pe =
  record { decl = d ; expBase = b ; expExponent = e
         ; linkBase = pb ; linkExponent = pe ; status = B.true }

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
  SubobjectClassifierAxiomAdapter
mkSubobjectClassifierAxiomAdapter d cm pcm =
  record { decl = d ; expCharMap = cm ; linkCharMap = pcm ; status = B.true }

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
  EtaleSpaceOverAdapter
mkEtaleSpaceOverAdapter d p pp =
  record { decl = d ; expProj = p ; linkProj = pp ; status = B.true }

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
  CategoryOfEtaleSpacesAdapter
mkCategoryOfEtaleSpacesAdapter d cat pcat =
  record { decl = d ; expCategory = cat ; linkCategory = pcat ; status = B.true }

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
  StalkConstructorAdapter
mkStalkConstructorAdapter d psh ppsh =
  record { decl = d ; expPresheaf = psh ; linkPresheaf = ppsh ; status = B.true }

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
  TotalSpaceOfStalksAdapter
mkTotalSpaceOfStalksAdapter d psh ppsh =
  record { decl = d ; expPresheaf = psh ; linkPresheaf = ppsh ; status = B.true }

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
  SheafOfSectionsFunctorAdapter
mkSheafOfSectionsFunctorAdapter d et sh pet psh =
  record { decl = d ; expEtale = et ; expSheaf = sh
         ; linkEtale = pet ; linkSheaf = psh ; status = B.true }

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
  SheafEtaleEquivalenceTheoremAdapter
mkSheafEtaleEquivalenceTheoremAdapter d sc ec sf tf psc pec psf ptf =
  record { decl = d ; expSheafCat = sc ; expEtaleCat = ec
         ; expStalksF = sf ; expSectionsF = tf
         ; linkSheafCat = psc ; linkEtaleCat = pec
         ; linkStalksF = psf ; linkSectionsF = ptf ; status = B.true }

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
  DirectImageFunctorLocaleAdapter
mkDirectImageFunctorLocaleAdapter d f pf =
  record { decl = d ; expFunctor = f ; linkFunctor = pf ; status = B.true }

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
  InverseImageFunctorLocaleAdapter
mkInverseImageFunctorLocaleAdapter d f pf =
  record { decl = d ; expFunctor = f ; linkFunctor = pf ; status = B.true }

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
  LocaleChangeOfBaseAdjunctionTheoremAdapter
mkLocaleChangeOfBaseAdjunctionTheoremAdapter d inv dir adj pinv pdir padj =
  record { decl = d ; expInverse = inv ; expDirect = dir ; expAdj = adj
         ; linkInverse = pinv ; linkDirect = pdir ; linkAdj = padj ; status = B.true }

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
  EtaleMorphismInducesSheafEquivalenceTheoremAdapter
mkEtaleMorphismInducesSheafEquivalenceTheoremAdapter d inv pinv =
  record { decl = d ; expInverse = inv ; linkInverse = pinv ; status = B.true }

isFilledEtaleMorphismInducesSheafEquivalenceTheorem : EtaleMorphismInducesSheafEquivalenceTheoremAdapter → B.Bool
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
  SheavesAreCompleteOmegaSetsRefinedTheoremAdapter
mkSheavesAreCompleteOmegaSetsRefinedTheoremAdapter d sc oc fa fs psc poc pfa pfs =
  record { decl = d ; expSheafCat = sc ; expOmegaCat = oc
         ; expFunctorA = fa ; expFunctorS = fs
         ; linkSheafCat = psc ; linkOmegaCat = poc
         ; linkFunctorA = pfa ; linkFunctorS = pfs ; status = B.true }

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
  SheafOfRingsAdapter
mkSheafOfRingsAdapter d sh psh =
  record { decl = d ; expSheaf = sh ; linkSheaf = psh ; status = B.true }

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
  SheafOfOModulesAdapter
mkSheafOfOModulesAdapter d rs ms prs pms =
  record { decl = d ; expRingSheaf = rs ; expModSheaf = ms
         ; linkRingSheaf = prs ; linkModSheaf = pms ; status = B.true }

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
  CategoryOfOModulesIsAbelianCorollaryAdapter
mkCategoryOfOModulesIsAbelianCorollaryAdapter d rs cat prs pcat =
  record { decl = d ; expRingSheaf = rs ; expCategory = cat
         ; linkRingSheaf = prs ; linkCategory = pcat ; status = B.true }

isFilledCategoryOfOModulesIsAbelianCorollary : CategoryOfOModulesIsAbelianCorollaryAdapter → B.Bool
isFilledCategoryOfOModulesIsAbelianCorollary a = CategoryOfOModulesIsAbelianCorollaryAdapter.status a

------------------------------------------------------------------------
-- Algebra Adapters
------------------------------------------------------------------------

-- Magma
record MagmaAdapter : Set₁ where
  field
    decl : AFo.MagmaDeclaration
    status : B.Bool

mkMagmaAdapter : AFo.MagmaDeclaration → MagmaAdapter
mkMagmaAdapter d = record { decl = d ; status = B.true }

isFilledMagma : MagmaAdapter → B.Bool
isFilledMagma a = MagmaAdapter.status a

-- Semigroup
record SemigroupAdapter : Set₁ where
  field
    decl : AFo.SemigroupDeclaration
    expMagma : AFo.MagmaDeclaration
    linkMagma : AFo.SemigroupDeclaration.underlyingMagma decl ≡ expMagma
    status : B.Bool

mkSemigroupAdapter :
  (d : AFo.SemigroupDeclaration) →
  (m : AFo.MagmaDeclaration) →
  (pm : AFo.SemigroupDeclaration.underlyingMagma d ≡ m) →
  SemigroupAdapter
mkSemigroupAdapter d m pm =
  record { decl = d ; expMagma = m ; linkMagma = pm ; status = B.true }

isFilledSemigroup : SemigroupAdapter → B.Bool
isFilledSemigroup a = SemigroupAdapter.status a

-- Monoid
record MonoidAdapter : Set₁ where
  field
    decl : AFo.MonoidDeclaration
    expSemigroup : AFo.SemigroupDeclaration
    linkSemigroup : AFo.MonoidDeclaration.underlyingSemigroup decl ≡ expSemigroup
    status : B.Bool

mkMonoidAdapter :
  (d : AFo.MonoidDeclaration) →
  (s : AFo.SemigroupDeclaration) →
  (ps : AFo.MonoidDeclaration.underlyingSemigroup d ≡ s) →
  MonoidAdapter
mkMonoidAdapter d s ps =
  record { decl = d ; expSemigroup = s ; linkSemigroup = ps ; status = B.true }

isFilledMonoid : MonoidAdapter → B.Bool
isFilledMonoid a = MonoidAdapter.status a

-- Group
record GroupAdapter : Set₁ where
  field
    decl : AFo.GroupDeclaration
    expMonoid : AFo.MonoidDeclaration
    linkMonoid : AFo.GroupDeclaration.underlyingMonoid decl ≡ expMonoid
    status : B.Bool

mkGroupAdapter :
  (d : AFo.GroupDeclaration) →
  (m : AFo.MonoidDeclaration) →
  (pm : AFo.GroupDeclaration.underlyingMonoid d ≡ m) →
  GroupAdapter
mkGroupAdapter d m pm =
  record { decl = d ; expMonoid = m ; linkMonoid = pm ; status = B.true }

isFilledGroup : GroupAdapter → B.Bool
isFilledGroup a = GroupAdapter.status a

-- AbelianGroup
record AbelianGroupAdapter : Set₁ where
  field
    decl : AFo.AbelianGroupDeclaration
    expGroup : AFo.GroupDeclaration
    linkGroup : AFo.AbelianGroupDeclaration.underlyingGroup decl ≡ expGroup
    status : B.Bool

mkAbelianGroupAdapter :
  (d : AFo.AbelianGroupDeclaration) →
  (g : AFo.GroupDeclaration) →
  (pg : AFo.AbelianGroupDeclaration.underlyingGroup d ≡ g) →
  AbelianGroupAdapter
mkAbelianGroupAdapter d g pg =
  record { decl = d ; expGroup = g ; linkGroup = pg ; status = B.true }

isFilledAbelianGroup : AbelianGroupAdapter → B.Bool
isFilledAbelianGroup a = AbelianGroupAdapter.status a

-- Ring
record RingAdapter : Set₁ where
  field
    decl : AR.RingDeclaration
    expAdditiveGroup : AFo.AbelianGroupDeclaration
    linkAdditiveGroup : AR.RingDeclaration.additiveGroup decl ≡ expAdditiveGroup
    status : B.Bool

mkRingAdapter :
  (d : AR.RingDeclaration) →
  (ag : AFo.AbelianGroupDeclaration) →
  (pag : AR.RingDeclaration.additiveGroup d ≡ ag) →
  RingAdapter
mkRingAdapter d ag pag =
  record { decl = d ; expAdditiveGroup = ag ; linkAdditiveGroup = pag ; status = B.true }

isFilledRing : RingAdapter → B.Bool
isFilledRing a = RingAdapter.status a

-- UnitalRing
record UnitalRingAdapter : Set₁ where
  field
    decl : AR.UnitalRingDeclaration
    expRing : AR.RingDeclaration
    linkRing : AR.UnitalRingDeclaration.underlyingRing decl ≡ expRing
    status : B.Bool

mkUnitalRingAdapter :
  (d : AR.UnitalRingDeclaration) →
  (r : AR.RingDeclaration) →
  (pr : AR.UnitalRingDeclaration.underlyingRing d ≡ r) →
  UnitalRingAdapter
mkUnitalRingAdapter d r pr =
  record { decl = d ; expRing = r ; linkRing = pr ; status = B.true }

isFilledUnitalRing : UnitalRingAdapter → B.Bool
isFilledUnitalRing a = UnitalRingAdapter.status a

-- CommutativeRing
record CommutativeRingAdapter : Set₁ where
  field
    decl : AR.CommutativeRingDeclaration
    expUnitalRing : AR.UnitalRingDeclaration
    linkUnitalRing : AR.CommutativeRingDeclaration.underlyingRing decl ≡ expUnitalRing
    status : B.Bool

mkCommutativeRingAdapter :
  (d : AR.CommutativeRingDeclaration) →
  (ur : AR.UnitalRingDeclaration) →
  (pur : AR.CommutativeRingDeclaration.underlyingRing d ≡ ur) →
  CommutativeRingAdapter
mkCommutativeRingAdapter d ur pur =
  record { decl = d ; expUnitalRing = ur ; linkUnitalRing = pur ; status = B.true }

isFilledCommutativeRing : CommutativeRingAdapter → B.Bool
isFilledCommutativeRing a = CommutativeRingAdapter.status a

-- DivisionRing
record DivisionRingAdapter : Set₁ where
  field
    decl : AR.DivisionRingDeclaration
    expUnitalRing : AR.UnitalRingDeclaration
    linkUnitalRing : AR.DivisionRingDeclaration.underlyingRing decl ≡ expUnitalRing
    status : B.Bool

mkDivisionRingAdapter :
  (d : AR.DivisionRingDeclaration) →
  (ur : AR.UnitalRingDeclaration) →
  (pur : AR.DivisionRingDeclaration.underlyingRing d ≡ ur) →
  DivisionRingAdapter
mkDivisionRingAdapter d ur pur =
  record { decl = d ; expUnitalRing = ur ; linkUnitalRing = pur ; status = B.true }

isFilledDivisionRing : DivisionRingAdapter → B.Bool
isFilledDivisionRing a = DivisionRingAdapter.status a

-- Field
record FieldAdapter : Set₁ where
  field
    decl : AR.FieldDeclaration
    expCommutativeRing : AR.CommutativeRingDeclaration
    linkCommutativeRing : AR.FieldDeclaration.underlyingRing decl ≡ expCommutativeRing
    status : B.Bool

mkFieldAdapter :
  (d : AR.FieldDeclaration) →
  (cr : AR.CommutativeRingDeclaration) →
  (pcr : AR.FieldDeclaration.underlyingRing d ≡ cr) →
  FieldAdapter
mkFieldAdapter d cr pcr =
  record { decl = d ; expCommutativeRing = cr ; linkCommutativeRing = pcr ; status = B.true }

isFilledField : FieldAdapter → B.Bool
isFilledField a = FieldAdapter.status a

-- ==========================================================
-- Core.UniversalProperties: adapters for general UMPs
-- ==========================================================

-- Initial object
record InitialObjectAdapter : Set₁ where
  field
    decl : CUP.InitialObject
    expInitial : M.Identifier
    linkInitial : CUP.InitialObject.initial decl ≡ expInitial
    status : B.Bool

mkInitialObjectAdapter :
  (d : CUP.InitialObject) →
  (i : M.Identifier) →
  (pi : CUP.InitialObject.initial d ≡ i) →
  InitialObjectAdapter
mkInitialObjectAdapter d i pi =
  record { decl = d ; expInitial = i ; linkInitial = pi ; status = B.true }

isFilledInitialObject : InitialObjectAdapter → B.Bool
isFilledInitialObject a = InitialObjectAdapter.status a

-- Terminal object
record TerminalObjectAdapter : Set₁ where
  field
    decl : CUP.TerminalObject
    expTerminal : M.Identifier
    linkTerminal : CUP.TerminalObject.terminal decl ≡ expTerminal
    status : B.Bool

mkTerminalObjectAdapter :
  (d : CUP.TerminalObject) →
  (t : M.Identifier) →
  (pt : CUP.TerminalObject.terminal d ≡ t) →
  TerminalObjectAdapter
mkTerminalObjectAdapter d t pt =
  record { decl = d ; expTerminal = t ; linkTerminal = pt ; status = B.true }

isFilledTerminalObject : TerminalObjectAdapter → B.Bool
isFilledTerminalObject a = TerminalObjectAdapter.status a

-- Product property
record ProductPropertyAdapter : Set₁ where
  field
    A B : M.Identifier
    decl : CUP.ProductProperty A B
    expProduct : M.Identifier
    linkProduct : CUP.ProductProperty.product decl ≡ expProduct
    status : B.Bool

mkProductPropertyAdapter :
  (A B : M.Identifier) →
  (d : CUP.ProductProperty A B) →
  (p : M.Identifier) →
  (pp : CUP.ProductProperty.product d ≡ p) →
  ProductPropertyAdapter
mkProductPropertyAdapter A B d p pp =
  record { A = A ; B = B ; decl = d ; expProduct = p ; linkProduct = pp ; status = B.true }

isFilledProductProperty : ProductPropertyAdapter → B.Bool
isFilledProductProperty a = ProductPropertyAdapter.status a

-- Coproduct property
record CoproductPropertyAdapter : Set₁ where
  field
    A B : M.Identifier
    decl : CUP.CoproductProperty A B
    expCoproduct : M.Identifier
    linkCoproduct : CUP.CoproductProperty.coproduct decl ≡ expCoproduct
    status : B.Bool

mkCoproductPropertyAdapter :
  (A B : M.Identifier) →
  (d : CUP.CoproductProperty A B) →
  (c : M.Identifier) →
  (pc : CUP.CoproductProperty.coproduct d ≡ c) →
  CoproductPropertyAdapter
mkCoproductPropertyAdapter A B d c pc =
  record { A = A ; B = B ; decl = d ; expCoproduct = c ; linkCoproduct = pc ; status = B.true }

isFilledCoproductProperty : CoproductPropertyAdapter → B.Bool
isFilledCoproductProperty a = CoproductPropertyAdapter.status a

-- Equalizer property
record EqualizerPropertyAdapter : Set₁ where
  field
    A B f g : M.Identifier
    decl : CUP.EqualizerProperty A B f g
    expEqualizer : M.Identifier
    linkEqualizer : CUP.EqualizerProperty.equalizer decl ≡ expEqualizer
    status : B.Bool

mkEqualizerPropertyAdapter :
  (A B f g : M.Identifier) →
  (d : CUP.EqualizerProperty A B f g) →
  (e : M.Identifier) →
  (pe : CUP.EqualizerProperty.equalizer d ≡ e) →
  EqualizerPropertyAdapter
mkEqualizerPropertyAdapter A B f g d e pe =
  record { A = A ; B = B ; f = f ; g = g ; decl = d ; expEqualizer = e ; linkEqualizer = pe ; status = B.true }

isFilledEqualizerProperty : EqualizerPropertyAdapter → B.Bool
isFilledEqualizerProperty a = EqualizerPropertyAdapter.status a

-- Coequalizer property
record CoequalizerPropertyAdapter : Set₁ where
  field
    A B f g : M.Identifier
    decl : CUP.CoequalizerProperty A B f g
    expCoequalizer : M.Identifier
    linkCoequalizer : CUP.CoequalizerProperty.coequalizer decl ≡ expCoequalizer
    status : B.Bool

mkCoequalizerPropertyAdapter :
  (A B f g : M.Identifier) →
  (d : CUP.CoequalizerProperty A B f g) →
  (e : M.Identifier) →
  (pe : CUP.CoequalizerProperty.coequalizer d ≡ e) →
  CoequalizerPropertyAdapter
mkCoequalizerPropertyAdapter A B f g d e pe =
  record { A = A ; B = B ; f = f ; g = g ; decl = d ; expCoequalizer = e ; linkCoequalizer = pe ; status = B.true }

isFilledCoequalizerProperty : CoequalizerPropertyAdapter → B.Bool
isFilledCoequalizerProperty a = CoequalizerPropertyAdapter.status a

-- Pullback property
record PullbackPropertyAdapter : Set₁ where
  field
    A B C f g : M.Identifier
    decl : CUP.PullbackProperty A B C f g
    expPullback : M.Identifier
    linkPullback : CUP.PullbackProperty.pullback decl ≡ expPullback
    status : B.Bool

mkPullbackPropertyAdapter :
  (A B C f g : M.Identifier) →
  (d : CUP.PullbackProperty A B C f g) →
  (p : M.Identifier) →
  (pp : CUP.PullbackProperty.pullback d ≡ p) →
  PullbackPropertyAdapter
mkPullbackPropertyAdapter A B C f g d p pp =
  record { A = A ; B = B ; C = C ; f = f ; g = g ; decl = d ; expPullback = p ; linkPullback = pp ; status = B.true }

isFilledPullbackProperty : PullbackPropertyAdapter → B.Bool
isFilledPullbackProperty a = PullbackPropertyAdapter.status a

-- Pushout property
record PushoutPropertyAdapter : Set₁ where
  field
    A B C f g : M.Identifier
    decl : CUP.PushoutProperty A B C f g
    expPushout : M.Identifier
    linkPushout : CUP.PushoutProperty.pushout decl ≡ expPushout
    status : B.Bool

mkPushoutPropertyAdapter :
  (A B C f g : M.Identifier) →
  (d : CUP.PushoutProperty A B C f g) →
  (p : M.Identifier) →
  (pp : CUP.PushoutProperty.pushout d ≡ p) →
  PushoutPropertyAdapter
mkPushoutPropertyAdapter A B C f g d p pp =
  record { A = A ; B = B ; C = C ; f = f ; g = g ; decl = d ; expPushout = p ; linkPushout = pp ; status = B.true }

isFilledPushoutProperty : PushoutPropertyAdapter → B.Bool
isFilledPushoutProperty a = PushoutPropertyAdapter.status a

-- Limit property
record LimitPropertyAdapter : Set₁ where
  field
    D : M.Identifier
    decl : CUP.LimitProperty D
    expLimit : M.Identifier
    linkLimit : CUP.LimitProperty.limit decl ≡ expLimit
    status : B.Bool

mkLimitPropertyAdapter :
  (D : M.Identifier) →
  (d : CUP.LimitProperty D) →
  (l : M.Identifier) →
  (pl : CUP.LimitProperty.limit d ≡ l) →
  LimitPropertyAdapter
mkLimitPropertyAdapter D d l pl =
  record { D = D ; decl = d ; expLimit = l ; linkLimit = pl ; status = B.true }

isFilledLimitProperty : LimitPropertyAdapter → B.Bool
isFilledLimitProperty a = LimitPropertyAdapter.status a

-- Colimit property
record ColimitPropertyAdapter : Set₁ where
  field
    D : M.Identifier
    decl : CUP.ColimitProperty D
    expColimit : M.Identifier
    linkColimit : CUP.ColimitProperty.colimit decl ≡ expColimit
    status : B.Bool

mkColimitPropertyAdapter :
  (D : M.Identifier) →
  (d : CUP.ColimitProperty D) →
  (c : M.Identifier) →
  (pc : CUP.ColimitProperty.colimit d ≡ c) →
  ColimitPropertyAdapter
mkColimitPropertyAdapter D d c pc =
  record { D = D ; decl = d ; expColimit = c ; linkColimit = pc ; status = B.true }

isFilledColimitProperty : ColimitPropertyAdapter → B.Bool
isFilledColimitProperty a = ColimitPropertyAdapter.status a

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
    status : B.Bool

mkLeftModuleAdapter :
  (R : AR.RingDeclaration) →
  (d : AM.LeftModule R) →
  (er : AR.RingDeclaration) →
  (pr : AM.LeftModule.ring d ≡ er) →
  LeftModuleAdapter
mkLeftModuleAdapter R d er pr =
  record { R = R ; decl = d ; expRing = er ; linkRing = pr ; status = B.true }

isFilledLeftModule : LeftModuleAdapter → B.Bool
isFilledLeftModule a = LeftModuleAdapter.status a

-- Module homomorphism
record ModuleHomomorphismAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    M N : AM.LeftModule R
    decl : AM.ModuleHomomorphism R M N
    expRing : AR.RingDeclaration
    linkRing : AM.ModuleHomomorphism.ring decl ≡ expRing
    status : B.Bool

mkModuleHomomorphismAdapter :
  (R : AR.RingDeclaration) →
  (M N : AM.LeftModule R) →
  (d : AM.ModuleHomomorphism R M N) →
  (er : AR.RingDeclaration) →
  (pr : AM.ModuleHomomorphism.ring d ≡ er) →
  ModuleHomomorphismAdapter
mkModuleHomomorphismAdapter R M N d er pr =
  record { R = R ; M = M ; N = N ; decl = d ; expRing = er ; linkRing = pr ; status = B.true }

isFilledModuleHom : ModuleHomomorphismAdapter → B.Bool
isFilledModuleHom a = ModuleHomomorphismAdapter.status a

-- Submodule
record SubmoduleAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    M : AM.LeftModule R
    decl : AM.Submodule R M
    expRing : AR.RingDeclaration
    linkRing : AM.Submodule.ring decl ≡ expRing
    status : B.Bool

mkSubmoduleAdapter :
  (R : AR.RingDeclaration) →
  (M : AM.LeftModule R) →
  (d : AM.Submodule R M) →
  (er : AR.RingDeclaration) →
  (pr : AM.Submodule.ring d ≡ er) →
  SubmoduleAdapter
mkSubmoduleAdapter R M d er pr =
  record { R = R ; M = M ; decl = d ; expRing = er ; linkRing = pr ; status = B.true }

isFilledSubmodule : SubmoduleAdapter → B.Bool
isFilledSubmodule a = SubmoduleAdapter.status a

-- Quotient module
record QuotientModuleAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    M : AM.LeftModule R
    N : AM.Submodule R M
    decl : AM.QuotientModule R M N
    expRing : AR.RingDeclaration
    linkRing : AM.QuotientModule.ring decl ≡ expRing
    status : B.Bool

mkQuotientModuleAdapter :
  (R : AR.RingDeclaration) →
  (M : AM.LeftModule R) →
  (N : AM.Submodule R M) →
  (d : AM.QuotientModule R M N) →
  (er : AR.RingDeclaration) →
  (pr : AM.QuotientModule.ring d ≡ er) →
  QuotientModuleAdapter
mkQuotientModuleAdapter R M N d er pr =
  record { R = R ; M = M ; N = N ; decl = d ; expRing = er ; linkRing = pr ; status = B.true }

isFilledQuotientModule : QuotientModuleAdapter → B.Bool
isFilledQuotientModule a = QuotientModuleAdapter.status a

-- Kernel of module homomorphism
record KernelOfModuleHomomorphismAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    f : M.Identifier
    decl : AM.KernelOfModuleHomomorphism R f
    expRing : AR.RingDeclaration
    linkRing : AM.KernelOfModuleHomomorphism.ring decl ≡ expRing
    status : B.Bool

mkKernelOfModuleHomomorphismAdapter :
  (R : AR.RingDeclaration) →
  (f : M.Identifier) →
  (d : AM.KernelOfModuleHomomorphism R f) →
  (er : AR.RingDeclaration) →
  (pr : AM.KernelOfModuleHomomorphism.ring d ≡ er) →
  KernelOfModuleHomomorphismAdapter
mkKernelOfModuleHomomorphismAdapter R f d er pr =
  record { R = R ; f = f ; decl = d ; expRing = er ; linkRing = pr ; status = B.true }

isFilledKernelModuleHom : KernelOfModuleHomomorphismAdapter → B.Bool
isFilledKernelModuleHom a = KernelOfModuleHomomorphismAdapter.status a

-- Image of module homomorphism
record ImageOfModuleHomomorphismAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    f : M.Identifier
    decl : AM.ImageOfModuleHomomorphism R f
    expRing : AR.RingDeclaration
    linkRing : AM.ImageOfModuleHomomorphism.ring decl ≡ expRing
    status : B.Bool

mkImageOfModuleHomomorphismAdapter :
  (R : AR.RingDeclaration) →
  (f : M.Identifier) →
  (d : AM.ImageOfModuleHomomorphism R f) →
  (er : AR.RingDeclaration) →
  (pr : AM.ImageOfModuleHomomorphism.ring d ≡ er) →
  ImageOfModuleHomomorphismAdapter
mkImageOfModuleHomomorphismAdapter R f d er pr =
  record { R = R ; f = f ; decl = d ; expRing = er ; linkRing = pr ; status = B.true }

isFilledImageModuleHom : ImageOfModuleHomomorphismAdapter → B.Bool
isFilledImageModuleHom a = ImageOfModuleHomomorphismAdapter.status a

-- Cokernel of module homomorphism
record CokernelOfModuleHomomorphismAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    f : M.Identifier
    decl : AM.CokernelOfModuleHomomorphism R f
    expRing : AR.RingDeclaration
    linkRing : AM.CokernelOfModuleHomomorphism.ring decl ≡ expRing
    status : B.Bool

mkCokernelOfModuleHomomorphismAdapter :
  (R : AR.RingDeclaration) →
  (f : M.Identifier) →
  (d : AM.CokernelOfModuleHomomorphism R f) →
  (er : AR.RingDeclaration) →
  (pr : AM.CokernelOfModuleHomomorphism.ring d ≡ er) →
  CokernelOfModuleHomomorphismAdapter
mkCokernelOfModuleHomomorphismAdapter R f d er pr =
  record { R = R ; f = f ; decl = d ; expRing = er ; linkRing = pr ; status = B.true }

isFilledCokernelModuleHom : CokernelOfModuleHomomorphismAdapter → B.Bool
isFilledCokernelModuleHom a = CokernelOfModuleHomomorphismAdapter.status a

-- Exact sequence (modules)
record ModuleExactSequenceAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    decl : AM.ExactSequence R
    expRing : AR.RingDeclaration
    linkRing : AM.ExactSequence.ring decl ≡ expRing
    status : B.Bool

mkModuleExactSequenceAdapter :
  (R : AR.RingDeclaration) →
  (d : AM.ExactSequence R) →
  (er : AR.RingDeclaration) →
  (pr : AM.ExactSequence.ring d ≡ er) →
  ModuleExactSequenceAdapter
mkModuleExactSequenceAdapter R d er pr =
  record { R = R ; decl = d ; expRing = er ; linkRing = pr ; status = B.true }

isFilledModuleExactSequence : ModuleExactSequenceAdapter → B.Bool
isFilledModuleExactSequence a = ModuleExactSequenceAdapter.status a

-- Short exact sequence (modules)
record ModuleShortExactSequenceAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    decl : AM.ShortExactSequence R
    expRing : AR.RingDeclaration
    linkRing : AM.ShortExactSequence.ring decl ≡ expRing
    status : B.Bool

mkModuleShortExactSequenceAdapter :
  (R : AR.RingDeclaration) →
  (d : AM.ShortExactSequence R) →
  (er : AR.RingDeclaration) →
  (pr : AM.ShortExactSequence.ring d ≡ er) →
  ModuleShortExactSequenceAdapter
mkModuleShortExactSequenceAdapter R d er pr =
  record { R = R ; decl = d ; expRing = er ; linkRing = pr ; status = B.true }

isFilledModuleShortExactSequence : ModuleShortExactSequenceAdapter → B.Bool
isFilledModuleShortExactSequence a = ModuleShortExactSequenceAdapter.status a

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
    status : B.Bool

mkInseparableExtensionAdapter :
  (F E : AR.FieldDeclaration) →
  (d : AFA.InseparableExtension F E) →
  (eb : AR.FieldDeclaration) →
  (pb : AFA.InseparableExtension.baseField d ≡ eb) →
  InseparableExtensionAdapter
mkInseparableExtensionAdapter F E d eb pb =
  record { F = F ; E = E ; decl = d ; expBase = eb ; linkBase = pb ; status = B.true }

isFilledInseparableExtension : InseparableExtensionAdapter → B.Bool
isFilledInseparableExtension a = InseparableExtensionAdapter.status a

-- Purely inseparable extension
record PurelyInseparableExtensionAdapter : Set₁ where
  field
    F E : AR.FieldDeclaration
    decl : AFA.PurelyInseparableExtension F E
    expBase : AR.FieldDeclaration
    linkBase : AFA.PurelyInseparableExtension.baseField decl ≡ expBase
    status : B.Bool

mkPurelyInseparableExtensionAdapter :
  (F E : AR.FieldDeclaration) →
  (d : AFA.PurelyInseparableExtension F E) →
  (eb : AR.FieldDeclaration) →
  (pb : AFA.PurelyInseparableExtension.baseField d ≡ eb) →
  PurelyInseparableExtensionAdapter
mkPurelyInseparableExtensionAdapter F E d eb pb =
  record { F = F ; E = E ; decl = d ; expBase = eb ; linkBase = pb ; status = B.true }

isFilledPurelyInseparableExtension : PurelyInseparableExtensionAdapter → B.Bool
isFilledPurelyInseparableExtension a = PurelyInseparableExtensionAdapter.status a

-- Perfect field
record PerfectFieldAdapter : Set₁ where
  field
    F : AR.FieldDeclaration
    decl : AFA.PerfectField F
    expBase : AR.FieldDeclaration
    linkBase : AFA.PerfectField.baseField decl ≡ expBase
    status : B.Bool

mkPerfectFieldAdapter :
  (F : AR.FieldDeclaration) →
  (d : AFA.PerfectField F) →
  (eb : AR.FieldDeclaration) →
  (pb : AFA.PerfectField.baseField d ≡ eb) →
  PerfectFieldAdapter
mkPerfectFieldAdapter F d eb pb =
  record { F = F ; decl = d ; expBase = eb ; linkBase = pb ; status = B.true }

isFilledPerfectField : PerfectFieldAdapter → B.Bool
isFilledPerfectField a = PerfectFieldAdapter.status a

-- Algebraically closed field
record AlgebraicallyClosedFieldAdapter : Set₁ where
  field
    F : AR.FieldDeclaration
    decl : AFA.AlgebraicallyClosedField F
    expBase : AR.FieldDeclaration
    linkBase : AFA.AlgebraicallyClosedField.baseField decl ≡ expBase
    status : B.Bool

mkAlgebraicallyClosedFieldAdapter :
  (F : AR.FieldDeclaration) →
  (d : AFA.AlgebraicallyClosedField F) →
  (eb : AR.FieldDeclaration) →
  (pb : AFA.AlgebraicallyClosedField.baseField d ≡ eb) →
  AlgebraicallyClosedFieldAdapter
mkAlgebraicallyClosedFieldAdapter F d eb pb =
  record { F = F ; decl = d ; expBase = eb ; linkBase = pb ; status = B.true }

isFilledAlgebraicallyClosedField : AlgebraicallyClosedFieldAdapter → B.Bool
isFilledAlgebraicallyClosedField a = AlgebraicallyClosedFieldAdapter.status a

-- Normal closure
record NormalClosureAdapter : Set₁ where
  field
    F E : AR.FieldDeclaration
    decl : AFA.NormalClosure F E
    expNormal : AR.FieldDeclaration
    linkNormal : AFA.NormalClosure.normalClosure decl ≡ expNormal
    status : B.Bool

mkNormalClosureAdapter :
  (F E : AR.FieldDeclaration) →
  (d : AFA.NormalClosure F E) →
  (en : AR.FieldDeclaration) →
  (pn : AFA.NormalClosure.normalClosure d ≡ en) →
  NormalClosureAdapter
mkNormalClosureAdapter F E d en pn =
  record { F = F ; E = E ; decl = d ; expNormal = en ; linkNormal = pn ; status = B.true }

isFilledNormalClosure : NormalClosureAdapter → B.Bool
isFilledNormalClosure a = NormalClosureAdapter.status a

-- Galois closure
record GaloisClosureAdapter : Set₁ where
  field
    F E : AR.FieldDeclaration
    decl : AFA.GaloisClosure F E
    expGalois : AR.FieldDeclaration
    linkGalois : AFA.GaloisClosure.galoisClosure decl ≡ expGalois
    status : B.Bool

mkGaloisClosureAdapter :
  (F E : AR.FieldDeclaration) →
  (d : AFA.GaloisClosure F E) →
  (eg : AR.FieldDeclaration) →
  (pg : AFA.GaloisClosure.galoisClosure d ≡ eg) →
  GaloisClosureAdapter
mkGaloisClosureAdapter F E d eg pg =
  record { F = F ; E = E ; decl = d ; expGalois = eg ; linkGalois = pg ; status = B.true }

isFilledGaloisClosure : GaloisClosureAdapter → B.Bool
isFilledGaloisClosure a = GaloisClosureAdapter.status a

-- Frobenius endomorphism
record FrobeniusEndomorphismAdapter : Set₁ where
  field
    F : AR.FieldDeclaration
    decl : AFA.FrobeniusEndomorphism F
    expBase : AR.FieldDeclaration
    linkBase : AFA.FrobeniusEndomorphism.baseField decl ≡ expBase
    status : B.Bool

mkFrobeniusEndomorphismAdapter :
  (F : AR.FieldDeclaration) →
  (d : AFA.FrobeniusEndomorphism F) →
  (eb : AR.FieldDeclaration) →
  (pb : AFA.FrobeniusEndomorphism.baseField d ≡ eb) →
  FrobeniusEndomorphismAdapter
mkFrobeniusEndomorphismAdapter F d eb pb =
  record { F = F ; decl = d ; expBase = eb ; linkBase = pb ; status = B.true }

isFilledFrobeniusEndomorphism : FrobeniusEndomorphismAdapter → B.Bool
isFilledFrobeniusEndomorphism a = FrobeniusEndomorphismAdapter.status a

-- Rational function field
record RationalFunctionFieldAdapter : Set₁ where
  field
    K : AR.FieldDeclaration
    decl : AFA.RationalFunctionField K
    expFunctionField : AR.FieldDeclaration
    linkFunctionField : AFA.RationalFunctionField.functionField decl ≡ expFunctionField
    status : B.Bool

mkRationalFunctionFieldAdapter :
  (K : AR.FieldDeclaration) →
  (d : AFA.RationalFunctionField K) →
  (eff : AR.FieldDeclaration) →
  (pf : AFA.RationalFunctionField.functionField d ≡ eff) →
  RationalFunctionFieldAdapter
mkRationalFunctionFieldAdapter K d eff pf =
  record { K = K ; decl = d ; expFunctionField = eff ; linkFunctionField = pf ; status = B.true }

isFilledRationalFunctionField : RationalFunctionFieldAdapter → B.Bool
isFilledRationalFunctionField a = RationalFunctionFieldAdapter.status a

-- Algebraic function field
record AlgebraicFunctionFieldAdapter : Set₁ where
  field
    K : AR.FieldDeclaration
    decl : AFA.AlgebraicFunctionField K
    expFunctionField : AR.FieldDeclaration
    linkFunctionField : AFA.AlgebraicFunctionField.functionField decl ≡ expFunctionField
    status : B.Bool

mkAlgebraicFunctionFieldAdapter :
  (K : AR.FieldDeclaration) →
  (d : AFA.AlgebraicFunctionField K) →
  (eff : AR.FieldDeclaration) →
  (pf : AFA.AlgebraicFunctionField.functionField d ≡ eff) →
  AlgebraicFunctionFieldAdapter
mkAlgebraicFunctionFieldAdapter K d eff pf =
  record { K = K ; decl = d ; expFunctionField = eff ; linkFunctionField = pf ; status = B.true }

isFilledAlgebraicFunctionField : AlgebraicFunctionFieldAdapter → B.Bool
isFilledAlgebraicFunctionField a = AlgebraicFunctionFieldAdapter.status a

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
    status : B.Bool

mkSubfieldAdapter :
  (F : AR.FieldDeclaration) →
  (d : AFB.Subfield F) →
  (es : AR.FieldDeclaration) →
  (ps : AFB.Subfield.subfield d ≡ es) →
  SubfieldAdapter
mkSubfieldAdapter F d es ps =
  record { F = F ; decl = d ; expSubfield = es ; linkSubfield = ps ; status = B.true }

isFilledSubfield : SubfieldAdapter → B.Bool
isFilledSubfield a = SubfieldAdapter.status a

-- Field extension
record FieldExtensionAdapter : Set₁ where
  field
    F E : AR.FieldDeclaration
    decl : AFB.FieldExtension F E
    expBase : AR.FieldDeclaration
    linkBase : AFB.FieldExtension.baseField decl ≡ expBase
    status : B.Bool

mkFieldExtensionAdapter :
  (F E : AR.FieldDeclaration) →
  (d : AFB.FieldExtension F E) →
  (eb : AR.FieldDeclaration) →
  (pb : AFB.FieldExtension.baseField d ≡ eb) →
  FieldExtensionAdapter
mkFieldExtensionAdapter F E d eb pb =
  record { F = F ; E = E ; decl = d ; expBase = eb ; linkBase = pb ; status = B.true }

isFilledFieldExtension : FieldExtensionAdapter → B.Bool
isFilledFieldExtension a = FieldExtensionAdapter.status a

-- Algebraic element
record AlgebraicElementAdapter : Set₁ where
  field
    F E : AR.FieldDeclaration
    α : M.Identifier
    decl : AFB.AlgebraicElement F E α
    expBase : AR.FieldDeclaration
    linkBase : AFB.AlgebraicElement.baseField decl ≡ expBase
    status : B.Bool

mkAlgebraicElementAdapter :
  (F E : AR.FieldDeclaration) →
  (α : M.Identifier) →
  (d : AFB.AlgebraicElement F E α) →
  (eb : AR.FieldDeclaration) →
  (pb : AFB.AlgebraicElement.baseField d ≡ eb) →
  AlgebraicElementAdapter
mkAlgebraicElementAdapter F E α d eb pb =
  record { F = F ; E = E ; α = α ; decl = d ; expBase = eb ; linkBase = pb ; status = B.true }

isFilledAlgebraicElement : AlgebraicElementAdapter → B.Bool
isFilledAlgebraicElement a = AlgebraicElementAdapter.status a

-- Algebraic extension
record AlgebraicExtensionAdapter : Set₁ where
  field
    F E : AR.FieldDeclaration
    decl : AFB.AlgebraicExtension F E
    expBase : AR.FieldDeclaration
    linkBase : AFB.AlgebraicExtension.baseField decl ≡ expBase
    status : B.Bool

mkAlgebraicExtensionAdapter :
  (F E : AR.FieldDeclaration) →
  (d : AFB.AlgebraicExtension F E) →
  (eb : AR.FieldDeclaration) →
  (pb : AFB.AlgebraicExtension.baseField d ≡ eb) →
  AlgebraicExtensionAdapter
mkAlgebraicExtensionAdapter F E d eb pb =
  record { F = F ; E = E ; decl = d ; expBase = eb ; linkBase = pb ; status = B.true }

isFilledAlgebraicExtension : AlgebraicExtensionAdapter → B.Bool
isFilledAlgebraicExtension a = AlgebraicExtensionAdapter.status a

-- Field automorphism
record FieldAutomorphismAdapter : Set₁ where
  field
    F E : AR.FieldDeclaration
    decl : AFB.FieldAutomorphism F E
    expBase : AR.FieldDeclaration
    linkBase : AFB.FieldAutomorphism.baseField decl ≡ expBase
    status : B.Bool

mkFieldAutomorphismAdapter :
  (F E : AR.FieldDeclaration) →
  (d : AFB.FieldAutomorphism F E) →
  (eb : AR.FieldDeclaration) →
  (pb : AFB.FieldAutomorphism.baseField d ≡ eb) →
  FieldAutomorphismAdapter
mkFieldAutomorphismAdapter F E d eb pb =
  record { F = F ; E = E ; decl = d ; expBase = eb ; linkBase = pb ; status = B.true }

isFilledFieldAutomorphism : FieldAutomorphismAdapter → B.Bool
isFilledFieldAutomorphism a = FieldAutomorphismAdapter.status a

-- Galois group
record GaloisGroupAdapter : Set₁ where
  field
    F E : AR.FieldDeclaration
    decl : AFB.GaloisGroup F E
    expBase : AR.FieldDeclaration
    linkBase : AFB.GaloisGroup.baseField decl ≡ expBase
    status : B.Bool

mkGaloisGroupAdapter :
  (F E : AR.FieldDeclaration) →
  (d : AFB.GaloisGroup F E) →
  (eb : AR.FieldDeclaration) →
  (pb : AFB.GaloisGroup.baseField d ≡ eb) →
  GaloisGroupAdapter
mkGaloisGroupAdapter F E d eb pb =
  record { F = F ; E = E ; decl = d ; expBase = eb ; linkBase = pb ; status = B.true }

isFilledGaloisGroup : GaloisGroupAdapter → B.Bool
isFilledGaloisGroup a = GaloisGroupAdapter.status a

-- Galois extension
record GaloisExtensionAdapter : Set₁ where
  field
    F E : AR.FieldDeclaration
    decl : AFB.GaloisExtension F E
    expBase : AR.FieldDeclaration
    linkBase : AFB.GaloisExtension.baseField decl ≡ expBase
    status : B.Bool

mkGaloisExtensionAdapter :
  (F E : AR.FieldDeclaration) →
  (d : AFB.GaloisExtension F E) →
  (eb : AR.FieldDeclaration) →
  (pb : AFB.GaloisExtension.baseField d ≡ eb) →
  GaloisExtensionAdapter
mkGaloisExtensionAdapter F E d eb pb =
  record { F = F ; E = E ; decl = d ; expBase = eb ; linkBase = pb ; status = B.true }

isFilledGaloisExtension : GaloisExtensionAdapter → B.Bool
isFilledGaloisExtension a = GaloisExtensionAdapter.status a

-- Normal extension
record NormalExtensionAdapter : Set₁ where
  field
    F E : AR.FieldDeclaration
    decl : AFB.NormalExtension F E
    expBase : AR.FieldDeclaration
    linkBase : AFB.NormalExtension.baseField decl ≡ expBase
    status : B.Bool

mkNormalExtensionAdapter :
  (F E : AR.FieldDeclaration) →
  (d : AFB.NormalExtension F E) →
  (eb : AR.FieldDeclaration) →
  (pb : AFB.NormalExtension.baseField d ≡ eb) →
  NormalExtensionAdapter
mkNormalExtensionAdapter F E d eb pb =
  record { F = F ; E = E ; decl = d ; expBase = eb ; linkBase = pb ; status = B.true }

isFilledNormalExtension : NormalExtensionAdapter → B.Bool
isFilledNormalExtension a = NormalExtensionAdapter.status a

-- Separable extension
record SeparableExtensionAdapter : Set₁ where
  field
    F E : AR.FieldDeclaration
    decl : AFB.SeparableExtension F E
    expBase : AR.FieldDeclaration
    linkBase : AFB.SeparableExtension.baseField decl ≡ expBase
    status : B.Bool

mkSeparableExtensionAdapter :
  (F E : AR.FieldDeclaration) →
  (d : AFB.SeparableExtension F E) →
  (eb : AR.FieldDeclaration) →
  (pb : AFB.SeparableExtension.baseField d ≡ eb) →
  SeparableExtensionAdapter
mkSeparableExtensionAdapter F E d eb pb =
  record { F = F ; E = E ; decl = d ; expBase = eb ; linkBase = pb ; status = B.true }

isFilledSeparableExtension : SeparableExtensionAdapter → B.Bool
isFilledSeparableExtension a = SeparableExtensionAdapter.status a

-- Splitting field
record SplittingFieldAdapter : Set₁ where
  field
    F : AR.FieldDeclaration
    f : M.Identifier
    decl : AFB.SplittingField F f
    expBase : AR.FieldDeclaration
    linkBase : AFB.SplittingField.baseField decl ≡ expBase
    status : B.Bool

mkSplittingFieldAdapter :
  (F : AR.FieldDeclaration) →
  (f : M.Identifier) →
  (d : AFB.SplittingField F f) →
  (eb : AR.FieldDeclaration) →
  (pb : AFB.SplittingField.baseField d ≡ eb) →
  SplittingFieldAdapter
mkSplittingFieldAdapter F f d eb pb =
  record { F = F ; f = f ; decl = d ; expBase = eb ; linkBase = pb ; status = B.true }

isFilledSplittingField : SplittingFieldAdapter → B.Bool
isFilledSplittingField a = SplittingFieldAdapter.status a

-- Algebraic closure
record AlgebraicClosureAdapter : Set₁ where
  field
    F : AR.FieldDeclaration
    decl : AFB.AlgebraicClosure F
    expBase : AR.FieldDeclaration
    linkBase : AFB.AlgebraicClosure.baseField decl ≡ expBase
    status : B.Bool

mkAlgebraicClosureAdapter :
  (F : AR.FieldDeclaration) →
  (d : AFB.AlgebraicClosure F) →
  (eb : AR.FieldDeclaration) →
  (pb : AFB.AlgebraicClosure.baseField d ≡ eb) →
  AlgebraicClosureAdapter
mkAlgebraicClosureAdapter F d eb pb =
  record { F = F ; decl = d ; expBase = eb ; linkBase = pb ; status = B.true }

isFilledAlgebraicClosure : AlgebraicClosureAdapter → B.Bool
isFilledAlgebraicClosure a = AlgebraicClosureAdapter.status a

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
    status : B.Bool

mkIdealAdapter :
  (R : AR.RingDeclaration) →
  (d : AR.Ideal R) →
  (er : AR.RingDeclaration) →
  (pr : AR.Ideal.ring d ≡ er) →
  IdealAdapter
mkIdealAdapter R d er pr =
  record { R = R ; decl = d ; expRing = er ; linkRing = pr ; status = B.true }

isFilledIdeal : IdealAdapter → B.Bool
isFilledIdeal a = IdealAdapter.status a

-- Prime ideal
record PrimeIdealAdapter : Set₁ where
  field
    R : AR.CommutativeRingDeclaration
    decl : AR.PrimeIdeal R
    expRing : AR.CommutativeRingDeclaration
    linkRing : AR.PrimeIdeal.ring decl ≡ expRing
    status : B.Bool

mkPrimeIdealAdapter :
  (R : AR.CommutativeRingDeclaration) →
  (d : AR.PrimeIdeal R) →
  (er : AR.CommutativeRingDeclaration) →
  (pr : AR.PrimeIdeal.ring d ≡ er) →
  PrimeIdealAdapter
mkPrimeIdealAdapter R d er pr =
  record { R = R ; decl = d ; expRing = er ; linkRing = pr ; status = B.true }

isFilledPrimeIdeal : PrimeIdealAdapter → B.Bool
isFilledPrimeIdeal a = PrimeIdealAdapter.status a

-- Maximal ideal
record MaximalIdealAdapter : Set₁ where
  field
    R : AR.CommutativeRingDeclaration
    decl : AR.MaximalIdeal R
    expRing : AR.CommutativeRingDeclaration
    linkRing : AR.MaximalIdeal.ring decl ≡ expRing
    status : B.Bool

mkMaximalIdealAdapter :
  (R : AR.CommutativeRingDeclaration) →
  (d : AR.MaximalIdeal R) →
  (er : AR.CommutativeRingDeclaration) →
  (pr : AR.MaximalIdeal.ring d ≡ er) →
  MaximalIdealAdapter
mkMaximalIdealAdapter R d er pr =
  record { R = R ; decl = d ; expRing = er ; linkRing = pr ; status = B.true }

isFilledMaximalIdeal : MaximalIdealAdapter → B.Bool
isFilledMaximalIdeal a = MaximalIdealAdapter.status a

-- Integral domain
record IntegralDomainAdapter : Set₁ where
  field
    decl : AR.IntegralDomain
    expRing : AR.CommutativeRingDeclaration
    linkRing : AR.IntegralDomain.underlyingRing decl ≡ expRing
    status : B.Bool

mkIntegralDomainAdapter :
  (d : AR.IntegralDomain) →
  (er : AR.CommutativeRingDeclaration) →
  (pr : AR.IntegralDomain.underlyingRing d ≡ er) →
  IntegralDomainAdapter
mkIntegralDomainAdapter d er pr =
  record { decl = d ; expRing = er ; linkRing = pr ; status = B.true }

isFilledIntegralDomain : IntegralDomainAdapter → B.Bool
isFilledIntegralDomain a = IntegralDomainAdapter.status a

-- Irreducible element
record IrreducibleElementAdapter : Set₁ where
  field
    R : AR.IntegralDomain
    p : M.Identifier
    decl : AR.IrreducibleElement R p
    expDomain : AR.IntegralDomain
    linkDomain : AR.IrreducibleElement.domain decl ≡ expDomain
    status : B.Bool

mkIrreducibleElementAdapter :
  (R : AR.IntegralDomain) →
  (p : M.Identifier) →
  (d : AR.IrreducibleElement R p) →
  (ed : AR.IntegralDomain) →
  (pd : AR.IrreducibleElement.domain d ≡ ed) →
  IrreducibleElementAdapter
mkIrreducibleElementAdapter R p d ed pd =
  record { R = R ; p = p ; decl = d ; expDomain = ed ; linkDomain = pd ; status = B.true }

isFilledIrreducibleElement : IrreducibleElementAdapter → B.Bool
isFilledIrreducibleElement a = IrreducibleElementAdapter.status a

-- Prime element
record PrimeElementAdapter : Set₁ where
  field
    R : AR.IntegralDomain
    p : M.Identifier
    decl : AR.PrimeElement R p
    expDomain : AR.IntegralDomain
    linkDomain : AR.PrimeElement.domain decl ≡ expDomain
    status : B.Bool

mkPrimeElementAdapter :
  (R : AR.IntegralDomain) →
  (p : M.Identifier) →
  (d : AR.PrimeElement R p) →
  (ed : AR.IntegralDomain) →
  (pd : AR.PrimeElement.domain d ≡ ed) →
  PrimeElementAdapter
mkPrimeElementAdapter R p d ed pd =
  record { R = R ; p = p ; decl = d ; expDomain = ed ; linkDomain = pd ; status = B.true }

isFilledPrimeElement : PrimeElementAdapter → B.Bool
isFilledPrimeElement a = PrimeElementAdapter.status a

-- Unique factorization domain (UFD)
record UFDAdapter : Set₁ where
  field
    decl : AR.UFD
    expDomain : AR.IntegralDomain
    linkDomain : AR.UFD.domain decl ≡ expDomain
    status : B.Bool

mkUFDAdapter :
  (d : AR.UFD) →
  (ed : AR.IntegralDomain) →
  (pd : AR.UFD.domain d ≡ ed) →
  UFDAdapter
mkUFDAdapter d ed pd =
  record { decl = d ; expDomain = ed ; linkDomain = pd ; status = B.true }

isFilledUFD : UFDAdapter → B.Bool
isFilledUFD a = UFDAdapter.status a

-- Principal ideal domain (PID)
record PrincipalIdealDomainAdapter : Set₁ where
  field
    decl : AR.PrincipalIdealDomain
    expDomain : AR.IntegralDomain
    linkDomain : AR.PrincipalIdealDomain.domain decl ≡ expDomain
    status : B.Bool

mkPrincipalIdealDomainAdapter :
  (d : AR.PrincipalIdealDomain) →
  (ed : AR.IntegralDomain) →
  (pd : AR.PrincipalIdealDomain.domain d ≡ ed) →
  PrincipalIdealDomainAdapter
mkPrincipalIdealDomainAdapter d ed pd =
  record { decl = d ; expDomain = ed ; linkDomain = pd ; status = B.true }

isFilledPrincipalIdealDomain : PrincipalIdealDomainAdapter → B.Bool
isFilledPrincipalIdealDomain a = PrincipalIdealDomainAdapter.status a

-- Euclidean domain
record EuclideanDomainAdapter : Set₁ where
  field
    decl : AR.EuclideanDomain
    expDomain : AR.IntegralDomain
    linkDomain : AR.EuclideanDomain.domain decl ≡ expDomain
    status : B.Bool

mkEuclideanDomainAdapter :
  (d : AR.EuclideanDomain) →
  (ed : AR.IntegralDomain) →
  (pd : AR.EuclideanDomain.domain d ≡ ed) →
  EuclideanDomainAdapter
mkEuclideanDomainAdapter d ed pd =
  record { decl = d ; expDomain = ed ; linkDomain = pd ; status = B.true }

isFilledEuclideanDomain : EuclideanDomainAdapter → B.Bool
isFilledEuclideanDomain a = EuclideanDomainAdapter.status a

-- Multiplicative system
record MultiplicativeSystemAdapter : Set₁ where
  field
    R : AR.CommutativeRingDeclaration
    decl : AR.MultiplicativeSystem R
    expRing : AR.CommutativeRingDeclaration
    linkRing : AR.MultiplicativeSystem.ring decl ≡ expRing
    status : B.Bool

mkMultiplicativeSystemAdapter :
  (R : AR.CommutativeRingDeclaration) →
  (d : AR.MultiplicativeSystem R) →
  (er : AR.CommutativeRingDeclaration) →
  (pr : AR.MultiplicativeSystem.ring d ≡ er) →
  MultiplicativeSystemAdapter
mkMultiplicativeSystemAdapter R d er pr =
  record { R = R ; decl = d ; expRing = er ; linkRing = pr ; status = B.true }

isFilledMultiplicativeSystem : MultiplicativeSystemAdapter → B.Bool
isFilledMultiplicativeSystem a = MultiplicativeSystemAdapter.status a

-- Localization
record LocalizationAdapter : Set₁ where
  field
    R : AR.CommutativeRingDeclaration
    S : AR.MultiplicativeSystem R
    decl : AR.Localization R S
    expRing : AR.CommutativeRingDeclaration
    linkRing : AR.Localization.ring decl ≡ expRing
    status : B.Bool

mkLocalizationAdapter :
  (R : AR.CommutativeRingDeclaration) →
  (S : AR.MultiplicativeSystem R) →
  (d : AR.Localization R S) →
  (er : AR.CommutativeRingDeclaration) →
  (pr : AR.Localization.ring d ≡ er) →
  LocalizationAdapter
mkLocalizationAdapter R S d er pr =
  record { R = R ; S = S ; decl = d ; expRing = er ; linkRing = pr ; status = B.true }

isFilledLocalization : LocalizationAdapter → B.Bool
isFilledLocalization a = LocalizationAdapter.status a

-- Field of fractions
record FieldOfFractionsAdapter : Set₁ where
  field
    R : AR.IntegralDomain
    decl : AR.FieldOfFractions R
    expDomain : AR.IntegralDomain
    linkDomain : AR.FieldOfFractions.domain decl ≡ expDomain
    status : B.Bool

mkFieldOfFractionsAdapter :
  (R : AR.IntegralDomain) →
  (d : AR.FieldOfFractions R) →
  (ed : AR.IntegralDomain) →
  (pd : AR.FieldOfFractions.domain d ≡ ed) →
  FieldOfFractionsAdapter
mkFieldOfFractionsAdapter R d ed pd =
  record { R = R ; decl = d ; expDomain = ed ; linkDomain = pd ; status = B.true }

isFilledFieldOfFractions : FieldOfFractionsAdapter → B.Bool
isFilledFieldOfFractions a = FieldOfFractionsAdapter.status a

-- Polynomial ring
record PolynomialRingAdapter : Set₁ where
  field
    R : AR.CommutativeRingDeclaration
    decl : AR.PolynomialRing R
    expCoeffRing : AR.CommutativeRingDeclaration
    linkCoeffRing : AR.PolynomialRing.coefficientRing decl ≡ expCoeffRing
    status : B.Bool

mkPolynomialRingAdapter :
  (R : AR.CommutativeRingDeclaration) →
  (d : AR.PolynomialRing R) →
  (ec : AR.CommutativeRingDeclaration) →
  (pc : AR.PolynomialRing.coefficientRing d ≡ ec) →
  PolynomialRingAdapter
mkPolynomialRingAdapter R d ec pc =
  record { R = R ; decl = d ; expCoeffRing = ec ; linkCoeffRing = pc ; status = B.true }

isFilledPolynomialRing : PolynomialRingAdapter → B.Bool
isFilledPolynomialRing a = PolynomialRingAdapter.status a

-- Quotient ring
record QuotientRingAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    I : AR.Ideal R
    decl : AR.QuotientRing R I
    expRing : AR.RingDeclaration
    linkRing : AR.QuotientRing.ring decl ≡ expRing
    status : B.Bool

mkQuotientRingAdapter :
  (R : AR.RingDeclaration) →
  (I : AR.Ideal R) →
  (d : AR.QuotientRing R I) →
  (er : AR.RingDeclaration) →
  (pr : AR.QuotientRing.ring d ≡ er) →
  QuotientRingAdapter
mkQuotientRingAdapter R I d er pr =
  record { R = R ; I = I ; decl = d ; expRing = er ; linkRing = pr ; status = B.true }

isFilledQuotientRing : QuotientRingAdapter → B.Bool
isFilledQuotientRing a = QuotientRingAdapter.status a

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
    status : B.Bool

mkProductInGrpAdapter :
  (G H : AFo.GroupDeclaration) →
  (d : AGF.ProductInGrp G H) →
  (eg : AFo.GroupDeclaration) →
  (pg : AGF.ProductInGrp.group1 d ≡ eg) →
  ProductInGrpAdapter
mkProductInGrpAdapter G H d eg pg =
  record { G = G ; H = H ; decl = d ; expGroup1 = eg ; linkGroup1 = pg ; status = B.true }

isFilledProductInGrp : ProductInGrpAdapter → B.Bool
isFilledProductInGrp a = ProductInGrpAdapter.status a

-- Coproduct in Grp
record CoproductInGrpAdapter : Set₁ where
  field
    G H : AFo.GroupDeclaration
    decl : AGF.CoproductInGrp G H
    expGroup1 : AFo.GroupDeclaration
    linkGroup1 : AGF.CoproductInGrp.group1 decl ≡ expGroup1
    status : B.Bool

mkCoproductInGrpAdapter :
  (G H : AFo.GroupDeclaration) →
  (d : AGF.CoproductInGrp G H) →
  (eg : AFo.GroupDeclaration) →
  (pg : AGF.CoproductInGrp.group1 d ≡ eg) →
  CoproductInGrpAdapter
mkCoproductInGrpAdapter G H d eg pg =
  record { G = G ; H = H ; decl = d ; expGroup1 = eg ; linkGroup1 = pg ; status = B.true }

isFilledCoproductInGrp : CoproductInGrpAdapter → B.Bool
isFilledCoproductInGrp a = CoproductInGrpAdapter.status a

-- Free group object
record FreeGroupObjectAdapter : Set₁ where
  field
    X : M.Identifier
    decl : AGF.FreeGroupObject X
    expGenSet : M.Identifier
    linkGenSet : AGF.FreeGroupObject.generatingSet decl ≡ expGenSet
    status : B.Bool

mkFreeGroupObjectAdapter :
  (X : M.Identifier) →
  (d : AGF.FreeGroupObject X) →
  (eg : M.Identifier) →
  (pg : AGF.FreeGroupObject.generatingSet d ≡ eg) →
  FreeGroupObjectAdapter
mkFreeGroupObjectAdapter X d eg pg =
  record { X = X ; decl = d ; expGenSet = eg ; linkGenSet = pg ; status = B.true }

isFilledFreeGroupObject : FreeGroupObjectAdapter → B.Bool
isFilledFreeGroupObject a = FreeGroupObjectAdapter.status a

-- Free group
record FreeGroupAdapter : Set₁ where
  field
    X : M.Identifier
    decl : AGF.FreeGroup X
    expGenSet : M.Identifier
    linkGenSet : AGF.FreeGroup.generatingSet decl ≡ expGenSet
    status : B.Bool

mkFreeGroupAdapter :
  (X : M.Identifier) →
  (d : AGF.FreeGroup X) →
  (eg : M.Identifier) →
  (pg : AGF.FreeGroup.generatingSet d ≡ eg) →
  FreeGroupAdapter
mkFreeGroupAdapter X d eg pg =
  record { X = X ; decl = d ; expGenSet = eg ; linkGenSet = pg ; status = B.true }

isFilledFreeGroup : FreeGroupAdapter → B.Bool
isFilledFreeGroup a = FreeGroupAdapter.status a

-- Group presentation
record GroupPresentationAdapter : Set₁ where
  field
    decl : AGF.GroupPresentation
    expGenerators : M.Identifier
    linkGenerators : AGF.GroupPresentation.generators decl ≡ expGenerators
    status : B.Bool

mkGroupPresentationAdapter :
  (d : AGF.GroupPresentation) →
  (eg : M.Identifier) →
  (pg : AGF.GroupPresentation.generators d ≡ eg) →
  GroupPresentationAdapter
mkGroupPresentationAdapter d eg pg =
  record { decl = d ; expGenerators = eg ; linkGenerators = pg ; status = B.true }

isFilledGroupPresentation : GroupPresentationAdapter → B.Bool
isFilledGroupPresentation a = GroupPresentationAdapter.status a

-- Abelianization
record AbelianizationAdapter : Set₁ where
  field
    G : AFo.GroupDeclaration
    decl : AGF.Abelianization G
    expGroup : AFo.GroupDeclaration
    linkGroup : AGF.Abelianization.group decl ≡ expGroup
    status : B.Bool

mkAbelianizationAdapter :
  (G : AFo.GroupDeclaration) →
  (d : AGF.Abelianization G) →
  (eg : AFo.GroupDeclaration) →
  (pg : AGF.Abelianization.group d ≡ eg) →
  AbelianizationAdapter
mkAbelianizationAdapter G d eg pg =
  record { G = G ; decl = d ; expGroup = eg ; linkGroup = pg ; status = B.true }

isFilledAbelianization : AbelianizationAdapter → B.Bool
isFilledAbelianization a = AbelianizationAdapter.status a

-- Finitely generated abelian group
record FinitelyGeneratedAbelianGroupAdapter : Set₁ where
  field
    decl : AGF.FinitelyGeneratedAbelianGroup
    expGroup : AFo.AbelianGroupDeclaration
    linkGroup : AGF.FinitelyGeneratedAbelianGroup.underlyingGroup decl ≡ expGroup
    status : B.Bool

mkFinitelyGeneratedAbelianGroupAdapter :
  (d : AGF.FinitelyGeneratedAbelianGroup) →
  (eg : AFo.AbelianGroupDeclaration) →
  (pg : AGF.FinitelyGeneratedAbelianGroup.underlyingGroup d ≡ eg) →
  FinitelyGeneratedAbelianGroupAdapter
mkFinitelyGeneratedAbelianGroupAdapter d eg pg =
  record { decl = d ; expGroup = eg ; linkGroup = pg ; status = B.true }

isFilledFinitelyGeneratedAbelianGroup : FinitelyGeneratedAbelianGroupAdapter → B.Bool
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
    status : B.Bool

mkInvariantFactorDecompositionAdapter :
  (A : AGF.FinitelyGeneratedAbelianGroup) →
  (d : AGS.InvariantFactorDecomposition A) →
  (ef : M.Identifier) →
  (pf : AGS.InvariantFactorDecomposition.freeRank d ≡ ef) →
  InvariantFactorDecompositionAdapter
mkInvariantFactorDecompositionAdapter A d ef pf =
  record { A = A ; decl = d ; expFreeRank = ef ; linkFreeRank = pf ; status = B.true }

isFilledInvariantFactorDecomposition : InvariantFactorDecompositionAdapter → B.Bool
isFilledInvariantFactorDecomposition a = InvariantFactorDecompositionAdapter.status a

-- Torsion subgroup
record TorsionSubgroupAdapter : Set₁ where
  field
    A : AFo.AbelianGroupDeclaration
    decl : AGS.TorsionSubgroup A
    expAbelianGroup : AFo.AbelianGroupDeclaration
    linkAbelianGroup : AGS.TorsionSubgroup.abelianGroup decl ≡ expAbelianGroup
    status : B.Bool

mkTorsionSubgroupAdapter :
  (A : AFo.AbelianGroupDeclaration) →
  (d : AGS.TorsionSubgroup A) →
  (ea : AFo.AbelianGroupDeclaration) →
  (pa : AGS.TorsionSubgroup.abelianGroup d ≡ ea) →
  TorsionSubgroupAdapter
mkTorsionSubgroupAdapter A d ea pa =
  record { A = A ; decl = d ; expAbelianGroup = ea ; linkAbelianGroup = pa ; status = B.true }

isFilledTorsionSubgroup : TorsionSubgroupAdapter → B.Bool
isFilledTorsionSubgroup a = TorsionSubgroupAdapter.status a

-- Group action
record GroupActionAdapter : Set₁ where
  field
    G : AFo.GroupDeclaration
    X : M.Identifier
    decl : AGS.GroupAction G X
    expGroup : AFo.GroupDeclaration
    linkGroup : AGS.GroupAction.group decl ≡ expGroup
    status : B.Bool

mkGroupActionAdapter :
  (G : AFo.GroupDeclaration) →
  (X : M.Identifier) →
  (d : AGS.GroupAction G X) →
  (eg : AFo.GroupDeclaration) →
  (pg : AGS.GroupAction.group d ≡ eg) →
  GroupActionAdapter
mkGroupActionAdapter G X d eg pg =
  record { G = G ; X = X ; decl = d ; expGroup = eg ; linkGroup = pg ; status = B.true }

isFilledGroupAction : GroupActionAdapter → B.Bool
isFilledGroupAction a = GroupActionAdapter.status a

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
    status : B.Bool

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
  record { G = G ; X = X ; act = act ; x = x ; decl = d ; expGroupAction = ea ; linkGroupAction = pa ; status = B.true }

isFilledOrbit : OrbitAdapter → B.Bool
isFilledOrbit a = OrbitAdapter.status a

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
    status : B.Bool

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
  record { G = G ; X = X ; act = act ; x = x ; decl = d ; expGroupAction = ea ; linkGroupAction = pa ; status = B.true }

isFilledStabilizer : StabilizerAdapter → B.Bool
isFilledStabilizer a = StabilizerAdapter.status a

-- P-group
record PGroupAdapter : Set₁ where
  field
    p : M.Identifier
    G : AFo.GroupDeclaration
    decl : AGS.PGroup p G
    expPrime : M.Identifier
    linkPrime : AGS.PGroup.prime decl ≡ expPrime
    status : B.Bool

mkPGroupAdapter :
  (p : M.Identifier) →
  (G : AFo.GroupDeclaration) →
  (d : AGS.PGroup p G) →
  (ep : M.Identifier) →
  (pp : AGS.PGroup.prime d ≡ ep) →
  PGroupAdapter
mkPGroupAdapter p G d ep pp =
  record { p = p ; G = G ; decl = d ; expPrime = ep ; linkPrime = pp ; status = B.true }

isFilledPGroup : PGroupAdapter → B.Bool
isFilledPGroup a = PGroupAdapter.status a

-- Sylow p-subgroup
record SylowPSubgroupAdapter : Set₁ where
  field
    p : M.Identifier
    G : AFo.GroupDeclaration
    decl : AGS.SylowPSubgroup p G
    expPrime : M.Identifier
    linkPrime : AGS.SylowPSubgroup.prime decl ≡ expPrime
    status : B.Bool

mkSylowPSubgroupAdapter :
  (p : M.Identifier) →
  (G : AFo.GroupDeclaration) →
  (d : AGS.SylowPSubgroup p G) →
  (ep : M.Identifier) →
  (pp : AGS.SylowPSubgroup.prime d ≡ ep) →
  SylowPSubgroupAdapter
mkSylowPSubgroupAdapter p G d ep pp =
  record { p = p ; G = G ; decl = d ; expPrime = ep ; linkPrime = pp ; status = B.true }

isFilledSylowPSubgroup : SylowPSubgroupAdapter → B.Bool
isFilledSylowPSubgroup a = SylowPSubgroupAdapter.status a

-- Simple group
record SimpleGroupAdapter : Set₁ where
  field
    G : AFo.GroupDeclaration
    decl : AGS.SimpleGroup G
    expGroup : AFo.GroupDeclaration
    linkGroup : AGS.SimpleGroup.group decl ≡ expGroup
    status : B.Bool

mkSimpleGroupAdapter :
  (G : AFo.GroupDeclaration) →
  (d : AGS.SimpleGroup G) →
  (eg : AFo.GroupDeclaration) →
  (pg : AGS.SimpleGroup.group d ≡ eg) →
  SimpleGroupAdapter
mkSimpleGroupAdapter G d eg pg =
  record { G = G ; decl = d ; expGroup = eg ; linkGroup = pg ; status = B.true }

isFilledSimpleGroup : SimpleGroupAdapter → B.Bool
isFilledSimpleGroup a = SimpleGroupAdapter.status a

-- Composition series
record CompositionSeriesAdapter : Set₁ where
  field
    G : AFo.GroupDeclaration
    decl : AGS.CompositionSeries G
    expGroup : AFo.GroupDeclaration
    linkGroup : AGS.CompositionSeries.group decl ≡ expGroup
    status : B.Bool

mkCompositionSeriesAdapter :
  (G : AFo.GroupDeclaration) →
  (d : AGS.CompositionSeries G) →
  (eg : AFo.GroupDeclaration) →
  (pg : AGS.CompositionSeries.group d ≡ eg) →
  CompositionSeriesAdapter
mkCompositionSeriesAdapter G d eg pg =
  record { G = G ; decl = d ; expGroup = eg ; linkGroup = pg ; status = B.true }

isFilledCompositionSeries : CompositionSeriesAdapter → B.Bool
isFilledCompositionSeries a = CompositionSeriesAdapter.status a

-- Solvable group
record SolvableGroupAdapter : Set₁ where
  field
    G : AFo.GroupDeclaration
    decl : AGS.SolvableGroup G
    expGroup : AFo.GroupDeclaration
    linkGroup : AGS.SolvableGroup.group decl ≡ expGroup
    status : B.Bool

mkSolvableGroupAdapter :
  (G : AFo.GroupDeclaration) →
  (d : AGS.SolvableGroup G) →
  (eg : AFo.GroupDeclaration) →
  (pg : AGS.SolvableGroup.group d ≡ eg) →
  SolvableGroupAdapter
mkSolvableGroupAdapter G d eg pg =
  record { G = G ; decl = d ; expGroup = eg ; linkGroup = pg ; status = B.true }

isFilledSolvableGroup : SolvableGroupAdapter → B.Bool
isFilledSolvableGroup a = SolvableGroupAdapter.status a

-- Nilpotent group
record NilpotentGroupAdapter : Set₁ where
  field
    G : AFo.GroupDeclaration
    decl : AGS.NilpotentGroup G
    expGroup : AFo.GroupDeclaration
    linkGroup : AGS.NilpotentGroup.group decl ≡ expGroup
    status : B.Bool

mkNilpotentGroupAdapter :
  (G : AFo.GroupDeclaration) →
  (d : AGS.NilpotentGroup G) →
  (eg : AFo.GroupDeclaration) →
  (pg : AGS.NilpotentGroup.group d ≡ eg) →
  NilpotentGroupAdapter
mkNilpotentGroupAdapter G d eg pg =
  record { G = G ; decl = d ; expGroup = eg ; linkGroup = pg ; status = B.true }

isFilledNilpotentGroup : NilpotentGroupAdapter → B.Bool
isFilledNilpotentGroup a = NilpotentGroupAdapter.status a

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
    status : B.Bool

mkFreeAbelianGroupAdapter :
  (X : M.Identifier) →
  (d : AGA.FreeAbelianGroup X) →
  (eu : M.Identifier) →
  (pu : AGA.FreeAbelianGroup.underlyingSet d ≡ eu) →
  FreeAbelianGroupAdapter
mkFreeAbelianGroupAdapter X d eu pu =
  record { X = X ; decl = d ; expUnderlyingSet = eu ; linkUnderlyingSet = pu ; status = B.true }

isFilledFreeAbelianGroup : FreeAbelianGroupAdapter → B.Bool
isFilledFreeAbelianGroup a = FreeAbelianGroupAdapter.status a

-- Free-Forgetful adjunction for Ab
record FreeForgetfulAdjunctionAbAdapter : Set₁ where
  field
    decl : AGA.FreeForgetfulAdjunctionAb
    expFreeFunctor : M.Identifier
    linkFreeFunctor : AGA.FreeForgetfulAdjunctionAb.freeFunctor decl ≡ expFreeFunctor
    status : B.Bool

mkFreeForgetfulAdjunctionAbAdapter :
  (d : AGA.FreeForgetfulAdjunctionAb) →
  (ef : M.Identifier) →
  (pf : AGA.FreeForgetfulAdjunctionAb.freeFunctor d ≡ ef) →
  FreeForgetfulAdjunctionAbAdapter
mkFreeForgetfulAdjunctionAbAdapter d ef pf =
  record { decl = d ; expFreeFunctor = ef ; linkFreeFunctor = pf ; status = B.true }

isFilledFreeForgetfulAdjunctionAb : FreeForgetfulAdjunctionAbAdapter → B.Bool
isFilledFreeForgetfulAdjunctionAb a = FreeForgetfulAdjunctionAbAdapter.status a

-- Grothendieck group
record GrothendieckGroupAdapter : Set₁ where
  field
    M : AFo.MonoidDeclaration
    decl : AGA.GrothendieckGroup M
    expUnderlyingSet : M.Identifier
    linkUnderlyingSet : AGA.GrothendieckGroup.underlyingSet decl ≡ expUnderlyingSet
    status : B.Bool

mkGrothendieckGroupAdapter :
  (M : AFo.MonoidDeclaration) →
  (d : AGA.GrothendieckGroup M) →
  (eu : M.Identifier) →
  (pu : AGA.GrothendieckGroup.underlyingSet d ≡ eu) →
  GrothendieckGroupAdapter
mkGrothendieckGroupAdapter M d eu pu =
  record { M = M ; decl = d ; expUnderlyingSet = eu ; linkUnderlyingSet = pu ; status = B.true }

isFilledGrothendieckGroup : GrothendieckGroupAdapter → B.Bool
isFilledGrothendieckGroup a = GrothendieckGroupAdapter.status a

-- Tensor product of abelian groups
record TensorProductAbAdapter : Set₁ where
  field
    A : AFo.AbelianGroupDeclaration
    B : AFo.AbelianGroupDeclaration
    decl : AGA.TensorProductAb A B
    expTensorProduct : AFo.AbelianGroupDeclaration
    linkTensorProduct : AGA.TensorProductAb.tensorProduct decl ≡ expTensorProduct
    status : B.Bool

mkTensorProductAbAdapter :
  (A B : AFo.AbelianGroupDeclaration) →
  (d : AGA.TensorProductAb A B) →
  (et : AFo.AbelianGroupDeclaration) →
  (pt : AGA.TensorProductAb.tensorProduct d ≡ et) →
  TensorProductAbAdapter
mkTensorProductAbAdapter A B d et pt =
  record { A = A ; B = B ; decl = d ; expTensorProduct = et ; linkTensorProduct = pt ; status = B.true }

isFilledTensorProductAb : TensorProductAbAdapter → B.Bool
isFilledTensorProductAb a = TensorProductAbAdapter.status a

-- Basis of vector space
record BasisOfVectorSpaceAdapter : Set₁ where
  field
    F : AR.FieldDeclaration
    V : AM.VectorSpace F
    decl : AM.BasisOfVectorSpace F V
    expBasisSet : M.Identifier
    linkBasisSet : AM.BasisOfVectorSpace.basisSet decl ≡ expBasisSet
    status : B.Bool

mkBasisOfVectorSpaceAdapter :
  (F : AR.FieldDeclaration) →
  (V : AM.VectorSpace F) →
  (d : AM.BasisOfVectorSpace F V) →
  (eb : M.Identifier) →
  (pb : AM.BasisOfVectorSpace.basisSet d ≡ eb) →
  BasisOfVectorSpaceAdapter
mkBasisOfVectorSpaceAdapter F V d eb pb =
  record { F = F ; V = V ; decl = d ; expBasisSet = eb ; linkBasisSet = pb ; status = B.true }

isFilledBasisOfVectorSpace : BasisOfVectorSpaceAdapter → B.Bool
isFilledBasisOfVectorSpace a = BasisOfVectorSpaceAdapter.status a

-- Dimension of vector space
record DimensionAdapter : Set₁ where
  field
    F : AR.FieldDeclaration
    V : AM.VectorSpace F
    decl : AM.Dimension F V
    expDimensionValue : M.Identifier
    linkDimensionValue : AM.Dimension.dimension decl ≡ expDimensionValue
    status : B.Bool

mkDimensionAdapter :
  (F : AR.FieldDeclaration) →
  (V : AM.VectorSpace F) →
  (d : AM.Dimension F V) →
  (edv : M.Identifier) →
  (pdv : AM.Dimension.dimension d ≡ edv) →
  DimensionAdapter
mkDimensionAdapter F V d edv pdv =
  record { F = F ; V = V ; decl = d ; expDimensionValue = edv ; linkDimensionValue = pdv ; status = B.true }

isFilledDimension : DimensionAdapter → B.Bool
isFilledDimension a = DimensionAdapter.status a

-- Multivariate polynomial ring
record MultivariatePolynomialRingAdapter : Set₁ where
  field
    R : AR.CommutativeRingDeclaration
    n : M.Identifier
    decl : AR.MultivariatePolynomialRing R n
    expRing : AR.CommutativeRingDeclaration
    linkRing : AR.MultivariatePolynomialRing.polynomialRing decl ≡ expRing
    status : B.Bool

mkMultivariatePolynomialRingAdapter :
  (R : AR.CommutativeRingDeclaration) →
  (n : M.Identifier) →
  (d : AR.MultivariatePolynomialRing R n) →
  (er : AR.CommutativeRingDeclaration) →
  (pr : AR.MultivariatePolynomialRing.polynomialRing d ≡ er) →
  MultivariatePolynomialRingAdapter
mkMultivariatePolynomialRingAdapter R n d er pr =
  record { R = R ; n = n ; decl = d ; expRing = er ; linkRing = pr ; status = B.true }

isFilledMultivariatePolynomialRing : MultivariatePolynomialRingAdapter → B.Bool
isFilledMultivariatePolynomialRing a = MultivariatePolynomialRingAdapter.status a

-- Content of polynomial
record ContentOfPolynomialAdapter : Set₁ where
  field
    R : AR.UFD
    f : M.Identifier
    decl : AR.ContentOfPolynomial R f
    expContent : M.Identifier
    linkContent : AR.ContentOfPolynomial.content decl ≡ expContent
    status : B.Bool

mkContentOfPolynomialAdapter :
  (R : AR.UFD) →
  (f : M.Identifier) →
  (d : AR.ContentOfPolynomial R f) →
  (ec : M.Identifier) →
  (pc : AR.ContentOfPolynomial.content d ≡ ec) →
  ContentOfPolynomialAdapter
mkContentOfPolynomialAdapter R f d ec pc =
  record { R = R ; f = f ; decl = d ; expContent = ec ; linkContent = pc ; status = B.true }

isFilledContentOfPolynomial : ContentOfPolynomialAdapter → B.Bool
isFilledContentOfPolynomial a = ContentOfPolynomialAdapter.status a

-- Primitive polynomial
record PrimitivePolynomialAdapter : Set₁ where
  field
    R : AR.UFD
    f : M.Identifier
    decl : AR.PrimitivePolynomial R f
    expUFD : AR.UFD
    linkUFD : AR.PrimitivePolynomial.ufd decl ≡ expUFD
    status : B.Bool

mkPrimitivePolynomialAdapter :
  (R : AR.UFD) →
  (f : M.Identifier) →
  (d : AR.PrimitivePolynomial R f) →
  (eu : AR.UFD) →
  (pu : AR.PrimitivePolynomial.ufd d ≡ eu) →
  PrimitivePolynomialAdapter
mkPrimitivePolynomialAdapter R f d eu pu =
  record { R = R ; f = f ; decl = d ; expUFD = eu ; linkUFD = pu ; status = B.true }

isFilledPrimitivePolynomial : PrimitivePolynomialAdapter → B.Bool
isFilledPrimitivePolynomial a = PrimitivePolynomialAdapter.status a

-- Prime spectrum
record PrimeSpectrumAdapter : Set₁ where
  field
    R : AR.CommutativeRingDeclaration
    decl : AR.PrimeSpectrum R
    expTopologicalSpace : M.Identifier
    linkTopologicalSpace : AR.PrimeSpectrum.topology decl ≡ expTopologicalSpace
    status : B.Bool

mkPrimeSpectrumAdapter :
  (R : AR.CommutativeRingDeclaration) →
  (d : AR.PrimeSpectrum R) →
  (ets : M.Identifier) →
  (pts : AR.PrimeSpectrum.topology d ≡ ets) →
  PrimeSpectrumAdapter
mkPrimeSpectrumAdapter R d ets pts =
  record { R = R ; decl = d ; expTopologicalSpace = ets ; linkTopologicalSpace = pts ; status = B.true }

isFilledPrimeSpectrum : PrimeSpectrumAdapter → B.Bool
isFilledPrimeSpectrum a = PrimeSpectrumAdapter.status a

-- Projective module
record ProjectiveModuleAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    P : AM.LeftModule R
    decl : AM.ProjectiveModule R P
    expRing : AR.RingDeclaration
    linkRing : AM.ProjectiveModule.ring decl ≡ expRing
    status : B.Bool

mkProjectiveModuleAdapter :
  (R : AR.RingDeclaration) →
  (P : AM.LeftModule R) →
  (d : AM.ProjectiveModule R P) →
  (er : AR.RingDeclaration) →
  (pr : AM.ProjectiveModule.ring d ≡ er) →
  ProjectiveModuleAdapter
mkProjectiveModuleAdapter R P d er pr =
  record { R = R ; P = P ; decl = d ; expRing = er ; linkRing = pr ; status = B.true }

isFilledProjectiveModule : ProjectiveModuleAdapter → B.Bool
isFilledProjectiveModule a = ProjectiveModuleAdapter.status a

-- Injective module
record InjectiveModuleAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    I : AM.LeftModule R
    decl : AM.InjectiveModule R I
    expRing : AR.RingDeclaration
    linkRing : AM.InjectiveModule.ring decl ≡ expRing
    status : B.Bool

mkInjectiveModuleAdapter :
  (R : AR.RingDeclaration) →
  (I : AM.LeftModule R) →
  (d : AM.InjectiveModule R I) →
  (er : AR.RingDeclaration) →
  (pr : AM.InjectiveModule.ring d ≡ er) →
  InjectiveModuleAdapter
mkInjectiveModuleAdapter R I d er pr =
  record { R = R ; I = I ; decl = d ; expRing = er ; linkRing = pr ; status = B.true }

isFilledInjectiveModule : InjectiveModuleAdapter → B.Bool
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
    status : B.Bool

mkTorsionElementAdapter :
  (R : AR.IntegralDomain) →
  (M : AM.LeftModule (AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing (AR.IntegralDomain.underlyingRing R)))) →
  (m : M.Identifier) →
  (d : AM.TorsionElement R M m) →
  (ed : AR.IntegralDomain) →
  (pd : AM.TorsionElement.domain d ≡ ed) →
  TorsionElementAdapter
mkTorsionElementAdapter R M m d ed pd =
  record { R = R ; M = M ; m = m ; decl = d ; expDomain = ed ; linkDomain = pd ; status = B.true }

isFilledTorsionElement : TorsionElementAdapter → B.Bool
isFilledTorsionElement a = TorsionElementAdapter.status a

-- Torsion submodule
record TorsionSubmoduleAdapter : Set₁ where
  field
    R : AR.IntegralDomain
    M : AM.LeftModule (AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing (AR.IntegralDomain.underlyingRing R)))
    decl : AM.TorsionSubmodule R M
    expDomain : AR.IntegralDomain
    linkDomain : AM.TorsionSubmodule.domain decl ≡ expDomain
    status : B.Bool

mkTorsionSubmoduleAdapter :
  (R : AR.IntegralDomain) →
  (M : AM.LeftModule (AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing (AR.IntegralDomain.underlyingRing R)))) →
  (d : AM.TorsionSubmodule R M) →
  (ed : AR.IntegralDomain) →
  (pd : AM.TorsionSubmodule.domain d ≡ ed) →
  TorsionSubmoduleAdapter
mkTorsionSubmoduleAdapter R M d ed pd =
  record { R = R ; M = M ; decl = d ; expDomain = ed ; linkDomain = pd ; status = B.true }

isFilledTorsionSubmodule : TorsionSubmoduleAdapter → B.Bool
isFilledTorsionSubmodule a = TorsionSubmoduleAdapter.status a

-- Torsion-free module
record TorsionFreeModuleAdapter : Set₁ where
  field
    R : AR.IntegralDomain
    M : AM.LeftModule (AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing (AR.IntegralDomain.underlyingRing R)))
    decl : AM.TorsionFreeModule R M
    expDomain : AR.IntegralDomain
    linkDomain : AM.TorsionFreeModule.domain decl ≡ expDomain
    status : B.Bool

mkTorsionFreeModuleAdapter :
  (R : AR.IntegralDomain) →
  (M : AM.LeftModule (AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing (AR.IntegralDomain.underlyingRing R)))) →
  (d : AM.TorsionFreeModule R M) →
  (ed : AR.IntegralDomain) →
  (pd : AM.TorsionFreeModule.domain d ≡ ed) →
  TorsionFreeModuleAdapter
mkTorsionFreeModuleAdapter R M d ed pd =
  record { R = R ; M = M ; decl = d ; expDomain = ed ; linkDomain = pd ; status = B.true }

isFilledTorsionFreeModule : TorsionFreeModuleAdapter → B.Bool
isFilledTorsionFreeModule a = TorsionFreeModuleAdapter.status a

-- Structure theorem for finitely generated modules over PID
record StructureTheoremPIDAdapter : Set₁ where
  field
    R : AR.PrincipalIdealDomain
    M : AM.LeftModule (AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing (AR.IntegralDomain.underlyingRing (AR.PrincipalIdealDomain.domain R))))
    decl : AM.StructureTheoremPID R M
    expPID : AR.PrincipalIdealDomain
    linkPID : AM.StructureTheoremPID.pid decl ≡ expPID
    status : B.Bool

mkStructureTheoremPIDAdapter :
  (R : AR.PrincipalIdealDomain) →
  (M : AM.LeftModule (AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing (AR.IntegralDomain.underlyingRing (AR.PrincipalIdealDomain.domain R))))) →
  (d : AM.StructureTheoremPID R M) →
  (ep : AR.PrincipalIdealDomain) →
  (pp : AM.StructureTheoremPID.pid d ≡ ep) →
  StructureTheoremPIDAdapter
mkStructureTheoremPIDAdapter R M d ep pp =
  record { R = R ; M = M ; decl = d ; expPID = ep ; linkPID = pp ; status = B.true }

isFilledStructureTheoremPID : StructureTheoremPIDAdapter → B.Bool
isFilledStructureTheoremPID a = StructureTheoremPIDAdapter.status a

-- Hom functor
record HomFunctorAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    M : AM.LeftModule R
    decl : AM.HomFunctor R M
    expRing : AR.RingDeclaration
    linkRing : AM.HomFunctor.ring decl ≡ expRing
    status : B.Bool

mkHomFunctorAdapter :
  (R : AR.RingDeclaration) →
  (M : AM.LeftModule R) →
  (d : AM.HomFunctor R M) →
  (er : AR.RingDeclaration) →
  (pr : AM.HomFunctor.ring d ≡ er) →
  HomFunctorAdapter
mkHomFunctorAdapter R M d er pr =
  record { R = R ; M = M ; decl = d ; expRing = er ; linkRing = pr ; status = B.true }

isFilledHomFunctor : HomFunctorAdapter → B.Bool
isFilledHomFunctor a = HomFunctorAdapter.status a

-- Dual module
record DualModuleAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    M : AM.LeftModule R
    decl : AM.DualModule R M
    expRing : AR.RingDeclaration
    linkRing : AM.DualModule.ring decl ≡ expRing
    status : B.Bool

mkDualModuleAdapter :
  (R : AR.RingDeclaration) →
  (M : AM.LeftModule R) →
  (d : AM.DualModule R M) →
  (er : AR.RingDeclaration) →
  (pr : AM.DualModule.ring d ≡ er) →
  DualModuleAdapter
mkDualModuleAdapter R M d er pr =
  record { R = R ; M = M ; decl = d ; expRing = er ; linkRing = pr ; status = B.true }

isFilledDualModule : DualModuleAdapter → B.Bool
isFilledDualModule a = DualModuleAdapter.status a

-- Reflexive module
record ReflexiveModuleAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    M : AM.LeftModule R
    decl : AM.ReflexiveModule R M
    expRing : AR.RingDeclaration
    linkRing : AM.ReflexiveModule.ring decl ≡ expRing
    status : B.Bool

mkReflexiveModuleAdapter :
  (R : AR.RingDeclaration) →
  (M : AM.LeftModule R) →
  (d : AM.ReflexiveModule R M) →
  (er : AR.RingDeclaration) →
  (pr : AM.ReflexiveModule.ring d ≡ er) →
  ReflexiveModuleAdapter
mkReflexiveModuleAdapter R M d er pr =
  record { R = R ; M = M ; decl = d ; expRing = er ; linkRing = pr ; status = B.true }

isFilledReflexiveModule : ReflexiveModuleAdapter → B.Bool
isFilledReflexiveModule a = ReflexiveModuleAdapter.status a

-- Tensor product of modules
record TensorProductModuleAdapter : Set₁ where
  field
    R : AR.CommutativeRingDeclaration
    M : AM.LeftModule (AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing R))
    N : AM.LeftModule (AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing R))
    decl : AM.TensorProduct R M N
    expTensorProduct : AM.LeftModule (AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing R))
    linkTensorProduct : AM.TensorProduct.tensorProduct decl ≡ expTensorProduct
    status : B.Bool

mkTensorProductModuleAdapter :
  (R : AR.CommutativeRingDeclaration) →
  (M N : AM.LeftModule (AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing R))) →
  (d : AM.TensorProduct R M N) →
  (et : AM.LeftModule (AR.UnitalRingDeclaration.underlyingRing (AR.CommutativeRingDeclaration.underlyingRing R))) →
  (pt : AM.TensorProduct.tensorProduct d ≡ et) →
  TensorProductModuleAdapter
mkTensorProductModuleAdapter R M N d et pt =
  record { R = R ; M = M ; N = N ; decl = d ; expTensorProduct = et ; linkTensorProduct = pt ; status = B.true }

isFilledTensorProductModule : TensorProductModuleAdapter → B.Bool
isFilledTensorProductModule a = TensorProductModuleAdapter.status a

-- Free module
record FreeModuleAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    X : M.Identifier
    decl : AM.FreeModule R X
    expRing : AR.RingDeclaration
    linkRing : AM.FreeModule.ring decl ≡ expRing
    status : B.Bool

mkFreeModuleAdapter :
  (R : AR.RingDeclaration) →
  (X : M.Identifier) →
  (d : AM.FreeModule R X) →
  (er : AR.RingDeclaration) →
  (pr : AM.FreeModule.ring d ≡ er) →
  FreeModuleAdapter
mkFreeModuleAdapter R X d er pr =
  record { R = R ; X = X ; decl = d ; expRing = er ; linkRing = pr ; status = B.true }

isFilledFreeModule : FreeModuleAdapter → B.Bool
isFilledFreeModule a = FreeModuleAdapter.status a

-- Free module functor
record FreeModuleFunctorAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    decl : AM.FreeModuleFunctor R
    expRing : AR.RingDeclaration
    linkRing : AM.FreeModuleFunctor.ring decl ≡ expRing
    status : B.Bool

mkFreeModuleFunctorAdapter :
  (R : AR.RingDeclaration) →
  (d : AM.FreeModuleFunctor R) →
  (er : AR.RingDeclaration) →
  (pr : AM.FreeModuleFunctor.ring d ≡ er) →
  FreeModuleFunctorAdapter
mkFreeModuleFunctorAdapter R d er pr =
  record { R = R ; decl = d ; expRing = er ; linkRing = pr ; status = B.true }

isFilledFreeModuleFunctor : FreeModuleFunctorAdapter → B.Bool
isFilledFreeModuleFunctor a = FreeModuleFunctorAdapter.status a

-- Forgetful module functor
record ForgetfulModuleFunctorAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    decl : AM.ForgetfulModuleFunctor R
    expRing : AR.RingDeclaration
    linkRing : AM.ForgetfulModuleFunctor.ring decl ≡ expRing
    status : B.Bool

mkForgetfulModuleFunctorAdapter :
  (R : AR.RingDeclaration) →
  (d : AM.ForgetfulModuleFunctor R) →
  (er : AR.RingDeclaration) →
  (pr : AM.ForgetfulModuleFunctor.ring d ≡ er) →
  ForgetfulModuleFunctorAdapter
mkForgetfulModuleFunctorAdapter R d er pr =
  record { R = R ; decl = d ; expRing = er ; linkRing = pr ; status = B.true }

isFilledForgetfulModuleFunctor : ForgetfulModuleFunctorAdapter → B.Bool
isFilledForgetfulModuleFunctor a = ForgetfulModuleFunctorAdapter.status a

-- Right module
record RightModuleAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    decl : AM.RightModule R
    expRing : AR.RingDeclaration
    linkRing : AM.RightModule.ring decl ≡ expRing
    status : B.Bool

mkRightModuleAdapter :
  (R : AR.RingDeclaration) →
  (d : AM.RightModule R) →
  (er : AR.RingDeclaration) →
  (pr : AM.RightModule.ring d ≡ er) →
  RightModuleAdapter
mkRightModuleAdapter R d er pr =
  record { R = R ; decl = d ; expRing = er ; linkRing = pr ; status = B.true }

isFilledRightModule : RightModuleAdapter → B.Bool
isFilledRightModule a = RightModuleAdapter.status a


-- ============================================================================
-- Extension Degree and Polynomial-Related Adapters
-- ============================================================================

-- Extension degree [E : F]
record ExtensionDegreeAdapter : Set₁ where
  field
    F E : AFB.FieldDeclaration
    decl : AFB.ExtensionDegree F E
    expected : AFB.FieldDeclaration
    expExt : AFB.FieldDeclaration
    link : AFB.ExtensionDegree.baseField decl ≡ expected
    linkExt : AFB.ExtensionDegree.extensionField decl ≡ expExt
    status : B.Bool

mkExtensionDegreeAdapter :
  (F E : AFB.FieldDeclaration) →
  (d : AFB.ExtensionDegree F E) →
  (ef : AFB.FieldDeclaration) →
  (ee : AFB.FieldDeclaration) →
  (pf : AFB.ExtensionDegree.baseField d ≡ ef) →
  (pe : AFB.ExtensionDegree.extensionField d ≡ ee) →
  ExtensionDegreeAdapter
mkExtensionDegreeAdapter F E d ef ee pf pe =
  record { F = F ; E = E ; decl = d ; expected = ef ; expExt = ee ; link = pf ; linkExt = pe ; status = B.true }

isFilledExtensionDegree : ExtensionDegreeAdapter → B.Bool
isFilledExtensionDegree a = ExtensionDegreeAdapter.status a


-- Inseparable degree [E : F]ᵢ
record InseparableDegreeAdapter : Set₁ where
  field
    F E : AFB.FieldDeclaration
    decl : AFA.InseparableDegree F E
    expected : AFB.FieldDeclaration
    expExt : AFB.FieldDeclaration
    link : AFA.InseparableDegree.baseField decl ≡ expected
    linkExt : AFA.InseparableDegree.extensionField decl ≡ expExt
    status : B.Bool

mkInseparableDegreeAdapter :
  (F E : AFB.FieldDeclaration) →
  (d : AFA.InseparableDegree F E) →
  (ef : AFB.FieldDeclaration) →
  (ee : AFB.FieldDeclaration) →
  (pf : AFA.InseparableDegree.baseField d ≡ ef) →
  (pe : AFA.InseparableDegree.extensionField d ≡ ee) →
  InseparableDegreeAdapter
mkInseparableDegreeAdapter F E d ef ee pf pe =
  record { F = F ; E = E ; decl = d ; expected = ef ; expExt = ee ; link = pf ; linkExt = pe ; status = B.true }

isFilledInseparableDegree : InseparableDegreeAdapter → B.Bool
isFilledInseparableDegree a = InseparableDegreeAdapter.status a


-- Separable degree [E : F]ₛ
record SeparableDegreeAdapter : Set₁ where
  field
    F E : AFB.FieldDeclaration
    decl : AFA.SeparableDegree F E
    expected : AFB.FieldDeclaration
    expExt : AFB.FieldDeclaration
    link : AFA.SeparableDegree.baseField decl ≡ expected
    linkExt : AFA.SeparableDegree.extensionField decl ≡ expExt
    status : B.Bool

mkSeparableDegreeAdapter :
  (F E : AFB.FieldDeclaration) →
  (d : AFA.SeparableDegree F E) →
  (ef : AFB.FieldDeclaration) →
  (ee : AFB.FieldDeclaration) →
  (pf : AFA.SeparableDegree.baseField d ≡ ef) →
  (pe : AFA.SeparableDegree.extensionField d ≡ ee) →
  SeparableDegreeAdapter
mkSeparableDegreeAdapter F E d ef ee pf pe =
  record { F = F ; E = E ; decl = d ; expected = ef ; expExt = ee ; link = pf ; linkExt = pe ; status = B.true }

isFilledSeparableDegree : SeparableDegreeAdapter → B.Bool
isFilledSeparableDegree a = SeparableDegreeAdapter.status a


-- Simple extension F(α)
record SimpleExtensionAdapter : Set₁ where
  field
    F E : AFB.FieldDeclaration
    α : M.Identifier
    decl : AFB.SimpleExtension F E α
    expected : AFB.FieldDeclaration
    expExt : AFB.FieldDeclaration
    link : AFB.SimpleExtension.baseField decl ≡ expected
    linkExt : AFB.SimpleExtension.extensionField decl ≡ expExt
    status : B.Bool

mkSimpleExtensionAdapter :
  (F E : AFB.FieldDeclaration) →
  (α : M.Identifier) →
  (d : AFB.SimpleExtension F E α) →
  (ef : AFB.FieldDeclaration) →
  (ee : AFB.FieldDeclaration) →
  (pf : AFB.SimpleExtension.baseField d ≡ ef) →
  (pe : AFB.SimpleExtension.extensionField d ≡ ee) →
  SimpleExtensionAdapter
mkSimpleExtensionAdapter F E α d ef ee pf pe =
  record { F = F ; E = E ; α = α ; decl = d ; expected = ef ; expExt = ee ; link = pf ; linkExt = pe ; status = B.true }

isFilledSimpleExtension : SimpleExtensionAdapter → B.Bool
isFilledSimpleExtension a = SimpleExtensionAdapter.status a


-- Transcendental element
record TranscendentalElementAdapter : Set₁ where
  field
    F E : AFB.FieldDeclaration
    α : M.Identifier
    decl : AFB.TranscendentalElement F E α
    expected : AFB.FieldDeclaration
    expExt : AFB.FieldDeclaration
    link : AFB.TranscendentalElement.baseField decl ≡ expected
    linkExt : AFB.TranscendentalElement.extensionField decl ≡ expExt
    status : B.Bool

mkTranscendentalElementAdapter :
  (F E : AFB.FieldDeclaration) →
  (α : M.Identifier) →
  (d : AFB.TranscendentalElement F E α) →
  (ef : AFB.FieldDeclaration) →
  (ee : AFB.FieldDeclaration) →
  (pf : AFB.TranscendentalElement.baseField d ≡ ef) →
  (pe : AFB.TranscendentalElement.extensionField d ≡ ee) →
  TranscendentalElementAdapter
mkTranscendentalElementAdapter F E α d ef ee pf pe =
  record { F = F ; E = E ; α = α ; decl = d ; expected = ef ; expExt = ee ; link = pf ; linkExt = pe ; status = B.true }

isFilledTranscendentalElement : TranscendentalElementAdapter → B.Bool
isFilledTranscendentalElement a = TranscendentalElementAdapter.status a


-- Transcendence basis
record TranscendenceBasisAdapter : Set₁ where
  field
    F E : AFB.FieldDeclaration
    decl : AFB.TranscendenceBasis F E
    expected : AFB.FieldDeclaration
    expExt : AFB.FieldDeclaration
    link : AFB.TranscendenceBasis.baseField decl ≡ expected
    linkExt : AFB.TranscendenceBasis.extensionField decl ≡ expExt
    status : B.Bool

mkTranscendenceBasisAdapter :
  (F E : AFB.FieldDeclaration) →
  (d : AFB.TranscendenceBasis F E) →
  (ef : AFB.FieldDeclaration) →
  (ee : AFB.FieldDeclaration) →
  (pf : AFB.TranscendenceBasis.baseField d ≡ ef) →
  (pe : AFB.TranscendenceBasis.extensionField d ≡ ee) →
  TranscendenceBasisAdapter
mkTranscendenceBasisAdapter F E d ef ee pf pe =
  record { F = F ; E = E ; decl = d ; expected = ef ; expExt = ee ; link = pf ; linkExt = pe ; status = B.true }

isFilledTranscendenceBasis : TranscendenceBasisAdapter → B.Bool
isFilledTranscendenceBasis a = TranscendenceBasisAdapter.status a


-- ============================================================================
-- Enrichment-Specific Adapters
-- ============================================================================

-- Monoid as monoidal category
record MonoidAsMonoidalCategoryAdapter : Set₁ where
  field
    decl : AE.MonoidAsMonoidalCategory
    expectedMonoid : AFo.MonoidDeclaration
    link : AE.MonoidAsMonoidalCategory.monoid decl ≡ expectedMonoid
    status : B.Bool

mkMonoidAsMonoidalCategoryAdapter :
  (d : AE.MonoidAsMonoidalCategory) →
  (em : AFo.MonoidDeclaration) →
  (p : AE.MonoidAsMonoidalCategory.monoid d ≡ em) →
  MonoidAsMonoidalCategoryAdapter
mkMonoidAsMonoidalCategoryAdapter d em p =
  record { decl = d ; expectedMonoid = em ; link = p ; status = B.true }

isFilledMonoidAsMonoidalCategory : MonoidAsMonoidalCategoryAdapter → B.Bool
isFilledMonoidAsMonoidalCategory a = MonoidAsMonoidalCategoryAdapter.status a


-- Abelian group as symmetric monoidal category
record AbelianGroupAsSymmetricMonoidalAdapter : Set₁ where
  field
    decl : AE.AbelianGroupAsSymmetricMonoidal
    expectedAbGroup : AGA.AbelianGroupDeclaration
    link : AE.AbelianGroupAsSymmetricMonoidal.abelianGroup decl ≡ expectedAbGroup
    status : B.Bool

mkAbelianGroupAsSymmetricMonoidalAdapter :
  (d : AE.AbelianGroupAsSymmetricMonoidal) →
  (eab : AGA.AbelianGroupDeclaration) →
  (p : AE.AbelianGroupAsSymmetricMonoidal.abelianGroup d ≡ eab) →
  AbelianGroupAsSymmetricMonoidalAdapter
mkAbelianGroupAsSymmetricMonoidalAdapter d eab p =
  record { decl = d ; expectedAbGroup = eab ; link = p ; status = B.true }

isFilledAbelianGroupAsSymmetricMonoidal : AbelianGroupAsSymmetricMonoidalAdapter → B.Bool
isFilledAbelianGroupAsSymmetricMonoidal a = AbelianGroupAsSymmetricMonoidalAdapter.status a


-- Monoid-enriched category
record MonoidEnrichedCategoryAdapter : Set₁ where
  field
    decl : AE.MonoidEnrichedCategory
    expectedMonoid : AFo.MonoidDeclaration
    link : AE.MonoidEnrichedCategory.enrichingMonoid decl ≡ expectedMonoid
    status : B.Bool

mkMonoidEnrichedCategoryAdapter :
  (d : AE.MonoidEnrichedCategory) →
  (em : AFo.MonoidDeclaration) →
  (p : AE.MonoidEnrichedCategory.enrichingMonoid d ≡ em) →
  MonoidEnrichedCategoryAdapter
mkMonoidEnrichedCategoryAdapter d em p =
  record { decl = d ; expectedMonoid = em ; link = p ; status = B.true }

isFilledMonoidEnrichedCategory : MonoidEnrichedCategoryAdapter → B.Bool
isFilledMonoidEnrichedCategory a = MonoidEnrichedCategoryAdapter.status a


-- Distance category (enriched over ℕ)
record DistanceCategoryAdapter : Set₁ where
  field
    decl : AE.DistanceCategory
    expectedMonoid : AFo.MonoidDeclaration
    link : AE.DistanceCategory.naturalNumbersMonoid decl ≡ expectedMonoid
    status : B.Bool

mkDistanceCategoryAdapter :
  (d : AE.DistanceCategory) →
  (em : AFo.MonoidDeclaration) →
  (p : AE.DistanceCategory.naturalNumbersMonoid d ≡ em) →
  DistanceCategoryAdapter
mkDistanceCategoryAdapter d em p =
  record { decl = d ; expectedMonoid = em ; link = p ; status = B.true }

isFilledDistanceCategory : DistanceCategoryAdapter → B.Bool
isFilledDistanceCategory a = DistanceCategoryAdapter.status a


-- Ab-enriched category (additive category)
record AbEnrichedCategoryAdapter : Set₁ where
  field
    decl : AE.AbEnrichedCategory
    expectedCat : AGA.CategoryOfAbelianGroups
    link : AE.AbEnrichedCategory.enrichingCategory decl ≡ expectedCat
    status : B.Bool

mkAbEnrichedCategoryAdapter :
  (d : AE.AbEnrichedCategory) →
  (ec : AGA.CategoryOfAbelianGroups) →
  (p : AE.AbEnrichedCategory.enrichingCategory d ≡ ec) →
  AbEnrichedCategoryAdapter
mkAbEnrichedCategoryAdapter d ec p =
  record { decl = d ; expectedCat = ec ; link = p ; status = B.true }

isFilledAbEnrichedCategory : AbEnrichedCategoryAdapter → B.Bool
isFilledAbEnrichedCategory a = AbEnrichedCategoryAdapter.status a


-- Generic enrichment over monoidal category V
record GenericEnrichmentAdapter : Set₁ where
  field
    V : Enriched.MonoidalCategoryDeclaration
    decl : AE.GenericEnrichment V
    expectedCat : C.CategoryDeclaration
    link : AE.GenericEnrichment.enrichingCategory decl ≡ expectedCat
    status : B.Bool

mkGenericEnrichmentAdapter :
  (V : Enriched.MonoidalCategoryDeclaration) →
  (d : AE.GenericEnrichment V) →
  (ec : C.CategoryDeclaration) →
  (p : AE.GenericEnrichment.enrichingCategory d ≡ ec) →
  GenericEnrichmentAdapter
mkGenericEnrichmentAdapter V d ec p =
  record { V = V ; decl = d ; expectedCat = ec ; link = p ; status = B.true }

isFilledGenericEnrichment : GenericEnrichmentAdapter → B.Bool
isFilledGenericEnrichment a = GenericEnrichmentAdapter.status a


-- Group action enriched category
record GroupActionEnrichedCategoryAdapter : Set₁ where
  field
    decl : AE.GroupActionEnrichedCategory
    expectedGroup : AGB.GroupDeclaration
    link : AE.GroupActionEnrichedCategory.actingGroup decl ≡ expectedGroup
    status : B.Bool

mkGroupActionEnrichedCategoryAdapter :
  (d : AE.GroupActionEnrichedCategory) →
  (eg : AGB.GroupDeclaration) →
  (p : AE.GroupActionEnrichedCategory.actingGroup d ≡ eg) →
  GroupActionEnrichedCategoryAdapter
mkGroupActionEnrichedCategoryAdapter d eg p =
  record { decl = d ; expectedGroup = eg ; link = p ; status = B.true }

isFilledGroupActionEnrichedCategory : GroupActionEnrichedCategoryAdapter → B.Bool
isFilledGroupActionEnrichedCategory a = GroupActionEnrichedCategoryAdapter.status a


-- Module-enriched category (over a ring)
record ModuleEnrichedCategoryAdapter : Set₁ where
  field
    decl : AE.ModuleEnrichedCategory
    status : B.Bool

mkModuleEnrichedCategoryAdapter :
  (d : AE.ModuleEnrichedCategory) →
  ModuleEnrichedCategoryAdapter
mkModuleEnrichedCategoryAdapter d =
  record { decl = d ; status = B.true }

isFilledModuleEnrichedCategory : ModuleEnrichedCategoryAdapter → B.Bool
isFilledModuleEnrichedCategory a = ModuleEnrichedCategoryAdapter.status a


-- Lawvere theory enriched category
record LawvereTheoryEnrichedCategoryAdapter : Set₁ where
  field
    decl : AE.LawvereTheoryEnrichedCategory
    status : B.Bool

mkLawvereTheoryEnrichedCategoryAdapter :
  (d : AE.LawvereTheoryEnrichedCategory) →
  LawvereTheoryEnrichedCategoryAdapter
mkLawvereTheoryEnrichedCategoryAdapter d =
  record { decl = d ; status = B.true }

isFilledLawvereTheoryEnrichedCategory : LawvereTheoryEnrichedCategoryAdapter → B.Bool
isFilledLawvereTheoryEnrichedCategory a = LawvereTheoryEnrichedCategoryAdapter.status a


-- Ab self-enriched
record AbSelfEnrichedAdapter : Set₁ where
  field
    decl : AGA.AbSelfEnriched
    expectedCat : AGA.CategoryOfAbelianGroups
    link : AGA.AbSelfEnriched.category decl ≡ expectedCat
    status : B.Bool

mkAbSelfEnrichedAdapter :
  (d : AGA.AbSelfEnriched) →
  (ec : AGA.CategoryOfAbelianGroups) →
  (p : AGA.AbSelfEnriched.category d ≡ ec) →
  AbSelfEnrichedAdapter
mkAbSelfEnrichedAdapter d ec p =
  record { decl = d ; expectedCat = ec ; link = p ; status = B.true }

isFilledAbSelfEnriched : AbSelfEnrichedAdapter → B.Bool
isFilledAbSelfEnriched a = AbSelfEnrichedAdapter.status a


-- Ab self-enrichment via internal hom
record AbSelfEnrichmentViaInternalHomAdapter : Set₁ where
  field
    decl : AGA.AbSelfEnrichmentViaInternalHom
    expectedCat : AGA.CategoryOfAbelianGroups
    link : AGA.AbSelfEnrichmentViaInternalHom.category decl ≡ expectedCat
    status : B.Bool

mkAbSelfEnrichmentViaInternalHomAdapter :
  (d : AGA.AbSelfEnrichmentViaInternalHom) →
  (ec : AGA.CategoryOfAbelianGroups) →
  (p : AGA.AbSelfEnrichmentViaInternalHom.category d ≡ ec) →
  AbSelfEnrichmentViaInternalHomAdapter
mkAbSelfEnrichmentViaInternalHomAdapter d ec p =
  record { decl = d ; expectedCat = ec ; link = p ; status = B.true }

isFilledAbSelfEnrichmentViaInternalHom : AbSelfEnrichmentViaInternalHomAdapter → B.Bool
isFilledAbSelfEnrichmentViaInternalHom a = AbSelfEnrichmentViaInternalHomAdapter.status a


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
    status : B.Bool

mkExactSequenceAdapter :
  (R : AR.RingDeclaration) →
  (d : AM.ExactSequence R) →
  (er : AR.RingDeclaration) →
  (p : AM.ExactSequence.ring d ≡ er) →
  ExactSequenceAdapter
mkExactSequenceAdapter R d er p =
  record { R = R ; decl = d ; expectedRing = er ; link = p ; status = B.true }

isFilledExactSequence : ExactSequenceAdapter → B.Bool
isFilledExactSequence a = ExactSequenceAdapter.status a


-- Category of modules R-Mod
record CategoryOfModulesAdapter : Set₁ where
  field
    R : AR.RingDeclaration
    decl : AM.CategoryOfModules R
    expectedRing : AR.RingDeclaration
    link : AM.CategoryOfModules.ring decl ≡ expectedRing
    status : B.Bool

mkCategoryOfModulesAdapter :
  (R : AR.RingDeclaration) →
  (d : AM.CategoryOfModules R) →
  (er : AR.RingDeclaration) →
  (p : AM.CategoryOfModules.ring d ≡ er) →
  CategoryOfModulesAdapter
mkCategoryOfModulesAdapter R d er p =
  record { R = R ; decl = d ; expectedRing = er ; link = p ; status = B.true }

isFilledCategoryOfModules : CategoryOfModulesAdapter → B.Bool
isFilledCategoryOfModules a = CategoryOfModulesAdapter.status a


-- Vector space over a field
record VectorSpaceAdapter : Set₁ where
  field
    F : AFB.FieldDeclaration
    decl : AM.VectorSpace F
    expectedField : AFB.FieldDeclaration
    link : AM.VectorSpace.field' decl ≡ expectedField
    status : B.Bool

mkVectorSpaceAdapter :
  (F : AFB.FieldDeclaration) →
  (d : AM.VectorSpace F) →
  (ef : AFB.FieldDeclaration) →
  (p : AM.VectorSpace.field' d ≡ ef) →
  VectorSpaceAdapter
mkVectorSpaceAdapter F d ef p =
  record { F = F ; decl = d ; expectedField = ef ; link = p ; status = B.true }

isFilledVectorSpace : VectorSpaceAdapter → B.Bool
isFilledVectorSpace a = VectorSpaceAdapter.status a


-- R-algebra (ring with compatible R-module structure)
record RAlgebraAdapter : Set₁ where
  field
    R : AR.CommutativeRingDeclaration
    decl : AM.RAlgebra R
    expectedRing : AR.CommutativeRingDeclaration
    link : AM.RAlgebra.coefficientRing decl ≡ expectedRing
    status : B.Bool

mkRAlgebraAdapter :
  (R : AR.CommutativeRingDeclaration) →
  (d : AM.RAlgebra R) →
  (er : AR.CommutativeRingDeclaration) →
  (p : AM.RAlgebra.coefficientRing d ≡ er) →
  RAlgebraAdapter
mkRAlgebraAdapter R d er p =
  record { R = R ; decl = d ; expectedRing = er ; link = p ; status = B.true }

isFilledRAlgebra : RAlgebraAdapter → B.Bool
isFilledRAlgebra a = RAlgebraAdapter.status a


-- Algebra homomorphism
record AlgebraHomomorphismAdapter : Set₁ where
  field
    R : AR.CommutativeRingDeclaration
    A B : AM.RAlgebra R
    decl : AM.AlgebraHomomorphism R A B
    expectedRing : AR.CommutativeRingDeclaration
    link : AM.AlgebraHomomorphism.coefficientRing decl ≡ expectedRing
    status : B.Bool

mkAlgebraHomomorphismAdapter :
  (R : AR.CommutativeRingDeclaration) →
  (A B : AM.RAlgebra R) →
  (d : AM.AlgebraHomomorphism R A B) →
  (er : AR.CommutativeRingDeclaration) →
  (p : AM.AlgebraHomomorphism.coefficientRing d ≡ er) →
  AlgebraHomomorphismAdapter
mkAlgebraHomomorphismAdapter R A B d er p =
  record { R = R ; A = A ; B = B ; decl = d ; expectedRing = er ; link = p ; status = B.true }

isFilledAlgebraHomomorphism : AlgebraHomomorphismAdapter → B.Bool
isFilledAlgebraHomomorphism a = AlgebraHomomorphismAdapter.status a


------------------------------------------------------------------------
-- Monad-Adjunction Theory (Chapter2.Level2sub4)
------------------------------------------------------------------------

-- Category of T-algebras (Eilenberg-Moore category)
record CategoryOfAlgebrasAdapter : Set₁ where
  field
    decl : C2S4.CategoryOfAlgebras
    expectedMonad : C2S4.MonadDeclaration
    link : C2S4.CategoryOfAlgebras.monad decl ≡ expectedMonad
    status : B.Bool

mkCategoryOfAlgebrasAdapter :
  (d : C2S4.CategoryOfAlgebras) →
  (em : C2S4.MonadDeclaration) →
  (p : C2S4.CategoryOfAlgebras.monad d ≡ em) →
  CategoryOfAlgebrasAdapter
mkCategoryOfAlgebrasAdapter d em p =
  record { decl = d ; expectedMonad = em ; link = p ; status = B.true }

isFilledCategoryOfAlgebras : CategoryOfAlgebrasAdapter → B.Bool
isFilledCategoryOfAlgebras a = CategoryOfAlgebrasAdapter.status a


-- Theorem: Adjunction induces monad
record AdjunctionInducesMonadTheoremAdapter : Set₁ where
  field
    decl : C2S4.AdjunctionInducesMonadTheorem
    expectedMonad : C2S4.MonadDeclaration
    link : C2S4.AdjunctionInducesMonadTheorem.inducedMonad decl ≡ expectedMonad
    status : B.Bool

mkAdjunctionInducesMonadTheoremAdapter :
  (d : C2S4.AdjunctionInducesMonadTheorem) →
  (em : C2S4.MonadDeclaration) →
  (p : C2S4.AdjunctionInducesMonadTheorem.inducedMonad d ≡ em) →
  AdjunctionInducesMonadTheoremAdapter
mkAdjunctionInducesMonadTheoremAdapter d em p =
  record { decl = d ; expectedMonad = em ; link = p ; status = B.true }

isFilledAdjunctionInducesMonadTheorem : AdjunctionInducesMonadTheoremAdapter → B.Bool
isFilledAdjunctionInducesMonadTheorem a = AdjunctionInducesMonadTheoremAdapter.status a


-- Eilenberg-Moore adjunction from monad
record EilenbergMooreAdjunctionAdapter : Set₁ where
  field
    decl : C2S4.EilenbergMooreAdjunction
    expectedMonad : C2S4.MonadDeclaration
    expectedAlgCat : C2S4.CategoryOfAlgebras
    link1 : C2S4.EilenbergMooreAdjunction.monad decl ≡ expectedMonad
    link2 : C2S4.EilenbergMooreAdjunction.algebraCategory decl ≡ expectedAlgCat
    status : B.Bool

mkEilenbergMooreAdjunctionAdapter :
  (d : C2S4.EilenbergMooreAdjunction) →
  (em : C2S4.MonadDeclaration) →
  (eac : C2S4.CategoryOfAlgebras) →
  (p1 : C2S4.EilenbergMooreAdjunction.monad d ≡ em) →
  (p2 : C2S4.EilenbergMooreAdjunction.algebraCategory d ≡ eac) →
  EilenbergMooreAdjunctionAdapter
mkEilenbergMooreAdjunctionAdapter d em eac p1 p2 =
  record { decl = d ; expectedMonad = em ; expectedAlgCat = eac ; link1 = p1 ; link2 = p2 ; status = B.true }

isFilledEilenbergMooreAdjunction : EilenbergMooreAdjunctionAdapter → B.Bool
isFilledEilenbergMooreAdjunction a = EilenbergMooreAdjunctionAdapter.status a


-- Monad-adjunction correspondence theorem
record MonadAdjunctionCorrespondenceTheoremAdapter : Set₁ where
  field
    decl : C2S4.MonadAdjunctionCorrespondenceTheorem
    expectedMonad : C2S4.MonadDeclaration
    expectedEM : C2S4.EilenbergMooreAdjunction
    link1 : C2S4.MonadAdjunctionCorrespondenceTheorem.monad decl ≡ expectedMonad
    link2 : C2S4.MonadAdjunctionCorrespondenceTheorem.emAdjunction decl ≡ expectedEM
    status : B.Bool

mkMonadAdjunctionCorrespondenceTheoremAdapter :
  (d : C2S4.MonadAdjunctionCorrespondenceTheorem) →
  (em : C2S4.MonadDeclaration) →
  (eEM : C2S4.EilenbergMooreAdjunction) →
  (p1 : C2S4.MonadAdjunctionCorrespondenceTheorem.monad d ≡ em) →
  (p2 : C2S4.MonadAdjunctionCorrespondenceTheorem.emAdjunction d ≡ eEM) →
  MonadAdjunctionCorrespondenceTheoremAdapter
mkMonadAdjunctionCorrespondenceTheoremAdapter d em eEM p1 p2 =
  record { decl = d ; expectedMonad = em ; expectedEM = eEM ; link1 = p1 ; link2 = p2 ; status = B.true }

isFilledMonadAdjunctionCorrespondenceTheorem : MonadAdjunctionCorrespondenceTheoremAdapter → B.Bool
isFilledMonadAdjunctionCorrespondenceTheorem a = MonadAdjunctionCorrespondenceTheoremAdapter.status a


-- Beck monadicity theorem
record BeckMonadicityTheoremAdapter : Set₁ where
  field
    decl : C2S4.BeckMonadicityTheorem
    expectedReflects : C2S4.ReflectsIsomorphismsProperty
    link : C2S4.BeckMonadicityTheorem.reflectsIsomorphisms decl ≡ expectedReflects
    status : B.Bool

mkBeckMonadicityTheoremAdapter :
  (d : C2S4.BeckMonadicityTheorem) →
  (er : C2S4.ReflectsIsomorphismsProperty) →
  (p : C2S4.BeckMonadicityTheorem.reflectsIsomorphisms d ≡ er) →
  BeckMonadicityTheoremAdapter
mkBeckMonadicityTheoremAdapter d er p =
  record { decl = d ; expectedReflects = er ; link = p ; status = B.true }

isFilledBeckMonadicityTheorem : BeckMonadicityTheoremAdapter → B.Bool
isFilledBeckMonadicityTheorem a = BeckMonadicityTheoremAdapter.status a


-- Functor property: is monadic
record MonadicFunctorPropertyAdapter : Set₁ where
  field
    decl : C2S4.MonadicFunctorProperty
    expectedFunctor : M.Identifier
    link : C2S4.MonadicFunctorProperty.functor decl ≡ expectedFunctor
    status : B.Bool

mkMonadicFunctorPropertyAdapter :
  (d : C2S4.MonadicFunctorProperty) →
  (ef : M.Identifier) →
  (p : C2S4.MonadicFunctorProperty.functor d ≡ ef) →
  MonadicFunctorPropertyAdapter
mkMonadicFunctorPropertyAdapter d ef p =
  record { decl = d ; expectedFunctor = ef ; link = p ; status = B.true }

isFilledMonadicFunctorProperty : MonadicFunctorPropertyAdapter → B.Bool
isFilledMonadicFunctorProperty a = MonadicFunctorPropertyAdapter.status a


-- Comonad from adjunction (for descent theory)
record ComonadFromAdjunctionAdapter : Set₁ where
  field
    decl : C2S4.ComonadFromAdjunction
    expectedComonad : C2S4.ComonadDeclaration
    link : C2S4.ComonadFromAdjunction.inducedComonad decl ≡ expectedComonad
    status : B.Bool

mkComonadFromAdjunctionAdapter :
  (d : C2S4.ComonadFromAdjunction) →
  (ec : C2S4.ComonadDeclaration) →
  (p : C2S4.ComonadFromAdjunction.inducedComonad d ≡ ec) →
  ComonadFromAdjunctionAdapter
mkComonadFromAdjunctionAdapter d ec p =
  record { decl = d ; expectedComonad = ec ; link = p ; status = B.true }

isFilledComonadFromAdjunction : ComonadFromAdjunctionAdapter → B.Bool
isFilledComonadFromAdjunction a = ComonadFromAdjunctionAdapter.status a


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
    status : B.Bool

mkForgetfulFunctorFromAlgebrasCreatesLimitsTheoremAdapter :
  (d : C2S4.ForgetfulFunctorFromAlgebrasCreatesLimitsTheorem) →
  (em : C2S4.MonadDeclaration) →
  (eac : C2S4.CategoryOfAlgebras) →
  (p1 : C2S4.ForgetfulFunctorFromAlgebrasCreatesLimitsTheorem.monad d ≡ em) →
  (p2 : C2S4.ForgetfulFunctorFromAlgebrasCreatesLimitsTheorem.algebraCategory d ≡ eac) →
  ForgetfulFunctorFromAlgebrasCreatesLimitsTheoremAdapter
mkForgetfulFunctorFromAlgebrasCreatesLimitsTheoremAdapter d em eac p1 p2 =
  record { decl = d ; expectedMonad = em ; expectedAlgCat = eac ; link1 = p1 ; link2 = p2 ; status = B.true }

isFilledForgetfulFunctorFromAlgebrasCreatesLimitsTheorem : ForgetfulFunctorFromAlgebrasCreatesLimitsTheoremAdapter → B.Bool
isFilledForgetfulFunctorFromAlgebrasCreatesLimitsTheorem a = ForgetfulFunctorFromAlgebrasCreatesLimitsTheoremAdapter.status a


-- Corollary: Completeness of algebra categories
record CompletenessOfAlgebraCategoriesCorollaryAdapter : Set₁ where
  field
    decl : C2S4.CompletenessOfAlgebraCategoriesCorollary
    expectedMonad : C2S4.MonadDeclaration
    expectedAlgCat : C2S4.CategoryOfAlgebras
    link1 : C2S4.CompletenessOfAlgebraCategoriesCorollary.monad decl ≡ expectedMonad
    link2 : C2S4.CompletenessOfAlgebraCategoriesCorollary.algebraCategory decl ≡ expectedAlgCat
    status : B.Bool

mkCompletenessOfAlgebraCategoriesCorollaryAdapter :
  (d : C2S4.CompletenessOfAlgebraCategoriesCorollary) →
  (em : C2S4.MonadDeclaration) →
  (eac : C2S4.CategoryOfAlgebras) →
  (p1 : C2S4.CompletenessOfAlgebraCategoriesCorollary.monad d ≡ em) →
  (p2 : C2S4.CompletenessOfAlgebraCategoriesCorollary.algebraCategory d ≡ eac) →
  CompletenessOfAlgebraCategoriesCorollaryAdapter
mkCompletenessOfAlgebraCategoriesCorollaryAdapter d em eac p1 p2 =
  record { decl = d ; expectedMonad = em ; expectedAlgCat = eac ; link1 = p1 ; link2 = p2 ; status = B.true }

isFilledCompletenessOfAlgebraCategoriesCorollary : CompletenessOfAlgebraCategoriesCorollaryAdapter → B.Bool
isFilledCompletenessOfAlgebraCategoriesCorollary a = CompletenessOfAlgebraCategoriesCorollaryAdapter.status a


-- Reflexive pair
record ReflexivePairAdapter : Set₁ where
  field
    decl : C2S4.ReflexivePair
    expectedDomain : M.Identifier
    expectedCodomain : M.Identifier
    link1 : C2S4.ReflexivePair.domain decl ≡ expectedDomain
    link2 : C2S4.ReflexivePair.codomain decl ≡ expectedCodomain
    status : B.Bool

mkReflexivePairAdapter :
  (d : C2S4.ReflexivePair) →
  (edom : M.Identifier) →
  (ecod : M.Identifier) →
  (p1 : C2S4.ReflexivePair.domain d ≡ edom) →
  (p2 : C2S4.ReflexivePair.codomain d ≡ ecod) →
  ReflexivePairAdapter
mkReflexivePairAdapter d edom ecod p1 p2 =
  record { decl = d ; expectedDomain = edom ; expectedCodomain = ecod ; link1 = p1 ; link2 = p2 ; status = B.true }

isFilledReflexivePair : ReflexivePairAdapter → B.Bool
isFilledReflexivePair a = ReflexivePairAdapter.status a


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
    status : B.Bool

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
  record { decl = d ; expectedMonad = em ; expectedAlgCat = eac ; expectedRefPair = erp ; link1 = p1 ; link2 = p2 ; link3 = p3 ; status = B.true }

isFilledForgetfulFunctorPreservesCertainCoequalizersTheorem : ForgetfulFunctorPreservesCertainCoequalizersTheoremAdapter → B.Bool
isFilledForgetfulFunctorPreservesCertainCoequalizersTheorem a = ForgetfulFunctorPreservesCertainCoequalizersTheoremAdapter.status a


-- Functor property: reflects isomorphisms
record ReflectsIsomorphismsPropertyAdapter : Set₁ where
  field
    decl : C2S4.ReflectsIsomorphismsProperty
    expectedFunctor : M.Identifier
    link : C2S4.ReflectsIsomorphismsProperty.functor decl ≡ expectedFunctor
    status : B.Bool

mkReflectsIsomorphismsPropertyAdapter :
  (d : C2S4.ReflectsIsomorphismsProperty) →
  (ef : M.Identifier) →
  (p : C2S4.ReflectsIsomorphismsProperty.functor d ≡ ef) →
  ReflectsIsomorphismsPropertyAdapter
mkReflectsIsomorphismsPropertyAdapter d ef p =
  record { decl = d ; expectedFunctor = ef ; link = p ; status = B.true }

isFilledReflectsIsomorphismsProperty : ReflectsIsomorphismsPropertyAdapter → B.Bool
isFilledReflectsIsomorphismsProperty a = ReflectsIsomorphismsPropertyAdapter.status a


-- U-split pair
record USplitPairAdapter : Set₁ where
  field
    decl : C2S4.USplitPair
    expectedFunctor : M.Identifier
    link : C2S4.USplitPair.functor decl ≡ expectedFunctor
    status : B.Bool

mkUSplitPairAdapter :
  (d : C2S4.USplitPair) →
  (ef : M.Identifier) →
  (p : C2S4.USplitPair.functor d ≡ ef) →
  USplitPairAdapter
mkUSplitPairAdapter d ef p =
  record { decl = d ; expectedFunctor = ef ; link = p ; status = B.true }

isFilledUSplitPair : USplitPairAdapter → B.Bool
isFilledUSplitPair a = USplitPairAdapter.status a


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
    status : B.Bool

mkRegularCategoryDeclarationAdapter :
  (d : C2S2.RegularCategoryDeclaration) →
  (efl : C2S2.FiniteLimitsProperty) →
  (es : C2S2.StabilityUnderPullbackProperty) →
  (p1 : C2S2.RegularCategoryDeclaration.finiteLimits d ≡ efl) →
  (p2 : C2S2.RegularCategoryDeclaration.regularEpiStability d ≡ es) →
  RegularCategoryDeclarationAdapter
mkRegularCategoryDeclarationAdapter d efl es p1 p2 =
  record { decl = d ; expectedFiniteLimits = efl ; expectedStability = es ; link1 = p1 ; link2 = p2 ; status = B.true }

isFilledRegularCategoryDeclaration : RegularCategoryDeclarationAdapter → B.Bool
isFilledRegularCategoryDeclaration a = RegularCategoryDeclarationAdapter.status a


-- Kernel pair of a morphism
record KernelPairDeclarationAdapter : Set₁ where
  field
    decl : C2S2.KernelPairDeclaration
    expectedMorphism : M.Identifier
    link : C2S2.KernelPairDeclaration.morphism decl ≡ expectedMorphism
    status : B.Bool

mkKernelPairDeclarationAdapter :
  (d : C2S2.KernelPairDeclaration) →
  (em : M.Identifier) →
  (p : C2S2.KernelPairDeclaration.morphism d ≡ em) →
  KernelPairDeclarationAdapter
mkKernelPairDeclarationAdapter d em p =
  record { decl = d ; expectedMorphism = em ; link = p ; status = B.true }

isFilledKernelPairDeclaration : KernelPairDeclarationAdapter → B.Bool
isFilledKernelPairDeclaration a = KernelPairDeclarationAdapter.status a


-- Internal equivalence relation
record InternalEquivalenceRelationDeclarationAdapter : Set₁ where
  field
    decl : C2S2.InternalEquivalenceRelationDeclaration
    expectedObjectA : M.Identifier
    link : C2S2.InternalEquivalenceRelationDeclaration.objectA decl ≡ expectedObjectA
    status : B.Bool

mkInternalEquivalenceRelationDeclarationAdapter :
  (d : C2S2.InternalEquivalenceRelationDeclaration) →
  (eoa : M.Identifier) →
  (p : C2S2.InternalEquivalenceRelationDeclaration.objectA d ≡ eoa) →
  InternalEquivalenceRelationDeclarationAdapter
mkInternalEquivalenceRelationDeclarationAdapter d eoa p =
  record { decl = d ; expectedObjectA = eoa ; link = p ; status = B.true }

isFilledInternalEquivalenceRelationDeclaration : InternalEquivalenceRelationDeclarationAdapter → B.Bool
isFilledInternalEquivalenceRelationDeclaration a = InternalEquivalenceRelationDeclarationAdapter.status a


-- Exact category (regular + effective relations)
record ExactCategoryDeclarationAdapter : Set₁ where
  field
    decl : C2S2.ExactCategoryDeclaration
    expectedRegular : C2S2.RegularCategoryDeclaration
    link : C2S2.ExactCategoryDeclaration.regular decl ≡ expectedRegular
    status : B.Bool

mkExactCategoryDeclarationAdapter :
  (d : C2S2.ExactCategoryDeclaration) →
  (er : C2S2.RegularCategoryDeclaration) →
  (p : C2S2.ExactCategoryDeclaration.regular d ≡ er) →
  ExactCategoryDeclarationAdapter
mkExactCategoryDeclarationAdapter d er p =
  record { decl = d ; expectedRegular = er ; link = p ; status = B.true }

isFilledExactCategoryDeclaration : ExactCategoryDeclarationAdapter → B.Bool
isFilledExactCategoryDeclaration a = ExactCategoryDeclarationAdapter.status a


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
    status : B.Bool

mkMonadWithRankAdapter :
  (d : C2S4.MonadWithRank) →
  (em : C2S4.MonadDeclaration) →
  (ec : C2S4.RegularCardinal) →
  (p1 : C2S4.MonadWithRank.monad d ≡ em) →
  (p2 : C2S4.MonadWithRank.cardinal d ≡ ec) →
  MonadWithRankAdapter
mkMonadWithRankAdapter d em ec p1 p2 =
  record { decl = d ; expectedMonad = em ; expectedCardinal = ec ; link1 = p1 ; link2 = p2 ; status = B.true }

isFilledMonadWithRank : MonadWithRankAdapter → B.Bool
isFilledMonadWithRank a = MonadWithRankAdapter.status a


-- Locally α-presentable category
record LocallyPresentableCategoryAdapter : Set₁ where
  field
    decl : C2S4.LocallyPresentableCategory
    expectedCardinal : C2S4.RegularCardinal
    link : C2S4.LocallyPresentableCategory.cardinal decl ≡ expectedCardinal
    status : B.Bool

mkLocallyPresentableCategoryAdapter :
  (d : C2S4.LocallyPresentableCategory) →
  (ec : C2S4.RegularCardinal) →
  (p : C2S4.LocallyPresentableCategory.cardinal d ≡ ec) →
  LocallyPresentableCategoryAdapter
mkLocallyPresentableCategoryAdapter d ec p =
  record { decl = d ; expectedCardinal = ec ; link = p ; status = B.true }

isFilledLocallyPresentableCategory : LocallyPresentableCategoryAdapter → B.Bool
isFilledLocallyPresentableCategory a = LocallyPresentableCategoryAdapter.status a


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
    status : B.Bool

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
  record { decl = d ; expectedBaseCategory = ebc ; expectedMonadWithRank = emr ; expectedAlgCat = eac ; link1 = p1 ; link2 = p2 ; link3 = p3 ; status = B.true }

isFilledRankTheoremForMonadicCategoriesTheorem : RankTheoremForMonadicCategoriesTheoremAdapter → B.Bool
isFilledRankTheoremForMonadicCategoriesTheorem a = RankTheoremForMonadicCategoriesTheoremAdapter.status a


------------------------------------------------------------------------
-- Functor Properties: Preserve/Reflect/Create Limits (Chapter1.Level1sub2)
------------------------------------------------------------------------

-- Functor preserves limits
record FunctorPreservesLimitsAdapter : Set₁ where
  field
    decl : C1S2.FunctorPreservesLimits
    expectedFunctor : M.Identifier
    link : C1S2.FunctorPreservesLimits.F decl ≡ expectedFunctor
    status : B.Bool

mkFunctorPreservesLimitsAdapter :
  (d : C1S2.FunctorPreservesLimits) →
  (ef : M.Identifier) →
  (p : C1S2.FunctorPreservesLimits.F d ≡ ef) →
  FunctorPreservesLimitsAdapter
mkFunctorPreservesLimitsAdapter d ef p =
  record { decl = d ; expectedFunctor = ef ; link = p ; status = B.true }

isFilledFunctorPreservesLimits : FunctorPreservesLimitsAdapter → B.Bool
isFilledFunctorPreservesLimits a = FunctorPreservesLimitsAdapter.status a


-- Functor reflects limits
record FunctorReflectsLimitsAdapter : Set₁ where
  field
    decl : C1S2.FunctorReflectsLimits
    expectedFunctor : M.Identifier
    link : C1S2.FunctorReflectsLimits.F decl ≡ expectedFunctor
    status : B.Bool

mkFunctorReflectsLimitsAdapter :
  (d : C1S2.FunctorReflectsLimits) →
  (ef : M.Identifier) →
  (p : C1S2.FunctorReflectsLimits.F d ≡ ef) →
  FunctorReflectsLimitsAdapter
mkFunctorReflectsLimitsAdapter d ef p =
  record { decl = d ; expectedFunctor = ef ; link = p ; status = B.true }

isFilledFunctorReflectsLimits : FunctorReflectsLimitsAdapter → B.Bool
isFilledFunctorReflectsLimits a = FunctorReflectsLimitsAdapter.status a


-- Functor creates limits
record FunctorCreatesLimitsAdapter : Set₁ where
  field
    decl : C1S2.FunctorCreatesLimits
    expectedFunctor : M.Identifier
    link : C1S2.FunctorCreatesLimits.F decl ≡ expectedFunctor
    status : B.Bool

mkFunctorCreatesLimitsAdapter :
  (d : C1S2.FunctorCreatesLimits) →
  (ef : M.Identifier) →
  (p : C1S2.FunctorCreatesLimits.F d ≡ ef) →
  FunctorCreatesLimitsAdapter
mkFunctorCreatesLimitsAdapter d ef p =
  record { decl = d ; expectedFunctor = ef ; link = p ; status = B.true }

isFilledFunctorCreatesLimits : FunctorCreatesLimitsAdapter → B.Bool
isFilledFunctorCreatesLimits a = FunctorCreatesLimitsAdapter.status a


-- Theorem: Creation implies reflection
record CreationImpliesReflectionAdapter : Set₁ where
  field
    decl : C1S2.CreationImpliesReflection
    expectedFunctor : M.Identifier
    link : C1S2.CreationImpliesReflection.F decl ≡ expectedFunctor
    status : B.Bool

mkCreationImpliesReflectionAdapter :
  (d : C1S2.CreationImpliesReflection) →
  (ef : M.Identifier) →
  (p : C1S2.CreationImpliesReflection.F d ≡ ef) →
  CreationImpliesReflectionAdapter
mkCreationImpliesReflectionAdapter d ef p =
  record { decl = d ; expectedFunctor = ef ; link = p ; status = B.true }

isFilledCreationImpliesReflection : CreationImpliesReflectionAdapter → B.Bool
isFilledCreationImpliesReflection a = CreationImpliesReflectionAdapter.status a


-- Theorem: Isomorphisms of categories reflect limits
record IsomorphismsOfCategoriesReflectLimitsAdapter : Set₁ where
  field
    decl : C1S2.IsomorphismsOfCategoriesReflectLimits
    expectedFunctor : M.Identifier
    link : C1S2.IsomorphismsOfCategoriesReflectLimits.F decl ≡ expectedFunctor
    status : B.Bool

mkIsomorphismsOfCategoriesReflectLimitsAdapter :
  (d : C1S2.IsomorphismsOfCategoriesReflectLimits) →
  (ef : M.Identifier) →
  (p : C1S2.IsomorphismsOfCategoriesReflectLimits.F d ≡ ef) →
  IsomorphismsOfCategoriesReflectLimitsAdapter
mkIsomorphismsOfCategoriesReflectLimitsAdapter d ef p =
  record { decl = d ; expectedFunctor = ef ; link = p ; status = B.true }

isFilledIsomorphismsOfCategoriesReflectLimits : IsomorphismsOfCategoriesReflectLimitsAdapter → B.Bool
isFilledIsomorphismsOfCategoriesReflectLimits a = IsomorphismsOfCategoriesReflectLimitsAdapter.status a


-- Theorem: Right adjoints preserve limits
record RightAdjointsPreserveLimits_L2Adapter : Set₁ where
  field
    decl : C1S2.RightAdjointsPreserveLimits_L2
    expectedFunctor : M.Identifier
    link : C1S2.RightAdjointsPreserveLimits_L2.F decl ≡ expectedFunctor
    status : B.Bool

mkRightAdjointsPreserveLimits_L2Adapter :
  (d : C1S2.RightAdjointsPreserveLimits_L2) →
  (ef : M.Identifier) →
  (p : C1S2.RightAdjointsPreserveLimits_L2.F d ≡ ef) →
  RightAdjointsPreserveLimits_L2Adapter
mkRightAdjointsPreserveLimits_L2Adapter d ef p =
  record { decl = d ; expectedFunctor = ef ; link = p ; status = B.true }

isFilledRightAdjointsPreserveLimits_L2 : RightAdjointsPreserveLimits_L2Adapter → B.Bool
isFilledRightAdjointsPreserveLimits_L2 a = RightAdjointsPreserveLimits_L2Adapter.status a


------------------------------------------------------------------------
-- Yoneda Lemma (Chapter1.Level1sub8)
------------------------------------------------------------------------

-- Internal Yoneda embedding
record InternalYonedaEmbeddingAdapter : Set₁ where
  field
    decl : C1S8.InternalYonedaEmbedding
    expectedInternalCategory : C1S8.InternalCategory
    link : C1S8.InternalYonedaEmbedding.internalCategory decl ≡ expectedInternalCategory
    status : B.Bool

mkInternalYonedaEmbeddingAdapter :
  (d : C1S8.InternalYonedaEmbedding) →
  (eic : C1S8.InternalCategory) →
  (p : C1S8.InternalYonedaEmbedding.internalCategory d ≡ eic) →
  InternalYonedaEmbeddingAdapter
mkInternalYonedaEmbeddingAdapter d eic p =
  record { decl = d ; expectedInternalCategory = eic ; link = p ; status = B.true }

isFilledInternalYonedaEmbedding : InternalYonedaEmbeddingAdapter → B.Bool
isFilledInternalYonedaEmbedding a = InternalYonedaEmbeddingAdapter.status a


-- Internal Yoneda lemma theorem
record InternalYonedaLemmaAdapter : Set₁ where
  field
    decl : C1S8.InternalYonedaLemma
    expectedInternalCategory : C1S8.InternalCategory
    expectedPresheaf : C1S8.InternalPresheaf
    link1 : C1S8.InternalYonedaLemma.internalCategory decl ≡ expectedInternalCategory
    link2 : C1S8.InternalYonedaLemma.presheaf decl ≡ expectedPresheaf
    status : B.Bool

mkInternalYonedaLemmaAdapter :
  (d : C1S8.InternalYonedaLemma) →
  (eic : C1S8.InternalCategory) →
  (ep : C1S8.InternalPresheaf) →
  (p1 : C1S8.InternalYonedaLemma.internalCategory d ≡ eic) →
  (p2 : C1S8.InternalYonedaLemma.presheaf d ≡ ep) →
  InternalYonedaLemmaAdapter
mkInternalYonedaLemmaAdapter d eic ep p1 p2 =
  record { decl = d ; expectedInternalCategory = eic ; expectedPresheaf = ep ; link1 = p1 ; link2 = p2 ; status = B.true }

isFilledInternalYonedaLemma : InternalYonedaLemmaAdapter → B.Bool
isFilledInternalYonedaLemma a = InternalYonedaLemmaAdapter.status a


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
    status : B.Bool

mkKanExtensionContextAdapter :
  (d : C1S3.KanExtensionContext) →
  (ek : M.Identifier) →
  (et : M.Identifier) →
  (p1 : C1S3.KanExtensionContext.K d ≡ ek) →
  (p2 : C1S3.KanExtensionContext.T d ≡ et) →
  KanExtensionContextAdapter
mkKanExtensionContextAdapter d ek et p1 p2 =
  record { decl = d ; expectedK = ek ; expectedT = et ; link1 = p1 ; link2 = p2 ; status = B.true }

isFilledKanExtensionContext : KanExtensionContextAdapter → B.Bool
isFilledKanExtensionContext a = KanExtensionContextAdapter.status a


-- Left Kan candidate
record LeftKanCandidateAdapter : Set₁ where
  field
    decl : C1S3.LeftKanCandidate
    expectedM : M.Identifier
    link : C1S3.LeftKanCandidate.M decl ≡ expectedM
    status : B.Bool

mkLeftKanCandidateAdapter :
  (d : C1S3.LeftKanCandidate) →
  (em : M.Identifier) →
  (p : C1S3.LeftKanCandidate.M d ≡ em) →
  LeftKanCandidateAdapter
mkLeftKanCandidateAdapter d em p =
  record { decl = d ; expectedM = em ; link = p ; status = B.true }

isFilledLeftKanCandidate : LeftKanCandidateAdapter → B.Bool
isFilledLeftKanCandidate a = LeftKanCandidateAdapter.status a


-- Right Kan candidate
record RightKanCandidateAdapter : Set₁ where
  field
    decl : C1S3.RightKanCandidate
    expectedM : M.Identifier
    link : C1S3.RightKanCandidate.M decl ≡ expectedM
    status : B.Bool

mkRightKanCandidateAdapter :
  (d : C1S3.RightKanCandidate) →
  (em : M.Identifier) →
  (p : C1S3.RightKanCandidate.M d ≡ em) →
  RightKanCandidateAdapter
mkRightKanCandidateAdapter d em p =
  record { decl = d ; expectedM = em ; link = p ; status = B.true }

isFilledRightKanCandidate : RightKanCandidateAdapter → B.Bool
isFilledRightKanCandidate a = RightKanCandidateAdapter.status a


-- Theorem: Left Kan extension is initial object
record LeftKanExtensionIsInitialObjectAdapter : Set₁ where
  field
    decl : C1S3.LeftKanExtensionIsInitialObject
    expectedK : M.Identifier
    expectedT : M.Identifier
    link1 : C1S3.LeftKanExtensionIsInitialObject.K decl ≡ expectedK
    link2 : C1S3.LeftKanExtensionIsInitialObject.T decl ≡ expectedT
    status : B.Bool

mkLeftKanExtensionIsInitialObjectAdapter :
  (d : C1S3.LeftKanExtensionIsInitialObject) →
  (ek : M.Identifier) →
  (et : M.Identifier) →
  (p1 : C1S3.LeftKanExtensionIsInitialObject.K d ≡ ek) →
  (p2 : C1S3.LeftKanExtensionIsInitialObject.T d ≡ et) →
  LeftKanExtensionIsInitialObjectAdapter
mkLeftKanExtensionIsInitialObjectAdapter d ek et p1 p2 =
  record { decl = d ; expectedK = ek ; expectedT = et ; link1 = p1 ; link2 = p2 ; status = B.true }

isFilledLeftKanExtensionIsInitialObject : LeftKanExtensionIsInitialObjectAdapter → B.Bool
isFilledLeftKanExtensionIsInitialObject a = LeftKanExtensionIsInitialObjectAdapter.status a


-- Theorem: Right Kan extension is terminal object
record RightKanExtensionIsTerminalObjectAdapter : Set₁ where
  field
    decl : C1S3.RightKanExtensionIsTerminalObject
    expectedK : M.Identifier
    expectedT : M.Identifier
    link1 : C1S3.RightKanExtensionIsTerminalObject.K decl ≡ expectedK
    link2 : C1S3.RightKanExtensionIsTerminalObject.T decl ≡ expectedT
    status : B.Bool

mkRightKanExtensionIsTerminalObjectAdapter :
  (d : C1S3.RightKanExtensionIsTerminalObject) →
  (ek : M.Identifier) →
  (et : M.Identifier) →
  (p1 : C1S3.RightKanExtensionIsTerminalObject.K d ≡ ek) →
  (p2 : C1S3.RightKanExtensionIsTerminalObject.T d ≡ et) →
  RightKanExtensionIsTerminalObjectAdapter
mkRightKanExtensionIsTerminalObjectAdapter d ek et p1 p2 =
  record { decl = d ; expectedK = ek ; expectedT = et ; link1 = p1 ; link2 = p2 ; status = B.true }

isFilledRightKanExtensionIsTerminalObject : RightKanExtensionIsTerminalObjectAdapter → B.Bool
isFilledRightKanExtensionIsTerminalObject a = RightKanExtensionIsTerminalObjectAdapter.status a


-- Theorem: Pointwise Kan formula
record PointwiseKanFormulaTheoremAdapter : Set₁ where
  field
    decl : C1S3.PointwiseKanFormulaTheorem
    expectedK : M.Identifier
    expectedT : M.Identifier
    link1 : C1S3.PointwiseKanFormulaTheorem.K decl ≡ expectedK
    link2 : C1S3.PointwiseKanFormulaTheorem.T decl ≡ expectedT
    status : B.Bool

mkPointwiseKanFormulaTheoremAdapter :
  (d : C1S3.PointwiseKanFormulaTheorem) →
  (ek : M.Identifier) →
  (et : M.Identifier) →
  (p1 : C1S3.PointwiseKanFormulaTheorem.K d ≡ ek) →
  (p2 : C1S3.PointwiseKanFormulaTheorem.T d ≡ et) →
  PointwiseKanFormulaTheoremAdapter
mkPointwiseKanFormulaTheoremAdapter d ek et p1 p2 =
  record { decl = d ; expectedK = ek ; expectedT = et ; link1 = p1 ; link2 = p2 ; status = B.true }

isFilledPointwiseKanFormulaTheorem : PointwiseKanFormulaTheoremAdapter → B.Bool
isFilledPointwiseKanFormulaTheorem a = PointwiseKanFormulaTheoremAdapter.status a


-- Theorem: Adjoints as Kan extensions
record AdjointsAsKanExtensionsAdapter : Set₁ where
  field
    decl : C1S3.AdjointsAsKanExtensions
    expectedF : M.Identifier
    expectedG : M.Identifier
    link1 : C1S3.AdjointsAsKanExtensions.F decl ≡ expectedF
    link2 : C1S3.AdjointsAsKanExtensions.G decl ≡ expectedG
    status : B.Bool

mkAdjointsAsKanExtensionsAdapter :
  (d : C1S3.AdjointsAsKanExtensions) →
  (ef : M.Identifier) →
  (eg : M.Identifier) →
  (p1 : C1S3.AdjointsAsKanExtensions.F d ≡ ef) →
  (p2 : C1S3.AdjointsAsKanExtensions.G d ≡ eg) →
  AdjointsAsKanExtensionsAdapter
mkAdjointsAsKanExtensionsAdapter d ef eg p1 p2 =
  record { decl = d ; expectedF = ef ; expectedG = eg ; link1 = p1 ; link2 = p2 ; status = B.true }

isFilledAdjointsAsKanExtensions : AdjointsAsKanExtensionsAdapter → B.Bool
isFilledAdjointsAsKanExtensions a = AdjointsAsKanExtensionsAdapter.status a


------------------------------------------------------------------------
-- Adjoint Functor Theorems (Chapter1.Level1sub3)
------------------------------------------------------------------------

-- Adjoint functor theorem (right version - dual theorem)
record AdjointFunctorTheoremRightAdapter : Set₁ where
  field
    decl : C1S3.AdjointFunctorTheoremRight
    status : B.Bool

mkAdjointFunctorTheoremRightAdapter :
  (d : C1S3.AdjointFunctorTheoremRight) →
  AdjointFunctorTheoremRightAdapter
mkAdjointFunctorTheoremRightAdapter d =
  record { decl = d ; status = B.true }

isFilledAdjointFunctorTheoremRight : AdjointFunctorTheoremRightAdapter → B.Bool
isFilledAdjointFunctorTheoremRight a = AdjointFunctorTheoremRightAdapter.status a


------------------------------------------------------------------------
-- Grothendieck Fibrations (Chapter2.Level2sub8)
------------------------------------------------------------------------

-- Fibration declaration (projection functor with Cartesian lifts)
record FibrationDeclarationAdapter : Set₁ where
  field
    decl : C2S8.FibrationDeclaration
    expectedProjection : M.Identifier
    link : C2S8.FibrationProjectionFunctor.projectionFunctor (C2S8.FibrationDeclaration.projectionFunctor decl) ≡ expectedProjection
    status : B.Bool

mkFibrationDeclarationAdapter :
  (d : C2S8.FibrationDeclaration) →
  (ep : M.Identifier) →
  (p : C2S8.FibrationProjectionFunctor.projectionFunctor (C2S8.FibrationDeclaration.projectionFunctor d) ≡ ep) →
  FibrationDeclarationAdapter
mkFibrationDeclarationAdapter d ep p =
  record { decl = d ; expectedProjection = ep ; link = p ; status = B.true }

isFilledFibrationDeclaration : FibrationDeclarationAdapter → B.Bool
isFilledFibrationDeclaration a = FibrationDeclarationAdapter.status a

-- Cartesian arrow (universal lifting property)
record CartesianArrowAdapter : Set₁ where
  field
    decl : C2S8.CartesianArrow
    expectedArrow : M.Identifier
    link : C2S8.CartesianArrow.arrow decl ≡ expectedArrow
    status : B.Bool

mkCartesianArrowAdapter :
  (d : C2S8.CartesianArrow) →
  (ea : M.Identifier) →
  (p : C2S8.CartesianArrow.arrow d ≡ ea) →
  CartesianArrowAdapter
mkCartesianArrowAdapter d ea p =
  record { decl = d ; expectedArrow = ea ; link = p ; status = B.true }

isFilledCartesianArrow : CartesianArrowAdapter → B.Bool
isFilledCartesianArrow a = CartesianArrowAdapter.status a

-- Cartesian functor between fibrations
record CartesianFunctorDeclarationAdapter : Set₁ where
  field
    decl : C2S8.CartesianFunctorDeclaration
    expectedFunctor : M.Identifier
    link : C2S8.CartesianFunctorDeclaration.underlyingFunctor decl ≡ expectedFunctor
    status : B.Bool

mkCartesianFunctorDeclarationAdapter :
  (d : C2S8.CartesianFunctorDeclaration) →
  (ef : M.Identifier) →
  (p : C2S8.CartesianFunctorDeclaration.underlyingFunctor d ≡ ef) →
  CartesianFunctorDeclarationAdapter
mkCartesianFunctorDeclarationAdapter d ef p =
  record { decl = d ; expectedFunctor = ef ; link = p ; status = B.true }

isFilledCartesianFunctorDeclaration : CartesianFunctorDeclarationAdapter → B.Bool
isFilledCartesianFunctorDeclaration a = CartesianFunctorDeclarationAdapter.status a

-- Category of fibrations over a base
record CategoryOfFibrationsAdapter : Set₁ where
  field
    decl : C2S8.CategoryOfFibrations
    expectedBase : M.Identifier
    link : C2S8.CategoryOfFibrations.baseCategory decl ≡ expectedBase
    status : B.Bool

mkCategoryOfFibrationsAdapter :
  (d : C2S8.CategoryOfFibrations) →
  (eb : M.Identifier) →
  (p : C2S8.CategoryOfFibrations.baseCategory d ≡ eb) →
  CategoryOfFibrationsAdapter
mkCategoryOfFibrationsAdapter d eb p =
  record { decl = d ; expectedBase = eb ; link = p ; status = B.true }

isFilledCategoryOfFibrations : CategoryOfFibrationsAdapter → B.Bool
isFilledCategoryOfFibrations a = CategoryOfFibrationsAdapter.status a

-- Pseudofunctor from fibration (unpacking)
record PseudofunctorFromFibrationAdapter : Set₁ where
  field
    decl : C2S8.PseudofunctorFromFibration
    status : B.Bool

mkPseudofunctorFromFibrationAdapter :
  (d : C2S8.PseudofunctorFromFibration) →
  PseudofunctorFromFibrationAdapter
mkPseudofunctorFromFibrationAdapter d =
  record { decl = d ; status = B.true }

isFilledPseudofunctorFromFibration : PseudofunctorFromFibrationAdapter → B.Bool
isFilledPseudofunctorFromFibration a = PseudofunctorFromFibrationAdapter.status a

-- Grothendieck construction (category of elements)
record GrothendieckConstructionAdapter : Set₁ where
  field
    decl : C2S8.GrothendieckConstruction
    expectedTotal : M.Identifier
    link : C2S8.GrothendieckConstruction.totalCategory decl ≡ expectedTotal
    status : B.Bool

mkGrothendieckConstructionAdapter :
  (d : C2S8.GrothendieckConstruction) →
  (et : M.Identifier) →
  (p : C2S8.GrothendieckConstruction.totalCategory d ≡ et) →
  GrothendieckConstructionAdapter
mkGrothendieckConstructionAdapter d et p =
  record { decl = d ; expectedTotal = et ; link = p ; status = B.true }

isFilledGrothendieckConstruction : GrothendieckConstructionAdapter → B.Bool
isFilledGrothendieckConstruction a = GrothendieckConstructionAdapter.status a

-- Grothendieck equivalence theorem (2-equivalence)
record GrothendieckEquivalenceTheoremAdapter : Set₁ where
  field
    decl : C2S8.GrothendieckEquivalenceTheorem
    expectedBase : M.Identifier
    link : C2S8.GrothendieckEquivalenceTheorem.baseCategory decl ≡ expectedBase
    status : B.Bool

mkGrothendieckEquivalenceTheoremAdapter :
  (d : C2S8.GrothendieckEquivalenceTheorem) →
  (eb : M.Identifier) →
  (p : C2S8.GrothendieckEquivalenceTheorem.baseCategory d ≡ eb) →
  GrothendieckEquivalenceTheoremAdapter
mkGrothendieckEquivalenceTheoremAdapter d eb p =
  record { decl = d ; expectedBase = eb ; link = p ; status = B.true }

isFilledGrothendieckEquivalenceTheorem : GrothendieckEquivalenceTheoremAdapter → B.Bool
isFilledGrothendieckEquivalenceTheorem a = GrothendieckEquivalenceTheoremAdapter.status a

-- Fibred adjunction (pointwise on fibres)
record FibredAdjunctionDeclarationAdapter : Set₁ where
  field
    decl : C2S8.FibredAdjunctionDeclaration
    expectedLeft : M.Identifier
    expectedRight : M.Identifier
    link1 : C2S8.FibredAdjunctionDeclaration.leftAdjoint decl ≡ expectedLeft
    link2 : C2S8.FibredAdjunctionDeclaration.rightAdjoint decl ≡ expectedRight
    status : B.Bool

mkFibredAdjunctionDeclarationAdapter :
  (d : C2S8.FibredAdjunctionDeclaration) →
  (el : M.Identifier) →
  (er : M.Identifier) →
  (p1 : C2S8.FibredAdjunctionDeclaration.leftAdjoint d ≡ el) →
  (p2 : C2S8.FibredAdjunctionDeclaration.rightAdjoint d ≡ er) →
  FibredAdjunctionDeclarationAdapter
mkFibredAdjunctionDeclarationAdapter d el er p1 p2 =
  record { decl = d ; expectedLeft = el ; expectedRight = er ; link1 = p1 ; link2 = p2 ; status = B.true }

isFilledFibredAdjunctionDeclaration : FibredAdjunctionDeclarationAdapter → B.Bool
isFilledFibredAdjunctionDeclaration a = FibredAdjunctionDeclarationAdapter.status a

-- Beck-Chevalley condition (coherence for fibred adjunctions)
record BeckChevalleyConditionAdapter : Set₁ where
  field
    decl : C2S8.BeckChevalleyCondition
    status : B.Bool

mkBeckChevalleyConditionAdapter :
  (d : C2S8.BeckChevalleyCondition) →
  BeckChevalleyConditionAdapter
mkBeckChevalleyConditionAdapter d =
  record { decl = d ; status = B.true }

isFilledBeckChevalleyCondition : BeckChevalleyConditionAdapter → B.Bool
isFilledBeckChevalleyCondition a = BeckChevalleyConditionAdapter.status a

-- Fibration completeness criterion theorem
record FibrationCompletenessCriterionTheoremAdapter : Set₁ where
  field
    decl : C2S8.FibrationCompletenessCriterionTheorem
    status : B.Bool

mkFibrationCompletenessCriterionTheoremAdapter :
  (d : C2S8.FibrationCompletenessCriterionTheorem) →
  FibrationCompletenessCriterionTheoremAdapter
mkFibrationCompletenessCriterionTheoremAdapter d =
  record { decl = d ; status = B.true }

isFilledFibrationCompletenessCriterionTheorem : FibrationCompletenessCriterionTheoremAdapter → B.Bool
isFilledFibrationCompletenessCriterionTheorem a = FibrationCompletenessCriterionTheoremAdapter.status a

-- Locally small fibration
record LocallySmallFibrationAdapter : Set₁ where
  field
    decl : C2S8.LocallySmallFibration
    status : B.Bool

mkLocallySmallFibrationAdapter :
  (d : C2S8.LocallySmallFibration) →
  LocallySmallFibrationAdapter
mkLocallySmallFibrationAdapter d =
  record { decl = d ; status = B.true }

isFilledLocallySmallFibration : LocallySmallFibrationAdapter → B.Bool
isFilledLocallySmallFibration a = LocallySmallFibrationAdapter.status a

-- Refined Grothendieck equivalence (for locally small fibrations)
record RefinedGrothendieckEquivalenceTheoremAdapter : Set₁ where
  field
    decl : C2S8.RefinedGrothendieckEquivalenceTheorem
    expectedBase : M.Identifier
    link : C2S8.RefinedGrothendieckEquivalenceTheorem.baseCategory decl ≡ expectedBase
    status : B.Bool

mkRefinedGrothendieckEquivalenceTheoremAdapter :
  (d : C2S8.RefinedGrothendieckEquivalenceTheorem) →
  (eb : M.Identifier) →
  (p : C2S8.RefinedGrothendieckEquivalenceTheorem.baseCategory d ≡ eb) →
  RefinedGrothendieckEquivalenceTheoremAdapter
mkRefinedGrothendieckEquivalenceTheoremAdapter d eb p =
  record { decl = d ; expectedBase = eb ; link = p ; status = B.true }

isFilledRefinedGrothendieckEquivalenceTheorem : RefinedGrothendieckEquivalenceTheoremAdapter → B.Bool
isFilledRefinedGrothendieckEquivalenceTheorem a = RefinedGrothendieckEquivalenceTheoremAdapter.status a

-- Codomain fibration (canonical example)
record CodomainFibrationAdapter : Set₁ where
  field
    decl : C2S8.CodomainFibration
    expectedBase : M.Identifier
    link : C2S8.CodomainFibration.baseCategory decl ≡ expectedBase
    status : B.Bool

mkCodomainFibrationAdapter :
  (d : C2S8.CodomainFibration) →
  (eb : M.Identifier) →
  (p : C2S8.CodomainFibration.baseCategory d ≡ eb) →
  CodomainFibrationAdapter
mkCodomainFibrationAdapter d eb p =
  record { decl = d ; expectedBase = eb ; link = p ; status = B.true }

isFilledCodomainFibration : CodomainFibrationAdapter → B.Bool
isFilledCodomainFibration a = CodomainFibrationAdapter.status a

-- Lindenbaum-Tarski fibration (logic connection)
record LindenbaumTarskiFibrationAdapter : Set₁ where
  field
    decl : C2S8.LindenbaumTarskiFibration
    status : B.Bool

mkLindenbaumTarskiFibrationAdapter :
  (d : C2S8.LindenbaumTarskiFibration) →
  LindenbaumTarskiFibrationAdapter
mkLindenbaumTarskiFibrationAdapter d =
  record { decl = d ; status = B.true }

isFilledLindenbaumTarskiFibration : LindenbaumTarskiFibrationAdapter → B.Bool
isFilledLindenbaumTarskiFibration a = LindenbaumTarskiFibrationAdapter.status a

-- Families fibration (indexed sets)
record FamiliesFibrationAdapter : Set₁ where
  field
    decl : C2S8.FamiliesFibration
    expectedBase : M.Identifier
    link : C2S8.FamiliesFibration.baseCategory decl ≡ expectedBase
    status : B.Bool

mkFamiliesFibrationAdapter :
  (d : C2S8.FamiliesFibration) →
  (eb : M.Identifier) →
  (p : C2S8.FamiliesFibration.baseCategory d ≡ eb) →
  FamiliesFibrationAdapter
mkFamiliesFibrationAdapter d eb p =
  record { decl = d ; expectedBase = eb ; link = p ; status = B.true }

isFilledFamiliesFibration : FamiliesFibrationAdapter → B.Bool
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
    status : B.Bool

mkHasZeroObjectPropertyAdapter :
  (d : C2S1.HasZeroObjectProperty) →
  (ec : M.Identifier) →
  (ez : M.Identifier) →
  (p1 : C2S1.HasZeroObjectProperty.category d ≡ ec) →
  (p2 : C2S1.HasZeroObjectProperty.zeroObj d ≡ ez) →
  HasZeroObjectPropertyAdapter
mkHasZeroObjectPropertyAdapter d ec ez p1 p2 =
  record { decl = d ; expectedCategory = ec ; expectedZero = ez ; link1 = p1 ; link2 = p2 ; status = B.true }

isFilledHasZeroObjectProperty : HasZeroObjectPropertyAdapter → B.Bool
isFilledHasZeroObjectProperty a = HasZeroObjectPropertyAdapter.status a

-- Kernel as equalizer definition
record KernelAsEqualizerDefinitionAdapter : Set₁ where
  field
    decl : C2S1.KernelAsEqualizerDefinition
    expectedMorphism : M.Identifier
    expectedKernel : M.Identifier
    link1 : C2S1.KernelAsEqualizerDefinition.morphism decl ≡ expectedMorphism
    link2 : C2S1.KernelAsEqualizerDefinition.equalizerObject decl ≡ expectedKernel
    status : B.Bool

mkKernelAsEqualizerDefinitionAdapter :
  (d : C2S1.KernelAsEqualizerDefinition) →
  (em : M.Identifier) →
  (ek : M.Identifier) →
  (p1 : C2S1.KernelAsEqualizerDefinition.morphism d ≡ em) →
  (p2 : C2S1.KernelAsEqualizerDefinition.equalizerObject d ≡ ek) →
  KernelAsEqualizerDefinitionAdapter
mkKernelAsEqualizerDefinitionAdapter d em ek p1 p2 =
  record { decl = d ; expectedMorphism = em ; expectedKernel = ek ; link1 = p1 ; link2 = p2 ; status = B.true }

isFilledKernelAsEqualizerDefinition : KernelAsEqualizerDefinitionAdapter → B.Bool
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
    status : B.Bool

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
  record { decl = d ; expectedLeft = el ; expectedRight = er ; expectedObject = eo ; link1 = p1 ; link2 = p2 ; link3 = p3 ; status = B.true }

isFilledBiproductObject : BiproductObjectAdapter → B.Bool
isFilledBiproductObject a = BiproductObjectAdapter.status a

-- Additive category declaration
record AdditiveCategoryDeclarationAdapter : Set₁ where
  field
    decl : C2S1.AdditiveCategoryDeclaration
    expectedCategory : M.Identifier
    link : C2S1.AdditiveCategoryDeclaration.category decl ≡ expectedCategory
    status : B.Bool

mkAdditiveCategoryDeclarationAdapter :
  (d : C2S1.AdditiveCategoryDeclaration) →
  (ec : M.Identifier) →
  (p : C2S1.AdditiveCategoryDeclaration.category d ≡ ec) →
  AdditiveCategoryDeclarationAdapter
mkAdditiveCategoryDeclarationAdapter d ec p =
  record { decl = d ; expectedCategory = ec ; link = p ; status = B.true }

isFilledAdditiveCategoryDeclaration : AdditiveCategoryDeclarationAdapter → B.Bool
isFilledAdditiveCategoryDeclaration a = AdditiveCategoryDeclarationAdapter.status a

-- Abelian category declaration (main definition)
record AbelianCategoryDeclarationAdapter : Set₁ where
  field
    decl : C2S1.AbelianCategoryDeclaration
    expectedCategory : M.Identifier
    link : C2S1.AbelianCategoryDeclaration.category decl ≡ expectedCategory
    status : B.Bool

mkAbelianCategoryDeclarationAdapter :
  (d : C2S1.AbelianCategoryDeclaration) →
  (ec : M.Identifier) →
  (p : C2S1.AbelianCategoryDeclaration.category d ≡ ec) →
  AbelianCategoryDeclarationAdapter
mkAbelianCategoryDeclarationAdapter d ec p =
  record { decl = d ; expectedCategory = ec ; link = p ; status = B.true }

isFilledAbelianCategoryDeclaration : AbelianCategoryDeclarationAdapter → B.Bool
isFilledAbelianCategoryDeclaration a = AbelianCategoryDeclarationAdapter.status a

-- First isomorphism theorem for abelian categories
record FirstIsomorphismForAbelianCategoriesTheoremAdapter : Set₁ where
  field
    decl : C2S1.FirstIsomorphismForAbelianCategoriesTheorem
    expectedCategory : M.Identifier
    expectedMorphism : M.Identifier
    link1 : C2S1.FirstIsomorphismForAbelianCategoriesTheorem.category decl ≡ expectedCategory
    link2 : C2S1.FirstIsomorphismForAbelianCategoriesTheorem.morphism decl ≡ expectedMorphism
    status : B.Bool

mkFirstIsomorphismForAbelianCategoriesTheoremAdapter :
  (d : C2S1.FirstIsomorphismForAbelianCategoriesTheorem) →
  (ec : M.Identifier) →
  (em : M.Identifier) →
  (p1 : C2S1.FirstIsomorphismForAbelianCategoriesTheorem.category d ≡ ec) →
  (p2 : C2S1.FirstIsomorphismForAbelianCategoriesTheorem.morphism d ≡ em) →
  FirstIsomorphismForAbelianCategoriesTheoremAdapter
mkFirstIsomorphismForAbelianCategoriesTheoremAdapter d ec em p1 p2 =
  record { decl = d ; expectedCategory = ec ; expectedMorphism = em ; link1 = p1 ; link2 = p2 ; status = B.true }

isFilledFirstIsomorphismForAbelianCategoriesTheorem : FirstIsomorphismForAbelianCategoriesTheoremAdapter → B.Bool
isFilledFirstIsomorphismForAbelianCategoriesTheorem a = FirstIsomorphismForAbelianCategoriesTheoremAdapter.status a

-- Normal monomorphism property
record NormalMonomorphismPropertyAdapter : Set₁ where
  field
    decl : C2S1.NormalMonomorphismProperty
    expectedMono : M.Identifier
    link : C2S1.NormalMonomorphismProperty.mono decl ≡ expectedMono
    status : B.Bool

mkNormalMonomorphismPropertyAdapter :
  (d : C2S1.NormalMonomorphismProperty) →
  (em : M.Identifier) →
  (p : C2S1.NormalMonomorphismProperty.mono d ≡ em) →
  NormalMonomorphismPropertyAdapter
mkNormalMonomorphismPropertyAdapter d em p =
  record { decl = d ; expectedMono = em ; link = p ; status = B.true }

isFilledNormalMonomorphismProperty : NormalMonomorphismPropertyAdapter → B.Bool
isFilledNormalMonomorphismProperty a = NormalMonomorphismPropertyAdapter.status a

-- Abelian category example: Ab
record AbelianCategoryExampleAbAdapter : Set₁ where
  field
    decl : C2S1.AbelianCategoryExampleAb
    status : B.Bool

mkAbelianCategoryExampleAbAdapter :
  (d : C2S1.AbelianCategoryExampleAb) →
  AbelianCategoryExampleAbAdapter
mkAbelianCategoryExampleAbAdapter d =
  record { decl = d ; status = B.true }

isFilledAbelianCategoryExampleAb : AbelianCategoryExampleAbAdapter → B.Bool
isFilledAbelianCategoryExampleAb a = AbelianCategoryExampleAbAdapter.status a

-- Abelian category example: R-Mod
record AbelianCategoryExampleRModAdapter : Set₁ where
  field
    decl : C2S1.AbelianCategoryExampleRMod
    expectedRing : M.Identifier
    link : C2S1.AbelianCategoryExampleRMod.ring decl ≡ expectedRing
    status : B.Bool

mkAbelianCategoryExampleRModAdapter :
  (d : C2S1.AbelianCategoryExampleRMod) →
  (er : M.Identifier) →
  (p : C2S1.AbelianCategoryExampleRMod.ring d ≡ er) →
  AbelianCategoryExampleRModAdapter
mkAbelianCategoryExampleRModAdapter d er p =
  record { decl = d ; expectedRing = er ; link = p ; status = B.true }

isFilledAbelianCategoryExampleRMod : AbelianCategoryExampleRModAdapter → B.Bool
isFilledAbelianCategoryExampleRMod a = AbelianCategoryExampleRModAdapter.status a

-- Functor additive property
record FunctorAdditivePropertyAdapter : Set₁ where
  field
    decl : C2S1.FunctorAdditiveProperty
    expectedFunctor : M.Identifier
    link : C2S1.FunctorAdditiveProperty.functor decl ≡ expectedFunctor
    status : B.Bool

mkFunctorAdditivePropertyAdapter :
  (d : C2S1.FunctorAdditiveProperty) →
  (ef : M.Identifier) →
  (p : C2S1.FunctorAdditiveProperty.functor d ≡ ef) →
  FunctorAdditivePropertyAdapter
mkFunctorAdditivePropertyAdapter d ef p =
  record { decl = d ; expectedFunctor = ef ; link = p ; status = B.true }

isFilledFunctorAdditiveProperty : FunctorAdditivePropertyAdapter → B.Bool
isFilledFunctorAdditiveProperty a = FunctorAdditivePropertyAdapter.status a

-- Additivity via biproduct coincidence theorem
record AdditivityViaBiproductCoincidenceTheoremAdapter : Set₁ where
  field
    decl : C2S1.AdditivityViaBiproductCoincidenceTheorem
    expectedCategory : M.Identifier
    link : C2S1.AdditivityViaBiproductCoincidenceTheorem.category decl ≡ expectedCategory
    status : B.Bool

mkAdditivityViaBiproductCoincidenceTheoremAdapter :
  (d : C2S1.AdditivityViaBiproductCoincidenceTheorem) →
  (ec : M.Identifier) →
  (p : C2S1.AdditivityViaBiproductCoincidenceTheorem.category d ≡ ec) →
  AdditivityViaBiproductCoincidenceTheoremAdapter
mkAdditivityViaBiproductCoincidenceTheoremAdapter d ec p =
  record { decl = d ; expectedCategory = ec ; link = p ; status = B.true }

isFilledAdditivityViaBiproductCoincidenceTheorem : AdditivityViaBiproductCoincidenceTheoremAdapter → B.Bool
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
    status : B.Bool

mkSubobjectLatticeAdapter :
  (d : C1S4.SubobjectLattice) →
  (ex : M.Identifier) →
  (p : C1S4.SubobjectLattice.X d ≡ ex) →
  SubobjectLatticeAdapter
mkSubobjectLatticeAdapter d ex p =
  record { decl = d ; expectedX = ex ; link = p ; status = B.true }

isFilledSubobjectLattice : SubobjectLatticeAdapter → B.Bool
isFilledSubobjectLattice a = SubobjectLatticeAdapter.status a

-- Well-powered category
record WellPoweredCategoryAdapter : Set₁ where
  field
    decl : C1S4.WellPoweredCategory
    expectedC : M.Identifier
    link : C1S4.WellPoweredCategory.C decl ≡ expectedC
    status : B.Bool

mkWellPoweredCategoryAdapter :
  (d : C1S4.WellPoweredCategory) →
  (ec : M.Identifier) →
  (p : C1S4.WellPoweredCategory.C d ≡ ec) →
  WellPoweredCategoryAdapter
mkWellPoweredCategoryAdapter d ec p =
  record { decl = d ; expectedC = ec ; link = p ; status = B.true }

isFilledWellPoweredCategory : WellPoweredCategoryAdapter → B.Bool
isFilledWellPoweredCategory a = WellPoweredCategoryAdapter.status a

-- Subobject lattice is complete theorem
record SubobjectLatticeIsCompleteAdapter : Set₁ where
  field
    decl : C1S4.SubobjectLatticeIsComplete
    status : B.Bool

mkSubobjectLatticeIsCompleteAdapter :
  (d : C1S4.SubobjectLatticeIsComplete) →
  SubobjectLatticeIsCompleteAdapter
mkSubobjectLatticeIsCompleteAdapter d =
  record { decl = d ; status = B.true }

isFilledSubobjectLatticeIsComplete : SubobjectLatticeIsCompleteAdapter → B.Bool
isFilledSubobjectLatticeIsComplete a = SubobjectLatticeIsCompleteAdapter.status a

-- Strong epimorphism (orthogonal to monomorphisms)
record StrongEpimorphismAdapter : Set₁ where
  field
    decl : C1S4.StrongEpimorphism
    expectedE : M.Identifier
    link : C1S4.StrongEpimorphism.e decl ≡ expectedE
    status : B.Bool

mkStrongEpimorphismAdapter :
  (d : C1S4.StrongEpimorphism) →
  (ee : M.Identifier) →
  (p : C1S4.StrongEpimorphism.e d ≡ ee) →
  StrongEpimorphismAdapter
mkStrongEpimorphismAdapter d ee p =
  record { decl = d ; expectedE = ee ; link = p ; status = B.true }

isFilledStrongEpimorphism : StrongEpimorphismAdapter → B.Bool
isFilledStrongEpimorphism a = StrongEpimorphismAdapter.status a

-- Canonical factorization system theorem
record CanonicalFactorizationSystemAdapter : Set₁ where
  field
    decl : C1S4.CanonicalFactorizationSystem
    status : B.Bool

mkCanonicalFactorizationSystemAdapter :
  (d : C1S4.CanonicalFactorizationSystem) →
  CanonicalFactorizationSystemAdapter
mkCanonicalFactorizationSystemAdapter d =
  record { decl = d ; status = B.true }

isFilledCanonicalFactorizationSystem : CanonicalFactorizationSystemAdapter → B.Bool
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
    status : B.Bool

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
  record { decl = d ; expectedF = ef ; expectedE = ee ; expectedM = em ; link1 = p1 ; link2 = p2 ; link3 = p3 ; status = B.true }

isFilledMorphismFactorization : MorphismFactorizationAdapter → B.Bool
isFilledMorphismFactorization a = MorphismFactorizationAdapter.status a

-- Has generator object
record HasGeneratorObjectAdapter : Set₁ where
  field
    decl : C1S4.HasGeneratorObject
    expectedC : M.Identifier
    expectedG : M.Identifier
    link1 : C1S4.HasGeneratorObject.C decl ≡ expectedC
    link2 : C1S4.HasGeneratorObject.G decl ≡ expectedG
    status : B.Bool

mkHasGeneratorObjectAdapter :
  (d : C1S4.HasGeneratorObject) →
  (ec : M.Identifier) →
  (eg : M.Identifier) →
  (p1 : C1S4.HasGeneratorObject.C d ≡ ec) →
  (p2 : C1S4.HasGeneratorObject.G d ≡ eg) →
  HasGeneratorObjectAdapter
mkHasGeneratorObjectAdapter d ec eg p1 p2 =
  record { decl = d ; expectedC = ec ; expectedG = eg ; link1 = p1 ; link2 = p2 ; status = B.true }

isFilledHasGeneratorObject : HasGeneratorObjectAdapter → B.Bool
isFilledHasGeneratorObject a = HasGeneratorObjectAdapter.status a

-- Projective object
record ProjectiveObjectAdapter : Set₁ where
  field
    decl : C1S4.ProjectiveObject
    expectedP : M.Identifier
    link : C1S4.ProjectiveObject.P decl ≡ expectedP
    status : B.Bool

mkProjectiveObjectAdapter :
  (d : C1S4.ProjectiveObject) →
  (ep : M.Identifier) →
  (p : C1S4.ProjectiveObject.P d ≡ ep) →
  ProjectiveObjectAdapter
mkProjectiveObjectAdapter d ep p =
  record { decl = d ; expectedP = ep ; link = p ; status = B.true }

isFilledProjectiveObject : ProjectiveObjectAdapter → B.Bool
isFilledProjectiveObject a = ProjectiveObjectAdapter.status a

-- Injective object (dual to projective)
record InjectiveObjectAdapter : Set₁ where
  field
    decl : C1S4.InjectiveObject
    expectedI : M.Identifier
    link : C1S4.InjectiveObject.I decl ≡ expectedI
    status : B.Bool

mkInjectiveObjectAdapter :
  (d : C1S4.InjectiveObject) →
  (ei : M.Identifier) →
  (p : C1S4.InjectiveObject.I d ≡ ei) →
  InjectiveObjectAdapter
mkInjectiveObjectAdapter d ei p =
  record { decl = d ; expectedI = ei ; link = p ; status = B.true }

isFilledInjectiveObject : InjectiveObjectAdapter → B.Bool
isFilledInjectiveObject a = InjectiveObjectAdapter.status a

-- Has enough projectives
record HasEnoughProjectivesAdapter : Set₁ where
  field
    decl : C1S4.HasEnoughProjectives
    expectedC : M.Identifier
    link : C1S4.HasEnoughProjectives.C decl ≡ expectedC
    status : B.Bool

mkHasEnoughProjectivesAdapter :
  (d : C1S4.HasEnoughProjectives) →
  (ec : M.Identifier) →
  (p : C1S4.HasEnoughProjectives.C d ≡ ec) →
  HasEnoughProjectivesAdapter
mkHasEnoughProjectivesAdapter d ec p =
  record { decl = d ; expectedC = ec ; link = p ; status = B.true }

isFilledHasEnoughProjectives : HasEnoughProjectivesAdapter → B.Bool
isFilledHasEnoughProjectives a = HasEnoughProjectivesAdapter.status a

-- Has enough injectives
record HasEnoughInjectivesAdapter : Set₁ where
  field
    decl : C1S4.HasEnoughInjectives
    expectedC : M.Identifier
    link : C1S4.HasEnoughInjectives.C decl ≡ expectedC
    status : B.Bool

mkHasEnoughInjectivesAdapter :
  (d : C1S4.HasEnoughInjectives) →
  (ec : M.Identifier) →
  (p : C1S4.HasEnoughInjectives.C d ≡ ec) →
  HasEnoughInjectivesAdapter
mkHasEnoughInjectivesAdapter d ec p =
  record { decl = d ; expectedC = ec ; link = p ; status = B.true }

isFilledHasEnoughInjectives : HasEnoughInjectivesAdapter → B.Bool
isFilledHasEnoughInjectives a = HasEnoughInjectivesAdapter.status a
