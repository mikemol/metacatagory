-- Tests.ObligationAdapters: Tiny adapters and a common status predicate

module Tests.ObligationAdapters where

import Agda.Builtin.Bool as B
open import Agda.Builtin.Unit using (⊤; tt)
open import Agda.Builtin.Equality using (_≡_)
open import Metamodel as M

-- Chapter imports
import Chapter1.Level1 as C1L
import Chapter1.Level1sub3 as C1S3
import Chapter1.Level1sub4 as C1S4
import Chapter1.Level1sub5 as C1S5
import Chapter1.Level1sub6 as C1S6
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
import Algebra.Foundation as AF
import Algebra.Rings.Basic as AR
import Algebra.Modules.Basic as AM
import Algebra.Fields.Advanced as AFA
import Core.UniversalProperties as CUP

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

------------------------------------------------------------------------
-- Algebra Adapters
------------------------------------------------------------------------

-- Magma
record MagmaAdapter : Set₁ where
  field
    decl : AF.MagmaDeclaration
    status : B.Bool

mkMagmaAdapter : AF.MagmaDeclaration → MagmaAdapter
mkMagmaAdapter d = record { decl = d ; status = B.true }

isFilledMagma : MagmaAdapter → B.Bool
isFilledMagma a = MagmaAdapter.status a

-- Semigroup
record SemigroupAdapter : Set₁ where
  field
    decl : AF.SemigroupDeclaration
    expMagma : AF.MagmaDeclaration
    linkMagma : AF.SemigroupDeclaration.underlyingMagma decl ≡ expMagma
    status : B.Bool

mkSemigroupAdapter :
  (d : AF.SemigroupDeclaration) →
  (m : AF.MagmaDeclaration) →
  (pm : AF.SemigroupDeclaration.underlyingMagma d ≡ m) →
  SemigroupAdapter
mkSemigroupAdapter d m pm =
  record { decl = d ; expMagma = m ; linkMagma = pm ; status = B.true }

isFilledSemigroup : SemigroupAdapter → B.Bool
isFilledSemigroup a = SemigroupAdapter.status a

-- Monoid
record MonoidAdapter : Set₁ where
  field
    decl : AF.MonoidDeclaration
    expSemigroup : AF.SemigroupDeclaration
    linkSemigroup : AF.MonoidDeclaration.underlyingSemigroup decl ≡ expSemigroup
    status : B.Bool

mkMonoidAdapter :
  (d : AF.MonoidDeclaration) →
  (s : AF.SemigroupDeclaration) →
  (ps : AF.MonoidDeclaration.underlyingSemigroup d ≡ s) →
  MonoidAdapter
mkMonoidAdapter d s ps =
  record { decl = d ; expSemigroup = s ; linkSemigroup = ps ; status = B.true }

isFilledMonoid : MonoidAdapter → B.Bool
isFilledMonoid a = MonoidAdapter.status a

-- Group
record GroupAdapter : Set₁ where
  field
    decl : AF.GroupDeclaration
    expMonoid : AF.MonoidDeclaration
    linkMonoid : AF.GroupDeclaration.underlyingMonoid decl ≡ expMonoid
    status : B.Bool

mkGroupAdapter :
  (d : AF.GroupDeclaration) →
  (m : AF.MonoidDeclaration) →
  (pm : AF.GroupDeclaration.underlyingMonoid d ≡ m) →
  GroupAdapter
mkGroupAdapter d m pm =
  record { decl = d ; expMonoid = m ; linkMonoid = pm ; status = B.true }

isFilledGroup : GroupAdapter → B.Bool
isFilledGroup a = GroupAdapter.status a

-- AbelianGroup
record AbelianGroupAdapter : Set₁ where
  field
    decl : AF.AbelianGroupDeclaration
    expGroup : AF.GroupDeclaration
    linkGroup : AF.AbelianGroupDeclaration.underlyingGroup decl ≡ expGroup
    status : B.Bool

mkAbelianGroupAdapter :
  (d : AF.AbelianGroupDeclaration) →
  (g : AF.GroupDeclaration) →
  (pg : AF.AbelianGroupDeclaration.underlyingGroup d ≡ g) →
  AbelianGroupAdapter
mkAbelianGroupAdapter d g pg =
  record { decl = d ; expGroup = g ; linkGroup = pg ; status = B.true }

isFilledAbelianGroup : AbelianGroupAdapter → B.Bool
isFilledAbelianGroup a = AbelianGroupAdapter.status a

-- Ring
record RingAdapter : Set₁ where
  field
    decl : AR.RingDeclaration
    expAdditiveGroup : AF.AbelianGroupDeclaration
    linkAdditiveGroup : AR.RingDeclaration.additiveGroup decl ≡ expAdditiveGroup
    status : B.Bool

mkRingAdapter :
  (d : AR.RingDeclaration) →
  (ag : AF.AbelianGroupDeclaration) →
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
