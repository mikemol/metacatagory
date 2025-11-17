-- Tests.ObligationAdapters: Tiny adapters and a common status predicate

module Tests.ObligationAdapters where

import Agda.Builtin.Bool as B
open import Agda.Builtin.Unit using (⊤; tt)
open import Agda.Builtin.Equality using (_≡_)
open import Metamodel as M

-- Chapter 2, Level 2.6 (enriched category fragments)
import Chapter2.Level2sub6 as S6
import Chapter1.Level1 as C1L
import Chapter1.Level1sub4 as C1S4
import Chapter1.Level1sub5 as C1S5
import Chapter2.Level2sub2 as C2S2

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
