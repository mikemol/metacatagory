{-# OPTIONS --without-K #-}

-- | Chapter 1 backbone: canonical axiom/subject identifiers that all
--   subchapters reuse when staging categorical proofs (products,
--   coproducts, adjunctions, exactness, torsion theory, etc.).
module Chapter1.Level1 where

open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Unit     using (⊤; tt)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Agda.Builtin.String using (String)

open import Metamodel as M
open import PropertyRegistry as P
open import Core.Phase using (_×_; _,_) public

-- | Canonical registry of axiom identifiers reused across Chapter 1.
data AxiomName : Set where
  IdentityAxiomName         : AxiomName
  AssociativityAxiomName    : AxiomName
  PreservesCompositionName  : AxiomName
  PreservesIdentityName     : AxiomName
  NaturalityName            : AxiomName
  -- Level 2 (products) axioms
  ProductCommutativityName  : AxiomName
  ProductUniquenessName     : AxiomName
  -- Level 2 (coproducts) axioms
  CoproductCommutativityName : AxiomName
  CoproductUniquenessName    : AxiomName
  -- Level 2 (initial/terminal) axioms
  InitialUniquenessName     : AxiomName
  TerminalUniquenessName    : AxiomName
  -- Level 3 (adjunctions) axioms
  TriangleIdentitiesName    : AxiomName
  RightAdjPreservesLimitsName : AxiomName
  LeftAdjPreservesColimitsName : AxiomName
  -- Level 4 (generators/projectives/factorization) axioms
  FactorizationUniquenessName : AxiomName
  SubobjectLatticeCompletenessName : AxiomName
  CanonicalFactorizationSystemName : AxiomName
  ProjectiveLiftingName : AxiomName
  InjectiveLiftingName : AxiomName
  -- Level 7 (2-categories, lax functors, limit hierarchy)
  TwoCatAssociativityName : AxiomName
  TwoCatUnitalityName     : AxiomName
  LaxAssociativityName    : AxiomName
  LaxUnitalityName        : AxiomName
  LimitHierarchyName      : AxiomName
  CauchyViaDistributorsName : AxiomName
  -- Level 8 (internal category theory)
  InternalAssociativityName : AxiomName
  InternalLeftUnitName      : AxiomName
  InternalRightUnitName     : AxiomName
  YonedaLemmaName           : AxiomName
  InternalLimitUniversalName : AxiomName
  InternalColimitDualityName : AxiomName
  InternalColimitUniversalName : AxiomName
  -- Level 2 (additive / abelian extensions)
  ZeroObjectPropertyName : AxiomName
  KernelAsEqualizerName  : AxiomName
  CokernelAsCoequalizerName : AxiomName
  AdditiveCategoryName   : AxiomName
  AbelianCategoryName    : AxiomName
  AdditivityViaBiproductMapIsoName : AxiomName
  CoimageImageIsomorphismName : AxiomName
  ShortExactSequenceExactnessName : AxiomName
  SplittingLemmaName : AxiomName
  LongExactSequenceExactnessName : AxiomName
  LeftExactFunctorName  : AxiomName
  RightExactFunctorName : AxiomName
  ExactFunctorName      : AxiomName
  AdditiveFunctorPreservesBiproductsName : AxiomName
  CovariantHomLeftExactName : AxiomName
  ExactHomProjectiveCharacterizationName : AxiomName
  InjectiveHomDualityName : AxiomName
  SubobjectLatticeModularName : AxiomName
  FiveLemmaName : AxiomName
  SnakeLemmaName : AxiomName
  ThreeByThreeLemmaName : AxiomName
  TorsionOrthogonalityName : AxiomName
  TorsionDecompositionName : AxiomName
  TorsionClassClosedUnderQuotientsName : AxiomName
  TorsionFreeClosedUnderSubobjectsName : AxiomName
  TorsionReflectiveName : AxiomName
  TorsionFreeCoreflectiveName : AxiomName
  TorsionTheoryAxiomsName : AxiomName
  -- Level 2 (regular / exact / Barr-exact categories)
  RegularCategoryName : AxiomName
  RegularEpiPropertyName : AxiomName
  RegularEpisAreStrongName : AxiomName
  KernelPairEquivRelName : AxiomName
  EffectiveRelationsName : AxiomName
  ExactCategoryName : AxiomName
  BarrExactCategoryName : AxiomName
  BarrEmbeddingTheoremName : AxiomName
  PreservesRegularEpisName : AxiomName
  PreservesFiniteLimitsName : AxiomName
  -- Generic property scaffolding (scales across chapters)
  HasPropertyName : AxiomName
  ClosedUnderName : AxiomName
  StableUnderName : AxiomName
  PreservesPropertyName : AxiomName
  ReflectsPropertyName  : AxiomName
  CreatesPropertyName   : AxiomName

-- | Subjects about which axioms speak (captures kind-specific payload).
data Subject : Set where
  PreCategoryS : (G : M.Identifier) -> Subject
  FunctorMapS  : (F : M.Identifier) (C : M.Identifier) (D : M.Identifier) -> Subject
  -- Subject for a product object P over a family of parts
  ProductS     : (P : M.Identifier) (parts : List M.Identifier) -> Subject
  -- Subject for a coproduct object Q over a family of parts
  CoproductS   : (Q : M.Identifier) (parts : List M.Identifier) -> Subject
  -- Subject for initial/terminal objects (single object carrier)
  InitialS     : (I : M.Identifier) -> Subject
  TerminalS    : (T : M.Identifier) -> Subject
  -- Subject for adjunctions (pair of functors)
  AdjunctionS  : (F G : M.Identifier) (C D : M.Identifier) -> Subject
  -- Subject for factorization systems
  FactorizationS : (C : M.Identifier) (E M : M.Identifier) -> Subject
  -- Subject for projective objects
  ProjectiveS  : (P : M.Identifier) -> Subject
  -- Subject for injective objects
  InjectiveS   : (I : M.Identifier) -> Subject
  -- Subject for category-level properties (well-powered, complete, etc.)
  CategoryPropertyS : (C : M.Identifier) -> Subject
  -- Level 7 subjects
  TwoCategoryS : (C : M.Identifier) -> Subject
  LaxFunctorS  : (F : M.Identifier) (C : M.Identifier) (D : M.Identifier) -> Subject
  BicategoryS  : (B : M.Identifier) -> Subject
  DistributorS : (A B : M.Identifier) (D : M.Identifier) -> Subject
  -- Level 8 subjects
  InternalCategoryS : (IC : M.Identifier) (ambient : M.Identifier) -> Subject
  InternalDiagramS  : (D : M.Identifier) (IC : M.Identifier) -> Subject
  InternalLimitS    : (L : M.Identifier) (D : M.Identifier) (IC : M.Identifier) -> Subject
  InternalColimitS  : (L : M.Identifier) (D : M.Identifier) (IC : M.Identifier) -> Subject
  -- Level 2 additive/abelian subjects
  ZeroObjectS : (C Z : M.Identifier) -> Subject
  KernelEqualizerS : (f : M.Identifier) (C : M.Identifier) -> Subject
  CokernelCoequalizerS : (f : M.Identifier) (C : M.Identifier) -> Subject
  AdditiveCategoryS : (C : M.Identifier) -> Subject
  AbelianCategoryS  : (C : M.Identifier) -> Subject
  BiproductComparisonS : (C : M.Identifier) -> Subject
  CoimageImageIsoS : (f : M.Identifier) (C : M.Identifier) -> Subject
  ShortExactSequenceS : (seq : M.Identifier) -> Subject
  SplittingLemmaS : (seq : M.Identifier) -> Subject
  LongExactSequenceS : (seq : M.Identifier) -> Subject
  FunctorExactnessS : (F : M.Identifier) (C : M.Identifier) (D : M.Identifier) -> Subject
  AdditiveFunctorS : (F : M.Identifier) (C : M.Identifier) (D : M.Identifier) -> Subject
  SubobjectLatticeS : (C X : M.Identifier) -> Subject
  DiagramLemmaS : (lemma : M.Identifier) -> Subject
  TorsionOrthogonalityS : (T F C : M.Identifier) -> Subject
  TorsionDecompositionS : (X T F C : M.Identifier) -> Subject
  TorsionClassClosureQuotientsS : (T C : M.Identifier) -> Subject
  TorsionFreeClosureSubobjectsS : (F C : M.Identifier) -> Subject
  TorsionReflectiveS : (T C : M.Identifier) -> Subject
  TorsionFreeCoreflectiveS : (F C : M.Identifier) -> Subject
  TorsionTheoryS : (T F C : M.Identifier) -> Subject
  -- Regular / exact / Barr-exact subjects
  RegularCategoryS : (C : M.Identifier) -> Subject
  RegularEpiS : (e A B C : M.Identifier) -> Subject
  RegularEpisAreStrongS : (C e : M.Identifier) -> Subject
  KernelPairEquivRelS : (C f K k1 k2 : M.Identifier) -> Subject
  EffectiveRelationsS : (C : M.Identifier) -> Subject
  ExactCategoryS : (C : M.Identifier) -> Subject
  BarrExactCategoryS : (C : M.Identifier) -> Subject
  BarrEmbeddingS : (C K F : M.Identifier) -> Subject
  PreservesRegularEpisS : (F C D : M.Identifier) -> Subject
  PreservesFiniteLimitsS : (F C D : M.Identifier) -> Subject
  -- Generic property scaffolding subjects
  CategoryHasPropertyS : (C prop : M.Identifier) -> Subject
  ClassClosedUnderS    : (C classId opId : M.Identifier) -> Subject
  ClassStableUnderS    : (C classId contextId : M.Identifier) -> Subject
  FunctorPreservesPropertyS : (F C D prop : M.Identifier) -> Subject
  FunctorReflectsPropertyS  : (F C D prop : M.Identifier) -> Subject
  FunctorCreatesPropertyS   : (F C D prop : M.Identifier) -> Subject
  -- Generic functor-level property (beyond preserve/reflect/create)
  FunctorHasPropertyS       : (F C D prop : M.Identifier) -> Subject

-- Bridge from object-language propositions to Agda witnesses
postulate
  Holds     : M.Proposition -> Set
  AxiomProp : (subject : Subject) -> (ax : AxiomName) -> M.Proposition

-- Concrete proposition constructors reflecting the categorical typing prose
postulate
  AssocProp    : (G : M.Identifier) -> M.Proposition
  IdProp       : (G : M.Identifier) -> M.Proposition
  PresCompProp : (F C D : M.Identifier) -> M.Proposition
  PresIdProp   : (F C D : M.Identifier) -> M.Proposition
  -- Level 2 product propositions
  ProductCommProp : (P : M.Identifier) -> (parts : List M.Identifier) -> M.Proposition
  ProductUniqProp : (P : M.Identifier) -> (parts : List M.Identifier) -> M.Proposition
  -- Level 2 coproduct propositions
  CoproductCommProp : (Q : M.Identifier) -> (parts : List M.Identifier) -> M.Proposition
  CoproductUniqProp : (Q : M.Identifier) -> (parts : List M.Identifier) -> M.Proposition
  -- Level 2 initial/terminal uniqueness propositions
  InitialUniqProp : (I : M.Identifier) -> M.Proposition
  TerminalUniqProp : (T : M.Identifier) -> M.Proposition
  -- Level 3 adjunction propositions
  TriangleIdentitiesProp : (F G C D : M.Identifier) -> M.Proposition
  RightAdjPreservesLimitsProp : (G C D : M.Identifier) -> M.Proposition
  LeftAdjPreservesColimitsProp : (F C D : M.Identifier) -> M.Proposition
  -- Level 4 factorization/generators/projectives propositions
  FactorizationUniqueProp : (C E M : M.Identifier) -> M.Proposition
  SubobjectLatticeCompleteProp : (C : M.Identifier) -> M.Proposition
  CanonicalFactorizationProp : (C E M : M.Identifier) -> M.Proposition
  ProjectiveLiftProp : (P : M.Identifier) -> M.Proposition
  InjectiveLiftProp : (I : M.Identifier) -> M.Proposition
  -- Level 7 propositions
  TwoCatAssociativityProp : (C : M.Identifier) -> M.Proposition
  TwoCatUnitalityProp     : (C : M.Identifier) -> M.Proposition
  LaxAssociativityProp    : (F C D : M.Identifier) -> M.Proposition
  LaxUnitalityProp        : (F C D : M.Identifier) -> M.Proposition
  LimitHierarchyProp      : (C : M.Identifier) -> M.Proposition
  CauchyViaDistributorsProp : (C : M.Identifier) -> M.Proposition
  -- Level 8 propositions
  InternalAssocProp   : (IC ambient : M.Identifier) -> M.Proposition
  InternalLeftUnitProp : (IC ambient : M.Identifier) -> M.Proposition
  InternalRightUnitProp : (IC ambient : M.Identifier) -> M.Proposition
  YonedaLemmaProp      : (IC : M.Identifier) (ambient : M.Identifier) -> M.Proposition
  InternalLimitUniversalProp : (L D IC : M.Identifier) -> M.Proposition
  InternalColimitDualityProp : (IC : M.Identifier) (ambient : M.Identifier) -> M.Proposition
  InternalColimitUniversalProp : (L D IC : M.Identifier) -> M.Proposition
  -- Level 2 additive / abelian propositions
  ZeroObjectProp : (C Z : M.Identifier) -> M.Proposition
  KernelAsEqualizerProp : (f C : M.Identifier) -> M.Proposition
  CokernelAsCoequalizerProp : (f C : M.Identifier) -> M.Proposition
  AdditiveCategoryProp : (C : M.Identifier) -> M.Proposition
  AbelianCategoryProp  : (C : M.Identifier) -> M.Proposition
  AdditivityViaBiproductMapIsoProp : (C : M.Identifier) -> M.Proposition
  CoimageImageIsoProp : (f C : M.Identifier) -> M.Proposition
  ShortExactSequenceProp : (seq : M.Identifier) -> M.Proposition
  SplittingLemmaProp : (seq : M.Identifier) -> M.Proposition
  LongExactSequenceProp : (seq : M.Identifier) -> M.Proposition
  LeftExactFunctorProp  : (F C D : M.Identifier) -> M.Proposition
  RightExactFunctorProp : (F C D : M.Identifier) -> M.Proposition
  ExactFunctorProp      : (F C D : M.Identifier) -> M.Proposition
  AdditiveFunctorPreservesBiproductsProp : (F C D : M.Identifier) -> M.Proposition
  CovariantHomLeftExactProp : (F C D : M.Identifier) -> M.Proposition
  ExactHomProjectiveCharacterizationProp : (F C D : M.Identifier) -> M.Proposition
  InjectiveHomDualityProp : (F C D : M.Identifier) -> M.Proposition
  SubobjectLatticeModularProp : (C X : M.Identifier) -> M.Proposition
  FiveLemmaProp : (lemma : M.Identifier) -> M.Proposition
  SnakeLemmaProp : (lemma : M.Identifier) -> M.Proposition
  ThreeByThreeLemmaProp : (lemma : M.Identifier) -> M.Proposition
  TorsionOrthogonalityProp : (T F C : M.Identifier) -> M.Proposition
  TorsionDecompositionProp : (X T F C : M.Identifier) -> M.Proposition
  TorsionClassClosureQuotientsProp : (T C : M.Identifier) -> M.Proposition
  TorsionFreeClosureSubobjectsProp : (F C : M.Identifier) -> M.Proposition
  TorsionReflectiveProp : (T C : M.Identifier) -> M.Proposition
  TorsionFreeCoreflectiveProp : (F C : M.Identifier) -> M.Proposition
  TorsionTheoryAxiomsProp : (T F C : M.Identifier) -> M.Proposition
  -- Regular / exact / Barr-exact propositions
  RegularCategoryProp : (C : M.Identifier) -> M.Proposition
  RegularEpiProp : (e A B C : M.Identifier) -> M.Proposition
  RegularEpisAreStrongProp : (C e : M.Identifier) -> M.Proposition
  KernelPairEquivRelProp : (C f K k1 k2 : M.Identifier) -> M.Proposition
  EffectiveRelationsProp : (C : M.Identifier) -> M.Proposition
  ExactCategoryProp : (C : M.Identifier) -> M.Proposition
  BarrExactCategoryProp : (C : M.Identifier) -> M.Proposition
  BarrEmbeddingTheoremProp : (C K F : M.Identifier) -> M.Proposition
  PreservesRegularEpisProp : (F C D : M.Identifier) -> M.Proposition
  PreservesFiniteLimitsProp : (F C D : M.Identifier) -> M.Proposition
  -- Generic property scaffolding propositions
  CategoryHasPropertyProp : (C prop : M.Identifier) -> M.Proposition
  ClassClosedUnderProp    : (C classId opId : M.Identifier) -> M.Proposition
  ClassStableUnderProp    : (C classId contextId : M.Identifier) -> M.Proposition
  FunctorPreservesPropertyProp : (F C D prop : M.Identifier) -> M.Proposition
  FunctorReflectsPropertyProp  : (F C D prop : M.Identifier) -> M.Proposition
  FunctorCreatesPropertyProp   : (F C D prop : M.Identifier) -> M.Proposition
  FunctorHasPropertyProp       : (F C D prop : M.Identifier) -> M.Proposition

  -- Canonicalization of some specific propositions into generic property form
  RegularCategoryProp-Canonical
    : (C : M.Identifier)
    -> RegularCategoryProp C ≡ CategoryHasPropertyProp C P.RegularCategoryId
  ExactCategoryProp-Canonical
    : (C : M.Identifier)
    -> ExactCategoryProp C ≡ CategoryHasPropertyProp C P.ExactCategoryId
  BarrExactCategoryProp-Canonical
    : (C : M.Identifier)
    -> BarrExactCategoryProp C ≡ CategoryHasPropertyProp C P.BarrExactCategoryId
  PreservesFiniteLimitsProp-Canonical
    : (F C D : M.Identifier)
    -> PreservesFiniteLimitsProp F C D ≡ FunctorPreservesPropertyProp F C D P.FiniteLimitsId
  PreservesRegularEpisProp-Canonical
    : (F C D : M.Identifier)
    -> PreservesRegularEpisProp F C D ≡ FunctorPreservesPropertyProp F C D P.RegularEpiClassId
  EffectiveRelationsProp-Canonical
    : (C : M.Identifier)
    -> EffectiveRelationsProp C ≡ CategoryHasPropertyProp C P.EffectiveEquivalenceRelationsId

-- AxiomProp equations binding subjects/axioms to concrete propositions
postulate
  AxiomProp-Assoc
    : (G : M.Identifier)
    -> AxiomProp (PreCategoryS G) AssociativityAxiomName ≡ AssocProp G

  AxiomProp-Id
    : (G : M.Identifier)
    -> AxiomProp (PreCategoryS G) IdentityAxiomName ≡ IdProp G

  AxiomProp-PresComp
    : (F C D : M.Identifier)
    -> AxiomProp (FunctorMapS F C D) PreservesCompositionName ≡ PresCompProp F C D

  AxiomProp-PresId
    : (F C D : M.Identifier)
    -> AxiomProp (FunctorMapS F C D) PreservesIdentityName ≡ PresIdProp F C D

  -- Product axioms
  AxiomProp-ProductComm
    : (P : M.Identifier) (parts : List M.Identifier)
    -> AxiomProp (ProductS P parts) ProductCommutativityName ≡ ProductCommProp P parts

  AxiomProp-ProductUniq
    : (P : M.Identifier) (parts : List M.Identifier)
    -> AxiomProp (ProductS P parts) ProductUniquenessName ≡ ProductUniqProp P parts

  -- Coproduct axioms
  AxiomProp-CoproductComm
    : (Q : M.Identifier) (parts : List M.Identifier)
    -> AxiomProp (CoproductS Q parts) CoproductCommutativityName ≡ CoproductCommProp Q parts

  AxiomProp-CoproductUniq
    : (Q : M.Identifier) (parts : List M.Identifier)
    -> AxiomProp (CoproductS Q parts) CoproductUniquenessName ≡ CoproductUniqProp Q parts

  -- Initial/Terminal uniqueness axioms
  AxiomProp-InitialUniq
    : (I : M.Identifier)
    -> AxiomProp (InitialS I) InitialUniquenessName ≡ InitialUniqProp I

  AxiomProp-TerminalUniq
    : (T : M.Identifier)
    -> AxiomProp (TerminalS T) TerminalUniquenessName ≡ TerminalUniqProp T

  -- Adjunction axioms
  AxiomProp-TriangleIds
    : (F G C D : M.Identifier)
    -> AxiomProp (AdjunctionS F G C D) TriangleIdentitiesName ≡ TriangleIdentitiesProp F G C D

  AxiomProp-RightAdjPreservesLimits
    : (G C D : M.Identifier)
    -> (F : M.Identifier)
    -> AxiomProp (AdjunctionS F G C D) RightAdjPreservesLimitsName ≡ RightAdjPreservesLimitsProp G C D

  AxiomProp-LeftAdjPreservesColimits
    : (F C D : M.Identifier)
    -> (G : M.Identifier)
    -> AxiomProp (AdjunctionS F G C D) LeftAdjPreservesColimitsName ≡ LeftAdjPreservesColimitsProp F C D

  -- Factorization system axioms
  AxiomProp-FactorizationUnique
    : (C E M : M.Identifier)
    -> AxiomProp (FactorizationS C E M) FactorizationUniquenessName ≡ FactorizationUniqueProp C E M

  AxiomProp-SubobjectLatticeComplete
    : (C : M.Identifier)
    -> AxiomProp (CategoryPropertyS C) SubobjectLatticeCompletenessName ≡ SubobjectLatticeCompleteProp C

  AxiomProp-CanonicalFactorization
    : (C E M : M.Identifier)
    -> AxiomProp (FactorizationS C E M) CanonicalFactorizationSystemName ≡ CanonicalFactorizationProp C E M

  -- Projective/Injective lifting axioms
  AxiomProp-ProjectiveLift
    : (P : M.Identifier)
    -> AxiomProp (ProjectiveS P) ProjectiveLiftingName ≡ ProjectiveLiftProp P

  AxiomProp-InjectiveLift
    : (I : M.Identifier)
    -> AxiomProp (InjectiveS I) InjectiveLiftingName ≡ InjectiveLiftProp I
  -- 2-category axioms
  AxiomProp-TwoCatAssoc
    : (C : M.Identifier)
    -> AxiomProp (TwoCategoryS C) TwoCatAssociativityName ≡ TwoCatAssociativityProp C
  AxiomProp-TwoCatUnit
    : (C : M.Identifier)
    -> AxiomProp (TwoCategoryS C) TwoCatUnitalityName ≡ TwoCatUnitalityProp C
  -- Bicategory coherence axioms (reuse 2-category proposition forms)
  AxiomProp-BicategoryAssoc
    : (B : M.Identifier)
    -> AxiomProp (BicategoryS B) TwoCatAssociativityName ≡ TwoCatAssociativityProp B
  AxiomProp-BicategoryUnit
    : (B : M.Identifier)
    -> AxiomProp (BicategoryS B) TwoCatUnitalityName ≡ TwoCatUnitalityProp B
  -- Lax functor coherence axioms
  AxiomProp-LaxAssoc
    : (F C D : M.Identifier)
    -> AxiomProp (LaxFunctorS F C D) LaxAssociativityName ≡ LaxAssociativityProp F C D
  AxiomProp-LaxUnit
    : (F C D : M.Identifier)
    -> AxiomProp (LaxFunctorS F C D) LaxUnitalityName ≡ LaxUnitalityProp F C D
  -- Limit hierarchy meta-theorem
  AxiomProp-LimitHierarchy
    : (C : M.Identifier)
    -> AxiomProp (CategoryPropertyS C) LimitHierarchyName ≡ LimitHierarchyProp C
  -- Cauchy completeness via distributors
  AxiomProp-CauchyViaDistributors
    : (C : M.Identifier)
    -> AxiomProp (CategoryPropertyS C) CauchyViaDistributorsName ≡ CauchyViaDistributorsProp C
  -- Internal category axioms
  AxiomProp-InternalAssoc
    : (IC ambient : M.Identifier)
    -> AxiomProp (InternalCategoryS IC ambient) InternalAssociativityName ≡ InternalAssocProp IC ambient
  AxiomProp-InternalLeftUnit
    : (IC ambient : M.Identifier)
    -> AxiomProp (InternalCategoryS IC ambient) InternalLeftUnitName ≡ InternalLeftUnitProp IC ambient
  AxiomProp-InternalRightUnit
    : (IC ambient : M.Identifier)
    -> AxiomProp (InternalCategoryS IC ambient) InternalRightUnitName ≡ InternalRightUnitProp IC ambient
  -- Internal Yoneda lemma
  AxiomProp-YonedaLemma
    : (IC ambient : M.Identifier)
    -> AxiomProp (InternalCategoryS IC ambient) YonedaLemmaName ≡ YonedaLemmaProp IC ambient
  -- Internal limit universal property
  AxiomProp-InternalLimitUniversal
    : (L D IC : M.Identifier)
    -> AxiomProp (InternalLimitS L D IC) InternalLimitUniversalName ≡ InternalLimitUniversalProp L D IC
  -- Internal colimit duality correctness
  AxiomProp-InternalColimitDuality
    : (IC ambient : M.Identifier)
    -> AxiomProp (InternalCategoryS IC ambient) InternalColimitDualityName ≡ InternalColimitDualityProp IC ambient
  -- Internal colimit universal property
  AxiomProp-InternalColimitUniversal
    : (L D IC : M.Identifier)
    -> AxiomProp (InternalColimitS L D IC) InternalColimitUniversalName ≡ InternalColimitUniversalProp L D IC
  -- Additive / abelian equations
  AxiomProp-ZeroObject
    : (C Z : M.Identifier)
    -> AxiomProp (ZeroObjectS C Z) ZeroObjectPropertyName ≡ ZeroObjectProp C Z
  AxiomProp-KernelAsEqualizer
    : (f C : M.Identifier)
    -> AxiomProp (KernelEqualizerS f C) KernelAsEqualizerName ≡ KernelAsEqualizerProp f C
  AxiomProp-CokernelAsCoequalizer
    : (f C : M.Identifier)
    -> AxiomProp (CokernelCoequalizerS f C) CokernelAsCoequalizerName ≡ CokernelAsCoequalizerProp f C
  AxiomProp-AdditiveCategory
    : (C : M.Identifier)
    -> AxiomProp (AdditiveCategoryS C) AdditiveCategoryName ≡ AdditiveCategoryProp C
  AxiomProp-AbelianCategory
    : (C : M.Identifier)
    -> AxiomProp (AbelianCategoryS C) AbelianCategoryName ≡ AbelianCategoryProp C
  AxiomProp-AdditivityViaBiproductMapIso
    : (C : M.Identifier)
    -> AxiomProp (BiproductComparisonS C) AdditivityViaBiproductMapIsoName ≡ AdditivityViaBiproductMapIsoProp C
  AxiomProp-CoimageImageIso
    : (f C : M.Identifier)
    -> AxiomProp (CoimageImageIsoS f C) CoimageImageIsomorphismName ≡ CoimageImageIsoProp f C
  AxiomProp-ShortExactSequenceExactness
    : (seq : M.Identifier)
    -> AxiomProp (ShortExactSequenceS seq) ShortExactSequenceExactnessName ≡ ShortExactSequenceProp seq
  AxiomProp-SplittingLemma
    : (seq : M.Identifier)
    -> AxiomProp (SplittingLemmaS seq) SplittingLemmaName ≡ SplittingLemmaProp seq
  AxiomProp-LongExactSequenceExactness
    : (seq : M.Identifier)
    -> AxiomProp (LongExactSequenceS seq) LongExactSequenceExactnessName ≡ LongExactSequenceProp seq
  AxiomProp-LeftExactFunctor
    : (F C D : M.Identifier)
    -> AxiomProp (FunctorExactnessS F C D) LeftExactFunctorName ≡ LeftExactFunctorProp F C D
  AxiomProp-RightExactFunctor
    : (F C D : M.Identifier)
    -> AxiomProp (FunctorExactnessS F C D) RightExactFunctorName ≡ RightExactFunctorProp F C D
  AxiomProp-ExactFunctor
    : (F C D : M.Identifier)
    -> AxiomProp (FunctorExactnessS F C D) ExactFunctorName ≡ ExactFunctorProp F C D
  AxiomProp-AdditiveFunctorPreservesBiproducts
    : (F C D : M.Identifier)
    -> AxiomProp (AdditiveFunctorS F C D) AdditiveFunctorPreservesBiproductsName ≡ AdditiveFunctorPreservesBiproductsProp F C D
  AxiomProp-CovariantHomLeftExact
    : (F C D : M.Identifier)
    -> AxiomProp (FunctorExactnessS F C D) CovariantHomLeftExactName ≡ CovariantHomLeftExactProp F C D
  AxiomProp-ExactHomProjectiveCharacterization
    : (F C D : M.Identifier)
    -> AxiomProp (FunctorExactnessS F C D) ExactHomProjectiveCharacterizationName ≡ ExactHomProjectiveCharacterizationProp F C D
  AxiomProp-InjectiveHomDuality
    : (F C D : M.Identifier)
    -> AxiomProp (FunctorExactnessS F C D) InjectiveHomDualityName ≡ InjectiveHomDualityProp F C D
  AxiomProp-SubobjectLatticeModular
    : (C X : M.Identifier)
    -> AxiomProp (SubobjectLatticeS C X) SubobjectLatticeModularName ≡ SubobjectLatticeModularProp C X
  AxiomProp-FiveLemma
    : (lemma : M.Identifier)
    -> AxiomProp (DiagramLemmaS lemma) FiveLemmaName ≡ FiveLemmaProp lemma
  AxiomProp-SnakeLemma
    : (lemma : M.Identifier)
    -> AxiomProp (DiagramLemmaS lemma) SnakeLemmaName ≡ SnakeLemmaProp lemma
  AxiomProp-ThreeByThreeLemma
    : (lemma : M.Identifier)
    -> AxiomProp (DiagramLemmaS lemma) ThreeByThreeLemmaName ≡ ThreeByThreeLemmaProp lemma
  AxiomProp-TorsionOrthogonality
    : (T F C : M.Identifier)
    -> AxiomProp (TorsionOrthogonalityS T F C) TorsionOrthogonalityName ≡ TorsionOrthogonalityProp T F C
  AxiomProp-TorsionDecomposition
    : (X T F C : M.Identifier)
    -> AxiomProp (TorsionDecompositionS X T F C) TorsionDecompositionName ≡ TorsionDecompositionProp X T F C
  AxiomProp-TorsionClassClosureQuotients
    : (T C : M.Identifier)
    -> AxiomProp (TorsionClassClosureQuotientsS T C) TorsionClassClosedUnderQuotientsName ≡ TorsionClassClosureQuotientsProp T C
  AxiomProp-TorsionFreeClosureSubobjects
    : (F C : M.Identifier)
    -> AxiomProp (TorsionFreeClosureSubobjectsS F C) TorsionFreeClosedUnderSubobjectsName ≡ TorsionFreeClosureSubobjectsProp F C
  AxiomProp-TorsionReflective
    : (T C : M.Identifier)
    -> AxiomProp (TorsionReflectiveS T C) TorsionReflectiveName ≡ TorsionReflectiveProp T C
  AxiomProp-TorsionFreeCoreflective
    : (F C : M.Identifier)
    -> AxiomProp (TorsionFreeCoreflectiveS F C) TorsionFreeCoreflectiveName ≡ TorsionFreeCoreflectiveProp F C
  AxiomProp-TorsionTheoryAxioms
    : (T F C : M.Identifier)
    -> AxiomProp (TorsionTheoryS T F C) TorsionTheoryAxiomsName ≡ TorsionTheoryAxiomsProp T F C
  -- Regular / exact / Barr-exact equations
  AxiomProp-RegularCategory
    : (C : M.Identifier)
    -> AxiomProp (RegularCategoryS C) RegularCategoryName ≡ RegularCategoryProp C
  AxiomProp-RegularEpi
    : (e A B C : M.Identifier)
    -> AxiomProp (RegularEpiS e A B C) RegularEpiPropertyName ≡ RegularEpiProp e A B C
  AxiomProp-RegularEpisAreStrong
    : (C e : M.Identifier)
    -> AxiomProp (RegularEpisAreStrongS C e) RegularEpisAreStrongName ≡ RegularEpisAreStrongProp C e
  AxiomProp-KernelPairEquivRel
    : (C f K k1 k2 : M.Identifier)
    -> AxiomProp (KernelPairEquivRelS C f K k1 k2) KernelPairEquivRelName ≡ KernelPairEquivRelProp C f K k1 k2
  AxiomProp-EffectiveRelations
    : (C : M.Identifier)
    -> AxiomProp (EffectiveRelationsS C) EffectiveRelationsName ≡ EffectiveRelationsProp C
  AxiomProp-ExactCategory
    : (C : M.Identifier)
    -> AxiomProp (ExactCategoryS C) ExactCategoryName ≡ ExactCategoryProp C
  AxiomProp-BarrExactCategory
    : (C : M.Identifier)
    -> AxiomProp (BarrExactCategoryS C) BarrExactCategoryName ≡ BarrExactCategoryProp C
  AxiomProp-BarrEmbeddingTheorem
    : (C K F : M.Identifier)
    -> AxiomProp (BarrEmbeddingS C K F) BarrEmbeddingTheoremName ≡ BarrEmbeddingTheoremProp C K F
  AxiomProp-PreservesRegularEpis
    : (F C D : M.Identifier)
    -> AxiomProp (PreservesRegularEpisS F C D) PreservesRegularEpisName ≡ PreservesRegularEpisProp F C D
  AxiomProp-PreservesFiniteLimits
    : (F C D : M.Identifier)
    -> AxiomProp (PreservesFiniteLimitsS F C D) PreservesFiniteLimitsName ≡ PreservesFiniteLimitsProp F C D
  -- Generic property scaffolding equations
  AxiomProp-CategoryHasProperty
    : (C prop : M.Identifier)
    -> AxiomProp (CategoryHasPropertyS C prop) HasPropertyName ≡ CategoryHasPropertyProp C prop
  AxiomProp-ClassClosedUnder
    : (C classId opId : M.Identifier)
    -> AxiomProp (ClassClosedUnderS C classId opId) ClosedUnderName ≡ ClassClosedUnderProp C classId opId
  AxiomProp-ClassStableUnder
    : (C classId contextId : M.Identifier)
    -> AxiomProp (ClassStableUnderS C classId contextId) StableUnderName ≡ ClassStableUnderProp C classId contextId
  AxiomProp-FunctorPreservesProperty
    : (F C D prop : M.Identifier)
    -> AxiomProp (FunctorPreservesPropertyS F C D prop) PreservesPropertyName ≡ FunctorPreservesPropertyProp F C D prop
  AxiomProp-FunctorReflectsProperty
    : (F C D prop : M.Identifier)
    -> AxiomProp (FunctorReflectsPropertyS F C D prop) ReflectsPropertyName ≡ FunctorReflectsPropertyProp F C D prop
  AxiomProp-FunctorCreatesProperty
    : (F C D prop : M.Identifier)
    -> AxiomProp (FunctorCreatesPropertyS F C D prop) CreatesPropertyName ≡ FunctorCreatesPropertyProp F C D prop
  AxiomProp-FunctorHasProperty
    : (F C D prop : M.Identifier)
    -> AxiomProp (FunctorHasPropertyS F C D prop) HasPropertyName ≡ FunctorHasPropertyProp F C D prop

-- A proof is a witness that the axiom proposition for a subject holds
Proof : (subject : Subject) -> (ax : AxiomName) -> Set
Proof subject ax = Holds (AxiomProp subject ax)

------------------------------------------------------------------------
-- Axiom records (used in postulates below)
------------------------------------------------------------------------

-- Associativity and Identity axioms (proofs are opaque at this level)
record AssociativityAxiom : Set where
  constructor AXIOM_Associativity
  field over : M.Identifier
-- CATEGORY: Composition is associative.

record IdentityAxiom : Set where
  constructor AXIOM_Identity
  field over : M.Identifier
-- CATEGORY: Identities act as two-sided units.

-- | Functor F preserves composition.
record FunctorPreservesCompositionAxiom : Set where
  constructor AXIOM_PreservesComposition
  field forF : M.Identifier

-- | Functor F preserves identities.
record FunctorPreservesIdentityAxiom : Set where
  constructor AXIOM_PreservesIdentity
  field forF : M.Identifier

-- CategoryPromotion and FunctorPromotion (used in smart constructors below)
-- | Package identity + associativity proofs to promote G to a Category.
record CategoryPromotion : Set where
  constructor PROMOTE_to_CATEGORY
  field
    G      : M.Identifier
    hasIdentity      : Proof (PreCategoryS G) IdentityAxiomName
    hasAssociativity : Proof (PreCategoryS G) AssociativityAxiomName

-- | Package preservation proofs to promote F to a Functor between C and D.
record FunctorPromotion : Set where
  constructor PROMOTE_to_FUNCTOR
  field
    F      : M.Identifier
    C      : M.Identifier
    D      : M.Identifier
    preservesComposition : Proof (FunctorMapS F C D) PreservesCompositionName
    preservesIdentity    : Proof (FunctorMapS F C D) PreservesIdentityName

------------------------------------------------------------------------
-- Bridge: From concrete axiom records to proof witnesses
------------------------------------------------------------------------

postulate
  -- Precategory structure axioms
  assocProofFromAxiom
    : (ax : AssociativityAxiom)
    -> Proof (PreCategoryS (AssociativityAxiom.over ax)) AssociativityAxiomName

  idProofFromAxiom
    : (ax : IdentityAxiom)
    -> Proof (PreCategoryS (IdentityAxiom.over ax)) IdentityAxiomName

  -- Functor structure preservation axioms (require stating domain/codomain)
  preservesCompositionProof
    : (ax : FunctorPreservesCompositionAxiom)
    -> (C D : M.Identifier)
    -> Proof (FunctorMapS (FunctorPreservesCompositionAxiom.forF ax) C D) PreservesCompositionName

  preservesIdentityProof
    : (ax : FunctorPreservesIdentityAxiom)
    -> (C D : M.Identifier)
    -> Proof (FunctorMapS (FunctorPreservesIdentityAxiom.forF ax) C D) PreservesIdentityName

------------------------------------------------------------------------
-- Smart constructors: build promotions from axiom records
------------------------------------------------------------------------

promoteToCategoryFromAxioms
  : (G : M.Identifier)  -- The precategory being promoted
  -> CategoryPromotion
promoteToCategoryFromAxioms G =
  let idAx = AXIOM_Identity G
      assocAx = AXIOM_Associativity G
  in PROMOTE_to_CATEGORY
       G                              -- G field
       (idProofFromAxiom idAx)         -- hasIdentity field
       (assocProofFromAxiom assocAx)   -- hasAssociativity field

promoteToFunctorFromAxioms
  : (F C D : M.Identifier)  -- The functor being promoted and its domain/codomain
  -> FunctorPromotion
promoteToFunctorFromAxioms F C D =
  let pcAx = AXIOM_PreservesComposition F
      piAx = AXIOM_PreservesIdentity F
  in PROMOTE_to_FUNCTOR
       F
       C D
       (preservesCompositionProof pcAx C D)
       (preservesIdentityProof    piAx C D)

-- Concepts mentioned by duality axioms
data ConceptName : Set where
  TERMINAL_OBJECT : ConceptName
  INITIAL_OBJECT  : ConceptName
  PRODUCT         : ConceptName
  COPRODUCT       : ConceptName
  PULLBACK        : ConceptName
  PUSHOUT         : ConceptName
  EQUALIZER       : ConceptName
  COEQUALIZER     : ConceptName
  LIMIT           : ConceptName
  COLIMIT         : ConceptName
  MONOMORPHISM    : ConceptName
  EPIMORPHISM     : ConceptName

-- Morphism declarations for a precategory signature
record MorphismDeclaration : Set where
  constructor mor
  field
    name : M.Identifier
    dom  : M.Identifier
    cod  : M.Identifier
open MorphismDeclaration public

-- Object and morphism mapping atoms as they appear in FUNCTOR_MAP blocks
record ObjectMapping : Set where
  constructor obj↦
  field src : M.Identifier
        dst : M.Identifier
open ObjectMapping public

record MorphismMapping : Set where
  constructor mor↦
  field src : M.Identifier
        dst : M.Identifier
open MorphismMapping public

------------------------------------------------------------------------
-- Declarations mirroring the categorical productions
------------------------------------------------------------------------

-- PreCategoryDeclaration ::= PRECATEGORY ...
record PreCategoryDeclaration : Set where
  constructor PRECATEGORY_has
  field
    G-id      : M.Identifier
    Objects   : List M.Identifier
    Morphisms : List MorphismDeclaration
-- CATEGORY: A directed graph (quiver) prior to imposing identities and composition laws.

-- (CategoryPromotion, FunctorPromotion, AssociativityAxiom, IdentityAxiom defined earlier)

-- FunctorMapDeclaration ::= FUNCTOR_MAP F : C -> D maps { ... }
record FunctorMapDeclaration : Set where
  constructor FUNCTOR_MAP_maps
  field
    F        : M.Identifier
    C        : M.Identifier
    D        : M.Identifier
    onObj    : List ObjectMapping
    onMor    : List MorphismMapping
-- CATEGORY: Graph homomorphism between underlying quivers.

-- (FunctorPromotion now defined earlier)

-- (FunctorPreservesCompositionAxiom and FunctorPreservesIdentityAxiom now defined earlier)

-- Parallel functors predicate
record AreParallel : Set where
  constructor AreParallel_of
  field F G : M.Identifier
-- CATEGORY: F and G share same domain and codomain categories.

-- Components of a natural transformation
record TransformationComponent : Set where
  constructor _at_
  field alpha : M.Identifier
        X     : M.Identifier
-- CATEGORY: Component arrow alpha_X : F(X) -> G(X).

-- Natural transformation declaration
record NaturalTransformationDeclaration : Set where
  constructor NATURAL_TRANSFORMATION_⇒
  field
    alpha : M.Identifier
    F     : M.Identifier
    G     : M.Identifier
-- CATEGORY: 2-morphism between functors in Cat.

record NaturalityConditionAxiom : Set where
  constructor AXIOM_Naturality
  field
    alpha : M.Identifier
    F     : M.Identifier
    G     : M.Identifier
-- CATEGORY: Coherence law G(f) ∘ alpha_X = alpha_Y ∘ F(f).

-- Contravariant functors as covariant on op
record ContravariantFunctorAsFunctorOp : Set where
  constructor DEFINE_as_CONTRAVARIANT_FUNCTOR
  field
    F : M.Identifier
    C : M.Identifier
    D : M.Identifier
-- CATEGORY: Equivalence with FUNCTOR C^op -> D.

-- Contravariant action on morphisms (notation F <~f~>)
record ContravariantMap : Set where
  constructor _<~_~>
  field F : M.Identifier
        f : M.Identifier

-- | Contravariant functor preserves identities.
record ContravariantIdentityAxiom : Set where
  constructor AXIOM_contravariant_identity
  field F A : M.Identifier

-- | Contravariant functor preserves composition (with reversal).
record ContravariantCompositionAxiom : Set where
  constructor AXIOM_contravariant_composition
  field F f g : M.Identifier

-- Comma category and its objects/morphisms
record GeneralCommaCategory : Set where
  constructor _↓_
  field F G : M.Identifier
-- CATEGORY: (F ↓ G) formed from F : A→C and G : B→C.

record CommaObject : Set where
  constructor Obj
  field a b h : M.Identifier
-- CATEGORY: Object (a,b,h) with h : F a -> G b.

record CommaMorphism : Set where
  constructor Mor
  field f g : M.Identifier
-- CATEGORY: Pair (f,g) commuting with h,h'.

record CommaMorphismCommutativityAxiom : Set where
  constructor AXIOM_CommaSquare
  field h h' f g : M.Identifier

-- Identity and constant functors
-- | Identity functor constructor on C.
record IdentityFunctorConstructor : Set where
  constructor Id
  field C : M.Identifier

-- | Constant functor constructor Δ_X.
record ConstantFunctorConstructor : Set where
  constructor Δ_  -- pronounced "Delta sub X"
  field X : M.Identifier

-- Slice and coslice categories
-- | Slice category C/X.
record SliceCategory : Set where
  constructor _/_
  field C X : M.Identifier

-- | Coslice category X\C.
record CosliceCategory : Set where
  constructor _\\_  -- backslash escaped
  field X C : M.Identifier

-- | Inverse morphism placeholder.
record InverseMorphism : Set where
  constructor inverse
  field f : M.Identifier

-- Theorems
-- FIXED: TheoremDeclaration needs to be Set₁ because it contains M.Proposition (which is Set₁)
record TheoremDeclaration : Set₁ where
  constructor THEOREM
  field name : String
        prop : M.Proposition

-- Morphism property assertions
data MorphismProperty : Set where
  SPLIT_MONOMORPHISM : MorphismProperty
  SPLIT_EPIMORPHISM  : MorphismProperty

record MorphismPropertyAssertion : Set where
  constructor _is_
  field f : M.Identifier
        P : MorphismProperty

-- Category property: balanced
record CategoryPropertyDefinition : Set where
  constructor _is_BALANCED
  field C : M.Identifier

-- Dual statements and mappings
postulate
  op : M.Proposition -> M.Proposition

-- FIXED: DualStatement needs to be Set₁ because it contains M.Proposition (which is Set₁)
record DualStatement : Set₁ where
  constructor _^op
  field P : M.Proposition

-- General duality mapping axiom
record DualityMappingAxiom : Set where
  constructor DUALITY_MAPPING_FOR_IS
  field C1 C2 : ConceptName

-- Fixed mapping presets captured by constructors (using × defined earlier)
DualityMappingPreset : Set
DualityMappingPreset = List (Core.Phase.Σ ConceptName (λ _ → ConceptName))

-- (_×_ product type defined earlier in Helpers section)

-- Example canonical preset list (TERMINAL↔INITIAL, PRODUCT↔COPRODUCT, ...)
DUALITY_PRESET : DualityMappingPreset
DUALITY_PRESET =
  Core.Phase._,ₛ_ TERMINAL_OBJECT INITIAL_OBJECT ∷
  Core.Phase._,ₛ_ INITIAL_OBJECT TERMINAL_OBJECT ∷
  Core.Phase._,ₛ_ PRODUCT COPRODUCT ∷
  Core.Phase._,ₛ_ COPRODUCT PRODUCT ∷
  Core.Phase._,ₛ_ PULLBACK PUSHOUT ∷
  Core.Phase._,ₛ_ PUSHOUT PULLBACK ∷
  Core.Phase._,ₛ_ EQUALIZER COEQUALIZER ∷
  Core.Phase._,ₛ_ COEQUALIZER EQUALIZER ∷
  Core.Phase._,ₛ_ LIMIT COLIMIT ∷
  Core.Phase._,ₛ_ COLIMIT LIMIT ∷
  Core.Phase._,ₛ_ MONOMORPHISM EPIMORPHISM ∷
  Core.Phase._,ₛ_ EPIMORPHISM MONOMORPHISM ∷
  []

-- Dual theorem inference
record DualTheoremInference : Set where
  constructor INFER_DUAL_THEOREM_FROM
  field dualName : String
        origName : String
-- CATEGORY: Functorial mapping of proofs under duality.

------------------------------------------------------------------------
-- Notes: This file formalizes the base Category Theory concepts as Agda records/enums.
-- The typing/CATEGORY annotations are preserved as comments or opaque
-- proof placeholders (⊤) when appropriate would be added later.
------------------------------------------------------------------------
