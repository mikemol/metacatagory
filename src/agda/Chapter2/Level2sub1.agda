{-# OPTIONS --without-K #-}

-- | Chapter 2 §1: abelian entry point—zero objects, zero morphisms, kernels,
--   cokernels, and exactness predicates that refine the additive toolkit.
module Chapter2.Level2sub1 where

open import Agda.Builtin.Unit     using (⊤; tt)
open import Agda.Builtin.String   using (String)
open import Agda.Builtin.List     using (List; []; _∷_)
open import Core.Phase using (Bool; true; false)
open import Agda.Builtin.Equality using (_≡_)

open import Metamodel as M
open import Core      as C

-- Expression placeholder (for equations / witnesses appearing textually)
Expr : Set
Expr = String

------------------------------------------------------------------------
-- Section 2.1: Zero objects, kernels, cokernels (Abelian setting foundations)
------------------------------------------------------------------------

-- Zero object property: C has_a ZERO_OBJECT
record HasZeroObjectProperty : Set where
  constructor _has_a_ZERO_OBJECT
  field
    category : M.Identifier
    zeroObj  : M.Identifier
-- CATEGORY: zeroObj is simultaneously initial and terminal (pointed category).

-- Unique zero morphism zero_mor(from:A, to:B)
record ZeroMorphismDeclaration : Set where
  constructor zero_mor
  field
    from : M.Identifier
    to   : M.Identifier
    viaZeroObject : M.Identifier  -- the chosen zero object Z
    factorizationLeft  : M.Identifier -- A -> Z
    factorizationRight : M.Identifier -- Z -> B
-- CATEGORY: Unique morphism A -> B factoring A -> Z -> B.

-- Kernel as Equalizer definition: DEFINE ker(f) as Equalizer(f, zero_mor)
record KernelAsEqualizerDefinition : Set where
  constructor DEFINE_ker_as_Equalizer
  field
    morphism     : M.Identifier  -- f : A -> B
    domain       : M.Identifier  -- A
    codomain     : M.Identifier  -- B
    zeroMorphism : ZeroMorphismDeclaration
    equalizerObject : M.Identifier  -- ker(f)
    equalizerMono   : M.Identifier  -- inclusion ker(f) -> A
-- CATEGORY: ker(f) definitionally equal to Equalizer(f, 0_{A,B}).

-- Predicate: m is_kernel_of(f)
record IsKernelOfPredicate : Set where
  constructor _is_kernel_of_
  field
    mono      : M.Identifier  -- m : K -> A
    morphism  : M.Identifier  -- f : A -> B
    kernelRep : M.Identifier  -- ker(f)
-- CATEGORY: mono represents the kernel subobject of f.

-- Duality mapping axioms (KERNEL <-> COKERNEL, Equalizer <-> Coequalizer)
-- | Duality mapping identifying kernels with cokernels.
record DualityMappingKernelCokernel : Set where
  constructor DUALITY_MAPPING_FOR_KERNEL_IS_COKERNEL
  field unit : ⊤

-- | Duality mapping identifying equalizers with coequalizers.
record DualityMappingEqualizerCoequalizer : Set where
  constructor DUALITY_MAPPING_FOR_Equalizer_IS_Coequalizer
  field unit : ⊤

-- Inferred dual axiom: cokernel as coequalizer
record CokernelAsCoequalizerInference : Set where
  constructor INFER_DUAL_AXIOM_CokernelAsCoequalizer_FROM_KernelAsEqualizer
  field
    originalKernelDef : KernelAsEqualizerDefinition
    cokernelObject    : M.Identifier  -- coker(f)
    coequalizerArrow  : M.Identifier  -- B -> coker(f)
-- CATEGORY: coker(f) definitionally equal to Coequalizer(f, 0_{A,B}).

------------------------------------------------------------------------
-- Section 2.2: Additive categories & biproducts
------------------------------------------------------------------------

-- Enrichment property: C is ENRICHED_OVER M
-- | Enrichment property: C is enriched over a monoidal category.
record EnrichedOverProperty : Set where
  constructor _is_ENRICHED_OVER_
  field
    category : M.Identifier
    monoidal : M.Identifier  -- e.g. Ab
-- CATEGORY: Hom-objects live in monoidal and composition is a morphism there.

-- Biproduct object A ⊕ B
-- | Biproduct object A ⊕ B with projections and injections.
record BiproductObject : Set where
  constructor _⊕_
  field
    left  : M.Identifier
    right : M.Identifier
    object : M.Identifier         -- A⊕B
    projectionLeft  : M.Identifier -- pA
    projectionRight : M.Identifier -- pB
    injectionLeft   : M.Identifier -- iA
    injectionRight  : M.Identifier -- iB
-- CATEGORY: Simultaneously product and coproduct with standard identities.

-- Additive category declaration
-- | Additive category: zero object, Ab-enrichment, and binary biproducts.
record AdditiveCategoryDeclaration : Set where
  constructor ADDITIVE_CATEGORY
  field
    category : M.Identifier
    hasZeroObject : HasZeroObjectProperty
    enrichment    : EnrichedOverProperty
    biproductWitnesses : List BiproductObject
-- CATEGORY: Zero object, Ab-enrichment, all binary biproducts.

-- Structural theorem: Additivity equivalence via canonical biproduct map
record AdditivityEquivalenceTheorem : Set where
  constructor THEOREM_AdditivityEquivalence
  field
    category : M.Identifier
    premiseHasFinProducts : Bool
    premiseHasFinCoproducts : Bool
    canonicalMapIsomorphismWitness : M.Identifier
-- CATEGORY: Canonical map Coproduct -> Product is iso iff category additive.

------------------------------------------------------------------------
-- Section 2.3: Additive functors
------------------------------------------------------------------------

-- Functor additivity property: F is ADDITIVE
record FunctorAdditiveProperty : Set where
  constructor _is_ADDITIVE
  field
    functor : M.Identifier
    source  : M.Identifier
    target  : M.Identifier
    homGroupPreservationWitness : M.Identifier
-- CATEGORY: Induced maps on hom-sets are group homomorphisms.

-- Additive functor biproduct preservation equivalence theorem
record AdditiveFunctorBiproductEquivalenceTheorem : Set where
  constructor THEOREM_AdditiveFunctorBiproductEquivalence
  field
    functor : M.Identifier
    source  : M.Identifier
    target  : M.Identifier
    preservesBiproductWitness : M.Identifier
-- CATEGORY: Functor additive iff it preserves biproducts.

-- Canonical example: Hom_C(A,-) is additive
record HomFunctorIsAdditiveTheorem : Set where
  constructor THEOREM_HomFunctorIsAdditive
  field
    category : M.Identifier
    objectA  : M.Identifier
    homFunctor : M.Identifier
    bilinearityWitness : M.Identifier
-- CATEGORY: Bilinearity of composition ensures additivity.

------------------------------------------------------------------------
-- Section 2.4: Abelian categories
------------------------------------------------------------------------

-- Normal monomorphism property
record NormalMonomorphismProperty : Set where
  constructor _is_NORMAL_MONOMORPHISM
  field
    mono          : M.Identifier  -- m
    kernelCokernelWitness : M.Identifier -- witness of m ≅ ker(coker(m))
-- CATEGORY: Mono arises as kernel of its cokernel.

-- Normal epimorphism property (dual)
record NormalEpimorphismProperty : Set where
  constructor _is_NORMAL_EPIMORPHISM
  field
    epi           : M.Identifier  -- e
    cokernelKernelWitness : M.Identifier -- witness of e ≅ coker(ker(e))
-- CATEGORY: Epi arises as cokernel of its kernel.

-- Abelian category declaration
record AbelianCategoryDeclaration : Set where
  constructor ABELIAN_CATEGORY
  field
    category : M.Identifier
    additive : AdditiveCategoryDeclaration
    hasAllKernels : Bool
    hasAllCokernels : Bool
    monosNormalWitness : Bool
    episNormalWitness  : Bool
-- CATEGORY: Additive + all kernels/cokernels + normality of monos/epis.

-- Coimage and Image constructs
record CoimageOfMorphism : Set where
  constructor Coim
  field
    morphism : M.Identifier
    quotientObject : M.Identifier -- coker(ker(f))

record ImageOfMorphism : Set where
  constructor Im
  field
    morphism : M.Identifier
    subobjectMono : M.Identifier -- ker(coker(f))

-- First isomorphism theorem (coimage ≅ image)
record FirstIsomorphismTheoremForCategories : Set where
  constructor THEOREM_FirstIsomorphismTheoremForCategories
  field
    category : M.Identifier
    morphism : M.Identifier
    isoWitness : M.Identifier
-- CATEGORY: Coim(f) ≅ Im(f) encapsulating exactness.

-- Canonical examples: Ab, R-Mod
record AbelianCategoryExampleAb : Set where
  constructor ABELIAN_CATEGORY_Ab
  field unit : ⊤

record AbelianCategoryExampleRMod : Set where
  constructor ABELIAN_CATEGORY_RMod
  field ring : M.Identifier
               ; unit : ⊤

------------------------------------------------------------------------
-- Section 2.5 & 2.6: Exactness & additivity properties (reinforcement)
------------------------------------------------------------------------

-- Re-declared Coim/Im constructors (exactness focus)
record CoimConstructor : Set where
  constructor Coim'
  field morphism : M.Identifier; domain : M.Identifier; codomain : M.Identifier

record ImConstructor : Set where
  constructor Im'
  field morphism : M.Identifier; domain : M.Identifier; codomain : M.Identifier

-- Canonical morphism f_bar : Coim(f) -> Im(f)
record CanonicalMorphism_f_bar : Set where
  constructor f_bar
  field
    morphism : M.Identifier
    sourceCoimage : M.Identifier
    targetImage   : M.Identifier
    factorizationWitness : M.Identifier

record FirstIsomorphismForAbelianCategoriesTheorem : Set where
  constructor THEOREM_FirstIsomorphismForAbelianCategories
  field
    category : M.Identifier
    morphism : M.Identifier
    isomorphismWitness : M.Identifier
-- CATEGORY: f_bar(f) iso for all f.

-- Canonical map from coproduct to product
record CanonicalMapCoproductToProduct : Set where
  constructor canonical_map
  field
    coproductObj : M.Identifier
    productObj   : M.Identifier
    map          : M.Identifier

record AdditivityViaBiproductCoincidenceTheorem : Set where
  constructor THEOREM_AdditivityViaBiproductCoincidence
  field
    category : M.Identifier
    hasZeroObject : Bool
    hasFiniteProducts : Bool
    hasFiniteCoproducts : Bool
    comparisonMapIsIsoWitness : M.Identifier
-- CATEGORY: Additive iff comparison map iso.

------------------------------------------------------------------------
-- Section 2.7: Union of subobjects (modular lattice)
------------------------------------------------------------------------

-- Join (union) of subobjects
record UnionOfSubobjects : Set where
  constructor Join
  field
    ambientObject : M.Identifier
    subobjectDomainCoproduct : M.Identifier
    inducedMap : M.Identifier
    imageMono  : M.Identifier
-- CATEGORY: Image of universal coproduct map.

-- Meet (intersection) of subobjects
record IntersectionOfSubobjects : Set where
  constructor Meet
  field
    ambientObject : M.Identifier
    pullbackObject : M.Identifier
    limitConeWitness : M.Identifier
-- CATEGORY: Wide pullback of monomorphisms.

-- Modular lattice property
record ModularLatticeProperty : Set where
  constructor _is_MODULAR_LATTICE
  field lattice : M.Identifier; modularLawWitness : M.Identifier

record SubobjectLatticeModularityTheorem : Set where
  constructor THEOREM_SubobjectLatticeInAbelianIsModular
  field
    category : M.Identifier
    object   : M.Identifier
    modularityWitness : M.Identifier
-- CATEGORY: Sub(X) modular for all X.

------------------------------------------------------------------------
-- Section 2.8: Exact sequences (SES, splitting lemma, long exact sequences)
------------------------------------------------------------------------

-- Exactness at an object
record ExactSequenceAtProperty : Set where
  constructor _is_EXACT_AT_
  field
    sequenceId : M.Identifier
    objectAt   : M.Identifier
    imageMono  : M.Identifier
    kernelMono : M.Identifier
    comparisonIso : M.Identifier

-- Short exact sequence (SES)
record ShortExactSequenceDeclaration : Set where
  constructor SHORT_EXACT_SEQUENCE
  field
    zeroObj   : M.Identifier
    A         : M.Identifier
    B         : M.Identifier
    C         : M.Identifier
    monoAB    : M.Identifier  -- A -> B
    epiBC     : M.Identifier  -- B -> C
    exactAtA  : Bool
    exactAtB  : Bool
    exactAtC  : Bool

-- Split mono / split epi properties
record HasRetractionProperty : Set where
  constructor _has_retraction
  field mono : M.Identifier; retraction : M.Identifier

record HasSectionProperty : Set where
  constructor _has_section
  field epi : M.Identifier; section : M.Identifier

-- Splitting lemma theorem
record SplittingLemmaForSESTheorem : Set where
  constructor THEOREM_SplittingLemmaForSES
  field
    sequence : ShortExactSequenceDeclaration
    hasRetractionWitness : M.Identifier
    hasSectionWitness    : M.Identifier
    biproductIsoWitness  : M.Identifier
-- CATEGORY: Conditions equivalent to splitting.

-- Long exact sequence declaration
record LongExactSequenceDeclaration : Set where
  constructor LONG_EXACT_SEQUENCE
  field
    objectsChain : List M.Identifier
    exactnessFlags : List Bool  -- positional flags for internal exactness

------------------------------------------------------------------------
-- Section 2.9 & 2.10: Diagram chasing & lemmas
------------------------------------------------------------------------

-- Proof by diagram chase technique
record ProofByDiagramChaseTechnique : Set where
  constructor PROOF_by_DIAGRAM_CHASE
  field justification : M.Identifier

-- Diagrammatic assertion (premises container)
record DiagrammaticAssertion : Set where
  constructor DIAGRAMMATIC_ASSERTION
  field
    name      : M.Identifier
    diagramId : M.Identifier
    premises  : List M.Identifier

-- Generic theorem from diagram chase
record DiagramChaseTheorem : Set where
  constructor THEOREM_from_DIAGRAM_CHASE
  field
    assertion : DiagrammaticAssertion
    conclusion : M.Identifier
    proofTechnique : ProofByDiagramChaseTechnique

-- Concrete lemmas
record FiveLemmaTheorem : Set where
  constructor THEOREM_TheFiveLemma
  field
    setup : DiagrammaticAssertion
    conclusionIso : M.Identifier
    proof : ProofByDiagramChaseTechnique

record SnakeLemmaTheorem : Set where
  constructor THEOREM_TheSnakeLemma
  field
    setup : DiagrammaticAssertion
    connectingHom : M.Identifier
    exactnessWitness : M.Identifier
    proof : ProofByDiagramChaseTechnique

record ThreeByThreeLemmaTheorem : Set where
  constructor THEOREM_The3x3Lemma
  field
    setup : DiagrammaticAssertion
    topRowSESWitness : M.Identifier
    proof : ProofByDiagramChaseTechnique

------------------------------------------------------------------------
-- Section 2.11: Exact functors (left/right/exact preservation)
------------------------------------------------------------------------

-- Left exact property (kernel preservation)
record LeftExactFunctorProperty : Set where
  constructor _is_LEFT_EXACT_within_Abelian
  field
    functor : M.Identifier; source : M.Identifier; target : M.Identifier
    preservesKernelsWitness : M.Identifier

-- Right exact property (cokernel preservation)
record RightExactFunctorProperty : Set where
  constructor _is_RIGHT_EXACT_within_Abelian
  field
    functor : M.Identifier; source : M.Identifier; target : M.Identifier
    preservesCokernelsWitness : M.Identifier

-- Exact functor property (both)
record ExactFunctorProperty : Set where
  constructor _is_EXACT_FUNCTOR_within_Abelian
  field
    functor : M.Identifier; source : M.Identifier; target : M.Identifier
    leftExactWitness  : LeftExactFunctorProperty
    rightExactWitness : RightExactFunctorProperty

-- Exactness via preservation of SES
record ExactnessViaShortExactSequencesTheorem : Set where
  constructor THEOREM_ExactnessViaShortExactSequences
  field
    functor : M.Identifier; source : M.Identifier; target : M.Identifier
    sesPreservationWitness : M.Identifier

-- Covariant Hom left exactness
record CovariantHomIsLeftExactTheorem : Set where
  constructor THEOREM_CovariantHomIsLeftExact
  field
    category : M.Identifier
    objectA  : M.Identifier
    homFunctor : M.Identifier
    kernelPreservationWitness : M.Identifier

-- Projective characterization via exact Hom
record ExactHomCharacterizesProjectivesTheorem : Set where
  constructor THEOREM_ExactHomCharacterizesProjectives
  field
    objectP : M.Identifier
    category : M.Identifier
    homFunctor : M.Identifier
    equivalenceWitness : M.Identifier

-- Injective duality inference
record InjectiveTheoryDualityInference : Set where
  constructor INFER_DUAL_THEORY_InjectiveTheoryFromHom_FROM_ProjectiveTheoryFromHom
  field
    projectiveTheoryFragment : M.Identifier
    injectiveTheoryFragment  : M.Identifier

------------------------------------------------------------------------
-- Section 2.12: Torsion theories (orthogonality & decomposition)
------------------------------------------------------------------------

-- Orthogonality T ⊥ F
record OrthogonalObjectClassesProperty : Set where
  constructor _⊥_
  field
    torsionClass     : M.Identifier
    torsionFreeClass : M.Identifier
    homOrthogonalityWitness : M.Identifier

-- (Further torsion theory constructs would continue here: decomposition sequence, etc.)
-- Torsion decomposition sequence: 0 -> tX -> X -> fX -> 0
record TorsionDecompositionSequence : Set where
  constructor TORSION_DECOMPOSITION_SEQ
  field
    category      : M.Identifier
    objectX       : M.Identifier
    torsionPart   : M.Identifier -- tX
    torsionFreePart : M.Identifier -- fX
    monoTorsionInclusion : M.Identifier -- tX -> X
    epiQuotientProjection : M.Identifier -- X -> fX
    shortExactWitness : M.Identifier -- witness of 0→tX→X→fX→0 exactness
    torsionClassId     : M.Identifier -- identifier for torsion class T
    torsionFreeClassId : M.Identifier -- identifier for torsion-free class F

-- Torsion theory declaration bundling orthogonality + universal decomposition
record TorsionTheoryDeclaration : Set where
  constructor TORSION_THEORY
  field
    category      : M.Identifier
    torsionClass  : M.Identifier
    torsionFreeClass : M.Identifier
    orthogonality : OrthogonalObjectClassesProperty
    decompositionSequences : List TorsionDecompositionSequence
    closureQuotientsWitness : M.Identifier  -- T closed under quotients
    closureSubobjectsWitness : M.Identifier -- F closed under subobjects
    reflectiveWitness  : M.Identifier       -- T coreflective / left adjoint inclusion
    coreflectiveWitness : M.Identifier      -- F reflective / right adjoint inclusion

------------------------------------------------------------------------
-- Bridge postulates placeholder (integration with unified proof layer pending)
------------------------------------------------------------------------

postulate
  -- Placeholder examples (to be refined when proof layer extended for Chapter 2)
  zeroObjectBridge
    : (prop : HasZeroObjectProperty)
    -> C.Proof (C.ZeroObjectS (HasZeroObjectProperty.category prop)
                               (HasZeroObjectProperty.zeroObj prop)) C.ZeroObjectPropertyName

  kernelAsEqualizerBridge
    : (def : KernelAsEqualizerDefinition)
    -> C.Proof (C.KernelEqualizerS (KernelAsEqualizerDefinition.morphism def)
                                   (KernelAsEqualizerDefinition.domain def)) C.KernelAsEqualizerName

  cokernelAsCoequalizerBridge
    : (inf : CokernelAsCoequalizerInference)
    -> C.Proof (C.CokernelCoequalizerS (KernelAsEqualizerDefinition.morphism (CokernelAsCoequalizerInference.originalKernelDef inf))
                                       (KernelAsEqualizerDefinition.domain (CokernelAsCoequalizerInference.originalKernelDef inf))) C.CokernelAsCoequalizerName

  additiveCategoryBridge
    : (decl : AdditiveCategoryDeclaration)
    -> C.Proof (C.AdditiveCategoryS (AdditiveCategoryDeclaration.category decl)) C.AdditiveCategoryName

  abelianCategoryBridge
    : (decl : AbelianCategoryDeclaration)
    -> C.Proof (C.AbelianCategoryS (AbelianCategoryDeclaration.category decl)) C.AbelianCategoryName

  biproductComparisonBridge
    : (thm : AdditivityEquivalenceTheorem)
    -> C.Proof (C.BiproductComparisonS (AdditivityEquivalenceTheorem.category thm)) C.AdditivityViaBiproductMapIsoName

  coimageImageIsoBridge
    : (thm : FirstIsomorphismTheoremForCategories)
    -> C.Proof (C.CoimageImageIsoS (FirstIsomorphismTheoremForCategories.morphism thm)
                                    (FirstIsomorphismTheoremForCategories.category thm)) C.CoimageImageIsomorphismName

  shortExactSequenceBridge
    : (ses : ShortExactSequenceDeclaration)
    -> C.Proof (C.ShortExactSequenceS (ShortExactSequenceDeclaration.B ses)) C.ShortExactSequenceExactnessName

  splittingLemmaBridge
    : (thm : SplittingLemmaForSESTheorem)
    -> C.Proof (C.SplittingLemmaS (ShortExactSequenceDeclaration.B (SplittingLemmaForSESTheorem.sequence thm))) C.SplittingLemmaName

  leftExactFunctorBridge
    : (prop : LeftExactFunctorProperty)
    -> C.Proof (C.FunctorExactnessS (LeftExactFunctorProperty.functor prop)
                                    (LeftExactFunctorProperty.source prop)
                                    (LeftExactFunctorProperty.target prop)) C.LeftExactFunctorName

  rightExactFunctorBridge
    : (prop : RightExactFunctorProperty)
    -> C.Proof (C.FunctorExactnessS (RightExactFunctorProperty.functor prop)
                                    (RightExactFunctorProperty.source prop)
                                    (RightExactFunctorProperty.target prop)) C.RightExactFunctorName

  exactFunctorBridge
    : (prop : ExactFunctorProperty)
    -> C.Proof (C.FunctorExactnessS (ExactFunctorProperty.functor prop)
                                    (ExactFunctorProperty.source prop)
                                    (ExactFunctorProperty.target prop)) C.ExactFunctorName

  covariantHomLeftExactBridge
    : (thm : CovariantHomIsLeftExactTheorem)
    -> C.Proof (C.FunctorExactnessS (CovariantHomIsLeftExactTheorem.homFunctor thm)
                                    (CovariantHomIsLeftExactTheorem.category thm)
                                    (CovariantHomIsLeftExactTheorem.category thm)) C.CovariantHomLeftExactName

  exactHomProjectiveCharBridge
    : (thm : ExactHomCharacterizesProjectivesTheorem)
    -> C.Proof (C.FunctorExactnessS (ExactHomCharacterizesProjectivesTheorem.homFunctor thm)
                                    (ExactHomCharacterizesProjectivesTheorem.category thm)
                                    (ExactHomCharacterizesProjectivesTheorem.category thm)) C.ExactHomProjectiveCharacterizationName

  subobjectLatticeModularBridge
    : (thm : SubobjectLatticeModularityTheorem)
    -> C.Proof (C.SubobjectLatticeS (SubobjectLatticeModularityTheorem.category thm)
                                     (SubobjectLatticeModularityTheorem.object thm)) C.SubobjectLatticeModularName

  fiveLemmaBridge
    : (thm : FiveLemmaTheorem)
    -> C.Proof (C.DiagramLemmaS (FiveLemmaTheorem.setup thm .DiagrammaticAssertion.name)) C.FiveLemmaName

  snakeLemmaBridge
    : (thm : SnakeLemmaTheorem)
    -> C.Proof (C.DiagramLemmaS (SnakeLemmaTheorem.setup thm .DiagrammaticAssertion.name)) C.SnakeLemmaName

  threeByThreeLemmaBridge
    : (thm : ThreeByThreeLemmaTheorem)
    -> C.Proof (C.DiagramLemmaS (ThreeByThreeLemmaTheorem.setup thm .DiagrammaticAssertion.name)) C.ThreeByThreeLemmaName
  
  torsionDecompositionBridge
    : (seq : TorsionDecompositionSequence)
    -> C.Proof (C.TorsionDecompositionS (TorsionDecompositionSequence.objectX seq)
                                        (TorsionDecompositionSequence.torsionPart seq)
                                        (TorsionDecompositionSequence.torsionFreePart seq)
                                        (TorsionDecompositionSequence.category seq)) C.TorsionDecompositionName
  
  torsionClassClosureQuotientsBridge
    : (decl : TorsionTheoryDeclaration)
    -> C.Proof (C.TorsionClassClosureQuotientsS (TorsionTheoryDeclaration.torsionClass decl)
                                               (TorsionTheoryDeclaration.category decl)) C.TorsionClassClosedUnderQuotientsName

  torsionFreeClosureSubobjectsBridge
    : (decl : TorsionTheoryDeclaration)
    -> C.Proof (C.TorsionFreeClosureSubobjectsS (TorsionTheoryDeclaration.torsionFreeClass decl)
                                               (TorsionTheoryDeclaration.category decl)) C.TorsionFreeClosedUnderSubobjectsName

  torsionReflectiveBridge
    : (decl : TorsionTheoryDeclaration)
    -> C.Proof (C.TorsionReflectiveS (TorsionTheoryDeclaration.torsionClass decl)
                                     (TorsionTheoryDeclaration.category decl)) C.TorsionReflectiveName

  torsionFreeCoreflectiveBridge
    : (decl : TorsionTheoryDeclaration)
    -> C.Proof (C.TorsionFreeCoreflectiveS (TorsionTheoryDeclaration.torsionFreeClass decl)
                                          (TorsionTheoryDeclaration.category decl)) C.TorsionFreeCoreflectiveName

  torsionTheoryAxiomsBridge
    : (decl : TorsionTheoryDeclaration)
    -> C.Proof (C.TorsionTheoryS (TorsionTheoryDeclaration.torsionClass decl)
                                 (TorsionTheoryDeclaration.torsionFreeClass decl)
                                 (TorsionTheoryDeclaration.category decl)) C.TorsionTheoryAxiomsName

------------------------------------------------------------------------
-- End of structural encoding for Abelian categories & related concepts
------------------------------------------------------------------------
