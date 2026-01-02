{-# OPTIONS --without-K #-}

-- | Chapter 1 §6: exactness toolkit—finite limits, (left/right) exact functors,
--   duality bridges, and homological lemmas captured as structured declarations.
module Chapter1.Level1sub6 where

open import Agda.Builtin.Unit     using (⊤; tt)
open import Agda.Builtin.String   using (String)
open import Agda.Builtin.List     using (List; []; _∷_)
open import Core.Phase using (Bool; true; false)
open import Agda.Builtin.Equality using (_≡_)

open import Metamodel as M
open import Core      as C

-- Lightweight expression placeholder
Expr6 : Set
Expr6 = String

------------------------------------------------------------------------
-- Part 1: Exact Functors (Section 6.1)
------------------------------------------------------------------------

-- CATEGORY: The property of an index category J being small in the strongest sense.
record CategoryIsFinite6 : Set where
  constructor _is_FINITE
  field category : M.Identifier

-- CATEGORY: A limit of a diagram whose shape J is a finite category.
record FiniteLimit : Set where
  constructor FINITE_LIMIT
  field
    limit : M.Identifier
    diagram : M.Identifier
    indexCategory : M.Identifier

-- CATEGORY: The property of a functor being 'left continuous'.
record FunctorIsLeftExact : Set where
  constructor _is_LEFT_EXACT
  field functor : M.Identifier

-- CATEGORY: The dual property of being 'right continuous'.
record FunctorIsRightExact : Set where
  constructor _is_RIGHT_EXACT
  field functor : M.Identifier

-- CATEGORY: The powerful property of a functor that preserves all finite
-- limit and colimit structures.
record FunctorIsExact : Set where
  constructor _is_EXACT
  field
    functor : M.Identifier
    leftExact : FunctorIsLeftExact
    rightExact : FunctorIsRightExact

-- CATEGORY: Duality mapping between left and right exactness
record DualityMappingLeftRightExact : Set where
  constructor DUALITY_MAPPING_FOR_LEFT_EXACT_IS_RIGHT_EXACT
  field unit : ⊤

-- CATEGORY: A classic and practical result in homological algebra.
record LeftExactnessViaKernels : Set where
  constructor THEOREM_LeftExactnessViaKernels
  field
    abelianCategory : M.Identifier
    functor : M.Identifier

------------------------------------------------------------------------
-- Part 2: Left Exact Reflection (Section 6.2)
------------------------------------------------------------------------

-- CATEGORY: The category of left exact functors from C to D.
record LeftExactFunctorSubcategory : Set where
  constructor Lex
  field
    domain : M.Identifier
    codomain : M.Identifier

-- CATEGORY: The fundamental theorem asserting that the subcategory of
-- left exact functors is reflective.
record LexIsReflective : Set where
  constructor THEOREM_LexIsReflective
  field
    domain : M.Identifier
    codomain : M.Identifier

-- CATEGORY: The left exact reflection of the functor F.
record LeftExactReflection : Set where
  constructor L_lex
  field functor : M.Identifier

-- CATEGORY: The universal natural transformation η_F : F ⇒ L(F).
record LeftExactReflectionArrow : Set where
  constructor lex_reflection_of
  field functor : M.Identifier

------------------------------------------------------------------------
-- Part 3: Flat Functors (Section 6.3)
------------------------------------------------------------------------

-- CATEGORY: The property of a presheaf F that makes it behave like a 'flat module'.
record FunctorIsFlat : Set where
  constructor _is_FLAT
  field presheaf : M.Identifier

-- CATEGORY: The representable presheaf on C associated with the object X.
record RepresentableFunctor : Set where
  constructor y
  field object : M.Identifier

-- CATEGORY: The property of a presheaf being constructible as a 'direct limit'.
record FunctorAsFilteredColimitOfRepresentables : Set where
  constructor _is_FilteredColimitOfRepresentables
  field functor : M.Identifier

-- CATEGORY: A fundamental theorem of presheaf theory.
record FlatFunctorEquivalence : Set where
  constructor THEOREM_FlatFunctorEquivalence
  field functor : M.Identifier

------------------------------------------------------------------------
-- Part 4: Regular Cardinals (Section 6.4)
------------------------------------------------------------------------

-- CATEGORY: A type of 'well-behaved' infinite cardinal.
record RegularCardinal : Set where
  constructor REGULAR_CARDINAL
  field cardinal : M.Identifier

-- CATEGORY: A limit whose diagram shape is 'small' relative to the regular cardinal α.
record AlphaLimit : Set where
  constructor α_LIMIT
  field
    limit : M.Identifier
    cardinal : RegularCardinal

-- CATEGORY: The generalization of a filtered category to size α.
record AlphaFilteredCategory : Set where
  constructor α_FILTERED_CATEGORY
  field
    category : M.Identifier
    cardinal : RegularCardinal

-- CATEGORY: The property of a functor preserving all limits of size less than α.
record FunctorIsAlphaContinuous : Set where
  constructor _is_α_CONTINUOUS
  field
    functor : M.Identifier
    cardinal : RegularCardinal

-- CATEGORY: The property of a functor being 'flat' up to size α.
record FunctorIsAlphaFlat : Set where
  constructor _is_α_FLAT
  field
    functor : M.Identifier
    cardinal : RegularCardinal

-- CATEGORY: The generalization of the Flat Functor Equivalence theorem.
record AlphaFunctorEquivalenceGeneralization : Set where
  constructor THEOREM_AlphaFunctorEquivalenceGeneralization
  field cardinal : RegularCardinal

------------------------------------------------------------------------
-- Part 5: Splitting of Idempotents (Section 6.5)
------------------------------------------------------------------------

-- CATEGORY: An endomorphism that acts like a projection operator.
record MorphismIsIdempotent : Set where
  constructor _is_IDEMPOTENT
  field morphism : M.Identifier

-- CATEGORY: A concrete realization of the abstract idempotent e.
record SplittingOfIdempotent : Set where
  constructor SPLITTING_OF_is
  field
    idempotent : M.Identifier
    retraction : M.Identifier
    section : M.Identifier
    intermediateObject : M.Identifier

-- CATEGORY: An object in the Karoubi envelope.
record KaroubiObject : Set where
  constructor KarObj
  field
    obj : M.Identifier
    idem : M.Identifier

-- CATEGORY: A morphism in the Karoubi envelope.
record KaroubiMorphism : Set where
  constructor KarMor
  field
    kMor : M.Identifier
    source : KaroubiObject
    target : KaroubiObject

-- CATEGORY: The Karoubi envelope of C.
record KaroubiEnvelope : Set where
  constructor Kar
  field category : M.Identifier

-- CATEGORY: The property of a category being 'idempotent complete'.
record CategoryIsCauchyComplete : Set where
  constructor _is_CAUCHY_COMPLETE
  field category : M.Identifier

-- CATEGORY: A foundational result connecting different forms of completeness.
record EqualizersImplyCauchyCompleteness : Set where
  constructor THEOREM_EqualizersImplyCauchyCompleteness
  field category : M.Identifier

-- CATEGORY: The canonical Yoneda-like embedding of a category C into its Karoubi envelope.
record KaroubiEmbeddingFunctor : Set where
  constructor Y_Kar
  field
    source : M.Identifier
    target : KaroubiEnvelope

-- CATEGORY: The refined assertion that the Karoubi envelope is the universal solution
-- to making a category Cauchy complete.
record KaroubiEnvelopeIsUniversalRefined : Set where
  constructor THEOREM_KaroubiEnvelopeIsUniversal_Refined
  field category : M.Identifier

------------------------------------------------------------------------
-- Part 6: General Adjoint Functor Theorem (Section 6.6)
------------------------------------------------------------------------

-- CATEGORY: A proof that the property of completeness is 'lifted' from a functor's domain D
-- to the comma category (c↓G).
record InheritedCompletenessOfCommaCategory : Set where
  constructor THEOREM_InheritedCompletenessOfCommaCategory
  field
    domain : M.Identifier
    codomain : M.Identifier
    functor : M.Identifier

-- CATEGORY: A formalization of the complete proof strategy for the General Adjoint Functor Theorem.
record GeneralAdjointFunctorTheoremConstructiveProof : Set where
  constructor PROOF_OF_GAFT
  field
    functor : M.Identifier
    domain : M.Identifier
    codomain : M.Identifier

-- CATEGORY: The core constructive step of the GAFT proof.
record InitialObjectConstructorFromWeaklyInitialSet : Set where
  constructor ConstructInitialObject_within_USING
  field
    category : M.Identifier
    weaklyInitialSet : M.Identifier

-- CATEGORY: An assertion of definitional equivalence.
record UniversalArrowFromInitialObject : Set where
  constructor UniversalArrow_for_is_InitialObject_of
  field
    object : M.Identifier
    commaCategory : M.Identifier

-- CATEGORY: The theorem which provides the final step of the GAFT proof.
record AdjunctionsFromUniversalArrows : Set where
  constructor THEOREM_AdjunctionsFromUniversalArrows
  field functor : M.Identifier

------------------------------------------------------------------------
-- Bridge postulates: Connect axiom records to typed Proof witnesses
------------------------------------------------------------------------

open C using (Subject; AxiomName; Proof)
open C using (CategoryPropertyS; FunctorMapS)
open C using (PreservesCompositionName; PreservesIdentityName)

postulate
  -- Left exactness via kernels proof
  leftExactnessViaKernelsProof
    : (thm : LeftExactnessViaKernels)
    -> (F C D : M.Identifier)
    -> Proof (FunctorMapS F C D) PreservesCompositionName

  -- Lex is reflective proof
  lexIsReflectiveProof
    : (thm : LexIsReflective)
    -> (C D : M.Identifier)
    -> Proof (CategoryPropertyS C) C.SubobjectLatticeCompletenessName

  -- Flat functor equivalence proof
  flatFunctorEquivalenceProof
    : (thm : FlatFunctorEquivalence)
    -> (F C D : M.Identifier)
    -> Proof (FunctorMapS F C D) PreservesCompositionName

  -- Alpha functor equivalence generalization proof
  alphaFunctorEquivalenceProof
    : (thm : AlphaFunctorEquivalenceGeneralization)
    -> (C : M.Identifier)
    -> Proof (CategoryPropertyS C) C.SubobjectLatticeCompletenessName

  -- Equalizers imply Cauchy completeness proof
  equalizersImplyCauchyProof
    : (thm : EqualizersImplyCauchyCompleteness)
    -> (C : M.Identifier)
    -> Proof (CategoryPropertyS C) C.SubobjectLatticeCompletenessName

  -- Karoubi envelope is universal refined proof
  karoubiEnvelopeUniversalProof
    : (thm : KaroubiEnvelopeIsUniversalRefined)
    -> (C : M.Identifier)
    -> Proof (CategoryPropertyS C) C.SubobjectLatticeCompletenessName

  -- Inherited completeness of comma category proof
  inheritedCompletenessProof
    : (thm : InheritedCompletenessOfCommaCategory)
    -> (D : M.Identifier)
    -> Proof (CategoryPropertyS D) C.SubobjectLatticeCompletenessName

  -- Adjunctions from universal arrows proof
  adjunctionsFromUniversalArrowsProof
    : (thm : AdjunctionsFromUniversalArrows)
    -> (F G C D : M.Identifier)
    -> Proof (C.AdjunctionS F G C D) C.TriangleIdentitiesName

------------------------------------------------------------------------
-- Notes: Structural encoding of Exact Functors and Regular Cardinals, left exact
-- reflection, flat functors, regular cardinals, Cauchy completeness, and the
-- general adjoint functor theorem. CATEGORY prose preserved as comments.
-- Bridge postulates connect theorem records to Core.Proof.
------------------------------------------------------------------------
