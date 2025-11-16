-- Level2_6: Enriched Category Theory (Chapter 2, Section 6)
-- This module encodes the structural content of Section 6 from the EBNF grammar.
-- It covers symmetric monoidal closed categories, enriched categories, change of base,
-- enriched functors, natural transformations, adjunctions, the Yoneda lemma,
-- tensors, cotensors, and weighted limits.

module Chapter2.Level2sub6 where

open import Core

-- ============================================================================
-- Section 6.1: Symmetric Monoidal Closed Categories
-- ============================================================================

-- Part 1: Monoidal Category

-- Data for a monoidal category
record MonoidalCategoryData : Set where
  field
    underlyingCategory : CategoryDeclaration
    tensorProduct : Set  -- Bifunctor ⊗ : C × C → C
    unitObject : ObjectDeclaration

-- The associator natural isomorphism
record AssociatorDeclaration : Set where
  field
    monoidalCategory : MonoidalCategoryData
    -- α : (A ⊗ B) ⊗ C ≅ A ⊗ (B ⊗ C)
    naturalIsomorphism : Set

-- The left unitor natural isomorphism
record LeftUnitorDeclaration : Set where
  field
    monoidalCategory : MonoidalCategoryData
    -- λ : I ⊗ A ≅ A
    naturalIsomorphism : Set

-- The right unitor natural isomorphism
record RightUnitorDeclaration : Set where
  field
    monoidalCategory : MonoidalCategoryData
    -- ρ : A ⊗ I ≅ A
    naturalIsomorphism : Set

-- Pentagon axiom for associator coherence
record PentagonAxiom : Set where
  field
    monoidalCategory : MonoidalCategoryData
    associator : AssociatorDeclaration
    -- For all A,B,C,D: pentagon diagram commutes
    diagramCommutes : Set

-- Triangle axiom for unitor coherence
record TriangleAxiom : Set where
  field
    monoidalCategory : MonoidalCategoryData
    associator : AssociatorDeclaration
    leftUnitor : LeftUnitorDeclaration
    rightUnitor : RightUnitorDeclaration
    -- For all A,B: triangle diagram commutes
    diagramCommutes : Set

-- Complete monoidal category definition
record MonoidalCategoryDeclaration : Set where
  field
    data : MonoidalCategoryData
    associator : AssociatorDeclaration
    leftUnitor : LeftUnitorDeclaration
    rightUnitor : RightUnitorDeclaration
    pentagonAxiom : PentagonAxiom
    triangleAxiom : TriangleAxiom

-- Part 2: Symmetric Monoidal Category

-- The braiding natural isomorphism
record BraidingDeclaration : Set where
  field
    monoidalCategory : MonoidalCategoryData
    -- γ : A ⊗ B ≅ B ⊗ A
    naturalIsomorphism : Set
    -- Symmetry: γ_{B,A} ∘ γ_{A,B} = id_{A⊗B}
    symmetryCondition : Set

-- Hexagon axiom for braiding coherence
record HexagonAxiom : Set where
  field
    monoidalCategory : MonoidalCategoryData
    associator : AssociatorDeclaration
    braiding : BraidingDeclaration
    -- For all A,B,C: hexagon diagram commutes
    diagramCommutes : Set

-- Symmetric monoidal category
record SymmetricMonoidalCategoryDeclaration : Set where
  field
    monoidalCategory : MonoidalCategoryDeclaration
    braiding : BraidingDeclaration
    hexagonAxiom : HexagonAxiom

-- Part 3: Symmetric Monoidal Closed Category

-- Internal hom-object (exponential object)
record InternalHomObjectDeclaration : Set where
  field
    category : CategoryDeclaration
    sourceObject : ObjectDeclaration
    targetObject : ObjectDeclaration
    -- [B, C] is an object in the category
    internalHomObject : ObjectDeclaration

-- Tensor-Hom adjunction theorem
record TensorHomAdjunctionTheorem : Set where
  field
    category : SymmetricMonoidalCategoryDeclaration
    -- (- ⊗ B) ⊣ [B, -]
    leftAdjoint : FunctorDeclaration
    rightAdjoint : FunctorDeclaration
    adjunction : AdjunctionDeclaration
    -- Hom(A ⊗ B, C) ≅ Hom(A, [B, C])
    adjunctionIsomorphism : Set

-- Symmetric monoidal closed category
record SymmetricMonoidalClosedCategoryDeclaration : Set where
  field
    symmetricMonoidalCategory : SymmetricMonoidalCategoryDeclaration
    internalHom : Set  -- Family of internal hom objects
    tensorHomAdjunction : TensorHomAdjunctionTheorem

-- ============================================================================
-- Section 6.2: Enriched Categories (V-Categories)
-- ============================================================================

-- Part 1: Data of a V-Category

-- Hom-object declaration
record HomObjectDeclaration : Set where
  field
    sourceObject : ObjectDeclaration
    targetObject : ObjectDeclaration
    -- C(A, B) is an object in the enriching category V
    homObjectInV : ObjectDeclaration

-- Composition morphism in the enriching category
record CompositionMorphismDeclaration : Set where
  field
    objectA : ObjectDeclaration
    objectB : ObjectDeclaration
    objectC : ObjectDeclaration
    -- compose : C(B,C) ⊗ C(A,B) → C(A,C) in V
    compositionMorphismInV : MorphismDeclaration

-- Identity morphism in the enriching category
record IdentityMorphismDeclaration_Enriched : Set where
  field
    object : ObjectDeclaration
    -- id_A : I → C(A,A) in V
    identityMorphismInV : MorphismDeclaration

-- Complete data for a V-category
record EnrichedCategoryData : Set where
  field
    enrichingCategory : MonoidalCategoryDeclaration
    objects : Set  -- Collection of objects
    homObjects : Set  -- Family of HomObjectDeclaration
    compositionMorphisms : Set  -- Family of CompositionMorphismDeclaration
    identityMorphisms : Set  -- Family of IdentityMorphismDeclaration_Enriched

-- Part 2: Axioms of a V-Category

-- Enriched associativity axiom
record EnrichedAssociativityAxiom : Set where
  field
    enrichedData : EnrichedCategoryData
    -- Associativity pentagon commutes in V
    diagramCommutesInV : Set

-- Enriched unitality axiom
record EnrichedUnitalityAxiom : Set where
  field
    enrichedData : EnrichedCategoryData
    -- Unit law triangles commute in V
    leftUnitalityDiagram : Set
    rightUnitalityDiagram : Set

-- Complete V-category definition
record EnrichedCategoryDeclaration : Set where
  field
    data : EnrichedCategoryData
    associativityAxiom : EnrichedAssociativityAxiom
    unitalityAxiom : EnrichedUnitalityAxiom

-- ============================================================================
-- Section 6.2 (continued): Change of Base
-- ============================================================================

-- Part 1: Strong Monoidal Functors

-- Strong monoidal functor between monoidal categories
record StrongMonoidalFunctorDeclaration : Set where
  field
    sourceCategory : MonoidalCategoryDeclaration
    targetCategory : MonoidalCategoryDeclaration
    underlyingFunctor : FunctorDeclaration
    -- μ_{A,B} : F(A) ⊗_W F(B) → F(A ⊗_V B)
    monoidalStructure : Set
    -- ε : I_W → F(I_V)
    unitStructure : Set
    -- Coherence conditions for μ and ε
    coherenceConditions : Set

-- Part 2: Change of Base Construction

-- Change of base: transforms a V-category into a W-category
record ChangeOfBaseConstructor : Set where
  field
    sourceCategoryV : MonoidalCategoryDeclaration
    targetCategoryW : MonoidalCategoryDeclaration
    monoidalFunctor : StrongMonoidalFunctorDeclaration
    vCategory : EnrichedCategoryDeclaration
    -- The resulting W-category F_*(C)
    resultWCategory : EnrichedCategoryDeclaration

-- Part 3: Underlying Ordinary Category

-- The underlying Set-enriched category of a V-category
record UnderlyingOrdinaryCategory : Set where
  field
    vCategory : EnrichedCategoryDeclaration
    enrichingCategory : SymmetricMonoidalClosedCategoryDeclaration
    -- Hom_V(I, -) : V → Set functor
    homFunctor : FunctorDeclaration
    -- Result is equivalent to change of base along Hom_V(I, -)
    underlyingCategory : CategoryDeclaration
    changeOfBaseEquivalence : Set

-- ============================================================================
-- Section 6.3 & 6.7: Enriched Functors, Natural Transformations, and Adjunctions
-- ============================================================================

-- Part 1: Enriched Functors (V-Functors)

-- Data for a V-functor
record EnrichedFunctorData : Set where
  field
    sourceCategory : EnrichedCategoryDeclaration
    targetCategory : EnrichedCategoryDeclaration
    enrichingCategory : MonoidalCategoryDeclaration
    -- Action on objects
    actionOnObjects : Set
    -- Action on hom-objects: F_{A,B} : C(A,B) → D(F(A),F(B)) in V
    actionOnHomObjects : Set

-- V-functor preserves composition axiom
record EnrichedFunctorAxiom_Composition : Set where
  field
    functorData : EnrichedFunctorData
    -- Composition preservation diagram commutes in V
    diagramCommutes : Set

-- V-functor preserves identity axiom
record EnrichedFunctorAxiom_Identity : Set where
  field
    functorData : EnrichedFunctorData
    -- Identity preservation diagram commutes in V
    diagramCommutes : Set

-- Complete V-functor definition
record EnrichedFunctorDeclaration : Set where
  field
    data : EnrichedFunctorData
    preservesComposition : EnrichedFunctorAxiom_Composition
    preservesIdentity : EnrichedFunctorAxiom_Identity

-- Part 2: Enriched Natural Transformations

-- V-naturality axiom
record V_NaturalityAxiom : Set where
  field
    sourceFunctor : EnrichedFunctorDeclaration
    targetFunctor : EnrichedFunctorDeclaration
    components : Set  -- Family of morphisms α_A : I → D(F(A), G(A)) in V
    -- V-enriched naturality square commutes
    diagramCommutes : Set

-- V-natural transformation
record EnrichedNaturalTransformation : Set where
  field
    sourceFunctor : EnrichedFunctorDeclaration
    targetFunctor : EnrichedFunctorDeclaration
    components : Set
    naturalityAxiom : V_NaturalityAxiom

-- Part 3: Enriched Adjunctions

-- V-adjunction between V-functors
record EnrichedAdjunctionDeclaration : Set where
  field
    leftAdjoint : EnrichedFunctorDeclaration
    rightAdjoint : EnrichedFunctorDeclaration
    -- D(F(A), B) ≅ C(A, G(B)) as V-natural isomorphism
    homObjectIsomorphism : Set
    naturalityInV : Set

-- Part 4: Enriched Yoneda Lemma

-- Category of V-enriched presheaves
record EnrichedPresheafCategory : Set where
  field
    baseCategory : EnrichedCategoryDeclaration
    enrichingCategory : SymmetricMonoidalClosedCategoryDeclaration
    -- [C^op, V] is a V-category
    presheafCategoryStructure : EnrichedCategoryDeclaration

-- Enriched Yoneda isomorphism theorem
record EnrichedYonedaIsomorphismTheorem : Set where
  field
    baseCategory : EnrichedCategoryDeclaration
    enrichingCategory : SymmetricMonoidalClosedCategoryDeclaration
    yonedaEmbedding : EnrichedFunctorDeclaration
    objectA : ObjectDeclaration
    presheafP : EnrichedFunctorDeclaration
    -- [C^op,V](y(A), P) ≅ P(A) in V
    yonedaIsomorphism : Set

-- Corollary: Yoneda embedding is fully faithful
record EnrichedYonedaEmbeddingIsFullyFaithfulCorollary : Set where
  field
    yonedaTheorem : EnrichedYonedaIsomorphismTheorem
    -- y : C → [C^op, V] is fully faithful
    fullyFaithful : Set

-- ============================================================================
-- Section 6.5 & 6.6: Tensors, Cotensors, and Weighted Limits
-- ============================================================================

-- Part 1: Tensors (Copowers) and Cotensors (Powers)

-- Tensor (copower): v ⊗ X
record TensorDeclaration : Set where
  field
    enrichingCategory : SymmetricMonoidalClosedCategoryDeclaration
    vCategory : EnrichedCategoryDeclaration
    objectV : ObjectDeclaration  -- v in V
    objectC : ObjectDeclaration  -- X in C
    -- Universal property: C(v⊗X, Y) ≅ V(v, C(X,Y)) in V
    universalProperty : Set

-- Cotensor (power): [v, X]
record CotensorDeclaration : Set where
  field
    enrichingCategory : SymmetricMonoidalClosedCategoryDeclaration
    vCategory : EnrichedCategoryDeclaration
    objectV : ObjectDeclaration  -- v in V
    objectC : ObjectDeclaration  -- X in C
    -- Universal property: C(Y, [v,X]) ≅ V(v, C(Y,X)) in V
    universalProperty : Set

-- Part 2: Weighted Limits and Colimits

-- Weighted limit: {W, D}
record WeightedLimitDeclaration : Set where
  field
    enrichingCategory : SymmetricMonoidalClosedCategoryDeclaration
    indexCategory : EnrichedCategoryDeclaration
    targetCategory : EnrichedCategoryDeclaration
    diagram : EnrichedFunctorDeclaration  -- D : J → C
    weight : EnrichedFunctorDeclaration    -- W : J^op → V
    -- Universal property: C(X, {W,D}) ≅ [J^op, V](W, C(X,D(-))) in V
    universalProperty : Set

-- Weighted colimit: {W * D}
record WeightedColimitDeclaration : Set where
  field
    enrichingCategory : SymmetricMonoidalClosedCategoryDeclaration
    indexCategory : EnrichedCategoryDeclaration
    targetCategory : EnrichedCategoryDeclaration
    diagram : EnrichedFunctorDeclaration  -- D : J → C
    weight : EnrichedFunctorDeclaration    -- W : J → V
    -- Universal property: C({W*D}, X) ≅ [J, V](W, C(D(-),X)) in V
    universalProperty : Set

-- Part 3: Unification Theorems

-- Theorem: Tensors are weighted colimits
record TensorsAreWeightedColimitsTheorem : Set where
  field
    tensor : TensorDeclaration
    -- Construction showing v⊗X ≅ {W*D} for specific W and D
    terminalVCategory : EnrichedCategoryDeclaration
    diagramSelectingX : EnrichedFunctorDeclaration
    weightSelectingV : EnrichedFunctorDeclaration
    equivalentColimit : WeightedColimitDeclaration
    isomorphism : Set

-- Dual theorem: Cotensors are weighted limits
record CotensorsAreWeightedLimitsTheorem : Set where
  field
    cotensor : CotensorDeclaration
    -- Construction showing [v,X] ≅ {W,D} for specific W and D
    terminalVCategory : EnrichedCategoryDeclaration
    diagramSelectingX : EnrichedFunctorDeclaration
    weightSelectingV : EnrichedFunctorDeclaration
    equivalentLimit : WeightedLimitDeclaration
    isomorphism : Set

-- ============================================================================
-- Instances and Examples
-- ============================================================================

-- Example: Set as a symmetric monoidal closed category
record SetAsSymmetricMonoidalClosedInstance : Set where
  field
    -- Set with Cartesian product and singleton set
    underlyingCategory : CategoryDeclaration
    cartesianProduct : Set
    singletonSet : ObjectDeclaration
    isSymmetricMonoidalClosed : SymmetricMonoidalClosedCategoryDeclaration

-- Example: Ordinary categories are Set-categories
record OrdinaryCategoriesAreSetCategoriesInstance : Set where
  field
    ordinaryCategory : CategoryDeclaration
    -- The enrichment over Set
    setEnrichment : EnrichedCategoryDeclaration
    equivalence : Set

-- ============================================================================
-- End of Level2_6
-- ============================================================================
