{-# OPTIONS --without-K #-}

-- | Chapter 1 §2: product/coproduct universal property scaffolding—declarations,
--   axioms, and proof hooks that refine the Level1 identifiers into concrete
--   limit/colimit witnesses.
module Chapter1.Level1sub2 where

open import Agda.Builtin.Unit     using (⊤; tt)
open import Agda.Builtin.String   using (String)
open import Agda.Builtin.List     using (List; []; _∷_)
open import Core.Phase using (Bool; true; false)
open import Agda.Builtin.Equality using (_≡_)

open import Metamodel as M
open import Core      as C

-- Local helpers

-- A simple stand-in for expression-like terms appearing in structural equations
Expr : Set
Expr = String

------------------------------------------------------------------------
-- Part 1: Products and the UMP
------------------------------------------------------------------------

record ProductObjectDeclaration : Set where
  constructor _is_PRODUCT_of_
  field P : M.Identifier
        parts : List M.Identifier
-- CATEGORY: P is the apex of a limit cone over the discrete diagram {A_i}.

-- | Named projection morphisms out of a product.
data ProjectionMorphism : Set where
  proj_ : M.Identifier -> ProjectionMorphism

-- | Universal mediating morphism into a product.
record UniversalProductMorphism : Set where
  constructor ⟨_⟩
  field components : M.NonEmpty Expr

-- | Axiom: existence of a universal morphism into a product.
record UMP_ProductExistenceAxiom : Set where
  constructor AXIOM_ProductCommutativity
  field P : M.Identifier
        apex : M.Identifier
        legs : List M.Identifier
-- CATEGORY: The universal morphism u makes each triangle commute: proj Ai ∘ u = f_i.

-- | Axiom: uniqueness of the universal morphism into a product.
record UMP_ProductUniquenessAxiom : Set where
  constructor AXIOM_ProductUniqueness
  field u′ : Expr
        u  : Expr
-- CATEGORY: Any mediating morphism u′ equals the universal u.

-- Bridges from Level 2 axioms to Level 1 structured proofs
postulate
  productCommutativityProof
    : (decl : ProductObjectDeclaration)
    -> (ax : UMP_ProductExistenceAxiom)
    -> C.Proof (C.ProductS (ProductObjectDeclaration.P decl)
                            (ProductObjectDeclaration.parts decl))
               C.ProductCommutativityName

  productUniquenessProof
    : (decl : ProductObjectDeclaration)
    -> (ax : UMP_ProductUniquenessAxiom)
    -> C.Proof (C.ProductS (ProductObjectDeclaration.P decl)
                            (ProductObjectDeclaration.parts decl))
               C.ProductUniquenessName

-- | Binary product declaration wrapper.
record BinaryProduct : Set where
  constructor BINARY_PRODUCT
  field A B : M.Identifier

-- | Nullary product equals the terminal object.
record NullaryProductIsTerminalAxiom : Set where
  constructor AXIOM_NullaryProductIsTerminal
  field unit : ⊤
-- CATEGORY: PRODUCT of () equals TERMINAL_OBJECT.

-- Coproduct via duality (derived theorems and axioms)
-- | Coproduct object declaration (dual of product).
record CoproductObjectDeclaration : Set where
  constructor _is_COPRODUCT_of_
  field Q : M.Identifier
        parts : List M.Identifier

-- | Axiom: coproduct injections form a cocone making triangles commute.
record CoproductCommutativityAxiom : Set where
  constructor AXIOM_CoproductCommutativity
  field cocone : M.Identifier
        injOf  : List M.Identifier

-- | Axiom: mediating map into a coproduct is unique.
record CoproductUniquenessAxiom : Set where
  constructor AXIOM_CoproductUniqueness
  field mediator′ : Expr
        mediator  : Expr

-- | Binary coproduct declaration wrapper.
record BinaryCoproduct : Set where
  constructor _+_
  field A B : M.Identifier

-- | Nullary coproduct equals the initial object.
record NullaryCoproductIsInitialAxiom : Set where
  constructor AXIOM_NullaryCoproductIsInitial
  field unit : ⊤

-- Bridges for coproduct axioms to Level 1 structured proofs
postulate
  coproductCommutativityProof
    : (decl : CoproductObjectDeclaration)
    -> (ax : CoproductCommutativityAxiom)
    -> C.Proof (C.CoproductS (CoproductObjectDeclaration.Q decl)
                              (CoproductObjectDeclaration.parts decl))
               C.CoproductCommutativityName

  coproductUniquenessProof
    : (decl : CoproductObjectDeclaration)
    -> (ax : CoproductUniquenessAxiom)
    -> C.Proof (C.CoproductS (CoproductObjectDeclaration.Q decl)
                              (CoproductObjectDeclaration.parts decl))
               C.CoproductUniquenessName

------------------------------------------------------------------------
-- Part 2: Initial and Terminal Objects
------------------------------------------------------------------------

data ObjectProperty : Set where
  INITIAL_OBJECT  : ObjectProperty
  TERMINAL_OBJECT : ObjectProperty

record ObjectPropertyDeclaration : Set where
  constructor _is_
  field X : M.Identifier
        P : ObjectProperty

-- | Unique morphism from an initial object to any X.
record UniversalInitialMorphism : Set where
  constructor unique_from_initial_to
  field X : M.Identifier

-- | Axiom: any two arrows from initial object coincide.
record InitialUniquenessAxiom : Set where
  constructor AXIOM_InitialUniqueness
  field f : M.Identifier
        X : M.Identifier

-- | Unique morphism to a terminal object from any X.
record UniversalTerminalMorphism : Set where
  constructor unique_to_terminal_from
  field X : M.Identifier

-- | Axiom: any two arrows into terminal object coincide.
record TerminalUniquenessAxiom : Set where
  constructor AXIOM_TerminalUniqueness
  field f : M.Identifier
        X : M.Identifier

-- | Theorem: any two initial objects are isomorphic.
record InitialObjectsAreIsomorphic : Set where
  constructor THEOREM_InitialObjectsAreIsomorphic
  field I J : M.Identifier

-- | Theorem: terminal object is the nullary product.
record TerminalIsNullaryProduct : Set where
  constructor THEOREM_TerminalIsNullaryProduct
  field T : M.Identifier

-- | Theorem: initial object is the nullary coproduct.
record InitialIsNullaryCoproduct : Set where
  constructor THEOREM_InitialIsNullaryCoproduct
  field I : M.Identifier

-- Bridges for initial/terminal uniqueness axioms to Level 1 structured proofs
postulate
  initialUniquenessProof
    : (decl : ObjectPropertyDeclaration)  -- expects decl.P ≡ INITIAL_OBJECT (informally)
    -> (ax : InitialUniquenessAxiom)
    -> C.Proof (C.InitialS (ObjectPropertyDeclaration.X decl)) C.InitialUniquenessName

  terminalUniquenessProof
    : (decl : ObjectPropertyDeclaration)  -- expects decl.P ≡ TERMINAL_OBJECT (informally)
    -> (ax : TerminalUniquenessAxiom)
    -> C.Proof (C.TerminalS (ObjectPropertyDeclaration.X decl)) C.TerminalUniquenessName

------------------------------------------------------------------------
-- Part 3: Diagram Shapes and (Co)Limits
------------------------------------------------------------------------

-- | Named morphism declaration (domain/codomain).
record MorphismDecl : Set where
  constructor MORPHISM_DECL
  field name dom cod : M.Identifier

-- | Small diagram shape described by its morphisms.
record DiagramShapeDeclaration : Set where
  constructor DIAGRAM_SHAPE_from
  field name : M.Identifier
        morphisms : List MorphismDecl

-- Canonical small shapes
-- | Parallel pair shape A ⇉ B.
record DiagramShape_ParallelPair : Set where
  constructor ParallelPair
  field A B f g : M.Identifier

-- | Cospan shape A → C ← B.
record DiagramShape_Cospan : Set where
  constructor Cospan
  field A B C f g : M.Identifier

-- | Span shape A ← C → B.
record DiagramShape_Span : Set where
  constructor Span
  field A B C f g : M.Identifier

-- Limit constructions
-- | Define an equalizer as a limit of a parallel pair.
record EqualizerAsLimit : Set where
  constructor DEFINE_Equalizer_as_LIMIT_of_ParallelPair
  field f g : M.Identifier

-- | Equalizer commutativity square.
record EqualizerCommutativityAxiom : Set where
  constructor AXIOM_EqualizerCommutativity
  field f e g : M.Identifier

-- | Define a pullback as a limit of a cospan.
record PullbackAsLimit : Set where
  constructor DEFINE_Pullback_as_LIMIT_of_Cospan
  field f g : M.Identifier

-- | Pullback square commutativity.
record PullbackCommutativityAxiom : Set where
  constructor AXIOM_PullbackSquare
  field f p1 g p2 : M.Identifier

-- Colimit constructions (duals)
-- | Define a coequalizer as a colimit of a parallel pair.
record CoequalizerAsColimit : Set where
  constructor DEFINE_Coequalizer_as_COLIMIT_of_ParallelPair
  field f g : M.Identifier

-- | Coequalizer commutativity square.
record CoequalizerCommutativityAxiom : Set where
  constructor AXIOM_CoequalizerCommutativity
  field q f g : M.Identifier

-- | Define a pushout as a colimit of a span.
record PushoutAsColimit : Set where
  constructor DEFINE_Pushout_as_COLIMIT_of_Span
  field f g : M.Identifier

-- | Pushout square commutativity.
record PushoutCommutativityAxiom : Set where
  constructor AXIOM_PushoutSquare
  field i1 f i2 g : M.Identifier

-- Diagrams, (co)cones, and categories of cones
-- | Diagram functor D : J → C.
record DiagramDeclaration : Set where
  constructor DIAGRAM_is_Functor
  field D J C : M.Identifier

-- | Cone over a diagram with apex and legs.
record ConeDeclaration : Set where
  constructor CONE_over_has
  field coneId : M.Identifier
        D      : M.Identifier
        apex   : M.Identifier
        legs   : List MorphismDecl

-- | Category of cones over a diagram.
record CategoryOfCones : Set where
  constructor ConeCategory
  field D : M.Identifier

-- | Limit as terminal object in the cone category.
record LimitAsTerminalInConeCategory : Set where
  constructor LIMIT_of_is_TERMINAL_within_ConeCategory
  field D : M.Identifier
        Lcone : M.Identifier

-- | Cocone over a diagram with apex and legs.
record CoconeDeclaration : Set where
  constructor COCONE_from_has
  field coconeId : M.Identifier
        D        : M.Identifier
        apex     : M.Identifier
        legs     : List MorphismDecl

-- | Category of cocones over a diagram.
record CategoryOfCocones : Set where
  constructor CoconeCategory
  field D : M.Identifier

-- | Colimit as initial object in the cocone category.
record ColimitAsInitialInCoconeCategory : Set where
  constructor COLIMIT_of_is_INITIAL_within_CoconeCategory
  field D : M.Identifier
        Lcocone : M.Identifier

------------------------------------------------------------------------
-- Part 4: Completeness and Cocompleteness
------------------------------------------------------------------------

-- | Property: category C is small.
record CategoryProperty_Small : Set where
  constructor _is_SMALL
  field C : M.Identifier

-- | Small diagram declaration D : J → C.
record SmallDiagramDeclaration : Set where
  constructor SMALL_DIAGRAM_from_to
  field D J C : M.Identifier

-- | Property: category C is complete.
record CategoryProperty_Complete : Set where
  constructor _is_COMPLETE
  field C : M.Identifier

-- | Property: category C is cocomplete.
record CategoryProperty_Cocomplete : Set where
  constructor _is_COCOMPLETE
  field C : M.Identifier

-- | Theorem: criteria witnessing completeness of C.
record CompletenessCriteria : Set where
  constructor THEOREM_CompletenessCriteria
  field C : M.Identifier

-- | Theorem: equivalence of completeness formulations.
record CompletenessEquivalenceTheorem : Set where
  constructor THEOREM_CompletenessEquivalenceTheorem
  field C : M.Identifier

-- | Construct limits from products and equalizers.
record GeneralLimitConstructor : Set where
  constructor CONSTRUCT_LIMIT_from_ProductsAndEqualizers
  field D : M.Identifier

-- | Product over the objects of a diagram.
record ProductOfDiagramObjects : Set where
  constructor Product_over_Objects
  field D : M.Identifier

-- | Parallel morphism construction toward an equalizer.
record ParallelMorphismConstruction : Set where
  constructor PARALLEL_MORPHISMS_for
  field D : M.Identifier
        uα uβ : M.Identifier

-- | Equalizer construction step inside the general limit build.
record EqualizerAsLimitStep : Set where
  constructor Equalizer_step
  field α β : M.Identifier

------------------------------------------------------------------------
-- Part 5: Colimit Adjunction and Dual Limit Adjunction
------------------------------------------------------------------------

-- | Diagonal functor Δ : C → C^J.
record DiagonalFunctorDeclaration : Set where
  constructor DIAGONAL_FUNCTOR_Δ
  field C CJ : M.Identifier

-- | Colimit functor colim : C^J → C.
record ColimitFunctorDeclaration : Set where
  constructor COLIMIT_FUNCTOR_colim
  field CJ C : M.Identifier

-- | Axiom: cocones correspond to natural transformations from ΔX.
record CoconeAsNaturalTransformationAxiom : Set where
  constructor AXIOM_CoconeAsNaturalTransformation
  field D X : M.Identifier

-- | Adjunction colim ⊣ Δ.
record AdjunctionDeclaration : Set where
  constructor ADJUNCTION_colim_⊣_Δ
  field unit : ⊤

-- | Dual adjunction for limits.
record LimitAdjunctionDual : Set where
  constructor INFER_DUAL_THEOREM_LimitAdjunction
  field unit : ⊤

------------------------------------------------------------------------
-- Part 6: Functor properties (preserve/reflect/create limits)
------------------------------------------------------------------------

-- | Functor preserves all limits.
record FunctorPreservesLimits : Set where
  constructor _preserves_LIMITS
  field F : M.Identifier

-- | Functor reflects limits.
record FunctorReflectsLimits : Set where
  constructor _reflects_LIMITS
  field F : M.Identifier

-- | Functor creates limits.
record FunctorCreatesLimits : Set where
  constructor _creates_LIMITS
  field F : M.Identifier

-- | Creation implies reflection of limits.
record CreationImpliesReflection : Set where
  constructor THEOREM_CreationImpliesReflection
  field F : M.Identifier

-- | Isomorphisms of categories reflect limits.
record IsomorphismsOfCategoriesReflectLimits : Set where
  constructor THEOREM_IsomorphismsOfCategoriesReflectLimits
  field F : M.Identifier

-- | Right adjoints preserve limits.
record RightAdjointsPreserveLimits_L2 : Set where
  constructor THEOREM_RightAdjointsPreserveLimits_L2
  field F : M.Identifier

------------------------------------------------------------------------
-- Part 7: Absolute colimits and split coequalizers
------------------------------------------------------------------------

-- | Predicate stating F preserves colimit of D.
record PreservationPredicate : Set where
  constructor _preserves_COLIMIT_of_
  field F D : M.Identifier

-- | Definition of an absolute colimit of D.
record AbsoluteColimitDefinition : Set where
  constructor _is_ABSOLUTE_COLIMIT_of_
  field L D : M.Identifier

-- | Split coequalizer cocone witness.
record SplitCoequalizerCocone : Set where
  constructor _is_SPLIT_COEQUALIZER_COCONE_for_
  field q fg : M.Identifier

-- | Theorem: absolute colimits are split.
record AbsoluteColimitsAreSplit : Set where
  constructor THEOREM_AbsoluteColimitsAreSplit
  field C D : M.Identifier

------------------------------------------------------------------------
-- Part 8: Filtered categories and commuting results
------------------------------------------------------------------------

record CategoryIsFiltered : Set where
  constructor _is_FILTERED
  field J : M.Identifier

record CategoryAxiom_UpperBoundsForObjects : Set where
  constructor AXIOM_UpperBoundsForObjects_within
  field J : M.Identifier

record CategoryAxiom_EqualizingMorphisms : Set where
  constructor AXIOM_EqualizingMorphisms_within
  field J : M.Identifier

record CategoryIsFinite : Set where
  constructor _is_FINITE
  field J : M.Identifier

record FilteredColimitsCommuteWithFiniteLimitsInSet : Set where
  constructor THEOREM_FilteredColimitsCommuteWithFiniteLimitsInSet
  field I J : M.Identifier

record DualityMapping_Filtered_Cofiltered : Set where
  constructor DUALITY_MAPPING_FILTERED_COFILTERED
  field unit : ⊤

record CofilteredLimitsCommuteWithFiniteColimits : Set where
  constructor INFER_DUAL_THEOREM_CofilteredLimitsCommuteWithFiniteColimits
  field unit : ⊤

------------------------------------------------------------------------
-- Part 9: Final functors and equivalence
------------------------------------------------------------------------

record OverCommaCategory : Set where
  constructor _↓_
  field U k : M.Identifier

record FunctorProperty_Final : Set where
  constructor _is_FINAL
  field U : M.Identifier

record FunctorBehavior_PreservesColimitsOnPrecomposition : Set where
  constructor _preserves_colimits_on_precomposition
  field U : M.Identifier

record FinalityEquivalence : Set where
  constructor THEOREM_FinalityEquivalence
  field U : M.Identifier

record DualityMapping_Final_Initial : Set where
  constructor DUALITY_MAPPING_FINAL_INITIAL
  field unit : ⊤

record InitialityEquivalence : Set where
  constructor INFER_DUAL_THEOREM_InitialityEquivalence
  field unit : ⊤

------------------------------------------------------------------------
-- Notes: This module formalizes Limits and Colimits with structural records.
-- Many fields are simplified to Identifiers and Strings (Expr) to keep
-- the module self-contained. CATEGORY narratives are preserved as comments.
------------------------------------------------------------------------
