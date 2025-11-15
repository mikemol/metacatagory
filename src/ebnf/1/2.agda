module Level1_2 where

open import Agda.Builtin.Unit     using (⊤; tt)
open import Agda.Builtin.String   using (String)
open import Agda.Builtin.List     using (List; []; _∷_)
open import Agda.Builtin.Bool     using (Bool)
open import Agda.Builtin.Equality using (_≡_)

open import Metamodel as M
open import Core      as C

-- Local helpers

-- A simple stand-in for expression-like terms appearing in EBNF equations
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

data ProjectionMorphism : Set where
  proj_ : M.Identifier -> ProjectionMorphism

record UniversalProductMorphism : Set where
  constructor ⟨_⟩
  field components : M.NonEmpty Expr

record UMP_ProductExistenceAxiom : Set where
  constructor AXIOM_ProductCommutativity
  field P : M.Identifier
        apex : M.Identifier
        legs : List M.Identifier
-- CATEGORY: The universal morphism u makes each triangle commute: proj Ai ∘ u = f_i.

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

record BinaryProduct : Set where
  constructor _×_
  field A B : M.Identifier

record NullaryProductIsTerminalAxiom : Set where
  constructor AXIOM_NullaryProductIsTerminal
  field unit : ⊤
-- CATEGORY: PRODUCT of () equals TERMINAL_OBJECT.

-- Coproduct via duality (derived theorems and axioms)
record CoproductObjectDeclaration : Set where
  constructor _is_COPRODUCT_of_
  field Q : M.Identifier
        parts : List M.Identifier

record CoproductCommutativityAxiom : Set where
  constructor AXIOM_CoproductCommutativity
  field cocone : M.Identifier
        injOf  : List M.Identifier

record CoproductUniquenessAxiom : Set where
  constructor AXIOM_CoproductUniqueness
  field mediator′ : Expr
        mediator  : Expr

record BinaryCoproduct : Set where
  constructor _+_
  field A B : M.Identifier

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

record UniversalInitialMorphism : Set where
  constructor unique_from_initial_to
  field X : M.Identifier

record InitialUniquenessAxiom : Set where
  constructor AXIOM_InitialUniqueness
  field f : M.Identifier
        X : M.Identifier

record UniversalTerminalMorphism : Set where
  constructor unique_to_terminal_from
  field X : M.Identifier

record TerminalUniquenessAxiom : Set where
  constructor AXIOM_TerminalUniqueness
  field f : M.Identifier
        X : M.Identifier

record InitialObjectsAreIsomorphic : Set where
  constructor THEOREM_InitialObjectsAreIsomorphic
  field I J : M.Identifier

record TerminalIsNullaryProduct : Set where
  constructor THEOREM_TerminalIsNullaryProduct
  field T : M.Identifier

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

record MorphismDecl : Set where
  constructor _:_→_
  field name dom cod : M.Identifier

record DiagramShapeDeclaration : Set where
  constructor DIAGRAM_SHAPE_from
  field name : M.Identifier
        morphisms : List MorphismDecl

-- Canonical small shapes
record DiagramShape_ParallelPair : Set where
  constructor ParallelPair
  field A B f g : M.Identifier

record DiagramShape_Cospan : Set where
  constructor Cospan
  field A B C f g : M.Identifier

record DiagramShape_Span : Set where
  constructor Span
  field A B C f g : M.Identifier

-- Limit constructions
record EqualizerAsLimit : Set where
  constructor DEFINE_Equalizer_as_LIMIT_of_ParallelPair
  field f g : M.Identifier

record EqualizerCommutativityAxiom : Set where
  constructor AXIOM_EqualizerCommutativity
  field f e g : M.Identifier

record PullbackAsLimit : Set where
  constructor DEFINE_Pullback_as_LIMIT_of_Cospan
  field f g : M.Identifier

record PullbackCommutativityAxiom : Set where
  constructor AXIOM_PullbackSquare
  field f p1 g p2 : M.Identifier

-- Colimit constructions (duals)
record CoequalizerAsColimit : Set where
  constructor DEFINE_Coequalizer_as_COLIMIT_of_ParallelPair
  field f g : M.Identifier

record CoequalizerCommutativityAxiom : Set where
  constructor AXIOM_CoequalizerCommutativity
  field q f g : M.Identifier

record PushoutAsColimit : Set where
  constructor DEFINE_Pushout_as_COLIMIT_of_Span
  field f g : M.Identifier

record PushoutCommutativityAxiom : Set where
  constructor AXIOM_PushoutSquare
  field i1 f i2 g : M.Identifier

-- Diagrams, (co)cones, and categories of cones
record DiagramDeclaration : Set where
  constructor DIAGRAM_is_Functor
  field D J C : M.Identifier

record ConeDeclaration : Set where
  constructor CONE_over_has
  field coneId : M.Identifier
        D      : M.Identifier
        apex   : M.Identifier
        legs   : List MorphismDecl

record CategoryOfCones : Set where
  constructor ConeCategory
  field D : M.Identifier

record LimitAsTerminalInConeCategory : Set where
  constructor LIMIT_of_is_TERMINAL_in_ConeCategory
  field D : M.Identifier
        Lcone : M.Identifier

record CoconeDeclaration : Set where
  constructor COCONE_from_has
  field coconeId : M.Identifier
        D        : M.Identifier
        apex     : M.Identifier
        legs     : List MorphismDecl

record CategoryOfCocones : Set where
  constructor CoconeCategory
  field D : M.Identifier

record ColimitAsInitialInCoconeCategory : Set where
  constructor COLIMIT_of_is_INITIAL_in_CoconeCategory
  field D : M.Identifier
        Lcocone : M.Identifier

------------------------------------------------------------------------
-- Part 4: Completeness and Cocompleteness
------------------------------------------------------------------------

record CategoryProperty_Small : Set where
  constructor _is_SMALL
  field C : M.Identifier

record SmallDiagramDeclaration : Set where
  constructor small_diagram_∶_→_
  field D J C : M.Identifier

record CategoryProperty_Complete : Set where
  constructor _is_COMPLETE
  field C : M.Identifier

record CategoryProperty_Cocomplete : Set where
  constructor _is_COCOMPLETE
  field C : M.Identifier

record CompletenessCriteria : Set where
  constructor THEOREM_CompletenessCriteria
  field C : M.Identifier

record CompletenessEquivalenceTheorem : Set where
  constructor THEOREM_CompletenessEquivalenceTheorem
  field C : M.Identifier

record GeneralLimitConstructor : Set where
  constructor CONSTRUCT_LIMIT_from_ProductsAndEqualizers
  field D : M.Identifier

record ProductOfDiagramObjects : Set where
  constructor Product_over_Objects
  field D : M.Identifier

record ParallelMorphismConstruction : Set where
  constructor PARALLEL_MORPHISMS_for
  field D : M.Identifier
        uα uβ : M.Identifier

record EqualizerAsLimitStep : Set where
  constructor Equalizer_step
  field α β : M.Identifier

------------------------------------------------------------------------
-- Part 5: Colimit Adjunction and Dual Limit Adjunction
------------------------------------------------------------------------

record DiagonalFunctorDeclaration : Set where
  constructor DIAGONAL_FUNCTOR_Δ
  field C CJ : M.Identifier

record ColimitFunctorDeclaration : Set where
  constructor COLIMIT_FUNCTOR_colim
  field CJ C : M.Identifier

record CoconeAsNaturalTransformationAxiom : Set where
  constructor AXIOM_CoconeAsNaturalTransformation
  field D X : M.Identifier

record AdjunctionDeclaration : Set where
  constructor ADJUNCTION_colim_⊣_Δ
  field unit : ⊤

record LimitAdjunctionDual : Set where
  constructor INFER_DUAL_THEOREM_LimitAdjunction
  field unit : ⊤

------------------------------------------------------------------------
-- Part 6: Functor properties (preserve/reflect/create limits)
------------------------------------------------------------------------

record FunctorPreservesLimits : Set where
  constructor _preserves_LIMITS
  field F : M.Identifier

record FunctorReflectsLimits : Set where
  constructor _reflects_LIMITS
  field F : M.Identifier

record FunctorCreatesLimits : Set where
  constructor _creates_LIMITS
  field F : M.Identifier

record CreationImpliesReflection : Set where
  constructor THEOREM_CreationImpliesReflection
  field F : M.Identifier

record IsomorphismsOfCategoriesReflectLimits : Set where
  constructor THEOREM_IsomorphismsOfCategoriesReflectLimits
  field F : M.Identifier

record RightAdjointsPreserveLimits : Set where
  constructor THEOREM_RightAdjointsPreserveLimits
  field F : M.Identifier

------------------------------------------------------------------------
-- Part 7: Absolute colimits and split coequalizers
------------------------------------------------------------------------

record PreservationPredicate : Set where
  constructor _preserves_COLIMIT_of_
  field F D : M.Identifier

record AbsoluteColimitDefinition : Set where
  constructor _is_ABSOLUTE_COLIMIT_of_
  field L D : M.Identifier

record SplitCoequalizerCocone : Set where
  constructor _is_SPLIT_COEQUALIZER_COCONE_for_
  field q fg : M.Identifier

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
  constructor AXIOM_UpperBoundsForObjects_in
  field J : M.Identifier

record CategoryAxiom_EqualizingMorphisms : Set where
  constructor AXIOM_EqualizingMorphisms_in
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
-- Notes: This module mirrors src/ebnf/1/2.ebnf with structural records.
-- Many fields are simplified to Identifiers and Strings (Expr) to keep
-- the module self-contained. CATEGORY narratives are preserved as comments.
------------------------------------------------------------------------
