-- Level2_8: Fibred Categories (Chapter 2, Section 8)
-- This module encodes the structural content of Section 8 from the EBNF grammar.
-- It covers fibrations, Cartesian arrows, Cartesian functors, Grothendieck construction,
-- fibred adjunctions, completeness criteria, locally small fibrations, and definability.

module Chapter2.Level2sub8 where

open import Core
open import Chapter1.Level1Index  -- Not public to avoid conflicts
open import Metamodel as M

-- ============================================================================
-- Section 8.1: Fibrations
-- ============================================================================

-- Part 1: Cartesian Arrows and the Definition of a Fibration

-- Fibration projection functor
record FibrationProjectionFunctor : Set₁ where
  field
    totalCategory : CategoryDeclaration
    baseCategory : CategoryDeclaration
    -- p : E → B
    projectionFunctor : M.Identifier

-- A morphism lies over another morphism
record MorphismLiesOver : Set₁ where
  field
    projectionFunctor : FibrationProjectionFunctor
    morphismInTotal : M.Identifier
    morphismInBase : M.Identifier
    -- f lies over u iff p(f) = u
    projectionEquals : Set

-- Cartesian arrow (universal lifting property)
record CartesianArrow : Set₁ where
  field
    projectionFunctor : FibrationProjectionFunctor
    arrow : M.Identifier  -- f : Y → X in E
    targetObject : M.Identifier  -- X
    baseMorphism : M.Identifier  -- u : p(Y) → p(X) in B
    -- f lies over u
    liesOver : MorphismLiesOver
    -- Universal property: for all g : Z → X lying over u ∘ w,
    -- there exists unique vertical h : Z → Y such that g = f ∘ h
    universalProperty : Set

-- Fibration declaration
record FibrationDeclaration : Set₁ where
  field
    projectionFunctor : FibrationProjectionFunctor
    -- For every object X in E and morphism u : I → p(X) in B,
    -- there exists a Cartesian arrow f : Y → X lifting u
    cartesianLiftsExist : Set

-- Part 2: Fibres and Reindexing Functors

-- Fibre category over an object in the base
record FibreCategory : Set₁ where
  field
    fibration : FibrationDeclaration
    baseObject : M.Identifier
    -- Subcategory of E consisting of objects lying over I
    -- and vertical morphisms (lying over identity)
    fibreCategoryStructure : CategoryDeclaration

-- Reindexing functor (pullback functor)
record ReindexingFunctor : Set₁ where
  field
    fibration : FibrationDeclaration
    baseMorphism : M.Identifier  -- u : I → J in B
    -- u* : E_J → E_I
    -- Constructed by choosing Cartesian lifts
    underlyingFunctor : M.Identifier
    sourceFibre : FibreCategory
    targetFibre : FibreCategory

-- Part 3: The Dual Theory (Opfibrations)

-- coCartesian arrow (dual to Cartesian arrow)
record CoCartesianArrow : Set₁ where
  field
    projectionFunctor : FibrationProjectionFunctor
    arrow : M.Identifier  -- f : X → Y in E
    sourceObject : M.Identifier  -- X
    baseMorphism : M.Identifier  -- u : p(X) → p(Y) in B
    -- f lies over u
    liesOver : MorphismLiesOver
    -- Universal property (dual to Cartesian)
    universalProperty : Set

-- Opfibration declaration
record OpfibrationDeclaration : Set₁ where
  field
    projectionFunctor : FibrationProjectionFunctor
    -- For every object X in E and morphism u : p(X) → J in B,
    -- there exists a coCartesian arrow f : X → Y lifting u
    coCartesianLiftsExist : Set

-- Pushforward functor (dual to reindexing)
record PushforwardFunctor : Set₁ where
  field
    opfibration : OpfibrationDeclaration
    baseMorphism : M.Identifier  -- u : I → J in B
    -- u_! : E_I → E_J
    underlyingFunctor : M.Identifier
    sourceFibre : FibreCategory
    targetFibre : FibreCategory

-- ============================================================================
-- Section 8.2: Cartesian Functors
-- ============================================================================

-- Part 1: Deconstructing the Definition

-- Axiom: Functor commutes with projections
record CommutesWithProjectionsAxiom : Set₁ where
  field
    functorF : M.Identifier
    sourceFibration : FibrationDeclaration
    targetFibration : FibrationDeclaration
    -- p' ∘ F = p
    diagramCommutes : Set

-- Axiom: Functor preserves Cartesian arrows
record PreservesCartesianArrowsAxiom : Set₁ where
  field
    functorF : M.Identifier
    sourceFibration : FibrationDeclaration
    targetFibration : FibrationDeclaration
    -- If f is Cartesian in E, then F(f) is Cartesian in E'
    preservesCartesian : Set

-- Part 2: The Master Definition and Category of Fibrations

-- Cartesian functor between fibrations
record CartesianFunctorDeclaration : Set₁ where
  field
    sourceFibration : FibrationDeclaration
    targetFibration : FibrationDeclaration
    underlyingFunctor : M.Identifier
    commutesWithProjections : CommutesWithProjectionsAxiom
    preservesCartesianArrows : PreservesCartesianArrowsAxiom

-- Category of all fibrations over a base
record CategoryOfFibrations : Set₁ where
  field
    baseCategory : CategoryDeclaration
    -- Objects: fibrations over B
    fibrations : Set
    -- Morphisms: Cartesian functors
    cartesianFunctors : Set
    -- This is actually a 2-category
    categoryStructure : CategoryDeclaration

-- ============================================================================
-- Section 8.3: Grothendieck Construction
-- ============================================================================

-- Part 1: From Fibration to Pseudofunctor (Unpacking)

-- Pseudofunctor extracted from a fibration
record PseudofunctorFromFibration : Set₁ where
  field
    fibration : FibrationDeclaration
    -- F : B^op → Cat
    -- Objects: I ↦ Fibre(p, I)
    -- Morphisms: u ↦ ReindexingFunctor(u)
    underlyingPseudofunctor : Set  -- PseudoFunctor type
    actionOnObjects : Set
    actionOnMorphisms : Set

-- Part 2: From Pseudofunctor to Fibration (Grothendieck Construction)

-- The Grothendieck construction (category of elements)
record GrothendieckConstruction : Set₁ where
  field
    basePseudofunctor : Set  -- PseudoFunctor(B^op, Cat)
    -- Construct total category E = ∫ F
    -- Objects: pairs (I, x) where I in B, x in F(I)
    -- Morphisms: pairs (u, f) where u : I → J in B, f : x → F(u)(y) in F(I)
    totalCategory : CategoryDeclaration
    -- Projection functor p : E → B
    projectionFunctor : FibrationProjectionFunctor
    -- Result is a fibration
    isFibration : FibrationDeclaration

-- Part 3: The Main Equivalence Theorem

-- Grothendieck equivalence theorem
record GrothendieckEquivalenceTheorem : Set₁ where
  field
    baseCategory : CategoryDeclaration
    -- The 2-category of fibrations over B
    fibrationsOver : CategoryOfFibrations
    -- The 2-category of pseudofunctors B^op → Cat
    pseudofunctors : Set
    -- The two constructions form an equivalence of 2-categories
    equivalence : Set

-- ============================================================================
-- Section 8.4: Fibred Adjunctions
-- ============================================================================

-- Part 1: The Definition (Pointwise Adjunctions)

-- Fibred adjunction (pointwise adjunction on fibres)
record FibredAdjunctionDeclaration : Set₁ where
  field
    leftAdjoint : CartesianFunctorDeclaration
    rightAdjoint : CartesianFunctorDeclaration
    sourceFibration : FibrationDeclaration
    targetFibration : FibrationDeclaration
    -- For each object I in base, F_I ⊣ G_I on fibres
    pointwiseAdjunctions : Set

-- Part 2: The Beck-Chevalley Condition (Coherence Law)

-- Beck-Chevalley condition
record BeckChevalleyCondition : Set₁ where
  field
    fibredAdjunction : FibredAdjunctionDeclaration
    -- For all u : I → J in base,
    -- the comparison u* ∘ G_J → G_I ∘ u* is an isomorphism
    comparisonIsIsomorphism : Set

-- Part 3: The Main Theorem (Connecting Pointwise and Global)

-- Theorem: Fibred adjunction equivalence
record FibredAdjunctionEquivalenceTheorem : Set₁ where
  field
    leftAdjoint : CartesianFunctorDeclaration
    rightAdjoint : CartesianFunctorDeclaration
    -- Global adjunction F ⊣ G between total categories
    globalAdjunction : M.Identifier
    -- Pointwise fibred adjunction
    fibredAdjunction : FibredAdjunctionDeclaration
    -- Beck-Chevalley condition
    beckChevalley : BeckChevalleyCondition
    -- Equivalence: global ⟺ (pointwise ∧ Beck-Chevalley)
    equivalence : Set

-- ============================================================================
-- Section 8.5: Completeness and Cocompleteness
-- ============================================================================

-- Part 1: The Main Criterion Theorem

-- Fibration completeness criterion theorem
record FibrationCompletenessCriterionTheorem : Set₁ where
  field
    fibration : FibrationDeclaration
    -- Premise 1: Base is complete
    baseIsComplete : Set
    -- Premise 2: All fibres are complete
    fibresAreComplete : Set
    -- Premise 3: All reindexing functors preserve limits
    reindexingPreservesLimits : Set
    -- Conclusion: Total category is complete
    totalIsComplete : Set

-- Part 2: The Constructive Proof

-- Limit construction in fibration
record LimitConstructor_within_Fibration : Set₁ where
  field
    fibration : FibrationDeclaration
    diagram : Set  -- Diagram in total category
    -- Step 1: Compute limit in base
    baseLimitCone : Set
    baseLimitApex : M.Identifier
    -- Step 2: Construct diagram in fibre by pullback
    fibreDiagram : Set
    -- Step 3: Compute limit in fibre
    fibreLimitCone : Set
    totalLimitApex : M.Identifier

-- Part 3: The Dual Theory for Cocompleteness

-- Opfibration cocompleteness criterion (dual theorem)
record OpfibrationCocompletenessCriterionTheorem : Set₁ where
  field
    opfibration : OpfibrationDeclaration
    -- Premise 1: Base is cocomplete
    baseIsCocomplete : Set
    -- Premise 2: All fibres are cocomplete
    fibresAreCocomplete : Set
    -- Premise 3: All pushforward functors preserve colimits
    pushforwardPreservesColimits : Set
    -- Conclusion: Total category is cocomplete
    totalIsCocomplete : Set

-- ============================================================================
-- Section 8.6 & 8.7: Fibrations, Smallness, and Definability
-- ============================================================================

-- Part 1: Locally Small Fibrations (Section 8.6)

-- Small category property
record SmallCategoryProperty : Set₁ where
  field
    category : CategoryDeclaration
    -- Objects form a set
    objectsAreSet : Set
    -- Morphisms form a set
    morphismsAreSet : Set

-- Locally small fibration
record LocallySmallFibration : Set₁ where
  field
    fibration : FibrationDeclaration
    -- All fibre categories are small
    allFibresAreSmall : Set

-- Refined Grothendieck equivalence theorem
record RefinedGrothendieckEquivalenceTheorem : Set₁ where
  field
    baseCategory : CategoryDeclaration
    -- 2-category of locally small fibrations over B
    locallySmallFibrations : Set
    -- 2-category of pseudofunctors B^op → Cat (small categories)
    pseudofunctorsToSmallCat : Set
    -- Equivalence
    equivalence : Set

-- Codomain fibration
record CodomainFibration : Set₁ where
  field
    baseCategory : CategoryDeclaration
    arrowCategory : CategoryDeclaration
    -- cod : C^→ → C
    codomainFunctor : M.Identifier
    -- This is a fibration (fibre over X is C/X)
    isFibration : FibrationDeclaration

-- Locally small category property
record LocallySmallCategoryProperty : Set₁ where
  field
    category : CategoryDeclaration
    -- For all A, B, Hom_C(A,B) is a set
    homSetsAreSmall : Set

-- Corollary: Codomain fibration smallness
record CodomainFibrationSmallnessCorollary : Set₁ where
  field
    category : CategoryDeclaration
    isLocallySmall : LocallySmallCategoryProperty
    hasPullbacks : Set
    codomainFib : CodomainFibration
    -- Conclusion: codomain fibration is locally small
    codomainFibIsLocallySmall : LocallySmallFibration

-- Part 2: Definability and Connection to Logic (Section 8.7)

-- Logical context (typed variables)
record LogicalContext : Set₁ where
  field
    -- Finite list of typed variables
    variables : Set

-- Substitution morphism
record SubstitutionMorphism : Set₁ where
  field
    sourceContext : LogicalContext
    targetContext : LogicalContext
    -- Map of variable assignments
    substitutionMap : Set

-- Formula in context
record FormulaInContext : Set₁ where
  field
    context : LogicalContext
    formula : Set  -- Well-formed formula

-- First-order theory
record FirstOrderTheory : Set₁ where
  field
    language : Set
    axioms : Set

-- Lindenbaum-Tarski fibration
record LindenbaumTarskiFibration : Set₁ where
  field
    theory : FirstOrderTheory
    -- Base category: contexts and substitutions
    baseCategory : CategoryDeclaration
    -- Total category: formulas in context and proofs
    totalCategory : CategoryDeclaration
    -- Projection functor
    projectionFunctor : FibrationProjectionFunctor
    -- This is a fibration
    isFibration : FibrationDeclaration

-- Definability and generic models theorem
record DefinabilityAndGenericModelsTheorem : Set₁ where
  field
    theory : FirstOrderTheory
    ltFibration : LindenbaumTarskiFibration
    -- LT_Fibration(T) has terminal object (generic model)
    hasTerminalObject : Set
    -- T admits elimination of quantifiers
    quantifierElimination : Set
    -- Equivalence
    equivalence : Set

-- ============================================================================
-- Instances and Examples
-- ============================================================================

-- Example: Codomain fibration is the canonical example
record CodomainFibrationInstance : Set₁ where
  field
    category : CategoryDeclaration
    hasPullbacks : Set
    codomainFib : CodomainFibration
    -- Cartesian arrows are pullback squares
    cartesianArrowsArePullbacks : Set

-- Example: Families fibration
record FamiliesFibration : Set₁ where
  field
    baseCategory : CategoryDeclaration
    -- Fibration of families of sets indexed by objects of base
    familiesCategory : CategoryDeclaration
    projectionFunctor : FibrationProjectionFunctor
    isFibration : FibrationDeclaration

-- ============================================================================
-- End of Level2_8
-- ============================================================================
