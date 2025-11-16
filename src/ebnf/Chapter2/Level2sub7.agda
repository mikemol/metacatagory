-- Level2_7: Topological Categories (Chapter 2, Section 7)
-- This module encodes the structural content of Section 7 from the EBNF grammar.
-- It covers exponentiable spaces, compactly generated spaces, and topological functors.

module Chapter2.Level2sub7 where

open import Core
open import Chapter1.Level1Index  -- Not public to avoid conflicts
open import Metamodel as M

-- ============================================================================
-- Section 7.1: Exponentiable Spaces
-- ============================================================================

-- Part 1: The Context and Core Property

-- The category of topological spaces
record TopologicalSpacesCategory : Set₁ where
  field
    -- Category of all topological spaces and continuous maps
    underlyingCategory : CategoryDeclaration
    -- Objects are topological spaces
    spaces : Set
    -- Morphisms are continuous maps
    continuousMaps : Set

-- Product functor (- × X) on Top
record ProductFunctor_Top : Set₁ where
  field
    category : TopologicalSpacesCategory
    fixedSpace : M.Identifier
    -- The endofunctor (- × X) : Top → Top
    underlyingFunctor : M.Identifier

-- Exponentiable space property
record ExponentiableSpaceProperty : Set₁ where
  field
    category : TopologicalSpacesCategory
    space : M.Identifier
    -- The product functor (- × X)
    productFunctor : ProductFunctor_Top
    -- X is exponentiable if (- × X) has a right adjoint
    hasRightAdjoint : Set

-- Part 2: The Exponential Object and Adjunction

-- Exponential object Y^X (function space)
record ExponentialObject_Top : Set₁ where
  field
    category : TopologicalSpacesCategory
    baseSpace : M.Identifier  -- X
    targetSpace : M.Identifier  -- Y
    -- Premise: X is exponentiable
    baseIsExponentiable : ExponentiableSpaceProperty
    -- Y^X with compact-open topology
    exponentialSpace : M.Identifier

-- Exponentiation adjunction theorem
record ExponentiationAdjunctionInTopTheorem : Set₁ where
  field
    category : TopologicalSpacesCategory
    space : M.Identifier
    exponentiableProperty : ExponentiableSpaceProperty
    -- (- × X) ⊣ (-)^X
    productFunctor : M.Identifier
    exponentialFunctor : M.Identifier
    adjunction : M.Identifier
    -- Hom_Top(A × X, Y) ≅ Hom_Top(A, Y^X)
    homIsomorphism : Set

-- Part 3: Cartesian Closed Categories and the Problem

-- Cartesian closed category property
record CartesianClosedCategoryProperty : Set₁ where
  field
    category : CategoryDeclaration
    -- Has terminal object and all binary products
    hasFiniteProducts : Set
    -- Every object is exponentiable
    allObjectsExponentiable : Set

-- Theorem: Top is not Cartesian closed
record TopIsNotCartesianClosedTheorem : Set₁ where
  field
    topCategory : TopologicalSpacesCategory
    -- Top has finite products
    hasProducts : Set
    -- But there exists a non-exponentiable space (e.g., rationals Q)
    counterexample : M.Identifier
    counterexampleNotExponentiable : Set
    -- Therefore Top is not Cartesian closed
    conclusion : Set

-- ============================================================================
-- Section 7.2: Compactly Generated Spaces
-- ============================================================================

-- Part 1: The Properties and the Subcategory

-- Compactly generated space property (k-space)
record CompactlyGeneratedProperty : Set₁ where
  field
    topCategory : TopologicalSpacesCategory
    space : M.Identifier
    -- A subset A is closed iff f⁻¹(A) is closed in K
    -- for all continuous f : K → X from compact Hausdorff K
    topologyDeterminedByCompacta : Set

-- Weak Hausdorff property
record WeakHausdorffProperty : Set₁ where
  field
    topCategory : TopologicalSpacesCategory
    space : M.Identifier
    -- Image of any continuous map from compact Hausdorff is closed
    compactImagesAreClosed : Set

-- The CGWH category
record CGWH_CategoryDeclaration : Set₁ where
  field
    topCategory : TopologicalSpacesCategory
    -- Full subcategory of Top on CGWH objects
    underlyingCategory : CategoryDeclaration
    -- Objects satisfy both properties
    objectsAreCompactlyGenerated : Set
    objectsAreWeakHausdorff : Set

-- Part 2: The Reflection and Main Theorem

-- k-ification functor
record KificationFunctorDeclaration : Set₁ where
  field
    topCategory : TopologicalSpacesCategory
    cgwhCategory : CGWH_CategoryDeclaration
    -- k : Top → CGWH (the reflector)
    underlyingFunctor : M.Identifier
    -- Gives a space the finest compactly generated topology
    refinesTopology : Set

-- Theorem: CGWH is reflective in Top
record CGWH_isReflectiveInTopTheorem : Set₁ where
  field
    topCategory : TopologicalSpacesCategory
    cgwhCategory : CGWH_CategoryDeclaration
    -- Inclusion functor I : CGWH ↪ Top
    inclusionFunctor : M.Identifier
    -- k-ification functor k : Top → CGWH
    kificationFunctor : KificationFunctorDeclaration
    -- k ⊣ I adjunction
    adjunction : M.Identifier

-- Theorem: CGWH is Cartesian closed
record CGWH_isCartesianClosedTheorem : Set₁ where
  field
    cgwhCategory : CGWH_CategoryDeclaration
    -- CGWH has finite products
    hasFiniteProducts : Set
    -- Every object in CGWH is exponentiable (in CGWH)
    allObjectsExponentiable : Set
    -- Therefore CGWH is Cartesian closed
    isCartesianClosed : CartesianClosedCategoryProperty

-- ============================================================================
-- Section 7.3: Topological Functors
-- ============================================================================

-- Part 1: The Lifting Problem and Solutions

-- Structured sink for a functor
record StructuredSink : Set₁ where
  field
    sourceFunctor : M.Identifier  -- U : C → B
    -- Target object in base category B
    targetObject : M.Identifier
    -- Family of morphisms f_i : U(A_i) → B_o
    sourceMaps : Set

-- Initial lift of a structured sink
record InitialLift : Set₁ where
  field
    sink : StructuredSink
    -- The lifted object C_o in source category C
    liftedObject : M.Identifier
    -- Lifted morphisms m_i : A_i → C_o
    liftedMorphisms : Set
    -- U(C_o) = B_o
    preservesTargetObject : Set
    -- U(m_i) = f_i for all i
    preservesSourceMaps : Set
    -- Universal property: initial among all such lifts
    universalProperty : Set

-- Structured source (dual to structured sink)
record StructuredSource : Set₁ where
  field
    sourceFunctor : M.Identifier  -- U : C → B
    -- Source object in base category B
    sourceObject : M.Identifier
    -- Family of morphisms f_i : B_o → U(A_i)
    targetMaps : Set

-- Final lift of a structured source
record FinalLift : Set₁ where
  field
    source : StructuredSource
    -- The lifted object C_o in source category C
    liftedObject : M.Identifier
    -- Lifted morphisms m_i : C_o → A_i
    liftedMorphisms : Set
    -- U(C_o) = B_o
    preservesSourceObject : Set
    -- U(m_i) = f_i for all i
    preservesTargetMaps : Set
    -- Universal property: final among all such lifts
    universalProperty : Set

-- Part 2: The Master Definition and Consequences

-- Topological functor property
record TopologicalFunctorProperty : Set₁ where
  field
    functor : M.Identifier  -- U : C → B
    -- U admits initial lifts for all structured sinks
    admitsInitialLifts : (sink : StructuredSink) → InitialLift
    -- (Dually, admits final lifts for all structured sources)
    admitsFinalLifts : (source : StructuredSource) → FinalLift

-- Theorem: Consequences of being a topological functor
record ConsequencesOfBeingTopologicalTheorem : Set₁ where
  field
    functor : M.Identifier
    topologicalProperty : TopologicalFunctorProperty
    -- 1. Domain category is complete and cocomplete
    domainIsComplete : Set
    domainIsCocomplete : Set
    -- 2. Functor has both left and right adjoints
    hasLeftAdjoint : Set
    hasRightAdjoint : Set
    -- 3. Functor preserves and reflects limits and colimits
    preservesLimits : Set
    preservesColimits : Set
    reflectsLimits : Set
    reflectsColimits : Set

-- Instance: Forgetful functor U : Top → Set is topological
record ForgetfulFunctorForTopIsTopologicalInstance : Set₁ where
  field
    topCategory : TopologicalSpacesCategory
    setCategory : CategoryDeclaration
    -- U : Top → Set forgetful functor
    forgetfulFunctor : M.Identifier
    -- U is topological (admits initial and final lifts)
    isTopological : TopologicalFunctorProperty

-- ============================================================================
-- Instances and Examples
-- ============================================================================

-- Example: Compact Hausdorff spaces are exponentiable
record CompactHausdorffIsExponentiableInstance : Set₁ where
  field
    topCategory : TopologicalSpacesCategory
    compactHausdorffSpace : M.Identifier
    -- Every compact Hausdorff space is exponentiable
    isExponentiable : ExponentiableSpaceProperty

-- Example: Locally compact Hausdorff spaces are exponentiable
record LocallyCompactHausdorffIsExponentiableInstance : Set₁ where
  field
    topCategory : TopologicalSpacesCategory
    locallyCompactHausdorffSpace : M.Identifier
    -- Every locally compact Hausdorff space is exponentiable
    isExponentiable : ExponentiableSpaceProperty

-- ============================================================================
-- End of Level2_7
-- ============================================================================
