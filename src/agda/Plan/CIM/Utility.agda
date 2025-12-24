{-# OPTIONS --without-K --cubical-compatible --safe #-}

module Plan.CIM.Utility where

open import Agda.Primitive using (Level; lsuc)
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.String using (String; primStringAppend)
open import Agda.Builtin.Nat using (Nat; zero; suc; _-_)
open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.Maybe using (Maybe; just; nothing)
open import Agda.Builtin.Sigma using (Σ; _,_)

_×_ : ∀ {ℓ} → Set ℓ → Set ℓ → Set ℓ
A × B = Σ A (λ _ → B)

ℕ : Set
ℕ = Nat

-- String concatenation
_++_ : String → String → String
_++_ = primStringAppend

infixr 20 _++_

map : ∀ {A B : Set} → (A → B) → List A → List B
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs

------------------------------------------------------------------------
-- Recursive RoadmapStep Record
------------------------------------------------------------------------
record RoadmapStep : Set₁ where
    inductive
    field
        provenance   : String
        relatedNodes : List String
        step         : String
        implication  : String
        status       : String
        targetModule : String
        next         : List RoadmapStep

------------------------------------------------------------------------
-- Core CIM Framework Types
------------------------------------------------------------------------

-- RENAMED: Ambiguity -> PhaseAmbiguity
-- This structure specifically handles the RoPE phase/rotation aspect.
-- The general algebraic ambiguity is now in Plan.CIM.Ambiguity.
record PhaseAmbiguity {ℓ} (A B : Set ℓ) : Set ℓ where
    field
        valA : A
        valB : B
        phase : ℕ 

record TransformationSystem {ℓ} (A B : Set ℓ) : Set (lsuc ℓ) where
    field
        Step : Set ℓ
        cost : Step → ℕ

record EmergentMetric : Set where
    field
        magnitude : ℕ

data Path {ℓ} {A B : Set ℓ} (Sys : TransformationSystem {ℓ} A B) : Set ℓ where
    refl-path : Path Sys
    trans-step : (s : TransformationSystem.Step Sys) → (rest : Path Sys) → Path Sys

-- Updated to use PhaseAmbiguity
record CoherenceWitness {ℓ} {A B : Set ℓ} (amb : PhaseAmbiguity {ℓ} A B) (Sys : TransformationSystem {ℓ} A B) : Set (lsuc ℓ) where
    field
        proofPath : Path Sys
        metric    : EmergentMetric

record BraidedInheritanceFunctor {ℓ} (A B : Set ℓ) : Set (lsuc ℓ) where
    field
        inheritanceBraid : (A × B) → (B × A)
        coherenceCost    : EmergentMetric
        fromValue : A
        toValue   : B
        description : String

-- NEW: BraidedSPPF definition integrated from Intake
record BraidedSPPF {ℓ} (A B : Set ℓ) (Sys : TransformationSystem A B) : Set (lsuc ℓ) where
    constructor packed-node
    field
        leftPath   : Path Sys
        rightPath  : Path Sys
        resolution : BraidedInheritanceFunctor A B

record CostFunction : Set where
    field
        cost : ℕ → ℕ
        minimal : ℕ → Bool

record Witness {ℓ} (A B : Set ℓ) : Set ℓ where
    field
        source : A
        target : B
        evidence : String

-- Updated to use PhaseAmbiguity
record CNFProtocol (A B : Set) : Set₁ where
    field
        ambiguity : PhaseAmbiguity A B
        transSys  : TransformationSystem A B
        coherence : CoherenceWitness ambiguity transSys
        metric    : EmergentMetric

------------------------------------------------------------------------
-- AST-Dependent Types
------------------------------------------------------------------------

module ASTDependent (Block MdBlock BraidStep : Set) where

  record BraidTrace : Set where
    field
        steps : List BraidStep
        summary : String

  -- Updated to use PhaseAmbiguity
  record BlockProtocol : Set₁ where
    field
        ambiguity : PhaseAmbiguity Block MdBlock
        transSys  : TransformationSystem Block MdBlock
        coherence : CoherenceWitness ambiguity transSys
        metric    : EmergentMetric

  -- Updated to use PhaseAmbiguity
  record DocProtocol : Set₁ where
    field
        ambiguity : PhaseAmbiguity String String
        transSys  : TransformationSystem String String
        coherence : CoherenceWitness ambiguity transSys
        metric    : EmergentMetric

------------------------------------------------------------------------
-- Utility Functions (Preserved)
------------------------------------------------------------------------

metricMinimality : CostFunction → ℕ → Bool
metricMinimality cf m = CostFunction.minimal cf m

index : ∀ {A : Set} → List A → ℕ → Maybe A
index [] _ = nothing
index (x ∷ xs) zero = just x
index (x ∷ xs) (suc n) = index xs n

foldrGeneric : ∀ {A B : Set} → (A → B → B) → B → List A → B
foldrGeneric f acc [] = acc
foldrGeneric f acc (x ∷ xs) = f x (foldrGeneric f acc xs)

mapWithPrefix : String → List String → List String
mapWithPrefix prefix xs = map (λ d → prefix ++ d) xs

concatWithSep : String → List String → String
concatWithSep sep [] = ""
concatWithSep sep (x ∷ []) = x
concatWithSep sep (x ∷ xs) = x ++ sep ++ concatWithSep sep xs

-- Examples preserved from previous context...
exampleUnifiedTopologicalParserRoadmap : RoadmapStep
exampleUnifiedTopologicalParserRoadmap = record
    { provenance  = "GP699"
    ; relatedNodes = []
    ; step        = "Integrate Earley parsing..."
    ; implication = "Enables composable geometric..."
    ; status      = "not-started"
    ; targetModule = "nedge_topology/parser.py"
    ; next = []
    }

exampleDimensionalReliefRoadmap : RoadmapStep
exampleDimensionalReliefRoadmap = record
    { provenance  = "GP500, Dimensional Relief, Topological Inflation, Stasheff Expansion"
    ; relatedNodes = []
    ; step        = "Implement topological inflation: upgrade crowded semantic categories to higher-dimensional polytopes to relieve tension."
    ; implication = "Enables composable category expansion, tension relief, and dynamic geometry for semantic protocols. Supports recursive revisiting and concept differentiation."
    ; status      = "not-started"
    ; targetModule = "src/agda/Plan/CIM/PolytopeExpansion.agda, nedge_topology/mitosis.py"
    ; next = []
    }

examplePolytopeManifestRoadmap : RoadmapStep
examplePolytopeManifestRoadmap = record
    { provenance  = "GP501, Polytope Manifest, Mitosis Engine, Dynamic Polytopes"
    ; relatedNodes = []
    ; step        = "Implement Mitosis Engine to monitor topological tension and inflate categories to dynamic polytopes as needed."
    ; implication = "Enables dynamic, composable category geometry, tension monitoring, and concept differentiation. Supports recursive revisiting and protocol evolution."
    ; status      = "not-started"
    ; targetModule = "nedge_topology/mitosis.py, nedge_topology/parser.py, dashboard.py, src/agda/Plan/CIM/PolytopeExpansion.agda"
    ; next = []
    }

exampleElasticityOfMeaningRoadmap : RoadmapStep
exampleElasticityOfMeaningRoadmap = record
    { provenance  = "GP400, Elasticity of Meaning, Tension/Resonance phase space"
    ; relatedNodes = "exampleAlgebraicAmbiguityRoadmap" ∷ "exampleMetricizationRoadmap" ∷ "exampleTransformationSystemRoadmap" ∷ "exampleFunctorialConstructsRoadmap" ∷ []
    ; step        = "Integrate 2D gating logic (Tension/Resonance) into parser and protocol records, explicitly cross-referencing ambiguity, metricization, transformation system, and functorial constructs."
    ; implication = "Enables composable phase space modeling, creative/insightful parse acceptance, and pruning of non-sequitur/hallucination nodes. Supports recursive revisiting for grammar induction, protocol refinement, and functorial traceability."
    ; status      = "not-started"
    ; targetModule = "src/agda/Plan/CIM/Elasticity.agda, parser.py, dashboard.py, src/agda/Plan/CIM/Ambiguity.agda, src/agda/Plan/CIM/Metricization.agda, src/agda/Plan/CIM/TransformationSystem.agda, src/agda/Plan/CIM/FunctorialConstructs.agda"
    ; next = []
    }

-- Reference records mentioned in the 4 key roadmaps
exampleAlgebraicAmbiguityRoadmap : RoadmapStep
exampleAlgebraicAmbiguityRoadmap = record
    { provenance  = "Algebraic Ambiguity Infrastructure"
    ; relatedNodes = []
    ; step        = "Provide algebraic structures for representing and manipulating ambiguity in parse spaces."
    ; implication = "Enables formal treatment of ambiguity as algebraic object."
    ; status      = "not-started"
    ; targetModule = "src/agda/Plan/CIM/Ambiguity.agda"
    ; next = []
    }

exampleMetricizationRoadmap : RoadmapStep
exampleMetricizationRoadmap = record
    { provenance  = "Metrication of Semantic Space"
    ; relatedNodes = []
    ; step        = "Establish metric structures on semantic spaces for distance/similarity calculations."
    ; implication = "Enables quantitative semantic reasoning and optimization."
    ; status      = "not-started"
    ; targetModule = "src/agda/Plan/CIM/Metricization.agda"
    ; next = []
    }

exampleTransformationSystemRoadmap : RoadmapStep
exampleTransformationSystemRoadmap = record
    { provenance  = "Transformation System Architecture"
    ; relatedNodes = []
    ; step        = "Define compositional transformation operations on semantic objects."
    ; implication = "Enables systematic rewriting and protocol evolution."
    ; status      = "not-started"
    ; targetModule = "src/agda/Plan/CIM/TransformationSystem.agda"
    ; next = []
    }

exampleFunctorialConstructsRoadmap : RoadmapStep
exampleFunctorialConstructsRoadmap = record
    { provenance  = "Functorial Framework"
    ; relatedNodes = []
    ; step        = "Implement functorial mappings between semantic and computational spaces."
    ; implication = "Enables formal structure-preserving transformations."
    ; status      = "not-started"
    ; targetModule = "src/agda/Plan/CIM/FunctorialConstructs.agda"
    ; next = []
    }
