{-# OPTIONS --without-K --cubical-compatible --safe #-}

-- | Structured metadata describing frameworks and their key modules.
module Plan.CIM.FrameworkMetadata where

open import Agda.Builtin.String using (String)
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Nat using (Nat)

------------------------------------------------------------------------
-- Framework Metadata: Structured Description of Frameworks
------------------------------------------------------------------------

record Framework : Set where
  field
    name           : String
    emoji          : String
    primaryTheory  : String
    keyModules     : List String
    provides       : List String
    primaryGoal    : String
    description    : String

record InterfaceBoundary : Set where
  field
    name           : String
    fromFramework  : String
    toFramework    : String
    description    : String
    mechanismStart : String
    mechanismEnd   : String

record FlexibilityPoint : Set where
  field
    name           : String
    description    : String
    alternativeOne : String
    alternativeTwo : String
    rationale      : String

------------------------------------------------------------------------
-- Gap Analysis: Theory-vs-Implementation Comparison
------------------------------------------------------------------------

record TheoreticalConcept : Set where
  field
    name           : String
    location       : String  -- Reference in compendium
    description    : String
    implementationGap : String
    recommendation : String

record PlannedFeature : Set where
  field
    name           : String
    roadmapItem    : String
    description    : String
    theoryGap      : String
    recommendation : String

------------------------------------------------------------------------
-- Quality Framework: The Six Mandates
------------------------------------------------------------------------

record QualityMandate : Set where
  field
    name           : String
    abbreviation   : String
    description    : String
    examples       : List String

record A12ProtocolViolation : Set where
  field
    violationType  : String
    description    : String
    detection      : String
    correction     : String

------------------------------------------------------------------------
-- Composition Pattern: How frameworks interact
------------------------------------------------------------------------

record CompositionPattern : Set where
  field
    name           : String
    patternName    : String
    benefit        : String
    example        : String

------------------------------------------------------------------------
-- Framework Inventory (will be instantiated with actual frameworks)
------------------------------------------------------------------------

CategoryTheoryFramework : Framework
CategoryTheoryFramework = record
  { name = "CIM Categorical Core"
  ; emoji = "🏛️"
  ; primaryTheory = "Category theory, universal properties, constructive type theory"
  ; keyModules = 
      "Core.UniversalProperties" ∷
      "Core.AlgorithmUniversality" ∷
      "Core.PhaseCategory" ∷
      "Core.ConstructiveWitnesses" ∷
      []
  ; provides =
      "0→1→2 cell hierarchy (specifications → algorithms → coherence)" ∷
      "Rigorous proof obligations via Agda" ∷
      "Universal property characterizations" ∷
      "Categorical constructions (limits, colimits, products)" ∷
      []
  ; primaryGoal = "Verifiable correctness through constructive proofs"
  ; description = "The categorical foundation providing rigorous type-theoretic verification"
  }

TopologicalGeometricFramework : Framework
TopologicalGeometricFramework = record
  { name = "Topological/Geometric Layer"
  ; emoji = "🌐"
  ; primaryTheory = "Differential geometry, algebraic topology, manifold theory"
  ; keyModules =
      "Plan.CIM.PolytopeExpansion" ∷
      "Plan.CIM.RotationalTransport" ∷
      "Plan.CIM.TopologicalGating" ∷
      []
  ; provides =
      "Polytope expansion for semantic space representation" ∷
      "Rotational position encoding (RoPE) via SO(n) symmetry" ∷
      "Manifold structure on parse/protocol state spaces" ∷
      "Mitosis dynamics for category splitting" ∷
      []
  ; primaryGoal = "Geometric intuition and spatial reasoning for complex semantic spaces"
  ; description = "Provides geometric and topological reasoning for the system"
  }

AlgebraicStructuresFramework : Framework
AlgebraicStructuresFramework = record
  { name = "Algebraic Structures Layer"
  ; emoji = "🔢"
  ; primaryTheory = "Abstract algebra, lattice theory, monoid/group actions"
  ; keyModules =
      "Algebra.Groups.Basic" ∷
      "Algebra.Rings.Basic" ∷
      "Algebra.Fields.Basic" ∷
      "Core.AlgebraicAlgorithms" ∷
      []
  ; provides =
      "Concrete algebraic datatypes (fields, rings, groups)" ∷
      "Galois correspondence algorithms" ∷
      "Polynomial manipulation" ∷
      "Extension field constructions" ∷
      []
  ; primaryGoal = "Algorithmic algebra with field-theoretic computations"
  ; description = "Concrete algebraic structures and algorithms"
  }

TransformationRewritingFramework : Framework
TransformationRewritingFramework = record
  { name = "Transformation & Rewriting Systems"
  ; emoji = "♻️"
  ; primaryTheory = "Term rewriting, graph transformation, operational semantics"
  ; keyModules =
      "Plan.CIM.TransformationSystem" ∷
      "Plan.CIM.Ambiguity" ∷
      "Core.Phase" ∷
      []
  ; provides =
      "Step-by-step transformation sequences" ∷
      "Cost-annotated rewriting rules" ∷
      "Ambiguity resolution via path selection" ∷
      "Compositional transformation algebra" ∷
      []
  ; primaryGoal = "Operational semantics with explicit cost models"
  ; description = "Transformation and rewriting system with cost tracking"
  }

MetaprogrammingReflectionFramework : Framework
MetaprogrammingReflectionFramework = record
  { name = "Metaprogramming & Reflection Layer"
  ; emoji = "🪞"
  ; primaryTheory = "Metamodeling, reflection, adapter patterns"
  ; keyModules =
      "Core.AdapterReflection" ∷
      "Core.AdapterAutomation" ∷
      "Metamodel" ∷
      []
  ; provides =
      "Protocol introspection and dynamic dispatch" ∷
      "Adapter-based polymorphism" ∷
      "Metaprogramming facilities for code generation" ∷
      "Reflection over algorithmic structure" ∷
      []
  ; primaryGoal = "Flexible runtime composition and metaprogrammatic control"
  ; description = "Metaprogramming and reflection infrastructure"
  }

ComputationalPragmaticsFramework : Framework
ComputationalPragmaticsFramework = record
  { name = "Computational Pragmatics"
  ; emoji = "⚡"
  ; primaryTheory = "Performance analysis, resource management, implementation strategies"
  ; keyModules =
      "Core.AlgorithmComplexity" ∷
      "Core.GrowthMetrics" ∷
      "src/python/nedge_topology" ∷
      []
  ; provides =
      "Complexity analysis and bounds" ∷
      "Growth metric computation" ∷
      "Practical implementation strategies" ∷
      "Performance tuning guidance" ∷
      []
  ; primaryGoal = "Practical efficiency and real-world performance"
  ; description = "Pragmatic computational considerations"
  }

allFrameworks : List Framework
allFrameworks =
  CategoryTheoryFramework ∷
  TopologicalGeometricFramework ∷
  AlgebraicStructuresFramework ∷
  TransformationRewritingFramework ∷
  MetaprogrammingReflectionFramework ∷
  ComputationalPragmaticsFramework ∷
  []

------------------------------------------------------------------------
-- Interface Boundaries
------------------------------------------------------------------------

interfaceBoundaries : List InterfaceBoundary
interfaceBoundaries =
  record
  { name = "Universal Property Realization"
  ; fromFramework = "CIM Categorical Core"
  ; toFramework = "Algebraic Structures Layer"
  ; description = "Categorical universal properties are realized as concrete algebraic structures"
  ; mechanismStart = "Core.UniversalProperties.UniversalProperty"
  ; mechanismEnd = "Tests.ObligationAdapters"
  }
  ∷
  record
  { name = "Phase-Geometric Duality"
  ; fromFramework = "Transformation & Rewriting Systems"
  ; toFramework = "Topological/Geometric Layer"
  ; description = "Transformation steps correspond to geodesics in the polytope manifold"
  ; mechanismStart = "Core.Phase composition"
  ; mechanismEnd = "Plan.CIM.PolytopeExpansion"
  }
  ∷
  record
  { name = "Metric Emergence"
  ; fromFramework = "Transformation & Rewriting Systems"
  ; toFramework = "Topological/Geometric Layer"
  ; description = "Cost functions from transformation systems generate emergent metrics"
  ; mechanismStart = "Plan.CIM.Utility.EmergentMetric"
  ; mechanismEnd = "Dimension ascent"
  }
  ∷
  record
  { name = "Adapter Polymorphism"
  ; fromFramework = "Metaprogramming & Reflection Layer"
  ; toFramework = "CIM Categorical Core"
  ; description = "Adapters enable algebraic structures to conform to categorical interfaces"
  ; mechanismStart = "Core.CategoricalAdapter"
  ; mechanismEnd = "Tests.ObligationAdapters"
  }
  ∷
  record
  { name = "Complexity Witness"
  ; fromFramework = "Computational Pragmatics"
  ; toFramework = "CIM Categorical Core"
  ; description = "Complexity bounds are witnessed as categorical morphisms"
  ; mechanismStart = "Core.AlgorithmComplexity"
  ; mechanismEnd = "Core.ConstructiveWitnesses"
  }
  ∷
  []

------------------------------------------------------------------------
-- Intentional Flexibility Points
------------------------------------------------------------------------

flexibilityPoints : List FlexibilityPoint
flexibilityPoints =
  record
  { name = "Universal Property Formulation"
  ; description = "Universal properties can be expressed via limits (categorical) or natural isomorphisms (functor-based)"
  ; alternativeOne = "Limit-theoretic (Chapter1 approach)"
  ; alternativeTwo = "Isomorphism-theoretic (adjunction approach)"
  ; rationale = "Both are equivalent; choice depends on proof strategy and readability"
  }
  ∷
  record
  { name = "Ambiguity Resolution"
  ; description = "Ambiguities can be resolved via topological geodesics or categorical coherence witnesses"
  ; alternativeOne = "Topological geodesic (shortest path in manifold)"
  ; alternativeTwo = "Categorical witness (proof of coherence)"
  ; rationale = "Loose coupling allows choosing strategy per use-case"
  }
  ∷
  record
  { name = "Metric Aggregation"
  ; description = "Cost functions can aggregate via sum, max, or categorical product"
  ; alternativeOne = "Summation (additive cost)"
  ; alternativeTwo = "Categorical product (lattice-theoretic)"
  ; rationale = "System architecture is metric-agnostic; swappable cost functions"
  }
  ∷
  record
  { name = "Phase Composition"
  ; description = "Phases compose sequentially (pipe) or in parallel (product)"
  ; alternativeOne = "Sequential (⟫ operator)"
  ; alternativeTwo = "Parallel (⊗ operator)"
  ; rationale = "PhaseCategory supports both; semantics depend on use context"
  }
  ∷
  record
  { name = "Algorithm Selection"
  ; description = "Algorithm selection can be static (compile-time) or dynamic (runtime)"
  ; alternativeOne = "Compile-time dispatch via type class"
  ; alternativeTwo = "Runtime dispatch via adapter reflection"
  ; rationale = "Framework supports both; performance/flexibility tradeoff"
  }
  ∷
  []

------------------------------------------------------------------------
-- Quality Mandates (The Sextet)
------------------------------------------------------------------------

qualityMandates : List QualityMandate
qualityMandates =
  record
  { name = "Verifiable"
  ; abbreviation = "V"
  ; description = "Every claim has a constructive witness; nothing is postulated without justification"
  ; examples =
      "Agda proof obligations must be satisfied" ∷
      "Algorithms carry complexity witnesses" ∷
      []
  }
  ∷
  record
  { name = "Correct"
  ; abbreviation = "C"
  ; description = "Specifications match implementations; invariants are preserved"
  ; examples =
      "UniversalProperties match AlgebraicStructures via adapters" ∷
      "Phase composition respects associativity" ∷
      []
  }
  ∷
  record
  { name = "Complete"
  ; abbreviation = "C"
  ; description = "All covered concepts are fully developed; no truncated fragments"
  ; examples =
      "Framework definitions include all 6 frameworks (not partial)" ∷
      "Gap analysis identifies all symmetric differences" ∷
      []
  }
  ∷
  record
  { name = "Concrete"
  ; abbreviation = "C"
  ; description = "Abstractions are grounded in concrete algorithms and data structures"
  ; examples =
      "Universal properties realized as field algorithms" ∷
      "Phases execute as actual transformations" ∷
      []
  }
  ∷
  record
  { name = "Meticulous"
  ; abbreviation = "M"
  ; description = "Care taken with edge cases, termination, and resource bounds"
  ; examples =
      "Complexity analysis includes worst-case bounds" ∷
      "Termination proofs for recursive structures" ∷
      []
  }
  ∷
  record
  { name = "Coherent"
  ; abbreviation = "C"
  ; description = "All components integrate coherently; no contradictions or gaps"
  ; examples =
      "Category laws verified for PhaseCategory" ∷
      "Emergent metrics consistent across induction layers" ∷
      []
  }
  ∷
  []

------------------------------------------------------------------------
-- A12 Protocol Violations
------------------------------------------------------------------------

a12Violations : List A12ProtocolViolation
a12Violations =
  record
  { violationType = "Structural Incoherence"
  ; description = "Inconsistent n-Cell types; 0-cells don't match 1-cell specifications"
  ; detection = "Type mismatch in Core modules"
  ; correction = "Verify CategoricalAdapter witnesses satisfy UniversalProperty mandates"
  }
  ∷
  record
  { violationType = "EmergentMetric Contamination"
  ; description = "Conflated metrics from different transformation contexts"
  ; detection = "Multiple cost functions applied to same context"
  ; correction = "Isolate metrics to their originating transformation systems"
  }
  ∷
  record
  { violationType = "Categorical Mandate Elision"
  ; description = "Missing universal properties; incomplete categorical structure"
  ; detection = "Open postulates in Core modules"
  ; correction = "Add universal property proofs in Core.UniversalProperties"
  }
  ∷
  record
  { violationType = "Coherence Debt Accumulation"
  ; description = "Unproven equivalences between categorical and computational perspectives"
  ; detection = "Postulates in Chapter2 or Chapter3"
  ; correction = "Provide constructive coherence witnesses"
  }
  ∷
  record
  { violationType = "Algorithmic Non-Termination"
  ; description = "Unbounded computations; lack of termination proofs"
  ; detection = "#-# TERMINATING pragmas without justification"
  ; correction = "Prove termination via well-founded induction"
  }
  ∷
  record
  { violationType = "Witness Non-Constructivity"
  ; description = "Proofs assume classical logic or unproven lemmas"
  ; detection = "Classical axioms (LEM, choice) without constructive interpretation"
  ; correction = "Provide constructive witnesses or dependent type encodings"
  }
  ∷
  record
  { violationType = "Homological Obstruction"
  ; description = "Cycle detection failures in dependency graphs"
  ; detection = "Circular imports or mutual without clear stratification"
  ; correction = "Enforce DAG structure via Axiom of Well-Founded Indexed Composition"
  }
  ∷
  record
  { violationType = "Braid Diagram Malformation"
  ; description = "Invalid inheritance paths in BraidedInheritanceFunctor"
  ; detection = "BIF composition violates monoidal coherence"
  ; correction = "Verify hexagon and unit coherence in PhaseCategory"
  }
  ∷
  []
