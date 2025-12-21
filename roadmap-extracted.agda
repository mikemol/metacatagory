-- 4 key Roadmap examples extracted and cleaned from Utility-broken.agda
-- All nested `next` structures simplified to [] for syntactic clarity
-- This represents the 4 referenced in COPILOT_SYNERGY.md plus their dependencies

exampleUnifiedTopologicalParserRoadmap : RoadmapStep
exampleUnifiedTopologicalParserRoadmap = record
    { provenance  = "GP699, Unified Topological Parser, Nedge-Topology, SPPF + RoPE + SymNum"
    ; relatedNodes = "exampleDimensionalReliefRoadmap" ∷ "examplePolytopeManifestRoadmap" ∷ "exampleElasticityOfMeaningRoadmap" ∷ []
    ; step        = "Integrate Earley parsing, RoPE, and symmetry group concepts into a unified topological parser. Treat syntax as a manifold and ambiguity as vector superposition."
    ; implication = "Enables composable geometric and topological integration, active topological pruning, and algebraic superposition for ambiguity. Supports recursive revisiting, fiber bundle architecture, and advanced induction/training features."
    ; status      = "not-started"
    ; targetModule = "nedge_topology/parser.py, nedge_topology/train.py, nedge_topology/mitosis.py, nedge_topology/search.py, dashboard.py, src/agda/Plan/CIM/RotationalTransport.agda, src/agda/Plan/CIM/TopologicalGating.agda, src/agda/Plan/CIM/TopologicalSuperposition.agda"
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

