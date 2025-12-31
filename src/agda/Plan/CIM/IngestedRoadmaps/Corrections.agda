-- Corrections Roadmap Entries
-- Auto-generated from intake/GP/*.md files
-- Mathematical context preserved

module Plan.CIM.IngestedRoadmaps.Corrections where

open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.String
open import Plan.CIM.Utility using (RoadmapStep; RoadmapStepV2)

------------------------------------------------------------------------
-- Corrections Phase Roadmap Steps
------------------------------------------------------------------------

roadmapGp400 : RoadmapStep
roadmapGp400 = record
    { provenance   = "GP400: The \"Elasticity\" of Meaning"
    ; relatedNodes = []  -- 
    ; step         = "See full GP file for details"
    ; implication  = "Concepts: Geometry, Manifold, Sppf"
    ; status       = "not-started"
    ; targetModule = "src/agda/Plan/CIM/Implementation.agda"
    ; next         = []
    }

roadmapGp500 : RoadmapStep
roadmapGp500 = record
    { provenance   = "GP500: Dimensional Relief"
    ; relatedNodes = []  -- GP0, GP3
    ; step         = "See full GP file for details"
    ; implication  = "Concepts: Associahedron, Geometry, Manifold, Polytope, Stasheff"
    ; status       = "not-started"
    ; targetModule = "src/agda/Plan/CIM/Implementation.agda"
    ; next         = []
    }

roadmapGp501 : RoadmapStep
roadmapGp501 = record
    { provenance   = "GP501: The Topological Diagram (II)"
    ; relatedNodes = []  -- GP0, GP3
    ; step         = "See full GP file for details"
    ; implication  = "Concepts: Geometry, Manifold, Polytope, Rope, Sppf"
    ; status       = "not-started"
    ; targetModule = "src/agda/Plan/CIM/Implementation.agda"
    ; next         = []
    }

-- Enriched view (placeholder; to be filled by semantic extractor)
roadmapsV2 : List RoadmapStepV2
roadmapsV2 =
  record
    { gpNumber      = "AXIOM-SYZYGY-001"
    ; theme         = "Syzygy modules / chain-complex audit"
    ; category      = "Corrections"
    ; relatedGPs    = "GP400" ∷ "GP500" ∷ "GP501" ∷ []
    ; actionItems   = "Materialize F0/F1/F2 with d1,d2 and enforce d1∘d2=0 for kits" ∷ []
    ; concepts      = "syzygy" ∷ "homological audit" ∷ "generator/relations" ∷ []
    ; targetModules = "src/agda/Plan/CIM/Implementation.agda" ∷ []
    ; status        = "planned"
    ; notes         = "Chain complex over generators/relations to detect missing witnesses (nontrivial homology)."
    ; nextV2        = []
    } ∷ []
