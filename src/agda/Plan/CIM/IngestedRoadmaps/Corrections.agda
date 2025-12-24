-- Corrections Roadmap Entries
-- Auto-generated from intake/GP/*.md files
-- Mathematical context preserved

module Plan.CIM.IngestedRoadmaps.Corrections where

open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.String
open import Plan.CIM.Utility using (RoadmapStep)

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
