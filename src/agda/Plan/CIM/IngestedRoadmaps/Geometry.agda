-- Geometry Roadmap Entries
-- Auto-generated from intake/GP/*.md files
-- Mathematical context preserved

module Plan.CIM.IngestedRoadmaps.Geometry where

open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.String
open import Plan.CIM.Utility using (RoadmapStep)

------------------------------------------------------------------------
-- Geometry Phase Roadmap Steps
------------------------------------------------------------------------

roadmapGp200 : RoadmapStep
roadmapGp200 = record
    { provenance   = "GP200: The Geometry of Associativity"
    ; relatedNodes = []  -- 
    ; step         = "Would you like that?"
    ; implication  = "Concepts: Abelian, Associahedron, Category Theory, Complex, Geometry"
    ; status       = "not-started"
    ; targetModule = "src/agda/Plan/CIM/Implementation.agda"
    ; next         = []
    }

roadmapGp201 : RoadmapStep
roadmapGp201 = record
    { provenance   = "GP201: The Non-Abelian Manifold"
    ; relatedNodes = []  -- 
    ; step         = "Would you like me to update the nedge\\_topology/\\_\\_init\\_\\_.py and nedge\\_topology/geometry.py to officially switch from the RotationalGeometry (RoPE) to this new SymNumGeometry?"
    ; implication  = "Concepts: Abelian, Geometry, Group Action, Manifold, Rope"
    ; status       = "not-started"
    ; targetModule = "src/agda/Plan/CIM/Implementation.agda"
    ; next         = []
    }

roadmapGp300 : RoadmapStep
roadmapGp300 = record
    { provenance   = "GP300: The Fiber Bundle Architecture"
    ; relatedNodes = []  -- GP0, GP3
    ; step         = "See full GP file for details"
    ; implication  = "Concepts: Adjunction, Associahedron, Functor, Geometry, Lie Group"
    ; status       = "not-started"
    ; targetModule = "src/agda/Plan/CIM/Implementation.agda"
    ; next         = []
    }

roadmapGp302 : RoadmapStep
roadmapGp302 = record
    { provenance   = "GP302: The Operationalized Adjunction"
    ; relatedNodes = []  -- 
    ; step         = "See full GP file for details"
    ; implication  = "Concepts: Abelian, Adjunction, Geometry, Manifold, Rope"
    ; status       = "not-started"
    ; targetModule = "src/agda/Plan/CIM/Implementation.agda"
    ; next         = []
    }

roadmapGp303 : RoadmapStep
roadmapGp303 = record
    { provenance   = "GP303: The Adjoint Manifest (v3.0)"
    ; relatedNodes = []  -- 
    ; step         = "See full GP file for details"
    ; implication  = "Concepts: Abelian, Functor, Geometry, Manifold, Rope"
    ; status       = "not-started"
    ; targetModule = "src/agda/Plan/CIM/Implementation.agda"
    ; next         = []
    }
