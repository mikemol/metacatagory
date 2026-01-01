{-# OPTIONS --without-K #-}

-- | Example registry mapping technical debt to roadmap SPPF nodes.
module Examples.TechnicalDebtRegistry where

open import Plan.CIM.RoadmapSPPF using (SPPFNode; SPPFGraph)
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Maybe using (Maybe; just; nothing)

-- Technical debt registry as a canonical SPPFGraph
-- Each debt item is an SPPFNode; the registry is an SPPFGraph

debtNode1 : SPPFNode
-- Example node; extend as needed
-- Use tags or category for technical debt classification
-- Use parent1/parent2 for dependency/aggregation

debtNode1 = record
  { nodeId   = "TD001"
  ; title    = "Refactor registry to SPPFGraph"
  ; status   = "not-started"
  ; category = "technical-debt"
  ; source   = "src/agda/Examples/TechnicalDebtRegistry.agda"
  ; files    = []
  ; tags     = "core-refactor" ∷ []
  ; parent1  = nothing
  ; parent2  = nothing
  }

debtRegistry : SPPFGraph
debtRegistry = record { nodes = debtNode1 ∷ [] }
