{-# OPTIONS --without-K #-}

-- | RoadmapSPPF: compose/decompose roadmap data into an SPPF (≤2-parent invariant).
module Plan.CIM.RoadmapSPPF where

open import Agda.Builtin.String using (String; primStringAppend)
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Maybe using (Maybe; just; nothing)
open import Agda.Builtin.Bool using (Bool; true; false)

open import Plan.CIM.RoadmapIndex using (RoadmapItem; RoadmapAdapter; unifiedIndex)
open import Plan.CIM.PlanningKernel using (planningAdapters)

------------------------------------------------------------------------
-- SPPF schema for roadmap nodes
------------------------------------------------------------------------

record SPPFNode : Set where
  field
    nodeId     : String
    title      : String
    status     : String
    category   : String
    source     : String
    files      : List String
    tags       : List String
    parent1    : Maybe String
    parent2    : Maybe String

record SPPFGraph : Set where
  field
    nodes : List SPPFNode

record PackResult : Set where
  field
    packedNodes : List SPPFNode
    p1          : Maybe String
    p2          : Maybe String

------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------

_++ˡ_ : ∀ {A : Set} → List A → List A → List A
[] ++ˡ ys = ys
(x ∷ xs) ++ˡ ys = x ∷ (xs ++ˡ ys)

_++_ : String → String → String
_++_ = primStringAppend

------------------------------------------------------------------------
-- Packing parents when >2 dependencies
------------------------------------------------------------------------

mkPacked : String → String → String → SPPFNode
SPPFNode.nodeId   (mkPacked pid _ _) = pid
SPPFNode.title    (mkPacked _ a b) = "packed:" ++ (a ++ ("+" ++ b))
SPPFNode.status   (mkPacked _ _ _) = "packed"
SPPFNode.category (mkPacked _ _ _) = "SPPF"
SPPFNode.source   (mkPacked _ _ _) = "packing"
SPPFNode.files    (mkPacked _ _ _) = []
SPPFNode.tags     (mkPacked _ _ _) = []
SPPFNode.parent1  (mkPacked _ a _) = just a
SPPFNode.parent2  (mkPacked _ _ b) = just b

-- | Create a synthetic packed node id from two parents and a base
packedId : String → String → String → String
packedId base a b = base ++ (".packed." ++ (a ++ ("+" ++ b)))

{-# TERMINATING #-}
packDeps : String → List String → PackResult
packDeps base [] = record { packedNodes = [] ; p1 = nothing ; p2 = nothing }
packDeps base (a ∷ []) = record { packedNodes = [] ; p1 = just a ; p2 = nothing }
packDeps base (a ∷ b ∷ []) = record { packedNodes = [] ; p1 = just a ; p2 = just b }
packDeps base (a ∷ b ∷ c ∷ rest) =
  let pid  = packedId base a b
      node = mkPacked pid a b
      next = pid ∷ c ∷ rest
      rec  = packDeps base next
  in record
       { packedNodes = node ∷ PackResult.packedNodes rec
       ; p1          = PackResult.p1 rec
       ; p2          = PackResult.p2 rec
       }

------------------------------------------------------------------------
-- Normalization: RoadmapItem → SPPFNode(s)
------------------------------------------------------------------------

fromItemPacked : RoadmapItem → List SPPFNode
fromItemPacked i =
  let deps = RoadmapItem.dependsOn i
      res  = packDeps (RoadmapItem.id i) deps
      node = record
        { nodeId     = RoadmapItem.id i
        ; title      = RoadmapItem.title i
        ; status     = RoadmapItem.status i
        ; category   = RoadmapItem.category i
        ; source     = RoadmapItem.source i
        ; files      = RoadmapItem.files i
        ; tags       = RoadmapItem.tags i
        ; parent1    = PackResult.p1 res
        ; parent2    = PackResult.p2 res
        }
  in node ∷ PackResult.packedNodes res

------------------------------------------------------------------------
-- Aggregation: adapters → SPPF graph
------------------------------------------------------------------------

toSPPF : List RoadmapAdapter → SPPFGraph
SPPFGraph.nodes (toSPPF adapters) = mapItems (unifiedIndex adapters)
  where
    mapItems : List RoadmapItem → List SPPFNode
    mapItems [] = []
    mapItems (x ∷ xs) = fromItemPacked x ++ˡ mapItems xs

-- | Default SPPF graph derived from the planning kernel’s adapters.
planningSPPF : SPPFGraph
planningSPPF = toSPPF planningAdapters

------------------------------------------------------------------------
-- Composition / Decomposition helpers
------------------------------------------------------------------------

compose : String → String → SPPFNode → SPPFNode → SPPFNode
SPPFNode.nodeId   (compose nid t a b) = nid
SPPFNode.title    (compose nid t a b) = t
SPPFNode.status   (compose nid t a b) = "composed"
SPPFNode.category (compose nid t a b) = "Composite"
SPPFNode.source   (compose nid t a b) = "SPPF"
SPPFNode.files    (compose nid t a b) = []
SPPFNode.tags     (compose nid t a b) = []
SPPFNode.parent1  (compose nid t a b) = just (SPPFNode.nodeId a)
SPPFNode.parent2  (compose nid t a b) = just (SPPFNode.nodeId b)

decompose : SPPFNode → List String
decompose n with SPPFNode.parent1 n | SPPFNode.parent2 n
... | nothing | nothing = []
... | just p | nothing = p ∷ []
... | nothing | just q = q ∷ []
... | just p | just q = p ∷ q ∷ []
