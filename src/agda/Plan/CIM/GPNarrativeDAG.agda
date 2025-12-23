{-# OPTIONS --cubical-compatible --safe #-}

-- | SPPF-style DAG capturing the GP narrative structure
-- Each node has at most 2 parents, showing how concepts build on each other
-- This enables gap analysis: (Capability defined) - (Implementation present)

module Plan.CIM.GPNarrativeDAG where

open import Agda.Builtin.String
open import Agda.Builtin.Nat
open import Agda.Builtin.Maybe
open import Agda.Builtin.List

-- | Node ID type for the DAG
NodeId : Set
NodeId = Nat

-- | Implementation status of a capability
data ImplementationStatus : Set where
  notStarted   : ImplementationStatus
  inProgress   : ImplementationStatus
  implemented  : ImplementationStatus
  verified     : ImplementationStatus

-- | SPPF Node with at most 2 parents
record SPPFNode : Set where
  field
    nodeId      : NodeId
    label       : String
    description : String
    parent1     : Maybe NodeId  -- First parent (if exists)
    parent2     : Maybe NodeId  -- Second parent (if exists)
    status      : ImplementationStatus
    gpRefs      : String        -- GP file references (e.g., "GP01, GP02")

-- ============================================================================
-- Foundation Nodes (N0-N8): Core parsing-as-topology framework
-- ============================================================================

n0 : SPPFNode
n0 = record
  { nodeId = 0
  ; label = "Parsing-as-TQFT"
  ; description = "Core insight: parsing = topological quantum field theory. Parsing = sheaf gluing; ambiguity = cohomology; meaning = global sections."
  ; parent1 = nothing
  ; parent2 = nothing
  ; status = inProgress
  ; gpRefs = "GP01"
  }

n1 : SPPFNode
n1 = record
  { nodeId = 1
  ; label = "Tokens-as-Germs"
  ; description = "Tokens are germs of local sections on a manifold. Each token seeds local parse structure."
  ; parent1 = just 0
  ; parent2 = nothing
  ; status = inProgress
  ; gpRefs = "GP01, GP02"
  }

n2 : SPPFNode
n2 = record
  { nodeId = 2
  ; label = "SPPF-as-Fiber-Bundle"
  ; description = "Shared Packed Parse Forests are fiber bundles. Parse histories are fibers over base space."
  ; parent1 = just 0
  ; parent2 = nothing
  ; status = inProgress
  ; gpRefs = "GP01, GP05"
  }

n3 : SPPFNode
n3 = record
  { nodeId = 3
  ; label = "RoPE-as-Lie-Transport"
  ; description = "Rotational Position Encoding as Lie group action. Positions encode parallel transport through parse forest."
  ; parent1 = just 1
  ; parent2 = just 2
  ; status = inProgress
  ; gpRefs = "GP01, GP10"
  }

n4 : SPPFNode
n4 = record
  { nodeId = 4
  ; label = "Curvature-as-Ambiguity"
  ; description = "Parse ambiguity is curvature in the connection. Parallel transport non-commutativity = multiple interpretations."
  ; parent1 = just 3
  ; parent2 = nothing
  ; status = notStarted
  ; gpRefs = "GP15, GP20"
  }

n5 : SPPFNode
n5 = record
  { nodeId = 5
  ; label = "Braid-Traces"
  ; description = "Parse histories as braids. Token streams are strands; reorderings are crossings."
  ; parent1 = just 2
  ; parent2 = nothing
  ; status = inProgress
  ; gpRefs = "GP02, GP05"
  }

n6 : SPPFNode
n6 = record
  { nodeId = 6
  ; label = "CHIP-Coherence"
  ; description = "Coherent Hierarchical Induction Protocol. Every transformation has coherence witness + magnitude metric."
  ; parent1 = just 4
  ; parent2 = just 5
  ; status = implemented
  ; gpRefs = "GP01-GP111"
  }

n7 : SPPFNode
n7 = record
  { nodeId = 7
  ; label = "Connection-Form"
  ; description = "Explicit connection 1-form on parse bundle. Curvature measured via holonomy."
  ; parent1 = just 3
  ; parent2 = just 4
  ; status = notStarted
  ; gpRefs = "GP20, GP30"
  }

n8 : SPPFNode
n8 = record
  { nodeId = 8
  ; label = "Energy-Metric"
  ; description = "Magnitude of transformations. Least-action paths through parse space."
  ; parent1 = just 6
  ; parent2 = just 7
  ; status = inProgress
  ; gpRefs = "GP01, GP30"
  }

-- ============================================================================
-- Geometry/Polytopes Nodes (N9-N18): Higher structures and rewrite strategies
-- ============================================================================

n9 : SPPFNode
n9 = record
  { nodeId = 9
  ; label = "Yang-Baxter/Hexagon Constraints"
  ; description = "Braid coherence via Yang-Baxter equation. Hexagon identities for monoidal transformations."
  ; parent1 = just 5
  ; parent2 = nothing
  ; status = notStarted
  ; gpRefs = "GP200, GP201"
  }

n10 : SPPFNode
n10 = record
  { nodeId = 10
  ; label = "Associahedra-A∞"
  ; description = "Higher associativity encoded in Stasheff polytopes. A∞-algebras allow coherent associativity relaxation."
  ; parent1 = just 7
  ; parent2 = just 9
  ; status = notStarted
  ; gpRefs = "GP200, GP300"
  }

n11 : SPPFNode
n11 = record
  { nodeId = 11
  ; label = "Polytope-Laplace-Spectrum"
  ; description = "Spectral geometry of parse polytopes. Eigenvalues encode complexity of parse strategies."
  ; parent1 = just 8
  ; parent2 = just 10
  ; status = notStarted
  ; gpRefs = "GP302, GP303"
  }

n12 : SPPFNode
n12 = record
  { nodeId = 12
  ; label = "Loday-Realization"
  ; description = "Geometric realization of associahedra via Loday's construction. Concrete polytope embeddings."
  ; parent1 = just 10
  ; parent2 = nothing
  ; status = notStarted
  ; gpRefs = "GP703, GP704"
  }

n13 : SPPFNode
n13 = record
  { nodeId = 13
  ; label = "Topological-Inflation"
  ; description = "Lift ambiguities into higher-dimensional cells. Projection obscures; inflation clarifies."
  ; parent1 = just 11
  ; parent2 = just 12
  ; status = notStarted
  ; gpRefs = "GP699, GP700"
  }

n14 : SPPFNode
n14 = record
  { nodeId = 14
  ; label = "Formal-Correction-Section"
  ; description = "Explicit correction witness per transformation. Each GP has formal correction annotation."
  ; parent1 = just 6
  ; parent2 = nothing
  ; status = implemented
  ; gpRefs = "GP01-GP832"
  }

n15 : SPPFNode
n15 = record
  { nodeId = 15
  ; label = "Elastic-Deformation-Model"
  ; description = "Parse errors as elastic deformations. Stress/strain view: cost of correction is strain energy."
  ; parent1 = just 8
  ; parent2 = just 14
  ; status = notStarted
  ; gpRefs = "GP400, GP500"
  }

n16 : SPPFNode
n16 = record
  { nodeId = 16
  ; label = "Dimensional-Relief"
  ; description = "Resolve ambiguity by lifting dimension. Like distinguishing 3D objects from 2D shadows."
  ; parent1 = just 13
  ; parent2 = just 15
  ; status = notStarted
  ; gpRefs = "GP501"
  }

n17 : SPPFNode
n17 = record
  { nodeId = 17
  ; label = "MacLane-Pentagon"
  ; description = "Coherent reassociation law. Pentagon identity for associators in monoidal categories."
  ; parent1 = just 10
  ; parent2 = nothing
  ; status = notStarted
  ; gpRefs = "GP200"
  }

n18 : SPPFNode
n18 = record
  { nodeId = 18
  ; label = "Rewrite-Geodesics"
  ; description = "Minimal-cost transitions between parse strategies. Geodesics on polytope graphs."
  ; parent1 = just 11
  ; parent2 = just 17
  ; status = notStarted
  ; gpRefs = "GP302, GP303"
  }

-- ============================================================================
-- Analysis Nodes (N19-N25): Cohomology, sheaves, Yoneda
-- ============================================================================

n19 : SPPFNode
n19 = record
  { nodeId = 19
  ; label = "Cohomology-as-Obstruction"
  ; description = "H¹ detects unresolved ambiguity. Nontrivial cohomology = global obstruction to parsing."
  ; parent1 = just 18
  ; parent2 = just 16
  ; status = notStarted
  ; gpRefs = "GP800"
  }

n20 : SPPFNode
n20 = record
  { nodeId = 20
  ; label = "Exactness-Criterion"
  ; description = "Exact sequences: decomposition/recomposition without information loss."
  ; parent1 = just 19
  ; parent2 = nothing
  ; status = notStarted
  ; gpRefs = "GP818"
  }

n21 : SPPFNode
n21 = record
  { nodeId = 21
  ; label = "Spectral-Clustering-of-Parses"
  ; description = "Cluster parse interpretations via Laplacian eigenmodes. Spectral graph theory for ambiguity."
  ; parent1 = just 10
  ; parent2 = just 18
  ; status = notStarted
  ; gpRefs = "GP302"
  }

n22 : SPPFNode
n22 = record
  { nodeId = 22
  ; label = "Yoneda-View"
  ; description = "Understand parse objects via all morphisms into them. Yoneda embedding for parsers."
  ; parent1 = just 18
  ; parent2 = nothing
  ; status = notStarted
  ; gpRefs = "GP800"
  }

n23 : SPPFNode
n23 = record
  { nodeId = 23
  ; label = "Representable-Parsers"
  ; description = "Parsers as representable functors. Universal property-driven parsing."
  ; parent1 = just 22
  ; parent2 = just 20
  ; status = notStarted
  ; gpRefs = "GP800"
  }

n24 : SPPFNode
n24 = record
  { nodeId = 24
  ; label = "Abelian-Sheaf-Structure"
  ; description = "Parse fragments form Abelian sheaf. Vector addition = coherent gluing of fragments."
  ; parent1 = just 20
  ; parent2 = nothing
  ; status = notStarted
  ; gpRefs = "GP818"
  }

n25 : SPPFNode
n25 = record
  { nodeId = 25
  ; label = "Linear-Section-Space"
  ; description = "Vector spaces of meanings. Sections form linear space with gluing operations."
  ; parent1 = just 23
  ; parent2 = just 24
  ; status = notStarted
  ; gpRefs = "GP818"
  }

-- ============================================================================
-- Fractal/Gauge/Final Nodes (N26-N31): Self-reference and completion
-- ============================================================================

n26 : SPPFNode
n26 = record
  { nodeId = 26
  ; label = "Gauge-Symmetries"
  ; description = "Representational freedom preserving meaning. Local gauge transformations like in physics."
  ; parent1 = just 21
  ; parent2 = just 25
  ; status = notStarted
  ; gpRefs = "GP832"
  }

n27 : SPPFNode
n27 = record
  { nodeId = 27
  ; label = "Renormalization-Over-Scale"
  ; description = "Parsing rules flow with scale changes. Renormalization group for formal languages."
  ; parent1 = just 21
  ; parent2 = just 16
  ; status = notStarted
  ; gpRefs = "GP822"
  }

n28 : SPPFNode
n28 = record
  { nodeId = 28
  ; label = "Lexing=Parsing-All-Scales"
  ; description = "Fractal self-similarity: no distinction between lexing and parsing. Pattern matching at all scales."
  ; parent1 = just 27
  ; parent2 = nothing
  ; status = notStarted
  ; gpRefs = "GP822"
  }

n29 : SPPFNode
n29 = record
  { nodeId = 29
  ; label = "Hyper-Metric-Engine"
  ; description = "Riemannian distance on parse states. Geodesics computed via metric tensor."
  ; parent1 = just 28
  ; parent2 = just 26
  ; status = notStarted
  ; gpRefs = "GP832"
  }

n30 : SPPFNode
n30 = record
  { nodeId = 30
  ; label = "Self-Defining-Manifold"
  ; description = "Manifest v26.0: Parser defines itself. Topology encodes navigation rules."
  ; parent1 = just 29
  ; parent2 = just 23
  ; status = notStarted
  ; gpRefs = "GP832"
  }

n31 : SPPFNode
n31 = record
  { nodeId = 31
  ; label = "Final-Coherence"
  ; description = "Closed loop: self-reference without paradox. All coherence conditions satisfied."
  ; parent1 = just 30
  ; parent2 = just 19
  ; status = notStarted
  ; gpRefs = "GP832"
  }

-- ============================================================================
-- DAG as a list for traversal
-- ============================================================================

allNodes : List SPPFNode
allNodes =
  n0 ∷ n1 ∷ n2 ∷ n3 ∷ n4 ∷ n5 ∷ n6 ∷ n7 ∷ n8 ∷ n9 ∷
  n10 ∷ n11 ∷ n12 ∷ n13 ∷ n14 ∷ n15 ∷ n16 ∷ n17 ∷ n18 ∷ n19 ∷
  n20 ∷ n21 ∷ n22 ∷ n23 ∷ n24 ∷ n25 ∷ n26 ∷ n27 ∷ n28 ∷ n29 ∷
  n30 ∷ n31 ∷ []

-- Topological sort would follow parent dependencies
-- Gap analysis: for each node, check if targetModule has implementation
-- Status tracking: update node.status as implementation progresses
