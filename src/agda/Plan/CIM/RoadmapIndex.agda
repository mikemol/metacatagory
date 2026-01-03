{-# OPTIONS --without-K #-}

-- | RoadmapIndex: canonical, machine-actionable consolidation of roadmap items
-- Architecture: pure core with parameterized adapters; FFI at the edge.
-- Note: --safe removed to allow canonical postulate (external data import)

module Plan.CIM.RoadmapIndex where

open import Agda.Builtin.String using (String; primStringAppend; primStringEquality)
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Maybe using (Maybe; just; nothing)
open import Agda.Builtin.Bool using (Bool; true; false)

------------------------------------------------------------------------
-- Canonical schema
------------------------------------------------------------------------

record RoadmapItem : Set where
  field
    id          : String
    title       : String
    description : String
    status      : String
    category    : String
    source      : String
    files       : List String
    tags        : List String
    dependsOn   : List String
    provenance  : List String
    related     : List String

------------------------------------------------------------------------
-- Adapter interface (pure requirement)
------------------------------------------------------------------------

-- | Source adapter yields a list of RoadmapItem for a given origin.
record RoadmapAdapter : Set₁ where
  field
    name    : String
    items   : List RoadmapItem

------------------------------------------------------------------------
-- Consolidation logic (pure)
------------------------------------------------------------------------

_++ˡ_ : ∀ {A : Set} → List A → List A → List A
[] ++ˡ ys = ys
(x ∷ xs) ++ˡ ys = x ∷ (xs ++ˡ ys)

eqString : String → String → Bool
eqString = primStringEquality

containsId : String → List RoadmapItem → Bool
containsId _ [] = false
containsId k (r ∷ rs) with eqString k (RoadmapItem.id r)
... | true  = true
... | false = containsId k rs

-- | Merge items from multiple adapters (naive concatenation; dedup later).
consolidate : List RoadmapAdapter → List RoadmapItem
consolidate [] = []
consolidate (a ∷ as) = RoadmapAdapter.items a ++ˡ consolidate as

-- | Placeholder for deduplication by id (architecture stub).
dedupById : List RoadmapItem → List RoadmapItem
dedupById xs = go xs []
  where
    go : List RoadmapItem → List RoadmapItem → List RoadmapItem
    go [] acc = acc
    go (r ∷ rs) acc with containsId (RoadmapItem.id r) acc
    ... | true  = go rs acc
    ... | false = go rs (acc ++ˡ (r ∷ []))

-- | Consolidated unified index.
unifiedIndex : List RoadmapAdapter → List RoadmapItem
unifiedIndex adapters = dedupById (consolidate adapters)

------------------------------------------------------------------------
-- Canonical index (parameterized, no postulates)
------------------------------------------------------------------------

-- | Canonical roadmap data abstraction
record CanonicalRoadmap : Set where
  field
    items : List RoadmapItem

-- | Modules that depend on canonical data can be parameterized:
module WithCanonical (canon : CanonicalRoadmap) where
  canonical : List RoadmapItem
  canonical = CanonicalRoadmap.items canon

------------------------------------------------------------------------
-- Built-in adapters (pure specimens)
------------------------------------------------------------------------

-- | Adapter from static tasks (e.g., imported from .github/roadmap/tasks.json)
-- In pure core, we treat these as parameters; a boundary module can populate.
record TasksJson : Set where
  field
    items : List RoadmapItem

tasksJsonAdapter : TasksJson → RoadmapAdapter
RoadmapAdapter.name (tasksJsonAdapter tj) = "tasks.json"
RoadmapAdapter.items (tasksJsonAdapter tj) = TasksJson.items tj

-- | Adapter from Agda-ingested roadmap modules (Plan.CIM.IngestedRoadmaps)
record AgdaIngest : Set where
  field
    items : List RoadmapItem

agdaIngestAdapter : AgdaIngest → RoadmapAdapter
RoadmapAdapter.name (agdaIngestAdapter ai) = "agda-ingest"
RoadmapAdapter.items (agdaIngestAdapter ai) = AgdaIngest.items ai

-- | Adapter from Markdown sources (e.g., ROADMAP.md, DRAFTs)
record MarkdownIngest : Set where
  field
    items : List RoadmapItem

markdownAdapter : MarkdownIngest → RoadmapAdapter
RoadmapAdapter.name (markdownAdapter md) = "markdown"
RoadmapAdapter.items (markdownAdapter md) = MarkdownIngest.items md

------------------------------------------------------------------------
-- Example: empty consolidated index (to keep compilation under --safe)
------------------------------------------------------------------------

emptyTasks : TasksJson
TasksJson.items emptyTasks = []

emptyAgda : AgdaIngest
AgdaIngest.items emptyAgda = []

emptyMd : MarkdownIngest
MarkdownIngest.items emptyMd = []

exampleIndex : List RoadmapItem
exampleIndex = unifiedIndex (tasksJsonAdapter emptyTasks ∷ agdaIngestAdapter emptyAgda ∷ markdownAdapter emptyMd ∷ [])
