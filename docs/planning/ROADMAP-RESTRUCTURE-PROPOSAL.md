# Roadmap Restructuring Proposal

## Current Issues

The `Plan.CIM.IngestedRoadmaps` module has several problems:

1.  **Size**: 870 lines with 156 truncated roadmap entries
2.  **String Errors**: Smart quotes, bad escape sequences, truncated text
3.  **Poor Extraction**: Auto-generated from GP\*.md files but loses context
4.  **Flat Structure**: No semantic grouping or dependency tracking

## Analysis of Source Material

The intake/GP/\*.md files contain rich mathematical content organized by topic:

*   **GP01-GP111**: Foundational work (RoPE, SPPF, rotational transport)
*   **GP200-GP303**: Higher geometry (Stasheff polytopes, non-Abelian groups)
*   **GP400-GP501**: Advanced corrections (elasticity, dimensional relief)
*   **GP699-GP714**: Polytope theory (associahedra, spectral methods)
*   **GP800-GP832**: Deep analysis (exterior algebra, cohomology, sheaves)

## Proposed Solution

### Module Structure

    Plan/CIM/IngestedRoadmaps/
    ├── Foundation.agda       -- GP01-GP111: Core SPPF, RoPE, packaging
    ├── Geometry.agda         -- GP200-GP303: Stasheff, polytopes, manifolds  
    ├── Corrections.agda      -- GP400-GP501: Elasticity, dimensional work
    ├── Polytopes.agda        -- GP699-GP714: Associahedra, spectral theory
    ├── Analysis.agda         -- GP800-GP832: Cohomology, sheaves, fractals
    └── Index.agda            -- Re-exports and combined roadmap

    Plan/CIM/IngestedRoadmaps.agda  -- Top-level module, re-exports all

### Better Extraction Strategy

Instead of truncating strings, extract:

1.  **Theme**: From section headers (e.g., "I. Formal Correction: The Quantum Manifold")
2.  **Actionable Items**: The "Would you like me to..." questions
3.  **Dependencies**: References to other GP files or Agda modules
4.  **Target Implementation**: Which modules/files to modify
5.  **Mathematical Context**: Key concepts (RoPE, Stasheff, Sheaves, etc.)

### RoadmapStep Improvements

The current `RoadmapStep` record should be enhanced:

```agda
record RoadmapStep : Set₁ where
  field
    gpNumber     : String           -- GP701
    theme        : String           -- "The Loday Realization"  
    category     : String           -- "Polytope Theory"
    relatedGPs   : List String      -- ["GP700", "GP702"]
    actionItems  : List String      -- Concrete tasks
    concepts     : List String      -- ["Associahedron", "Stasheff"]
    targetModules: List String      -- Where to implement
    status       : String           -- not-started/in-progress/completed
    next         : List RoadmapStep -- Sub-tasks
```

## Implementation Strategy

### Phase 1: Clean Extraction (Immediate)

1.  Fix the parser script that generates IngestedRoadmaps
2.  Extract full section content, not truncated strings
3.  Properly escape all strings (handle quotes, backslashes)
4.  Group by mathematical theme, not just number

### Phase 2: Semantic Organization (Short-term)

1.  Build dependency graph between GP files
2.  Identify critical path (which GPs must be done first)
3.  Create proper sub-modules with complete content
4.  Add cross-references and related concepts

### Phase 3: Integration (Medium-term)

1.  Link roadmap items to actual Agda modules
2.  Track implementation status programmatically
3.  Generate progress reports from compilation results
4.  Validate that completed items have corresponding code

## Temporary Fix for Docs Build

For immediate unblocking:

1.  Comment out `IngestedRoadmaps` imports in modules that use it
2.  Or create minimal stub modules that compile
3.  Fix string escaping issues in existing file
4.  Re-enable once proper extraction is working

## Questions for User

1.  Should we keep auto-generation from GP\*.md files, or curate roadmaps manually?
2.  Priority: Get docs building now, or restructure properly first?
3.  Are the GP files the source of truth, or is IngestedRoadmaps?
4.  Should roadmap completion tracking be automated or manual?
