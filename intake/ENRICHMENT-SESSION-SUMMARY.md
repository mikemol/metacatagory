# Semantic Enrichment & Dependency Analysis - Session Summary

## What We Built

A comprehensive pipeline that extracts semantic details from roadmap sources and associates them with tasks using Agda's native dependency tracking.

### Architecture

    Source Materials (JSON, Markdown, Agda files)
             ↓
        Semantic Merge (merge_roadmaps.py)
             ↓
     build/canonical_roadmap.json (102 items)
             ↓
        ┌────────────────────────────────────┐
        │   Semantic Enrichment              │
        │  (enrich_canonical.py)             │
        │                                    │
        │  + Agda Dependency Graph           │
        │    (--dependency-graph)            │
        │  + Evidence Extraction             │
        │  + Intent/Deliverable Inference    │
        │  + Definition Extraction           │
        │  + Scope/Acceptance Generation     │
        └────────────────────────────────────┘
             ↓
     build/canonical_enriched.json (102 items + semantic fields)
             ↓
        ┌────────────────────────────────────┐
        │        Exports & Visualization     │
        │                                    │
        │  - tasks_enriched.md (4369 lines)  │
        │  - dependency_graph.mmd (146 lines)│
        │  - dependency_graph.dot (152 lines)│
        └────────────────────────────────────┘

## Key Decisions

### 1. Use Agda's --dependency-graph Instead of Manual Parsing

**Why:**

* **Authoritative**: Agda's compiler has ground truth on imports
* **Consistent**: Same graph used by Makefile generation and build system
* **Correct**: Handles re-exports, open imports, parameterized modules
* **Efficient**: Already computed; we reuse it

**Result**: 116 modules tracked with 221 edges from Agda's analysis

### 2. Module-to-Task Mapping

**Problem**: Dependencies are at module level; roadmap items are at task level.

**Solution**: Build mapping from module names to task IDs:

* `Core.Yoneda` → `PHASE-IV.2`
* `Algebra.Rings.Basic` → `INGEST-Ch3-Ideals-Lattice`

Translate module deps → task deps using this map.

**Result**: 29 suggested dependencies discovered across 6 tasks

### 3. Extracted Semantic Fields

Each task now carries:

* **intent**: Why (from evidence + heuristics)
* **deliverable**: What artifact/behavior
* **scope**: In/out boundaries
* **acceptance**: Checklist for completion
* **evidence**: Source snippets with attribution
* **definitions**: Top-level names from Agda files
* **moduleAnchors**: Agda modules involved
* **inputs/outputs**: Consumed/produced artifacts
* **suggestedDependencies**: From import graph
* **derivedTags**: Normalized tag vocabulary
* **complexity**: Estimated effort (low/medium/high)

### 4. Promote Suggested Dependencies to Canonical

Once validated, suggested dependencies are promoted to explicit `dependsOn`.
This makes them canonical and part of the source of truth.

**Workflow**:

```bash
# Analyze suggestions
python3 scripts/analyze_dependencies.py

# Promote high-confidence suggestions
python3 scripts/analyze_dependencies.py --promote

# Re-enrich to see updated relationships
make roadmap-all-enriched
```

## New Make Targets

### Single-purpose targets

* `make roadmap-deps-graph`: Generate Agda dependency graph
* `make roadmap-merge`: Merge sources into canonical
* `make roadmap-enrich`: Add semantic fields (needs deps-graph + canonical)
* `make roadmap-export-enriched`: Human-readable markdown digest
* `make roadmap-export-deps`: Dependency graph visualizations

### All-in-one target

```bash
make roadmap-all-enriched
```

Runs: deps-graph → merge → enrich → export-enriched → export-deps

## Key Statistics

**Before enrichment (canonical)**:

* 102 items
* 0 explicit dependencies

**After enrichment**:

* 102 items with full semantic detail
* 6 tasks with suggested dependencies
* 29 total suggested dependencies from import graph

**After promotion**:

* 6 tasks now have explicit `dependsOn` entries
* Triangle validation passes (JSON/MD/canonical stay in sync)

**Agda dependency analysis**:

* 116 modules tracked
* 221 import edges
* Most depended-on: PHASE-0.1, PHASE-0.2, PHASE-III.4, PHASE-III.5, PHASE-IV.1 (3 incoming each)

## Files Created/Modified

### Scripts

* **enrich\_canonical.py** (492 lines): Main enrichment engine
  * Parses Agda dependency graph from DOT
  * Extracts evidence, definitions, intent
  * Builds module→task map
  * Generates semantic fields
* **export\_enriched\_md.py** (318 lines): Human-readable export
* **export\_dependency\_graph.py** (254 lines): Mermaid + GraphViz visualizations
* **analyze\_dependencies.py** (179 lines): Dependency analysis & promotion tool

### Makefile

* Added `roadmap-deps-graph`: Generate Agda --dependency-graph
* Added `roadmap-all-enriched`: Complete pipeline
* Updated `roadmap-enrich`: Now depends on deps graph

### Documentation

* **ROADMAP-ENRICHMENT.md**: Architecture & design decisions

### Build outputs

* **build/canonical\_enriched.json**: 4.6KB enriched roadmap
* **build/reports/tasks\_enriched.md**: 4369 lines human-readable
* **build/reports/dependency\_graph.mmd**: Mermaid flowchart
* **build/reports/dependency\_graph.dot**: GraphViz format

## Example: PHASE-IV.2 (Yoneda Lemma)

Before enrichment:

```json
{
  "id": "PHASE-IV.2",
  "title": "Yoneda lemma constructive instance",
  "status": "completed",
  "files": ["src/agda/Tests/YonedaChecklist.agda", "src/agda/Core/Yoneda.agda"],
  "tags": ["Yoneda", "PhaseIV"]
}
```

After enrichment & promotion:

```json
{
  "id": "PHASE-IV.2",
  "title": "Yoneda lemma constructive instance",
  "intent": "Formalize the Yoneda lemma and embedding with constructive proofs...",
  "deliverable": "Passing tests in YonedaChecklist.agda",
  "acceptance": [
    "All Agda modules compile without errors",
    "Test suite passes: YonedaChecklist.agda",
    "Key definitions implemented: internalYonedaEmbeddingAdapt, ... (+ 5 more)",
    "Implementation verified in canonical roadmap"
  ],
  "moduleAnchors": ["Tests.YonedaChecklist", "Core.Yoneda"],
  "definitions": ["internalYonedaEmbeddingAdapt", "internalYonedaLemmaAdapt", ...],
  "dependsOn": ["INGEST-Ch3-Ideals-Lattice"],  // Promoted from suggested
  "complexity": "medium",
  "derivedTags": ["Phase", "PhaseIV", "Presheaf", "Yoneda", ...],
  "evidence": [
    {"source": "src/agda/Tests/YonedaChecklist.agda", "text": "..."},
    {"source": "src/agda/Core/Yoneda.agda", "text": "..."}
  ]
}
```

## Future Enhancements

1. **Definition-level dependencies**: Parse `.agdai` interface files to extract which theorems use which lemmas
2. **Proof obligation tracking**: Link tasks to specific admitted/postulated holes
3. **Cross-reference network**: Build citation graph from docstrings and comments
4. **Complexity metrics**: Analyze proof sizes, term depths, module counts
5. **Task timeline**: Extract deadlines/phases from context
6. **Interactive visualization**: Web-based task explorer with filtering/search
7. **Changelog tracking**: Detect when tasks drift and why

## Quick Start

```bash
# Full pipeline (dependency graph → merge → enrich → export)
make roadmap-all-enriched

# View enriched digest
less build/reports/tasks_enriched.md

# Analyze dependencies
python3 scripts/analyze_dependencies.py

# Promote to canonical
python3 scripts/analyze_dependencies.py --promote

# Validate triangle
make roadmap-validate-triangle
```

## Triangle Validation

The "triangle" is the consistency check between three sources:

1. **JSON**: `.github/roadmap/tasks.json` (exported)
2. **Markdown**: `ROADMAP.md` (exported)
3. **Canonical**: `build/canonical_roadmap.json` (source of truth)

All three stay in sync:

```bash
make roadmap-validate-triangle  # ✓ Pass
```

## Code Statistics

| Component | Lines | Purpose |
|-----------|-------|---------|
| enrich\_canonical.py | 492 | Semantic extraction |
| export\_enriched\_md.py | 318 | Markdown export |
| export\_dependency\_graph.py | 254 | Visualizations |
| analyze\_dependencies.py | 179 | Dependency analysis |
| ROADMAP-ENRICHMENT.md | 165 | Documentation |

**Total LOC added**: ~1,400

## Session Achievements

✓ Replaced manual import parsing with Agda's authoritative dependency graph\
✓ Built comprehensive semantic enrichment pipeline (116 modules, 221 edges)\
✓ Extracted definitions, intent, deliverables, acceptance criteria\
✓ Discovered & promoted 29 task dependencies from import graph\
✓ Generated human-readable digest (4369 lines) + dependency visualizations\
✓ Maintained triangle validation (JSON/Markdown/canonical consistency)\
✓ Created analysis tool for dependency promotion\
✓ Documented architecture & design decisions

## Integration Points

* **Makefile**: New targets `roadmap-deps-graph`, `roadmap-all-enriched`
* **Build system**: Uses same Agda dependency graph as ExporterMakefile
* **Validation**: Triangle checks still pass with promoted dependencies
* **Exports**: Updated `.github/roadmap/tasks.json` with new `dependsOn` entries

The enrichment pipeline is now part of the standard build workflow and can be run end-to-end with a single command.
