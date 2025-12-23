# Quick Start: Semantic Enrichment Pipeline

## One-Command Complete Pipeline

```bash
make roadmap-all-enriched
```

This runs:
1. Generate Agda dependency graph (116 modules, 221 edges)
2. Merge roadmap sources (470 items → 102 canonical)
3. Enrich with semantic fields (intent, deliverable, dependencies, etc.)
4. Export human-readable digest and visualizations

## What You Get

**Files generated:**
- `build/canonical_enriched.json` - Enriched roadmap (150KB)
- `build/reports/tasks_enriched.md` - Human-readable digest (4,369 lines)
- `build/reports/dependency_graph.mmd` - Mermaid flowchart (7.6KB)
- `build/reports/dependency_graph.dot` - GraphViz format (13KB)
- `.github/roadmap/tasks.json` - Exported to GitHub (with new dependencies)

## View Results

```bash
# Human-readable enriched tasks
less build/reports/tasks_enriched.md

# Analyze dependencies
python3 scripts/analyze_dependencies.py

# Show specific task
python3 scripts/analyze_dependencies.py --task PHASE-IV.2

# Promote suggestions to canonical (after review)
python3 scripts/analyze_dependencies.py --promote
```

## Individual Commands

If you want to run steps separately:

```bash
# 1. Generate Agda dependency graph (116 modules)
make roadmap-deps-graph

# 2. Merge sources into canonical JSON
make roadmap-merge

# 3. Enrich with semantic details (needs steps 1 & 2)
make roadmap-enrich

# 4. Export human-readable markdown
make roadmap-export-enriched

# 5. Generate dependency visualizations
make roadmap-export-deps

# 6. Validate consistency (JSON/Markdown/canonical)
make roadmap-validate-triangle
```

## Key Outputs Explained

### canonical_enriched.json
Each item now has:
- **intent**: Why (from evidence)
- **deliverable**: What artifact/behavior
- **scope**: In/out boundaries
- **acceptance**: Checklist for completion
- **evidence**: Source snippets with attribution
- **definitions**: Top-level Agda names
- **moduleAnchors**: Agda modules involved
- **complexity**: Estimated effort (low/medium/high)
- **suggestedDependencies**: From import graph
- **derivedTags**: Normalized vocabulary

### tasks_enriched.md
Human-readable format with:
- One section per task (102 sections)
- All semantic fields formatted nicely
- Evidence excerpts from sources
- Dependency relationships
- Status/complexity breakdown
- Statistics at end

### dependency_graph.*
Two formats for visualization:
- **Mermaid (.mmd)**: Embed in markdown, renders in GitHub
- **GraphViz (.dot)**: Render with `dot -Tpng -o graph.png dependency_graph.dot`

Color coding:
- Green: Completed
- Blue: Planned
- Orange/tan: Not started
- Gray: Deferred

## Understanding Suggested Dependencies

Tasks can have two types of dependencies:

1. **Explicit** (`dependsOn`): Manually specified, canonical
2. **Suggested** (`suggestedDependencies`): Auto-discovered from Agda imports

Example (PHASE-IV.2 - Yoneda):
```
moduleAnchors: [Tests.YonedaChecklist, Core.Yoneda]

Imports from Core.Yoneda:
  → Algebra.Rings.Basic (from INGEST-Ch3-Ideals-Lattice)

Suggested: [INGEST-Ch3-Ideals-Lattice]
```

### Promoting Suggestions

After review, promote high-confidence suggestions:
```bash
python3 scripts/analyze_dependencies.py --promote
```

This:
1. Adds suggestions to `dependsOn` in canonical
2. Maintains triangle consistency (JSON/MD/canonical)
3. Makes them actionable in tracking

Then re-enrich to see updated relationships:
```bash
make roadmap-all-enriched
```

## Pipeline Architecture

```
Source Materials (JSON, MD, Agda files)
         ↓
   merge_roadmaps.py
         ↓
canonical_roadmap.json (102 items, no enrichment)
         ↓
    ┌────────────────────────────────┐
    │  enrich_canonical.py           │
    │                                │
    │  Uses:                         │
    │  • Agda --dependency-graph     │
    │  • Evidence extraction         │
    │  • Intent/deliverable inference│
    │  • Definition extraction       │
    │  • Scope/acceptance generation │
    └────────────────────────────────┘
         ↓
canonical_enriched.json (102 items + fields)
         ↓
    export_enriched_md.py  →  tasks_enriched.md
    export_dependency_graph.py  →  {.mmd, .dot}
    analyze_dependencies.py  →  analysis & promotion
```

## Why This Approach?

### Agda --dependency-graph

Instead of manually parsing imports, we use Agda's authoritative dependency graph:
- **Accurate**: Handles re-exports, open imports, parameterized modules
- **Consistent**: Same graph used by build system and Makefile generation
- **Efficient**: Reuses Agda's already-computed analysis
- **Maintainable**: Changes to imports automatically reflect in dependencies

### Module-to-Task Mapping

Connects Agda's module-level dependencies to roadmap's task level:
- `Core.Yoneda` → PHASE-IV.2
- `Algebra.Rings.Basic` → INGEST-Ch3-Ideals-Lattice
- If task X's modules import task Y's modules → X depends on Y

### Semantic Extraction

Rich annotations from multiple sources:
- **Evidence**: Direct quotes from markdown & Agda doc comments
- **Intent**: Inferred from evidence + heuristics
- **Deliverable**: What gets produced (tests, modules, proofs)
- **Acceptance**: Checklist derived from status + files
- **Definitions**: Extracted from Agda source code

## Statistics

**Current state:**
- 102 tasks in canonical
- 29 suggested dependencies discovered
- 6 tasks with import-based relationships
- 116 modules tracked from Agda
- 221 import edges analyzed
- 4,369 lines in human-readable digest

**Promotion:**
- 0 conflicts between suggestions and existing `dependsOn`
- All 29 suggestions safe to promote
- Triangle validation passes after promotion

## Troubleshooting

**"agda-deps-full.dot not found"**
```bash
make roadmap-deps-graph
# or manually:
agda --dependency-graph=build/diagrams/agda-deps-full.dot -i src/agda src/agda/Tests/Index.agda
```

**"canonical_enriched.json missing"**
```bash
make roadmap-enrich
# (requires canonical_roadmap.json and agda-deps-full.dot)
```

**"Triangle validation fails"**
```bash
make roadmap-export-json
make roadmap-validate-triangle
```

**"Want to reset suggested dependencies"**
```bash
git checkout build/canonical_roadmap.json
make roadmap-all-enriched
```

## Next Steps

1. **Review enriched digest**: `less build/reports/tasks_enriched.md`
2. **Analyze dependencies**: `python3 scripts/analyze_dependencies.py`
3. **Promote suggestions**: `python3 scripts/analyze_dependencies.py --promote`
4. **Validate consistency**: `make roadmap-validate-triangle`
5. **Integrate into CI/CD**: Add `make roadmap-all-enriched` to continuous builds
6. **Explore visualizations**: View dependency graphs in Mermaid or GraphViz

## Integration with Existing Workflow

The enrichment pipeline integrates with existing make targets:

```bash
# Full development workflow
make agda-all                 # Compile all Agda
make roadmap-all-enriched     # Enrich roadmap
make roadmap-validate-triangle # Validate consistency
make md-lint                  # Lint markdown
```

All outputs are committed to git:
- `build/canonical_roadmap.json` - source of truth
- `.github/roadmap/tasks.json` - GitHub export
- `ROADMAP.md` - markdown export
- `build/canonical_enriched.json` - enriched data
- `build/reports/tasks_enriched.md` - human-readable
- `build/diagrams/agda-deps-full.dot` - dependency graph
