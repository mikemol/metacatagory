# JSON Decomposition via Natural Transformations

**Status**: Planning (not-started)  
**Roadmap**: [BUILD-JSON-DECOMPOSITION](../../ROADMAP.md#build-json-decomposition)  
**Agda Formalization**: [src/agda/Plan/CIM/JSONTransformation.agda](../../src/agda/Plan/CIM/JSONTransformation.agda) (planned)

## Overview

Large monolithic JSON artifacts in `build/` (dependency graphs, roadmap enrichment, planning indices) are currently single files, making them:
- ❌ Difficult to diff (entire file changes for single item updates)
- ❌ Hard to parallelize (must regenerate whole file)
- ❌ Inefficient for queries (need to scan entire structure)
- ✅ Machine-readable at filesystem level

This document describes a **natural transformation** between:
- **Source**: Monolithic JSON structures
- **Target**: Hierarchical JSON structures split across subdirectories with index manifests

The transformation is:
- **Lossless**: `backward ∘ forward = id` (recovers original bit-for-bit)
- **Structure-preserving**: Respects composition and aggregation
- **Formalized**: Proofs in Agda establish isomorphism properties

## Architectural Pattern

This follows the SPPF (Shared Packed Parse Forest) model where:
- Large structures decompose into reusable subtrees
- Index files act as skip-lists for efficient navigation
- Fragments can be updated/processed independently
- The whole reconstructs from parts without loss

## Target Hierarchies

### 1. Dependency Graph (`data/deps/`)

**Input**: `data/dependency_graph.json` (84 modules, 151 edges)

**Output Structure**:
```
data/deps/
├── _metadata.json              # { total_modules, total_deps, cycles, layers }
├── modules/
│   ├── _index.json             # [ "Algebra.Groups.Abelian", ... ]
│   ├── Algebra/
│   │   ├── _index.json         # [ "Groups.Abelian", "Groups.Basic", ... ]
│   │   └── Groups/
│   │       ├── Abelian.json    # { imports: [], imported_by: [] }
│   │       └── Basic.json
│   └── ...
├── layers/                     # Stratified by dependency depth
│   ├── _index.json             # ["layer-0", "layer-1", ...]
│   ├── layer-0.json            # leaf modules
│   └── ...
└── cycles/                     # If cycles exist
    ├── _index.json             # [ "cycle-001", ... ]
    └── cycle-001.json
```

**Benefits**:
- Query: "all modules in Algebra" via `Algebra/_index.json`
- Update: Change one module without rewriting 84-module file
- Hierarchy: Natural package structure emerges

### 2. Roadmap Enrichment (`data/enriched/`)

**Input**: `build/canonical_enriched.json` (roadmap items + semantic annotations)

**Output Structure**:
```
data/enriched/
├── _metadata.json              # { total_items, categories, sources, timestamp }
├── items/
│   ├── _index.json             # [ {id, category, status}, ... ]
│   ├── 001-item-name.json      # Full item record
│   ├── 002-item-name.json
│   └── ...
├── dependencies/
│   ├── _manifest.json          # Which items depend on what
│   ├── item-001.deps.json      # { id: "001", dependsOn: [...] }
│   └── ...
└── annotations/                # Semantic enrichment layer
    ├── _sources.json           # map item-id → provenance
    ├── item-001.source.json    # { source: "...", evidence: [...] }
    └── ...
```

**Benefits**:
- Filter: "all items in category X" via `items/_index.json`
- Modularity: Source annotations separate from item core
- Traceability: Provenance linkage at granular level

### 3. Planning Index (`data/planning/`)

**Input**: `data/planning_index.json` (merged roadmap + metadata)

**Output Structure**:
```
data/planning/
├── _metadata.json              # { total_items, categories, adapters, timestamp }
├── items/
│   ├── _index.json             # [ {id, title, category}, ... ]
│   ├── category/
│   │   ├── _index.json         # [ item-id, ... ]
│   │   ├── item-001.json
│   │   └── ...
├── sources/
│   ├── _manifest.json          # Which adapters contributed what
│   ├── canonical.json          # { items: [...], count: 45 }
│   └── local-definition.json   # { items: [...], count: 8 }
└── artifacts/
    ├── _index.json             # All target files
    └── files.json              # { source_file → [items] }
```

**Benefits**:
- Incremental: Update single adapter's items without touching others
- Multi-source: Clear attribution of items to planning adapters
- Composition: Natural way to merge roadmaps

## Category-Theoretic Formulation

### Objects

**Monolithic**:
```agda
record Monolithic : Set where
  field
    content : JSON
```

**Hierarchical**:
```agda
record Hierarchical : Set where
  field
    metadata : JSON
    items : List (Filepath × JSON)  -- all {_index,_manifest,...}.json files
    manifest : ManifestSpec         -- rules for reconstruction
```

### Morphisms (Natural Transformations)

```agda
record JSONTransformation : Set where
  field
    -- Forward: decompose monolithic to hierarchical
    forward : Monolithic → Hierarchical
    
    -- Backward: reconstruct monolithic from hierarchical
    backward : Hierarchical → Monolithic
    
    -- Roundtrip law: backward ∘ forward = id
    roundtrip : ∀ m → backward (forward m) ≡ m
    
    -- Structure preservation: composition is preserved
    structure-preserving : ∀ m₁ m₂ →
      compose(forward(m₁), forward(m₂)) ≡ forward(compose(m₁, m₂))
```

### Proof Obligations

For each decomposition target:
1. **Losslessness**: `roundtrip` proof (by JSON comparison)
2. **Completeness**: All data in monolithic appears in hierarchical
3. **Determinism**: `forward(m)` always produces same output
4. **Idempotence**: `forward(backward(h)) ≡ h`

## Implementation Roadmap

### Phase 1: Schema & Theory (Weeks 1-2)

- `BUILD-JSON-SCHEMA`: Define Agda types
- `BUILD-JSON-HIERARCHY`: Document target structures
- Proofs: formalize isomorphism properties

### Phase 2: Implementation (Weeks 2-3)

- `BUILD-JSON-FORWARD`: Python decomposer
- `BUILD-JSON-BACKWARD`: Python recomposer
- `BUILD-JSON-VALIDATION`: CI roundtrip check

### Phase 3: Application (Weeks 3-4)

- `BUILD-JSON-APPLY-DEPS`: Decompose dependency graph
- `BUILD-JSON-APPLY-ENRICHED`: Decompose roadmap enrichment
- `BUILD-JSON-APPLY-PLANNING`: Decompose planning index

### Phase 4: Integration (Week 4+)

- `BUILD-JSON-QUERY-LAYER`: Query utilities
- Update Makefile targets to use hierarchical format
- Deprecate old monolithic exports (with compatibility layer)

## Query Patterns

Once decomposed, these patterns become efficient:

```python
# Query: All modules in Algebra package
modules_in_algebra = load_index("data/deps/modules/Algebra/_index.json")

# Query: All items in "Infrastructure" category
items_in_cat = load_index("data/enriched/items/_index.json")
infrastructure = [i for i in items_in_cat if i['category'] == 'Infrastructure']

# Query: Transitive dependencies of module X
def transitive_deps(module_id):
    visited = set()
    queue = [module_id]
    while queue:
        m = queue.pop(0)
        if m in visited: continue
        visited.add(m)
        data = load_json(f"data/deps/modules/{m}.json")
        queue.extend(data['imports'])
    return visited
```

## Benefits Summary

| Concern | Before | After |
|---------|--------|-------|
| **Diff size** | Entire file changes | Only affected fragment |
| **Parallel build** | Must serialize | Many fragments in parallel |
| **Query speed** | O(n) full scan | O(1) index lookup |
| **Update cycle** | Expensive (recompile+rewrite) | Cheap (single fragment) |
| **Composability** | Monolithic blob | Reusable SPPF subtrees |
| **Versioning** | Single version | Per-fragment history |
| **Formalization** | Implicit | Explicit Agda proofs |

## Integration with Build System

### Current Makefile

```makefile
data/dependency_graph.json: build/diagrams/agda-deps-full.dot
    $(AGDA) ... --compile src/agda/Plan/CIM/DependencyGraphExport.agda

.github/roadmap/tasks.json: data/planning_index.json
    # exports monolithic canonical_enriched.json
```

### After Decomposition

```makefile
# Legacy targets (for backward compatibility)
data/dependency_graph.json: data/deps/_metadata.json  # aggregated
canonical-enriched-json: data/enriched/_metadata.json

# New granular targets
data/deps/_metadata.json: build/diagrams/agda-deps-full.dot
    python3 scripts/json_decompose.py --input ... --output data/deps/

# Validation: ensure roundtrip
validate-json-roundtrip: data/deps/_metadata.json
    python3 scripts/validate_json_roundtrip.py data/deps/
```

## Cross-References

- **Roadmap**: See roadmap items `BUILD-JSON-*` in [ROADMAP.md](../../ROADMAP.md)
- **Planning Kernel**: [src/agda/Plan/CIM/PlanningKernel.agda](../../src/agda/Plan/CIM/PlanningKernel.agda) (definitionRoadmapItems)
- **Architecture**: [ARCHITECTURE.md](../architecture/ARCHITECTURE.md) — SPPF model, natural transformations
- **Related Work**: Build topology in [BUILD-TOPOLOGY.md](BUILD-TOPOLOGY.md)

## Questions & Discussion

1. **Backward compatibility**: Should we maintain monolithic JSON exports alongside hierarchical? Yes, via aggregation on-the-fly.
2. **Index format**: Manifest files JSON or YAML? JSON for consistency with data.
3. **Versioning strategy**: Git tracks all fragments or snapshots? All fragments (fine-grained history).
4. **Query performance**: Use indices or Agda query types? Both: Python for ad-hoc, Agda for formal queries.

