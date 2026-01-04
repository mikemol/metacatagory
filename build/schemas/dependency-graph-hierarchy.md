# Dependency Graph Hierarchical Schema

## Source: `build/dependency_graph.json`

**Monolithic Structure:**
```json
{
  "metadata": {
    "total_modules": 84,
    "total_dependencies": 151,
    "timestamp": "2026-01-04T...",
    "source": "agda --dependency-graph"
  },
  "nodes": [
    {
      "module": "Algebra.Groups.Abelian",
      "imports": ["Algebra.Enrichment", "Chapter2.Level2sub1"],
      "imported_by": ["Algebra.Modules.Basic"]
    },
    ...
  ]
}
```

## Target: `build/deps/`

**Hierarchical Structure:**

```
build/deps/
├── _metadata.json           # Top-level metadata + reconstruction rules
├── _index.json              # Fast lookup index of all modules
├── modules/                 # Per-module dependency information
│   ├── Algebra/
│   │   ├── Groups/
│   │   │   └── Abelian.json
│   │   └── Modules/
│   │       └── Basic.json
│   ├── Chapter2/
│   │   └── Level2sub1.json
│   └── ...
├── layers/                  # Topological layers
│   ├── _index.json
│   ├── layer-0.json        # Leaf modules (no dependencies)
│   ├── layer-1.json        # Modules depending only on layer-0
│   └── ...
└── cycles/                 # Circular dependencies (if any)
    ├── _index.json
    └── cycle-0.json        # Set of modules in first cycle
```

### File Formats

**`_metadata.json`:**
```json
{
  "schema_version": "1.0.0",
  "source_file": "build/dependency_graph.json",
  "decomposition_timestamp": "2026-01-04T...",
  "total_modules": 84,
  "total_dependencies": 151,
  "total_layers": 12,
  "total_cycles": 0,
  "aggregation_rule": "merge-modules-by-manifest"
}
```

**`_index.json`:**
```json
{
  "items": [
    "Algebra.Groups.Abelian",
    "Algebra.Modules.Basic",
    ...
  ],
  "count": 84
}
```

**`modules/Algebra/Groups/Abelian.json`:**
```json
{
  "module": "Algebra.Groups.Abelian",
  "imports": [
    "Algebra.Enrichment",
    "Chapter2.Level2sub1"
  ],
  "imported_by": [
    "Algebra.Modules.Basic"
  ],
  "layer": 3,
  "in_cycle": false
}
```

**`layers/layer-0.json`:**
```json
{
  "layer_id": 0,
  "description": "Leaf modules (no dependencies)",
  "modules": [
    "Agda.Builtin.Bool",
    "Agda.Builtin.String",
    ...
  ],
  "count": 15
}
```

**`cycles/cycle-0.json`** (if cycles exist):
```json
{
  "cycle_id": 0,
  "modules": [
    "Module.A",
    "Module.B"
  ],
  "edges": [
    ["Module.A", "Module.B"],
    ["Module.B", "Module.A"]
  ]
}
```

## Reconstruction Rules

**Aggregation:** Read `_metadata.json` → collect all `modules/**/*.json` → sort by `module` field → reconstruct `nodes` array → validate counts match metadata.

## Benefits

- **Diffing**: Git diffs show per-module changes, not entire 84-module list
- **Parallelism**: Can process modules independently (e.g., compute layer assignment)
- **Query**: Fast lookup by module name without parsing entire JSON
- **Versioning**: Can track history of individual modules over time
- **Navigation**: Hierarchical structure mirrors Agda package structure
