# Planning Index Hierarchical Schema

## Source: `data/planning_index.json`

**Monolithic Structure:**
```json
{
  "metadata": {
    "total_items": 150,
    "total_adapters": 5,
    "timestamp": "2026-01-04T...",
    "adapters": [
      "roadmap",
      "deferred",
      "makefile",
      "documentation",
      "testing"
    ]
  },
  "items": [
    {
      "id": "BUILD-JSON-DECOMPOSITION",
      "source_adapter": "roadmap",
      "category": "Build Infrastructure",
      "status": "in-progress",
      "files": [...],
      ...
    },
    {
      "id": "DEFERRED-TYPE-SAFETY-042",
      "source_adapter": "deferred",
      "category": "Agda Formalization",
      ...
    },
    ...
  ]
}
```

## Target: `data/planning/`

**Hierarchical Structure:**

```
data/planning/
├── _metadata.json              # Top-level metadata + reconstruction rules
├── _index.json                 # Fast lookup index of all item IDs
├── items/                      # Items organized by category
│   ├── Build_Infrastructure/
│   │   ├── _index.json
│   │   ├── BUILD-JSON-DECOMPOSITION.json
│   │   └── ...
│   ├── Agda_Formalization/
│   │   ├── _index.json
│   │   ├── DEFERRED-TYPE-SAFETY-042.json
│   │   └── ...
│   └── ...
├── sources/                    # Items organized by source adapter
│   ├── _index.json
│   ├── roadmap/
│   │   ├── _index.json
│   │   ├── BUILD-JSON-DECOMPOSITION.json (symlink to ../../items/...)
│   │   └── ...
│   ├── deferred/
│   │   ├── _index.json
│   │   ├── DEFERRED-TYPE-SAFETY-042.json (symlink)
│   │   └── ...
│   └── ...
├── artifacts/                  # File-based indices
│   ├── _index.json
│   ├── files.json              # Map: filepath → [item IDs]
│   └── modules/
│       ├── JSONTransformation.agda.json  # Items touching this file
│       └── ...
└── status/                     # Status-based indices
    ├── _index.json
    ├── not-started.json
    ├── in-progress.json
    └── done.json
```

### File Formats

**`_metadata.json`:**
```json
{
  "schema_version": "1.0.0",
  "source_file": "data/planning_index.json",
  "decomposition_timestamp": "2026-01-04T...",
  "total_items": 150,
  "total_adapters": 5,
  "total_categories": 12,
  "adapters": [
    "roadmap",
    "deferred",
    "makefile",
    "documentation",
    "testing"
  ],
  "aggregation_rule": "merge-all-items-by-id"
}
```

**`items/Build_Infrastructure/_index.json`:**
```json
{
  "category": "Build Infrastructure",
  "items": [
    "BUILD-JSON-DECOMPOSITION",
    "BUILD-JSON-SCHEMA",
    "MAKEFILE-GRAVITY-WELLS",
    ...
  ],
  "count": 18
}
```

**`items/Build_Infrastructure/BUILD-JSON-DECOMPOSITION.json`:**
```json
{
  "id": "BUILD-JSON-DECOMPOSITION",
  "source_adapter": "roadmap",
  "category": "Build Infrastructure",
  "status": "in-progress",
  "title": "Decompose large JSON artifacts via natural transformations",
  "files": [
    "src/agda/Plan/CIM/JSONTransformation.agda",
    "scripts/json_decompose.py",
    "scripts/json_recompose.py"
  ],
  "dependencies": ["BUILD-JSON-SCHEMA", "BUILD-JSON-HIERARCHY"],
  "tags": ["JSON", "NaturalTransformation", "SPPF"]
}
```

**`sources/roadmap/_index.json`:**
```json
{
  "adapter": "roadmap",
  "items": [
    "BUILD-JSON-DECOMPOSITION",
    "BUILD-JSON-SCHEMA",
    ...
  ],
  "count": 117
}
```

**`artifacts/files.json`:**
```json
{
  "src/agda/Plan/CIM/JSONTransformation.agda": [
    "BUILD-JSON-DECOMPOSITION",
    "BUILD-JSON-SCHEMA"
  ],
  "scripts/json_decompose.py": [
    "BUILD-JSON-DECOMPOSITION",
    "BUILD-JSON-FORWARD"
  ],
  ...
}
```

**`status/in-progress.json`:**
```json
{
  "status": "in-progress",
  "items": [
    "BUILD-JSON-DECOMPOSITION",
    "AGDA-ROADMAP-EXPORT",
    ...
  ],
  "count": 12
}
```

## Reconstruction Rules

**Aggregation:**
1. Read `_metadata.json` for configuration
2. Collect all `items/**/*.json` files (skip symlinks)
3. Sort by `id` field
4. Group by `source_adapter` to validate adapter counts
5. Wrap in `{"metadata": {...}, "items": [...]}`
6. Validate total counts match metadata

**Note:** Symlinks in `sources/` are for navigation only, not included in reconstruction.

## Benefits

- **Diffing**: Per-category/adapter changes isolated, not 150-item monolith
- **Parallelism**: Can process adapters independently (e.g., refresh deferred items)
- **Query**: Fast lookup by status, category, or adapter without full parse
- **Versioning**: Track evolution of planning items from different sources
- **Navigation**: Multiple access paths (by category, by adapter, by file)
- **Extensibility**: Can add new adapters without touching existing structure
- **Artifacts**: File-based index enables "what depends on this file?" queries
