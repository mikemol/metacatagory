# Enriched Roadmap Hierarchical Schema

## Source: `build/canonical_enriched.json`

**Monolithic Structure:**
```json
{
  "metadata": {
    "total_items": 117,
    "timestamp": "2026-01-04T...",
    "source": "PlanningExport.agda"
  },
  "items": [
    {
      "id": "BUILD-JSON-DECOMPOSITION",
      "title": "Decompose large JSON artifacts via natural transformations",
      "status": "in-progress",
      "category": "Build Infrastructure",
      "dependencies": ["BUILD-JSON-SCHEMA", "BUILD-JSON-HIERARCHY"],
      "files": ["src/agda/Plan/CIM/JSONTransformation.agda", ...],
      "tags": ["JSON", "NaturalTransformation", "SPPF"],
      "provenance": ["session: JSON decomposition design review (2026-01-04)"]
    },
    ...
  ]
}
```

## Target: `data/enriched/`

**Hierarchical Structure:**

```
data/enriched/
├── _metadata.json                  # Top-level metadata + reconstruction rules
├── _index.json                     # Fast lookup index of all item IDs
├── items/                          # Core item data
│   ├── BUILD-JSON-DECOMPOSITION.json
│   ├── BUILD-JSON-SCHEMA.json
│   └── ...
├── dependencies/                   # Dependency graphs
│   ├── _index.json
│   ├── BUILD-JSON-DECOMPOSITION.deps.json
│   └── ...
├── annotations/                    # Metadata annotations
│   ├── _index.json
│   ├── sources/                    # Provenance tracking
│   │   ├── BUILD-JSON-DECOMPOSITION.source.json
│   │   └── ...
│   └── tags/                       # Tag indices
│       ├── JSON.json               # All items tagged "JSON"
│       ├── NaturalTransformation.json
│       └── ...
└── categories/                     # Category-based indices
    ├── _index.json
    ├── Build_Infrastructure.json   # All items in this category
    └── ...
```

### File Formats

**`_metadata.json`:**
```json
{
  "schema_version": "1.0.0",
  "source_file": "build/canonical_enriched.json",
  "decomposition_timestamp": "2026-01-04T...",
  "total_items": 117,
  "total_categories": 8,
  "total_tags": 42,
  "aggregation_rule": "merge-items-with-dependencies"
}
```

**`items/BUILD-JSON-DECOMPOSITION.json`:**
```json
{
  "id": "BUILD-JSON-DECOMPOSITION",
  "title": "Decompose large JSON artifacts via natural transformations",
  "description": "Implement hierarchical decomposition of monolithic build JSONs using category-theoretic natural transformations. Structure as SPPF with shared subtrees.",
  "status": "in-progress",
  "category": "Build Infrastructure",
  "files": [
    "src/agda/Plan/CIM/JSONTransformation.agda",
    "scripts/json_decompose.py",
    "scripts/json_recompose.py"
  ],
  "tags": ["JSON", "NaturalTransformation", "SPPF", "Build"]
}
```

**`dependencies/BUILD-JSON-DECOMPOSITION.deps.json`:**
```json
{
  "item_id": "BUILD-JSON-DECOMPOSITION",
  "depends_on": [
    "BUILD-JSON-SCHEMA",
    "BUILD-JSON-HIERARCHY"
  ],
  "required_by": [
    "BUILD-JSON-FORWARD",
    "BUILD-JSON-BACKWARD"
  ],
  "related": [
    "BUILD-JSON-APPLY-DEPS",
    "BUILD-JSON-APPLY-ENRICHED"
  ]
}
```

**`annotations/sources/BUILD-JSON-DECOMPOSITION.source.json`:**
```json
{
  "item_id": "BUILD-JSON-DECOMPOSITION",
  "provenance": [
    "session: JSON decomposition design review (2026-01-04)",
    "doc: docs/process/JSON-DECOMPOSITION.md",
    "commit: 73147ea"
  ],
  "source_module": "PlanningKernel.agda",
  "source_line": 29
}
```

**`annotations/tags/JSON.json`:**
```json
{
  "tag": "JSON",
  "items": [
    "BUILD-JSON-DECOMPOSITION",
    "BUILD-JSON-SCHEMA",
    "BUILD-JSON-FORWARD",
    ...
  ],
  "count": 10
}
```

**`categories/Build_Infrastructure.json`:**
```json
{
  "category": "Build Infrastructure",
  "items": [
    "BUILD-JSON-DECOMPOSITION",
    "MAKEFILE-GRAVITY-WELLS",
    ...
  ],
  "count": 15
}
```

## Reconstruction Rules

**Aggregation:**
1. Read `_metadata.json` for configuration
2. Collect all `items/*.json` files
3. For each item, merge in `dependencies/{id}.deps.json` if exists
4. For each item, merge in `annotations/sources/{id}.source.json` if exists
5. Add `tags` array by inverting `annotations/tags/*.json` indices
6. Sort by `id` field
7. Wrap in `{"metadata": {...}, "items": [...]}`
8. Validate counts match metadata

## Benefits

- **Diffing**: Per-item changes visible in Git, not 117-item array churn
- **Parallelism**: Can process items independently (status updates, tag indexing)
- **Query**: Fast tag-based lookup (`annotations/tags/JSON.json`)
- **Versioning**: Track evolution of individual roadmap items over time
- **Navigation**: Category structure mirrors project organization
- **Extensibility**: Can add new annotation types without monolithic schema changes
