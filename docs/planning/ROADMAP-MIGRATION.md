# Roadmap Infrastructure Migration Guide

## Overview

The roadmap infrastructure has been consolidated into a **canonical index** with validated projections, eliminating scattered roadmap files and establishing a triangle identity between Agda, Markdown, and GitHub.

## Architecture

                        ┌─────────────────────────────┐
                        │  Canonical (Agda)           │
                        │  CanonicalRoadmap.agda      │
                        │  (Single Source of Truth)   │
                        └──────────┬──────────────────┘
                                   │
                  ┌────────────────┼────────────────┐
                  ▼                ▼                ▼
        ┌─────────────────┐ ┌─────────────┐ ┌──────────────┐
        │ tasks.json      │ │ ROADMAP.md  │ │ SPPF JSON    │
        │ (GitHub Issues) │ │ (Human)     │ │ (Graph Viz)  │
        └─────────────────┘ └─────────────┘ └──────────────┘
                  │                │                │
                  └────────────────┼────────────────┘
                                   ▼
                        ┌─────────────────────┐
                        │ Triangle Validation │
                        │ (Round-trip equiv)  │
                        └─────────────────────┘

## Canonical Source

**Location**: [../../src/agda/Plan/CIM/CanonicalRoadmap.agda](../../src/agda/Plan/CIM/CanonicalRoadmap.agda)

**Schema**: `RoadmapItem` record (id, title, status, category, source, files, tags, dependsOn, related)

**Type**: Module parameterization (no postulates) via `CanonicalRoadmap` record

## Workflow

### 1. Merging Sources

```bash
make roadmap-merge
```

Merges all roadmap sources into `build/canonical_roadmap.json`:

* `.github/roadmap/tasks.json` (GitHub issues)
* `ROADMAP.md` (structured markdown)
* `src/agda/Plan/CIM/IngestedRoadmaps/*.agda` (GP file extractions)
* Legacy `roadmap-*.agda` files

### 2. Exporting Projections

```bash
make .github/roadmap/tasks.json  # → .github/roadmap/tasks.json
make ROADMAP.md    # → ROADMAP.md
make build/gp_roadmap_sppf.json  # → build/gp_roadmap_sppf.json
```

### 3. Validating Triangle

```bash
make roadmap-validate-triangle
```

Ensures:

* `tasks.json` matches canonical (ID and content)
* `ROADMAP.md` contains all canonical items
* No drift between projections

### 4. Syncing to GitHub

```bash
MUTATE_OK=1 make roadmap-sync
```

Depends on `.github/roadmap/tasks.json` to ensure tasks.json is current before sync.
This is a local Make target; CI runs the roadmap/JSON job via `ci.yml` (no standalone workflow).

## Key Files

### Core Modules

| File | Purpose | Postulates? |
|------|---------|-------------|
| `RoadmapIndex.agda` | Schema, adapters, deduplication | No (module parameterization) |
| `CanonicalRoadmap.agda` | Authoritative data (auto-generated) | No |
| `RoadmapSPPF.agda` | SPPF projection with packed nodes | No (TERMINATING pragma) |
| `RoadmapSync.agda` | GitHub sync orchestrator | No (parameterized over GitHubAPI) |

### Scripts

| Script | Purpose |
|--------|---------|
| `merge_roadmaps.py` | Merge all sources → canonical |
| `export_canonical_json.py` | Canonical → tasks.json |
| `export_canonical_md.py` | Canonical → ROADMAP.md |
| `export_roadmap_sppf.py` | Canonical → SPPF JSON |
| `validate_json.py` | Check tasks.json ≡ canonical |
| `validate_md.py` | Check ROADMAP.md ⊆ canonical |

### Makefile Targets

| Target | Dependencies | Action |
|--------|--------------|--------|
| `roadmap-merge` | - | Merge sources → `build/canonical_roadmap.json` |
| `.github/roadmap/tasks.json` | `build/canonical_roadmap.json` | Export → `tasks.json` |
| `ROADMAP.md` | `build/canonical_roadmap.json` | Export → `ROADMAP.md` |
| `build/gp_roadmap_sppf.json` | `build/canonical_roadmap.json` | Export → `gp_roadmap_sppf.json` |
| `roadmap-validate-json` | canonical, tasks.json | Validate JSON projection |
| `roadmap-validate-md` | canonical, ROADMAP.md | Validate Markdown projection |
| `roadmap-validate-triangle` | validate-json, validate-md | Full triangle check |
| `roadmap-sync` | `.github/roadmap/tasks.json` | Sync to GitHub (local Make target; CI uses `ci.yml`) |

## Deprecated Files

### To Be Retired

The following files are now **redundant** and can be removed after confirming canonical has all data:

* `roadmap-v4.agda` (1979 lines, legacy schema)
* `roadmap-typed.agda` (1979 lines, legacy schema)
* `roadmap-python.agda` (1973 lines, legacy schema)
* `roadmap-flat.agda` (1952 lines, legacy schema)
* `roadmap-correct-body.agda` (1981 lines, legacy schema)
* `roadmap-cons-body.agda` (1774 lines, legacy schema)
* `roadmap-test.agda` (350 lines, legacy schema)
* `roadmap-extracted.agda` (93 lines, legacy schema)
* `ROADMAP-DRAFT.md` (superseded by canonical)
* `ROADMAP-RESTRUCTURE-PROPOSAL.md` (superseded by canonical)
* `ROADMAP_EXTRACTION_SUMMARY.md` (superseded by canonical)

### Migration Checklist

* \[x] Merge all sources into canonical
* \[x] Export projections (JSON, Markdown, SPPF)
* \[x] Validate triangle identity
* \[ ] Review canonical for completeness
* \[ ] Archive legacy files to `archive/` directory
* \[ ] Update documentation references
* \[ ] Remove legacy Makefile targets

## Maintenance

### Adding New Items

1. Edit `../../src/agda/Plan/CIM/CanonicalRoadmap.agda` directly, OR
2. Edit `tasks.json`, then run `make roadmap-merge` to re-import

### Updating Projections

```bash
make roadmap-merge          # Reimport from all sources
make .github/roadmap/tasks.json    # Update tasks.json
make ROADMAP.md      # Update ROADMAP.md
make roadmap-validate-triangle  # Verify consistency
```

### Checking for Drift

```bash
make roadmap-validate-triangle
```

If validation fails:

* Review diffs between canonical and projections
* Run `make .github/roadmap/tasks.json ROADMAP.md` to sync
* Re-validate

## Benefits

✓ **Single source of truth**: Canonical index in Agda\
✓ **No postulates**: Module parameterization and records\
✓ **Triangle identity**: Validated equivalence between projections\
✓ **Deduplication**: Automatic ID-based merging\
✓ **SPPF projection**: Packed nodes for >2 dependencies\
✓ **Type safety**: Agda schema prevents invalid data\
✓ **Bidirectional sync**: Can import from or export to JSON/Markdown

## Current Status

* **Canonical**: 196 items (merged from tasks.json, ROADMAP.md, IngestedRoadmaps, legacy)
* **tasks.json**: ✓ 196 items (validated)
* **ROADMAP.md**: ✓ 102 items shown (validated subset)
* **SPPF JSON**: ✓ 102 nodes (graph visualization)
* **Triangle**: ✓ Validated (no drift)

***

**Last Updated**: 2025-12-22\
**Generated By**: Roadmap consolidation infrastructure\
**Validation**: `make roadmap-validate-triangle`
