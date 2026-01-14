# Configuration Parameterization

This document describes the externalized configuration for the metacategory build system.

## Overview

Previously, many configuration values were hardcoded throughout scripts and build files. These have been externalized to JSON configuration files for maintainability and customization.

## Configuration Files

### Badge Thresholds

**File:** `.github/badges/thresholds.json`

Defines thresholds and colors for various metrics:

* **roadmap_progress**: Percentage completion thresholds
* **deferred_total**: Total deferred items thresholds
* **postulates**: Postulate count thresholds
* **todo**: TODO comment thresholds
* **fixme**: FIXME comment thresholds
* **weighted_total**: Weighted technical debt thresholds

Each metric has an array of `{limit, color}` pairs for conditional badge coloring.

### Badge Weights

**File:** `.github/badges/weights.json`

Defines weights for different types of technical debt:

* `postulate`: 2.0 (high priority)
* `todo`: 1.0 (standard priority)
* `fixme`: 1.5 (elevated priority)
* `deviation`: 3.0 (critical priority)

### Scan Configuration

**File:** `.github/badges/scan-config.json`

Defines file scanning behavior:

* **file_extensions**: Which file types to scan (`.agda`, `.md`, `.py`, etc.)
* **excluded_dirs**: Directories to skip (`node_modules`, `build`, `.git`, etc.)
* **top_offenders_limit**: Number of top files to report (default: 15)
* **max_history_entries**: History retention limit (default: 60)

### Mathematical Concept Extraction

**File:** `scripts/extract-concepts-config.json`

Defines patterns for extracting mathematical concepts from documentation:

* **concept_patterns**: Regex alternation patterns for concept matching
* **default_target_module**: Where to direct extracted concepts
* **max_concepts**: Maximum concepts to extract per file (default: 15)
* **concept_title_case**: Whether to normalize to title case (default: true)

Patterns include:

* Category theory: Functor, Yoneda, Natural Transformation, etc.
* Topology: Homotopy, Fibration, Sheaf, Cohomology, etc.
* Algebra: Group Action, Lie Group, Quaternion, etc.
* Constructions: RoPE, SPPF, Stasheff, Associahedron, etc.

### Makefile Variables

**File:** `Makefile` (generated from `src/agda/Examples/ExporterMakefile.agda`)

* **AGDA**: Path to Agda compiler (defaults to `agda`)
* **AGDA_FLAGS**: Common compilation flags (`-i src/agda --include-path=$(AGDA_PRIM_DIR) --no-default-libraries --no-libraries --ghc-flag=-Wno-star-is-type`)

### Parallelism Controls

These variables enable opt-in parallelism in Python pipelines and JSON decomposition:

* **METACATAGORY_PARALLEL**: `true` enables parallel execution in pipeline phases that use `ConcurrentPhase`.
* **METACATAGORY_WORKERS**: Integer worker count for parallel execution and JSON fragment writes.

Pytest parallelism uses the make variable `PYTEST_WORKERS`, which defaults to
`METACATAGORY_WORKERS` when set, otherwise `CORES`.

All Agda compilation targets use `$(AGDA) $(AGDA_FLAGS)` instead of hardcoded paths and flags.

## Usage

### Modifying Thresholds

Edit `.github/badges/thresholds.json` to change when badges change color:

```json
{
  "postulates": [
    {"limit": 10, "color": "brightgreen"},
    {"limit": 50, "color": "yellow"},
    {"limit": 10000000, "color": "red"}
  ]
}
```

### Adding Mathematical Concepts

Edit `scripts/extract-concepts-config.json` to recognize new patterns:

```json
{
  "concept_patterns": [
    "RoPE|SPPF|Stasheff",
    "YourNewConcept|AnotherConcept"
  ]
}
```

### Changing File Scanning

Edit `.github/badges/scan-config.json` to adjust what gets scanned:

```json
{
  "file_extensions": [".agda", ".md", ".py", ".hs"],
  "excluded_dirs": ["node_modules", "build", ".git"],
  "top_offenders_limit": 20
}
```

### Adjusting Agda Compilation

Edit `src/agda/Examples/ExporterMakefile.agda` to change AGDA_FLAGS:

```agda
"AGDA_FLAGS := -i src/agda --cubical-compatible --ghc-flag=-Wno-star-is-type\n"
```

Then regenerate:

```bash
make regen-makefile
```

## Benefits

* **Maintainability**: Configuration changes don't require code edits
* **Modularity**: Different environments can use different configs
* **Transparency**: Configuration is explicit and documented
* **Testability**: Can test with different configurations without modifying code
* **Version control**: Configuration changes are tracked separately from logic

## Migration Notes

Previously hardcoded values that are now configurable:

* Badge thresholds (was hardcoded in `generate-badges.py`)
* File extensions and exclusions (was hardcoded in multiple scripts)
* Mathematical concept patterns (was hardcoded in `extract_roadmaps.py`)
* Agda compilation flags (was repeated ~100+ times in Makefile)
* Top offenders limit (was hardcoded as 15)

All scripts now load configuration from JSON files with sensible defaults if files are missing.
