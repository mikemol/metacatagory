# Python Scripts Refactoring Plan

## Objective
Recursively extract common functionality from Python scripts into shared subcomponents to eliminate code duplication and improve maintainability.

## Completed Work

### Phase 1: Shared Module Creation ✓

Created three core shared modules under `scripts/shared/`:

#### 1. `scripts/shared/io.py` (6 functions)
- **Purpose**: Unified file I/O operations with consistent error handling
- **Functions**:
  - `load_json(path, default=None, required=False)` - Load JSON with optional defaults
  - `save_json(path, data, indent=2, create_parents=True)` - Save JSON with automatic directory creation
  - `load_markdown(path, default="", required=False)` - Load markdown files
  - `save_markdown(path, content, create_parents=True)` - Save markdown files
  - `ensure_file_exists(path)` - Check file existence
  - `create_directory(path)` - Create directories recursively
- **Key Features**: UTF-8 encoding, automatic parent directory creation, consistent error handling

#### 2. `scripts/shared/paths.py` (15 constants + 3 functions)
- **Purpose**: Centralize repository structure knowledge and standard build paths
- **Constants**: `REPO_ROOT`, `BUILD_DIR`, `REPORTS_DIR`, `AGDA_DIR`, `SCRIPTS_DIR`, etc.
- **Artifact Paths**: `PLANNING_INDEX_JSON`, `DEPENDENCY_GRAPH_JSON`, `DOCLINT_ROADMAP_JSON`, etc.
- **Functions**:
  - `get_module_path_from_name(module_name)` - Convert "Plan.CIM.Utility" → Path
  - `get_module_name_from_path(file_path)` - Reverse conversion
  - `ensure_build_dirs()` - Create standard build directories

#### 3. `scripts/shared/normalization.py` (8 functions)
- **Purpose**: String normalization, data validation, field defaulting helpers
- **Functions**:
  - `normalize_title(title)` - Regex-based title normalization
  - `normalize_id(id_str)` - ID normalization
  - `ensure_item_fields(item, defaults)` - Ensure required fields with defaults
  - `ensure_provenance(item)` - Add provenance tracking
  - `deduplicate_items_by_id(items)` - Deduplicate with dependency merging
  - `merge_dependencies(existing, new)` - Smart dependency merging
  - `validate_item_structure(item, schema)` - Schema validation
  - `clean_empty_strings(obj)` - Remove empty string values

### Phase 2: Test Coverage ✓

Created comprehensive test suite under `tests/shared/`:

- **test_io.py**: 20 tests covering load/save operations, error handling, defaults
- **test_normalization.py**: 23 tests for all normalization functions
- **test_paths.py**: 10 tests for path utilities and module conversions

**Result**: 53/53 tests passing in 0.29s

### Phase 3: Example Refactoring ✓

Refactored `scripts/merge_roadmaps.py` as proof-of-concept:

**Before**:
- Inline `normalize_title()` function (duplicated across scripts)
- Inline `ensure_provenance()` function (duplicated)
- Manual JSON file I/O with explicit error handling
- Manual path construction with `Path(__file__).resolve().parent.parent`
- Local `deduplicate_by_id()` function

**After**:
```python
from shared.io import load_json, save_json
from shared.paths import REPO_ROOT, BUILD_DIR, DOCLINT_ROADMAP_JSON, CANONICAL_ROADMAP_JSON
from shared.normalization import (
    normalize_title,
    ensure_provenance,
    deduplicate_items_by_id,
    ensure_item_fields
)
```

**Lines Removed**: ~40 lines of duplicated code
**Result**: Script runs successfully, produces identical output

## Remaining Work

### High-Priority Scripts (Next Phase)

Scripts with significant duplication potential:

1. **export_roadmap.py**
   - Uses: JSON I/O, normalization, path handling
   - Estimated reduction: ~30 lines
   - Impact: High (frequently used)

2. **doclint_to_roadmap.py**
   - Uses: JSON I/O, item field defaults, provenance
   - Estimated reduction: ~25 lines
   - Impact: High (documentation pipeline)

3. **intake_scan.py**
   - Uses: File I/O, path handling, directory operations
   - Estimated reduction: ~20 lines
   - Impact: Medium (data ingestion)

4. **enrich_planning_index.py**
   - Uses: JSON I/O, field normalization, dependency merging
   - Estimated reduction: ~35 lines
   - Impact: High (planning pipeline)

5. **extract_module_dependencies.py**
   - Uses: Path utilities, module name conversions, JSON I/O
   - Estimated reduction: ~25 lines
   - Impact: Medium (dependency analysis)

### Medium-Priority Scripts

6. **build_dependency_graph.py**
   - Uses: JSON I/O, graph data structures
   - Potential for graph utilities module

7. **validate_roadmap_schema.py**
   - Uses: Schema validation, JSON I/O
   - Already has validate_item_structure() in normalization module

8. **generate_makefile_targets.py**
   - Uses: Path utilities, file I/O
   - Potential for Makefile generation utilities

### Additional Shared Modules to Consider

Based on analysis of remaining scripts:

#### 4. `scripts/shared/graph.py` (Future)
- Graph construction utilities
- Topological sorting
- Dependency resolution
- Used by: build_dependency_graph.py, extract_dependencies.py

#### 5. `scripts/shared/schema.py` (Future)
- JSON schema validation
- Type checking utilities
- Schema evolution helpers
- Used by: validate_roadmap_schema.py, merge_roadmaps.py

#### 6. `scripts/shared/markdown.py` (Future)
- Markdown parsing utilities
- Table extraction
- Section navigation
- Used by: parse_roadmap_md() sections, doclint scripts

## Refactoring Protocol

For each script refactoring:

1. **Analysis**
   - Read script fully
   - Identify functions matching shared module signatures
   - Note any custom variations

2. **Refactoring**
   - Add imports from shared modules
   - Replace duplicated code with shared function calls
   - Update any custom logic to use shared utilities
   - Remove now-unnecessary imports (e.g., `json`, `re` if no longer needed)

3. **Validation**
   - Run the refactored script
   - Compare output with original (if deterministic)
   - Run script-specific tests if available
   - Check for any regressions

4. **Documentation**
   - Update script docstring if behavior changed
   - Note any breaking changes (should be none)
   - Update this tracking document

## Success Metrics

- **Lines Reduced**: Target 200+ lines across all scripts
- **Test Coverage**: Maintain 100% passing tests
- **Zero Regressions**: All scripts produce identical output
- **Maintainability**: Single source of truth for common operations

## Next Actions

1. Refactor export_roadmap.py (highest impact)
2. Refactor doclint_to_roadmap.py
3. Refactor enrich_planning_index.py
4. Run full test suite: `pytest tests/ --cov=scripts`
5. Document any edge cases or variations discovered
6. Consider creating graph.py and schema.py modules if patterns emerge

## Notes

- All refactored scripts maintain backward compatibility
- Shared modules use consistent error handling patterns
- Path handling centralized in paths.py prevents "../.." anti-patterns
- Normalization ensures consistent data formats across pipelines
