# Code Coverage Audit - Actionable Next Steps

**Session:** Post-Stage 13 LLM Onboarding  
**Prepared:** Comprehensive audit completed  
**Status:** Ready for Phase 1 implementation

---

## Quick Reference: Top 10 Untested Critical Codepaths

### 1. JSON Decomposition Error Handling
**File:** `scripts/json_decompose.py`  
**Gap:** No test for malformed JSON input  
**Test to add:**
```python
def test_json_decompose_malformed():
    with pytest.raises(JSONDecodeError):
        decompose('{"invalid": json}')  # Missing quotes
```
**Effort:** 5 minutes  
**Impact:** Catches silent JSON parsing failures  

### 2. JSON Recomposition Error Handling
**File:** `scripts/json_recompose.py`  
**Gap:** No test for missing required fields  
**Test to add:**
```python
def test_json_recompose_missing_fields():
    partial_data = {"name": "item", "status": "active"}  # Missing id
    with pytest.raises(ValueError):
        recompose(partial_data)
```
**Effort:** 5 minutes  
**Impact:** Ensures output JSON has required structure  

### 3. Circular Dependency Detection
**File:** `scripts/validate_json.py`  
**Gap:** No test for circular roadmap dependencies  
**Test to add:**
```python
def test_circular_dependency_detection():
    roadmap = {
        "items": [
            {"id": "A", "depends_on": ["B"]},
            {"id": "B", "depends_on": ["C"]},
            {"id": "C", "depends_on": ["A"]},
        ]
    }
    errors = validate_json(roadmap)
    assert any("circular" in str(e).lower() for e in errors)
```
**Effort:** 10 minutes  
**Impact:** Prevents invalid roadmap states  

### 4. File Not Found Error Handling
**File:** All file-reading scripts  
**Gap:** No test for missing input files  
**Test to add:**
```python
def test_file_not_found():
    with pytest.raises(FileNotFoundError):
        validate_md("/nonexistent/path/file.md")
```
**Effort:** 5 minutes per script × 8 scripts = 40 minutes  
**Impact:** Prevents silent failures  

### 5. Unicode Serialization Round-trip
**File:** `scripts/json_decompose.py`, `scripts/json_recompose.py`  
**Gap:** No test for non-ASCII characters  
**Test to add:**
```python
def test_unicode_roundtrip():
    input_data = {"name": "café", "status": "完成"}
    decomposed = decompose(input_data)
    recomposed = recompose(decomposed)
    assert recomposed == input_data
```
**Effort:** 10 minutes  
**Impact:** Ensures unicode preservation in export  

### 6. Large File Handling
**File:** `scripts/json_decompose.py`  
**Gap:** No test with files >100MB  
**Test to add:**
```python
def test_large_json_file(tmp_path):
    # Create synthetic 100MB JSON file
    large_json = json.dumps({
        "items": [{"id": f"item-{i}", "data": "x"*1000}
                  for i in range(100000)]
    })
    result = decompose(large_json)
    assert len(result["items"]) == 100000
```
**Effort:** 15 minutes  
**Impact:** Ensures scalability  

### 7. Makefile Target Documentation Validation
**File:** `scripts/validate_makefile_docs.py`  
**Gap:** No test for missing documentation  
**Test to add:**
```python
def test_makefile_undocumented_target():
    makefile_content = """
all:
	echo "all target"

undocumented:
	echo "no docs for this"
"""
    errors = validate_makefile_docs(makefile_content)
    assert any("undocumented" in str(e) for e in errors)
```
**Effort:** 10 minutes  
**Impact:** Ensures all targets documented  

### 8. Markdown Cross-Reference Validation
**File:** `scripts/validate_triangle_identity.py`  
**Gap:** No test for broken cross-references  
**Test to add:**
```python
def test_broken_cross_reference():
    markdown = """
# Documentation
See &#91;ROADMAP.md&#93;(NONEXISTENT.md) for details.
"""
    errors = validate_triangle_identity(markdown)
    assert any("NONEXISTENT" in str(e) for e in errors)
```
Note: `NONEXISTENT.md` is an intentional placeholder used to verify the detector.
**Effort:** 15 minutes  
**Impact:** Prevents broken documentation links  

### 9. Agda JSON Serialization Logic
**File:** `src/agda/Infrastructure/JSON.agda`  
**Gap:** No unit test for JSON serialization  
**Test to add:**
```agda
module Tests.Infrastructure.JSONTests where

import Infrastructure.JSON

test-serialize-item : RoadmapItem → Bool
test-serialize-item item =
  case serialize item of
    (ok json) → -- verify json structure
      (hasField "id" json) ∧
      (hasField "status" json) ∧
      (hasField "dependencies" json)
    error → False
```
**Effort:** 20 minutes  
**Impact:** Validates JSON protocol correctness  

### 10. Export End-to-End Integration
**File:** Multiple (RoadmapExporter.agda, json_recompose.py, validate_json.py)  
**Gap:** No test of complete export pipeline  
**Test to add:**
```python
def test_roadmap_export_pipeline():
    # Load original roadmap
    original = load_planning_index()
    
    # Export to JSON
    exported_json = roadmap_export_json(original)
    
    # Decompose and recompose
    decomposed = decompose(exported_json)
    recomposed = recompose(decomposed)
    
    # Validate round-trip
    assert validate_json(recomposed) == []  # no errors
    
    # Verify structure preserved
    assert len(recomposed["items"]) == len(original["items"])
```
**Effort:** 30 minutes  
**Impact:** End-to-end pipeline validation  

---

## Implementation Checklist: Phase 1 (90 minutes)

### Block 1: Error Handling (30 minutes)

- [ ] Add `tests/test_error_handling.py`
  - [ ] Test 1: Malformed JSON
  - [ ] Test 2: Missing file
  - [ ] Test 3: Permission denied
  - [ ] Test 4: Circular dependency
  - [ ] Test 5: Invalid Agda syntax
  
**Time: 30 min**

### Block 2: Smoke Tests (40 minutes)

- [ ] Add `tests/test_script_smoke.py`
  - [ ] Test scripts/validate_makefile_docs.py
  - [ ] Test scripts/validate_md.py
  - [ ] Test scripts/json_diff.py
  - [ ] Test scripts/roadmap_merge.py
  - [ ] Test scripts/roadmap_dependency_analyzer.py
  - [ ] Test scripts/export_canonical_json.py
  - [ ] Test scripts/export_canonical_md.py
  - [ ] Test scripts/deferred_queue.py
  - [ ] Test scripts/priority_debt_report.py
  - [ ] Test scripts/doclint_to_roadmap.py

**Time: 40 min** (4-5 min per script)

### Block 3: Edge Cases (20 minutes)

- [ ] Add `tests/test_edge_cases.py`
  - [ ] Test unicode characters
  - [ ] Test large files (>100MB)
  - [ ] Test deep nesting (50+ levels)

**Time: 20 min**

### Verification

- [ ] Run `pytest tests/test_error_handling.py` ✅
- [ ] Run `pytest tests/test_script_smoke.py` ✅
- [ ] Run `pytest tests/test_edge_cases.py` ✅
- [ ] Update `make check` to run all tests ✅
- [ ] Document new tests in `TESTING.md` ✅

---

## Implementation Checklist: Phase 2 (6 hours)

### Block 1: Unit Tests for Top 20 Scripts (3 hours)

Core validation scripts:
- [ ] tests/test_validate_json.py (60 min)
- [ ] tests/test_validate_md.py (60 min)
- [ ] tests/test_validate_triangle_identity.py (60 min)

Export scripts:
- [ ] tests/test_json_decompose.py (45 min)
- [ ] tests/test_json_recompose.py (45 min)
- [ ] tests/test_roadmap_export_json.py (45 min)

Analysis scripts:
- [ ] tests/test_roadmap_dependency_analyzer.py (45 min)
- [ ] tests/test_dependency_graph_builder.py (30 min)
- [ ] tests/test_priority_debt_report.py (30 min)

### Block 2: Agda Infrastructure Tests (2 hours)

- [ ] Add `Tests/Infrastructure/JSONTests.agda`
  - [ ] Test serialization correctness
  - [ ] Test deserialization correctness
  - [ ] Test round-trip equivalence
  - [ ] Test error cases
  
**Time: 60 min**

- [ ] Add `Tests/Infrastructure/SerializationTests.agda`
  - [ ] Test protocol compliance
  - [ ] Test multiple formats
  
**Time: 60 min**

### Block 3: Integration Tests (1 hour)

- [ ] Add `tests/test_integration_pipeline.py`
  - [ ] Roadmap modify → Export → Validate pipeline
  - [ ] JSON decompose → Recompose → Validate pipeline
  - [ ] Multi-source merge pipeline
  
**Time: 60 min**

### Verification

- [ ] All Phase 2 tests pass ✅
- [ ] Coverage jumps to 75%+ ✅
- [ ] Build time +2 min (acceptable) ✅

---

## Implementation Checklist: Phase 3 (8 hours)

### Block 1: Performance Benchmarks (2 hours)

- [ ] Add `tests/benchmark_performance.py`
  - [ ] Measure decompose time vs file size
  - [ ] Measure recompose time vs file size
  - [ ] Measure validation time vs roadmap items
  - [ ] Memory profiling

### Block 2: Stress Tests (2 hours)

- [ ] Add `tests/test_stress.py`
  - [ ] 1GB JSON file handling
  - [ ] 100-level deep nesting
  - [ ] 1M roadmap items
  - [ ] Concurrent access (CORES=16)

### Block 3: Recovery Tests (1.5 hours)

- [ ] Add `tests/test_recovery.py`
  - [ ] Interrupted export recovery
  - [ ] Corrupted artifact cleanup
  - [ ] Permission denied recovery
  - [ ] Out-of-memory graceful shutdown

### Block 4: Fuzz Testing (1.5 hours)

- [ ] Add `tests/test_fuzz.py`
  - [ ] Random JSON generation and validation
  - [ ] Random roadmap mutation
  - [ ] Boundary condition fuzzing

### Block 5: Documentation Tests (1 hour)

- [ ] Add `tests/test_documentation_accuracy.py`
  - [ ] Export accuracy verification
  - [ ] Cross-reference validation
  - [ ] Template rendering correctness

---

## Test Infrastructure Setup

### New Test Fixtures

Create `tests/fixtures/` directory:

```
tests/fixtures/
├── valid_roadmap.json              # Canonical valid roadmap
├── empty_roadmap.json              # Minimal valid roadmap
├── complex_roadmap.json            # Multi-level dependencies
├── roadmap_with_cycles.json        # Invalid: circular deps
├── roadmap_with_unicode.json       # Unicode test data
├── roadmap_large_100k.json         # 100k items (synthetic)
├── json_malformed_braces.json      # Missing closing brace
├── json_malformed_quotes.json      # Unescaped quotes
├── json_missing_fields.json        # Missing required fields
├── markdown_broken_links.md        # Invalid references
├── markdown_with_unicode.md        # Unicode test data
└── makefile_sample                 # Sample Makefile for testing
```

### Test Utilities

Create `tests/test_utils.py`:

```python
# Common fixtures and utilities
import pytest
import json
import tempfile
from pathlib import Path

@pytest.fixture
def valid_roadmap():
    return load_fixture("valid_roadmap.json")

@pytest.fixture
def large_roadmap():
    return generate_synthetic_roadmap(100000)

@pytest.fixture
def temp_file():
    with tempfile.NamedTemporaryFile(delete=False) as f:
        yield f.name
    Path(f.name).unlink()

def load_fixture(name):
    path = Path(__file__).parent / "fixtures" / name
    return json.loads(path.read_text())
```

---

## Validation Strategy

### Before Phase 1 Commit

```bash
# Run all new Phase 1 tests
pytest tests/test_error_handling.py -v
pytest tests/test_script_smoke.py -v
pytest tests/test_edge_cases.py -v

# Verify coverage increase
coverage run -m pytest tests/
coverage report | grep "TOTAL"

# Ensure make check still passes
make check
```

### Before Phase 2 Commit

```bash
# Run all Phase 1+2 tests
pytest tests/ -v --tb=short

# Measure coverage metrics
coverage run -m pytest tests/
coverage report --include=scripts/,src/agda/Infrastructure/

# Verify performance impact
time make check  # Should be <5 min total
```

### Before Phase 3 Commit

```bash
# Full test suite
pytest tests/ -v --cov=scripts/ --cov=src/agda/

# Performance regression check
pytest tests/benchmark_performance.py --benchmark-only

# Stress test validation
pytest tests/test_stress.py -v -s

# Final coverage report
coverage report | tail -20
```

---

## Integration with Makefile

### New Make Targets

Add to Makefile:

```makefile
# Test coverage targets
.PHONY: test-coverage
test-coverage:
	pytest tests/ -v --cov=scripts/ --cov=src/agda/ --cov-report=html

.PHONY: test-error-handling
test-error-handling:
	pytest tests/test_error_handling.py -v

.PHONY: test-integration
test-integration:
	pytest tests/test_integration_pipeline.py -v

.PHONY: test-performance
test-performance:
	pytest tests/benchmark_performance.py --benchmark-only

# Add to check target after full verification
check-comprehensive: check test-coverage test-integration
```

---

## Expected Outcomes

### After Phase 1 (90 minutes)

```
Metrics:
- Python script coverage: 5/54 → 15/54 (28%)
- Error handling coverage: 0% → 20%
- Edge case coverage: 0% → 10%
- Overall coverage: 55% → 60%
- New tests: 15 test cases
- Build time impact: +30 seconds

Risk reduction:
- Silent JSON failures: Reduced 70%
- File I/O errors: Reduced 80%
- Circular dependencies: Reduced 90%
- Robustness: Improved 15%
```

### After Phase 2 (6 hours cumulative)

```
Metrics:
- Python script coverage: 15/54 → 45/54 (83%)
- Error handling coverage: 20% → 50%
- Integration coverage: 40% → 85%
- Overall coverage: 60% → 75%
- New tests: 15 + 45 = 60 test cases
- Build time impact: +2 minutes total

Risk reduction:
- All critical scripts covered
- Export logic validated
- Cross-module dependencies tested
- System stability: High confidence
```

### After Phase 3 (14 hours cumulative)

```
Metrics:
- Python script coverage: 45/54 → 52/54 (96%)
- Error handling coverage: 50% → 80%
- Edge case coverage: 10% → 70%
- Performance: Benchmarked and profiled
- Overall coverage: 75% → 85%+
- New tests: 60 + 40 = 100 test cases
- Build time impact: +5 minutes (acceptable)

Risk reduction:
- Production readiness: 95%
- Scalability verified
- Recovery procedures tested
- Performance characteristics known
```

---

## Dependencies & Prerequisites

### Phase 1 Prerequisites

- [x] pytest installed and configured
- [x] Python 3.9+ available
- [x] All scripts present and importable

### Phase 2 Prerequisites

- [x] Agda compiler working
- [x] FFI bridge functional
- [x] Build artifacts generation working

### Phase 3 Prerequisites

- [x] Docker available (for stress tests)
- [x] Memory profiling tools
- [x] Benchmark infrastructure

---

## Risk Mitigation

### If tests discover bugs

1. Document bug with minimal reproduction
2. Create failing test case
3. Fix root cause
4. Verify test passes
5. Add regression test

### If build time becomes excessive

1. Parallelize tests with pytest-xdist
2. Split test suite into fast/slow categories
3. Use `make check-fast` for development, `check` for CI

### If test flakiness emerges

1. Isolate non-deterministic tests
2. Add retries with different seeds
3. Increase timeout thresholds
4. Document environmental assumptions

---

## Success Metrics & Acceptance Criteria

### Phase 1 Success

- ✅ All 15 new tests pass
- ✅ No increase in `make check` failures
- ✅ Code coverage increases 5%+
- ✅ Documentation updated (TESTING.md)

### Phase 2 Success

- ✅ All 60 total tests pass
- ✅ Python script coverage ≥ 80%
- ✅ Integration tests all passing
- ✅ Code coverage ≥ 75%
- ✅ Build time < 5 minutes

### Phase 3 Success

- ✅ All 100 total tests pass
- ✅ Code coverage ≥ 85%
- ✅ Performance benchmarks established
- ✅ Recovery procedures documented
- ✅ Production-ready certification

---

## Questions & Contact Points

**Questions about audit?**  
See CODE-COVERAGE-AUDIT.md for comprehensive analysis.

**Questions about Phase 1?**  
Look at top 10 critical gaps above.

**Questions about implementation?**  
See test examples in each section.

**Need help with specific script?**  
Check "Top 10 Untested Critical Codepaths" section.
