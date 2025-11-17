# Testing Infrastructure Design

## Current State: Regex-Based Fragility

**Problem:** `scripts/test_report.py` uses regex to parse Agda source files:
```python
STATUS_ASSERT_RE = re.compile(r"^\s*([a-zA-Z0-9_\-']+)\s*:\s*A\.[A-Za-z0-9_]+\s+[a-zA-Z0-9_\-']+\s*≡\s*(?:B\.)?true\s*$")
```

**Issues:**
- Brittle: Breaks if formatting changes
- Incomplete: May miss valid patterns
- Untype-safe: Can't verify counts match reality
- Maintenance burden: Must keep regex in sync with code conventions

## Proposed Solution: Agda-Validated Metadata

### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│ Tests/CoverageReport.agda (Type-Checked Source of Truth)   │
│  - Explicit registry of all adapter types                  │
│  - Expected assertion count (verified by type system)      │
│  - If it compiles, metadata is guaranteed correct           │
└──────────────────────┬──────────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────────┐
│ scripts/test_report.py (Report Generator)                  │
│  - Reads structured metadata from CoverageReport.agda      │
│  - Cross-validates against actual files                    │
│  - Detects discrepancies (someone forgot to update)        │
└─────────────────────────────────────────────────────────────┘
```

### Implementation (Already Done!)

**1. Type-Checked Metadata (`Tests/CoverageReport.agda`):**

```agda
module Tests.CoverageReport where

-- Explicit registry of checklist modules
allChecklists : List ChecklistModule
allChecklists =
  record { moduleName = "Tests.GrothendieckFibrationsChecklist"
         ; assertionCount = 15
         ; adapterTypes = [FibrationDeclarationAdapter, ...]
         } ∷
  record { moduleName = "Tests.AbelianCategoriesChecklist"
         ; assertionCount = 11
         ; adapterTypes = [HasZeroObjectPropertyAdapter, ...]
         } ∷
  []

-- This MUST match reality or Agda won't compile
totalAssertions : Nat
totalAssertions = sumAssertions allChecklists

_ : totalAssertions ≡ 37  -- Type-checked assertion!
_ = refl  -- Fails to compile if count is wrong
```

**Benefits:**
- ✅ **Type-safe:** Can't list non-existent adapters (won't compile)
- ✅ **Verified:** Total count must match or compilation fails
- ✅ **Maintainable:** Single source of truth
- ✅ **No regex:** Python reads structured data

**2. Validation Script (`scripts/hybrid_test_report.py`):**

```python
def parse_coverage_metadata():
    """Extract type-checked metadata from Agda file"""
    content = Path("Tests/CoverageReport.agda").read_text()
    
    # Extract Agda-verified total
    match = re.search(r'totalAssertions ≡ (\d+)', content)
    return int(match.group(1))  # This came from type-checked Agda!

def validate_actual_vs_expected(actual, expected):
    if actual != expected:
        print("⚠️  WARNING: Metadata out of sync!")
        print(f"Update Tests/CoverageReport.agda")
```

### Workflow

**Developer adds new adapter:**

1. Add adapter type to `Tests/ObligationAdapters.agda`
2. Create checklist file (e.g., `Tests/FooChecklist.agda`)
3. **Update `Tests/CoverageReport.agda`:**
   - Add module to `allChecklists`
   - Update `totalAssertions ≡ N` assertion
4. Run `make` - if forgotten step 3, compilation fails!

**Build process:**

```bash
make                          # Agda validates metadata
python scripts/test_report.py # Uses validated metadata
# If someone forgot to update CoverageReport.agda, gets warning
```

## Future Enhancements

### Option A: Full Reflection-Based Automation

Use Agda's reflection API to extract metadata at compile-time:

```agda
open import Agda.Builtin.Reflection

-- Macro that enumerates all adapter types automatically
macro
  getAllAdapters : Term → TC ⊤
  getAllAdapters hole = do
    defs ← getDefinitions (quote Tests.ObligationAdapters)
    adapters ← filterAdapterTypes defs
    unify hole (quoteTerm adapters)
```

**Pros:** Fully automatic, impossible to forget
**Cons:** Complex, requires deep Agda reflection knowledge

### Option B: Compiled Agda → JSON Export

Compile CoverageReport.agda to executable that outputs JSON:

```agda
open import Agda.Builtin.IO

main : IO Unit
main = putStrLn (toJSON allChecklists)
```

```bash
agda --compile --ghc Tests/CoverageReport.agda
./Tests/CoverageReport > build/reports/metadata.json
python scripts/test_report.py  # reads JSON, no parsing!
```

**Pros:** Clean separation, no parsing needed
**Cons:** Requires GHC backend setup

### Option C: Adopt Agda Test Framework

Use existing Agda testing infrastructure (like agda-stdlib's):

**Pros:** Battle-tested, good tooling
**Cons:** External dependency, learning curve

## Recommendation

**Current state is sufficient** - we have type-checked metadata and validation.

**If fragility becomes an issue:** Implement Option B (compiled JSON export).

**For new projects:** Start with a proper test framework (Option C).

## Migration Path

To fully eliminate regex:

1. ✅ **Done:** `Tests/CoverageReport.agda` with type-checked metadata
2. **Next:** Compile to JSON executable (add to Makefile)
3. **Then:** Update `test_report.py` to read JSON exclusively
4. **Finally:** Remove all regex patterns from `test_report.py`

## Key Insight

**The solution isn't "more Agda introspection" - it's "single source of truth enforced by types."**

We don't need complex reflection if we:
1. Maintain explicit registry (CoverageReport.agda)
2. Type system ensures it's correct (won't compile if wrong)
3. Build catches discrepancies (validation warnings)

This is pragmatic, maintainable, and leverages Agda's strengths without fighting its limitations.
