# Agda Parameterization Migration Guide

This guide helps developers migrate from hardcoded Agda elements to the new parameterized modules.

## Executive Summary

**What Changed:**

* 21 algorithm postulates → parameterized `Algorithms.Basic` module
* 18 hardcoded structures → `Algorithms.TestInstances` module
* Hardcoded growth history → parameterized `GrowthAnalysis` module
* Hardcoded priorities → strategy-based `TechnicalDebt.Priorities` module

**When to Migrate:**

* New code should always use parameterized modules
* Existing code can use `Defaults` modules during transition
* No breaking changes - old imports still work

**Current Status:**

* All parameterized modules created and compiled ✅
* Backward compatibility maintained ✅
* Gradual migration path enabled ✅

---

## 1. Algorithm Parameterization

### Old Pattern (Using Postulates)

**Core.AlgebraicAlgorithms:**

```agda
postulate
  defaultMinimalPolynomial : (F E : FieldDeclaration) → (α : M.Identifier) → M.Identifier
  defaultGaloisGroup : (F E : FieldDeclaration) → (f : M.Identifier) → GaloisGroup F E
  -- ... 19 more postulates

MinimalPolynomialAlgorithm-generic : ∀ {F E} → MinimalPolynomialAlgorithm F E
MinimalPolynomialAlgorithm-generic = record
  { minimalPolynomial = λ α → defaultMinimalPolynomial F E α
  ; isAlgebraic = λ α → defaultIsAlgebraic F E α
  ; limitation = nothing
  }
```

**Usage:**

```agda
testAlgorithm : MinimalPolynomialAlgorithm packedFieldBase packedFieldExt
testAlgorithm = MinimalPolynomialAlgorithm-generic
```

### New Pattern (Using Parameters)

**Algorithms.Basic:**

```agda
module Basic
  (minimalPolynomial : (F E : FieldDeclaration) → (α : M.Identifier) → M.Identifier)
  (isAlgebraic : (F E : FieldDeclaration) → (α : M.Identifier) → Dec (AlgebraicElement F E α))
  (galoisGroup : (F E : FieldDeclaration) → (f : M.Identifier) → GaloisGroup F E)
  -- ... 18 more parameters
  where

  mkMinimalPolynomialAlgorithm : ∀ {F E} → MinimalPolynomialAlgorithm F E
  mkMinimalPolynomialAlgorithm = record
    { minimalPolynomial = minimalPolynomial
    ; isAlgebraic = isAlgebraic
    ; limitation = nothing
    }
```

**Migration: Three Steps**

**Step 1: Use Defaults (No Changes Needed)**

```agda
open import Algorithms.Basic.Defaults as AlgoDefaults

testAlgorithm : MinimalPolynomialAlgorithm packedFieldBase packedFieldExt
testAlgorithm = AlgoDefaults.mkMinimalPolynomialAlgorithm
```

**Step 2: Use With Real Implementations**

```agda
module MyAlgorithms where
  open import Algorithms.Basic

  myMinimalPoly : (F E : FieldDeclaration) → (α : M.Identifier) → M.Identifier
  myMinimalPoly F E α = M.mkId "my-algorithm"

  myIsAlgebraic : (F E : FieldDeclaration) → (α : M.Identifier) → Dec (AlgebraicElement F E α)
  myIsAlgebraic F E α = yes (record {})

  -- ... other algorithm implementations

  open Basic myMinimalPoly myIsAlgebraic {- ... other args -} public
```

**Step 3: Use Computational or Symbolic Variants**

```agda
-- For actual computations (future implementation)
open import Algorithms.Computational

-- Or for symbolic manipulation (future implementation)
open import Algorithms.Symbolic
```

### Algorithm Composition

The new parameterized module enables deriving composite algorithms:

```agda
open import Algorithms.Basic using (isGaloisExtension; galoisGroupIfGalois)

-- Only compute Galois group if extension is already known to be Galois
myGaloisGroup : (F E : FieldDeclaration) → (f : M.Identifier) → Maybe (GaloisGroup F E)
myGaloisGroup = galoisGroupIfGalois
```

---

## 2. Dummy Structure Parameterization

### Old Pattern (Hardcoded)

**Core.AlgebraicAlgorithms lines 29-177:**

```agda
packedMagmaBase : MagmaDeclaration
packedMagmaBase = record { ... }

packedSemigroupBase : SemigroupDeclaration
packedSemigroupBase = record { underlyingMagma = packedMagmaBase ; ... }

-- ... 18 total definitions
```

### New Pattern (Isolated Fixtures)

**Algorithms.TestInstances:**

```agda
open import Algorithms.TestInstances

-- Use convenience names
dummyRing : RingDeclaration
dummyRing = packedRingBase

dummyExtField : FieldDeclaration
dummyExtField = packedFieldExt

-- Or use full names
myRing = Algorithms.TestInstances.packedCommRingBase
```

### Migration Path

**Step 1: Update Imports**

**Before:**

```agda
open import Core.AlgebraicAlgorithms using (packedFieldBase)

test = packedFieldBase
```

**After:**

```agda
open import Algorithms.TestInstances using (packedFieldBase)

test = packedFieldBase
```

**Step 2: Use Convenience Names**

```agda
open import Algorithms.TestInstances using 
  ( dummyBaseField
  , dummyExtField
  , dummyRing
  , dummyAbelianGroup
  )

test₁ = dummyBaseField
test₂ = dummyExtField
```

**Step 3: Parameterize Your Code**

```agda
module MyTests (baseField extField : FieldDeclaration) where
  -- Your tests work with any fields, not just test dummies
  testAlgorithm : MinimalPolynomialAlgorithm baseField extField
  testAlgorithm = record { ... }
```

---

## 3. Growth History Parameterization

### Old Pattern (Hardcoded)

**Core.GrowthMetrics lines 233-244:**

```agda
metacatagoryGrowthHistory : List CoordinateAllocation
metacatagoryGrowthHistory =
  record { coordinate = M.mkCoord 0 0 ; timestamp = 0 ; context = "dispatch-root" } ∷
  record { coordinate = M.mkCoord 1 0 ; timestamp = 1 ; context = "field-basic" } ∷
  -- ...
  []

phase13Density = calculatePhaseDensity 13 metacatagoryGrowthHistory
```

### New Pattern (Multiple Branches)

**GrowthAnalysis:**

```agda
open import GrowthAnalysis

-- Analyze main branch
open GrowthAnalysis.Metacatagory

-- Or analyze categorical branch independently
open GrowthAnalysis.Categorical

-- Or classical algebra branch
open GrowthAnalysis.Classical

-- Access analysis results
density = Metacatagory.phase13Density
rate = Categorical.growthRate
pattern = Classical.expansionPattern
```

### Migration Path

**Step 1: Use Backward Compatibility Exports**

```agda
-- These still work
phase13Density = GrowthAnalysis.phase13Density  
metacatagoryGrowthSnapshot = GrowthAnalysis.metacatagoryGrowthSnapshot
```

**Step 2: Analyze Different Branches**

```agda
-- Compare development strategies
categoricalDensity = GrowthAnalysis.Categorical.growthRate
classicalDensity = GrowthAnalysis.Classical.growthRate

-- Observe differences in expansion patterns
catPattern = GrowthAnalysis.Categorical.expansionPattern  -- Expected: vertical (phases)
classPattern = GrowthAnalysis.Classical.expansionPattern  -- Expected: horizontal (coords)
```

**Step 3: Create Your Own Analysis**

```agda
module MyBranchAnalysis where
  myHistoryEvents : List CoordinateAllocation
  myHistoryEvents = {- your event list -}
  
  open GrowthAnalysis.Analysis myHistoryEvents
  
  -- Analyze your custom history
  myGrowthRate = growthRate
  myExpansionPattern = expansionPattern
```

---

## 4. Priority Strategy Parameterization

### Old Pattern (Hardcoded)

**Core.TechnicalDebt lines 30-33:**

```agda
lowPriority : Priority
lowPriority = mkPriority (("test-fixture", pos 1) ∷ []) []

highPriority : Priority
highPriority = mkPriority (("core-critical", pos 100) ∷ []) (lowPriority ∷ [])

-- Fixed weights used in badge generation
```

### New Pattern (Strategy Selection)

**TechnicalDebt.Priorities:**

```agda
open import TechnicalDebt.Priorities

-- Choose strategy based on project needs
module MyProject where
  -- For FFI-heavy project
  open strategy using ffiSafetyStrategy
  
  -- Extract priority levels
  criticalPriority = PriorityStrategy.critical ffiSafetyStrategy
  -- safety: 800 (high), proof: 30 (low)

-- Or use different strategy for proof formalization
module MathProject where
  open strategy using proofCompletenessStrategy
  
  criticalPriority = PriorityStrategy.critical proofCompletenessStrategy
  -- safety: 150 (low), proof: 300 (high)
```

### Available Strategies

| Strategy | Use Case | Safety Weight | Proof Weight |
|----------|----------|---------------|--------------|
| `defaultStrategy` | General development | 200 | 150 |
| `ffiSafetyStrategy` | FFI-heavy projects | 800 | 30 |
| `proofCompletenessStrategy` | Formalization projects | 150 | 300 |
| `rapidDevelopmentStrategy` | Prototyping | 75 | 25 |
| `productionStrategy` | Release hardening | 500 | 400 |

### Migration Path

**Step 1: Use Backward Compatibility Exports**

```agda
-- These still work
lowP = TechnicalDebt.Priorities.lowPriority
highP = TechnicalDebt.Priorities.highPriority
```

**Step 2: Select Project Strategy**

```agda
-- In your project config
STRATEGY := ffiSafetyStrategy  -- For FFI-focused development

-- Use in badge generation
python3 generate-badges.py --strategy ffiSafety
```

**Step 3: Extend with Custom Strategies**

```agda
module MyOrganization where
  open import TechnicalDebt.Priorities

  myCustomStrategy : PriorityStrategy
  myCustomStrategy = record
    { minimal = mkPriority (("dev", pos 5) ∷ []) []
    ; low = mkPriority (("low", pos 20) ∷ []) []
    ; medium = mkPriority (("mid", pos 75) ∷ []) []
    ; high = mkPriority (("high", pos 200) ∷ []) []
    ; critical = mkPriority (("critical", pos 1000) ∷ []) []
    -- ... specialized priorities
    }
```

---

## Backward Compatibility

### What Still Works

All original imports and usages remain functional:

```agda
-- These still compile and work (using Defaults internally)
import Core.AlgebraicAlgorithms using (MinimalPolynomialAlgorithm-generic)
import Core.GrowthMetrics using (metacatagoryGrowthHistory)
import Core.TechnicalDebt using (lowPriority; highPriority)
```

### Migration Timelines

**Phase 1 (Current):** Create parameterized modules, maintain old postulates

**Phase 2 (Next):** Update consumers to use parameterized modules gradually

**Phase 3 (Later):** Mark old postulates as deprecated with comments

**Phase 4 (Future):** Remove old postulates after all consumers migrated

---

## Testing Your Migration

### Verify Compilation

```bash
# Test each new module independently
agda -i src/agda src/agda/Algorithms/Basic.agda
agda -i src/agda src/agda/Algorithms/TestInstances.agda
agda -i src/agda src/agda/GrowthAnalysis.agda
agda -i src/agda src/agda/TechnicalDebt/Priorities.agda

# Verify full build still works
make validate-constructive
```

### Test Parameterization

```agda
-- In a test file, verify you can instantiate with different implementations
module TestParameterization where
  import Algorithms.Basic
  import Algorithms.Computational
  import Algorithms.Symbolic
  
  -- Both should work
  compAlgo = Algorithms.Computational.mkMinimalPolynomialAlgorithm
  symAlgo = Algorithms.Symbolic.mkMinimalPolynomialAlgorithm
```

---

## FAQ

**Q: Do I need to migrate existing code immediately?**

A: No. Backward compatibility is maintained. Migrate gradually - new code should use parameterized modules, existing code can use Defaults modules.

**Q: What if I want to keep using postulates?**

A: The old postulates are preserved in `Core.AlgebraicAlgorithms` and re-exported via `Algorithms.Basic.Defaults`. You can keep using them during migration.

**Q: How do I create multiple algorithm implementations?**

A: Create separate modules that provide the algorithm functions, then instantiate `Algorithms.Basic` with your implementations. See `Algorithms.Computational` and `Algorithms.Symbolic` placeholders.

**Q: Can I combine multiple growth branches in analysis?**

A: Yes - they're all instances of the same `Analysis` module. Create composite analyses by instantiating with merged history lists.

**Q: How do I add a new priority strategy?**

A: Create a new definition following the `PriorityStrategy` record pattern in `TechnicalDebt.Priorities`. Export it for use in your project.

---

## 5. Logic-Formatting Separation Principle (PRIO-ADOPT-1)

All parameterized systems enforce strict separation between **domain logic** and **presentation/formatting**. This ensures testability, maintainability, and clean interfaces.

### Pattern

**Agda: Pure Logic Only**

```agda
module TechnicalDebt.PriorityMapping where

-- LOGIC LAYER: Domain computation, no side effects
extractPriorityWeight : Priority → Int
strategyToWeights : PriorityStrategy → CategoryWeights

-- Postulate FFI details (implementation detail)
postulate extractPriorityWeight : Priority → Int

-- No JSON, no formatting, no I/O
```

**Python: Explicit Layers**

```python
# LOGIC LAYER: Pure computation
def agda_to_python_weights(strategy_name: str) -> Dict[str, float]:
    """Pure mapping from Agda priorities to Python categories.
    No I/O, no side effects."""
    # ... compute weights ...
    return weights

def build_weight_profiles() -> Dict[str, Any]:
    """Compute all profiles. Pure function."""
    return {s: agda_to_python_weights(s) for s in strategies}

# FORMAT LAYER: Presentation/serialization  
def format_weights_json(profiles: Dict[str, Any]) -> Dict[str, Any]:
    """Wrap computed data with JSON metadata."""
    return {"active": "default", "profiles": profiles, "_comment": "..."}
```

### Benefits

| Concern | Pure Logic | Presentation |
|---------|-----------|--------------|
| Testing | Unit test logic independently | Integration test I/O |
| Debugging | Isolate domain bugs | Isolate format bugs |
| Maintenance | Change logic without touching serialization | Change format without touching logic |
| Reuse | Use same logic in multiple formats | Swap formats without code changes |
| Dependencies | Minimal (domain types only) | Can import logic layer freely |

### Implementation Checklist

When adding a new parameterized system:

* [ ] **Agda**: Define pure mapping functions, postulate any FFI
* [ ] **Agda**: Export only types and pure functions (no I/O)
* [ ] **Python**: Create LOGIC LAYER functions (pure, no side effects)
* [ ] **Python**: Create FORMAT LAYER for serialization
* [ ] **Python**: Add unit tests for logic layer only
* [ ] **Python**: Document mapping strategy clearly

### Example: Adding a New Priority Category

If you wanted to add a `reliability` category to technical debt:

**Agda change** (TechnicalDebt.Priorities):

```agda
record PriorityStrategy : Set where
  field
    -- ... existing fields ...
    reliability : Priority  -- NEW: tracking reliability issues
```

**Python changes** (adopt_priority_strategies.py):

```python
# LOGIC LAYER - update mapping
def agda_to_python_weights(strategy_name: str):
    # ... existing logic ...
    # reliability ← highPriority field (safety-adjacent)
    return {
        # ... existing categories ...
        "reliability": agda_strat["high"] / 100.0  # NEW
    }

# FORMAT LAYER - automatic (inherits from logic output)
# No changes needed - format_weights_json() will serialize new key
```

**Result**: New category flows from Agda through Python logic to output format, with clear separation at each layer.

---

---

## 5. Logic-Formatting Separation

### Pattern Overview

Separate **domain logic** (pure computation) from **formatting** (output generation):

* **Logic Layer**: Pure Agda computation with no I/O or side effects
* **Format Layer**: Serialization and output (ideally in Agda for compile-time guarantees)
* **Orchestration Layer**: Python integration and tool invocation

### Example: Priority Strategy Formatting

#### Old Pattern (Mixed Concerns)

```python
# scripts/generate-badges.py (LOGIC + FORMAT + ORCHESTRATION)

def format_priority_weights(strategy):
    # THIS MIXES LOGIC AND FORMATTING
    weights = {
        'postulate': compute_postulate_weight(strategy),  # LOGIC
        'todo': compute_todo_weight(strategy),            # LOGIC
        'fixme': compute_fixme_weight(strategy),          # LOGIC
    }
    json_string = json.dumps(weights)  # FORMATTING
    return json_string                  # ORCHESTRATION
```

**Problems:**

* Logic buried in Python script
* Formatting logic scattered across multiple functions
* Hard to test logic independently
* Hard to add new output formats (YAML, TOML, etc.)

#### New Pattern (Separated Layers)

**Layer 1: Logic (Pure Agda)**

```agda
module TechnicalDebt.PriorityMapping where

-- Pure computation, no I/O, no formatting
strategyToWeights : PriorityStrategy → CategoryWeights
strategyToWeights strategy = record
  { postulateWeight = extractPriorityWeight (strategy.proof)
  ; todoWeight = extractPriorityWeight (strategy.documentation)
  ; fixmeWeight = extractPriorityWeight (strategy.safety)
  ; deviationWeight = extractPriorityWeight (strategy.critical)
  }
```

**Benefits:**

* Type-checked at compile-time
* No I/O, purely computational
* Independently testable
* Can be verified mathematically

**Layer 2: Format (Agda)**

```agda
module TechnicalDebt.PriorityFormatting where

open import TechnicalDebt.PriorityMapping

-- Postulated JSON generation (can be implemented via FFI)
formatAllStrategyProfiles : String
formatAllStrategyProfiles = {- produces valid JSON -}

-- Or build from components
formatStrategy : PriorityStrategy → String
formatStrategy s = 
  "{\"weights\": " ++ formatWeights (strategyToWeights s) ++ "}"
```

**Benefits:**

* Domain-specific formatting co-located with logic
* Compile-time format generation
* Easy to add new strategies without Python changes
* All formatting rules in one place

**Layer 3: Orchestration (Python)**

```python
# scripts/adopt_priority_strategies.py (PURE ORCHESTRATION)

def main():
    # LOGIC LAYER: Load from TechnicalDebt.PriorityMapping (Agda compiled)
    weights = agda_to_python_weights()
    
    # FORMAT LAYER: Load from TechnicalDebt.PriorityFormatting (Agda compiled)
    json_output = load_agda_formatted_json()
    
    # ORCHESTRATION: Integrate with rest of system
    validate_and_save(json_output)
    trigger_build_artifacts()
```

**Benefits:**

* Python does integration only
* No domain logic in Python
* No formatting logic in Python
* Easy to swap implementations

### Migration Steps

**Step 1: Extract Logic**

Create pure logic function in Agda with **no I/O, no formatting**:

```agda
-- ✓ GOOD: Pure computation
myPureLogic : Input → Output
myPureLogic input = record { ... }

-- ✗ BAD: Contains I/O
myMixedLogic : Input → IO Output
myMixedLogic input = do { ... }
```

**Step 2: Move Formatting to Agda**

Create formatting functions separate from logic:

```agda
-- ✗ BAD: Mixed logic and formatting
strategyToJson : PriorityStrategy → String
strategyToJson s = ... strategyToWeights s ...  -- MIXING

-- ✓ GOOD: Separate layers
strategyToJson : PriorityStrategy → String
strategyToJson s = 
  let weights = strategyToWeights s  -- LOGIC
  in formatWeightsAsJson weights     -- FORMAT
```

**Step 3: Parameterize Orchestration (Complete Agda)**

Instead of keeping orchestration in Python, move it to Agda with parameterized I/O:

```agda
-- Parameterized orchestration module
open import Agda.Builtin.String
open import Agda.Builtin.Unit
open import Agda.Builtin.IO

module TechnicalDebt.PriorityOrchestration
  (writeFile : String → String → IO ⊤)
  (readFile : String → IO String)
  (putStrLn : String → IO ⊤)
  -- ... other I/O operations
  where
  
  -- Use the I/O operations to coordinate logic and formatting
  generateConfig : IO ⊤
  generateConfig = do
    let jsonOutput = formatAllStrategyProfiles  -- FORMAT LAYER
    writeFile "output.json" jsonOutput          -- I/O OPERATION
    putStrLn "✓ Generated configuration"
```

**Step 4: Instantiate with FFI**

Create concrete instantiation with GHC FFI:

```agda
module TechnicalDebt.PriorityOrchestrationFFI where

postulate
  ffi-writeFile : String → String → IO ⊤
  ffi-putStrLn : String → IO ⊤
  -- ... other FFI operations

{-# COMPILE GHC ffi-writeFile = \path content -> System.IO.writeFile path content #-}
{-# COMPILE GHC ffi-putStrLn = System.IO.putStrLn #-}

-- Instantiate with FFI implementations
open import TechnicalDebt.PriorityOrchestration
  ffi-writeFile
  ffi-readFile
  ffi-putStrLn
  -- ... other implementations
  public
```

**Step 5: Optional Python Wrapper**

**Step 5: Optional Python Wrapper**

Python becomes optional thin wrapper for compilation:

```python
# ✓ GOOD: Pure orchestration
def main():
    logic_result = call_agda_logic()      # Load logic results
    formatted = load_agda_formatting()    # Load formatted output
    trigger_downstream(formatted)         # Orchestrate
```

Or compile Agda directly to executable:

```bash
agda --compile --include-path=src/agda \
  src/agda/TechnicalDebt/PriorityOrchestrationFFI.agda

# Run the compiled binary
./build/agda/TechnicalDebt/PriorityOrchestrationFFI
```

### Testing Each Layer

**Logic Layer Tests** (Pure, Fast, Deterministic)

```python
# tests/test_priority_mapping.py
def test_strategy_weights_are_consistent():
    # Logic should produce same result every time
    result1 = compute_weights(strategy)
    result2 = compute_weights(strategy)
    assert result1 == result2  # ✓ PASSES: deterministic

def test_strategy_specific_emphasis():
    # ffiSafety emphasizes safety over proof
    ffi_weights = compute_weights(ffiSafetyStrategy)
    assert ffi_weights['fixme'] > ffi_weights['postulate']
```

**Format Layer Tests** (Output Validation)

```python
def test_json_output_valid():
    json_str = load_agda_json()
    parsed = json.loads(json_str)
    assert 'strategies' in parsed
    assert all('weights' in s for s in parsed['strategies'])
```

**Orchestration Tests** (Can be in Agda too!)

```agda
-- Test orchestration by mocking I/O operations
module TestOrchestration where
  
  -- Mock I/O that captures calls
  mockWriteFile : String → String → IO ⊤
  mockWriteFile path content = {- record call -}
  
  mockPutStrLn : String → IO ⊤
  mockPutStrLn s = {- record output -}
  
  -- Instantiate with mocks
  open import TechnicalDebt.PriorityOrchestration
    mockWriteFile
    mockReadFile
    mockFileExists
    mockPutStrLn
    -- ... other mocks
  
  -- Run tests
  testGeneration : IO ⊤
  testGeneration = generateReferenceConfig
```

### Benefits of Separation

| Aspect | Before | After (Complete Agda) |
|--------|--------|----------------------|
| **Testability** | Hard (mixed concerns) | Easy (all layers testable in Agda) |
| **Reusability** | Logic trapped in Python | All logic in Agda, reusable everywhere |
| **Correctness** | Manual testing only | Compile-time verification of all layers |
| **Extensibility** | Add Python code for new formats | Add Agda format function, parameterize I/O |
| **Maintainability** | Logic scattered across files | Single-language implementation |
| **Performance** | Python formatting at runtime | Native compiled executable (GHC) |
| **Type Safety** | Only at Python boundaries | Full type safety from logic to I/O |
| **Testing** | Separate Python test suite | Mock I/O in Agda, test all layers |

### Implementation Checklist

* [ ] **Extract pure logic** from Python into Agda (no I/O, no formatting)
* [ ] **Create logic tests** (verify computation correctness)
* [ ] **Move formatting** to Agda (separate from logic)
* [ ] **Create format tests** (verify output structure)
* [ ] **Parameterize orchestration** (I/O operations as module parameters)
* [ ] **Create FFI instantiation** (concrete I/O implementations via GHC)
* [ ] **Create orchestration tests** (mock I/O operations in Agda)
* [ ] **Optional: Python wrapper** for build system integration
* [ ] **Document layer responsibilities** in module comments
* [ ] **Update ROADMAP** to reference architecture
* [ ] **Add to onboarding** so new developers understand separation

### Validation Questions

When reviewing code with mixed concerns, ask:

1. **Is there pure computation?** → Should be in Agda logic layer, tested independently
2. **Is there string building?** → Should be in Agda format layer, co-located with logic
3. **Is there I/O or side effects?** → Should be parameterized in Agda orchestration layer
4. **Can logic be tested without format?** → If not, they're not separated
5. **Can format be changed without logic changes?** → If not, they're not separated
6. **Can I/O be mocked for testing?** → If not, use module parameterization
7. **Can the entire system run without Python?** → If not, consider full Agda migration

**Advanced Pattern: Complete Agda Implementation**

For maximum type safety and single-language implementation:

* Logic: Pure Agda (no I/O, no formatting)
* Format: Pure Agda (string generation, co-located with logic)
* Orchestration: Parameterized Agda (I/O operations as module parameters)
* FFI: Concrete instantiation (GHC backend with System.IO)
* Optional: Python wrapper (thin compilation/execution wrapper)

This enables:

* Compile-time verification of all three layers
* Native executable performance (via GHC)
* Testable I/O via mocking (parameterized module instantiation)
* Single-language codebase (Agda only, Python optional)

---

## References

* [AGDA-PARAMETERIZATION-PLAN.md](../architecture/AGDA-PARAMETERIZATION-PLAN.md) - Full design rationale
* [AGDA-PARAMETERIZATION-SUMMARY.md](../architecture/AGDA-PARAMETERIZATION-SUMMARY.md) - Implementation status
* [Algebra.Groups.Basic](../../src/agda/Algebra/Groups/Basic.agda) - Reference implementation
* [Session Notes](../../intake/ALGEBRA-PARAMETERIZATION-COMPLETE.md) - Original Groups pattern

---

**Last Updated:** 2025-12-24  
**Status:** Logic-Formatting Separation Pattern Documented  
**Target Audience:** Developers maintaining/extending the Agda codebase
