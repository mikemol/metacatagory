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

## References

* [AGDA-PARAMETERIZATION-PLAN.md](../architecture/AGDA-PARAMETERIZATION-PLAN.md) - Full design rationale
* [AGDA-PARAMETERIZATION-SUMMARY.md](../architecture/AGDA-PARAMETERIZATION-SUMMARY.md) - Implementation status
* [Algebra.Groups.Basic](../../src/agda/Algebra/Groups/Basic.agda) - Reference implementation
* [Session Notes](../../intake/ALGEBRA-PARAMETERIZATION-COMPLETE.md) - Original Groups pattern

---

**Last Updated:** 2025-12-24  
**Status:** Migration Guide Complete  
**Target Audience:** Developers maintaining/extending the Agda codebase
