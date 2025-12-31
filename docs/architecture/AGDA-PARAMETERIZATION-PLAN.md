# Agda Code Parameterization Plan

## Executive Summary

This document outlines the systematic transition of hardcoded Agda elements to parameterized, reusable patterns following the successful `Algebra.Groups.Basic` parameterization (which moved 10 theorems from postulates to module parameters).

## Current State Analysis

### Hardcoded Elements Audit

#### 1. Dummy Algebraic Structures (`Core.AlgebraicAlgorithms`)

**Location:** Lines 29-177  
**Type:** Test scaffolding  
**Impact:** 18 hardcoded structure definitions

**Current Pattern:**

```agda
packedFieldBase : FieldDeclaration
packedFieldBase = record
  { underlyingRing = packedCommRingBase
  ; inverses = M.mkId "dummy-inverses"
  }
```

**Used By:**

* `Tests.AlgorithmSmokeTests` (3 references)
* Direct instantiation of algorithm interfaces

**Parameterization Opportunity:**

Replace with parameterized module pattern:

```agda
module AlgebraicAlgorithms 
  (baseField : FieldDeclaration)
  (extField : FieldDeclaration)
  where
  -- All algorithm interfaces receive fields as parameters
```

**Benefits:**

* Same algorithms work with test dummies or `ClassicalInstance` structures
* Eliminates 36 lines of Base/Ext duplication
* Enables property-based testing with arbitrary field instances

#### 2. Postulated Algorithm Scaffolds (`Core.AlgebraicAlgorithms`)

**Location:** Lines 315-338  
**Type:** Default implementations  
**Impact:** 21 postulates

**Current Pattern:**

```agda
postulate
  defaultMinimalPolynomial : (F E : FieldDeclaration) → (α : M.Identifier) → M.Identifier
  defaultGaloisGroup : (F E : FieldDeclaration) → (f : M.Identifier) → GaloisGroup F E
  defaultIsNormal : (F E : FieldDeclaration) → Dec (NormalExtension F E)
  -- ... 18 more
```

**Used By:**

* `MinimalPolynomialAlgorithm-generic` (21 algorithm constructors)
* Direct instantiation in tests

**Parameterization Opportunity:**

Follow `Algebra.Groups.Basic` pattern with module parameters:

```agda
module Algorithms.Basic
  (minimalPolynomial : (F E : FieldDeclaration) → (α : M.Identifier) → M.Identifier)
  (galoisGroup : (F E : FieldDeclaration) → (f : M.Identifier) → GaloisGroup F E)
  (isNormal : (F E : FieldDeclaration) → Dec (NormalExtension F E))
  -- ... remaining algorithms as parameters
  where
  
  -- Provide algorithm record constructors using parameters
  mkMinimalPolynomialAlgorithm : ∀ {F E} → MinimalPolynomialAlgorithm F E
  mkMinimalPolynomialAlgorithm = record
    { minimalPolynomial = minimalPolynomial
    ; isAlgebraic = isAlgebraic
    ; limitation = nothing
    }
```

**Benefits:**

* Eliminates 21 postulates (43% of non-FFI postulates)
* Makes algorithm dependencies explicit
* Enables multiple algorithm implementations (e.g., computational vs symbolic)
* Follows established parameterization pattern

#### 3. Hardcoded Growth History (`Core.GrowthMetrics`)

**Location:** Lines 233-244  
**Type:** Historical data  
**Impact:** 1 hardcoded list (9 entries)

**Current Pattern:**

```agda
metacatagoryGrowthHistory : List CoordinateAllocation
metacatagoryGrowthHistory =
  record { coordinate = M.mkCoord 0 0 ; timestamp = 0 ; context = "dispatch-root" } ∷
  record { coordinate = M.mkCoord 1 0 ; timestamp = 1 ; context = "field-basic" } ∷
  -- ... 7 more entries
  []
```

**Used By:**

* `metacatagoryGrowthSnapshot` (1 consumer)
* `phase13Density`, `phase13YDistribution` (2 analyses)
* `Tests.PerformanceBoundaryTests` (1 test)

**Parameterization Opportunity:**

Convert to module parameter accepting history list:

```agda
module GrowthAnalysis
  (history : List CoordinateAllocation)
  where
  
  growthSnapshot : GrowthSnapshot
  growthSnapshot = captureGrowthSnapshot (length history) history
  
  phaseDensities : List PhaseDensity
  phaseDensities = map (λ phase → calculatePhaseDensity phase history) (uniquePhases history)
```

**Benefits:**

* Enables analysis of different development branches (Categorical vs Classical)
* Supports historical analysis without modifying core module
* Allows testing with synthetic growth patterns

#### 4. Hardcoded Priority Levels (`Core.TechnicalDebt`)

**Location:** Lines 30-33  
**Type:** Configuration constants  
**Impact:** 2 priority definitions

**Current Pattern:**

```agda
lowPriority : Priority
lowPriority = mkPriority (("test-fixture", pos 1) ∷ []) []

highPriority : Priority
highPriority = mkPriority (("core-critical", pos 100) ∷ []) (lowPriority ∷ [])
```

**Used By:**

* Direct reference in debt annotations (implicit usage)
* Weight calculation function (lines 47-48)

**Parameterization Opportunity:**

Introduce priority strategy pattern:

```agda
record PriorityStrategy : Set where
  field
    base : Priority
    critical : Priority
    testFixture : Priority
    
module TechnicalDebtAnalysis
  (strategy : PriorityStrategy)
  where
  
  -- Use strategy.base, strategy.critical, etc.
```

**Alternative:** External JSON configuration (already established pattern):

```json
{
  "priority_levels": {
    "low": {"terms": [["test-fixture", 1]]},
    "high": {"terms": [["core-critical", 100]]}
  }
}
```

**Benefits:**

* Different projects can define criticality differently
* FFI safety vs proof completeness priorities can be customized
* Testing with extreme priority distributions

## Implementation Phases

### Phase 1: Algorithm Scaffolding Parameterization (High Priority)

**Target:** Eliminate 21 postulates in `Core.AlgebraicAlgorithms`

**Steps:**

1. Create `Algorithms.Basic` module with algorithm function parameters
2. Migrate `-generic` constructors to use parameters
3. Create `Algorithms.Defaults` with postulated implementations for backward compatibility
4. Update consumers (`Tests.AlgorithmSmokeTests`, etc.)
5. Document migration pattern

**Estimated Impact:**

* -21 postulates
* +1 parameterized module
* Changes to 3 test files

**Success Criteria:**

* All tests pass with parameterized algorithms
* Can instantiate with both dummy structures and `ClassicalInstance`

### Phase 2: Dummy Structure Parameterization (Medium Priority)

**Target:** Eliminate 18 hardcoded packed structures

**Steps:**

1. Create `module AlgebraicAlgorithms (baseField extField : FieldDeclaration)`
2. Remove `packedFieldBase`, `packedFieldExt`, etc.
3. Create `AlgebraicAlgorithms.TestInstances` with dummy structures
4. Update algorithm consumers to receive structures as parameters

**Estimated Impact:**

* -36 lines of duplicated code
* +1 test fixtures module
* Changes to algorithm interface modules

**Success Criteria:**

* Algorithm tests work with test dummies
* Algorithm production code works with `ClassicalInstance` fields
* No hardcoded structures in algorithm core

### Phase 3: Growth History Parameterization (Low Priority)

**Target:** Parameterize `metacatagoryGrowthHistory`

**Steps:**

1. Create `module GrowthAnalysis (history : List CoordinateAllocation)`
2. Move analysis functions into parameterized module
3. Create `GrowthAnalysis.Metacatagory` with current history
4. Create `GrowthAnalysis.Categorical` and `GrowthAnalysis.Classical` branches

**Estimated Impact:**

* +3 growth analysis instances
* Better separation of analysis vs data

**Success Criteria:**

* Can analyze different development branches independently
* Historical trends visible across branches

### Phase 4: Priority Strategy Parameterization (Optional)

**Target:** Make priority weighting configurable

**Steps:**

1. Define `PriorityStrategy` record
2. Create `module TechnicalDebtAnalysis (strategy : PriorityStrategy)`
3. Provide default strategies: `DefaultStrategy`, `FFISafetyStrategy`, `ProofCompletenessStrategy`

**Estimated Impact:**

* +1 strategy abstraction
* +3 strategy implementations

**Success Criteria:**

* Can generate badges with different priority weightings
* FFI-focused projects can prioritize external bindings

## Migration Pattern Reference

### From: Global Postulates

```agda
postulate
  theorem1 : ∀ G → Property1 G
  theorem2 : ∀ G → Property2 G

useTheorems : ∀ G → Result
useTheorems G = combine (theorem1 G) (theorem2 G)
```

### To: Module Parameters

```agda
module Basic
  (theorem1 : ∀ G → Property1 G)
  (theorem2 : ∀ G → Property2 G)
  where
  
  useTheorems : ∀ G → Result
  useTheorems G = combine (theorem1 G) (theorem2 G)

-- For backward compatibility during transition
module DefaultBasic where
  postulate
    defaultTheorem1 : ∀ G → Property1 G
    defaultTheorem2 : ∀ G → Property2 G
  
  open Basic defaultTheorem1 defaultTheorem2 public
```

## Expected Outcomes

### Quantitative

* **Postulate Reduction:** -21 (from 46 to 25 non-FFI postulates)
* **Code Reuse:** +100% (algorithms work with test and production data)
* **Configuration:** +4 parameterization points

### Qualitative

* **Transparency:** Algorithm dependencies visible as module parameters
* **Testability:** Can test with arbitrary algebraic structures
* **Modularity:** Different analysis branches without code duplication
* **Consistency:** Follows established `Algebra.Groups.Basic` pattern

## Dependencies

* `Algebra.Groups.Basic` (reference implementation)
* `ALGEBRA-PARAMETERIZATION-COMPLETE.md` (design rationale)
* Current test suite (compatibility validation)

## Risks and Mitigation

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Breaking existing tests | High | Medium | Create backward-compatible defaults module |
| Increased module complexity | Medium | Low | Follow established pattern from Groups.Basic |
| Performance regression | Low | Low | Agda normalizes module parameters efficiently |
| Incomplete migration | Medium | Medium | Phased approach with independent milestones |

## Next Actions

1. **Immediate:** Create `Algorithms.Basic` skeleton
2. **Week 1:** Migrate first 5 algorithm postulates
3. **Week 2:** Complete algorithm parameterization
4. **Week 3:** Parameterize dummy structures
5. **Month 1:** Review and document patterns

---

**Document Status:** Planning  
**Last Updated:** 2025-12-24  
**Related:** `ALGEBRA-PARAMETERIZATION-COMPLETE.md`, `docs/workflows/PARAMETERIZATION.md`
