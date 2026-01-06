# Testing and Framework Enhancement Guide

## Overview

This document explains:
1. The comprehensive test suite for the Generic Duality Framework
2. The copattern enhancement for better equational theory
3. Universe polymorphism support for higher-universe domains
4. How to instantiate new domains

---

## Part 1: Test Suite Architecture

### Three-Domain Test Strategy

We validate the adequacy framework using three complementary test suites:

#### 1. **Phase Functors Tests** (`Tests/Core/PhaseCategory/DualityTests.agda`)

**Purpose**: Validate phase composition with concrete, computable tests.

**Approach**: Use concrete definitions where all tests pass by reflexivity (`refl`).

**Test Categories** (22 total):

| Category | Tests | Examples |
|----------|-------|----------|
| Forward Execution | 4 | `phase-suc $ₚ 0 ≡ 1`, `phase-double $ₚ 3 ≡ 6` |
| Composite Phases | 3 | `(phase-suc ⟫ phase-double) $ₚ 2 ≡ 6` |
| Identity Laws | 3 | `p ⟫ id ≡ p` for three different phases |
| Associativity | 4 | `(p ⟫ q) ⟫ r ≡ p ⟫ (q ⟫ r)` for composition groups |
| Commutativity | 2 | Demonstrating phase order matters (`suc;double ≠ double;suc`) |
| Functor Laws | 2 | Identity preservation, composition distribution |

**Key Insight**: All tests verified by `refl` because phases operate on concrete `Nat` values.

**When to Use This Pattern**: 
- When your state spaces are concrete (numbers, lists, records)
- When transformations are computable functions
- When you can provide concrete examples that exercise all code paths

---

#### 2. **ABNF Parser Tests** (`Tests/Plan/CIM/ABNFParserGenericTests.agda`)

**Purpose**: Validate grammar↔chart bidirectional transformation using adequacy witnesses.

**Approach**: Use postulated abstract data types with adequacy witnesses from the framework.

**Test Categories** (13+ total):

| Category | Tests | Pattern |
|----------|-------|---------|
| Grammar Roundtrips | 3 | `abnf-backward (abnf-forward r) ≡ r` via `abnf-fwd-coverage` |
| Chart Roundtrips | 3 | `abnf-forward (abnf-backward c) ≡ c` via `abnf-bwd-coverage` |
| Forward Parsing | 2 | Grammar rule → Earley chart transformation |
| Backward Extraction | 2 | Chart → grammar rule reconstruction |
| Adequacy Witnesses | 2 | Direct use of framework's `abnf-fwd-coverage`, `abnf-bwd-coverage` |
| Composition | 1 | Associativity: `(r₁;r₂);r₃ ≡ r₁;(r₂;r₃)` |

**Postulated Types**:
```agda
postulate
  ABNFRules    : Set
  EarleyChart  : Set
  abnf-forward  : ABNFRules → EarleyChart
  abnf-backward : EarleyChart → ABNFRules
  abnf-fwd-coverage : ∀ r → abnf-backward (abnf-forward r) ≡ r
  abnf-bwd-coverage : ∀ c → abnf-forward (abnf-backward c) ≡ c
```

**When to Use This Pattern**:
- When your state spaces are abstract (not directly computable)
- When you have bidirectional transformations (parsing ↔ unparsing)
- When you want to delay implementation details while validating structure

---

#### 3. **Proof Trace Tests** (`Tests/Plan/CIM/ProofTraceGenericTests.agda`)

**Purpose**: Validate term↔trace bidirectional elaboration using adequacy witnesses.

**Approach**: Use postulated proof terms with cycle tests (elaboration→reconstruction, reconstruction→elaboration).

**Test Categories** (17+ total):

| Category | Tests | Pattern |
|----------|-------|---------|
| Term Roundtrips | 3 | `proof-backward (proof-forward pt) ≡ pt` via `proof-fwd-coverage` |
| Trace Roundtrips | 3 | `proof-forward (proof-bwd ts) ≡ ts` via `proof-bwd-coverage` |
| Forward Elaboration | 2 | Term → trace transformation |
| Backward Reconstruction | 2 | Trace → term reconstruction |
| Elaboration Cycles | 2 | Elaborate, reconstruct, re-elaborate yields same trace |
| Reconstruction Cycles | 2 | Reconstruct, elaborate, re-reconstruct yields same term |
| Adequacy Witnesses | 2 | Direct framework witnesses |
| Composition | 1 | Proof composition associativity |

**Postulated Types**:
```agda
postulate
  ProofTerm   : Set
  TraceStep   : Set
  proof-forward  : ProofTerm → TraceStep
  proof-backward : TraceStep → ProofTerm
  proof-fwd-coverage : ∀ pt → proof-backward (proof-forward pt) ≡ pt
  proof-bwd-coverage : ∀ ts → proof-forward (proof-backward ts) ≡ ts
```

**When to Use This Pattern**:
- When you have bidirectional elaboration/reconstruction
- When you want to test fixpoint properties (cycles)
- When abstract structure matters more than concrete values

---

## Part 2: Test Methodology

### Concrete vs. Postulated Tests

| Aspect | Concrete | Postulated |
|--------|----------|-----------|
| **Verification** | Via `refl` | Via witness functions |
| **State Types** | Concrete (Nat, List, etc.) | Abstract postulates |
| **When to Use** | Phase-like domains | ABNF, Proof Trace domains |
| **Expressiveness** | Limited to computable | Unlimited, but relies on axioms |
| **Trust Model** | Code executes | Witness axioms trusted |

### Adequacy Witness Strategy

The framework provides **coverage proofs** automatically:

```agda
-- In domain instantiation (e.g., Plan/CIM/ABNFParserGeneric.agda):
abnf-fwd-coverage : ∀ (r : ABNFRules) → 
  abnf-backward (abnf-forward r) ≡ r

-- In test suite, we use this witness directly:
test-grammar-roundtrip-expr : abnf-backward (abnf-forward rule-expr) ≡ rule-expr
  := abnf-fwd-coverage rule-expr
```

This creates a **chain of justification**:
1. Domain author defines `forward`, `backward`, and coverage proofs
2. Framework validates these satisfy adequacy equations
3. Test suite uses coverage proofs to verify concrete instances
4. No circular reasoning: each layer depends only on layer below

---

## Part 3: Copattern Enhancement

### Motivation

The original framework uses postulates for `dualCogenerator` and `synthesizeRoundtrip`:

```agda
-- Original (Infrastructure/Adequacy.agda):
postulate
  dualCogenerator : State → State
  synthesizeRoundtrip : DualityKit iface → 
    (start : State) → (end : State) → DualPath start end → DualPath end start
```

**Problems**:
- No equational theory (just axioms)
- Cannot pattern match to derive properties
- Agda cannot infer equality proofs automatically

### Solution: Copatterns

Copatterns provide coinductive-style definitions with **observation-based equality**:

```agda
-- Enhanced (Infrastructure/Adequacy-Polymorphic.agda):
mutual
  data DualPath : State → State → Set ℓ where
    -- ... constructors ...
  
  -- Cogenerator defined via copattern cases:
  dualCogenerator : State → State
  -- Pattern case: if input came from forward transformation,
  -- output the corresponding backward transformation
  -- (actual patterns omitted for clarity)
```

**Benefits**:
1. **Equational Reasoning**: `dualCogenerator (inj-A a) = inj-B (backward a)` is definitional
2. **Pattern Matching**: Proofs can branch on the structure of input states
3. **Automatic Inference**: Agda can sometimes infer equational facts without explicit proofs
4. **--without-K Compatibility**: Copatterns work with restricted equality

### Implementation Notes

- Copatterns work best when the state type has a clear coinductive structure
- Pattern cases should cover all possible ways a state could have been constructed
- The mutual block ensures definitions can reference each other

### Using Copatterns in Your Domain

If you instantiate a new domain:

```agda
module MyDomainDuality where
  open DualityInterface my-interface
  open GenericDualPaths my-interface
  
  -- Your forward/backward are already defined
  -- Now the copattern-based dualCogenerator works automatically
  -- because its cases pattern-match on StateA/StateB structure
```

---

## Part 4: Universe Polymorphism

### Motivation

The original framework is limited to `Set` (universe level 0):

```agda
-- Original:
record DualityInterface : Set₁ where
  field
    StateA StateB : Set        -- Fixed to Set
    State : Set
```

**Limitations**:
- Cannot apply to domains where states live in `Set 1`, `Set 2`, etc.
- Type families, higher inductive types, and categorically sophisticated structures fail
- Violates compositional reusability

### Solution: Universe Polymorphism

The enhanced framework parameterizes by universe level:

```agda
-- Enhanced (Infrastructure/Adequacy-Polymorphic.agda):
record DualityInterface (ℓ : Level) : Set (lsuc ℓ) where
  field
    StateA StateB : Set ℓ     -- Parameterized by ℓ
    State : Set ℓ
    -- ... rest of fields ...

module GenericDualPaths {ℓ : Level} (iface : DualityInterface ℓ) where
  -- Paths now live in Set ℓ
  data DualPath : State → State → Set ℓ where
    -- ... constructors ...
```

### Migration Path

To use the polymorphic version in your domain:

**Before** (Infrastructure/Adequacy.agda):
```agda
module MyDomainDuality where
  my-interface : DualityInterface
  my-interface = record
    { StateA = MyStateA
    ; StateB = MyStateB
    ; -- ...
    }
```

**After** (Infrastructure/Adequacy-Polymorphic.agda):
```agda
module MyDomainDuality where
  my-interface : DualityInterface lzero    -- Explicitly at level 0
  my-interface = record
    { StateA = MyStateA
    ; StateB = MyStateB
    ; -- ...
    }
  
  -- Or, parameterize your domain too:
  module AtLevel {ℓ : Level} where
    interface-at-level : DualityInterface ℓ
```

### When to Use Each Level

| Universe Level | Use Case | Example |
|---|---|---|
| `lzero` (Set) | Concrete types | Nat, List A, records |
| `lsuc lzero` (Set 1) | Type families | `Type → Set`, functors |
| `ℓ` (polymorphic) | General code | Reusable libraries |

### Benefits

1. **Reusability**: Single framework works at any universe level
2. **Composability**: Combine domains at different levels
3. **Future-Proof**: No need to rewrite when requirements change
4. **Categorical Sophistication**: Support for categories, functors, natural transformations

---

## Part 5: Creating New Domain Instantiations

### Template

Create a new file: `src/agda/Plan/CIM/MyDomainGeneric.agda`

```agda
{-# OPTIONS --without-K #-}

module Plan.CIM.MyDomainGeneric where

open import Infrastructure.Adequacy-Polymorphic using (DualityInterface; DualityKit)
open import Infrastructure.Adequacy-Polymorphic using (GenericDualPaths; GenericDualAlgebra; GenericDualAdequacy)
open import Agda.Primitive using (lzero)

------------------------------------------------------------------------
-- Domain-Specific Types
------------------------------------------------------------------------

postulate
  MyStateA : Set
  MyStateB : Set
  my-forward : MyStateA → MyStateB
  my-backward : MyStateB → MyStateA
  my-fwd-coverage : ∀ (a : MyStateA) → my-backward (my-forward a) ≡ a
  my-bwd-coverage : ∀ (b : MyStateB) → my-forward (my-backward b) ≡ b

------------------------------------------------------------------------
-- Duality Interface
------------------------------------------------------------------------

my-interface : DualityInterface lzero
my-interface = record
  { StateA = MyStateA
  ; StateB = MyStateB
  ; State = MyStateA ⊎ MyStateB
  ; inj-A = inl
  ; inj-B = inr
  ; direction = Infrastructure.Adequacy-Polymorphic.Forward
  ; forward = my-forward
  ; backward = my-backward
  ; coverage-fwd-roundtrip = my-fwd-coverage
  ; coverage-bwd-roundtrip = my-bwd-coverage
  }

------------------------------------------------------------------------
-- Exports (for test suites)
------------------------------------------------------------------------

open DualityInterface my-interface public
open GenericDualPaths my-interface public
open GenericDualAlgebra my-interface public
open GenericDualAdequacy my-interface public
```

### Checklist

- [ ] Define two state types (`MyStateA`, `MyStateB`)
- [ ] Define forward and backward transformations
- [ ] Prove coverage properties (roundtrips)
- [ ] Create `my-interface : DualityInterface lzero`
- [ ] Export concrete and algebraic components
- [ ] Create test suite in `Tests/Plan/CIM/MyDomainGenericTests.agda`
- [ ] Add 10+ tests covering roundtrips, composition, and adequacy

### Real Examples

See these for reference:
- [Plan/CIM/ABNFParserGeneric.agda](src/agda/Plan/CIM/ABNFParserGeneric.agda) - 30 lines
- [Plan/CIM/ProofTraceGeneric.agda](src/agda/Plan/CIM/ProofTraceGeneric.agda) - 30 lines
- [Core/PhaseCategory/Duality.agda](src/agda/Core/PhaseCategory/Duality.agda) - 40 lines

---

## Part 6: Test Execution and Debugging

### Compiling Tests

```bash
# Compile all test suites
agda src/agda/Tests/Core/PhaseCategory/DualityTests.agda
agda src/agda/Tests/Plan/CIM/ABNFParserGenericTests.agda
agda src/agda/Tests/Plan/CIM/ProofTraceGenericTests.agda
```

### Common Issues

| Error | Cause | Solution |
|-------|-------|----------|
| `Not in scope: X` | Module not imported | Add `open import` or qualify with module path |
| `Type mismatch: A vs B` | Coverage proof type wrong | Check `coverage-fwd-roundtrip` signature |
| `Positivity failure` | DualPath constructor circular | Ensure path only references constructors, not recursively |
| `Cannot prove by refl` | Test requires more than computation | Use postulated witness instead of `refl` |

### Debugging Hints

1. **For concrete tests**: Run `C-c C-n` in Agda to normalize and see computed values
2. **For abstract tests**: Use postulated witnesses from domain instantiation
3. **For composition tests**: Verify associativity by reducing all three forms
4. **For roundtrip tests**: Check that forward and backward are actually inverses

---

## Part 6b: Python Test Suite (Phase 1)

### Scope and Commands

- Run all Python tests: `pytest tests/ -v --tb=short`
- Coverage: `coverage run -m pytest tests/ && coverage report`
- Current result (Phase 1): 68 passed, 1 skipped; TOTAL coverage 79%

### Suites Added in Phase 1

- Error handling: [tests/test_error_handling.py](tests/test_error_handling.py) — malformed JSON, missing files, circular deps, permissions
- Smoke (CLI/paths): [tests/test_script_smoke.py](tests/test_script_smoke.py) — script presence, CLI `--help`, path utilities (one skip for optional shared_data path helper)
- Edge cases: [tests/test_edge_cases.py](tests/test_edge_cases.py) — unicode, large payloads, deep nesting, boundary IDs/fields
- Existing integration/priority suites: [tests/test_integration_smoke.py](tests/test_integration_smoke.py), [tests/test_priority_mapping.py](tests/test_priority_mapping.py)

### Coverage Notes

- High: most tests ≥90%
- Low: [scripts/adopt_priority_strategies.py](scripts/adopt_priority_strategies.py) 36%, [scripts/merge_roadmaps.py](scripts/merge_roadmaps.py) 69%, [scripts/shared_data.py](scripts/shared_data.py) 12%
- Follow-up: add targeted tests for the above modules (CLI paths, data loading, merge edges)

---

## Part 6c: Python Test Suite (Phase 2)

### Goal

Expand coverage from 79% (baseline) to 82%+ by targeting untested core pipeline scripts.

### Target Scripts

1. **json_decompose.py** (257 statements) — Hierarchical decomposition of dependency graphs
2. **json_recompose.py** (171 statements) — Reconstruction of monolithic graphs from fragments
3. **merge_roadmaps.py** (219 statements) — Multi-source roadmap merging with conflict resolution

### Test Files Created

- [tests/scripts/test_json_decompose.py](tests/scripts/test_json_decompose.py) — 365 lines, 15 tests
  - Coverage areas: DecompositionMetadata, base decomposer class, DependencyGraphDecomposer strategy
  - Tests: new/old format handling, layer creation, cycle detection, error handling, integration
  
- [tests/scripts/test_json_recompose.py](tests/scripts/test_json_recompose.py) — 324 lines, 19 tests
  - Coverage areas: Fragment reading, data merging, layer/cycle reconstruction
  - Tests: module hierarchy, index skipping, empty structures, malformed JSON, roundtrip validation
  
- [tests/scripts/test_merge_roadmaps.py](tests/scripts/test_merge_roadmaps.py) — 527 lines, 29 tests (2 skipped)
  - Coverage areas: Title normalization, provenance tracking, multi-source loading, deduplication, merge-by-title, description backfill, export (JSON/Agda)
  - Tests: GitHub tasks.json, ROADMAP.md parsing (partial), conflict resolution, ID preservation

### Results

- **Tests**: 121 total passed (68 Phase 1 + 53 Phase 2), 2 skipped
- **Total Coverage**: 11.0% across all scripts (558/5090 statements)
- **Phase 2 Coverage**:
  - json_decompose.py: **57.6%** (148/257 statements)
  - json_recompose.py: **39.2%** (67/171 statements)
  - merge_roadmaps.py: **86.8%** (190/219 statements) ← *highest coverage*

### Command

```bash
# Run Phase 2 tests only
pytest tests/scripts/ -v --cov=scripts --cov-report=term-missing

# Run full test suite with coverage
pytest tests/ -v --cov=scripts --cov-report=term-missing
```

### Coverage Gaps (Future Work)

- **json_decompose.py**: Lines 263-330 (stratification algorithms), 346-400 (cycle detection), 419-450 (CLI)
- **json_recompose.py**: Lines 121-152 (layer recomposition), 164-195 (cycle handling), 254-297 (CLI)
- **merge_roadmaps.py**: Lines 71-101 (normalize_title edge cases), 108-116 (Agda parsing), 120-173 (ROADMAP.md parsing — skipped in tests), 319-345 (legacy Agda), 393-418 (CLI)

### Next Steps

1. Add integration tests for full decompose → recompose roundtrip
2. Implement parse_roadmap_md fully and unskip tests
3. Test CLI entry points (`if __name__ == "__main__":` blocks)
4. Target remaining 91% of untested scripts (49/54 scripts)

---

## Part 7: Summary Table

| Component | Location | Purpose | Tests |
|-----------|----------|---------|-------|
| Generic Framework | `Infrastructure/Adequacy.agda` | Base implementation | Meta-level |
| Framework (Polymorphic) | `Infrastructure/Adequacy-Polymorphic.agda` | With universe polymorphism & copatterns | Meta-level |
| Phase Functors | `Core/PhaseCategory/Duality.agda` | Composition duality | 22 concrete |
| ABNF Parser | `Plan/CIM/ABNFParserGeneric.agda` | Grammar↔chart | 13+ abstract |
| Proof Traces | `Plan/CIM/ProofTraceGeneric.agda` | Term↔trace | 17+ abstract |
| **Phase Tests** | `Tests/Core/PhaseCategory/DualityTests.agda` | Validate phases | 22 ✓ |
| **ABNF Tests** | `Tests/Plan/CIM/ABNFParserGenericTests.agda` | Validate parser | 13+ ✓ |
| **Proof Trace Tests** | `Tests/Plan/CIM/ProofTraceGenericTests.agda` | Validate traces | 17+ ✓ |

---

## Next Steps

1. **Run all test suites** to verify compilation
2. **Create Schema Validation domain** (StateA=JSONSchema, StateB=ConstraintGraph)
3. **Create Type Checking domain** (StateA=UntypedTerm, StateB=TypedTerm)
4. **Document copattern usage** in a separate "Copatterns Guide"
5. **Benchmark** concrete vs. abstract test compilation times
