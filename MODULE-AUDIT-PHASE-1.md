# Phase 1: Foundational Audit of Shared Modules
## Homological Review Following CHIP-N+1 Protocol

**Date:** January 6, 2026  
**Protocol:** Certified Coherence Induction Hierarchy (CHIP-N+1_RecursiveFibrationCycle)  
**Scope:** Audit of 8 shared modules for completeness, correctness, concreteness  

---

## Review Framework

### Three Orthogonal Audit Dimensions

1. **Syntactic Dimension** - Code structure, type safety, API consistency
2. **Semantic Dimension** - Correctness of meaning, absence of elisions, alignment with intent
3. **Pragmatic Dimension** - Test coverage, real-world applicability, edge cases

### Three Correction Priorities (Before Enrichment)

1. **Axiomatic Dependencies** - Unchallenged assumptions, missing verification
2. **Foundational Gaps** - Missing critical functionality
3. **Shallow Treatments** - "Conceptual" rather than concrete implementations

---

## Module 1: errors.py (278 LOC)

### Current State

**Source Claims:**
- Third-order implications 3.2.1-3.2.3 from behavioral taxonomy
- Implication 3.2.1: error classification
- Implication 3.2.3: error transformation
- Implication 3.3.3: error recovery hints
- FileIO primitive error boundary (implication 1.3)
- Validation primitive (implications 1.4.2.2.1-1.4.2.2.3)

**Current Implementation:**
- `ErrorSeverity` enum with 3 levels (WARNING=1, ERROR=2, FATAL=3)
- `ScriptError` base class with context dict
- 4 domain-specific errors: `FileOperationError`, `ValidationError`, `ParseError`, `ConfigurationError`
- 3 decorators: `@handle_errors`, `@retry_on_error`, `@collect_errors`

**Current Tests (30 tests, test_errors.py):**
- ErrorSeverity levels and representation
- ScriptError construction, hints, context, repr
- Domain-specific error construction
- Decorator functionality

### Audit Findings

#### A. SYNTACTIC DIMENSION ✓ SOUND

**Strengths:**
- Type hints present on all public APIs
- Clear module structure with logical organization
- Dataclass usage for context management
- Enum for severity (better than strings)

**Issues Identified:**

1. **Return Type Ambiguity in `handle_errors` wrapper:**
   - Line 176: `return None  # Continue execution`
   - Problem: For WARNING severity, decorator returns `None` instead of preserving function result
   - **Axiomatic Dependency**: Assumes caller expects `None` on WARNING
   - **Severity**: MEDIUM - affects type safety

2. **Missing Type Annotations:**
   - `wrapper` functions in decorators lack explicit return type annotations
   - `exceptions` parameter normalization happens but isn't type-safe

#### B. SEMANTIC DIMENSION ⚠️ INCOMPLETE

**Strengths:**
- Error messages are descriptive
- Context dict enables diagnostic tracing
- Recovery hints provide actionable guidance

**Gaps Identified:**

1. **ERROR RECOVERY INCOMPLETE:**
   - `recoverable` field on `ScriptError` is set but never checked
   - Decorators don't use recoverable flag to determine retry strategy
   - **Shallow Treatment**: Recovery field is conceptual, not operationalized
   - **Source Missing**: Where should recovery logic live?

2. **AXIOMATIC DEPENDENCY: Severity Routing Behavior:**
   - Line 169-178: Assumes WARNING → log and continue, others → raise
   - **Never Verified**: Is this the right policy? What if caller wants different behavior?
   - **Missing**: Configuration or strategy pattern for severity handling

3. **ERROR TRANSFORMATION INCOMPLETE:**
   - `ParseError` creates context with source/line/column
   - But no standardized way to transform context into diagnostic message
   - Claim: "error transformation" (3.2.3) but transformation is implicit

4. **VALIDATION ERROR CONTEXT MERGING:**
   - Line 88-89: Context dict merging logic is fragile
   - If `extra_context` and other kwargs have overlapping keys, `extra_context` wins
   - No clear ordering semantics

#### C. PRAGMATIC DIMENSION ⚠️ INCOMPLETE COVERAGE

**Current Test Coverage (30 tests):**
- ✓ ErrorSeverity basics
- ✓ ScriptError construction patterns
- ✓ Domain-specific errors
- ✓ Decorator basic functionality
- ✗ Retry backoff exponential growth verification
- ✗ Error propagation through function call stacks
- ✗ Context accumulation across nested calls
- ✗ Recovery strategy selection based on severity + recoverable flag
- ✗ Thread safety of decorators
- ✗ Performance impact of decorators on normal path
- ✗ Error serialization (needed for logging/provenance)

**Missing Real-World Scenarios:**
1. Cascading errors (error within error handler)
2. Resource cleanup on error (finally blocks in decorator)
3. Error context growth (nested function calls add context)
4. Timeout handling (no TimeoutError class)
5. Permission/authentication errors (no AuthError class)

#### D. FOUNDATIONAL GAPS

1. **No Error Aggregation Strategy:**
   - `collect_errors` collects but doesn't combine context intelligently
   - What if errors share context keys? How to merge?

2. **No Error Suppression/Silencing:**
   - Some errors should be ignorable (e.g., cache invalidation)
   - No decorator for "ignore this class of error"

3. **No Error Metadata/Tagging:**
   - Errors can't be categorized beyond domain (file, validation, parse)
   - No support for error IDs, error codes, or error versioning

4. **No Error Stack Tracing:**
   - Multiple errors don't preserve their causal chain
   - "error B caused by error A" not representable

5. **Missing: Error Recovery Callback:**
   - `with_hint()` suggests recovery, but no `on_recovery` callback
   - `@retry_on_error` retries blindly, no pre-retry hook

---

## FIRST-ORDER IMPLICATIONS (Orthogonal Test Dimensions)

### Syntactic Implication 1: **Type Safety at Boundaries**
- **Definition**: All decorator return types must match input function signature
- **Tests Needed**:
  - `handle_errors` preserves return type of wrapped function (including None)
  - `retry_on_error` preserves return type exactly
  - `collect_errors` returns list[ScriptError] or raises

### Semantic Implication 1: **Recovery Chain Completeness**
- **Definition**: Error must specify whether it's recoverable AND provide recovery strategy
- **Tests Needed**:
  - Recoverable errors can be caught and retried
  - Non-recoverable errors propagate immediately
  - Recovery strategies are actionable (hint + callback)

### Pragmatic Implication 1: **Error Context Accumulation**
- **Definition**: As error propagates through call stack, context accumulates correctly
- **Tests Needed**:
  - Inner errors add context (file → function)
  - Context keys don't collide (no silent overwrites)
  - Context can be inspected at each level

---

## SECOND-ORDER IMPLICATIONS (Orthogonal to First-Order)

### Syntactic Implication 1.1: **Decorator Composition Safety**
- **Definition**: Multiple decorators can be stacked without type conflicts
- **Tests Needed**:
  - `@retry_on_error` + `@handle_errors` on same function
  - `@collect_errors` + `@retry_on_error` interaction
  - Error suppression doesn't break retry logic

### Semantic Implication 1.1: **Error Causality Representation**
- **Definition**: Each error can express what caused it (causal chain)
- **Tests Needed**:
  - Error A caused by Error B is representable
  - Causal chain prints in diagnostic output
  - Recovery strategy can inspect cause

### Pragmatic Implication 1.1: **Performance Under Error Load**
- **Definition**: Error handling doesn't degrade performance in failure scenarios
- **Tests Needed**:
  - `collect_errors` with 1000 errors doesn't OOM
  - Retry backoff math is correct (2^n growth)
  - Context dict access is O(1)

---

## THIRD-ORDER IMPLICATIONS (Orthogonal to First & Second)

### Syntactic Implication 1.1.1: **Decorator Identity Preservation**
- **Definition**: Decorated function metadata (name, doc) preserved
- **Tests Needed**:
  - `func.__name__` preserved by decorators
  - `func.__doc__` preserved
  - `inspect.signature(func)` shows original signature

### Semantic Implication 1.1.1: **Error Equivalence Under Transformation**
- **Definition**: Two errors with same message/context but different paths are equivalent
- **Tests Needed**:
  - Error equality based on content, not identity
  - Error comparison works for logging/debugging
  - Identical errors deduplicate in aggregation

### Pragmatic Implication 1.1.1: **Error Serialization for Provenance**
- **Definition**: Errors can be serialized to JSON/YAML for traceability
- **Tests Needed**:
  - ScriptError → JSON (with context, severity, hint)
  - JSON → ScriptError (reconstruction)
  - Serialization preserves all information

---

## REQUIRED CORRECTIONS (FOUNDATIONAL)

### Priority 1: FIX TYPE SAFETY IN DECORATORS

**Current Problem:**
```python
def wrapper(*args, **kwargs) -> T:
    try:
        return f(*args, **kwargs)
    except ScriptError as e:
        if e.severity == ErrorSeverity.WARNING:
            print(f"Warning: {e}", file=sys.stderr)
            return None  # ❌ TYPE VIOLATION: should return T, not None
```

**Fix Required:**
- Make WARNING handling configurable
- Preserve return type always
- Document severity routing policy

### Priority 2: OPERATIONALIZE RECOVERY FIELD

**Current Problem:**
```python
def __init__(self, message: str, ...):
    ...
    self.recoverable: bool = False  # ❌ SET BUT NEVER USED
```

**Fix Required:**
- `@retry_on_error` should check `error.recoverable` flag
- Add recovery strategy callback
- Document recovery semantics

### Priority 3: ERROR CONTEXT SERIALIZATION

**Current Problem:**
- No way to serialize ScriptError to JSON for logging
- Provenance module can't track errors
- Context dict may contain non-serializable objects

**Fix Required:**
- Add `to_dict()` and `to_json()` methods
- Add `from_dict()` and `from_json()` class methods
- Document serialization format

---

## NEXT STEPS

1. **This Turn**: Document corrections for errors.py and logging.py
2. **Next Turn**: Implement corrections and add first-order implication tests
3. **Turn 3**: Implement second-order implications and cross-module tests
4. **Turn 4**: Implement third-order implications and provenance integration

