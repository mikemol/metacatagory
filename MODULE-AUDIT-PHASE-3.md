#!/usr/bin/env python3
"""MODULE AUDIT PHASE 3: config.py

Comprehensive audit following CHIP-N+1 recursive coherence protocol.
Three orthogonal audit dimensions, three correction priorities, three implication levels.

Source: scripts/shared/config.py (320 LOC)
Date: Session 6, Turn 3
Methodology: SPPF-modeled architecture, homological reasoning, third-order implications
"""

# ============================================================================
# SYNTACTIC AUDIT (Implementation Structure)
# ============================================================================

"""
SYNTACTIC FINDINGS:

1. ✅ WELL-FORMED: Dataclass structure clear
   - @dataclass Config with field defaults
   - Properties for computed paths (build_dir, src_dir, etc.)
   - Class methods for loading (from_env, from_file)

2. ⚠️ TYPE SAFETY ISSUE: Property paths not validated
   - Line 34-63: Properties return Path objects
   - No verification that paths exist or are valid
   - Could return Path("/nonexistent") without warning
   - Violates "fail fast" principle

3. ⚠️ SERIALIZATION GAP: to_dict() incomplete
   - Line 191-208: to_dict() exports to plain dict
   - But no from_dict() method (asymmetric API)
   - Cannot round-trip: config -> dict -> config
   - Contrast with errors.py which has both to_dict() and from_dict()

4. ✅ TYPE HINTS: Modern syntax used correctly
   - dict[str, Any] not Dict[...]
   - Path | None not Optional[Path]
   - Proper return type annotations

5. ⚠️ DECORATOR ISSUE: with_config exception handling
   - Line 307-309: Bare except catches signature inspection errors
   - Too broad - could hide real bugs
   - Should catch only expected exceptions (TypeError, ValueError)
"""

# ============================================================================
# SEMANTIC AUDIT (Conceptual Completeness)
# ============================================================================

"""
SEMANTIC FINDINGS:

ISSUE 2.1: Global State Management Has Race Conditions
  Status: FOUNDATIONAL GAP (Priority 1)
  Description: Lines 236-268 (get_config, set_config, reset_config)
    - Uses module-level global _config variable
    - Not thread-safe (no locking mechanism)
    - with_config decorator (line 296) temporarily swaps global state
    - Parallel execution could corrupt state
    - No isolation between concurrent function calls
  
  Implications:
    2.1.1 [Syntactic]: Global state violates referential transparency
    2.1.2 [Semantic]: Concurrent calls to with_config() can collide
    2.1.3 [Pragmatic]: config.parallel=True makes config system unsafe
  
  Source: Semantic Audit, Priority 1 (Axiomatic Dependencies)
  Fix Required: Add thread-local storage or context managers
  
ISSUE 2.2: Path Validation Is Missing
  Status: FOUNDATIONAL GAP (Priority 2)
  Description: 
    - Properties (build_dir, src_dir, etc.) construct paths but never verify
    - from_file() checks file existence (line 157) but not for directories
    - No validation that repo_root actually points to metacatagory repo
    - Could silently use wrong directory
  
  Implications:
    2.2.1 [Syntactic]: Path objects returned may be invalid
    2.2.2 [Semantic]: No guarantee paths point to expected structure
    2.2.3 [Pragmatic]: Errors surface late (when path is used, not when config created)
  
  Source: Semantic Audit, Priority 2 (Foundational Gaps)
  Fix Required: Add validate() method, check critical paths exist
  
ISSUE 2.3: Environment Variable Parsing Is Incomplete
  Status: SHALLOW TREATMENT (Priority 3)
  Description: from_env() (line 101-143)
    - Only parses a subset of config fields
    - No env var for json_indent, create_parents, backup_on_overwrite
    - No env var for fail_on_warning, log_structured
    - Inconsistent: some fields get env var support, others don't
  
  Implications:
    2.3.1 [Syntactic]: API incompleteness (arbitrary subset)
    2.3.2 [Semantic]: Users cannot fully configure via environment
    2.3.3 [Pragmatic]: CI/CD workflows limited (can't override all settings)
  
  Source: Pragmatic Audit, Priority 3 (Shallow Treatments)
  Fix Required: Add env vars for all config fields
  
ISSUE 2.4: Custom Dict Has No Schema
  Status: FOUNDATIONAL GAP (Priority 2)
  Description: custom: dict[str, Any] field (line 91)
    - Arbitrary key-value store
    - No schema validation
    - No type checking for values
    - from_file() puts unknown keys here (line 173)
    - Could accumulate typos or invalid data
  
  Implications:
    2.4.1 [Syntactic]: Type safety lost for custom fields
    2.4.2 [Semantic]: No contract for what belongs in custom dict
    2.4.3 [Pragmatic]: Debugging harder (typos silent)
  
  Source: Semantic Audit, Priority 2 (Foundational Gaps)
  Fix Required: Add schema validation or at least warning for unknown keys
  
ISSUE 2.5: Override Method Lacks Validation
  Status: SEMANTIC INCONSISTENCY (Priority 3)
  Description: override() method (line 178-198)
    - Accepts **kwargs without validation
    - Unknown keys go to custom dict silently
    - Could typo 'verbos' instead of 'verbose' - no error
    - No type checking on override values
  
  Implications:
    2.5.1 [Syntactic]: Type contracts bypassed
    2.5.2 [Semantic]: Silent failures from typos
    2.5.3 [Pragmatic]: Debugging difficulty (wrong behavior, no error)
  
  Source: Semantic Audit, Priority 3 (Shallow Treatments)
  Fix Required: Validate kwargs against known fields, warn on unknown
  
ISSUE 2.6: with_config Decorator Not Composable
  Status: SEMANTIC GAP (Priority 3)
  Description: with_config decorator (line 270-320)
    - Temporarily replaces global _config
    - Nesting @with_config decorators leads to unpredictable behavior
    - Restoration happens in finally block, but stack order unclear
    - No mechanism to merge nested overrides
  
  Implications:
    2.6.1 [Syntactic]: Decorator stacking semantics undefined
    2.6.2 [Semantic]: Inner decorator overwrites outer decorator settings
    2.6.3 [Pragmatic]: Composability violated
  
  Source: Semantic Audit, Priority 3 (Shallow Treatments)
  Fix Required: Use context stack for nested config scopes
"""

# ============================================================================
# PRAGMATIC AUDIT (Behavioral Correctness)
# ============================================================================

"""
PRAGMATIC FINDINGS:

TEST COVERAGE GAPS:

1. ✓ COVERED: Basic config creation and field access
   - Tests verify Config() initializes with defaults
   - Tests verify properties return Path objects
   
2. ✗ GAP: Thread safety
   - No test for concurrent get_config() calls
   - No test for parallel with_config() usage
   - No test for race conditions on global state
   
3. ✗ GAP: Path validation
   - No test for invalid repo_root
   - No test for nonexistent paths
   - No test for path validation in from_file()
   
4. ✗ GAP: Environment variable parsing
   - Tests exist for subset of env vars
   - No test for ALL config fields having env var support
   - No test for malformed env var values
   
5. ✗ GAP: Custom dict behavior
   - No test for schema validation
   - No test for unknown keys warning
   - No test for custom dict serialization round-trip
   
6. ✗ GAP: Override validation
   - No test for typo detection in override()
   - No test for type mismatch in override values
   - No test for invalid override keys
   
7. ✗ GAP: Decorator composition
   - No test for nested @with_config decorators
   - No test for decorator + manual set_config() interaction
   - No test for decorator exception handling
   
8. ✗ GAP: Serialization round-trip
   - No from_dict() method exists
   - No test for config -> dict -> config identity
   - No test for to_dict() completeness

BEHAVIORAL EXPECTATIONS:

Thread Safety Expected:
  - get_config() should be safe to call from multiple threads
  - with_config() should isolate per-thread or per-context
  - Global state should use threading.local() or contextvars

Path Validation Expected:
  - config.validate() → checks repo_root exists and is metacatagory repo
  - config.validate() → checks critical dirs exist (src/, build/)
  - Invalid paths raise FileOperationError early

Environment Variable Completeness Expected:
  - Every Config field should have METACATAGORY_* env var
  - Env vars should support all types (bool, int, Path, str)
  - Malformed env vars should log warning and use default

Override Validation Expected:
  - config.override(verbos=True) → raises ValueError (typo detected)
  - config.override(workers="not_a_number") → raises TypeError
  - config.override(unknown_key="value") → logs warning

Decorator Composition Expected:
  - @with_config(verbose=True) over @with_config(strict=False) should merge
  - Inner decorator should see outer overrides
  - Nesting should be deterministic and traceable
"""

# ============================================================================
# FOUNDATIONAL CORRECTIONS REQUIRED (Priority Order)
# ============================================================================

"""
PRIORITY 1 (Axiomatic Dependencies - Fix First):
┌─────────────────────────────────────────────────────────────────────────┐
│ 1. Add Thread-Safe Context Management                                  │
│                                                                         │
│ Current State:                                                          │
│   - Global _config variable (not thread-safe)                         │
│   - with_config() swaps global state temporarily                       │
│   - Race conditions possible with config.parallel=True                │
│                                                                         │
│ Required:                                                               │
│   - Use contextvars.ContextVar for thread-local config                │
│   - Each context gets isolated config instance                        │
│   - with_config() uses context manager protocol                        │
│   - Thread safety guaranteed                                           │
│                                                                         │
│ Implementation:                                                         │
│   - Replace _config global with _config_var = ContextVar('config')    │
│   - Update get_config() to use _config_var.get()                      │
│   - Update with_config to use _config_var.set() in context manager    │
│   - Add tests for concurrent access                                    │
│                                                                         │
│ Implication:                                                           │
│   - 2.1.1 resolved: Context-local state preserves transparency        │
│   - 2.1.2 resolved: No collision between concurrent calls             │
│   - 2.1.3 resolved: Parallel execution safe                           │
└─────────────────────────────────────────────────────────────────────────┘

PRIORITY 2 (Foundational Gaps - Fix Second):
┌─────────────────────────────────────────────────────────────────────────┐
│ 2. Add Path Validation and Serialization Symmetry                      │
│                                                                         │
│ Current State:                                                          │
│   - Paths constructed but never validated                              │
│   - to_dict() exists but no from_dict()                               │
│   - No guarantee paths point to valid locations                        │
│                                                                         │
│ Required:                                                               │
│   - Add validate() method checking critical paths                      │
│   - Add from_dict() for serialization symmetry                        │
│   - Check repo_root contains expected structure (.git/, src/, etc.)   │
│   - Validation optional (flag) but recommended                         │
│                                                                         │
│ Implementation:                                                         │
│   - Add validate(strict: bool = False) method                         │
│   - Check repo_root.exists() and repo_root / ".git" exists            │
│   - Check src_dir, build_dir exist or are creatable                   │
│   - Add from_dict(data: dict) class method                            │
│   - Ensure to_dict() -> from_dict() round-trip identity               │
│                                                                         │
│ Implication:                                                           │
│   - 2.2.1 resolved: Paths guaranteed valid after validate()           │
│   - 2.2.2 resolved: Structural integrity verified                     │
│   - 2.2.3 resolved: Errors surface early                              │
│   - Serialization symmetric like errors.py                            │
└─────────────────────────────────────────────────────────────────────────┘

PRIORITY 3 (Shallow Treatments - Fix Third):
┌─────────────────────────────────────────────────────────────────────────┐
│ 3. Complete Environment Variable Support & Override Validation         │
│                                                                         │
│ Current State:                                                          │
│   - Only subset of fields have env var support                         │
│   - override() accepts any kwargs without validation                   │
│   - Typos silent, type errors silent                                   │
│                                                                         │
│ Required:                                                               │
│   - Add env vars for ALL Config fields                                │
│   - Validate override() kwargs against known fields                    │
│   - Warn on unknown keys (or raise in strict mode)                    │
│   - Type-check override values                                         │
│                                                                         │
│ Implementation:                                                         │
│   - Extend from_env() to parse all fields                             │
│   - Add METACATAGORY_JSON_INDENT, _CREATE_PARENTS, etc.               │
│   - Add _validate_override_key() helper                                │
│   - In override(), check key in __dataclass_fields__                  │
│   - Warn or raise on unknown keys                                      │
│   - Add type checking using field type annotations                    │
│                                                                         │
│ Implication:                                                           │
│   - 2.3.1 resolved: Complete env var API                              │
│   - 2.3.2 resolved: Full CI/CD configurability                        │
│   - 2.5.1 resolved: Type contracts enforced                           │
│   - 2.5.2 resolved: Typos detected                                     │
└─────────────────────────────────────────────────────────────────────────┘

SOURCE TRACEABILITY:

Issue 2.1: Found at line 236-268, 296 via semantic audit (thread safety)
Issue 2.2: Found at line 34-63 via semantic audit (path validation)
Issue 2.3: Found at line 101-143 via pragmatic audit (env var completeness)
Issue 2.4: Found at line 91, 173 via semantic audit (custom dict schema)
Issue 2.5: Found at line 178-198 via semantic audit (override validation)
Issue 2.6: Found at line 270-320 via semantic audit (decorator composition)

All issues traced to gaps between:
  - Abstract specification (config should be thread-safe, validated, complete)
  - Implementation (global state, missing validation, partial env var support)
  - Integration (with errors/logging modules for provenance)
"""

# ============================================================================
# HOMOLOGICAL IMPLICATIONS (Three Levels of Orthogonal Reasoning)
# ============================================================================

"""
FIRST-ORDER IMPLICATIONS (Operator Level - Immediate Behaviors)

1. Implication 1.1: Context-Local Configuration
   ├─ 1.1.1 [Syntactic]: Use contextvars.ContextVar for thread isolation
   ├─ 1.1.2 [Semantic]: Each async task/thread gets independent config
   └─ 1.1.3 [Pragmatic]: No locks needed (copy-on-write semantics)

2. Implication 1.2: Path Validation Contract
   ├─ 1.2.1 [Syntactic]: validate() returns self for chaining
   ├─ 1.2.2 [Semantic]: Validated config guaranteed to have valid paths
   └─ 1.2.3 [Pragmatic]: Fail-fast behavior (errors at config time, not use time)

3. Implication 1.3: Serialization Symmetry
   ├─ 1.3.1 [Syntactic]: from_dict(config.to_dict()) == config (round-trip)
   ├─ 1.3.2 [Semantic]: All config state preservable
   └─ 1.3.3 [Pragmatic]: Can serialize config to provenance logs


SECOND-ORDER IMPLICATIONS (Interaction Level - Compositional Behavior)

1. Implication 2.1: Config-Error Integration
   ├─ 2.1.1 [Syntactic]: Validation errors use FileOperationError from errors.py
   ├─ 2.1.2 [Semantic]: Config errors serializable via .to_dict()
   └─ 2.1.3 [Pragmatic]: Provenance can log config validation failures

2. Implication 2.2: Config-Logging Integration
   ├─ 2.2.1 [Syntactic]: config.log_level feeds configure_logging()
   ├─ 2.2.2 [Semantic]: Changing config.verbose updates logger.level
   └─ 2.2.3 [Pragmatic]: with_config(verbose=True) auto-updates logging

3. Implication 2.3: Override Composition
   ├─ 2.3.1 [Syntactic]: config.override(...).override(...) chains
   ├─ 2.3.2 [Semantic]: Later overrides take priority over earlier
   └─ 2.3.3 [Pragmatic]: Nested with_config() merges overrides


THIRD-ORDER IMPLICATIONS (System Level - Recursive Verification)

1. Implication 3.1: Config Provenance
   ├─ 3.1.1 [Syntactic]: Config changes logged to provenance system
   ├─ 3.1.2 [Semantic]: Can replay execution with original config
   └─ 3.1.3 [Pragmatic]: Reproducible builds from config snapshots

2. Implication 3.2: Config as Aspect
   ├─ 3.2.1 [Syntactic]: Config transparent to application logic
   ├─ 3.2.2 [Semantic]: with_config() doesn't affect return types
   └─ 3.2.3 [Pragmatic]: Config injection doesn't pollute function signatures

3. Implication 3.3: Validation Composability
   ├─ 3.3.1 [Syntactic]: validate() can be extended with custom validators
   ├─ 3.3.2 [Semantic]: Validators compose (all-must-pass or any-must-pass)
   └─ 3.3.3 [Pragmatic]: Script-specific validation rules pluggable

SOURCE: Derived from CHIP-N+1 recursive coherence protocol
        Applied to config.py via third-order homological expansion
"""

# ============================================================================
# SUMMARY
# ============================================================================

"""
AUDIT SUMMARY FOR config.py:

Status: REQUIRES CORRECTIONS (Priority 1-3)
LOC: 320 LOC current, estimate 420-450 LOC after enhancements
Complexity: Medium-High (global state + decorator + serialization)

Gap Count by Priority:
  - Priority 1 (Axiomatic): 1 gap (thread safety)
  - Priority 2 (Foundational): 2 gaps (path validation, serialization symmetry)
  - Priority 3 (Shallow): 3 gaps (env var completeness, override validation, decorator composition)
  Total: 6 foundational gaps

Test Coverage Gaps: 8 major gaps identified
  - Thread safety / concurrent access
  - Path validation
  - Environment variable completeness
  - Custom dict schema
  - Override validation
  - Decorator composition
  - Serialization round-trip
  - Integration with errors/logging

Implications Defined: 9 total (3+3+3)
  - First-order: 3 implications (context-local, validation, serialization)
  - Second-order: 3 implications (error integration, logging integration, override composition)
  - Third-order: 3 implications (provenance, aspect weaving, validation composability)

Recommended Corrections (in priority order):
  1. Add contextvars for thread-safe config (resolves 2.1)
  2. Add validate() and from_dict() methods (resolves 2.2)
  3. Complete env var support and override validation (resolves 2.3, 2.5)
  4. Create test suite with 25-30 tests
  5. Verify integration with errors.py and logging.py

Expected Outcome:
  - config.py thread-safe for parallel execution
  - Path validation prevents silent failures
  - Full serialization symmetry like errors.py
  - Complete env var support for CI/CD
  - All 9 implications testable and verifiable
"""
