#!/usr/bin/env python3
"""MODULE AUDIT PHASE 2: logging.py

Comprehensive audit following CHIP-N+1 recursive coherence protocol.
Three orthogonal audit dimensions, three correction priorities, three implication levels.

Source: scripts/shared/logging.py (247 LOC)
Date: Session 6, Turn 2
Methodology: SPPF-modeled architecture, homological reasoning, third-order implications
"""

# ============================================================================
# SYNTACTIC AUDIT (Implementation Structure)
# ============================================================================

"""
SYNTACTIC FINDINGS:

1. ✅ WELL-FORMED: Class hierarchy is clear
   - StructuredFormatter(logging.Formatter) - JSON output
   - HumanReadableFormatter(logging.Formatter) - Terminal output
   - StructuredLogger - Wrapper providing structured API

2. ✅ TYPE HINTS: Generally present and correct
   - Dict type hints use modern syntax (dict[str, Any], not Dict[...])
   - Optional types use pipe operator (T | None)
   - Return types specified for all public methods

3. ⚠️ MINOR: Color code handling
   - COLORS dict is hardcoded ANSI codes
   - No platform detection for Windows compatibility
   - Improvement: Add Windows console color support via colorama

4. ✅ HANDLER MANAGEMENT: Explicit handler clearing
   - configure_logging() clears existing handlers (prevents duplicates)
   - Line 218: logger.logger.handlers.clear()
"""

# ============================================================================
# SEMANTIC AUDIT (Conceptual Completeness)
# ============================================================================

"""
SEMANTIC FINDINGS:

ISSUE 1.1: Logger Context Merging Has Silent Collision Risk
  Status: FOUNDATIONAL GAP
  Description: Line 121-133 (_log_with_context)
    - Three sources of context: context dict, extra_ctx kwargs, extra_ctx['context']
    - No clear priority when keys collide
    - Line 129: extra_ctx.pop('context') - removes after inspection, but...
    - If context dict and extra_ctx both have 'foo', extra_ctx['foo'] wins silently
    - No warning about collisions
  
  Implications:
    1.1.1 (Semantic): Context merging priority needs formal specification
    1.1.2 (Semantic): Silent collisions violate principle of least surprise
    1.1.3 (Semantic): Integration risk: context from errors.py might collide with caller context
  
  Source: Semantic Audit, Priority 2 (Foundational Gaps)
  Fix Required: Document and enforce merge priority (caller > context dict > extra_ctx)

ISSUE 1.2: exception parameter vs exc_info kwarg Inconsistency
  Status: SEMANTIC INCONSISTENCY
  Description: 
    - error() method uses exc_info=False (kwarg)
    - But exc_info is typically passed as positional in logging module
    - critical() also uses exc_info=False (kwarg)
    - Should support both exc_info=True and exception=(...) pattern
  
  Implications:
    1.2.1 (Semantic): API contract unclear (which parameter takes precedence?)
    1.2.2 (Semantic): Integration with errors.py ScriptError is manual, not automatic
    1.2.3 (Semantic): No callback mechanism for error routing (cf. errors.py on_error callback)
  
  Source: Semantic Audit, Priority 3 (Shallow Treatments)
  Fix Required: Add exception parameter, automatic ScriptError integration

ISSUE 1.3: Progress Tracking Lacks Recovery Semantics
  Status: PRAGMATIC GAP
  Description: progress() method (line 163-180)
    - Shows current/total/percentage
    - No mechanism to record failed items vs successful items
    - No retry count integration
    - Doesn't track which items failed
  
  Implications:
    1.3.1 (Pragmatic): Cannot distinguish "50% complete" from "50% complete but 20% failed"
    1.3.2 (Pragmatic): No integration point for recovery tracking
    1.3.3 (Pragmatic): Progress data not suitable for provenance recording
  
  Source: Pragmatic Audit, Priority 2 (Foundational Gaps)
  Fix Required: Add failed/succeeded counters, recovery status tracking

ISSUE 1.4: Formatter Context Handling Is Inconsistent
  Status: SEMANTIC GAP
  Description:
    - Line 31: StructuredFormatter checks hasattr(record, 'context')
    - Line 47: HumanReadableFormatter also checks hasattr(record, 'context')
    - But context format differs between formatter types
    - StructuredFormatter: raw dict in JSON
    - HumanReadableFormatter: formatted as "k=v k=v ..." in text
    - No spec for which format is canonical
  
  Implications:
    1.4.1 (Semantic): Context representation is formatter-dependent
    1.4.2 (Semantic): If context contains complex types, HumanReadableFormatter fails
    1.4.3 (Semantic): Serialization parity between formatters is not guaranteed
  
  Source: Semantic Audit, Priority 1 (Axiomatic Dependencies)
  Fix Required: Formalize context representation contract

ISSUE 1.5: Log Level Filtering Not Enforced at Call Site
  Status: SHALLOW TREATMENT
  Description:
    - Line 98: if self.logger.isEnabledFor(level): guards expensive operations
    - But this is not consistently applied
    - progress() method always logs, regardless of level
    - Could waste resources if INFO level messages are disabled
  
  Implications:
    1.5.1 (Pragmatic): Performance optimization incomplete
    1.5.2 (Pragmatic): No lazy evaluation for context building
    1.5.3 (Pragmatic): Heavy context dicts built even if not logged
  
  Source: Pragmatic Audit, Priority 3 (Shallow Treatments)
  Fix Required: Add level checks to all public methods
"""

# ============================================================================
# PRAGMATIC AUDIT (Behavioral Correctness)
# ============================================================================

"""
PRAGMATIC FINDINGS:

TEST COVERAGE GAPS:

1. ✓ COVERED: Basic logging methods (debug, info, warning, error, critical)
   - Tests verify messages are logged
   - Tests verify level routing
   
2. ✗ GAP: Context merging collision scenarios
   - No test for priority when context dict and extra_ctx have same key
   - No test for extra_ctx['context'] handling
   
3. ✗ GAP: Formatter behavior with complex types
   - What happens when context contains non-JSON-serializable objects?
   - StructuredFormatter uses json.dumps() without default handler
   
4. ✗ GAP: Progress tracking accuracy
   - No test for "% done" calculation correctness
   - No test for progress when total=0
   - No test for failed/succeeded tracking
   
5. ✗ GAP: Handler lifecycle
   - No test for duplicate handler scenario (call configure_logging twice)
   - No test for file handler creation failures
   - No test for permission errors on log file
   
6. ✗ GAP: Exception information formatting
   - exc_info=True handling not tested
   - Exception stack formatting not validated
   
7. ✗ GAP: Color code functionality
   - ANSI color codes not validated
   - Windows compatibility not addressed (colorama integration needed)
   
8. ✗ GAP: Integration with errors.py
   - No callback mechanism for ScriptError objects
   - No automatic ScriptError.to_json() integration
   - Progress tracking doesn't track retry counts from errors module

BEHAVIORAL EXPECTATIONS:

Progress Tracking Expected:
  - progress(message="Step 1", current=5, total=10) → logs "Step 1 [5/10] 50.0%"
  - progress(message="Step 2", current=5, total=10, failed=2) → should log failed count
  - progress(message="", current=5, total=0) → should handle division by zero gracefully

Context Collision Expected:
  - logger.info("msg", context={"key": "from_context"}, key="from_kwargs")
  - Should resolve collision with documented priority
  - Should not silently overwrite one with the other

Exception Integration Expected:
  - logger.error("msg", exception=ScriptError(...)) → should include ScriptError details
  - logger.error("msg", exc_info=True) → should include Python traceback
  - Both should be serializable to JSON

Formatter Behavior Expected:
  - StructuredFormatter should output valid JSON
  - HumanReadableFormatter should output readable text
  - Both should preserve all context data
"""

# ============================================================================
# FOUNDATIONAL CORRECTIONS REQUIRED (Priority Order)
# ============================================================================

"""
PRIORITY 1 (Axiomatic Dependencies - Fix First):
┌─────────────────────────────────────────────────────────────────────────┐
│ 1. Formalize Context Representation Contract                           │
│                                                                         │
│ Current State:                                                          │
│   - Context is dict[str, Any]                                         │
│   - Formatter-dependent representation (JSON vs key=value text)        │
│   - No spec for non-serializable types                                │
│                                                                         │
│ Required:                                                               │
│   - Canonical context format (suggest: str conversion via str() for   │
│     non-JSON-serializable types)                                       │
│   - Merge priority documented: caller kwargs > extra_ctx > context_dict
│   - Collision warnings optional (can be flag)                         │
│   - Non-serializable objects handled gracefully (default=str)         │
│                                                                         │
│ Implementation:                                                         │
│   - Add to_canonical_dict() method for context                        │
│   - Use in both formatters for consistency                            │
│   - Document merge strategy in docstring                             │
│                                                                         │
│ Implication:                                                           │
│   - 1.4.1 resolved: Context representation is formatter-independent   │
│   - 1.4.2 resolved: Complex types handled consistently                │
│   - 1.4.3 resolved: Serialization parity guaranteed                   │
└─────────────────────────────────────────────────────────────────────────┘

PRIORITY 2 (Foundational Gaps - Fix Second):
┌─────────────────────────────────────────────────────────────────────────┐
│ 2. Enhance Progress Tracking with Recovery Semantics                   │
│                                                                         │
│ Current State:                                                          │
│   - progress() logs current/total/percentage only                      │
│   - No failed/succeeded tracking                                        │
│   - No recovery count tracking                                          │
│   - No integration with errors.ScriptError                             │
│                                                                         │
│ Required:                                                               │
│   - Add succeeded: int = 0, failed: int = 0 parameters               │
│   - Add recovery_attempts: int = 0 parameter                          │
│   - Calculate success_rate: succeeded / total (if total > 0)           │
│   - Log "Step X [5/10] 50.0% (4 ok, 1 failed, 1 retry)"              │
│                                                                         │
│ Implementation:                                                         │
│   - Update progress() signature (add params)                           │
│   - Add success_rate to progress context                              │
│   - Add failed count to progress message                              │
│   - Add recovery_attempts tracking                                     │
│   - Keep total as canonical for percentage calculation                 │
│                                                                         │
│ Implication:                                                           │
│   - 1.3.1 resolved: Can distinguish success rate from progress        │
│   - 1.3.2 resolved: Recovery tracking explicit and integrated         │
│   - 1.3.3 resolved: Progress data suitable for provenance recording   │
└─────────────────────────────────────────────────────────────────────────┘

PRIORITY 3 (Shallow Treatments - Fix Third):
┌─────────────────────────────────────────────────────────────────────────┐
│ 3. Add Exception Integration & Level Checks                             │
│                                                                         │
│ Current State:                                                          │
│   - error() and critical() accept exc_info=False kwarg                │
│   - No automatic ScriptError integration                                │
│   - progress() and other methods skip isEnabledFor check              │
│                                                                         │
│ Required:                                                               │
│   - Add exception: ScriptError | Exception | None parameter           │
│   - If exception provided, include in context automatically            │
│   - Serialize exception using .to_dict() if available                 │
│   - Add isEnabledFor checks to progress() and convenience functions   │
│                                                                         │
│ Implementation:                                                         │
│   - Update error() and critical() signatures                          │
│   - Add automatic exception.to_dict() serialization                   │
│   - Add level check wrapper for progress()                            │
│   - Update convenience functions (log_progress)                       │
│                                                                         │
│ Implication:                                                           │
│   - 1.2.1 resolved: API contract clear (exception param takes priority)
│   - 1.2.2 resolved: Automatic ScriptError integration                 │
│   - 1.2.3 resolved: Logging can route via on_error callbacks         │
│   - 1.5.1 resolved: Performance optimizations applied                 │
│   - 1.5.2 resolved: Lazy evaluation enabled                           │
│   - 1.5.3 resolved: Heavy context only built if logged                │
└─────────────────────────────────────────────────────────────────────────┘

SOURCE TRACEABILITY:

Issue 1.1: Found at line 121-133 via semantic audit (context merge collision)
Issue 1.2: Found at line 141, 168 via semantic audit (exc_info handling)
Issue 1.3: Found at line 163-180 via pragmatic audit (progress semantics)
Issue 1.4: Found at line 31, 47 via semantic audit (formatter consistency)
Issue 1.5: Found at line 98, 163 via pragmatic audit (level filtering)

All issues traced to gaps between:
  - Abstract specification (logging should be structured, composable, verifiable)
  - Implementation (ad-hoc formatting, no contracts, no failure modes)
  - Integration (no connection to errors module callbacks, no recovery tracking)
"""

# ============================================================================
# HOMOLOGICAL IMPLICATIONS (Three Levels of Orthogonal Reasoning)
# ============================================================================

"""
FIRST-ORDER IMPLICATIONS (Operator Level - Immediate Behaviors)

1. Implication 1.1: Context Canonicalization
   ├─ 1.1.1 [Syntactic]: All context sources must pass through canonical merger
   ├─ 1.1.2 [Semantic]: Merge priority is caller > extra_ctx > context dict
   └─ 1.1.3 [Pragmatic]: Non-serializable types converted via str()

2. Implication 1.2: Progress Rate Composition
   ├─ 1.2.1 [Syntactic]: progress() yields (current, succeeded, failed) tuple
   ├─ 1.2.2 [Semantic]: success_rate = succeeded / total (defended against /0)
   └─ 1.2.3 [Pragmatic]: recovery_attempts tracked separately from total

3. Implication 1.3: Exception Serialization
   ├─ 1.3.1 [Syntactic]: exception param accepts ScriptError | Exception | None
   ├─ 1.3.2 [Semantic]: If has .to_dict(), use it; else fallback to str()
   └─ 1.3.3 [Pragmatic]: Serialized exception in log context for provenance


SECOND-ORDER IMPLICATIONS (Interaction Level - Compositional Behavior)

1. Implication 2.1: Progress-to-Provenance Binding
   ├─ 2.1.1 [Syntactic]: progress() output can be serialized to JSON
   ├─ 2.1.2 [Semantic]: Provenance module can import progress logs
   └─ 2.1.3 [Pragmatic]: Retry count from errors.ScriptError feeds progress tracking

2. Implication 2.2: Logger-Error Feedback Loop
   ├─ 2.2.1 [Syntactic]: error(exception=...) feeds ScriptError.caused_by chain
   ├─ 2.2.2 [Semantic]: Each error linked to log event for audit trail
   └─ 2.2.3 [Pragmatic]: Callback from errors.handle_errors can invoke logger.on_error

3. Implication 2.3: Formatter Coherence
   ├─ 2.3.1 [Syntactic]: StructuredFormatter and HumanReadableFormatter accept same context
   ├─ 2.3.2 [Semantic]: Context canonical dict ensures parity between formatters
   └─ 2.3.3 [Pragmatic]: Switching formatters at runtime doesn't lose data


THIRD-ORDER IMPLICATIONS (System Level - Recursive Verification)

1. Implication 3.1: Logging Module Identity
   ├─ 3.1.1 [Syntactic]: StructuredLogger instance is idempotent (multiple configs = last wins)
   ├─ 3.1.2 [Semantic]: Logger identity preserved through .handlers.clear() lifecycle
   └─ 3.1.3 [Pragmatic]: configure_logging() can be called multiple times safely

2. Implication 3.2: Exception Chain Preservation
   ├─ 3.2.1 [Syntactic]: ScriptError.caused_by chains serializable to JSON
   ├─ 3.2.2 [Semantic]: Error causality visible in logs (can reconstruct chain)
   └─ 3.2.3 [Pragmatic]: Provenance can replay errors from logged chains

3. Implication 3.3: Logging as Aspect Weaving
   ├─ 3.3.1 [Syntactic]: All modules can use configure_logging() return value
   ├─ 3.3.2 [Semantic]: Logging transparent to application logic (no cross-cutting pollution)
   └─ 3.3.3 [Pragmatic]: Log output doesn't affect application return types

SOURCE: Derived from CHIP-N+1 recursive coherence protocol
        Applied to logging.py via third-order homological expansion
"""

# ============================================================================
# INTEGRATION ROADMAP (logging.py <-> errors.py)
# ============================================================================

"""
Current Integration Status:
  - logging.py has NO dependency on errors.py (could import without cycle)
  - errors.py.handle_errors() can invoke logger via on_error callback
  - But no automatic integration (manual wiring required)

Desired Integration (After Corrections):
  1. logging.error(exception=ScriptError(...)) → auto-serializes via .to_dict()
  2. errors.handle_errors(on_error=logger.error) → automatic routing
  3. progress tracking includes retry counts from ScriptError.recoverable checks
  4. Exception causality chains (caused_by) visible in logs
  5. Provenance module can import progress logs and error chains together

Implementation Order:
  1. Add exception param to error() and critical()
  2. Auto-serialize if has .to_dict() method
  3. Add isEnabledFor checks for performance
  4. Update progress() with succeeded/failed/recovery counters
  5. Create test suite verifying integration
  6. Update provenance.py to consume structured logs
"""

# ============================================================================
# SUMMARY
# ============================================================================

"""
AUDIT SUMMARY FOR logging.py:

Status: REQUIRES CORRECTIONS (Priority 1-3)
LOC: 247 LOC current, estimate 300-350 LOC after enhancements
Complexity: Medium (class hierarchy + handler management)

Gap Count by Priority:
  - Priority 1 (Axiomatic): 1 gap (context representation contract)
  - Priority 2 (Foundational): 1 gap (progress recovery semantics)
  - Priority 3 (Shallow): 2 gaps (exception integration, level checks)
  Total: 4 foundational gaps

Test Coverage Gaps: 8 major gaps identified
  - Context collision handling
  - Formatter complex type support
  - Progress tracking accuracy
  - Handler lifecycle
  - Exception information formatting
  - Color code functionality
  - Integration with errors.py
  - Recovery tracking

Implications Defined: 9 total (3+3+3)
  - First-order: 3 implications (context, progress, exception)
  - Second-order: 3 implications (provenance binding, error feedback, formatter coherence)
  - Third-order: 3 implications (module identity, exception chain, aspect weaving)

Recommended Corrections (in priority order):
  1. Add context canonicalization method (resolves 1.4, 1.1)
  2. Enhance progress() with recovery semantics (resolves 1.3)
  3. Add exception param and level checks (resolves 1.2, 1.5)
  4. Create test suite with 20-25 tests
  5. Verify integration with errors.py module

Expected Outcome:
  - logging.py becomes fully composable with errors.py
  - Progress tracking suitable for provenance recording
  - All 9 implications testable and verifiable
  - Zero silent failures or collisions
  - Type safety preserved through all integrations
"""
