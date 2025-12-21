# Roadmap & Refactoring Work Queue

## Context

After Agda 2.8.0 upgrade and full rebuild (140/157 files compiled, 89% complete):

* Plan.CIM.Utility.agda: clean core version (148 lines), compiles with --safe
* CHIPConformance.agda: fixed `composeBraids`, all stubs compile
* Utility-broken.agda: preserved in repo root with 77 machine-actionable roadmap examples (~93KB)
* ROADMAP.md, DEFERRED-TRACKING.md: empty (deferred-summary.json tracks 567 items)

## Non-Critical Refactoring (WAL Entry)

### 1. Recover & Integrate Roadmap Data

* **File**: Utility-broken.agda (lines ~23–2000)
* **Task**: 77 example roadmaps (exampleCompositionalityRoadmap through exampleFuzzyGhostRoadmap)
* **Status**: Syntax corrupted from earlier regex processing ([ ∷ [] patterns, stray ])
* **Action**: Manual cleanup of list syntax, then append to Plan.CIM.Utility.agda
* **Reference**: Plan.CIM.Utility.agda:35 (RoadmapStep record definition)
* **Blocked by**: Build completion (critical first)

### 2. Populate ROADMAP.md

* **File**: ROADMAP.md (currently empty)
* **Task**: Add entries for:

  * Utility-broken.agda recovery   * CHIPConformance.agda stubs (makeSPPFNode, richer adjunction)   * composeBraids semantic refinement (optional)   * Cross-reference deferred-summary.json (567 items)

* **Status**: Awaiting WAL initialization
* **Reference**: DEFERRED-TRACKING.md (also empty)

### 3. CHIPConformance Stubs

* **File**: src/agda/Plan/CIM/CHIPConformance.agda:42–43
* **Task**: Implement makeSPPFNode with BraidedSPPF type (currently commented)
* **Status**: Stub placeholder only
* **Dependency**: BraidedSPPF not yet in Utility.agda
* **Reference**: Plan.CIM.Utility.agda (add BraidedSPPF record if needed)

### 4. composeBraids Refinement (Optional)

* **File**: src/agda/Plan/CIM/CHIPConformance.agda:34–41
* **Current**: Simple swap `(a, c) → (c, a)`, sum costs
* **Task**: Encode proper braid composition using inheritanceBraid semantics
* **Status**: Functional but minimalist
* **Blocked by**: Design clarity on braid semantics (currently deferred)

## Cross-References

* Utility.agda: RoadmapStep (line 35), ASTDependent module (line 104)
* CHIPConformance.agda: composeBraids (line 34), makeSPPFNode (line 42)
* Utility-broken.agda: 77 example roadmaps (lines ~23–2000)
* deferred-summary.json: 567 total items, 351 postulates, 155 TODOs

## Build Priority

* **Current**: 140/157 files compiled (89%)
* **Next**: Complete full rebuild (17 files remaining)
* **Then**: Address refactoring items above in order

---

Last updated: 2025-12-20 after Agda 2.8.0 compatibility pass
