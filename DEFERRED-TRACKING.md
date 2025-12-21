# Deferred Items Tracking

## Overview

Tracks deferred work items identified during the Agda 2.8.0 migration and refactoring efforts. Counts reflect deferred-summary.json snapshots.

Source: deferred-summary.json (auto-generated)

* Totals: 567 items
* Postulates: 351
* TODOs: 155
* Other markers (FIXME/HACK/etc.): 61

## Tracking Strategy

### High-Priority Deferred Items

Items blocking core functionality or Plan.CIM completion.

* [ ] CHIPCoreRecompose.agda (8-line stub); blocked by Core re-enable
  Effort: 100-150 lines; Timeline: after Core restored; Ref: ROADMAP.md Phase 2.1
* [ ] PandocProtocols.agda stubs (blockAmb, blockTransSys, blockCoherence, docAmb, docTransSys, docCoherence)
  Effort: 50-100 lines semantic impls; Ref: ROADMAP.md Phase 2.3
* [ ] CHIPConformance.agda stubs (makeSPPFNode, composeBraids refinement)
  Effort: 50-80 lines; Ref: ROADMAP.md Phase 2.3
* [ ] Core.disabled/ modules (23 modules)
  Phase theory, categorical foundations, adjunction/limits; Effort: 1-2 weeks; Ref: ROADMAP.md Phase 3.2
* [ ] Algebra.disabled/ subset (for Core deps)
  Groups/rings/modules/fields; Effort: 3-5 days; Ref: ROADMAP.md Phase 3.2

### Medium-Priority Deferred Items

Improves functionality but not blocking major milestones.

* [ ] Examples.disabled/ modules (13 modules)
  TechnicalDebtRegistry and demonstration examples; Effort: 1-3 days/module; Ref: ROADMAP.md Phase 3.1
* [ ] Plan.CIM integration tests
  E2E transformation pipeline, grammar conformance, proof export; Effort: 100-150 lines; Ref: ROADMAP.md Phase 2.2
* [ ] Plan.CIM documentation
  Architecture/API/tutorials; Effort: 1-2 days; Ref: ROADMAP.md Phase 4.1

### Low-Priority Deferred Items

Nice-to-have improvements, cleanup, or exploratory work.

* [ ] Utility-broken.agda recovery
  Extract 77 roadmap examples; fix syntax; merge into Plan.CIM.Utility.agda; Effort: 4-6 hours; Ref: intake/refactoring-roadmap-migration.md
* [ ] Script modernization
  Python 3.10+ features, type hints, error handling; Effort: 2-3 days; Ref: scripts/
* [ ] CHIP protocol semantic enrichment
  Ambiguity detection, transformation metrics, coherence validation; Research 1-2 weeks; Deferred to ROADMAP-DRAFT.md

## Deferred Item Sources

### By Module Category

```text
Core.disabled/          : ~80 postulates, ~30 TODOs
Algebra.disabled/       : ~45 postulates, ~20 TODOs
Examples.disabled/      : ~25 postulates, ~15 TODOs
Plan.CIM/               : ~15 TODOs (stubs)
Scripts/                : ~40 TODOs (improvements)
Documentation/          : ~50 TODOs (missing sections)
Other                   : ~106 postulates, ~35 TODOs
```

### By Type

```text
Postulates (351):
  - Unproven theorems awaiting formal proofs
  - Placeholder functions for disabled modules
  - Abstract interface requirements

TODOs (155):
  - Code improvements and refactoring
  - Missing implementations
  - Documentation gaps

Other markers (61):
  - FIXME: Known bugs or issues
  - HACK: Temporary workarounds
  - NOTE: Important context or warnings
```

## Tracking Process

### Adding New Deferred Items

* Add inline marker: -- TODO: [category] description or postulate name : Type
* Run make deferred-items to refresh deferred-summary.json
* If high/medium priority, add to sections above
* Reference ROADMAP.md if it blocks a phase

### Resolving Deferred Items

* Remove marker or replace postulate with implementation
* Run make deferred-items to refresh counts
* Check off item in this document
* Update ROADMAP.md if a milestone is unblocked

### Periodic Review

* Weekly: review high-priority items and blockers
* Monthly: reprioritize medium/low items
* Per milestone: regenerate deferred-summary.json and verify counts

## Tools

### Generate Deferred Summary

```text
make deferred-items
# Outputs: deferred-summary.json
```

### Search for Deferred Items

```text
# TODOs
grep -r "TODO" src/agda/

# Postulates
grep -r "postulate" src/agda/

# All markers
grep -rE "TODO|FIXME|HACK|XXX|NOTE" src/agda/
```

### Flag Unannotated Technical Debt

```text
make Flag Unannotated Technical Debt
# Or: python3 scripts/flag_unannotated_debt.py
```

## Cross-References

* Primary roadmap: ROADMAP.md
* Worklog: intake/refactoring-roadmap-migration.md
* Generated data: deferred-summary.json (gitignored)
* Build tasks: .vscode/tasks.json

## Status Snapshot

Last updated: 2025-12-20; Next review: after Phase 1 cleanup (ROADMAP.md); Totals: 567 (351 postulates, 155 TODOs, 61 other)
