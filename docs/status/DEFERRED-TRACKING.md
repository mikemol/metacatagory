# Deferred Items Tracking

## Overview

Tracks deferred work items identified during the Agda 2.8.0 migration and refactoring efforts. Counts reflect build/reports/deferred-summary.json snapshots.

Source: build/reports/deferred-summary.json (auto-generated)

- Totals: 567 items

- Postulates: 351

- TODOs: 155

- Other markers (FIXME/HACK/etc.): 61

Note: `docs/status/deferred-items.md` is a separate generated snapshot from a
different scan; counts can diverge. This file is the canonical tracker for
decision-making.

## Tracking Strategy

### High-Priority Deferred Items

Items blocking core functionality or Plan.CIM completion.

- [ ] CHIPCoreRecompose.agda (8-line stub); blocked by Core re-enable

Effort: 100-150 lines; Timeline: after Core restored; Ref: ROADMAP.md Phase 2.1

- [ ] PandocProtocols.agda stubs (blockAmb, blockTransSys, blockCoherence, docAmb, docTransSys, docCoherence)

Effort: 50-100 lines semantic impls; Ref: ROADMAP.md Phase 2.3

- [ ] CHIPConformance.agda stubs (makeSPPFNode, composeBraids refinement)

Effort: 50-80 lines; Ref: ROADMAP.md Phase 2.3

- [ ] Core deferred modules (no Core.disabled/ directory; see deferred summary)

Phase theory, categorical foundations, adjunction/limits; Effort: 1-2 weeks; Ref: ROADMAP.md Phase 3.2

- [ ] Algebra deferred subset (for Core deps; see deferred summary)

Groups/rings/modules/fields; Effort: 3-5 days; Ref: ROADMAP.md Phase 3.2

### Core/Algebra Cleanup Plan (Phase 3.2)

Goal: restore Core and Algebra modules so Plan.CIM can rely on real
definitions rather than postulates.

1) Inventory
   - Re-run `make deferred-items` and extract Core/Algebra buckets from
     `build/reports/deferred-summary.json`.
   - Produce a short module list grouped by directory (Core, Algebra/*).

2) Dependency layering
   - Identify the minimal Core base (Phase, PhaseCategory, BraidTree,
     UniversalProperties).
   - Stage dependent modules (limits/adjunctions, fibrations, algorithms).

3) Core re-enable pass
   - Replace postulates with constructive definitions where possible.
   - Convert placeholders into TODOs with explicit types if proofs are
     deferred to later phases.

4) Algebra re-enable pass
   - Start with Types/Basic modules (Groups, Rings, Fields, Modules).
   - Then fill in properties/instances that rely on Core proofs.

5) Verification
   - Run `make agda-all` and `make agda-test` after each batch.
   - Clear ModulesChecklist warnings once record shapes stabilize.

6) Tracking updates
   - Update this file with cleared items and new TODO counts.
   - Add a short status note to `docs/status/PROJECT-STATUS.md`.

### Medium-Priority Deferred Items

Improves functionality but not blocking major milestones.

- [ ] ModulesChecklist record literals include non-existent `homomorphism` fields
  - Agda warning: `TooManyFields` in `src/agda/Tests/ModulesChecklist.agda` (kernel/image/cokernel)
  - Outcome: update record literals to match current `AM.*OfModuleHomomorphism` fields
  - Trigger: `make agda-all` warnings (2026-01-13)

- [ ] Examples deferred modules (see deferred summary)

TechnicalDebtRegistry and demonstration examples; Effort: 1-3 days/module; Ref: ROADMAP.md Phase 3.1

- [ ] Plan.CIM integration tests

E2E transformation pipeline, grammar conformance, proof export; Effort: 100-150 lines; Ref: ROADMAP.md Phase 2.2

- [ ] Plan.CIM documentation

Architecture/API/tutorials; Effort: 1-2 days; Ref: ROADMAP.md Phase 4.1

### Low-Priority Deferred Items

Nice-to-have improvements, cleanup, or exploratory work.

- [ ] Utility-broken.agda recovery

Extract 77 roadmap examples; fix syntax; merge into Plan.CIM.Utility.agda; Effort: 4-6 hours; Ref: intake/refactoring-roadmap-migration.md

- [ ] Script modernization

Python 3.10+ features, type hints, error handling; Effort: 2-3 days; Ref: scripts/

- [ ] CHIP protocol semantic enrichment

Ambiguity detection, transformation metrics, coherence validation; Research 1-2 weeks; Deferred to ROADMAP-DRAFT.md

## Deferred Item Sources

### By Module Category

```text
Core (deferred)         : ~80 postulates, ~30 TODOs
Algebra (deferred)      : ~45 postulates, ~20 TODOs
Examples (deferred)     : ~25 postulates, ~15 TODOs
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

- Add inline marker: -- TODO: [category] description or postulate name : Type

- Run make deferred-items to refresh build/reports/deferred-summary.json

- If high/medium priority, add to sections above

- Reference ROADMAP.md if it blocks a phase

### Resolving Deferred Items

- Remove marker or replace postulate with implementation

- Run make deferred-items to refresh counts

- Check off item in this document

- Update ROADMAP.md if a milestone is unblocked

### Periodic Review

- Weekly: review high-priority items and blockers

- Monthly: reprioritize medium/low items

- Per milestone: regenerate build/reports/deferred-summary.json and verify counts

## Tools

### Generate Deferred Summary

```text
make deferred-items
# Outputs: build/reports/deferred-summary.json
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

- Primary roadmap: ROADMAP.md

- Worklog: intake/refactoring-roadmap-migration.md

- Generated data: build/reports/deferred-summary.json (gitignored)

- Build tasks: .vscode/tasks.json

## Status Snapshot

Last updated: 2025-12-20; Next review: after Phase 1 cleanup (ROADMAP.md); Totals: 567 (351 postulates, 155 TODOs, 61 other)
