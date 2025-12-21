# Deferred Items Tracking

## Overview
This document tracks deferred work items identified during the Agda 2.8.0 migration and refactoring efforts. As of 2025-12-20, there are **567 total deferred items** across the codebase.

**Source**: deferred-summary.json (auto-generated)
**Categories**:
- 351 postulates (placeholders for unproven theorems)
- 155 TODOs (inline code comments)
- 61 other markers (FIXME, HACK, etc.)

## Tracking Strategy

### High-Priority Deferred Items
Items blocking core functionality or Plan.CIM completion.

#### Plan.CIM Layer
- [ ] **CHIPCoreRecompose.agda** (8 lines stub)
  - Blocked by: Core module re-enabling
  - Effort: 100-150 lines
  - Timeline: 1-2 weeks after Core layer restored
  - Reference: [ROADMAP.md Phase 2.1](#phase-2-plan-cim-completion)

- [ ] **PandocProtocols.agda stubs** (39 lines)
  - blockAmb, blockTransSys, blockCoherence - return defaults
  - docAmb, docTransSys, docCoherence - return defaults
  - Effort: 50-100 lines for semantic implementations
  - Reference: ROADMAP.md Phase 2.3

- [ ] **CHIPConformance.agda stubs**
  - makeSPPFNode (line 42) - requires BraidedSPPF type
  - composeBraids semantic refinement (optional)
  - Effort: 50-80 lines
  - Reference: ROADMAP.md Phase 2.3

#### Core Module Re-enabling
- [ ] **Core.disabled/ modules** (23 modules)
  - Phase theory restoration
  - Categorical foundations
  - Adjunction and limit structures
  - Effort: 1-2 weeks full-time
  - Reference: ROADMAP.md Phase 3.2

- [ ] **Algebra.disabled/ modules** (subset for Core dependencies)
  - Groups, rings, modules structures
  - Field theory
  - Effort: 3-5 days
  - Reference: ROADMAP.md Phase 3.2

### Medium-Priority Deferred Items
Items improving functionality but not blocking major features.

#### Examples Re-enabling
- [ ] **Examples.disabled/ modules** (13 modules)
  - TechnicalDebtRegistry.agda
  - Workflow demonstrations
  - Categorical examples
  - Effort: 1-3 days per module
  - Reference: ROADMAP.md Phase 3.1

#### Documentation & Testing
- [ ] **Plan.CIM integration tests**
  - End-to-end transformation pipeline validation
  - Grammar conformance testing
  - Proof export verification
  - Effort: 100-150 lines
  - Reference: ROADMAP.md Phase 2.2

- [ ] **Plan.CIM documentation**
  - Architecture overview
  - API reference
  - Tutorials and examples
  - Effort: 1-2 days
  - Reference: ROADMAP.md Phase 4.1

### Low-Priority Deferred Items
Nice-to-have improvements, optimizations, or exploratory work.

#### Refactoring & Cleanup
- [ ] **Utility-broken.agda recovery**
  - Extract 77 roadmap examples (93KB)
  - Fix list syntax errors
  - Append to Plan.CIM.Utility.agda
  - Effort: 4-6 hours manual cleanup
  - Reference: intake/refactoring-roadmap-migration.md section 1

- [ ] **Script modernization**
  - Update Python scripts to Python 3.10+ features
  - Add type hints
  - Improve error handling
  - Effort: 2-3 days
  - Reference: scripts/ directory

#### Exploratory Work
- [ ] **CHIP protocol semantic enrichment**
  - Richer ambiguity detection
  - Advanced transformation metrics
  - Compositional coherence validation
  - Effort: Research phase, 1-2 weeks
  - Deferred to ROADMAP-DRAFT.md

## Deferred Item Sources

### By Module Category
```
Core.disabled/          : ~80 postulates, ~30 TODOs
Algebra.disabled/       : ~45 postulates, ~20 TODOs
Examples.disabled/      : ~25 postulates, ~15 TODOs
Plan.CIM/               : ~15 TODOs (stubs)
Scripts/                : ~40 TODOs (improvements)
Documentation/          : ~50 TODOs (missing sections)
Other                   : ~106 postulates, ~35 TODOs
```

### By Type
```
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
When creating a new deferred item:
1. Add inline marker: `-- TODO: [category] description` or `postulate name : Type`
2. Update deferred-summary.json via `make deferred-items` task
3. Add to appropriate section above if high/medium priority
4. Reference in ROADMAP.md if blocks a phase

### Resolving Deferred Items
When completing a deferred item:
1. Remove inline marker or replace postulate with implementation
2. Update deferred-summary.json via `make deferred-items`
3. Check off item in this document
4. Update ROADMAP.md if phase milestone reached

### Periodic Review
- **Weekly**: Review high-priority items, update blockers
- **Monthly**: Re-prioritize medium/low items based on project goals
- **Per milestone**: Regenerate deferred-summary.json, verify counts

## Tools

### Generate Deferred Summary
```bash
make deferred-items
# Outputs to: deferred-summary.json
```

### Search for Deferred Items
```bash
# TODOs
grep -r "TODO" src/agda/

# Postulates
grep -r "postulate" src/agda/

# All markers
grep -rE "TODO|FIXME|HACK|XXX|NOTE" src/agda/
```

### Flag Unannotated Technical Debt
```bash
make Flag Unannotated Technical Debt
# Or: python3 scripts/flag_unannotated_debt.py
```

## Cross-References
- Primary roadmap: [ROADMAP.md](ROADMAP.md)
- WAL: [intake/refactoring-roadmap-migration.md](intake/refactoring-roadmap-migration.md)
- Generated data: deferred-summary.json (in .gitignore, auto-generated)
- Build tasks: .vscode/tasks.json

---
*Last updated: 2025-12-20*
*Next review: After Phase 1 cleanup (ROADMAP.md)*
*Total items: 567 (351 postulates, 155 TODOs, 61 other)*
