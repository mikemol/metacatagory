# Metacatagory Development Roadmap

## Current Status (2025-12-20)
- **Agda Version**: 2.8.0
- **Compilation**: 153 modules compiled successfully
- **Recent Work**: Plan.CIM layer implementation (10/11 modules, 1268 lines)
- **Repository State**: Cleanup phase - organizing for production readiness

## Phase 1: Repository Hygiene & Organization (Current)

### 1.1 Cleanup Temporary Files (Priority: High)
**Goal**: Remove or archive files not suitable for version control

Files to prune:
- `combined.agda.txt` (1.6M) - Generated artifact from combine_agda.py
- `tmp_patch.txt` (40 bytes) - Temporary patch file
- `Utility-broken.agda` (118K) - Contains 77 roadmap examples with syntax errors
- `deferred-summary.json` (186 bytes) - Generated tracking data

**Actions**:
1. Extract roadmap data from Utility-broken.agda before deletion
2. Update .gitignore to prevent future temp file commits
3. Archive or delete combined.agda.txt (check if needed for any workflows)
4. Remove tmp_patch.txt
5. Decide: migrate deferred-summary.json to DEFERRED-TRACKING.md or keep as generated artifact

**Reference**: intake/refactoring-roadmap-migration.md sections 1-2

### 1.2 Initialize Tracking Documents (Priority: High)
**Goal**: Establish structured tracking for deferred work and roadmap items

Empty files to populate:
- `ROADMAP-DRAFT.md` - Future feature planning
- `DEFERRED-TRACKING.md` - 567 deferred items from migration

**Actions**:
1. Populate DEFERRED-TRACKING.md with structure for tracking postulates, TODOs, stubs
2. Decide on ROADMAP-DRAFT.md vs ROADMAP.md relationship
3. Cross-reference with deferred-summary.json (567 items: 351 postulates, 155 TODOs)

**Reference**: intake/refactoring-roadmap-migration.md sections 2, 4

### 1.3 Update .gitignore (Priority: High)
**Goal**: Prevent accidental commits of generated/temporary files

Add to .gitignore:
```
# Generated/temporary files
combined.agda.txt
tmp_patch.txt
deferred-summary.json
Utility-broken.agda

# WAL intake directory (optional - decide if intake/ should be committed)
intake/*
!intake/refactoring-roadmap-migration.md
```

**Actions**:
1. Add patterns to .gitignore
2. Review other generated files in repo root
3. Document policy for what belongs in version control

## Phase 2: Plan.CIM Completion (Next)

### 2.1 Implement CHIPCoreRecompose.agda (Priority: Medium, Blocked)
**Status**: Stub (8 lines), requires Core layer re-enabling
**Effort**: 100-150 lines estimated
**Blocker**: Core/Algebra/Phase modules disabled (142 modules total)
**Timeline**: 1-2 weeks after Core modules restored

**Dependencies**:
- Core.Phase module structure
- Algebra categorical semantics
- Reflection API updates for Agda 2.8.0

**Reference**: src/agda/Plan/CIM/CHIPCoreRecompose.agda, intake/refactoring-roadmap-migration.md section 1

### 2.2 Integration Tests for Plan.CIM (Priority: High)
**Status**: Not started
**Effort**: 100-150 lines
**Goal**: Validate end-to-end Pandoc → Markdown transformation pipeline

**Test coverage**:
1. PandocToMarkdown transformations (all 13 Inline, 11 Block cases)
2. BraidTrace generation and validation
3. Structure attestation and composition
4. GrammarBridge conformance checking
5. PandocProofExport JSON/YAML serialization
6. CHIPRecomposed witness aggregation

**Actions**:
1. Create src/agda/Plan/CIM/Tests.agda
2. Import PandocProofExample as baseline
3. Add property-based test cases
4. Validate against grammar rules

### 2.3 Refine CHIPConformance Stubs (Priority: Medium)
**Status**: Functional but minimal
**Files**: src/agda/Plan/CIM/CHIPConformance.agda, PandocProtocols.agda

Stubs to implement:
1. **makeSPPFNode** (line 42): Requires BraidedSPPF type definition
2. **composeBraids** (line 34): Currently simple swap, could encode richer semantics
3. **PandocProtocols witnesses** (39 lines): All return default values

**Actions**:
1. Design BraidedSPPF record type in Utility.agda
2. Implement makeSPPFNode with proper SPPF construction
3. Add semantic depth to composeBraids (optional, deferred)
4. Populate blockAmb, blockTransSys, blockCoherence with real logic
5. Populate docAmb, docTransSys, docCoherence with document-level semantics

**Reference**: intake/refactoring-roadmap-migration.md sections 3-4

## Phase 3: Re-enable Disabled Modules (Future)

### 3.1 Examples Re-enabling (Priority: Medium)
**Status**: 13 modules disabled in src/agda/Examples.disabled/
**Effort**: 1-3 days per module
**Goal**: Showcase Plan.CIM and other capabilities with practical demonstrations

Example modules to re-enable:
- TechnicalDebtRegistry.agda (already has task for export)
- Workflow examples demonstrating CHIP protocols
- Categorical examples using re-enabled Core modules

**Actions**:
1. Audit each Example for Agda 2.8.0 compatibility
2. Update imports and type signatures
3. Test compilation and execution
4. Document expected outputs

### 3.2 Core Modules Re-enabling (Priority: High, Long-term)
**Status**: 23+ modules disabled in src/agda/Core.disabled/
**Effort**: 1-2 weeks full-time
**Goal**: Restore advanced categorical semantics, Phase theory, adjunctions

Core modules to re-enable:
- Core.Phase structure
- Core.Functors (adjunctions, limits)
- Core categorical foundations
- Algebra integration

**Blockers**:
- Agda 2.8.0 stdlib changes (especially reflection API)
- Universe level updates
- Termination checking for complex recursion

**Actions**:
1. Create systematic re-enabling plan (module dependency order)
2. Update one Core module at a time
3. Test compilation with --safe where possible
4. Document required pragma usage ({-# TERMINATING #-}, etc.)

**Impact**: Unblocks CHIPCoreRecompose.agda, completes Plan.CIM layer

## Phase 4: Documentation & Onboarding (Ongoing)

### 4.1 Plan.CIM Documentation (Priority: Medium)
**Status**: Code comments only
**Effort**: 1-2 days
**Goal**: Comprehensive usage guide for Pandoc transformation pipeline

Documentation needed:
1. Architecture overview (ARCHITECTURE.md update)
2. API reference for each module
3. Tutorial: Building custom transformations
4. Tutorial: Exporting proofs to JSON/YAML
5. Tutorial: Grammar-based validation

**Actions**:
1. Create docs/Plan-CIM-Guide.md
2. Add inline documentation to key functions
3. Expand PandocProofExample.agda with explanatory comments
4. Create visual diagrams (transformation flow, witness composition)

### 4.2 Navigation & Contribution Guides (Priority: Low)
**Status**: NAVIGATION.md, CONTRIBUTING.md exist but need updates
**Effort**: 1 day
**Goal**: Help new contributors understand codebase structure

Updates needed:
1. NAVIGATION.md: Add Plan.CIM layer description
2. CONTRIBUTING.md: Update for Agda 2.8.0 environment
3. COPILOT_SYNERGY.md: Add patterns from this migration session
4. ARCHITECTURE.md: Document CHIP protocol integration

## Cross-References
- WAL: intake/refactoring-roadmap-migration.md
- Deferred items: deferred-summary.json (567 items)
- Build tasks: .vscode/tasks.json (8 tasks defined)
- Scripts: scripts/ directory (13 Python scripts)

## Decision Log

### Temp File Policy (2025-12-20)
- **Decision**: Prune combined.agda.txt, tmp_patch.txt from repository
- **Rationale**: Generated artifacts, large size, not needed for builds
- **Action**: Add to .gitignore, document in scripts/combine_agda.py if needed

### Utility-broken.agda Handling (2025-12-20)
- **Decision**: Extract roadmap examples, then archive/delete
- **Rationale**: Contains 77 example roadmaps with syntax errors (93KB)
- **Action**: Manual syntax cleanup → append to Plan.CIM.Utility.agda, then delete original
- **Blocker**: Lower priority than repository hygiene

### ROADMAP.md vs ROADMAP-DRAFT.md (2025-12-20)
- **Decision**: ROADMAP.md is primary, ROADMAP-DRAFT.md for speculative work
- **Rationale**: Clear separation of committed vs exploratory planning
- **Action**: Populate this file (ROADMAP.md) as primary roadmap

---
*Last updated: 2025-12-20 after Plan.CIM implementation*
*Next review: After Phase 1 cleanup complete*
