# Deferred Items Tracking System

## Overview

The repository now has automated tracking of deferred items (technical debt) through GitHub Actions. This system provides **visibility without obstruction** - it informs but doesn't block.

## What Gets Tracked

### 1. **DeviationLog Entries** (16 found)
Documented deviations from ideal implementation with rationale and future plans.

**Example:**
```agda
-- DeviationLog [2025-11-18]: Removed inline validation proof that Group index
-- is greater than Monoid index. Validation now covered in Tests.HierarchyValidation
-- as Bool-based checks to avoid brittle proofs that break builds during refactors.
```

### 2. **Postulates** (366 found)
Axioms or assumptions not yet proven constructively. Many are intentional placeholders for future work.

**Example:**
```agda
postulate
  F E : FieldDeclaration
```

### 3. **TODO Items** (68 found)
General tasks marked for future work.

### 4. **PLANNED Items** (0 found)
Features or work explicitly planned in documentation (currently tracked in `testing.md` phases).

### 5. **FIXME Items** (0 found)
Known issues requiring immediate fixes.

## How It Works

### On Every Push to Main

1. **Typecheck & Build** (existing workflow continues)
2. **Scan for Deferred Items** (new step)
3. **Create/Update Tracking Issue** (new step)
   - A single GitHub issue acts as the central tracking point
   - Issue updates with current counts and details
   - Issue remains open as long as items exist

### On Every Pull Request

1. **Compare Before/After** 
   - Scans both base and PR branches
   - Calculates the delta (increase/decrease/unchanged)
2. **Post Informational Comment**
   - Shows impact on deferred items
   - ✅ Celebrates reductions
   - ⚠️ Highlights increases (without blocking)
   - ➡️ Notes no change
3. **Does NOT Block Merge**
   - This is informational only
   - Increases are allowed if justified

### Weekly Review (Scheduled)

- Runs every Monday at 9 AM UTC
- Generates fresh report
- Updates tracking issue
- Provides weekly health check

## Running Locally

You can generate the report on your local machine:

```bash
# Generate report
.github/scripts/detect-deferred-items.sh

# View markdown report
cat deferred-items.md

# View JSON summary
jq . deferred-summary.json
```

## Philosophy

This system embodies a **trust-based, informational approach**:

### ✅ What It Does

- **Provides visibility** into technical debt trends
- **Tracks progress** toward constructive proofs
- **Celebrates wins** when items are resolved
- **Informs decisions** about prioritization
- **Creates accountability** through transparency

### ❌ What It Does NOT Do

- **Block PRs** based on deferred item counts
- **Enforce arbitrary thresholds** ("must have < X postulates")
- **Create false urgency** for all items equally
- **Ignore context** (many postulates are appropriate placeholders)
- **Punish incremental progress** (sometimes items must increase temporarily)

## Understanding the Numbers

### Current State (as of 2025-11-19)

| Category | Count | Context |
|----------|-------|---------|
| **DeviationLog** | 16 | Documented architectural decisions |
| **Postulates** | 366 | Mix of placeholders and to-be-proven theorems |
| **TODO** | 68 | Future work items |
| **PLANNED** | 0 | Tracked in `testing.md` phases instead |
| **FIXME** | 0 | No urgent issues |
| **Total** | 450 | Healthy for a research codebase |

### Interpreting Postulates

Not all postulates are equal:

1. **Scaffolding Postulates** - Intentional placeholders for fields/algorithms
   - Example: `postulate F E : FieldDeclaration`
   - Status: ✅ Appropriate for current phase

2. **Bridge Postulates** - Connectors awaiting full implementation
   - Example: Integration between UMP and constructive witnesses
   - Status: ⏳ In progress (Phase II)

3. **Proof Postulates** - Theorems to be proven constructively
   - Example: Universal property uniqueness
   - Status: 📋 Planned (Phase III-IV)

## Next Steps

1. **Review Tracking Issue** - Check the auto-generated issue for current state
2. **Prioritize by Phase** - Use `testing.md` to guide resolution order
3. **Address Systematically** - Work through phases rather than cherry-picking
4. **Monitor Trends** - Watch for unexpected increases
5. **Celebrate Progress** - Note when counts decrease!

## Future Enhancements

Potential additions to this system:

- [ ] Category breakdown in PR comments (show which types changed)
- [ ] Historical trending graph (track over time)
- [ ] Automatic sub-issue creation for DeviationLog items
- [ ] Integration with GitHub Projects for Kanban tracking
- [ ] Configurable thresholds for custom warnings
- [ ] Badge in README showing current count

## Questions?

- **Why not fail builds on postulates?** Because many are intentional and appropriate for current development phase.
- **Won't this create noise?** No - notifications are consolidated into a single tracking issue and PR comments.
- **Can I disable this?** Yes - remove or disable the `deferred-items.yml` workflow.
- **How do I exclude false positives?** Refine the grep patterns in `detect-deferred-items.sh`.

---

**Status:** ✅ Active and tracking  
**Last Updated:** 2025-11-19  
**Tracking Issue:** Will be created on next push to main
