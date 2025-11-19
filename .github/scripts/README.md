# GitHub Actions Scripts

This directory contains automation scripts used by the CI/CD workflows.

## Scripts

### `detect-deferred-items.sh`

Scans the codebase for deferred items including:

- **DeviationLog** entries: Documented deviations from ideal implementation
- **Postulates**: Axioms or assumptions not yet proven constructively
- **TODO** items: General tasks marked for future work
- **PLANNED** items: Features or work explicitly planned in documentation
- **FIXME** items: Known issues requiring fixes

**Usage:**

```bash
./detect-deferred-items.sh [output-file]
```

**Outputs:**

- Markdown report with all findings
- JSON summary for programmatic access
- GitHub Actions outputs (when running in CI)

### `create-or-update-tracking-issue.sh`

Creates or updates a GitHub issue to track deferred items. The issue serves as a central tracking point for all deferred work.

**Usage:**

```bash
GH_TOKEN=<token> ./create-or-update-tracking-issue.sh <report-file> <summary-file>
```

**Behavior:**

- Creates a new tracking issue if none exists
- Updates existing tracking issue with latest report
- Issue stays open as long as deferred items remain

## Workflows

### `ci.yml`

Main CI workflow that:

1. Type-checks Agda code
2. Builds documentation
3. Tracks deferred items (on main branch pushes)
4. Creates/updates tracking issue

### `deferred-items.yml`

Dedicated deferred items review workflow that:

1. Runs weekly on schedule
2. Runs on every PR
3. Compares deferred items between PR and base branch
4. Posts informational comment on PRs (does not block merge)
5. Provides visibility into technical debt trends

## Labels

- `deferred-tracking`: Applied to the main tracking issue
- `enhancement`: Indicates improvement opportunity

## Local Testing

You can run the detection script locally:

```bash
# From repository root
.github/scripts/detect-deferred-items.sh

# View the report
cat deferred-items.md

# Check the JSON summary
jq . deferred-summary.json
```

## Philosophy

This automation treats deferred items as **informational** rather than **blocking**:

- ✅ Provides visibility into technical debt
- ✅ Tracks trends over time
- ✅ Encourages addressing items when appropriate
- ❌ Does NOT block PRs or fail builds
- ❌ Does NOT enforce arbitrary coverage thresholds

The goal is awareness and continuous improvement, not gatekeeping.
