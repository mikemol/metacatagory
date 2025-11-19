# MetaCategory Roadmap

> Authoritative consolidated roadmap integrating phases, ingestion curriculum, deferrals, and future research tasks. Generated from `ROADMAP-DRAFT.md` and normalized for automation.

For full contextual detail see `ROADMAP-DRAFT.md`. This file focuses on actionable tracking and GitHub issue integration.

## Phase Status Summary (2025-11-19)

* Phase 0: COMPLETE
* Phase I: COMPLETE
* Phase II: COMPLETE (focused index closure)
* Phase III.1–III.4: COMPLETE
* Phase III.5: DEFERRED (Topos checklist constructor alignment)
* Phase IV (Adjunctions, Yoneda, Fibrations): PLANNED
* Phase IV.0 (Algorithmic foundations arithmetic): DEFERRED
* Phase V (Gödel boundary, growth metrics, HoTT closure): PLANNED

## Integration with Issue Workflow

Tasks are defined in `.github/roadmap/tasks.json` and synchronized to GitHub Issues via `.github/scripts/sync-roadmap-issues.sh`.

### Labels

All roadmap issues receive the `roadmap` label. Status changes should be made by editing `tasks.json` then re-running the sync (locally or via workflow).

### Adding / Updating Tasks

1. Edit `.github/roadmap/tasks.json` (add new JSON object).  
2. Commit and push.  
3. Run sync script locally or rely on the scheduled workflow (if enabled).  

### Manual Local Sync

```bash
export GITHUB_TOKEN=YOUR_TOKEN
export GITHUB_REPOSITORY=owner/repo   # e.g. mikemol/metacatagory
bash .github/scripts/sync-roadmap-issues.sh
```

### GitHub Actions Workflow (Proposed)

Create `.github/workflows/roadmap-sync.yml`:

```yaml
name: Roadmap Sync
on:
  push:
    paths:
      - '.github/roadmap/tasks.json'
  workflow_dispatch: {}
  schedule:
    - cron: '0 10 * * 1'
jobs:
  sync:
    runs-on: ubuntu-latest
    permissions:
      issues: write
      contents: read
    steps:
      - uses: actions/checkout@v4
      - name: Sync roadmap issues
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
        run: bash .github/scripts/sync-roadmap-issues.sh
```

## Deferred Tracking Cross-Reference

Deferred items (postulates, TODO, etc.) remain separately tracked via the existing scripts: `.github/scripts/detect-deferred-items.sh` and its workflow. This roadmap complements (does not replace) that visibility. Consider linking reductions (e.g., closing a roadmap task that eliminates postulates) in issue comments.

## Task Categories

* Phase: Milestone-driven structural/semantic evolutions.
* Ingestion: Textbook-derived coverage expansion.
* Research: Longer-term theoretical or performance objectives.

## Editing Principles

* Keep `tasks.json` small and meaningful; avoid flooding with micro-tasks.
* Use consistent `id` prefixes: `PHASE-`, `INGEST-`, `RESEARCH-`.
* Prefer `status` values: `planned`, `in-progress`, `completed`, `deferred`.

## Next Automation Enhancements (Optional)

* Add status labels automatically (e.g. `status:planned`).
* Post delta comment on PRs showing changed task counts.
* Generate progress badge in README via workflow.

## Source of Truth Note

Descriptive narrative (rationale, deep context) lives in `ROADMAP-DRAFT.md`. This file should remain concise for automation and quick scanning.

