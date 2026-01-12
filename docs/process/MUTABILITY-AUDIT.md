# Mutability Audit (Make + Workflows)

This review documents which targets are mutative, why, and where the current
workflows are invoking mutative targets without an explicit justification.
The goal is consistency: every mutation should have a clear, traceable reason.

## Summary

- The mutability gates are working as intended: they fail when a target (or any
  of its dependencies) writes artifacts.
- The current act workflow failures are explained by missing `MUTATE_OK=1` in
  workflow steps that call make targets which write to `build/` or `docs/`.
- The core issue is not incorrect behavior, but implicit assumptions. These
  need to be made explicit by either (a) setting `MUTATE_OK=1` in steps that
  are expected to emit artifacts, or (b) splitting out read-only variants that
  do not write to disk.

## Workflow Targets and Mutability

This list is derived from the workflows and the Makefile’s mutability graph
(direct + transitive dependencies).

### `.github/workflows/markdown-lint.yml`

- `regen-makefile` (mutative)
  - **Reason:** generates `Makefile.generated` and `build/recipes/**`.
- `md-lint` (mutative, via deps)
  - **Reason:** writes `build/reports/md-lint.txt`.
  - **Source of mutability:** `build/reports/dir.stamp` + output report.

### `.github/workflows/makefile-validate.yml`

- `makefile-validate` (mutative, via deps)
  - **Reason:** writes `build/reports/makefile-validate.txt`.
  - **Source of mutability:** `build/reports/dir.stamp` + output report.

### `.github/workflows/ci.yml`

All `check-*` targets are mutative (transitively) because they produce reports
or derived artifacts:

- `check-docs` → `md-lint`, `docs-lint`, `docs-validate`
  - **Reason:** lint and validation reports under `build/reports/`.
- `check-json` → `json-roundtrip-*`
  - **Reason:** JSON recomposition/validation artifacts under `build/`.
- `check-python` → `python-test`
  - **Reason:** writes test report artifacts under `build/reports/` (via tests).
- `check-roadmap` → `roadmap-validate-triangle`
  - **Reason:** consumes artifacts and emits validation output under `build/`.
- `check-debt` → `deferred-items` + `intake-scan`
  - **Reason:** generates `docs/status/*` and related reports.

### `.github/workflows/roadmap-sync.yml`

- `roadmap-sync` (mutative)
  - **Reason:** generates `.github/roadmap/tasks.json` and related artifacts.

### `.github/workflows/deferred-items.yml`

- `deferred-items` (mutative)
  - **Reason:** writes `docs/status/DEFERRED-TRACKING.md` and `build/reports/*`.

### `.github/workflows/badge-update.yml`

- `badges` (mutative)
  - **Reason:** writes `.github/badges/*` and related outputs.

## Immediate Inconsistencies Observed During act Runs

1) Workflows call mutative targets without `MUTATE_OK=1`.
   - Example: `Markdown Lint` runs `make md-lint` without `MUTATE_OK=1`, yet
     `md-lint` writes to `build/reports/`.
2) `Roadmap Sync` invokes `make` targets that create `.github/roadmap/*` and
   build artifacts, but doesn’t grant mutation.

These are *expected* mutations; the issue is the missing explicit grant.

## Recommendations (Pick One Policy)

### Policy A — Explicit mutation grants in workflows

Add `MUTATE_OK=1` to workflow steps that are expected to write artifacts. This
keeps mutation explicit and preserves the safety gate.

### Policy B — Read-only variants for lint/validation

Introduce `*-check` targets that:
- write to stdout only, or
- write into a temp dir that is deleted, or
- use a read-only workdir + `ACT_WORKDIR` copy.

Then update workflows to use the read-only variants and reserve mutative targets
for artifact-generation workflows.

## Open Questions

- Should “lint” and “validate” targets be classified as mutative because they
  emit reports, or should they be read-only and only emit stdout?
- Are workflow artifacts (e.g., lint logs) required in all runs, or only in CI?
- Which artifacts are essential for local dev vs. CI-only?
