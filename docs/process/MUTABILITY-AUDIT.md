# Mutability Audit (Make + Workflows)

This review documents which targets are mutative, why, and where the current
workflows are invoking mutative targets without an explicit justification.
The goal is consistency: every mutation should have a clear, traceable reason.

## Summary

- The mutability gates are working as intended: they fail when a target (or any
  of its dependencies) writes artifacts.
- The act failures that exposed mutative behavior are resolved by explicitly
  granting mutation (`MUTATE_LEVEL=report|build|repo`) in CI steps that write reports or derived
  artifacts.
- The core issue is not incorrect behavior, but implicit assumptions. These
  need to be made explicit by either (a) setting `MUTATE_LEVEL=report|build|repo` in steps that
  are expected to emit artifacts, or (b) splitting out read-only variants that
  do not write to disk.

## Workflow Targets and Mutability

This list is derived from the current CI workflow and the Makefile’s mutability graph
(direct + transitive dependencies).

## Mutable Surface Boundaries

- **build/**: ephemeral build artifacts and reports (default, untracked).
- **data/** and **docs/**: tracked state; these are the intended persistence roots.
- **Repo-root docs**: `README.md`, `NAVIGATION.md`, `CONTRIBUTING.md`, `ROADMAP.md`
  are treated as docs-scoped outputs for mutability gating.
- **.github/**: workflow-facing outputs (e.g. `.github/roadmap/tasks.json`,
  `.github/badges/weights.json`) are explicitly scoped as `github` and require
  repo-level mutation permission.

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

## Immediate Inconsistencies Observed During act Runs

1) Historical: CI steps called mutative targets without `MUTATE_LEVEL` set.
   - Example: `check-docs` and `check-json` write reports and recomposed JSON
     artifacts under `build/`.

These are *expected* mutations; the issue has been addressed by adding explicit
grants in `ci.yml`.

## Recommendations (Pick One Policy)

### Policy A — Explicit mutation grants in workflows (Implemented)

Add `MUTATE_LEVEL=report|build|repo` to workflow steps that are expected to write artifacts. This
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
- Are report artifacts required in all runs, or only in CI?
- Which artifacts are essential for local dev vs. CI-only?
