## Test Plan: Make Targets and Workflows

This plan defines a comprehensive, repeatable test matrix for local and CI
validation, plus a workflow audit (what each workflow does, value, and cleanup
opportunities).

### Principles

- Prefer the Makefile entry points for local verification to match CI.
- Run read-only targets first to catch structural issues before mutation.
- Separate network-dependent steps; they will fail in restricted environments.
- Use `MUTATE_OK=1` explicitly for mutative targets.

### Local Make Targets

#### 1) Read-only guardrail checks (no mutation)

Order is chosen to surface infra/graph errors first.

```bash
make graph-assert-ok
make makefile-validate
make md-lint
make intake-lint
```

Expected:
- `graph-assert-ok`: `Graph OK: status=OK edges=...`
- `makefile-validate`: report in `build/reports/makefile-validate.txt`
- `md-lint`: markdown lint success
- `intake-lint`: report in `build/reports/intake-md-lint.txt`

Expected artifacts:
- `build/graph_parsed_state.txt`
- `build/reports/makefile-validate.txt`
- `build/reports/intake-md-lint.txt`

#### 2) Dependency graph roundtrip (mutative)

```bash
MUTATE_OK=1 make json-decompose
MUTATE_OK=1 make json-roundtrip-validate
```

Expected:
- Decomposition writes to `data/deps/` and recompose to `build/dependency_graph_recomposed.json`.
- Roundtrip should pass on module + edge counts.

Expected artifacts:
- `data/dependency_graph.json`
- `data/deps/_metadata.json`
- `data/deps/modules/_index.json`
- `build/dependency_graph_recomposed.json`

#### 3) Full validation sweep (mutative)

```bash
MUTATE_OK=1 make check-all
```

Expected:
- Runs infra checks, docs checks, JSON roundtrip checks, debt checks, Python tests.
- If network is restricted, the Python venv step may fail (`pip install`).

Expected artifacts (subset; grows with dependencies):
- `build/venv/python_setup.stamp`
- `build/reports/validate_json_provenance.json`
- `build/reports/roadmap_export_provenance.json`
- `build/reports/roundtrip_validation_provenance.json`

Notes:
- Network‑dependent step: `build/venv/python_setup.stamp` (pip install).

#### 4) Regeneration (mutative)

```bash
MUTATE_OK=1 make regen-makefile
MUTATE_OK=1 make regen-all
```

Expected:
- `Makefile` + recipe scripts regenerate.
- Artifacts in `data/`, `docs/`, `build/`.

Expected artifacts:
- `Makefile.generated`
- `build/recipes/**`
- `docs/automation/MAKEFILE-TARGETS.md`
- `docs/automation/makefile_targets_generated.md`

### Workflow Audit: What Each Workflow Does

#### `.github/workflows/ci.yml`

Purpose:
- Core CI validation: Agda typechecking, docs, JSON checks, and coverage.

Jobs:
- `build`: checkout, configure Agda, install Python/Node, `make check`, run `scripts/makefile_coverage.py --run-targets`, upload reports.
- `deferred-items-check` (push on main): runs `make deferred-items`, uploads reports, creates/updates tracking issue.

Value:
- High. Provides the primary correctness gate for build + docs + JSON checks.

Local parity:
- `make check` for build logic.
- `make act-ci` for full workflow emulation (requires act).

Cleanup opportunities:
- None obvious; primary CI flow is consolidated in `build`.

#### `.github/workflows/makefile-validate.yml`

Purpose:
- Verifies Makefile/Agda/Docs triangle consistency when relevant files change.

Value:
- High. Keeps Makefile generation and docs in sync.

Local parity:
- `make makefile-validate`
- `make act-makefile-validate`

Cleanup opportunities:
- None obvious; scope is narrow and targeted.

#### `.github/workflows/markdown-lint.yml`

Purpose:
- Markdown linting on `.md` changes.

Value:
- Medium. Helps maintain doc quality.

Local parity:
- `make md-lint`
- `make act-lint`

Cleanup opportunities:
- None obvious; low maintenance.

#### `.github/workflows/markdown-auto-fix.yml`

Purpose:
- Auto-fixes markdown in PRs (same-repo only), commits and pushes.

Value:
- Medium. Automates style fixes.

Local parity:
- `make md-fix`
- `make act-markdown-fix`

Cleanup opportunities:
- Consider moving the auto‑commit into a separate bot/branch workflow if commit noise is undesirable.

#### `.github/workflows/roadmap-sync.yml`

Purpose:
- Syncs `.github/roadmap/tasks.json` to GitHub issues (scheduled or on change).

Value:
- High for issue tracking alignment.

Local parity:
- `make roadmap-sync`
- `make act-roadmap-sync`

Cleanup opportunities:
- Ensure issue write permissions are only enabled where needed (already scoped).

#### `.github/workflows/deferred-items.yml`

Purpose:
- Compares deferred items in PR base vs head and posts a comment.

Value:
- Medium/High. Helps keep debt tracking visible.

Local parity:
- `make deferred-items` in both base and current, plus compare.
- `make act-deferred` for workflow emulation.

Cleanup opportunities:
- The workflow runs `make deferred-items` twice; could cache the base report or avoid regenerating if artifact exists.

#### `.github/workflows/badge-update.yml`

Purpose:
- Regenerates badges on roadmap/report/script/Agda changes; commits if changes.

Value:
- Medium. Keeps badges accurate.

Local parity:
- `make badges`
- `make act-badges`

Cleanup opportunities:
- Consider skipping badge updates on PRs if badge commits are noisy.

### Workflow Test Matrix (Local)

Use `act` only if available and configured (uses container image).

```bash
make act-list
make act-ci
make act-makefile-validate
make act-lint
make act-markdown-fix
make act-roadmap-sync
make act-deferred
make act-badges
```

### Minimum CI Parity (Local)

Smallest local target set that mirrors CI’s critical gates:

```bash
make graph-assert-ok
make makefile-validate
make md-lint
MUTATE_OK=1 make json-roundtrip-validate
```

```bash
MUTATE_OK=1 make python-test
```

### Known Constraints

- Some workflows (auto‑fix, badges) mutate the repo by design.

### Follow‑ups

- None currently identified.
