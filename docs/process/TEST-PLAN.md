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
- `build/ingested_metadata.json` (validated for schema if present)

Notes:
- Network‑dependent step: `build/venv/python_setup.stamp` (pip install).
- Offline mode: set `PYTHON_OFFLINE=1` to skip pip install and require deps already present.

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

### Workflow Audit: Current CI

#### `.github/workflows/ci.yml`

Purpose:
- Core CI validation: Agda exports, docs, JSON checks, Python/debt checks, and report collection.

Jobs:
- `agda-exports`: regen Makefile recipes, export planning/dependency artifacts, run makefile coverage.
- `docs-checks`: docs/markdown lint + module docs checks.
- `roadmap-json-checks`: roadmap exports and JSON roundtrip checks.
- `python-checks`: pytest + debt checks.
- `collect-reports`: merge report artifacts from per-job report dirs.

Value:
- High. Single source of truth for CI gates and report outputs.

Local parity:
- `MUTATE_OK=1 make check` for core validation logic.
- `make act-ci` for full workflow emulation (requires act).

Cleanup opportunities:
- Keep CI consolidated; avoid reintroducing single-purpose workflows unless they
  serve a distinct gate or permission boundary.

### Workflow Test Matrix (Local)

Use `act` only if available and configured (uses container image).

```bash
make act-list
make act-ci
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

- Some local targets (e.g., `md-fix`, `badges`, `regen-*`) mutate the repo by design.
- CI is consolidated under `ci.yml`; avoid assuming other workflows exist.

### Follow‑ups

- None currently identified.
