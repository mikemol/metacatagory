## Environment Variable Inventory

This catalog lists environment variables referenced by Makefile logic, scripts, and
workflows. For each variable, it notes the primary consumers, how it is filled,
and whether execution is guarded or defaults are provided.

### Makefile and Build-Time Variables

| Variable | Consumers | Filled by | Guard / Default | Assumption Check |
| --- | --- | --- | --- | --- |
| `BUILD_WORKDIR` | `Makefile`, `Makefile.generated`, `scripts/recipes/regen-makefile.sh`, `scripts/export_cnf_grammar.sh`, `ci.yml` | Defaults to `.` in Makefile, overridden by `ACT_WORKDIR` or `WORKDIR` | `BUILD_WORKDIR ?= .` with guarded overrides | Safe: defaulted before use |
| `WORKDIR` | `Makefile`, `Makefile.generated` (legacy alias) | User shell or `act_prepare_workspace.sh` | Aliased to `BUILD_WORKDIR` when unset | Safe: legacy compatibility |
| `ACT_WORKDIR` | `Makefile`, `Makefile.generated` | `scripts/act_prepare_workspace.sh` via `GITHUB_ENV`, or user shell | Used only if non-empty; overrides `BUILD_WORKDIR` | Safe: guarded |
| `BUILD_MUTATE_OK` | `Makefile` guard `require_mutate` | User/CI when running mutative targets | Default `0` in Makefile | Safe: guard fails if not set |
| `MUTATE_OK` | `Makefile` guard `require_mutate` (legacy alias) | User/CI | Aliased to `BUILD_MUTATE_OK` when unset | Safe: legacy compatibility |
| `BUILD_SKIP_GHC_BACKEND` | Agda build targets in `src/agda/Examples/Makefile/Targets/AgdaBuild.agda` and `Docs.agda` | User/CI env or Makefile default | Default empty; aliased into `SKIP_GHC_BACKEND` | Safe: empty means "do not skip" |
| `SKIP_GHC_BACKEND` | Agda build targets (legacy alias) | User/CI env | Aliased to `BUILD_SKIP_GHC_BACKEND` when unset | Safe: legacy compatibility |
| `BUILD_JSON_DECOMPOSE_FALLBACK_DIR` | `Makefile`, `Makefile.generated` | User/CI env | Default empty; aliased into `JSON_DECOMPOSE_FALLBACK_DIR` | Safe: empty falls back to `data/deps` |
| `JSON_DECOMPOSE_FALLBACK_DIR` | `Makefile`, `Makefile.generated` (legacy alias) | User/CI env | Aliased to `BUILD_JSON_DECOMPOSE_FALLBACK_DIR` | Safe: legacy compatibility |
| `BUILD_CORES` | `Makefile` parallelism | Shell (nproc/sysctl) or env | Default computed; aliased into `CORES` | Safe: default computed |
| `CORES` | `Makefile` parallelism (legacy alias) | Shell or env | Aliased to `BUILD_CORES` when unset | Safe: legacy compatibility |
| `BUILD_PROFILE_DIR` | `Makefile`, `scripts/run_profiled.sh` | Default `$(BUILD_WORKDIR)/build/profiles.d` | Aliased into `PROFILE_DIR` | Safe: defaulted |
| `PROFILE_DIR` | `Makefile`, `scripts/run_profiled.sh` (legacy alias) | Env or Makefile | Aliased to `BUILD_PROFILE_DIR` when unset | Safe: legacy compatibility |
| `BUILD_PROFILE_RUN` | `Makefile` | Default timestamp | Aliased into `PROFILE_RUN` | Safe: defaulted |
| `PROFILE_RUN` | `Makefile` (legacy alias) | Env or Makefile | Aliased to `BUILD_PROFILE_RUN` when unset | Safe: legacy compatibility |
| `PROFILE_LOG` | `Makefile`, `scripts/run_profiled.sh` | Makefile default or env | Default derived from `PROFILE_DIR` | Safe: defaulted |
| `XDG_DATA_HOME` | `Makefile`, `scripts/recipes/regen-makefile.sh`, `scripts/export_cnf_grammar.sh` | Defaults to `$(BUILD_WORKDIR)/build/xdg-data` or `${BUILD_WORKDIR:-${WORKDIR:-.}}` | Defaulted in Makefile and scripts | Safe: defaulted |
| `XDG_CACHE_HOME` | `Makefile`, `scripts/recipes/regen-makefile.sh`, `scripts/export_cnf_grammar.sh` | Defaults to `$(BUILD_WORKDIR)/build/xdg-cache` or `${BUILD_WORKDIR:-${WORKDIR:-.}}` | Defaulted in Makefile and scripts | Safe: defaulted |
| `AGDA` | `Makefile`, `Makefile.generated` | Makefile uses `.local/agda` if present; can be overridden via env | Defaulted in Makefile | Safe: defaulted |
| `AGDA_FLAGS` | `Makefile`, `Makefile.generated` | Makefile default or env override | Default `-i src/agda --ghc-flag=-Wno-star-is-type` | Safe: defaulted |
| `AGDA_COMPILE_DIR` | `Makefile`, `Makefile.generated`, `scripts/recipes/regen-makefile.sh`, `scripts/export_cnf_grammar.sh` | Defaults to `$(BUILD_WORKDIR)/build/agda` or `${BUILD_WORKDIR:-${WORKDIR:-.}}` | Defaulted in Makefile and scripts | Safe: defaulted |
| `AGDA_COMPILE` | `Makefile`, `Makefile.generated` | Composed in Makefile | Default includes `--compile` and `--compile-dir` | Safe: defaulted |
| `BUILD_VENV_DIR` | `Makefile`, `Makefile.generated` | Defaults to `$(BUILD_WORKDIR)/build/venv/.venv` | Defaulted in Makefile | Safe: defaulted |
| `VIRTUAL_ENV` | `build/recipes/*` (python targets) | Defaults to `BUILD_VENV_DIR` | Defaulted in Makefile and recipe scripts | Safe: defaulted |

### GitHub Actions / CI Variables

| Variable | Consumers | Filled by | Guard / Default | Assumption Check |
| --- | --- | --- | --- | --- |
| `ACT` | `.github/workflows/*.yml` conditions | `act` runner | Used in `if` checks with default false | Safe: only used for branching |
| `GITHUB_ENV` | `scripts/act_prepare_workspace.sh`, `ci.yml` | GitHub Actions runtime | Guarded in `act_prepare_workspace.sh` | Safe when running in Actions/act |
| `GITHUB_OUTPUT` | `.github/scripts/detect-deferred-items.sh` (invoked via CI or local targets) | GitHub Actions runtime | Guarded in `detect-deferred-items.sh` | Safe in Actions; guarded in script |
| `CI_GITHUB_TOKEN` | `.github/scripts/sync-roadmap-issues.sh` | GitHub Actions secrets | Script validates presence (fallback to `GITHUB_TOKEN`) | Safe: guarded before use |
| `GITHUB_TOKEN` | `.github/scripts/sync-roadmap-issues.sh` (legacy alias) | GitHub Actions secrets | Script validates presence | Safe: guarded before use |
| `GH_TOKEN` | `ci.yml` for `gh` CLI | GitHub Actions secrets | Not validated in `.github/scripts/create-or-update-tracking-issue.sh` | Risk: assumes `gh` is authenticated |
| `CI_GITHUB_REPOSITORY` | `.github/scripts/sync-roadmap-issues.sh` | GitHub Actions runtime | Script validates presence (fallback to `GITHUB_REPOSITORY`) | Safe: guarded before use |
| `GITHUB_REPOSITORY` | `.github/scripts/sync-roadmap-issues.sh` (legacy alias) | GitHub Actions runtime | Script validates presence | Safe: guarded before use |
| `CI_GITHUB_SHA` | `ci.yml`, `.github/scripts/create-or-update-tracking-issue.sh` | GitHub Actions runtime | Script defaults to `unknown` if unset | Safe: guarded |
| `CI_DEFERRED_REPORT_FILE` | `ci.yml`, `.github/scripts/create-or-update-tracking-issue.sh`, `.github/scripts/detect-deferred-items.sh` | GitHub Actions runtime | Defaults to `${CI_REPORT_DIR}/deferred-items.md` when `CI_REPORT_DIR` is set, otherwise `deferred-items.md` | Safe: defaulted |
| `CI_DEFERRED_SUMMARY_FILE` | `ci.yml`, `.github/scripts/create-or-update-tracking-issue.sh`, `.github/scripts/detect-deferred-items.sh` | GitHub Actions runtime | Defaults to `${CI_REPORT_DIR}/deferred-summary.json` when `CI_REPORT_DIR` is set, otherwise `deferred-summary.json` | Safe: defaulted |
| `CI_TASKS_FILE` | `.github/scripts/sync-roadmap-issues.sh` | GitHub Actions runtime | Defaults to `.github/roadmap/tasks.json` when unset | Safe: defaulted |
| `CI_REPORT_DIR` | `ci.yml` | GitHub Actions runtime | Job-scoped: `build/reports/agda`, `.../docs`, `.../roadmap`, `.../python` | Safe: scoped |
| `CI_ARTIFACT_DIR` | `ci.yml` | GitHub Actions runtime | Job-scoped: same as `CI_REPORT_DIR` | Safe: scoped |
| `METACATAGORY_STRICT_ROUNDTRIP` | `ci.yml`, local runs | Default strict structural validation for roundtrip checks | Default `true` | Safe: strict by default |
| `METACATAGORY_DEEP_STRICT_ROUNDTRIP` | `scripts/validate_json_roundtrip.py` | Shell/CI env | Default `false`; when `true` also compares labeled edges in strict roundtrip | Safe: optional |
| `AGDA_DIR` | `ci.yml` | Job env in `ci.yml` | Set in job `env` | Safe: provided by workflow |
| `AGDA_EXEC_OPTIONS` | `ci.yml` (exported to `GITHUB_ENV`) | Set in `ci.yml` step | Used indirectly by `agda` | Safe: set by workflow |
| `AGDA_STDLIB` | `ci.yml` | Job env in `ci.yml` | Set in job `env` | Safe: provided by workflow |

### Python Configuration Variables (`scripts/shared/config.py`)

| Variable | Consumers | Filled by | Guard / Default | Assumption Check |
| --- | --- | --- | --- | --- |
| `METACATAGORY_REPO_ROOT` | `scripts/shared/config.py` | Shell/CI env | Optional override | Safe: optional |
| `METACATAGORY_VERBOSE` | `scripts/shared/config.py` | Shell/CI env | Defaults to `false` | Safe: defaulted |
| `METACATAGORY_DRY_RUN` | `scripts/shared/config.py` | Shell/CI env | Defaults to `false` | Safe: defaulted |
| `METACATAGORY_PARALLEL` | `scripts/shared/config.py`, `scripts/shared/pipelines.py`, `scripts/json_decompose.py` | Shell/CI env | Defaults to `false` | Safe: defaulted |
| `METACATAGORY_STRICT` | `scripts/shared/config.py` | Shell/CI env | Defaults to `true` | Safe: defaulted |
| `METACATAGORY_WORKERS` | `scripts/shared/config.py`, `scripts/shared/pipelines.py`, `scripts/json_decompose.py` | Shell/CI env | Optional; parsed int | Safe: optional |
| `METACATAGORY_REPORT_MODE` | `scripts/shared/config.py`, report writers | Shell/CI env | `stdout` (default) or `write` | Safe: defaulted |
| `METACATAGORY_LOG_LEVEL` | `scripts/shared/config.py` | Shell/CI env | Optional; uppercased | Safe: optional |
| `METACATAGORY_LOG_FILE` | `scripts/shared/config.py` | Shell/CI env | Optional | Safe: optional |

Note: tests reference `METACATAGORY_FAIL_ON_WARNING`, `METACATAGORY_LOG_STRUCTURED`,
`METACATAGORY_JSON_INDENT`, `METACATAGORY_CREATE_PARENTS`, and
`METACATAGORY_BACKUP_ON_OVERWRITE`, but `scripts/shared/config.py` does not read these.

### Docker / Rootless Helper Variables

| Variable | Consumers | Filled by | Guard / Default | Assumption Check |
| --- | --- | --- | --- | --- |
| `XDG_RUNTIME_DIR` | `scripts/docker-rootless-env.sh` | Computed from `$UID` | Exported by script | Safe when run as normal user |
| `DOCKER_HOST` | `scripts/docker-rootless-env.sh` | Computed from `$UID` | Exported by script | Safe when run as normal user |

### Validation Notes and Potential Gaps

1. `ci.yml` now uses `BUILD_WORKDIR` for all working-directory references to avoid
   unset `ACT_WORKDIR` reliance in coverage jobs.
2. `.github/scripts/create-or-update-tracking-issue.sh` assumes `GITHUB_SHA` and an
   authenticated `gh` CLI (`GH_TOKEN`/`GITHUB_TOKEN`). It does not validate these.
3. `METACATAGORY_*` env vars referenced in tests exceed the set read in
   `scripts/shared/config.py`. Either add parsing or update tests/docs to match.
