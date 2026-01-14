# Agda Path Audit

This document models how Agda-related paths are computed and modified in this repo,
with an emphasis on prim/MAlonzo resolution and the act workflows.

## Scope

- Path inputs and overrides for `agda` invocations.
- Locations that can introduce *multiple* prim roots.
- Scripts and workflows that bypass the standard `AGDA_*` plumbing.

## Key path roots in play

- `AGDA_DATA_DIR` (from `agda --print-agda-data-dir` or environment)
  - Example in act: `/home/runner/.cabal/store/.../share`
- System prim fallback: `/usr/share/agda/lib/prim`
- System MAlonzo fallback: `/usr/share/agda/MAlonzo/src`
- XDG data/cache overrides (repo-local):
  - `XDG_DATA_HOME=$BUILD_WORKDIR/build/xdg-data`
  - `XDG_CACHE_HOME=$BUILD_WORKDIR/build/xdg-cache`

## Where paths are set or modified

### Makefile (global defaults)

File: `Makefile`

- **AGDA_BIN**: `agda` (override via env/make var)
- **AGDA_DATA_DIR**: `env AGDA_EXEC_OPTIONS= $(AGDA_BIN) --library-file=/dev/null --no-libraries --no-default-libraries --print-agda-data-dir`
- **AGDA_PRIM_DIR**: `$(AGDA_DATA_DIR)/lib/prim`
- **AGDA_ENV**: forces
  - `AGDA_EXEC_OPTIONS=` (empty)
  - `AGDA_DATA_DIR=$(AGDA_DATA_DIR)`
  - `XDG_DATA_HOME`, `XDG_CACHE_HOME`
- **AGDA_FLAGS**: includes `--include-path=$(AGDA_PRIM_DIR)` and `--no-libraries --no-default-libraries`

### Recipe preamble (generated for each build target)

Source: `src/agda/Examples/ExporterMakefile.agda`
Generated into: `build/recipes/**/*.sh`

Behavior:

1) Sets `BUILD_WORKDIR` from `WORKDIR` or `ACT_WORKDIR`.
2) Sets `XDG_DATA_HOME`/`XDG_CACHE_HOME` under `$BUILD_WORKDIR/build`.
3) Determines `AGDA_BIN` (defaults to `agda` unless overridden).
4) Clears `AGDA_EXEC_OPTIONS`.
5) Computes `AGDA_DATA_DIR` using `agda --print-agda-data-dir` (with `--no-libraries --no-default-libraries`).
6) Sets `AGDA_PRIM_DIR` to `$AGDA_DATA_DIR/lib/prim`.
7) Exports `AGDA_FLAGS` with `--include-path=$AGDA_PRIM_DIR` and `--no-libraries --no-default-libraries`.

### Makefile regeneration

File: `scripts/recipes/regen-makefile.sh`

- Mirrors the recipe preamble logic for `AGDA_BIN`, `AGDA_DATA_DIR`, and
  `AGDA_PRIM_DIR`.
- Compiles `src/agda/Examples/ExporterMakefile.agda` with
  `--no-libraries --no-default-libraries` and `--include-path=$AGDA_PRIM_DIR`.

## External sources that can affect Agda resolution

- **Agda’s own data directory**: determined by the binary at runtime.
  - In act, this is typically the cabal store path under `/home/runner/.cabal/.../share`.
- **System prim install**: `/usr/share/agda/lib/prim` (should no longer be
  pulled in by default; only the data-dir prim should be used).
- **`AGDA_EXEC_OPTIONS`**: environment variable read by Agda.
  - We explicitly clear it in Makefile and recipe preambles, but it can be
    reintroduced by the environment if a shell or toolchain sets it later.
- **`~/.agda/libraries` and `~/.agda/defaults`**:
  - Disabled by `--no-libraries --no-default-libraries`, but if those flags
    are missing in any script, they can reintroduce stdlib paths.

## Places that bypass `AGDA_FLAGS` (divergent path logic)

These call `agda` directly and can ignore `AGDA_PRIM_DIR` or `--no-libraries`.

- `scripts/export_cnf_grammar.sh`
- `scripts/phase3-validate.sh`
- `scripts/phase3-integration.sh`
- `scripts/demo-concrete-execution.sh`

## Path conflict model (why the ambiguity happens)

The ambiguity arises when:

- Agda itself resolves prim from its **data dir** (e.g. cabal store), AND
- The invocation also includes `--include-path=/usr/share/agda/lib/prim`.

This yields two valid prim roots:

1) `/home/runner/.cabal/.../share/lib/prim`
2) `/usr/share/agda/lib/prim`

## Observed act specifics

- act sets `WORKDIR=/tmp/act-workdir`, so recipe preambles rewrite
  `XDG_DATA_HOME`/`XDG_CACHE_HOME` under `/tmp/act-workdir/build`.
- act runs as root in setup/checkout and performs `chown -R 1000:1000`
  on the workspace inside the container.

## Audit summary (single source of truth)

- **Primary control plane**: generated recipe preamble from
  `src/agda/Examples/ExporterMakefile.agda`
- **Makefile defaults**: should match the preamble logic
- **Known divergent scripts**: the four `scripts/*.sh` listed above

## Suggested next checks

- Add a guarded debug print (e.g. `AGDA_DEBUG=1`) in the preamble to emit:
  `AGDA_BIN`, `AGDA_DATA_DIR`, `AGDA_PRIM_DIR`, `AGDA_FLAGS`,
  and `agda --print-agda-data-dir`.
- Ensure `AGDA_PRIM_DIR` resolves to the *same* root as `AGDA_DATA_DIR` to
  avoid dual prim roots.
