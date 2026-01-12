# act-ci Findings Report (for image maintainer)

This report summarizes the failures and environmental issues observed when running `make act-ci` with `ghcr.io/mikemol/act-ubuntu-agda:latest`, plus the workflow adjustments applied in the repo. It is intended for the agent maintaining the ACT worker image.

## Key Findings

1) **act mutates host file ownership**
- Each `act` run executes (from logs):
  - `docker exec cmd=[chown -R 1000:1000 /home/mikemol/github/metacatagory] user=0`
- With rootless/user‑namespaced Docker, UID/GID `1000:1000` inside the container maps to `100999:100999` on the host.
- This is why repo files (e.g., `.github/workflows/ci.yml`) are repeatedly owned by `100999` after ACT runs.
- This is act behavior, not repo logic. Removing `-b` from `.actrc` reduces bind mounts but act still does `docker cp` + `chown`, so ownership changes persist.

2) **Agda data dir must exist and be used**
- In the refreshed image:
  - `agda --print-agda-data-dir` => `/home/runner/.cabal/store/ghc-8.8.4/Agda-2.8.0-.../share`
- That directory is **not present** until `agda --setup` is run.
- Without setup, Agda fails when trying to load prim files or library metadata.
- After `agda --setup`, prim libs appear under `$AGDA_DATA_DIR/lib/prim`.
- Using `/usr/share/agda/lib/prim` caused `COMPILE pragma not allowed in safe mode` errors (because those prim files carry `{-# OPTIONS --safe #-}` and include COMPILE pragmas).
- Therefore, **ACT should use** `$AGDA_DATA_DIR/lib/prim` and run `agda --setup` before Agda checks.

3) **`rsync` missing in the image**
- The workflow copies repo content into `/tmp/act-workdir` during ACT to prevent mutating the host.
- Initial version used `rsync`, which failed: `rsync: command not found`.
- We switched to: `tar -cf - . | (cd "$ACT_TMP" && tar -xf -)` which works.
- `rsync` is not required, but **tar must be present** (it is).

4) **Python tests fail: pytest missing**
- `make check` calls `python-test`, which runs `$(VIRTUAL_ENV)/bin/python -m pytest tests/ -v`.
- The repo now installs `requirements.txt` + `requirements-dev.txt` via `build/venv/python_setup.stamp`, and `requirements-dev.txt` includes pytest.
- Action: ensure CI runs the python setup target (or bake pytest into the image to avoid network installs).

5) **GLIBC mismatch for compiled Agda binaries**
- During `build/reports/roadmap_ast.txt` / `docs-modules`, Agda compiles and then executes:
  - `/tmp/act-workdir/build/agda/Plan/CIM/RoadmapExporterMain`
  - `/tmp/act-workdir/build/agda/Plan/CIM/ModuleExporter`
- These fail with:
  - `.../libc.so.6: version 'GLIBC_2.38' not found`
- This indicates the image provides older glibc than the compiled artifacts expect.
- Action options for image:
  - Base on a distro with glibc >= 2.38, or
  - Build/run with a consistent glibc (avoid host/other image mixing), or
  - Avoid executing those binaries under ACT (if acceptable for the workflow intent).

6) **Agda/GHC versions in the image**
- `agda --version`: 2.8.0
- `ghc --version`: 8.8.4
- Earlier GHC panics (e.g., `Prelude.chr: bad argument`) were seen with older images; after the updated image + `agda --setup` the big Agda check proceeds. Current blockers are GLIBC + pytest.

## Workflow changes already applied in repo

File: `/home/mikemol/github/metacatagory/.github/workflows/ci.yml`

- **ACT-only workdir copy** (non‑mutative):
  - Copy repo to `/tmp/act-workdir` and set `ACT_WORKDIR`.
- **Swap rsync to tar**:
  - `tar -cf - . | (cd "$ACT_TMP" && tar -xf -)`
- **Run all build steps in `ACT_WORKDIR`**:
  - Python deps install, npm install, `make check`, `makefile_coverage.py`.
- **Run `agda --setup` under ACT**:
  - Ensures `$AGDA_DATA_DIR` exists.
- **Prefer `$AGDA_DATA_DIR/lib/prim` for prim path**:
  - Avoids safe‑mode COMPILE pragma errors from `/usr/share/agda/lib/prim`.
- **Disable deferred-items job under ACT**:
  - prevents issue/PR mutation; job steps gated with `if: ${{ !env.ACT }}`.

File: `/home/mikemol/github/metacatagory/.actrc`

- Removed `-b` bind mount.

## Current act-ci blockers (latest run)

1) **pytest missing**
- `pytest: command not found` during `python-test`.

2) **GLIBC mismatch**
- `RoadmapExporter` and `ModuleExporter` binaries require `GLIBC_2.38`, not available in the image.

3) **Host ownership changes**
- act still chowns the host workspace to UID/GID 1000 inside container (maps to 100999 on host).

## Recommendations for image maintainer

1) **Install pytest**
- Add to `requirements.txt` or bake into the image.

2) **Resolve glibc mismatch**
- Upgrade base image to glibc >= 2.38 (e.g., Ubuntu 24.04), or
- Ensure all compiled outputs are produced & run in an environment with matching glibc.

3) **Pre-run `agda --setup` during image build** (optional)
- Saves CI time and ensures `$AGDA_DATA_DIR/lib/prim` exists by default.

4) **Investigate act chown behavior**
- This appears to be `act`/Docker behavior with rootless daemon. If you want to prevent host ownership changes, consider:
  - a disposable worktree, or
  - non‑bind‑mount and non‑`docker cp` approaches, if supported by act.

## Notes for reproducing

Commands used during ACT runs:
- `mise exec -- make act-ci`
- act image: `ghcr.io/mikemol/act-ubuntu-agda:latest`

Key failures observed in logs:
- `pytest: command not found`
- `GLIBC_2.38 not found` when running compiled Agda binaries
- Ownership flip to uid/gid `100999` on host
