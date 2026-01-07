# Makefile Generation Report

## Generation Assumptions (Verified by ExporterMakefile)

1. **Agda toolchain**: Available as `$(AGDA)` with `-i src/agda --ghc-flag=-Wno-star-is-type`.
2. **Dependency graph**: `build/diagrams/agda-deps-full.dot` - Status: OK
3. **Module classification**: Agda.* modules excluded from internal deps; others are internal.
4. **External tools**: python3, npx, npm, act, docker available on PATH.
5. **Shell environment**: bash with set -euo pipefail for recipe isolation.
6. **Parallelism**: Automatic core detection; override via MAKEFLAGS if needed.
7. **Profiling**: JSONL logs optional; $(PROFILE_LOG) path is fallback-safe.

## Key Orchestration Targets

- `check`: Full validation suite (makefile + docs + tests + code)
- `ci-light`: Lightweight CI without GHC backend
- `all`: Complete Agda + documentation build
- `docker-all`: Docker build and GHCR push

## All Generated Targets

| Target | Description |
| :--- | :--- |
| `regen-makefile` | Regenerate the Makefile from Agda source (Self-Hosting) |
| `python-build` | Prepare Python artifacts (interpreted; placeholder for future bytecode/vendor steps) |
| `python-test` | Run Python tests (includes pytest suite) |
| `python-verified` | Witness target: build + test contracted |
| `roadmap-merge` | Merge ingestion streams |
| `build/canonical_enriched.json` | Enrich canonical roadmap |
| `roadmap-enrich` | Enrich roadmap with graph data |
| `roadmap-export-json` | Export canonical roadmap to JSON |
| `roadmap-export-md` | Export canonical roadmap to Markdown |
| `roadmap-export-enriched` | Export enriched roadmap |
| `roadmap-export-deps` | Export roadmap dependency graph |
| `roadmap-validate-json` | Validate canonical JSON |
| `roadmap-validate-md` | Validate canonical Markdown |
| `roadmap-validate-triangle` | Verify Triangle Identity (Agda <-> JSON <-> MD) |
| `roadmap-sppf-export` | Export SPPF structure |
| `roadmap-all-enriched` | Build all enriched artifacts |
| `md-lint` | Lint all markdown files (fail on error) |
| `md-fix` | Auto-fix markdown lint errors |
| `md-normalize` | Normalize markdown formatting |
| `docs-generate` | Compile and run Roadmap Exporter |
| `docs-modules` | Generate per-module markdown documentation |
| `docs-all` | Generate documentation (markdown only) |
| `docs-validate` | Validate documentation integrity |
| `json-decompose` | Decompose monolithic JSON to hierarchical structure |
| `json-decompose-prebuilt` | Decompose monolithic JSON using prebuilt inputs |
| `json-recompose` | Recompose hierarchical JSON back to monolithic form |
| `json-recompose-light` | Recompose hierarchical JSON (prebuilt, fallback-safe) |
| `json-roundtrip-validate` | Validate JSON decomposition roundtrip |
| `json-roundtrip-validate-light` | Validate JSON decomposition roundtrip (light) |
| `json-decompose-enriched` | Decompose canonical_enriched.json into item hierarchy |
| `json-recompose-enriched` | Recompose enriched items into canonical_enriched.json |
| `json-roundtrip-validate-enriched` | Validate enriched roundtrip |
| `json-decompose-planning` | Decompose planning_index.json into plan hierarchy |
| `json-recompose-planning` | Recompose planning items into planning_index.json |
| `json-roundtrip-validate-planning` | Validate planning roundtrip |
| `intake-lint` | Lint intake files specifically |
| `build/canonical_roadmap.json` | Generate canonical roadmap JSON from intake |
| `intake-scan` | Scan intake directory for new files |
| `makefile-validate` | Validate Makefile consistency |
| `node-deps` | Install Node.js dependencies |
| `deferred-items` | Scan for TODOs and FIXMEs (Agda FFI binary) |
| `act-list` | List available GitHub Actions jobs (act) |
| `act-ci` | Run CI workflow locally via act |
| `act-lint` | Run markdown linting workflow locally via act |
| `act-markdown-fix` | Run markdown auto-fix workflow locally via act |
| `act-makefile-validate` | Run makefile validation workflow locally via act |
| `act-roadmap-sync` | Run roadmap sync workflow locally via act |
| `act-deferred` | Run deferred items workflow locally via act |
| `act-badges` | Run badge update workflow locally via act |
| `act-all` | Run all workflows locally via act |
| `badges` | Generate status badges |
| `priority-strategy-profiles` | Compile and run Agda priority orchestration (generate strategy profiles) |
| `priority-badge-weights` | Normalize Agda strategy profiles into badge weights |
| `priority-profile-json` | Export structured priority profile (lazy; derived from planning index) |
| `priority-refresh` | Re-run priority pipeline and refresh roadmap/badge outputs |
| `roadmap-index` | Compile Roadmap Index |
| `planning-index-json` | Export planning index to JSON |
| `planning-kernel` | Compile Planning Kernel |
| `roadmap-sync` | Sync roadmap with external tracker |
| `roadmap-sppf` | Compile Roadmap SPPF |
| `build/diagrams/agda-deps-full.dot` | Generate dependency graph |
| `roadmap-deps-graph` | Generate dependency graph |
| `dependency-graph-json` | Export dependency graph JSON via Agda (from agda-deps-full.dot) |
| `all` | Build all code and documentation |
| `debt-check` | Run debt tracking validation |
| `validate-constructive` | Run all constructive build targets |
| `check` | Run all validation checks |
| `ci-light` | Lightweight CI target (no GHC backend) |
| `ci-preflight` | Fast guard: graph + makefile docs |
| `docker-rootless-status` | Check rootless Docker daemon status |
| `docker-build` | Build Docker image (metacatagory:dev) |
| `docker-build-ghcr` | Build and tag image for GHCR (requires GHCR_REGISTRY and GHCR_USERNAME env vars) |
| `docker-push-ghcr` | Push image to GHCR (requires docker login; set GHCR_REGISTRY and GHCR_USERNAME) |
| `docker-all` | Build and push to GHCR (full pipeline) |
