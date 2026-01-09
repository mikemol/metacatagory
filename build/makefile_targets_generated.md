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

Mutability indicates whether a target is allowed to write artifacts or update the workspace.

| Target | Description | Mutability |
| :--- | :--- | :--- |
| `regen-makefile` | Regenerate the Makefile from Agda source (Self-Hosting) | mutative |
| `python-build` | Prepare Python artifacts (interpreted; placeholder for future bytecode/vendor steps) | mutative |
| `python-test` | Run Python tests (includes pytest suite) | mutative |
| `python-verified` | Witness target: build + test contracted | mutative |
| `roadmap-merge` | Merge ingestion streams | mutative |
| `build/canonical_enriched.json` | Enrich canonical roadmap | mutative |
| `roadmap-enrich` | Enrich roadmap with graph data | mutative |
| `roadmap-export-json` | Export canonical roadmap to JSON | mutative |
| `roadmap-export-md` | Export canonical roadmap to Markdown | mutative |
| `roadmap-export-enriched` | Export enriched roadmap | mutative |
| `roadmap-export-deps` | Export roadmap dependency graph | mutative |
| `roadmap-validate-json` | Validate canonical JSON | mutative |
| `roadmap-validate-md` | Validate canonical Markdown | mutative |
| `roadmap-validate-triangle` | Verify Triangle Identity (Agda <-> JSON <-> MD) | mutative |
| `roadmap-sppf-export` | Export SPPF structure | mutative |
| `roadmap-all-enriched` | Build all enriched artifacts | mutative |
| `md-lint` | Lint all markdown files (fail on error) | read-only |
| `md-fix` | Auto-fix markdown lint errors | mutative |
| `md-normalize` | Normalize markdown formatting | mutative |
| `docs-generate` | Compile and run Roadmap Exporter | mutative |
| `docs-modules` | Generate per-module markdown documentation | mutative |
| `docs-all` | Generate documentation (markdown only) | mutative |
| `docs-validate` | Validate documentation integrity | mutative |
| `json-decompose` | Decompose monolithic JSON to hierarchical structure | mutative |
| `json-decompose-prebuilt` | Decompose monolithic JSON using prebuilt inputs | mutative |
| `json-recompose` | Recompose hierarchical JSON back to monolithic form | mutative |
| `json-recompose-light` | Recompose hierarchical JSON (prebuilt, fallback-safe) | mutative |
| `json-roundtrip-validate` | Validate JSON decomposition roundtrip | mutative |
| `json-roundtrip-validate-light` | Validate JSON decomposition roundtrip (light) | mutative |
| `json-decompose-enriched` | Decompose canonical_enriched.json into item hierarchy | mutative |
| `json-recompose-enriched` | Recompose enriched items into canonical_enriched.json | mutative |
| `json-roundtrip-validate-enriched` | Validate enriched roundtrip | mutative |
| `json-decompose-planning` | Decompose planning_index.json into plan hierarchy | mutative |
| `json-recompose-planning` | Recompose planning items into planning_index.json | mutative |
| `json-roundtrip-validate-planning` | Validate planning roundtrip | mutative |
| `intake-lint` | Lint intake files specifically | read-only |
| `build/canonical_roadmap.json` | Generate canonical roadmap JSON from intake | mutative |
| `intake-scan` | Scan intake directory for new files | mutative |
| `makefile-validate` | Validate Makefile consistency | read-only |
| `node-deps` | Install Node.js dependencies | mutative |
| `deferred-items` | Scan for TODOs and FIXMEs (Agda FFI binary) | mutative |
| `act-list` | List available GitHub Actions jobs (act) | mutative |
| `act-ci` | Run CI workflow locally via act | mutative |
| `act-lint` | Run markdown linting workflow locally via act | mutative |
| `act-markdown-fix` | Run markdown auto-fix workflow locally via act | mutative |
| `act-makefile-validate` | Run makefile validation workflow locally via act | mutative |
| `act-roadmap-sync` | Run roadmap sync workflow locally via act | mutative |
| `act-deferred` | Run deferred items workflow locally via act | mutative |
| `act-badges` | Run badge update workflow locally via act | mutative |
| `act-all` | Run all workflows locally via act | mutative |
| `badges` | Generate status badges | mutative |
| `priority-strategy-profiles` | Compile and run Agda priority orchestration (generate strategy profiles) | mutative |
| `priority-badge-weights` | Normalize Agda strategy profiles into badge weights | mutative |
| `priority-profile-json` | Export structured priority profile (lazy; derived from planning index) | mutative |
| `priority-refresh` | Re-run priority pipeline and refresh roadmap/badge outputs | mutative |
| `roadmap-index` | Compile Roadmap Index | mutative |
| `planning-index-json` | Export planning index to JSON | mutative |
| `planning-kernel` | Compile Planning Kernel | mutative |
| `roadmap-sync` | Sync roadmap with external tracker | mutative |
| `roadmap-sppf` | Compile Roadmap SPPF | mutative |
| `build/diagrams/agda-deps-full.dot` | Generate dependency graph | mutative |
| `roadmap-deps-graph` | Generate dependency graph | mutative |
| `dependency-graph-json` | Export dependency graph JSON via Agda (from agda-deps-full.dot) | mutative |
| `all` | Build all code and documentation | mutative |
| `debt-check` | Run debt tracking validation | mutative |
| `validate-constructive` | Run all constructive build targets | mutative |
| `check` | Run all validation checks | mutative |
| `ci-light` | Lightweight CI target (no GHC backend) | mutative |
| `ci-preflight` | Fast guard: graph + makefile docs | mutative |
| `docker-rootless-status` | Check rootless Docker daemon status | mutative |
| `docker-build` | Build Docker image (metacatagory:dev) | mutative |
| `docker-build-ghcr` | Build and tag image for GHCR (requires GHCR_REGISTRY and GHCR_USERNAME env vars) | mutative |
| `docker-push-ghcr` | Push image to GHCR (requires docker login; set GHCR_REGISTRY and GHCR_USERNAME) | mutative |
| `docker-all` | Build and push to GHCR (full pipeline) | mutative |
