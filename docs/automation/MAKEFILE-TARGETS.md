# Makefile Generation Report

## Generation Assumptions (Verified by ExporterMakefile)

1. **Agda toolchain**: Available as `$(AGDA)` with `-i src/agda --include-path=$(AGDA_PRIM_DIR) --no-default-libraries --no-libraries --ghc-flag=-Wno-star-is-type`.
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
| `build/venv/dir.stamp` | Ensure build/venv exists | mutative |
| `build/venv/requirements-main.stamp` | Track requirements.txt changes | mutative |
| `build/venv/requirements-dev.stamp` | Track requirements-dev.txt changes | mutative |
| `build/venv/requirements-inputs.stamp` | Track Python dependency inputs | mutative |
| `build/venv/venv_created.stamp` | Create Python virtualenv | mutative |
| `build/venv/pip_upgraded.stamp` | Upgrade pip in virtualenv | mutative |
| `build/venv/requirements_installed.stamp` | Install Python dependencies | mutative |
| `build/venv/python_setup.stamp` | Create Python venv and install dependencies | mutative |
| `python-test` | Run Python tests (includes pytest suite) | mutative |
| `python-verified` | Witness target: test suite contracted | mutative |
| `roadmap-merge` | Merge ingestion streams | mutative |
| `build/ingested_metadata.json` | Ingest GP metadata | mutative |
| `build/canonical_enriched.json` | Enrich canonical roadmap | mutative |
| `roadmap-enrich` | Enrich roadmap with graph data | mutative |
| `.github/roadmap/tasks.json` | Export canonical roadmap to JSON | mutative |
| `ROADMAP.md` | Export canonical roadmap to Markdown | mutative |
| `build/reports/tasks_enriched.md` | Export enriched roadmap | mutative |
| `roadmap-export-deps` | Export roadmap dependency graph | mutative |
| `roadmap-validate-json` | Validate canonical JSON | mutative |
| `roadmap-validate-md` | Validate canonical Markdown | mutative |
| `roadmap-validate-triangle` | Verify Triangle Identity (Agda <-> JSON <-> MD) | mutative |
| `build/gp_roadmap_sppf.json` | Export SPPF structure | mutative |
| `roadmap-all-enriched` | Build all enriched artifacts | mutative |
| `md-lint` | Lint all markdown files (fail on error) | read-only |
| `build/reports/docs-lint.json` | Emit Agda docs lint report | mutative |
| `docs-lint` | Lint Agda docs coverage (fail on error) | read-only |
| `md-fix` | Auto-fix markdown lint errors | mutative |
| `md-normalize` | Normalize markdown formatting | mutative |
| `build/agda/Plan/CIM/RoadmapExporterMain` | Compile roadmap exporter binary | mutative |
| `build/reports/roadmap_ast.txt` | Export roadmap AST report | mutative |
| `build/agda/Plan/CIM/ModuleExporter` | Compile module exporter binary | mutative |
| `docs-modules` | Generate per-module markdown documentation | mutative |
| `docs-normalize` | Normalize generated markdown after docs export | mutative |
| `docs-all` | Generate documentation (markdown only) | mutative |
| `docs-validate` | Validate documentation integrity | mutative |
| `json-decompose` | Decompose monolithic JSON to hierarchical structure | mutative |
| `json-decompose-prebuilt` | Decompose monolithic JSON using prebuilt inputs | mutative |
| `build/dependency_graph_recomposed.json` | Recompose hierarchical JSON back to monolithic form | mutative |
| `json-roundtrip-validate` | Validate JSON decomposition roundtrip | mutative |
| `json-roundtrip-validate-light` | Validate JSON decomposition roundtrip (light) | mutative |
| `json-decompose-enriched` | Decompose canonical_enriched.json into item hierarchy | mutative |
| `build/canonical_enriched_recomposed.json` | Recompose enriched items into canonical_enriched.json | mutative |
| `json-roundtrip-validate-enriched` | Validate enriched roundtrip | mutative |
| `json-decompose-planning` | Decompose planning_index.json into plan hierarchy | mutative |
| `build/planning_index_recomposed.json` | Recompose planning items into planning_index.json | mutative |
| `json-roundtrip-validate-planning` | Validate planning roundtrip | mutative |
| `build/dir.stamp` | Ensure build/ exists | mutative |
| `build/reports/dir.stamp` | Ensure build/reports exists | mutative |
| `docs/status/dir.stamp` | Ensure docs/status exists | mutative |
| `intake-lint` | Lint intake files specifically | read-only |
| `build/canonical_roadmap.json` | Generate canonical roadmap JSON from intake | mutative |
| `intake-scan` | Scan intake directory for new files | mutative |
| `makefile-validate` | Validate Makefile consistency | read-only |
| `node-deps` | Install Node.js dependencies | mutative |
| `build/agda/TechnicalDebt/DeferredItemsOrchestrationFFI` | Compile deferred items scanner (MAlonzo + binary) | mutative |
| `deferred-items-dirs` | Ensure deferred items output directories exist | mutative |
| `deferred-items` | Scan for TODOs and FIXMEs (Agda FFI binary) | mutative |
| `act-list` | List available GitHub Actions jobs (act) | mutative |
| `act-ci` | Run CI workflow locally via act | mutative |
| `act-lint` | Run docs checks job locally via act | mutative |
| `act-makefile-validate` | Run Agda exports job locally via act | mutative |
| `act-roadmap-sync` | Run roadmap/JSON job locally via act | mutative |
| `act-deferred` | Run Python/debt job locally via act | mutative |
| `act-all` | Run all workflows locally via act | mutative |
| `badges` | Generate status badges | mutative |
| `build/agda/PriorityOrchestrationFFI` | Compile Agda priority orchestration (MAlonzo + binary) | mutative |
| `priority-strategy-profiles` | Compile and run Agda priority orchestration (generate strategy profiles) | mutative |
| `.github/badges/weights.json` | Normalize Agda strategy profiles into badge weights | mutative |
| `build/priority_profile_inputs.stamp` | Track priority profile inputs | mutative |
| `build/agda/Plan/CIM/PriorityProfileExport` | Compile PriorityProfileExport (MAlonzo + binary) | mutative |
| `build/priority_profile.json` | Export structured priority profile (lazy; derived from planning index) | mutative |
| `priority-refresh` | Re-run priority pipeline and refresh roadmap/badge outputs | mutative |
| `roadmap-index` | Compile Roadmap Index | mutative |
| `build/agda/Plan/CIM/PlanningExport` | Compile PlanningExport (MAlonzo + binary) | mutative |
| `data/planning_index.json` | Export planning index to JSON | mutative |
| `planning-kernel` | Compile Planning Kernel | mutative |
| `roadmap-sync` | Sync roadmap with external tracker | mutative |
| `roadmap-sppf` | Compile Roadmap SPPF | mutative |
| `build/diagrams/dir.stamp` | Ensure build/diagrams exists | mutative |
| `build/diagrams/agda-deps-full.dot` | Generate dependency graph | mutative |
| `roadmap-deps-graph` | Generate dependency graph | mutative |
| `build/agda/Plan/CIM/DependencyGraphExport` | Compile DependencyGraphExport (MAlonzo + binary) | mutative |
| `data/dependency_graph.json` | Export dependency graph JSON via Agda (from agda-deps-full.dot) | mutative |
| `regen-agda` | Regenerate Agda build outputs | mutative |
| `regen-exports` | Regenerate Agda export artifacts | mutative |
| `regen-roadmap` | Regenerate roadmap artifacts | mutative |
| `regen-docs` | Regenerate documentation exports | mutative |
| `regen-badges` | Regenerate badge artifacts | mutative |
| `regen-intake` | Regenerate intake-derived artifacts | mutative |
| `regen-all` | Regenerate all build artifacts | mutative |
| `all` | Build all code and documentation | mutative |
| `debt-check` | Run debt tracking validation | mutative |
| `check-infra` | Validate graph + Makefile | mutative |
| `check-docs` | Validate documentation outputs | mutative |
| `check-roadmap` | Validate roadmap exports | mutative |
| `check-python` | Validate Python test suite | mutative |
| `check-json` | Validate JSON roundtrip outputs | mutative |
| `check-debt` | Validate deferred items + intake scan | mutative |
| `check-all` | Run full validation suite | mutative |
| `check` | Run all validation checks (alias) | mutative |
| `validate-constructive` | Regenerate and validate all artifacts | mutative |
| `ci-light` | Lightweight CI target (no GHC backend) | mutative |
| `ci-preflight` | Fast guard: graph + makefile docs | mutative |
| `docker-rootless-status` | Check rootless Docker daemon status | mutative |
| `docker-build` | Build Docker image (metacatagory:dev) | mutative |
| `docker-build-ghcr` | Build and tag image for GHCR (requires GHCR_REGISTRY and GHCR_USERNAME env vars) | mutative |
| `docker-push-ghcr` | Push image to GHCR (requires docker login; set GHCR_REGISTRY and GHCR_USERNAME) | mutative |
| `docker-all` | Build and push to GHCR (full pipeline) | mutative |
