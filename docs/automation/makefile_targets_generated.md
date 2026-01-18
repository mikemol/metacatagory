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
| `regen-makefile` | Regenerate the Makefile from Agda source (Self-Hosting) | repo-write |
| `build/venv/dir.stamp` | Ensure build/venv exists | build-only |
| `build/venv/requirements-main.stamp` | Track requirements.txt changes | build-only |
| `build/venv/requirements-dev.stamp` | Track requirements-dev.txt changes | build-only |
| `build/venv/requirements-inputs.stamp` | Track Python dependency inputs | build-only |
| `build/venv/venv_created.stamp` | Create Python virtualenv | build-only |
| `build/venv/pip_upgraded.stamp` | Upgrade pip in virtualenv | build-only |
| `build/venv/requirements_installed.stamp` | Install Python dependencies | build-only |
| `build/venv/python_setup.stamp` | Create Python venv and install dependencies | build-only |
| `python-test` | Run Python tests (includes pytest suite) | repo-write |
| `python-verified` | Witness target: test suite contracted | repo-write |
| `build/canonical_roadmap.json` | Merge ingestion streams into canonical roadmap JSON | build-only |
| `roadmap-merge` | Merge ingestion streams | repo-write |
| `build/ingested_metadata.json` | Ingest GP metadata | build-only |
| `ingested-metadata-validate` | Validate ingested metadata schema | repo-write |
| `canonical-roadmap-validate` | Validate canonical roadmap schema | repo-write |
| `build/canonical_enriched.json` | Enrich canonical roadmap | build-only |
| `roadmap-enrich` | Enrich roadmap with graph data | repo-write |
| `.github/roadmap/tasks.json` | Export canonical roadmap to JSON | repo-write |
| `ROADMAP.md` | Export canonical roadmap to Markdown | repo-write |
| `build/reports/tasks_enriched.md` | Export enriched roadmap | report-only |
| `roadmap-export-deps` | Export roadmap dependency graph | repo-write |
| `roadmap-validate-json` | Validate canonical JSON | repo-write |
| `roadmap-validate-md` | Validate canonical Markdown | repo-write |
| `roadmap-validate-triangle` | Verify Triangle Identity (Agda <-> JSON <-> MD) | repo-write |
| `build/gp_roadmap_sppf.json` | Export SPPF structure | build-only |
| `build/formalisms/canonical_constructions.json` | Ingest formalism adapter spec | build-only |
| `formalism-validate` | Validate formalism constructions | repo-write |
| `build/formalisms/construction_templates.agda` | Generate formalism Agda templates | build-only |
| `roadmap-all-enriched` | Build all enriched artifacts | repo-write |
| `md-lint` | Lint all markdown files (fail on error) | report-only |
| `build/reports/docs-lint.json` | Emit Agda docs lint report | report-only |
| `docs-lint` | Lint Agda docs coverage (fail on error) | report-only |
| `md-fix` | Auto-fix markdown lint errors | repo-write |
| `md-normalize` | Normalize markdown formatting | repo-write |
| `build/agda/Plan/CIM/RoadmapExporterMain` | Compile roadmap exporter binary | build-only |
| `build/reports/roadmap_ast.txt` | Export roadmap AST report | report-only |
| `build/agda/Plan/CIM/ModuleExporter` | Compile module exporter binary | build-only |
| `docs-modules` | Generate per-module markdown documentation | repo-write |
| `docs-normalize` | Normalize generated markdown after docs export | repo-write |
| `docs-all` | Generate documentation (markdown only) | repo-write |
| `docs-validate` | Validate documentation integrity | repo-write |
| `json-decompose` | Decompose monolithic JSON to hierarchical structure | repo-write |
| `json-decompose-prebuilt` | Decompose monolithic JSON using prebuilt inputs | repo-write |
| `build/dependency_graph_recomposed.json` | Recompose hierarchical JSON back to monolithic form | build-only |
| `json-roundtrip-validate` | Validate JSON decomposition roundtrip | repo-write |
| `json-roundtrip-validate-light` | Validate JSON decomposition roundtrip (light) | repo-write |
| `json-decompose-enriched` | Decompose canonical_enriched.json into item hierarchy | repo-write |
| `build/canonical_enriched_recomposed.json` | Recompose enriched items into canonical_enriched.json | build-only |
| `json-roundtrip-validate-enriched` | Validate enriched roundtrip | repo-write |
| `json-decompose-planning` | Decompose planning_index.json into plan hierarchy | repo-write |
| `build/planning_index_recomposed.json` | Recompose planning items into planning_index.json | build-only |
| `json-roundtrip-validate-planning` | Validate planning roundtrip | repo-write |
| `build/dir.stamp` | Ensure build/ exists | build-only |
| `build/reports/dir.stamp` | Ensure build/reports exists | report-only |
| `docs/status/dir.stamp` | Ensure docs/status exists | repo-write |
| `intake-lint` | Lint intake files specifically | report-only |
| `intake-scan` | Scan intake directory for new files | repo-write |
| `makefile-validate` | Validate Makefile consistency | report-only |
| `check-makefile-generated` | Fail if Makefile.generated is stale | repo-write |
| `node-deps` | Install Node.js dependencies | repo-write |
| `build/agda/TechnicalDebt/DeferredItemsOrchestrationFFI` | Compile deferred items scanner (MAlonzo + binary) | build-only |
| `deferred-items-dirs` | Ensure deferred items output directories exist | repo-write |
| `deferred-items` | Scan for TODOs and FIXMEs (Agda FFI binary) | repo-write |
| `act-list` | List available GitHub Actions jobs (act) | repo-write |
| `act-ci` | Run CI workflow locally via act | repo-write |
| `act-lint` | Run docs checks job locally via act | repo-write |
| `act-makefile-validate` | Run Agda exports job locally via act | repo-write |
| `act-roadmap-sync` | Run roadmap/JSON job locally via act | repo-write |
| `act-deferred` | Run Python/debt job locally via act | repo-write |
| `act-all` | Run all workflows locally via act | repo-write |
| `badges` | Generate status badges | repo-write |
| `build/agda/PriorityOrchestrationFFI` | Compile Agda priority orchestration (MAlonzo + binary) | build-only |
| `priority-strategy-profiles` | Compile and run Agda priority orchestration (generate strategy profiles) | repo-write |
| `.github/badges/weights.json` | Normalize Agda strategy profiles into badge weights | repo-write |
| `build/priority_profile_inputs.stamp` | Track priority profile inputs | build-only |
| `build/agda/Plan/CIM/PriorityProfileExport` | Compile PriorityProfileExport (MAlonzo + binary) | build-only |
| `build/priority_profile.json` | Export structured priority profile (lazy; derived from planning index) | build-only |
| `priority-refresh` | Re-run priority pipeline and refresh roadmap/badge outputs | repo-write |
| `roadmap-index` | Compile Roadmap Index | repo-write |
| `build/agda/Plan/CIM/PlanningExport` | Compile PlanningExport (MAlonzo + binary) | build-only |
| `data/planning_index.json` | Export planning index to JSON | repo-write |
| `planning-kernel` | Compile Planning Kernel | repo-write |
| `roadmap-sync` | Sync roadmap with external tracker | repo-write |
| `roadmap-sppf` | Compile Roadmap SPPF | repo-write |
| `build/diagrams/dir.stamp` | Ensure build/diagrams exists | build-only |
| `build/diagrams/agda-deps-full.dot` | Generate dependency graph | build-only |
| `roadmap-deps-graph` | Generate dependency graph | repo-write |
| `build/agda/Plan/CIM/DependencyGraphExport` | Compile DependencyGraphExport (MAlonzo + binary) | build-only |
| `data/dependency_graph.json` | Export dependency graph JSON via Agda (from agda-deps-full.dot) | repo-write |
| `regen-agda` | Regenerate Agda build outputs | repo-write |
| `regen-exports` | Regenerate Agda export artifacts | repo-write |
| `regen-roadmap` | Regenerate roadmap artifacts | repo-write |
| `regen-docs` | Regenerate documentation exports | repo-write |
| `regen-badges` | Regenerate badge artifacts | repo-write |
| `regen-intake` | Regenerate intake-derived artifacts | repo-write |
| `regen-all` | Regenerate all build artifacts | repo-write |
| `all` | Build all code and documentation | repo-write |
| `debt-check` | Run debt tracking validation | repo-write |
| `check-infra` | Validate graph + Makefile | repo-write |
| `check-docs` | Validate documentation outputs | repo-write |
| `check-roadmap` | Validate roadmap exports | repo-write |
| `check-python` | Validate Python test suite | repo-write |
| `check-json` | Validate JSON roundtrip outputs | repo-write |
| `check-debt` | Validate deferred items + intake scan | repo-write |
| `check-all` | Run full validation suite | repo-write |
| `check` | Run all validation checks (alias) | read-only |
| `validate-constructive` | Regenerate and validate all artifacts | repo-write |
| `ci-light` | Lightweight CI target (no GHC backend) | repo-write |
| `ci-preflight` | Fast guard: graph + makefile docs | repo-write |
| `docker-rootless-status` | Check rootless Docker daemon status | repo-write |
| `docker-build` | Build Docker image (metacatagory:dev) | repo-write |
| `docker-build-ghcr` | Build and tag image for GHCR (requires GHCR_REGISTRY and GHCR_USERNAME env vars) | repo-write |
| `docker-push-ghcr` | Push image to GHCR (requires docker login; set GHCR_REGISTRY and GHCR_USERNAME) | repo-write |
| `docker-all` | Build and push to GHCR (full pipeline) | repo-write |
