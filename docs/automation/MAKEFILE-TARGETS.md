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
| `json-recompose` | Recompose hierarchical JSON back to monolithic form |
| `json-roundtrip-validate` | Validate JSON decomposition roundtrip |
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
