# Makefile targets (witness)

This table is the human-readable witness for the Makefile triangle identity: the generated Makefile, its regeneration source, and this documentation must agree on the public targets.

| Target | Description |
| :--- | :--- |
| `regen-makefile` | Regenerate the Makefile from Agda source (Self-Hosting) |
| `md-lint` | Lint all markdown files (fail on error) |
| `md-fix` | Auto-fix markdown lint errors |
| `intake-lint` | Lint intake files specifically |
| `intake-scan` | Scan intake directory for new files |
| `md-normalize` | Normalize markdown formatting |
| `makefile-validate` | Validate Makefile consistency |
| `all` | Build all code and documentation |
| `check` | Run all validation checks |
| `badges` | Generate status badges |
| `node-deps` | Install Node.js dependencies |
| `deferred-items` | Scan for TODOs and FIXMEs |
| `roadmap-index` | Compile Roadmap Index |
| `roadmap-sync` | Sync roadmap with external tracker |
| `roadmap-sppf` | Compile Roadmap SPPF |
| `roadmap-merge` | Merge ingestion streams |
| `roadmap-deps-graph` | Generate dependency graph |
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
| `docs-generate` | Compile and run Roadmap Exporter |
| `docs-validate` | Validate documentation integrity |
