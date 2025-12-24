# Makefile targets (witness)

This table is the human-readable witness for the Makefile triangle identity: the generated Makefile, its regeneration source, and this documentation must agree on the public targets.

| Target | Purpose | Notes |
| --- | --- | --- |
| `all` | Aggregate default build | Depends on project defaults (intended to cover Agda + docs); placeholder until wired. |
| `check` | Aggregate validation | Intended to run core validators (triangle, docs); placeholder until wired. |
| `regen-makefile` | Regenerate Makefile from Agda exporter | Runs `src/agda/Examples/ExporterMakefile.agda` to refresh Makefile.generated then copies over. |
| `md-fix` | Auto-format Markdown | Uses remark to rewrite Markdown in-place. |
| `md-lint` | Lint Markdown | remark lint across repo; writes `build/reports/md-lint.txt`. |
| `intake-lint` | Lint intake/ Markdown | remark lint scoped to intake/; writes `build/reports/intake-md-lint.txt`. |
| `intake-scan` | Report intake coverage | Runs `scripts/intake_scan.py` to emit coverage reports. |
| `md-normalize` | Normalize generated Markdown | Runs `scripts/normalize_generated_markdown.py`. |
| `makefile-validate` | Check Makefile ↔ docs ↔ generator | Runs `scripts/validate_makefile_docs.py`. |
| `badges` | Generate status badges | Uses `scripts/generate-badges.py`. |
| `node-deps` | Install Node dependencies | Runs `npm install`. |
| `deferred-items` | Detect deferred items | Runs `.github/scripts/detect-deferred-items.sh`. |
| `roadmap-index` | Build roadmap index | Compiles Agda index. |
| `roadmap-sync` | Sync roadmap to GitHub | Runs Agda sync helper. |
| `roadmap-sppf` | Build roadmap SPPF | Compiles SPPF artifact. |
| `roadmap-merge` | Merge roadmap sources | Creates canonical JSON. |
| `roadmap-deps-graph` | Emit Agda dependency graph | Writes graph DOT. |
| `roadmap-enrich` | Enrich canonical roadmap | Adds metadata to canonical JSON. |
| `roadmap-export-json` | Export canonical to JSON | Writes `build/canonical_roadmap.json`. |
| `roadmap-export-md` | Export canonical to Markdown | Writes `ROADMAP.md`. |
| `roadmap-export-enriched` | Export enriched Markdown | Writes enriched docs. |
| `roadmap-export-deps` | Export dependency graph report | Writes dependency graph JSON/MD. |
| `roadmap-validate-json` | Validate JSON projection | Checks tasks.json against canonical. |
| `roadmap-validate-md` | Validate Markdown projection | Checks ROADMAP.md against canonical. |
| `roadmap-validate-triangle` | Validate projections | Ensures JSON/MD agree with canonical. |
| `roadmap-sppf-export` | Export SPPF projection | Writes SPPF JSON. |
| `roadmap-all-enriched` | Aggregate enriched exports | Placeholder until wired. |
| `agda-all` | Build all Agda interfaces | Compiles all .agdai artifacts. |
| `docs-all` | Build all HTML docs | Generates HTML for Agda sources. |
| `docs-generate` | Generate documentation bundles | Runs roadmap exporter and normalization. |
| `docs-validate` | Validate documentation triangle | Runs triangle validation script. |
