# Contributing to MetaCategory

Welcome! This guide will help you get started as a contributor to the MetaCategory project. Please read carefully to ensure your contributions are effective and consistent with project standards.

## Getting Started

- Clone the repository and set up your environment using the provided Makefile targets.

- Review the documentation in README.md, ROADMAP.md, and the relevant subdirectory manuals (see src/agda/*/README.md).

- Install dependencies:

```text
make venv
make node-deps
```

## Coding Standards

- Agda code should follow the conventions in the onboarding and module manuals.

- Python scripts should be formatted with black and linted with flake8.

- Markdown should be auto-formatted and linted using make md-fix and make md-lint.

- Commit messages should be clear and reference relevant issues or roadmap items.

## Adding Tests & Checklists

- Place new checklists in src/agda/Tests/ and update the relevant README.

- Use the checklist/test philosophy described in src/agda/Tests/README.md.

- Run make check-tests to verify your additions.

- Document the purpose and expected outcome of each test in its file header.

## Documentation

- Update or add documentation in the appropriate README.md or manual.

- For new modules, include a README describing its purpose, key files, and navigation.

- Link to relevant Makefile targets and automation scripts where appropriate.

## Understanding Directory Structure

### data/ - Version-Controlled Hierarchical JSON

The `data/` directory contains decomposed JSON artifacts generated via natural transformations:

- `data/deps/` - Decomposed dependency graph (91 fragments)
- `data/enriched/` - Decomposed canonical enriched items (122 fragments)
- `data/planning/` - Decomposed planning index items (122 fragments)

These are **pure text files** that version-control well in git and are tracked in `.gitignore` with `!data/**/*.json`.

**Never add large monolithic JSON files to data/.** The decomposition/recomposition transformation ensures only hierarchical fragments live here.

### build/ - Temporary Build Artifacts

The `build/` directory contains ephemeral outputs and is generally ignored:

- Monolithic source JSONs: `build/dependency_graph.json`, `build/canonical_enriched.json`, `build/planning_index.json`
- Temporary reconstructions: `build/*_recomposed.json` (created by roundtrip validation)
- Other build outputs: ignored by `.gitignore` except explicit witness files

### JSON Decomposition Workflow

If you modify the JSON decomposition/recomposition logic:

```bash
# 1. Decompose artifacts into hierarchies
make json-decompose json-decompose-enriched json-decompose-planning

# 2. Verify roundtrip (decompose → recompose → validate)
make json-roundtrip-validate json-roundtrip-validate-enriched json-roundtrip-validate-planning

# 3. These are automatically run by `make check`
make check
```

See `docs/process/JSON-DECOMPOSITION.md` for technical details.

## Interpreting Reports & Metrics

- Generated reports (e.g., top-offenders.md) highlight technical debt and deferred items.

- Use these reports to prioritize refactoring and documentation efforts.

- See .github/scripts/README.md for details on automation and metrics.

## Submitting Changes

1. Fork the repository and create a feature branch.

2. Make your changes and commit with a descriptive message.

3. Run all relevant Makefile targets to verify code, tests, and docs.

4. Open a pull request, referencing related issues or roadmap items.

5. Respond to review feedback and iterate as needed.

## Community & Support

- For questions, open a discussion or issue on GitHub.

- See CREDITS.md for contributors and acknowledgments.

Thank you for helping build MetaCategory!
