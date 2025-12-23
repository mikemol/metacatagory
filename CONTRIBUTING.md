# Contributing to MetaCategory

Welcome! This guide will help you get started as a contributor to the MetaCategory project. Please read carefully to ensure your contributions are effective and consistent with project standards.

## Getting Started

*   Clone the repository and set up your environment using the provided Makefile targets.

*   Review the documentation in README.md, ROADMAP.md, and the relevant subdirectory manuals (see src/agda/\*/README.md).

*   Install dependencies:

```text
make venv
make node-deps
```

## Coding Standards

*   Agda code should follow the conventions in the onboarding and module manuals.

*   Python scripts should be formatted with black and linted with flake8.

*   Markdown should be auto-formatted and linted using make md-fix and make md-lint.

*   Commit messages should be clear and reference relevant issues or roadmap items.

## Adding Tests & Checklists

*   Place new checklists in src/agda/Tests/ and update the relevant README.

*   Use the checklist/test philosophy described in src/agda/Tests/README.md.

*   Run make check-tests to verify your additions.

*   Document the purpose and expected outcome of each test in its file header.

## Documentation

*   Update or add documentation in the appropriate README.md or manual.

*   For new modules, include a README describing its purpose, key files, and navigation.

*   Link to relevant Makefile targets and automation scripts where appropriate.

## Interpreting Reports & Metrics

*   Generated reports (e.g., top-offenders.md) highlight technical debt and deferred items.

*   Use these reports to prioritize refactoring and documentation efforts.

*   See .github/scripts/README.md for details on automation and metrics.

## Submitting Changes

1.  Fork the repository and create a feature branch.

2.  Make your changes and commit with a descriptive message.

3.  Run all relevant Makefile targets to verify code, tests, and docs.

4.  Open a pull request, referencing related issues or roadmap items.

5.  Respond to review feedback and iterate as needed.

## Community & Support

*   For questions, open a discussion or issue on GitHub.

*   See CREDITS.md for contributors and acknowledgments.

Thank you for helping build MetaCategory!
