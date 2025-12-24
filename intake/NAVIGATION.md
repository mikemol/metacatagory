# MetaCategory Global Navigation

This document provides a high-level map of the repository's documentation and code structure, helping contributors and users find relevant manuals, guides, and modules quickly.

## Top-Level Documentation

*   README.md: System overview, axioms, architecture, and ontological stack.

*   ROADMAP.md: Project roadmap, phase status, and planning.

*   CONTRIBUTING.md: Contribution guidelines, onboarding, coding standards, and PR process.

*   DEFERRED-TRACKING.md: Technical debt and deferred items tracking.

*   COPILOT\_SYNERGY.md: AI and Copilot integration notes.

*   CREDITS.md: Contributors and acknowledgments.

*   ingest.md: Data ingestion notes.

*   testing.md: Testing philosophy and details.

## Source Atlas & Manuals

*   src/agda/README.md: Atlas of the source lattice, dimensional breakdown.

*   src/agda/Core/README.md: Core physics, axioms, limits, and algorithms.

*   src/agda/Algebra/README.md: Algebraic structures and navigation.

*   src/agda/Algebra/Groups/README.md: Abelian groups, enrichment, Grothendieck connection.

*   src/agda/Chapter1/README.md: Level 1 onboarding and curriculum.

*   src/agda/Tests/README.md: Checklist/test philosophy and boundaries.

## Automation & Scripts

.github/scripts/README.md: Automation philosophy, metric functor, topology visualizer, debt surveyor, and Makefile targets.

## Generated Reports

.github/badges/top-offenders.md: Technical debt offenders report.

## Navigation Tips

*   Each major directory contains a README.md with local context and navigation.

*   Use the Makefile targets for automation, documentation, and reporting (see .github/scripts/README.md).

*   For onboarding, start with src/agda/Chapter1/README.md and CONTRIBUTING.md.

*   For technical debt and deferred items, see DEFERRED-TRACKING.md and generated reports.

*   For roadmap and planning, see ROADMAP.md.

For further questions, see CONTRIBUTING.md or open an issue/discussion on GitHub.
