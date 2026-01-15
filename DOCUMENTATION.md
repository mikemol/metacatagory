# MetaCategory Documentation Index

This document provides an index to all project documentation.

## Essential Reading

* [README.md](README.md) - Project overview and quick start
* [CONTRIBUTING.md](CONTRIBUTING.md) - How to contribute
* [CREDITS.md](CREDITS.md) - Attribution and acknowledgments

## Canonical vs Generated

Some documents are hand-edited sources, while others are generated outputs.
When in doubt, regenerate rather than editing generated artifacts directly.

**Canonical (edit these):**
- `src/agda/Plan/CIM/Utility.agda` (roadmap sources)
- `intake/GP/` (intake source files)
- `docs/**` (architecture/process/status sources, except generated outputs listed below)

**Generated (regenerate via Makefile):**
- `ROADMAP.md` and `docs/planning/ROADMAP.md`
- `.github/roadmap/tasks.json`
- `data/planning_index.json`
- `data/dependency_graph.json`
- `docs/status/deferred-items.md`
- `docs/automation/makefile_targets_generated.md`

See `Makefile` and `TESTING.md` for regeneration targets.

## Documentation Structure

### Architecture & Design

* [docs/architecture/](docs/architecture/)
  * [ARCHITECTURE.md](docs/architecture/ARCHITECTURE.md) - Overall system architecture
  * [ALGEBRA-PARAMETERIZATION-COMPLETE.md](docs/architecture/ALGEBRA-PARAMETERIZATION-COMPLETE.md) - Algebra module parameterization details
  * [THEOREM-PARAMETERIZATION.md](docs/architecture/THEOREM-PARAMETERIZATION.md) - Theorem proof parameterization approach
  * [FRAMEWORK-INTEROPERABILITY.md](docs/architecture/FRAMEWORK-INTEROPERABILITY.md) - Framework integration patterns

### Automation & Pipelines

* [docs/automation/](docs/automation/)
  * [COMPOSITION-PATTERNS.md](docs/automation/COMPOSITION-PATTERNS.md) - Shared pipeline composition patterns for Python scripts
  * [MAKEFILE-TARGETS.md](docs/automation/MAKEFILE-TARGETS.md) - Makefile target reference
  * [MAKEFILE-TRIANGLE-IDENTITY.md](docs/automation/MAKEFILE-TRIANGLE-IDENTITY.md) - Triangle identity validation
  * [BUILD-TOPOLOGY.md](docs/process/BUILD-TOPOLOGY.md) - Build/CI topology and report layout

### Planning & Roadmap

* [docs/planning/](docs/planning/)
  * [ROADMAP.md](docs/planning/ROADMAP.md) - Main project roadmap
  * [ROADMAP-INDEX.md](docs/planning/ROADMAP-INDEX.md) - Roadmap index and navigation
  * [ROADMAP-ENRICHMENT.md](docs/planning/ROADMAP-ENRICHMENT.md) - Roadmap enrichment process
  * [ROADMAP-MIGRATION.md](docs/planning/ROADMAP-MIGRATION.md) - Roadmap migration notes
  * [ROADMAP-RESTRUCTURE-PROPOSAL.md](docs/planning/ROADMAP-RESTRUCTURE-PROPOSAL.md) - Restructuring proposals
  * [ROADMAP-DRAFT.md](docs/planning/ROADMAP-DRAFT.md) - Draft roadmap items

### Process & Quality

* [docs/process/](docs/process/)
  * [QUALITY-FRAMEWORK.md](docs/process/QUALITY-FRAMEWORK.md) - Quality assurance framework
  * [copilot-instructions.md](.github/copilot-instructions.md) - LLM integration guidelines

### Project Status

* [docs/status/](docs/status/)
  * [PROJECT-STATUS.md](docs/status/PROJECT-STATUS.md) - Current project status
  * [DEFERRED-TRACKING.md](docs/status/DEFERRED-TRACKING.md) - Deferred items tracking system
  * [deferred-items.md](docs/status/deferred-items.md) - Current deferred items report

### Workflows & Guides

* [docs/workflows/](docs/workflows/)
  * [ENRICHMENT-QUICKSTART.md](docs/workflows/ENRICHMENT-QUICKSTART.md) - Quick start guide for roadmap enrichment
  * [INTAKE-INGESTION-README.md](docs/workflows/INTAKE-INGESTION-README.md) - Intake ingestion process guide

### Session Summaries

* [docs/sessions/](docs/sessions/)
  * [ENRICHMENT-SESSION-SUMMARY.md](docs/sessions/ENRICHMENT-SESSION-SUMMARY.md) - Roadmap enrichment session summary
  * [INTAKE-INGESTION-SUMMARY.md](docs/sessions/INTAKE-INGESTION-SUMMARY.md) - Intake ingestion summary
  * [INTAKE-INGESTION-COMPLETION.md](docs/sessions/INTAKE-INGESTION-COMPLETION.md) - Intake completion report
  * [ROADMAP\_EXTRACTION\_SUMMARY.md](docs/sessions/ROADMAP_EXTRACTION_SUMMARY.md) - Roadmap extraction summary
  * [PHASE3-AGENTIC-TRAVERSAL.md](docs/sessions/PHASE3-AGENTIC-TRAVERSAL.md) - Phase 3 agentic traversal
  * [PHASE4-CROSS-REFERENCE.md](docs/sessions/PHASE4-CROSS-REFERENCE.md) - Phase 4 cross-reference work

### Theory & Analysis

* [docs/theory/](docs/theory/)
  * [CIM-COMPENDIUM-INTEGRATED.md](docs/theory/CIM-COMPENDIUM-INTEGRATED.md) - Coherence Induction Metacategory compendium
  * [CIM-INTEGRATION-ANALYSIS.md](docs/theory/CIM-INTEGRATION-ANALYSIS.md) - CIM integration analysis
  * [CIM-ROADMAP-SYMMETRIC-DIFFERENCE.md](docs/theory/CIM-ROADMAP-SYMMETRIC-DIFFERENCE.md) - Gap analysis between theory and roadmap
  * [INTAKE-CONTENT-ANALYSIS.md](docs/theory/INTAKE-CONTENT-ANALYSIS.md) - Content analysis of intake materials

### Navigation

* [docs/NAVIGATION.md](docs/NAVIGATION.md) - Site navigation and cross-references

### Intake & Working Files

* [intake/](intake/) - Raw ingested content, conversation fragments, and working files
  * [intake/GP/](intake/GP/) - 78 roadmap specification files
  * Various conversation fragments and analysis files

## Quick Links by Task

### I want to

* **Understand the project** → Start with [README.md](README.md) and [ARCHITECTURE.md](docs/architecture/ARCHITECTURE.md)
* **Contribute** → Read [CONTRIBUTING.md](CONTRIBUTING.md) and [QUALITY-FRAMEWORK.md](docs/process/QUALITY-FRAMEWORK.md)
* **See the roadmap** → Check [ROADMAP.md](docs/planning/ROADMAP.md) and [PROJECT-STATUS.md](docs/status/PROJECT-STATUS.md)
* **Run enrichment pipeline** → Follow [ENRICHMENT-QUICKSTART.md](docs/workflows/ENRICHMENT-QUICKSTART.md)
* **Understand the algebra work** → Read [ALGEBRA-PARAMETERIZATION-COMPLETE.md](docs/architecture/ALGEBRA-PARAMETERIZATION-COMPLETE.md)
* **Work with Copilot** → See [copilot-instructions.md](.github/copilot-instructions.md)

## Documentation Standards

All documentation follows:

* Markdown format with markdownlint validation
* Auto-formatted via `make md-fix` or `markdownlint-cli2 --fix`
* Cross-referenced using relative links
* Organized by purpose (architecture, planning, process, status, workflows, sessions, theory)
