# GitHub Copilot / AI Agent Synergy Guide

Last Updated: 2025-11-19

Purpose: Provide precise, low-friction instructions for AI assistants (Copilot, Chat-based, or automation agents) operating in this repository so they produce high‑value, phase‑aligned, minimally disruptive contributions.

***

## Core Orientation Sequence (Run on First Entry)

1.  Read `ROADMAP.md` (concise status) and, if deeper rationale needed, skim `ROADMAP-DRAFT.md`.
2.  Load `.github/roadmap/tasks.json` to know active/planned/deferred tasks.
3.  Check `DEFERRED-TRACKING.md` for current technical debt structure (postulates, TODO counts, rationale philosophy).
4.  Consult `docs/ARCHITECTURE.md` for layer mapping and file purpose before editing any core module.
5.  For domain ingestion tasks, reference `ingest.md` (curriculum outline) and map section → record(s)/adapter(s) needed.
6.  When working inside tests, open the relevant file in `src/agda/Tests/` and confirm phase numbering alignment.

Optional quick commands:

```bash
grep -R "Phase III" src/agda/Tests
grep -R "postulate" src/agda/Core | wc -l
```

***

## Workflow Patterns

### A. Implementing a Phase Task

1.  Identify task by `id` (e.g., `PHASE-IV.2`) in `tasks.json`.
2.  Locate related source files listed in the `files` array.
3.  Add smallest cohesive Agda module changes; avoid speculative refactors.
4.  Provide test scaffolding (new phase block at end of existing test suite or a dedicated checklist).
5.  Run narrowed typecheck commands:

```bash
agda --no-main -i src/agda src/agda/Tests/YonedaChecklist.agda
```

1.  Update task status in `tasks.json` (planned → in-progress → completed).
2.  Run sync script:

```bash
bash .github/scripts/sync-roadmap-issues.sh
```

1.  If the change reduces postulates or TODOs, mention it in the phase issue comment.

### B. Ingestion Expansion

Map textbook section → internal representation:

*   Define or extend a declaration record (`GroupDeclaration`, `IdealDeclaration`, etc.).
*   Add adapter(s) for properties (e.g. Normality, PrimeIdeal) in an appropriate checklist.
*   Use Bool-based placeholder validations first; upgrade to proofs later.
*   Tag new ingestion task with `INGEST-` prefix when adding to `tasks.json`.

### C. Deferred Item Resolution

1.  Run `.github/scripts/detect-deferred-items.sh` locally to capture baseline.
2.  Remove a specific postulate only if constructive data is available (avoid partial broken proofs).
3.  Add a DeviationLog comment if temporarily replacing a brittle proof with Bool scaffolding.
4.  Re-run scan; celebrate delta in PR description.

### D. Serialization / External Boundary Work

Maintain coordinate fidelity: preserve usage of `mkIdAt` and ordering checks `_ <ⁱ _` after externalization.
When adding new external serialization formats, ensure roundtrip test proves ordering + coordinate preservation.

***

## Code Editing Principles

| Principle                                                    | Rationale                                              |
| ------------------------------------------------------------ | ------------------------------------------------------ |
| Minimize diff scope                                          | Reduces merge friction and review overhead             |
| Preserve public API names                                    | Stability for search tooling and existing tests        |
| Prefer Bool scaffolding before proofs                        | Maintains build stability in exploratory phases        |
| Explicitly tag new warnings or placeholders with identifiers | Enables later search and refactoring                   |
| Keep tests phase-indexed consistently                        | Ensures roadmap and test suite correlation             |
| Avoid large batch renames                                    | High risk for token/context mismatch in LLM follow-ups |

***

## File Role Cheat-Sheet

| File                                   | Role                                       |
| -------------------------------------- | ------------------------------------------ |
| `Metamodel.agda`                       | Base identifiers, coordinates, ordering    |
| `Core/Limitations.agda`                | Error-as-specification carrier types       |
| `Core/AlgorithmComplexity.agda`        | Complexity classes & ordering              |
| `Core/AlgebraicAlgorithms.agda`        | Algorithm interfaces (fields, groups)      |
| `Core/AlgorithmUniversality.agda`      | Bridge algorithms ↔ universal properties  |
| `Core/ConstructiveWitnesses.agda`      | Constructive algorithm witnesses & helpers |
| `Tests/*Checklist.agda`                | Structural validation & property coverage  |
| `Tests/SerializationTests.agda`        | Roundtrip / HoTT path isomorphism tests    |
| `Tests/ErrorAsSpecificationTests.agda` | Limitation integration tests               |
| `ROADMAP.md`                           | Actionable condensed roadmap               |
| `DEFERRED-TRACKING.md`                 | Technical debt philosophy & counts         |
| `.github/roadmap/tasks.json`           | Machine-readable task definitions          |

***

## Issue & Task Automation

Script: `.github/scripts/sync-roadmap-issues.sh`
Environment variables required: `GITHUB_TOKEN`, `GITHUB_REPOSITORY`.
Labels: All tasks labeled `roadmap`. Status transitions only via JSON edits.

Add new task snippet:

```json
{
  "id": "INGEST-Ch2-GroupActions",
  "category": "Ingestion",
  "title": "Ingest group actions (orbit/stabilizer adapters)",
  "status": "planned",
  "source": "ingest.md",
  "files": ["src/agda/Algebra/Groups/Actions.agda"],
  "tags": ["Groups", "Actions", "Ingestion"]
}
```

***

## Response Formatting Guidance for AI Agents

When generating responses (comments, PR descriptions, summaries):

1.  Start with a one-line outcome statement.
2.  Use concise bullet sections (≤6 bullets) with **bold nouns**.
3.  Wrap file paths and commands in backticks.
4.  Do not paste entire large Agda modules; reference paths.
5.  For math, use inline `$` or block `$$` with KaTeX-compatible syntax (when needed).
6.  If patching: apply minimal contextual diff, avoid unrelated reformatting.

***

## Safety & Content Policy Reminders

*   Reject requests for harmful, hateful, violent, sexist, or lewd content (respond with: `Sorry, I can't assist with that.`).
*   Do not fabricate proofs; mark placeholders explicitly.
*   Avoid speculative complexity claims—derive from `AlgorithmComplexity.agda` annotations.
*   Respect license and avoid introducing incompatible third-party code without clearance.

***

## Common Pitfalls & Avoidance

| Pitfall                                              | Avoidance Strategy                                        |
| ---------------------------------------------------- | --------------------------------------------------------- |
| Replacing working Bool scaffolds with partial proofs | Keep Bool until full constructive witness available       |
| Large unscoped refactors in hierarchy                | Introduce changes per declaration; verify index integrity |
| Adding tasks without issue sync                      | Run sync script post-commit                               |
| Ignoring coordinate ordering in new serialization    | Add ordering preservation test immediately                |
| Leaving postulates undocumented                      | Add DeviationLog or roadmap task referencing removal plan |

***

## Quick Validation Suite

Focused commands to validate typical changes:

```bash
# Universal property & witness bridging
agda --no-main -i src/agda src/agda/Tests/UniversalPropertyTests.agda

# Serialization integrity
agda --no-main -i src/agda src/agda/Tests/SerializationTests.agda

# Error-as-specification boundary
agda --no-main -i src/agda src/agda/Tests/ErrorAsSpecificationTests.agda

# Performance / complexity annotations
agda --no-main -i src/agda src/agda/Tests/PerformanceBoundaryTests.agda
```

***

## Extension Playbook (Example: Adjunction Proof Task `PHASE-IV.1`)

1.  Add new checklist file `Tests/YonedaChecklist.agda` or extend existing.
2.  Introduce record capturing unit/counit identifiers.
3.  Provide Bool validation placeholder for naturality squares.
4.  Future pass: replace Bool with dependent equality proofs once stable.
5.  Update `tasks.json` status and sync.

***

## Review Heuristics for Merges

Before approving an AI-generated PR:

*   Diff size < 300 lines unless adding a new checklist module.
*   No unrelated whitespace-only churn.
*   `ROADMAP.md` or `tasks.json` updated if task claimed complete.
*   Postulate count does not spike without added justification comment.
*   New serialization code includes roundtrip + ordering test.

***

## Escalation Paths

If blocked by missing constructive arithmetic or deep category-theoretic witness:

1.  Park with a new `INGEST-` or `PHASE-` task (status `deferred`).
2.  Add concise DeviationLog entry: motivation + revisit conditions.
3.  Link to issue via sync script.

***

## Minimal Example Commit Message Template

```text
feat(phase-iv): add Yoneda checklist scaffold

Adds Bool-based naturality placeholders and unit/counit identifiers.
Roadmap: PHASE-IV.2 (status: in-progress)
No change in postulate count.
```

***

## TL;DR

Consult roadmap → pick task → minimal diff → tests green → update tasks.json → sync issues → document deltas. Preserve ordering, reduce postulates intentionally, defer safely.

***

## Reproducibility & Automation

- Use Makefile targets for setup and automation:
  - `make venv` — Set up Python environment
  - `make node-deps` — Install Node dependencies
  - `make badges` — Generate technical debt badges and metrics
  - `make docs-all` — Build all documentation
  - `make deferred-items` — Track deferred items
  - `make test_report` — Run the full constraint solver and generate test reports
- See `.github/scripts/README.md` for details on automation scripts and manual invocation.
- For troubleshooting and process, see `CONTRIBUTING.md` and `src/agda/Tests/README.md`.
